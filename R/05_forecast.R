# ============================================================
# Question 5: Predicting Trump's August 2026 net approval
# ============================================================

# Install ggplot2 once if needed:
# install.packages("ggplot2")

library(ggplot2)

# ------------------------------------------------------------
# 1. Read the data
# ------------------------------------------------------------

polls <- read.csv(
  "Silver_Bulletin_Polling.csv",
  stringsAsFactors = FALSE
)

# Identify rows belonging to the same underlying survey.
polls$poll_id <- paste(
  polls$pollster,
  polls$startdate,
  polls$enddate,
  polls$samplesize,
  polls$population,
  ifelse(is.na(polls$url), "", polls$url),
  sep = " | "
)

# ------------------------------------------------------------
# 2. Keep one overall presidential-approval result per survey
# ------------------------------------------------------------

make_overall_poll <- function(g) {
  
  all_row <- g[g$subgroup == "All polls", ]
  strong_row <- g[g$subgroup == "Strong", ]
  weak_row <- g[g$subgroup == "Weak", ]
  adults_row <- g[g$subgroup == "Adults", ]
  voters_row <- g[g$subgroup == "Voters", ]
  
  net_result <- NA_real_
  result_date <- NA_character_
  
  # Prefer the reported overall result.
  if (nrow(all_row) > 0) {
    
    all_row <- all_row[1, ]
    net_result <- all_row$approve - all_row$disapprove
    result_date <- all_row$enddate
    
    # Otherwise combine Strong and Weak approval categories.
  } else if (
    nrow(strong_row) > 0 &&
    nrow(weak_row) > 0
  ) {
    
    strong_row <- strong_row[1, ]
    weak_row <- weak_row[1, ]
    
    total_approve <-
      strong_row$approve +
      weak_row$approve
    
    total_disapprove <-
      strong_row$disapprove +
      weak_row$disapprove
    
    net_result <- total_approve - total_disapprove
    result_date <- strong_row$enddate
    
    # Use Adults as a fallback.
  } else if (nrow(adults_row) > 0) {
    
    adults_row <- adults_row[1, ]
    net_result <- adults_row$approve - adults_row$disapprove
    result_date <- adults_row$enddate
    
    # Use Voters as the final fallback.
  } else if (nrow(voters_row) > 0) {
    
    voters_row <- voters_row[1, ]
    net_result <- voters_row$approve - voters_row$disapprove
    result_date <- voters_row$enddate
  }
  
  if (is.na(net_result)) {
    return(NULL)
  }
  
  data.frame(
    pollster = g$pollster[1],
    poll_date = result_date,
    net_approval = net_result,
    stringsAsFactors = FALSE
  )
}

poll_list <- split(
  polls,
  polls$poll_id
)

overall_polls <- do.call(
  rbind,
  lapply(poll_list, make_overall_poll)
)

rownames(overall_polls) <- NULL

# ------------------------------------------------------------
# 3. Convert the date and calculate daily averages
# ------------------------------------------------------------

overall_polls$poll_date <- as.Date(
  overall_polls$poll_date,
  format = "%m/%d/%Y"
)

overall_polls <- overall_polls[
  !is.na(overall_polls$poll_date) &
    !is.na(overall_polls$net_approval),
]

cat("\nDate datatype:\n")
print(class(overall_polls$poll_date))

cat("\nObserved date range:\n")
print(range(overall_polls$poll_date))

# Average all polls completed on the same date so a date with
# many polls does not automatically receive more influence.
daily_data <- aggregate(
  net_approval ~ poll_date,
  data = overall_polls,
  FUN = mean
)

daily_data <- daily_data[
  order(daily_data$poll_date),
]

rownames(daily_data) <- NULL

first_date <- min(daily_data$poll_date)

daily_data$time <- as.numeric(
  daily_data$poll_date - first_date
)

cat("\nNumber of daily observations:\n")
print(nrow(daily_data))

# ------------------------------------------------------------
# 4. Use a chronological 80/20 training/test split
# ------------------------------------------------------------

split_index <- floor(0.80 * nrow(daily_data))

training_data <- daily_data[1:split_index, ]
test_data <- daily_data[(split_index + 1):nrow(daily_data), ]

cat("\nTraining period:\n")
print(range(training_data$poll_date))

cat("\nTest period:\n")
print(range(test_data$poll_date))

cat("\nTraining observations:\n")
print(nrow(training_data))

cat("\nTest observations:\n")
print(nrow(test_data))

# ------------------------------------------------------------
# 5. Fit three distinct regression model types
#    - Linear regression
#    - Quadratic polynomial regression
#    - Nonlinear exponential regression
# ------------------------------------------------------------

linear_model <- lm(
  net_approval ~ time,
  data = training_data
)

quadratic_model <- lm(
  net_approval ~ time + I(time^2),
  data = training_data
)

exponential_model <- nls(
  net_approval ~ asymptote + amplitude * exp(-rate * time),
  data = training_data,
  start = list(
    asymptote = -25,
    amplitude = 30,
    rate = 0.002
  ),
  algorithm = "port",
  lower = c(
    asymptote = -100,
    amplitude = -200,
    rate = 0
  ),
  upper = c(
    asymptote = 100,
    amplitude = 200,
    rate = 1
  ),
  control = nls.control(
    maxiter = 1000,
    warnOnly = TRUE
  )
)

# ------------------------------------------------------------
# 6. Generate training and test predictions
# ------------------------------------------------------------

training_predictions <- list(
  Linear = predict(linear_model, newdata = training_data),
  Quadratic = predict(quadratic_model, newdata = training_data),
  Exponential = predict(exponential_model, newdata = training_data)
)

test_predictions <- list(
  Linear = predict(linear_model, newdata = test_data),
  Quadratic = predict(quadratic_model, newdata = test_data),
  Exponential = predict(exponential_model, newdata = test_data)
)

# ------------------------------------------------------------
# 7. Calculate model errors
# ------------------------------------------------------------

calculate_errors <- function(
    actual,
    predicted,
    model_name,
    dataset_name
) {
  
  residuals <- actual - predicted
  nonzero <- actual != 0
  
  mape <- if (any(nonzero)) {
    mean(abs(residuals[nonzero] / actual[nonzero])) * 100
  } else {
    NA_real_
  }
  
  denominator <- sum((actual - mean(actual))^2)
  
  r_squared <- if (denominator > 0) {
    1 - sum(residuals^2) / denominator
  } else {
    NA_real_
  }
  
  data.frame(
    Model = model_name,
    Dataset = dataset_name,
    ME = mean(residuals),
    SSE = sum(residuals^2),
    MAE = mean(abs(residuals)),
    MSE = mean(residuals^2),
    RMSE = sqrt(mean(residuals^2)),
    MAPE = mape,
    R_squared = r_squared,
    stringsAsFactors = FALSE
  )
}

error_results <- rbind(
  calculate_errors(
    training_data$net_approval,
    training_predictions$Linear,
    "Linear",
    "Training"
  ),
  calculate_errors(
    test_data$net_approval,
    test_predictions$Linear,
    "Linear",
    "Test"
  ),
  calculate_errors(
    training_data$net_approval,
    training_predictions$Quadratic,
    "Quadratic",
    "Training"
  ),
  calculate_errors(
    test_data$net_approval,
    test_predictions$Quadratic,
    "Quadratic",
    "Test"
  ),
  calculate_errors(
    training_data$net_approval,
    training_predictions$Exponential,
    "Exponential",
    "Training"
  ),
  calculate_errors(
    test_data$net_approval,
    test_predictions$Exponential,
    "Exponential",
    "Test"
  )
)

numeric_columns <- c(
  "ME",
  "SSE",
  "MAE",
  "MSE",
  "RMSE",
  "MAPE",
  "R_squared"
)

error_results[numeric_columns] <- lapply(
  error_results[numeric_columns],
  round,
  digits = 3
)

cat("\nTraining and test errors:\n")
print(error_results, row.names = FALSE)

write.csv(
  error_results,
  "question5_model_errors.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Check for overfitting
# ------------------------------------------------------------

training_errors <- error_results[
  error_results$Dataset == "Training",
  c("Model", "RMSE")
]

test_errors <- error_results[
  error_results$Dataset == "Test",
  c("Model", "RMSE")
]

names(training_errors)[2] <- "Training_RMSE"
names(test_errors)[2] <- "Test_RMSE"

overfitting_table <- merge(
  training_errors,
  test_errors,
  by = "Model"
)

overfitting_table$RMSE_increase <-
  overfitting_table$Test_RMSE -
  overfitting_table$Training_RMSE

overfitting_table$RMSE_ratio <-
  overfitting_table$Test_RMSE /
  overfitting_table$Training_RMSE

overfitting_table$RMSE_increase <- round(
  overfitting_table$RMSE_increase,
  3
)

overfitting_table$RMSE_ratio <- round(
  overfitting_table$RMSE_ratio,
  3
)

cat("\nOverfitting comparison:\n")
print(overfitting_table, row.names = FALSE)

write.csv(
  overfitting_table,
  "question5_overfitting.csv",
  row.names = FALSE
)

# Choose the model with the lowest test RMSE.
test_rows <- error_results[
  error_results$Dataset == "Test",
]

best_model <- test_rows$Model[
  which.min(test_rows$RMSE)
]

cat(
  "\nBest model according to test RMSE:",
  best_model,
  "\n"
)

# ------------------------------------------------------------
# 9. Refit all models using every observed date
# ------------------------------------------------------------

linear_full <- lm(
  net_approval ~ time,
  data = daily_data
)

quadratic_full <- lm(
  net_approval ~ time + I(time^2),
  data = daily_data
)

exponential_full <- nls(
  net_approval ~ asymptote + amplitude * exp(-rate * time),
  data = daily_data,
  start = list(
    asymptote = -35,
    amplitude = 40,
    rate = 0.0015
  ),
  algorithm = "port",
  lower = c(
    asymptote = -100,
    amplitude = -200,
    rate = 0
  ),
  upper = c(
    asymptote = 100,
    amplitude = 200,
    rate = 1
  ),
  control = nls.control(
    maxiter = 1000,
    warnOnly = TRUE
  )
)

# ------------------------------------------------------------
# 10. Predict through August 31, 2026
# ------------------------------------------------------------

last_observed_date <- max(daily_data$poll_date)

prediction_dates <- data.frame(
  poll_date = seq(
    from = first_date,
    to = as.Date("2026-08-31"),
    by = "day"
  )
)

prediction_dates$time <- as.numeric(
  prediction_dates$poll_date - first_date
)

model_plot_data <- rbind(
  data.frame(
    poll_date = prediction_dates$poll_date,
    Predicted_net = predict(linear_full, newdata = prediction_dates),
    Model = "Linear"
  ),
  data.frame(
    poll_date = prediction_dates$poll_date,
    Predicted_net = predict(quadratic_full, newdata = prediction_dates),
    Model = "Quadratic"
  ),
  data.frame(
    poll_date = prediction_dates$poll_date,
    Predicted_net = predict(exponential_full, newdata = prediction_dates),
    Model = "Exponential"
  )
)

model_plot_data$Period <- ifelse(
  model_plot_data$poll_date <= last_observed_date,
  "Fitted historical value",
  "Forecast"
)

model_plot_data$Model <- factor(
  model_plot_data$Model,
  levels = c("Linear", "Quadratic", "Exponential")
)

model_plot_data$Period <- factor(
  model_plot_data$Period,
  levels = c("Fitted historical value", "Forecast")
)

# ------------------------------------------------------------
# 11. Calculate average August forecasts
# ------------------------------------------------------------

august_predictions <- model_plot_data[
  model_plot_data$poll_date >= as.Date("2026-08-01") &
    model_plot_data$poll_date <= as.Date("2026-08-31"),
]

august_averages <- aggregate(
  Predicted_net ~ Model,
  data = august_predictions,
  FUN = mean
)

august_averages$Predicted_net <- round(
  august_averages$Predicted_net,
  2
)

cat("\nAverage predicted net approval in August 2026:\n")
print(august_averages, row.names = FALSE)

write.csv(
  august_averages,
  "question5_august_predictions.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 12. Calculate weekly historical averages for the graph
# ------------------------------------------------------------

daily_data$week_start <-
  daily_data$poll_date -
  as.integer(format(daily_data$poll_date, "%u")) +
  1

weekly_history <- aggregate(
  net_approval ~ week_start,
  data = daily_data,
  FUN = mean
)

# ------------------------------------------------------------
# 13. Prepare graph annotations with real Date objects
# ------------------------------------------------------------

august_region <- data.frame(
  start = as.Date("2026-08-01"),
  end = as.Date("2026-08-31")
)

cutoff_line <- data.frame(
  date = last_observed_date
)

august_values <- setNames(
  august_averages$Predicted_net,
  as.character(august_averages$Model)
)

august_label <- paste(
  "Average August forecast",
  paste0("Quadratic: ", sprintf("%.2f", august_values["Quadratic"])),
  paste0("Exponential: ", sprintf("%.2f", august_values["Exponential"])),
  paste0("Linear: ", sprintf("%.2f", august_values["Linear"])),
  sep = "\n"
)

best_august_value <- august_values[best_model]

graph_title <- paste(
  paste0(
    best_model,
    " Regression Performs Best and Predicts ",
    sprintf("%.1f", best_august_value)
  ),
  "Net Approval in August"
)

test_rmse_values <- setNames(
  test_rows$RMSE,
  test_rows$Model
)

graph_subtitle <- paste0(
  "Test RMSE: quadratic ",
  sprintf("%.2f", test_rmse_values["Quadratic"]),
  ", exponential ",
  sprintf("%.2f", test_rmse_values["Exponential"]),
  ", and linear ",
  sprintf("%.2f", test_rmse_values["Linear"]),
  "."
)

annotation_height <- max(
  c(
    weekly_history$net_approval,
    model_plot_data$Predicted_net
  ),
  na.rm = TRUE
) - 1

# Use uncluttered two-month labels for the historical period, then
# explicitly label August 2026 so the forecast month cannot be missed.
x_axis_breaks <- c(
  seq(
    from = as.Date("2025-03-01"),
    to = as.Date("2026-07-01"),
    by = "2 months"
  ),
  as.Date("2026-08-01")
)

# ------------------------------------------------------------
# 14. Create the finalized graph
# ------------------------------------------------------------

question5_plot <- ggplot() +
  
  geom_rect(
    data = august_region,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = "#E6F2FF",
    alpha = 0.75
  ) +
  
  geom_line(
    data = weekly_history,
    aes(
      x = week_start,
      y = net_approval
    ),
    color = "grey65",
    linewidth = 0.55
  ) +
  
  geom_point(
    data = weekly_history,
    aes(
      x = week_start,
      y = net_approval
    ),
    color = "grey45",
    size = 1.5,
    alpha = 0.8
  ) +
  
  geom_line(
    data = model_plot_data,
    aes(
      x = poll_date,
      y = Predicted_net,
      color = Model,
      linetype = Period
    ),
    linewidth = 1.05
  ) +
  
  geom_vline(
    data = cutoff_line,
    aes(xintercept = date),
    inherit.aes = FALSE,
    color = "grey20",
    linewidth = 0.65
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "grey70",
    linewidth = 0.65
  ) +
  
  scale_color_manual(
    name = "Model",
    values = c(
      "Linear" = "#0072B2",
      "Quadratic" = "#D55E00",
      "Exponential" = "#009E73"
    ),
    breaks = c("Linear", "Quadratic", "Exponential")
  ) +
  
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Fitted historical value" = "solid",
      "Forecast" = "dashed"
    ),
    breaks = c("Fitted historical value", "Forecast")
  ) +
  
  scale_x_date(
    breaks = x_axis_breaks,
    date_labels = "%b\n%Y",
    limits = c(
      min(weekly_history$week_start),
      as.Date("2026-09-01")
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  labs(
    title = graph_title,
    subtitle = graph_subtitle,
    x = NULL,
    y = "Average net approval (percentage points)",
    caption = paste0(
      "Grey points show weekly averages. The vertical line marks the final observed date; ",
      "dashed lines are forecasts and blue shading marks August 2026.\n",
      "Source: Silver Bulletin Polling dataset."
    )
  ) +
  
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(
      color = "grey30",
      size = 11,
      margin = margin(b = 10)
    ),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      color = "grey35",
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(margin = margin(r = 8)),
    plot.margin = margin(12, 20, 12, 12)
  )

# Display the graph.
print(question5_plot)

# ------------------------------------------------------------
# 15. Save the graph
# ------------------------------------------------------------

output_file <- file.path(
  getwd(),
  "question5_august_forecast.png"
)

ggsave(
  filename = output_file,
  plot = question5_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat(
  "\nGraph saved here:\n",
  normalizePath(
    output_file,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

cat(
  "File successfully created:",
  file.exists(output_file),
  "\n"
)