# ============================================================
# Question 2: Bayes factors by polling firm
# ============================================================

# Install ggplot2 once if needed:
# install.packages("ggplot2")

library(ggplot2)

# The CSV should normally be in the R working directory. The second
# path lets this copy run directly in the provided workspace as well.
data_path <- if (file.exists("Silver_Bulletin_Polling.csv")) {
  "Silver_Bulletin_Polling.csv"
} else {
  "upload/Silver_Bulletin_Polling.csv"
}

polls <- read.csv(
  data_path,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 1. Identify rows from the same underlying survey
# ------------------------------------------------------------

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
  
  # Prefer an explicitly reported overall result.
  if (nrow(all_row) > 0) {
    all_row <- all_row[1, ]
    net_result <- all_row$approve - all_row$disapprove
    
    # Otherwise reconstruct overall approval by adding the
    # Strong and Weak approval/disapproval components.
  } else if (nrow(strong_row) > 0 && nrow(weak_row) > 0) {
    strong_row <- strong_row[1, ]
    weak_row <- weak_row[1, ]
    
    net_result <-
      (strong_row$approve + weak_row$approve) -
      (strong_row$disapprove + weak_row$disapprove)
    
    # Adults and Voters are fallbacks when no overall result or
    # Strong/Weak pair is available.
  } else if (nrow(adults_row) > 0) {
    adults_row <- adults_row[1, ]
    net_result <- adults_row$approve - adults_row$disapprove
    
  } else if (nrow(voters_row) > 0) {
    voters_row <- voters_row[1, ]
    net_result <- voters_row$approve - voters_row$disapprove
  }
  
  if (is.na(net_result)) return(NULL)
  
  data.frame(
    pollster = g$pollster[1],
    net_approval = net_result,
    stringsAsFactors = FALSE
  )
}

overall_polls <- do.call(
  rbind,
  lapply(split(polls, polls$poll_id), make_overall_poll)
)

rownames(overall_polls) <- NULL
overall_polls <- overall_polls[!is.na(overall_polls$net_approval), ]
overall_polls$positive <- overall_polls$net_approval > 0

# ------------------------------------------------------------
# 3. Combine firms with fewer than 10 cleaned surveys into Other
# ------------------------------------------------------------

firm_counts <- table(overall_polls$pollster)
small_firms <- names(firm_counts[firm_counts < 10])

overall_polls$firm <- ifelse(
  overall_polls$pollster %in% small_firms,
  "Other",
  overall_polls$pollster
)

# ------------------------------------------------------------
# 4. Calculate BF = P(firm | positive) / P(firm | non-positive)
# ------------------------------------------------------------

total_positive <- sum(overall_polls$positive)
total_nonpositive <- sum(!overall_polls$positive)

result_list <- lapply(sort(unique(overall_polls$firm)), function(f) {
  firm_rows <- overall_polls[overall_polls$firm == f, ]
  positive <- sum(firm_rows$positive)
  nonpositive <- sum(!firm_rows$positive)
  
  p_firm_given_positive <- positive / total_positive
  p_firm_given_nonpositive <- nonpositive / total_nonpositive
  bayes_factor <- p_firm_given_positive / p_firm_given_nonpositive
  
  data.frame(
    firm = f,
    n_polls = nrow(firm_rows),
    positive = positive,
    nonpositive = nonpositive,
    bayes_factor = bayes_factor,
    stringsAsFactors = FALSE
  )
})

results <- do.call(rbind, result_list)

classify_bf <- function(bf) {
  if (bf > 100) return("Extreme for positive")
  if (bf >= 30) return("Very strong for positive")
  if (bf >= 10) return("Strong for positive")
  if (bf >= 3) return("Moderate for positive")
  if (bf > 1) return("Anecdotal")
  if (bf == 1) return("No evidence")
  if (bf >= 1 / 3) return("Anecdotal")
  if (bf >= 1 / 10) return("Moderate for non-positive")
  if (bf >= 1 / 30) return("Strong for non-positive")
  if (bf >= 1 / 100) return("Very strong for non-positive")
  "Extreme for non-positive"
}

results$evidence <- vapply(
  results$bayes_factor,
  classify_bf,
  character(1)
)

# Shorten only the longest labels so every firm remains identifiable.
results$firm_label <- results$firm
results$firm_label[results$firm_label ==
                     "Global Strategy Group/GBAO (Navigator Research)"] <-
  "GSG/GBAO (Navigator Research)"
results$firm_label[results$firm_label ==
                     "Hart Research Associates/Public Opinion Strategies"] <-
  "Hart Research/POS"

results$value_label <- paste0(
  formatC(results$bayes_factor, format = "f", digits = 2),
  "  (n=", results$n_polls, ")"
)

# Use restrained colors: orange for meaningful evidence of positive
# net approval, blue for meaningful evidence of non-positive net
# approval, and grey for anecdotal/no evidence.
results$direction <- ifelse(
  results$bayes_factor >= 3,
  "Positive net approval",
  ifelse(
    results$bayes_factor <= 1 / 3,
    "Non-positive net approval",
    "Only anecdotal/no evidence"
  )
)

results$direction <- factor(
  results$direction,
  levels = c(
    "Positive net approval",
    "Only anecdotal/no evidence",
    "Non-positive net approval"
  )
)

# ------------------------------------------------------------
# 5. Create two slide-ready graphs
#
# Graph 1 shows all nonzero Bayes factors on the log scale.
# Graph 2 separately documents every BF = 0 firm. Keeping the
# boundary cases separate gives every row enough space to read.
# ------------------------------------------------------------

bf_breaks <- c(1 / 100, 1 / 30, 1 / 10, 1 / 3, 1, 3, 10, 30)
bf_labels <- c("1/100", "1/30", "1/10", "1/3", "1", "3", "10", "30")

nonzero_results <- results[results$bayes_factor > 0, ]
nonzero_results <- nonzero_results[order(nonzero_results$bayes_factor), ]
nonzero_results$firm_label <- factor(
  nonzero_results$firm_label,
  levels = nonzero_results$firm_label
)

p_nonzero <- ggplot(
  nonzero_results,
  aes(x = bayes_factor, y = firm_label)
) +
  geom_vline(
    xintercept = c(1 / 10, 1 / 3, 1, 3, 10),
    color = c("#9CA3AF", "#D1D5DB", "#374151", "#D1D5DB", "#9CA3AF"),
    linewidth = c(0.4, 0.4, 0.8, 0.4, 0.4)
  ) +
  geom_segment(
    aes(x = 1, xend = bayes_factor, yend = firm_label, color = direction),
    linewidth = 0.75,
    alpha = 0.75
  ) +
  geom_point(
    aes(color = direction),
    size = 3.4
  ) +
  geom_text(
    aes(label = value_label, color = direction),
    hjust = ifelse(nonzero_results$bayes_factor < 1, 1.10, -0.10),
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_log10(
    breaks = bf_breaks,
    labels = bf_labels,
    limits = c(1 / 160, 48),
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  scale_color_manual(
    values = c(
      "Positive net approval" = "#D55E00",
      "Only anecdotal/no evidence" = "#6B7280",
      "Non-positive net approval" = "#0072B2"
    )
  ) +
  labs(
    title = "Only a Few Pollsters Provide More Than Anecdotal Evidence",
    subtitle = paste0(
      "InsiderAdvantage, RMG, and McLaughlin provide evidence toward positive net approval; ",
      "most other nonzero results are anecdotal."
    ),
    x = "Bayes factor on a logarithmic scale",
    y = NULL,
    color = NULL,
    caption = paste0(
      "BF = P(firm | positive net) / P(firm | non-positive net). Labels show BF and number of cleaned surveys.\n",
      "Issue-specific subgroups excluded; Strong and Weak combined when needed. ",
      "Source: Silver Bulletin Polling dataset."
    )
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 3)
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11.5,
      color = "#4B5563",
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#4B5563",
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(size = 10.5, color = "#1F2937"),
    axis.text.x = element_text(size = 10.5, color = "#374151"),
    axis.title.x = element_text(size = 11.5, margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 100, 12, 15)
  )

print(p_nonzero)

ggsave(
  "question2_bayes_factors_nonzero.png",
  plot = p_nonzero,
  width = 13.33,
  height = 7.5,
  dpi = 220,
  bg = "white"
)

# ------------------------------------------------------------
# 6. Create a separate graph for BF = 0 firms
# ------------------------------------------------------------

zero_results <- results[results$bayes_factor == 0, ]
zero_results <- zero_results[order(zero_results$n_polls), ]
zero_results$firm_label <- factor(
  zero_results$firm_label,
  levels = zero_results$firm_label
)

p_zero <- ggplot(
  zero_results,
  aes(x = n_polls, y = firm_label)
) +
  geom_segment(
    aes(x = 0, xend = n_polls, yend = firm_label),
    color = "#56B4E9",
    linewidth = 0.9
  ) +
  geom_point(
    color = "#0072B2",
    size = 3.8
  ) +
  geom_text(
    aes(label = paste0("n=", n_polls)),
    hjust = -0.25,
    color = "#0072B2",
    size = 3.8
  ) +
  scale_x_continuous(
    breaks = seq(0, max(zero_results$n_polls) + 5, by = 5),
    limits = c(0, max(zero_results$n_polls) + 9),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Twelve Firms Had No Positive-Net Polls in the Cleaned Data",
    subtitle = paste0(
      "Their BF = 0 values fall in the chart's extreme-evidence category, ",
      "but small samples make this boundary result uncertain."
    ),
    x = "Number of cleaned surveys, all with non-positive net approval",
    y = NULL,
    caption = paste0(
      "BF = 0 because each listed firm had zero positive-net polls. ",
      "These values should not be interpreted as proof that a positive result is impossible.\n",
      "Issue-specific subgroups excluded; Strong and Weak combined when needed. ",
      "Source: Silver Bulletin Polling dataset."
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 19,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 11.5,
      color = "#4B5563",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "#4B5563",
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 10)
    ),
    axis.text.y = element_text(size = 11, color = "#1F2937"),
    axis.text.x = element_text(size = 10.5, color = "#374151"),
    axis.title.x = element_text(size = 11.5, margin = margin(t = 8)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E5E7EB", linewidth = 0.4),
    plot.margin = margin(15, 55, 12, 15)
  )

print(p_zero)

ggsave(
  "question2_bayes_factors_zero.png",
  plot = p_zero,
  width = 13.33,
  height = 7.5,
  dpi = 220,
  bg = "white"
)

# ------------------------------------------------------------
# 7. Save and print the complete numerical results
# ------------------------------------------------------------
results_output <- results[
  order(-results$bayes_factor),
  c(
    "firm",
    "n_polls",
    "positive",
    "nonpositive",
    "bayes_factor",
    "evidence"
  )
]

results_output$bayes_factor <- round(results_output$bayes_factor, 3)

cat("\nComplete Bayes-factor results:\n")
print(results_output, row.names = FALSE)

write.csv(
  results_output,
  "question2_bayes_factor_results.csv",
  row.names = FALSE
)

cat("\nSaved graphs as:\n")
cat("  question2_bayes_factors_nonzero.png\n")
cat("  question2_bayes_factors_zero.png\n")