# ============================================================
# Question 3
# RealClear Opinion Research vs. The Tyson Group
# ============================================================

library(ggplot2)

# Read data
polls <- read.csv(
  "Silver_Bulletin_Polling.csv",
  stringsAsFactors = FALSE
)

# Select the two firms and retain every subgroup
firms <- c(
  "RealClear Opinion Research",
  "The Tyson Group"
)

q3 <- polls[
  polls$pollster %in% firms &
    !is.na(polls$net),
]

# Helper function: calculate n, mean, and median
summarize_groups <- function(data, grouping_variables) {
  
  formula_text <- paste(
    "net ~",
    paste(grouping_variables, collapse = " + ")
  )
  
  group_formula <- as.formula(formula_text)
  
  group_means <- aggregate(
    group_formula,
    data = data,
    FUN = mean
  )
  
  group_medians <- aggregate(
    group_formula,
    data = data,
    FUN = median
  )
  
  group_counts <- aggregate(
    group_formula,
    data = data,
    FUN = length
  )
  
  names(group_means)[names(group_means) == "net"] <-
    "mean_net"
  
  names(group_medians)[names(group_medians) == "net"] <-
    "median_net"
  
  names(group_counts)[names(group_counts) == "net"] <-
    "n"
  
  result <- merge(
    group_means,
    group_medians,
    by = grouping_variables
  )
  
  result <- merge(
    result,
    group_counts,
    by = grouping_variables
  )
  
  result$mean_net <- round(result$mean_net, 2)
  result$median_net <- round(result$median_net, 2)
  
  result
}

# ------------------------------------------------------------
# 1. Analyze every subgroup separately
# ------------------------------------------------------------

all_subgroup_results <- summarize_groups(
  q3,
  c("pollster", "subgroup")
)

all_subgroup_results <- all_subgroup_results[
  order(
    all_subgroup_results$subgroup,
    all_subgroup_results$pollster
  ),
]

cat("\nResults for every subgroup:\n")
print(all_subgroup_results, row.names = FALSE)

# ------------------------------------------------------------
# 2. Identify subgroups shared by both firms
# ------------------------------------------------------------

realclear_subgroups <- unique(
  q3$subgroup[
    q3$pollster == "RealClear Opinion Research"
  ]
)

tyson_subgroups <- unique(
  q3$subgroup[
    q3$pollster == "The Tyson Group"
  ]
)

shared_subgroups <- intersect(
  realclear_subgroups,
  tyson_subgroups
)

cat("\nShared subgroups:\n")
print(shared_subgroups)

shared <- q3[
  q3$subgroup %in% shared_subgroups,
]

# Analyze shared subgroups separately
shared_separate <- summarize_groups(
  shared,
  c("pollster", "subgroup")
)

shared_separate <- shared_separate[
  order(
    shared_separate$subgroup,
    shared_separate$pollster
  ),
]

cat("\nShared subgroups analyzed separately:\n")
print(shared_separate, row.names = FALSE)

# ------------------------------------------------------------
# 3. Pool all observations from shared subgroups
# ------------------------------------------------------------

shared_pooled <- summarize_groups(
  shared,
  "pollster"
)

cat("\nShared subgroups pooled:\n")
print(shared_pooled, row.names = FALSE)

# ------------------------------------------------------------
# 4. Completely ignore subgroup
# ------------------------------------------------------------

all_pooled <- summarize_groups(
  q3,
  "pollster"
)

cat("\nAll subgroups pooled:\n")
print(all_pooled, row.names = FALSE)

# ------------------------------------------------------------
# 5. Prepare graph data
# ------------------------------------------------------------

graph_shared <- shared_separate[
  c("pollster", "subgroup", "mean_net", "n")
]

graph_shared$comparison <- graph_shared$subgroup

graph_shared <- graph_shared[
  c("pollster", "comparison", "mean_net", "n")
]

graph_shared_pooled <- shared_pooled[
  c("pollster", "mean_net", "n")
]

graph_shared_pooled$comparison <-
  "Shared subgroups pooled"

graph_shared_pooled <- graph_shared_pooled[
  c("pollster", "comparison", "mean_net", "n")
]

graph_all_pooled <- all_pooled[
  c("pollster", "mean_net", "n")
]

graph_all_pooled$comparison <-
  "All subgroups pooled"

graph_all_pooled <- graph_all_pooled[
  c("pollster", "comparison", "mean_net", "n")
]

graph_data <- rbind(
  graph_shared,
  graph_shared_pooled,
  graph_all_pooled
)

# Shorter names for direct labels
graph_data$firm <- ifelse(
  graph_data$pollster ==
    "RealClear Opinion Research",
  "RealClear",
  "Tyson"
)

# Logical top-to-bottom order
graph_data$comparison <- factor(
  graph_data$comparison,
  levels = rev(c(
    "All polls",
    "Immigration",
    "Shared subgroups pooled",
    "All subgroups pooled"
  ))
)

# Create wide data for connecting lines
graph_wide <- reshape(
  graph_data[
    c("comparison", "firm", "mean_net")
  ],
  idvar = "comparison",
  timevar = "firm",
  direction = "wide"
)

realclear_labels <- graph_data[
  graph_data$firm == "RealClear",
]

tyson_labels <- graph_data[
  graph_data$firm == "Tyson",
]

# ------------------------------------------------------------
# 6. Create graph
# ------------------------------------------------------------

q3_plot <- ggplot() +
  
  # Zero represents equal approval and disapproval
  geom_vline(
    xintercept = 0,
    color = "grey70",
    linewidth = 0.7
  ) +
  
  # Connect the two firms within each comparison
  geom_segment(
    data = graph_wide,
    aes(
      x = mean_net.RealClear,
      xend = mean_net.Tyson,
      y = comparison,
      yend = comparison
    ),
    color = "grey65",
    linewidth = 1
  ) +
  
  # Plot firm means
  geom_point(
    data = graph_data,
    aes(
      x = mean_net,
      y = comparison,
      color = firm,
      shape = firm
    ),
    size = 4
  ) +
  
  # Directly label RealClear
  geom_text(
    data = realclear_labels,
    aes(
      x = mean_net,
      y = comparison,
      label = paste0(
        "RealClear ",
        sprintf("%.2f", mean_net),
        " (n=", n, ")"
      )
    ),
    nudge_y = 0.17,
    color = "#0072B2",
    size = 3.8
  ) +
  
  # Directly label Tyson
  geom_text(
    data = tyson_labels,
    aes(
      x = mean_net,
      y = comparison,
      label = paste0(
        "Tyson ",
        sprintf("%.2f", mean_net),
        " (n=", n, ")"
      )
    ),
    nudge_y = -0.17,
    color = "#D55E00",
    size = 3.8
  ) +
  
  # Color-blind-friendly colors
  scale_color_manual(
    values = c(
      "RealClear" = "#0072B2",
      "Tyson" = "#D55E00"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "RealClear" = 16,
      "Tyson" = 17
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(-6, 6, 2),
    expand = expansion(
      mult = c(0.08, 0.08)
    )
  ) +
  
  coord_cartesian(
    xlim = c(-7.5, 6.5),
    clip = "off"
  ) +
  
  labs(
    title = paste(
      "Pooling Shared Subgroups Reverses",
      "Which Firm Appears More Pro-Trump"
    ),
    subtitle = paste(
      "RealClear has higher mean net approval within both shared",
      "subgroups, but Tyson leads when they are pooled."
    ),
    x = "Mean net approval (percentage points)",
    y = NULL,
    caption = paste(
      "Net approval = approve minus disapprove.",
      "Labels show the number of observations.",
      "Source: Silver Bulletin Polling dataset."
    )
  ) +
  
  # Direct labels make a separate legend unnecessary
  guides(
    color = "none",
    shape = "none"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey30",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(
      size = 9,
      color = "grey35",
      hjust = 0,
      margin = margin(t = 12)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(
      margin = margin(t = 8)
    ),
    plot.margin = margin(
      12, 35, 12, 12
    )
  )

# Display graph
print(q3_plot)

# Save a high-resolution copy
ggsave(
  filename = "question3_simpsons_paradox.png",
  plot = q3_plot,
  width = 11,
  height = 6.5,
  dpi = 300,
  bg = "white"
)