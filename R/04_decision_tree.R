# ============================================================
# Question 4: Decision tree predicting positive net approval
# ============================================================

# Run these only if the packages are not installed:
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# ------------------------------------------------------------
# 1. Read the data
# ------------------------------------------------------------

polls <- read.csv(
  "Silver_Bulletin_Polling.csv",
  stringsAsFactors = FALSE
)

# Identify rows belonging to the same underlying survey
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
# 2. Clean the subgroups
# ------------------------------------------------------------

# Question 4 retains issue-specific subgroups
issue_subgroups <- c(
  "Economy",
  "Immigration",
  "Trade",
  "Cost"
)

make_tree_rows <- function(g) {
  
  output <- data.frame(
    pollster = character(),
    subgroup_clean = character(),
    enddate = character(),
    net_clean = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Retain all available issue-specific approval results
  for (current_subgroup in issue_subgroups) {
    
    issue_row <- g[
      g$subgroup == current_subgroup,
    ]
    
    if (nrow(issue_row) > 0) {
      
      issue_row <- issue_row[1, ]
      
      output <- rbind(
        output,
        data.frame(
          pollster = issue_row$pollster,
          subgroup_clean = current_subgroup,
          enddate = issue_row$enddate,
          net_clean =
            issue_row$approve -
            issue_row$disapprove,
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  # Find possible overall-approval rows
  all_row <- g[g$subgroup == "All polls", ]
  strong_row <- g[g$subgroup == "Strong", ]
  weak_row <- g[g$subgroup == "Weak", ]
  adults_row <- g[g$subgroup == "Adults", ]
  voters_row <- g[g$subgroup == "Voters", ]
  
  overall_net <- NA_real_
  overall_date <- NA_character_
  overall_pollster <- NA_character_
  
  # Prefer the reported overall result
  if (nrow(all_row) > 0) {
    
    all_row <- all_row[1, ]
    
    overall_net <-
      all_row$approve -
      all_row$disapprove
    
    overall_date <- all_row$enddate
    overall_pollster <- all_row$pollster
    
    # Otherwise combine Strong and Weak
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
    
    overall_net <-
      total_approve -
      total_disapprove
    
    overall_date <- strong_row$enddate
    overall_pollster <- strong_row$pollster
    
    # Use Adults as a fallback
  } else if (nrow(adults_row) > 0) {
    
    adults_row <- adults_row[1, ]
    
    overall_net <-
      adults_row$approve -
      adults_row$disapprove
    
    overall_date <- adults_row$enddate
    overall_pollster <- adults_row$pollster
    
    # Use Voters as the final fallback
  } else if (nrow(voters_row) > 0) {
    
    voters_row <- voters_row[1, ]
    
    overall_net <-
      voters_row$approve -
      voters_row$disapprove
    
    overall_date <- voters_row$enddate
    overall_pollster <- voters_row$pollster
  }
  
  # Add one overall result for the survey
  if (!is.na(overall_net)) {
    
    output <- rbind(
      output,
      data.frame(
        pollster = overall_pollster,
        subgroup_clean = "Overall approval",
        enddate = overall_date,
        net_clean = overall_net,
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(output)
}

# Apply the cleaning function to each survey
poll_list <- split(
  polls,
  polls$poll_id
)

tree_rows <- lapply(
  poll_list,
  make_tree_rows
)

tree_data <- do.call(
  rbind,
  tree_rows
)

tree_data <- tree_data[
  !is.na(tree_data$net_clean),
]

rownames(tree_data) <- NULL

# ------------------------------------------------------------
# 3. Convert the date correctly
# ------------------------------------------------------------

tree_data$poll_date <- as.Date(
  tree_data$enddate,
  format = "%m/%d/%Y"
)

tree_data <- tree_data[
  !is.na(tree_data$poll_date),
]

cat("\nDate datatype:\n")
print(class(tree_data$poll_date))

cat("\nDate range:\n")
print(
  range(tree_data$poll_date)
)

# Convert valid dates to decimal years for readable splits
tree_data$year <- as.integer(
  format(
    tree_data$poll_date,
    "%Y"
  )
)

tree_data$day_of_year <- as.integer(
  format(
    tree_data$poll_date,
    "%j"
  )
)

tree_data$date_decimal <-
  tree_data$year +
  (tree_data$day_of_year - 1) / 365.25

# ------------------------------------------------------------
# 4. Create the outcome
# ------------------------------------------------------------

tree_data$positive_net <- factor(
  ifelse(
    tree_data$net_clean > 0,
    "Positive",
    "Non-positive"
  ),
  levels = c(
    "Non-positive",
    "Positive"
  )
)

tree_data$subgroup_clean <- factor(
  tree_data$subgroup_clean
)

cat("\nCleaned subgroup counts:\n")
print(
  table(tree_data$subgroup_clean)
)

cat("\nOutcome counts:\n")
print(
  table(tree_data$positive_net)
)

# ------------------------------------------------------------
# 5. Create readable pollster groups
# ------------------------------------------------------------

firm_counts <- table(
  tree_data$pollster
)

major_pollsters <- sort(
  names(
    firm_counts[
      firm_counts >= 50
    ]
  )
)

major_codes <- sprintf(
  "P%02d",
  seq_along(major_pollsters)
)

code_map <- setNames(
  major_codes,
  major_pollsters
)

tree_data$pollster_group <- "P00"

is_major <- tree_data$pollster %in%
  major_pollsters

tree_data$pollster_group[is_major] <-
  unname(
    code_map[
      tree_data$pollster[is_major]
    ]
  )

tree_data$pollster_group <- factor(
  tree_data$pollster_group
)

# Pollster key for the slide
pollster_key <- data.frame(
  code = c(
    "P00",
    major_codes
  ),
  meaning = c(
    paste(
      "All pollsters with fewer than",
      "50 cleaned observations"
    ),
    major_pollsters
  ),
  stringsAsFactors = FALSE
)

cat("\nPollster key:\n")
print(
  pollster_key,
  row.names = FALSE
)

write.csv(
  pollster_key,
  "question4_pollster_key.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. Fit the decision tree
# ------------------------------------------------------------

set.seed(101)

tree_model <- rpart(
  positive_net ~
    pollster_group +
    subgroup_clean +
    date_decimal,
  data = tree_data,
  method = "class",
  parms = list(
    split = "gini"
  ),
  control = rpart.control(
    cp = 0.005,
    minsplit = 80,
    minbucket = 30,
    maxdepth = 3,
    xval = 10
  )
)

cat("\nDecision-tree summary:\n")
print(tree_model)

cat("\nComplexity table:\n")
printcp(tree_model)

# ------------------------------------------------------------
# 7. Calculate variable importance
# ------------------------------------------------------------

importance <- tree_model$variable.importance

importance_percent <- round(
  100 * importance /
    sum(importance),
  1
)

importance_table <- data.frame(
  factor = names(importance_percent),
  importance_percent =
    as.numeric(importance_percent),
  stringsAsFactors = FALSE
)

importance_table <- importance_table[
  order(
    importance_table$importance_percent,
    decreasing = TRUE
  ),
]

cat("\nVariable importance:\n")
print(
  importance_table,
  row.names = FALSE
)

write.csv(
  importance_table,
  "question4_variable_importance.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Define the graph function
# ------------------------------------------------------------
draw_tree <- function() {
  
  # Inner margins hold the tree.
  # Outer bottom margin holds the notes without overlap.
  par(
    mar = c(1, 1, 4, 1),
    oma = c(4.5, 0, 0, 0),
    xpd = NA
  )
  
  rpart.plot(
    tree_model,
    type = 2,
    extra = 104,
    fallen.leaves = TRUE,
    faclen = 0,
    varlen = 0,
    roundint = FALSE,
    digits = 5,
    tweak = 1.02,
    box.palette = c(
      "#D55E00",
      "#0072B2"
    ),
    shadow.col = "grey85",
    branch.col = "grey45",
    branch.lwd = 1.3,
    yesno = 2,
    under = TRUE,
    main = paste(
      "Poll Date Was the Strongest Predictor",
      "of Positive Net Approval"
    )
  )
  
  mtext(
    paste(
      "Positive means approval exceeds disapproval.",
      "The 2025.2 split is approximately mid-March 2025."
    ),
    side = 1,
    outer = TRUE,
    line = 2.3,
    cex = 0.72,
    col = "grey30"
  )
  
  mtext(
    "Source: Silver Bulletin Polling dataset.",
    side = 1,
    outer = TRUE,
    line = 0.9,
    cex = 0.68,
    col = "grey40"
  )
}

# ------------------------------------------------------------
# 9. Display the graph in RStudio
# ------------------------------------------------------------

draw_tree()

# ------------------------------------------------------------
# 10. Save the graph as a PNG
# ------------------------------------------------------------

output_file <- file.path(
  getwd(),
  "question4_decision_tree.png"
)

grDevices::png(
  filename = output_file,
  width = 2800,
  height = 1800,
  res = 240,
  bg = "white"
)

draw_tree()

grDevices::dev.off()

cat(
  "\nGraph save location:\n",
  normalizePath(
    output_file,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

cat(
  "File created:",
  file.exists(output_file),
  "\n"
)

# Display it again after closing the PNG device
draw_tree()