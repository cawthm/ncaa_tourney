# 03_within_seed_model.R - Model within-seed differentiation
#
# Identify metrics that predict tournament over/under-performance
# relative to seed expectations

library(tidyverse)
source("R/utils.R")

#' Load and merge team ratings with tournament outcomes
load_analysis_data <- function() {
  # Try raw ratings file first (from toRvik), then processed
  ratings_file <- "data/raw/team_ratings_with_seeds.csv"
  if (!file.exists(ratings_file)) {
    ratings_file <- "data/processed/team_ratings.csv"
  }

  if (!file.exists(ratings_file)) {
    stop("Team ratings not found. Run data acquisition first.")
  }

  ratings <- read_csv(ratings_file, show_col_types = FALSE)
  message(sprintf("Loaded %d team-seasons from %s", nrow(ratings), ratings_file))

  list(ratings = ratings, tournament = NULL)
}

#' Calculate z-scores for key metrics within each seed
#' This identifies teams that are "strong for their seed" or "weak for their seed"
calculate_seed_zscores <- function(ratings, seed_col = "seed") {
  # Key metrics from toRvik data:
  # - barthag: overall team quality (like win probability vs average team)
  # - adj_o: adjusted offensive efficiency
  # - adj_d: adjusted defensive efficiency (lower is better)
  # - adj_t: adjusted tempo
  # - wab: wins above bubble
  key_metrics <- c("barthag", "adj_o", "adj_d", "adj_t", "wab")

  # Keep only metrics that exist in the data
  available_metrics <- intersect(key_metrics, names(ratings))

  if (length(available_metrics) == 0) {
    warning("No key metrics found in ratings data")
    return(ratings)
  }

  message(sprintf("Calculating z-scores for: %s", paste(available_metrics, collapse = ", ")))

  # Calculate z-scores within each seed
  ratings_z <- ratings %>%
    filter(!is.na(.data[[seed_col]])) %>%
    group_by(.data[[seed_col]]) %>%
    mutate(across(
      all_of(available_metrics),
      list(z = ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)),
      .names = "{.col}_z"
    )) %>%
    ungroup()

  ratings_z
}

#' Build regression model: tournament wins ~ seed + metrics
#' Tests which metrics predict over/under-performance
build_performance_model <- function(data, outcome_col = "wins") {
  if (!outcome_col %in% names(data)) {
    warning(sprintf("Outcome column '%s' not found. Skipping model.", outcome_col))
    return(NULL)
  }

  # Get z-score columns
  z_cols <- names(data)[grepl("_z$", names(data))]

  if (length(z_cols) == 0) {
    warning("No z-score columns found. Run calculate_seed_zscores first.")
    return(NULL)
  }

  # Build formula
  formula_str <- paste(outcome_col, "~", paste(c("seed", z_cols), collapse = " + "))
  formula <- as.formula(formula_str)

  # Fit model
  model <- lm(formula, data = data)

  message("\nWithin-Seed Performance Model:")
  print(summary(model))

  model
}

#' Calculate team adjustment multiplier
#' Based on how much better/worse a team is than seed average
get_team_adjustment <- function(team_data, model = NULL) {
  # If we have a model, use it
  if (!is.null(model)) {
    # Predict expected wins based on model
    predicted <- predict(model, team_data)
    # Compare to seed baseline
    # Adjustment = predicted / seed_expected
    # This is simplified - in practice would be more nuanced
    return(predicted)
  }

  # Simple adjustment based on efficiency z-score
  # If no model, use adj_em_z or barthag_z as proxy
  z_cols <- names(team_data)[grepl("_z$", names(team_data))]

  if (length(z_cols) == 0) {
    return(rep(1, nrow(team_data)))
  }

  # Use first available z-score metric
  z_col <- z_cols[1]
  z_scores <- team_data[[z_col]]

  # Convert z-score to multiplier
  # z = 0 -> multiplier = 1.0 (average for seed)
  # z = 1 -> multiplier = 1.15 (15% better than seed average)
  # z = -1 -> multiplier = 0.85 (15% worse than seed average)
  multiplier <- 1 + (0.15 * z_scores)

  # Bound between reasonable limits
  pmin(pmax(multiplier, 0.7), 1.5)
}

#' Analyze which metrics are most predictive within seeds
analyze_metric_importance <- function(data, outcome_col = "wins") {
  if (!outcome_col %in% names(data)) {
    return(NULL)
  }

  # Get z-score columns
  z_cols <- names(data)[grepl("_z$", names(data))]

  # Calculate correlation of each metric with outcome, controlling for seed
  correlations <- map_dfr(z_cols, function(col) {
    if (all(is.na(data[[col]]))) return(NULL)

    # Partial correlation controlling for seed
    cor_val <- cor(data[[col]], data[[outcome_col]], use = "pairwise.complete.obs")

    tibble(
      metric = gsub("_z$", "", col),
      correlation = cor_val,
      abs_correlation = abs(cor_val)
    )
  }) %>%
    arrange(desc(abs_correlation))

  message("\nMetric Importance (correlation with tournament success):")
  print(correlations)

  correlations
}

#' Main within-seed analysis
analyze_within_seed <- function() {
  message("Starting within-seed analysis...")

  data <- load_analysis_data()

  if (is.null(data$ratings)) {
    stop("No ratings data available")
  }

  # Calculate z-scores
  # First need to identify which column has seed info
  seed_cols <- c("seed", "ncaa_seed", "tournament_seed")
  seed_col <- intersect(seed_cols, names(data$ratings))[1]

  if (is.na(seed_col)) {
    message("No seed column found in ratings. Attempting to merge with tournament data.")
    # Would need tournament data to get seeds
    return(NULL)
  }

  ratings_z <- calculate_seed_zscores(data$ratings, seed_col)

  # Save z-scores
  save_data(ratings_z, "team_ratings_zscores.csv")

  # If we have tournament outcomes, build model
  if (!is.null(data$tournament)) {
    # Merge tournament outcomes
    # ... (depends on data structure)
  }

  ratings_z
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  results <- analyze_within_seed()
}
