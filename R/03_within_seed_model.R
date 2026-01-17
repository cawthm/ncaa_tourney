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

#' Empirically validated EV adjustment rates by seed
#' From cross-validation analysis in 07_validate_intra_seed.R
#' Each σ of barthag adds ~0.58 tournament wins on average
#' The EV impact per σ varies by seed due to different round values
SEED_EV_ADJUSTMENT_PER_SIGMA <- c(
  `1` = 0.20,   # 1-seeds: ~20% EV change per σ
  `2` = 0.31,   # 2-seeds: ~31% EV change per σ
  `3` = 0.41,   # 3-seeds: ~41% EV change per σ
  `4` = 0.45,   # 4-seeds: ~45% EV change per σ
  `5` = 0.62,   # 5-seeds: ~62% EV change per σ
  `6` = 0.63,   # 6-seeds: ~63% EV change per σ
  `7` = 0.72,   # 7-seeds: ~72% EV change per σ
  `8` = 0.87,   # 8-seeds: ~87% EV change per σ
  `9` = 0.96,   # 9-seeds: ~96% EV change per σ
  `10` = 0.91,  # 10-seeds: ~91% EV change per σ
  `11` = 0.79,  # 11-seeds: ~79% EV change per σ
  `12` = 1.27,  # 12-seeds: ~127% EV change per σ (volatile)
  `13` = 2.78,  # 13-seeds: high % but tiny base EV
  `14` = 3.07,  # 14-seeds: high % but tiny base EV
  `15` = 7.58,  # 15-seeds: extreme % but ~0% base EV
  `16` = 52.26  # 16-seeds: extreme % but near-0 base EV
)

#' Calculate team adjustment multiplier
#' Based on how much better/worse a team is than seed average
#' Uses empirically validated adjustments from cross-validation
#' @param team_data Data frame with seed and z-score columns
#' @param model Optional fitted model (for predicted wins approach)
#' @param use_seed_specific If TRUE, use seed-specific adjustment rates
get_team_adjustment <- function(team_data, model = NULL, use_seed_specific = TRUE) {
  # If we have a model, use it
  if (!is.null(model)) {
    # Predict expected wins based on model
    predicted <- predict(model, team_data)
    # Compare to seed baseline
    return(predicted)
  }

  # Get z-score column (prefer barthag_z as most predictive)
  z_cols <- names(team_data)[grepl("_z$", names(team_data))]

  if (length(z_cols) == 0) {
    return(rep(1, nrow(team_data)))
  }

  # Use barthag_z if available, otherwise first z-score
  z_col <- if ("barthag_z" %in% z_cols) "barthag_z" else z_cols[1]
  z_scores <- team_data[[z_col]]

  if (use_seed_specific && "seed" %in% names(team_data)) {
    # Use seed-specific adjustment rates (capped at reasonable levels)
    # Cap adjustment rates at 100% to avoid extreme multipliers for low seeds
    seeds <- as.character(team_data$seed)
    adj_rates <- pmin(SEED_EV_ADJUSTMENT_PER_SIGMA[seeds], 1.0)
    adj_rates[is.na(adj_rates)] <- 0.20  # Default to 20% if seed not found

    # Convert z-score to multiplier using seed-specific rates
    # z = 0 -> multiplier = 1.0 (average for seed)
    # z = 1 -> multiplier = 1 + adj_rate (better than seed average)
    multiplier <- 1 + (adj_rates * z_scores)
  } else {
    # Fallback: use flat 20% adjustment (conservative estimate)
    # This is based on empirical finding of ~0.58 wins per σ
    multiplier <- 1 + (0.20 * z_scores)
  }

  # Bound between reasonable limits
  pmin(pmax(multiplier, 0.5), 2.0)
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
