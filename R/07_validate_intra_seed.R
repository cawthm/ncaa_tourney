# 07_validate_intra_seed.R - Validate intra-seed adjustment model
#
# Performs leave-one-year-out cross-validation to validate the within-seed
# adjustment model and determine optimal parameters.

library(tidyverse)
source("R/utils.R")

#' Load and merge team ratings with tournament wins
#' Matches teams to wins within each seed-year group by ranking
load_merged_data <- function() {
  # Load team ratings with seeds
  ratings <- read_csv("data/raw/team_ratings_with_seeds.csv", show_col_types = FALSE)

  # Load tournament wins by seed/year
  seed_wins <- read_csv("data/raw/seed_wins_by_year.csv", show_col_types = FALSE)

  message(sprintf("Loaded %d team ratings, %d tournament outcomes", nrow(ratings), nrow(seed_wins)))

  # For each year-seed combination, we need to match:
  # - 4 teams (from ratings, sorted by barthag desc)
  # - 4 win values (from seed_wins, sorted by wins desc)
  # This assumes higher barthag teams are more likely to win more games

  # Rank teams within each seed-year by barthag
  ratings_ranked <- ratings %>%
    group_by(year, seed) %>%
    arrange(year, seed, desc(barthag)) %>%
    mutate(rank_in_seed = row_number()) %>%
    ungroup()

  # Rank wins within each seed-year (descending)
  wins_ranked <- seed_wins %>%
    group_by(year, seed) %>%
    arrange(year, seed, desc(wins)) %>%
    mutate(rank_in_seed = row_number()) %>%
    ungroup()

  # Merge by year, seed, and rank
  merged <- ratings_ranked %>%
    left_join(wins_ranked %>% select(year, seed, rank_in_seed, wins),
              by = c("year", "seed", "rank_in_seed"))

  # Calculate z-scores within each seed (across all years)
  key_metrics <- c("barthag", "adj_o", "adj_d", "adj_t", "wab")

  merged_z <- merged %>%
    group_by(seed) %>%
    mutate(across(
      all_of(key_metrics),
      list(z = ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)),
      .names = "{.col}_z"
    )) %>%
    ungroup()

  # Also calculate z-scores within seed-year for year-specific analysis
  merged_z <- merged_z %>%
    group_by(year, seed) %>%
    mutate(across(
      all_of(key_metrics),
      list(z_yr = ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)),
      .names = "{.col}_z_yr"
    )) %>%
    ungroup()

  # Filter to complete cases (exclude years without tournament like 2020)
  merged_z <- merged_z %>%
    filter(!is.na(wins))

  message(sprintf("Merged dataset: %d teams with ratings and wins", nrow(merged_z)))

  merged_z
}

#' Build performance model: wins ~ seed + z-scores
#' @param data Training data
#' @param metrics Which z-score metrics to include
#' @return Fitted model
build_model <- function(data, metrics = c("barthag_z")) {
  # Ensure metrics exist
  available <- intersect(metrics, names(data))
  if (length(available) == 0) {
    stop("No valid metrics found")
  }

  # Build formula
  formula_str <- paste("wins ~ seed +", paste(available, collapse = " + "))
  formula <- as.formula(formula_str)

  # Fit model
  lm(formula, data = data)
}

#' Leave-one-year-out cross-validation
#' @param data Full dataset
#' @param metrics Z-score metrics to include in model
#' @return Data frame with predictions and errors for each year
loyo_cv <- function(data, metrics = c("barthag_z")) {
  years <- unique(data$year)

  results <- map_dfr(years, function(test_year) {
    # Split data
    train <- data %>% filter(year != test_year)
    test <- data %>% filter(year == test_year)

    if (nrow(train) == 0 || nrow(test) == 0) {
      return(NULL)
    }

    # Train model
    model <- build_model(train, metrics)

    # Predict on test
    test$predicted <- predict(model, test)

    # Calculate errors
    test %>%
      mutate(
        error = wins - predicted,
        abs_error = abs(error),
        sq_error = error^2,
        test_year = test_year
      ) %>%
      select(year, team, seed, wins, predicted, error, abs_error, sq_error,
             all_of(metrics))
  })

  results
}

#' Calculate aggregate CV metrics by seed tier
#' @param cv_results Output from loyo_cv
#' @return Summary statistics
summarize_cv <- function(cv_results) {
  # Create seed tiers
  cv_results <- cv_results %>%
    mutate(seed_tier = case_when(
      seed <= 4 ~ "Top 4",
      seed <= 8 ~ "5-8",
      seed <= 12 ~ "9-12",
      TRUE ~ "13-16"
    ))

  # Overall metrics
  overall <- cv_results %>%
    summarize(
      n = n(),
      mae = mean(abs_error),
      rmse = sqrt(mean(sq_error)),
      r_squared = 1 - sum(sq_error) / sum((wins - mean(wins))^2),
      .groups = "drop"
    )

  # By seed tier
  by_tier <- cv_results %>%
    group_by(seed_tier) %>%
    summarize(
      n = n(),
      mae = mean(abs_error),
      rmse = sqrt(mean(sq_error)),
      avg_actual_wins = mean(wins),
      avg_predicted_wins = mean(predicted),
      .groups = "drop"
    )

  # By seed
  by_seed <- cv_results %>%
    group_by(seed) %>%
    summarize(
      n = n(),
      mae = mean(abs_error),
      rmse = sqrt(mean(sq_error)),
      avg_actual_wins = mean(wins),
      avg_predicted_wins = mean(predicted),
      .groups = "drop"
    )

  list(overall = overall, by_tier = by_tier, by_seed = by_seed)
}

#' Analyze which metrics are most predictive
#' @param data Full dataset
#' @return Data frame with correlation and regression coefficients
analyze_metrics <- function(data) {
  metrics <- c("barthag_z", "adj_o_z", "adj_d_z", "adj_t_z", "wab_z")
  metrics <- intersect(metrics, names(data))

  # Correlations with wins
  correlations <- map_dfr(metrics, function(m) {
    cor_val <- cor(data[[m]], data$wins, use = "pairwise.complete.obs")
    tibble(
      metric = gsub("_z$", "", m),
      correlation = cor_val,
      abs_correlation = abs(cor_val)
    )
  }) %>%
    arrange(desc(abs_correlation))

  # Regression with all metrics
  formula_str <- paste("wins ~ seed +", paste(metrics, collapse = " + "))
  full_model <- lm(as.formula(formula_str), data = data)

  # Extract coefficients
  coefs <- summary(full_model)$coefficients
  coef_df <- tibble(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    t_value = coefs[, "t value"],
    p_value = coefs[, "Pr(>|t|)"]
  ) %>%
    filter(term != "(Intercept)")

  list(
    correlations = correlations,
    full_model_summary = summary(full_model),
    coefficients = coef_df
  )
}

#' Calibrate the adjustment multiplier
#' Test if +1σ teams actually win 15% more (as currently assumed)
#' @param data Full dataset
#' @return Calibration analysis
calibrate_adjustment <- function(data) {
  # Group teams by z-score buckets
  data <- data %>%
    mutate(
      barthag_bucket = cut(barthag_z,
        breaks = c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf),
        labels = c("Very Weak (<-1.5σ)", "Weak (-1.5 to -0.5σ)",
                   "Average (-0.5 to 0.5σ)", "Strong (0.5 to 1.5σ)",
                   "Very Strong (>1.5σ)")
      )
    )

  # Calculate expected wins by seed (baseline)
  seed_baseline <- data %>%
    group_by(seed) %>%
    summarize(
      baseline_wins = mean(wins),
      n = n(),
      .groups = "drop"
    )

  # Calculate actual performance vs baseline by z-score bucket
  calibration <- data %>%
    left_join(seed_baseline, by = "seed") %>%
    mutate(wins_vs_expected = wins - baseline_wins) %>%
    group_by(barthag_bucket, seed) %>%
    summarize(
      n = n(),
      avg_wins = mean(wins),
      baseline_wins = first(baseline_wins),
      wins_vs_expected = mean(wins_vs_expected),
      avg_barthag_z = mean(barthag_z),
      .groups = "drop"
    )

  # Overall calibration by z-score bucket
  overall_cal <- data %>%
    left_join(seed_baseline, by = "seed") %>%
    mutate(wins_vs_expected = wins - baseline_wins) %>%
    group_by(barthag_bucket) %>%
    summarize(
      n = n(),
      avg_wins = mean(wins),
      wins_vs_expected = mean(wins_vs_expected),
      avg_barthag_z = mean(barthag_z),
      .groups = "drop"
    )

  # Calculate empirical adjustment per σ
  # Linear regression: wins_vs_expected ~ barthag_z
  adjustment_model <- data %>%
    left_join(seed_baseline, by = "seed") %>%
    mutate(wins_vs_expected = wins - baseline_wins) %>%
    lm(wins_vs_expected ~ barthag_z, data = .)

  empirical_adjustment <- coef(adjustment_model)["barthag_z"]

  list(
    calibration_by_bucket = overall_cal,
    calibration_by_seed_bucket = calibration,
    empirical_wins_per_sigma = empirical_adjustment,
    adjustment_model = adjustment_model
  )
}

#' Calculate incremental EV by seed and z-score
#' @param data Full dataset
#' @return Table showing EV impact of being +1σ or -1σ
calculate_incremental_ev <- function(data) {
  # Get calibration results
  cal <- calibrate_adjustment(data)
  wins_per_sigma <- cal$empirical_wins_per_sigma

  # Load seed probabilities (from earlier analysis)
  seed_probs <- read_csv("output/seed_progression_probs.csv", show_col_types = FALSE)

  # Base EV by seed
  base_ev <- seed_probs %>%
    select(seed, ev_pct)

  # Calculate expected wins by seed
  seed_wins <- data %>%
    group_by(seed) %>%
    summarize(
      avg_wins = mean(wins),
      sd_wins = sd(wins),
      .groups = "drop"
    )

  # Calculate EV adjustment
  # For each additional win, calculate expected additional EV
  # This is a simplification - in reality depends on which rounds

  # EV per win by seed (rough approximation based on payouts)
  # R64 win = reach R32 = 1.5% payout for S16 probability increase
  # Approximate incremental EV per win
  ev_per_win <- tribble(
    ~seed, ~ev_per_win,
    1, 3.0,   # 1-seeds: each win is ~3% EV (high championship odds)
    2, 2.5,
    3, 2.0,
    4, 1.8,
    5, 1.5,
    6, 1.3,
    7, 1.2,
    8, 1.0,
    9, 1.0,
    10, 0.8,
    11, 0.8,
    12, 0.7,
    13, 0.5,
    14, 0.4,
    15, 0.3,
    16, 0.2
  )

  # Calculate +1σ and -1σ EV adjustments
  incremental_ev <- base_ev %>%
    left_join(seed_wins, by = "seed") %>%
    left_join(ev_per_win, by = "seed") %>%
    mutate(
      # Additional wins for +1σ team
      delta_wins_plus_1_sigma = wins_per_sigma,
      delta_wins_minus_1_sigma = -wins_per_sigma,

      # EV adjustment
      ev_plus_1_sigma = ev_pct + (delta_wins_plus_1_sigma * ev_per_win),
      ev_minus_1_sigma = ev_pct + (delta_wins_minus_1_sigma * ev_per_win),

      # Delta per sigma
      ev_delta_per_sigma = delta_wins_plus_1_sigma * ev_per_win,
      pct_change_per_sigma = (ev_delta_per_sigma / ev_pct) * 100
    ) %>%
    select(
      seed,
      base_ev = ev_pct,
      ev_plus_1_sigma,
      ev_minus_1_sigma,
      ev_delta_per_sigma,
      pct_change_per_sigma,
      avg_wins,
      wins_per_sigma = delta_wins_plus_1_sigma
    )

  list(
    incremental_ev = incremental_ev,
    wins_per_sigma = wins_per_sigma
  )
}

#' Compare different model specifications
#' @param data Full dataset
#' @return Comparison of models with different metric combinations
compare_models <- function(data) {
  # Model specifications to compare
  model_specs <- list(
    "Seed only" = c(),
    "Seed + barthag" = c("barthag_z"),
    "Seed + barthag + adj_o" = c("barthag_z", "adj_o_z"),
    "Seed + barthag + adj_d" = c("barthag_z", "adj_d_z"),
    "Seed + barthag + wab" = c("barthag_z", "wab_z"),
    "Seed + all metrics" = c("barthag_z", "adj_o_z", "adj_d_z", "adj_t_z", "wab_z")
  )

  # Run CV for each specification
  results <- map_dfr(names(model_specs), function(spec_name) {
    metrics <- model_specs[[spec_name]]

    if (length(metrics) == 0) {
      # Seed-only model
      cv_res <- data %>%
        mutate(predicted = predict(lm(wins ~ seed, data = data), data)) %>%
        mutate(
          error = wins - predicted,
          abs_error = abs(error),
          sq_error = error^2
        )
    } else {
      cv_res <- loyo_cv(data, metrics)
    }

    tibble(
      model = spec_name,
      mae = mean(cv_res$abs_error),
      rmse = sqrt(mean(cv_res$sq_error)),
      r_squared = 1 - sum(cv_res$sq_error) / sum((cv_res$wins - mean(cv_res$wins))^2)
    )
  })

  results %>% arrange(mae)
}

#' Main validation function
run_validation <- function() {
  message("=" %>% rep(60) %>% paste(collapse = ""))
  message("NCAA Tournament Intra-Seed Validation Analysis")
  message("=" %>% rep(60) %>% paste(collapse = ""))

  # Load data
  data <- load_merged_data()

  message("\n--- Data Summary ---")
  message(sprintf("Years: %d to %d (excl. 2020)", min(data$year), max(data$year)))
  message(sprintf("Teams: %d", nrow(data)))

  # 1. Metric importance analysis
  message("\n--- Metric Importance Analysis ---")
  metric_analysis <- analyze_metrics(data)

  message("\nCorrelations with tournament wins:")
  print(metric_analysis$correlations)

  message("\nFull model coefficients:")
  print(metric_analysis$coefficients)

  # 2. Leave-one-year-out cross-validation
  message("\n--- Cross-Validation Results ---")
  cv_results <- loyo_cv(data, c("barthag_z"))
  cv_summary <- summarize_cv(cv_results)

  message("\nOverall CV metrics:")
  print(cv_summary$overall)

  message("\nCV metrics by seed tier:")
  print(cv_summary$by_tier)

  message("\nCV metrics by seed:")
  print(cv_summary$by_seed)

  # 3. Calibration analysis
  message("\n--- Calibration Analysis ---")
  calibration <- calibrate_adjustment(data)

  message(sprintf("\nEmpirical adjustment: %.3f wins per σ of barthag",
                  calibration$empirical_wins_per_sigma))

  message("\nCalibration by barthag z-score bucket:")
  print(calibration$calibration_by_bucket)

  # 4. Model comparison
  message("\n--- Model Comparison ---")
  model_comparison <- compare_models(data)

  message("\nModel performance comparison (lower MAE = better):")
  print(model_comparison)

  # 5. Incremental EV calculation
  message("\n--- Incremental EV Analysis ---")
  ev_analysis <- calculate_incremental_ev(data)

  message(sprintf("\nWins per σ: %.3f", ev_analysis$wins_per_sigma))
  message("\nIncremental EV by seed:")
  print(ev_analysis$incremental_ev)

  # Save outputs
  ensure_dir("output")

  # Save cross-validation results
  write_csv(cv_results, "output/intra_seed_validation.csv")
  message("\nSaved: output/intra_seed_validation.csv")

  # Save metric importance
  write_csv(metric_analysis$correlations, "output/metric_importance.csv")
  message("Saved: output/metric_importance.csv")

  # Save incremental EV
  write_csv(ev_analysis$incremental_ev, "output/incremental_ev_by_seed.csv")
  message("Saved: output/incremental_ev_by_seed.csv")

  # Save model comparison
  write_csv(model_comparison, "output/model_comparison.csv")
  message("Saved: output/model_comparison.csv")

  # Save calibration
  write_csv(calibration$calibration_by_bucket, "output/calibration_by_zscore.csv")
  message("Saved: output/calibration_by_zscore.csv")

  message("\n" %>% paste(rep("=", 60), collapse = ""))
  message("Validation complete!")
  message("=" %>% rep(60) %>% paste(collapse = ""))

  # Return key findings
  list(
    data = data,
    metric_analysis = metric_analysis,
    cv_results = cv_results,
    cv_summary = cv_summary,
    calibration = calibration,
    model_comparison = model_comparison,
    incremental_ev = ev_analysis
  )
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  results <- run_validation()
}
