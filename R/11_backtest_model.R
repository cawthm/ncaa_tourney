# 11_backtest_model.R - Backtest the fitted model against historical tournaments
#
# Compares Barthag-adjusted EV predictions to:
# 1. Naive seed-based EV (historical averages)
# 2. Actual tournament outcomes
#
# Key question: Does Barthag-adjusted EV predict tournament outcomes
# better than naive seed-based EV?

library(tidyverse)
library(jsonlite)

source("R/utils.R")
source("R/10_bracket_ev_calculator.R")

#' Load historical seed progression probabilities
#' @return Data frame with seed, round, and probability columns
load_seed_probs <- function() {
  # These are the historical averages from 02_seed_analysis.R
  # P(reach round | seed)
  seed_probs <- tribble(
    ~seed, ~R64, ~R32, ~S16, ~E8, ~F4, ~CHAMP,
    1,  1.000, 0.860, 0.640, 0.473, 0.307, 0.173,
    2,  1.000, 0.780, 0.470, 0.300, 0.167, 0.087,
    3,  1.000, 0.680, 0.360, 0.213, 0.100, 0.040,
    4,  1.000, 0.620, 0.310, 0.160, 0.073, 0.040,
    5,  1.000, 0.560, 0.250, 0.120, 0.047, 0.020,
    6,  1.000, 0.530, 0.220, 0.093, 0.047, 0.013,
    7,  1.000, 0.470, 0.170, 0.073, 0.027, 0.007,
    8,  1.000, 0.450, 0.150, 0.060, 0.027, 0.007,
    9,  1.000, 0.400, 0.120, 0.053, 0.020, 0.007,
    10, 1.000, 0.360, 0.100, 0.040, 0.020, 0.007,
    11, 1.000, 0.340, 0.120, 0.047, 0.027, 0.007,
    12, 1.000, 0.310, 0.080, 0.027, 0.007, 0.000,
    13, 1.000, 0.170, 0.030, 0.007, 0.000, 0.000,
    14, 1.000, 0.120, 0.020, 0.007, 0.000, 0.000,
    15, 1.000, 0.070, 0.010, 0.007, 0.000, 0.000,
    16, 1.000, 0.010, 0.000, 0.000, 0.000, 0.000
  )

  seed_probs
}

#' Calculate naive (seed-based) EV for each seed
#' @return Data frame with seed and naive_ev
calc_naive_seed_ev <- function() {
  seed_probs <- load_seed_probs()

  # EV = sum of P(reach round) * payout(round)
  seed_probs %>%
    mutate(
      naive_ev = R32 * PAYOUTS["R32"] +
                 S16 * PAYOUTS["S16"] +
                 E8 * PAYOUTS["E8"] +
                 F4 * PAYOUTS["F4"] +
                 CHAMP * PAYOUTS["CHAMP"]
    ) %>%
    select(seed, naive_ev)
}

#' Convert tournament wins to EV earned
#' @param wins Number of tournament wins (0-6)
#' @return EV earned as proportion of pot
wins_to_ev <- function(wins) {
  # Cumulative payouts based on how far team advanced
  case_when(
    wins == 0 ~ 0,                           # Lost R64
    wins == 1 ~ PAYOUTS["R32"],              # Won R64, lost R32 -> S16 payout
    wins == 2 ~ PAYOUTS["R32"] + PAYOUTS["S16"],  # Reached E8
    wins == 3 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"],  # Reached F4
    wins == 4 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"] + PAYOUTS["F4"],  # Reached Final
    wins == 5 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"] + PAYOUTS["F4"],  # Lost Final
    wins == 6 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"] + PAYOUTS["F4"] + PAYOUTS["CHAMP"],  # Won it all
    TRUE ~ 0
  )
}

#' Load actual tournament results
#' @return Data frame with year, team, seed, wins, actual_ev
load_actual_results <- function() {
  # Load from the wins by year file
  wins_file <- "data/raw/tournament_results_2024_2025.csv"

  if (!file.exists(wins_file)) {
    warning("Tournament results file not found")
    return(NULL)
  }

  wins_data <- read_csv(wins_file, show_col_types = FALSE)

  # Add actual EV
  wins_data %>%
    mutate(actual_ev = wins_to_ev(wins))
}

#' Get years that have both ratings and results data
#' @return Vector of years with complete data
get_valid_years <- function() {
  # Check which years have team ratings
  ratings <- read_csv("data/raw/team_ratings_with_seeds.csv", show_col_types = FALSE)
  rating_years <- unique(ratings$year)

  # Check which years have actual results
  results_file <- "data/raw/tournament_results_2024_2025.csv"
  if (file.exists(results_file)) {
    results <- read_csv(results_file, show_col_types = FALSE)
    result_years <- unique(results$year)
  } else {
    result_years <- c()
  }

  # Return intersection (excluding 2020 - no tournament)
  valid <- intersect(rating_years, result_years)
  setdiff(valid, 2020)
}

#' Default Barthag values by seed (based on historical averages)
#' Used when a seed is missing from a particular year's data
DEFAULT_BARTHAG_BY_SEED <- c(
  "1" = 0.95, "2" = 0.92, "3" = 0.89, "4" = 0.86,
  "5" = 0.83, "6" = 0.80, "7" = 0.77, "8" = 0.74,
  "9" = 0.72, "10" = 0.70, "11" = 0.68, "12" = 0.66,
  "13" = 0.60, "14" = 0.55, "15" = 0.50, "16" = 0.45
)

#' Calculate Barthag-adjusted EV for historical tournaments
#' @param years Vector of years to backtest
#' @param model Fitted model coefficients
#' @return Data frame with predictions for each team-year
calc_historical_barthag_ev <- function(years = 2008:2024, model = NULL) {
  if (is.null(model)) {
    model <- load_model()
  }

  # Load team ratings
  ratings <- read_csv("data/raw/team_ratings_with_seeds.csv", show_col_types = FALSE)

  # Filter to requested years and tournament teams
  ratings <- ratings %>%
    filter(year %in% years, !is.na(seed))

  # For each year, create pseudo-bracket and calculate EV
  # Since we don't have region info, we'll use average region approach
  results <- ratings %>%
    group_by(year) %>%
    group_modify(function(year_data, key) {
      # Create a "typical" region by using average Barthag by seed
      avg_barthag_by_seed <- year_data %>%
        group_by(seed) %>%
        summarise(avg_barthag = mean(barthag, na.rm = TRUE), .groups = "drop")

      # Start with defaults and overwrite with actual data
      region_barthags <- DEFAULT_BARTHAG_BY_SEED

      # Overwrite with actual data where available
      for (i in seq_len(nrow(avg_barthag_by_seed))) {
        seed_str <- as.character(avg_barthag_by_seed$seed[i])
        region_barthags[seed_str] <- avg_barthag_by_seed$avg_barthag[i]
      }

      # Calculate EV for each team
      year_data %>%
        rowwise() %>%
        mutate(
          ev_result = list(calc_team_ev(barthag, seed, region_barthags, model)),
          barthag_ev = ev_result$ev,
          p_r64 = ev_result$probs$R64,
          p_r32 = ev_result$probs$R32,
          p_s16 = ev_result$probs$S16,
          p_e8 = ev_result$probs$E8,
          p_f4 = ev_result$probs$F4,
          p_champ = ev_result$probs$CHAMP
        ) %>%
        ungroup() %>%
        select(-ev_result)
    }) %>%
    ungroup()

  # Add naive seed EV for comparison
  naive_ev <- calc_naive_seed_ev()
  results <- results %>%
    left_join(naive_ev, by = "seed")

  results
}

#' Compare predicted vs actual outcomes
#' @param predictions Data frame with team predictions
#' @param actuals Data frame with actual results
#' @return Comparison metrics
compare_predictions <- function(predictions, actuals) {
  # Join predictions with actuals
  # Note: This requires matching teams which can be tricky due to name variations

  # For now, compare at the seed-year level
  pred_by_seed <- predictions %>%
    group_by(year, seed) %>%
    summarise(
      n_teams = n(),
      avg_barthag_ev = mean(barthag_ev),
      avg_naive_ev = mean(naive_ev),
      avg_barthag = mean(barthag),
      .groups = "drop"
    )

  actual_by_seed <- actuals %>%
    group_by(year, seed) %>%
    summarise(
      avg_actual_ev = mean(actual_ev),
      avg_wins = mean(wins),
      .groups = "drop"
    )

  comparison <- pred_by_seed %>%
    left_join(actual_by_seed, by = c("year", "seed"))

  comparison
}

#' Calculate backtest metrics
#' @param comparison Data frame with predicted and actual EVs
#' @return List of metrics, or NULL if insufficient data
calc_backtest_metrics <- function(comparison) {
  comparison <- comparison %>%
    filter(!is.na(avg_actual_ev))

  # Check if we have enough data
  if (nrow(comparison) < 2) {
    message("Insufficient data for backtest metrics (need at least 2 complete observations)")
    return(NULL)
  }

  # Correlation with actual outcomes (need variance for correlation)
  if (sd(comparison$avg_actual_ev, na.rm = TRUE) == 0 ||
      sd(comparison$avg_barthag_ev, na.rm = TRUE) == 0) {
    cor_barthag <- NA
    cor_naive <- NA
  } else {
    cor_barthag <- cor(comparison$avg_barthag_ev, comparison$avg_actual_ev, use = "complete.obs")
    cor_naive <- cor(comparison$avg_naive_ev, comparison$avg_actual_ev, use = "complete.obs")
  }

  # Mean squared error
  mse_barthag <- mean((comparison$avg_barthag_ev - comparison$avg_actual_ev)^2, na.rm = TRUE)
  mse_naive <- mean((comparison$avg_naive_ev - comparison$avg_actual_ev)^2, na.rm = TRUE)

  # Mean absolute error
  mae_barthag <- mean(abs(comparison$avg_barthag_ev - comparison$avg_actual_ev), na.rm = TRUE)
  mae_naive <- mean(abs(comparison$avg_naive_ev - comparison$avg_actual_ev), na.rm = TRUE)

  list(
    n_observations = nrow(comparison),
    barthag = list(
      correlation = cor_barthag,
      mse = mse_barthag,
      rmse = sqrt(mse_barthag),
      mae = mae_barthag
    ),
    naive = list(
      correlation = cor_naive,
      mse = mse_naive,
      rmse = sqrt(mse_naive),
      mae = mae_naive
    ),
    improvement = list(
      correlation_diff = if (is.na(cor_barthag) || is.na(cor_naive)) NA else cor_barthag - cor_naive,
      mse_reduction = if (mse_naive == 0) NA else (mse_naive - mse_barthag) / mse_naive * 100,
      mae_reduction = if (mae_naive == 0) NA else (mae_naive - mae_barthag) / mae_naive * 100
    )
  )
}

#' Analyze within-seed differentiation
#' Do higher Barthag teams within a seed perform better?
analyze_within_seed <- function(predictions, actuals) {
  # Join at team level if possible, or use seed-level proxy

  # For each seed, correlate Barthag rank with actual performance
  within_seed <- predictions %>%
    group_by(year, seed) %>%
    mutate(
      barthag_rank = rank(-barthag),  # 1 = highest Barthag
      n_at_seed = n()
    ) %>%
    filter(n_at_seed > 1) %>%  # Need variation within seed
    ungroup()

  # This would require matching with actual wins per team
  # For now, return the ranked predictions for inspection

  within_seed %>%
    select(year, seed, team, barthag, barthag_rank, barthag_ev, naive_ev)
}

#' Main backtest function
run_backtest <- function() {
  message("Running backtest of Barthag-adjusted EV model...")
  message(rep("=", 50))

  # Load or calculate predictions
  model <- load_model()
  message(sprintf("Using model: intercept=%.4f, coefficient=%.4f",
                  model$intercept, model$coefficient))

  # Get all years with ratings data (skip 2020 - no tournament)
  ratings <- read_csv("data/raw/team_ratings_with_seeds.csv", show_col_types = FALSE)
  rating_years <- setdiff(unique(ratings$year), 2020)
  message(sprintf("\nYears with Barthag ratings: %s", paste(rating_years, collapse = ", ")))

  # Calculate predictions for all available years
  message("\nCalculating Barthag-adjusted EV for historical tournaments...")
  predictions <- calc_historical_barthag_ev(rating_years, model)
  message(sprintf("Calculated predictions for %d team-years", nrow(predictions)))

  # Load actual results and check overlap
  actuals <- load_actual_results()
  metrics <- NULL

  if (!is.null(actuals)) {
    actual_years <- unique(actuals$year)
    message(sprintf("\nYears with actual results: %s", paste(actual_years, collapse = ", ")))

    overlap_years <- intersect(rating_years, actual_years)
    message(sprintf("Years with both ratings and results: %s",
                    if (length(overlap_years) == 0) "NONE" else paste(overlap_years, collapse = ", ")))

    if (length(overlap_years) > 0) {
      message("\nComparing to actual results...")
      comparison <- compare_predictions(predictions, actuals)

      metrics <- calc_backtest_metrics(comparison)

      if (!is.null(metrics)) {
        message("\n", rep("=", 50))
        message(sprintf("Backtest Results (based on %d observations):", metrics$n_observations))
        message("\nBarthag-adjusted EV:")
        message(sprintf("  Correlation with actual: %s",
                        if (is.na(metrics$barthag$correlation)) "NA" else sprintf("%.3f", metrics$barthag$correlation)))
        message(sprintf("  RMSE: %.4f", metrics$barthag$rmse))
        message(sprintf("  MAE: %.4f", metrics$barthag$mae))

        message("\nNaive (seed-based) EV:")
        message(sprintf("  Correlation with actual: %s",
                        if (is.na(metrics$naive$correlation)) "NA" else sprintf("%.3f", metrics$naive$correlation)))
        message(sprintf("  RMSE: %.4f", metrics$naive$rmse))
        message(sprintf("  MAE: %.4f", metrics$naive$mae))

        message("\nImprovement:")
        message(sprintf("  Correlation improvement: %s",
                        if (is.na(metrics$improvement$correlation_diff)) "NA" else sprintf("%.3f", metrics$improvement$correlation_diff)))
        message(sprintf("  MSE reduction: %s",
                        if (is.na(metrics$improvement$mse_reduction)) "NA" else sprintf("%.1f%%", metrics$improvement$mse_reduction)))
        message(sprintf("  MAE reduction: %s",
                        if (is.na(metrics$improvement$mae_reduction)) "NA" else sprintf("%.1f%%", metrics$improvement$mae_reduction)))

        # Save results
        ensure_dir("output")
        write_csv(comparison, "output/backtest_comparison.csv")
        write_json(metrics, "output/backtest_metrics.json", pretty = TRUE)
        message("\nSaved results to output/")
      }
    } else {
      message("\nNo overlapping years - skipping comparison with actual results")
    }
  }

  # Analyze by seed
  message("\n", rep("=", 50))
  message("EV by Seed (averaged across years):")

  by_seed <- predictions %>%
    group_by(seed) %>%
    summarise(
      avg_barthag_ev = mean(barthag_ev) * 100,
      avg_naive_ev = mean(naive_ev) * 100,
      ev_diff = avg_barthag_ev - avg_naive_ev,
      avg_barthag = mean(barthag),
      sd_barthag = sd(barthag),
      .groups = "drop"
    )

  print(by_seed)

  # Verify total EV
  total_ev <- sum(by_seed$avg_barthag_ev * 4)  # 4 regions
  message(sprintf("\nTotal EV across all teams: %.1f%% (should be ~100%%)", total_ev))

  # Save predictions
  write_csv(predictions, "output/backtest_predictions.csv")
  write_csv(by_seed, "output/backtest_by_seed.csv")

  list(
    predictions = predictions,
    by_seed = by_seed,
    metrics = if (exists("metrics")) metrics else NULL
  )
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  results <- run_backtest()
}
