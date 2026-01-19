# 12_backtest_by_year.R - Year-by-year backtest of Adj EV vs Naive EV
#
# This script compares the predictive accuracy of:
# 1. Naive EV (historical seed averages)
# 2. Barthag-Adjusted EV (using that year's Barthag data)
#
# Against actual tournament outcomes from seed_wins_by_year.csv

library(tidyverse)
library(jsonlite)

source("R/utils.R")
source("R/10_bracket_ev_calculator.R")

# ============================================================================
# Data Loading
# ============================================================================

#' Load actual tournament wins by seed and year
load_seed_wins <- function() {
  read_csv("data/raw/seed_wins_by_year.csv", show_col_types = FALSE)
}

#' Load naive seed EV from historical probabilities
load_naive_ev <- function() {
  probs <- read_csv("output/seed_progression_probs.csv", show_col_types = FALSE)
  probs %>%
    select(seed, ev_pct) %>%
    mutate(naive_ev = ev_pct / 100)  # Convert to proportion
}

#' Load team ratings with Barthag
load_team_ratings <- function() {
  read_csv("data/raw/team_ratings_with_seeds.csv", show_col_types = FALSE)
}

# ============================================================================
# EV Calculation Functions
# ============================================================================

#' Convert wins to EV earned (proportion of pot)
#' @param wins Number of tournament wins (0-6)
#' @return EV earned as proportion of pot
wins_to_ev <- function(wins) {
  case_when(
    wins == 0 ~ 0,                           # Lost R64
    wins == 1 ~ PAYOUTS["R32"],              # Made S16
    wins == 2 ~ PAYOUTS["R32"] + PAYOUTS["S16"],  # Made E8
    wins == 3 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"],  # Made F4
    wins == 4 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"] + PAYOUTS["F4"],  # Made Final
    wins == 5 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"] + PAYOUTS["F4"],  # Lost Final
    wins == 6 ~ PAYOUTS["R32"] + PAYOUTS["S16"] + PAYOUTS["E8"] + PAYOUTS["F4"] + PAYOUTS["CHAMP"],  # Won it all
    TRUE ~ 0
  )
}

#' Default Barthag values by seed (used when data is missing)
DEFAULT_BARTHAG_BY_SEED <- c(
  "1" = 0.95, "2" = 0.92, "3" = 0.89, "4" = 0.86,
  "5" = 0.83, "6" = 0.80, "7" = 0.77, "8" = 0.74,
  "9" = 0.72, "10" = 0.70, "11" = 0.68, "12" = 0.66,
  "13" = 0.60, "14" = 0.55, "15" = 0.50, "16" = 0.45
)

#' Calculate Barthag-adjusted EV for a single year
#' @param year Tournament year
#' @param team_ratings Full team ratings data frame
#' @param model Fitted logistic model
#' @return Data frame with seed-level Adj EV for that year
calc_year_adj_ev <- function(year, team_ratings, model) {
  # Filter to this year's tournament teams
  year_data <- team_ratings %>%
    filter(year == !!year, !is.na(seed))

  if (nrow(year_data) == 0) {
    return(NULL)
  }

  # Calculate average Barthag by seed for this year (to approximate region)
  avg_barthag_by_seed <- year_data %>%
    group_by(seed) %>%
    summarise(avg_barthag = mean(barthag, na.rm = TRUE), .groups = "drop")

  # Fill in missing seeds with defaults
  region_barthags <- DEFAULT_BARTHAG_BY_SEED
  for (i in seq_len(nrow(avg_barthag_by_seed))) {
    seed_str <- as.character(avg_barthag_by_seed$seed[i])
    region_barthags[seed_str] <- avg_barthag_by_seed$avg_barthag[i]
  }

  # Calculate Adj EV for each team
  team_evs <- year_data %>%
    rowwise() %>%
    mutate(
      ev_result = list(calc_team_ev(barthag, seed, region_barthags, model)),
      adj_ev = ev_result$ev,
      p_champ = ev_result$probs$CHAMP
    ) %>%
    ungroup() %>%
    select(team, seed, barthag, adj_ev, p_champ)

  # Aggregate to seed level (average across 4 teams per seed)
  seed_evs <- team_evs %>%
    group_by(seed) %>%
    summarise(
      n_teams = n(),
      avg_barthag = mean(barthag, na.rm = TRUE),
      adj_ev = mean(adj_ev, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year = !!year)

  seed_evs
}

# ============================================================================
# Main Backtest Function
# ============================================================================

#' Run year-by-year backtest
run_yearly_backtest <- function() {
  message("Running year-by-year backtest of Adj EV vs Naive EV")
  message(rep("=", 60))

  # Load data
  seed_wins <- load_seed_wins()
  naive_ev <- load_naive_ev()
  team_ratings <- load_team_ratings()
  model <- load_model()

  message(sprintf("Model: intercept=%.4f, coefficient=%.4f",
                  model$intercept, model$coefficient))

  # Get years (exclude 2020 - no tournament)
  years <- seed_wins %>%
    filter(year != 2020) %>%
    pull(year) %>%
    unique() %>%
    sort()

  message(sprintf("Years to analyze: %d (%d-%d)",
                  length(years), min(years), max(years)))

  # Calculate actual EV by seed-year
  actual_ev <- seed_wins %>%
    filter(year != 2020) %>%
    mutate(ev_earned = wins_to_ev(wins)) %>%
    group_by(year, seed) %>%
    summarise(
      n_teams = n(),
      total_wins = sum(wins),
      avg_wins = mean(wins),
      actual_ev = mean(ev_earned),  # Average across 4 teams in this seed
      .groups = "drop"
    )

  # Calculate Adj EV for each year
  message("\nCalculating Adj EV for each year...")
  adj_ev_by_year <- map_dfr(years, function(y) {
    result <- calc_year_adj_ev(y, team_ratings, model)
    if (!is.null(result)) {
      message(sprintf("  %d: %d seeds calculated", y, nrow(result)))
    }
    result
  })

  # Join everything together
  comparison <- actual_ev %>%
    left_join(naive_ev %>% select(seed, naive_ev), by = "seed") %>%
    left_join(adj_ev_by_year %>% select(year, seed, adj_ev, avg_barthag),
              by = c("year", "seed"))

  # Calculate errors
  comparison <- comparison %>%
    mutate(
      naive_error = naive_ev - actual_ev,
      adj_error = adj_ev - actual_ev,
      naive_abs_error = abs(naive_error),
      adj_abs_error = abs(adj_error),
      adj_improved = adj_abs_error < naive_abs_error
    )

  # ============================================================================
  # Year-level Summary
  # ============================================================================

  yearly_summary <- comparison %>%
    filter(!is.na(adj_ev)) %>%
    group_by(year) %>%
    summarise(
      n_seeds = n(),
      naive_mae = mean(naive_abs_error),
      adj_mae = mean(adj_abs_error),
      naive_rmse = sqrt(mean(naive_error^2)),
      adj_rmse = sqrt(mean(adj_error^2)),
      pct_improved = mean(adj_improved) * 100,
      mae_improvement = (naive_mae - adj_mae) / naive_mae * 100,
      .groups = "drop"
    )

  message("\n", rep("=", 60))
  message("Year-by-Year Results:")
  message(rep("-", 60))
  message(sprintf("%-6s %8s %8s %8s %8s %10s",
                  "Year", "Naive", "Adj", "Improve", "N Seeds", "% Better"))
  message(sprintf("%-6s %8s %8s %8s %8s %10s",
                  "", "MAE", "MAE", "(%)", "", ""))
  message(rep("-", 60))

  for (i in seq_len(nrow(yearly_summary))) {
    row <- yearly_summary[i, ]
    message(sprintf("%-6d %8.4f %8.4f %+7.1f%% %8d %9.1f%%",
                    row$year, row$naive_mae, row$adj_mae,
                    row$mae_improvement, row$n_seeds, row$pct_improved))
  }

  # Overall summary
  message(rep("=", 60))
  overall <- yearly_summary %>%
    summarise(
      mean_naive_mae = mean(naive_mae),
      mean_adj_mae = mean(adj_mae),
      mean_improvement = mean(mae_improvement),
      mean_pct_improved = mean(pct_improved),
      years_where_adj_won = sum(adj_mae < naive_mae),
      total_years = n()
    )

  message(sprintf("\nOverall (across %d years):", overall$total_years))
  message(sprintf("  Average Naive MAE:  %.4f", overall$mean_naive_mae))
  message(sprintf("  Average Adj MAE:    %.4f", overall$mean_adj_mae))
  message(sprintf("  Mean Improvement:   %.1f%%", overall$mean_improvement))
  message(sprintf("  Years Adj won:      %d/%d (%.1f%%)",
                  overall$years_where_adj_won, overall$total_years,
                  overall$years_where_adj_won / overall$total_years * 100))
  message(sprintf("  Avg %% seeds improved: %.1f%%", overall$mean_pct_improved))

  # ============================================================================
  # Seed-level Summary
  # ============================================================================

  seed_summary <- comparison %>%
    filter(!is.na(adj_ev)) %>%
    group_by(seed) %>%
    summarise(
      n_years = n(),
      naive_mae = mean(naive_abs_error),
      adj_mae = mean(adj_abs_error),
      mae_improvement = (naive_mae - adj_mae) / naive_mae * 100,
      avg_naive_ev = mean(naive_ev) * 100,
      avg_adj_ev = mean(adj_ev) * 100,
      avg_actual_ev = mean(actual_ev) * 100,
      .groups = "drop"
    )

  message("\n", rep("=", 60))
  message("By Seed (averaged across years):")
  message(rep("-", 60))
  message(sprintf("%-4s %8s %8s %8s %8s %8s %8s",
                  "Seed", "Naive", "Adj", "Actual", "Naive", "Adj", "Improve"))
  message(sprintf("%-4s %8s %8s %8s %8s %8s %8s",
                  "", "EV%", "EV%", "EV%", "MAE", "MAE", "(%)"))
  message(rep("-", 60))

  for (i in seq_len(nrow(seed_summary))) {
    row <- seed_summary[i, ]
    message(sprintf("%-4d %8.2f %8.2f %8.2f %8.4f %8.4f %+7.1f%%",
                    row$seed, row$avg_naive_ev, row$avg_adj_ev, row$avg_actual_ev,
                    row$naive_mae, row$adj_mae, row$mae_improvement))
  }

  # Save results
  ensure_dir("output")
  write_csv(comparison, "output/backtest_yearly_comparison.csv")
  write_csv(yearly_summary, "output/backtest_yearly_summary.csv")
  write_csv(seed_summary, "output/backtest_seed_summary.csv")

  message("\n\nSaved results to output/")

  list(
    comparison = comparison,
    yearly_summary = yearly_summary,
    seed_summary = seed_summary,
    overall = overall
  )
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  results <- run_yearly_backtest()
}
