# 06_bracket_evaluator.R - Evaluate specific matchups when bracket is announced
#
# Usage:
#   source("R/06_bracket_evaluator.R")
#   evaluate_matchup(high_seed_barthag = 0.955, low_seed_barthag = 0.42, matchup = "1/16")

library(tidyverse)
source("R/utils.R")

# Load benchmarks
benchmarks <- read_csv("data/processed/team_ratings_zscores.csv", show_col_types = FALSE) %>%
  group_by(seed) %>%
  summarize(
    barthag_avg = mean(barthag),
    barthag_75 = quantile(barthag, 0.75),
    barthag_25 = quantile(barthag, 0.25),
    adj_d_avg = mean(adj_d),
    .groups = "drop"
  )

# Base EVs for matchups
matchup_base_ev <- tribble(
  ~matchup, ~high_seed, ~low_seed, ~base_ev_pct,
  "1/16", 1, 16, 7.60,
  "2/15", 2, 15, 5.21,
  "3/14", 3, 14, 3.12,
  "4/13", 4, 13, 2.49,
  "5/12", 5, 12, 1.83,
  "6/11", 6, 11, 1.71,
  "7/10", 7, 10, 1.63,
  "8/9",  8, 9,  1.42
)

#' Evaluate a specific matchup
#' @param high_seed_barthag Barthag rating of the higher seed
#' @param low_seed_barthag Barthag rating of the lower seed
#' @param matchup Matchup string like "1/16" or "5/12"
#' @param pot_size Total pot size in dollars
evaluate_matchup <- function(high_seed_barthag, low_seed_barthag, matchup, pot_size = 1000) {

  matchup_info <- matchup_base_ev %>% filter(matchup == !!matchup)

  if (nrow(matchup_info) == 0) {
    stop("Invalid matchup. Use format like '1/16', '5/12', etc.")
  }

  high_seed <- matchup_info$high_seed
  low_seed <- matchup_info$low_seed
  base_ev <- matchup_info$base_ev_pct

  # Get benchmarks for these seeds
  high_bench <- benchmarks %>% filter(seed == high_seed)
  low_bench <- benchmarks %>% filter(seed == low_seed)

  # Calculate z-scores vs historical
  high_z <- (high_seed_barthag - high_bench$barthag_avg) /
            ((high_bench$barthag_75 - high_bench$barthag_25) / 1.35)
  low_z <- (low_seed_barthag - low_bench$barthag_avg) /
           ((low_bench$barthag_75 - low_bench$barthag_25) / 1.35)

  # Adjust EV based on team quality
  # Each standard deviation above average adds ~10% to EV
  adjustment <- 1 + (0.10 * (high_z + low_z) / 2)
  adjustment <- pmax(0.7, pmin(1.3, adjustment))  # Cap at ±30%

  adjusted_ev <- base_ev * adjustment

  # Calculate bid range
  ev_dollars <- adjusted_ev / 100 * pot_size

  cat("\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("  MATCHUP EVALUATION: %s\n", matchup))
  cat("═══════════════════════════════════════════════════════\n\n")

  cat(sprintf("  %d-seed: Barthag %.3f ", high_seed, high_seed_barthag))
  if (high_z > 0.5) cat("(STRONG for seed) ↑\n")
  else if (high_z < -0.5) cat("(WEAK for seed) ↓\n")
  else cat("(average for seed)\n")

  cat(sprintf("  %d-seed: Barthag %.3f ", low_seed, low_seed_barthag))
  if (low_z > 0.5) cat("(STRONG for seed) ↑\n")
  else if (low_z < -0.5) cat("(WEAK for seed) ↓\n")
  else cat("(average for seed)\n")

  cat("\n")
  cat(sprintf("  Base EV:      %.2f%% ($%.0f)\n", base_ev, base_ev/100*pot_size))
  cat(sprintf("  Adjusted EV:  %.2f%% ($%.0f)\n", adjusted_ev, ev_dollars))
  cat(sprintf("  Adjustment:   %+.0f%%\n", (adjustment - 1) * 100))

  cat("\n  ─────────────────────────────────────────────────────\n")
  cat(sprintf("  TARGET BID:   $%.0f - $%.0f\n", ev_dollars * 0.65, ev_dollars * 0.85))
  cat(sprintf("  MAX BID:      $%.0f (break-even)\n", ev_dollars))
  cat("  ─────────────────────────────────────────────────────\n\n")

  invisible(list(
    matchup = matchup,
    base_ev = base_ev,
    adjusted_ev = adjusted_ev,
    ev_dollars = ev_dollars,
    high_z = high_z,
    low_z = low_z
  ))
}

#' Compare multiple versions of the same matchup across regions
#' @param matchups_df Data frame with columns: region, high_barthag, low_barthag
#' @param matchup Matchup string like "1/16"
compare_regions <- function(matchups_df, matchup, pot_size = 1000) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("  COMPARING %s MATCHUPS ACROSS REGIONS\n", matchup))
  cat("═══════════════════════════════════════════════════════\n\n")

  results <- matchups_df %>%
    rowwise() %>%
    mutate(
      eval = list(evaluate_matchup(high_barthag, low_barthag, matchup, pot_size))
    ) %>%
    ungroup() %>%
    mutate(
      adjusted_ev = map_dbl(eval, "adjusted_ev"),
      ev_dollars = map_dbl(eval, "ev_dollars")
    ) %>%
    arrange(desc(adjusted_ev))

  cat("\n  RANKING (best to worst):\n\n")
  for (i in 1:nrow(results)) {
    cat(sprintf("  %d. %s region: EV = $%.0f (target bid: $%.0f)\n",
        i, results$region[i], results$ev_dollars[i], results$ev_dollars[i] * 0.75))
  }
  cat("\n")

  invisible(results)
}

#' Print bid sheet summary for all 8 matchup types
print_bid_sheet <- function(pot_size = 1000) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("  CALCUTTA BID SHEET ($%s pot)\n", format(pot_size, big.mark=",")))
  cat("═══════════════════════════════════════════════════════\n\n")

  cat(sprintf("  %-10s %10s %12s %12s\n", "Matchup", "EV", "Target Bid", "Max Bid"))
  cat("  ", rep("-", 48), "\n", sep="")

  for (i in 1:nrow(matchup_base_ev)) {
    ev <- matchup_base_ev$base_ev_pct[i] / 100 * pot_size
    cat(sprintf("  %-10s %10.0f %12.0f %12.0f\n",
        matchup_base_ev$matchup[i],
        ev,
        ev * 0.75,
        ev))
  }

  cat("\n  Total EV per region: $", format(pot_size/4, big.mark=","), "\n", sep="")
  cat("  (4 regions × 8 matchups = 32 total auction items)\n\n")
}

# Print usage on load
cat("\n=== Bracket Evaluator Loaded ===\n")
cat("\nFunctions available:\n")
cat("  evaluate_matchup(high_barthag, low_barthag, matchup, pot_size=1000)\n")
cat("  compare_regions(matchups_df, matchup, pot_size=1000)\n")
cat("  print_bid_sheet(pot_size=1000)\n")
cat("\nExample:\n")
cat("  evaluate_matchup(0.955, 0.42, '1/16', pot_size=2000)\n\n")
