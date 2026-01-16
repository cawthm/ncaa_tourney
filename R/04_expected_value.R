# 04_expected_value.R - Calculate expected value for tournament teams
#
# Combines seed-based probabilities with team-specific adjustments
# to produce expected value estimates for each team

library(tidyverse)
source("R/utils.R")

#' Load seed progression probabilities
load_seed_probs <- function(file = "output/seed_progression_probs.csv") {
  if (file.exists(file)) {
    read_csv(file, show_col_types = FALSE)
  } else {
    # Fall back to historical
    source("R/02_seed_analysis.R")
    get_historical_seed_probs() %>%
      add_expected_values()
  }
}

#' Adjust seed probabilities for a specific team
#' @param seed_probs Row from seed probability matrix
#' @param adjustment Multiplier (1.0 = average, 1.15 = 15% better)
#' @return Adjusted probabilities
adjust_team_probs <- function(seed_probs, adjustment) {
  prob_cols <- c("p_R64", "p_R32", "p_S16", "p_E8", "p_F4", "p_CHAMP")

  # Simple multiplicative adjustment
  # More sophisticated would adjust each round differently
  adjusted <- seed_probs

  for (col in prob_cols) {
    if (col %in% names(adjusted)) {
      # Adjust probability
      new_val <- adjusted[[col]] * adjustment

      # Cap at reasonable bounds
      # Can't exceed 1.0 or the probability of the previous round
      adjusted[[col]] <- pmin(new_val, 1.0)
    }
  }

  # Ensure monotonicity (each round prob <= previous round prob)
  for (i in seq_along(prob_cols)[-1]) {
    adjusted[[prob_cols[i]]] <- pmin(
      adjusted[[prob_cols[i]]],
      adjusted[[prob_cols[i-1]]]
    )
  }

  adjusted
}

#' Calculate expected value for a team
#' @param probs Named vector or data frame row with p_R64, p_R32, etc.
#' @return Expected value as percentage of pot
calculate_team_ev <- function(probs) {
  ev <- probs$p_R64 * PAYOUTS["R64"] +
        probs$p_R32 * PAYOUTS["R32"] +
        probs$p_S16 * PAYOUTS["S16"] +
        probs$p_E8 * PAYOUTS["E8"] +
        probs$p_F4 * PAYOUTS["F4"] +
        probs$p_CHAMP * PAYOUTS["CHAMP"]

  ev * 100  # Convert to percentage
}

#' Calculate variance of tournament outcome
#' Higher variance = more uncertainty = potentially higher discount
calculate_outcome_variance <- function(probs) {
  # Payouts for each round
  payouts <- c(
    PAYOUTS["R64"], PAYOUTS["R32"], PAYOUTS["S16"],
    PAYOUTS["E8"], PAYOUTS["F4"], PAYOUTS["CHAMP"]
  )

  # Probability of stopping at each round
  # P(stop at R64) = P(reach R64) - P(reach R32) = p_R64 - p_R32
  # But our p_R64 is actually P(win at least 1), so:
  # P(exactly 1 win) = p_R64 - p_R32
  p_exactly <- c(
    probs$p_R64 - probs$p_R32,  # Exactly 1 win
    probs$p_R32 - probs$p_S16,  # Exactly 2 wins
    probs$p_S16 - probs$p_E8,   # Exactly 3 wins
    probs$p_E8 - probs$p_F4,    # Exactly 4 wins
    probs$p_F4 - probs$p_CHAMP, # Exactly 5 wins
    probs$p_CHAMP               # Exactly 6 wins (champion)
  )

  # Also include probability of 0 wins
  p_zero <- 1 - probs$p_R64
  payouts_with_zero <- c(0, payouts)
  p_all <- c(p_zero, p_exactly)

  # Expected value
  ev <- sum(p_all * payouts_with_zero)

  # Variance = E[X^2] - E[X]^2
  ev_sq <- sum(p_all * payouts_with_zero^2)
  variance <- ev_sq - ev^2

  # Return standard deviation (more interpretable)
  sqrt(variance)
}

#' Calculate EV and variance for all teams in a bracket
#' @param teams Data frame with team, seed, and adjustment columns
#' @param seed_probs Seed probability matrix
calculate_bracket_ev <- function(teams, seed_probs) {
  teams %>%
    left_join(seed_probs, by = "seed") %>%
    rowwise() %>%
    mutate(
      # Adjust probabilities for this team
      adj_probs = list(adjust_team_probs(
        pick(starts_with("p_")),
        if_else(is.na(adjustment), 1, adjustment)
      )),
      # Calculate EV
      ev_pct = calculate_team_ev(adj_probs[[1]]),
      # Calculate variance
      variance = calculate_outcome_variance(adj_probs[[1]])
    ) %>%
    ungroup() %>%
    select(team, seed, adjustment, ev_pct, variance) %>%
    arrange(desc(ev_pct))
}

#' Generate EV summary by seed tier
summarize_ev_by_seed <- function(ev_data) {
  ev_data %>%
    group_by(seed) %>%
    summarize(
      n_teams = n(),
      avg_ev = mean(ev_pct),
      min_ev = min(ev_pct),
      max_ev = max(ev_pct),
      avg_variance = mean(variance),
      .groups = "drop"
    ) %>%
    arrange(seed)
}

#' Main function: Calculate EV for current year's bracket
#' @param bracket_file Optional CSV with current bracket (team, seed)
#' @param ratings_file Optional CSV with team ratings/adjustments
calculate_bracket_values <- function(bracket_file = NULL, ratings_file = NULL) {
  message("Calculating bracket expected values...")

  # Load seed probabilities
  seed_probs <- load_seed_probs()

  # If no bracket provided, just show seed-level EV
  if (is.null(bracket_file)) {
    message("\nNo bracket provided. Showing seed-level EV:")
    print(seed_probs %>% select(seed, ev_pct))

    message("\nTotal EV across all seeds (should be ~100%):")
    total_ev <- sum(seed_probs$ev_pct * 4)  # 4 teams per seed
    message(sprintf("  %.1f%%", total_ev))

    return(seed_probs)
  }

  # Load bracket
  bracket <- read_csv(bracket_file, show_col_types = FALSE)

  # Load ratings for adjustments if available
  if (!is.null(ratings_file) && file.exists(ratings_file)) {
    ratings <- read_csv(ratings_file, show_col_types = FALSE)
    # Merge adjustments
    bracket <- bracket %>%
      left_join(ratings %>% select(team, adjustment), by = "team")
  } else {
    bracket$adjustment <- 1.0
  }

  # Calculate EV for each team
  ev_results <- calculate_bracket_ev(bracket, seed_probs)

  # Save results
  save_data(ev_results, "team_expected_values.csv", "output")

  # Print summary
  message("\nTeam Expected Values:")
  print(ev_results)

  message("\nEV by Seed Tier:")
  print(summarize_ev_by_seed(ev_results))

  ev_results
}

#' Quick EV lookup for a specific seed
get_seed_ev <- function(seed) {
  probs <- load_seed_probs()
  probs %>%
    filter(seed == !!seed) %>%
    select(seed, starts_with("p_"), ev_pct)
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  ev <- calculate_bracket_values()
}
