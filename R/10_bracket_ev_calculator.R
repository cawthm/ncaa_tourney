# 10_bracket_ev_calculator.R - Bracket-aware EV calculation
#
# Calculates expected value for each team through the actual bracket structure,
# using the fitted win probability model and specific opponent Barthag ratings.
#
# Key insight: Instead of using average seed performance, we calculate
# exact probabilities through the bracket based on actual team strengths.

library(tidyverse)
library(jsonlite)

source("R/utils.R")

#' Load fitted model coefficients
#' @return List with intercept and coefficient
load_model <- function() {
  model_file <- "data/processed/fitted_model_coefficients.json"

  if (!file.exists(model_file)) {
    warning("Model file not found, using theoretical values (intercept=0, coefficient=1)")
    return(list(intercept = 0, coefficient = 1))
  }

  fromJSON(model_file)
}

#' Transform Barthag to log-odds
barthag_to_log_odds <- function(barthag) {
  barthag <- pmax(pmin(barthag, 0.9999), 0.0001)
  log(barthag / (1 - barthag))
}

#' Calculate win probability for team A vs team B using fitted model
#' @param barthag_a Team A's Barthag rating
#' @param barthag_b Team B's Barthag rating
#' @param model List with intercept and coefficient
#' @return Probability that team A wins (0-1)
calc_win_prob <- function(barthag_a, barthag_b, model) {
  log_odds_diff <- barthag_to_log_odds(barthag_a) - barthag_to_log_odds(barthag_b)
  eta <- model$intercept + model$coefficient * log_odds_diff
  1 / (1 + exp(-eta))
}

#' Define bracket structure
#' Returns which seeds can meet in each round
#' @return List of matchup possibilities by round
get_bracket_structure <- function() {
  list(
    # R64: Fixed first-round matchups
    R64 = list(
      "1" = 16,
      "2" = 15,
      "3" = 14,
      "4" = 13,
      "5" = 12,
      "6" = 11,
      "7" = 10,
      "8" = 9
    ),

    # R32: Who each seed could face
    R32 = list(
      "1" = c(8, 9),    # 1-seed faces winner of 8/9
      "2" = c(7, 10),   # 2-seed faces winner of 7/10
      "3" = c(6, 11),   # 3-seed faces winner of 6/11
      "4" = c(5, 12),   # 4-seed faces winner of 5/12
      "5" = c(4, 13),   # 5-seed faces 4-seed bracket winner
      "6" = c(3, 14),   # 6-seed faces 3-seed bracket winner
      "7" = c(2, 15),   # 7-seed faces 2-seed bracket winner
      "8" = c(1, 16),   # 8-seed faces 1-seed bracket winner
      "9" = c(1, 16),   # 9-seed faces 1-seed bracket winner
      "10" = c(2, 15),
      "11" = c(3, 14),
      "12" = c(4, 13),
      "13" = c(4, 5, 12),  # 13 has beaten 4, so faces 5/12 winner
      "14" = c(3, 6, 11),
      "15" = c(2, 7, 10),
      "16" = c(1, 8, 9)
    ),

    # S16: Seeds from opposite side of regional bracket
    S16 = list(
      top_half = c(1, 8, 9, 16),   # 1-seed region
      bottom_half = c(4, 5, 12, 13) # 4-seed region
    ),

    # E8: Regional finals
    E8 = list(
      top_quarter = c(1, 8, 9, 16, 4, 5, 12, 13),
      bottom_quarter = c(2, 7, 10, 15, 3, 6, 11, 14)
    )
  )
}

#' Calculate EV for a team through the bracket
#' @param team_barthag The team's Barthag rating
#' @param team_seed The team's seed (1-16)
#' @param region_barthags Named vector of Barthag ratings for all teams in region
#'        Names should be seed numbers as strings ("1", "2", ..., "16")
#' @param model Fitted model (list with intercept, coefficient)
#' @return List with probabilities for each round and total EV
calc_team_ev <- function(team_barthag, team_seed, region_barthags, model) {
  seed_str <- as.character(team_seed)

  # Helper: get opponent's Barthag by seed
  get_barthag <- function(seed) {
    region_barthags[[as.character(seed)]]
  }

  # Initialize probabilities
  probs <- list()

  # ========== R64 ==========
  # Known opponent based on seed
  r64_opp_seed <- switch(seed_str,
    "1" = 16, "2" = 15, "3" = 14, "4" = 13,
    "5" = 12, "6" = 11, "7" = 10, "8" = 9,
    "9" = 8, "10" = 7, "11" = 6, "12" = 5,
    "13" = 4, "14" = 3, "15" = 2, "16" = 1
  )

  probs$R64 <- calc_win_prob(team_barthag, get_barthag(r64_opp_seed), model)

  # ========== R32 ==========
  # Calculate probability of reaching R32 and winning
  # Depends on who wins the adjacent first-round game

  # Define bracket pairs (who plays in R32)
  r32_matchups <- list(
    "1" = c(8, 9),   # 1 plays winner of 8/9
    "16" = c(8, 9),  # 16 plays winner of 8/9
    "8" = c(1, 16),  # 8 plays winner of 1/16
    "9" = c(1, 16),  # 9 plays winner of 1/16
    "4" = c(5, 12),
    "13" = c(5, 12),
    "5" = c(4, 13),
    "12" = c(4, 13),
    "3" = c(6, 11),
    "14" = c(6, 11),
    "6" = c(3, 14),
    "11" = c(3, 14),
    "2" = c(7, 10),
    "15" = c(7, 10),
    "7" = c(2, 15),
    "10" = c(2, 15)
  )

  potential_opps <- r32_matchups[[seed_str]]
  opp1 <- potential_opps[1]
  opp2 <- potential_opps[2]

  # P(opp1 advances) = P(opp1 beats their R64 opponent)
  opp1_r64_opp <- switch(as.character(opp1),
    "1" = 16, "2" = 15, "3" = 14, "4" = 13, "5" = 12, "6" = 11, "7" = 10, "8" = 9,
    "9" = 8, "10" = 7, "11" = 6, "12" = 5, "13" = 4, "14" = 3, "15" = 2, "16" = 1
  )

  p_opp1_advances <- calc_win_prob(get_barthag(opp1), get_barthag(opp1_r64_opp), model)
  p_opp2_advances <- 1 - p_opp1_advances  # They play each other

  p_beat_opp1 <- calc_win_prob(team_barthag, get_barthag(opp1), model)
  p_beat_opp2 <- calc_win_prob(team_barthag, get_barthag(opp2), model)

  probs$R32 <- probs$R64 * (p_opp1_advances * p_beat_opp1 + p_opp2_advances * p_beat_opp2)

  # ========== S16 ==========
  # More complex: need to consider all possible opponents from adjacent bracket

  # Define S16 pairings (which first-round bracket pairs meet in S16)
  s16_pairings <- list(
    # Top half (1-seed side) vs 4-seed side
    "1" = c(4, 5, 12, 13),
    "16" = c(4, 5, 12, 13),
    "8" = c(4, 5, 12, 13),
    "9" = c(4, 5, 12, 13),
    "4" = c(1, 8, 9, 16),
    "13" = c(1, 8, 9, 16),
    "5" = c(1, 8, 9, 16),
    "12" = c(1, 8, 9, 16),
    # Bottom half (2-seed side) vs 3-seed side
    "2" = c(3, 6, 11, 14),
    "15" = c(3, 6, 11, 14),
    "7" = c(3, 6, 11, 14),
    "10" = c(3, 6, 11, 14),
    "3" = c(2, 7, 10, 15),
    "14" = c(2, 7, 10, 15),
    "6" = c(2, 7, 10, 15),
    "11" = c(2, 7, 10, 15)
  )

  # Calculate probability each potential S16 opponent reaches S16
  potential_s16_opps <- s16_pairings[[seed_str]]

  # For each potential S16 opponent, calculate their probability of reaching S16
  s16_opp_probs <- sapply(potential_s16_opps, function(opp_seed) {
    opp_barthag <- get_barthag(opp_seed)
    opp_str <- as.character(opp_seed)

    # Their R64 opponent
    opp_r64 <- switch(opp_str,
      "1" = 16, "2" = 15, "3" = 14, "4" = 13, "5" = 12, "6" = 11, "7" = 10, "8" = 9,
      "9" = 8, "10" = 7, "11" = 6, "12" = 5, "13" = 4, "14" = 3, "15" = 2, "16" = 1
    )

    p_r64 <- calc_win_prob(opp_barthag, get_barthag(opp_r64), model)

    # Their R32 opponents
    opp_r32_pair <- r32_matchups[[opp_str]]
    r32_1 <- opp_r32_pair[1]
    r32_2 <- opp_r32_pair[2]

    r32_1_r64_opp <- switch(as.character(r32_1),
      "1" = 16, "2" = 15, "3" = 14, "4" = 13, "5" = 12, "6" = 11, "7" = 10, "8" = 9,
      "9" = 8, "10" = 7, "11" = 6, "12" = 5, "13" = 4, "14" = 3, "15" = 2, "16" = 1
    )

    p_r32_1_advances <- calc_win_prob(get_barthag(r32_1), get_barthag(r32_1_r64_opp), model)
    p_r32_2_advances <- 1 - p_r32_1_advances

    p_beat_r32_1 <- calc_win_prob(opp_barthag, get_barthag(r32_1), model)
    p_beat_r32_2 <- calc_win_prob(opp_barthag, get_barthag(r32_2), model)

    p_r32 <- p_r64 * (p_r32_1_advances * p_beat_r32_1 + p_r32_2_advances * p_beat_r32_2)
    p_r32
  })

  # Normalize probabilities (they should sum to 1 for the 4 potential opponents)
  s16_opp_probs <- s16_opp_probs / sum(s16_opp_probs)

  p_beat_s16_opps <- sapply(potential_s16_opps, function(opp_seed) {
    calc_win_prob(team_barthag, get_barthag(opp_seed), model)
  })

  probs$S16 <- probs$R32 * sum(s16_opp_probs * p_beat_s16_opps)

  # ========== E8 (Regional Final) ==========
  # Even more opponents possible - use simplified weighted average

  # Opponents are from the other half of the region
  e8_pairings <- list(
    "1" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "16" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "8" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "9" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "4" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "5" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "12" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "13" = c(2, 3, 6, 7, 10, 11, 14, 15),
    "2" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "3" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "6" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "7" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "10" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "11" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "14" = c(1, 4, 5, 8, 9, 12, 13, 16),
    "15" = c(1, 4, 5, 8, 9, 12, 13, 16)
  )

  potential_e8_opps <- e8_pairings[[seed_str]]

  # Approximate: weight by historical likelihood (favor higher seeds)
  # This is a simplification - full calculation would be recursive
  e8_weights <- sapply(potential_e8_opps, function(s) {
    # Higher seeds more likely to reach E8
    w <- switch(as.character(s),
      "1" = 0.5, "2" = 0.35, "3" = 0.25, "4" = 0.2,
      "5" = 0.12, "6" = 0.1, "7" = 0.08, "8" = 0.06,
      "9" = 0.06, "10" = 0.05, "11" = 0.07, "12" = 0.06,
      "13" = 0.02, "14" = 0.02, "15" = 0.01, "16" = 0.001,
      0.05  # default
    )
    w
  })
  e8_weights <- e8_weights / sum(e8_weights)

  p_beat_e8_opps <- sapply(potential_e8_opps, function(opp_seed) {
    calc_win_prob(team_barthag, get_barthag(opp_seed), model)
  })

  probs$E8 <- probs$S16 * sum(e8_weights * p_beat_e8_opps)

  # ========== F4 (National Semi-final) ==========
  # Face teams from other regions - use league-wide average for simplicity
  # Could be enhanced with actual other region data

  # For now, assume facing average of expected F4 opponents (mostly 1-3 seeds)
  avg_f4_opponent_barthag <- 0.94  # Approximate for typical F4 team

  probs$F4 <- probs$E8 * calc_win_prob(team_barthag, avg_f4_opponent_barthag, model)

  # ========== Championship ==========
  probs$CHAMP <- probs$F4 * calc_win_prob(team_barthag, avg_f4_opponent_barthag, model)

  # Calculate EV
  ev <- sum(c(
    probs$R64 * 0,       # R64 win has no payout
    probs$R32 * PAYOUTS["R32"],   # Reach S16
    probs$S16 * PAYOUTS["S16"],   # Reach E8
    probs$E8 * PAYOUTS["E8"],     # Reach F4
    probs$F4 * PAYOUTS["F4"],     # Reach Championship
    probs$CHAMP * PAYOUTS["CHAMP"] # Win Championship
  ))

  list(
    probs = probs,
    ev = ev,
    seed = team_seed,
    barthag = team_barthag
  )
}

#' Calculate EV for all teams in a region
#' @param region_teams Data frame with columns: seed, team, barthag
#' @param model Fitted model
#' @return Data frame with team, seed, barthag, probabilities, and EV
calc_region_ev <- function(region_teams, model) {
  # Create named vector of Barthag ratings
  region_barthags <- setNames(region_teams$barthag, as.character(region_teams$seed))

  # Calculate EV for each team
  results <- region_teams %>%
    rowwise() %>%
    mutate(
      ev_result = list(calc_team_ev(barthag, seed, region_barthags, model)),
      p_r64 = ev_result$probs$R64,
      p_r32 = ev_result$probs$R32,
      p_s16 = ev_result$probs$S16,
      p_e8 = ev_result$probs$E8,
      p_f4 = ev_result$probs$F4,
      p_champ = ev_result$probs$CHAMP,
      ev = ev_result$ev
    ) %>%
    ungroup() %>%
    select(-ev_result)

  results
}

#' Calculate EV for entire bracket
#' @param bracket Data frame with columns: region, seed, team, barthag
#' @param model Fitted model (optional, loads from file if not provided)
#' @return Data frame with all teams and their EVs
calc_bracket_ev <- function(bracket, model = NULL) {
  if (is.null(model)) {
    model <- load_model()
  }

  # Calculate for each region
  results <- bracket %>%
    group_by(region) %>%
    group_modify(~ calc_region_ev(.x, model)) %>%
    ungroup()

  # Verify EVs sum to ~100%
  total_ev <- sum(results$ev)
  message(sprintf("Total EV across all teams: %.2f%% (should be ~100%%)", total_ev * 100))

  results
}

#' Demo: Calculate EV for a sample bracket
demo_bracket_ev <- function() {
  # Sample bracket with placeholder Barthag values
  sample_bracket <- tibble(
    region = rep(c("East", "South", "West", "Midwest"), each = 16),
    seed = rep(1:16, 4),
    team = paste0("Team_", region, "_", seed),
    # Typical Barthag values by seed
    barthag = rep(c(
      0.96, 0.93, 0.91, 0.88, 0.86, 0.83, 0.80, 0.78,
      0.76, 0.74, 0.72, 0.70, 0.68, 0.65, 0.62, 0.55
    ), 4)
  )

  model <- load_model()
  results <- calc_bracket_ev(sample_bracket, model)

  message("\nSample EV by seed:")
  results %>%
    group_by(seed) %>%
    summarise(
      avg_ev = mean(ev) * 100,
      min_ev = min(ev) * 100,
      max_ev = max(ev) * 100,
      .groups = "drop"
    ) %>%
    print()

  results
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  demo_bracket_ev()
}
