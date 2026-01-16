# 02_seed_analysis.R - Analyze historical seed performance
#
# Calculate probability of each seed reaching each tournament round
# based on historical data

library(tidyverse)
source("R/utils.R")

#' Process tournament games to get team outcomes
#' Determines how far each team advanced in each tournament
process_tournament_outcomes <- function(games) {
  # Assuming games has: year, team, opponent, team_score, opp_score, round, seed
  # We need to determine wins per team per tournament

  # If we have win/loss data directly
  if ("result" %in% names(games)) {
    outcomes <- games %>%
      group_by(year, team, seed) %>%
      summarize(
        wins = sum(result == "W"),
        .groups = "drop"
      )
  } else if (all(c("team_score", "opp_score") %in% names(games))) {
    # Calculate from scores
    outcomes <- games %>%
      mutate(win = team_score > opp_score) %>%
      group_by(year, team, seed) %>%
      summarize(
        wins = sum(win),
        .groups = "drop"
      )
  } else {
    # Fallback: try to infer from game structure
    warning("Could not determine win/loss from game data. Check column names.")
    return(NULL)
  }

  outcomes
}

#' Calculate seed progression probabilities from historical data
#' @param outcomes Data frame with year, team, seed, wins columns
#' @return Data frame with seed and probability of reaching each round
calculate_seed_probabilities <- function(outcomes) {
  # For each seed, calculate proportion reaching each round
  # wins = 0 means lost in R64
  # wins = 1 means reached R32 (beat R64)
  # wins = 2 means reached S16
  # wins = 3 means reached E8
  # wins = 4 means reached F4
  # wins = 5 means reached Final
  # wins = 6 means Champion

  seed_probs <- outcomes %>%
    group_by(seed) %>%
    summarize(
      n_teams = n(),
      # P(at least N wins) = proportion with wins >= N
      p_R64 = mean(wins >= 1),   # Win at least 1 (reach R32)
      p_R32 = mean(wins >= 2),   # Win at least 2 (reach S16)
      p_S16 = mean(wins >= 3),   # Win at least 3 (reach E8)
      p_E8 = mean(wins >= 4),    # Win at least 4 (reach F4)
      p_F4 = mean(wins >= 5),    # Win at least 5 (reach Final)
      p_CHAMP = mean(wins >= 6), # Win 6 (Champion)
      .groups = "drop"
    ) %>%
    arrange(seed)

  seed_probs
}

#' Use well-known historical seed probabilities as fallback/validation
#' Based on historical NCAA tournament data (1985-2024)
#' Normalized so that total payouts sum to exactly 100%
get_historical_seed_probs <- function() {
  # Raw historical averages (approximate)
  raw_probs <- tribble(
    ~seed, ~p_R64, ~p_R32, ~p_S16, ~p_E8, ~p_F4, ~p_CHAMP,
    1,  0.993, 0.853, 0.600, 0.420, 0.200, 0.105,
    2,  0.940, 0.720, 0.480, 0.300, 0.140, 0.055,
    3,  0.850, 0.580, 0.350, 0.180, 0.070, 0.025,
    4,  0.790, 0.500, 0.280, 0.130, 0.050, 0.020,
    5,  0.650, 0.380, 0.190, 0.080, 0.025, 0.010,
    6,  0.620, 0.350, 0.160, 0.060, 0.020, 0.008,
    7,  0.600, 0.320, 0.140, 0.055, 0.018, 0.007,
    8,  0.500, 0.240, 0.100, 0.035, 0.012, 0.004,
    9,  0.500, 0.220, 0.090, 0.030, 0.010, 0.003,
    10, 0.400, 0.180, 0.070, 0.025, 0.008, 0.002,
    11, 0.380, 0.170, 0.065, 0.022, 0.007, 0.002,
    12, 0.350, 0.140, 0.050, 0.015, 0.005, 0.001,
    13, 0.210, 0.060, 0.015, 0.004, 0.001, 0.000,
    14, 0.150, 0.035, 0.008, 0.002, 0.000, 0.000,
    15, 0.070, 0.015, 0.003, 0.001, 0.000, 0.000,
    16, 0.010, 0.002, 0.000, 0.000, 0.000, 0.000
  )

  # Normalize so probabilities sum correctly:
  # 4 teams/seed × 16 seeds × P(round) = expected teams reaching that round
  # p_R32 (reach S16): 4 * sum = 16 → sum should be 4
  # p_S16 (reach E8):  4 * sum = 8  → sum should be 2
  # p_E8 (reach F4):   4 * sum = 4  → sum should be 1
  # p_F4 (reach Final): 4 * sum = 2 → sum should be 0.5
  # p_CHAMP:           4 * sum = 1  → sum should be 0.25

  target_sums <- c(p_R32 = 4, p_S16 = 2, p_E8 = 1, p_F4 = 0.5, p_CHAMP = 0.25)

  for (col in names(target_sums)) {
    current_sum <- sum(raw_probs[[col]])
    if (current_sum > 0) {
      raw_probs[[col]] <- raw_probs[[col]] * target_sums[col] / current_sum
    }
  }

  raw_probs
}

#' Calculate expected value for each seed
#' @param seed_probs Data frame from calculate_seed_probabilities
#' @return Data frame with EV added
add_expected_values <- function(seed_probs) {
  seed_probs %>%
    mutate(
      ev_pct = p_R64 * PAYOUTS["R64"] +
               p_R32 * PAYOUTS["R32"] +
               p_S16 * PAYOUTS["S16"] +
               p_E8 * PAYOUTS["E8"] +
               p_F4 * PAYOUTS["F4"] +
               p_CHAMP * PAYOUTS["CHAMP"],
      ev_pct = ev_pct * 100  # Convert to percentage
    )
}

#' Main seed analysis function
analyze_seeds <- function(tournament_file = "data/processed/tournament_games.csv") {
  message("Starting seed analysis...")

  # Try to load processed tournament data
  if (file.exists(tournament_file)) {
    games <- read_csv(tournament_file, show_col_types = FALSE)
    outcomes <- process_tournament_outcomes(games)

    if (!is.null(outcomes)) {
      seed_probs <- calculate_seed_probabilities(outcomes)
      message("Calculated seed probabilities from tournament data")
    } else {
      message("Using historical reference probabilities")
      seed_probs <- get_historical_seed_probs()
    }
  } else {
    message("No tournament data found. Using historical reference probabilities.")
    seed_probs <- get_historical_seed_probs()
  }

  # Add expected values
  seed_probs <- add_expected_values(seed_probs)

  # Save results
  save_data(seed_probs, "seed_progression_probs.csv", "output")

  # Print summary
  message("\nSeed Progression Probabilities:")
  print(seed_probs %>% select(seed, p_R64, p_S16, p_F4, p_CHAMP, ev_pct))

  seed_probs
}

#' Visualize seed probabilities
plot_seed_probs <- function(seed_probs) {
  # Reshape for plotting
  probs_long <- seed_probs %>%
    select(seed, starts_with("p_")) %>%
    pivot_longer(
      cols = starts_with("p_"),
      names_to = "round",
      names_prefix = "p_",
      values_to = "probability"
    ) %>%
    mutate(round = factor(round, levels = ROUND_NAMES))

  ggplot(probs_long, aes(x = seed, y = probability, fill = round)) +
    geom_col(position = "dodge") +
    scale_x_continuous(breaks = 1:16) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "NCAA Tournament: Probability of Reaching Each Round by Seed",
      x = "Seed",
      y = "Probability",
      fill = "Round"
    ) +
    theme_minimal()
}

#' Plot expected value by seed
plot_seed_ev <- function(seed_probs) {
  ggplot(seed_probs, aes(x = seed, y = ev_pct)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = sprintf("%.1f%%", ev_pct)), vjust = -0.3, size = 3) +
    scale_x_continuous(breaks = 1:16) +
    labs(
      title = "Expected Value by Seed (Calcutta Payout Structure)",
      subtitle = "Cumulative: S16=1.5%, E8=3%, F4=8%, Final=20%, Champ=40%",
      x = "Seed",
      y = "Expected Value (% of Pot)"
    ) +
    theme_minimal()
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  seed_probs <- analyze_seeds()
}
