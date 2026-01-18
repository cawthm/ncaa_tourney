# 08_pull_tournament_games.R - Generate tournament game training data
#
# Since game-level tournament data isn't easily available from toRvik,
# this script constructs training data by:
# 1. Using the known bracket structure (1v16, 2v15, etc.)
# 2. Pairing teams from the same year based on seed matchups
# 3. Using historical seed win rates to determine expected outcomes
#
# This allows us to fit the logistic model on Barthag differences
# and validate against known tournament statistics.

library(tidyverse)

source("R/utils.R")

#' Historical win rates by seed matchup (based on actual tournament data)
#' Format: higher_seed -> lower_seed -> win_probability for higher seed
HISTORICAL_WIN_RATES <- list(
  # R64 matchups
  "1_16" = 0.993,  # 1-seeds beat 16-seeds 99.3%
  "2_15" = 0.935,  # 2-seeds beat 15-seeds 93.5%
  "3_14" = 0.850,  # 3-seeds beat 14-seeds 85.0%
  "4_13" = 0.787,  # 4-seeds beat 13-seeds 78.7%
  "5_12" = 0.650,  # 5-seeds beat 12-seeds 65.0% (upset prone)
  "6_11" = 0.620,  # 6-seeds beat 11-seeds 62.0%
  "7_10" = 0.605,  # 7-seeds beat 10-seeds 60.5%
  "8_9" = 0.520    # 8-seeds beat 9-seeds 52.0%
)

#' Generate tournament game pairs for a single year
#' Creates R64 matchup pairs with Barthag ratings
generate_year_games <- function(year_data, year) {
  # Define R64 seed matchups
  r64_matchups <- tibble(
    high_seed = c(1, 2, 3, 4, 5, 6, 7, 8),
    low_seed = c(16, 15, 14, 13, 12, 11, 10, 9)
  )

  # For each region's worth of teams, pair by seed
  # Each year has 64 teams in 4 regions of 16 seeds each
  games <- list()

  # Create game for each matchup
  for (i in 1:nrow(r64_matchups)) {
    high <- r64_matchups$high_seed[i]
    low <- r64_matchups$low_seed[i]
    matchup_key <- paste0(high, "_", low)

    # Get all teams at these seeds
    high_teams <- year_data %>% filter(seed == high)
    low_teams <- year_data %>% filter(seed == low)

    # Pair teams (each high-seed team plays each low-seed team as a game sample)
    if (nrow(high_teams) > 0 && nrow(low_teams) > 0) {
      for (j in 1:min(nrow(high_teams), nrow(low_teams))) {
        high_team <- high_teams[j, ]
        low_team <- low_teams[j, ]

        # Historical win rate for this matchup
        p_high_wins <- HISTORICAL_WIN_RATES[[matchup_key]]

        games[[length(games) + 1]] <- tibble(
          year = year,
          round = "R64",
          team_a = high_team$team,
          team_b = low_team$team,
          seed_a = high,
          seed_b = low,
          barthag_a = high_team$barthag,
          barthag_b = low_team$barthag,
          p_a_wins_historical = p_high_wins,
          # Winner is determined by historical probability
          # (for fitting, we use the expected value)
          winner_is_a = p_high_wins  # Use continuous for fitting
        )
      }
    }
  }

  bind_rows(games)
}

#' Generate complete tournament game dataset
generate_tournament_games <- function() {
  # Load team ratings
  ratings_file <- "data/raw/team_ratings_with_seeds.csv"

  if (!file.exists(ratings_file)) {
    stop("Team ratings file not found. Run 01_data_acquisition.R first.")
  }

  ratings <- read_csv(ratings_file, show_col_types = FALSE)
  message(sprintf("Loaded %d team-seasons", nrow(ratings)))

  # Filter to tournament teams (those with seeds)
  tournament_teams <- ratings %>%
    filter(!is.na(seed))

  message(sprintf("Tournament teams: %d", nrow(tournament_teams)))

  # Generate games for each year (skip 2020)
  years <- unique(tournament_teams$year)
  years <- years[years != 2020]

  all_games <- map_dfr(years, function(yr) {
    year_data <- tournament_teams %>% filter(year == yr)
    generate_year_games(year_data, yr)
  })

  message(sprintf("Generated %d game pairs across %d years",
                  nrow(all_games), length(years)))

  # Also create a version with binary outcomes sampled from historical rates
  # This is useful for model fitting with actual W/L outcomes
  set.seed(42)  # For reproducibility
  all_games <- all_games %>%
    mutate(
      # Sample binary outcome based on historical probability
      winner_binary = rbinom(n(), 1, p_a_wins_historical),
      winner = if_else(winner_binary == 1, team_a, team_b)
    )

  all_games
}

#' Main function
main <- function() {
  message("Generating tournament game training data...")
  message(rep("=", 50))

  games <- generate_tournament_games()

  # Save output
  save_data(games, "tournament_games_with_barthag.csv")

  # Summary statistics
  message("\n", rep("=", 50))
  message("Summary:")
  message(sprintf("  Total games: %d", nrow(games)))
  message(sprintf("  Years: %s", paste(range(games$year), collapse = " - ")))

  # Check Barthag relationship
  games <- games %>%
    mutate(
      log_odds_a = log(barthag_a / (1 - barthag_a)),
      log_odds_b = log(barthag_b / (1 - barthag_b)),
      log_odds_diff = log_odds_a - log_odds_b
    )

  # Correlation between Barthag diff and historical win rate
  cor_test <- cor.test(games$log_odds_diff, games$p_a_wins_historical)
  message(sprintf("  Correlation (log-odds diff vs win rate): %.3f (p < %.3g)",
                  cor_test$estimate, cor_test$p.value))

  # Show sample
  message("\nSample games:")
  print(head(games %>% select(year, team_a, team_b, seed_a, seed_b,
                               barthag_a, barthag_b, winner), 10))

  games
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  games <- main()
}
