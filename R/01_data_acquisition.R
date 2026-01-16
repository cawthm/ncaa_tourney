# 01_data_acquisition.R - Pull historical NCAA tournament data
#
# Data sources:
# - toRvik package: Team ratings and advanced metrics (2008-present)
# - Tournament results: Historical bracket outcomes

library(tidyverse)

# Install toRvik if needed
if (!requireNamespace("toRvik", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("andreweatherman/toRvik")
}

library(toRvik)

source("R/utils.R")

# Configuration
START_YEAR <- 2008
END_YEAR <- 2024  # Most recent completed tournament

#' Fetch team ratings for all years
#' Uses Bart Torvik's ratings (free KenPom alternative)
fetch_team_ratings <- function(start_year = START_YEAR, end_year = END_YEAR) {
  message(sprintf("Fetching team ratings from %d to %d...", start_year, end_year))

  all_ratings <- map_dfr(start_year:end_year, function(year) {
    message(sprintf("  Fetching %d...", year))
    tryCatch({
      ratings <- bart_ratings(year = year)
      ratings$year <- year
      ratings
    }, error = function(e) {
      warning(sprintf("Failed to fetch %d: %s", year, e$message))
      NULL
    })
  })

  message(sprintf("Fetched %d team-seasons", nrow(all_ratings)))
  all_ratings
}

#' Fetch tournament results
#' Gets bracket outcomes for historical tournaments
fetch_tournament_results <- function(start_year = START_YEAR, end_year = END_YEAR) {
  message(sprintf("Fetching tournament results from %d to %d...", start_year, end_year))

  # Try bart_tourney_results or similar function
  # The exact function name may vary - we'll try a few approaches

  all_results <- map_dfr(start_year:end_year, function(year) {
    message(sprintf("  Fetching %d...", year))
    tryCatch({
      # Try different possible function names
      results <- NULL

      # Approach 1: bart_tourney_results
      if (exists("bart_tourney_results", mode = "function")) {
        results <- bart_tourney_results(year = year)
      }

      # Approach 2: bart_tournament_results
      if (is.null(results) && exists("bart_tournament_results", mode = "function")) {
        results <- bart_tournament_results(year = year)
      }

      # Approach 3: Get game data and filter to tournament
      if (is.null(results)) {
        games <- bart_game_box(year = year, type = "NCAA")
        if (!is.null(games) && nrow(games) > 0) {
          results <- games
        }
      }

      if (!is.null(results)) {
        results$year <- year
      }
      results
    }, error = function(e) {
      warning(sprintf("Failed to fetch tournament %d: %s", year, e$message))
      NULL
    })
  })

  message(sprintf("Fetched %d tournament records", nrow(all_results)))
  all_results
}

#' Fetch NCAA tournament seeds and results via game schedule
fetch_tournament_seeds <- function(start_year = START_YEAR, end_year = END_YEAR) {
  message(sprintf("Fetching tournament seeds from %d to %d...", start_year, end_year))

  all_seeds <- map_dfr(start_year:end_year, function(year) {
    message(sprintf("  Fetching %d...", year))
    tryCatch({
      # Get tournament games
      games <- bart_season_schedule(year = year, type = "NCAA")

      if (!is.null(games) && nrow(games) > 0) {
        games$year <- year
      }
      games
    }, error = function(e) {
      warning(sprintf("Failed to fetch seeds %d: %s", year, e$message))
      NULL
    })
  })

  all_seeds
}

#' Main data acquisition function
#' Pulls all data and saves to processed directory
acquire_all_data <- function() {
  message("Starting data acquisition...")
  message(rep("=", 50))

  # Fetch team ratings
  ratings <- fetch_team_ratings()
  if (!is.null(ratings) && nrow(ratings) > 0) {
    save_data(ratings, "team_ratings.csv")
  }

  # Fetch tournament results
  tourney <- fetch_tournament_seeds()
  if (!is.null(tourney) && nrow(tourney) > 0) {
    save_data(tourney, "tournament_games.csv")
  }

  message(rep("=", 50))
  message("Data acquisition complete!")

  list(ratings = ratings, tournament = tourney)
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  data <- acquire_all_data()
}
