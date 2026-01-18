# 09_fit_win_probability_model.R - Fit logistic regression for win probability
#
# Fits an empirical logistic regression model to predict win probability
# based on Barthag ratings. Validates model calibration and compares to
# theoretical formula.
#
# Input: data/processed/tournament_games_with_barthag.csv
# Output:
#   - data/processed/fitted_model_coefficients.json
#   - output/model_calibration.csv
#   - output/model_comparison.csv

library(tidyverse)
library(jsonlite)

source("R/utils.R")

#' Transform Barthag to log-odds
#' @param barthag Barthag rating (probability 0-1)
#' @return Log-odds (can be any real number)
barthag_to_log_odds <- function(barthag) {
  # Clamp to avoid infinite values
  barthag <- pmax(pmin(barthag, 0.9999), 0.0001)
  log(barthag / (1 - barthag))
}

#' Predict win probability using fitted model
#' @param barthag_a Team A's Barthag
#' @param barthag_b Team B's Barthag
#' @param intercept Model intercept
#' @param coefficient Model coefficient on log_odds_diff
#' @return Probability that team A wins (0-1)
predict_win_prob <- function(barthag_a, barthag_b, intercept, coefficient) {
  log_odds_a <- barthag_to_log_odds(barthag_a)
  log_odds_b <- barthag_to_log_odds(barthag_b)
  log_odds_diff <- log_odds_a - log_odds_b

  # Apply logistic function
  eta <- intercept + coefficient * log_odds_diff
  1 / (1 + exp(-eta))
}

#' Calculate theoretical win probability (assumes coef=1, intercept=0)
#' This is what you'd get if Barthag perfectly predicted outcomes
#' @param barthag_a Team A's Barthag
#' @param barthag_b Team B's Barthag
#' @return Theoretical probability that team A wins
theoretical_win_prob <- function(barthag_a, barthag_b) {
  # From Barthag definition: P(A beats B) = barthag_a * (1 - barthag_b) / denominator
  # But we use log-odds formulation with coef=1, intercept=0
  predict_win_prob(barthag_a, barthag_b, intercept = 0, coefficient = 1)
}

#' Fit logistic regression model
#' @param games Data frame with barthag_a, barthag_b, winner, team_a columns
#' @return Fitted glm model
fit_win_model <- function(games) {
  message("Fitting logistic regression model...")

  # Prepare features
  model_data <- games %>%
    mutate(
      log_odds_a = barthag_to_log_odds(barthag_a),
      log_odds_b = barthag_to_log_odds(barthag_b),
      log_odds_diff = log_odds_a - log_odds_b,
      winner_is_a = as.integer(winner == team_a)
    )

  # Fit model
  model <- glm(
    winner_is_a ~ log_odds_diff,
    family = binomial(link = "logit"),
    data = model_data
  )

  message("\nModel Summary:")
  print(summary(model))

  model
}

#' Validate model calibration
#' @param games Data frame with game data
#' @param model Fitted glm model
#' @return Data frame with calibration metrics by probability bin
validate_calibration <- function(games, model) {
  message("\nValidating model calibration...")

  # Get predictions - must compute log_odds_diff first, then predict separately
  model_data <- games %>%
    mutate(
      log_odds_diff = barthag_to_log_odds(barthag_a) - barthag_to_log_odds(barthag_b),
      winner_is_a = as.integer(winner == team_a)
    )

  # Now add predictions
  model_data <- model_data %>%
    mutate(
      pred_prob = predict(model, newdata = model_data, type = "response"),
      theoretical_prob = theoretical_win_prob(barthag_a, barthag_b)
    )

  # Bin by predicted probability
  calibration <- model_data %>%
    mutate(prob_bin = cut(pred_prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)) %>%
    group_by(prob_bin) %>%
    summarise(
      n_games = n(),
      mean_pred_prob = mean(pred_prob),
      actual_win_rate = mean(winner_is_a),
      mean_theoretical = mean(theoretical_prob),
      .groups = "drop"
    ) %>%
    mutate(
      calibration_error = actual_win_rate - mean_pred_prob,
      theoretical_error = actual_win_rate - mean_theoretical
    )

  calibration
}

#' Calculate model metrics
#' @param games Data frame with game data
#' @param model Fitted glm model
#' @return List of metrics (brier_score, log_loss, accuracy)
calculate_metrics <- function(games, model) {
  model_data <- games %>%
    mutate(
      log_odds_diff = barthag_to_log_odds(barthag_a) - barthag_to_log_odds(barthag_b),
      winner_is_a = as.integer(winner == team_a)
    )

  model_data <- model_data %>%
    mutate(
      pred_prob = predict(model, newdata = model_data, type = "response"),
      theoretical_prob = theoretical_win_prob(barthag_a, barthag_b)
    )

  # Brier score (lower is better)
  brier_fitted <- mean((model_data$pred_prob - model_data$winner_is_a)^2)
  brier_theoretical <- mean((model_data$theoretical_prob - model_data$winner_is_a)^2)

  # Log loss (lower is better)
  eps <- 1e-15
  log_loss_fitted <- -mean(
    model_data$winner_is_a * log(pmax(model_data$pred_prob, eps)) +
      (1 - model_data$winner_is_a) * log(pmax(1 - model_data$pred_prob, eps))
  )
  log_loss_theoretical <- -mean(
    model_data$winner_is_a * log(pmax(model_data$theoretical_prob, eps)) +
      (1 - model_data$winner_is_a) * log(pmax(1 - model_data$theoretical_prob, eps))
  )

  # Accuracy (fraction correct if we predict p > 0.5)
  accuracy_fitted <- mean((model_data$pred_prob > 0.5) == model_data$winner_is_a)
  accuracy_theoretical <- mean((model_data$theoretical_prob > 0.5) == model_data$winner_is_a)

  list(
    fitted = list(
      brier_score = brier_fitted,
      log_loss = log_loss_fitted,
      accuracy = accuracy_fitted
    ),
    theoretical = list(
      brier_score = brier_theoretical,
      log_loss = log_loss_theoretical,
      accuracy = accuracy_theoretical
    )
  )
}

#' Compare model to theoretical by round
#' @param games Data frame with game data
#' @param model Fitted glm model
#' @return Data frame with metrics by round
compare_by_round <- function(games, model) {
  if (!"round" %in% names(games) || all(is.na(games$round))) {
    message("No round information available")
    return(NULL)
  }

  model_data <- games %>%
    filter(!is.na(round)) %>%
    mutate(
      log_odds_diff = barthag_to_log_odds(barthag_a) - barthag_to_log_odds(barthag_b),
      winner_is_a = as.integer(winner == team_a)
    )

  model_data <- model_data %>%
    mutate(
      pred_prob = predict(model, newdata = model_data, type = "response"),
      theoretical_prob = theoretical_win_prob(barthag_a, barthag_b)
    )

  by_round <- model_data %>%
    group_by(round) %>%
    summarise(
      n_games = n(),
      brier_fitted = mean((pred_prob - winner_is_a)^2),
      brier_theoretical = mean((theoretical_prob - winner_is_a)^2),
      accuracy_fitted = mean((pred_prob > 0.5) == winner_is_a),
      accuracy_theoretical = mean((theoretical_prob > 0.5) == winner_is_a),
      avg_barthag_diff = mean(abs(barthag_a - barthag_b)),
      .groups = "drop"
    ) %>%
    mutate(brier_improvement = brier_theoretical - brier_fitted)

  by_round
}

#' Main function to fit and validate model
fit_and_validate <- function() {
  # Load tournament games
  games_file <- "data/processed/tournament_games_with_barthag.csv"

  if (!file.exists(games_file)) {
    stop(sprintf("Games file not found: %s\n  Run 08_pull_tournament_games.R first", games_file))
  }

  games <- read_csv(games_file, show_col_types = FALSE)
  message(sprintf("Loaded %d tournament games", nrow(games)))

  # Filter to games with complete data
  games <- games %>%
    filter(!is.na(barthag_a) & !is.na(barthag_b) & !is.na(winner))

  message(sprintf("After filtering: %d games with complete Barthag data", nrow(games)))

  # Fit model
  model <- fit_win_model(games)

  # Extract coefficients
  coefs <- coef(model)
  intercept <- coefs["(Intercept)"]
  coefficient <- coefs["log_odds_diff"]

  message("\n", rep("=", 50))
  message("Model Interpretation:")
  message(sprintf("  Intercept: %.4f (theoretical: 0)", intercept))
  message(sprintf("  Coefficient: %.4f (theoretical: 1)", coefficient))

  if (abs(intercept) < 0.1) {
    message("  -> Intercept near 0: No significant baseline bias")
  } else if (intercept > 0) {
    message("  -> Positive intercept: Higher seeds (team_a) have baseline advantage")
  } else {
    message("  -> Negative intercept: Lower seeds (team_b) have baseline advantage")
  }

  if (abs(coefficient - 1) < 0.1) {
    message("  -> Coefficient near 1: Barthag is well-calibrated for tournament games")
  } else if (coefficient < 1) {
    message("  -> Coefficient < 1: Barthag overestimates win probability differences")
  } else {
    message("  -> Coefficient > 1: Barthag underestimates win probability differences")
  }

  # Calibration
  calibration <- validate_calibration(games, model)
  message("\nCalibration by probability bin:")
  print(calibration)

  # Overall metrics
  metrics <- calculate_metrics(games, model)
  message("\n", rep("=", 50))
  message("Model Performance Metrics:")
  message("\nFitted Model:")
  message(sprintf("  Brier Score: %.4f", metrics$fitted$brier_score))
  message(sprintf("  Log Loss: %.4f", metrics$fitted$log_loss))
  message(sprintf("  Accuracy: %.1f%%", metrics$fitted$accuracy * 100))
  message("\nTheoretical (coef=1, intercept=0):")
  message(sprintf("  Brier Score: %.4f", metrics$theoretical$brier_score))
  message(sprintf("  Log Loss: %.4f", metrics$theoretical$log_loss))
  message(sprintf("  Accuracy: %.1f%%", metrics$theoretical$accuracy * 100))

  # Compare by round
  by_round <- compare_by_round(games, model)
  if (!is.null(by_round)) {
    message("\nPerformance by Round:")
    print(by_round)
  }

  # Save outputs
  ensure_dir("output")
  ensure_dir("data/processed")

  # Save coefficients
  model_output <- list(
    intercept = unname(intercept),
    coefficient = unname(coefficient),
    n_games = nrow(games),
    years = paste(range(games$year), collapse = "-"),
    metrics = metrics,
    fitted_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  write_json(model_output, "data/processed/fitted_model_coefficients.json", pretty = TRUE)
  message("\nSaved model coefficients to data/processed/fitted_model_coefficients.json")

  # Save calibration
  write_csv(calibration, "output/model_calibration.csv")
  message("Saved calibration to output/model_calibration.csv")

  # Save round comparison if available
  if (!is.null(by_round)) {
    write_csv(by_round, "output/model_by_round.csv")
    message("Saved round comparison to output/model_by_round.csv")
  }

  list(
    model = model,
    coefficients = model_output,
    calibration = calibration,
    metrics = metrics,
    by_round = by_round
  )
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")
  results <- fit_and_validate()
}
