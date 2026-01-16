# 05_bid_strategy.R - Translate expected value to bidding strategy
#
# Accounts for:
# - Risk/variance
# - Budget constraints
# - Portfolio diversification
# - Market inefficiencies

library(tidyverse)
source("R/utils.R")

#' Calculate risk-adjusted bid based on variance
#' Risk-averse bidders should discount high-variance teams
#' @param ev Expected value (% of pot)
#' @param variance Variance of outcome
#' @param risk_aversion Risk aversion coefficient (0 = neutral, 1 = very averse)
#' @return Risk-adjusted value
risk_adjust_value <- function(ev, variance, risk_aversion = 0.5) {
  # Simple mean-variance utility: U = E[X] - (lambda/2) * Var[X]
  # Higher risk_aversion = more discount for variance
  adjusted <- ev - (risk_aversion / 2) * (variance * 100)^2

  # Don't go negative
  pmax(adjusted, 0)
}

#' Kelly criterion adaptation for auction bidding
#' Optimal fraction of bankroll to risk
#' @param ev Expected value as proportion (not percentage)
#' @param variance Variance of outcome
#' @param edge Your estimated edge over the market (0 = none)
#' @return Optimal bid as fraction of total pot
kelly_bid <- function(ev, variance, edge = 0) {
  # In pure Kelly: f* = (p*b - q) / b where b is odds, p is prob, q = 1-p
  # For auction context, we adapt: bid fraction based on EV and variance

  # Simple approximation: bid proportional to EV, scaled by edge
  ev_adj <- ev * (1 + edge)

  # Kelly suggests betting proportional to edge/variance
  # But in auction context, we're buying fixed payoffs

  # Return EV as maximum sensible bid
  # (bidding more than EV is negative expected value)
  ev_adj
}

#' Calculate bid range for a team
#' @param ev Expected value (% of pot)
#' @param variance Variance of outcome
#' @param risk_tolerance "low", "medium", "high"
#' @return List with min_bid, target_bid, max_bid
calculate_bid_range <- function(ev, variance, risk_tolerance = "medium") {
  # Base discount factors by risk tolerance
  discount <- switch(risk_tolerance,
    "low"    = c(min = 0.50, target = 0.65, max = 0.80),
    "medium" = c(min = 0.60, target = 0.75, max = 0.90),
    "high"   = c(min = 0.70, target = 0.85, max = 0.95)
  )

  # Additional variance adjustment
  # High variance teams get extra discount
  var_penalty <- pmin(variance * 10, 0.15)  # Up to 15% additional discount

  adjusted_discount <- discount * (1 - var_penalty)

  list(
    min_bid = ev * adjusted_discount["min"],
    target_bid = ev * adjusted_discount["target"],
    max_bid = ev * adjusted_discount["max"],
    ev = ev,
    variance = variance
  )
}

#' Estimate market prices for each seed
#' Based on common behavioral biases in auctions
estimate_market_prices <- function(seed_ev) {
  # Common biases:
  # - 1-seeds often overbid (prestige, FOMO)
  # - Mid-seeds (5-8) often underbid
  # - Cinderella seeds (11-14) can be overbid due to upset potential
  # - 15-16 seeds usually underbid (seen as hopeless)

  market_adjustment <- tribble(
    ~seed, ~market_factor,
    1,  1.20,   # Overbid by ~20%
    2,  1.15,
    3,  1.10,
    4,  1.05,
    5,  0.95,   # Slightly underbid
    6,  0.95,
    7,  0.90,   # Underbid
    8,  0.85,   # Often underbid (tough first round)
    9,  0.85,
    10, 0.90,
    11, 1.05,   # Cinderella appeal
    12, 1.05,
    13, 0.90,
    14, 0.85,
    15, 0.80,   # Underbid
    16, 0.70    # Severely underbid
  )

  seed_ev %>%
    left_join(market_adjustment, by = "seed") %>%
    mutate(
      estimated_market_price = ev_pct * market_factor,
      value_gap = ev_pct - estimated_market_price  # Positive = undervalued
    )
}

#' Identify value opportunities
#' Teams where market price likely < true value
find_value_opportunities <- function(ev_data) {
  ev_with_market <- estimate_market_prices(ev_data)

  ev_with_market %>%
    mutate(
      opportunity_score = value_gap / ev_pct,  # Relative undervaluation
      recommendation = case_when(
        opportunity_score > 0.10 ~ "Strong Buy",
        opportunity_score > 0.05 ~ "Buy",
        opportunity_score > -0.05 ~ "Fair Value",
        opportunity_score > -0.10 ~ "Avoid",
        TRUE ~ "Strong Avoid"
      )
    ) %>%
    arrange(desc(opportunity_score))
}

#' Build optimal portfolio given budget
#' @param ev_data Data frame with team, seed, ev_pct, bid columns
#' @param budget Total budget as % of pot (typically 100/n_participants)
#' @param max_teams Maximum teams to bid on
build_portfolio <- function(ev_data, budget, max_teams = 10) {
  # Simple greedy approach: buy best value until budget exhausted
  # More sophisticated would use integer programming

  ev_sorted <- ev_data %>%
    filter(!is.na(target_bid)) %>%
    arrange(desc(ev_pct / target_bid))  # Best EV per dollar

  portfolio <- tibble()
  remaining_budget <- budget
  teams_bought <- 0

  for (i in seq_len(nrow(ev_sorted))) {
    team <- ev_sorted[i, ]

    if (team$target_bid <= remaining_budget && teams_bought < max_teams) {
      portfolio <- bind_rows(portfolio, team)
      remaining_budget <- remaining_budget - team$target_bid
      teams_bought <- teams_bought + 1
    }
  }

  list(
    portfolio = portfolio,
    total_cost = budget - remaining_budget,
    total_ev = sum(portfolio$ev_pct),
    expected_roi = (sum(portfolio$ev_pct) / (budget - remaining_budget) - 1) * 100
  )
}

#' Generate bid sheet for auction
#' @param ev_data Data from calculate_bracket_values
#' @param pot_size Total pot size in dollars
#' @param risk_tolerance "low", "medium", "high"
generate_bid_sheet <- function(ev_data = NULL, pot_size = 1000, risk_tolerance = "medium") {
  # Load seed probabilities if no data provided
  if (is.null(ev_data)) {
    source("R/02_seed_analysis.R")
    ev_data <- get_historical_seed_probs() %>%
      add_expected_values() %>%
      mutate(variance = 0.05)  # Default variance estimate
  }

  # Calculate bid ranges for each entry
  bid_sheet <- ev_data %>%
    rowwise() %>%
    mutate(
      bid_info = list(calculate_bid_range(ev_pct, variance, risk_tolerance)),
      min_bid = bid_info$min_bid,
      target_bid = bid_info$target_bid,
      max_bid = bid_info$max_bid
    ) %>%
    ungroup() %>%
    select(-bid_info) %>%
    # Add dollar amounts
    mutate(
      min_bid_dollars = min_bid / 100 * pot_size,
      target_bid_dollars = target_bid / 100 * pot_size,
      max_bid_dollars = max_bid / 100 * pot_size,
      ev_dollars = ev_pct / 100 * pot_size
    )

  # Add market analysis
  bid_sheet <- find_value_opportunities(bid_sheet)

  # Save
  save_data(bid_sheet, "bid_sheet.csv", "output")

  # Print summary
  message("\n=== BID SHEET ===")
  message(sprintf("Pot Size: $%s", format(pot_size, big.mark = ",")))
  message(sprintf("Risk Tolerance: %s\n", risk_tolerance))

  print(bid_sheet %>%
    select(seed, ev_pct, target_bid, ev_dollars, target_bid_dollars, recommendation) %>%
    mutate(across(where(is.numeric), ~ round(., 2))))

  bid_sheet
}

#' Interactive bid calculator
#' Quick lookup during live auction
bid_calculator <- function(seed, pot_size = 1000, risk_tolerance = "medium") {
  source("R/02_seed_analysis.R")
  seed_probs <- get_historical_seed_probs() %>%
    add_expected_values() %>%
    filter(seed == !!seed)

  if (nrow(seed_probs) == 0) {
    stop(sprintf("Invalid seed: %d", seed))
  }

  variance <- 0.05  # Default estimate

  bid_range <- calculate_bid_range(seed_probs$ev_pct, variance, risk_tolerance)

  cat(sprintf("\n=== SEED %d BID GUIDE ===\n", seed))
  cat(sprintf("Expected Value: %.2f%% ($%.0f)\n",
              seed_probs$ev_pct, seed_probs$ev_pct / 100 * pot_size))
  cat(sprintf("\nBid Range (%s risk tolerance):\n", risk_tolerance))
  cat(sprintf("  Min Bid:    %.2f%% ($%.0f)\n",
              bid_range$min_bid, bid_range$min_bid / 100 * pot_size))
  cat(sprintf("  Target Bid: %.2f%% ($%.0f)\n",
              bid_range$target_bid, bid_range$target_bid / 100 * pot_size))
  cat(sprintf("  Max Bid:    %.2f%% ($%.0f)\n",
              bid_range$max_bid, bid_range$max_bid / 100 * pot_size))

  invisible(bid_range)
}

# Run if executed directly
if (sys.nframe() == 0) {
  setwd("/Users/wmc/ncaa_tourney")

  # Generate bid sheet with default $1000 pot
  bid_sheet <- generate_bid_sheet(pot_size = 1000, risk_tolerance = "medium")

  # Example: Quick lookup for specific seed
  cat("\n\n")
  bid_calculator(seed = 1, pot_size = 1000)
  bid_calculator(seed = 5, pot_size = 1000)
  bid_calculator(seed = 12, pot_size = 1000)
}
