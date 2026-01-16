# utils.R - Utility functions for NCAA Tournament Calcutta Analysis

# Payout structure (as % of total pot) - INCREMENTAL payouts per round
# Cumulative: S16=1.5%, E8=3%, F4=8%, Final=20%, Champ=40%
# These are incremental (what you earn for reaching that round)
PAYOUTS <- c(
  R64 = 0.00,     # No payout for just winning R64
  R32 = 0.015,    # 1.5% for reaching Sweet 16 (cumulative: 1.5%)
  S16 = 0.015,    # 1.5% for reaching Elite 8 (cumulative: 3.0%)
  E8  = 0.05,     # 5.0% for reaching Final Four (cumulative: 8.0%)
  F4  = 0.12,     # 12% for reaching Championship game (cumulative: 20%)
  CHAMP = 0.20    # 20% for winning tournament (cumulative: 40%)
)
# Verification: 16*1.5% + 8*1.5% + 4*5% + 2*12% + 1*20% = 24+12+20+24+20 = 100%

ROUND_NAMES <- c("R64", "R32", "S16", "E8", "F4", "CHAMP")

#' Calculate expected value for a team given progression probabilities
#' @param probs Named vector of probabilities for reaching each round
#' @return Expected value as proportion of total pot
calculate_ev <- function(probs) {
  # probs should be P(reach round) for each round
  # EV = sum of P(reach round) * payout(round)
  sum(probs * PAYOUTS[names(probs)], na.rm = TRUE)
}

#' Convert round number to round name
#' @param round_num Integer 1-6
#' @return Round name string
round_to_name <- function(round_num) {
  ROUND_NAMES[round_num]
}

#' Convert round name to round number
#' @param round_name Character string
#' @return Integer 1-6
name_to_round <- function(round_name) {
  match(round_name, ROUND_NAMES)
}

#' Ensure directory exists
ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

#' Save data with timestamp
save_data <- function(data, filename, dir = "data/processed") {
  ensure_dir(dir)
  filepath <- file.path(dir, filename)
  write_csv(data, filepath)
  message(sprintf("Saved %d rows to %s", nrow(data), filepath))
}
