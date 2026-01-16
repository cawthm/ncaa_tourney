# run_analysis.R - Main entry point for NCAA Calcutta analysis
#
# Run this script to execute the full analysis pipeline
# Usage: Rscript R/run_analysis.R [pot_size] [risk_tolerance]

library(tidyverse)

# Set working directory to project root
setwd("/Users/wmc/ncaa_tourney")

# Source all modules
source("R/utils.R")
source("R/02_seed_analysis.R")
source("R/04_expected_value.R")
source("R/05_bid_strategy.R")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
pot_size <- if (length(args) >= 1) as.numeric(args[1]) else 1000
risk_tolerance <- if (length(args) >= 2) args[2] else "medium"

cat("
================================================================================
                    NCAA TOURNAMENT CALCUTTA ANALYSIS
================================================================================

")

cat(sprintf("Configuration:
  Pot Size: $%s

  Risk Tolerance: %s

", format(pot_size, big.mark = ","), risk_tolerance))

# Step 1: Seed Analysis
cat("Step 1: Analyzing historical seed performance...\n")
cat(rep("-", 60), "\n", sep = "")
seed_probs <- analyze_seeds()

# Step 2: Expected Value Calculation
cat("\n\nStep 2: Calculating expected values...\n")
cat(rep("-", 60), "\n", sep = "")
ev_summary <- calculate_bracket_values()

# Step 3: Generate Bid Sheet
cat("\n\nStep 3: Generating bid recommendations...\n")
cat(rep("-", 60), "\n", sep = "")
bid_sheet <- generate_bid_sheet(pot_size = pot_size, risk_tolerance = risk_tolerance)

# Step 4: Summary output
cat("\n\n")
cat("================================================================================\n")
cat("                           QUICK REFERENCE GUIDE\n")
cat("================================================================================\n\n")

cat("KEY INSIGHTS:\n\n")

# Top value picks
top_value <- bid_sheet %>%
  filter(recommendation %in% c("Strong Buy", "Buy")) %>%
  arrange(desc(opportunity_score)) %>%
  head(5)

if (nrow(top_value) > 0) {
  cat("Best Value Seeds (likely underpriced):\n")
  for (i in seq_len(nrow(top_value))) {
    cat(sprintf("  - Seed %d: EV=%.1f%%, Target Bid=$%.0f\n",
                top_value$seed[i],
                top_value$ev_pct[i],
                top_value$target_bid_dollars[i]))
  }
}

# Seeds to avoid
avoid <- bid_sheet %>%
  filter(recommendation %in% c("Avoid", "Strong Avoid")) %>%
  head(3)

if (nrow(avoid) > 0) {
  cat("\nSeeds to Avoid (likely overpriced):\n")
  for (i in seq_len(nrow(avoid))) {
    cat(sprintf("  - Seed %d: Often overbid by %.0f%%\n",
                avoid$seed[i],
                (avoid$market_factor[i] - 1) * 100))
  }
}

cat("\n\nBID RANGES BY SEED:\n\n")
cat(sprintf("%-6s %10s %12s %12s %12s\n",
            "Seed", "EV", "Min Bid", "Target", "Max Bid"))
cat(rep("-", 55), "\n", sep = "")

for (i in seq_len(nrow(bid_sheet))) {
  cat(sprintf("%-6d $%8.0f $%10.0f $%10.0f $%10.0f\n",
              bid_sheet$seed[i],
              bid_sheet$ev_dollars[i],
              bid_sheet$min_bid_dollars[i],
              bid_sheet$target_bid_dollars[i],
              bid_sheet$max_bid_dollars[i]))
}

cat("\n\nSanity Check - Total EV across 64 teams: ",
    sprintf("%.1f%%", sum(bid_sheet$ev_pct) * 4), "\n")
cat("(Should be approximately 100% - the sum of all payouts)\n")

cat("\n================================================================================\n")
cat("                              END OF ANALYSIS\n")
cat("================================================================================\n")
cat("\nOutput files saved to: output/\n")
cat("  - seed_progression_probs.csv\n")
cat("  - bid_sheet.csv\n")
