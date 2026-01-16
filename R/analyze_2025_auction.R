# analyze_2025_auction.R - Analyze actual 2025 auction results

library(tidyverse)
source("R/utils.R")

cat("=== 2025 AUCTION ACTUAL RESULTS ===\n\n")

# Load auction data
auction <- read_csv("data/2025_auction results.csv", show_col_types = FALSE)
total_auction <- sum(auction$sold_price)
rake <- 300  # House rake for charity
pot_size <- total_auction - rake

cat(sprintf("Total Auction Proceeds: $%s\n", format(total_auction, big.mark = ",")))
cat(sprintf("House Rake (charity):   $%d\n", rake))
cat(sprintf("Net Pot Size:           $%s\n\n", format(pot_size, big.mark = ",")))

# 2025 tournament results - how many wins each HIGH seed got
high_seed_wins <- tribble(
  ~Team, ~wins,
  "Florida", 6,       # Champion
  "Houston", 5,       # Final
  "Auburn", 4,        # F4
  "Duke", 4,          # F4
  "Michigan St", 3,   # E8
  "Texas Tech", 3,    # E8
  "Michigan", 3,      # E8
  "Ole Miss", 3,      # E8
  "Alabama", 2,       # S16
  "Iowa St", 2,       # S16
  "BYU", 2,           # S16
  "Wisconsin", 2,     # S16
  "Arizona", 2,       # S16
  "Maryland", 2,      # S16
  "Arkansas", 2,      # S16 (this was 7 seed)
  "Tennessee", 1,
  "Kentucky", 1,
  "Clemson", 1,
  "Purdue", 1,
  "Illinois", 1,
  "UCLA", 1,
  "Gonzaga", 1,
  "Baylor", 1,
  "Louisville", 1,
  "Marquette", 1,
  "Texas A&M", 1,
  "Oregon", 0,
  "Memphis", 0,
  "Mizzou", 0,
  "Uconn", 0          # Defending champ out R64!
)

# Low seed wins (the paired opponent that came with purchase)
# These are the Cinderella stories
low_seed_wins <- tribble(
  ~Team, ~low_wins,
  "BYU", 2,           # 11-seed Drake made S16
  "UCLA", 2,          # 10-seed (Arkansas) made S16 -- wait UCLA bought gets 10
  "Uconn", 2,         # 9-seed Creighton made S16
  "Texas Tech", 1,    # 14-seed Lipscomb got 1 win
  "Michigan", 1,      # 12-seed got 1 win
  "Clemson", 1,       # 12-seed Liberty/Colorado St got 1 win
  "Illinois", 1,      # 11-seed UNC got 1 win
  "Gonzaga", 1,       # 9-seed Baylor got 1 win
  "Louisville", 1     # 9-seed got 1 win
)

# Payout function (cumulative)
calc_payout <- function(wins) {
  case_when(
    wins >= 6 ~ 40,   # Champion
    wins >= 5 ~ 20,   # Final
    wins >= 4 ~ 8,    # F4
    wins >= 3 ~ 3,    # E8 (cumulative)
    wins >= 2 ~ 1.5,  # S16 (cumulative)
    TRUE ~ 0
  )
}

# Clean team names for matching
auction <- auction %>%
  mutate(Team_clean = gsub("'", "", Team))

high_seed_wins <- high_seed_wins %>%
  mutate(Team_clean = gsub("'", "", Team))

low_seed_wins <- low_seed_wins %>%
  mutate(Team_clean = gsub("'", "", Team))

# Merge and calculate
auction_results <- auction %>%
  left_join(high_seed_wins %>% select(Team_clean, wins), by = "Team_clean") %>%
  left_join(low_seed_wins %>% select(Team_clean, low_wins), by = "Team_clean") %>%
  mutate(
    wins = coalesce(wins, 0),
    low_wins = coalesce(low_wins, 0),
    high_payout_pct = calc_payout(wins),
    low_payout_pct = calc_payout(low_wins),
    total_payout_pct = high_payout_pct + low_payout_pct,
    actual_payout = total_payout_pct / 100 * pot_size,
    profit = actual_payout - sold_price,
    roi = round((actual_payout / sold_price - 1) * 100, 0)
  )

# Print results
cat("=== RESULTS BY PURCHASE ===\n\n")
cat(sprintf("%-10s %-14s %4s %6s %8s %8s %7s\n",
            "Region", "Team", "Seed", "Paid", "Payout", "Profit", "ROI"))
cat(strrep("-", 62), "\n")

for (i in 1:nrow(auction_results)) {
  cat(sprintf("%-10s %-14s %4d %6.0f %8.0f %+8.0f %+6.0f%%\n",
              auction_results$Region[i],
              auction_results$Team[i],
              auction_results$Seed[i],
              auction_results$sold_price[i],
              auction_results$actual_payout[i],
              auction_results$profit[i],
              auction_results$roi[i]))
}

# Winners and losers
cat("\n\n=== BIGGEST WINNERS ===\n")
winners <- auction_results %>% arrange(desc(profit)) %>% head(5)
for (i in 1:nrow(winners)) {
  cat(sprintf("  %s %s (%d): Paid $%.0f -> Payout $%.0f = $%+.0f profit (%+.0f%% ROI)\n",
              winners$Region[i], winners$Team[i], winners$Seed[i],
              winners$sold_price[i], winners$actual_payout[i],
              winners$profit[i], winners$roi[i]))
}

cat("\n=== BIGGEST LOSERS ===\n")
losers <- auction_results %>% arrange(profit) %>% head(5)
for (i in 1:nrow(losers)) {
  cat(sprintf("  %s %s (%d): Paid $%.0f -> Payout $%.0f = $%+.0f profit (%+.0f%% ROI)\n",
              losers$Region[i], losers$Team[i], losers$Seed[i],
              losers$sold_price[i], losers$actual_payout[i],
              losers$profit[i], losers$roi[i]))
}

# Summary by seed
cat("\n\n=== SUMMARY BY MATCHUP ===\n\n")
seed_summary <- auction_results %>%
  group_by(Seed) %>%
  summarize(
    n = n(),
    total_paid = sum(sold_price),
    total_payout = sum(actual_payout),
    total_profit = sum(profit),
    avg_roi = mean(roi),
    .groups = "drop"
  ) %>%
  arrange(Seed)

cat(sprintf("%-6s %6s %10s %10s %10s %8s\n",
            "Seed", "Count", "Paid", "Payout", "Profit", "Avg ROI"))
cat(strrep("-", 55), "\n")
for (i in 1:nrow(seed_summary)) {
  cat(sprintf("%-6d %6d %10.0f %10.0f %+10.0f %+7.0f%%\n",
              seed_summary$Seed[i],
              seed_summary$n[i],
              seed_summary$total_paid[i],
              seed_summary$total_payout[i],
              seed_summary$total_profit[i],
              seed_summary$avg_roi[i]))
}

# Total
cat(strrep("-", 55), "\n")
cat(sprintf("%-6s %6d %10.0f %10.0f %+10.0f\n",
            "TOTAL", nrow(auction_results),
            sum(auction_results$sold_price),
            sum(auction_results$actual_payout),
            sum(auction_results$profit)))

# Save results
write_csv(auction_results %>%
            select(Region, Team, Seed, sold_price, wins, low_wins,
                   total_payout_pct, actual_payout, profit, roi),
          "output/2025_auction_actual_results.csv")
cat("\n\nSaved: output/2025_auction_actual_results.csv\n")
