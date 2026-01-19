# 14_2025_bucket_summary.R - Create comprehensive 2025 auction analysis by bucket
#
# Shows naive EV, adj EV, auction prices, and final payouts for each team/bucket

library(tidyverse)

source("R/utils.R")

# Load data
team_evs <- read_csv("output/2025_team_adjusted_evs.csv", show_col_types = FALSE)
auction_results <- read_csv("output/2025_auction_actual_results.csv", show_col_types = FALSE)

# Calculate total pot from auction prices
total_pot <- sum(auction_results$sold_price)
message(sprintf("Total pot: $%d", total_pot))

# Join the data
combined <- team_evs %>%
  select(region, seed, team, barthag, barthag_ev, naive_ev) %>%
  rename(adj_ev = barthag_ev) %>%
  left_join(
    auction_results %>%
      select(Region, Team, Seed, sold_price, wins, total_payout_pct, actual_payout, profit, roi) %>%
      rename(region = Region, team = Team, seed = Seed,
             price = sold_price, payout_pct = total_payout_pct, payout = actual_payout),
    by = c("region", "seed")
  ) %>%
  # Use team name from auction results if available (more accurate)
  mutate(team = coalesce(team.y, team.x)) %>%
  select(-team.x, -team.y)

# Define buckets
get_bucket <- function(seed) {
  case_when(
    seed %in% c(1, 16) ~ "1/16",
    seed %in% c(2, 15) ~ "2/15",
    seed %in% c(3, 14) ~ "3/14",
    seed %in% c(4, 13) ~ "4/13",
    seed %in% c(5, 12) ~ "5/12",
    seed %in% c(6, 11) ~ "6/11",
    seed %in% c(7, 10) ~ "7/10",
    seed %in% c(8, 9) ~ "8/9",
    TRUE ~ NA_character_
  )
}

combined <- combined %>%
  mutate(bucket = get_bucket(seed))

# Create bucket-level summary with both teams
bucket_summary <- combined %>%
  filter(seed <= 8) %>%  # High seeds
  rename_with(~ paste0("high_", .), -c(region, bucket)) %>%
  left_join(
    combined %>%
      filter(seed > 8) %>%  # Low seeds
      rename_with(~ paste0("low_", .), -c(region, bucket)),
    by = c("region", "bucket")
  ) %>%
  mutate(
    # Bucket totals
    bucket_naive_ev = high_naive_ev + low_naive_ev,
    bucket_adj_ev = high_adj_ev + low_adj_ev,
    bucket_price = coalesce(high_price, 0) + coalesce(low_price, 0),
    bucket_payout = coalesce(high_payout, 0) + coalesce(low_payout, 0),
    bucket_profit = bucket_payout - bucket_price,
    # Convert EVs to dollar amounts based on total pot
    high_naive_ev_dollars = high_naive_ev / 100 * total_pot,
    high_adj_ev_dollars = high_adj_ev / 100 * total_pot,
    low_naive_ev_dollars = low_naive_ev / 100 * total_pot,
    low_adj_ev_dollars = low_adj_ev / 100 * total_pot,
    bucket_naive_ev_dollars = bucket_naive_ev / 100 * total_pot,
    bucket_adj_ev_dollars = bucket_adj_ev / 100 * total_pot
  ) %>%
  # Order by bucket value
  mutate(bucket = factor(bucket, levels = c("1/16", "2/15", "3/14", "4/13", "5/12", "6/11", "7/10", "8/9"))) %>%
  arrange(bucket, region)

# Print detailed table
message("\n", rep("=", 120))
message("2025 NCAA Tournament Auction Analysis by Bucket")
message(rep("=", 120))

for (b in levels(bucket_summary$bucket)) {
  bucket_data <- bucket_summary %>% filter(bucket == b)

  message(sprintf("\n### Bucket %s ###", b))
  message(rep("-", 120))
  message(sprintf("%-8s | %-12s %6s %6s %6s %6s %4s %7s | %-12s %6s %6s %6s %6s %4s %7s | %7s %7s",
                  "Region", "High Seed", "Barth", "Naive", "Adj", "Price", "W", "Payout",
                  "Low Seed", "Barth", "Naive", "Adj", "Price", "W", "Payout",
                  "B.Price", "B.Payot"))
  message(rep("-", 120))

  for (i in seq_len(nrow(bucket_data))) {
    row <- bucket_data[i, ]
    message(sprintf("%-8s | %-12s %6.3f %5.1f%% %5.1f%% $%5.0f %4d $%6.0f | %-12s %6.3f %5.1f%% %5.1f%% $%5.0f %4d $%6.0f | $%6.0f $%6.0f",
                    row$region,
                    substr(row$high_team, 1, 12), row$high_barthag, row$high_naive_ev, row$high_adj_ev,
                    coalesce(row$high_price, 0), coalesce(row$high_wins, 0), coalesce(row$high_payout, 0),
                    substr(coalesce(row$low_team, paste0(row$low_seed, "-seed")), 1, 12),
                    coalesce(row$low_barthag, 0.7), coalesce(row$low_naive_ev, 0.5), coalesce(row$low_adj_ev, 0.2),
                    coalesce(row$low_price, 0), coalesce(row$low_wins, 0), coalesce(row$low_payout, 0),
                    row$bucket_price, row$bucket_payout))
  }
}

# Create clean output table
output_table <- bucket_summary %>%
  select(
    region, bucket,
    high_team, high_barthag, high_naive_ev, high_adj_ev, high_price, high_wins, high_payout,
    low_team, low_barthag, low_naive_ev, low_adj_ev, low_price, low_wins, low_payout,
    bucket_price, bucket_payout, bucket_profit
  ) %>%
  mutate(
    high_naive_ev = round(high_naive_ev, 2),
    high_adj_ev = round(high_adj_ev, 2),
    low_naive_ev = round(low_naive_ev, 2),
    low_adj_ev = round(low_adj_ev, 2)
  )

write_csv(output_table, "output/2025_bucket_analysis.csv")
message("\n\nSaved to output/2025_bucket_analysis.csv")

# Summary statistics
message("\n\n", rep("=", 80))
message("Summary by Bucket (All Regions Combined)")
message(rep("=", 80))

bucket_totals <- bucket_summary %>%
  group_by(bucket) %>%
  summarise(
    total_price = sum(bucket_price, na.rm = TRUE),
    total_payout = sum(bucket_payout, na.rm = TRUE),
    total_profit = sum(bucket_profit, na.rm = TRUE),
    avg_naive_ev = mean(bucket_naive_ev, na.rm = TRUE),
    avg_adj_ev = mean(bucket_adj_ev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    roi = (total_payout - total_price) / total_price * 100,
    ev_dollar = avg_adj_ev / 100 * total_pot
  )

message(sprintf("%-8s %10s %10s %10s %8s %8s %8s %10s",
                "Bucket", "Spent", "Payout", "Profit", "ROI%", "NaiveEV", "AdjEV", "EV$"))
message(rep("-", 80))
for (i in seq_len(nrow(bucket_totals))) {
  row <- bucket_totals[i, ]
  message(sprintf("%-8s $%9.0f $%9.0f $%9.0f %7.0f%% %7.2f%% %7.2f%% $%9.0f",
                  row$bucket, row$total_price, row$total_payout, row$total_profit,
                  row$roi, row$avg_naive_ev, row$avg_adj_ev, row$ev_dollar))
}

# Grand total
grand <- bucket_totals %>%
  summarise(
    total_price = sum(total_price),
    total_payout = sum(total_payout),
    total_profit = sum(total_profit)
  )
message(rep("-", 80))
message(sprintf("%-8s $%9.0f $%9.0f $%9.0f",
                "TOTAL", grand$total_price, grand$total_payout, grand$total_profit))
