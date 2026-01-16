# compare_model_to_actual.R - Compare model predictions to actual 2025 outcomes

library(tidyverse)

cat("=== MODEL PREDICTION vs ACTUAL 2025 RESULTS ===\n\n")

# Load both datasets
model_pred <- read_csv("output/2025_auction_analysis.csv", show_col_types = FALSE)
actual <- read_csv("output/2025_auction_actual_results.csv", show_col_types = FALSE)

# Merge
comparison <- model_pred %>%
  left_join(actual %>% select(Team, wins, low_wins, actual_payout, profit, roi),
            by = "Team")

# Calculate key metrics
comparison <- comparison %>%
  mutate(
    paid_vs_ev = sold_price - ev_dollars,
    paid_vs_ev_pct = (sold_price / ev_dollars - 1) * 100,
    ev_accurate = case_when(
      good_buy & profit > 0 ~ "Good buy -> Profit",
      good_buy & profit <= 0 ~ "Good buy -> Loss",
      !good_buy & profit > 0 ~ "Bad buy -> Profit",
      !good_buy & profit <= 0 ~ "Bad buy -> Loss"
    )
  )

# Print detailed comparison
cat("=== DETAILED COMPARISON ===\n\n")
cat(sprintf("%-12s %-6s %8s %8s %8s %8s  %-20s\n",
            "Team", "Seed", "EV$", "Paid$", "Payout$", "Profit$", "Model Assessment"))
cat(strrep("-", 80), "\n")

for (i in 1:nrow(comparison)) {
  cat(sprintf("%-12s %-6d %8.0f %8.0f %8.0f %+8.0f  %-20s\n",
              comparison$Team[i],
              comparison$Seed[i],
              comparison$ev_dollars[i],
              comparison$sold_price[i],
              comparison$actual_payout[i],
              comparison$profit[i],
              comparison$ev_accurate[i]))
}

# Summary statistics
cat("\n\n=== MODEL PERFORMANCE SUMMARY ===\n\n")

# How did "good buys" perform?
good_buys <- comparison %>% filter(good_buy)
bad_buys <- comparison %>% filter(!good_buy)

cat("'Good Buy' recommendations (price < EV):\n")
cat(sprintf("  Count: %d\n", nrow(good_buys)))
cat(sprintf("  Profitable: %d (%.0f%%)\n",
            sum(good_buys$profit > 0),
            100 * sum(good_buys$profit > 0) / nrow(good_buys)))
cat(sprintf("  Avg profit: $%.0f\n", mean(good_buys$profit)))
cat(sprintf("  Total profit: $%.0f\n\n", sum(good_buys$profit)))

cat("'Bad Buy' recommendations (price > EV):\n")
cat(sprintf("  Count: %d\n", nrow(bad_buys)))
cat(sprintf("  Profitable: %d (%.0f%%)\n",
            sum(bad_buys$profit > 0),
            100 * sum(bad_buys$profit > 0) / nrow(bad_buys)))
cat(sprintf("  Avg profit: $%.0f\n", mean(bad_buys$profit)))
cat(sprintf("  Total profit: $%.0f\n\n", sum(bad_buys$profit)))

# Best model picks that worked
cat("=== BEST MODEL CALLS ===\n")
cat("(Teams flagged as good buys that were profitable)\n\n")
winners <- comparison %>%
  filter(good_buy & profit > 0) %>%
  arrange(desc(profit))

for (i in 1:nrow(winners)) {
  cat(sprintf("  %s (%d): EV=$%.0f, Paid=$%.0f (%.0f%% under EV), Payout=$%.0f, Profit=$%+.0f\n",
              winners$Team[i], winners$Seed[i],
              winners$ev_dollars[i], winners$sold_price[i],
              -winners$paid_vs_ev_pct[i],
              winners$actual_payout[i], winners$profit[i]))
}

# Worst model misses
cat("\n=== MODEL MISSES ===\n")
cat("(Teams flagged as bad buys that were actually profitable)\n\n")
misses <- comparison %>%
  filter(!good_buy & profit > 0) %>%
  arrange(desc(profit))

if (nrow(misses) > 0) {
  for (i in 1:nrow(misses)) {
    cat(sprintf("  %s (%d): EV=$%.0f, Paid=$%.0f (%.0f%% over EV), Payout=$%.0f, Profit=$%+.0f\n",
                misses$Team[i], misses$Seed[i],
                misses$ev_dollars[i], misses$sold_price[i],
                misses$paid_vs_ev_pct[i],
                misses$actual_payout[i], misses$profit[i]))
  }
} else {
  cat("  None - all 'bad buy' flags were correct!\n")
}

# By seed analysis
cat("\n\n=== PREDICTION ACCURACY BY SEED ===\n\n")
by_seed <- comparison %>%
  group_by(Seed) %>%
  summarize(
    n = n(),
    avg_ev = mean(ev_dollars),
    avg_paid = mean(sold_price),
    avg_payout = mean(actual_payout),
    model_said_good = sum(good_buy),
    actually_profit = sum(profit > 0),
    total_profit = sum(profit),
    .groups = "drop"
  )

cat(sprintf("%-6s %6s %8s %8s %8s %8s %8s %10s\n",
            "Seed", "Count", "Avg EV", "Avg Paid", "Avg Payout", "Good?", "Profit?", "Tot Profit"))
cat(strrep("-", 72), "\n")
for (i in 1:nrow(by_seed)) {
  cat(sprintf("%-6d %6d %8.0f %8.0f %8.0f %8d %8d %+10.0f\n",
              by_seed$Seed[i], by_seed$n[i],
              by_seed$avg_ev[i], by_seed$avg_paid[i], by_seed$avg_payout[i],
              by_seed$model_said_good[i], by_seed$actually_profit[i],
              by_seed$total_profit[i]))
}

# Overall conclusion
cat("\n\n=== CONCLUSION ===\n\n")
total_good_buy_profit <- sum(good_buys$profit)
total_bad_buy_profit <- sum(bad_buys$profit)
cat(sprintf("Following 'Good Buy' recommendations: $%+.0f total profit\n", total_good_buy_profit))
cat(sprintf("Following 'Bad Buy' (overpay) recommendations: $%+.0f total profit\n", total_bad_buy_profit))
cat(sprintf("\nDifference: Following model saved $%.0f vs ignoring it\n",
            total_good_buy_profit - total_bad_buy_profit))

# Save comparison
write_csv(comparison %>%
            select(Region, Team, Seed, ev_dollars, sold_price, paid_vs_ev, paid_vs_ev_pct,
                   good_buy, wins, low_wins, actual_payout, profit, roi, ev_accurate),
          "output/2025_model_vs_actual.csv")
cat("\nSaved: output/2025_model_vs_actual.csv\n")
