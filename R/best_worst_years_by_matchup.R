# best_worst_years_by_matchup.R - Find best/worst years for each matchup bucket

library(tidyverse)
source("R/utils.R")

# Load historical seed wins by year
seed_wins <- read_csv("data/raw/seed_wins_by_year.csv", show_col_types = FALSE)

# Create matchup column
seed_wins <- seed_wins %>%
  mutate(
    matchup = case_when(
      seed == 1 | seed == 16 ~ "1/16",
      seed == 2 | seed == 15 ~ "2/15",
      seed == 3 | seed == 14 ~ "3/14",
      seed == 4 | seed == 13 ~ "4/13",
      seed == 5 | seed == 12 ~ "5/12",
      seed == 6 | seed == 11 ~ "6/11",
      seed == 7 | seed == 10 ~ "7/10",
      seed == 8 | seed == 9 ~ "8/9"
    ),
    is_high_seed = seed <= 8
  )

# Calculate EV payout for each team (cumulative structure)
calc_ev_pct <- function(wins) {
  case_when(
    wins >= 6 ~ 40,    # Champion
    wins >= 5 ~ 20,    # Final
    wins >= 4 ~ 8,     # F4
    wins >= 3 ~ 3,     # E8
    wins >= 2 ~ 1.5,   # S16
    wins >= 1 ~ 0,     # R32 only (no payout)
    TRUE ~ 0
  )
}

seed_wins <- seed_wins %>%
  mutate(ev_pct = calc_ev_pct(wins))

# Aggregate by year and matchup
matchup_by_year <- seed_wins %>%
  group_by(year, matchup) %>%
  summarize(
    total_ev_pct = sum(ev_pct),
    high_seed_wins = sum(wins[is_high_seed]),
    low_seed_wins = sum(wins[!is_high_seed]),
    .groups = "drop"
  )

# Order matchups properly
matchup_order <- c("1/16", "2/15", "3/14", "4/13", "5/12", "6/11", "7/10", "8/9")
matchup_by_year$matchup <- factor(matchup_by_year$matchup, levels = matchup_order)

# Find best year for each matchup
best_years <- matchup_by_year %>%
  group_by(matchup) %>%
  slice_max(total_ev_pct, n = 1, with_ties = FALSE) %>%
  arrange(matchup) %>%
  select(matchup, year, total_ev_pct, high_seed_wins, low_seed_wins)

# Find worst year for each matchup
worst_years <- matchup_by_year %>%
  group_by(matchup) %>%
  slice_min(total_ev_pct, n = 1, with_ties = FALSE) %>%
  arrange(matchup) %>%
  select(matchup, year, total_ev_pct, high_seed_wins, low_seed_wins)

# Print tables
cat("=== BEST YEARS BY MATCHUP BUCKET ===\n")
cat("(Year when that matchup type generated the highest total payout %)\n\n")

cat(sprintf("%-8s %6s %10s %12s %12s\n",
            "Matchup", "Year", "Total EV%", "High Wins", "Low Wins"))
cat(strrep("-", 52), "\n")

for (i in 1:nrow(best_years)) {
  cat(sprintf("%-8s %6d %9.1f%% %12d %12d\n",
              as.character(best_years$matchup[i]),
              best_years$year[i],
              best_years$total_ev_pct[i],
              best_years$high_seed_wins[i],
              best_years$low_seed_wins[i]))
}

cat("\n\n=== WORST YEARS BY MATCHUP BUCKET ===\n")
cat("(Year when that matchup type generated the lowest total payout %)\n\n")

cat(sprintf("%-8s %6s %10s %12s %12s\n",
            "Matchup", "Year", "Total EV%", "High Wins", "Low Wins"))
cat(strrep("-", 52), "\n")

for (i in 1:nrow(worst_years)) {
  cat(sprintf("%-8s %6d %9.1f%% %12d %12d\n",
              as.character(worst_years$matchup[i]),
              worst_years$year[i],
              worst_years$total_ev_pct[i],
              worst_years$high_seed_wins[i],
              worst_years$low_seed_wins[i]))
}

# Create a nice grid showing all years
cat("\n\n=== FULL GRID: TOTAL EV% BY MATCHUP AND YEAR ===\n\n")

# Pivot to wide format
ev_grid <- matchup_by_year %>%
  select(year, matchup, total_ev_pct) %>%
  pivot_wider(names_from = matchup, values_from = total_ev_pct) %>%
  arrange(year)

# Print header
cat(sprintf("%6s", "Year"))
for (m in matchup_order) {
  cat(sprintf(" %7s", m))
}
cat("\n")
cat(strrep("-", 6 + 8 * 8), "\n")

# Print each year
for (i in 1:nrow(ev_grid)) {
  cat(sprintf("%6d", ev_grid$year[i]))
  for (m in matchup_order) {
    val <- ev_grid[[m]][i]
    if (is.na(val)) {
      cat(sprintf(" %7s", "NA"))
    } else {
      cat(sprintf(" %6.1f%%", val))
    }
  }
  cat("\n")
}

# Add averages row
cat(strrep("-", 6 + 8 * 8), "\n")
cat(sprintf("%6s", "AVG"))
for (m in matchup_order) {
  avg_val <- mean(ev_grid[[m]], na.rm = TRUE)
  cat(sprintf(" %6.1f%%", avg_val))
}
cat("\n")

# Notable stories
cat("\n\n=== NOTABLE STORIES ===\n\n")

# 2025 - all 1 seeds to F4
y2025 <- matchup_by_year %>% filter(year == 2025, matchup == "1/16")
if (nrow(y2025) > 0) {
  cat(sprintf("2025 1/16: %.1f%% - All four 1-seeds made Final Four (historic!)\n", y2025$total_ev_pct))
}

# 2018 UMBC
y2018_1 <- matchup_by_year %>% filter(year == 2018, matchup == "1/16")
if (nrow(y2018_1) > 0) {
  cat(sprintf("2018 1/16: %.1f%% - UMBC became first 16-seed to beat a 1-seed\n", y2018_1$total_ev_pct))
}

# Best Cinderella years (5/12, 6/11)
cat("\nBest Cinderella years (high low-seed production):\n")
cinderella <- matchup_by_year %>%
  filter(matchup %in% c("5/12", "6/11")) %>%
  arrange(desc(low_seed_wins)) %>%
  head(5)

for (i in 1:nrow(cinderella)) {
  cat(sprintf("  %d %s: %d low-seed wins, %.1f%% total EV\n",
              cinderella$year[i], cinderella$matchup[i],
              cinderella$low_seed_wins[i], cinderella$total_ev_pct[i]))
}

# Save results
write_csv(best_years, "output/best_years_by_matchup.csv")
write_csv(worst_years, "output/worst_years_by_matchup.csv")
write_csv(ev_grid, "output/ev_grid_by_year_matchup.csv")

cat("\n\nSaved: output/best_years_by_matchup.csv")
cat("\nSaved: output/worst_years_by_matchup.csv")
cat("\nSaved: output/ev_grid_by_year_matchup.csv\n")
