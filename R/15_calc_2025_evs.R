# 15_calc_2025_evs.R - Calculate normalized Adj EVs for 2025 tournament
#
# Generates 2025_team_adjusted_evs.csv with properly normalized EVs

library(tidyverse)
library(jsonlite)

source("R/utils.R")
source("R/10_bracket_ev_calculator.R")

# Load 2025 team data
team_ratings <- read_csv("data/raw/team_ratings_with_seeds.csv", show_col_types = FALSE) %>%
  filter(year == 2025, !is.na(seed))

# Load naive EVs
naive_evs <- read_csv("output/seed_progression_probs.csv", show_col_types = FALSE) %>%
  select(seed, ev_pct) %>%
  mutate(naive_ev = ev_pct)  # Already in percent

# Define region assignments for 2025 (seeds 1-8 have known teams)
# Based on actual 2025 bracket
bracket_2025 <- tribble(
  ~region, ~seed, ~team, ~barthag,
  # East
  "East", 1, "Duke", 0.9824,
  "East", 2, "Alabama", 0.9612,
  "East", 3, "Wisconsin", 0.9410,
  "East", 4, "Arizona", 0.9459,
  "East", 5, "Oregon", 0.8998,
  "East", 6, "BYU", 0.9373,
  "East", 7, "St. Mary's", 0.9137,
  "East", 8, "Baylor", 0.8990,
  # Midwest
  "Midwest", 1, "Houston", 0.9836,
  "Midwest", 2, "Tennessee", 0.9563,
  "Midwest", 3, "Kentucky", 0.9305,
  "Midwest", 4, "Purdue", 0.9366,
  "Midwest", 5, "Clemson", 0.9137,
  "Midwest", 6, "Illinois", 0.9248,
  "Midwest", 7, "UCLA", 0.9288,
  "Midwest", 8, "Gonzaga", 0.9494,
  # South
  "South", 1, "Auburn", 0.9746,
  "South", 2, "Michigan St", 0.9399,
  "South", 3, "Iowa St", 0.9441,
  "South", 4, "Texas A&M", 0.9165,
  "South", 5, "Michigan", 0.9166,
  "South", 6, "Ole Miss", 0.9146,
  "South", 7, "Marquette", 0.8909,
  "South", 8, "Louisville", 0.9151,
  # West
  "West", 1, "Florida", 0.9709,
  "West", 2, "St. John's", 0.9314,
  "West", 3, "Texas Tech", 0.9501,
  "West", 4, "Maryland", 0.9484,
  "West", 5, "Memphis", 0.8015,
  "West", 6, "Mizzou", 0.9247,
  "West", 7, "Arkansas", 0.8815,
  "West", 8, "UConn", 0.8841
)

# Add placeholder seeds 9-16 with default Barthag values
DEFAULT_BARTHAG <- c(
  "9" = 0.76, "10" = 0.74, "11" = 0.72, "12" = 0.70,
  "13" = 0.68, "14" = 0.65, "15" = 0.62, "16" = 0.55
)

low_seeds <- expand_grid(
  region = c("East", "Midwest", "South", "West"),
  seed = 9:16
) %>%
  mutate(
    team = paste0(seed, "-seed"),
    barthag = DEFAULT_BARTHAG[as.character(seed)]
  )

bracket_2025 <- bind_rows(bracket_2025, low_seeds)

# Calculate EVs with normalization
message("Calculating bracket EVs...")
model <- load_model()
results <- calc_bracket_ev(bracket_2025, model, normalize = TRUE)

# Add naive EVs
results <- results %>%
  left_join(naive_evs, by = "seed") %>%
  mutate(
    barthag_ev = ev * 100,  # Convert to percent
    ev_vs_naive = barthag_ev - naive_ev
  ) %>%
  select(region, seed, team, barthag, barthag_ev, p_champ, naive_ev, ev_vs_naive) %>%
  arrange(desc(barthag_ev))

# Verify totals
cat("\n=== Verification ===\n")
cat(sprintf("Total Adj EV:   %.1f%%\n", sum(results$barthag_ev)))
cat(sprintf("Total Naive EV: %.1f%%\n", sum(results$naive_ev)))

# Save
write_csv(results, "output/2025_team_adjusted_evs.csv")
message("\nSaved to output/2025_team_adjusted_evs.csv")

# Print summary by seed
cat("\n=== EV by Seed (4 teams each) ===\n")
results %>%
  group_by(seed) %>%
  summarise(
    avg_adj = mean(barthag_ev),
    avg_naive = mean(naive_ev),
    diff = avg_adj - avg_naive,
    .groups = "drop"
  ) %>%
  print(n = 16)

# Print 1-seeds
cat("\n=== 1-Seeds ===\n")
results %>%
  filter(seed == 1) %>%
  select(region, team, barthag, naive_ev, barthag_ev, ev_vs_naive) %>%
  print()
