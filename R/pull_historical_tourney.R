# pull_historical_tourney.R - Pull historical tournament results by year

library(tidyverse)
library(toRvik)

cat("Pulling historical tournament data...\n")

# Pull tournament results by team
results <- bart_tourney_results(min_year = 2008, max_year = 2023, type = "team")

cat(sprintf("Retrieved %d team-tournament records\n", nrow(results)))
cat("\nColumns: ", paste(names(results), collapse = ", "), "\n")

# Check structure
print(head(results, 10))

# Save raw results
write_csv(results, "data/raw/tournament_results_historical.csv")
cat("\nSaved: data/raw/tournament_results_historical.csv\n")
