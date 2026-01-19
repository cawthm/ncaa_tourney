# 13_bucket_progress_chart.R - Visualize expected tournament progress by seed bucket
#
# Creates a horizontal bar chart showing how far each seed bucket is expected
# to progress through the tournament, with each seed's contribution shown in
# different colors, plus standard deviation brackets.

library(tidyverse)
library(ggplot2)

# Load actual wins by year to calculate mean and SD
wins_by_year <- read_csv("data/raw/seed_wins_by_year.csv", show_col_types = FALSE) %>%
  filter(year != 2020)  # Exclude 2020 (no tournament)

# Calculate mean and SD of wins for each seed
seed_stats <- wins_by_year %>%
  group_by(seed) %>%
  summarise(
    expected_wins = mean(wins),
    sd_wins = sd(wins),
    n_obs = n(),
    .groups = "drop"
  )

# For reference, also load theoretical expected wins from probabilities
seed_probs <- read_csv("output/seed_progression_probs.csv", show_col_types = FALSE)
theoretical_wins <- seed_probs %>%
  mutate(
    theoretical_wins = p_R64 + p_R32 + p_S16 + p_E8 + p_F4 + p_CHAMP
  ) %>%
  select(seed, theoretical_wins)

seed_wins <- seed_stats %>%
  left_join(theoretical_wins, by = "seed")

# Define seed buckets (matchups)
buckets <- tibble(
  bucket = c("1/16", "2/15", "3/14", "4/13", "5/12", "6/11", "7/10", "8/9"),
  high_seed = c(1, 2, 3, 4, 5, 6, 7, 8),
  low_seed = c(16, 15, 14, 13, 12, 11, 10, 9)
)

# Join with expected wins and SD
bucket_data <- buckets %>%
  left_join(seed_wins %>%
              select(seed, expected_wins, sd_wins) %>%
              rename(high_seed = seed, high_wins = expected_wins, high_sd = sd_wins),
            by = "high_seed") %>%
  left_join(seed_wins %>%
              select(seed, expected_wins, sd_wins) %>%
              rename(low_seed = seed, low_wins = expected_wins, low_sd = sd_wins),
            by = "low_seed") %>%
  mutate(
    total_wins = high_wins + low_wins,
    # Combined SD for the bucket (sum of independent random variables)
    # SD(X + Y) = sqrt(SD(X)^2 + SD(Y)^2)
    total_sd = sqrt(high_sd^2 + low_sd^2),
    # Order buckets from 8/9 at top to 1/16 at bottom
    bucket = factor(bucket, levels = rev(c("1/16", "2/15", "3/14", "4/13", "5/12", "6/11", "7/10", "8/9")))
  )

# Reshape for stacked bar chart
plot_data <- bucket_data %>%
  pivot_longer(
    cols = c(high_wins, low_wins),
    names_to = "seed_type",
    values_to = "wins"
  ) %>%
  mutate(
    seed_label = if_else(seed_type == "high_wins",
                         as.character(high_seed),
                         as.character(low_seed)),
    seed_type = factor(seed_type,
                       levels = c("high_wins", "low_wins"),
                       labels = c("Favored Seed", "Underdog Seed"))
  )

# Create error bar data (need bucket-level summary for error bars)
error_data <- bucket_data %>%
  select(bucket, total_wins, total_sd) %>%
  mutate(
    sd_low = pmax(0, total_wins - total_sd),  # Clamp at 0
    sd_high = total_wins + total_sd
  )

# Create the chart
p <- ggplot(plot_data, aes(x = wins, y = bucket, fill = seed_type)) +
  # Vertical lines for round markers (behind bars)
  geom_vline(xintercept = 1:6, linetype = "dashed", alpha = 0.3, color = "gray40") +

  # Stacked bars

  geom_col(position = "stack", width = 0.7, color = "white", linewidth = 0.3) +

  # Add SD brackets/error bars
  geom_errorbarh(data = error_data,
                 aes(xmin = sd_low, xmax = sd_high, y = bucket),
                 inherit.aes = FALSE,
                 height = 0.3, linewidth = 0.8, color = "gray30") +

  # Add seed labels on the bars
  geom_text(aes(label = seed_label),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 3.5) +

  # Custom colors
  scale_fill_manual(
    values = c("Favored Seed" = "#1a365d", "Underdog Seed" = "#ed8936"),
    name = NULL
  ) +

  # X-axis as tournament rounds
  scale_x_continuous(
    breaks = 0:6,
    labels = c("0", "R64", "R32", "S16", "E8", "F4", "Champ"),
    limits = c(0, 6.5),
    expand = c(0, 0)
  ) +

  # Labels
  labs(
    title = "Expected Tournament Progress by Seed Bucket",
    subtitle = "Average wins per bucket with \u00B11 SD brackets (based on 2008-2024 data)",
    x = "Tournament Round Reached",
    y = "Seed Bucket"
  ) +

  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.y = element_text(face = "bold"),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save the chart
ggsave("output/bucket_progress_chart.png", p, width = 10, height = 6, dpi = 150, bg = "white")

message("Saved chart to output/bucket_progress_chart.png")

# Print summary table
summary_table <- bucket_data %>%
  select(bucket, high_seed, high_wins, high_sd, low_seed, low_wins, low_sd, total_wins, total_sd) %>%
  arrange(desc(total_wins))

message("\nExpected Wins by Bucket (with SD):")
print(summary_table, n = 8)
