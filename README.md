# NCAA Tournament Calcutta Auction Analysis

A data-driven bidding strategy tool for Calcutta-style auctions on the NCAA Men's Basketball Tournament. Built using 17 years of historical tournament data (2008-2025, excluding 2020).

## What is a Calcutta Auction?

In this format, participants bid on **matchup pairs** (e.g., buying the 1-seed also gives you their 16-seed opponent). There are 32 items per auction (8 matchup types × 4 regions). Payouts are based on how far your teams advance.

### Payout Structure (Cumulative)

| Round Reached | Payout |
|---------------|--------|
| Sweet 16 | 1.5% |
| Elite 8 | 3.0% |
| Final Four | 8.0% |
| Championship Game | 20.0% |
| Champion | 40.0% |

*Total payouts sum to 100% of the pot (less any house rake).*

---

## Key Findings

### Expected Value by Matchup

Based on 17 years of historical data:

| Matchup | Combined EV | High Seed EV | Low Seed EV | CV (Risk) |
|---------|-------------|--------------|-------------|-----------|
| 1/16 | 8.75% | 8.75% | 0.002% | 1.49 (lowest) |
| 2/15 | 4.75% | 4.73% | 0.02% | 1.99 |
| 3/14 | 2.93% | 2.85% | 0.08% | 2.35 |
| 4/13 | 2.40% | 2.30% | 0.10% | 2.57 |
| 5/12 | 1.72% | 1.40% | 0.32% | 2.81 |
| 6/11 | 1.78% | 1.19% | 0.59% | 2.65 |
| 7/10 | 1.48% | 0.97% | 0.51% | 3.03 |
| 8/9 | 1.27% | 0.67% | 0.60% | 3.21 (highest) |

**Insight**: 1-seeds have the best risk-adjusted value (lowest coefficient of variation). The 8/9 matchup is the most volatile.

### Best & Worst Historical Years by Matchup

#### Best Years (Highest Total Payout)

| Matchup | Year | EV% | Notable |
|---------|------|-----|---------|
| 1/16 | 2025 | 76.0% | All 4 1-seeds to Final Four (historic!) |
| 2/15 | 2022 | 34.0% | Kansas won, Villanova/Duke deep runs |
| 3/14 | 2011 | 48.0% | UConn (3-seed) won championship |
| 4/13 | 2011 | 31.0% | Butler (4) Final, Kentucky (4) F4 |
| 5/12 | 2019 | 26.0% | Auburn (5) to Final |
| 6/11 | 2024 | 12.5% | Strong 11-seed performance |
| 7/10 | 2014 | 46.0% | UConn (7) won title! |
| 8/9 | 2014 | 21.5% | Kentucky (8) to Championship |

#### Worst Years (Lowest Total Payout)

| Matchup | Year | EV% | Notable |
|---------|------|-----|---------|
| 1/16 | 2011 | 6.0% | All 1-seeds out by Elite 8 |
| 2/15 | 2011 | 3.0% | Chaos year |
| 3/14 | 2014 | 0.0% | All 3-seeds eliminated early |
| 4/13 | 2009 | 1.5% | 4-seeds collapsed |
| 5/12 | 2012 | 0.0% | Neither 5s nor 12s produced |
| 6/11 | 2008 | 0.0% | Complete shutout |
| 7/10 | 2009 | 0.0% | No value generated |
| 8/9 | 2008 | 0.0% | Entire bracket worthless |

---

## Model Validation: 2025 Auction

We tested our model against actual 2025 auction results ($21,060 pot, $300 rake).

### Model Performance

| Metric | "Good Buys" (price < EV) | "Bad Buys" (price > EV) |
|--------|--------------------------|-------------------------|
| Count | 18 | 14 |
| % Profitable | 22% | 21% |
| Total Profit | **+$1,824** | **-$1,501** |

**Following the model saved $3,325 vs. ignoring it.**

### Best Model Calls

| Team | Seed | EV | Paid | Discount | Payout | Profit |
|------|------|-----|------|----------|--------|--------|
| Florida | 1 | $1,843 | $1,700 | 8% under | $8,304 | +$6,604 |
| UConn | 8 | $268 | $120 | 55% under | $311 | +$191 |
| Arkansas | 7 | $312 | $210 | 33% under | $311 | +$101 |
| UCLA | 7 | $312 | $250 | 20% under | $311 | +$61 |

### Model Misses (Profitable Despite Overpay)

| Team | Seed | EV | Paid | Premium | Payout | Profit |
|------|------|-----|------|---------|--------|--------|
| Houston | 1 | $1,843 | $1,950 | 6% over | $4,152 | +$2,202 |
| Ole Miss | 6 | $375 | $525 | 40% over | $623 | +$98 |
| BYU | 6 | $375 | $550 | 47% over | $623 | +$73 |

---

## Bid Recommendations

For a **$20,000 pot**, recommended bid ranges:

| Matchup | Min Bid | Target | Max Bid |
|---------|---------|--------|---------|
| 1/16 | $1,111 | $1,389 | $1,667 |
| 2/15 | $601 | $751 | $901 |
| 3/14 | $370 | $463 | $555 |
| 4/13 | $304 | $380 | $456 |
| 5/12 | $218 | $272 | $327 |
| 6/11 | $225 | $281 | $337 |
| 7/10 | $187 | $234 | $281 |
| 8/9 | $161 | $201 | $241 |

*Target = EV. Min/Max = ±20% for risk tolerance.*

---

## Project Structure

```
ncaa_tourney/
├── R/
│   ├── utils.R                    # Constants and helpers
│   ├── 01_data_acquisition.R      # Pull data from toRvik
│   ├── 02_seed_analysis.R         # Historical seed probabilities
│   ├── 03_within_seed_model.R     # Team differentiation metrics
│   ├── 04_expected_value.R        # EV calculations
│   ├── 05_bid_strategy.R          # Bid recommendations
│   ├── 06_bracket_evaluator.R     # Live auction tools
│   ├── analyze_2025_auction.R     # 2025 results analysis
│   ├── compare_model_to_actual.R  # Model validation
│   ├── historical_seed_wins.R     # Year-by-year data
│   └── best_worst_years_by_matchup.R
├── data/
│   ├── raw/
│   │   ├── team_ratings_with_seeds.csv
│   │   ├── tournament_results_2024_2025.csv
│   │   └── seed_wins_by_year.csv
│   └── processed/
│       └── team_ratings_zscores.csv
├── output/
│   ├── seed_progression_probs.csv
│   ├── matchup_ev.csv
│   ├── bid_sheet.csv
│   ├── 2025_auction_analysis.csv
│   ├── 2025_model_vs_actual.csv
│   └── ev_grid_by_year_matchup.csv
└── README.md
```

## Usage

### Generate Bid Sheet for Your Pot Size

```r
source("R/06_bracket_evaluator.R")
print_bid_sheet(pot_size = 25000)
```

### Evaluate a Specific Team

```r
# Compare a team to others in its matchup
# high_barthag = team's barthag rating (from KenPom/Barttorvik)
evaluate_matchup(high_barthag = 0.95, low_barthag = 0.45, matchup = "1/16", pot_size = 20000)
```

### Run Full Analysis

```r
source("R/run_analysis.R")
```

## Data Sources

- **[Barttorvik/toRvik](https://github.com/andreweatherman/toRvik)**: Team ratings, adjusted efficiency metrics
- **Historical brackets**: 2008-2025 tournament results (excluding 2020 COVID cancellation)

## Key Metrics for Within-Seed Differentiation

When comparing teams with the same seed:

1. **Barthag** (Barttorvik rating) - Most predictive single metric
2. **Adjusted Offensive Efficiency** - Points per 100 possessions (adjusted)
3. **Adjusted Defensive Efficiency** - Points allowed per 100 possessions (adjusted)

Teams significantly above their seed's average in these metrics historically outperform expectations.

---

## License

MIT
