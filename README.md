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

## Live Auction Tracker

A web-based tool for tracking bids during the auction. Available at `docs/index.html` or deploy to GitHub Pages.

**Features:**
- Price entry grid organized by region and matchup
- Running pot total with dynamic final pot estimation
- Real-time bid target updates based on estimated pot size
- Personal budget tracker (spent vs remaining)
- Mark purchases as "yours" to track your portfolio
- Export/import auction data as JSON
- LocalStorage persistence (refreshing won't lose data)

**To use locally:**
```bash
cd docs
python -m http.server 8000
# Open http://localhost:8000 in your browser
```

**To deploy to GitHub Pages:**
1. Push the `docs/` directory to your repository
2. Go to Settings > Pages
3. Set source to "Deploy from a branch" and select `main` branch, `/docs` folder

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

## Within-Seed Differentiation: Validated Model

Our cross-validation analysis (2008-2023, excluding 2020) quantifies how team quality metrics predict tournament performance *beyond* seed alone.

### Key Finding: Barthag is the Most Predictive Metric

| Metric | Correlation with Tournament Wins |
|--------|----------------------------------|
| **Barthag** | 0.376 (strongest) |
| Adj. Defense | -0.179 (negative = better) |
| Adj. Offense | 0.142 |
| Wins Above Bubble | 0.096 |
| Tempo | -0.040 (not significant) |

**Each σ (standard deviation) of Barthag above seed average = +0.58 extra tournament wins.**

### Incremental EV by Seed

The impact of being a "+1σ team" vs "-1σ team" varies significantly by seed:

| Seed | Base EV | +1σ Team EV | -1σ Team EV | Δ per σ |
|------|---------|-------------|-------------|---------|
| 1 | 8.75% | 10.49% | 7.01% | +1.74% |
| 2 | 4.73% | 6.18% | 3.28% | +1.45% |
| 3 | 2.85% | 4.01% | 1.69% | +1.16% |
| 4 | 2.30% | 3.35% | 1.26% | +1.05% |
| 5 | 1.40% | 2.27% | 0.53% | +0.87% |
| 6 | 1.19% | 1.94% | 0.43% | +0.76% |
| 7 | 0.97% | 1.67% | 0.28% | +0.70% |
| 8 | 0.67% | 1.25% | 0.09% | +0.58% |

**Implication**: A 1-seed that's "strong for a 1-seed" (e.g., 2024 Houston) is worth ~20% more than an average 1-seed. For middle seeds, the relative impact is even larger.

### Model Validation Results

Leave-one-year-out cross-validation metrics:

| Metric | Value |
|--------|-------|
| R² | 0.471 |
| Mean Absolute Error | 0.79 wins |
| RMSE | 1.03 wins |

**Model Comparison** (lower MAE = better):

| Model | MAE | R² |
|-------|-----|-----|
| Seed + barthag_z | **0.791** | 0.471 |
| Seed + all metrics | 0.791 | 0.472 |
| Seed only (baseline) | 0.884 | 0.307 |

Adding barthag as a within-seed adjustment significantly improves predictions over seed alone. Adding more metrics (offense, defense, tempo, WAB) provides minimal additional value.

### Calibration Check: Is the Current Model Accurate?

Teams grouped by Barthag z-score bucket:

| Bucket | Avg Wins | Wins vs Expected |
|--------|----------|------------------|
| Very Strong (>1.5σ) | 1.77 | +0.93 |
| Strong (0.5 to 1.5σ) | 1.68 | +0.65 |
| Average (-0.5 to 0.5σ) | 0.80 | -0.17 |
| Weak (-1.5 to -0.5σ) | 0.38 | -0.55 |
| Very Weak (<-1.5σ) | 0.26 | -0.98 |

The model correctly identifies that "Very Strong" teams win nearly a full game more than expected.

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
│   ├── 07_validate_intra_seed.R   # Cross-validation analysis
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
├── docs/                            # Live Auction Tracker (GitHub Pages)
│   ├── index.html                   # Main app
│   ├── app.js                       # Application logic
│   ├── style.css                    # Styling
│   └── data.json                    # Historical EVs and ratios
├── output/
│   ├── seed_progression_probs.csv
│   ├── matchup_ev.csv
│   ├── bid_sheet.csv
│   ├── intra_seed_validation.csv    # CV results
│   ├── metric_importance.csv        # Which metrics matter
│   ├── incremental_ev_by_seed.csv   # EV impact of z-scores
│   ├── calibration_by_zscore.csv    # Model calibration
│   ├── model_comparison.csv         # Model performance
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

### Run Intra-Seed Validation

```r
source("R/07_validate_intra_seed.R")
results <- run_validation()
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

---

## License

MIT
