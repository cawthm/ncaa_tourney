# Backtest Analysis: Barthag-Adjusted EV vs Naive Seed EV

This document presents the results of backtesting our Barthag-adjusted expected value model against naive seed-based EV across 16 NCAA tournaments (2008-2025, excluding 2020).

## Summary

**Bottom Line**: Barthag-adjusted EV outperformed naive seed-based EV in **15 of 16 years** (93.8%), with an average mean absolute error (MAE) improvement of **11.4%**.

| Metric | Naive EV | Adj EV | Improvement |
|--------|----------|--------|-------------|
| Average MAE | 0.0189 | 0.0167 | 11.4% |
| Years Won | 1 | 15 | - |
| Average % Seeds Improved | - | - | 48.8% |

## Year-by-Year Results

The following table shows how each prediction method performed each year, measured by mean absolute error (MAE) - lower is better.

| Year | Naive MAE | Adj MAE | Improvement | Seeds Improved |
|------|-----------|---------|-------------|----------------|
| 2008 | 0.0141 | 0.0139 | +1.4% | 31.2% |
| 2009 | 0.0143 | 0.0136 | +5.3% | 43.8% |
| 2010 | 0.0123 | 0.0116 | +5.8% | 37.5% |
| 2011 | 0.0273 | 0.0253 | +7.1% | 50.0% |
| 2012 | 0.0160 | 0.0141 | +12.0% | 43.8% |
| 2013 | 0.0167 | 0.0135 | +19.0% | 56.2% |
| 2014 | 0.0217 | 0.0185 | +14.9% | 56.2% |
| 2015 | 0.0161 | 0.0155 | +3.9% | 43.8% |
| 2016 | 0.0194 | 0.0176 | +9.2% | 56.2% |
| 2017 | 0.0211 | 0.0195 | +7.5% | 50.0% |
| 2018 | 0.0206 | 0.0207 | **-0.5%** | 56.2% |
| 2019 | 0.0198 | 0.0125 | +36.9% | 37.5% |
| 2021 | 0.0154 | 0.0112 | +27.4% | 68.8% |
| 2022 | 0.0237 | 0.0198 | +16.5% | 56.2% |
| 2023 | 0.0158 | 0.0142 | +9.9% | 56.2% |
| 2025 | 0.0271 | 0.0256 | +5.5% | 37.5% |

**Best years for Adj EV**: 2019 (+36.9%), 2021 (+27.4%), 2013 (+19.0%)

**Only year Naive EV won**: 2018 (-0.5%) - essentially a tie

## Analysis by Seed Line

Results vary significantly by seed line. Adj EV provides the most value for top seeds where Barthag differentiation is meaningful.

| Seed | Naive EV% | Adj EV% | Actual EV% | Naive MAE | Adj MAE | Improvement |
|------|-----------|---------|------------|-----------|---------|-------------|
| 1 | 8.75% | 11.89% | 16.42% | 0.0836 | 0.0575 | **+31.2%** |
| 2 | 4.73% | 7.31% | 7.55% | 0.0376 | 0.0316 | **+15.9%** |
| 3 | 2.85% | 4.99% | 5.08% | 0.0283 | 0.0235 | **+16.9%** |
| 4 | 2.30% | 4.31% | 4.51% | 0.0261 | 0.0224 | **+14.2%** |
| 5 | 1.40% | 3.14% | 2.71% | 0.0183 | 0.0185 | -0.9% |
| 6 | 1.19% | 2.46% | 1.40% | 0.0086 | 0.0120 | -39.3% |
| 7 | 0.97% | 1.84% | 2.50% | 0.0193 | 0.0196 | -1.7% |
| 8 | 0.67% | 1.48% | 1.63% | 0.0139 | 0.0178 | -27.9% |
| 9 | 0.60% | 1.25% | 0.96% | 0.0096 | 0.0125 | -30.1% |
| 10 | 0.51% | 1.34% | 1.45% | 0.0122 | 0.0130 | -6.8% |
| 11 | 0.59% | 1.26% | 1.58% | 0.0142 | 0.0128 | +9.8% |
| 12 | 0.32% | 0.73% | 1.26% | 0.0098 | 0.0083 | **+15.5%** |
| 13 | 0.10% | 0.38% | 0.47% | 0.0040 | 0.0022 | **+45.8%** |
| 14 | 0.08% | 0.27% | 0.22% | 0.0023 | 0.0025 | -7.4% |
| 15 | 0.02% | 0.12% | 0.33% | 0.0033 | 0.0031 | +6.0% |
| 16 | 0.00% | 0.04% | 0.05% | 0.0005 | 0.0007 | -43.5% |

### Key Findings by Seed

**Top Seeds (1-4): Adj EV significantly better**
- 1-seeds: +31.2% improvement - the biggest win for Adj EV
- Strong Barthag teams are undervalued by naive historical averages
- Actual 1-seed performance (16.42% EV) exceeds even Adj EV predictions (11.89%)

**Middle Seeds (5-10): Mixed results**
- Naive EV is slightly better for 6-seeds and 8-9 seeds
- The model may be overweighting Barthag for these seeds where parity is higher
- Small sample sizes in deep runs make this noisy

**Low Seeds (11-16): Mixed but notable 13-seed improvement**
- 13-seeds show +45.8% improvement - Adj EV better captures Cinderella potential
- 12-seeds also improved by +15.5%
- Very low seeds (14-16) have too few wins to draw conclusions

## Model Details

The Barthag-adjusted EV model uses:
- **Logistic regression** fit on 478 tournament games (2008-2023)
- **Intercept**: 0.3242
- **Coefficient**: 0.7990

Win probability formula:
```
log_odds_diff = log(barthag_a / (1 - barthag_a)) - log(barthag_b / (1 - barthag_b))
P(A wins) = 1 / (1 + exp(-(0.3242 + 0.7990 * log_odds_diff)))
```

## Methodology

1. **Actual EV**: For each seed-year, calculated average EV earned by the 4 teams at that seed using actual tournament results
2. **Naive EV**: Used historical seed progression probabilities (2008-2024)
3. **Adj EV**: Calculated using that year's Barthag data and the fitted model
4. **MAE**: Mean absolute error between predicted and actual EV

## Interpretation for Auction Bidding

**When Adj EV > Naive EV (as it usually is for strong teams):**
- The team is undervalued by naive historical averages
- Consider bidding up to Adj EV, not just Naive EV

**When to trust Adj EV most:**
- Top 4 seeds where Barthag differentiation is strongest
- Years where the top 1-seeds are historically dominant (high Barthag)

**When to be cautious:**
- Middle seeds (5-10) where parity is high
- Very low seeds where any single upset swings EV significantly

## Files Generated

- `output/backtest_yearly_comparison.csv` - Full comparison data by seed-year
- `output/backtest_yearly_summary.csv` - Year-level summary statistics
- `output/backtest_seed_summary.csv` - Seed-level summary statistics
- `R/12_backtest_by_year.R` - Script to reproduce this analysis
