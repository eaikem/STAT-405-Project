# Sungmin Work: Regression analysis

This folder answers the second half of our question: *what factors are associated with a higher probability of drive failure?* It also compares drive models after adjusting for age, temperature, and warning signals.

**Results and write-up draft: see [RESULTS.md](RESULTS.md).**

## Files
| file | what it does |
|---|---|
| `build_tables.R` | turns the 91 daily Q2 2026 CSVs into `drives.csv` (one row per drive) and `window.csv` (14-day pre-failure window) |
| `regression.R` | fits the models, runs the checks, and saves graphs, tables, and a text summary to `results/` |
| `simulate_data.R` | makes a **fake** `drives.csv` for testing without the real data |
| `results/` | output from the real Q2 2026 data |

The data tables are not in the repo because they are too large (`drives.csv` is 29 MB). Rebuild them with step 1 below.

## How to run

### 1. Build the tables
Run this where the unzipped `data_Q2_2026` folder is.

On one machine:
```
Rscript build_tables.R all  data_Q2_2026/  tables/
```

On CHTC, run 91 day jobs and then one combine job:
```
Rscript build_tables.R day      2026-04-01.csv  slim/     # one job per daily file
Rscript build_tables.R combine  slim/           tables/
```
- An optional 4th argument sets the pre-failure window length in days. The default is 14.
- The script reads one day at a time, so memory use stays under about 0.5 GB.

### 2. Run the regression
```
Rscript regression.R tables/drives.csv results/
```
Requires R with `data.table` and `ggplot2`.

## `drives.csv` columns
| column | meaning |
|---|---|
| serial_number, model, capacity_bytes | drive identity |
| failure | 1 if the drive failed in Q2, else 0 |
| days_observed | number of days the drive appears in Q2 |
| age_hours | max power-on hours (SMART 9) |
| temp_mean | mean temperature, °C (SMART 194) |
| smart_5_max, smart_187_max, smart_197_max | quarter maximum, which includes the failure day |
| smart_5_first, smart_187_first, smart_197_first | value on the drive's first day in Q2, before any failure |

## Model notes
- Both models are binomial GLMs with a cloglog link and `offset(log(days_observed))`. This is the drive-level form of a per-day failure model.
  - exp(coef) is a hazard ratio. Because failures are rare, it is almost the same as the odds ratio from a logistic model.
  - Days observed is treated as exposure, not as a predictor, because failed drives drop out of the data after they fail.
- Warning flags use each drive's **first-day** SMART 5 and 197 values. The quarter maximum includes the failure day and inflates the effect, so it is reported only as a comparison.
- SMART 187 is not used, because only Seagate reports it.
- Before fitting, the script drops:
  - boot drives (< 1 TB)
  - drives from other manufacturers
  - models with ≤ 100 drives or ≤ 10,000 drive-days
- The same model is sometimes recorded with and without a maker prefix (e.g. `WDC WUH721816ALE6L4` and `WUH721816ALE6L4`). These are merged, and each spelling is listed in `reg_summary.txt`.
