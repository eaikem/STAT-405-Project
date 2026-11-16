# Amaka Work

## Project Overview

This analysis examines daily hard drive failure observations in the Backblaze Q2 2026 Drive Stats dataset. The purpose of my contribution was to examine the dataset's daily coverage, summarize failure observations, and visualize how recorded failures changed throughout the quarter.

## Data Preparation

I downloaded and extracted the Backblaze Q2 2026 dataset. The dataset contains 91 daily CSV files, approximately 12 GB of extracted data, and 197 columns.

I verified that all 91 daily files had consistent column headers. I then used R and the `data.table` package to process the files individually, extracting the failure variable and calculating daily observation totals and failure counts.

## Results

The analysis included:

- 91 daily CSV files.
- 31,968,003 drive-day observations.
- 1,522 failure observations.
- An overall failure-observation percentage of approximately 0.00476%.

I generated two visualizations: daily failure observation counts and daily failure percentages throughout Q2 2026.

## Files

- `daily_failure_summary.R`: Processes daily CSV files and calculates summary statistics.
- `plot_daily_failures.R`: Generates the two visualizations.
- `results/daily_failures.csv`: Contains daily observation and failure summaries.
- `results/daily_failure_counts.png`: Daily failure count graph.
- `results/daily_failure_percentage.png`: Daily failure percentage graph.

## Limitations

The dataset contains repeated daily observations of the same physical drives. Therefore, drive-day observations are not independent, and the overall failure-observation percentage should not be interpreted as the percentage of unique drives that failed. Additionally, some SMART variables may contain missing values.

## Reproducibility

The analysis was conducted using R on the UW–Madison Statistics server. The scripts process the daily CSV files individually rather than loading the entire dataset into memory.

## Individual Contribution

Amaka Ikem: Downloaded and extracted the Backblaze Q2 2026 dataset, checked consistency across daily CSV headers, calculated daily drive-day and failure-observation totals, computed daily and overall failure percentages, and generated visualizations summarizing daily failure patterns.
