
library(data.table)

# Q2 2026 daily failure analysis

# Dataset location relative to Amaka-Work
data_dir <- "../data/raw/data_Q2_2026"

# Output location
output_dir <- "results"

dir.create(output_dir, showWarnings = FALSE)

# Find all daily CSV files
files <- sort(list.files(
  data_dir,
  pattern = "^[0-9]{4}-[0-9]{2}-[0-9]{2}\\.csv$",
  full.names = TRUE
))

if (length(files) == 0) {
  stop("No CSV files found. Check the dataset path.")
}

cat("Files found:", length(files), "\n")

# Process each daily file
daily_results <- rbindlist(lapply(files, function(file) {

  dt <- fread(file, select = "failure")

  total_observations <- nrow(dt)

  failure_observations <- sum(
    dt$failure == 1,
    na.rm = TRUE
  )

  date <- sub("\\.csv$", "", basename(file))

  data.table(
    date = as.Date(date),
    total_observations = total_observations,
    failure_observations = failure_observations
  )
}))

# Calculate daily failure percentage
daily_results[, failure_percentage :=
  failure_observations / total_observations * 100
]

setorder(daily_results, date)

# Save daily results
fwrite(
  daily_results,
  file.path(output_dir, "daily_failures.csv")
)

# Overall results
total_observations <- sum(daily_results$total_observations)
total_failures <- sum(daily_results$failure_observations)

cat("\nOVERALL RESULTS\n")
cat("Daily files:", nrow(daily_results), "\n")
cat("Drive-day observations:", total_observations, "\n")
cat("Failure observations:", total_failures, "\n")

cat(
  "Failure percentage:",
  round(total_failures / total_observations * 100, 6),
  "%\n"
)

# Highest and lowest failure days
print(daily_results[which.max(failure_observations)])
print(daily_results[which.min(failure_observations)])

cat("\nCSV saved in results/daily_failures.csv\n")
