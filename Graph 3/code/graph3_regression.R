
library(data.table)
library(ggplot2)

# Project directories
data_dir <- "data/raw/data_Q2_2026"
output_dir <- "Graph 3"

# Read list of eligible drives
complete_drives <- scan(
  "Graph 3/results/complete_drives.txt",
  what = character(),
  quiet = TRUE
)

cat("Eligible drives:", length(complete_drives), "\n")

# Find daily CSV files
files <- sort(list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
))

stopifnot(length(files) == 91)

# Store daily results
daily_results <- data.table()

# Process each day
for (i in seq_along(files)) {

  cat("Processing:", basename(files[i]), "\n")

  # Read only necessary columns
  dt <- fread(
    files[i],
    select = c("serial_number", "failure")
  )

  dt[, serial_number := as.character(serial_number)]

  # Keep only eligible drives
  eligible <- dt[serial_number %chin% complete_drives]

  # Validate one record per eligible drive per day
  if (nrow(eligible) != length(complete_drives) ||
      uniqueN(eligible$serial_number) != length(complete_drives)) {
    stop(paste("Completeness validation failed:", basename(files[i])))
  }

  # Count failures
  failures <- sum(
    as.integer(eligible$failure) == 1,
    na.rm = TRUE
  )

  # Calculate daily failure rate
  failure_rate <- failures / length(complete_drives) * 100

  # Extract date
  date <- as.Date(
    sub("\\.csv$", "", basename(files[i]))
  )

  daily_results <- rbind(
    daily_results,
    data.table(
      date = date,
      day = i,
      drives_observed = nrow(eligible),
      failures = failures,
      failure_rate = failure_rate
    )
  )

  rm(dt, eligible)
  gc()
}

# Save daily summary
fwrite(
  daily_results,
  file.path(output_dir, "results", "daily_failure_rates.csv")
)

# Linear regression using failure counts
model <- lm(failures ~ day, data = daily_results)

# Save regression results
capture.output(
  summary(model),
  file = file.path(output_dir, "results", "regression_results.txt")
)

# Create graph using actual failure counts
regression_plot <- ggplot(
  daily_results,
  aes(x = day, y = failures)
) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = "darkred",
    se = TRUE
  ) +
  labs(
    title = "Daily Hard-Drive Failures Over Time",
    subtitle = "Backblaze Q2 2026 - 325,689 Complete Drives",
    x = "Day of Quarter",
    y = "Number of Drive Failures"
  ) +
  theme_minimal()

# Save graph
ggsave(
  file.path(output_dir, "figures", "daily_failures_regression.png"),
  regression_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Print results
print(summary(model))

cat("\nGraph 3 analysis completed successfully.\n")
cat("Drives analyzed:", length(complete_drives), "\n")
cat("Daily observations:", nrow(daily_results), "\n")
