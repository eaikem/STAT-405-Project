library(data.table)
library(ggplot2)

# Project directories
project_dir <- path.expand("~/STAT405_Project")
data_dir <- file.path(project_dir, "data/raw/data_Q2_2026")
output_dir <- file.path(project_dir, "Graph 3")

# Read list of drives with observations for all 91 days
complete_drives <- fread(
  file.path(output_dir, "results/complete_drives.txt"),
  header = FALSE,
  col.names = "serial_number"
)

complete_drives <- unique(complete_drives$serial_number)

cat("Complete drives:", length(complete_drives), "\n")

# Get all 91 daily CSV files
files <- sort(list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE
))

if (length(files) != 91) {
  stop("Expected 91 daily CSV files, found ", length(files))
}

# Calculate daily failure counts
daily_results <- rbindlist(
  lapply(seq_along(files), function(i) {

    dat <- fread(
      files[i],
      select = c("date", "serial_number", "failure")
    )

    # Keep only complete drives
    dat <- dat[serial_number %in% complete_drives]

    # Check that every complete drive appears once
    if (nrow(dat) != length(complete_drives)) {
      stop(
        "Unexpected number of complete-drive observations on ",
        basename(files[i]),
        ": ", nrow(dat)
      )
    }

    data.table(
      date = dat$date[1],
      day = i,
      failures = sum(dat$failure == 1, na.rm = TRUE),
      total_drives = nrow(dat)
    )
  })
)

# Calculate failure percentage for reference
daily_results[, failure_rate := 100 * failures / total_drives]

# Save daily results
fwrite(
  daily_results,
  file.path(output_dir, "results/daily_failure_counts.csv")
)

# Linear regression using daily failure counts
model <- lm(failures ~ day, data = daily_results)

# Save regression results
sink(file.path(output_dir, "results/failure_counts_regression.txt"))
cat("Linear Regression: Daily Failure Counts ~ Day\n\n")
print(summary(model))
sink()

# Create bar chart with regression line
failure_plot <- ggplot(
  daily_results,
  aes(x = day, y = failures)
) +
  geom_col(fill = "steelblue", width = 0.9) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 1
  ) +
  scale_x_continuous(
    breaks = seq(1, 91, by = 10)
  ) +
  labs(
    title = "Daily Hard-Drive Failures Over Q2 2026",
    subtitle = "325,689 drives observed for all 91 days",
    x = "Day of Quarter",
    y = "Number of Drive Failures"
  ) +
  theme_minimal(base_size = 13)

# Save graph
ggsave(
  file.path(
    output_dir,
    "figures/daily_failures_counts_regression.png"
  ),
  failure_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Print results
cat("\nAnalysis complete.\n")
cat("Complete drives:", length(complete_drives), "\n")
cat("Days analyzed:", nrow(daily_results), "\n")
cat("Total failures:", sum(daily_results$failures), "\n")
cat("\nRegression results:\n")
print(summary(model))

cat(
  "\nGraph saved to: ",
  file.path(
    output_dir,
    "figures/daily_failures_counts_regression.png"
  ),
  "\n"
)
