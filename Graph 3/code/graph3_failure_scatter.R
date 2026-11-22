
library(data.table)
library(ggplot2)

# Project directories
project_dir <- path.expand("~/STAT405_Project")
output_dir <- file.path(project_dir, "Graph 3")

# Read daily failure counts
daily_results <- fread(
  file.path(output_dir, "results/daily_failure_counts.csv")
)

# Identify days with failures
failure_days <- daily_results[failures > 0]

# Linear regression
model <- lm(failures ~ day, data = daily_results)

# Save regression results
sink(file.path(
  output_dir,
  "results/failure_scatter_regression.txt"
))

cat("Daily Hard-Drive Failure Count Regression\n\n")
print(summary(model))

sink()

# Scatterplot with regression line
failure_plot <- ggplot(
  daily_results,
  aes(x = day, y = failures)
) +
  geom_point(
    size = 3,
    color = "steelblue"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linewidth = 1
  ) +
  geom_text(
    data = failure_days,
    aes(label = paste0("June 30: ", failures, " failures")),
    nudge_y = 0.6,
    size = 4
  ) +
  scale_x_continuous(
    breaks = seq(1, 91, by = 10),
    limits = c(1, 91)
  ) +
  scale_y_continuous(
    breaks = seq(0, 9, by = 1),
    limits = c(0, 10)
  ) +
  labs(
    title = "Daily Hard-Drive Failures During Q2 2026",
    subtitle = "325,689 complete drives | 9 failures recorded on June 30",
    x = "Day of Quarter",
    y = "Number of Drive Failures"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save graph
ggsave(
  file.path(
    output_dir,
    "figures/daily_failure_scatter_regression.png"
  ),
  failure_plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("\nGraph created successfully.\n")
cat("Total failures:", sum(daily_results$failures), "\n")
cat("Days analyzed:", nrow(daily_results), "\n")
