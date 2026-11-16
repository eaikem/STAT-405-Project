
library(data.table)
library(ggplot2)

# Read daily failure summary
failures <- fread("results/daily_failures.csv")

# Convert date column
failures[, date := as.Date(date)]

# Graph 1: Daily failure observations
graph1 <- ggplot(
  failures,
  aes(x = date, y = failure_observations)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Daily Hard Drive Failure Observations",
    subtitle = "Backblaze Q2 2026",
    x = "Date",
    y = "Number of Failure Observations"
  ) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "results/daily_failure_counts.png",
  plot = graph1,
  width = 10,
  height = 5,
  dpi = 300
)

# Graph 2: Daily failure percentage
graph2 <- ggplot(
  failures,
  aes(x = date, y = failure_percentage)
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Daily Hard Drive Failure Percentage",
    subtitle = "Backblaze Q2 2026",
    x = "Date",
    y = "Failure Observations (%)"
  ) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "results/daily_failure_percentage.png",
  plot = graph2,
  width = 10,
  height = 5,
  dpi = 300
)

cat("Both graphs were successfully saved.\n")
