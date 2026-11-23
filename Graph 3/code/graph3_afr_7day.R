
# Seven-day AFR analysis: June 24-30, 2026

data <- read.csv("Graph 3/results/afr_7day_summary.csv")

# Require at least 10,000 drive-days per model
data <- subset(data, drive_days >= 10000)

# Exact Poisson confidence intervals
data$lower_count <- ifelse(
  data$failures == 0,
  0,
  qchisq(0.025, 2 * data$failures) / 2
)

data$upper_count <- qchisq(
  0.975,
  2 * (data$failures + 1)
) / 2

# Annualized failure rate
data$AFR <- data$failures / data$drive_days * 365 * 100
data$lower <- data$lower_count / data$drive_days * 365 * 100
data$upper <- data$upper_count / data$drive_days * 365 * 100

data <- data[order(data$AFR), ]

# Save results
write.csv(
  data,
  "Graph 3/results/afr_7day_by_model.csv",
  row.names = FALSE
)

# Create forest plot
png(
  "Graph 3/figures/afr_7day_by_model.png",
  width = 1600,
  height = max(900, nrow(data) * 50),
  res = 150
)

par(mar = c(5, 14, 5, 2))

n <- nrow(data)

plot(
  data$AFR,
  seq_len(n),
  xlim = c(0, max(data$upper) * 1.15),
  ylim = c(0.5, n + 0.5),
  yaxt = "n",
  xlab = "Annualized Failure Rate (%)",
  ylab = "",
  main = "Seven-Day Annualized Failure Rate by Drive Model",
  pch = 19
)

axis(
  2,
  at = seq_len(n),
  labels = data$model,
  las = 1,
  cex.axis = 0.75
)

arrows(
  data$lower,
  seq_len(n),
  data$upper,
  seq_len(n),
  angle = 90,
  code = 3,
  length = 0.04
)

points(data$AFR, seq_len(n), pch = 19)

dev.off()

cat("Models included:", nrow(data), "\n")
cat("Total failures:", sum(data$failures), "\n")
cat("Total drive-days:", sum(data$drive_days), "\n")
