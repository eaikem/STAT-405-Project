# Read model failure summary
data <- read.csv("Graph 3/results/model_failures.csv")

# Sort models by number of failures
data <- data[order(data$failures), ]

# Create PNG graph
png(
  "Graph 3/figures/failures_by_model.png",
  width = 1400,
  height = 900,
  res = 150
)

par(mar = c(5, 14, 5, 2))

# Horizontal bar chart
barplot(
  data$failures,
  names.arg = data$model,
  horiz = TRUE,
  las = 1,
  col = "steelblue",
  border = NA,
  xlim = c(0, max(data$failures) + 1),
  main = "Distribution of Drive Failures by Model",
  xlab = "Number of Failures",
  cex.names = 0.85
)

# Add counts at the end of each bar
text(
  x = data$failures,
  y = seq_along(data$failures),
  labels = data$failures,
  pos = 4,
  cex = 0.9
)

dev.off()

