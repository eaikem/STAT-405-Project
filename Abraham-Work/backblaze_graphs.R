library(data.table)
library(ggplot2)
library(parallel)

data_dir <- "Q2_2026/data_Q2_2026"
n_cores <- 1   # PARALLEL: set to the number of CPUs requested from CHTC (mclapply needs Linux/Mac)
dir.create("output", showWarnings = FALSE)

files <- sort(list.files(data_dir, full.names = TRUE))
if (length(files) == 0) stop("No files found in ", data_dir)
last_file <- files[length(files)]

header <- names(fread(files[1], nrows = 0))
warn_cols <- intersect(c("smart_5_raw", "smart_187_raw", "smart_197_raw", "smart_198_raw"), header)
keep <- c("serial_number", "model", "failure", "smart_9_raw", warn_cols)
cat("Warning columns used:", warn_cols, "\n")

get_manufacturer <- function(model) {
  m <- toupper(model)
  ifelse(grepl("^ST[0-9]|^SEAGATE", m), "Seagate",
  ifelse(grepl("^HGST|^HITACHI|^HU[HS]", m), "HGST",
  ifelse(grepl("^WDC|^WD", m), "Western Digital",
  ifelse(grepl("^TOSHIBA|^MG", m), "Toshiba", "Other"))))
}

# Handles one day of data. Each file is independent of the others.
process_file <- function(f) {
  dt <- fread(f, select = keep, integer64 = "double")
  dt[, age_bin := pmin(floor(smart_9_raw / 8766), 7)]   # 8766 power-on hours = 1 year

  age <- dt[!is.na(age_bin), .(drive_days = .N, failures = sum(failure)),
            by = .(model, age_bin)]
  failed <- dt[failure == 1]
  operating <- if (f == last_file) dt[failure == 0] else NULL

  list(age = age, failed = failed, operating = operating)
}

# PARALLEL: main bottleneck (~90 files, tens of millions of rows). Every file is
# independent, so this is the part to split up. Easiest: more n_cores above.
# On CHTC: one job per file (or per week), each saves its result with saveRDS(),
# then one final job combines them with the rbindlist() lines below.
results <- mclapply(files, process_file, mc.cores = n_cores)

age       <- rbindlist(lapply(results, `[[`, "age"))
failed    <- rbindlist(lapply(results, `[[`, "failed"))
operating <- rbindlist(lapply(results, `[[`, "operating"))

# ---- Graph 1: annualized failure rate by drive age ----
age[, manufacturer := get_manufacturer(model)]
age <- age[manufacturer != "Other",
           .(drive_days = sum(drive_days), failures = sum(failures)),
           by = .(manufacturer, age_bin)]
age <- age[drive_days >= 20000]
age[, afr := failures / drive_days * 365 * 100]
setorder(age, manufacturer, age_bin)

age_labels <- c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7+")

p1 <- ggplot(age, aes(x = age_bin, y = afr, color = manufacturer)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:7, labels = age_labels) +
  labs(title = "Annualized failure rate by drive age and manufacturer",
       subtitle = "Backblaze, Q2 2026",
       x = "Drive age (years)", y = "Annualized failure rate (%)",
       color = "Manufacturer") +
  theme_minimal()

print(p1)
ggsave("output/graph1_afr_by_age.png", p1, width = 9, height = 5.5, dpi = 300)

# ---- Graph 2: warning signals, failed vs. operating drives ----
failed[, status := "Failed"]
operating[, status := "Operating"]
d <- rbind(failed, operating)
d[, manufacturer := get_manufacturer(model)]

warn_vals <- as.matrix(d[, ..warn_cols])
d[, has_data := rowSums(!is.na(warn_vals)) > 0]
d[, warn := rowSums(warn_vals > 0, na.rm = TRUE) > 0]
cat("Drives dropped for missing SMART data:", sum(!d$has_data), "of", nrow(d), "\n")
d <- d[has_data == TRUE & manufacturer != "Other"]

# PARALLEL: if you extend this to many quarters, or add a bootstrap or
# permutation test on d (resampling thousands of times), that is a good
# CHTC job too since each resample is independent.

sig <- d[, .(n = .N, pct_warn = mean(warn) * 100), by = .(manufacturer, status)]
enough <- sig[status == "Failed" & n >= 20, manufacturer]
sig <- sig[manufacturer %in% enough]
sig[, status := factor(status, levels = c("Operating", "Failed"))]

p2 <- ggplot(sig, aes(x = manufacturer, y = pct_warn, fill = status)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0("n=", n)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c(Operating = "grey65", Failed = "firebrick")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Drives with a nonzero SMART warning value, by manufacturer",
       subtitle = "Failed drives on failure day vs. operating drives on 2026-06-30",
       x = NULL, y = "Drives with a warning (%)", fill = NULL) +
  theme_minimal()

print(p2)
ggsave("output/graph2_warning_signals.png", p2, width = 9, height = 5.5, dpi = 300)

# ---- numbers for the write-up ----
print(age)
print(sig)
cat("Overall AFR (%):", round(sum(age$failures) / sum(age$drive_days) * 365 * 100, 2), "\n")
