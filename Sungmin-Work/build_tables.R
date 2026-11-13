# STAT 405 Group 10 - Build the analysis tables from the Backblaze daily CSVs
#
# Turns the 91 Q2 2026 daily files into two small tables:
#   drives.csv - one row per drive (input to regression.R and the AFR graphs)
#   window.csv - the last WINDOW+1 days before failure for every failed drive,
#                plus the same-length stretch for a random sample of healthy drives
#
# ---- Option A: one machine (e.g. the stats server) --------------------------
#   Rscript build_tables.R all  <folder with the daily CSVs>  <output folder>
#
# ---- Option B: CHTC (one job per day, then one combine job) ------------------
#   Rscript build_tables.R day      2026-04-01.csv   slim/        # 91 jobs
#   Rscript build_tables.R combine  slim/            <output folder>
#
# Optional 4th argument: window length in days (default 14).

suppressPackageStartupMessages(library(data.table))

args   <- commandArgs(trailingOnly = TRUE)
mode   <- if (length(args) >= 1) args[1] else "all"
input  <- if (length(args) >= 2) args[2] else "data_Q2_2026"
output <- if (length(args) >= 3) args[3] else "tables"
WINDOW <- if (length(args) >= 4) as.integer(args[4]) else 14L
N_HEALTHY <- 2000L          # healthy drives sampled for the window comparison
set.seed(405)

COLS <- c("date", "serial_number", "model", "capacity_bytes", "failure",
          "smart_9_raw", "smart_194_raw", "smart_5_raw", "smart_187_raw", "smart_197_raw")

# ---- Step 1: one day -> slim file (only the columns we use) ----------------------
process_day <- function(csv, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  header <- names(fread(csv, nrows = 0, integer64 = "double"))
  dt <- fread(csv, select = intersect(COLS, header), integer64 = "double")
  for (col in setdiff(COLS, names(dt))) set(dt, j = col, value = NA_real_)   # column absent that day
  for (col in setdiff(COLS, c("date", "serial_number", "model")))            # same type every day
    set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
  dt[, date := as.IDate(date)]
  out <- file.path(out_dir, sub("\\.csv$", ".rds", basename(csv)))
  saveRDS(dt[, ..COLS], out)
  out
}

# ---- Step 2: all slim files -> drives.csv + window.csv ---------------------------
# Reads one day at a time, so memory stays around 1-2 GB even for 31.5M drive-days.
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

combine <- function(slim_dir, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  files <- sort(list.files(slim_dir, pattern = "\\.rds$", full.names = TRUE))
  if (!length(files)) stop("No .rds files in ", slim_dir)

  # Pass 1: running per-drive summary
  agg <- NULL; n_rows <- 0
  for (i in seq_along(files)) {
    day <- unique(readRDS(files[i]), by = c("serial_number", "date"))
    n_rows <- n_rows + nrow(day)
    day <- day[, .(model, capacity_bytes, failure = as.integer(failure %in% 1), days = 1L,
                   first_date = date, last_date = date,
                   fail_date = fifelse(failure %in% 1, date, as.IDate(NA)),
                   age_hours = smart_9_raw,
                   temp_sum = fcoalesce(smart_194_raw, 0), temp_n = as.integer(!is.na(smart_194_raw)),
                   smart_5_max = smart_5_raw, smart_187_max = smart_187_raw,
                   smart_197_max = smart_197_raw,
                   smart_5_first = smart_5_raw, smart_187_first = smart_187_raw,
                   smart_197_first = smart_197_raw, serial_number)]
    agg <- rbind(agg, day)[, .(
      smart_5_first   = smart_5_first[which.min(first_date)],
      smart_187_first = smart_187_first[which.min(first_date)],
      smart_197_first = smart_197_first[which.min(first_date)],
      model = model[.N], capacity_bytes = safe_max(capacity_bytes),
      failure = max(failure), days = sum(days),
      first_date = min(first_date), last_date = max(last_date),
      fail_date = if (all(is.na(fail_date))) as.IDate(NA) else min(fail_date, na.rm = TRUE),
      age_hours = safe_max(age_hours), temp_sum = sum(temp_sum), temp_n = sum(temp_n),
      smart_5_max = safe_max(smart_5_max), smart_187_max = safe_max(smart_187_max),
      smart_197_max = safe_max(smart_197_max)
    ), by = serial_number]
    if (i %% 10 == 0 || i == length(files)) cat(sprintf("  summarized %d / %d days\n", i, length(files)))
  }
  drives <- agg[, .(serial_number, model, capacity_bytes, failure, days_observed = days,
                    age_hours, temp_mean = fifelse(temp_n > 0, temp_sum / temp_n, NA_real_),
                    smart_5_max, smart_187_max, smart_197_max,
                    smart_5_first, smart_187_first, smart_197_first)]
  fwrite(drives, file.path(out_dir, "drives.csv"))
  cat(sprintf("Read %d days: %s drive-day rows\n", length(files), format(n_rows, big.mark = ",")))
  cat(sprintf("drives.csv: %s drives, %s failed\n",
              format(nrow(drives), big.mark = ","), format(sum(drives$failure), big.mark = ",")))

  # Which drives and which dates go into window.csv
  failed <- agg[failure == 1 & fail_date - first_date >= WINDOW,
                .(serial_number, end = fail_date, group = "Failed")]
  cat(sprintf("Failed drives with a full %d-day window: %d of %d\n",
              WINDOW, nrow(failed), sum(agg$failure)))
  pool <- agg[failure == 0 & last_date - first_date >= WINDOW]
  pool <- pool[sample.int(.N, min(N_HEALTHY, .N))]
  healthy <- pool[, .(serial_number,
                      end = first_date + WINDOW +
                        floor(runif(.N) * (as.integer(last_date - first_date) - WINDOW + 1)),
                      group = "Healthy")]
  ends <- rbind(failed, healthy)

  # Pass 2: keep only the window rows of the selected drives
  w <- rbindlist(lapply(files, function(f) {
    day <- readRDS(f)[serial_number %in% ends$serial_number]
    day[ends, on = "serial_number", nomatch = 0L][date >= end - WINDOW & date <= end]
  }))
  w <- unique(w, by = c("serial_number", "date"))
  w[, days_before := as.integer(end - date)]
  setorder(w, group, serial_number, date)
  fwrite(w[, .(serial_number, model, group, date, days_before,
               smart_5_raw, smart_187_raw, smart_197_raw)],
         file.path(out_dir, "window.csv"))
  cat(sprintf("window.csv: %s rows (%d failed + %d healthy drives)\n",
              format(nrow(w), big.mark = ","), nrow(failed), nrow(healthy)))
}

# ---- Run -----------------------------------------------------------------------
t0 <- Sys.time()
if (mode == "day") {
  process_day(input, output)
} else if (mode == "combine") {
  combine(input, output)
} else if (mode == "all") {
  csvs <- sort(list.files(input, pattern = "^\\d{4}-\\d{2}-\\d{2}\\.csv$", full.names = TRUE, recursive = TRUE))
  if (!length(csvs)) stop("No daily CSVs (YYYY-MM-DD.csv) in ", input)
  slim <- file.path(output, "slim")
  for (i in seq_along(csvs)) {
    process_day(csvs[i], slim)
    if (i %% 10 == 0 || i == length(csvs)) cat(sprintf("  processed %d / %d days\n", i, length(csvs)))
  }
  combine(slim, output)
} else stop("mode must be 'day', 'combine' or 'all'")

mem <- gc()
cat(sprintf("Elapsed: %.1f min. Peak R memory: %.0f MB.\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins")), sum(mem[, ncol(mem)])))
