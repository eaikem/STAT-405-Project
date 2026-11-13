# Makes FAKE drives.csv and window.csv with the same columns the pipeline will
# produce, so the graph script can be tested before the real data is ready.
# Usage: Rscript simulate_data.R
suppressPackageStartupMessages(library(data.table))
set.seed(405)

models <- data.table(
  model = c("ST12000NM0008", "ST14000NM001G", "ST16000NM001G", "ST8000NM0055",
            "ST10000NM0086", "ST20000NM007D", "ST24000NM002H",
            "HGST HUH721212ALE604", "HGST HUH721212ALN604", "HGST HMS5C4040BLE640",
            "TOSHIBA MG07ACA14TA", "TOSHIBA MG08ACA16TEY", "TOSHIBA MG09ACA18TE",
            "TOSHIBA MG10ACA20TE", "TOSHIBA MG11ACA24TE",
            "WDC WUH721816ALE6L4", "WDC WUH722222ALE6L4", "WDC WUH721414ALE6L4",
            "WDC WUH722626ALE6L4", "ST4000DM000", "HGST HUH728080ALE600", "ST6000DX000"),
  tb    = c(12, 14, 16, 8, 10, 20, 24, 12, 12, 4, 14, 16, 18, 20, 24, 16, 22, 14, 26, 4, 8, 6),
  n     = c(4000, 5000, 6000, 2500, 800, 4000, 3000, 2500, 2000, 1500,
            5000, 4000, 3000, 2500, 1500, 5000, 3500, 1200, 600, 60, 90, 40),
  age_y = c(5.5, 4.5, 3.5, 7.5, 7, 2, 1, 6, 5.5, 9, 5, 4, 3, 2, 1, 3, 2, 4.5, 0.6, 9, 8, 10),
  risk  = c(1.2, 1.1, 0.8, 1.6, 4.5, 0.7, 0.9, 3.5, 0.9, 1.4, 0.9, 0.6, 0.7, 0.6, 1.0,
            0.5, 0.6, 0.7, 1.0, 1.5, 1.2, 2.0)
)

d <- models[rep(seq_len(.N), n)]
d[, serial_number := sprintf("SIM%07d", .I)]
d[, capacity_bytes := tb * 1e12]
d[, age_hours := pmax(200, rnorm(.N, age_y, 0.4) * 8760)]
d[, temp_mean := rnorm(.N, 30 + (tb > 16) * 2, 3)]
d[, smart_5_max   := rpois(.N, 0.05) * rbinom(.N, 1, 0.3)]
d[, smart_187_max := fifelse(startsWith(model, "ST"), rbinom(.N, 1, 0.02) * rpois(.N, 3), NA_real_)]
d[, smart_197_max := rbinom(.N, 1, 0.01) * rpois(.N, 2)]
d[, days_observed := fifelse(runif(.N) < 0.05, sample(10:90, .N, TRUE), 91L)]

daily <- 0.0173 / 365 * d$risk * exp(0.05 * (d$age_hours / 8760) +
         1.2 * (d$smart_5_max > 0) + 1.5 * (fcoalesce(d$smart_187_max, 0) > 0) +
         2.0 * (d$smart_197_max > 0))
p_fail <- 1 - exp(-daily * d$days_observed)
d[, failure := rbinom(.N, 1, p_fail)]
d[failure == 1, days_observed := sample(15:91, .N, TRUE)]

fwrite(d[, .(serial_number, model, capacity_bytes, failure, days_observed,
             age_hours, temp_mean, smart_5_max, smart_187_max, smart_197_max)],
       "drives.csv")

# window.csv: 15 days per drive. Failed drives get rising warnings near the end.
failed  <- d[failure == 1, .(serial_number, model, group = "Failed")]
healthy <- d[failure == 0][sample(.N, 2000), .(serial_number, model, group = "Healthy")]
win <- rbind(failed, healthy)[, .(days_before = 14:0), by = .(serial_number, model, group)]
ramp <- function(g, k, base, slope) fifelse(g == "Failed", base + slope * (14 - k) / 14, base)
win[, smart_5_raw   := rbinom(.N, 1, ramp(group, days_before, 0.03, 0.25)) * rpois(.N, 8)]
win[, smart_187_raw := fifelse(startsWith(model, "ST"),
                               rbinom(.N, 1, ramp(group, days_before, 0.02, 0.30)) * rpois(.N, 3), NA_real_)]
win[, smart_197_raw := rbinom(.N, 1, ramp(group, days_before, 0.01, 0.35)) * rpois(.N, 4)]
fwrite(win[, .(serial_number, group, days_before, smart_5_raw, smart_187_raw, smart_197_raw)],
       "window.csv")

cat("Fake data:", nrow(d), "drives,", sum(d$failure), "failures\n")
