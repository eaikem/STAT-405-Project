# STAT 405 Group 10 - Regression analysis (Backblaze Q2 2026)
#
# Question: Which drive models are the most reliable, and what factors are
#           associated with a higher probability of failure?
#
# Usage:  Rscript regression.R drives.csv results/
#
# Input: drives.csv, one row per drive (columns listed in README.md)
# Output (in results/):
#   reg_graph_main.png          Hazard ratios: drive models + drive condition
#   reg_graph_warning_by_mfr.png  Does a SMART warning mean the same thing for each manufacturer?
#   reg_main_coefficients.csv   Numbers behind the main graph
#   reg_warning_by_mfr.csv      Numbers behind the second graph
#   reg_summary.txt             Diagnostics + key numbers for the write-up
#
# ---------------------------------------------------------------------------
# Why this model (for the Methods paragraph):
# Drives are observed for different numbers of days (new drives join mid-quarter,
# failed drives leave the data after failing). So days observed is exposure, not
# a predictor. We fit a binomial GLM with a complementary log-log link and
# offset log(days observed). This is the drive-level form of a discrete-time
# (per-day) failure model; exp(coef) is a hazard ratio, and with a rare event
# (about 0.005% of drive-days) it is almost identical to an odds ratio from
# logistic regression.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args        <- commandArgs(trailingOnly = TRUE)
drives_file <- if (length(args) >= 1) args[1] else "drives.csv"
out_dir     <- if (length(args) >= 2) args[2] else "results"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

MIN_DRIVES     <- 100      # Backblaze's own inclusion rule for a model
MIN_DRIVE_DAYS <- 10000
MIN_CAPACITY_TB <- 1       # drop boot drives (small SSD/HDD boot disks)

log_lines <- character()
say <- function(...) { line <- sprintf(...); cat(line, "\n"); log_lines <<- c(log_lines, line) }

# ---- Look -------------------------------------------------------------------
ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#898781"; grid_col <- "#e1e0d9"
blue <- "#2a78d6"
theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(text = element_text(colour = ink), axis.text = element_text(colour = ink2),
          axis.title = element_text(colour = ink2),
          panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
          panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          strip.text.y = element_text(face = "bold", colour = ink, angle = 0, hjust = 0),
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = ink2),
          plot.caption = element_text(colour = muted, hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          plot.background = element_rect(fill = "white", colour = NA))
}

manufacturer_of <- function(model) {
  m <- toupper(trimws(model))
  fcase(grepl("^(ST|SEAGATE)", m),       "Seagate",
        grepl("^(HGST|HUH|HUS|HMS)", m), "HGST",
        grepl("^(TOSHIBA|MG)", m),       "Toshiba",
        grepl("^(WDC|WUH|WD)", m),       "WDC",
        default = "Other")
}

# ---- Data ---------------------------------------------------------------------
d <- fread(drives_file, integer64 = "double")
n_raw <- nrow(d)
d[, manufacturer := manufacturer_of(model)]

# The same model is sometimes recorded with and without a maker prefix
# (e.g. "WDC WUH721816ALE6L4" and "WUH721816ALE6L4"). Merge them, but report each spelling.
d[, model_raw := model]
d[, model := sub("^(WDC|HGST|TOSHIBA|SEAGATE|HITACHI) +", "", toupper(trimws(model)))]
alias <- d[, .(spellings = uniqueN(model_raw)), by = model][spellings > 1, model]
if (length(alias)) {
  say("Model names recorded in more than one spelling (merged):")
  tab <- d[model %in% alias, .(drives = .N, failures = sum(failure),
                                drive_days = sum(days_observed)), by = .(model, model_raw)]
  tab[, afr := round(100 * 365 * failures / drive_days, 2)]
  for (i in seq_len(nrow(tab)))
    say("  %-22s as %-24s drives = %7s, failures = %4d, AFR = %6.2f%%", tab$model[i], tab$model_raw[i],
        format(tab$drives[i], big.mark = ","), tab$failures[i], tab$afr[i])
}
d[, capacity_tb := capacity_bytes / 1e12]
d <- d[capacity_tb >= MIN_CAPACITY_TB & manufacturer != "Other" & days_observed > 0]

by_model <- d[, .(drives = .N, drive_days = sum(days_observed), failures = sum(failure)), by = model]
keep <- by_model[drives > MIN_DRIVES & drive_days > MIN_DRIVE_DAYS, model]
d <- d[model %in% keep]
say("Drives: %s in file -> %s after dropping boot drives, 'Other' makers and small models (%d models kept).",
    format(n_raw, big.mark = ","), format(nrow(d), big.mark = ","), length(keep))
say("Failures: %s over %s drive-days.", format(sum(d$failure), big.mark = ","),
    format(sum(d$days_observed), big.mark = ","))

# Predictors. SMART 187 is reported only by Seagate drives, so the warning flag
# uses SMART 5 and 197, which every manufacturer reports.
# Main analysis: SMART values on each drive's FIRST day in Q2 (measured before any
# failure). The quarter maximum includes the failure day itself, so it is only
# used as a comparison.
num <- function(x) fcoalesce(as.numeric(x), 0)
WARN <- if ("smart_5_first" %in% names(d)) "first" else "max"
d[, `:=`(
  age_years   = age_hours / 8760,
  temp_5c     = temp_mean / 5,
  realloc     = as.integer(num(get(paste0("smart_5_", WARN)))   > 0),
  pending     = as.integer(num(get(paste0("smart_197_", WARN))) > 0),
  realloc_max = as.integer(num(smart_5_max)   > 0),
  pending_max = as.integer(num(smart_197_max) > 0)
)]
say("Warning signals measured on: %s.",
    if (WARN == "first") "each drive's first day in Q2 (before failure)"
    else "the quarter maximum (includes the failure day; rerun build_tables.R for first-day values)")
d[, any_warning := as.integer(realloc | pending)]
d <- d[!is.na(age_years) & !is.na(temp_5c)]

ref_model <- by_model[model %in% keep][which.max(drive_days), model]
d[, model := relevel(factor(model), ref = ref_model)]
d[, manufacturer := relevel(factor(manufacturer), ref = "Seagate")]

ratio_table <- function(fit, pattern = NULL) {
  est <- data.table(term = names(coef(fit)), b = coef(fit), se = sqrt(diag(vcov(fit))))
  est <- est[term != "(Intercept)" & !is.na(b)]
  if (!is.null(pattern)) est <- est[grepl(pattern, term)]
  est[, `:=`(hr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se),
             p = 2 * pnorm(-abs(b / se)))]
  est[]
}

# =============================================================================
# Model 1 (main): which models are more / less reliable, holding age,
# temperature and warning signals fixed?
#   failure ~ model + age + temperature + reallocated>0 + pending>0
# =============================================================================
f1 <- failure ~ model + age_years + temp_5c + realloc + pending
fit1 <- glm(f1, family = binomial(link = "cloglog"), offset = log(days_observed), data = d)

# Does drive model matter after adjusting? Likelihood-ratio tests per term.
lrt <- drop1(fit1, test = "LRT")
say("\nLikelihood-ratio tests (Model 1):")
for (t in rownames(lrt)[-1])
  say("  %-12s chi-sq = %8.1f, df = %3d, p = %s", t, lrt[t, "LRT"], lrt[t, "Df"],
      format.pval(lrt[t, "Pr(>Chi)"], digits = 3, eps = 1e-16))

est1 <- ratio_table(fit1)
pretty <- c(age_years = "Age (+1 year powered on)",
            temp_5c   = "Temperature (+5 °C)",
            realloc   = "Reallocated sectors > 0 at start (SMART 5)",
            pending   = "Pending sectors > 0 at start (SMART 197)")
est1[, group := fifelse(startsWith(term, "model"), "Drive model", "Drive condition")]
est1[, label := fifelse(group == "Drive model", sub("^model", "", term), pretty[term])]
mfr_of_model <- unique(d[, .(model = as.character(model), manufacturer = as.character(manufacturer))], by = "model")
est1[group == "Drive model", label := paste0(label, " (", mfr_of_model[match(label, model), manufacturer], ")")]
unstable <- est1[se > 5]                     # model with ~no failures: unbounded estimate
est1_plot <- est1[se <= 5]
est1_plot[, label := factor(label, levels = est1_plot[order(group, hr), label])]
est1_plot[, group := factor(group, levels = c("Drive condition", "Drive model"))]
fwrite(est1[, .(group, label, hazard_ratio = hr, ci_low = lo, ci_high = hi, p_value = p)],
       file.path(out_dir, "reg_main_coefficients.csv"))

if (WARN == "first") {
  fit1_max <- glm(failure ~ model + age_years + temp_5c + realloc_max + pending_max,
                  family = binomial(link = "cloglog"), offset = log(days_observed), data = d)
  em <- ratio_table(fit1_max, "_max$")
  say("\nComparison - same model with quarter-maximum SMART (includes failure day):")
  for (i in seq_len(nrow(em)))
    say("  %-12s %7.2f  (%.2f-%.2f)", em$term[i], em$hr[i], em$lo[i], em$hi[i])
}

say("\nModel 1 - drive condition (hazard ratio, 95%% CI):")
for (i in which(est1$group == "Drive condition"))
  say("  %-46s %6.2f  (%.2f-%.2f)", est1$label[i], est1$hr[i], est1$lo[i], est1$hi[i])
mods <- est1[group == "Drive model" & se <= 5][order(hr)]
say("\nModel 1 - most reliable models vs %s (lowest hazard ratios):", ref_model)
for (i in seq_len(min(3, nrow(mods))))
  say("  %-36s %6.2f  (%.2f-%.2f)", mods$label[i], mods$hr[i], mods$lo[i], mods$hi[i])
say("Least reliable models vs %s:", ref_model)
for (i in rev(tail(seq_len(nrow(mods)), 3)))
  say("  %-36s %6.2f  (%.2f-%.2f)", mods$label[i], mods$hr[i], mods$lo[i], mods$hi[i])
if (nrow(unstable)) say("Not estimable (no/few failures): %s", paste(sub("^model", "", unstable$term), collapse = ", "))

p1 <- ggplot(est1_plot, aes(y = label)) +
  geom_vline(xintercept = 1, colour = muted, linewidth = 0.4) +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = blue, linewidth = 0.6, alpha = 0.55) +
  geom_point(aes(x = hr), colour = blue, size = 2.2) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_x_log10(labels = function(x) format(x, drop0trailing = TRUE, trim = TRUE)) +
  labs(title = "Factors associated with drive failure, Q2 2026",
       subtitle = paste0("Hazard ratio with 95% CI (log scale). Right of 1 = higher failure risk.\n",
                         "Model ratios are relative to ", ref_model, " (most drive-days), ",
                         "holding age, temperature and warnings fixed."),
       x = "Hazard ratio", y = NULL,
       caption = sprintf(paste0("Binomial GLM, cloglog link, offset log(days observed). ",
                                "n = %s drives, %s failures.%s"),
                         format(nrow(d), big.mark = ","), format(sum(d$failure), big.mark = ","),
                         if (nrow(unstable)) sprintf(" %d model(s) with no failures not shown.",
                                                     nrow(unstable)) else "")) +
  theme_report()
ggsave(file.path(out_dir, "reg_graph_main.png"), p1,
       width = 8, height = 1.9 + 0.24 * nrow(est1_plot), dpi = 300)

# =============================================================================
# Model 2: does a warning signal mean the same thing for every manufacturer?
#   failure ~ manufacturer + capacity + age + temperature + manufacturer:any_warning
# The nested term gives one warning hazard ratio per manufacturer; the LRT
# against a single common warning effect tests whether they differ.
# =============================================================================
fit2_common <- glm(failure ~ manufacturer + capacity_tb + age_years + temp_5c + any_warning,
                   family = binomial(link = "cloglog"), offset = log(days_observed), data = d)
fit2 <- glm(failure ~ manufacturer + capacity_tb + age_years + temp_5c + manufacturer:any_warning,
            family = binomial(link = "cloglog"), offset = log(days_observed), data = d)
lrt2 <- anova(fit2_common, fit2, test = "LRT")
say("\nModel 2 - warning effect differs by manufacturer? LRT chi-sq = %.1f, df = %d, p = %s",
    lrt2$Deviance[2], lrt2$Df[2], format.pval(lrt2$`Pr(>Chi)`[2], digits = 3, eps = 1e-16))

est2 <- ratio_table(fit2, ":any_warning$")
est2[, manufacturer := sub("^manufacturer(.*):any_warning$", "\\1", term)]
fwrite(est2[, .(manufacturer, warning_hazard_ratio = hr, ci_low = lo, ci_high = hi, p_value = p)],
       file.path(out_dir, "reg_warning_by_mfr.csv"))
say("Warning hazard ratio by manufacturer:")
for (i in seq_len(nrow(est2)))
  say("  %-8s %7.1f  (%.1f-%.1f)", est2$manufacturer[i], est2$hr[i], est2$lo[i], est2$hi[i])

est2[, manufacturer := factor(manufacturer, levels = est2[order(hr), manufacturer])]
p2 <- ggplot(est2, aes(y = manufacturer)) +
  geom_vline(xintercept = 1, colour = muted, linewidth = 0.4) +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = blue, linewidth = 0.6, alpha = 0.55) +
  geom_point(aes(x = hr), colour = blue, size = 2.4) +
  scale_x_log10(labels = function(x) format(x, drop0trailing = TRUE, trim = TRUE)) +
  labs(title = "How much a SMART warning raises failure risk, by manufacturer",
       subtitle = "Hazard ratio for drives with reallocated or pending sectors > 0 at the start of Q2 vs. none (95% CI, log scale)",
       x = "Hazard ratio", y = NULL,
       caption = sprintf(paste0("Adjusted for capacity, age and temperature. Test that the effect is ",
                                "the same for all manufacturers: p = %s."),
                         format.pval(lrt2$`Pr(>Chi)`[2], digits = 2, eps = 1e-16))) +
  theme_report()
ggsave(file.path(out_dir, "reg_graph_warning_by_mfr.png"), p2, width = 8, height = 3.2, dpi = 300)

# =============================================================================
# Checks promised in the proposal
# =============================================================================
# 1. Rare events: events per parameter (EPV). >= 10 is the usual comfort level.
epv <- sum(d$failure) / (length(coef(fit1)) - 1)
say("\nRare-event check: %s failures / %d parameters = %.1f events per parameter.",
    format(sum(d$failure), big.mark = ","), length(coef(fit1)) - 1, epv)
small <- by_model[model %in% keep & failures < 5]
say("Models with fewer than 5 failures (wide CIs): %d of %d.", nrow(small), length(keep))

# 2. Multicollinearity: VIF of each numeric predictor given everything else
#    (including the model dummies). Drive model itself determines manufacturer
#    and capacity, which is why Model 1 uses model and Model 2 uses manufacturer
#    + capacity - never both.
X <- model.matrix(f1, data = d)[, -1]
vif <- sapply(c("age_years", "temp_5c", "realloc", "pending"), function(v) {
  r2 <- summary(lm(X[, v] ~ X[, colnames(X) != v]))$r.squared
  1 / (1 - r2)
})
say("Multicollinearity (VIF, Model 1): %s",
    paste(sprintf("%s = %.1f", names(vif), vif), collapse = ", "))
say("  (VIF > 5 means the effect is hard to separate from drive model - age usually is.)")

writeLines(log_lines, file.path(out_dir, "reg_summary.txt"))
cat("\nDone. Results written to", normalizePath(out_dir), "\n")
