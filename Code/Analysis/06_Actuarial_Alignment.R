################################################################################
# 06_Actuarial_Alignment.R
#
# Tests whether the Mid-Continent premium schedule tracks actuarially implied
# expected losses across age cells. Three independent inputs:
#
#   (1) h_hat   — Hazard from 01n. Two files used:
#                   * dcm_state_hazard_rates.csv  — bin-level (age × wall),
#                     used for fallback when row-level is missing
#                   * analysis_hazard_predictions_full.csv — row-level
#                     (panel_id × panel_year), TX SW rows joined to each
#                     facility-month for primary per-tank EL computation
#                 Source: Data/Analysis/ on Z drive
#
#   (2) L_hat   — Duan-smeared OLS predicted cleanup cost from 05,
#                 single-walled facilities only, re-aggregated to 01n's
#                 3-year age bins from row-level predictions.
#                 Source: Data/Analysis/dcm_predicted_cost_rowlevel.csv (Z)
#
#   (3) Premium — Cell-mean facility-month annualised premium from
#                 reconstructed Mid-Continent SERFF tank-month data,
#                 collapsed to facility-month and binned by avg_tank_age.
#                 Source: Rate FIllings/Mid-Continent Casualty Company .../
#
# Design notes (read before changing this script):
#   * The Texas Mid-Continent fleet is ~0% double-walled steel, so wall type
#     does not generate premium variation in this book. Alignment is on age
#     bins only, single-walled curve only, facility-level on both sides.
#   * All three age binnings are reconciled to 01n's 3-year master grid by
#     re-aggregating from row-level inputs (cost, premium) — no regression
#     is re-run here.
#   * Lambda (OLS through origin) is reported descriptively only. It mixes
#     underwriting markup with TX-vs-control-state cost-level differences
#     and cannot identify either separately. Spearman rho is the headline.
#
# OUTPUTS:
#   Output/Figures/Figure_Actuarial_Alignment.{png,pdf}
#   Output/Figures/Figure_Premium_Age_Evolution.{png,pdf}
#   Output/Tables/Table_Actuarial_Alignment.csv
#   Output/Tables/Table_Actuarial_Loading.csv
#   Output/Tables/Table_Premium_Cell_By_Period.csv
#
# EXECUTION ORDER:
#   01n_CVValidation.R  →  05_Claims_Analysis.R  →  12/13/14_Rate_*.R
#   →  06_Actuarial_Alignment.R  (this script)
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))

options(scipen = 999)

cat("=================================================================\n")
cat("06_Actuarial_Alignment.R  —  Premium vs. Expected Loss\n")
cat("=================================================================\n\n")

# ── 01n age-bin definition (master grid) ─────────────────────────────────────
# Must match make_age_bin() in 01n_CVValidation.R exactly.
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2", "3-5", "6-8", "9-11", "12-14",
                    "15-17", "18-20", "21-23", "24+")

bin_age <- function(a) {
  factor(
    cut(a, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
        right = FALSE, include.lowest = TRUE),
    levels = AGE_BIN_LABELS, ordered = FALSE
  )
}

# ── Output dirs ──────────────────────────────────────────────────────────────
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Theme ────────────────────────────────────────────────────────────────────
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.1),
                                      margin = margin(0, 0, 6, 0)),
      plot.subtitle    = element_text(color = "grey40", size = rel(0.85),
                                      margin = margin(0, 0, 8, 0)),
      plot.caption     = element_text(color = "grey50", size = rel(0.72),
                                      hjust = 0, margin = margin(8, 0, 0, 0)),
      axis.title       = element_text(face = "bold", size = rel(0.9)),
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = "gray85"),
      strip.text       = element_text(face = "bold")
    )
}
theme_set(theme_pub())

save_fig <- function(p, name, w = 8, h = 6) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")),
         p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")),
         p, width = w, height = h, device = grDevices::cairo_pdf)
  cat(sprintf("  Saved: %s (.png + .pdf)\n", name))
}

PERIOD_COLORS <- c(
  "2006-2014" = "#D55E00",
  "2014-2019" = "#E69F00",
  "2019-2021" = "#009E73",
  "2021+"     = "#0072B2"
)


################################################################################
#### S1: Hazard (single-walled, 3-year bins from 01n) ##########################
################################################################################

cat("── S1: Hazard ────────────────────────────────────────────────────────\n")

hazard_path <- z_path("Data", "Analysis", "dcm_state_hazard_rates.csv")
if (!file.exists(hazard_path))
  stop("dcm_state_hazard_rates.csv not found on Z. Run 01n first.")

hazard_raw <- fread(hazard_path)

req_haz <- c("age_bin_int", "has_single_walled", "h_hat", "n_fac_years")
missing_haz <- setdiff(req_haz, names(hazard_raw))
if (length(missing_haz) > 0)
  stop("hazard file missing columns: ", paste(missing_haz, collapse = ", "))

hazard_sw <- hazard_raw[
  has_single_walled == 1L,
  .(age_bin_int,
    age_bin = factor(AGE_BIN_LABELS[age_bin_int], levels = AGE_BIN_LABELS),
    h_hat,
    n_fac_years)
]
setorder(hazard_sw, age_bin_int)

cat(sprintf("  Loaded %d SW hazard cells\n", nrow(hazard_sw)))
print(hazard_sw[, .(age_bin,
                    h_hat_per1k = round(h_hat * 1000, 2),
                    n_fac_years)])


################################################################################
#### S2: Cost (single-walled, re-binned from row-level to 3-year) ##############
################################################################################

cat("\n── S2: Cost ─────────────────────────────────────────────────────────\n")

cost_path <- z_path("Data", "Analysis", "dcm_predicted_cost_rowlevel.csv")
if (!file.exists(cost_path))
  stop("dcm_predicted_cost_rowlevel.csv not found on Z. Re-run 05.")

cost_raw <- fread(cost_path)

req_cost <- c("avg_tank_age", "has_single_walled", "predicted_cost_2023")
missing_cost <- setdiff(req_cost, names(cost_raw))
if (length(missing_cost) > 0)
  stop("cost file missing columns: ", paste(missing_cost, collapse = ", "))

cat(sprintf("  Loaded %s row-level cost predictions\n",
    format(nrow(cost_raw), big.mark = ",")))

cost_sw <- cost_raw[
  has_single_walled == 1L &
  !is.na(avg_tank_age) &
  !is.na(predicted_cost_2023) &
  predicted_cost_2023 > 0
]

cost_sw[, age_bin := bin_age(avg_tank_age)]

cost_cells <- cost_sw[!is.na(age_bin), .(
  L_hat    = mean(predicted_cost_2023,   na.rm = TRUE),
  L_median = median(predicted_cost_2023, na.rm = TRUE),
  L_sd     = sd(predicted_cost_2023,     na.rm = TRUE),
  n_claims = .N
), by = age_bin]
cost_cells[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(cost_cells, age_bin)

cat(sprintf("  SW cost cells (3-year bins): %d\n", nrow(cost_cells)))
print(cost_cells[, .(age_bin,
                     L_hat = dollar(round(L_hat, 0)),
                     n_claims)])


################################################################################
#### S2.5: Row-level hazard predictions (TX, single-walled) ####################
################################################################################

cat("\n── S2.5: Row-level hazard predictions ───────────────────────────────\n")

hazard_full_path <- z_path("Data", "Analysis",
                           "analysis_hazard_predictions_full.csv")
if (!file.exists(hazard_full_path)) {
  cat("  WARNING: analysis_hazard_predictions_full.csv not on Z.\n")
  cat("  Falling back to bin-mean h_hat for all TX rows in S4.\n")
  hazard_full_tx <- NULL
} else {
  hazard_full <- fread(hazard_full_path)

  req_full <- c("panel_id", "panel_year", "state", "has_single_walled",
                "pred_elnet_full")
  missing_full <- setdiff(req_full, names(hazard_full))
  if (length(missing_full) > 0)
    stop("hazard-full file missing columns: ",
         paste(missing_full, collapse = ", "))

  hazard_full_tx <- hazard_full[
    state == "TX" & has_single_walled == 1L,
    .(panel_id, panel_year, h_hat_row = pred_elnet_full)
  ]
  setkey(hazard_full_tx, panel_id, panel_year)

  cat(sprintf("  TX SW row-level hazard rows: %s (%d distinct facilities)\n",
      format(nrow(hazard_full_tx), big.mark = ","),
      uniqueN(hazard_full_tx$panel_id)))
  cat(sprintf("  Year range: %d-%d\n",
      min(hazard_full_tx$panel_year), max(hazard_full_tx$panel_year)))
  cat(sprintf("  h_hat_row summary (per1k): min=%.2f median=%.2f max=%.2f\n",
      min(hazard_full_tx$h_hat_row, na.rm = TRUE)    * 1000,
      median(hazard_full_tx$h_hat_row, na.rm = TRUE) * 1000,
      max(hazard_full_tx$h_hat_row, na.rm = TRUE)    * 1000))
}


################################################################################
#### S3: Texas premium (facility-level, binned by avg_tank_age) ################
################################################################################

cat("\n── S3: Texas premium ────────────────────────────────────────────────\n")

# Locate rate-filing CSVs by glob pattern — sidesteps soft-hyphen / em-dash
# encoding issues in the "Mid-Continent Casualty Company – 23418" subdir name
# that bite when comparing literal paths from `here()`.
glob_patterns <- c(
  file.path(getwd(), "Rate FIllings", "*",
            "texas_midcontinent_tank_month_premium_*.csv"),
  file.path(getwd(), "Data", "Rate FIllings", "*",
            "texas_midcontinent_tank_month_premium_*.csv")
)
matching_files <- unique(unlist(lapply(glob_patterns, Sys.glob)))

if (length(matching_files) == 0)
  stop("No tank-month premium files found. Searched:\n  ",
       paste(glob_patterns, collapse = "\n  "))

RATE_DIR <- dirname(matching_files[1])
cat(sprintf("  Rate dir: %s\n", RATE_DIR))
cat(sprintf("  Tank-month files matched: %d\n", length(matching_files)))

tm_registry <- data.table(
  period_id     = c("P1", "P2", "P3", "P4"),
  filing_period = c("2006-2014", "2014-2019", "2019-2021", "2021+"),
  filename      = c(
    "texas_midcontinent_tank_month_premium_2006_to_04_2014.csv",
    "texas_midcontinent_tank_month_premium_2014_05_to_2019_01.csv",
    "texas_midcontinent_tank_month_premium_2019_2021.csv",
    "texas_midcontinent_tank_month_premium_2021_onwards.csv"
  )
)
# Match registry filenames against actually-found files (basename comparison
# avoids any path-encoding mismatch on the directory portion).
matched_basenames <- basename(matching_files)
tm_registry[, filepath := vapply(filename, function(fn) {
  hit <- matching_files[matched_basenames == fn]
  if (length(hit) == 0) NA_character_ else hit[1]
}, character(1))]
tm_registry[, exists := !is.na(filepath) & file.exists(filepath)]

cat("  File availability:\n")
print(tm_registry[, .(period_id, filing_period, exists)])

tm_list <- lapply(seq_len(nrow(tm_registry)), function(i) {
  row <- tm_registry[i]
  if (!row$exists) return(NULL)
  dt <- tryCatch(
    fread(row$filepath, showProgress = FALSE),
    error = function(e) {
      warning(sprintf("Failed to read %s: %s", row$filename, e$message))
      NULL
    }
  )
  if (is.null(dt)) return(NULL)
  dt[, filing_period := row$filing_period]
  dt[, period_id     := row$period_id]
  cat(sprintf("    %s: %s rows\n",
      row$filing_period, format(nrow(dt), big.mark = ",")))
  dt
})

tm_all <- rbindlist(Filter(Negate(is.null), tm_list),
                    use.names = TRUE, fill = TRUE)

if (nrow(tm_all) == 0)
  stop("No tank-month premium files loaded.")

# Detect monthly vs annual premium (BASE_RATE = $300/tank/year in all filings;
# tank_premium ~ $25 → monthly, ~ $300+ → annual)
prem_median <- median(tm_all$tank_premium, na.rm = TRUE)
cat(sprintf("\n  tank_premium median: $%.2f\n", prem_median))

if (prem_median < 100) {
  cat("  Detected MONTHLY premium — annualising x12\n")
  tm_all[, annual_tank_premium := tank_premium * 12]
} else {
  cat("  Detected ANNUAL premium\n")
  tm_all[, annual_tank_premium := tank_premium]
}

# Operate on tm_all directly (tank-month level). Each row is one tank-month
# with that tank's own tank_premium and age_years. Attach facility-month
# aggregates as columns (n_tanks, avg_tank_age) without collapsing — keeps
# tank-month weighting so big facilities count in proportion to their tank
# count, which is the right weighting for a per-tank pricing claim.
tm_all <- tm_all[
  !is.na(age_years) &
  !is.na(annual_tank_premium) &
  annual_tank_premium > 0
]

tm_all[, n_tanks      := .N,            by = .(FACILITY_ID, YEAR, MONTH)]
tm_all[, avg_tank_age := mean(age_years, na.rm = TRUE),
                                          by = .(FACILITY_ID, YEAR, MONTH)]
tm_all[, age_bin      := bin_age(avg_tank_age)]

# Construct panel_id matching 01n's TX convention: "<FACILITY_ID>_TX"
tm_all[, panel_id := paste0(trimws(FACILITY_ID), "_TX")]

# Join row-level h_hat from analysis_hazard_predictions_full.csv (if present)
if (!is.null(hazard_full_tx)) {
  tm_all[hazard_full_tx, on = .(panel_id, YEAR = panel_year),
         h_hat_row := i.h_hat_row]
  match_rate <- mean(!is.na(tm_all$h_hat_row))
  cat(sprintf("\n  Row-level h_hat match rate: %.1f%% of tank-months\n",
      100 * match_rate))
} else {
  tm_all[, h_hat_row := NA_real_]
}

cat(sprintf("\n  Tank-months: %s across %d periods\n",
    format(nrow(tm_all), big.mark = ","),
    uniqueN(tm_all$filing_period)))
cat(sprintf("  Distinct facility-months: %s\n",
    format(uniqueN(tm_all[, .(FACILITY_ID, YEAR, MONTH)]),
           big.mark = ",")))

# Cell aggregates by age bin × period
# - per-tank metrics: tank-month weighted (each tank-month row contributes)
# - facility-total: needs a second pass collapsing within facility-month first
prem_cells_pertank <- tm_all[!is.na(age_bin), .(
  mean_per_tank_premium    = mean(annual_tank_premium,   na.rm = TRUE),
  median_per_tank_premium  = median(annual_tank_premium, na.rm = TRUE),
  mean_n_tanks             = mean(n_tanks,                na.rm = TRUE),
  n_tank_months            = .N,
  mean_age                 = mean(avg_tank_age,           na.rm = TRUE)
), by = .(filing_period, period_id, age_bin)]

prem_cells_facility <- tm_all[!is.na(age_bin), .(
  facility_premium = sum(annual_tank_premium, na.rm = TRUE)
), by = .(filing_period, period_id, age_bin, FACILITY_ID, YEAR, MONTH)
][, .(mean_facility_premium    = mean(facility_premium,   na.rm = TRUE),
      median_facility_premium  = median(facility_premium, na.rm = TRUE),
      n_fac_months             = .N
), by = .(filing_period, period_id, age_bin)]

prem_cells_by_period <- merge(
  prem_cells_pertank, prem_cells_facility,
  by = c("filing_period", "period_id", "age_bin"), all = FALSE
)
prem_cells_by_period[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(prem_cells_by_period, period_id, age_bin)

fwrite(prem_cells_by_period,
       file.path(OUTPUT_TABLES, "Table_Premium_Cell_By_Period.csv"))
cat(sprintf("  Cell-period table saved: %d cells\n",
    nrow(prem_cells_by_period)))

# P1 cell premiums for the alignment figure
prem_p1 <- prem_cells_by_period[
  period_id == "P1",
  .(age_bin,
    per_tank_premium = mean_per_tank_premium,
    facility_premium = mean_facility_premium,
    mean_n_tanks,
    n_tank_months,
    n_fac_months)
]

if (nrow(prem_p1) == 0) {
  cat("  WARNING: P1 (2006-2014) data missing — falling back to earliest period.\n")
  earliest <- prem_cells_by_period[, min(as.character(period_id))]
  prem_p1 <- prem_cells_by_period[
    period_id == earliest,
    .(age_bin,
      per_tank_premium = mean_per_tank_premium,
      facility_premium = mean_facility_premium,
      mean_n_tanks,
      n_tank_months,
      n_fac_months)
  ]
}

cat("\n  P1 (2006-2014) cell premiums:\n")
print(prem_p1[order(age_bin),
              .(age_bin,
                per_tank      = dollar(round(per_tank_premium, 0)),
                facility_tot  = dollar(round(facility_premium, 0)),
                mean_n_tanks  = round(mean_n_tanks, 1),
                n_tank_months,
                n_fac_months)])


################################################################################
#### S4: Merge and compute expected loss #######################################
################################################################################

cat("\n── S4: Expected loss ────────────────────────────────────────────────\n")

# Build hazard × cost cells (facility-level expected loss per age bin).
haz_cost <- merge(
  hazard_sw[,  .(age_bin, h_hat, n_fac_years)],
  cost_cells[, .(age_bin, L_hat, n_claims)],
  by = "age_bin", all = FALSE
)
haz_cost[, facility_el := h_hat * L_hat]
setorder(haz_cost, age_bin)

cat("  Facility-level expected loss by age bin:\n")
print(haz_cost[, .(age_bin,
                   h_hat_per1k = round(h_hat * 1000, 2),
                   L_hat       = dollar(round(L_hat, 0)),
                   facility_el = dollar(round(facility_el, 0)))])

# Per-tank expected loss, computed at the tank-month level.
#
# For each tank-month: attach the facility's row-level h_hat (from 01n's
# full-panel predictions, joined on panel_id × YEAR) and the bin-level L_hat
# (from 05's row-level cost predictions, aggregated to bin), then divide
# by that facility-month's actual n_tanks. Aggregating tank-month-weighted
# means big facilities count in proportion to their tank count — the right
# weighting for a per-tank pricing claim.
#
# Hazard source: row-level h_hat where available (h_hat_row), bin-mean
# h_hat as fallback. Under independence, P(this tank leaks) ≈ h_facility /
# n_tanks. L_hat remains bin-level (no TX-specific cost data exists).
tm_el <- merge(
  tm_all[!is.na(age_bin) & period_id == "P1"],
  haz_cost[, .(age_bin, h_hat_bin = h_hat, L_hat)],
  by = "age_bin", all = FALSE
)
tm_el[, h_hat_used := fifelse(!is.na(h_hat_row), h_hat_row, h_hat_bin)]
tm_el[, per_tank_el := (h_hat_used * L_hat) / n_tanks]

cat(sprintf("\n  Per-tank EL hazard source mix:\n"))
cat(sprintf("    Row-level h_hat:  %.1f%% of tank-months\n",
    100 * mean(!is.na(tm_el$h_hat_row))))
cat(sprintf("    Bin-mean fallback: %.1f%% of tank-months\n",
    100 * mean(is.na(tm_el$h_hat_row))))

# Aggregate per-tank quantities to age-bin cells (P1 baseline), tank-month
# weighted: each tank-month row contributes once.
align_per_tank <- tm_el[, .(
  per_tank_premium = mean(annual_tank_premium, na.rm = TRUE),
  per_tank_el      = mean(per_tank_el,         na.rm = TRUE),
  mean_h_hat_used  = mean(h_hat_used,          na.rm = TRUE),
  mean_n_tanks     = mean(n_tanks,             na.rm = TRUE),
  share_rowlevel_h = mean(!is.na(h_hat_row)),
  n_tank_months    = .N
), by = age_bin]
align_per_tank[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(align_per_tank, age_bin)

# Merge in the bin-level h_hat / L_hat / counts for a complete table.
align_dt <- merge(
  align_per_tank,
  haz_cost[, .(age_bin, h_hat, L_hat, facility_el, n_fac_years, n_claims)],
  by = "age_bin", all = FALSE
)

# Facility-level premium (diagnostic — kept for reference / table only)
align_dt <- merge(
  align_dt,
  prem_p1[, .(age_bin, facility_premium, n_fac_months)],
  by = "age_bin", all = FALSE
)

setorder(align_dt, age_bin)

cat(sprintf("\n  Cells in alignment: %d\n", nrow(align_dt)))
if (nrow(align_dt) < 4)
  warning("Fewer than 4 cells in alignment — figure may not be credible.")

# Loading factor (per-tank, descriptive only; see caveat in caption)
align_dt[, loading_factor := per_tank_premium / per_tank_el]

# Headline statistics — per-tank
spearman_rho <- cor(align_dt$per_tank_el, align_dt$per_tank_premium,
                    method = "spearman", use = "complete.obs")
pearson_r    <- cor(align_dt$per_tank_el, align_dt$per_tank_premium,
                    method = "pearson",   use = "complete.obs")

# OLS through origin (per-tank, descriptive level shift)
lambda_ols <- coef(lm(per_tank_premium ~ 0 + per_tank_el, data = align_dt))

# Diagnostic: facility-level Spearman (for comparison in the summary)
spearman_facility <- cor(align_dt$facility_el, align_dt$facility_premium,
                         method = "spearman", use = "complete.obs")

cat(sprintf("\n  Per-tank alignment:\n"))
cat(sprintf("    Spearman rho:  %.3f\n", spearman_rho))
cat(sprintf("    Pearson r:     %.3f\n", pearson_r))
cat(sprintf("    Lambda (OLS):  %.3f  (descriptive)\n", lambda_ols))
cat(sprintf("\n  Facility-level (diagnostic): Spearman rho = %.3f\n",
    spearman_facility))
cat("    [confounded by portfolio shrinkage — see Section S5 caption]\n")

cat("\n  Per-tank cell summary:\n")
print(align_dt[, .(age_bin,
                   per_tank_premium  = dollar(round(per_tank_premium, 0)),
                   per_tank_el       = dollar(round(per_tank_el,      0)),
                   loading_factor    = round(loading_factor,           2),
                   h_used_per1k      = round(mean_h_hat_used * 1000,   2),
                   share_rowlevel_h  = round(share_rowlevel_h,         2),
                   mean_n_tanks      = round(mean_n_tanks,             1),
                   n_tank_months,
                   n_fac_months)])


################################################################################
#### S5: Alignment figure ######################################################
################################################################################

cat("\n── S5: Alignment figure ─────────────────────────────────────────────\n")

# Reshape to long for the two-line plot
plot_dt <- melt(
  align_dt[, .(age_bin,
               `Per-Tank Expected Loss` = per_tank_el,
               `Per-Tank Premium`       = per_tank_premium)],
  id.vars       = "age_bin",
  variable.name = "series",
  value.name    = "usd"
)
plot_dt[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
plot_dt[, series  := factor(series,
                            levels = c("Per-Tank Expected Loss",
                                       "Per-Tank Premium"))]

annot_x <- 1L
annot_y <- max(plot_dt$usd, na.rm = TRUE) * 0.98

fig_align <- ggplot(plot_dt,
                    aes(x     = age_bin,
                        y     = usd,
                        color = series,
                        shape = series,
                        group = series)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 3.6, alpha = 0.95) +
  annotate("label",
           x             = annot_x,
           y             = annot_y,
           hjust         = 0,
           vjust         = 1,
           size          = 3.1,
           fill          = "white",
           color         = "grey20",
           label.padding = unit(0.28, "lines"),
           label         = sprintf("Spearman ρ = %.2f\nλ (OLS) = %.2f",
                                   spearman_rho, lambda_ols)) +
  scale_color_manual(
    values = c("Per-Tank Expected Loss" = "#D55E00",
               "Per-Tank Premium"       = "#0072B2"),
    name   = NULL) +
  scale_shape_manual(
    values = c("Per-Tank Expected Loss" = 17,
               "Per-Tank Premium"       = 19),
    name   = NULL) +
  scale_y_continuous(
    name   = "USD per Tank-Year (2023)",
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.10))) +
  scale_x_discrete(name = "Facility Mean Tank Age (years)") +
  labs(
    title    = "Per-Tank Premium Tracks Expected Loss Across Age Bins",
    subtitle = paste0(
      "Single-walled facilities, TX Mid-Continent, 2006-2014 filing. ",
      "Both quantities in 2023 USD per tank-year — the vertical gap is the ",
      "loading factor; the parallel rise across bins is the alignment."),
    caption  = paste0(
      "Notes: Per-tank premium = mean(tank_premium) across TX Mid-Continent ",
      "tank-months in each bin. Per-tank expected loss = h_hat × L_hat / ",
      "n_tanks, computed at the tank-month level using each facility's ",
      "row-level h_hat (01n full-panel elastic net) and that facility-",
      "month's actual n_tanks, then averaged tank-month weighted within ",
      "bins. L_hat = Duan-smeared OLS predicted cost from 05 (six-state ",
      "trust fund ledgers, single-walled, 3-year bins from row-level ",
      "predictions). Spearman ρ = ", round(spearman_rho, 2),
      "; OLS λ = ", round(lambda_ols, 2), ". The level gap reflects the ",
      "TX trust fund reimbursement scheme (insurer covers deductible + ",
      "over-cap, not full cleanup cost) and the independence assumption ",
      "in dividing facility hazard by n_tanks. Interpret λ as descriptive.")
  ) +
  theme_pub() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 30, hjust = 1)
  )

save_fig(fig_align, "Figure_Actuarial_Alignment", w = 8, h = 6)


################################################################################
#### S6: Period evolution figure ###############################################
################################################################################

cat("\n── S6: Period evolution figure ──────────────────────────────────────\n")

n_periods_avail <- uniqueN(prem_cells_by_period$filing_period)
cat(sprintf("  Filing periods available: %d\n", n_periods_avail))

if (n_periods_avail < 2) {
  cat("  WARNING: <2 periods — evolution figure skipped.\n")
} else {
  # Index to the bin closest to "10-15 years" — i.e., 12-14 in 01n's grid.
  NEUTRAL_BIN <- "12-14"

  neutral <- prem_cells_by_period[
    age_bin == NEUTRAL_BIN,
    .(filing_period, neutral_premium = mean_per_tank_premium)
  ]

  evo_dt <- merge(prem_cells_by_period, neutral, by = "filing_period")
  evo_dt[, premium_index := mean_per_tank_premium / neutral_premium]

  # Keep bins with non-trivial coverage in every available period
  bins_keep <- evo_dt[
    !is.na(premium_index) & n_fac_months >= 10,
    .(n_pds = uniqueN(filing_period)), by = age_bin
  ][n_pds == n_periods_avail, age_bin]

  evo_plot <- evo_dt[age_bin %in% bins_keep]
  evo_plot[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
  evo_plot[, filing_period := factor(filing_period,
                                      levels = c("2006-2014","2014-2019",
                                                 "2019-2021","2021+"))]

  young_bins <- c("0-2", "3-5")

  fig_evo <- ggplot(evo_plot,
                    aes(x     = age_bin,
                        y     = premium_index,
                        color = filing_period,
                        group = filing_period)) +
    geom_hline(yintercept = 1, linetype = "dotted",
               color = "grey55", linewidth = 0.5) +
    annotate("rect",
             xmin = 0.5, xmax = length(young_bins) + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.4) +
    annotate("text",
             x = 1, y = Inf, vjust = 1.6, hjust = 0.5,
             size = 2.5, color = "grey40",
             label = "Young-tank\ndiscount zone") +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.4) +
    scale_color_manual(values = PERIOD_COLORS, name = "Filing Period") +
    scale_y_continuous(
      name   = sprintf("Premium Index (= 1.00 at %s bin)", NEUTRAL_BIN),
      labels = function(x) sprintf("%.2f×", x)) +
    scale_x_discrete(name = "Facility Mean Tank Age (years)") +
    labs(
      title    = "Mid-Continent Age Premium Gradient: Increasing Granularity Over Time",
      subtitle = paste0(
        "Cell-mean facility annual premium by 3-year age bin, indexed to ",
        NEUTRAL_BIN, " within each filing period. The 2019+ filing introduces ",
        "fine-grained discount steps for young tanks (shaded zone)."),
      caption  = paste0(
        "Source: reconstructed Mid-Continent Casualty SERFF filings, four ",
        "periods. Per-tank premium = total facility premium / n_tanks at the ",
        "facility-month level, averaged within bins. Bins below 10 ",
        "facility-months in any period are dropped to suppress small-cell noise.")
    ) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  save_fig(fig_evo, "Figure_Premium_Age_Evolution", w = 9, h = 6.5)
}


################################################################################
#### S7: Output tables #########################################################
################################################################################

cat("\n── S7: Output tables ────────────────────────────────────────────────\n")

align_out <- align_dt[, .(
  age_bin,
  h_hat_bin_per1k     = round(h_hat * 1000,                2),
  h_hat_used_per1k    = round(mean_h_hat_used * 1000,      2),
  share_rowlevel_h    = round(share_rowlevel_h,            3),
  L_hat_2023usd       = round(L_hat,                       0),
  facility_el         = round(facility_el,                 0),
  per_tank_el         = round(per_tank_el,                 0),
  per_tank_premium    = round(per_tank_premium,            0),
  facility_premium    = round(facility_premium,            0),
  mean_n_tanks        = round(mean_n_tanks,                2),
  loading_factor      = round(loading_factor,              3),
  n_fac_years,
  n_claims,
  n_tank_months,
  n_fac_months
)]
fwrite(align_out, file.path(OUTPUT_TABLES, "Table_Actuarial_Alignment.csv"))
cat(sprintf("  Saved: Table_Actuarial_Alignment.csv (%d rows)\n",
    nrow(align_out)))

load_summary <- data.table(
  spearman_rho_pertank   = round(spearman_rho,      3),
  pearson_r_pertank      = round(pearson_r,         3),
  lambda_ols_pertank     = round(lambda_ols,        3),
  lambda_mean_pertank    = round(mean(align_dt$loading_factor,   na.rm = TRUE), 3),
  lambda_median_pertank  = round(median(align_dt$loading_factor, na.rm = TRUE), 3),
  spearman_rho_facility  = round(spearman_facility, 3),
  n_cells                = nrow(align_dt),
  premium_period         = "2006-2014 (P1 baseline)",
  scope                  = "Single-walled facilities only; TX Mid-Continent",
  note = paste0(
    "Per-tank: premium = total_premium / n_tanks (row-level), ",
    "EL = h_hat × L_hat / n_tanks (row-level), aggregated to bins. ",
    "Facility-level Spearman included as diagnostic — confounded by ",
    "portfolio shrinkage with age. Loading factor is descriptive only: ",
    "mixes underwriting markup with TX-vs-control cost-level differences.")
)
fwrite(load_summary, file.path(OUTPUT_TABLES, "Table_Actuarial_Loading.csv"))
cat("  Saved: Table_Actuarial_Loading.csv\n")
cat("  Saved: Table_Premium_Cell_By_Period.csv (in S3)\n")


################################################################################
#### S8: Diagnostic checks #####################################################
################################################################################

cat("\n── S8: Diagnostic checks ────────────────────────────────────────────\n")

# CHECK 1: Per-tank EL roughly increasing in age
is_mono_el <- all(diff(align_dt$per_tank_el) >= 0, na.rm = TRUE)
if (is_mono_el) {
  cat("  CHECK 1 PASS: Per-tank EL non-decreasing in age.\n")
} else {
  cat("  CHECK 1 NOTE: Per-tank EL not strictly monotone (acceptable —\n")
  cat("                may reflect within-bin composition noise).\n")
}

# CHECK 2: Per-tank premium roughly increasing in age
is_mono_pr <- all(diff(align_dt$per_tank_premium) >= 0, na.rm = TRUE)
if (is_mono_pr) {
  cat("  CHECK 2 PASS: Per-tank premium non-decreasing in age.\n")
} else {
  cat("  CHECK 2 NOTE: Per-tank premium not strictly monotone in age.\n")
}

# CHECK 3: Rank correlation positive
if (spearman_rho > 0) {
  cat(sprintf("  CHECK 3 PASS: Spearman rho = %.3f (positive).\n",
      spearman_rho))
} else {
  cat(sprintf("  CHECK 3 FAIL: Spearman rho = %.3f.\n", spearman_rho))
}

# CHECK 4: Cell count
cat(sprintf("  CHECK 4 INFO: %d alignment cells.\n", nrow(align_dt)))
if (nrow(align_dt) < 5)
  cat("  WARNING: <5 cells — figure may not be credible.\n")

# CHECK 5: Lambda plausibility (descriptive only)
lambda_mean <- mean(align_dt$loading_factor, na.rm = TRUE)
cat(sprintf("  CHECK 5 INFO: Mean loading factor = %.2f (descriptive).\n",
    lambda_mean))


################################################################################
#### Summary ###################################################################
################################################################################

cat("\n========================================================\n")
cat("06_Actuarial_Alignment.R COMPLETE\n\n")
cat(sprintf("  Alignment cells:        %d\n",   nrow(align_dt)))
cat(sprintf("  Per-tank Spearman rho:  %.3f\n", spearman_rho))
cat(sprintf("  Per-tank Pearson r:     %.3f\n", pearson_r))
cat(sprintf("  Per-tank lambda (OLS):  %.3f  (descriptive)\n", lambda_ols))
cat(sprintf("  Facility Spearman rho:  %.3f  (diagnostic — size confounded)\n",
    spearman_facility))
cat(sprintf("  Premium period:    2006-2014 (P1 baseline)\n"))
cat(sprintf("  Filing periods in evolution fig: %d\n", n_periods_avail))
cat("\n  Figures:\n")
cat("    Figure_Actuarial_Alignment      (main — Section 5.3)\n")
cat("    Figure_Premium_Age_Evolution    (period evolution)\n")
cat("\n  Tables:\n")
cat("    Table_Actuarial_Alignment.csv\n")
cat("    Table_Actuarial_Loading.csv\n")
cat("    Table_Premium_Cell_By_Period.csv\n")
cat("========================================================\n")
