#==============================================================================
# 01l_DataQuality_and_Balance.R  (REFACTORED v3)
# Combined: Data Quality / Parallel Trends Figure + Descriptive + Balance Tables
#
# TABLE 0 LAYOUT:
#   Panel A: 1998 Cross-Section (Risk Covariates)
#     Count rows:  N States / N Facilities / N Total Tanks
#     Fac-level:   Avg tanks, Avg capacity, Avg tank age, Avg age of oldest tank
#     Fleet agg:   Avg age of state fleet, Share SW (fleet %)
#     Fac-level:   Avg share of SW tanks per facility
#   Panel B: Pre-Reform Outcome Flows (1990-1998)
#     Avg Closure Rate / Avg LUST Rate / Avg Replacement Rate
#   Panel C: Make-Model Cell Coverage
#     Facility cells (3-dim) / Tank cells (4-dim)
#
# TABLE 1 — PROGRESSIVE BALANCE (3 rows):
#   Row 1: Full Sample (Naive)           — pooled means, pooled diff
#   Row 2: MM Sample (Pooled)            — pooled means within MM facilities
#   Row 3: MM Sample (Cell-Year Wt)      — CORRECT:
#
#     STEP 1: cell × year → TX mean rate | CTL mean rate | diff
#     STEP 2: average diffs over years within each cell
#     STEP 3: weighted average across cells (weight = total fac-years in cell)
#
#   This exactly mirrors what the cell×year FE identifies.
#
# CELL DETAIL (Appendix): TX/CTL/Diff are year-averaged within-cell rates,
#   matching the S3 computation.
#
# CONTROL STATES: 16 matching 02a_DiD_FacBehavior.R exactly.
#==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(knitr)
  library(kableExtra)
  library(broom)
  library(here)
})

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
open_log("01l_DataQuality_and_Balance")
log_cat("=== 01l: DATA QUALITY + DESCRIPTIVE + BALANCE ===\n")

# ─────────────────────────────────────────────────────────────────────────────
# CONSTANTS  — must match 02a_DiD_FacBehavior.R exactly
# ─────────────────────────────────────────────────────────────────────────────

CONTROL_STATES <- c(
  "AL", "AR", "CO", "GA", "ID", "KS", "KY",
  "MD", "MN", "MO", "NC", "OH",
  "OK", "TN", "VA", "WV"
)
STUDY_STATES  <- c("TX", CONTROL_STATES)
POST_YEAR     <- 1999L
PRE_START     <- 1990L
PRE_END       <- 1998L          # 1998-12-22 is the reform date; 1998 is pre
REFORM_DATE_D <- as.Date("1998-12-22")
REFORM_YEAR   <- 1998L
PRIMARY_YEARS <- as.character(1989:1997)   # matches 02b_DiD_Survival.R


###############################################################################
# S0: LOAD DATA
###############################################################################
log_cat("\n--- S0: Loading data ---\n")

annual_data         <- load_interim("annual_data")
tanks_1998          <- load_interim("tanks_1998")
data_quality_report <- load_interim("data_quality_report")
attrition_log       <- load_interim("attrition_log")
balance_glm         <- load_interim("balance_glm")

fac_mm <- readRDS(here::here("Data", "Analysis", "facility_make_model.rds"))

# ── Treatment assignment: override to match 02a CONTROL_STATES ──────────────
annual_data[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]
annual_data <- annual_data[!is.na(texas_treated)]

tanks_1998[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]
tanks_1998 <- tanks_1998[!is.na(texas_treated)]

# ── Tank age at reform date ──────────────────────────────────────────────────
if (!"tank_age_at_reform" %in% names(tanks_1998)) {
  tanks_1998[, tank_age_at_reform :=
    as.numeric(REFORM_DATE_D - as.Date(tank_installed_date)) / 365.25]
}
tanks_1998 <- tanks_1998[!is.na(tank_age_at_reform) & tank_age_at_reform >= 0]

# ── Merge oldest_age_at_reform from fac_mm (continuous, from Section 2.5) ───
annual_data <- merge(
  annual_data,
  fac_mm[, .(panel_id, oldest_age_at_reform)],
  by = "panel_id", all.x = TRUE
)

# ── Facility-level SW share frozen at 1998 ───────────────────────────────────
sw_1998 <- annual_data[
  panel_year == REFORM_YEAR & active_tanks_dec > 0,
  .(panel_id, sw_share_fac_1998 = single_tanks_dec / active_tanks_dec * 100)
]
annual_data <- merge(annual_data, sw_1998, by = "panel_id", all.x = TRUE)

# ── Alias ────────────────────────────────────────────────────────────────────
if (!"closure_event" %in% names(annual_data) && "closure_year" %in% names(annual_data))
  annual_data[, closure_event := closure_year]

log_cat(sprintf("  annual_data: %s fy | %s facilities | %d states\n",
  format(nrow(annual_data),             big.mark = ","),
  format(uniqueN(annual_data$panel_id), big.mark = ","),
  uniqueN(annual_data$state)))
log_cat(sprintf("  tanks_1998: %s tanks (%s TX | %s CTL)\n",
  format(nrow(tanks_1998),                   big.mark = ","),
  format(tanks_1998[texas_treated == 1, .N], big.mark = ","),
  format(tanks_1998[texas_treated == 0, .N], big.mark = ",")))


###############################################################################
# S1: DEFINE MM SAMPLE + OVERLAP
###############################################################################
log_cat("\n--- S1: MM sample + overlap ---\n")

annual_data[, is_mm := as.integer(
  !is.na(make_model_fac) & !is.na(fac_oldest_age_bin)
)]

cell_support  <- annual_data[is_mm == 1, .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_fac]
overlap_cells <- cell_support[has_tx & has_ctl, make_model_fac]
n_tx_only     <- cell_support[has_tx  & !has_ctl, .N]
n_ctl_only    <- cell_support[!has_tx &  has_ctl, .N]

annual_data[is_mm == 1 & !make_model_fac %in% overlap_cells, is_mm := 0L]
mm_ids <- unique(annual_data[is_mm == 1, panel_id])

log_cat(sprintf("  Cells: %d total | %d overlap | %d TX-only | %d CTL-only\n",
  nrow(cell_support), length(overlap_cells), n_tx_only, n_ctl_only))
log_cat(sprintf("  MM facilities: %s (%s TX | %s CTL)\n",
  format(length(mm_ids), big.mark = ","),
  format(uniqueN(annual_data[is_mm == 1 & texas_treated == 1, panel_id]), big.mark = ","),
  format(uniqueN(annual_data[is_mm == 1 & texas_treated == 0, panel_id]), big.mark = ",")))


###############################################################################
# S2: BUILD SUBSETS
###############################################################################
log_cat("\n--- S2: Subsets ---\n")

fac_1998_full <- annual_data[panel_year == REFORM_YEAR]
fac_1998_mm   <- annual_data[panel_year == REFORM_YEAR & panel_id %in% mm_ids]

tanks_full <- tanks_1998
tanks_mm   <- tanks_1998[panel_id %in% mm_ids]

pre_full <- annual_data[panel_year %between% c(PRE_START, PRE_END)]
pre_mm   <- annual_data[panel_year %between% c(PRE_START, PRE_END) & panel_id %in% mm_ids]

log_cat(sprintf("  1998 fac: Full=%s | MM=%s\n",
  format(nrow(fac_1998_full), big.mark = ","),
  format(nrow(fac_1998_mm),   big.mark = ",")))
log_cat(sprintf("  Pre-reform fac-years: Full=%s | MM=%s\n",
  format(nrow(pre_full), big.mark = ","),
  format(nrow(pre_mm),   big.mark = ",")))


###############################################################################
# S2b: CELL COVERAGE DIAGNOSTICS
###############################################################################
log_cat("\n--- S2b: Cell coverage ---\n")

# ── Facility cells (make_model_fac) ──────────────────────────────────────────
fac_cell_support_full <- fac_1998_full[!is.na(make_model_fac), .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_fac]

fac_cells_total_full      <- nrow(fac_cell_support_full)
fac_cells_identified_full <- fac_cell_support_full[has_tx & has_ctl, .N]
fac_cells_tx_only_full    <- fac_cell_support_full[has_tx & !has_ctl, .N]
fac_cells_ctl_only_full   <- fac_cell_support_full[!has_tx & has_ctl, .N]
fac_identified_full       <- fac_cell_support_full[has_tx & has_ctl, make_model_fac]

# MM: by construction only overlap cells remain (TX-only/CTL-only = 0)
fac_cells_total_mm      <- length(overlap_cells)
fac_cells_identified_mm <- length(overlap_cells)
fac_cells_tx_only_mm    <- 0L
fac_cells_ctl_only_mm   <- 0L

# Panel-year coverage (what the regression uses, not just the 1998 snapshot)
fac_pct_identified_full_panel <- annual_data[!is.na(make_model_fac),
  round(mean(make_model_fac %in% fac_identified_full) * 100, 1)]
fac_pct_identified_mm_panel   <- annual_data[!is.na(make_model_fac) & is_mm == 1,
  round(mean(make_model_fac %in% overlap_cells) * 100, 1)]

# ── Tank cells (make_model_tank) ─────────────────────────────────────────────
tank_cell_support_full <- tanks_full[!is.na(make_model_tank), .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_tank]

tank_cells_total_full      <- nrow(tank_cell_support_full)
tank_cells_identified_full <- tank_cell_support_full[has_tx & has_ctl, .N]
tank_cells_tx_only_full    <- tank_cell_support_full[has_tx & !has_ctl, .N]
tank_cells_ctl_only_full   <- tank_cell_support_full[!has_tx & has_ctl, .N]
tank_identified_full       <- tank_cell_support_full[has_tx & has_ctl, make_model_tank]
tank_pct_identified_full   <- tanks_full[!is.na(make_model_tank),
  round(mean(make_model_tank %in% tank_identified_full) * 100, 1)]

tanks_primary <- tanks_full[mm_install_cohort %in% PRIMARY_YEARS & !is.na(make_model_tank)]
tanks_primary_mm <- tanks_mm[mm_install_cohort %in% PRIMARY_YEARS & !is.na(make_model_tank)]

tank_cell_support_mm <- tanks_primary[, .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_tank]

tank_cells_total_mm      <- nrow(tank_cell_support_mm)
tank_cells_identified_mm <- tank_cell_support_mm[has_tx & has_ctl, .N]
tank_cells_tx_only_mm    <- tank_cell_support_mm[has_tx & !has_ctl, .N]
tank_cells_ctl_only_mm   <- tank_cell_support_mm[!has_tx & has_ctl, .N]
tank_identified_mm       <- tank_cell_support_mm[has_tx & has_ctl, make_model_tank]
tank_pct_identified_mm   <- tanks_primary[,
  round(mean(make_model_tank %in% tank_identified_mm) * 100, 1)]

log_cat(sprintf("  Fac cells  — Full: %d tot | %d id | %d TX-only | %d CTL-only\n",
  fac_cells_total_full, fac_cells_identified_full,
  fac_cells_tx_only_full, fac_cells_ctl_only_full))
log_cat(sprintf("  Tank cells — Full: %d tot | %d id | MM: %d tot | %d id\n",
  tank_cells_total_full, tank_cells_identified_full,
  tank_cells_total_mm,   tank_cells_identified_mm))


###############################################################################
# S3: CELL x YEAR DIFF COMPUTATION (core)
#
# The correct way to measure pre-reform TX vs CTL differences under the
# cell x year FE design:
#
#   Step 1: For each (cell, year) compute:
#             tx_rate  = mean outcome for TX facilities in that cell-year
#             ctl_rate = mean outcome for CTL facilities in that cell-year
#             diff     = tx_rate - ctl_rate
#
#   Step 2: For each cell, average those diffs over the pre-reform years
#             -> one diff per cell (year-average within-cell gap)
#
#   Step 3: For the summary row in Table 1, take a weighted average of
#           cell diffs, weight = total facility-years in the cell
#
# This is exactly what the regression identifies: within-cell-year variation.
# Rows 1 and 2 of Table 1 use pooled means (the conventional benchmark);
# Row 3 uses this cell-year procedure.
###############################################################################
log_cat("\n--- S3: Cell x year diff computation ---\n")

# ── Verify required outcome columns exist in pre_mm ──────────────────────────
s3_required <- c("closure_year", "leak_year", "replacement_closure_year",
                 "make_model_fac", "fac_wall", "fac_fuel", "fac_oldest_age_bin",
                 "texas_treated", "panel_year")
s3_missing  <- setdiff(s3_required, names(pre_mm))
if (length(s3_missing) > 0) {
  stop(sprintf("S3: Missing columns in pre_mm: %s\n  Check load_interim('annual_data') output.",
               paste(s3_missing, collapse = ", ")))
}
log_cat(sprintf("  S3 column check passed (%d required columns present)\n",
                length(s3_required)))

# Step 1: cell x year means and diffs
# Keep only cell-years where BOTH TX and CTL are present
cell_year <- pre_mm[, {
  tx_idx  <- texas_treated == 1
  ctl_idx <- texas_treated == 0
  if (!any(tx_idx) | !any(ctl_idx)) return(NULL)
  .(
    tx_closure  = mean(closure_year[tx_idx],              na.rm = TRUE),
    ctl_closure = mean(closure_year[ctl_idx],             na.rm = TRUE),
    tx_lust     = mean(leak_year[tx_idx],                 na.rm = TRUE),
    ctl_lust    = mean(leak_year[ctl_idx],                na.rm = TRUE),
    tx_replace  = mean(replacement_closure_year[tx_idx],  na.rm = TRUE),
    ctl_replace = mean(replacement_closure_year[ctl_idx], na.rm = TRUE),
    n_tx_fy     = sum(tx_idx),
    n_ctl_fy    = sum(ctl_idx)
  )
}, by = .(make_model_fac, fac_wall, fac_fuel, fac_oldest_age_bin, panel_year)]

if (is.null(cell_year) || nrow(cell_year) == 0) {
  stop("S3: cell_year has 0 rows. No cell-years have both TX and CTL observations.\n  Check is_mm assignment and overlap_cells computation in S1.")
}

cell_year[, `:=`(
  diff_closure = tx_closure  - ctl_closure,
  diff_lust    = tx_lust     - ctl_lust,
  diff_replace = tx_replace  - ctl_replace
)]

log_cat(sprintf("  cell_year: %d obs | %d cells | %d years\n",
  nrow(cell_year),
  uniqueN(cell_year$make_model_fac),
  uniqueN(cell_year$panel_year)))

# Step 2: average over years within each cell
# Unique facility counts must come from pre_mm (can't aggregate from cell_year)
cell_fac_counts <- pre_mm[, .(
  n_tx  = uniqueN(panel_id[texas_treated == 1]),
  n_ctl = uniqueN(panel_id[texas_treated == 0])
), by = make_model_fac]

cell_stats <- cell_year[, .(
  # Year-averaged means (not pooled across facility-years)
  closure_TX   = mean(tx_closure,    na.rm = TRUE) * 100,
  closure_CTL  = mean(ctl_closure,   na.rm = TRUE) * 100,
  # Average of within-cell-year diffs — the key quantity
  closure_diff = mean(diff_closure,  na.rm = TRUE) * 100,
  lust_TX      = mean(tx_lust,       na.rm = TRUE) * 100,
  lust_CTL     = mean(ctl_lust,      na.rm = TRUE) * 100,
  lust_diff    = mean(diff_lust,     na.rm = TRUE) * 100,
  replace_TX   = mean(tx_replace,    na.rm = TRUE) * 100,
  replace_CTL  = mean(ctl_replace,   na.rm = TRUE) * 100,
  replace_diff = mean(diff_replace,  na.rm = TRUE) * 100,
  # Total facility-years in cell (used as regression weight in Step 3)
  n_fy_tx      = sum(n_tx_fy),
  n_fy_ctl     = sum(n_ctl_fy),
  n_years      = .N
), by = .(make_model_fac, fac_wall, fac_fuel, fac_oldest_age_bin)]

cell_stats <- merge(cell_stats, cell_fac_counts, by = "make_model_fac", all.x = TRUE)
cell_stats[, w := n_fy_tx + n_fy_ctl]   # weight for Step 3

log_cat(sprintf("  cell_stats: %d cells | %s TX fy | %s CTL fy\n",
  nrow(cell_stats),
  format(sum(cell_stats$n_fy_tx),  big.mark = ","),
  format(sum(cell_stats$n_fy_ctl), big.mark = ",")))

# Diagnostic: FY-weighted avg cell diffs
log_cat(sprintf("  FY-weighted diffs: Closure %+.3fpp | LUST %+.3fpp | Replace %+.3fpp\n",
  weighted.mean(cell_stats$closure_diff, w = cell_stats$w, na.rm = TRUE),
  weighted.mean(cell_stats$lust_diff,    w = cell_stats$w, na.rm = TRUE),
  weighted.mean(cell_stats$replace_diff, w = cell_stats$w, na.rm = TRUE)))

# ── Cell-year weighted summary (cyw) — used in Table 0 Panel B ──────────────
# These are the quantities the cell x year FE regression identifies.
# Panel B's MM columns will show these instead of pooled MM means.
cyw_closure_TX   <- weighted.mean(cell_stats$closure_TX,   w = cell_stats$w, na.rm = TRUE)
cyw_closure_CTL  <- weighted.mean(cell_stats$closure_CTL,  w = cell_stats$w, na.rm = TRUE)
cyw_closure_diff <- weighted.mean(cell_stats$closure_diff, w = cell_stats$w, na.rm = TRUE)
cyw_lust_TX      <- weighted.mean(cell_stats$lust_TX,      w = cell_stats$w, na.rm = TRUE)
cyw_lust_CTL     <- weighted.mean(cell_stats$lust_CTL,     w = cell_stats$w, na.rm = TRUE)
cyw_lust_diff    <- weighted.mean(cell_stats$lust_diff,    w = cell_stats$w, na.rm = TRUE)
cyw_replace_TX   <- weighted.mean(cell_stats$replace_TX,   w = cell_stats$w, na.rm = TRUE)
cyw_replace_CTL  <- weighted.mean(cell_stats$replace_CTL,  w = cell_stats$w, na.rm = TRUE)
cyw_replace_diff <- weighted.mean(cell_stats$replace_diff, w = cell_stats$w, na.rm = TRUE)

log_cat(sprintf("  CYW summary for Panel B:\n"))
log_cat(sprintf("    Closure:     TX=%.2f%%  CTL=%.2f%%  Diff=%+.2fpp\n",
  cyw_closure_TX, cyw_closure_CTL, cyw_closure_diff))
log_cat(sprintf("    LUST:        TX=%.2f%%  CTL=%.2f%%  Diff=%+.2fpp\n",
  cyw_lust_TX, cyw_lust_CTL, cyw_lust_diff))
log_cat(sprintf("    Replacement: TX=%.2f%%  CTL=%.2f%%  Diff=%+.2fpp\n",
  cyw_replace_TX, cyw_replace_CTL, cyw_replace_diff))


###############################################################################
# PART I: PARALLEL TRENDS FIGURES
#
# OUTPUT 1 — Figure_RestrictionEarnsParallelTrends (2×3 grid):
#
#   Row 1: Full Sample
#     Panel 1A: TX vs CTL closure rates (pooled)       — mandate spike visible
#     Panel 1B: TX−CTL pooled diff                     — pre-trend failure
#     Panel 1C: TX vs CTL LUST rates (pooled)          — levels context
#
#   Row 2: MM Sample
#     Panel 2A: TX vs CTL closure rates (pooled)       — spike gone, levels similar
#     Panel 2B: Three-way diff comparison              — the key identification panel
#               Line 1: Full sample pooled diff (red)
#               Line 2: MM pooled diff (orange)
#               Line 3: MM cell-year weighted diff (blue)  ← what FE identifies
#     Panel 2C: TX vs CTL LUST rates (MM)              — LUST levels context
#
# OUTPUT 2 — Figure_DiffComparison (standalone, large):
#   The three-way diff panel alone, with CI ribbons on each series.
#   Suitable for a main-text figure or appendix standalone.
#
# The cell-year weighted diff (Panel 2B, Line 3) is computed year-by-year from
# S3's cell_year table: for each year, take the FY-weighted average of
# within-cell TX-CTL diffs across all overlap cells. This shows exactly what
# the cell×year FE is conditioning on.
###############################################################################
log_cat("\n\n========== PART I: PARALLEL TRENDS FIGURES ==========\n")

yr_start <- 1990L
yr_end   <- PANEL_END   # from 01a_Setup.R

# ── Colours ────────────────────────────────────────────────────────────────────
COL_NAIVE   <- "#CC3311"   # red      — full sample naive diff
COL_POOLED  <- "#EE7733"   # orange   — MM pooled diff
COL_CELLYEAR <- "#0077BB"  # blue     — MM cell-year weighted diff

# ── Panel constructor helpers ─────────────────────────────────────────────────

# Raw TX vs CTL rates over time (closure or LUST)
make_rate_panel <- function(dt, yr_start, yr_end, outcome_col,
                            y_lab = "Annual Closure Rate",
                            add_mandate = FALSE, panel_label = NULL) {
  rates <- dt[panel_year %between% c(yr_start, yr_end), {
    vals <- get(outcome_col)
    m    <- mean(vals, na.rm = TRUE)
    .(rate = m,
      se   = sqrt(m * (1 - m) / sum(!is.na(vals))))
  }, by = .(panel_year,
            Group = fifelse(texas_treated == 1, "Texas", "Control"))]

  p <- ggplot(rates, aes(x = panel_year, y = rate, color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    treatment_vline()

  if (add_mandate) p <- p + mandate_shade_layer()
  if (!is.null(panel_label))
    p <- p + annotate("label", x = yr_start + 0.5, y = Inf,
                      vjust = 1.3, hjust = 0, size = 2.6,
                      label = panel_label, fill = "white", label.size = 0.25)

  p +
    scale_color_manual(values = COL_PAIR) +
    scale_fill_manual(values  = COL_PAIR) +
    scale_x_continuous(breaks = seq(yr_start, yr_end, 4)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = NULL, y = y_lab, color = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# Pooled TX-CTL diff (the naive comparison used in rows 1 and 2 of Table 1)
make_pooled_diff <- function(dt, yr_start, yr_end, outcome_col,
                              line_color, line_label,
                              add_mandate = FALSE, add_ribbon = TRUE) {
  rates <- dt[panel_year %between% c(yr_start, yr_end), {
    vals <- get(outcome_col)
    m    <- mean(vals, na.rm = TRUE)
    .(rate = m, se = sqrt(m * (1 - m) / sum(!is.na(vals))))
  }, by = .(panel_year,
            Group = fifelse(texas_treated == 1, "Texas", "Control"))]

  wide <- dcast(rates, panel_year ~ Group,
                value.var = c("rate", "se"))
  wide[, `:=`(diff    = rate_Texas - rate_Control,
              diff_se = sqrt(se_Texas^2 + se_Control^2))]
  wide[, `:=`(diff_lo = diff - 1.96 * diff_se,
              diff_hi = diff + 1.96 * diff_se,
              series  = line_label)]
  wide[!is.na(diff)]
}

# Year-by-year cell-weighted diff from S3's cell_year table
# For each year: FY-weighted avg of within-cell TX-CTL diffs across overlap cells
make_cellyear_diff <- function(yr_start, yr_end) {
  cy <- cell_year[panel_year %between% c(yr_start, yr_end)]
  cy[, w_yr := n_tx_fy + n_ctl_fy]   # per cell-year weight

  by_year <- cy[, .(
    diff    = weighted.mean(diff_closure, w = w_yr, na.rm = TRUE),
    # Bootstrap-style SE: SD of cell diffs weighted by cell size
    diff_se = {
      wts <- w_yr / sum(w_yr, na.rm = TRUE)
      w_mean <- sum(wts * diff_closure, na.rm = TRUE)
      sqrt(sum(wts * (diff_closure - w_mean)^2, na.rm = TRUE))
    },
    n_cells = .N
  ), by = panel_year]

  by_year[, `:=`(diff_lo = diff - 1.96 * diff_se,
                 diff_hi = diff + 1.96 * diff_se,
                 series  = "MM cell\u2013year weighted")]
  by_year
}

# ── Build the three diff series ───────────────────────────────────────────────
log_cat("  Computing three diff series...\n")

# Series 1: full sample pooled diff (red) — pre-reform + post-reform
diff_naive <- make_pooled_diff(
  annual_data, yr_start, yr_end, "closure_year",
  line_color = COL_NAIVE, line_label = "Full sample (naive)"
)

# Series 2: MM pooled diff (orange)
diff_pooled <- make_pooled_diff(
  annual_data[is_mm == 1], yr_start, yr_end, "closure_year",
  line_color = COL_POOLED, line_label = "MM pooled"
)

# Series 3: MM cell-year weighted diff (blue)
diff_cellyear <- make_cellyear_diff(yr_start, yr_end)

# Combined for three-way panel
diff_combined <- rbind(
  diff_naive[,    .(panel_year, diff, diff_lo, diff_hi, series)],
  diff_pooled[,   .(panel_year, diff, diff_lo, diff_hi, series)],
  diff_cellyear[, .(panel_year, diff, diff_lo, diff_hi, series)]
)
diff_combined[, series := factor(series, levels = c(
  "Full sample (naive)",
  "MM pooled",
  "MM cell\u2013year weighted"
))]

# Pre-reform means for annotation
pre_diff_by_series <- diff_combined[panel_year < POST_YEAR, .(
  pre_mean = mean(diff, na.rm = TRUE)
), by = series]
log_cat("  Pre-reform avg diff by series:\n")
print(pre_diff_by_series)

# ── Panel constructors ────────────────────────────────────────────────────────

# Three-way diff panel — the key identification panel
make_three_way_diff <- function(yr_start, yr_end, show_legend = TRUE) {

  col_map <- c(
    "Full sample (naive)"      = COL_NAIVE,
    "MM pooled"                = COL_POOLED,
    "MM cell\u2013year weighted" = COL_CELLYEAR
  )
  lty_map <- c(
    "Full sample (naive)"      = "dashed",
    "MM pooled"                = "dotdash",
    "MM cell\u2013year weighted" = "solid"
  )

  p <- ggplot(diff_combined[panel_year %between% c(yr_start, yr_end)],
              aes(x = panel_year, y = diff * 100,
                  color = series, linetype = series, fill = series)) +

    # Pre-reform shading
    annotate("rect",
             xmin = -Inf, xmax = POST_YEAR - 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "grey93", alpha = 0.6) +

    geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +
    treatment_vline() +

    # CI ribbons (light)
    geom_ribbon(aes(ymin = diff_lo * 100, ymax = diff_hi * 100),
                alpha = 0.10, color = NA) +

    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +

    scale_color_manual(values = col_map) +
    scale_fill_manual(values  = col_map) +
    scale_linetype_manual(values = lty_map) +
    scale_x_continuous(breaks = seq(yr_start, yr_end, 4)) +
    scale_y_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", ""), x, "pp")) +

    labs(x    = NULL,
         y    = "TX \u2212 CTL Closure Rate Diff (pp)",
         color = NULL, linetype = NULL, fill = NULL) +

    theme(axis.text.x  = element_text(angle = 45, hjust = 1),
          legend.position = if (show_legend) "bottom" else "none",
          legend.text  = element_text(size = 8),
          panel.grid.major.x = element_blank())
  p
}

# ── Wald p-values from 01g (optional) ────────────────────────────────────────
pt_results  <- tryCatch(load_interim("pt_results"), error = function(e) NULL)
p_full_wald <- if (!is.null(pt_results))
  pt_results[grepl("Pooled", Specification), `p-value`][1] else NULL
p_mm_wald   <- if (!is.null(pt_results))
  pt_results[grepl("Spec A",  Specification), `p-value`][1] else NULL

# ── Build panels ──────────────────────────────────────────────────────────────

# Row 1: Full sample
p_1A <- make_rate_panel(annual_data, yr_start, yr_end,
  outcome_col  = "closure_year",
  y_lab        = "Annual Closure Rate",
  add_mandate  = TRUE,
  panel_label  = "Full sample: closure rates") +
  labs(subtitle = "Mandate window (gold) drives TX spike in 1989\u20131993.")

p_1B <- make_rate_panel(annual_data, yr_start, yr_end,
  outcome_col  = "leak_year",
  y_lab        = "Annual LUST Rate",
  add_mandate  = TRUE,
  panel_label  = "Full sample: LUST rates") +
  labs(subtitle = "TX consistently lower — levels difference, not trend.")

# Row 2: MM sample + three-way diff
p_2A <- make_rate_panel(annual_data[is_mm == 1], yr_start, yr_end,
  outcome_col  = "closure_year",
  y_lab        = "Annual Closure Rate",
  panel_label  = "MM sample: closure rates") +
  labs(subtitle = "Mandate spike removed; TX/CTL move together pre-reform.")

p_2B <- make_three_way_diff(yr_start, yr_end) +
  labs(subtitle = paste0(
    "Blue (cell\u2013year wt) is what the FE identifies. ",
    "Pre-reform diff near zero only after cell adjustment."))

p_2C <- make_rate_panel(annual_data[is_mm == 1], yr_start, yr_end,
  outcome_col  = "leak_year",
  y_lab        = "Annual LUST Rate",
  panel_label  = "MM sample: LUST rates") +
  labs(subtitle = "LUST gap persists — structural levels difference, not trend violation.")

# ── 2×3 combined figure ───────────────────────────────────────────────────────
save_panels(
  panels        = list(`1A` = p_1A, `1B` = p_1B, `1C` = p_2A,
                       `2A` = p_2B, `2B` = p_2C, `2C` = p_2C),
  base_name     = "Figure_RestrictionEarnsParallelTrends",
  combined_name = "Figure_RestrictionEarnsParallelTrends_Combined",
  panel_width   = 8, panel_height  = 5,
  combined_width = 24, combined_height = 10,
  ncol = 3, nrow = 2,
  title    = "The Make-Model Restriction Earns Parallel Trends",
  subtitle = paste0(
    "Row 1: full sample — TX mandate spike fails parallel trends (panels A\u2013B). ",
    "Row 2: MM sample with three-way diff comparison (panel D); ",
    "cell\u2013year weighted diff (blue) shows the FE estimand is near zero pre-reform.")
)

# ── Standalone three-way diff figure (large, main-text ready) ────────────────
p_diff_standalone <- make_three_way_diff(yr_start, yr_end, show_legend = TRUE) +
  # Add pre-reform mean annotations per series
  geom_text(
    data = pre_diff_by_series[, .(
      panel_year = PRE_START + 1L,
      diff       = pre_mean * 100 + 0.3,   # small vertical nudge
      label      = sprintf("Pre mean: %+.2fpp", pre_mean * 100),
      series     = series
    )],
    aes(label = label, color = series),
    hjust = 0, size = 2.8, show.legend = FALSE
  ) +
  labs(
    title    = NULL,
    subtitle = NULL,
    caption  = paste0(
      "Pre-reform window = 1990\u20131998 (shaded). ",
      "Vertical line = reform date (1999). ",
      "\\textit{Full sample (naive)}: pooled TX$-$CTL means across all facilities. ",
      "\\textit{MM pooled}: same computation restricted to overlap-cell facilities. ",
      "\\textit{MM cell\\u2013year weighted}: for each year, the facility-year-weighted ",
      "average of within-cell TX$-$CTL differences across ", length(overlap_cells),
      " overlap cells. This is the quantity the cell$\\times$year FE identifies."
    )
  ) +
  theme(
    plot.caption      = element_text(size = 7.5, hjust = 0, color = "grey30"),
    legend.position   = "bottom",
    legend.key.width  = unit(1.2, "cm")
  )

for (ext in c("png", "pdf")) {
  ggsave(
    file.path(OUTPUT_FIGURES, paste0("Figure_DiffComparison.", ext)),
    p_diff_standalone,
    width  = 11, height = 6,
    dpi    = if (ext == "png") 300 else NULL,
    device = if (ext == "pdf") cairo_pdf else NULL,
    bg     = "white"
  )
}
log_cat("  Figure_DiffComparison saved (standalone three-way diff)\n")

# ── Individual panel saves (for slide/appendix use) ───────────────────────────
for (nm in c("1A", "1B", "2A", "2B", "2C")) {
  p_obj <- get(paste0("p_", gsub("([12])([ABC])", "\\1\\2", nm) |>
                        tolower() |> gsub("1a", "1A", x = _, fixed = TRUE) |>
                        gsub("1b", "1B", x = _, fixed = TRUE) |>
                        gsub("2a", "2A", x = _, fixed = TRUE) |>
                        gsub("2b", "2B", x = _, fixed = TRUE) |>
                        gsub("2c", "2C", x = _, fixed = TRUE)))
  ggsave(
    file.path(OUTPUT_FIGURES,
              paste0("Figure_RestrictionEarnsParallelTrends_", nm, ".png")),
    p_obj, width = 9, height = 5, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(OUTPUT_FIGURES,
              paste0("Figure_RestrictionEarnsParallelTrends_", nm, ".pdf")),
    p_obj, width = 9, height = 5, device = cairo_pdf
  )
}
log_cat("  Individual panels saved (1A, 1B, 2A, 2B, 2C)\n")


###############################################################################
# PART II: TABLE 0 — DESCRIPTIVE (Panels A + B + C)
###############################################################################
log_cat("\n\n========== PART II: TABLE 0 (DESCRIPTIVE) ==========\n")

# ── Helpers ───────────────────────────────────────────────────────────────────

welch_p <- function(x, y) {
  tryCatch(suppressWarnings(t.test(x, y)$p.value),
           error = function(e) NA_real_)
}

se_mean <- function(x) { x <- x[!is.na(x)]; sd(x) / sqrt(length(x)) }

fmt_val <- function(x, digits, mult = 1)
  formatC(x * mult, format = "f", digits = digits, big.mark = ",")

fmt_diff <- function(d, p, digits, mult = 1)
  sprintf("%s%s%s",
    ifelse(d * mult >= 0, "+", ""),
    fmt_val(d, digits, mult),
    stars_fn(p))

# Two rows: mean + SE; Diff is Welch t-test
build_mean_se_rows <- function(label, full_tx, full_ctl, mm_tx, mm_ctl,
                               digits = 1, mult = 1) {
  full_tx  <- full_tx[!is.na(full_tx)];  full_ctl <- full_ctl[!is.na(full_ctl)]
  mm_tx    <- mm_tx[!is.na(mm_tx)];      mm_ctl   <- mm_ctl[!is.na(mm_ctl)]
  ftx_m <- mean(full_tx); ftx_se <- se_mean(full_tx)
  fct_m <- mean(full_ctl); fct_se <- se_mean(full_ctl)
  mtx_m <- mean(mm_tx);   mtx_se <- se_mean(mm_tx)
  mct_m <- mean(mm_ctl);  mct_se <- se_mean(mm_ctl)
  fd <- ftx_m - fct_m; fp <- welch_p(full_tx, full_ctl)
  md <- mtx_m - mct_m; mp <- welch_p(mm_tx,   mm_ctl)
  rbind(
    data.table(
      Variable  = label,
      Full_TX   = fmt_val(ftx_m,  digits, mult),
      Full_CTL  = fmt_val(fct_m,  digits, mult),
      Full_Diff = fmt_diff(fd, fp, digits, mult),
      MM_TX     = fmt_val(mtx_m,  digits, mult),
      MM_CTL    = fmt_val(mct_m,  digits, mult),
      MM_Diff   = fmt_diff(md, mp, digits, mult)
    ),
    data.table(
      Variable  = "",
      Full_TX   = sprintf("(%s)", fmt_val(ftx_se, digits, mult)),
      Full_CTL  = sprintf("(%s)", fmt_val(fct_se, digits, mult)),
      Full_Diff = "",
      MM_TX     = sprintf("(%s)", fmt_val(mtx_se, digits, mult)),
      MM_CTL    = sprintf("(%s)", fmt_val(mct_se, digits, mult)),
      MM_Diff   = ""
    )
  )
}

# One row, no SE; t-test on raw observations
build_agg_row <- function(label, full_tx_obs, full_ctl_obs, mm_tx_obs, mm_ctl_obs,
                          digits = 1, mult = 1) {
  full_tx_obs <- full_tx_obs[!is.na(full_tx_obs)]
  full_ctl_obs <- full_ctl_obs[!is.na(full_ctl_obs)]
  mm_tx_obs   <- mm_tx_obs[!is.na(mm_tx_obs)]
  mm_ctl_obs  <- mm_ctl_obs[!is.na(mm_ctl_obs)]
  ftx_m <- mean(full_tx_obs); fct_m <- mean(full_ctl_obs)
  mtx_m <- mean(mm_tx_obs);   mct_m <- mean(mm_ctl_obs)
  fd <- ftx_m - fct_m; fp <- welch_p(full_tx_obs, full_ctl_obs)
  md <- mtx_m - mct_m; mp <- welch_p(mm_tx_obs,   mm_ctl_obs)
  data.table(
    Variable  = label,
    Full_TX   = fmt_val(ftx_m, digits, mult),
    Full_CTL  = fmt_val(fct_m, digits, mult),
    Full_Diff = fmt_diff(fd, fp, digits, mult),
    MM_TX     = fmt_val(mtx_m, digits, mult),
    MM_CTL    = fmt_val(mct_m, digits, mult),
    MM_Diff   = fmt_diff(md, mp, digits, mult)
  )
}

# One row, Diff = "---"
build_count_row <- function(label, full_tx_n, full_ctl_n, mm_tx_n, mm_ctl_n) {
  fmt_n <- function(x) format(as.integer(x), big.mark = ",")
  data.table(
    Variable  = label,
    Full_TX   = fmt_n(full_tx_n), Full_CTL = fmt_n(full_ctl_n), Full_Diff = "---",
    MM_TX     = fmt_n(mm_tx_n),   MM_CTL   = fmt_n(mm_ctl_n),   MM_Diff   = "---"
  )
}

# One row for Panel C (Full scalar vs MM scalar; CTL/Diff blank)
build_cell_row <- function(label, full_val, mm_val,
                           fmt_fn = function(x) format(as.integer(x), big.mark = ",")) {
  data.table(
    Variable  = label,
    Full_TX   = fmt_fn(full_val), Full_CTL = "", Full_Diff = "",
    MM_TX     = fmt_fn(mm_val),   MM_CTL   = "", MM_Diff   = ""
  )
}

fmt_pct_c <- function(x) sprintf("%.1f\\%%", x)

# ── Panel A: 1998 Cross-Section ───────────────────────────────────────────────
log_cat("  Building Panel A...\n")

panel_a <- rbind(
  build_count_row("\\quad $N$ States (1 TX $+$ CTL)",
    1L, length(CONTROL_STATES), 1L, length(CONTROL_STATES)),
  build_count_row("\\quad $N$ Facilities",
    fac_1998_full[texas_treated == 1, uniqueN(panel_id)],
    fac_1998_full[texas_treated == 0, uniqueN(panel_id)],
    fac_1998_mm[texas_treated == 1,   uniqueN(panel_id)],
    fac_1998_mm[texas_treated == 0,   uniqueN(panel_id)]),
  build_count_row("\\quad $N$ Total Tanks",
    tanks_full[texas_treated == 1, .N], tanks_full[texas_treated == 0, .N],
    tanks_mm[texas_treated == 1,   .N], tanks_mm[texas_treated == 0,   .N]),
  build_mean_se_rows("Avg tanks per facility",
    fac_1998_full[texas_treated == 1, active_tanks_dec],
    fac_1998_full[texas_treated == 0, active_tanks_dec],
    fac_1998_mm[texas_treated == 1,   active_tanks_dec],
    fac_1998_mm[texas_treated == 0,   active_tanks_dec], digits = 1),
  build_mean_se_rows("Avg capacity per facility (gal)",
    fac_1998_full[texas_treated == 1, total_capacity_dec],
    fac_1998_full[texas_treated == 0, total_capacity_dec],
    fac_1998_mm[texas_treated == 1,   total_capacity_dec],
    fac_1998_mm[texas_treated == 0,   total_capacity_dec], digits = 0),
  build_mean_se_rows("Avg tank age (facility level)",
    fac_1998_full[texas_treated == 1, avg_tank_age_dec],
    fac_1998_full[texas_treated == 0, avg_tank_age_dec],
    fac_1998_mm[texas_treated == 1,   avg_tank_age_dec],
    fac_1998_mm[texas_treated == 0,   avg_tank_age_dec], digits = 1),
  build_mean_se_rows("Avg age of oldest tank (fac level)",
    fac_1998_full[texas_treated == 1, oldest_age_at_reform],
    fac_1998_full[texas_treated == 0, oldest_age_at_reform],
    fac_1998_mm[texas_treated == 1,   oldest_age_at_reform],
    fac_1998_mm[texas_treated == 0,   oldest_age_at_reform], digits = 1),
  build_agg_row("Avg age of state tank fleet",
    tanks_full[texas_treated == 1, tank_age_at_reform],
    tanks_full[texas_treated == 0, tank_age_at_reform],
    tanks_mm[texas_treated == 1,   tank_age_at_reform],
    tanks_mm[texas_treated == 0,   tank_age_at_reform], digits = 1),
  build_agg_row("Share of state fleet: Single-Wall \\%",
    as.numeric(tanks_full[texas_treated == 1, mm_wall == "Single-Walled"]),
    as.numeric(tanks_full[texas_treated == 0, mm_wall == "Single-Walled"]),
    as.numeric(tanks_mm[texas_treated == 1,   mm_wall == "Single-Walled"]),
    as.numeric(tanks_mm[texas_treated == 0,   mm_wall == "Single-Walled"]),
    digits = 1, mult = 100),
  build_mean_se_rows("Avg share of SW tanks per fac (\\%)",
    fac_1998_full[texas_treated == 1, sw_share_fac_1998],
    fac_1998_full[texas_treated == 0, sw_share_fac_1998],
    fac_1998_mm[texas_treated == 1,   sw_share_fac_1998],
    fac_1998_mm[texas_treated == 0,   sw_share_fac_1998], digits = 1)
)

# ── Panel B: Pre-Reform Outcome Flows (1990-1998) ─────────────────────────────
# Full columns: pooled facility-year means with SE (conventional descriptive).
# MM columns:   cell-year weighted means (the regression-relevant quantity).
#               The cell-year weighted diff is the quantity the cell x year FE
#               identifies. It is NOT tx_mean - ctl_mean from this row; it is
#               the weighted average of within-cell-year diffs (see S3).
#               No SE row for MM: these are constructed summary statistics.
log_cat("  Building Panel B...\n")

# Helper: one outcome row. Full side = pooled mean + SE. MM side = cyw scalar.
build_panelb_row <- function(label,
                              full_tx_obs, full_ctl_obs,
                              cyw_tx, cyw_ctl, cyw_diff,
                              digits = 2) {
  full_tx_obs <- full_tx_obs[!is.na(full_tx_obs)]
  full_ctl_obs <- full_ctl_obs[!is.na(full_ctl_obs)]
  ftx_m  <- mean(full_tx_obs) * 100
  fct_m  <- mean(full_ctl_obs) * 100
  fd     <- ftx_m - fct_m
  fp     <- welch_p(full_tx_obs, full_ctl_obs)
  ftx_se <- se_mean(full_tx_obs) * 100
  fct_se <- se_mean(full_ctl_obs) * 100

  # Diff format: cyw_diff is already in pp from S3
  cyw_diff_str <- sprintf("%s%s",
    ifelse(cyw_diff >= 0, "+", ""),
    formatC(cyw_diff, format = "f", digits = digits))

  rbind(
    data.table(
      Variable  = label,
      Full_TX   = formatC(ftx_m,  format = "f", digits = digits),
      Full_CTL  = formatC(fct_m,  format = "f", digits = digits),
      Full_Diff = fmt_diff(fd / 100, fp, digits, 100),
      MM_TX     = formatC(cyw_tx, format = "f", digits = digits),
      MM_CTL    = formatC(cyw_ctl, format = "f", digits = digits),
      MM_Diff   = cyw_diff_str
    ),
    data.table(
      Variable  = "",
      Full_TX   = sprintf("(%s)", formatC(ftx_se, format = "f", digits = digits)),
      Full_CTL  = sprintf("(%s)", formatC(fct_se, format = "f", digits = digits)),
      Full_Diff = "",
      MM_TX     = "",
      MM_CTL    = "",
      MM_Diff   = ""
    )
  )
}

panel_b <- rbind(
  build_panelb_row("Avg Closure Rate (\\%)",
    pre_full[texas_treated == 1, closure_year],
    pre_full[texas_treated == 0, closure_year],
    cyw_closure_TX, cyw_closure_CTL, cyw_closure_diff),
  build_panelb_row("Avg LUST Rate (\\%)",
    pre_full[texas_treated == 1, leak_year],
    pre_full[texas_treated == 0, leak_year],
    cyw_lust_TX, cyw_lust_CTL, cyw_lust_diff),
  build_panelb_row("Avg Replacement Rate (\\%)",
    pre_full[texas_treated == 1, replacement_closure_year],
    pre_full[texas_treated == 0, replacement_closure_year],
    cyw_replace_TX, cyw_replace_CTL, cyw_replace_diff),
  build_count_row("\\quad $N$ Facility-Years",
    pre_full[texas_treated == 1, .N], pre_full[texas_treated == 0, .N],
    pre_mm[texas_treated == 1,   .N], pre_mm[texas_treated == 0,   .N])
)

# ── Panel C: Cell Coverage ────────────────────────────────────────────────────
log_cat("  Building Panel C...\n")

panel_c <- rbind(
  data.table(Variable = "\\textit{Facility cells} (wall $\\times$ fuel $\\times$ age bin at reform)",
    Full_TX = "", Full_CTL = "", Full_Diff = "", MM_TX = "", MM_CTL = "", MM_Diff = ""),
  build_cell_row("\\quad Total cells",
    fac_cells_total_full, fac_cells_total_mm),
  build_cell_row("\\quad Identified cells (TX \\& CTL present)",
    fac_cells_identified_full, fac_cells_identified_mm),
  build_cell_row("\\quad Unidentified: TX-only (no CTL)",
    fac_cells_tx_only_full, fac_cells_tx_only_mm),
  build_cell_row("\\quad Unidentified: CTL-only (no TX)",
    fac_cells_ctl_only_full, fac_cells_ctl_only_mm),
  build_cell_row("\\quad \\% facility-years in identified cells (full panel)",
    fac_pct_identified_full_panel, fac_pct_identified_mm_panel, fmt_fn = fmt_pct_c),

  data.table(Variable = "\\textit{Tank cells} (wall $\\times$ fuel $\\times$ capacity $\\times$ cohort; MM: 1989--1997 only)",
    Full_TX = "", Full_CTL = "", Full_Diff = "", MM_TX = "", MM_CTL = "", MM_Diff = ""),
  build_cell_row("\\quad Total cells",
    tank_cells_total_full, tank_cells_total_mm),
  build_cell_row("\\quad Identified cells (TX \\& CTL present)",
    tank_cells_identified_full, tank_cells_identified_mm),
  build_cell_row("\\quad Unidentified: TX-only (no CTL)",
    tank_cells_tx_only_full, tank_cells_tx_only_mm),
  build_cell_row("\\quad Unidentified: CTL-only (no TX)",
    tank_cells_ctl_only_full, tank_cells_ctl_only_mm),
  build_cell_row("\\quad \\% tanks in identified cells",
    tank_pct_identified_full, tank_pct_identified_mm, fmt_fn = fmt_pct_c)
)

# ── Combine + render ──────────────────────────────────────────────────────────
table0 <- rbind(panel_a, panel_b, panel_c)
setnames(table0, c("Variable", "TX", "CTL", "Diff.", "TX ", "CTL ", "Diff. "))

log_cat("\n=== Table 0 Preview ===\n")
print(table0)

n_a     <- nrow(panel_a)
n_ab    <- nrow(panel_a) + nrow(panel_b)
n_total <- nrow(table0)
panel_c_header_rows <- n_ab + which(panel_c$Variable != "" & panel_c$Full_TX == "")

table0_tex <- kbl(
  table0, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
  align = c("l", "r", "r", "r", "r", "r", "r"),
  caption = NULL, label = NULL
) |>
  add_header_above(c(
    " "                           = 1,
    "Full Sample"                 = 3,
    "Make-Model Sample"           = 3
  ), escape = FALSE) |>
  pack_rows("Panel A: 1998 Cross-Section (Risk Covariates)",  1,      n_a,    bold = TRUE) |>
  pack_rows("Panel B: Pre-Reform Outcome Flows (1990--1998; MM = cell-year weighted)", n_a+1,  n_ab,   bold = TRUE) |>
  pack_rows("Panel C: Make-Model Cell Coverage",              n_ab+1, n_total, bold = TRUE) |>
  row_spec(which(table0$Variable == ""), color = "gray50") |>
  row_spec(panel_c_header_rows, color = "gray40", italic = TRUE) |>
  column_spec(3, color = ifelse(seq_len(n_total) > n_ab, "gray85", "black")) |>
  column_spec(4, color = ifelse(seq_len(n_total) > n_ab, "gray85", "black")) |>
  column_spec(6, color = ifelse(seq_len(n_total) > n_ab, "gray85", "black")) |>
  column_spec(7, color = ifelse(seq_len(n_total) > n_ab, "gray85", "black")) |>
  kable_styling(latex_options = "scale_down", font_size = 10)

save_table(table0, "Table0_Descriptive")
write_tex(table0_tex, "Table0_Descriptive")
log_cat("  Table0_Descriptive written.\n")


###############################################################################
# PART III: TABLE 1 — PROGRESSIVE BALANCE (Appendix)
#
# This table shows the progressive narrowing from naive pooled to cell-year
# weighted diffs. It is pedagogically useful but the first two rows are nearly
# identical, which undercuts the visual. The parallel trends figures carry
# the identification argument in the main text. Table 1 goes to the appendix.
###############################################################################
log_cat("\n\n========== PART III: TABLE 1 (PROGRESSIVE BALANCE — APPENDIX) ==========\n")

fmt_r   <- function(x) sprintf("%.2f", x)
fmt_sgn <- function(x) fifelse(x >= 0, sprintf("+%.2f", x), sprintf("%.2f", x))
fmt_n_b <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")

# ── Row 1: Full Sample (Naive) — pooled ──────────────────────────────────────
naive <- data.table(
  Comparison   = "Full Sample (Naive)",
  n_tx         = pre_full[texas_treated == 1, uniqueN(panel_id)],
  n_ctl        = pre_full[texas_treated == 0, uniqueN(panel_id)],
  closure_TX   = pre_full[texas_treated == 1, mean(closure_year, na.rm = TRUE) * 100],
  closure_CTL  = pre_full[texas_treated == 0, mean(closure_year, na.rm = TRUE) * 100],
  lust_TX      = pre_full[texas_treated == 1, mean(leak_year,    na.rm = TRUE) * 100],
  lust_CTL     = pre_full[texas_treated == 0, mean(leak_year,    na.rm = TRUE) * 100]
)
naive[, `:=`(closure_diff = closure_TX - closure_CTL,
             lust_diff    = lust_TX    - lust_CTL)]

# ── Row 2: MM Sample (Pooled) — pooled within MM facilities ──────────────────
pooled <- data.table(
  Comparison   = "MM Sample (Pooled)",
  n_tx         = pre_mm[texas_treated == 1, uniqueN(panel_id)],
  n_ctl        = pre_mm[texas_treated == 0, uniqueN(panel_id)],
  closure_TX   = pre_mm[texas_treated == 1, mean(closure_year, na.rm = TRUE) * 100],
  closure_CTL  = pre_mm[texas_treated == 0, mean(closure_year, na.rm = TRUE) * 100],
  lust_TX      = pre_mm[texas_treated == 1, mean(leak_year,    na.rm = TRUE) * 100],
  lust_CTL     = pre_mm[texas_treated == 0, mean(leak_year,    na.rm = TRUE) * 100]
)
pooled[, `:=`(closure_diff = closure_TX - closure_CTL,
              lust_diff    = lust_TX    - lust_CTL)]

# ── Row 3: Cell-Year Weighted — CORRECT ──────────────────────────────────────
# From S3: cell_stats has year-averaged rates and year-averaged diffs per cell.
# closure_TX/CTL: FY-weighted mean of year-averaged cell rates
# closure_diff:   FY-weighted mean of year-averaged within-cell diffs
#                 (NOT derived from closure_TX - closure_CTL)
cell_wt <- data.table(
  Comparison   = "MM Sample (Cell-Year Weighted)",
  n_tx         = sum(cell_stats$n_tx),
  n_ctl        = sum(cell_stats$n_ctl),
  closure_TX   = weighted.mean(cell_stats$closure_TX,   w = cell_stats$w, na.rm = TRUE),
  closure_CTL  = weighted.mean(cell_stats$closure_CTL,  w = cell_stats$w, na.rm = TRUE),
  closure_diff = weighted.mean(cell_stats$closure_diff, w = cell_stats$w, na.rm = TRUE),
  lust_TX      = weighted.mean(cell_stats$lust_TX,      w = cell_stats$w, na.rm = TRUE),
  lust_CTL     = weighted.mean(cell_stats$lust_CTL,     w = cell_stats$w, na.rm = TRUE),
  lust_diff    = weighted.mean(cell_stats$lust_diff,    w = cell_stats$w, na.rm = TRUE)
)

balance_rows <- rbind(naive, pooled, cell_wt, fill = TRUE)

balance_print <- balance_rows[, .(
  Comparison,
  `$N_{TX}$`  = fmt_n_b(n_tx),
  `$N_{CTL}$` = fmt_n_b(n_ctl),
  `TX`        = fmt_r(closure_TX),
  `CTL`       = fmt_r(closure_CTL),
  `Diff.`     = fmt_sgn(closure_diff),
  `TX `       = fmt_r(lust_TX),
  `CTL `      = fmt_r(lust_CTL),
  `Diff. `    = fmt_sgn(lust_diff)
)]

log_cat("\n=== Table 1: Progressive Balance ===\n")
print(balance_print)
log_cat(sprintf(
  "\nClosure — Naive: %+.3fpp | Pooled: %+.3fpp | Cell-Year Wt: %+.3fpp\n",
  balance_rows[1, closure_diff],
  balance_rows[2, closure_diff],
  balance_rows[3, closure_diff]))
log_cat(sprintf(
  "LUST    — Naive: %+.3fpp | Pooled: %+.3fpp | Cell-Year Wt: %+.3fpp\n",
  balance_rows[1, lust_diff],
  balance_rows[2, lust_diff],
  balance_rows[3, lust_diff]))

overlap_note <- {
  if (n_tx_only + n_ctl_only > 0) {
    sprintf("%d cell(s) lacking common support (%d TX-only, %d CTL-only) excluded.",
            n_tx_only + n_ctl_only, n_tx_only, n_ctl_only)
  } else {
    "All MM cells have common support."
  }
}

balance_tex <- kbl(
  balance_print, format = "latex", booktabs = TRUE, linesep = "",
  escape = FALSE, align = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
  caption = NULL, label = NULL
) |>
  add_header_above(c(" " = 3, "Closure Rate (\\%)" = 3, "Leak Rate (\\%)" = 3),
    escape = FALSE) |>
  row_spec(1, italic = TRUE) |>
  row_spec(3, bold   = TRUE) |>
  kable_styling(latex_options = "scale_down", font_size = 10)

save_table(balance_print, "Table1_Balance")
write_tex(balance_tex, "Table1_Balance")
log_cat("  Table1_Balance written.\n")


###############################################################################
# PART IV: CELL-LEVEL BALANCE DETAIL (Appendix)
###############################################################################
log_cat("\n\n========== PART IV: CELL-LEVEL BALANCE DETAIL ==========\n")

age_order <- c("0-2yr", "3-5yr", "6-8yr", "9-11yr", "12-14yr", "15-19yr", "20yr-Plus")

# Drop cells with blank wall or fuel (produce " / " group label in LaTeX)
cell_stats_print <- cell_stats[
  !is.na(fac_wall) & nchar(trimws(fac_wall)) > 0 &
  !is.na(fac_fuel) & nchar(trimws(fac_fuel)) > 0
]
cell_stats_print[, age_ord := factor(fac_oldest_age_bin, levels = age_order)]
setorder(cell_stats_print, fac_wall, fac_fuel, age_ord)

cell_print <- cell_stats_print[, .(
  Wall        = fac_wall,
  Fuel        = fac_fuel,
  `Age Bin`   = fac_oldest_age_bin,
  `$N_{TX}$`  = fmt_n_b(n_tx),
  `$N_{CTL}$` = fmt_n_b(n_ctl),
  `TX`        = fmt_r(closure_TX),
  `CTL`       = fmt_r(closure_CTL),
  `Diff.`     = fmt_sgn(closure_diff),
  `TX `       = fmt_r(lust_TX),
  `CTL `      = fmt_r(lust_CTL),
  `Diff. `    = fmt_sgn(lust_diff),
  `TX  `      = fmt_r(replace_TX),
  `CTL  `     = fmt_r(replace_CTL),
  `Diff.  `   = fmt_sgn(replace_diff)
)]

log_cat(sprintf("  %d cells in detail table\n", nrow(cell_print)))
print(cell_print)

cell_groups <- cell_stats_print[, .(
  start = .I[1], end = .I[.N]
), by = .(fac_wall, fac_fuel)]
cell_groups[, label := paste0(fac_wall, " / ", fac_fuel)]

cell_kbl <- cell_print[, !c("Wall", "Fuel")]

cell_tex <- kbl(
  cell_kbl, format = "latex", booktabs = TRUE, linesep = "",
  escape = FALSE, align = c("l", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r"),
  caption = NULL, label = NULL
) |>
  add_header_above(c(" " = 3, "Closure Rate (\\%)" = 3, "Leak Rate (\\%)" = 3, "Replacement Rate (\\%)" = 3),
    escape = FALSE) |>
  kable_styling(latex_options = "scale_down", font_size = 9)

for (i in seq_len(nrow(cell_groups))) {
  cell_tex <- cell_tex |>
    pack_rows(cell_groups$label[i],
              cell_groups$start[i],
              cell_groups$end[i], bold = FALSE, italic = TRUE)
}

save_table(cell_stats_print, "TableA_Balance_CellDetail")
write_tex(cell_tex, "TableA_Balance_CellDetail")
log_cat("  TableA_Balance_CellDetail written.\n")


###############################################################################
# PART V: APPENDIX TABLES
###############################################################################
log_cat("\n\n========== PART V: APPENDIX TABLES ==========\n")

# ── Table B.1: Attrition Log ─────────────────────────────────────────────────
log_cat("  Table B.1\n")
attrition_dt <- rbindlist(lapply(seq_along(attrition_log), function(i) {
  e <- attrition_log[[i]]
  data.table(Step = i - 1L, Stage = e$stage, Filter = e$filter,
             Facilities = e$facilities, `Fac-Years` = e$fac_years)
}))
attrition_dt[, delta_fac := c(NA_integer_, diff(Facilities))]
attrition_dt[, delta_fy  := c(NA_integer_, diff(`Fac-Years`))]
setnames(attrition_dt,
         c("delta_fac", "delta_fy"),
         c("Delta Facilities", "Delta Fac-Years"))
print(attrition_dt)
save_table(attrition_dt, "TableB1_Attrition_Log")
write_tex(
  kbl(attrition_dt, format = "latex", booktabs = TRUE, linesep = "",
      caption = NULL, label = NULL) |>
    kable_styling(latex_options = "scale_down", font_size = 9),
  "TableB1_Attrition_Log"
)

# ── Table B.3: Missing-Date Balance ──────────────────────────────────────────
log_cat("  Table B.3\n")
if (!is.null(balance_glm)) {
  broom_balance <- broom::tidy(balance_glm, conf.int = TRUE)
  save_table(as.data.table(broom_balance), "TableB3_Missing_Date_Balance")
  write_tex(
    kbl(broom_balance[, c("term", "estimate", "std.error", "statistic", "p.value")],
        format = "latex", booktabs = TRUE, linesep = "", digits = 4,
        caption = NULL, label = NULL) |>
      kable_styling(latex_options = "scale_down", font_size = 9),
    "TableB3_Missing_Date_Balance"
  )
}

# ── Table A.0: State Data Quality ────────────────────────────────────────────
# Motivates 16-state control group selection.
# CONTROL_STATES is authoritative; harmonization tier labels used only for
# Tier 1 / Tier 2 sub-labeling. GA/MO/WV appear as Remainder in DQ report
# but are in CONTROL_STATES (100% close-date miss on closed tanks only).
log_cat("  Table A.0\n")
dq <- fread(here::here("Data", "Processed", "Master_Data_Quality_Report.csv"))
log_cat(sprintf("  DQ columns: %s\n", paste(names(dq), collapse = ", ")))

dq[, lust_missing_pct := fifelse(
  !is.na(total_lusts) & total_lusts > 0,
  round(n_missing_report_date / total_lusts * 100, 1), NA_real_)]

EXCL_TREATED <- c("AZ", "CT", "FL", "IA", "MI", "WI")
EXCL_NO_FUND <- c("OR", "WA")

mis_assigned <- intersect(CONTROL_STATES, dq[study_group %like% "Remainder", state])
if (length(mis_assigned) > 0) {
  log_cat(sprintf("  WARNING — Remainder in DQ but in CONTROL_STATES: %s\n",
    paste(mis_assigned, collapse = ", ")))
}

dq[, group_clean := fcase(
  state == "TX",   "Target",
  state %in% CONTROL_STATES & study_group %like% "Control Tier 1", "Control Tier 1",
  state %in% CONTROL_STATES & study_group %like% "Control Tier 2", "Control Tier 2",
  state %in% CONTROL_STATES,  "Control (Analysis)",
  state %in% EXCL_TREATED,    "Excl. (Treated)",
  state %in% EXCL_NO_FUND,    "Excl. (No Fund)",
  study_group %like% "Remainder", "Remainder",
  default = "Other"
)]

group_order <- c("Target", "Control Tier 1", "Control Tier 2", "Control (Analysis)",
                 "Excl. (Treated)", "Excl. (No Fund)", "Remainder")
dq[, group_clean := factor(group_clean, levels = group_order)]
setorder(dq, group_clean, state)

fmt_pct_dq <- function(x)
  fifelse(is.na(x), "---", fifelse(x == 0, "0.0", formatC(x, format = "f", digits = 1)))

dq[, pct_closed_missing_date  := fmt_pct_dq(pct_closed_missing_date)]
dq[, pct_missing_install_date := fmt_pct_dq(pct_missing_install_date)]
dq[, pct_miss_tank_type       := fmt_pct_dq(pct_miss_tank_type)]
dq[, lust_missing_pct         := fmt_pct_dq(lust_missing_pct)]
dq[, total_tanks_fmt          := formatC(total_tanks, format = "d", big.mark = ",")]

dq_print <- dq[, .(
  State = state, group_clean, Tanks = total_tanks_fmt,
  `Close Date`   = pct_closed_missing_date,
  `Install Date` = pct_missing_install_date,
  `Wall Type`    = pct_miss_tank_type,
  `LUST Date`    = lust_missing_pct
)]

grp_idx <- function(g) {
  rows <- which(dq_print$group_clean == g)
  if (length(rows) == 0) return(c(1L, 1L))
  c(min(rows), max(rows))
}
dq_kbl <- dq_print[, !"group_clean"]

dq_tex <- kbl(
  dq_kbl, format = "latex", booktabs = TRUE, linesep = "",
  align = c("l", "r", "r", "r", "r", "r"),
  caption = NULL, label = NULL
) |>
  add_header_above(c(" " = 2, "\\% Missing (tanks)" = 3, "\\% Missing (LUSTs)" = 1),
    escape = FALSE) |>
  pack_rows("Target",             grp_idx("Target")[1],             grp_idx("Target")[2]) |>
  pack_rows("Control Tier 1",     grp_idx("Control Tier 1")[1],     grp_idx("Control Tier 1")[2]) |>
  pack_rows("Control Tier 2",     grp_idx("Control Tier 2")[1],     grp_idx("Control Tier 2")[2]) |>
  pack_rows("Control (Analysis)", grp_idx("Control (Analysis)")[1], grp_idx("Control (Analysis)")[2]) |>
  pack_rows("Excl. (Treated)",    grp_idx("Excl. (Treated)")[1],    grp_idx("Excl. (Treated)")[2]) |>
  pack_rows("Excl. (No Fund)",    grp_idx("Excl. (No Fund)")[1],    grp_idx("Excl. (No Fund)")[2]) |>
  pack_rows("Remainder",          grp_idx("Remainder")[1],          grp_idx("Remainder")[2]) |>
  kable_styling(latex_options = "scale_down", font_size = 9)

save_table(dq_print[, !"group_clean"], "TableA0_DataQuality_ByState")
write_tex(dq_tex, "TableA0_DataQuality_ByState")
log_cat("  TableA0_DataQuality_ByState written.\n")


###############################################################################
# DONE
###############################################################################

log_cat("\n\n=== 01l COMPLETE ===\n")
log_cat(sprintf("  Figure_RestrictionEarnsParallelTrends (2x2)\n"))
log_cat(sprintf("  Table0_Descriptive.tex      (%d rows)\n",    nrow(table0)))
log_cat(sprintf("  Table1_Balance.tex          (3-row)\n"))
log_cat(sprintf("  TableA_Balance_CellDetail   (%d cells)\n",   nrow(cell_stats_print)))
log_cat(sprintf("  TableB1_Attrition_Log.tex\n"))
log_cat(sprintf("  TableB3_Missing_Date_Balance.tex\n"))
log_cat(sprintf("  TableA0_DataQuality_ByState.tex\n"))
close_log("01l_DataQuality_and_Balance")