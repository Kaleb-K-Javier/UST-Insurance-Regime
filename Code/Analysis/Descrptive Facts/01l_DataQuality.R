#==============================================================================
# 01l_DataQuality_and_Balance.R  (REFACTORED)
# Combined: Data Quality / Parallel Trends Figure + Descriptive + Balance Tables
#
# TABLE 0 LAYOUT (matches summary_stat_table_temp.txt):
#
#   Panel A: 1998 Cross-Section (Risk Covariates)
#     Count rows (no SE, no Diff):
#       N Facilities
#       N Total Tanks
#     Facility-level rows (mean + SE, t-test Diff):
#       Avg tanks per facility              [active_tanks_dec]
#       Avg capacity per facility (gal)     [total_capacity_dec]
#       Avg tank age (facility level)       [avg_tank_age_dec]
#       Avg age of oldest tank (fac level)  [oldest_age_at_reform, from fac_mm.rds]
#     Tank-aggregate rows (fleet-level scalar, t-test Diff on tank obs, no SE line):
#       Avg age of state tank fleet         [tank_age_at_reform, from tanks_1998]
#       Share of state fleet: Single-Wall % [mm_wall=="Single-Walled", tanks_1998]
#     Facility-level proportion rows (mean + SE, t-test Diff):
#       Avg share of SW tanks per fac (%)   [single_tanks_dec / active_tanks_dec * 100]
#
#   Panel B: Pre-Reform Outcome Flows (1990–1998)
#     Facility-year rows (mean + SE, t-test Diff):
#       Avg Closure Rate (%)                [closure_year * 100]
#       Avg LUST Rate (%)                   [leak_year * 100]
#       Avg Replacement Rate (%)            [replacement_closure_year * 100]
#
# TWO SAMPLES (column groups):
#   Full Sample       — study states, unit + year FE world (no cell restriction)
#   Make-Model Sample — overlap cells only, unit + cell-by-year FE world
#
# OUTPUTS:
#   Figures:  Figure_RestrictionEarnsParallelTrends_{1A,1B,2A,2B}.png/pdf
#             Figure_RestrictionEarnsParallelTrends_Combined.png/pdf
#   Tables:   Table0_Descriptive.{csv,tex}
#             Table1_Balance.{csv,tex}
#             TableA_Balance_CellDetail.{csv,tex}
#             TableB1_Attrition_Log.{csv,tex}
#             TableB3_Missing_Date_Balance.{csv,tex}
#             TableA0_DataQuality_ByState.{csv,tex}     ← kept for Quarto embed
#
# CONTROL STATES: 16 states matching 02a_DiD_FacBehavior.R
#   (PA excluded; MI/NJ/WI not in facility analysis control group)
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
# CONSTANTS
# ─────────────────────────────────────────────────────────────────────────────

# Must match 02a_DiD_FacBehavior.R exactly
CONTROL_STATES <- c(
  "AL", "AR", "CO", "GA", "ID", "KS", "KY",
  "MD", "MN", "MO", "NC", "OH",
  "OK", "TN", "VA", "WV"
)
STUDY_STATES  <- c("TX", CONTROL_STATES)
POST_YEAR     <- 1999L
PRE_START     <- 1990L
PRE_END       <- 1998L        # Reform = 1998-12-22; year 1998 is entirely pre-reform
REFORM_DATE_D <- as.Date("1998-12-22")
REFORM_YEAR   <- 1998L


# ─────────────────────────────────────────────────────────────────────────────
# S0: LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- S0: Loading data ---\n")

annual_data         <- load_interim("annual_data")
tanks_1998          <- load_interim("tanks_1998")          # individual tanks at reform date
data_quality_report <- load_interim("data_quality_report")
attrition_log       <- load_interim("attrition_log")
balance_glm         <- load_interim("balance_glm")

# fac_mm from panel builder — has oldest_age_at_reform (continuous)
fac_mm <- readRDS(here::here("Data", "Analysis", "facility_make_model.rds"))

# ── Align treatment assignment to 02a control set ───────────────────────────
# Panel builder sets texas_treated = 1 for TX only; 0 for all non-TX.
# Override here so non-study states get NA and are excluded.
annual_data[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]
annual_data <- annual_data[!is.na(texas_treated)]

# ── tanks_1998: ensure required columns ─────────────────────────────────────
# Assign treatment status if not already present / correctly scoped
tanks_1998[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]
tanks_1998 <- tanks_1998[!is.na(texas_treated)]

# Compute exact tank age at reform date if missing
if (!"tank_age_at_reform" %in% names(tanks_1998)) {
  tanks_1998[, tank_age_at_reform :=
    as.numeric(REFORM_DATE_D - as.Date(tank_installed_date)) / 365.25]
}
tanks_1998 <- tanks_1998[!is.na(tank_age_at_reform) & tank_age_at_reform >= 0]

# ── Merge oldest_age_at_reform onto annual_data (for Panel A fac row) ────────
# fac_mm has this as a continuous value computed in the panel builder (Section 2.5)
annual_data <- merge(
  annual_data,
  fac_mm[, .(panel_id, oldest_age_at_reform)],
  by    = "panel_id",
  all.x = TRUE
)

# ── Facility-level SW share at reform (Panel A) ──────────────────────────────
# Proportion of active tanks (Dec 1998) that are single-walled, per facility
annual_data[
  panel_year == REFORM_YEAR & active_tanks_dec > 0,
  sw_share_fac_1998 := single_tanks_dec / active_tanks_dec * 100
]
# Carry the 1998 value to all years so the merge in S2 works cleanly
sw_1998 <- annual_data[panel_year == REFORM_YEAR,
                        .(panel_id, sw_share_fac_1998)]
annual_data[, sw_share_fac_1998 := NULL]
annual_data <- merge(annual_data, sw_1998, by = "panel_id", all.x = TRUE)

# ── Alias: closure_event = closure_year (annual_data may use either name) ───
if (!"closure_event" %in% names(annual_data) && "closure_year" %in% names(annual_data))
  annual_data[, closure_event := closure_year]

log_cat(sprintf("  annual_data: %s facility-years, %s facilities, %d states\n",
  format(nrow(annual_data),            big.mark = ","),
  format(uniqueN(annual_data$panel_id), big.mark = ","),
  uniqueN(annual_data$state)))
log_cat(sprintf("  tanks_1998:  %s tanks (%s TX, %s CTL)\n",
  format(nrow(tanks_1998), big.mark = ","),
  format(tanks_1998[texas_treated == 1, .N], big.mark = ","),
  format(tanks_1998[texas_treated == 0, .N], big.mark = ",")))


# ─────────────────────────────────────────────────────────────────────────────
# S1: DEFINE MM SAMPLE + OVERLAP (canonical; matches 02a sample construction)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- S1: MM sample + overlap restriction ---\n")

annual_data[, is_mm := as.integer(
  !is.na(make_model_fac) & !is.na(fac_oldest_age_bin)
)]

cell_support  <- annual_data[is_mm == 1, .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_fac]
overlap_cells <- cell_support[has_tx == TRUE & has_ctl == TRUE, make_model_fac]
n_tx_only     <- cell_support[has_tx == TRUE  & has_ctl == FALSE, .N]
n_ctl_only    <- cell_support[has_tx == FALSE & has_ctl == TRUE,  .N]

annual_data[is_mm == 1 & !make_model_fac %in% overlap_cells, is_mm := 0L]
mm_ids <- unique(annual_data[is_mm == 1, panel_id])

log_cat(sprintf("  Cells total: %d | Overlap: %d | TX-only: %d | CTL-only: %d\n",
  nrow(cell_support), length(overlap_cells), n_tx_only, n_ctl_only))
log_cat(sprintf("  MM facilities: %s (%s TX, %s CTL)\n",
  format(length(mm_ids), big.mark = ","),
  format(uniqueN(annual_data[is_mm == 1 & texas_treated == 1, panel_id]), big.mark = ","),
  format(uniqueN(annual_data[is_mm == 1 & texas_treated == 0, panel_id]), big.mark = ",")))


# ─────────────────────────────────────────────────────────────────────────────
# S2: BUILD ALL SUBSETS
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- S2: Subsets ---\n")

# 1998 cross-section (Panel A facility-level)
fac_1998_full <- annual_data[panel_year == REFORM_YEAR]
fac_1998_mm   <- annual_data[panel_year == REFORM_YEAR & panel_id %in% mm_ids]

# 1998 tanks (Panel A tank-level aggregates)
tanks_full <- tanks_1998
tanks_mm   <- tanks_1998[panel_id %in% mm_ids]

# Pre-reform flows 1990–1998 (Panel B + balance)
pre_full <- annual_data[panel_year %between% c(PRE_START, PRE_END)]
pre_mm   <- annual_data[panel_year %between% c(PRE_START, PRE_END) &
                          panel_id %in% mm_ids]

log_cat(sprintf("  1998 fac cross-section : Full=%s | MM=%s\n",
  format(nrow(fac_1998_full), big.mark = ","),
  format(nrow(fac_1998_mm),   big.mark = ",")))
log_cat(sprintf("  1998 tanks             : Full=%s | MM=%s\n",
  format(nrow(tanks_full), big.mark = ","),
  format(nrow(tanks_mm),   big.mark = ",")))
log_cat(sprintf("  Pre-reform fac-years   : Full=%s | MM=%s\n",
  format(nrow(pre_full), big.mark = ","),
  format(nrow(pre_mm),   big.mark = ",")))


# ─────────────────────────────────────────────────────────────────────────────
# S2b: CELL COVERAGE DIAGNOSTICS
#   Two cell taxonomies, reported side-by-side in Panel C of Table 0.
#
#   FACILITY cells  (make_model_fac = fac_wall × fac_fuel × oldest_age_bin):
#     "Full Sample" = all study-state facilities with a cell assignment
#     "MM Sample"   = overlap cells only (already computed in S1)
#
#   TANK cells  (make_model_tank = mm_wall × mm_fuel × mm_capacity × cohort):
#     "Full Sample" = all study-state tanks with a cell assignment (all cohorts)
#     "MM Sample"   = primary cohorts only (1989–1997, matching 02b)
#
#   For each: total cells, identified (TX+CTL both present), TX-only, CTL-only,
#   % of observations in identified cells.
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- S2b: Cell coverage diagnostics ---\n")

PRIMARY_YEARS <- as.character(1989:1997)   # matches 02b_DiD_Survival.R

# ── Facility cells ────────────────────────────────────────────────────────────
# Use the 1998 cross-section so cell assignment is fixed at the reform snapshot.
fac_cell_support_full <- fac_1998_full[!is.na(make_model_fac), .(
  has_tx  = any(texas_treated == 1),
  has_ctl = any(texas_treated == 0)
), by = make_model_fac]

fac_cells_total_full  <- nrow(fac_cell_support_full)
fac_cells_identified_full <- fac_cell_support_full[has_tx == TRUE & has_ctl == TRUE, .N]
fac_cells_tx_only_full    <- fac_cell_support_full[has_tx == TRUE & has_ctl == FALSE, .N]
fac_cells_ctl_only_full   <- fac_cell_support_full[has_tx == FALSE & has_ctl == TRUE, .N]

# Identified cell IDs in full sample (for % coverage denominator)
fac_identified_full <- fac_cell_support_full[has_tx & has_ctl, make_model_fac]
fac_pct_identified_full <- fac_1998_full[!is.na(make_model_fac),
  round(mean(make_model_fac %in% fac_identified_full) * 100, 1)]

# MM sample: overlap_cells already computed in S1
fac_cells_total_mm       <- length(overlap_cells)           # identified cells = MM cells
fac_cells_identified_mm  <- length(overlap_cells)
fac_cells_tx_only_mm     <- n_tx_only
fac_cells_ctl_only_mm    <- n_ctl_only
fac_pct_identified_mm    <- fac_1998_mm[!is.na(make_model_fac),
  round(mean(make_model_fac %in% overlap_cells) * 100, 1)]

log_cat(sprintf("  Facility cells — Full: %d total | %d identified | %d TX-only | %d CTL-only\n",
  fac_cells_total_full, fac_cells_identified_full,
  fac_cells_tx_only_full, fac_cells_ctl_only_full))
log_cat(sprintf("  Facility cells — MM  : %d overlap cells | %.1f%% of fac-years identified\n",
  fac_cells_total_mm, fac_pct_identified_mm))

# ── Tank cells ────────────────────────────────────────────────────────────────
# Full sample: all study-state tanks with a non-NA make_model_tank (all cohorts)
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

# MM / primary-cohort sample: 1989–1997 only (matches 02b mm_tank_primary)
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

log_cat(sprintf("  Tank cells — Full (all cohorts)  : %d total | %d identified | %d TX-only | %d CTL-only\n",
  tank_cells_total_full, tank_cells_identified_full,
  tank_cells_tx_only_full, tank_cells_ctl_only_full))
log_cat(sprintf("  Tank cells — MM (cohorts 1989-97): %d total | %d identified | %d TX-only | %d CTL-only\n",
  tank_cells_total_mm, tank_cells_identified_mm,
  tank_cells_tx_only_mm, tank_cells_ctl_only_mm))
log_cat(sprintf("  %% tanks in identified cells — Full: %.1f%% | MM: %.1f%%\n",
  tank_pct_identified_full, tank_pct_identified_mm))


###############################################################################
#                                                                             #
#  PART I: PARALLEL TRENDS FIGURE (2×2)                                      #
#                                                                             #
###############################################################################
log_cat("\n\n========== PART I: PARALLEL TRENDS FIGURE ==========\n")

make_rate_panel <- function(dt, yr_start, yr_end, label,
                            add_mandate = FALSE, show_label = TRUE) {
  rates <- dt[panel_year %between% c(yr_start, yr_end), .(
    closure_rate = mean(closure_event, na.rm = TRUE),
    se = sqrt(mean(closure_event, na.rm = TRUE) *
              (1 - mean(closure_event, na.rm = TRUE)) / .N)
  ), by = .(panel_year,
            Group = fifelse(texas_treated == 1, "Texas", "Control"))]

  p <- ggplot(rates,
              aes(x = panel_year, y = closure_rate,
                  color = Group, fill = Group)) +
    geom_ribbon(aes(ymin = closure_rate - 1.96 * se,
                    ymax = closure_rate + 1.96 * se),
                alpha = 0.12, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    treatment_vline()

  if (add_mandate) p <- p + mandate_shade_layer()

  if (show_label)
    p <- p + annotate("label", x = yr_start + 1, y = Inf,
                      vjust = 1.3, hjust = 0, size = 2.8,
                      label = label, fill = "white", label.size = 0.3)

  p + scale_color_manual(values = COL_PAIR) +
    scale_fill_manual(values  = COL_PAIR) +
    scale_x_continuous(breaks = seq(yr_start, yr_end, 4)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Year", y = "Annual Closure Rate", color = NULL, fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_diff_panel <- function(dt, yr_start, yr_end, wald_p = NULL,
                            add_mandate = FALSE, show_label = TRUE) {
  rates <- dt[panel_year %between% c(yr_start, yr_end), .(
    closure_rate = mean(closure_event, na.rm = TRUE),
    se = sqrt(mean(closure_event, na.rm = TRUE) *
              (1 - mean(closure_event, na.rm = TRUE)) / .N)
  ), by = .(panel_year,
            Group = fifelse(texas_treated == 1, "Texas", "Control"))]

  wide <- dcast(rates, panel_year ~ Group,
                value.var = c("closure_rate", "se"))
  wide[, `:=`(
    diff    = closure_rate_Texas - closure_rate_Control,
    diff_se = sqrt(se_Texas^2 + se_Control^2)
  )]
  wide[, `:=`(diff_lo = diff - 1.96 * diff_se,
              diff_hi = diff + 1.96 * diff_se)]

  p <- ggplot(wide[!is.na(diff)], aes(x = panel_year, y = diff)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    geom_ribbon(aes(ymin = diff_lo, ymax = diff_hi),
                fill = COL_TX, alpha = 0.18) +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(color = COL_TX, size = 1.8) +
    treatment_vline()

  if (add_mandate) p <- p + mandate_shade_layer()

  if (!is.null(wald_p) && show_label)
    p <- p + annotate("label",
                      x = yr_start + 1, y = Inf, vjust = 1.3, hjust = 0,
                      size = 2.8, fill = "white", label.size = 0.3,
                      label = sprintf("Pre-trend p = %.3f", wald_p))

  p + scale_x_continuous(breaks = seq(yr_start, yr_end, 4)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Year", y = "Closure Rate Diff (TX \u2212 CTL)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

pt_results  <- tryCatch(load_interim("pt_results"), error = function(e) NULL)
p_full_wald <- if (!is.null(pt_results))
  pt_results[grepl("Pooled", Specification), `p-value`][1] else NULL
p_mm_wald   <- if (!is.null(pt_results))
  pt_results[grepl("Spec A",  Specification), `p-value`][1] else NULL

yr_start <- 1990L
yr_end   <- PANEL_END   # defined in 01a_Setup.R

p_1A <- make_rate_panel(annual_data, yr_start, yr_end,
  label = "Full Sample", add_mandate = TRUE) +
  labs(title    = "Full Sample: Closure Rate",
       subtitle = "All incumbent facilities. Gold = TX mandate window (1989\u20131993).")

p_1B <- make_diff_panel(annual_data, yr_start, yr_end,
  wald_p = p_full_wald, add_mandate = TRUE) +
  labs(title    = "Full Sample: TX \u2212 Control Difference",
       subtitle = "Mandate-driven TX spike violates parallel trends (p < 0.05).")

p_2A <- make_rate_panel(annual_data[is_mm == 1], yr_start, yr_end,
  label = "Make-Model Sample") +
  labs(title    = "Make-Model Sample: Closure Rate",
       subtitle = "Overlap-cell facilities with cell assignment at reform date.")

p_2B <- make_diff_panel(annual_data[is_mm == 1], yr_start, yr_end,
  wald_p = p_mm_wald) +
  labs(title    = "Make-Model Sample: TX \u2212 Control Difference",
       subtitle = "Pre-period near zero. Parallel trends NOT rejected.")

save_panels(
  panels        = list(`1A` = p_1A, `1B` = p_1B, `2A` = p_2A, `2B` = p_2B),
  base_name     = "Figure_RestrictionEarnsParallelTrends",
  combined_name = "Figure_RestrictionEarnsParallelTrends_Combined",
  panel_width   = 9,  panel_height   = 5,
  combined_width = 18, combined_height = 10,
  ncol = 2, nrow = 2,
  title    = "The Make-Model Restriction Earns Parallel Trends",
  subtitle = paste0(
    "Row 1: full sample fails parallel trends due to TX mandate spike (gold shading). ",
    "Row 2: make-model sample passes. Restriction is empirically justified, not arbitrary.")
)


###############################################################################
#                                                                             #
#  PART II: TABLE 0 — DESCRIPTIVE (Panels A + B)                             #
#                                                                             #
###############################################################################
log_cat("\n\n========== PART II: TABLE 0 (DESCRIPTIVE) ==========\n")

# ── Low-level helpers ─────────────────────────────────────────────────────────

# Run a two-sided Welch t-test; return p-value (NA on error)
welch_p <- function(x, y) {
  tryCatch(
    suppressWarnings(t.test(x, y)$p.value),
    error = function(e) NA_real_
  )
}

# Standard error of the mean
se_mean <- function(x) {
  x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

# Format a number with big-mark commas
fmt_val <- function(x, digits, mult = 1) {
  formatC(x * mult, format = "f", digits = digits, big.mark = ",")
}

# Format a diff with sign prefix and significance stars
fmt_diff <- function(d, p, digits, mult = 1) {
  sprintf("%s%s%s",
    ifelse(d * mult >= 0, "+", ""),
    fmt_val(d, digits, mult),
    stars_fn(p))   # stars_fn defined in 01a_Setup.R
}

# ── Row builders ──────────────────────────────────────────────────────────────

# build_mean_se_rows():
#   Returns 2 data.table rows: [mean row] + [SE row].
#   Used for all facility-level and facility-year variables.
#
# Arguments:
#   label     : Variable label (first column)
#   full_tx / full_ctl : numeric vectors (full sample, TX vs CTL)
#   mm_tx   / mm_ctl   : numeric vectors (MM sample, TX vs CTL)
#   digits    : decimal places
#   mult      : multiplier applied before formatting (e.g. 100 for rates -> %)

build_mean_se_rows <- function(label, full_tx, full_ctl, mm_tx, mm_ctl,
                               digits = 1, mult = 1) {
  full_tx  <- full_tx[!is.na(full_tx)]
  full_ctl <- full_ctl[!is.na(full_ctl)]
  mm_tx    <- mm_tx[!is.na(mm_tx)]
  mm_ctl   <- mm_ctl[!is.na(mm_ctl)]

  ftx_m <- mean(full_tx);  ftx_se <- se_mean(full_tx)
  fct_m <- mean(full_ctl); fct_se <- se_mean(full_ctl)
  mtx_m <- mean(mm_tx);    mtx_se <- se_mean(mm_tx)
  mct_m <- mean(mm_ctl);   mct_se <- se_mean(mm_ctl)

  fd <- ftx_m - fct_m;  fp <- welch_p(full_tx, full_ctl)
  md <- mtx_m - mct_m;  mp <- welch_p(mm_tx,   mm_ctl)

  mean_row <- data.table(
    Variable  = label,
    Full_TX   = fmt_val(ftx_m,  digits, mult),
    Full_CTL  = fmt_val(fct_m,  digits, mult),
    Full_Diff = fmt_diff(fd, fp, digits, mult),
    MM_TX     = fmt_val(mtx_m,  digits, mult),
    MM_CTL    = fmt_val(mct_m,  digits, mult),
    MM_Diff   = fmt_diff(md, mp, digits, mult)
  )

  se_row <- data.table(
    Variable  = "",
    Full_TX   = sprintf("(%s)", fmt_val(ftx_se, digits, mult)),
    Full_CTL  = sprintf("(%s)", fmt_val(fct_se, digits, mult)),
    Full_Diff = "",
    MM_TX     = sprintf("(%s)", fmt_val(mtx_se, digits, mult)),
    MM_CTL    = sprintf("(%s)", fmt_val(mct_se, digits, mult)),
    MM_Diff   = ""
  )

  rbind(mean_row, se_row)
}

# build_agg_row():
#   Returns 1 data.table row (no SE line).
#   Used for state-fleet scalars (age of fleet, SW fleet share) where
#   the statistic is a population aggregate, not a facility-level average.
#   The Diff still gets a t-test because we test on underlying tank obs.
#
# Arguments same as build_mean_se_rows; tx_obs/ctl_obs are the raw tank
# vectors used for the t-test (may differ from the means if mult is applied).

build_agg_row <- function(label, full_tx_obs, full_ctl_obs, mm_tx_obs, mm_ctl_obs,
                          digits = 1, mult = 1) {
  full_tx_obs  <- full_tx_obs[!is.na(full_tx_obs)]
  full_ctl_obs <- full_ctl_obs[!is.na(full_ctl_obs)]
  mm_tx_obs    <- mm_tx_obs[!is.na(mm_tx_obs)]
  mm_ctl_obs   <- mm_ctl_obs[!is.na(mm_ctl_obs)]

  ftx_m <- mean(full_tx_obs);  fct_m <- mean(full_ctl_obs)
  mtx_m <- mean(mm_tx_obs);    mct_m <- mean(mm_ctl_obs)
  fd <- ftx_m - fct_m;  fp <- welch_p(full_tx_obs, full_ctl_obs)
  md <- mtx_m - mct_m;  mp <- welch_p(mm_tx_obs,   mm_ctl_obs)

  data.table(
    Variable  = label,
    Full_TX   = fmt_val(ftx_m,  digits, mult),
    Full_CTL  = fmt_val(fct_m,  digits, mult),
    Full_Diff = fmt_diff(fd, fp, digits, mult),
    MM_TX     = fmt_val(mtx_m,  digits, mult),
    MM_CTL    = fmt_val(mct_m,  digits, mult),
    MM_Diff   = fmt_diff(md, mp, digits, mult)
  )
}

# build_count_row():
#   Returns 1 data.table row for N counts (no SE, no Diff — shown as "---").

build_count_row <- function(label, full_tx_n, full_ctl_n, mm_tx_n, mm_ctl_n) {
  fmt_n <- function(x) format(as.integer(x), big.mark = ",")
  data.table(
    Variable  = label,
    Full_TX   = fmt_n(full_tx_n),
    Full_CTL  = fmt_n(full_ctl_n),
    Full_Diff = "---",
    MM_TX     = fmt_n(mm_tx_n),
    MM_CTL    = fmt_n(mm_ctl_n),
    MM_Diff   = "---"
  )
}

# ── Panel A: 1998 Cross-Section ───────────────────────────────────────────────
log_cat("  Building Panel A (1998 cross-section)...\n")

# -- Count rows (no SE) -------------------------------------------------------
row_a0_fac <- build_count_row(
  "\\quad $N$ Facilities",
  fac_1998_full[texas_treated == 1, uniqueN(panel_id)],
  fac_1998_full[texas_treated == 0, uniqueN(panel_id)],
  fac_1998_mm[texas_treated == 1,   uniqueN(panel_id)],
  fac_1998_mm[texas_treated == 0,   uniqueN(panel_id)]
)

row_a0_tanks <- build_count_row(
  "\\quad $N$ Total Tanks",
  tanks_full[texas_treated == 1, .N],
  tanks_full[texas_treated == 0, .N],
  tanks_mm[texas_treated == 1,   .N],
  tanks_mm[texas_treated == 0,   .N]
)

# -- Facility-level rows (mean + SE) ------------------------------------------

# Avg tanks per facility
row_a1 <- build_mean_se_rows(
  "Avg tanks per facility",
  fac_1998_full[texas_treated == 1, active_tanks_dec],
  fac_1998_full[texas_treated == 0, active_tanks_dec],
  fac_1998_mm[texas_treated == 1,   active_tanks_dec],
  fac_1998_mm[texas_treated == 0,   active_tanks_dec],
  digits = 1
)

# Avg capacity per facility (gallons)
row_a2 <- build_mean_se_rows(
  "Avg capacity per facility (gal)",
  fac_1998_full[texas_treated == 1, total_capacity_dec],
  fac_1998_full[texas_treated == 0, total_capacity_dec],
  fac_1998_mm[texas_treated == 1,   total_capacity_dec],
  fac_1998_mm[texas_treated == 0,   total_capacity_dec],
  digits = 0
)

# Avg tank age at facility level (mean of facility-level avg_tank_age)
row_a3 <- build_mean_se_rows(
  "Avg tank age (facility level)",
  fac_1998_full[texas_treated == 1, avg_tank_age_dec],
  fac_1998_full[texas_treated == 0, avg_tank_age_dec],
  fac_1998_mm[texas_treated == 1,   avg_tank_age_dec],
  fac_1998_mm[texas_treated == 0,   avg_tank_age_dec],
  digits = 1
)

# Avg age of oldest tank at facility (continuous, from fac_mm)
row_a4 <- build_mean_se_rows(
  "Avg age of oldest tank (fac level)",
  fac_1998_full[texas_treated == 1, oldest_age_at_reform],
  fac_1998_full[texas_treated == 0, oldest_age_at_reform],
  fac_1998_mm[texas_treated == 1,   oldest_age_at_reform],
  fac_1998_mm[texas_treated == 0,   oldest_age_at_reform],
  digits = 1
)

# -- State fleet aggregate rows (single scalar, no SE line) -------------------
# NOTE: "state tank fleet" stats treat each tank as an observation.
# t-test is on tank-level observations; formatted mean is the fleet aggregate.

# Avg age of state tank fleet
row_a5 <- build_agg_row(
  "Avg age of state tank fleet",
  tanks_full[texas_treated == 1, tank_age_at_reform],
  tanks_full[texas_treated == 0, tank_age_at_reform],
  tanks_mm[texas_treated == 1,   tank_age_at_reform],
  tanks_mm[texas_treated == 0,   tank_age_at_reform],
  digits = 1
)

# Share of state fleet: Single-Wall % (tank-level proportion)
row_a6 <- build_agg_row(
  "Share of state fleet: Single-Wall \\%",
  as.numeric(tanks_full[texas_treated == 1, mm_wall == "Single-Walled"]),
  as.numeric(tanks_full[texas_treated == 0, mm_wall == "Single-Walled"]),
  as.numeric(tanks_mm[texas_treated == 1,   mm_wall == "Single-Walled"]),
  as.numeric(tanks_mm[texas_treated == 0,   mm_wall == "Single-Walled"]),
  digits = 1, mult = 100
)

# -- Facility-level SW share (mean of per-facility proportion, with SE) -------
# Captures within-facility composition, not fleet-level share
row_a7 <- build_mean_se_rows(
  "Avg share of SW tanks per fac (\\%)",
  fac_1998_full[texas_treated == 1, sw_share_fac_1998],
  fac_1998_full[texas_treated == 0, sw_share_fac_1998],
  fac_1998_mm[texas_treated == 1,   sw_share_fac_1998],
  fac_1998_mm[texas_treated == 0,   sw_share_fac_1998],
  digits = 1
)

panel_a <- rbind(
  row_a0_fac,
  row_a0_tanks,
  row_a1, row_a2, row_a3, row_a4,
  row_a5, row_a6,
  row_a7
)

# ── Panel B: Pre-Reform Outcome Flows (1990–1998) ─────────────────────────────
log_cat("  Building Panel B (pre-reform 1990-1998 flows)...\n")

# Avg annual closure rate (%)
row_b1 <- build_mean_se_rows(
  "Avg Closure Rate (\\%)",
  pre_full[texas_treated == 1, closure_year],
  pre_full[texas_treated == 0, closure_year],
  pre_mm[texas_treated == 1,   closure_year],
  pre_mm[texas_treated == 0,   closure_year],
  digits = 2, mult = 100
)

# Avg annual LUST rate (%)
row_b2 <- build_mean_se_rows(
  "Avg LUST Rate (\\%)",
  pre_full[texas_treated == 1, leak_year],
  pre_full[texas_treated == 0, leak_year],
  pre_mm[texas_treated == 1,   leak_year],
  pre_mm[texas_treated == 0,   leak_year],
  digits = 2, mult = 100
)

# Avg annual replacement/retrofit rate (%)
# replacement_closure_year = 1 if facility closed a tank AND installed another
# after that closure date (i.e., forward-looking replacement flag from builder)
row_b3 <- build_mean_se_rows(
  "Avg Replacement Rate (\\%)",
  pre_full[texas_treated == 1, replacement_closure_year],
  pre_full[texas_treated == 0, replacement_closure_year],
  pre_mm[texas_treated == 1,   replacement_closure_year],
  pre_mm[texas_treated == 0,   replacement_closure_year],
  digits = 2, mult = 100
)

# Facility-year count row for Panel B
row_b_n <- build_count_row(
  "\\quad $N$ Facility-Years",
  pre_full[texas_treated == 1, .N],
  pre_full[texas_treated == 0, .N],
  pre_mm[texas_treated == 1,   .N],
  pre_mm[texas_treated == 0,   .N]
)

panel_b <- rbind(row_b1, row_b2, row_b3, row_b_n)

# ── Panel C: Make-Model Cell Coverage ─────────────────────────────────────────
# These rows describe the CELL STRUCTURE of each sample, not a TX-vs-CTL
# comparison.  The six value columns are repurposed as:
#
#   Full_TX  = value in the Full Sample   |  MM_TX  = value in the MM Sample
#   Full_CTL = (blank)                    |  MM_CTL = (blank)
#   Full_Diff = (blank)                   |  MM_Diff = (blank)
#
# The column header note in the caption explains the reinterpretation.
# All rows use build_cell_row(); no SE lines (counts/percentages, not means).

log_cat("  Building Panel C (make-model cell coverage)...\n")

build_cell_row <- function(label, full_val, mm_val, fmt_fn = function(x) format(as.integer(x), big.mark = ",")) {
  data.table(
    Variable  = label,
    Full_TX   = fmt_fn(full_val),
    Full_CTL  = "",
    Full_Diff = "",
    MM_TX     = fmt_fn(mm_val),
    MM_CTL    = "",
    MM_Diff   = ""
  )
}

fmt_pct_c <- function(x) sprintf("%.1f\\%%", x)   # e.g. "87.3\%"

panel_c <- rbind(
  # ── Facility cells (make_model_fac) ──────────────────────────────────────
  # Header-like label — no values, just a visual separator
  data.table(Variable = "\\textit{Facility cells} (wall $\\times$ fuel $\\times$ age bin)",
    Full_TX = "", Full_CTL = "", Full_Diff = "",
    MM_TX   = "", MM_CTL   = "", MM_Diff   = ""),

  build_cell_row(
    "\\quad Total cells",
    fac_cells_total_full,
    # MM column: total = identified + TX-only + CTL-only
    fac_cells_total_mm + fac_cells_tx_only_mm + fac_cells_ctl_only_mm
  ),
  build_cell_row(
    "\\quad Identified cells (TX \\& CTL present)",
    fac_cells_identified_full,
    fac_cells_identified_mm
  ),
  build_cell_row(
    "\\quad TX-only cells (no CTL overlap)",
    fac_cells_tx_only_full,
    fac_cells_tx_only_mm
  ),
  build_cell_row(
    "\\quad CTL-only cells (no TX overlap)",
    fac_cells_ctl_only_full,
    fac_cells_ctl_only_mm
  ),
  build_cell_row(
    "\\quad \\% facilities in identified cells",
    fac_pct_identified_full,
    fac_pct_identified_mm,
    fmt_fn = fmt_pct_c
  ),

  # ── Tank cells (make_model_tank) ──────────────────────────────────────────
  data.table(Variable = "\\textit{Tank cells} (wall $\\times$ fuel $\\times$ capacity $\\times$ cohort)",
    Full_TX = "", Full_CTL = "", Full_Diff = "",
    MM_TX   = "", MM_CTL   = "", MM_Diff   = ""),

  build_cell_row(
    "\\quad Total cells",
    tank_cells_total_full,
    tank_cells_total_mm
  ),
  build_cell_row(
    "\\quad Identified cells (TX \\& CTL present)",
    tank_cells_identified_full,
    tank_cells_identified_mm
  ),
  build_cell_row(
    "\\quad TX-only cells (no CTL overlap)",
    tank_cells_tx_only_full,
    tank_cells_tx_only_mm
  ),
  build_cell_row(
    "\\quad CTL-only cells (no TX overlap)",
    tank_cells_ctl_only_full,
    tank_cells_ctl_only_mm
  ),
  build_cell_row(
    "\\quad \\% tanks in identified cells",
    tank_pct_identified_full,
    tank_pct_identified_mm,
    fmt_fn = fmt_pct_c
  )
)


# ── Combine panels ────────────────────────────────────────────────────────────
table0 <- rbind(panel_a, panel_b, panel_c)

# Friendly column names for console preview
setnames(table0, c(
  "Variable",
  "TX",  "CTL",  "Diff.",
  "TX ", "CTL ", "Diff. "
))

log_cat("\n=== Table 0 Preview ===\n")
print(table0)

# ── Render LaTeX: Table 0 ─────────────────────────────────────────────────────
n_a     <- nrow(panel_a)
n_ab    <- nrow(panel_a) + nrow(panel_b)
n_total <- nrow(table0)

# Panel C section-header rows (italic sub-labels with no values)
panel_c_header_rows <- n_ab + which(panel_c$Variable != "" & panel_c$Full_TX == "")

table0_tex <- kbl(
  table0,
  format   = "latex",
  booktabs = TRUE,
  linesep  = "",
  escape   = FALSE,
  align    = c("l", "r", "r", "r", "r", "r", "r"),
  caption  = paste(
    "Sample characteristics: full analysis sample versus make-model comparison sample.",
    "\\textit{Panel A} reports the December~1998 cross-section.",
    "Facility-level rows (active tanks, capacity, tank age, oldest-tank age,",
    "single-wall share per facility) are means over facilities; parentheses",
    "report standard errors of the mean.",
    "State-fleet rows (avg age of fleet, single-wall fleet share) are",
    "population-weighted fleet aggregates across all tanks active on",
    "December~22, 1998; no SE row is shown.",
    "\\textit{Panel B} reports annual flow means over 1990--1998 (pre-reform).",
    "Replacement rate $=$ share of facility-years with at least one replacement",
    "closure (a closed tank followed by a new install at the same facility).",
    "\\textit{Panel C} reports the make-model cell structure of each",
    "regression design. In Panel~C the column headers TX and CTL do not apply;",
    "the left value column shows the full-sample statistic and the right value",
    "column shows the analysis-sample statistic.",
    "Facility cells: wall~$\\times$~fuel~$\\times$~oldest-tank age bin at reform.",
    "Tank cells: wall~$\\times$~fuel~$\\times$~capacity~$\\times$~install cohort,",
    "restricted to 1989--1997 cohorts in the MM/analysis sample.",
    "An ``identified'' cell contains at least one TX and one CTL observation;",
    "only identified cells contribute to cell-by-year fixed effects.",
    "Diff.\\ $=$ TX $-$ CTL (Panels A--B only); stars from Welch $t$-tests.",
    "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
  ),
  label = "tab:descriptive"
) |>
  add_header_above(c(
    " "                    = 1,
    "Full Sample (Unit + Year FE)"                      = 3,
    "MM Sample (Unit + Cell $\\times$ Year FE)"         = 3
  ), escape = FALSE) |>
  pack_rows(
    "Panel A: 1998 Cross-Section (Risk Covariates)",
    1, n_a, bold = TRUE, italic = FALSE
  ) |>
  pack_rows(
    "Panel B: Pre-Reform Outcome Flows (1990--1998)",
    n_a + 1, n_ab, bold = TRUE, italic = FALSE
  ) |>
  pack_rows(
    "Panel C: Make-Model Cell Coverage",
    n_ab + 1, n_total, bold = TRUE, italic = FALSE
  ) |>
  # De-emphasize SE rows (empty Variable label)
  row_spec(
    which(table0$Variable == ""),
    color = "gray50"
  ) |>
  # De-emphasize section-header rows within Panel C (italic sub-group labels)
  row_spec(
    panel_c_header_rows,
    color = "gray40", italic = TRUE
  ) |>
  kable_styling(latex_options = "scale_down", font_size = 10)

save_table(table0, "Table0_Descriptive")
write_tex(table0_tex, "Table0_Descriptive")
log_cat("  Table0_Descriptive written.\n")


###############################################################################
#                                                                             #
#  PART III: TABLE 1 — PROGRESSIVE BALANCE (3-row)                           #
#                                                                             #
###############################################################################
log_cat("\n\n========== PART III: TABLE 1 (PROGRESSIVE BALANCE) ==========\n")

fmt_r    <- function(x) sprintf("%.2f", x)
fmt_sgn  <- function(x) fifelse(x >= 0, sprintf("+%.2f", x), sprintf("%.2f", x))
fmt_n_b  <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")

# ── Row 1: Full Sample (Naive) ────────────────────────────────────────────────
naive <- data.table(
  Comparison  = "Full Sample (Naive)",
  n_tx        = pre_full[texas_treated == 1, uniqueN(panel_id)],
  n_ctl       = pre_full[texas_treated == 0, uniqueN(panel_id)],
  closure_TX  = pre_full[texas_treated == 1, mean(closure_year, na.rm = TRUE) * 100],
  closure_CTL = pre_full[texas_treated == 0, mean(closure_year, na.rm = TRUE) * 100],
  leak_TX     = pre_full[texas_treated == 1, mean(leak_year > 0, na.rm = TRUE) * 100],
  leak_CTL    = pre_full[texas_treated == 0, mean(leak_year > 0, na.rm = TRUE) * 100]
)

# ── Row 2: MM Sample (Pooled) ─────────────────────────────────────────────────
pooled <- data.table(
  Comparison  = "MM Sample (Pooled)",
  n_tx        = pre_mm[texas_treated == 1, uniqueN(panel_id)],
  n_ctl       = pre_mm[texas_treated == 0, uniqueN(panel_id)],
  closure_TX  = pre_mm[texas_treated == 1, mean(closure_year, na.rm = TRUE) * 100],
  closure_CTL = pre_mm[texas_treated == 0, mean(closure_year, na.rm = TRUE) * 100],
  leak_TX     = pre_mm[texas_treated == 1, mean(leak_year > 0, na.rm = TRUE) * 100],
  leak_CTL    = pre_mm[texas_treated == 0, mean(leak_year > 0, na.rm = TRUE) * 100]
)

# ── Per-cell stats (used by Row 3 + cell-detail appendix) ────────────────────
# fac_wall, fac_fuel, fac_oldest_age_bin are already on annual_data
cell_stats <- pre_mm[, .(
  n_tx        = uniqueN(panel_id[texas_treated == 1]),
  n_ctl       = uniqueN(panel_id[texas_treated == 0]),
  n_fy_tx     = sum(texas_treated == 1),
  n_fy_ctl    = sum(texas_treated == 0),
  closure_TX  = mean(closure_year[texas_treated == 1], na.rm = TRUE) * 100,
  closure_CTL = mean(closure_year[texas_treated == 0], na.rm = TRUE) * 100,
  leak_TX     = mean((leak_year > 0)[texas_treated == 1], na.rm = TRUE) * 100,
  leak_CTL    = mean((leak_year > 0)[texas_treated == 0], na.rm = TRUE) * 100
), by = .(make_model_fac, fac_wall, fac_fuel, fac_oldest_age_bin)]

cell_stats[, `:=`(
  closure_diff = closure_TX - closure_CTL,
  leak_diff    = leak_TX    - leak_CTL
)]

# ── Row 3: MM Sample (Cell-Weighted) ─────────────────────────────────────────
w <- cell_stats$n_tx + cell_stats$n_ctl

cell_weighted <- data.table(
  Comparison  = "MM Sample (Cell-Weighted)",
  n_tx        = sum(cell_stats$n_tx),
  n_ctl       = sum(cell_stats$n_ctl),
  closure_TX  = weighted.mean(cell_stats$closure_TX,  w = w, na.rm = TRUE),
  closure_CTL = weighted.mean(cell_stats$closure_CTL, w = w, na.rm = TRUE),
  leak_TX     = weighted.mean(cell_stats$leak_TX,     w = w, na.rm = TRUE),
  leak_CTL    = weighted.mean(cell_stats$leak_CTL,    w = w, na.rm = TRUE)
)

# ── Combine + format ──────────────────────────────────────────────────────────
balance_rows <- rbind(naive, pooled, cell_weighted)
balance_rows[, `:=`(
  closure_diff = closure_TX - closure_CTL,
  leak_diff    = leak_TX    - leak_CTL
)]

balance_print <- balance_rows[, .(
  Comparison,
  `$N_{TX}$`  = fmt_n_b(n_tx),
  `$N_{CTL}$` = fmt_n_b(n_ctl),
  `TX`        = fmt_r(closure_TX),
  `CTL`       = fmt_r(closure_CTL),
  `Diff.`     = fmt_sgn(closure_diff),
  `TX `       = fmt_r(leak_TX),
  `CTL `      = fmt_r(leak_CTL),
  `Diff. `    = fmt_sgn(leak_diff)
)]

log_cat("\n=== Table 1: Progressive Balance ===\n")
print(balance_print)
log_cat(sprintf(
  "\nClosure shrinkage: Naive %+.2fpp -> Pooled %+.2fpp -> Cell-Wt %+.2fpp\n",
  balance_rows[1, closure_diff],
  balance_rows[2, closure_diff],
  balance_rows[3, closure_diff]))
log_cat(sprintf(
  "Leak shrinkage:    Naive %+.2fpp -> Pooled %+.2fpp -> Cell-Wt %+.2fpp\n",
  balance_rows[1, leak_diff],
  balance_rows[2, leak_diff],
  balance_rows[3, leak_diff]))

# ── Render LaTeX: Table 1 ─────────────────────────────────────────────────────
overlap_note <- if (n_tx_only + n_ctl_only > 0)
  sprintf("%d cell(s) lacking common support (%d TX-only, %d CTL-only) excluded.",
          n_tx_only + n_ctl_only, n_tx_only, n_ctl_only)
else
  "All MM cells have common support."

balance_tex <- kbl(
  balance_print,
  format   = "latex",
  booktabs = TRUE,
  linesep  = "",
  escape   = FALSE,
  align    = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
  caption  = paste(
    "Progressive balance: naive comparison, MM pooled, and MM cell-weighted.",
    "\\textit{Full Sample (Naive)} is the unconditional pre-reform difference",
    "across all study-state facilities.",
    "\\textit{MM Sample (Pooled)} restricts to overlap-cell facilities",
    "but computes a raw pooled mean.",
    "\\textit{MM Sample (Cell-Weighted)} is the cell-size-weighted mean of",
    sprintf("within-cell rates across %d overlap cells.", length(overlap_cells)),
    "Shrinkage in Diff.\\ measures compositional sorting removed by the design.",
    "$N$ = unique facilities; rates averaged over 1990--1998.",
    overlap_note
  ),
  label = "tab:balance"
) |>
  add_header_above(c(
    " "                  = 3,
    "Closure Rate (\\%)" = 3,
    "Leak Rate (\\%)"    = 3
  ), escape = FALSE) |>
  row_spec(1, italic = TRUE) |>
  row_spec(3, bold   = TRUE) |>
  kable_styling(latex_options = "scale_down", font_size = 10)

save_table(balance_print, "Table1_Balance")
write_tex(balance_tex, "Table1_Balance")
log_cat("  Table1_Balance written.\n")


###############################################################################
#                                                                             #
#  PART IV: TABLE A — CELL-LEVEL BALANCE DETAIL (Appendix)                   #
#                                                                             #
###############################################################################
log_cat("\n\n========== PART IV: CELL-LEVEL BALANCE DETAIL ==========\n")

age_order <- c("0-2yr", "3-5yr", "6-8yr", "9-11yr", "12-14yr", "15-19yr", "20yr-Plus")
cell_stats[, age_ord := factor(fac_oldest_age_bin, levels = age_order)]
setorder(cell_stats, fac_wall, fac_fuel, age_ord)

cell_print <- cell_stats[, .(
  Wall      = fac_wall,
  Fuel      = fac_fuel,
  `Age Bin` = fac_oldest_age_bin,
  `$N_{TX}$`  = fmt_n_b(n_tx),
  `$N_{CTL}$` = fmt_n_b(n_ctl),
  `TX`      = fmt_r(closure_TX),
  `CTL`     = fmt_r(closure_CTL),
  `Diff.`   = fmt_sgn(closure_diff),
  `TX `     = fmt_r(leak_TX),
  `CTL `    = fmt_r(leak_CTL),
  `Diff. `  = fmt_sgn(leak_diff)
)]

log_cat(sprintf("\n  Cell detail: %d overlap cells\n", nrow(cell_print)))
print(cell_print)

# Determine pack_rows groups by Wall × Fuel
cell_groups <- cell_stats[, .(
  start = .I[1],
  end   = .I[.N]
), by = .(fac_wall, fac_fuel)]
cell_groups[, label := paste0(fac_wall, " / ", fac_fuel)]

cell_kbl <- cell_print[, !c("Wall", "Fuel")]

cell_tex <- kbl(
  cell_kbl,
  format   = "latex",
  booktabs = TRUE,
  linesep  = "",
  escape   = FALSE,
  align    = c("l", "r", "r", "r", "r", "r", "r", "r", "r"),
  caption  = paste(
    "Within-cell balance for each make-model overlap cell.",
    "Each row is one cell: wall type $\\times$ fuel type $\\times$",
    "oldest-tank age bin at December~22, 1998.",
    "Pre-reform means over 1990--1998.",
    "Diff.\\ $=$ TX $-$ CTL within cell.",
    "Cell-weighted summary appears as the final row of Table~\\ref{tab:balance}."
  ),
  label = "tab:balance_cells"
) |>
  add_header_above(c(
    " "                  = 3,
    "Closure Rate (\\%)" = 3,
    "Leak Rate (\\%)"    = 3
  ), escape = FALSE) |>
  kable_styling(latex_options = "scale_down", font_size = 9)

for (i in seq_len(nrow(cell_groups))) {
  cell_tex <- cell_tex |>
    pack_rows(cell_groups$label[i],
              cell_groups$start[i],
              cell_groups$end[i],
              bold = FALSE, italic = TRUE)
}

save_table(cell_stats, "TableA_Balance_CellDetail")
write_tex(cell_tex, "TableA_Balance_CellDetail")
log_cat("  TableA_Balance_CellDetail written.\n")


###############################################################################
#                                                                             #
#  PART V: APPENDIX TABLES (Attrition, Missing Balance, State DQ)            #
#                                                                             #
###############################################################################
log_cat("\n\n========== PART V: APPENDIX TABLES ==========\n")

# ── Table B.1: Attrition Log ─────────────────────────────────────────────────
log_cat("  Table B.1: Attrition Log\n")

attrition_dt <- rbindlist(lapply(seq_along(attrition_log), function(i) {
  e <- attrition_log[[i]]
  data.table(
    Step        = i - 1L,
    Stage       = e$stage,
    Filter      = e$filter,
    Facilities  = e$facilities,
    `Fac-Years` = e$fac_years
  )
}))
attrition_dt[, `:=`(
  `\u0394 Facilities` = c(NA_integer_, diff(Facilities)),
  `\u0394 Fac-Years`  = c(NA_integer_, diff(`Fac-Years`))
)]
print(attrition_dt)
save_table(attrition_dt, "TableB1_Attrition_Log")

write_tex(
  kbl(attrition_dt, format = "latex", booktabs = TRUE, linesep = "",
      caption = paste(
        "Sequential attrition log. Each row applies one additional filter.",
        "$\\Delta$ columns show marginal change in observations."),
      label = "tab:attrition") |>
    kable_styling(latex_options = "scale_down", font_size = 9),
  "TableB1_Attrition_Log"
)

# ── Table B.3: Missing-Date Balance ──────────────────────────────────────────
log_cat("  Table B.3: Missing-Date Balance\n")

if (!is.null(balance_glm)) {
  broom_balance <- broom::tidy(balance_glm, conf.int = TRUE)
  save_table(as.data.table(broom_balance), "TableB3_Missing_Date_Balance")

  write_tex(
    kbl(broom_balance[, c("term", "estimate", "std.error", "statistic", "p.value")],
        format = "latex", booktabs = TRUE, linesep = "", digits = 4,
        caption = paste(
          "Balance test for missing-date exclusion.",
          "Logistic regression of \\textit{any missing date} on Texas dummy.",
          "$p < 0.05$ indicates differential exclusion."),
        label = "tab:missing-balance") |>
      kable_styling(latex_options = "scale_down", font_size = 9),
    "TableB3_Missing_Date_Balance"
  )
}

# ── Table A.0: State Data Quality ────────────────────────────────────────────
# This table motivates the 16-state control group selection:
#   - Target (TX): the treated state
#   - Control Tier 1/2: states retained based on data quality thresholds
#   - Excl. (Treated): states with their own reform events
#   - Remainder: excluded for pervasive data missingness
# The Quarto document embeds this table directly from the .tex file.
log_cat("  Table A.0: State Data Quality\n")

dq <- fread(here::here("Data", "Processed", "Master_Data_Quality_Report.csv"))

log_cat(sprintf("  Columns in Master_Data_Quality_Report.csv: %s\n",
                paste(names(dq), collapse = ", ")))

# Compute LUST missing rate
dq[, lust_missing_pct := fifelse(
  !is.na(total_lusts) & total_lusts > 0,
  round(n_missing_report_date / total_lusts * 100, 1),
  NA_real_
)]

# Clean group labels
dq[, group_clean := fcase(
  study_group %like% "Target",          "Target",
  study_group %like% "Control Tier 1",  "Control Tier 1",
  study_group %like% "Control Tier 2",  "Control Tier 2",
  study_group %like% "Excluded.*Treat", "Excl. (Treated)",
  study_group %like% "Excluded.*Other", "Excl. (No Fund)",
  study_group %like% "Remainder",       "Remainder",
  default = "Other"
)]

group_order <- c("Target", "Control Tier 1", "Control Tier 2",
                 "Excl. (Treated)", "Excl. (No Fund)", "Remainder")
dq[, group_clean := factor(group_clean, levels = group_order)]
setorder(dq, group_clean, state)

fmt_pct_dq <- function(x) {
  fifelse(is.na(x), "---",
    fifelse(x == 0, "0.0",
      formatC(x, format = "f", digits = 1)))
}

dq[, pct_closed_missing_date  := fmt_pct_dq(pct_closed_missing_date)]
dq[, pct_missing_install_date := fmt_pct_dq(pct_missing_install_date)]
dq[, pct_miss_tank_type       := fmt_pct_dq(pct_miss_tank_type)]
dq[, lust_missing_pct         := fmt_pct_dq(lust_missing_pct)]
dq[, total_tanks_fmt          := formatC(total_tanks, format = "d", big.mark = ",")]

dq_print <- dq[, .(
  State          = state,
  group_clean,
  Tanks          = total_tanks_fmt,
  `Close Date`   = pct_closed_missing_date,
  `Install Date` = pct_missing_install_date,
  `Wall Type`    = pct_miss_tank_type,
  `LUST Date`    = lust_missing_pct
)]

# Helper for pack_rows row ranges
grp_idx <- function(g) {
  rows <- which(dq_print$group_clean == g)
  if (length(rows) == 0) return(c(1L, 1L))
  c(min(rows), max(rows))
}

dq_kbl <- dq_print[, !"group_clean"]

dq_tex <- kbl(
  dq_kbl,
  format   = "latex",
  booktabs = TRUE,
  linesep  = "",
  align    = c("l", "r", "r", "r", "r", "r"),
  caption  = paste(
    "State-level data completeness on four dimensions required for panel",
    "construction. Each cell $=$ \\% of records missing the indicated field.",
    "``---'' $=$ not applicable or zero denominator.",
    "Control Tier~1 states have custom cleaning pipelines; Tier~2 states use",
    "the standard EPA harmonization.",
    "Remainder states are excluded from the control group due to pervasive",
    "missingness on one or more dimensions."
  ),
  label = "tab:data-quality"
) |>
  add_header_above(
    c(" " = 2, "\\% Missing (tanks)" = 3, "\\% Missing (LUSTs)" = 1),
    escape = FALSE
  ) |>
  pack_rows("Target",          grp_idx("Target")[1],          grp_idx("Target")[2]) |>
  pack_rows("Control Tier 1",  grp_idx("Control Tier 1")[1],  grp_idx("Control Tier 1")[2]) |>
  pack_rows("Control Tier 2",  grp_idx("Control Tier 2")[1],  grp_idx("Control Tier 2")[2]) |>
  pack_rows("Excl. (Treated)", grp_idx("Excl. (Treated)")[1], grp_idx("Excl. (Treated)")[2]) |>
  pack_rows("Excl. (No Fund)", grp_idx("Excl. (No Fund)")[1], grp_idx("Excl. (No Fund)")[2]) |>
  pack_rows("Remainder",       grp_idx("Remainder")[1],       grp_idx("Remainder")[2]) |>
  kable_styling(latex_options = "scale_down", font_size = 9)

save_table(dq_print[, !"group_clean"], "TableA0_DataQuality_ByState")
write_tex(dq_tex, "TableA0_DataQuality_ByState")
log_cat("  TableA0_DataQuality_ByState written.\n")


###############################################################################
#                                                                             #
#  DONE                                                                       #
#                                                                             #
###############################################################################

log_cat("\n\n=== 01l COMPLETE ===\n")
log_cat(sprintf("  Figure: RestrictionEarnsParallelTrends (2x2 + 4 panels)\n"))
log_cat(sprintf("  Table0_Descriptive.tex      (%d rows, Panels A+B)\n", nrow(table0)))
log_cat(sprintf("  Table1_Balance.tex          (3-row progressive)\n"))
log_cat(sprintf("  TableA_Balance_CellDetail   (%d cells)\n", nrow(cell_stats)))
log_cat(sprintf("  TableB1_Attrition_Log.tex\n"))
log_cat(sprintf("  TableB3_Missing_Date_Balance.tex\n"))
log_cat(sprintf("  TableA0_DataQuality_ByState.tex\n"))
close_log("01l_DataQuality_and_Balance")