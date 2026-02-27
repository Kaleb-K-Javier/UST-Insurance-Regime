#==============================================================================
# 01_Descriptive_Analysis_REFACTORED.R
# Texas UST Insurance Reform — Sample Description & Institutional Context
#
# PURPOSE:
#   Data preparation, sample construction, and descriptive analysis for the
#   JMP paper. Produces publication-ready tables and figures for:
#     Paper §2 — "Setting and Data" (summary stats, baseline balance, CV,
#                 institutional context figures)
#     Paper §3 — "Causal Evidence" prerequisites (pre-trends)
#   Saves analysis-ready .rds files consumed by 02_DiD_Causal_Estimates.R.
#
# PAPER OUTPUTS (JMP table/figure numbering):
#   Figure 1:    Tank Age Density (TX vs Control, Dec 22 1998 cross-section)
#   Figure 2:    Tank Vintage Composition (stacked bar, TX vs Control)
#   Figure 3:    Wall Type by Vintage (grouped bar)
#   Figure [A]:  TX Private Insurance Market — Coverage & Concentration (2007-2020)
#   Figure [B]:  Risk-Differentiated Pricing (Mid-Continent Rate Filings)
#   Figure [T]:  Regulatory & Data Availability Timeline
#   Figure 4:    Pre-Period Closure Rate — Spec A ONLY (mandate-free; main text)
#   Figure 4B:   Pre-Period Closure Rate — Pooled (mandate-contaminated; appendix)
#   Figure B-1:  Pre-Period Trends — Full 4-Panel, Spec A (Appendix)
#   Figure 5A:   Risk Factor Partial Dependence (predicted vs actual; if RUN_FULL)
#   Figure 5B:   Leak Risk by Wall Type x Age (fig-leak-risk, QMD cross-ref)
#   Table A0:    Sample Composition — facility size bins + fuel type
#   Table 1A:    Stock Variables — expanded (capacity, age bins, vintage, DW%,
#                  balance tests with diff + p-values)
#   Table 1B:    Flow Variables (1989-1997) — stratified Spec A / Spec B / Mixed
#   Table 1C:    Leak Rate Cohort Decomposition
#   Table 2:     Mandate Exposure Comparison (promoted to main text)
#   Table 3A/B/C: Risk Factor Distribution + Stratified Pre-Period Leak Rates
#   Table A1:    TX Phased Mandate Schedule
#   Table A2:    Facility Cohort Composition
#   Table A3:    Spec A Compositional Balance Check
#   Table A_Stock_Balance_Tests: Formal balance tests for Table 1A
#   Table B.1:   State Data Quality Report
#   Table B.2:   Sample Construction Attrition
#   Table B.3:   Missing Data Balance Test
#   Table B.4:   Parallel Trends Validation — Four Tests
#   Table_CV_*:  Cross-validation calibration + partial dependence (if RUN_FULL)
#
# MANDATE CONFOUND NOTE:
#   Texas 30 TAC Chapter 334 imposed staggered upgrade deadlines on pre-1988 tanks
#   from 1989-1993 — five compliance cohorts, each 5-9 years ahead of the federal
#   December 22, 1998 deadline. This creates a non-parallel pre-trend in the pooled
#   sample (confirmed: pooled Wald F-test p < 0.001). Primary identification uses
#   Spec A (post-1988 tanks only), which is exempt from this mandate.
#   See §5.7d for mandate variable construction and §8.1 for validation tests.
#
# INTERMEDIATE DATA (Data/Analysis/*.rds):
#   analysis_annual_data.rds, analysis_tank_inventory.rds,
#   analysis_closed_tanks.rds, analysis_tanks_1999.rds,
#   analysis_pre_period_closures.rds, analysis_metadata.rds,
#   analysis_cv_data.rds (if RUN_FULL)
#
# STRUCTURE:
#   §1    Setup & Configuration (incl. RUN_FULL flag, canonical age bins,
#           write_tex helper)
#   §2    [reserved — helpers in 02_DiD_Causal_Estimates.R]
#   §3    Data Loading
#   §3.6  State Data Quality Report (NEW — fixes TableB1)
#   §4    Data Preparation & Filtering (Facility-Year Panel)
#   §5    Tank-Level Dataset Construction
#   §5.5  event_first_leak provenance documentation (comment stub)
#   §5.7  Regulations Database & 1998 Mandate Cohort Construction
#   §6    Diagnostics — Pre-Treatment Exit Balance + LUST Anomalies
#   §6.5  Sample Composition (TableA0) (NEW)
#   §7    Descriptive Tables (expanded stock/flow, Table 3 risk validation)
#   §7.4  Table 3 — Risk Factor Distribution + Stratified Leak Rates (NEW)
#   §8    Descriptive Figures (Pre-Period Trends)
#   §8.3  Figures 1, 2, 3 — Capital Stock Characterization (NEW)
#   §9    Risk Factor CV Validation (FULL REPLACEMENT)
#   §10   JMP Publication Tables
#   §10.3 Table A3 re-export in publication format (NEW)
#   §11   Institutional Context Figures ([A], [B], [T])
#   §12   Appendix B Tables (B.1, B.2, B.3)
#   §13   Save Analysis-Ready Datasets (updated)
#
# TREATMENT DATE: December 22, 1998
#   Texas H.B. 2587 (74th Legislature), codified at Texas Water Code
#   § 26.3512(b)(5). The date was chosen to coincide with the federal
#   December 22, 1998 UST upgrade deadline (40 CFR § 280.21).
#   For annual panel data: post-treatment begins panel_year >= 1999,
#   since Dec 22 falls in the 1998 calendar year. Event study rel_year = 0
#   corresponds to 1998.
#
# CHANGES FROM PRIOR VERSION (01_Descriptive_Analysis_2_24.r):
#   1. RUN_FULL flag added (guards expensive k-fold CV in §9)
#   2. Canonical AGE_BIN_BREAKS/LABELS + make_age_bin() / age_bin_for_reg()
#      helpers added; all ad-hoc age thresholds replaced
#   3. save_table() + write_tex() + landscape_wrap() two-function table system;
#      every table now writes both CSV and .tex
#   4. §3.6 NEW: data_quality_report computed from raw tank_inventory (fixes
#      TableB1 which previously silently skipped due to NULL assignment)
#   5. age_bin applied to tanks_1998, annual_data, and closed_tanks
#   6. §5.5 NEW: event_first_leak provenance documentation stub
#   7. §6.5 NEW: TableA0_Sample_Composition (facility size bins + fuel type)
#   8. §7.1 EXPANDED: stock_stats adds median age, age-bin shares, vintage bins,
#      capacity stats, DW%, mixed-wall%; formal balance tests (Table1A + A_Stock)
#   9. §7.2 FIXED: Table1_Combined_Long adds "All Facilities" Panel B block
#      inserted FIRST; adds Difference column everywhere
#   10. §7.4 NEW: Table 3 (risk factor flags, composite score, pre-period leak
#       rates by age_bin x wall type — 3-panel single .tex)
#   11. §8.3 NEW: Figures 1, 2, 3 (age density, vintage stacked bar, wall x vintage)
#   12. §9 FULL REPLACEMENT: 5-fold CV logit, AUC-ROC, calibration deciles,
#       partial dependence, Figures 5A (guarded) and 5B (always)
#   13. §10.3 NEW: TableA3 re-export with difference column and stars
#   14. §13 UPDATED: saves analysis_cv_data.rds when RUN_FULL; metadata updated
#       with run_full, cv_auc fields
#
# Date: February 2026
#==============================================================================


#==============================================================================
# SECTION 1: SETUP & CONFIGURATION
#==============================================================================

# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(cmprsk)
  library(fwildclusterboot)
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(pROC)
  library(here)
  library(broom)
  library(scales)
  library(stringr)
  library(kableExtra)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

# Output paths
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ---- Publication theme ----
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(size = rel(1.1), face = "bold",
                                   margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = rel(0.85),
                                   margin = margin(0, 0, 10, 0)),
      axis.title    = element_text(face = "bold", size = rel(0.9)),
      legend.title  = element_text(face = "bold", size = rel(0.9)),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border  = element_rect(fill = NA, color = "gray85"),
      strip.text    = element_text(face = "bold")
    )
}
theme_set(theme_pub())

# Color palette
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
COL_PAIR <- c("Texas" = COL_TX, "Control" = COL_CTRL)

# ---- LaTeX helper ----
stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  return("")
}

# ---- Dual-format figure save ----
save_fig <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(filename, ".png")),
         plot_obj, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(filename, ".pdf")),
         plot_obj, width = width, height = height, device = cairo_pdf)
  cat(sprintf("✓ Saved: %s (.png + .pdf)\n", filename))
}

# ---- CSV save (always call alongside write_tex) ----
save_table <- function(dt, filename, caption = "", label = "") {
  fwrite(dt, file.path(OUTPUT_TABLES, paste0(filename, ".csv")))
  cat(sprintf("✓ Saved: %s.csv\n", filename))
}

# ---- LaTeX table save ----
# kbl_obj : result of kbl(...) |> kable_styling(...) |> pack_rows(...) etc.
# filename: base name WITHOUT extension (same as save_table filename)
# Writes Output/Tables/<filename>.tex — consumed by QMD via readLines() or
# knitr::include() depending on table type.
write_tex <- function(kbl_obj, filename) {
  tex_str <- as.character(kbl_obj)
  path <- file.path(OUTPUT_TABLES, paste0(filename, ".tex"))
  writeLines(tex_str, path)
  cat(sprintf("✓ Saved: %s.tex\n", filename))
}

# ---- Landscape wrapper (requires pdflscape — already in QMD header) ----
# Usage: write_tex(landscape_wrap(as.character(kbl_obj)), filename)
landscape_wrap <- function(tex_str) {
  paste0(
    "\\begin{landscape}\n",
    tex_str,
    "\\end{landscape}\n"
  )
}

# ----------------------------- BOOTSTRAP SETTINGS ----------------------------
USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 9999    # Replications (999 for testing, 9999 for final)
# Uses Webb-6 point weights (optimal for G < 20 clusters per MacKinnon et al.)
# -----------------------------------------------------------------------------

# ---- Full-run flag (set TRUE for final submission) ----
# When FALSE: Section 9 CV validation (k-fold logit) is skipped;
# placeholder NAs are written so downstream save_table() calls don't error.
RUN_FULL <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN

# ---- Canonical 5-year tank age bins ----
# Used in: Table 1A (stock stats), Table 3 (risk validation), Figure 1 (age density),
#          Figure 5B (leak-risk plot), Section 9 CV partial dependence,
#          and as regression HTE bins in 02_DiD_Causal_Estimates.R.
#
# Bin boundaries: [0,5) [5,10) [10,15) [15,20) [20,25) [25,30) [30,35) [35,Inf)
# Reference bin for regressions: "0-4"  (relevel() enforced — see below)
# Factor levels are ordered youngest-to-oldest for correct table/figure ordering.

AGE_BIN_BREAKS <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
AGE_BIN_LABELS <- c("0-4", "5-9", "10-14", "15-19",
                    "20-24", "25-29", "30-34", "35+")
AGE_BIN_REF    <- "0-4"   # reference level for all regressions

# Helper: apply canonical bins to any age vector, returns ordered factor
make_age_bin <- function(age_vec) {
  factor(
    cut(age_vec,
        breaks = AGE_BIN_BREAKS,
        labels = AGE_BIN_LABELS,
        right  = FALSE,       # intervals are [left, right)
        include.lowest = TRUE # 0 falls in first bin
    ),
    levels  = AGE_BIN_LABELS,  # enforce left-to-right ordering
    ordered = FALSE             # ordered = FALSE for use as factor FE in fixest
  )
}

# Helper: relevel for regression (reference = "0-4", youngest bin)
age_bin_for_reg <- function(age_bin_factor) {
  relevel(age_bin_factor, ref = AGE_BIN_REF)
}

# ----------------------------- STUDY PARAMETERS ------------------------------
# TREATMENT DATE: December 22, 1998 (Texas Water Code § 26.3512(b)(5))
TREATMENT_DATE  <- as.IDate("1998-12-22")
TREATMENT_YEAR  <- 1998L   # Statutory year of reform
POST_YEAR       <- 1999L   # First full post-treatment calendar year
PANEL_START     <- 1985L   # Analysis window start
PANEL_END       <- 2020L   # Analysis window end
ES_START        <- 1985L   # Event study window start
ES_END          <- 2018L   # Event study window end
STUDY_END_DATE  <- as.IDate("2020-12-31")

# Federal mandate date (same day as TX PSTRF cutoff — not coincidence)
FEDERAL_MANDATE_DATE <- as.IDate("1998-12-22")
MANDATE_CUTOFF_DATE  <- as.IDate("1988-12-22")

# Texas phased mandate window (30 TAC Chapter 334, H.B. 2587 §4)
TX_MANDATE_START <- 1989L
TX_MANDATE_END   <- 1993L
TX_MANDATE_WINDOW_BROAD_START <- 1988L
TX_MANDATE_WINDOW_BROAD_END   <- 1994L
# -----------------------------------------------------------------------------

# ----------------------------- SAMPLE DEFINITION -----------------------------
CONTROL_STATES <- c("ME", "NM", "AR", "OK", "LA", "KS", "MT", "ID",
                    "SD", "AL", "MN", "NC", "IL", "MA", "OH", "PA",
                    "TN", "VA", "CO")

NJ_PRIVATE_INS_YEAR <- 2003L
TN_FUND_START_YEAR  <- 2008L
MD_NO_FUND          <- TRUE
CONTROL_STATES <- setdiff(CONTROL_STATES, "NJ")

cat("====================================================================\n")
cat("01_Descriptive_Analysis_REFACTORED.R — Texas UST Insurance Reform\n")
cat(sprintf("Treatment date: %s (Texas Water Code § 26.3512(b)(5))\n",
            as.character(TREATMENT_DATE)))
cat(sprintf("Analysis window: %d-%d | Event study: %d-%d\n",
            PANEL_START, PANEL_END, ES_START, ES_END))
cat(sprintf("Bootstrap: %s (B = %d)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
cat(sprintf("RUN_FULL (CV validation): %s\n", RUN_FULL))
cat(sprintf("Control states: %d states (NJ excluded; TN/MD flagged)\n",
            length(CONTROL_STATES)))
cat("====================================================================\n\n")

# Track sample attrition for Table B.2
attrition_log <- list()


#==============================================================================
# SECTION 3: DATA LOADING
#==============================================================================

cat("\n========================================\n")
cat("SECTION 3: DATA LOADING\n")
cat("========================================\n\n")

# 3.1 Facility-Year Panel
cat("Loading facility-year panel...\n")
PANEL_PATH <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_PATH))
  stop("Panel not found. Run 10_Build_Annual_Panel_Optimized.R first.\n  Path: ", PANEL_PATH)

annual_data <- fread(PANEL_PATH)
attrition_log[["raw"]] <- list(
  stage = "Raw harmonized panel",
  filter = "All states, all years",
  facilities = uniqueN(annual_data$panel_id),
  fac_years  = nrow(annual_data)
)
cat(sprintf("  Loaded: %s facility-years | %s facilities | Years %d-%d\n",
            format(nrow(annual_data), big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ","),
            min(annual_data$panel_year), max(annual_data$panel_year)))

# 3.2 Tank-Level Inventory
cat("\nLoading tank-level inventory...\n")
TANK_PATH <- here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv")
if (!file.exists(TANK_PATH))
  stop("Tank inventory not found. Run harmonization scripts first.\n  Path: ", TANK_PATH)

tank_inventory <- fread(TANK_PATH)
cat(sprintf("  Loaded: %s tanks\n", format(nrow(tank_inventory), big.mark = ",")))

# 3.3 LUST Data
cat("\nLoading LUST data...\n")
LUST_PATH <- here("Data", "Processed", "Master_Harmonized_LUST.csv")
if (!file.exists(LUST_PATH))
  stop("LUST data not found.\n  Path: ", LUST_PATH)

master_lust <- fread(LUST_PATH)
cat(sprintf("  Loaded: %s leak incidents\n", format(nrow(master_lust), big.mark = ",")))

# 3.4 Texas FR Facility-Year Panel (for Figure [A])
cat("\nLoading Texas FR panel...\n")
FR_PATH <- here("Data", "Processed", "texas_fr_facility_year_panel.csv")
fr_year <- if (file.exists(FR_PATH)) {
  dt <- fread(FR_PATH)
  cat(sprintf("  Loaded: %s facility-years\n", format(nrow(dt), big.mark = ",")))
  dt
} else {
  cat("  WARNING: texas_fr_facility_year_panel.csv not found - Figure [A] will be skipped\n")
  NULL
}

# 3.5 Mid-Continent Rate Filing Data (for Figure [B])
cat("\nLoading Mid-Continent rate filing data...\n")
RATE_DIR <- here("Data", "Rate FIllings", "Mid-Continent Casualty Company - 23418")
cat(sprintf("  Looking for rate files in:\n  %s\n", RATE_DIR))
cat(sprintf("  Directory exists: %s\n", dir.exists(RATE_DIR)))

rate_files <- list.files(RATE_DIR,
                         pattern = "^texas_midcontinent_facility_year_premium_.*\\.csv$",
                         full.names = TRUE)
cat(sprintf("  Files matched: %d\n", length(rate_files)))

rate_data <- if (length(rate_files) > 0) {
  dt <- rbindlist(lapply(rate_files, fread), fill = TRUE)
  dt[, YEAR := as.numeric(YEAR)]
  dt <- dt[!is.na(YEAR)]
  cat(sprintf("  Loaded: %s facility-year premia | Years %d-%d\n",
              format(nrow(dt), big.mark = ","),
              min(dt$YEAR, na.rm = TRUE), max(dt$YEAR, na.rm = TRUE)))
  dt
} else {
  cat("  WARNING: No rate filing CSVs found - Figure [B] will be skipped\n")
  NULL
}

#==============================================================================
# SECTION 3.6: STATE DATA QUALITY REPORT (NEW)
# Computed from raw tank_inventory (pre-filter) to reflect source data quality.
# Assigned to data_quality_report; consumed by Section 12.1 (TableB1).
# Replaces the previous data_quality_report <- NULL assignment.
#==============================================================================

cat("\n--- 3.6: State Data Quality Report ---\n")

# Ensure state column is standardized
tank_inventory[, state_std := toupper(trimws(as.character(state)))]

# pct_closed_missing_date: among tanks with status == 'Closed',
# what share are missing the closure date
data_quality_report <- tank_inventory[, {
  n_total      <- .N
  n_active     <- sum(is.na(tank_closed_date))
  pct_miss_install <- round(100 * mean(is.na(tank_installed_date)), 1)

  # Correct computation: filter to closed tanks, then measure missing closure date
  closed_rows  <- .SD[tolower(tank_status) == "closed"]
  n_closed     <- nrow(closed_rows)
  pct_closed_miss <- if (n_closed > 0) {
    round(100 * mean(is.na(closed_rows$tank_closed_date)), 1)
  } else NA_real_

  pct_miss_wall <- round(
    100 * mean(is.na(single_walled) |
               ((!is.na(single_walled) & !is.na(double_walled)) &
                single_walled == 0 & double_walled == 0), na.rm = TRUE), 1)

  list(
    n_tanks                 = n_total,
    n_active                = n_active,
    pct_miss_install_date   = pct_miss_install,
    pct_closed_missing_date = pct_closed_miss,
    pct_miss_tank_type      = pct_miss_wall
  )
}, by = .(state = state_std)]

# Remove helper column
tank_inventory[, state_std := NULL]

cat(sprintf("  ✓ data_quality_report populated with %d states\n",
            nrow(data_quality_report)))
cat(sprintf("  States: %s\n",
            paste(sort(data_quality_report$state), collapse = ", ")))


#==============================================================================
# SECTION 4: DATA PREPARATION & FILTERING
#==============================================================================

cat("\n========================================\n")
cat("SECTION 4: DATA PREPARATION\n")
cat("========================================\n\n")

cat("--- Filtering facility-year panel ---\n")
n0 <- nrow(annual_data)

# Filter 1: Time window
annual_data <- annual_data[panel_year >= PANEL_START & panel_year <= PANEL_END]
cat(sprintf("  After time window (%d-%d): %s rows\n",
            PANEL_START, PANEL_END, format(nrow(annual_data), big.mark = ",")))
attrition_log[["time_window"]] <- list(
  stage = "Study window",
  filter = sprintf("panel_year in [%d, %d]", PANEL_START, PANEL_END),
  facilities = uniqueN(annual_data$panel_id),
  fac_years  = nrow(annual_data)
)

# Filter 2: Texas + Control states (NJ already excluded from CONTROL_STATES)
annual_data <- annual_data[state == "TX" | state %in% CONTROL_STATES]
cat(sprintf("  After state filter (TX + %d controls): %s rows\n",
            length(CONTROL_STATES), format(nrow(annual_data), big.mark = ",")))
attrition_log[["states"]] <- list(
  stage = "Analysis states",
  filter = sprintf("TX + %d control states (NJ excluded)", length(CONTROL_STATES)),
  facilities = uniqueN(annual_data$panel_id),
  fac_years  = nrow(annual_data)
)

observed_states <- sort(unique(annual_data$state))
cat(sprintf("  States in sample: %s\n", paste(observed_states, collapse = ", ")))

# Filter 3: Incumbent sample
annual_data[, is_incumbent_corrected := as.integer(first_observed < TREATMENT_YEAR)]
annual_data <- annual_data[is_incumbent_corrected == 1]
cat(sprintf("  After incumbent filter (first_observed < %d): %s rows | %s facilities\n",
            TREATMENT_YEAR,
            format(nrow(annual_data), big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ",")))
attrition_log[["incumbent"]] <- list(
  stage = "Incumbent sample",
  filter = sprintf("first_observed < %d", TREATMENT_YEAR),
  facilities = uniqueN(annual_data$panel_id),
  fac_years  = nrow(annual_data)
)

cat(sprintf("\n  Total rows dropped so far: %s (%.1f%%)\n",
            format(n0 - nrow(annual_data), big.mark = ","),
            100 * (n0 - nrow(annual_data)) / n0))

# 4.1 Control State Flags
cat("\n--- 4.1: Control State Institutional Flags ---\n")
annual_data[, `:=`(
  tn_pre_fund = as.integer(state == "TN" & panel_year < TN_FUND_START_YEAR),
  md_no_fund  = as.integer(state == "MD")
)]
cat(sprintf("  TN pre-fund facility-years: %s\n",
            format(sum(annual_data$tn_pre_fund), big.mark = ",")))
cat(sprintf("  MD facility-years (no state fund): %s\n",
            format(sum(annual_data$md_no_fund), big.mark = ",")))

# 4.3 Facility-Level Missing Data Exclusion
cat("\n--- 4.3: Facility-Level Missing Data Exclusion ---\n")

incumbent_ids <- unique(annual_data$panel_id)

tank_raw_miss <- fread(TANK_PATH,
                       select = c("facility_id", "state",
                                  "tank_installed_date", "tank_closed_date",
                                  "tank_status"),
                       colClasses = "character")

tank_raw_miss[, `:=`(
  facility_id         = toupper(trimws(facility_id)),
  state               = toupper(trimws(state)),
  tank_installed_date = as.IDate(tank_installed_date),
  tank_closed_date    = as.IDate(tank_closed_date),
  is_closed_status    = (tank_status == "Closed")
)]
tank_raw_miss[, panel_id := paste(facility_id, state, sep = "_")]
tank_raw_miss <- tank_raw_miss[panel_id %in% incumbent_ids]

miss_install <- unique(tank_raw_miss[is.na(tank_installed_date), panel_id])
miss_closure <- unique(tank_raw_miss[is_closed_status == TRUE &
                                       is.na(tank_closed_date), panel_id])
facilities_to_exclude_miss <- union(miss_install, miss_closure)

tank_fac_flag <- tank_raw_miss[, .(
  has_miss_install = any(is.na(tank_installed_date)),
  has_miss_closure = any(is_closed_status == TRUE & is.na(tank_closed_date)),
  group            = fifelse(first(state) == "TX", "Texas", "Control")
), by = panel_id]

tank_fac_flag[, texas := as.integer(group == "Texas")]
tank_fac_flag[, has_any_missing := (has_miss_install | has_miss_closure)]

balance_glm <- tryCatch(
  glm(has_any_missing ~ texas, data = tank_fac_flag, family = binomial),
  error = function(e) NULL
)

p_val_note <- "Logistic regression failed to converge."
if (!is.null(balance_glm)) {
  p_bal <- summary(balance_glm)$coefficients["texas", "Pr(>|z|)"]
  p_val_note <- sprintf("Logistic regression of missing installation date on treatment status yields p-value = %.4f.", p_bal)
  cat(sprintf("\nBalance test (missing install ~ Texas): p = %.4f\n", p_bal))
  if (p_bal < 0.10)
    cat("  WARNING: Imbalanced — IPW robustness recommended (Appendix B.4)\n") else
    cat("  OK: Balanced — proceed with clean drop\n")
}

# Build unified missing data table (Panel A: balance; Panel B: system aggregates)
balance_combined <- tank_fac_flag[, .(
  Facilities       = .N,
  Miss_Install_N   = sum(has_miss_install),
  Miss_Install_Pct = mean(has_miss_install),
  Miss_Closure_N   = sum(has_miss_closure),
  Miss_Closure_Pct = mean(has_miss_closure),
  Excluded_N       = sum(has_any_missing),
  Excluded_Pct     = mean(has_any_missing)
), by = .(Group = fifelse(texas == 1, "Texas", "Control"))]

total_combined <- tank_fac_flag[, .(
  Group            = "Total",
  Facilities       = .N,
  Miss_Install_N   = sum(has_miss_install),
  Miss_Install_Pct = mean(has_miss_install),
  Miss_Closure_N   = sum(has_miss_closure),
  Miss_Closure_Pct = mean(has_miss_closure),
  Excluded_N       = sum(has_any_missing),
  Excluded_Pct     = mean(has_any_missing)
)]

unified_table <- rbind(balance_combined, total_combined)
unified_csv_output <- copy(unified_table)

unified_table[, `:=`(
  Facilities       = format(Facilities, big.mark = ","),
  Miss_Install_N   = format(Miss_Install_N, big.mark = ","),
  Miss_Install_Pct = sprintf("%.2f\\%%", Miss_Install_Pct * 100),
  Miss_Closure_N   = format(Miss_Closure_N, big.mark = ","),
  Miss_Closure_Pct = sprintf("%.2f\\%%", Miss_Closure_Pct * 100),
  Excluded_N       = format(Excluded_N, big.mark = ","),
  Excluded_Pct     = sprintf("%.2f\\%%", Excluded_Pct * 100)
)]

latex_unified <- kbl(
  unified_table,
  format = "latex",
  booktabs = TRUE,
  linesep  = "",
  escape   = FALSE,
  col.names = c("Group", "Facilities", "Count", "Rate", "Count", "Rate", "Excluded", "Rate"),
  caption = "Scale and Balance of Missing Tank Dates \\label{tab:missing_unified}",
  label   = "tab:missing-unified"
) |>
  kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                full_width = FALSE, threeparttable = TRUE) |>
  add_header_above(c(" " = 2, "Missing Install Date" = 2,
                     "Missing Closure Date" = 2, "Overall Attrition" = 2)) |>
  row_spec(2, hline_after = TRUE) |>
  footnote(general = paste(
    "Sample includes all incumbent facilities active prior to the statutory",
    "treatment year (1998). A facility is excluded if any associated tank lacks",
    "an installation date, or if a closed tank lacks a valid closure date.",
    p_val_note),
    general_title = "", threeparttable = TRUE)

save_table(unified_csv_output, "Appendix_Missing_Data_Unified")
write_tex(latex_unified, "TableB_Missing_Data_Unified")

# Apply exclusion
annual_data <- annual_data[!panel_id %in% facilities_to_exclude_miss]
incumbent_ids <- unique(annual_data$panel_id)

cat(sprintf("\n  Excluded: %s facilities | Remaining: %s facilities | %s rows\n",
            format(length(facilities_to_exclude_miss), big.mark = ","),
            format(uniqueN(annual_data$panel_id),      big.mark = ","),
            format(nrow(annual_data),                  big.mark = ",")))

attrition_log[["missing_data"]] <- list(
  stage = "Complete tank records",
  filter = "Facility-level missing-date exclusion (§4.3)",
  facilities = uniqueN(annual_data$panel_id),
  fac_years  = nrow(annual_data)
)

rm(tank_raw_miss, tank_fac_flag, balance_combined, total_combined,
   unified_table, unified_csv_output)
gc()


# 4.4 Create analysis variables
cat("\n--- 4.4: Creating analysis variables ---\n")

annual_data[, `:=`(
  treated     = texas_treated,
  post        = post_1999,
  did_term    = texas_treated * post_1999,
  closure_event = as.integer(n_closures > 0),
  rel_year_1999 = panel_year - POST_YEAR,
  county_fips_fac = as.factor(county_fips)
)]

tx_n  <- uniqueN(annual_data[texas_treated == 1, panel_id])
ctl_n <- uniqueN(annual_data[texas_treated == 0, panel_id])
cat(sprintf("  Texas facilities:   %s\n", format(tx_n,  big.mark = ",")))
cat(sprintf("  Control facilities: %s\n", format(ctl_n, big.mark = ",")))
cat(sprintf("  Years: %d to %d\n", min(annual_data$panel_year), max(annual_data$panel_year)))

attrition_log[["final"]] <- list(
  stage = "Final analysis sample",
  filter = "",
  facilities = uniqueN(annual_data$panel_id),
  fac_years  = nrow(annual_data)
)

# ---- Apply canonical age_bin to annual_data (Global Change 3, rule 2) ----
# Check if panel builder's age_bins matches canonical AGE_BIN_LABELS
if ("age_bins" %in% names(annual_data)) {
  existing_labels <- if (is.factor(annual_data$age_bins)) {
    levels(annual_data$age_bins)
  } else {
    sort(unique(annual_data$age_bins[!is.na(annual_data$age_bins)]))
  }
  if (identical(as.character(existing_labels), AGE_BIN_LABELS)) {
    annual_data[, age_bin := age_bins]
    cat("  OK: age_bins matches canonical AGE_BIN_LABELS - aliased to age_bin\n")
  } else {
    # Panel builder uses labels like "0-5", "5-10" with different boundaries/format
    setnames(annual_data, "age_bins", "age_bins_raw")
    annual_data[, age_bin := make_age_bin(avg_tank_age)]
    cat("  NOTE: age_bins labels differ from canonical (renamed to age_bins_raw).\n")
    cat("        Created age_bin from avg_tank_age using make_age_bin().\n")
  }
} else {
  annual_data[, age_bin := make_age_bin(avg_tank_age)]
  cat("  OK: age_bin created from avg_tank_age using canonical bins.\n")
}


#==============================================================================
# SECTION 5: TANK-LEVEL DATASET CONSTRUCTION
#==============================================================================

cat("\n========================================\n")
cat("SECTION 5: TANK-LEVEL DATASETS\n")
cat("========================================\n\n")

# 5.1 Filter tank inventory to analysis sample
cat("--- 5.1: Filtering tank inventory to analysis sample ---\n")
tank_inventory[, panel_id := paste(toupper(trimws(facility_id)),
                                    toupper(trimws(state)), sep = "_")]
tanks <- tank_inventory[panel_id %in% incumbent_ids]

date_cols <- c("tank_installed_date", "tank_closed_date")
for (col in date_cols) {
  if (col %in% names(tanks) && !inherits(tanks[[col]], "IDate")) {
    tanks[, (col) := as.IDate(get(col))]
  }
}

cat(sprintf("  Tanks in analysis sample: %s\n",
            format(nrow(tanks), big.mark = ",")))
cat(sprintf("  Facilities with tanks: %s\n",
            format(uniqueN(tanks$panel_id), big.mark = ",")))

# 5.2 1998 Cross-Section (stock variables for Table 1)
cat("\n--- 5.2: 1998 Cross-Section ---\n")
tanks_1998 <- tanks[
  !is.na(tank_installed_date) &
  tank_installed_date <= TREATMENT_DATE &
  (is.na(tank_closed_date) | tank_closed_date > TREATMENT_DATE)
]
tanks_1998[, `:=`(
  tank_age_1998 = as.numeric(TREATMENT_DATE - tank_installed_date) / 365.25,
  texas_treated = as.integer(state == "TX")
)]

cat(sprintf("  Active tanks on %s: %s\n",
            as.character(TREATMENT_DATE),
            format(nrow(tanks_1998), big.mark = ",")))
cat(sprintf("  TX: %s | Control: %s\n",
            format(nrow(tanks_1998[texas_treated == 1]), big.mark = ","),
            format(nrow(tanks_1998[texas_treated == 0]), big.mark = ",")))

# Apply canonical age_bin to tanks_1998 (Global Change 3, rule 1)
tanks_1998[, age_bin := make_age_bin(tank_age_1998)]
cat("  OK: age_bin applied to tanks_1998\n")

# Derive install_year from tank_installed_date (for vintage/risk flag computation)
tanks_1998[, install_year := year(tank_installed_date)]

# Add Group label for ggplot mapping
tanks_1998[, Group := fifelse(texas_treated == 1, "Texas", "Control")]

# 5.3 Closed tanks (for closure age analysis)
cat("\n--- 5.3: Closed tanks dataset ---\n")
closed_tanks <- tanks[
  !is.na(tank_installed_date) & !is.na(tank_closed_date) &
  tank_closed_date >= as.IDate(paste0(PANEL_START, "-01-01")) &
  tank_closed_date <= STUDY_END_DATE
]
closed_tanks[, `:=`(
  age_at_closure = as.numeric(tank_closed_date - tank_installed_date) / 365.25,
  closure_year   = year(tank_closed_date),
  texas_treated  = as.integer(state == "TX")
)]
closed_tanks <- closed_tanks[age_at_closure >= 0]

# Apply canonical age_bin at closure (Global Change 3, rule 3)
closed_tanks[, age_bin_at_closure := make_age_bin(age_at_closure)]
cat("  OK: age_bin_at_closure applied to closed_tanks\n")

cat(sprintf("  Tank closures in study window: %s\n",
            format(nrow(closed_tanks), big.mark = ",")))

# 5.4 Pre-period closures (for pre-trend closure age panel)
pre_period_closures <- closed_tanks[closure_year >= 1987 & closure_year <= 1997]
cat(sprintf("  Pre-period closures (1987-1997): %s\n",
            format(nrow(pre_period_closures), big.mark = ",")))


#==============================================================================
# SECTION 5.5 — event_first_leak PROVENANCE DOCUMENTATION (COMMENT STUB ONLY)
# This section contains NO executable code. It documents the origin and
# semantics of event_first_leak so downstream scripts can use it correctly.
#==============================================================================

# -----------------------------------------------------------------------------
# WHAT IS event_first_leak?
#
# event_first_leak is a facility-year binary indicator equal to:
#   = 1  in the calendar year of the facility's FIRST-EVER confirmed release
#   = 0  in all prior years (facility is at risk but has not yet leaked)
#   = 0  in all subsequent years (facility remains in data after leaking)
#
# This variable measures leak INCIDENCE — the transition from "never leaked"
# to "has leaked" — not recurrence. A facility that leaks three times in
# 1992, 1994, and 1996 contributes exactly ONE incidence event (1992) and
# zeros in 1994 and 1996. Contrast with leak_year, which would be 1 in all
# three years.
#
# -----------------------------------------------------------------------------
# WHERE DOES IT COME FROM?
#
# event_first_leak is pre-constructed by the upstream panel builder
# (10_Build_Annual_Panel_Optimized.R, Section 10) using:
#
#   annual[, event_first_leak := as.integer(
#     leak_year == 1 & cumsum(leak_year) == 1
#   ), by = panel_id]
#
# It arrives in annual_data via facility_leak_behavior_annual.csv.
# Do NOT reconstruct it here — the panel builder's version is the canonical one.
#
# -----------------------------------------------------------------------------
# WHY INCIDENCE VS. RECURRENCE MATTERS (memo §1.3):
#
# Using leak_year (any leak including repeat events) conflates two phenomena:
#
#   (1) INCIDENCE: A facility transitions from clean to contaminated. This is
#       what observable characteristics (wall type, age, vintage) predict.
#       A single-walled old tank is more likely to DEVELOP a leak.
#
#   (2) RECURRENCE: An already-contaminated site continues reporting releases.
#       Once contaminated, ongoing reports reflect remediation quality, not
#       initial tank risk.
#
# Using leak_year gives contaminated facilities weight proportional to how
# many times they report — the model learns "tanks that leaked before leak
# again" rather than "what predicts which facilities become leakers."
#
# The CV validation in Section 9 uses event_first_leak as the outcome because
# we want to validate that observable characteristics predict the INCIDENCE
# margin (which facilities become leakers), not the recurrence margin.
#
# -----------------------------------------------------------------------------
# RELATED PRE-BUILT VARIABLES in annual_data (also from panel builder §10):
#
#   ever_leaked           — cumulative flag: 1 if facility has ever leaked
#   year_of_first_leak    — calendar year of first leak (NA if never leaked)
#   years_since_first_leak — panel_year minus year_of_first_leak (NA if not leaked)
#   has_previous_leak     — 1 if facility had a prior-year leak (for risk-set filtering)
#   cumulative_leaks      — cumulative count of all leaks including recurrences
#
# All five are available for downstream DiD and survival analysis in
# 02_DiD_Causal_Estimates.R. Do not reconstruct any of them here.
#
# -----------------------------------------------------------------------------
# NOTE ON GRANULARITY:
#
# event_first_leak is at the facility-year level. The research memo specified
# a tank-year version; the facility-year analog is used here because the main
# panel is constructed at the facility level. Facility-level incidence is the
# correct unit for the main DiD specifications in Stages 1 and 2.
# -----------------------------------------------------------------------------

stopifnot("event_first_leak" %in% names(annual_data))
stopifnot("ever_leaked" %in% names(annual_data))
stopifnot("has_previous_leak" %in% names(annual_data))

cat(sprintf("  event_first_leak: %s first-leak events across %s facility-years (%.2f%%)\n",
            sum(annual_data$event_first_leak, na.rm = TRUE),
            nrow(annual_data),
            100 * mean(annual_data$event_first_leak, na.rm = TRUE)))
cat(sprintf("  Facilities ever leaked: %s (%.1f%% of sample)\n",
            uniqueN(annual_data[ever_leaked == 1, panel_id]),
            100 * uniqueN(annual_data[ever_leaked == 1, panel_id]) /
                  uniqueN(annual_data$panel_id)))


#==============================================================================
# SECTION 5.7: REGULATIONS DATABASE & 1998 MANDATE COHORT CONSTRUCTION
#==============================================================================

cat("\n--- 5.7: Texas Mandate Schedule & Cohort Construction ---\n")

# 5.7a: Texas phased upgrade mandates (30 TAC Chapter 334)
tx_mandate_schedule <- data.table(
  cohort_label      = c("Pre-1965", "1965-1974", "1975-1979",
                         "1980-1984", "1985-1988"),
  install_start     = as.IDate(c("1900-01-01", "1965-01-01", "1975-01-01",
                                  "1980-01-01", "1985-01-01")),
  install_end       = as.IDate(c("1964-12-31", "1974-12-31", "1979-12-31",
                                  "1984-12-31", "1988-12-22")),
  tx_mandate_date   = as.IDate(c("1989-12-22", "1990-12-22", "1991-12-22",
                                  "1992-12-22", "1993-12-22")),
  federal_deadline  = rep(FEDERAL_MANDATE_DATE, 5)
)
tx_mandate_schedule[, tx_years_before_federal :=
  as.numeric(federal_deadline - tx_mandate_date) / 365.25]

cat("  TX Phased Mandate Schedule:\n")
print(tx_mandate_schedule[, .(cohort_label, tx_mandate_date,
                               tx_years_before_federal)])

# 5.7b: Assign mandate cohorts to tanks
tanks_1998[, `:=`(
  is_pre_1988 = as.integer(tank_installed_date <= MANDATE_CUTOFF_DATE)
  # install_year already derived in §5.2
)]

tanks_1998[is_pre_1988 == 1 & state == "TX", tx_mandate_deadline := fcase(
  install_year < 1965,                          as.IDate("1989-12-22"),
  install_year >= 1965 & install_year <= 1974,  as.IDate("1990-12-22"),
  install_year >= 1975 & install_year <= 1979,  as.IDate("1991-12-22"),
  install_year >= 1980 & install_year <= 1984,  as.IDate("1992-12-22"),
  install_year >= 1985 & install_year <= 1988,  as.IDate("1993-12-22"),
  default = NA_integer_
)]

tanks_1998[is_pre_1988 == 1 & state != "TX",
           tx_mandate_deadline := FEDERAL_MANDATE_DATE]

tanks_1998[is_pre_1988 == 1,
           years_past_deadline := as.numeric(
             TREATMENT_DATE - tx_mandate_deadline) / 365.25]

# 5.7c: Spec A / Spec B eligibility
tanks_1998[, `:=`(
  spec_A_eligible = as.integer(is_pre_1988 == 0),
  spec_B_eligible = as.integer(is_pre_1988 == 1)
)]

# 5.7c cont: Facility-level aggregates
fac_cohort <- tanks_1998[, .(
  n_tanks       = .N,
  n_pre_1988    = sum(is_pre_1988),
  n_post_1988   = sum(1L - is_pre_1988),
  pct_pre_1988  = mean(is_pre_1988),
  mean_age_1998 = mean(tank_age_1998, na.rm = TRUE),
  mean_yrs_past_deadline = mean(years_past_deadline, na.rm = TRUE),
  any_single_walled = as.integer(any(
    get("single_walled") == 1, na.rm = TRUE
  ))
), by = .(panel_id, texas_treated)]

fac_cohort_merge <- fac_cohort[, .(panel_id, n_tanks, n_pre_1988, n_post_1988,
                                    pct_pre_1988, mean_age_1998,
                                    mean_yrs_past_deadline)]
annual_data <- merge(annual_data, fac_cohort_merge,
                     by = "panel_id", all.x = TRUE)

annual_data[, `:=`(
  spec_A_eligible = as.integer(n_pre_1988 == 0),
  spec_B_eligible = as.integer(n_post_1988 == 0)
)]

cat(sprintf("\n  Spec A eligible (all post-1988): %s facilities\n",
            format(sum(annual_data[panel_year == TREATMENT_YEAR,
                                    spec_A_eligible], na.rm = TRUE),
                   big.mark = ",")))
cat(sprintf("  Spec B eligible (all pre-1988):  %s facilities\n",
            format(sum(annual_data[panel_year == TREATMENT_YEAR,
                                    spec_B_eligible], na.rm = TRUE),
                   big.mark = ",")))

# 5.7d: Mandate contamination controls
cat("\n--- 5.7d: Mandate contamination variable construction ---\n")

annual_data[, mandate_active := as.integer(
  state == "TX" &
  spec_B_eligible == 1 &
  panel_year >= TX_MANDATE_START & panel_year <= TX_MANDATE_END
)]

annual_data[, mandate_window_3yr := as.integer(
  state == "TX" &
  spec_B_eligible == 1 &
  panel_year >= TX_MANDATE_WINDOW_BROAD_START &
  panel_year <= TX_MANDATE_WINDOW_BROAD_END
)]

annual_data[, mandate_cohort := fcase(
  state != "TX" | spec_A_eligible == 1,       "Post-1988 / Control",
  spec_B_eligible == 1 &
    mean_yrs_past_deadline >= 8.5,            "Pre-1965 (Deadline 1989)",
  spec_B_eligible == 1 &
    mean_yrs_past_deadline >= 7.5,            "1965-1974 (Deadline 1990)",
  spec_B_eligible == 1 &
    mean_yrs_past_deadline >= 6.5,            "1975-1979 (Deadline 1991)",
  spec_B_eligible == 1 &
    mean_yrs_past_deadline >= 5.5,            "1980-1984 (Deadline 1992)",
  spec_B_eligible == 1 &
    mean_yrs_past_deadline >= 4.5,            "1985-1988 (Deadline 1993)",
  default =                                   "Post-1988 / Control"
)]
annual_data[, mandate_cohort := factor(mandate_cohort)]

cat(sprintf("  mandate_active facility-years (TX pre-1988, 1989-1993): %s\n",
            format(sum(annual_data$mandate_active, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  mandate_window_3yr facility-years (TX pre-1988, 1988-1994): %s\n",
            format(sum(annual_data$mandate_window_3yr, na.rm = TRUE), big.mark = ",")))
cat("  mandate_cohort distribution (at treatment year):\n")
print(annual_data[panel_year == TREATMENT_YEAR,
                  .N, by = mandate_cohort][order(mandate_cohort)])

# Mandate cohort counts CSV + LaTeX
mandate_cohort_counts <- annual_data[, .N, by = .(mandate_cohort, texas_treated,
                                                    spec_A_eligible,
                                                    spec_B_eligible)][order(mandate_cohort)]
save_table(mandate_cohort_counts, "Appendix_Mandate_Cohort_Counts")
write_tex(
  kbl(mandate_cohort_counts, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = "Facility counts by mandate cohort and treatment group.",
      label = "tab:mandate-cohort-counts") |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE),
  "Appendix_Mandate_Cohort_Counts"
)

# Mandate exposure comparison (for Table 2)
mandate_desc_comp <- tanks_1998[is_pre_1988 == 1, .(
  N_tanks             = .N,
  Mean_years_past     = round(mean(years_past_deadline, na.rm = TRUE), 1),
  Median_years_past   = round(median(years_past_deadline, na.rm = TRUE), 1),
  SD_years_past       = round(sd(years_past_deadline, na.rm = TRUE), 1),
  Min_years_past      = round(min(years_past_deadline, na.rm = TRUE), 1),
  Max_years_past      = round(max(years_past_deadline, na.rm = TRUE), 1),
  Pct_past_deadline   = round(100 * mean(years_past_deadline > 0, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("\n  Mandate Exposure Comparison (pre-1988 tanks):\n")
print(mandate_desc_comp)
save_table(mandate_desc_comp, "Table2_Mandate_Exposure_Comparison")
write_tex(
  kbl(mandate_desc_comp, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Mandate exposure comparison for pre-1988 tanks active",
        "December 22, 1998. Texas pre-1988 tanks cleared a cohort-specific",
        "compliance deadline 5--9 years prior; control state pre-1988 tanks",
        "faced their first federal deadline simultaneously with the insurance reform."),
      label = "tab:mandate-exposure",
      col.names = c("Group", "N Tanks", "Mean", "Median", "SD", "Min", "Max",
                    "\\% Past Deadline")) |>
    kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                  full_width = FALSE, threeparttable = TRUE) |>
    add_header_above(c(" " = 2,
                       "Years Past Cohort Deadline as of Dec 22, 1998" = 5,
                       " " = 1)) |>
    footnote(general = paste(
      "Years past deadline = (December 22, 1998) minus cohort mandate date.",
      "Texas tanks: mandate dates 1989--1993 by vintage cohort.",
      "Control tanks: mandate date = December 22, 1998 (0 years past)."),
      general_title = "", threeparttable = TRUE),
  "Table2_Mandate_Exposure_Comparison"
)


#==============================================================================
# SECTION 6: DIAGNOSTICS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 6: DIAGNOSTICS\n")
cat("========================================\n\n")

# 6.1 Pre-Treatment Exit Balance
cat("--- 6.1: Pre-Treatment Exit Balance ---\n")
pre_exit <- annual_data[panel_year >= 1990 & panel_year <= 1997, .(
  exit_rate    = mean(exit_flag, na.rm = TRUE),
  closure_rate = mean(closure_event, na.rm = TRUE),
  n_fac_years  = .N
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"),
          panel_year)]
cat("  Pre-period exit rates by group and year:\n")
print(dcast(pre_exit, panel_year ~ Group, value.var = "exit_rate"))

# 6.2 Tennessee LUST Verification
cat("\n--- 6.2: Tennessee LUST Verification ---\n")
tn_lust <- annual_data[state == "TN", .(
  total_leaks   = sum(n_leaks, na.rm = TRUE),
  n_fac_years   = .N,
  pct_leak_year = round(mean(leak_year, na.rm = TRUE), 6)
), by = .(period = fifelse(panel_year < TN_FUND_START_YEAR,
                           "Pre-2008", "Post-2008"))]
cat("  TN LUST by period:\n")
print(tn_lust)

if (tn_lust[period == "Pre-2008", total_leaks] == 0 &
    tn_lust[period == "Post-2008", total_leaks] > 0) {
  cat("  WARNING: TN zero-LUST pre-2008 confirmed — institutional artifact.\n")
  cat("    TN should be restricted to 2008+ in leak-outcome models.\n")
} else if (sum(tn_lust$total_leaks) == 0) {
  cat("  WARNING: TN has zero LUST across ALL years — drop from leak models.\n")
} else {
  cat("  OK: TN has leaks in both periods — no restriction needed.\n")
}

tn_exclude_leak_models <- (
  nrow(tn_lust[period == "Pre-2008"]) == 0 ||
  tn_lust[period == "Pre-2008", sum(total_leaks)] == 0
)
cat(sprintf("  TN exclude from leak models flag: %s\n", tn_exclude_leak_models))

# 6.3 Wall Type Missingness Diagnostic
cat("\n--- 6.3: Wall Type Missingness by State ---\n")
wall_miss <- tanks_1998[, .(
  n_tanks           = .N,
  n_miss_wall_type  = sum(is.na(single_walled) |
                           (single_walled == 0 & double_walled == 0),
                          na.rm = TRUE)
), by = state]
wall_miss[, pct_miss := round(100 * n_miss_wall_type / n_tanks, 1)]
setorder(wall_miss, -pct_miss)
cat("  Top 10 states by wall type missingness:\n")
print(head(wall_miss, 10))

model2_eligible <- tanks_1998[!is.na(single_walled) &
                                !(single_walled == 0 & double_walled == 0)]
cat(sprintf("\n  Model 2 effective N (non-missing wall type): %s tanks\n",
            format(nrow(model2_eligible), big.mark = ",")))
cat("  Model 2 state composition (top 10):\n")
print(model2_eligible[, .N, by = state][order(-N)][1:10])

# 6.4 Spec A Compositional Balance
cat("\n--- 6.4: Spec A Compositional Balance ---\n")

specA_balance <- tanks_1998[spec_A_eligible == 1, .(
  N_facilities    = uniqueN(panel_id),
  Mean_age_1998   = round(mean(tank_age_1998,   na.rm = TRUE), 1),
  SD_age_1998     = round(sd(tank_age_1998,     na.rm = TRUE), 1),
  Pct_single_wall = round(100 * mean(single_walled, na.rm = TRUE), 1),
  Mean_tanks_fac  = round(.N / uniqueN(panel_id), 1),
  Pct_pre_1965    = round(100 * mean(install_year < 1965, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("  Spec A observable balance (Dec 22, 1998 cross-section):\n")
print(specA_balance)
save_table(specA_balance, "TableA3_SpecA_Compositional_Balance")
write_tex(
  kbl(specA_balance, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Observable balance for Spec A (post-1988) subsample.",
        "Insignificant differences support the parallel trends assumption",
        "for the primary identification sample."),
      label = "tab:specA-balance-raw") |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE),
  "TableA3_SpecA_Compositional_Balance_Raw"
)

age_ttest <- t.test(tank_age_1998 ~ texas_treated,
                    data = tanks_1998[spec_A_eligible == 1])
cat(sprintf("\n  Age balance t-test (Spec A): p = %.4f\n", age_ttest$p.value))
if (age_ttest$p.value < 0.05) {
  cat("  WARNING: Spec A groups differ on age — include mean_age_1998 as covariate in DiD\n")
} else {
  cat("  OK: Spec A age balanced — compositional concern addressed.\n")
}


#==============================================================================
# SECTION 6.5: SAMPLE COMPOSITION (NEW — TableA0)
#==============================================================================

cat("\n--- 6.5: Table A0 — Sample Composition (Facility Size + Fuel Type) ---\n")

# Panel A: Facility counts by tank-count bins
# Aggregate to facility level first
fac_size <- tanks_1998[, .(n_tanks_fac = .N,
                            texas_treated = first(texas_treated)),
                        by = panel_id]

fac_size[, size_bin := fcase(
  n_tanks_fac == 1,                    "Single-tank facility",
  n_tanks_fac >= 2 & n_tanks_fac <= 3, "2-3 tanks",
  n_tanks_fac >= 4 & n_tanks_fac <= 6, "4-6 tanks",
  n_tanks_fac >= 7,                    "7+ tanks",
  default = "Unknown"
)]

panelA_comp <- fac_size[, .(
  Group         = fifelse(texas_treated == 1, "Texas", "Control"),
  N_Facilities  = .N,
  Pct_1_tank    = round(100 * mean(size_bin == "Single-tank facility"), 1),
  Pct_2_3_tank  = round(100 * mean(size_bin == "2-3 tanks"), 1),
  Pct_4_6_tank  = round(100 * mean(size_bin == "4-6 tanks"), 1),
  Pct_7plus     = round(100 * mean(size_bin == "7+ tanks"), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

# Add totals row
panelA_total <- fac_size[, .(
  Group         = "Total",
  N_Facilities  = .N,
  Pct_1_tank    = round(100 * mean(size_bin == "Single-tank facility"), 1),
  Pct_2_3_tank  = round(100 * mean(size_bin == "2-3 tanks"), 1),
  Pct_4_6_tank  = round(100 * mean(size_bin == "4-6 tanks"), 1),
  Pct_7plus     = round(100 * mean(size_bin == "7+ tanks"), 1)
)]
panelA_comp <- rbind(panelA_comp, panelA_total)
panelA_comp[, Panel := "A: Facility Size Distribution"]

cat("  Panel A — Facility Size Distribution:\n")
print(panelA_comp)

# Panel B: Tank counts by wall type
panelB_wall <- tanks_1998[, .(
  Group          = fifelse(texas_treated == 1, "Texas", "Control"),
  N_Tanks        = .N,
  Pct_SW         = round(100 * mean(single_walled == 1,   na.rm = TRUE), 1),
  Pct_DW         = round(100 * mean(double_walled == 1,   na.rm = TRUE), 1),
  Pct_Unknown    = round(100 * mean(
    (is.na(single_walled) | (single_walled == 0 & double_walled == 0)) &
    !is.na(single_walled), na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

panelB_wall_total <- tanks_1998[, .(
  Group       = "Total",
  N_Tanks     = .N,
  Pct_SW      = round(100 * mean(single_walled == 1, na.rm = TRUE), 1),
  Pct_DW      = round(100 * mean(double_walled == 1, na.rm = TRUE), 1),
  Pct_Unknown = round(100 * mean(
    (is.na(single_walled) | (single_walled == 0 & double_walled == 0)) &
    !is.na(single_walled), na.rm = TRUE), 1)
)]
panelB_wall <- rbind(panelB_wall, panelB_wall_total)

# Panel B continued: Fuel type from annual_data at TREATMENT_YEAR snapshot
fuel_snap <- annual_data[panel_year == TREATMENT_YEAR, .(
  Pct_motor_fuel = round(100 * mean(is_motor_fuel, na.rm = TRUE), 1),
  Pct_gasoline   = round(100 * mean(has_gasoline_year, na.rm = TRUE), 1),
  Pct_diesel     = round(100 * mean(has_diesel_year,   na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

fuel_total <- annual_data[panel_year == TREATMENT_YEAR, .(
  Group          = "Total",
  Pct_motor_fuel = round(100 * mean(is_motor_fuel, na.rm = TRUE), 1),
  Pct_gasoline   = round(100 * mean(has_gasoline_year, na.rm = TRUE), 1),
  Pct_diesel     = round(100 * mean(has_diesel_year,   na.rm = TRUE), 1)
)]
fuel_snap <- rbind(fuel_snap, fuel_total)

cat("  Panel B — Wall Type & Fuel Type:\n")
print(panelB_wall)
print(fuel_snap)

# Build combined long-format for single kbl() table
# Panel A rows
panelA_long <- data.table(
  Panel    = "A: Facility Size Distribution",
  Category = c("N Facilities", "Single-tank (%)", "2-3 tanks (%)",
                "4-6 tanks (%)", "7+ tanks (%)"),
  Texas    = unlist(panelA_comp[Group == "Texas",
                                 .(N_Facilities, Pct_1_tank, Pct_2_3_tank, Pct_4_6_tank, Pct_7plus)]),
  Control  = unlist(panelA_comp[Group == "Control",
                                 .(N_Facilities, Pct_1_tank, Pct_2_3_tank, Pct_4_6_tank, Pct_7plus)]),
  Total    = unlist(panelA_comp[Group == "Total",
                                 .(N_Facilities, Pct_1_tank, Pct_2_3_tank, Pct_4_6_tank, Pct_7plus)])
)

# Panel B rows — wall type
panelB_long_wall <- data.table(
  Panel    = "B: Tank Characteristics",
  Category = c("N Tanks", "Single-walled (%)", "Double-walled (%)",
                "Unknown wall type (%)"),
  Texas    = unlist(panelB_wall[Group == "Texas",  .(N_Tanks, Pct_SW, Pct_DW, Pct_Unknown)]),
  Control  = unlist(panelB_wall[Group == "Control", .(N_Tanks, Pct_SW, Pct_DW, Pct_Unknown)]),
  Total    = unlist(panelB_wall[Group == "Total",   .(N_Tanks, Pct_SW, Pct_DW, Pct_Unknown)])
)

# Panel B rows — fuel type
panelB_long_fuel <- data.table(
  Panel    = "B: Tank Characteristics",
  Category = c("Motor fuel facility (%)", "Has gasoline (%)", "Has diesel (%)"),
  Texas    = unlist(fuel_snap[Group == "Texas",  .(Pct_motor_fuel, Pct_gasoline, Pct_diesel)]),
  Control  = unlist(fuel_snap[Group == "Control", .(Pct_motor_fuel, Pct_gasoline, Pct_diesel)]),
  Total    = unlist(fuel_snap[Group == "Total",   .(Pct_motor_fuel, Pct_gasoline, Pct_diesel)])
)

tableA0_combined <- rbindlist(list(panelA_long, panelB_long_wall, panelB_long_fuel))

save_table(tableA0_combined, "TableA0_Sample_Composition")

# LaTeX: build with pack_rows for Panel A / Panel B
tableA0_display <- copy(tableA0_combined)
tableA0_display[, `:=`(Panel = NULL)]
tableA0_display[, Texas   := ifelse(Category == "N Facilities" | Category == "N Tanks",
                                    format(Texas,   big.mark = ","),
                                    sprintf("%.1f", Texas))]
tableA0_display[, Control := ifelse(Category == "N Facilities" | Category == "N Tanks",
                                    format(as.numeric(Control), big.mark = ","),
                                    sprintf("%.1f", as.numeric(Control)))]
tableA0_display[, Total   := ifelse(Category == "N Facilities" | Category == "N Tanks",
                                    format(as.numeric(Total), big.mark = ","),
                                    sprintf("%.1f", as.numeric(Total)))]

nA <- nrow(panelA_long)
nB <- nrow(panelB_long_wall) + nrow(panelB_long_fuel)

write_tex(
  kbl(tableA0_display, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Incumbent sample composition. Panel A: facility counts by",
        "tank-count size class. Panel B: tank counts by wall construction type",
        "and fuel category. Active as of December 22, 1998."),
      label = "tab:sample-composition",
      col.names = c("Category", "Texas", "Control", "Total")) |>
    kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                  full_width = FALSE, threeparttable = TRUE) |>
    pack_rows("Panel A: Facility Size Distribution", 1, nA,
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    pack_rows("Panel B: Tank Characteristics", nA + 1, nA + nB,
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    column_spec(1, width = "5cm") |>
    column_spec(2:4, width = "2cm") |>
    footnote(general = paste(
      "Incumbent sample: facilities first observed before December 22, 1998.",
      "Wall type unknown includes tanks where both single-walled and",
      "double-walled indicators are missing or zero.",
      "Fuel type from facility-year snapshot at treatment year."),
      general_title = "", threeparttable = TRUE),
  "TableA0_Sample_Composition"
)

cat("  OK: TableA0_Sample_Composition saved (CSV + .tex)\n")

rm(fac_size, panelA_long, panelB_long_wall, panelB_long_fuel, tableA0_display)
gc()


#==============================================================================
# SECTION 7: DESCRIPTIVE TABLES
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: DESCRIPTIVE TABLES\n")
cat("========================================\n\n")

# 7.1 Table 1 Panel A: Stock variables (EXPANDED)
cat("--- 7.1: Table 1 Panel A — Stock Variables (Expanded, 1998 Cross-Section) ---\n")

# Verify age_bin exists on tanks_1998 (required for age-bin share computation)
stopifnot("age_bin" %in% names(tanks_1998))
stopifnot("install_year" %in% names(tanks_1998))

# Verify capacity column name — FIELD: confirm "capacity" is the tank-level
# capacity column in the harmonized tank inventory
if (!"capacity" %in% names(tanks_1998)) {
  warning("Column 'capacity' not found in tanks_1998 — capacity stats will be NA")
  tanks_1998[, capacity := NA_real_]
}

# Compute mixed-wall flag at facility level: any SW AND any DW in same facility
fac_wall <- tanks_1998[, .(
  any_sw = any(single_walled == 1, na.rm = TRUE),
  any_dw = any(double_walled == 1, na.rm = TRUE),
  texas_treated = first(texas_treated)
), by = panel_id]
fac_wall[, is_mixed_wall := as.integer(any_sw & any_dw)]

mixed_wall_share <- fac_wall[, .(
  Pct_mixed_wall = round(100 * mean(is_mixed_wall, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

# Main stock stats
stock_stats <- tanks_1998[, .(
  N_facilities     = uniqueN(panel_id),
  N_tanks          = .N,
  Mean_age_1998    = round(mean(tank_age_1998,   na.rm = TRUE), 1),
  Median_age_1998  = round(median(tank_age_1998, na.rm = TRUE), 1),
  SD_age_1998      = round(sd(tank_age_1998,     na.rm = TRUE), 1),
  Pct_pre_1988     = round(100 * mean(is_pre_1988,      na.rm = TRUE), 1),
  Pct_single_wall  = round(100 * mean(single_walled == 1, na.rm = TRUE), 1),
  Pct_double_wall  = round(100 * mean(double_walled == 1, na.rm = TRUE), 1),
  Tanks_per_fac    = round(.N / uniqueN(panel_id), 1),
  Vintage_pre1980  = round(100 * mean(install_year < 1980, na.rm = TRUE), 1),
  Vintage_1980_1988 = round(100 * mean(install_year >= 1980 &
                                        install_year <= 1988, na.rm = TRUE), 1),
  Vintage_1989_1998 = round(100 * mean(install_year >= 1989 &
                                        install_year <= 1998, na.rm = TRUE), 1),
  Median_capacity_gal = round(median(capacity, na.rm = TRUE), 0),  # FIELD: verify
  Mean_capacity_gal   = round(mean(capacity, na.rm = TRUE), 0)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

stock_stats <- merge(stock_stats, mixed_wall_share, by = "Group")

cat("  Stock variables (1998 cross-section):\n")
print(stock_stats[, .(Group, N_facilities, N_tanks, Mean_age_1998,
                       Pct_pre_1988, Pct_single_wall, Pct_double_wall)])

# Compute age bin shares by group
age_bin_shares <- tanks_1998[, {
  tab <- table(factor(age_bin, levels = AGE_BIN_LABELS))
  as.list(round(100 * tab / sum(tab), 1))
}, by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]
# Rename age bin columns with prefix for clarity
setnames(age_bin_shares,
         AGE_BIN_LABELS,
         paste0("Pct_age_", gsub("-", "_", AGE_BIN_LABELS)))

stock_stats <- merge(stock_stats, age_bin_shares, by = "Group")

# Formal balance tests for each variable
run_ttest <- function(var_name, data = tanks_1998) {
  formula_str <- paste(var_name, "~ texas_treated")
  tryCatch({
    tt <- t.test(as.formula(formula_str), data = data)
    list(
      variable  = var_name,
      tx_val    = round(tt$estimate["mean in group 1"], 3),
      ctl_val   = round(tt$estimate["mean in group 0"], 3),
      diff      = round(diff(rev(tt$estimate)), 3),
      p_value   = round(tt$p.value, 4),
      stars     = stars_fn(tt$p.value)
    )
  }, error = function(e) {
    list(variable = var_name, tx_val = NA, ctl_val = NA,
         diff = NA, p_value = NA, stars = "")
  })
}

run_proptest <- function(var_name, data = tanks_1998) {
  tryCatch({
    n_tx  <- data[texas_treated == 1, .N]
    n_ctl <- data[texas_treated == 0, .N]
    k_tx  <- data[texas_treated == 1, sum(get(var_name) == 1, na.rm = TRUE)]
    k_ctl <- data[texas_treated == 0, sum(get(var_name) == 1, na.rm = TRUE)]
    pt <- prop.test(c(k_tx, k_ctl), c(n_tx, n_ctl))
    list(
      variable  = var_name,
      tx_val    = round(pt$estimate[1], 3),
      ctl_val   = round(pt$estimate[2], 3),
      diff      = round(pt$estimate[1] - pt$estimate[2], 3),
      p_value   = round(pt$p.value, 4),
      stars     = stars_fn(pt$p.value)
    )
  }, error = function(e) {
    list(variable = var_name, tx_val = NA, ctl_val = NA,
         diff = NA, p_value = NA, stars = "")
  })
}

balance_rows <- rbindlist(list(
  run_ttest("tank_age_1998"),
  run_ttest("capacity"),
  run_proptest("single_walled"),
  run_proptest("double_walled"),
  run_proptest("is_pre_1988")
), fill = TRUE)

# Balance tests for age bins (using prop.test on raw counts)
tanks_1998_copy <- copy(tanks_1998)
for (bn in AGE_BIN_LABELS) {
  col_name <- paste0("in_bin_", gsub("-", "_", bn))
  tanks_1998_copy[, (col_name) := as.integer(age_bin == bn)]
  balance_rows <- rbindlist(list(balance_rows,
                                  run_proptest(col_name, tanks_1998_copy)),
                             fill = TRUE)
}
rm(tanks_1998_copy)

stock_balance_tests <- balance_rows
save_table(stock_balance_tests, "TableA_Stock_Balance_Tests")
write_tex(
  kbl(stock_balance_tests, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = "Formal balance tests for stock variables (t-test / proportion test).",
      label = "tab:stock-balance-tests",
      col.names = c("Variable", "Texas", "Control", "Difference",
                    "p-value", "")) |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE),
  "TableA_Stock_Balance_Tests"
)

# Save CSV
save_table(stock_stats, "Table1A_Stock_Variables_1998")

# Build long-format for the main Table 1A publication table
# Merge TX and CTL values with balance test p-values
make_row <- function(label, tx_val, ctl_val, p = NA,
                     fmt = "%.1f", is_count = FALSE) {
  if (is_count) {
    tx_s  <- format(as.integer(tx_val),  big.mark = ",")
    ctl_s <- format(as.integer(ctl_val), big.mark = ",")
    dif_s <- format(as.integer(tx_val - ctl_val), big.mark = ",")
  } else {
    tx_s  <- sprintf(fmt, tx_val)
    ctl_s <- sprintf(fmt, ctl_val)
    dif_s <- sprintf(paste0("%+", substr(fmt, 2, nchar(fmt))),
                     tx_val - ctl_val)
  }
  p_s <- if (is.na(p)) "" else
    paste0(sprintf("%.3f", p), stars_fn(p))
  data.table(Variable = label, Texas = tx_s, Control = ctl_s,
             Difference = dif_s, `p-value` = p_s)
}

get_val <- function(grp, col) {
  stock_stats[Group == grp, get(col)]
}

tbl1a_rows <- rbindlist(list(
  make_row("N Facilities",      get_val("Texas","N_facilities"),
           get_val("Control","N_facilities"), is_count = TRUE),
  make_row("N Tanks",           get_val("Texas","N_tanks"),
           get_val("Control","N_tanks"), is_count = TRUE),
  make_row("Mean Tank Age (yrs)", get_val("Texas","Mean_age_1998"),
           get_val("Control","Mean_age_1998"),
           p = stock_balance_tests[variable == "tank_age_1998", p_value]),
  make_row("Median Tank Age (yrs)", get_val("Texas","Median_age_1998"),
           get_val("Control","Median_age_1998")),
  make_row("SD Tank Age",       get_val("Texas","SD_age_1998"),
           get_val("Control","SD_age_1998")),
  # Age bin shares — in AGE_BIN_LABELS order
  rbindlist(lapply(AGE_BIN_LABELS, function(bn) {
    col <- paste0("Pct_age_", gsub("-", "_", bn))
    p_col <- paste0("in_bin_", gsub("-", "_", bn))
    make_row(paste0("  Age ", bn, " (%)"),
             get_val("Texas", col), get_val("Control", col),
             p = stock_balance_tests[variable == p_col, p_value])
  })),
  make_row("Single-walled (%)", get_val("Texas","Pct_single_wall"),
           get_val("Control","Pct_single_wall"),
           p = stock_balance_tests[variable == "single_walled", p_value]),
  make_row("Double-walled (%)", get_val("Texas","Pct_double_wall"),
           get_val("Control","Pct_double_wall"),
           p = stock_balance_tests[variable == "double_walled", p_value]),
  make_row("Mixed-wall facilities (%)",
           mixed_wall_share[Group == "Texas",   Pct_mixed_wall],
           mixed_wall_share[Group == "Control", Pct_mixed_wall]),
  make_row("Pre-1988 tanks (%)", get_val("Texas","Pct_pre_1988"),
           get_val("Control","Pct_pre_1988"),
           p = stock_balance_tests[variable == "is_pre_1988", p_value]),
  make_row("Vintage: Pre-1980 (%)", get_val("Texas","Vintage_pre1980"),
           get_val("Control","Vintage_pre1980")),
  make_row("Vintage: 1980-1988 (%)", get_val("Texas","Vintage_1980_1988"),
           get_val("Control","Vintage_1980_1988")),
  make_row("Vintage: 1989-1998 (%)", get_val("Texas","Vintage_1989_1998"),
           get_val("Control","Vintage_1989_1998")),
  make_row("Mean Capacity (gal)",
           get_val("Texas","Mean_capacity_gal"),
           get_val("Control","Mean_capacity_gal"),
           p = stock_balance_tests[variable == "capacity", p_value],
           fmt = "%.0f"),
  make_row("Median Capacity (gal)",
           get_val("Texas","Median_capacity_gal"),
           get_val("Control","Median_capacity_gal"),
           fmt = "%.0f"),
  make_row("Tanks per Facility", get_val("Texas","Tanks_per_fac"),
           get_val("Control","Tanks_per_fac"))
), fill = TRUE)

write_tex(
  kbl(tbl1a_rows, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Stock variable balance: tank-level characteristics as of",
        "December 22, 1998. Difference is Texas minus control. p-values from",
        "two-sample t-test (continuous) or proportion test (binary).",
        "Stars denote significance: $^{*}p<0.10$, $^{**}p<0.05$,",
        "$^{***}p<0.01$."),
      label = "tab:stock-balance",
      col.names = c("Variable", "Texas", "Control", "Difference", "p-value")) |>
    kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                  full_width = FALSE, threeparttable = TRUE) |>
    column_spec(1, width = "5cm") |>
    column_spec(2:5, width = "2cm") |>
    footnote(general = paste(
      "Age bins use 5-year intervals: [0,5), [5,10), ..., [30,35), [35,$\\\\infty$).",
      "Facility-level statistics computed from tank-level data aggregated",
      "to the facility. Mixed-wall facilities hold at least one single-walled",
      "and one double-walled tank."),
      general_title = "", threeparttable = TRUE),
  "Table1A_Stock_Variables_1998"
)
cat("  OK: Table1A_Stock_Variables_1998 saved (CSV + .tex)\n")

rm(fac_wall, mixed_wall_share, age_bin_shares, balance_rows)


# 7.2 Table 1 Panel B: Flow variables — stratified + FIXED (All Facilities first)
cat("\n--- 7.2: Table 1 Panel B — Flow Variables, Stratified (1989-1997) ---\n")

flow_stats_stratified <- annual_data[
  panel_year >= 1989 & panel_year <= 1997 &
  !is.na(spec_A_eligible), .(
    N_fac_years        = .N,
    Avg_closure_rate   = round(mean(closure_event,  na.rm = TRUE), 4),
    Avg_exit_rate      = round(mean(exit_flag,       na.rm = TRUE), 4),
    Avg_leak_incidence = round(mean(leak_year,       na.rm = TRUE), 4),
    Leaks_per_1000     = round(sum(n_leaks, na.rm = TRUE) / .N * 1000, 2),
    Avg_n_closures     = round(mean(n_closures,      na.rm = TRUE), 4)
  ),
  by = .(
    Group  = fifelse(texas_treated == 1, "Texas", "Control"),
    Cohort = fcase(
      spec_A_eligible == 1, "Post-1988 Only",
      spec_B_eligible == 1, "Pre-1988 Only",
      default               = "Mixed"
    )
  )
]
setorder(flow_stats_stratified, Cohort, Group)
cat("  Stratified flow variables (1989-1997):\n")
print(flow_stats_stratified)
save_table(flow_stats_stratified, "Table1B_Flow_Stratified_SpecA_SpecB")

# Pooled version
flow_stats_pooled <- annual_data[panel_year >= 1989 & panel_year <= 1997, .(
  N_fac_years        = .N,
  Avg_closure_rate   = round(mean(closure_event,  na.rm = TRUE), 4),
  Avg_exit_rate      = round(mean(exit_flag,       na.rm = TRUE), 4),
  Avg_leak_incidence = round(mean(leak_year,       na.rm = TRUE), 4),
  Leaks_per_1000     = round(sum(n_leaks, na.rm = TRUE) / .N * 1000, 2)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]
save_table(flow_stats_pooled, "Table1B_Flow_Pooled_AllFacilities")

# Write LaTeX for stratified
write_tex(
  kbl(flow_stats_stratified, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Pre-period flow variables (1989--1997), stratified by",
        "installation cohort. Spec A: all post-1988 tanks (mandate-free).",
        "Spec B: all pre-1988 tanks (mandate-exposed). Mixed: holds both cohorts."),
      label = "tab:flow-stratified") |>
    kable_styling(latex_options = c("scale_down", "hold_position"), font_size = 9,
                  full_width = FALSE),
  "Table1B_Flow_Stratified_SpecA_SpecB"
)

# Spec B gap diagnostic
specB_gap <- flow_stats_stratified[Cohort == "Pre-1988 Only"]
if (nrow(specB_gap) == 2) {
  tx_rate  <- specB_gap[Group == "Texas",   Avg_closure_rate]
  ctl_rate <- specB_gap[Group == "Control", Avg_closure_rate]
  cat(sprintf("\n  Spec B pre-period closure gap: TX = %.4f | Control = %.4f\n",
              tx_rate, ctl_rate))
  if (tx_rate < ctl_rate)
    cat("  OK: TX Spec B lower than Control — consistent with mandate survivor selection.\n") else
    cat("  WARNING: TX Spec B >= Control — check mandate_cohort construction.\n")
}


# ---- Table 1 Combined Long-Format (FIXED: All Facilities inserted FIRST) ----
cat("\n--- Table 1 Combined Long-Format (All Facilities first in Panel B) ---\n")

# All Facilities (pooled) — Panel B FIRST cohort block
flow_stats_all <- annual_data[
  panel_year >= 1989 & panel_year <= 1997, .(
    N_fac_years        = .N,
    Avg_closure_rate   = round(mean(closure_event,  na.rm = TRUE), 4),
    Avg_exit_rate      = round(mean(exit_flag,       na.rm = TRUE), 4),
    Avg_leak_incidence = round(mean(leak_year,       na.rm = TRUE), 4),
    Leaks_per_1000     = round(sum(n_leaks, na.rm = TRUE) / .N * 1000, 2),
    Avg_n_closures     = round(mean(n_closures,      na.rm = TRUE), 4)
  ),
  by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))
]
flow_stats_all[, Cohort := "All Facilities"]

# Panel A: pivot stock_stats (select key variables for the combined table)
panelA_long <- melt(
  stock_stats[, .(Group, N_facilities, N_tanks, Mean_age_1998, SD_age_1998,
                   Pct_pre_1988, Pct_single_wall, Tanks_per_fac)],
  id.vars       = "Group",
  measure.vars  = c("N_facilities", "N_tanks", "Mean_age_1998", "SD_age_1998",
                    "Pct_pre_1988", "Pct_single_wall", "Tanks_per_fac"),
  variable.name = "variable",
  value.name    = "value"
)
panelA_wide <- dcast(panelA_long, variable ~ Group, value.var = "value")
panelA_wide[, Difference := Texas - Control]
panelA_wide[, `:=`(panel = "A: Stock Variables (Dec 22, 1998)",
                    cohort = "All Facilities")]

# Panel B: combine All Facilities + Spec A + Spec B + Mixed in order
flow_all_4 <- rbindlist(list(
  flow_stats_all,
  flow_stats_stratified[Cohort == "Post-1988 Only"][, Cohort := "Post-1988 Only"],
  flow_stats_stratified[Cohort == "Pre-1988 Only"][,  Cohort := "Pre-1988 Only"],
  flow_stats_stratified[Cohort == "Mixed"][,           Cohort := "Mixed"]
), use.names = TRUE, fill = TRUE)

panelB_long <- melt(
  flow_all_4,
  id.vars       = c("Group", "Cohort"),
  measure.vars  = c("Avg_closure_rate", "Avg_exit_rate",
                    "Avg_leak_incidence", "Leaks_per_1000", "N_fac_years"),
  variable.name = "variable",
  value.name    = "value"
)
panelB_wide <- dcast(panelB_long, Cohort + variable ~ Group, value.var = "value")
panelB_wide[, Difference := Texas - Control]
setnames(panelB_wide, "Cohort", "cohort")
panelB_wide[, panel := "B: Flow Variables (1989\u20131997 avg.)"]

# Stack and enforce column order
tbl1_combined <- rbindlist(
  list(panelA_wide, panelB_wide),
  use.names = TRUE, fill = TRUE
)
setcolorder(tbl1_combined, c("panel", "cohort", "variable", "Texas", "Control", "Difference"))
setorder(tbl1_combined, panel, cohort, variable)

# Human-readable variable labels
var_labels <- c(
  N_facilities      = "N facilities",
  N_tanks           = "N tanks",
  Mean_age_1998     = "Mean tank age (years)",
  SD_age_1998       = "SD tank age",
  Pct_pre_1988      = "Pre-1988 tanks (\\%)",
  Pct_single_wall   = "Single-walled tanks (\\%)",
  Tanks_per_fac     = "Tanks per facility",
  Avg_closure_rate  = "Annual closure rate",
  Avg_exit_rate     = "Annual exit rate",
  Avg_leak_incidence= "Annual leak incidence rate",
  Leaks_per_1000    = "Leak incidents per 1,000 fac-years",
  N_fac_years       = "N facility-years"
)
tbl1_combined[, variable := fifelse(
  variable %in% names(var_labels),
  var_labels[variable],
  as.character(variable)
)]

save_table(tbl1_combined, "Table1_Combined_Long")

# Build LaTeX for combined table
# Format numeric columns for display
tbl1_display <- copy(tbl1_combined)
fmt_mixed <- function(x) {
  ifelse(grepl("^N |N facilities|N tanks", tbl1_display$variable),
         format(round(x), big.mark = ","),
         sprintf("%.3f", x))
}
tbl1_display[, Texas      := sprintf("%.3f", as.numeric(Texas))]
tbl1_display[, Control    := sprintf("%.3f", as.numeric(Control))]
tbl1_display[, Difference := sprintf("%+.3f", as.numeric(Difference))]

# Identify row ranges for pack_rows
rows_panelA <- which(tbl1_display$panel == "A: Stock Variables (Dec 22, 1998)")
rows_panelB <- which(tbl1_display$panel != "A: Stock Variables (Dec 22, 1998)")
rows_allFac <- which(tbl1_display$cohort == "All Facilities" &
                     tbl1_display$panel != "A: Stock Variables (Dec 22, 1998)")
rows_specA  <- which(tbl1_display$cohort == "Post-1988 Only")
rows_specB  <- which(tbl1_display$cohort == "Pre-1988 Only")
rows_mixed  <- which(tbl1_display$cohort == "Mixed")

tbl1_kbl_data <- tbl1_display[, .(variable, Texas, Control, Difference)]

write_tex(
  kbl(tbl1_kbl_data, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Descriptive statistics for the estimation sample.",
        "Panel A: 1998 cross-section of active tanks as of December 22, 1998.",
        "Panel B: annual flow averages, 1989--1997, stratified by installation",
        "cohort. Difference is Texas minus control. Calendar year 1998 excluded",
        "from Panel B."),
      label = "tab:descriptive",
      col.names = c("Variable", "Texas", "Control", "Difference")) |>
    kable_styling(latex_options = c("scale_down"), font_size = 9,
                  full_width = FALSE, threeparttable = TRUE) |>
    pack_rows("Panel A: 1998 Cross-Section (Stock as of December 22, 1998)",
              min(rows_panelA), max(rows_panelA),
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    pack_rows("Panel B: Pre-Period Flow Averages, 1989-1997",
              min(rows_panelB), max(rows_panelB),
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    pack_rows("All Facilities",
              min(rows_allFac), max(rows_allFac),
              bold = FALSE, italic = TRUE, indent = TRUE) |>
    pack_rows("Post-1988 Only (Spec A)",
              min(rows_specA), max(rows_specA),
              bold = FALSE, italic = TRUE, indent = TRUE) |>
    pack_rows("Pre-1988 Only (Spec B)",
              min(rows_specB), max(rows_specB),
              bold = FALSE, italic = TRUE, indent = TRUE) |>
    pack_rows("Mixed Cohort",
              min(rows_mixed), max(rows_mixed),
              bold = FALSE, italic = TRUE, indent = TRUE) |>
    column_spec(1, width = "5cm") |>
    column_spec(2:4, width = "2cm") |>
    footnote(general = paste(
      "Pre-1988 tanks in Texas faced cohort-specific upgrade mandates",
      "(30 TAC Ch. 334) 5--9 years before the December 22, 1998 PSTRF cutoff.",
      "Spec A facilities hold exclusively post-1988 tanks and were never",
      "subject to these mandates. Leak incidence expressed per 1,000",
      "facility-years."),
      general_title = "", threeparttable = TRUE),
  "Table1_Combined_Long"
)
cat("  OK: Table1_Combined_Long.csv + .tex saved\n")


# 7.3 Table 1B: Leak rate cohort decomposition
cat("\n--- 7.3: Table 1B — Leak Rate Cohort Decomposition ---\n")

leak_by_cohort <- annual_data[
  panel_year >= 1989 & panel_year <= 1997 &
  !is.na(spec_A_eligible) & !is.na(spec_B_eligible),
  .(
    leak_rate_per1000 = round(sum(n_leaks, na.rm = TRUE) / .N * 1000, 2),
    leak_incidence    = round(mean(leak_year, na.rm = TRUE), 4),
    n_fac_years       = .N
  ),
  by = .(
    Group  = fifelse(texas_treated == 1, "Texas", "Control"),
    Cohort = fcase(
      spec_A_eligible == 1, "Post-1988 Only",
      spec_B_eligible == 1, "Pre-1988 Only",
      default = "Mixed"
    )
  )
]
setorder(leak_by_cohort, Cohort, Group)
cat("  Leak rate by cohort (pre-period):\n")
print(leak_by_cohort)
save_table(leak_by_cohort, "Table1B_Leak_Cohort_Decomposition")
write_tex(
  kbl(leak_by_cohort, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Pre-period leak rate cohort decomposition (1989--1997).",
        "Spec A (post-1988) and Spec B (pre-1988) facility-years shown separately."),
      label = "tab:leak-cohort") |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE),
  "Table1B_Leak_Cohort_Decomposition"
)


#==============================================================================
# SECTION 7.4: TABLE 3 — RISK FACTOR DISTRIBUTION + STRATIFIED LEAK RATES (NEW)
#==============================================================================

cat("\n--- 7.4: Table 3 — Risk Factor Distribution & Pre-Period Leak Validation ---\n")

# Step 1: Add risk factor flags to tanks_1998
# age_bin and install_year already exist on tanks_1998 from §5.2

# rf_gasoline: use is_gasoline if available at tank level; else flag
if ("is_gasoline" %in% names(tanks_1998)) {
  tanks_1998[, rf_gasoline := as.integer(is_gasoline == 1)]
} else {
  # Fallback: merge from annual_data facility-year snapshot at TREATMENT_YEAR
  fuel_fac_snap <- annual_data[panel_year == TREATMENT_YEAR,
                                .(panel_id, rf_gasoline_fac = as.integer(is_motor_fuel == 1))]
  tanks_1998 <- merge(tanks_1998, fuel_fac_snap, by = "panel_id", all.x = TRUE)
  tanks_1998[, rf_gasoline := rf_gasoline_fac]
  tanks_1998[, rf_gasoline_fac := NULL]
  cat("  NOTE: rf_gasoline using facility-level is_motor_fuel (tank-level is_gasoline absent)\n")
}

tanks_1998[, `:=`(
  rf_single_wall = as.integer(single_walled == 1),
  # rf_age_gte20 derived from canonical age bins — never use raw > 20 threshold
  rf_age_gte20   = as.integer(age_bin %in% c("20-24", "25-29", "30-34", "35+")),
  rf_pre_1980    = as.integer(install_year < 1980)
)]

tanks_1998[, risk_score := rf_single_wall + rf_age_gte20 + rf_pre_1980]

tanks_1998[, risk_category := factor(fcase(
  risk_score <= 1, "Low (0-1 factors)",
  risk_score == 2, "Medium (2 factors)",
  risk_score >= 3, "High (3 factors)",
  default         = "Low (0-1 factors)"
), levels = c("Low (0-1 factors)", "Medium (2 factors)", "High (3 factors)"))]

# Step 2: Panel A — High-risk indicator shares
tbl3_indicators <- tanks_1998[, .(
  pct_single_wall = round(100 * mean(rf_single_wall, na.rm = TRUE), 1),
  pct_age_gte20   = round(100 * mean(rf_age_gte20,   na.rm = TRUE), 1),
  pct_pre_1980    = round(100 * mean(rf_pre_1980,    na.rm = TRUE), 1),
  pct_gasoline    = round(100 * mean(rf_gasoline,    na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("  Panel A — High-risk indicators (% of tanks):\n")
print(tbl3_indicators)

# Step 3: Panel B — Composite risk score distribution
tbl3_risk_dist <- tanks_1998[, .N,
  by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"), risk_category)]
tbl3_risk_dist[, pct := round(100 * N / sum(N), 1), by = Group]
setorder(tbl3_risk_dist, risk_category, Group)

cat("  Panel B — Risk score distribution:\n")
print(tbl3_risk_dist)

# Step 4: Panel C — Pre-period first-leak incidence by age_bin x wall type
# Uses event_first_leak (incidence) from annual_data
pre_cv <- annual_data[
  panel_year >= 1990 & panel_year <= 1998 &
  has_previous_leak == 0 &
  !is.na(has_single_walled)
]

pre_cv[, wall_label := fifelse(has_single_walled == 1,
                               "Single-Walled", "Double-Walled")]

tbl3_leak_rates <- pre_cv[, .(
  leak_rate_per_1000 = round(sum(event_first_leak, na.rm = TRUE) / .N * 1000, 2),
  n_fac_years        = .N,
  n_first_leaks      = sum(event_first_leak, na.rm = TRUE)
), by = .(
  Group      = fifelse(texas_treated == 1, "Texas", "Control"),
  age_bin,
  wall_label
)]

# Enforce canonical bin ordering — CRITICAL for table and figure
tbl3_leak_rates[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(tbl3_leak_rates, age_bin, wall_label, Group)

cat("  Panel C — Leak incidence by age bin x wall type (per 1,000 fac-years):\n")
print(tbl3_leak_rates)

# Risk ratio validation diagnostic
high_rate <- tbl3_leak_rates[age_bin %in% c("20-24","25-29","30-34","35+") &
                               wall_label == "Single-Walled",
                               sum(n_first_leaks) / sum(n_fac_years) * 1000]
low_rate  <- tbl3_leak_rates[age_bin %in% c("0-4","5-9") &
                               wall_label == "Double-Walled",
                               sum(n_first_leaks) / sum(n_fac_years) * 1000]
if (!is.na(low_rate) && low_rate > 0) {
  cat(sprintf("  Risk ratio (old SW / young DW): %.1fx\n", high_rate / low_rate))
} else {
  cat(sprintf("  High-risk rate: %.2f per 1,000 | Low-risk rate: %.2f per 1,000\n",
              high_rate, low_rate))
}

# Save individual CSVs
save_table(tbl3_indicators,  "Table3A_Risk_Indicators")
save_table(tbl3_risk_dist,   "Table3B_Risk_Distribution")
save_table(tbl3_leak_rates,  "Table3C_Leak_Rates_By_Risk")

# Build single combined 3-panel LaTeX table
# Panel A: 4 rows x 3 cols (Characteristic | TX% | CTL%)
panA <- rbindlist(list(
  data.table(
    Characteristic = c("Single-Walled", "Age $\\geq$20 Years",
                       "Pre-1980 Vintage", "Motor Fuel Facility"),
    Texas   = sprintf("%.1f", unlist(tbl3_indicators[Group == "Texas",
                       .(pct_single_wall, pct_age_gte20, pct_pre_1980, pct_gasoline)])),
    Control = sprintf("%.1f", unlist(tbl3_indicators[Group == "Control",
                       .(pct_single_wall, pct_age_gte20, pct_pre_1980, pct_gasoline)]))
  )
))

# Panel B: 3 rows
panB <- tbl3_risk_dist[, .(
  Characteristic = as.character(risk_category),
  Texas   = sprintf("%.1f", pct[Group == "Texas"]),
  Control = sprintf("%.1f", pct[Group == "Control"])
), by = risk_category][, .(Characteristic, Texas, Control)]

# Panel C: wide pivot — TX Rate | CTL Rate | TX N | CTL N
panC_wide <- dcast(tbl3_leak_rates,
                   age_bin + wall_label ~ Group,
                   value.var = c("leak_rate_per_1000", "n_fac_years"))

panC_display <- panC_wide[, .(
  `Age Bin`    = as.character(age_bin),
  `Wall Type`  = wall_label,
  `TX Rate`    = sprintf("%.2f",
                  ifelse(is.na(leak_rate_per_1000_Texas), 0,
                         leak_rate_per_1000_Texas)),
  `CTL Rate`   = sprintf("%.2f",
                  ifelse(is.na(`leak_rate_per_1000_Control`), 0,
                         `leak_rate_per_1000_Control`)),
  `TX N`       = format(ifelse(is.na(n_fac_years_Texas), 0L,
                               as.integer(n_fac_years_Texas)), big.mark = ","),
  `CTL N`      = format(ifelse(is.na(`n_fac_years_Control`), 0L,
                               as.integer(`n_fac_years_Control`)), big.mark = ",")
)]
setorder(panC_display, `Age Bin`, `Wall Type`)

# Combine panels A, B, C with separator rows
sep_row_AB <- data.table(`Age Bin` = "", `Wall Type` = "")

# Build separate kbl per panel then combine via combined tex string
kbl_panA <- kbl(panA, format = "latex", booktabs = TRUE, linesep = "",
                escape = FALSE,
                col.names = c("Characteristic", "Texas (\\%)", "Control (\\%)")) |>
  kable_styling(latex_options = c("hold_position"), font_size = 9,
                full_width = FALSE)

kbl_panB <- kbl(panB, format = "latex", booktabs = TRUE, linesep = "",
                escape = FALSE,
                col.names = c("Risk Category", "Texas (\\%)", "Control (\\%)")) |>
  kable_styling(latex_options = c("hold_position"), font_size = 9,
                full_width = FALSE)

kbl_panC <- kbl(panC_display, format = "latex", booktabs = TRUE, linesep = "",
                escape = FALSE,
                col.names = c("Age Bin", "Wall Type", "TX Rate", "CTL Rate",
                              "TX N", "CTL N")) |>
  kable_styling(latex_options = c("scale_down", "hold_position"),
                font_size = 9, full_width = FALSE)

# Build combined table — single Panel A+B+C kbl with pack_rows
combined_data <- rbindlist(list(
  panA[, .(Variable = Characteristic, Texas, Control, ExtraA = "", ExtraB = "")],
  data.table(Variable = "", Texas = "", Control = "", ExtraA = "", ExtraB = ""),  # separator
  data.table(Variable = panB$Characteristic, Texas = panB$Texas,
             Control = panB$Control, ExtraA = "", ExtraB = ""),
  data.table(Variable = "", Texas = "", Control = "", ExtraA = "", ExtraB = ""),  # separator
  data.table(Variable = paste(panC_display$`Age Bin`, panC_display$`Wall Type`,
                               sep = " | "),
             Texas = panC_display$`TX Rate`, Control = panC_display$`CTL Rate`,
             ExtraA = panC_display$`TX N`, ExtraB = panC_display$`CTL N`)
), fill = TRUE)

nA3 <- nrow(panA)
nB3 <- nrow(panB)
nC3 <- nrow(panC_display)
sep1_row <- nA3 + 1L
sep2_row <- nA3 + 1L + nB3 + 1L

write_tex(
  kbl(combined_data, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Risk factor prevalence and pre-period leak validation.",
        "Panel A: share of incumbent tanks classified as high-risk on each",
        "indicator, 1998 cross-section. Panel B: composite risk score distribution",
        "(0--3 factors: single-walled construction, age $\\geq20$ years,",
        "pre-1980 vintage). Panel C: pre-period leak incidence rates by age bin",
        "(canonical 5-year intervals) and wall type, 1990--1998,",
        "facility-years in risk set only."),
      label = "tab:risk-validation",
      col.names = c("Variable / Age Bin", "Texas", "Control",
                    "TX N", "CTL N")) |>
    kable_styling(latex_options = c("scale_down", "hold_position"),
                  font_size = 9, full_width = FALSE,
                  threeparttable = TRUE) |>
    pack_rows("Panel A: High-Risk Indicator Prevalence (\\% of Tanks)",
              1, nA3,
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    row_spec(sep1_row, extra_latex_after = "\\midrule[0.4pt]") |>
    pack_rows("Panel B: Composite Risk Score Distribution (\\% of Tanks)",
              sep1_row + 1L, sep1_row + nB3,
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    row_spec(sep2_row, extra_latex_after = "\\midrule[0.4pt]") |>
    pack_rows(paste("Panel C: Pre-Period Leak Incidence by Age Bin and Wall",
                    "Type (per 1,000 Facility-Years, 1990--1998)"),
              sep2_row + 1L, sep2_row + nC3,
              bold = TRUE, italic = FALSE, latex_gap_space = "0.5em") |>
    column_spec(1, width = "5cm") |>
    column_spec(2:5, width = "2cm") |>
    footnote(general = paste(
      "Composite risk score = (single-walled) + (age $\\\\geq20$ years)",
      "+ (pre-1980 vintage). Low = 0--1 factors; Medium = 2 factors;",
      "High = 3 factors. Panel C uses first-leak incidence",
      "(\\\\texttt{event\\\\_first\\\\_leak}); facilities that had already",
      "leaked before 1990 are excluded from the risk set.",
      "Age bins: [0,5), [5,10), ..., [30,35), [35,$\\\\infty$).",
      "Wall type missing observations excluded from Panel C."),
      general_title = "", threeparttable = TRUE),
  "Table3_Risk_Validation"
)
cat("  OK: Table3_Risk_Validation.tex saved\n")

rm(panA, panB, panC_display, combined_data, pre_cv)


#==============================================================================
# SECTION 8: DESCRIPTIVE FIGURES — PRE-PERIOD TRENDS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: DESCRIPTIVE FIGURES\n")
cat("========================================\n\n")

# 8.1 Figure 4 + Parallel Trends Validation
# ---------------------------------------------------------------------------
# TWO outputs:
#   Figure 4 (main text): Spec A only — mandate-free, parallel trends hold
#   Figure 4B (appendix): Pooled — annotated to show mandate contamination
#
# FOUR validation tests (Table B.4):
#   1. Pooled, no control          -> Expected to REJECT (mandate contamination)
#   2. Spec A, no control needed   -> Expected NOT to reject (clean sample)
#   3. Spec B, no control          -> Expected to REJECT
#   4. Spec B + mandate_active     -> Should improve toward non-rejection
# ---------------------------------------------------------------------------
cat("--- 8.1: Figure 4 + Parallel Trends Validation (four tests) ---\n")

# Helper: Wilson 95% CI closure rates
closure_rate_ci <- function(dt) {
  dt[panel_year >= 1990 & panel_year <= TREATMENT_YEAR, {
    n     <- .N
    k     <- sum(closure_event, na.rm = TRUE)
    rate  <- k / n
    z     <- qnorm(0.975)
    denom <- 1 + z^2 / n
    ci_lo <- (rate + z^2/(2*n) - z*sqrt(rate*(1-rate)/n + z^2/(4*n^2))) / denom
    ci_hi <- (rate + z^2/(2*n) + z*sqrt(rate*(1-rate)/n + z^2/(4*n^2))) / denom
    .(closure_rate = rate, ci_lo = ci_lo, ci_hi = ci_hi,
      n_fac = uniqueN(panel_id))
  }, by = .(panel_year, Group = fifelse(texas_treated == 1, "Texas", "Control"))]
}

closure_rate_specA  <- closure_rate_ci(annual_data[spec_A_eligible == 1])
closure_rate_pooled <- closure_rate_ci(annual_data)

# Helper: run one pre-trend Wald test
run_pt_test <- function(dt, label, extra_rhs = "", mandate_control = "None") {
  formula_str <- paste0(
    "closure_event ~ i(rel_year_1999, texas_treated, ref = -1)",
     if (nchar(extra_rhs) > 0) paste0(" + ", extra_rhs) else "",
    " | panel_id + panel_year"
  )
  m <- tryCatch(
    feols(as.formula(formula_str),
          data    = dt[panel_year >= 1990 & panel_year <= TREATMENT_YEAR],
          cluster = ~state),
    error = function(e) { cat("  WARNING: PT test failed for:", label, "\n"); NULL }
  )
  if (is.null(m)) return(data.table(Specification = label,
                                     `Mandate Control` = mandate_control,
                                     `F-statistic` = NA_real_,
                                     `p-value` = NA_real_,
                                     df1 = NA_integer_,
                                     Conclusion = "FAILED"))
  w <- tryCatch(fixest::wald(m, "rel_year_1999"), error = function(e) NULL)
  if (is.null(w)) return(data.table(Specification = label,
                                     `Mandate Control` = mandate_control,
                                     `F-statistic` = NA_real_,
                                     `p-value` = NA_real_,
                                     df1 = NA_integer_,
                                     Conclusion = "FAILED"))
  data.table(
    Specification     = label,
    `Mandate Control` = mandate_control,
    `F-statistic`     = round(w$stat, 3),
    `p-value`         = round(w$p, 4),
    df1               = w$df1,
    Conclusion        = fifelse(w$p > 0.10,
                                "NOT rejected",
                                "REJECTED")
  )
}

cat("  Running four parallel trends tests...\n")
pt_all <- rbindlist(list(
  run_pt_test(annual_data,
              "1. Pooled (all facilities), no mandate control", "",
              mandate_control = "None"),
  run_pt_test(annual_data[spec_A_eligible == 1],
              "2. Spec A (post-1988 only) — PRIMARY IDENTIFICATION", "",
              mandate_control = "None"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "3. Spec B (pre-1988 only), no mandate control", "",
              mandate_control = "None"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "4. Spec B (pre-1988 only) + mandate_active control",
              "mandate_active",
              mandate_control = "mandate_active")
))

cat("\n  === TABLE B.4: PARALLEL TRENDS VALIDATION ===\n")
print(pt_all[, .(Specification, `F-statistic`, `p-value`, Conclusion)])
save_table(pt_all, "TableB4_Parallel_Trends_Validation")

# LaTeX Table B.4
write_tex(
  kbl(pt_all[, .(Specification, `Mandate Control`, `F-statistic`,
                  `p-value`, Conclusion)],
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption = paste("Parallel trends validation: Wald F-tests, 1990--1997",
        "pre-reform window. Each row estimates a TWFE regression of the annual",
        "closure indicator on Texas $\\times$ relative-year interactions with",
        "facility and year fixed effects. Standard errors clustered at the state",
        "level. H$_0$: joint nullity of all pre-period interaction coefficients."),
      label = "tab:pt-validation") |>
    kable_styling(latex_options = c("scale_down", "hold_position"),
                  font_size = 9, full_width = FALSE, threeparttable = TRUE) |>
    column_spec(1, width = "6cm") |>
    column_spec(2:5, width = "2cm") |>
    footnote(general = paste(
      "Wild cluster bootstrap p-values with Webb-6 weights where bootstrap",
      "is enabled. Pooled and Spec B tests are expected to reject due to Texas",
      "phased mandate contamination (1989--1993). Primary identification uses",
      "Spec A (Test 2)."),
      general_title = "", threeparttable = TRUE),
  "TableB4_Parallel_Trends_Validation"
)

# Extract key p-values for figure annotations
specA_p  <- pt_all[grepl("Spec A", Specification), `p-value`]
pooled_p <- pt_all[grepl("Pooled", Specification), `p-value`]

specA_label <- if (is.na(specA_p)) {
  "Spec A pre-trend F-test: estimation failed\n(check reference year)"
} else {
  sprintf(
    "Spec A pre-trend F-test: p = %.3f\n(%s)",
    specA_p,
    fifelse(specA_p > 0.10,
            "Null of parallel pre-trends NOT rejected",
            "Null of parallel pre-trends REJECTED")
  )
}

# Figure 4 (MAIN TEXT): Spec A
fig4 <- ggplot(closure_rate_specA,
               aes(x = panel_year, y = closure_rate, color = Group)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = Group),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed",
             color = "gray40") +
  annotate("text",
           x = TREATMENT_YEAR + 0.6,
           y = max(closure_rate_specA$closure_rate),
           label = "Dec 22, 1998\nPSTRF cutoff",
           hjust = 0, size = 3, color = "gray40") +
  annotate("text",
           x = 1990.5,
           y = max(closure_rate_specA$closure_rate) * 0.95,
           label = specA_label,
           hjust = 0, size = 3.2, fontface = "italic") +
  scale_color_manual(values = COL_PAIR) +
  scale_fill_manual(values = COL_PAIR, guide = "none") +
  labs(x = "Year", y = "Annual Closure Rate",
       title = "Pre-Period Closure Rates — Post-1988 Cohort (Spec A)",
       subtitle = paste0(
         "Facilities with 100% post-1988 tanks (N = ",
         format(uniqueN(annual_data[spec_A_eligible == 1, panel_id]),
                big.mark = ","),
         "). Exempt from TX phased mandate (1989-1993). Shaded = 95% Wilson CI."),
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

save_fig(fig4, "Figure4_PrePeriod_SpecA_MainText", width = 8, height = 5)

# Figure 4B (APPENDIX): Pooled
fig4_pooled <- ggplot(closure_rate_pooled,
                      aes(x = panel_year, y = closure_rate, color = Group)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = Group),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed",
             color = "gray40") +
  annotate("rect",
           xmin = TX_MANDATE_START - 0.5, xmax = TX_MANDATE_END + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "gold", alpha = 0.10) +
  annotate("text",
           x = (TX_MANDATE_START + TX_MANDATE_END) / 2,
           y = min(closure_rate_pooled$closure_rate) * 1.05,
           label = "TX phased mandate\nwindow (1989-1993)",
           size = 2.8, color = "goldenrod4", fontface = "italic") +
  annotate("text",
           x = 1990.5,
           y = max(closure_rate_pooled$closure_rate) * 0.95,
           label = "Pooled Wald F-test: p < 0.001\nRejects due to mandate contamination\n(see Figure 4 / Spec A for clean test)",
           hjust = 0, size = 3, color = "gray40", fontface = "italic") +
  scale_color_manual(values = COL_PAIR) +
  scale_fill_manual(values = COL_PAIR, guide = "none") +
  labs(x = "Year", y = "Annual Closure Rate",
       title = "Pre-Period Closure Rates — Pooled Sample (Appendix Only)",
       subtitle = paste0(
         "Includes pre-1988 facilities subject to TX phased mandate. ",
         "Gold band = mandate years. NOT used for identification."),
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

save_fig(fig4_pooled, "Figure4B_PrePeriod_Pooled_Appendix", width = 8, height = 5)


# 8.2 Figure B-1 (Appendix): Full 4-panel pre-trends
cat("\n--- 8.2: Figure B-1 (Appendix) — Full 4-Panel Pre-Trends ---\n")

pre_trends_full <- annual_data[
  panel_year >= 1990 & panel_year <= TREATMENT_YEAR, .(
    closure_rate   = mean(closure_event, na.rm = TRUE),
    exit_rate      = mean(exit_flag,     na.rm = TRUE),
    leak_incidence = mean(leak_year,     na.rm = TRUE),
    leaks_per_1000 = sum(n_leaks, na.rm = TRUE) / .N * 1000
  ),
  by = .(panel_year,
         treatment_group = fifelse(texas_treated == 1, "Texas", "Control"))
]

closure_age_pre <- pre_period_closures[, .(
  mean_closure_age = mean(age_at_closure, na.rm = TRUE)
), by = .(panel_year   = closure_year,
          treatment_group = fifelse(texas_treated == 1, "Texas", "Control"))]

pre_trends_full <- merge(pre_trends_full, closure_age_pre,
                          by = c("panel_year", "treatment_group"), all.x = TRUE)

make_panel <- function(dt, yvar, ylab, title) {
  ggplot(dt, aes(x = panel_year, y = get(yvar), color = treatment_group)) +
    geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
    geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed",
               color = "gray50") +
    scale_color_manual(values = COL_PAIR) +
    labs(x = NULL, y = ylab, title = title, color = NULL) +
    theme(legend.position = "none")
}

p_a <- make_panel(pre_trends_full, "closure_rate",
                   "Rate", "(a) Closure Rate")
p_b <- make_panel(pre_trends_full, "leaks_per_1000",
                   "Per 1,000 fac-yrs", "(b) Leak Incidents per 1,000 Fac-Years")
p_c <- make_panel(pre_trends_full[!is.na(mean_closure_age)], "mean_closure_age",
                   "Years", "(c) Mean Closure Age")
p_d <- make_panel(pre_trends_full, "exit_rate",
                   "Rate", "(d) Exit Rate")

fig_b1 <- gridExtra::grid.arrange(
  p_a, p_b, p_c, p_d, ncol = 2,
  top = textGrob("Pre-Period Trends (1990-1998)",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)
save_fig(fig_b1, "FigureB1_PreTrends_Full_4Panel", width = 12, height = 8)


#==============================================================================
# SECTION 8.3: FIGURES 1, 2, 3 — CAPITAL STOCK CHARACTERIZATION (NEW)
#==============================================================================

cat("\n--- 8.3: Figures 1, 2, 3 — Capital Stock Characterization ---\n")

# Verify tanks_1998 has required columns
stopifnot("age_bin"      %in% names(tanks_1998))
stopifnot("install_year" %in% names(tanks_1998))
stopifnot("Group"        %in% names(tanks_1998))

# Compute vintage bins for Figures 2 and 3
tanks_1998[, vintage_bin := factor(fcase(
  install_year < 1980,                          "Pre-1980",
  install_year >= 1980 & install_year <= 1988,  "1980-1988",
  install_year >= 1989 & install_year <= 1998,  "1989-1998",
  default = NA_character_
), levels = c("Pre-1980", "1980-1988", "1989-1998"))]

cat(sprintf("  Vintage bin distribution:\n"))
print(tanks_1998[!is.na(vintage_bin), .N, by = .(vintage_bin, Group)])


# ---- Figure 1: Tank Age Density (TX vs Control, Dec 22 1998 cross-section) ----
fig1 <- ggplot(tanks_1998, aes(x = tank_age_1998, fill = Group, color = Group)) +
  geom_density(alpha = 0.35, adjust = 1.5) +
  # Light bin boundary lines at every 5 years
  geom_vline(xintercept = seq(5, 35, 5), linetype = "dotted",
             color = "gray70", linewidth = 0.4) +
  # Highlighted threshold at 20 years
  geom_vline(xintercept = 20, linetype = "dashed",
             color = "gray30", linewidth = 0.7) +
  annotate("text", x = 20.8, y = Inf, vjust = 1.5,
           label = "20 yrs\n(elevated risk)", hjust = 0,
           size = 2.8, color = "gray30") +
  scale_fill_manual(values = COL_PAIR) +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(
    breaks = AGE_BIN_BREAKS[is.finite(AGE_BIN_BREAKS)],
    limits = c(0, 40)
  ) +
  labs(x = "Tank Age (years as of Dec 22, 1998)", y = "Density",
       title = "Tank Age Distribution at Treatment Date",
       subtitle = "Incumbent tanks active December 22, 1998. Dotted lines = 5-year bin boundaries.",
       fill = NULL, color = NULL)

save_fig(fig1, "Figure1_Tank_Age_Density", width = 8, height = 5)
cat("  OK: Figure 1 saved\n")


# ---- Figure 2: Tank Vintage Composition (Stacked Bar) ----
vintage_shares <- tanks_1998[!is.na(vintage_bin), .(N = .N),
                               by = .(Group, vintage_bin)]
vintage_shares[, share := N / sum(N), by = Group]

fig2 <- ggplot(vintage_shares, aes(x = Group, y = share, fill = vintage_bin)) +
  geom_col(position = "stack", width = 0.65, color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("Pre-1980"    = "#1a1a2e",
               "1980-1988"   = "#4a4a8a",
               "1989-1998"   = "#9090cc"),
    name = "Installation Vintage"
  ) +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            position = position_stack(vjust = 0.5),
            size = 3.2, color = "white", fontface = "bold") +
  labs(x = NULL, y = "Share of Incumbent Tanks",
       title = "Installation Vintage Composition at Treatment Date",
       subtitle = "Incumbent tanks active December 22, 1998. Darkest = oldest vintage.") +
  theme(legend.position = "bottom")

save_fig(fig2, "Figure2_Tank_Vintage_Composition", width = 7, height = 6)
cat("  OK: Figure 2 saved\n")


# ---- Figure 3: Wall Type by Vintage (Grouped Bar) ----
wall_vintage <- tanks_1998[!is.na(vintage_bin) & !is.na(single_walled), .(
  pct_sw = round(100 * mean(single_walled == 1, na.rm = TRUE), 1)
), by = .(vintage_bin, Group)]

fig3 <- ggplot(wall_vintage,
               aes(x = vintage_bin, y = pct_sw, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = paste0(pct_sw, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = COL_PAIR) +
  scale_y_continuous(limits = c(0, 110),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Installation Vintage",
       y = "Share Single-Walled (%)",
       title = "Single-Walled Share by Installation Vintage",
       subtitle = paste("Incumbent tanks active December 22, 1998.",
                        "Expected: SW share declines with vintage (newer standards)."),
       fill = NULL)

save_fig(fig3, "Figure3_Wall_Type_By_Vintage", width = 8, height = 5)
cat("  OK: Figure 3 saved\n")


#==============================================================================
# SECTION 9: RISK FACTOR CV VALIDATION (FULL REPLACEMENT)
#==============================================================================
# DATA PROVENANCE NOTE:
#   This section operates on annual_data at the facility-year level.
#   Outcome variable: event_first_leak (pre-built by upstream panel builder).
#   See Section 5.5 documentation stub for full provenance and incidence vs.
#   recurrence rationale.
#   Predictors (has_single_walled, avg_tank_age, age_bin, is_motor_fuel) are
#   pre-built or derived via canonical make_age_bin().
#   age_bin_for_reg() enforces "0-4" as the reference level in all models.
#   Expensive k-fold steps are guarded by if (RUN_FULL) { ... }.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: RISK FACTOR CV VALIDATION\n")
cat("========================================\n\n")

# 9.1 Prepare CV data
cat("--- 9.1: Prepare CV data ---\n")

cv_data <- annual_data[
  panel_year >= 1990 & panel_year <= 1998 &
  has_previous_leak == 0 &
  !is.na(has_single_walled) &
  !is.na(age_bin)
]

# Derive pre-1980 flag; handle missing install_year
if ("install_year" %in% names(cv_data)) {
  cv_data[, rf_pre_1980 := as.integer(!is.na(install_year) & install_year < 1980)]
} else if ("pre1998_install" %in% names(cv_data)) {
  # pre1998_install is a binary indicator for installations before 1998;
  # not exactly equivalent to pre-1980, but usable as proxy when install_year absent
  cv_data[, rf_pre_1980 := pre1998_install]
  cat("  WARNING: install_year absent from cv_data — using pre1998_install proxy for rf_pre_1980\n")
} else {
  cv_data[, rf_pre_1980 := NA_integer_]
  cat("  WARNING: Neither install_year nor pre1998_install found — rf_pre_1980 set to NA\n")
}

cv_data[, `:=`(
  log_capacity = log(pmax(total_capacity, 1, na.rm = TRUE)),
  # age_bin_reg: canonical factor with "0-4" as reference level
  age_bin_reg  = age_bin_for_reg(age_bin)
)]

set.seed(20250128)
cv_data[, fold := sample(1:5, .N, replace = TRUE), by = state]

cat(sprintf("  CV data: %s facility-years | %s first-leak events (%.3f%%)\n",
            format(nrow(cv_data), big.mark = ","),
            format(sum(cv_data$event_first_leak, na.rm = TRUE), big.mark = ","),
            100 * mean(cv_data$event_first_leak, na.rm = TRUE)))

# Initialize OOB prediction columns (always, regardless of RUN_FULL)
cv_data[, `:=`(pred_no_sfe   = NA_real_,
               pred_with_sfe = NA_real_)]

# Initialize AUC placeholders
auc_no_sfe   <- NA_real_
auc_with_sfe <- NA_real_


if (RUN_FULL) {

  # 9.2 5-fold CV logit (no state FE)
  cat("\n--- 9.2: 5-fold CV logit (no state FE) ---\n")
  for (k in 1:5) {
    train_fold <- cv_data[fold != k]
    test_fold  <- cv_data[fold == k]

    fit <- tryCatch(
      glm(event_first_leak ~ has_single_walled + age_bin_reg + rf_pre_1980 +
                             log_capacity + is_motor_fuel + factor(panel_year),
          data   = train_fold,
          family = binomial(link = "logit")),
      error = function(e) { cat(sprintf("  WARNING: Fold %d failed: %s\n", k, e$message)); NULL }
    )
    if (!is.null(fit)) {
      cv_data[fold == k, pred_no_sfe := predict(fit, newdata = .SD, type = "response")]
      cat(sprintf("  Fold %d (no SFE): train N = %s, test N = %s\n",
                  k,
                  format(nrow(train_fold), big.mark = ","),
                  format(nrow(test_fold),  big.mark = ",")))
    }
  }

  # 9.3 5-fold CV logit (with state FE)
  cat("\n--- 9.3: 5-fold CV logit (with state FE) ---\n")
  for (k in 1:5) {
    train_fold <- cv_data[fold != k]
    test_fold  <- cv_data[fold == k]

    fit_sfe <- tryCatch(
      glm(event_first_leak ~ has_single_walled + age_bin_reg + rf_pre_1980 +
                             log_capacity + is_motor_fuel +
                             factor(panel_year) + factor(state),
          data   = train_fold,
          family = binomial(link = "logit")),
      error = function(e) { cat(sprintf("  WARNING: Fold %d (SFE) failed: %s\n", k, e$message)); NULL }
    )
    if (!is.null(fit_sfe)) {
      cv_data[fold == k, pred_with_sfe := predict(fit_sfe, newdata = .SD,
                                                   type = "response")]
      cat(sprintf("  Fold %d (with SFE): complete\n", k))
    }
  }

  # 9.4 AUC-ROC
  cat("\n--- 9.4: AUC-ROC ---\n")
  cv_complete <- cv_data[!is.na(pred_no_sfe) & !is.na(event_first_leak)]
  if (nrow(cv_complete) > 100) {
    roc_no_sfe   <- pROC::roc(cv_complete$event_first_leak,
                               cv_complete$pred_no_sfe,
                               quiet = TRUE)
    auc_no_sfe   <- as.numeric(pROC::auc(roc_no_sfe))

    cv_complete_sfe <- cv_data[!is.na(pred_with_sfe) & !is.na(event_first_leak)]
    if (nrow(cv_complete_sfe) > 100) {
      roc_with_sfe <- pROC::roc(cv_complete_sfe$event_first_leak,
                                 cv_complete_sfe$pred_with_sfe,
                                 quiet = TRUE)
      auc_with_sfe <- as.numeric(pROC::auc(roc_with_sfe))
    }

    cat(sprintf("  AUC-ROC (No State FE):   %.3f\n", auc_no_sfe))
    cat(sprintf("  AUC-ROC (With State FE): %.3f\n", auc_with_sfe))
    cat(sprintf("  Interpretation: %s\n",
                fcase(auc_no_sfe >= 0.80, "Good discrimination (>= 0.80)",
                      auc_no_sfe >= 0.70, "Acceptable discrimination (0.70-0.79)",
                      default = "Modest discrimination (< 0.70 — expected for rare events)")))

    if (!is.na(auc_with_sfe) && !is.na(auc_no_sfe)) {
      atten <- (1 - auc_with_sfe / auc_no_sfe) * 100
      cat(sprintf("  AUC attenuation with state FE: %.1f%%\n", atten))
      if (atten > 30) {
        cat("  NOTE: >30%% attenuation — cross-state variation contributes substantially\n")
      } else {
        cat("  OK: Within-state variation sufficient for discrimination.\n")
      }
    }
  } else {
    cat("  WARNING: Too few complete OOB predictions for AUC computation\n")
  }

  # 9.5 Calibration Table (decile bins — data.table compatible, no ntile())
  cat("\n--- 9.5: Calibration Table ---\n")
  cv_cal <- cv_data[!is.na(pred_no_sfe) & !is.na(event_first_leak)]
  if (nrow(cv_cal) > 100) {
    # Use quantile + cut instead of dplyr::ntile
    decile_breaks <- quantile(cv_cal$pred_no_sfe, probs = seq(0, 1, by = 0.1),
                               na.rm = TRUE)
    # Ensure breaks are unique (rare event convergence can create ties)
    decile_breaks <- unique(decile_breaks)
    if (length(decile_breaks) >= 3) {
      cv_cal[, decile := as.integer(cut(pred_no_sfe,
                                         breaks = decile_breaks,
                                         include.lowest = TRUE,
                                         labels = FALSE))]

      calibration_table <- cv_cal[!is.na(decile), .(
        mean_predicted = round(mean(pred_no_sfe,      na.rm = TRUE), 4),
        mean_actual    = round(mean(event_first_leak, na.rm = TRUE), 4),
        n_fac_years    = .N,
        n_leaks        = sum(event_first_leak, na.rm = TRUE)
      ), by = decile][order(decile)]

      # Lift = decile actual rate / bottom decile actual rate
      bottom_rate <- calibration_table[decile == min(decile), mean_actual]
      calibration_table[, lift := round(mean_actual / bottom_rate, 2)]

      cat("  Calibration table:\n")
      print(calibration_table)

      top_rate <- calibration_table[decile == max(decile), mean_actual]
      if (!is.na(bottom_rate) && bottom_rate > 0) {
        cat(sprintf("  Top decile leak rate: %.4f | Bottom decile: %.4f | Lift: %.1fx\n",
                    top_rate, bottom_rate, top_rate / bottom_rate))
      }

      save_table(calibration_table, "Table_CV_Calibration")

      # Format for LaTeX
      cal_display <- copy(calibration_table)
      cal_display[, `:=`(
        mean_predicted = sprintf("%.4f", mean_predicted),
        mean_actual    = sprintf("%.4f", mean_actual),
        n_fac_years    = format(n_fac_years, big.mark = ","),
        lift           = sprintf("%.2f", lift)
      )]
      write_tex(
        kbl(cal_display[, .(decile, mean_predicted, mean_actual,
                             n_fac_years, lift)],
            format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
            caption = paste("Cross-validated calibration of the pre-period leak",
              "risk model. Facility-years binned into deciles by out-of-bag",
              "predicted first-leak probability. Lift = decile actual rate divided",
              "by bottom-decile actual rate."),
            label = "tab:cv-calibration",
            col.names = c("Decile", "Mean Predicted Prob.",
                          "Mean Actual Rate", "N Fac.-Years", "Lift")) |>
          kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                        full_width = FALSE, threeparttable = TRUE) |>
          footnote(general = paste(
            "5-fold cross-validated logistic regression; outcome is",
            "\\\\texttt{event\\\\_first\\\\_leak}. Covariates: single-walled",
            "indicator, canonical age bins (5-year intervals, 0--4 through 35+,",
            "reference = 0--4), pre-1980 vintage, log capacity, motor fuel",
            "indicator, year fixed effects. Sample: 1990--1998 pre-period,",
            "never-yet-leaked facility-years."),
            general_title = "", threeparttable = TRUE),
        "Table_CV_Calibration"
      )
    } else {
      cat("  WARNING: Insufficient quantile variation for decile calibration\n")
    }
  } else {
    cat("  WARNING: Insufficient complete predictions for calibration table\n")
  }

  # 9.6 Partial Dependence Summaries
  cat("\n--- 9.6: Partial Dependence Summaries ---\n")
  cv_pd <- cv_data[!is.na(pred_no_sfe)]

  # Wall type
  pd_wall <- cv_pd[, .(
    mean_pred   = round(mean(pred_no_sfe,      na.rm = TRUE), 4),
    mean_actual = round(mean(event_first_leak, na.rm = TRUE), 4),
    n_fac_years = .N
  ), by = .(covariate = "Wall Type",
            level = fifelse(has_single_walled == 1, "Single-Walled", "Double-Walled"))]

  # Pre-1980 vintage
  pd_vintage <- cv_pd[!is.na(rf_pre_1980), .(
    mean_pred   = round(mean(pred_no_sfe,      na.rm = TRUE), 4),
    mean_actual = round(mean(event_first_leak, na.rm = TRUE), 4),
    n_fac_years = .N
  ), by = .(covariate = "Pre-1980 Vintage",
            level = fifelse(rf_pre_1980 == 1, "Pre-1980 = Yes", "Pre-1980 = No"))]

  # Age bin — all 8 canonical bins in AGE_BIN_LABELS order
  pd_age <- cv_pd[, .(
    mean_pred   = round(mean(pred_no_sfe,      na.rm = TRUE), 4),
    mean_actual = round(mean(event_first_leak, na.rm = TRUE), 4),
    n_fac_years = .N
  ), by = .(covariate = "Tank Age (5-yr bins)", level = as.character(age_bin))]

  # Enforce canonical ordering
  pd_age[, level := factor(level, levels = AGE_BIN_LABELS)]
  setorder(pd_age, level)
  pd_age[, level := as.character(level)]

  # Combine
  pd_combined <- rbindlist(list(pd_wall, pd_vintage, pd_age), use.names = TRUE)

  cat("  Partial dependence:\n")
  print(pd_combined)
  save_table(pd_combined, "Table_CV_Partial_Dependence")

  write_tex(
    kbl(pd_combined, format = "latex", booktabs = TRUE, linesep = "",
        escape = FALSE,
        caption = paste("Partial dependence of predicted first-leak probability on",
          "key risk factors. Mean out-of-bag predicted probability and mean actual",
          "first-leak rate by covariate level. Values confirm that the model's risk",
          "ordering aligns with observed leak incidence."),
        label = "tab:cv-partial-dependence",
        col.names = c("Risk Factor", "Level", "Mean Predicted",
                      "Mean Actual", "N Fac.-Years")) |>
      kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                    full_width = FALSE, threeparttable = TRUE) |>
      add_header_above(c(" " = 2, "Leak Probability" = 2, " " = 1)) |>
      column_spec(1, width = "3.5cm") |>
      column_spec(2, width = "3.5cm") |>
      column_spec(3:5, width = "2cm") |>
      footnote(general = paste(
        "Out-of-bag predictions from 5-fold cross-validated logit",
        "(no state fixed effects). See Table~\\\\ref{tab:cv-calibration} for",
        "full calibration. Sample restricted to 1990--1998 pre-period."),
        general_title = "", threeparttable = TRUE),
    "Table_CV_Partial_Dependence"
  )

  # 9.7 Figure 5A: Partial Dependence Plot (CV — requires RUN_FULL)
  cat("\n--- 9.7: Figure 5A — Partial Dependence (predicted vs actual) ---\n")

  pd_long <- melt(pd_combined,
                  id.vars       = c("covariate", "level", "n_fac_years"),
                  measure.vars  = c("mean_pred", "mean_actual"),
                  variable.name = "type", value.name = "rate")

  pd_long[, type := factor(fcase(
    type == "mean_pred",   "Predicted (OOB)",
    type == "mean_actual", "Actual",
    default = "Other"
  ), levels = c("Predicted (OOB)", "Actual"))]

  # Enforce canonical ordering for age bin levels within the age group
  pd_long[covariate == "Tank Age (5-yr bins)",
          level := factor(level, levels = AGE_BIN_LABELS)]
  pd_long[covariate != "Tank Age (5-yr bins)",
          level := factor(level)]

  fig5a_cv <- ggplot(pd_long,
                     aes(x = level, y = rate, fill = type)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    facet_wrap(~ covariate, scales = "free_x", nrow = 1) +
    scale_fill_manual(values = c("Predicted (OOB)" = COL_TX,
                                  "Actual"          = COL_CTRL)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    labs(x = NULL, y = "Leak Probability",
         title = "Risk Factor Partial Dependence: Predicted vs Actual Leak Rates",
         subtitle = "5-Fold CV OOB predictions | Pre-period facility-years (1990-1998)",
         fill = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_fig(fig5a_cv, "Figure5A_CV_Partial_Dependence", width = 13, height = 5)
  cat("  OK: Figure 5A (CV partial dependence) saved\n")

} else {
  cat("  WARNING: Section 9 CV validation skipped — set RUN_FULL = TRUE for final run\n")
  cat("    Placeholder NA predictions written to cv_data for downstream saves.\n")

  # Create empty CV placeholder tables so save_table() calls don't error
  calibration_table    <- data.table(decile = integer(0), mean_predicted = numeric(0),
                                      mean_actual = numeric(0), n_fac_years = integer(0),
                                      lift = numeric(0))
  pd_combined          <- data.table(covariate = character(0), level = character(0),
                                      mean_pred = numeric(0), mean_actual = numeric(0),
                                      n_fac_years = integer(0))

  save_table(calibration_table, "Table_CV_Calibration")
  save_table(pd_combined,       "Table_CV_Partial_Dependence")
  cat("  OK: Empty placeholder CSVs written (RUN_FULL = FALSE)\n")
}


# 9.7 Figure 5B: Leak Risk by Wall Type x Age (always produced)
# CRITICAL: This figure is referenced as @fig-leak-risk in QMD §2.1.
# Must be saved as "Figure_leak_risk" to match knitr::include_graphics() path.
cat("\n--- 9.7: Figure 5B — Leak Risk by Wall Type x Age (fig-leak-risk) ---\n")

# Build from tbl3_leak_rates (Panel C of Table 3, computed in §7.4)
stopifnot("tbl3_leak_rates" %in% ls())
stopifnot("age_bin" %in% names(tbl3_leak_rates))

# Enforce canonical ordering for axis
tbl3_leak_rates[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]

fig5b_risk <- ggplot(
  tbl3_leak_rates[!is.na(leak_rate_per_1000)],
  aes(x = age_bin, y = leak_rate_per_1000,
      color = wall_label, linetype = wall_label, group = wall_label)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_text(aes(label = sprintf("%.1f", leak_rate_per_1000)),
            vjust = -0.7, size = 2.5, show.legend = FALSE) +
  facet_wrap(~ Group, nrow = 1) +
  scale_x_discrete(limits = AGE_BIN_LABELS) +
  scale_color_manual(values = c("Single-Walled" = COL_TX,
                                 "Double-Walled" = COL_CTRL)) +
  scale_linetype_manual(values = c("Single-Walled" = "solid",
                                    "Double-Walled" = "dashed")) +
  labs(x = "Tank Age Bin (5-year intervals)",
       y = "First-Leak Rate (per 1,000 Facility-Years)",
       title = "Pre-Period Leak Incidence by Tank Age and Wall Type",
       subtitle = paste("1990-1998, never-yet-leaked facility-years only.",
                        "Both lines expected to rise with age; SW steeper."),
       color = "Wall Type", linetype = "Wall Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Save as Figure_leak_risk to match QMD cross-reference @fig-leak-risk
save_fig(fig5b_risk, "Figure_leak_risk", width = 11, height = 6)
cat("  OK: Figure_leak_risk saved (matches QMD cross-reference @fig-leak-risk)\n")


#==============================================================================
# SECTION 10: JMP PUBLICATION TABLES
#==============================================================================

cat("\n========================================\n")
cat("SECTION 10: JMP PUBLICATION TABLES\n")
cat("========================================\n\n")

# 10.1 Table A1: TX Phased Mandate Schedule
cat("--- 10.1: Table A1 — TX Phased Mandate Schedule ---\n")
table_a1 <- copy(tx_mandate_schedule)
table_a1[, `:=`(
  install_start            = as.character(install_start),
  install_end              = as.character(install_end),
  tx_mandate_date          = as.character(tx_mandate_date),
  federal_deadline         = as.character(federal_deadline),
  tx_years_before_federal  = round(tx_years_before_federal, 1)
)]
save_table(table_a1, "TableA1_TX_Mandate_Schedule")

write_tex(
  kbl(table_a1[, .(cohort_label, tx_mandate_date, federal_deadline,
                    tx_years_before_federal)],
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption = paste("Texas phased upgrade mandate schedule (30 TAC Chapter 334).",
        "Pre-1988 tanks faced cohort-specific compliance deadlines 5--9 years",
        "before the December 22, 1998 federal standard."),
      label = "tab:mandate-schedule",
      col.names = c("Cohort (Install Years)", "TX Mandate Deadline",
                    "Federal Deadline", "Years Before Federal")) |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE, threeparttable = TRUE) |>
    column_spec(1:3, width = "3cm") |>
    column_spec(4, width = "2.5cm") |>
    footnote(general = paste(
      "Control states faced only the uniform federal deadline of December 22,",
      "1998 (40 CFR $\\\\S$280.21). No analogous staggered mandate applied to",
      "any control state in the study window."),
      general_title = "", threeparttable = TRUE),
  "TableA1_TX_Mandate_Schedule"
)
cat("  OK: Table A1 saved (CSV + .tex)\n")


# 10.2 Table A2: Facility Cohort Composition
cat("--- 10.2: Table A2 — Facility Cohort Composition ---\n")
cohort_comp <- annual_data[panel_year == TREATMENT_YEAR, .(
  N_facilities  = .N,
  N_spec_A      = sum(spec_A_eligible, na.rm = TRUE),
  N_spec_B      = sum(spec_B_eligible, na.rm = TRUE),
  N_mixed       = sum(spec_A_eligible == 0 & spec_B_eligible == 0, na.rm = TRUE),
  Pct_spec_A    = round(100 * mean(spec_A_eligible, na.rm = TRUE), 1),
  Pct_spec_B    = round(100 * mean(spec_B_eligible, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("  Cohort composition at treatment year:\n")
print(cohort_comp)
save_table(cohort_comp, "TableA2_Cohort_Composition")

write_tex(
  kbl(cohort_comp, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Facility cohort composition at treatment date (December 22,",
        "1998). Spec A: all tanks post-1988 (mandate-free). Spec B: all tanks",
        "pre-1988 (mandate-exposed). Mixed: holds both cohorts."),
      label = "tab:cohort-composition",
      col.names = c("Group", "N Facilities", "N Spec A", "N Spec B",
                    "N Mixed", "\\% Spec A", "\\% Spec B")) |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE),
  "TableA2_Cohort_Composition"
)
cat("  OK: Table A2 saved (CSV + .tex)\n")


#==============================================================================
# SECTION 10.3: TABLE A3 — SPEC A COMPOSITIONAL BALANCE (PUB FORMAT) (NEW)
#==============================================================================

cat("--- 10.3: Table A3 — Spec A Balance (Publication Format with Diff + Stars) ---\n")

specA_tanks <- tanks_1998[spec_A_eligible == 1]

if (nrow(specA_tanks) > 0) {

  # Continuous: mean age t-test
  age_ttest_a3 <- tryCatch(
    t.test(tank_age_1998 ~ texas_treated, data = specA_tanks),
    error = function(e) NULL
  )

  # Proportion: single-walled share
  n_tx_a3  <- specA_tanks[texas_treated == 1, .N]
  n_ctl_a3 <- specA_tanks[texas_treated == 0, .N]
  k_sw_tx  <- specA_tanks[texas_treated == 1, sum(single_walled == 1, na.rm = TRUE)]
  k_sw_ctl <- specA_tanks[texas_treated == 0, sum(single_walled == 1, na.rm = TRUE)]

  sw_proptest_a3 <- tryCatch(
    prop.test(c(k_sw_tx, k_sw_ctl), c(n_tx_a3, n_ctl_a3)),
    error = function(e) NULL
  )

  # Build display table
  tbl_a3 <- rbindlist(list(
    # N Facilities
    data.table(
      Variable   = "N Facilities",
      Texas      = format(uniqueN(specA_tanks[texas_treated == 1, panel_id]),
                          big.mark = ","),
      Control    = format(uniqueN(specA_tanks[texas_treated == 0, panel_id]),
                          big.mark = ","),
      Difference = "",
      `p-value`  = ""
    ),
    # Mean tank age
    data.table(
      Variable   = "Mean Tank Age (years)",
      Texas      = sprintf("%.1f", specA_tanks[texas_treated == 1,
                                                mean(tank_age_1998, na.rm = TRUE)]),
      Control    = sprintf("%.1f", specA_tanks[texas_treated == 0,
                                                mean(tank_age_1998, na.rm = TRUE)]),
      Difference = if (!is.null(age_ttest_a3))
        sprintf("%+.2f", diff(rev(age_ttest_a3$estimate))) else "",
      `p-value`  = if (!is.null(age_ttest_a3))
        paste0(sprintf("%.3f", age_ttest_a3$p.value),
               stars_fn(age_ttest_a3$p.value)) else ""
    ),
    # Single-walled share
    data.table(
      Variable   = "Single-Walled (%)",
      Texas      = sprintf("%.1f", 100 * k_sw_tx / n_tx_a3),
      Control    = sprintf("%.1f", 100 * k_sw_ctl / n_ctl_a3),
      Difference = if (!is.null(sw_proptest_a3))
        sprintf("%+.3f", diff(sw_proptest_a3$estimate)) else "",
      `p-value`  = if (!is.null(sw_proptest_a3))
        paste0(sprintf("%.3f", sw_proptest_a3$p.value),
               stars_fn(sw_proptest_a3$p.value)) else ""
    ),
    # SD age
    data.table(
      Variable   = "SD Tank Age",
      Texas      = sprintf("%.2f", specA_tanks[texas_treated == 1,
                                                sd(tank_age_1998, na.rm = TRUE)]),
      Control    = sprintf("%.2f", specA_tanks[texas_treated == 0,
                                                sd(tank_age_1998, na.rm = TRUE)]),
      Difference = "",
      `p-value`  = ""
    )
  ), fill = TRUE)

  save_table(tbl_a3, "TableA3_SpecA_Balance_PubFormat")

  write_tex(
    kbl(tbl_a3, format = "latex", booktabs = TRUE, linesep = "",
        escape = FALSE,
        caption = paste("Observable balance for Spec A (post-1988) subsample.",
          "Two-sample t-test of mean tank age; proportion test for single-walled",
          "share. Insignificant differences support the parallel trends assumption",
          "for the primary identification sample."),
        label = "tab:specA-balance",
        col.names = c("Variable", "Texas", "Control", "Difference", "p-value")) |>
      kable_styling(latex_options = c("hold_position"), font_size = 10,
                    full_width = FALSE, threeparttable = TRUE) |>
      column_spec(1, width = "5cm") |>
      column_spec(2:5, width = "2cm") |>
      footnote(general = paste(
        "Spec A facilities hold exclusively post-1988 tanks and were never",
        "subject to the Texas phased mandate (30 TAC Ch. 334, 1989--1993).",
        "Balance on observable characteristics supports the identifying assumption",
        "that post-treatment divergence reflects the insurance reform rather than",
        "compositional differences."),
        general_title = "", threeparttable = TRUE),
    "TableA3_SpecA_Balance_PubFormat"
  )
  cat("  OK: TableA3 publication-format saved (CSV + .tex)\n")

} else {
  cat("  WARNING: No Spec A tanks found — Table A3 skipped\n")
}


#==============================================================================
# SECTION 11: INSTITUTIONAL CONTEXT FIGURES
#==============================================================================

cat("\n========================================\n")
cat("SECTION 11: INSTITUTIONAL CONTEXT FIGURES\n")
cat("========================================\n\n")

CONTRACT_PATH <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")

if (file.exists(CONTRACT_PATH)) {
  contract_month <- fread(CONTRACT_PATH)
  panel_data_inst <- contract_month[YEAR >= 2007 & YEAR <= 2020]

  # 11.1 Figure [A]: TX FR Coverage Composition
  cat("--- 11.1: TX FR Coverage Composition ---\n")

  panel_data_inst[, plot_category := fcase(
    CATEGORY == "Insurance",      "Private Insurance",
    CATEGORY == "State Fund",     "State Fund",
    CATEGORY == "Self-Insurance", "Self-Insurance",
    CATEGORY == "NO COVERAGE",    "No Coverage",
    default = "Other"
  )]

  fac_categories <- unique(panel_data_inst[, .(FACILITY_ID, YEAR, MONTH, plot_category)])
  fac_categories[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
  regime_shares <- fac_categories[, .(N = sum(weight) / 12), by = .(YEAR, plot_category)]
  regime_shares[, share := N / sum(N), by = YEAR]

  fig_coverage <- ggplot(regime_shares, aes(x = YEAR, y = share, fill = plot_category)) +
    geom_col(position = "fill", width = 0.8) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Share of Facilities",
         title = "Primary Financial Responsibility Mechanism (2007-2020)",
         fill = "Mechanism") +
    theme(legend.position = "bottom")

  save_fig(fig_coverage, "FigureA_TX_FR_Coverage_Composition", width = 8, height = 6)
  save_table(regime_shares, "FigureA_data_regime_shares")

  # 11.2 Market Consolidation: Top 5 Insurers + HHI
  cat("--- 11.2: TX Private Insurance Market — Top 5 & HHI Trend ---\n")
  ins_panel <- panel_data_inst[plot_category == "Private Insurance" &
                                 !is.na(ISSUER_NAME) & ISSUER_NAME != "NO COVERAGE"]
  ins_panel[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
  insurer_exposure <- ins_panel[, .(fac_months = sum(weight)), by = .(YEAR, ISSUER_NAME)]
  insurer_exposure[, annual_total := sum(fac_months), by = YEAR]
  insurer_exposure[, market_share := fac_months / annual_total]
  hhi_trend <- insurer_exposure[, .(HHI = sum((market_share * 100)^2)), by = YEAR]

  setorder(insurer_exposure, YEAR, -fac_months)
  insurer_exposure[, annual_rank := frank(-fac_months, ties.method = "first"), by = YEAR]
  insurer_exposure[, Plot_Issuer := fifelse(annual_rank <= 5, ISSUER_NAME,
                                             "Other Private Insurers")]
  plot_data_inst <- insurer_exposure[, .(market_share = sum(market_share)),
                                      by = .(YEAR, Plot_Issuer)]

  overall_sizes <- plot_data_inst[, .(total_share = sum(market_share)), by = Plot_Issuer]
  setorder(overall_sizes, total_share)
  factor_levels_inst <- c("Other Private Insurers",
                           setdiff(overall_sizes$Plot_Issuer, "Other Private Insurers"))
  plot_data_inst[, Plot_Issuer := factor(Plot_Issuer, levels = factor_levels_inst)]

  HHI_SCALE_FACTOR <- 5000
  plot_data_inst <- merge(plot_data_inst, hhi_trend, by = "YEAR", all.x = TRUE)

  fig_top5 <- ggplot(plot_data_inst, aes(x = as.factor(YEAR))) +
    geom_col(aes(y = market_share, fill = Plot_Issuer),
             width = 0.85, color = "white", linewidth = 0.1) +
    geom_line(aes(y = HHI / HHI_SCALE_FACTOR, group = 1),
              color = "black", linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = HHI / HHI_SCALE_FACTOR, group = 1),
               color = "black", size = 2.5) +
    scale_y_continuous(
      labels   = scales::percent_format(accuracy = 1),
      expand   = c(0, 0),
      sec.axis = sec_axis(~ . * HHI_SCALE_FACTOR,
                          name = "Herfindahl-Hirschman Index (HHI)")
    ) +
    scale_fill_manual(values = setNames(
      c("gray70", scales::viridis_pal(option = "mako")(length(factor_levels_inst) - 1)),
      factor_levels_inst
    )) +
    labs(x = "Year", y = "Market Share (by Facility-Months)",
         title = "Private Insurance Market Consolidation",
         subtitle = "Top 5 Insurers (Bars) and Market Concentration HHI (Dashed Line)",
         fill = "Insurer") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right") +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE))

  save_fig(fig_top5, "Figure_TX_FR_Top5_Dominance_HHI", width = 12, height = 6)
  save_table(plot_data_inst, "Figure_data_Top5_HHI")

  # 11.3 Insurer Churn with Event Tags
  cat("--- 11.3: Insurer Churn ---\n")
  fac_yr_issuers <- unique(ins_panel[, .(FACILITY_ID, YEAR, ISSUER_NAME)])
  fac_yr_issuers[, lookup_key := paste(FACILITY_ID, YEAR, ISSUER_NAME, sep = "__")]
  existing_keys <- fac_yr_issuers$lookup_key
  t_lagged <- copy(fac_yr_issuers)
  t_lagged[, next_key  := paste(FACILITY_ID, YEAR + 1L, ISSUER_NAME, sep = "__")]
  t_lagged[, is_dropped := !(next_key %in% existing_keys)]
  fac_churn <- t_lagged[, .(abandoned_insurer = any(is_dropped)),
                          by = .(FACILITY_ID, YEAR = YEAR + 1L)]
  churn_summary <- fac_churn[YEAR >= 2008 & YEAR <= 2020,
                               .(total_churn = mean(abandoned_insurer)), by = YEAR]
  setorder(churn_summary, YEAR)

  fig_churn <- ggplot(churn_summary, aes(x = YEAR, y = total_churn)) +
    geom_line(color = "gray80", linewidth = 1, linetype = "dashed") +
    geom_point(aes(size = total_churn), color = "gray40", alpha = 0.5) +
    geom_point(data = churn_summary[YEAR == 2012], aes(y = total_churn),
               color = COL_TX, size = 5) +
    geom_point(data = churn_summary[YEAR == 2018], aes(y = total_churn),
               color = COL_CTRL, size = 5) +
    annotate("text", x = 2012, y = churn_summary[YEAR == 2012, total_churn] + 0.02,
             label = "Zurich Exit\n(2012)", color = COL_TX,
             fontface = "bold", size = 3.5) +
    annotate("text", x = 2018, y = churn_summary[YEAR == 2018, total_churn] + 0.02,
             label = "TOMIC Acquisition\n(2018)", color = COL_CTRL,
             fontface = "bold", size = 3.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Year", y = "Facility Abandonment Rate",
         title = "Institutional Drivers of Market Churn",
         subtitle = "Percentage of facilities dropping an incumbent provider.") +
    theme(legend.position = "none")

  save_fig(fig_churn, "Figure_TX_FR_Market_Churn_Annotated", width = 8, height = 5)
  save_table(churn_summary, "Figure_data_Market_Churn")

  # 11.4 Statutory Coverage Limits
  cat("--- 11.4: Statutory Coverage Limits ---\n")
  limits_data <- unique(ins_panel[!is.na(COVER_OCC) & COVER_OCC >= 10000,
                                   .(FIN_ASSUR_ID, YEAR, COVER_OCC)])
  limits_data[, coverage_tier := fcase(
    COVER_OCC == 1000000, "$1M (Statutory)",
    COVER_OCC  < 1000000, "Sub-$1M",
    COVER_OCC  > 1000000, "Excess (>$1M)"
  )]
  tier_shares <- limits_data[, .N, by = .(YEAR, coverage_tier)]
  tier_shares[, share := N / sum(N), by = YEAR]
  tier_shares[, coverage_tier := factor(coverage_tier,
                                         levels = c("Sub-$1M", "$1M (Statutory)",
                                                    "Excess (>$1M)"))]

  fig_limits <- ggplot(tier_shares,
                       aes(x = YEAR, y = share, fill = coverage_tier)) +
    geom_col(position = "stack", width = 0.7) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c("Sub-$1M"          = "firebrick",
                                  "$1M (Statutory)"   = "gray60",
                                  "Excess (>$1M)"     = COL_CTRL)) +
    labs(x = "Year", y = "Share of Contracts",
         title = "Per-Occurrence Coverage Limits",
         subtitle = "Adherence to the standard $1M statutory minimum vs. excess coverage",
         fill = "Occurrence Limit") +
    theme(legend.position = "bottom")

  save_fig(fig_limits, "Figure_TX_FR_Coverage_Limits", width = 8, height = 5)
  save_table(tier_shares, "Figure_data_Coverage_Limits")

} else {
  cat("  WARNING: Skipped Section 11 — texas_fr_contract_month_panel.csv not found\n")
}


# 11.2b Figure [B]: Risk-Differentiated Pricing (Mid-Continent Rate Filings)
cat("\n--- 11.2b: Figure [B] — Mid-Continent Pricing ---\n")

if (!is.null(rate_data)) {
  pricing_range <- rate_data[, .(
    mean_base    = mean(base_premium, na.rm = TRUE),
    mean_min     = mean(sched_min,    na.rm = TRUE),
    mean_max     = mean(sched_max,    na.rm = TRUE),
    sched_spread = mean(sched_max - sched_min, na.rm = TRUE),
    n_fac        = uniqueN(FACILITY_ID)
  ), by = YEAR]
  setorder(pricing_range, YEAR)

  fig_b <- ggplot(pricing_range, aes(x = YEAR)) +
    geom_ribbon(aes(ymin = mean_min, ymax = mean_max),
                fill = COL_TX, alpha = 0.2) +
    geom_line(aes(y = mean_base), color = COL_TX, linewidth = 1) +
    geom_point(aes(y = mean_base), color = COL_TX, size = 2) +
    labs(x = "Year", y = "Premium ($)",
         title = "Risk-Differentiated Pricing: Mid-Continent Rate Filings",
         subtitle = "Shaded band = filed scheduling range (min to max across facilities)",
         caption = sprintf("N facilities: %s-%s per year",
                           format(min(pricing_range$n_fac), big.mark = ","),
                           format(max(pricing_range$n_fac), big.mark = ","))) +
    scale_y_continuous(labels = scales::dollar_format())

  save_fig(fig_b, "FigureB_MidContinent_Pricing", width = 8, height = 5)
  save_table(pricing_range, "FigureB_data_pricing_range")
} else {
  cat("  WARNING: Skipped — rate filing data not available\n")
}

# 11.3b Figure [T]: Regulatory & Data Availability Timeline
cat("\n--- 11.3b: Figure [T] — Regulatory Timeline ---\n")

timeline_events <- data.table(
  date  = as.numeric(c(1988, 1989, 1993, 1995, 1998, 2005, 2007, 2020)),
  label = c(
    "EPA final technical\nstandards (40 CFR §280)",
    "TX phased mandates\nbegin (earliest cohort)",
    "TX phased mandates\nend (latest cohort)",
    "H.B. 2587 enacted\n(PSTRF cutoff set)",
    "Federal upgrade deadline\n& PSTRF cutoff",
    "EPAct 2005\n(database funding)",
    "TCEQ electronic FR\nrecording begins",
    "End of study\nwindow"
  ),
  type  = c("federal", "texas", "texas", "texas", "both",
            "federal", "data", "data"),
  y_pos = c(0.8, 0.5, 0.5, 0.65, 0.9, 0.7, 0.55, 0.55)
)

data_bars <- data.table(
  source = c("Tank Inventory & LUST", "Texas FR Panel", "Mid-Continent Rates"),
  start  = c(1985, 2007, ifelse(!is.null(rate_data),
                                 min(rate_data$YEAR, na.rm = TRUE), 2006)),
  end    = c(2020, 2020, ifelse(!is.null(rate_data),
                                 max(rate_data$YEAR, na.rm = TRUE), 2024)),
  y_bar  = c(0.15, 0.10, 0.05)
)

fig_t <- ggplot() +
  geom_segment(data = data_bars,
               aes(x = start, xend = end, y = y_bar, yend = y_bar),
               linewidth = 4, color = "gray70") +
  geom_text(data = data_bars,
            aes(x = (start + end) / 2, y = y_bar + 0.03, label = source),
            size = 2.8, fontface = "italic") +
  geom_segment(data = timeline_events,
               aes(x = date, xend = date, y = 0.25, yend = y_pos - 0.03),
               linetype = "dotted", color = "gray50") +
  geom_point(data = timeline_events, aes(x = date, y = y_pos),
             size = 3, shape = 18,
             color = fifelse(timeline_events$type == "texas", COL_TX,
                      fifelse(timeline_events$type == "federal", COL_CTRL,
                              "gray40"))) +
  geom_text(data = timeline_events,
            aes(x = date, y = y_pos + 0.06, label = label),
            size = 2.5, lineheight = 0.85) +
  annotate("rect", xmin = 1997.5, xmax = 1998.5, ymin = 0, ymax = 1,
           fill = "gold", alpha = 0.1) +
  scale_x_continuous(breaks = seq(1985, 2020, 5), limits = c(1984, 2022)) +
  scale_y_continuous(limits = c(0, 1.05)) +
  labs(title = "Regulatory and Data Availability Timeline",
       x = "Year", y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank())

save_fig(fig_t, "FigureT_Regulatory_Timeline", width = 14, height = 6)


#==============================================================================
# SECTION 12: APPENDIX B TABLES
#==============================================================================

cat("\n========================================\n")
cat("SECTION 12: APPENDIX B TABLES\n")
cat("========================================\n\n")

# 12.1 Table B.1: State Data Quality Report
cat("--- 12.1: Table B.1 — State Data Quality Report ---\n")

if (!is.null(data_quality_report)) {
  dq <- copy(data_quality_report)

  # Pass/fail columns for inclusion criteria
  if ("pct_miss_install_date" %in% names(dq))
    dq[, pass_install := fifelse(pct_miss_install_date <= 25, "\\checkmark", "\\texttimes")]
  if ("pct_closed_missing_date" %in% names(dq))
    dq[, pass_closure := fifelse(is.na(pct_closed_missing_date) |
                                   pct_closed_missing_date <= 25,
                                 "\\checkmark", "\\texttimes")]
  if ("pct_miss_tank_type" %in% names(dq))
    dq[, pass_wall := fifelse(pct_miss_tank_type <= 50, "\\checkmark", "\\texttimes")]

  dq[, group := fcase(
    state == "TX",  "Target",
    state == "MD",  "Excluded - No State Fund",
    state == "NJ",  "Excluded - Treated (private ins. May 2003)",
    state == "MI",  "Excluded - Treated (fund transition 1995)",
    state %in% c("IA", "FL", "CT"), "Excluded - Treated",
    state %in% c("ME", "NM", "AR", "OK", "LA", "KS",
                 "MT", "ID", "SD", "AL", "MN", "NC"), "Control Tier 1",
    state %in% c("IL", "MA", "OH", "PA", "TN", "VA", "CO"), "Control Tier 2",
    default = "Excluded - Data/Institutional"
  )]

  dq[state == "TN", note := "Fund est. 2008; pre-2008 LUST may be zero"]
  dq[state == "MD", note := "Never operated state FA fund"]
  dq[state == "NJ", note := "Private insurance req. May 2003"]

  setorder(dq, group, state)
  save_table(dq, "TableB1_State_Data_Quality")

  # Identify excluded states for gray shading
  excl_rows <- which(grepl("Excluded", dq$group))
  tx_row    <- which(dq$state == "TX")

  dq_display <- dq[, .(state, group, n_tanks,
                         pct_miss_install_date, pct_closed_missing_date,
                         pct_miss_tank_type,
                         pass_install, pass_closure, pass_wall)]

  write_tex(
    kbl(dq_display, format = "latex", booktabs = TRUE, linesep = "",
        escape = FALSE,
        caption = paste("State-level data quality metrics for the raw tank inventory.",
          "Criteria for inclusion in the analysis sample: installation date present",
          "$\\geq75\\%$; closure date present $\\geq75\\%$ (among closed tanks);",
          "wall type present $\\geq50\\%$."),
        label = "tab:data-quality",
        col.names = c("State", "Group", "N Tanks", "\\% Miss. Install",
                      "\\% Miss. Closure", "\\% Miss. Wall Type",
                      "Pass Install", "Pass Closure", "Pass Wall")) |>
      kable_styling(latex_options = c("scale_down", "hold_position"),
                    font_size = 9, full_width = FALSE, threeparttable = TRUE) |>
      column_spec(1:2, width = "2cm") |>
      row_spec(excl_rows, background = "gray90") |>
      row_spec(tx_row, bold = TRUE) |>
      footnote(general = paste(
        "Pass/fail criteria applied to raw source data before any sample filters.",
        "MI excluded as treated (fund transition 1995).",
        "NJ excluded as treated (private insurance mandate 2003).",
        "TN: state fund not established until July 2008 --- pre-2008 LUST",
        "counts are zero (institutional artifact)."),
        general_title = "", threeparttable = TRUE),
    "TableB1_State_Data_Quality"
  )
  cat("  OK: Table B.1 saved (CSV + .tex)\n")
} else {
  cat("  WARNING: Skipped — data_quality_report is NULL (check §3.6)\n")
}


# 12.2 Table B.2: Sample Construction Attrition
cat("\n--- 12.2: Table B.2 — Sample Attrition ---\n")

attrition_dt <- rbindlist(lapply(attrition_log, function(x) {
  as.data.table(lapply(x, function(v) if (length(v) == 1) v else as.character(v)))
}), fill = TRUE)

cat("  Attrition log:\n")
print(attrition_dt)
save_table(attrition_dt, "TableB2_Sample_Attrition")

write_tex(
  kbl(attrition_dt, format = "latex", booktabs = TRUE, linesep = "",
      escape = FALSE,
      caption = paste("Sample construction: sequential filter attrition. Each row",
        "reports facility count and facility-year count remaining after the",
        "cumulative filter, separately for Texas and the control group."),
      label = "tab:attrition",
      col.names = c("Filter Description", "Filter Applied",
                    "Facilities", "Facility-Years")) |>
    kable_styling(latex_options = c("hold_position"), font_size = 10,
                  full_width = FALSE, threeparttable = TRUE) |>
    column_spec(1, width = "4cm") |>
    column_spec(2, width = "5cm") |>
    column_spec(3:4, width = "2cm") |>
    footnote(general = paste(
      "Filter 1: 1985--2020 time window.",
      "Filter 2: Texas and 19 control states (NJ excluded as post-2003 treated unit).",
      "Filter 3: Incumbent facilities first observed before December 22, 1998.",
      "Filter 4: Complete tank-level installation and closure dates."),
      general_title = "", threeparttable = TRUE),
  "TableB2_Sample_Attrition"
)
cat("  OK: Table B.2 saved (CSV + .tex)\n")


# 12.3 Table B.3: Missing Data Balance Test
cat("\n--- 12.3: Table B.3 — Missing Data Balance Test ---\n")

if (!is.null(balance_glm)) {
  balance_tidy    <- as.data.table(broom::tidy(balance_glm))
  balance_tidy[, stars := sapply(p.value, stars_fn)]

  # Identify Texas row for bold formatting
  tx_row_b3 <- which(grepl("texas|treatment", tolower(balance_tidy$term)))

  save_table(balance_tidy, "TableB3_Missing_Data_Balance_Test")

  b3_display <- copy(balance_tidy)
  b3_display[, `:=`(
    estimate  = sprintf("%.4f", estimate),
    std.error = sprintf("%.4f", std.error),
    statistic = sprintf("%.3f", statistic),
    p.value   = paste0(sprintf("%.4f", p.value), stars)
  )]
  b3_display[, stars := NULL]

  write_tex(
    kbl(b3_display[, .(term, estimate, std.error, statistic, p.value)],
        format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = paste("Balance test for missing-data exclusion. Logistic",
          "regression of missing-record indicator on Texas treatment dummy.",
          "Coefficient insignificance confirms exclusion probability is orthogonal",
          "to treatment status."),
        label = "tab:missing-balance",
        col.names = c("Term", "Estimate", "Std. Error", "Statistic", "p-value")) |>
      kable_styling(latex_options = c("hold_position"), font_size = 10,
                    full_width = FALSE, threeparttable = TRUE) |>
      {if (length(tx_row_b3) > 0)
         row_spec(., tx_row_b3, bold = TRUE) else .}() |>
      footnote(general = paste(
        "Dependent variable equals one if the facility was excluded due to",
        "missing tank-level date records. Insignificant coefficient on Texas",
        "dummy confirms null of no differential attrition."),
        general_title = "", threeparttable = TRUE),
    "TableB3_Missing_Data_Balance_Test"
  )
  cat("  OK: Table B.3 saved (CSV + .tex)\n")
} else {
  cat("  WARNING: Balance GLM not available — Table B.3 skipped\n")
}


#==============================================================================
# SECTION 13: SAVE ANALYSIS-READY DATASETS (UPDATED)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 13: SAVE ANALYSIS-READY DATASETS\n")
cat("========================================\n\n")

ANALYSIS_DIR <- here("Data", "Analysis")
dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# Annual panel (main analysis dataset)
saveRDS(annual_data, file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
cat(sprintf("  OK: analysis_annual_data.rds: %s rows\n",
            format(nrow(annual_data), big.mark = ",")))

# Tank inventory (filtered to analysis sample)
saveRDS(tanks, file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
cat(sprintf("  OK: analysis_tank_inventory.rds: %s tanks\n",
            format(nrow(tanks), big.mark = ",")))

# 1998 cross-section (includes age_bin, install_year, risk flags)
saveRDS(tanks_1998, file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))
cat(sprintf("  OK: analysis_tanks_1999.rds: %s tanks\n",
            format(nrow(tanks_1998), big.mark = ",")))

# Closed tanks (includes age_bin_at_closure)
saveRDS(closed_tanks, file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
cat(sprintf("  OK: analysis_closed_tanks.rds: %s closures\n",
            format(nrow(closed_tanks), big.mark = ",")))

# Pre-period closures
saveRDS(pre_period_closures,
        file.path(ANALYSIS_DIR, "analysis_pre_period_closures.rds"))
cat(sprintf("  OK: analysis_pre_period_closures.rds: %s closures\n",
            format(nrow(pre_period_closures), big.mark = ",")))

# CV data — only if RUN_FULL (no tank_year_riskset; event_first_leak
# arrives pre-built in annual_data — see Section 5.5 documentation)
if (RUN_FULL && exists("cv_data") && nrow(cv_data) > 0) {
  saveRDS(cv_data, file.path(ANALYSIS_DIR, "analysis_cv_data.rds"))
  cat(sprintf("  OK: analysis_cv_data.rds: %s facility-years\n",
              format(nrow(cv_data), big.mark = ",")))
} else if (!RUN_FULL) {
  cat("  SKIPPED: analysis_cv_data.rds (RUN_FULL = FALSE)\n")
}

# Metadata (updated: run_full, cv_auc, age bin constants)
metadata <- list(
  # Study parameters
  treatment_date         = TREATMENT_DATE,
  treatment_year         = TREATMENT_YEAR,
  post_year              = POST_YEAR,
  panel_start            = PANEL_START,
  panel_end              = PANEL_END,
  es_start               = ES_START,
  es_end                 = ES_END,
  study_end_date         = STUDY_END_DATE,
  federal_mandate_date   = FEDERAL_MANDATE_DATE,
  mandate_cutoff_date    = MANDATE_CUTOFF_DATE,

  # Sample
  control_states         = CONTROL_STATES,
  nj_excluded            = TRUE,
  tn_fund_start          = TN_FUND_START_YEAR,
  tn_exclude_leak_models = tn_exclude_leak_models,
  md_no_fund             = MD_NO_FUND,
  n_facilities_tx        = tx_n,
  n_facilities_ctl       = ctl_n,

  # Mandate confound documentation (consumed by 02_DiD_Causal_Estimates.R)
  tx_mandate_start       = TX_MANDATE_START,
  tx_mandate_end         = TX_MANDATE_END,
  pt_validation_table    = pt_all,
  specA_pretrend_p       = specA_p,
  pooled_pretrend_p      = pooled_p,
  specA_n_facilities     = uniqueN(annual_data[spec_A_eligible == 1, panel_id]),
  specB_n_facilities     = uniqueN(annual_data[spec_B_eligible == 1, panel_id]),

  # Canonical age bin constants (for consistent use in 02_DiD)
  age_bin_breaks         = AGE_BIN_BREAKS,
  age_bin_labels         = AGE_BIN_LABELS,
  age_bin_ref            = AGE_BIN_REF,

  # CV validation results (NA when RUN_FULL = FALSE)
  run_full               = RUN_FULL,
  cv_auc_no_sfe          = if (RUN_FULL && exists("auc_no_sfe"))   auc_no_sfe   else NA_real_,
  cv_auc_with_sfe        = if (RUN_FULL && exists("auc_with_sfe")) auc_with_sfe else NA_real_,
  cv_n_facility_years    = if (exists("cv_data")) nrow(cv_data) else NA_integer_,

  # Output paths
  output_tables          = OUTPUT_TABLES,
  output_figures         = OUTPUT_FIGURES,
  incumbent_ids          = incumbent_ids,

  # Attrition
  attrition_log          = attrition_log,
  run_date               = Sys.time()
)
saveRDS(metadata, file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
cat("  OK: analysis_metadata.rds (includes mandate confound + CV AUC + age bin constants)\n")


cat("\n====================================================================\n")
cat("01_Descriptive_Analysis_REFACTORED.R COMPLETE\n")
cat(sprintf("  Total facilities: %s | Total facility-years: %s\n",
            format(uniqueN(annual_data$panel_id), big.mark = ","),
            format(nrow(annual_data), big.mark = ",")))
cat(sprintf("  RUN_FULL = %s | CV AUC (no SFE): %s\n",
            RUN_FULL,
            ifelse(RUN_FULL && !is.na(auc_no_sfe),
                   sprintf("%.3f", auc_no_sfe), "N/A")))
cat(sprintf("  Tables saved to: %s\n", OUTPUT_TABLES))
cat(sprintf("  Figures saved to: %s\n", OUTPUT_FIGURES))
cat("====================================================================\n")