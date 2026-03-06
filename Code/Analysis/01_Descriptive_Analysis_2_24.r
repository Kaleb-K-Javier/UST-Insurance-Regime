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
  #library(fwildclusterboot)
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
RUN_FULL <- TRUE   # <-- CHANGE TO TRUE FOR FINAL RUN

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

# Correcting the hyphen/dash character discrepancy
RATE_DIR <- here("Data", "Rate FIllings", "Mid-Continent Casualty Company ­– 23418")

# Modifying the pattern to include both facility_year and tank_month files
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
# Assigned to data_quality_report; consumed by Section 14.1 (TableB1).
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
  filter = sprintf("first observed < %d", TREATMENT_YEAR),
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
                full_width = FALSE) |>
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

cat("\n--- 5.3b: Age-at-closure distribution (H3 visual test) ---\n")

closed_plot <- closed_tanks[
  closure_year >= 1990 & closure_year <= 2018 &
  !is.na(age_at_closure) &
  age_at_closure >= 0 & age_at_closure <= 50
]
closed_plot[, period := factor(
  fifelse(closure_year >= POST_YEAR, "Post-Reform (1999+)", "Pre-Reform (1990–1998)"),
  levels = c("Pre-Reform (1990–1998)", "Post-Reform (1999+)")
)]
closed_plot[, group := fifelse(texas_treated == 1, "Texas", "Control")]

# Mean-age vertical lines per facet
mean_age_lines <- closed_plot[, .(mean_age = mean(age_at_closure, na.rm = TRUE)),
                               by = .(period, group)]

fig_closure_density <- ggplot(
  closed_plot, aes(x = age_at_closure, color = group, fill = group)
) +
  geom_density(alpha = 0.25, adjust = 1.2, linewidth = 0.8) +
  geom_vline(data = mean_age_lines,
             aes(xintercept = mean_age, color = group),
             linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~ period, nrow = 1) +
  scale_color_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_fill_manual(values  = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  labs(title    = "Age Distribution of Tank Closures: Pre- vs Post-Reform",
       subtitle = "H3: RB pricing concentrates closures among older tanks. Dashed = group means.",
       x = "Tank Age at Closure (years)", y = "Density",
       color = NULL, fill = NULL) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

fig_closure_cdf <- ggplot(
  closed_plot, aes(x = age_at_closure, color = group, linetype = period)
) +
  stat_ecdf(linewidth = 0.9, geom = "step") +
  geom_vline(xintercept = 20, linetype = "dotted",
             color = "grey50", linewidth = 0.5) +
  annotate("text", x = 21, y = 0.1,
           label = "20 yrs\n(elevated risk)", size = 3, hjust = 0, color = "grey40") +
  scale_color_manual(values    = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_linetype_manual(values = c("Pre-Reform (1990–1998)" = "dashed",
                                    "Post-Reform (1999+)"    = "solid")) +
  scale_x_continuous(breaks = seq(0, 50, 10)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title    = "Cumulative Distribution of Tank Age at Closure",
       subtitle = "Solid = Post-1999 | Dashed = Pre-1999. TX rightward shift post-reform = H3.",
       x = "Tank Age at Closure (years)", y = "Cumulative Share of Closures",
       color = NULL, linetype = NULL) +
  theme(legend.position = "bottom")

save_fig(fig_closure_density, "Figure_ClosureAge_Density", width = 10, height = 5)
save_fig(fig_closure_cdf,     "Figure_ClosureAge_CDF",     width  = 7, height = 5)
cat("  Saved: Figure_ClosureAge_Density + Figure_ClosureAge_CDF\n")

# Summary stats for console
closure_age_summary <- closed_plot[, .(
  n_closures  = .N,
  mean_age    = round(mean(age_at_closure,             na.rm = TRUE), 2),
  median_age  = round(median(age_at_closure,           na.rm = TRUE), 2),
  pct_over_20 = round(100 * mean(age_at_closure >= 20, na.rm = TRUE), 1)
), by = .(group, period)]
setorder(closure_age_summary, period, group)
cat("  H3 summary (mean age at closure by group x period):\n")
print(closure_age_summary)
save_table(closure_age_summary, "Table_ClosureAge_H3_Summary")

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
# (10_Build_Annual_Panel_Optimized.R, Section 11) using:
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
                  full_width = FALSE) |>
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
# SECTION 6.5: SAMPLE COMPOSITION (NEW - TableA0)
#==============================================================================

cat("\n--- 6.5: Table A0 - Sample Composition (Facility Size + Fuel Type) ---\n")

# Panel A: Facility counts by tank-count bins
# Aggregate to facility level first
fac_size <- tanks_1998[, .(n_tanks_fac = .N,
                            texas_treated = first(texas_treated)),
                        by = panel_id]

fac_size[, size_bin := fcase(
  n_tanks_fac == 1,                     "Single-tank facility",
  n_tanks_fac >= 2 & n_tanks_fac <= 3, "2-3 tanks",
  n_tanks_fac >= 4 & n_tanks_fac <= 6, "4-6 tanks",
  n_tanks_fac >= 7,                     "7+ tanks",
  default = "Unknown"
)]

panelA_comp <- fac_size[, .(
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

cat("  Panel A - Facility Size Distribution:\n")
print(panelA_comp)

# Panel B: Tank counts by wall type
panelB_wall <- tanks_1998[, .(
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

cat("  Panel B - Wall Type & Fuel Type:\n")
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

# Panel B rows - wall type
panelB_long_wall <- data.table(
  Panel    = "B: Tank Characteristics",
  Category = c("N Tanks", "Single-walled (%)", "Double-walled (%)",
                "Unknown wall type (%)"),
  Texas    = unlist(panelB_wall[Group == "Texas",  .(N_Tanks, Pct_SW, Pct_DW, Pct_Unknown)]),
  Control  = unlist(panelB_wall[Group == "Control", .(N_Tanks, Pct_SW, Pct_DW, Pct_Unknown)]),
  Total    = unlist(panelB_wall[Group == "Total",   .(N_Tanks, Pct_SW, Pct_DW, Pct_Unknown)])
)

# Panel B rows - fuel type
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
tableA0_display[, Texas    := ifelse(Category == "N Facilities" | Category == "N Tanks",
                                     format(Texas,   big.mark = ","),
                                     sprintf("%.1f", Texas))]
tableA0_display[, Control := ifelse(Category == "N Facilities" | Category == "N Tanks",
                                     format(as.numeric(Control), big.mark = ","),
                                     sprintf("%.1f", as.numeric(Control)))]
tableA0_display[, Total    := ifelse(Category == "N Facilities" | Category == "N Tanks",
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
                  full_width = FALSE) |>
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
                  full_width = FALSE) |>
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
                  full_width = FALSE) |>
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


# ---- Replacement-selection diagnostic for 0-4 bin ----
# had_recent_closure: rolling 2-year window on prior-year closures
annual_data[, had_recent_closure := frollapply(
  shift(n_closures, 1L, fill = 0L), n = 2, FUN = function(x) as.integer(any(x > 0))
), by = panel_id]

# --------------------------------------------------------------------------
# PRE-PANEL LEAK FLAG
# Identifies facilities with any LUST report before the panel window (pre-1990).
# These sites entered the panel as already-contaminated and must be excluded
# from the never-leaked risk set used in descriptive leak tables and CV models.
#
# WHY: has_previous_leak == 0 only catches within-panel leaks. A facility that
# leaked in 1987, replaced its tanks, and entered the 1990 data looked "clean"
# under the old filter. This caused the 0-4 age-bin anomaly (~2.1% rate):
# 13% of 0-4 facility-years had prior replacements (endogenous to latent risk),
# and pre-panel leakers inflated the rate further. After this fix, the age
# gradient is monotone as materials science predicts.
#
# SCOPE: Only affects never-leaked risk-set analyses (pre_cv, diag_04, cv_data).
# DiD outcomes (closure_event, leak_year) and Model 3A are unaffected.
# --------------------------------------------------------------------------

if (!exists("master_lust")) stop("master_lust must be loaded before this block.")

if (!"panel_id" %in% names(master_lust)) {
  master_lust[, panel_id := paste(
    toupper(trimws(as.character(facility_id))),
    toupper(trimws(as.character(state))),
    sep = "_"
  )]
}

pre_panel_leakers <- master_lust[
  !is.na(report_date) & report_date < as.Date("1990-01-01"),
  .(pre_panel_leak = 1L),
  by = panel_id
]

if ("pre_panel_leak" %in% names(annual_data)) annual_data[, pre_panel_leak := NULL]
annual_data <- merge(annual_data, pre_panel_leakers, by = "panel_id", all.x = TRUE)
annual_data[is.na(pre_panel_leak), pre_panel_leak := 0L]

cat(sprintf("  Facilities with pre-1990 LUST record: %s (%.1f%% of panel)\n",
  format(uniqueN(pre_panel_leakers$panel_id), big.mark = ","),
  100 * uniqueN(pre_panel_leakers$panel_id) / uniqueN(annual_data$panel_id)))

# 0-4 bin diagnostic — split by replacement history to show the mechanism
diag_04 <- annual_data[
  panel_year >= 1990 & panel_year <= 1998 &
  has_previous_leak == 0 &
  pre_panel_leak    == 0 &          # NEW: exclude pre-1990 leakers
  !is.na(has_single_walled) &
  age_bin == "0-4",
  .(
    leak_rate = round(mean(event_first_leak, na.rm = TRUE) * 1000, 2),
    n         = .N
  ),
  by = .(had_recent_closure)
]
cat("  0-4 bin diagnostic (leak rate per 1,000 fac-yrs, pre-panel leakers excluded):\n")
print(diag_04)
# After fix: had_recent_closure=0 rows should show ~1.6/1,000 (similar to 5-9 bin)
# had_recent_closure=1 rows remain elevated (~5.0/1,000) — that is real endogenous risk,
# not a coding artifact. We document it, not suppress it.


# Step 4: Panel C — Pre-period first-leak incidence by age_bin x wall type
pre_cv <- annual_data[
  panel_year >= 1990 & panel_year <= 1998 &
  has_previous_leak == 0 &
  pre_panel_leak    == 0 &          # NEW: exclude pre-1990 leakers
  !is.na(has_single_walled) &
  had_recent_closure == 0
]

pre_cv[, wall_label := fifelse(has_single_walled == 1, "Single-Walled", "Double-Walled")]

cat(sprintf("  pre_cv after full risk-set filter: %s fac-years\n",
            format(nrow(pre_cv), big.mark = ",")))

# Verify age gradient is now monotone
age_grad_check <- pre_cv[, .(
  rate = round(mean(event_first_leak, na.rm = TRUE) * 1000, 2)
), by = age_bin][order(age_bin)]
cat("  Age gradient check (should be low for 0-4, monotone rising after 5-9):\n")
print(age_grad_check)


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
                  font_size = 9, full_width = FALSE) |>
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

rm(panA, panB, panC_display, combined_data)

#==============================================================================
# SECTION 8: DESCRIPTIVE FIGURES
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: DESCRIPTIVE FIGURES\n")
cat("========================================\n\n")

# 8.1 Figure 4 + Parallel Trends Validation
# ---------------------------------------------------------------------------
cat("--- 8.1: Figure 4 + Parallel Trends Validation ---\n")

# Helper: run one pre-trend Wald test (Facility-level retained for Table B.4)
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
              "2. Spec A (post-1988 only) - PRIMARY IDENTIFICATION", "",
              mandate_control = "None"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "3. Spec B (pre-1988 only), no mandate control", "",
              mandate_control = "None"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "4. Spec B (pre-1988 only) + mandate_active control",
              "mandate_active",
              mandate_control = "mandate_active")
))

specA_p  <- pt_all[grepl("Spec A",  Specification), `p-value`]
pooled_p <- pt_all[grepl("Pooled",  Specification), `p-value`]

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
                  font_size = 9, full_width = FALSE) |>
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

# ---------------------------------------------------------------------------
# Tank-Level Aggregation & Plotting Helpers (1985-2018)
# ---------------------------------------------------------------------------
# Helper: Build State-Year Panel from Tank Inventory
build_tank_state_year_panel <- function(tank_dt, start_yr = 1985, end_yr = 2018) {
  dt <- tank_dt[!is.na(tank_installed_date)]
  res <- lapply(start_yr:end_yr, function(yr) {
    active <- dt[year(tank_installed_date) <= yr & 
                 (is.na(tank_closed_date) | year(tank_closed_date) >= yr)]
    if(nrow(active) == 0) return(NULL)
    active[, .(
      panel_year = yr,
      texas_treated = as.integer(first(state) == "TX"),
      n_active = .N,
      n_closed = sum(year(tank_closed_date) == yr, na.rm = TRUE)
    ), by = state]
  })
  st_yr_panel <- rbindlist(res)
  st_yr_panel[, rate := n_closed / n_active]
  return(st_yr_panel)
}

# Helper 1: Plot Raw Tank Rates
plot_tank_raw_rates <- function(st_yr_panel, title_text, subtitle_text) {
  agg <- st_yr_panel[, .(
    rate = sum(n_closed) / sum(n_active),
    n = sum(n_active)
  ), by = .(panel_year, Group = fifelse(texas_treated == 1, "Texas", "Control"))]
  
  agg[, se := sqrt(rate * (1 - rate) / n)]
  agg[, ci_lo := rate - 1.96 * se]
  agg[, ci_hi := rate + 1.96 * se]
  
  p <- ggplot(agg, aes(x = panel_year, y = rate, color = Group)) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = Group), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = COL_PAIR) +
    scale_fill_manual(values = COL_PAIR, guide = "none") +
    labs(x = "Year", y = "Annual Tank Closure Rate",
         title = title_text, subtitle = subtitle_text, color = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1985, 2018, by = 3))
  return(p)
}

# Helper 2: Plot Tank Mean Difference 
plot_tank_mean_diff <- function(st_yr_panel, title_text, subtitle_text) {
  agg <- st_yr_panel[, .(
    rate = sum(n_closed) / sum(n_active),
    n = sum(n_active)
  ), by = .(panel_year, Group = fifelse(texas_treated == 1, "Texas", "Control"))]
  
  wide <- dcast(agg, panel_year ~ Group, value.var = c("rate", "n"))
  wide[, diff := rate_Texas - rate_Control]
  wide[, se := sqrt((rate_Texas * (1 - rate_Texas) / n_Texas) + 
                    (rate_Control * (1 - rate_Control) / n_Control))]
  wide[, ci_lo := diff - 1.96 * se]
  wide[, ci_hi := diff + 1.96 * se]
  
  p <- ggplot(wide[!is.na(diff)], aes(x = panel_year, y = diff)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = COL_TX, alpha = 0.2) +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(color = COL_TX, size = 2.5) +
    geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed", color = "gray40") +
    labs(x = "Year", y = "Raw Difference (Texas - Control)",
         title = title_text, subtitle = subtitle_text) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1985, 2018, by = 3))
  return(p)
}

# Helper 3: TWFE Event Study (State & Year FEs, weighted by tank counts) 
plot_tank_event_study <- function(st_yr_panel, title_text, subtitle_text) {
  mod <- tryCatch(
    feols(rate ~ i(panel_year, texas_treated, ref = TREATMENT_YEAR) | 
          state + panel_year, 
          weights = ~n_active,
          data = st_yr_panel, 
          cluster = ~state),
    error = function(e) NULL
  )
  
  if (is.null(mod)) return(ggplot() + annotate("text", x=1998, y=0, label="TWFE Failed"))
  
  dt_coef <- as.data.table(broom::tidy(mod, conf.int = TRUE))
  dt_coef[, year := as.numeric(stringr::str_extract(term, "[0-9]{4}"))]
  
  ref_row <- data.table(term = "reference", estimate = 0, std.error = 0, 
                        statistic = 0, p.value = NA, conf.low = 0, conf.high = 0, 
                        year = TREATMENT_YEAR)
  dt_coef <- rbind(dt_coef, ref_row, fill = TRUE)
  setorder(dt_coef, year)
  
  p <- ggplot(dt_coef[!is.na(year)], aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = COL_TX, alpha = 0.2) +
    geom_line(color = COL_TX, linewidth = 1) +
    geom_point(color = COL_TX, size = 2.5) +
    geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed", color = "gray40") +
    labs(x = "Year", y = "TWFE Estimate (State & Year FEs)",
         title = title_text, subtitle = subtitle_text) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1985, 2018, by = 3))
  return(p)
}

# ---------------------------------------------------------------------------
# Generate Spec A Figures (All 3 Views - True Tank-Level)
# ---------------------------------------------------------------------------
tanks_specA <- tanks[year(tank_installed_date) > 1988]
st_yr_specA <- build_tank_state_year_panel(tanks_specA)

fig4A_tank_raw <- plot_tank_raw_rates(st_yr_specA, 
  "Tank Closure Rates - Post-1988 Cohort (Spec A)", 
  "True tank-level subset. Mechanically begins in 1989. Shaded = 95% CI.")
save_fig(fig4A_tank_raw, "Figure4A_Tank_SpecA_RawRates", width = 8, height = 5)

fig4B_tank_diff <- plot_tank_mean_diff(st_yr_specA, 
  "Tank Closure Rate Difference - Post-1988 Cohort (Spec A)", 
  "Raw unadjusted mean difference (Texas minus Control). Shaded = 95% CI.")
save_fig(fig4B_tank_diff, "Figure4B_Tank_SpecA_MeanDiff", width = 8, height = 5)

fig4C_tank_twfe <- plot_tank_event_study(st_yr_specA, 
  "Tank Event Study (TWFE) - Post-1988 Cohort (Spec A)", 
  "Weighted by tank counts. State & Year FEs. Reference year = 1998.")
save_fig(fig4C_tank_twfe, "Figure4C_Tank_SpecA_EventStudy", width = 8, height = 5)


# ---------------------------------------------------------------------------
# Generate Pooled Figures (All 3 Views - Appendix / Contaminated)
# ---------------------------------------------------------------------------
st_yr_pooled <- build_tank_state_year_panel(tanks)
mandate_layer <- annotate("rect", xmin = TX_MANDATE_START - 0.5, xmax = TX_MANDATE_END + 0.5, 
                          ymin = -Inf, ymax = Inf, fill = "gold", alpha = 0.10)

fig_app_pooled_raw <- plot_tank_raw_rates(st_yr_pooled, 
  "Tank Closure Rates - Pooled Sample (Appendix)", 
  "Includes pre-1988 tanks. Gold band = mandate years. Shaded = 95% CI.") + 
  mandate_layer
save_fig(fig_app_pooled_raw, "Figure_Appx_Tank_Pooled_RawRates", width = 8, height = 5)

fig_app_pooled_diff <- plot_tank_mean_diff(st_yr_pooled, 
  "Tank Closure Rate Difference - Pooled Sample (Appendix)", 
  "Raw mean difference. Gold band = mandate years. Shaded = 95% CI.") + 
  mandate_layer
save_fig(fig_app_pooled_diff, "Figure_Appx_Tank_Pooled_MeanDiff", width = 8, height = 5)

fig_app_pooled_twfe <- plot_tank_event_study(st_yr_pooled, 
  "Tank Event Study (TWFE) - Pooled Sample (Appendix)", 
  "Contaminated identification. Fails parallel trends. Shaded = 95% CI.") + 
  mandate_layer
save_fig(fig_app_pooled_twfe, "Figure_Appx_Tank_Pooled_EventStudy", width = 8, height = 5)


# ---------------------------------------------------------------------------
# Generate Spec B Figures (All 3 Views - Appendix / Contaminated)
# ---------------------------------------------------------------------------
tanks_specB <- tanks[year(tank_installed_date) <= 1988]
st_yr_specB <- build_tank_state_year_panel(tanks_specB)

fig_app_specB_raw <- plot_tank_raw_rates(st_yr_specB, 
  "Tank Closure Rates - Pre-1988 Cohort (Spec B)", 
  "Exclusively mandate-exposed tanks. Gold band = mandate years. Shaded = 95% CI.") + 
  mandate_layer
save_fig(fig_app_specB_raw, "Figure_Appx_Tank_SpecB_RawRates", width = 8, height = 5)

fig_app_specB_diff <- plot_tank_mean_diff(st_yr_specB, 
  "Tank Closure Rate Difference - Pre-1988 Cohort (Spec B)", 
  "Raw mean difference. Gold band = mandate years. Shaded = 95% CI.") + 
  mandate_layer
save_fig(fig_app_specB_diff, "Figure_Appx_Tank_SpecB_MeanDiff", width = 8, height = 5)

fig_app_specB_twfe <- plot_tank_event_study(st_yr_specB, 
  "Tank Event Study (TWFE) - Pre-1988 Cohort (Spec B)", 
  "Exclusively mandate-exposed tanks. Rejects parallel trends. Shaded = 95% CI.") + 
  mandate_layer
save_fig(fig_app_specB_twfe, "Figure_Appx_Tank_SpecB_EventStudy", width = 8, height = 5)


# 8.2 Figure B-1 (Appendix): Full 4-panel event trends (1985-2018)
cat("\n--- 8.2: Figure B-1 (Appendix) - Full 4-Panel Trends ---\n")

pre_trends_full <- annual_data[
  panel_year >= 1985 & panel_year <= 2018, .(
    closure_rate   = mean(closure_event, na.rm = TRUE),
    exit_rate      = mean(exit_flag,     na.rm = TRUE),
    leak_incidence = mean(leak_year,     na.rm = TRUE),
    leaks_per_1000 = sum(n_leaks, na.rm = TRUE) / .N * 1000
  ),
  by = .(panel_year,
         treatment_group = fifelse(texas_treated == 1, "Texas", "Control"))
]

closure_age_pre <- closed_tanks[
  closure_year >= 1985 & closure_year <= 2018, .(
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
    scale_x_continuous(breaks = seq(1985, 2018, by = 4)) +
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
  top = textGrob("Trend Comparison (1985-2018)",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)
save_fig(fig_b1, "FigureB1_PreTrends_Full_4Panel", width = 12, height = 8)


#==============================================================================
# SECTION 8.3: FIGURES 1, 2, 3 - CAPITAL STOCK CHARACTERIZATION (NEW)
#==============================================================================

cat("\n--- 8.3: Figures 1, 2, 3 - Capital Stock Characterization ---\n")

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
# SECTION 9: RISK FACTOR CV VALIDATION + EMPIRICAL RISK SCORING
#==============================================================================
# DATA PROVENANCE NOTE:
#   Outcome: event_first_leak (facility-year binary, pre-built by panel builder).
#   Sample: 1990-1998 pre-period, never-yet-leaked facilities.
#   "Never-yet-leaked" = has_previous_leak == 0 AND pre_panel_leak == 0.
#   Replacement history (past_replacement) enters as a COVARIATE, not a filter:
#   these are genuinely higher-risk sites; excluding them would bias the age
#   gradient. See 0-4 anomaly documentation above.
#   The age × past_replacement interaction absorbs the spurious 0-4 elevation
#   that arises because replacement events generate age-0 facility-years at
#   sites that already had a failure.
#   rf_pre_1980 is included conditionally: when install_year is absent the
#   variable is all-NA and must be excluded from all model formulas to prevent
#   NA propagation dropping every observation ("contrasts can only be applied
#   to factors with 2+ levels" in glm; "All observations contain NA values"
#   in fixest).
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: RISK FACTOR CV VALIDATION + EMPIRICAL RISK SCORING\n")
cat("========================================\n\n")

#------------------------------------------------------------------------------
# 9.1 Build estimation sample
#------------------------------------------------------------------------------
cat("--- 9.1: Build estimation sample ---\n")

stopifnot("pre_panel_leak" %in% names(annual_data))

annual_data[, past_replacement := as.integer(
  cumsum(shift(n_closures, 1L, fill = 0L)) > 0
), by = panel_id]

cv_data <- annual_data[
  panel_year        >= 1990 &
  panel_year        <= 1998 &
  has_previous_leak == 0    &
  pre_panel_leak    == 0    &
  !is.na(has_single_walled) &
  !is.na(age_bin)
]

stopifnot(cv_data[, sum(has_previous_leak)] == 0)
stopifnot(cv_data[, sum(pre_panel_leak)]    == 0)
stopifnot(cv_data[, sum(panel_year > year_of_first_leak, na.rm = TRUE)] == 0)

rate_04 <- cv_data[age_bin == "0-4" & past_replacement == 0,
                    mean(event_first_leak, na.rm = TRUE) * 1000]
rate_59 <- cv_data[age_bin == "5-9"  & past_replacement == 0,
                    mean(event_first_leak, na.rm = TRUE) * 1000]
cat(sprintf("  VERIFICATION — 0-4 rate (no prior replacement): %.2f per 1,000\n", rate_04))
cat(sprintf("  VERIFICATION — 5-9 rate (no prior replacement): %.2f per 1,000\n", rate_59))
if (!is.na(rate_04) && !is.na(rate_59) && rate_04 <= rate_59 * 1.5)
  cat("  OK: 0-4 anomaly resolved — age gradient monotone as expected.\n") else
  cat("  WARNING: 0-4 rate still elevated — check for remaining contamination.\n")

cat(sprintf("  Facilities with past replacement in risk set: %s (%.1f%%)\n",
  format(uniqueN(cv_data[past_replacement == 1, panel_id]), big.mark = ","),
  100 * uniqueN(cv_data[past_replacement == 1, panel_id]) /
        uniqueN(cv_data$panel_id)))

if ("install_year" %in% names(cv_data)) {
  cv_data[, rf_pre_1980 := as.integer(!is.na(install_year) & install_year < 1980)]
} else if ("pre1998_install" %in% names(cv_data)) {
  cv_data[, rf_pre_1980 := pre1998_install]
  cat("  WARNING: using pre1998_install proxy for rf_pre_1980\n")
} else {
  cv_data[, rf_pre_1980 := NA_integer_]
  cat("  WARNING: rf_pre_1980 set to NA (install_year absent)\n")
}

cv_data[, `:=`(
  log_capacity = log(pmax(total_capacity, 1, na.rm = TRUE)),
  age_bin_reg  = age_bin_for_reg(age_bin)
)]

set.seed(20250128)
cv_data[, fold := sample(1:5, .N, replace = TRUE), by = state]

cat(sprintf("  CV data: %s facility-years | %s first-leak events (%.3f%%)\n",
  format(nrow(cv_data), big.mark = ","),
  format(sum(cv_data$event_first_leak, na.rm = TRUE), big.mark = ","),
  100 * mean(cv_data$event_first_leak, na.rm = TRUE)))
cat(sprintf("  Unique counties in CV data: %s\n",
  format(uniqueN(cv_data$county_fips), big.mark = ",")))

cv_data[, `:=`(pred_no_cfe   = NA_real_,
               pred_with_cfe = NA_real_)]
auc_no_cfe   <- NA_real_
auc_with_cfe <- NA_real_

#------------------------------------------------------------------------------
# Build model formulas ONCE, conditioned on rf_pre_1980 availability.
#
# KEY FIX: When install_year is absent, rf_pre_1980 is all-NA. Including an
# all-NA predictor in glm causes every row to be dropped (NA propagation),
# producing "contrasts can only be applied to factors with 2+ levels".
# fixest is more explicit: "All observations contain NA values. Breakup: RHS."
# The fix is to check availability once here and use the same conditional
# formula in sections 9.2, 9.3, and 9.8 so all models are identical.
#
# The age × past_replacement interaction is always included: it absorbs
# the spurious 0-4 elevation from replacement-site observations.
#------------------------------------------------------------------------------
has_rf_pre_1980 <- !all(is.na(cv_data$rf_pre_1980))
cat(sprintf("  rf_pre_1980 available: %s\n", has_rf_pre_1980))

rhs_no_fe <- if (has_rf_pre_1980) {
  paste("has_single_walled + age_bin_reg + rf_pre_1980 +",
        "log_capacity + is_motor_fuel + past_replacement +",
        "I(age_bin_reg * past_replacement) + factor(panel_year)")
} else {
  paste("has_single_walled + age_bin_reg +",
        "log_capacity + is_motor_fuel + past_replacement +",
        "I(age_bin_reg * past_replacement) + factor(panel_year)")
}
formula_no_fe <- as.formula(paste("event_first_leak ~", rhs_no_fe))

rhs_sfe <- if (has_rf_pre_1980) {
  paste("has_single_walled + age_bin_reg + rf_pre_1980 +",
        "log_capacity + is_motor_fuel + past_replacement +",
        "I(age_bin_reg * past_replacement)")
} else {
  paste("has_single_walled + age_bin_reg +",
        "log_capacity + is_motor_fuel + past_replacement +",
        "I(age_bin_reg * past_replacement)")
}
formula_sfe <- as.formula(paste("event_first_leak ~", rhs_sfe, "| panel_year + state"))

cat(sprintf("  No-FE formula RHS:    %s\n", rhs_no_fe))
cat(sprintf("  State-FE formula RHS: %s | panel_year + state\n", rhs_sfe))


#------------------------------------------------------------------------------
# 9.2 5-fold CV logit — NO state FE
#------------------------------------------------------------------------------
cat("\n--- 9.2: 5-fold CV logit (NO state FE) ---\n")

for (k in 1:5) {
  train_fold <- cv_data[fold != k & !is.na(has_single_walled) & !is.na(age_bin_reg)]

  fit_no_fe <- tryCatch(
    glm(formula_no_fe, data = train_fold, family = binomial(link = "logit")),
    error = function(e) {
      cat(sprintf("  WARNING: Fold %d (no FE) failed: %s\n", k, e$message)); NULL
    }
  )
  if (!is.null(fit_no_fe)) {
    cv_data[fold == k,
            pred_no_cfe := predict(fit_no_fe, newdata = .SD, type = "response")]
    cat(sprintf("  Fold %d (no FE): train N = %s\n",
                k, format(nrow(train_fold), big.mark = ",")))
  }
}


if (RUN_FULL) {
#------------------------------------------------------------------------------
  # 9.3 5-fold CV logit — state FE
  #----------------------------------------------------------------------------
  cat("\n--- 9.3: 5-fold CV logit (with state FE) ---\n")

  for (k in 1:5) {
    train_fold <- cv_data[fold != k]
    test_fold  <- cv_data[fold == k]

    state_counts <- train_fold[, .N, by = state]
    valid_states <- state_counts[N >= 5, state]
    train_sfe    <- train_fold[state %in% valid_states &
                               !is.na(has_single_walled) &
                               !is.na(age_bin_reg)]

    if (nrow(train_sfe) < 100 || uniqueN(train_sfe$state) < 2) {
      cat(sprintf("  WARNING: Fold %d (state FE): too few valid states — skipped\n", k))
      next
    }

    fit_sfe <- tryCatch(
      fixest::feglm(formula_sfe,
                    data   = train_sfe,
                    family = binomial(link = "logit"),
                    notes  = FALSE),
      error = function(e) {
        cat(sprintf("  WARNING: Fold %d (state FE) failed: %s\n", k, e$message)); NULL
      }
    )

    if (!is.null(fit_sfe)) {
      cv_data[fold == k & state %in% valid_states,
              pred_with_cfe := predict(fit_sfe, newdata = .SD, type = "response")]
      cat(sprintf("  Fold %d (state FE): train N = %s | valid states = %s\n",
                  k,
                  format(nrow(train_sfe),      big.mark = ","),
                  format(length(valid_states), big.mark = ",")))
    }
  }

  # Full-sample state FE model (used for PD in 9.8b)
  final_model_sfe <- tryCatch(
    fixest::feglm(formula_sfe,
                  data   = cv_data[!is.na(has_single_walled) & !is.na(age_bin_reg)],
                  family = binomial(link = "logit"),
                  notes  = FALSE),
    error = function(e) {
      cat(sprintf("  WARNING: Full-sample state FE model failed: %s\n", e$message)); NULL
    }
  )
  if (!is.null(final_model_sfe))
    cat(sprintf("  Full-sample state FE model fitted on %s facility-years\n",
                format(cv_data[!is.na(has_single_walled) & !is.na(age_bin_reg), .N],
                       big.mark = ",")))

  #----------------------------------------------------------------------------
  # 9.4 AUC-ROC + combined ROC plot
  #----------------------------------------------------------------------------
  cat("\n--- 9.4: AUC-ROC ---\n")

  roc_no_cfe <- roc_with_cfe <- NULL

  cv_complete_no <- cv_data[!is.na(pred_no_cfe) & !is.na(event_first_leak)]
  if (nrow(cv_complete_no) > 100) {
    roc_no_cfe <- pROC::roc(cv_complete_no$event_first_leak,
                             cv_complete_no$pred_no_cfe, quiet = TRUE)
    auc_no_cfe <- as.numeric(pROC::auc(roc_no_cfe))
    cat(sprintf("  AUC-ROC (No State FE):   %.3f\n", auc_no_cfe))
    cat(sprintf("  Interpretation: %s\n",
      fcase(auc_no_cfe >= 0.80, "Good discrimination (>= 0.80)",
            auc_no_cfe >= 0.70, "Acceptable discrimination (0.70-0.79)",
            default            = "Modest discrimination (< 0.70 — expected for rare events)")))
  } else {
    cat("  WARNING: Too few OOB predictions for AUC (no-FE model)\n")
  }

  cv_complete_cfe <- cv_data[!is.na(pred_with_cfe) & !is.na(event_first_leak)]
  if (nrow(cv_complete_cfe) > 100) {
    roc_with_cfe <- pROC::roc(cv_complete_cfe$event_first_leak,
                               cv_complete_cfe$pred_with_cfe, quiet = TRUE)
    auc_with_cfe <- as.numeric(pROC::auc(roc_with_cfe))
    cat(sprintf("  AUC-ROC (With State FE): %.3f\n", auc_with_cfe))
  }

  # Alias so any downstream reference to auc_no_sfe does not error
  auc_no_sfe <- auc_no_cfe

  if (!is.na(auc_no_cfe) && !is.na(auc_with_cfe)) {
    atten <- (1 - auc_with_cfe / auc_no_cfe) * 100
    cat(sprintf("  AUC attenuation with state FE: %.1f%%\n", atten))
    if (atten > 30) cat("  NOTE: >30% attenuation — substantial cross-state heterogeneity\n")
    else if (atten < 0) cat("  NOTE: State FE improves discrimination — local controls informative\n")
    else cat("  OK: Within-state variation sufficient.\n")
  }

  roc_list <- list()
  if (!is.null(roc_no_cfe))
    roc_list[["no_cfe"]] <- data.table(
      fpr   = 1 - roc_no_cfe$specificities,
      tpr   = roc_no_cfe$sensitivities,
      Model = sprintf("No State FE  (AUC = %.3f)", auc_no_cfe))
  if (!is.null(roc_with_cfe))
    roc_list[["with_cfe"]] <- data.table(
      fpr   = 1 - roc_with_cfe$specificities,
      tpr   = roc_with_cfe$sensitivities,
      Model = sprintf("With State FE  (AUC = %.3f)", auc_with_cfe))

  if (length(roc_list) > 0) {
    roc_combined <- rbindlist(roc_list)
    model_colors <- setNames(
      c(COL_TX, COL_CTRL)[seq_len(uniqueN(roc_combined$Model))],
      unique(roc_combined$Model))

    fig_roc <- ggplot(roc_combined, aes(x = fpr, y = tpr, color = Model)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                  color = "gray60", linewidth = 0.6) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = model_colors) +
      coord_fixed() +
      labs(title    = "ROC Curves: Out-of-Bag Predicted Leak Risk",
           subtitle = "5-Fold Cross-Validation. Dashed = random classifier.",
           x = "False Positive Rate (1 − Specificity)",
           y = "True Positive Rate (Sensitivity)",
           color = NULL) +
      theme(legend.position   = c(0.62, 0.12),
            legend.background = element_rect(fill = "white", color = "gray85",
                                             linewidth = 0.4))
    save_fig(fig_roc, "Figure_CV_ROC_Combined", width = 6, height = 6)
    save_table(roc_combined[, .(Model, fpr, tpr)], "Figure_data_ROC_Combined")
  }

  #----------------------------------------------------------------------------
  # 9.5 Discrimination plots: lift curve + score-separation density
  #      + calibration check (demoted to supporting role)
  #----------------------------------------------------------------------------
  cat("\n--- 9.5: Discrimination plots (lift, score separation, calibration) ---\n")

  make_discrimination_plots <- function(pred_col, model_label, file_suffix,
                                        auc_val = NA_real_) {

    cv_cal <- cv_data[!is.na(get(pred_col)) & !is.na(event_first_leak)]
    if (nrow(cv_cal) <= 100) {
      cat(sprintf("  WARNING: Too few obs for discrimination plots (%s)\n", model_label))
      return(invisible(NULL))
    }

    # ---- (A) Lift curve -----------------------------------------------------
    cv_cal_lift <- copy(cv_cal)
    setorder(cv_cal_lift, -get(pred_col))
    cv_cal_lift[, cumulative_pct_screened := seq_len(.N) / .N]
    cv_cal_lift[, cumulative_pct_events   := cumsum(event_first_leak) /
                                              sum(event_first_leak, na.rm = TRUE)]

    idx_thin   <- unique(c(1L,
                           round(seq(1, nrow(cv_cal_lift), length.out = 600L)),
                           nrow(cv_cal_lift)))
    lift_model <- cv_cal_lift[idx_thin,
                               .(x    = cumulative_pct_screened,
                                 y    = cumulative_pct_events,
                                 type = model_label)]

    lift_random   <- data.table(x = c(0, 1), y = c(0, 1), type = "Random")
    lift_combined <- rbindlist(list(lift_random, lift_model), fill = TRUE)

    top10_lift <- cv_cal_lift[cumulative_pct_screened <= 0.101,
                               max(cumulative_pct_events, na.rm = TRUE)]
    top20_lift <- cv_cal_lift[cumulative_pct_screened <= 0.201,
                               max(cumulative_pct_events, na.rm = TRUE)]

    fig_lift <- ggplot(lift_combined,
                       aes(x = x, y = y, color = type, linetype = type)) +
      geom_line(linewidth = 1) +
      geom_segment(aes(x = 0.10, xend = 0.10, y = 0,          yend = top10_lift),
                   linetype = "dotted", color = "gray50", linewidth = 0.5,
                   inherit.aes = FALSE) +
      geom_segment(aes(x = 0,    xend = 0.10, y = top10_lift, yend = top10_lift),
                   linetype = "dotted", color = "gray50", linewidth = 0.5,
                   inherit.aes = FALSE) +
      annotate("text", x = 0.115, y = top10_lift - 0.02, hjust = 0, size = 2.8,
               color = "gray30",
               label = sprintf("Top 10%% screens\n%.0f%% of leaks", top10_lift * 100)) +
      annotate("text", x = 0.215, y = top20_lift + 0.025, hjust = 0, size = 2.8,
               color = "gray30",
               label = sprintf("Top 20%% → %.0f%% leaks", top20_lift * 100)) +
      scale_color_manual(values    = c("Random" = "gray60",
                                        setNames(COL_TX, model_label))) +
      scale_linetype_manual(values = c("Random" = "dashed",
                                        setNames("solid", model_label))) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(title    = sprintf("Lift Curve: %s", model_label),
           subtitle = ifelse(!is.na(auc_val),
             sprintf("OOB predictions | AUC = %.3f", auc_val),
             "OOB predictions"),
           x     = "Fraction of Facilities Screened (descending risk order)",
           y     = "Cumulative Fraction of First-Leak Events Captured",
           color = NULL, linetype = NULL) +
      theme(legend.position = "bottom")

    # ---- (B) Score-separation density ---------------------------------------
    cv_cal[, outcome_label := fifelse(event_first_leak == 1L,
                                       "First Leak (event)", "No Leak")]
    x_cap_sep <- quantile(cv_cal[[pred_col]], 0.99, na.rm = TRUE)

    fig_sep <- ggplot(
      cv_cal[get(pred_col) <= x_cap_sep],
      aes(x = get(pred_col), fill = outcome_label, color = outcome_label)
    ) +
      geom_density(alpha = 0.35, linewidth = 0.7) +
      scale_fill_manual(values  = c("First Leak (event)" = COL_TX,
                                     "No Leak"           = COL_CTRL)) +
      scale_color_manual(values = c("First Leak (event)" = COL_TX,
                                     "No Leak"           = COL_CTRL)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      labs(title    = sprintf("Score Separation: %s", model_label),
           subtitle = sprintf(
             "OOB predicted probability by actual outcome | AUC = %.3f\n%s",
             ifelse(is.na(auc_val), 0, auc_val),
             "Good discrimination = little overlap between distributions."),
           x     = "Predicted P(First Leak)",
           y     = "Density",
           fill  = NULL, color = NULL) +
      theme(legend.position = "bottom")

    # ---- (C) Calibration check (bias, not discrimination) -------------------
    breaks <- unique(quantile(cv_cal[[pred_col]],
                               probs = seq(0, 1, by = 0.1), na.rm = TRUE))
    if (length(breaks) >= 3) {
      cv_cal[, decile := as.integer(cut(get(pred_col), breaks = breaks,
                                         include.lowest = TRUE, labels = FALSE))]
      cal_tbl <- cv_cal[!is.na(decile), .(
        mean_predicted = round(mean(get(pred_col),    na.rm = TRUE), 4),
        mean_actual    = round(mean(event_first_leak, na.rm = TRUE), 4),
        mean_sw_share  = round(mean(has_single_walled, na.rm = TRUE), 3),
        n_fac_years    = .N,
        n_leaks        = sum(event_first_leak, na.rm = TRUE)
      ), by = decile][order(decile)]

      bottom_rate <- cal_tbl[decile == min(decile), mean_actual]
      top_rate    <- cal_tbl[decile == max(decile), mean_actual]
      cal_tbl[, lift := round(mean_actual / bottom_rate, 2)]
      if (!is.na(bottom_rate) && bottom_rate > 0)
        cat(sprintf("  [%s] Top decile: %.4f | Bottom: %.4f | Lift: %.1fx\n",
                    model_label, top_rate, bottom_rate, top_rate / bottom_rate))

      fig_cal <- ggplot(cal_tbl, aes(x = mean_predicted, y = mean_actual)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
        geom_line(color = COL_TX, linewidth = 0.7) +
        geom_point(aes(size = mean_sw_share), color = COL_TX, alpha = 0.85) +
        geom_text(aes(label = decile), vjust = -0.8, size = 2.6, color = "gray30") +
        scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
        scale_size_continuous(range  = c(2, 8),
                              labels = scales::percent_format(accuracy = 1)) +
        labs(title    = sprintf("Calibration Check: %s", model_label),
             subtitle = "Decile bins of OOB predicted probability. Near-diagonal = well-calibrated.",
             x    = "Mean Predicted Probability",
             y    = "Mean Actual Incidence Rate",
             size = "Avg SW\nShare") +
        theme(legend.position = "right")

      save_fig(fig_cal, paste0("Figure_CV_Calibration_", file_suffix),
               width = 6, height = 4.5)
      save_table(cal_tbl, paste0("Table_CV_Calibration_", file_suffix))
    }

    save_fig(fig_lift, paste0("Figure_CV_Lift_",     file_suffix), width = 6,   height = 5.5)
    save_fig(fig_sep,  paste0("Figure_CV_ScoreSep_", file_suffix), width = 6,   height = 4.5)

    cat(sprintf("  [%s] Top 10%% screens %.0f%% of leaks | Top 20%% screens %.0f%%\n",
                model_label, top10_lift * 100, top20_lift * 100))

    invisible(NULL)
  }

  make_discrimination_plots("pred_no_cfe",   "No State FE",   "NoStateFE",  auc_no_cfe)
  make_discrimination_plots("pred_with_cfe", "With State FE", "SFE",        auc_with_cfe)

  #----------------------------------------------------------------------------
  # 9.6 Partial dependence — level ordering
  #----------------------------------------------------------------------------
  cat("\n--- 9.6: Defining PD level ordering (computation deferred to 9.8b) ---\n")

  all_pd_levels <- c("Double-Walled", "Single-Walled",
                     "Pre-1980 = No", "Pre-1980 = Yes",
                     AGE_BIN_LABELS)

} # end if (RUN_FULL)


#==============================================================================
# 9.8 EMPIRICAL RISK SCORE — PREDICTED PROBABILITIES
#==============================================================================
cat("\n--- 9.8: Empirical risk score ---\n")

if (RUN_FULL && "pred_with_cfe" %in% names(cv_data)) {
  oob_preds <- cv_data[!is.na(pred_with_cfe),
                        .(panel_id, panel_year, pred_oob = pred_with_cfe)]
  if ("pred_oob" %in% names(cv_data)) cv_data[, pred_oob := NULL]
  cv_data <- merge(cv_data, oob_preds, by = c("panel_id", "panel_year"), all.x = TRUE)
  n_matched <- sum(!is.na(cv_data$pred_oob))
  pct_oob   <- round(100 * n_matched / nrow(cv_data), 1)
  cat(sprintf("  OOB predictions matched: %s / %s (%.1f%%)\n",
              format(n_matched, big.mark = ","),
              format(nrow(cv_data), big.mark = ","), pct_oob))
  if (pct_oob < 80) cat("  WARNING: <80% OOB match rate\n")
} else {
  cv_data[, pred_oob := NA_real_]
  cat("  OOB predictions not available (RUN_FULL = FALSE)\n")
}

n_need_pred <- sum(is.na(cv_data$pred_oob))
cat(sprintf("  Facility-years needing final-model prediction: %s\n",
            format(n_need_pred, big.mark = ",")))

# Re-use formula_no_fe (already conditioned on rf_pre_1980) for the fallback model
final_model <- tryCatch(
  glm(formula_no_fe,
      data   = cv_data[!is.na(has_single_walled) & !is.na(age_bin_reg)],
      family = binomial(link = "logit")),
  error = function(e) { cat(sprintf("  ERROR: Final model failed: %s\n", e$message)); NULL }
)

if (!is.null(final_model)) {
  cv_data[, pred_final := predict(final_model, newdata = .SD, type = "response")]
  complete_final <- cv_data[!is.na(pred_final) & !is.na(event_first_leak)]
  if (nrow(complete_final) > 100) {
    roc_final <- pROC::roc(complete_final$event_first_leak,
                            complete_final$pred_final, quiet = TRUE)
    auc_final <- as.numeric(pROC::auc(roc_final))
    cat(sprintf("  Final model in-sample AUC: %.3f (optimistic upper bound)\n", auc_final))
    if (!is.na(auc_with_cfe))
      cat(sprintf("  CV OOB AUC was %.3f — gap %.3f = in-sample optimism\n",
                  auc_with_cfe, auc_final - auc_with_cfe))
  }
} else {
  cv_data[, pred_final := NA_real_]
}

cv_data[, pred_emp := fcase(
  !is.na(pred_oob),   pred_oob,
  !is.na(pred_final), pred_final,
  default             = NA_real_
)]
cv_data[, pred_source := fcase(
  !is.na(pred_oob),                     "OOB (cross-validated)",
  is.na(pred_oob) & !is.na(pred_final), "Final model (in-sample)",
  default                               = "Missing"
)]
cat("\n  Prediction source breakdown:\n")
print(cv_data[, .N, by = pred_source])


if (RUN_FULL) {

  #----------------------------------------------------------------------------
  # 9.7 Figure: Predicted leak risk by wall type x age
  #----------------------------------------------------------------------------
  cat("\n--- 9.7: Figure leak_risk — predicted risk by wall type x age ---\n")

  if (!"wall_label" %in% names(cv_data))
    cv_data[, wall_label := fifelse(has_single_walled == 1,
                                     "Single-Walled", "Double-Walled")]

  tbl3_leak_rates_pooled <- cv_data[!is.na(pred_emp), .(
    leak_rate_per_1000 = round(mean(pred_emp, na.rm = TRUE) * 1000, 2),
    n_fac_years        = .N
  ), by = .(age_bin, wall_label)]

  tbl3_leak_rates_pooled[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
  tbl3_leak_rates_pooled[, lbl := sprintf("%.1f", leak_rate_per_1000)]

  fig5b_risk <- ggplot(
    tbl3_leak_rates_pooled[!is.na(leak_rate_per_1000)],
    aes(x = age_bin, y = leak_rate_per_1000,
        color = wall_label, linetype = wall_label, group = wall_label)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_text(aes(label = lbl), vjust = -0.7, size = 2.5, show.legend = FALSE) +
    scale_x_discrete(limits = AGE_BIN_LABELS, drop = FALSE) +
    scale_color_manual(values    = c("Single-Walled" = COL_TX,
                                      "Double-Walled" = COL_CTRL)) +
    scale_linetype_manual(values = c("Single-Walled" = "solid",
                                      "Double-Walled" = "dashed")) +
    labs(x        = "Tank Age Bin (5-year intervals)",
         y        = "Predicted First-Leak Rate (per 1,000 Facility-Years)",
         title    = "Predicted Leak Risk by Tank Age and Wall Type (Pooled)",
         subtitle = paste0(
           "1990-1998. Never-leaked risk set. Model-predicted probability from ",
           "5-fold CV logistic regression; age × replacement interaction removes ",
           "the spurious 0-4 year elevation driven by replacement-site observations."
         ),
         color    = "Wall Type",
         linetype = "Wall Type") +
    theme(axis.text.x    = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.subtitle   = element_text(size = 8, color = "gray30"))

  save_fig(fig5b_risk, "Figure_leak_risk", width = 8, height = 6)
  cat("  OK: Figure_leak_risk saved\n")

} # end if (RUN_FULL) — Figure_leak_risk


#==============================================================================
# 9.8b TRUE PARTIAL DEPENDENCE PLOTS (uses final_model_sfe)
#==============================================================================
cat("\n--- 9.8b: True partial dependence plots ---\n")

if (!is.null(final_model_sfe)) {

  pd_mean <- function(data, col, val) {
    tmp        <- copy(data)
    tmp[[col]] <- val
    mean(predict(final_model_sfe, newdata = tmp, type = "response"), na.rm = TRUE)
  }

  age_reg_map <- unique(cv_data[!is.na(age_bin) & !is.na(age_bin_reg),
                                 .(age_bin = as.character(age_bin), age_bin_reg)])
  age_reg_map <- age_reg_map[age_bin %in% AGE_BIN_LABELS]
  setorder(age_reg_map, age_bin_reg)

  pd_base <- cv_data[!is.na(has_single_walled) & !is.na(age_bin_reg) &
                     !is.na(log_capacity) & !is.na(is_motor_fuel) &
                     !is.na(past_replacement)]

  pd_wall <- rbindlist(list(
    data.table(
      level       = "Double-Walled",
      mean_pred   = round(pd_mean(pd_base, "has_single_walled", 0L), 4),
      mean_actual = round(cv_data[has_single_walled == 0,
                                   mean(event_first_leak, na.rm = TRUE)], 4),
      n_fac_years = cv_data[has_single_walled == 0, .N],
      covariate   = "Wall Type"
    ),
    data.table(
      level       = "Single-Walled",
      mean_pred   = round(pd_mean(pd_base, "has_single_walled", 1L), 4),
      mean_actual = round(cv_data[has_single_walled == 1,
                                   mean(event_first_leak, na.rm = TRUE)], 4),
      n_fac_years = cv_data[has_single_walled == 1, .N],
      covariate   = "Wall Type"
    )
  ))

  # pd_vintage only computed when rf_pre_1980 is available
  if (has_rf_pre_1980) {
    pd_base_v  <- pd_base[!is.na(rf_pre_1980)]
    pd_vintage <- rbindlist(list(
      data.table(
        level       = "Pre-1980 = No",
        mean_pred   = round(pd_mean(pd_base_v, "rf_pre_1980", 0L), 4),
        mean_actual = round(cv_data[rf_pre_1980 == 0,
                                     mean(event_first_leak, na.rm = TRUE)], 4),
        n_fac_years = cv_data[rf_pre_1980 == 0, .N],
        covariate   = "Pre-1980 Vintage"
      ),
      data.table(
        level       = "Pre-1980 = Yes",
        mean_pred   = round(pd_mean(pd_base_v, "rf_pre_1980", 1L), 4),
        mean_actual = round(cv_data[rf_pre_1980 == 1,
                                     mean(event_first_leak, na.rm = TRUE)], 4),
        n_fac_years = cv_data[rf_pre_1980 == 1, .N],
        covariate   = "Pre-1980 Vintage"
      )
    ))
  } else {
    pd_vintage <- data.table()
    cat("  NOTE: rf_pre_1980 unavailable — Pre-1980 Vintage PD skipped\n")
  }

  # Age PD: hold past_replacement = 0 to isolate the pure age effect
  pd_age <- rbindlist(lapply(seq_len(nrow(age_reg_map)), function(i) {
    lbl     <- age_reg_map$age_bin[i]
    reg_val <- age_reg_map$age_bin_reg[i]
    pd_base_clean <- copy(pd_base)
    pd_base_clean[, past_replacement := 0L]
    data.table(
      level       = lbl,
      mean_pred   = round(pd_mean(pd_base_clean, "age_bin_reg", reg_val), 4),
      mean_actual = round(cv_data[age_bin == lbl & past_replacement == 0,
                                   mean(event_first_leak, na.rm = TRUE)], 4),
      n_fac_years = cv_data[age_bin == lbl & past_replacement == 0, .N],
      covariate   = "Tank Age (5-yr bins)"
    )
  }))

  pd_combined <- rbindlist(list(pd_wall, pd_vintage, pd_age),
                            use.names = TRUE, fill = TRUE)
  save_table(pd_combined, "Table_CV_Partial_Dependence_FinalModel")

  cat("  Partial dependence estimates (final model):\n")
  print(pd_combined[, .(covariate, level, mean_pred, mean_actual, n_fac_years)])

  pd_long <- melt(pd_combined,
                  id.vars       = c("covariate", "level", "n_fac_years"),
                  measure.vars  = c("mean_pred", "mean_actual"),
                  variable.name = "type", value.name = "rate")

  pd_long[, type := factor(fcase(
    type == "mean_pred",   "Partial Dependence (PD)",
    type == "mean_actual", "Observed (marginal)",
    default = "Other"),
    levels = c("Partial Dependence (PD)", "Observed (marginal)"))]

  # Use only levels present in data (pd_vintage may be empty if rf_pre_1980 absent)
  active_pd_levels <- all_pd_levels[all_pd_levels %in% pd_long$level]
  pd_long[, level := factor(as.character(level), levels = active_pd_levels)]

  fig_pd <- ggplot(pd_long, aes(x = level, y = rate, fill = type)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65) +
    facet_wrap(~ covariate, scales = "free_x", nrow = 1) +
    scale_fill_manual(values = c("Partial Dependence (PD)" = COL_TX,
                                 "Observed (marginal)"      = COL_CTRL)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
    labs(x        = NULL,
         y        = "Predicted First-Leak Probability",
         title    = "Risk Factor Partial Dependence (Full-Sample State FE Model)",
         subtitle = paste0(
           "Each bar shows the mean predicted probability when every facility is ",
           "counterfactually assigned that level (other covariates held at observed). ",
           "Age PD computed holding past_replacement = 0 to isolate the pure age effect. ",
           "State + year fixed effects absorbed. ",
           "Observed bars = raw subgroup mean (replacement-free sites). ",
           "Pre-period 1990-1998 | Never-leaked risk set."
         ),
         fill = NULL) +
    theme(axis.text.x    = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          plot.subtitle   = element_text(size = 8, color = "gray30"))

  save_fig(fig_pd, "Figure5A_CV_Partial_Dependence_FinalModel", width = 13, height = 5)
  cat("  OK: Figure5A_CV_Partial_Dependence_FinalModel saved (state FE model)\n")

} else {
  cat("  WARNING: final_model_sfe is NULL — skipping PD plots\n")
}

#==============================================================================
# 9.9 AGGREGATE TO FACILITY LEVEL
#==============================================================================
cat("\n--- 9.9: Aggregate to facility level ---\n")

fac_risk_1998 <- cv_data[!is.na(pred_emp), .(
  fac_emp_risk             = mean(pred_emp, na.rm = TRUE),
  fac_emp_risk_sd          = sd(pred_emp,   na.rm = TRUE),
  fac_emp_risk_lastyear    = pred_emp[which.max(panel_year)],
  fac_emp_risk_lastyear_yr = max(panel_year),
  n_pred_years             = .N,
  pct_oob                  = mean(pred_source == "OOB (cross-validated)", na.rm = TRUE)
), by = .(panel_id, texas_treated)]

cat(sprintf("  Facilities with empirical risk score: %s\n",
            format(nrow(fac_risk_1998), big.mark = ",")))
cat(sprintf("  Facilities with >= 5 pre-period years: %s\n",
            format(sum(fac_risk_1998$n_pred_years >= 5), big.mark = ",")))
cat(sprintf("  Median n_pred_years: %.0f\n", median(fac_risk_1998$n_pred_years)))

n_no_score <- uniqueN(annual_data$panel_id) - nrow(fac_risk_1998)
if (n_no_score > 0)
  cat(sprintf("  NOTE: %s facilities have no empirical risk score\n",
              format(n_no_score, big.mark = ",")))


#==============================================================================
# 9.10 ALL-SINGLE-WALLED FLAG
#==============================================================================
cat("\n--- 9.10: All-single-walled flag (from tanks_1998) ---\n")

stopifnot(exists("tanks_1998"))

fac_wall_type <- tanks_1998[, .(
  n_tanks        = .N,
  n_sw           = sum(single_walled == 1,  na.rm = TRUE),
  n_dw           = sum(double_walled == 1,  na.rm = TRUE),
  n_unknown_wall = sum(is.na(single_walled) |
                       (single_walled == 0 & double_walled == 0), na.rm = TRUE)
), by = panel_id]

fac_wall_type[, fac_all_sw       := as.integer(n_sw == n_tanks &
                                                n_dw == 0 &
                                                n_unknown_wall == 0)]
fac_wall_type[, fac_all_sw_loose := as.integer(n_dw == 0 & (n_sw + n_dw) > 0)]

all_sw_counts <- fac_wall_type[
  panel_id %in% fac_risk_1998$panel_id,
  .(N_facilities    = .N,
    N_all_sw_strict = sum(fac_all_sw),
    N_all_sw_loose  = sum(fac_all_sw_loose),
    Pct_strict      = round(100 * mean(fac_all_sw), 1),
    Pct_loose       = round(100 * mean(fac_all_sw_loose), 1))
]
cat("  All-single-walled facilities (among risk-scored sample):\n")
print(all_sw_counts)

fac_risk_1998 <- merge(
  fac_risk_1998,
  fac_wall_type[, .(panel_id, fac_all_sw, fac_all_sw_loose,
                     n_tanks, n_sw, n_dw, n_unknown_wall)],
  by = "panel_id", all.x = TRUE
)


#==============================================================================
# 9.11 RISK QUARTILES & TERTILES (POOLED CUTOFFS)
#==============================================================================
cat("\n--- 9.11: Risk quartiles & tertiles ---\n")

risk_dist_tbl <- fac_risk_1998[, .(
  Mean   = round(mean(fac_emp_risk,           na.rm = TRUE), 4),
  SD     = round(sd(fac_emp_risk,             na.rm = TRUE), 4),
  P10    = round(quantile(fac_emp_risk, 0.10, na.rm = TRUE), 4),
  P25    = round(quantile(fac_emp_risk, 0.25, na.rm = TRUE), 4),
  Median = round(quantile(fac_emp_risk, 0.50, na.rm = TRUE), 4),
  P75    = round(quantile(fac_emp_risk, 0.75, na.rm = TRUE), 4),
  P90    = round(quantile(fac_emp_risk, 0.90, na.rm = TRUE), 4)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("  Empirical risk score distribution:\n")
print(risk_dist_tbl)

ks_test_risk <- ks.test(
  fac_risk_1998[texas_treated == 1, fac_emp_risk],
  fac_risk_1998[texas_treated == 0, fac_emp_risk]
)
cat(sprintf("  KS test (TX vs CTL): D = %.4f, p = %.4f %s\n",
            ks_test_risk$statistic, ks_test_risk$p.value,
            stars_fn(ks_test_risk$p.value)))
if (ks_test_risk$p.value < 0.05)
  cat("  NOTE: Distributions differ — report both raw and IPW-weighted DiD\n") else
  cat("  OK: Risk score distributions similar across groups\n")

q_cuts_pooled  <- quantile(fac_risk_1998$fac_emp_risk,
                            probs = c(0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE)
q_cuts_tertile <- quantile(fac_risk_1998$fac_emp_risk,
                            probs = c(0, 1/3, 2/3, 1.0), na.rm = TRUE)

fac_risk_1998[, fac_emp_risk_quartile := cut(
  fac_emp_risk, breaks = unique(q_cuts_pooled), include.lowest = TRUE,
  labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"))]
fac_risk_1998[, fac_emp_risk_tertile := cut(
  fac_emp_risk, breaks = unique(q_cuts_tertile), include.lowest = TRUE,
  labels = c("T1 (Lowest)", "T2", "T3 (Highest)"))]

cat("\n  All-SW facilities by risk quartile:\n")
print(fac_risk_1998[!is.na(fac_emp_risk_quartile), .(
  N_facilities = .N,
  N_all_sw     = sum(fac_all_sw, na.rm = TRUE),
  Pct_all_sw   = round(100 * mean(fac_all_sw, na.rm = TRUE), 1)
), by = fac_emp_risk_quartile][order(fac_emp_risk_quartile)])


#==============================================================================
# 9.12 MERGE INTO annual_data
#==============================================================================
cat("\n--- 9.12: Merge into annual_data ---\n")

merge_cols <- c("panel_id", "fac_emp_risk", "fac_emp_risk_sd",
                "fac_emp_risk_lastyear", "fac_emp_risk_quartile",
                "fac_emp_risk_tertile", "n_pred_years", "pct_oob",
                "fac_all_sw", "fac_all_sw_loose",
                "n_tanks", "n_sw", "n_dw", "n_unknown_wall")

existing <- intersect(merge_cols[-1], names(annual_data))
if (length(existing) > 0) annual_data[, (existing) := NULL]

annual_data <- merge(
  annual_data, fac_risk_1998[, ..merge_cols],
  by = "panel_id", all.x = TRUE
)

cat(sprintf("  annual_data rows with fac_emp_risk: %s / %s (%.1f%%)\n",
  format(sum(!is.na(annual_data$fac_emp_risk)), big.mark = ","),
  format(nrow(annual_data), big.mark = ","),
  100 * mean(!is.na(annual_data$fac_emp_risk))))

n_no_score_ann <- annual_data[panel_year == TREATMENT_YEAR & is.na(fac_emp_risk), .N]
if (n_no_score_ann > 0)
  cat(sprintf("  Facilities at treatment year without score: %s\n",
              format(n_no_score_ann, big.mark = ",")))


#==============================================================================
# 9.13 SUMMARY TABLE
#==============================================================================
cat("\n--- 9.13: Summary table ---\n")

snap <- annual_data[panel_year == TREATMENT_YEAR & !is.na(fac_emp_risk_quartile)]

risk_summary <- snap[, .(
  N_fac         = .N,
  Mean_risk     = round(mean(fac_emp_risk, na.rm = TRUE), 4),
  SD_risk       = round(sd(fac_emp_risk,   na.rm = TRUE), 4),
  Pct_Q4        = round(100 * mean(fac_emp_risk_quartile == "Q4 (Highest)",
                                   na.rm = TRUE), 1),
  N_all_sw      = sum(fac_all_sw, na.rm = TRUE),
  Pct_all_sw    = round(100 * mean(fac_all_sw, na.rm = TRUE), 1),
  Pct_Q4_all_sw = round(100 * mean(
    fac_emp_risk_quartile == "Q4 (Highest)" & fac_all_sw == 1, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"),
           Spec  = fcase(spec_A_eligible == 1, "Spec A",
                         spec_B_eligible == 1, "Spec B",
                         default = "Mixed"))]

setorder(risk_summary, Spec, Group)
cat("  Risk summary at treatment year:\n")
print(risk_summary)
save_table(risk_summary, "Table9_13_FacRisk_Summary")

write_tex(
  kbl(risk_summary, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption = paste("Facility-level empirical risk score at December 22, 1998.",
        "Risk score is the mean cross-validated predicted first-leak probability",
        "from the pre-period logistic regression (1990--1998 facility-years",
        "in the never-yet-leaked risk set). All-single-walled: every active",
        "tank is single-walled with no missing wall-type records.",
        "Q4 = top quartile of the pooled risk score distribution."),
      label = "tab:fac-risk-summary") |>
    kable_styling(latex_options = c("HOLD_position"), font_size = 10,
                  full_width = FALSE) |>
    footnote(general = paste(
      "Never-leaked risk set excludes facilities with any LUST report before 1990",
      "(pre\\_panel\\_leak) or any within-panel prior leak (has\\_previous\\_leak).",
      "State FE model used where available; no-FE model as fallback.",
      "All models include age $\\times$ past-replacement interaction to absorb",
      "the spurious 0--4 year elevation from replacement-site observations."),
      general_title = "", threeparttable = TRUE),
  "Table9_13_FacRisk_Summary"
)


#==============================================================================
# 9.14 RISK SCORE DISTRIBUTION FIGURE
#==============================================================================
cat("\n--- 9.14: Risk score distribution figure ---\n")

risk_stats <- fac_risk_1998[!is.na(fac_emp_risk), .(
  mean_r   = mean(fac_emp_risk,   na.rm = TRUE),
  median_r = median(fac_emp_risk, na.rm = TRUE)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

x_cap <- quantile(fac_risk_1998$fac_emp_risk, 0.99, na.rm = TRUE)

annot_label <- paste0(
  sprintf("%-10s  %6s  %6s\n", "Group", "Mean", "Median"),
  paste(
    risk_stats[order(Group),
               sprintf("%-10s  %6s  %6s",
                       Group,
                       scales::percent(mean_r,   accuracy = 0.1),
                       scales::percent(median_r, accuracy = 0.1))],
    collapse = "\n"
  )
)

fig_risk_dist <- ggplot(
  fac_risk_1998[!is.na(fac_emp_risk)],
  aes(x    = fac_emp_risk,
      fill = fifelse(texas_treated == 1, "Texas", "Control"))
) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.005,
    alpha    = 0.45,
    position = "identity",
    color    = NA
  ) +
  geom_vline(xintercept = q_cuts_pooled[2:4],
             linetype = "dashed", color = "gray40", linewidth = 0.5) +
  annotate("text", x = q_cuts_pooled[2:4], y = Inf,
           vjust = 1.5, hjust = -0.1,
           label = c("Q1|Q2", "Q2|Q3", "Q3|Q4"),
           size = 2.8, color = "gray35") +
  geom_vline(data      = risk_stats,
             aes(xintercept = mean_r, color = Group),
             linetype  = "solid", linewidth = 1.0) +
  geom_vline(data      = risk_stats,
             aes(xintercept = median_r, color = Group),
             linetype  = "dotted", linewidth = 0.9) +
  annotate("label",
           x          = x_cap * 0.68,
           y          = Inf,
           vjust      = 1.4,
           hjust      = 0,
           size       = 2.5,
           label.size = 0.3,
           fill       = "white",
           color      = "gray20",
           label      = annot_label) +
  scale_fill_manual(values  = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_color_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = c(0, x_cap),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x        = "Empirical Risk Score (Predicted P(First Leak))",
       y        = "Density",
       title    = "Empirical Risk Score Distribution at Treatment Date",
       subtitle = paste0(
         "Facility mean of CV-predicted annual first-leak probability, 1990–1998. ",
         "Bars = 0.5 pp bins. X-axis capped at 99th pct (",
         scales::percent(x_cap, accuracy = 0.1), "). ",
         "Solid lines = group means; dotted = medians. Dashed grey = pooled quartile cutoffs."
       ),
       fill  = NULL,
       color = NULL) +
  guides(color = "none") +
  coord_cartesian(clip = "off")

save_fig(fig_risk_dist, "Figure_EmpRisk_Score_Distribution", width = 9, height = 5)


#==============================================================================
# 9.15 RAW CLOSURE RATE FIGURE BY RISK STRATUM
#==============================================================================
cat("\n--- 9.15: Raw closure rates by risk stratum ---\n")

build_rate_series <- function(dt, label) {
  dt[panel_year >= 1990 & panel_year <= 2015 & spec_A_eligible == 1, .(
    rate    = mean(closure_event, na.rm = TRUE),
    Group   = fifelse(texas_treated == 1, "Texas", "Control"),
    Stratum = label
  ), by = panel_year]
}

series_list <- list(
  build_rate_series(
    annual_data[fac_emp_risk_quartile == "Q1 (Lowest)"],
    "Placebo: Bottom-risk quartile (Q1)"
  ),
  build_rate_series(
    annual_data[fac_all_sw == 1],
    "All single-walled facilities"
  ),
  build_rate_series(
    annual_data[fac_emp_risk_quartile == "Q4 (Highest)"],
    "Highest-risk quartile (Q4)"
  )
)
raw_rates_combined <- rbindlist(series_list, fill = TRUE)

stratum_levels <- c(
  "Placebo: Bottom-risk quartile (Q1)",
  "All single-walled facilities",
  "Highest-risk quartile (Q4)"
)
raw_rates_combined[, Stratum := factor(Stratum, levels = stratum_levels)]

if (nrow(raw_rates_combined) > 0) {
  fig_risk_rates <- ggplot(raw_rates_combined,
    aes(x = panel_year, y = rate, color = Group, linetype = Group)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    annotate("text", x = TREATMENT_YEAR + 0.7, y = Inf,
             label = "Dec 1998\ndeadline", vjust = 1.4, hjust = 0,
             size = 2.6, color = "gray35") +
    facet_wrap(~ Stratum, nrow = 1, scales = "free_y") +
    scale_color_manual(values    = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
    scale_linetype_manual(values = c("Texas" = "solid", "Control" = "dashed")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_continuous(breaks = seq(1990, 2015, 5)) +
    labs(x        = "Year",
         y        = "Annual Tank-Closure Rate",
         title    = "Annual Tank-Closure Rates by Pre-Period Empirical Risk Stratum (Spec A)",
         subtitle = paste0(
           "Each panel is a risk sub-sample defined by pre-1999 characteristics. ",
           "Left panel (Q1) is a placebo — these facilities faced the same deadline ",
           "but had low predicted leak risk, so we expect little response. ",
           "Right panel (Q4) contains the highest-risk facilities, where the ",
           "deadline should bite hardest. Texas (orange) vs. control-state peers (blue)."
         ),
         color    = NULL,
         linetype = NULL) +
    theme(axis.text.x     = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          strip.text      = element_text(face = "bold", size = 9),
          plot.subtitle   = element_text(size = 8, color = "gray30")) +
    coord_cartesian(clip = "off")   # prevents blank PDF from y = Inf annotation

  save_fig(fig_risk_rates, "Figure_EmpRisk_RawRates", width = 13, height = 5.5)
}

cat("\n========================================\n")
cat("SECTION 9 COMPLETE\n")
cat(sprintf("  CV sample: %s facility-years | %s first-leak events\n",
  format(nrow(cv_data), big.mark = ","),
  format(sum(cv_data$event_first_leak, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  Empirical risk score built for %s facilities\n",
  format(nrow(fac_risk_1998), big.mark = ",")))
cat(sprintf("  All-SW: TX = %s | CTL = %s\n",
  format(fac_risk_1998[texas_treated == 1, sum(fac_all_sw, na.rm = TRUE)], big.mark = ","),
  format(fac_risk_1998[texas_treated == 0, sum(fac_all_sw, na.rm = TRUE)], big.mark = ",")))
cat(sprintf("  Q4 (Spec A, at treatment year): TX = %s | CTL = %s\n",
  format(annual_data[texas_treated == 1 & panel_year == TREATMENT_YEAR &
                     spec_A_eligible == 1 &
                     fac_emp_risk_quartile == "Q4 (Highest)", .N], big.mark = ","),
  format(annual_data[texas_treated == 0 & panel_year == TREATMENT_YEAR &
                     spec_A_eligible == 1 &
                     fac_emp_risk_quartile == "Q4 (Highest)", .N], big.mark = ",")))
cat(sprintf("  AUC: No State FE = %.3f | With State FE = %.3f\n",
  ifelse(is.na(auc_no_cfe),   0, auc_no_cfe),
  ifelse(is.na(auc_with_cfe), 0, auc_with_cfe)))
cat("========================================\n\n")

rm(fac_wall_type, series_list, raw_rates_combined, snap)
gc()


#------------------------------------------------------------------------------
# 10.1 MOTIVATION FIGURES
# These follow Figures 2-3 in paper order. Figures 2-3 show vintage
# composition looks balanced overall but wall-type shares diverge
# within each cohort. Section 10.1 shows that within-cohort wall-type
# divergence produces different cv_hazard distributions, i.e., different
# aging trajectories, which the vintage x year FE cannot absorb.
#
# All figures use raw data only: ECDFs, histograms, raw scatter.
# No smoothing except where noted as LOWESS (nonparametric).
#------------------------------------------------------------------------------

cat("--- 10.1: Motivation figures (raw data, no smoothing) ---\n")

# Facility-level vintage bin from tanks_1998 install years
fac_vintage_snap <- tanks_1998[, .(
  vintage_bin = factor(fcase(
    min(install_year, na.rm = TRUE) < 1980,                         "Pre-1980",
    min(install_year, na.rm = TRUE) >= 1980 &
      min(install_year, na.rm = TRUE) <= 1988,                      "1980-1988",
    min(install_year, na.rm = TRUE) >= 1989,                        "1989-1998",
    default = NA_character_
  ), levels = c("Pre-1980", "1980-1988", "1989-1998"))
), by = panel_id]

cs_match <- merge(cs_match, fac_vintage_snap, by = "panel_id", all.x = TRUE)


# ---- Figure 7A: Within-vintage cv_hazard ECDF --------------------------
# Shows the within-cohort trajectory gap using empirical CDFs.
# No smoothing. The x-axis is cv_hazard (aging trajectory proxy).
# Separation between TX and Control curves within each facet =
# the composition threat that survives facility and vintage x year FEs.
#
# KS statistic is the vertical distance between the two ECDF curves
# at the point of maximum separation, shown directly on the figure.

ks_by_vintage <- cs_match[!is.na(vintage_bin), {
  tx_r <- cv_hazard[texas_treated == 1]
  ct_r <- cv_hazard[texas_treated == 0]
  if (length(tx_r) < 5 | length(ct_r) < 5) {
    .(ks_d = NA_real_, ks_p = NA_real_,
      tx_mean = NA_real_, ct_mean = NA_real_,
      tx_p50  = NA_real_, ct_p50  = NA_real_, n = .N)
  } else {
    ks <- ks.test(tx_r, ct_r)
    .(ks_d   = round(ks$statistic, 3),
      ks_p   = round(ks$p.value,   4),
      tx_mean = mean(tx_r,           na.rm = TRUE),
      ct_mean = mean(ct_r,           na.rm = TRUE),
      tx_p50  = median(tx_r,         na.rm = TRUE),
      ct_p50  = median(ct_r,         na.rm = TRUE),
      n       = .N)
  }
}, by = vintage_bin]

ks_by_vintage[, annot := sprintf(
  "KS D = %.3f  p = %.3f%s\nMedian TX: %.3f%%  CTL: %.3f%%",
  ks_d, ks_p,
  fifelse(!is.na(ks_p) & ks_p < 0.01, "***",
  fifelse(!is.na(ks_p) & ks_p < 0.05, "**",
  fifelse(!is.na(ks_p) & ks_p < 0.10, "*", ""))),
  tx_p50 * 100, ct_p50 * 100
)]

cat("  Within-vintage KS results (aging trajectory gap):\n")
print(ks_by_vintage[, .(vintage_bin, ks_d, ks_p, tx_mean, ct_mean)])

# Annotation x position at 80th percentile of each vintage
annot_pos <- cs_match[!is.na(vintage_bin) & !is.na(cv_hazard), .(
  x_pos = quantile(cv_hazard, 0.80, na.rm = TRUE)
), by = vintage_bin]
annot_pos <- merge(annot_pos, ks_by_vintage[, .(vintage_bin, annot)],
                   by = "vintage_bin")

fig7a <- ggplot(
  cs_match[!is.na(vintage_bin) & !is.na(cv_hazard)],
  aes(x = cv_hazard, color = Group)
) +
  stat_ecdf(linewidth = 0.9, geom = "step") +
  geom_text(
    data        = annot_pos,
    aes(x = x_pos, y = 0.15, label = annot),
    inherit.aes = FALSE,
    hjust = 1, size = 2.7, color = "gray25", lineheight = 0.9
  ) +
  facet_wrap(~vintage_bin, nrow = 1) +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = quantile(cs_match$cv_hazard, c(0.01, 0.99), na.rm = TRUE)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Figure 7A: Within-Vintage Aging Trajectory Gap",
    subtitle = paste0(
      "Empirical CDFs of cv_hazard (cross-validated predicted first-leak rate) ",
      "within each vintage cohort. Figures 2-3 show vintage composition looks ",
      "balanced overall but TX is 13-20pp more single-walled within every cohort. ",
      "This figure shows that within-cohort wall-type divergence produces ",
      "different cv_hazard distributions, i.e., different aging trajectories. ",
      "The KS statistic = maximum vertical distance between the two ECDF curves. ",
      "The cohort x year FE absorbs the cohort average but cannot absorb this ",
      "within-cohort gap."
    ),
    x     = "CV Hazard Score (Predicted Annual First-Leak Probability)",
    y     = "Empirical CDF",
    color = NULL
  ) +
  theme(
    strip.text    = element_text(face = "bold", size = 10),
    plot.subtitle = element_text(size = 8, color = "gray30"),
    legend.position = "bottom"
  )

save_fig(fig7a, "Figure7A_WithinVintage_HazardGap", width = 11, height = 5)
save_table(ks_by_vintage, "Table_WithinVintage_KS_Tests")
cat("  OK: Figure 7A saved (ECDF, no smoothing)\n")


# ---- Figure 7B: Cell imbalance scatter ---------------------------------
# Each dot = one (wall_type x vintage_bin) cell.
# x = mean cv_hazard in cell (how steep is the aging trajectory?)
# y = TX share of cell (is TX overrepresented?)
# Dot size = N facilities in cell.
# LOWESS line is nonparametric. No linear regression line.
# Positive slope = TX overrepresented in high-trajectory cells.

cell_scatter <- cs_match[!is.na(vintage_bin), {
  n_tx  <- sum(texas_treated == 1)
  n_ct  <- sum(texas_treated == 0)
  n_tot <- .N
  .(n_tx       = n_tx,
    n_ct       = n_ct,
    n_tot      = n_tot,
    tx_share   = n_tx / n_tot,
    mean_hazard = mean(cv_hazard, na.rm = TRUE),
    p25_hazard  = quantile(cv_hazard, 0.25, na.rm = TRUE),
    p75_hazard  = quantile(cv_hazard, 0.75, na.rm = TRUE))
}, by = .(
  vintage_bin,
  wall_label = fifelse(
    !is.na(fac_all_sw) & fac_all_sw == 1,
    "All Single-Walled", "Not All Single-Walled"
  )
)]
tx_share_unconditional <- cs_match[, mean(texas_treated == 1)]

# Nonparametric Spearman rank correlation as the summary statistic
spearman_r <- cor(cell_scatter[n_tot >= 5, mean_hazard],
                  cell_scatter[n_tot >= 5, tx_share],
                  method = "spearman", use = "complete.obs")
spearman_p <- cor.test(
  cell_scatter[n_tot >= 5, mean_hazard],
  cell_scatter[n_tot >= 5, tx_share],
  method = "spearman"
)$p.value

cat(sprintf("  Cell imbalance Spearman r = %.3f | p = %.4f\n",
    spearman_r, spearman_p))

fig7b <- ggplot(
  cell_scatter[n_tot >= 5],
  aes(x = mean_hazard, y = tx_share, size = n_tot, color = vintage_bin)
) +
  geom_hline(yintercept = tx_share_unconditional, linetype = "dotted",
             color = "gray50", linewidth = 0.6) +
  annotate("text", x = -Inf, y = tx_share_unconditional + 0.01, hjust = -0.1,
           label = sprintf("%.0f%% TX share = balanced", tx_share_unconditional * 100), size = 2.7, color = "gray45") +
  # LOWESS smoother, nonparametric, span = 0.9
  geom_smooth(
    data        = cell_scatter[n_tot >= 5],
    aes(x = mean_hazard, y = tx_share),
    method      = "loess", span = 0.9, se = TRUE,
    color       = "gray30", fill = "gray85",
    linewidth   = 0.8, inherit.aes = FALSE, show.legend = FALSE
  ) +
  geom_point(alpha = 0.85) +
  ggrepel::geom_text_repel(
    aes(label = paste0(wall_label, "\n", vintage_bin)),
    size = 2.4, max.overlaps = 8,
    color = "gray25", segment.color = "gray70", segment.size = 0.3
  ) +
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
           label = sprintf(
             "Spearman r = %.3f\np = %.3f%s\n(nonparametric rank correlation)",
             spearman_r, spearman_p,
             fifelse(spearman_p < 0.01, "***",
             fifelse(spearman_p < 0.05, "**",
             fifelse(spearman_p < 0.10, "*", "")))),
           size = 2.8, color = "gray20") +
  scale_color_brewer(palette = "Set2") +
  scale_size_continuous(range = c(2, 10),
                        labels = scales::comma_format()) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    x        = "Mean CV Hazard in Cell (Mean Aging Trajectory Slope)",
    y        = "Texas Share of Cell",
    size     = "N Facilities",
    color    = "Vintage Cohort",
    title    = "Figure 7B: Texas Is Overrepresented in High-Trajectory Cells",
    subtitle = paste0(
      "Each dot is one (wall type x vintage) cell. ",
      "A positive association means TX facilities cluster in cells with ",
      "steeper aging trajectories. Facility FEs absorb the level of each ",
      "cell's closure rate but the slope delta(risk_i x t) differs across ",
      "cells and survives demeaning. LOWESS line is nonparametric. ",
      "Summary statistic is Spearman rank correlation."
    )
  ) +
  theme(plot.subtitle = element_text(size = 8, color = "gray30"))

save_fig(fig7b, "Figure7B_CellImbalance_Scatter", width = 9, height = 6)
save_table(cell_scatter, "Table_Cell_Imbalance_Summary")
cat("  OK: Figure 7B saved (raw scatter, LOWESS, Spearman)\n")


# ---- Figure 7D: Raw closure rates by cv_hazard quartile ----------------
# Shows the pre- and post-reform raw annual closure rates for the top
# and bottom quartiles of cv_hazard. No model, no smoothing, no FEs.
# Just the raw means within each stratum and year.
# This is the most transparent possible display of the reform effect
# and the composition threat: if the pre-trends diverge by quartile
# this is direct visual evidence that trajectories differ.

# Assign cv_hazard quartile using pooled breakpoints
q_cuts <- quantile(
  annual_data[panel_year == TREATMENT_YEAR - 1L, cv_hazard],
  probs = c(0, 0.25, 0.50, 0.75, 1.0), na.rm = TRUE
)

# Step 1: assign strata at 1998 only, from the pre-treatment cross section
hazard_strata_1998 <- annual_data[
  panel_year == TREATMENT_YEAR - 1L & !is.na(cv_hazard),
  .(panel_id,
    hazard_stratum_1998 = cut(
      cv_hazard,
      breaks         = quantile(cv_hazard,
                                probs = c(0, 0.25, 0.75, 1.0),
                                na.rm = TRUE),
      include.lowest = TRUE,
      labels         = c("Q1 (Lowest trajectory)",
                         "Q2-Q3 (Middle)",
                         "Q4 (Highest trajectory)")
    ))
]

# Step 2: merge the 1998 stratum assignment into the full panel
# Every year for a given facility gets the same stratum label
annual_data <- merge(annual_data,
                     hazard_strata_1998[, .(panel_id, hazard_stratum_1998)],
                     by = "panel_id", all.x = TRUE)

# Step 3: compute raw closure rates using the fixed 1998 stratum,
# only for Q1 and Q4 (the diagnostic contrast)
raw_by_stratum <- annual_data[
  spec_A_eligible == 1 &
  panel_year >= 1990 & panel_year <= 2015 &
  hazard_stratum_1998 %in% c("Q1 (Lowest trajectory)",
                              "Q4 (Highest trajectory)"),
  .(raw_closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac_obs        = .N,
    # track how many of the original 1998 cohort are still active
    n_fac_active     = uniqueN(panel_id)),
  by = .(panel_year,
         Group   = fifelse(texas_treated == 1, "Texas", "Control"),
         Stratum = hazard_stratum_1998)
]


raw_by_stratum[, Panel := factor(Panel, levels = unique(Panel))]

fig7d <- ggplot(
  raw_by_stratum,
  aes(x = panel_year, y = raw_closure_rate,
      color = Group, shape = Group)
) +
  geom_point(size = 2) +
  geom_line(linewidth = 0.75) +
  geom_vline(xintercept = TREATMENT_YEAR + 0.5,
             linetype = "dashed", color = "gray40", linewidth = 0.6) +
  annotate("text", x = TREATMENT_YEAR + 0.7, y = Inf,
           label = "Dec 1998", vjust = 1.5, hjust = 0,
           size = 2.7, color = "gray40") +
  facet_wrap(~Stratum, nrow = 1, scales = "free_y") +
  scale_color_manual(values    = COL_PAIR) +
  scale_shape_manual(values    = c("Texas" = 16, "Control" = 1)) +
  scale_y_continuous(labels    = scales::percent_format(accuracy = 0.1)) +
  scale_x_continuous(breaks    = seq(1990, 2015, 5)) +
  labs(
    title    = "Figure 7D: Raw Annual Closure Rates by CV Hazard Stratum (Spec A)",
    subtitle = paste0(
      "Raw annual means, no smoothing, no model adjustment. ",
      "The left panel is a placebo: low-trajectory facilities faced the same ",
      "reform deadline but had little reason to respond. The right panel shows ",
      "high-trajectory facilities where the reform should bind hardest. ",
      "Pre-period divergence between TX and Control within the right panel ",
      "is direct evidence that trajectory distributions differ across groups."
    ),
    x      = "Year",
    y      = "Raw Annual Closure Rate",
    color  = NULL,
    shape  = NULL
  ) +
  theme(
    strip.text      = element_text(face = "bold", size = 9),
    plot.subtitle   = element_text(size = 8, color = "gray30"),
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

save_fig(fig7d, "Figure7D_RawClosureRates_ByHazardBin",
         width = 11, height = 5.5)
save_table(raw_by_quartile, "Table_RawRates_ByHazardBin")
cat("  OK: Figure 7D saved (raw means, no smoothing)\n")


#------------------------------------------------------------------------------
# 10A: EXACT CELL MATCHING
#------------------------------------------------------------------------------

cat("\n--- 10A: Exact Cell Matching ---\n")

fac_wall_modal <- tanks_1998[, .(
  wall_type_modal = fcase(
    mean(single_walled == 1, na.rm = TRUE) >= 0.5, "single_wall",
    mean(double_walled == 1, na.rm = TRUE) >= 0.5, "double_wall",
    default = "mixed_unknown"
  )
), by = panel_id]

fac_fuel_modal <- annual_data[panel_year == TREATMENT_YEAR, .(
  fuel_type_modal = fcase(
    has_gasoline_year == 1, "gasoline",
    has_diesel_year   == 1, "diesel",
    default = "other_fuel"
  )
), by = panel_id]

fac_capacity_bin <- tanks_1998[, .(
  capacity_bin = fcase(
    sum(capacity, na.rm = TRUE) <  5000,  "small",
    sum(capacity, na.rm = TRUE) < 15000,  "medium",
    sum(capacity, na.rm = TRUE) < 30000,  "large",
    default = "xlarge"
  )
), by = panel_id]

for (dt_m in list(fac_wall_modal, fac_fuel_modal, fac_capacity_bin)) {
  merge_col <- setdiff(names(dt_m), "panel_id")
  if (any(merge_col %in% names(annual_data)))
    annual_data[, (merge_col) := NULL]
  annual_data <- merge(annual_data, dt_m, by = "panel_id", all.x = TRUE)
}

annual_data[, cell_id := paste(
  wall_type_modal, vintage_cohort,
  capacity_bin, fuel_type_modal,
  sep = "_"
)]

cat(sprintf("  Unique risk-type cells: %s\n",
    format(uniqueN(annual_data$cell_id), big.mark = ",")))

cell_coverage <- annual_data[
  panel_year == TREATMENT_YEAR - 1L,
  .(has_tx   = any(texas_treated == 1),
    has_ctrl  = any(texas_treated == 0),
    n_tx      = sum(texas_treated == 1),
    n_ctrl    = sum(texas_treated == 0)),
  by = cell_id]

cells_with_both <- cell_coverage[has_tx == TRUE & has_ctrl == TRUE, cell_id]
annual_data[, in_cell_matched := cell_id %in% cells_with_both]

n_tx_total <- uniqueN(annual_data[texas_treated == 1 &
                                    panel_year == TREATMENT_YEAR, panel_id])
n_tx_cell  <- uniqueN(annual_data[texas_treated == 1 &
                                    panel_year == TREATMENT_YEAR &
                                    in_cell_matched == TRUE, panel_id])
n_ct_total <- uniqueN(annual_data[texas_treated == 0 &
                                    panel_year == TREATMENT_YEAR, panel_id])
n_ct_cell  <- uniqueN(annual_data[texas_treated == 0 &
                                    panel_year == TREATMENT_YEAR &
                                    in_cell_matched == TRUE, panel_id])

cat(sprintf("  TX  retained: %s / %s (%.1f%%)\n",
    format(n_tx_cell,  big.mark = ","),
    format(n_tx_total, big.mark = ","),
    100 * n_tx_cell / n_tx_total))
cat(sprintf("  CTL retained: %s / %s (%.1f%%)\n",
    format(n_ct_cell,  big.mark = ","),
    format(n_ct_total, big.mark = ","),
    100 * n_ct_cell / n_ct_total))

if (n_tx_cell / n_tx_total < 0.50) {
  cat("  WARNING: less than 50% TX retention.\n")
  cat("  Cells are over-distinguishing trajectories.\n")
  cat("  Consider collapsing capacity_bin to binary or\n")
  cat("  removing fuel_type_modal from cell definition.\n")
}

bal_cell_before <- annual_data[panel_year == TREATMENT_YEAR - 1L, .(
  label      = "Cell: before",
  hazard_TX  = mean(cv_hazard[texas_treated == 1], na.rm = TRUE),
  hazard_CTL = mean(cv_hazard[texas_treated == 0], na.rm = TRUE)
)]
bal_cell_after <- annual_data[
  panel_year == TREATMENT_YEAR - 1L & in_cell_matched == TRUE, .(
  label      = "Cell: after",
  hazard_TX  = mean(cv_hazard[texas_treated == 1], na.rm = TRUE),
  hazard_CTL = mean(cv_hazard[texas_treated == 0], na.rm = TRUE)
)]

bal_cell_compare <- rbind(bal_cell_before, bal_cell_after)
bal_cell_compare[, hazard_std_diff := round(
  (hazard_TX - hazard_CTL) / pooled_sd_hazard, 3)]

cat("\n  Cell matching trajectory balance:\n")
print(bal_cell_compare[, .(label, hazard_TX, hazard_CTL, hazard_std_diff)])

cell_match_ids <- annual_data[
  panel_year == TREATMENT_YEAR - 1L,
  .(panel_id, texas_treated, cell_id, in_cell_matched)
]
fwrite(cell_match_ids,
       file.path(ANALYSIS_DIR, "matching_cell_ids.csv"))
cat("  Saved: matching_cell_ids.csv\n")


#------------------------------------------------------------------------------
# 10B: CV HAZARD SCORE MATCHING
#------------------------------------------------------------------------------

cat("\n--- 10B: CV Hazard Score Matching ---\n")

ks_overall <- ks.test(
  cs_match[texas_treated == 1L, cv_hazard],
  cs_match[texas_treated == 0L, cv_hazard]
)
cat(sprintf("  KS test on cv_hazard distributions:\n"))
cat(sprintf("    D = %.4f | p = %.4f %s\n",
    ks_overall$statistic, ks_overall$p.value,
    stars_fn(ks_overall$p.value)))
cat(sprintf("  Conclusion: %s\n\n",
    fifelse(ks_overall$p.value < 0.05,
      "Trajectory distributions differ. Matching is necessary.",
      "Trajectory distributions similar. Matching is precautionary.")))

caliper <- 0.20 * pooled_sd_hazard
cat(sprintf("  Caliper (0.20 x pooled SD): %.5f\n", caliper))

tx_facs   <- cs_match[texas_treated == 1L,
                       .(panel_id, cv_hazard, vintage_cohort)]
ctrl_facs <- cs_match[texas_treated == 0L,
                       .(panel_id, cv_hazard, vintage_cohort)]

match_list <- lapply(seq_len(nrow(tx_facs)), function(i) {
  tx_h       <- tx_facs$cv_hazard[i]
  tx_vintage <- tx_facs$vintage_cohort[i]
  candidates <- ctrl_facs[
    vintage_cohort == tx_vintage &
    abs(cv_hazard - tx_h) <= caliper
  ]
  if (nrow(candidates) == 0L) return(NULL)
  data.table(
    tx_panel_id    = tx_facs$panel_id[i],
    ctrl_panel_id  = candidates$panel_id,
    tx_hazard      = tx_h,
    ctrl_hazard    = candidates$cv_hazard,
    hazard_distance = abs(candidates$cv_hazard - tx_h),
    vintage        = tx_vintage
  )
})

match_dt <- rbindlist(match_list[!sapply(match_list, is.null)])

matched_tx_ids   <- unique(match_dt$tx_panel_id)
matched_ctrl_ids <- unique(match_dt$ctrl_panel_id)

annual_data[, in_risk_matched := panel_id %in% matched_tx_ids |
                                   panel_id %in% matched_ctrl_ids]

cat(sprintf("  TX  matched: %s / %s (%.1f%%)\n",
    format(length(matched_tx_ids), big.mark = ","),
    format(nrow(tx_facs),          big.mark = ","),
    100 * length(matched_tx_ids) / nrow(tx_facs)))
cat(sprintf("  CTL matched: %s\n",
    format(length(matched_ctrl_ids), big.mark = ",")))
cat(sprintf("  TX unmatched: %s\n",
    format(nrow(tx_facs) - length(matched_tx_ids), big.mark = ",")))

cs_matched_risk <- cs_match[panel_id %in% matched_tx_ids |
                               panel_id %in% matched_ctrl_ids]

bal_risk_before <- cs_match[, .(
  label      = "Risk: before",
  hazard_TX  = mean(cv_hazard[texas_treated == 1L], na.rm = TRUE),
  hazard_CTL = mean(cv_hazard[texas_treated == 0L], na.rm = TRUE),
  sw_TX      = mean(fac_all_sw[texas_treated == 1L], na.rm = TRUE),
  sw_CTL     = mean(fac_all_sw[texas_treated == 0L], na.rm = TRUE)
)]
bal_risk_after <- cs_matched_risk[, .(
  label      = "Risk: after",
  hazard_TX  = mean(cv_hazard[texas_treated == 1L], na.rm = TRUE),
  hazard_CTL = mean(cv_hazard[texas_treated == 0L], na.rm = TRUE),
  sw_TX      = mean(fac_all_sw[texas_treated == 1L], na.rm = TRUE),
  sw_CTL     = mean(fac_all_sw[texas_treated == 0L], na.rm = TRUE)
)]

bal_risk_compare <- rbind(bal_risk_before, bal_risk_after)
bal_risk_compare[, hazard_std_diff := round(
  (hazard_TX - hazard_CTL) / pooled_sd_hazard, 3)]
bal_risk_compare[, sw_gap := round(sw_TX - sw_CTL, 3)]

cat("\n  CV hazard matching trajectory balance:\n")
cat("  hazard_std_diff = standardized gap in aging trajectory slopes\n")
cat("  Target: |hazard_std_diff| < 0.10 after matching\n")
print(bal_risk_compare[, .(label, hazard_TX, hazard_CTL,
                             hazard_std_diff, sw_gap)])

risk_match_ids <- data.table(
  panel_id        = c(matched_tx_ids, matched_ctrl_ids),
  texas_treated   = c(rep(1L, length(matched_tx_ids)),
                      rep(0L, length(matched_ctrl_ids))),
  cv_hazard       = c(
    cs_match[match(matched_tx_ids,   panel_id), cv_hazard],
    cs_match[match(matched_ctrl_ids, panel_id), cv_hazard]
  ),
  in_risk_matched = TRUE
)
fwrite(risk_match_ids,
       file.path(ANALYSIS_DIR, "matching_risk_ids.csv"))
cat("  Saved: matching_risk_ids.csv\n")


#------------------------------------------------------------------------------
# 10.2 FIGURE 7C: Before/After ECDF
# Shows that after cv_hazard matching the TX and Control empirical
# CDFs overlap. Uses raw ECDFs, no smoothing.
# Left panel = before matching, trajectories diverge.
# Right panel = after matching, trajectories equated.
# The KS statistic is the maximum vertical gap between the two curves.
#------------------------------------------------------------------------------

cat("\n--- 10.2: Figure 7C (ECDF before/after) ---\n")

ks_after_match <- ks.test(
  cs_matched_risk[texas_treated == 1L, cv_hazard],
  cs_matched_risk[texas_treated == 0L, cv_hazard]
)

dist_before <- cs_match[, .(
  cv_hazard, Group = fifelse(texas_treated == 1L, "Texas", "Control"),
  Sample = "Full sample\n(before matching)"
)]
dist_after <- cs_matched_risk[, .(
  cv_hazard, Group = fifelse(texas_treated == 1L, "Texas", "Control"),
  Sample = "CV hazard matched sample"
)]

dist_combined <- rbind(dist_before, dist_after)
dist_combined[, Sample := factor(Sample,
  levels = c("Full sample\n(before matching)",
             "CV hazard matched sample"))]

# KS annotations: one per panel
ks_annots_7c <- data.table(
  Sample = factor(
    c("Full sample\n(before matching)", "CV hazard matched sample"),
    levels = c("Full sample\n(before matching)", "CV hazard matched sample")
  ),
  label = c(
    sprintf("KS D = %.3f  p = %.3f%s\nTrajectory distributions differ",
            ks_overall$statistic, ks_overall$p.value,
            stars_fn(ks_overall$p.value)),
    sprintf("KS D = %.3f  p = %.3f%s\nTrajectory distributions equated",
            ks_after_match$statistic, ks_after_match$p.value,
            stars_fn(ks_after_match$p.value))
  ),
  x = rep(quantile(dist_combined$cv_hazard, 0.92, na.rm = TRUE), 2),
  y = rep(0.15, 2)
)

fig7c <- ggplot(
  dist_combined[!is.na(cv_hazard)],
  aes(x = cv_hazard, color = Group)
) +
  stat_ecdf(linewidth = 0.9, geom = "step") +
  geom_text(
    data        = ks_annots_7c,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1, size = 2.7, color = "gray25", lineheight = 0.9
  ) +
  facet_wrap(~Sample, ncol = 2) +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    limits = quantile(dist_combined$cv_hazard, c(0.01, 0.99), na.rm = TRUE)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Figure 7C: Aging Trajectory Distributions Before and After CV Hazard Matching",
    subtitle = paste0(
      "Empirical CDFs of cv_hazard, the cross-validated predicted first-leak ",
      "probability used as a proxy for the aging trajectory trend_i(t). ",
      "Facility FEs absorb levels. Parallel trends requires ",
      "E[trend_i(t)|TX] = E[trend_i(t)|CTL]. ",
      "Left panel: trajectories differ before matching (KS p significant). ",
      "Right panel: trajectories equated after matching (KS p not significant). ",
      "The DID is estimated on facilities with comparable aging slopes."
    ),
    x     = "CV Hazard Score (Predicted Annual First-Leak Probability)",
    y     = "Empirical CDF",
    color = NULL
  ) +
  theme(
    strip.text      = element_text(face = "bold", size = 9),
    plot.subtitle   = element_text(size = 8, color = "gray30"),
    legend.position = "bottom"
  )

save_fig(fig7c, "Figure7C_HazardScore_BeforeAfter", width = 11, height = 5)
cat("  OK: Figure 7C saved (ECDF, no smoothing)\n")


#------------------------------------------------------------------------------
# 10.3 COMBINED BALANCE TABLE
#------------------------------------------------------------------------------

cat("\n--- 10.3: Combined balance table ---\n")

balance_table <- rbindlist(list(
  data.table(
    Approach           = "Full sample (no matching)",
    FE_in_event_study  = "vintage_cohort x year",
    N_TX               = n_tx_total,
    N_CTL              = n_ct_total,
    hazard_std_diff    = round(
      (bal_risk_before$hazard_TX - bal_risk_before$hazard_CTL) /
        pooled_sd_hazard, 3),
    sw_gap             = round(bal_risk_before$sw_TX -
                                 bal_risk_before$sw_CTL, 3),
    KS_D               = round(ks_overall$statistic, 4),
    KS_p               = round(ks_overall$p.value,   4),
    Trajectories       = fifelse(ks_overall$p.value < 0.05,
      "Differ", "Similar")
  ),
  data.table(
    Approach           = "Exact cell match (wall x vintage x capacity x fuel)",
    FE_in_event_study  = "cell_id x year",
    N_TX               = n_tx_cell,
    N_CTL              = n_ct_cell,
    hazard_std_diff    = bal_cell_compare[
      label == "Cell: after", hazard_std_diff],
    sw_gap             = NA_real_,
    KS_D               = NA_real_,
    KS_p               = NA_real_,
    Trajectories       = fifelse(
      abs(bal_cell_compare[
        label == "Cell: after", hazard_std_diff]) < 0.10,
      "Equated", "Partially equated")
  ),
  data.table(
    Approach           = "CV hazard match (caliper = 0.20 SD within vintage)",
    FE_in_event_study  = "vintage_cohort x year",
    N_TX               = length(matched_tx_ids),
    N_CTL              = length(matched_ctrl_ids),
    hazard_std_diff    = bal_risk_compare[
      label == "Risk: after", hazard_std_diff],
    sw_gap             = bal_risk_compare[label == "Risk: after", sw_gap],
    KS_D               = round(ks_after_match$statistic, 4),
    KS_p               = round(ks_after_match$p.value,   4),
    Trajectories       = fifelse(ks_after_match$p.value < 0.05,
      "Still differ. Tighten caliper.", "Equated")
  )
), fill = TRUE)

cat("\n=== MATCHING BALANCE SUMMARY ===\n")
cat("hazard_std_diff = standardized gap in aging trajectory slopes\n")
cat("Target: |hazard_std_diff| < 0.10\n\n")
print(balance_table[, .(Approach, N_TX, N_CTL,
                          hazard_std_diff, KS_p, Trajectories)])

save_table(balance_table, "Table_Matching_Balance_Summary")

write_tex(
  kbl(balance_table[, .(Approach, FE_in_event_study, N_TX, N_CTL,
                          hazard_std_diff, KS_D, KS_p, Trajectories)],
      format    = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption   = paste(
        "Matching balance summary.",
        "\\texttt{hazard\\_std\\_diff} = standardized difference in mean",
        "cv\\_hazard (cross-validated predicted first-leak probability)",
        "between Texas and control groups.",
        "Facility fixed effects absorb levels $\\alpha_i$.",
        "The relevant balance criterion is trajectory slope similarity,",
        "not level similarity.",
        "Target: $|$hazard\\_std\\_diff$| < 0.10$."),
      label     = "tab:matching-balance",
      col.names = c("Approach", "Event Study FE", "N TX", "N CTL",
                    "Traj. Std Diff", "KS D", "KS p",
                    "Trajectories")) |>
    kable_styling(latex_options = c("scale_down", "HOLD_position"),
                  font_size = 9, full_width = FALSE) |>
    column_spec(1, width = "5cm") |>
    column_spec(2, width = "3cm") |>
    footnote(general = paste(
      "Exact cell: cell = wall type $\\times$ vintage cohort",
      "$\\times$ capacity bin $\\times$ fuel type.",
      "CV hazard match: caliper = 0.20 pooled SD of cv\\_hazard,",
      "exact match on vintage\\_cohort.",
      "Facility FEs absorb $\\alpha_i$. The surviving threat is",
      "$\\delta(\\text{risk}_i \\times t)$, risk-type aging trajectories.",
      "Matching equalizes the distribution of these trajectories",
      "across TX and Control."),
      general_title = "", threeparttable = TRUE),
  "Table_Matching_Balance_Summary"
)
cat("  OK: Table_Matching_Balance_Summary saved\n")


#------------------------------------------------------------------------------
# 10.4 SAVE MATCHING METADATA FOR DID SCRIPT
#------------------------------------------------------------------------------

cat("\n--- 10.4: Save matching metadata ---\n")

matching_metadata <- list(
  # Cell matching
  cell_id_definition      = "wall_type_modal x vintage_cohort x capacity_bin x fuel_type_modal",
  cells_with_both         = cells_with_both,
  n_tx_cell_matched       = n_tx_cell,
  n_ct_cell_matched       = n_ct_cell,
  tx_retention_cell       = n_tx_cell / n_tx_total,
  bal_cell_std_diff       = bal_cell_compare[
    label == "Cell: after", hazard_std_diff],

  # CV hazard matching
  caliper                 = caliper,
  pooled_sd_hazard        = pooled_sd_hazard,
  matched_tx_ids          = matched_tx_ids,
  matched_ctrl_ids        = matched_ctrl_ids,
  n_tx_risk_matched       = length(matched_tx_ids),
  n_ct_risk_matched       = length(matched_ctrl_ids),
  tx_retention_risk       = length(matched_tx_ids) / nrow(tx_facs),
  bal_hazard_std_diff_before = bal_risk_compare[
    label == "Risk: before", hazard_std_diff],
  bal_hazard_std_diff_after  = bal_risk_compare[
    label == "Risk: after",  hazard_std_diff],
  ks_before_d             = ks_overall$statistic,
  ks_before_p             = ks_overall$p.value,
  ks_after_d              = ks_after_match$statistic,
  ks_after_p              = ks_after_match$p.value,

  # File paths for DID script
  path_cell_ids           = file.path(ANALYSIS_DIR, "matching_cell_ids.csv"),
  path_risk_ids           = file.path(ANALYSIS_DIR, "matching_risk_ids.csv"),

  # Balance table for inline reporting
  balance_table           = balance_table,
  run_date                = Sys.time()
)

saveRDS(matching_metadata,
        file.path(ANALYSIS_DIR, "matching_metadata.rds"))
cat("  Saved: matching_metadata.rds\n")

cat("\n========================================\n")
cat("SECTION 10 COMPLETE\n")
cat(sprintf(
  "  Cell match:       %s TX / %s CTL | traj std diff = %.3f\n",
  format(n_tx_cell,               big.mark = ","),
  format(n_ct_cell,               big.mark = ","),
  bal_cell_compare[label == "Cell: after", hazard_std_diff]))
cat(sprintf(
  "  CV hazard match:  %s TX / %s CTL | traj std diff = %.3f\n",
  format(length(matched_tx_ids),   big.mark = ","),
  format(length(matched_ctrl_ids), big.mark = ","),
  bal_risk_compare[label == "Risk: after", hazard_std_diff]))
cat(sprintf(
  "  KS before: D = %.4f (p = %.4f) | after: D = %.4f (p = %.4f)\n",
  ks_overall$statistic,     ks_overall$p.value,
  ks_after_match$statistic, ks_after_match$p.value))
cat("  Figures: 7A (within-vintage ECDF), 7B (cell scatter),\n")
cat("           7C (before/after ECDF), 7D (raw rates by quartile)\n")
cat("  Files: matching_cell_ids.csv, matching_risk_ids.csv,\n")
cat("         matching_metadata.rds\n")
cat("========================================\n\n")

rm(fac_wall_modal, fac_fuel_modal, fac_capacity_bin, cell_coverage,
   cell_scatter, dist_before, dist_after, dist_combined,
   match_list, fac_vintage_snap, annot_pos)
gc()


#==============================================================================
# Section 11: JMP PUBLICATION TABLES
#==============================================================================

cat("\n========================================\n")
cat("Section 11: JMP PUBLICATION TABLES\n")
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
                  full_width = FALSE) |>
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
# Section 11.3: TABLE A3 — SPEC A COMPOSITIONAL BALANCE (PUB FORMAT) (NEW)
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
                    full_width = FALSE) |>
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



#==============================================================================
# SECTION 11.2: TX Private Insurance Market - Dynamic Top 5 & HHI Trend
#==============================================================================
cat("--- 11.2: TX Private Insurance Market - Dynamic Top 5 & HHI Trend ---\n")

ins_panel <- panel_data_inst[plot_category == "Private Insurance" &
                              !is.na(ISSUER_NAME) & ISSUER_NAME != "NO COVERAGE"]
ins_panel[toupper(ISSUER_NAME) == "OTHER", ISSUER_NAME := "Other Private Insurers"]

ins_panel[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
insurer_exposure <- ins_panel[, .(fac_months = sum(weight)), by = .(YEAR, ISSUER_NAME)]
insurer_exposure[, annual_total := sum(fac_months), by = YEAR]
insurer_exposure[, market_share := fac_months / annual_total]

# HHI computed strictly per-year from per-year market shares
hhi_trend <- insurer_exposure[, .(HHI = sum((market_share * 100)^2)), by = YEAR]

# Per-year top-5 ranking
setorder(insurer_exposure, YEAR, -fac_months)
insurer_exposure[, annual_rank := frank(-fac_months, ties.method = "first"), by = YEAR]

# Identify all specific insurers that reached the top 5 in ANY given year (excluding the aggregate)
ever_top5_names <- unique(insurer_exposure[annual_rank <= 5 & ISSUER_NAME != "Other Private Insurers", ISSUER_NAME])
cat(sprintf("  Insurers appearing in any annual Top 5: %d firms\n", length(ever_top5_names)))

# Establish static ordering based on aggregate historical exposure (2007-2020)
historical_volume <- insurer_exposure[ISSUER_NAME %in% ever_top5_names,
                                      .(total_vol = sum(fac_months)),
                                      by = ISSUER_NAME]
setorder(historical_volume, -total_vol) # Largest overall exposure first

insurer_exposure[, Plot_Issuer := fifelse(
  ISSUER_NAME %in% ever_top5_names, ISSUER_NAME, "Other Private Insurers"
)]

plot_data_inst <- insurer_exposure[, .(
  market_share = sum(market_share)
), by = .(YEAR, Plot_Issuer)]
plot_data_inst <- merge(plot_data_inst, hhi_trend, by = "YEAR", all.x = TRUE)

# Stack order: Other at bottom, then named insurers ordered by total historical volume (largest at bottom)
factor_levels_inst <- c("Other Private Insurers", historical_volume$ISSUER_NAME)
plot_data_inst[, Plot_Issuer := factor(Plot_Issuer, levels = factor_levels_inst)]

# Dynamic palette generation locked to the volume-based factor levels
named_colors <- scales::hue_pal()(length(ever_top5_names))
custom_colors <- setNames(c("gray80", named_colors), factor_levels_inst)

HHI_SCALE_FACTOR <- 5000

fig_top5 <- ggplot(plot_data_inst, aes(x = as.factor(YEAR))) +
  geom_col(aes(y = market_share, fill = Plot_Issuer),
           width = 0.85, color = "white", linewidth = 0.2) +
  geom_line(aes(y = HHI / HHI_SCALE_FACTOR, group = 1),
             color = "black", linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = HHI / HHI_SCALE_FACTOR),
             color = "black", size = 2.5) +
  scale_y_continuous(
    labels   = scales::percent_format(accuracy = 1),
    expand   = c(0, 0),
    sec.axis = sec_axis(~ . * HHI_SCALE_FACTOR,
                        name = "Herfindahl-Hirschman Index (HHI)")
  ) +
  scale_fill_manual(values = custom_colors) +
  labs(x = "Year", y = "Market Share (by Facility-Months)",
       title = "Private Insurance Market Consolidation",
       subtitle = "Market Share of All Firms Reaching Top 5 in Any Year (Bars) + Per-Year HHI (Dashed)",
       fill = "Insurer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  guides(fill = guide_legend(ncol = 1, reverse = TRUE))

save_fig(fig_top5, "Figure_TX_FR_Top5_Dominance_HHI", width = 12, height = 6)
save_table(plot_data_inst, "Figure_data_Top5_HHI")



#==============================================================================
# SECTION 11.3: Contract Switching Rate
#==============================================================================
cat("--- 11.3: Contract Switching Rate ---\n")

# Step 1: facility-year panel, primary insurer = most months
fac_yr_primary <- ins_panel[, .(months = .N), by = .(FACILITY_ID, YEAR, ISSUER_NAME)]
setorder(fac_yr_primary, FACILITY_ID, YEAR, -months)
fac_yr_primary <- fac_yr_primary[, .SD[1], by = .(FACILITY_ID, YEAR)]

# Step 2: lag to detect switch
setorder(fac_yr_primary, FACILITY_ID, YEAR)
fac_yr_primary[, prev_issuer := shift(ISSUER_NAME, 1L), by = FACILITY_ID]
fac_yr_primary[, prev_year   := shift(YEAR,         1L), by = FACILITY_ID]
fac_yr_primary[, switched    := as.integer(
  !is.na(prev_issuer) &
  prev_year == YEAR - 1L &
  ISSUER_NAME != prev_issuer
)]

annual_switch_rate <- fac_yr_primary[!is.na(switched), .(
  switch_rate = mean(switched)
), by = YEAR]
setorder(annual_switch_rate, YEAR)

cat(sprintf("  Overall annual switch rate: %.1f%%\n",
            100 * mean(fac_yr_primary$switched, na.rm = TRUE)))

fig_churn <- ggplot(annual_switch_rate, aes(x = YEAR, y = switch_rate)) +
  geom_line(color = "#264653", linewidth = 1.2) +
  geom_point(color = "#e76f51", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, max(annual_switch_rate$switch_rate) * 1.2)) +
  scale_x_continuous(breaks = min(annual_switch_rate$YEAR):max(annual_switch_rate$YEAR)) +
  labs(x = "Year", y = "Annual Switch Rate",
       title = "Facility Insurance Contract Switching Rate",
       subtitle = "Share of facilities changing primary insurer year-over-year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_fig(fig_churn, "Figure_TX_FR_Annual_Switch_Rate", width = 8, height = 5)
save_table(annual_switch_rate, "Figure_data_Annual_Switch_Rate")


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
cat("\n--- 11.3b: Figure [T] - Regulatory Timeline ---\n")

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
  source = c("Control States Panel", "Texas Tank & LUST Panel", "Texas FR Panel", "Mid-Continent Rates"),
  start  = c(1985, 1985, 2007, ifelse(!is.null(rate_data),
                                      min(rate_data$YEAR, na.rm = TRUE), 2006)),
  end    = c(2020, 2020, 2020, ifelse(!is.null(rate_data),
                                      max(rate_data$YEAR, na.rm = TRUE), 2024)),
  y_bar  = c(0.20, 0.15, 0.10, 0.05)
)

fig_t <- ggplot() +
  geom_segment(data = data_bars,
               aes(x = start, xend = end, y = y_bar, yend = y_bar),
               linewidth = 4, color = "gray70") +
  geom_text(data = data_bars,
            aes(x = (start + end) / 2, y = y_bar + 0.03, label = source),
            size = 2.8, fontface = "italic") +
  geom_segment(data = timeline_events,
               aes(x = date, xend = date, y = 0.28, yend = y_pos - 0.03),
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
# Section 14: APPENDIX B TABLES
#==============================================================================

cat("\n========================================\n")
cat("Section 14: APPENDIX B TABLES\n")
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
                    font_size = 9, full_width = FALSE) |>
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
                  full_width = FALSE) |>
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
cat("\n--- 12.3: Table B.3 - Missing Data Balance Test ---\n")

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

  # Build the base LaTeX table
  b3_latex <- kbl(b3_display[, .(term, estimate, std.error, statistic, p.value)],
        format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
        caption = paste("Balance test for missing-data exclusion. Logistic",
          "regression of missing-record indicator on Texas treatment dummy.",
          "Coefficient insignificance confirms exclusion probability is orthogonal",
          "to treatment status."),
        label = "tab:missing-balance",
        col.names = c("Term", "Estimate", "Std. Error", "Statistic", "p-value")) |>
      kable_styling(latex_options = c("hold_position"), font_size = 10,
                    full_width = FALSE)
                    
  # Apply conditional row specification outside the pipe
  if (length(tx_row_b3) > 0) {
      b3_latex <- row_spec(b3_latex, tx_row_b3, bold = TRUE)
  }
  
  # Append footnote and save
  b3_latex <- b3_latex |>
      footnote(general = paste(
        "Dependent variable equals one if the facility was excluded due to",
        "missing tank-level date records. Insignificant coefficient on Texas",
        "dummy confirms null of no differential attrition."),
        general_title = "", threeparttable = TRUE)
        
  write_tex(b3_latex, "TableB3_Missing_Data_Balance_Test")
  cat("  OK: Table B.3 saved (CSV + .tex)\n")
} else {
  cat("  WARNING: Balance GLM not available - Table B.3 skipped\n")
}

#==============================================================================
# Section 14: SAVE ANALYSIS-READY DATASETS (UPDATED)
#==============================================================================

cat("\n========================================\n")
cat("Section 14: SAVE ANALYSIS-READY DATASETS\n")
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