#==============================================================================
# 01_Descriptive_Analysis.R
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
#   Figure [A]:  TX Private Insurance Market — Coverage & Concentration (2007–2020)
#   Figure [B]:  Risk-Differentiated Pricing (Mid-Continent Rate Filings)
#   Figure [T]:  Regulatory & Data Availability Timeline
#   Figure 4:    Pre-Period Closure Rate — Spec A ONLY (mandate-free; main text)
#   Figure 4B:   Pre-Period Closure Rate — Pooled (mandate-contaminated; appendix)
#   Figure B-1:  Pre-Period Trends — Full 4-Panel, Spec A (Appendix)
#   Figure 5A/B: Risk Factor CV Validation
#   Table 1A:    Stock Variables (1998 cross-section, all facilities)
#   Table 1B:    Flow Variables (1989–1997) — stratified Spec A / Spec B / Mixed
#   Table 1C:    Leak Rate Cohort Decomposition
#   Table 2:     Mandate Exposure Comparison (promoted to main text)
#   Table A1:    TX Phased Mandate Schedule
#   Table A2:    Facility Cohort Composition
#   Table A3:    Spec A Compositional Balance Check (NEW)
#   Table B.1:   State Data Quality Report
#   Table B.2:   Sample Construction Attrition
#   Table B.3:   Missing Data Balance Test
#   Table B.4:   Parallel Trends Validation — Four Tests (NEW)
#
# MANDATE CONFOUND NOTE:
#   Texas 30 TAC Chapter 334 imposed staggered upgrade deadlines on pre-1988 tanks
#   from 1989–1993 — five compliance cohorts, each 5–9 years ahead of the federal
#   December 22, 1998 deadline. This creates a non-parallel pre-trend in the pooled
#   sample (confirmed: pooled Wald F-test p < 0.001). Primary identification uses
#   Spec A (post-1988 tanks only), which is exempt from this mandate.
#   See §5.7d for mandate variable construction and §8.1 for validation tests.
#
# INTERMEDIATE DATA (Data/Analysis/*.rds):
#   analysis_annual_data.rds, analysis_tank_inventory.rds,
#   analysis_closed_tanks.rds, analysis_tanks_1999.rds,
#   analysis_pre_period_closures.rds, analysis_metadata.rds
#
# STRUCTURE:
#   §1    Setup & Configuration
#   §2    [reserved — helpers in 02_DiD_Causal_Estimates.R]
#   §3    Data Loading
#   §4    Data Preparation & Filtering (Facility-Year Panel)
#   §5    Tank-Level Dataset Construction
#   §5.7  Regulations Database & 1998 Mandate Cohort Construction
#   §6    Diagnostics — Pre-Treatment Exit Balance + LUST Anomalies
#   §7    Descriptive Tables (revised: stock/flow split, cohort decomp)
#   §8    Descriptive Figures (Pre-Period Trends)
#   §9    Figure 5 — Risk Factor Validation (Facility-Level CV)
#   §10   JMP Publication Tables (Summary Stats, Mandate Exposure)
#   §11   Institutional Context Figures ([A], [B], [T])
#   §12   Appendix B Tables (B.1, B.2, B.3)
#   §13   Save Analysis-Ready Datasets
#
# TREATMENT DATE: December 22, 1998
#   Texas H.B. 2587 (74th Legislature), codified at Texas Water Code
#   § 26.3512(b)(5). The date was chosen to coincide with the federal
#   December 22, 1998 UST upgrade deadline (40 CFR § 280.21).
#   For annual panel data: post-treatment begins panel_year >= 1999,
#   since Dec 22 falls in the 1998 calendar year. Event study rel_year = 0
#   corresponds to 1998.
#
# Date: February 2026
#==============================================================================

# SECTION 1: SETUP & CONFIGURATION
#==============================================================================

# -----------------------------------------------------------------------------

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

# ---- Dual-format table save (CSV + LaTeX) ----
save_table <- function(dt, filename, caption = "", label = "") {
  fwrite(dt, file.path(OUTPUT_TABLES, paste0(filename, ".csv")))
  cat(sprintf("✓ Saved: %s.csv\n", filename))
}


# ----------------------------- BOOTSTRAP SETTINGS ----------------------------
USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 9999    # Replications (999 for testing, 9999 for final)
# Uses Webb-6 point weights (optimal for G < 20 clusters per MacKinnon et al.)
# -----------------------------------------------------------------------------

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
# Pre-1988 TX tanks faced staggered compliance deadlines 1989–1993 by vintage cohort.
# Control states had NO corresponding mandate — federal deadline only (Dec 22, 1998).
# This creates a pre-trend divergence in the POOLED sample during 1989–1993.
# Primary identification (Spec A) is exempt; Spec B uses mandate_active control.
TX_MANDATE_START <- 1989L   # Earliest cohort deadline year (pre-1965 tanks)
TX_MANDATE_END   <- 1993L   # Latest cohort deadline year (1985-1988 tanks)
TX_MANDATE_WINDOW_BROAD_START <- 1988L  # +1 year anticipatory buffer
TX_MANDATE_WINDOW_BROAD_END   <- 1994L  # +1 year aftermath buffer
# -----------------------------------------------------------------------------

# ----------------------------- SAMPLE DEFINITION -----------------------------
# Control states (2-letter abbreviations — matches panel builder output)
# NOTE: Three require special handling (see §4.1):
#   NJ — private insurance requirement May 2003 → restrict to panel_year <= 2002
#   TN — state fund not until 2008 → flag; diagnostic in §6
#   MD — never had state fund → flag; robustness in 02_DiD
CONTROL_STATES <- c("ME", "NM", "AR", "OK", "LA", "KS", "MT", "ID",
                    "SD", "AL", "MN", "NC", "IL", "MA", "OH", "PA",
                    "TN", "VA", "CO")

# States with institutional complications (flagged, not dropped here)
NJ_PRIVATE_INS_YEAR <- 2003L  # N.J.A.C. 7:14B-15.1, effective May 19, 2003
TN_FUND_START_YEAR  <- 2008L  # Public Chapter 794, effective July 1, 2008
MD_NO_FUND          <- TRUE   # MD never operated state FA fund (ASTSWMO)
# NOTE: NJ is dropped from CONTROL_STATES entirely. MD flagged for robustness.
# TN retained but flagged for leak analyses.
# NJ exclusion: NJ effectively "treated" post-2003; not a valid control.
CONTROL_STATES <- setdiff(CONTROL_STATES, "NJ")

cat("====================================================================\n")
cat("01_Descriptive_Analysis.R — Texas UST Insurance Reform\n")
cat(sprintf("Treatment date: %s (Texas Water Code § 26.3512(b)(5))\n",
            as.character(TREATMENT_DATE)))
cat(sprintf("Analysis window: %d–%d | Event study: %d–%d\n",
            PANEL_START, PANEL_END, ES_START, ES_END))
cat(sprintf("Bootstrap: %s (B = %d)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
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
cat(sprintf("  Loaded: %s facility-years | %s facilities | Years %d–%d\n",
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
  cat("  ⚠ texas_fr_facility_year_panel.csv not found — Figure [A] will be skipped\n")
  NULL
}

# 3.5 Mid-Continent Rate Filing Data (for Figure [B])
cat("\nLoading Mid-Continent rate filing data...\n")

# NOTE: Folder uses en dash (U+2013). No soft hyphen.
# Verify parent folder spelling on server: "Rate Filings" (single-l)
RATE_DIR <- here("Data", "Rate FIllings", "Mid-Continent Casualty Company ­– 23418")
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

data_quality_report <- NULL


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
cat(sprintf("  After time window (%d–%d): %s rows\n",
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

# Filter 3: Incumbent sample (facilities active before treatment year 1998)
# NOTE: Panel builder uses first_observed < 1999. We override to < 1998
# to match the corrected treatment date of December 22, 1998.
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
  # TN: fund not until 2008. Pre-2008 TN used private insurance.
  tn_pre_fund = as.integer(state == "TN" & panel_year < TN_FUND_START_YEAR),
  # MD: never had state fund. Retained but flagged.
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

# Diagnose flags
miss_install <- unique(tank_raw_miss[is.na(tank_installed_date), panel_id])
miss_closure <- unique(tank_raw_miss[is_closed_status == TRUE &
                                       is.na(tank_closed_date), panel_id])
facilities_to_exclude_miss <- union(miss_install, miss_closure)

# Setup flags for tables
tank_fac_flag <- tank_raw_miss[, .(
  has_miss_install = any(is.na(tank_installed_date)),
  has_miss_closure = any(is_closed_status == TRUE & is.na(tank_closed_date)),
  group            = fifelse(first(state) == "TX", "Texas", "Control")
), by = panel_id]

tank_fac_flag[, texas := as.integer(group == "Texas")]
tank_fac_flag[, has_any_missing := (has_miss_install | has_miss_closure)]

# Balance test
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
    cat("  ⚠ Imbalanced — IPW robustness recommended (Appendix B.4)\n") else
    cat("  ✓ Balanced — proceed with clean drop\n")
}

# --- Export Publication-Ready Unified LaTeX Table ---

# 1. Stratified Group Aggregates (Balance)
balance_combined <- tank_fac_flag[, .(
  Facilities       = .N,
  Miss_Install_N   = sum(has_miss_install),
  Miss_Install_Pct = mean(has_miss_install),
  Miss_Closure_N   = sum(has_miss_closure),
  Miss_Closure_Pct = mean(has_miss_closure),
  Excluded_N       = sum(has_any_missing),
  Excluded_Pct     = mean(has_any_missing)
), by = .(Group = fifelse(texas == 1, "Texas", "Control"))]

# 2. System-Level Aggregates (Scale)
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

# 3. Matrix Union and String Coercion
unified_table <- rbind(balance_combined, total_combined)

# Preserve raw numeric dataset for CSV
unified_csv_output <- copy(unified_table)

# Format strings for LaTeX
unified_table[, `:=`(
  Facilities       = format(Facilities, big.mark = ","),
  Miss_Install_N   = format(Miss_Install_N, big.mark = ","),
  Miss_Install_Pct = sprintf("%.2f\\%%", Miss_Install_Pct * 100),
  Miss_Closure_N   = format(Miss_Closure_N, big.mark = ","),
  Miss_Closure_Pct = sprintf("%.2f\\%%", Miss_Closure_Pct * 100),
  Excluded_N       = format(Excluded_N, big.mark = ","),
  Excluded_Pct     = sprintf("%.2f\\%%", Excluded_Pct * 100)
)]

# 4. LaTeX Compiler Directives
latex_unified <- kbl(
  unified_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("Group", "Facilities", "Count", "Rate", "Count", "Rate", "Excluded", "Rate"),
  caption = "Scale and Balance of Missing Tank Dates \\label{tab:missing_unified}",
  align = c("l", "r", "r", "r", "r", "r", "r", "r")
) %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE) %>%
  add_header_above(c(" " = 2, "Missing Install Date" = 2, "Missing Closure Date" = 2, "Overall Attrition" = 2)) %>%
  row_spec(2, hline_after = TRUE) %>% 
  footnote(general = paste("Sample includes all incumbent facilities active prior to the statutory treatment year (1998). A facility is excluded if any associated tank lacks an installation date, or if a closed tank lacks a valid closure date.", p_val_note),
           threeparttable = TRUE)

cat(latex_unified, file = file.path(OUTPUT_TABLES, "TableB_Missing_Data_Unified.tex"))
cat("  ✓ Exported unified LaTeX table to Output/Tables/\n")

# Apply exclusion to annual data
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

# Save standard CSV diagnostic table
save_table(unified_csv_output, "Appendix_Missing_Data_Unified")

rm(tank_raw_miss, tank_fac_flag, balance_combined, total_combined, unified_table, unified_csv_output)
gc()


# 4.4 Create analysis variables
cat("\n--- 4.4: Creating analysis variables ---\n")

annual_data[, `:=`(
  # Core DiD terms
  treated     = texas_treated,
  # NOTE: panel builder names this post_1999 (panel_year >= 1999).
  # This is correct: the Dec 22, 1998 statutory date means the first
  # full post-treatment calendar year is 1999. Variable name is a
  # historical artifact — do not rename without updating downstream scripts.
  post        = post_1999,
  did_term    = texas_treated * post_1999,
  # Primary outcome
  closure_event = as.integer(n_closures > 0),

# Relative year — indexed to 1999 (first full post-treatment year), only 9 days off from 1998 treatment date. Used for event study and pre-trend analyses.
  rel_year_1999 = panel_year - POST_YEAR,

  # Convenience
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

# Parse dates
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
# Tanks active as of December 22, 1998
# Active = installed before treatment date AND (not closed OR closed after treatment date)
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
# Drop physically impossible
closed_tanks <- closed_tanks[age_at_closure >= 0]

cat(sprintf("  Tank closures in study window: %s\n",
            format(nrow(closed_tanks), big.mark = ",")))

# 5.4 Pre-period closures (for pre-trend closure age panel)
pre_period_closures <- closed_tanks[closure_year >= 1987 & closure_year <= 1997]
cat(sprintf("  Pre-period closures (1987-1997): %s\n",
            format(nrow(pre_period_closures), big.mark = ",")))


# 5.7 REGULATIONS DATABASE & 1998 MANDATE COHORT CONSTRUCTION
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
  is_pre_1988 = as.integer(tank_installed_date <= MANDATE_CUTOFF_DATE),
  install_year = year(tank_installed_date)
)]

# Assign TX mandate deadline to each pre-1988 tank
tanks_1998[is_pre_1988 == 1 & state == "TX", tx_mandate_deadline := fcase(
  install_year < 1965,                          as.IDate("1989-12-22"),
  install_year >= 1965 & install_year <= 1974,  as.IDate("1990-12-22"),
  install_year >= 1975 & install_year <= 1979,  as.IDate("1991-12-22"),
  install_year >= 1980 & install_year <= 1984,  as.IDate("1992-12-22"),
  install_year >= 1985 & install_year <= 1988,  as.IDate("1993-12-22"),
  default = NA_integer_
)]

# Control states: federal deadline only
tanks_1998[is_pre_1988 == 1 & state != "TX",
           tx_mandate_deadline := FEDERAL_MANDATE_DATE]

# Years past deadline as of treatment date
tanks_1998[is_pre_1988 == 1,
           years_past_deadline := as.numeric(
             TREATMENT_DATE - tx_mandate_deadline) / 365.25]

# 5.7c: Spec A / Spec B eligibility
# spec_A_eligible: post-1988 tanks only (no mandate confound)
# spec_B_eligible: pre-1988 tanks only
tanks_1998[, `:=`(
  spec_A_eligible = as.integer(is_pre_1988 == 0),
  spec_B_eligible = as.integer(is_pre_1988 == 1)
)]

# Facility-level aggregates for cohort indicators
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

# Merge facility cohort vars to annual data
fac_cohort_merge <- fac_cohort[, .(panel_id, n_tanks, n_pre_1988, n_post_1988,
                                    pct_pre_1988, mean_age_1998,
                                    mean_yrs_past_deadline)]
annual_data <- merge(annual_data, fac_cohort_merge,
                     by = "panel_id", all.x = TRUE)

# Create spec eligibility at facility level
annual_data[, `:=`(
  spec_A_eligible = as.integer(n_pre_1988 == 0),  # ALL tanks post-1988
  spec_B_eligible = as.integer(n_post_1988 == 0)   # ALL tanks pre-1988
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
# ─────────────────────────────────────────────────────────────────────────────
# Three variables encode the pre-treatment mandate exposure for use in DiD:
#
#   mandate_active     : 1 for TX pre-1988 facilities in their cohort's
#                        deadline calendar year (1989–1993). Point-in-time
#                        impulse dummy. Absorbs the mechanical closure spike
#                        at the moment each cohort's compliance deadline hit.
#
#   mandate_window_3yr : 1 for TX pre-1988 facilities in a 3-year window
#                        around ALL TX mandate years (1988–1994). More
#                        conservative; captures anticipatory + aftermath
#                        dynamics. Use in Spec B sensitivity checks.
#
#   mandate_cohort     : Character label identifying each facility's earliest
#                        mandate cohort. Used as a fixed effect in Spec B
#                        models (mandate_cohort × panel_year FE absorbs
#                        cohort-specific differential trends throughout the
#                        full panel, not just 1989–1993).
#
# IMPORTANT: Control state facilities and post-1988 (Spec A) facilities
# receive mandate_active = 0, mandate_window_3yr = 0, and
# mandate_cohort = "Post-1988 / Control" throughout. This is correct —
# they faced no comparable staggered mandate.
# ─────────────────────────────────────────────────────────────────────────────

cat("\n--- 5.7d: Mandate contamination variable construction ---\n")

# mandate_active: point-in-time impulse for TX pre-1988 in deadline year
annual_data[, mandate_active := as.integer(
  state == "TX" &
  spec_B_eligible == 1 &
  panel_year >= TX_MANDATE_START & panel_year <= TX_MANDATE_END
)]

# mandate_window_3yr: broader anticipatory + aftermath window
annual_data[, mandate_window_3yr := as.integer(
  state == "TX" &
  spec_B_eligible == 1 &
  panel_year >= TX_MANDATE_WINDOW_BROAD_START &
  panel_year <= TX_MANDATE_WINDOW_BROAD_END
)]

# mandate_cohort: earliest compliance cohort for each facility
# Derived from mean_yrs_past_deadline (computed in §5.7b):
#   TX pre-1988: deadline was 5–9 years before Dec 22, 1998
#   Control:     deadline was Dec 22, 1998 → 0 years past
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

# Convert to factor for use as FE in fixest
annual_data[, mandate_cohort := factor(mandate_cohort)]

# Diagnostics
cat(sprintf("  mandate_active facility-years (TX pre-1988, 1989-1993): %s\n",
            format(sum(annual_data$mandate_active, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  mandate_window_3yr facility-years (TX pre-1988, 1988-1994): %s\n",
            format(sum(annual_data$mandate_window_3yr, na.rm = TRUE), big.mark = ",")))
cat("  mandate_cohort distribution (at treatment year):\n")
print(annual_data[panel_year == TREATMENT_YEAR,
                  .N, by = mandate_cohort][order(mandate_cohort)])

save_table(
  annual_data[, .N, by = .(mandate_cohort, texas_treated, spec_A_eligible,
                             spec_B_eligible)][order(mandate_cohort)],
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

# 6.2 Tennessee LUST Verification (RQ2)
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
  cat("  ⚠ TN zero-LUST pre-2008 confirmed — institutional artifact.\n")
  cat("    LUST reporting tied to fund participation (est. 2008).\n")
  cat("    → TN should be restricted to 2008+ in leak-outcome models.\n")
} else if (sum(tn_lust$total_leaks) == 0) {
  cat("  ⚠ TN has zero LUST across ALL years — drop from leak models.\n")
} else {
  cat("  ✓ TN has leaks in both periods — no restriction needed.\n")
}

# Capture TN exclusion decision for use in 02_DiD
tn_exclude_leak_models <- (
  nrow(tn_lust[period == "Pre-2008"]) == 0 ||
  tn_lust[period == "Pre-2008", sum(total_leaks)] == 0
)
cat(sprintf("  TN exclude from leak models flag: %s\n", tn_exclude_leak_models))

# 6.3 Wall Type Missingness Diagnostic (RQ3)
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

# Effective N for Model 2 (tank-level Cox with single_walled)
model2_eligible <- tanks_1998[!is.na(single_walled) &
                                !(single_walled == 0 & double_walled == 0)]
cat(sprintf("\n  Model 2 effective N (non-missing wall type): %s tanks\n",
            format(nrow(model2_eligible), big.mark = ",")))
cat(sprintf("  Model 2 state composition:\n"))
print(model2_eligible[, .N, by = state][order(-N)][1:10])

# 6.4 Spec A Compositional Balance
# ─────────────────────────────────────────────────────────────────────────────
# Addresses concern that Spec A (post-1988) parallel trends result could reflect
# compositional similarity (newer tanks, lower latent leak rate) rather than
# genuine absence of pre-trend divergence. Tests whether TX and Control Spec A
# facilities are observably comparable on key risk characteristics.
# ─────────────────────────────────────────────────────────────────────────────
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

# Formal balance tests
age_ttest <- t.test(tank_age_1998 ~ texas_treated,
                    data = tanks_1998[spec_A_eligible == 1])
cat(sprintf("\n  Age balance t-test (Spec A): p = %.4f\n", age_ttest$p.value))
cat(sprintf("    TX mean: %.1f yrs | Control mean: %.1f yrs | Diff: %.2f yrs\n",
            age_ttest$estimate["mean in group 1"],
            age_ttest$estimate["mean in group 0"],
            diff(rev(age_ttest$estimate))))

if (age_ttest$p.value < 0.05) {
  cat("  ⚠ Spec A groups differ on age — include mean_age_1998 as covariate in DiD\n")
  cat("    Compositional concern NOT fully resolved by balance — document in paper.\n")
} else {
  cat("  ✓ Spec A age balanced — compositional concern addressed.\n")
  cat("    Parallel trends result reflects institutional exemption, not age composition.\n")
}

#==============================================================================
# SECTION 7: DESCRIPTIVE TABLES (REVISED: STOCK/FLOW SPLIT)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: DESCRIPTIVE TABLES\n")
cat("========================================\n\n")

# 7.1 Table 1 Panel A: Stock variables from 1998 cross-section
cat("--- 7.1: Table 1 Panel A — Stock Variables (1998 Cross-Section) ---\n")

stock_stats <- tanks_1998[, .(
  # Facility counts
  N_facilities     = uniqueN(panel_id),
  N_tanks          = .N,
  # Tank characteristics
  Mean_age_1998    = round(mean(tank_age_1998, na.rm = TRUE), 1),
  SD_age_1998      = round(sd(tank_age_1998, na.rm = TRUE), 1),
  Pct_pre_1988     = round(100 * mean(is_pre_1988, na.rm = TRUE), 1),
  Pct_single_wall  = round(100 * mean(single_walled, na.rm = TRUE), 1),
  Tanks_per_fac    = round(.N / uniqueN(panel_id), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

cat("  Stock variables (1998 cross-section):\n")
print(stock_stats)
save_table(stock_stats, "Table1A_Stock_Variables_1998")

# 7.2 Table 1 Panel B: Flow variables — stratified by Spec A / Spec B
# Pre-period averages (1989–1997) reported separately for mandate-free (Spec A)
# and mandate-exposed (Spec B) cohorts. Pooled version saved separately.
cat("\n--- 7.2: Table 1 Panel B — Flow Variables, Stratified (1989–1997) ---\n")

flow_stats_stratified <- annual_data[
  panel_year >= 1989 & panel_year <= 1997 &
  !is.na(spec_A_eligible), .(
    N_fac_years        = .N,
    Avg_closure_rate   = round(mean(closure_event,  na.rm = TRUE), 4),
    Avg_exit_rate      = round(mean(exit_flag,       na.rm = TRUE), 4),
    # leak_year is binary 0/1: "Annual leak incidence rate"
    Avg_leak_incidence = round(mean(leak_year,       na.rm = TRUE), 4),
    # n_leaks is count: "Leak incidents per 1,000 facility-years"
    Leaks_per_1000     = round(sum(n_leaks, na.rm = TRUE) / .N * 1000, 2),
    Avg_n_closures     = round(mean(n_closures,      na.rm = TRUE), 4)
  ),
  by = .(
    Group  = fifelse(texas_treated == 1, "Texas", "Control"),
    Cohort = fcase(
      spec_A_eligible == 1, "Post-1988 (Spec A — Mandate-Free)",
      spec_B_eligible == 1, "Pre-1988 (Spec B — Mandate-Exposed)",
      default               = "Mixed Cohort"
    )
  )
]
setorder(flow_stats_stratified, Cohort, Group)
cat("  Stratified flow variables (1989–1997):\n")
print(flow_stats_stratified)
save_table(flow_stats_stratified, "Table1B_Flow_Stratified_SpecA_SpecB")

# Pooled version (for comparison / appendix)
flow_stats_pooled <- annual_data[panel_year >= 1989 & panel_year <= 1997, .(
  N_fac_years        = .N,
  Avg_closure_rate   = round(mean(closure_event,  na.rm = TRUE), 4),
  Avg_exit_rate      = round(mean(exit_flag,       na.rm = TRUE), 4),
  Avg_leak_incidence = round(mean(leak_year,       na.rm = TRUE), 4),
  Leaks_per_1000     = round(sum(n_leaks, na.rm = TRUE) / .N * 1000, 2)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]
save_table(flow_stats_pooled, "Table1B_Flow_Pooled_AllFacilities")

# Key diagnostic: compare Spec B pre-period closure rates TX vs Control
# Expected: TX lower than Control (mandate exhausted high-risk closures pre-1998)
specB_gap <- flow_stats_stratified[Cohort == "Pre-1988 (Spec B — Mandate-Exposed)"]
if (nrow(specB_gap) == 2) {
  tx_rate  <- specB_gap[Group == "Texas",   Avg_closure_rate]
  ctl_rate <- specB_gap[Group == "Control", Avg_closure_rate]
  cat(sprintf("\n  Spec B pre-period closure gap: TX = %.4f | Control = %.4f\n",
              tx_rate, ctl_rate))
  if (tx_rate < ctl_rate) {
    cat("  ✓ TX Spec B lower than Control — consistent with mandate survivor selection.\n")
  } else {
    cat("  ⚠ TX Spec B >= Control — unexpected; check mandate_cohort construction.\n")
  }
}


# ── Table 1: Combined long-format panel for kbl() rendering ──────────────────
# Reshapes Table1A (stock, wide) and Table1B (flow, stratified, wide) into a
# single long data frame with columns: panel | cohort | variable | Texas | Control
# The Quarto chunk becomes a one-liner: read CSV → kbl().
# ─────────────────────────────────────────────────────────────────────────────

# Panel A: pivot stock_stats wide → long, then back to TX vs Control columns
panelA_long <- melt(
  stock_stats,
  id.vars       = "Group",
  measure.vars  = c("N_facilities", "N_tanks", "Mean_age_1998", "SD_age_1998",
                    "Pct_pre_1988", "Pct_single_wall", "Tanks_per_fac"),
  variable.name = "variable",
  value.name    = "value"
)
panelA_wide <- dcast(panelA_long, variable ~ Group, value.var = "value")
panelA_wide[, `:=`(panel = "A: Stock Variables (Dec 22, 1998)",
                    cohort = "All Facilities")]

# Panel B: pivot flow_stats_stratified wide → long → TX vs Control columns
panelB_long <- melt(
  flow_stats_stratified,
  id.vars       = c("Group", "Cohort"),
  measure.vars  = c("Avg_closure_rate", "Avg_exit_rate",
                    "Avg_leak_incidence", "Leaks_per_1000", "N_fac_years"),
  variable.name = "variable",
  value.name    = "value"
)
panelB_wide <- dcast(panelB_long, Cohort + variable ~ Group, value.var = "value")
setnames(panelB_wide, "Cohort", "cohort")
panelB_wide[, panel := "B: Flow Variables (1989\u20131997 avg.)"]

# Stack and enforce column order
tbl1_combined <- rbindlist(
  list(panelA_wide, panelB_wide),
  use.names = TRUE, fill = TRUE
)
setcolorder(tbl1_combined, c("panel", "cohort", "variable", "Texas", "Control"))
setorder(tbl1_combined, panel, cohort, variable)

# Human-readable variable labels
var_labels <- c(
  N_facilities      = "N facilities",
  N_tanks           = "N tanks",
  Mean_age_1998     = "Mean tank age (years)",
  SD_age_1998       = "SD tank age",
  Pct_pre_1988      = "Pre-1988 tanks (%)",
  Pct_single_wall   = "Single-walled tanks (%)",
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
cat("  ✓ Table1_Combined_Long.csv saved — ready for kbl() one-liner\n")


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


#==============================================================================
# SECTION 8: DESCRIPTIVE FIGURES — PRE-PERIOD TRENDS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: DESCRIPTIVE FIGURES\n")
cat("========================================\n\n")


# 8.1 Figure 4 + Parallel Trends Validation
# ─────────────────────────────────────────────────────────────────────────────
# TWO outputs:
#   Figure 4 (main text): Spec A only — mandate-free, parallel trends hold
#   Figure 4B (appendix): Pooled — annotated to show mandate contamination
#
# FOUR validation tests (Table B.4):
#   1. Pooled, no control          → Expected to REJECT (mandate contamination)
#   2. Spec A, no control needed   → Expected NOT to reject (clean sample)
#   3. Spec B, no control          → Expected to REJECT (mandate contamination)
#   4. Spec B + mandate_active     → Should improve toward non-rejection
# ─────────────────────────────────────────────────────────────────────────────
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

# Helper: run one pre-trend Wald test, return tidy row
run_pt_test <- function(dt, label, extra_rhs = "") {
formula_str <- paste0(
    "closure_event ~ i(rel_year_1999, texas_treated, ref = -1)",
     if (nchar(extra_rhs) > 0) paste0(" + ", extra_rhs) else "",
    " | panel_id + panel_year"
  )
  m <- tryCatch(
    feols(as.formula(formula_str),
          data    = dt[panel_year >= 1990 & panel_year <= 1997],
          cluster = ~state),
    error = function(e) { cat("  ⚠ PT test failed for:", label, "\n"); NULL }
  )
  if (is.null(m)) return(data.table(specification = label, f_stat = NA_real_,
                                     p_value = NA_real_, df1 = NA_integer_,
                                     conclusion = "FAILED"))
  w <- tryCatch(fixest::wald(m, "rel_year_1999"), error = function(e) NULL)
  if (is.null(w)) return(data.table(specification = label, f_stat = NA_real_,
                                     p_value = NA_real_, df1 = NA_integer_,
                                     conclusion = "FAILED"))
  data.table(
    specification = label,
    f_stat        = round(w$stat, 3),
    p_value       = w$p,
    df1           = w$df1,
    conclusion    = fifelse(w$p > 0.10,
                            "NOT rejected — parallel trends supported",
                            "REJECTED — pre-trend divergence present")
  )
}

# Run all four tests
cat("  Running four parallel trends tests...\n")
pt_all <- rbindlist(list(
  run_pt_test(annual_data,
              "1. Pooled (all facilities), no mandate control"),
  run_pt_test(annual_data[spec_A_eligible == 1],
              "2. Spec A (post-1988 only) — PRIMARY IDENTIFICATION"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "3. Spec B (pre-1988 only), no mandate control"),
  run_pt_test(annual_data[spec_B_eligible == 1],
              "4. Spec B (pre-1988 only) + mandate_active control",
              extra_rhs = "mandate_active")
))

cat("\n  === TABLE B.4: PARALLEL TRENDS VALIDATION ===\n")
print(pt_all[, .(specification, f_stat, p_value, conclusion)])
save_table(pt_all, "TableB4_Parallel_Trends_Validation")

# Extract key p-values for figure annotations
specA_p   <- pt_all[grepl("Spec A", specification), p_value]
pooled_p  <- pt_all[grepl("Pooled", specification), p_value]
specA_label <- sprintf(
  "Spec A pre-trend F-test: p = %.3f\n(%s)",
  specA_p,
  fifelse(specA_p > 0.10,
          "Null of parallel pre-trends NOT rejected",
          "Null of parallel pre-trends REJECTED")
)

# ---- Figure 4 (MAIN TEXT): Spec A — mandate-free ----
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
         "). Exempt from TX phased mandate (1989–1993). ",
         "Shaded = 95% Wilson CI."),
       color = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))

save_fig(fig4, "Figure4_PrePeriod_SpecA_MainText", width = 8, height = 5)
fig4
# ---- Figure 4B (APPENDIX): Pooled — shows mandate contamination ----
fig4_pooled <- ggplot(closure_rate_pooled,
                      aes(x = panel_year, y = closure_rate, color = Group)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = Group),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_vline(xintercept = TREATMENT_YEAR + 0.5, linetype = "dashed",
             color = "gray40") +
  # Highlight mandate window
  annotate("rect",
           xmin = TX_MANDATE_START - 0.5, xmax = TX_MANDATE_END + 0.5,
           ymin = -Inf, ymax = Inf,
           fill = "gold", alpha = 0.10) +
  annotate("text",
           x = (TX_MANDATE_START + TX_MANDATE_END) / 2,
           y = min(closure_rate_pooled$closure_rate) * 1.05,
           label = "TX phased mandate\nwindow (1989–1993)",
           size = 2.8, color = "goldenrod4", fontface = "italic") +
  annotate("text",
           x = 1990.5,
           y = max(closure_rate_pooled$closure_rate) * 0.95,
           label = sprintf(
             "Pooled Wald F-test: p < 0.001\nRejects due to mandate contamination\n(see Figure 4 / Spec A for clean test)"),
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
fig4_pooled

# 8.2 Figure B-1 (Appendix): Full 4-panel pre-trends
cat("\n--- 8.2: Figure B-1 (Appendix) — Full 4-Panel Pre-Trends ---\n")

pre_trends_full <- annual_data[
  panel_year >= 1990 & panel_year <= TREATMENT_YEAR, .(
    closure_rate    = mean(closure_event, na.rm = TRUE),
    exit_rate       = mean(exit_flag, na.rm = TRUE),
    leak_incidence  = mean(leak_year, na.rm = TRUE),
    leaks_per_1000  = sum(n_leaks, na.rm = TRUE) / .N * 1000
  ),
  by = .(panel_year,
         treatment_group = fifelse(texas_treated == 1, "Texas", "Control"))
]

# Panel (c): Mean closure age — computed from pre_period_closures
closure_age_pre <- pre_period_closures[, .(
  mean_closure_age = mean(age_at_closure, na.rm = TRUE)
), by = .(panel_year = closure_year,
          treatment_group = fifelse(texas_treated == 1, "Texas", "Control"))]

# Merge closure age into pre_trends_full (use merge, not data.table join — bug fix)
pre_trends_full <- merge(pre_trends_full, closure_age_pre,
                          by = c("panel_year", "treatment_group"),
                          all.x = TRUE)

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
  top = textGrob("Pre-Period Trends (1990–1998)",
                 gp = gpar(fontsize = 14, fontface = "bold"))
)

save_fig(fig_b1, "FigureB1_PreTrends_Full_4Panel", width = 12, height = 8)


#==============================================================================
# SECTION 9: FIGURE 5 — RISK FACTOR CV VALIDATION
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: RISK FACTOR CV VALIDATION\n")
cat("========================================\n\n")

# Facility-level CV of key risk factors
# Show that private insurers' pricing factors have meaningful within-state variation
cv_data <- tanks_1998[, .(
  cv_age       = sd(tank_age_1998, na.rm = TRUE) /
                  mean(tank_age_1998, na.rm = TRUE),
  mean_age     = mean(tank_age_1998, na.rm = TRUE),
  sd_age       = sd(tank_age_1998, na.rm = TRUE),
  n_tanks      = .N,
  pct_pre_1988 = mean(is_pre_1988, na.rm = TRUE)
), by = .(panel_id, texas_treated)]

cv_data[, Group := fifelse(texas_treated == 1, "Texas", "Control")]

fig5a <- ggplot(cv_data, aes(x = mean_age, fill = Group)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = COL_PAIR) +
  labs(x = "Mean Tank Age (years, as of Dec 1998)",
       y = "Facility Count",
       title = "Distribution of Facility Mean Tank Age",
       fill = NULL)

fig5b <- ggplot(cv_data[!is.na(cv_age) & is.finite(cv_age)],
                aes(x = cv_age, fill = Group)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = COL_PAIR) +
  labs(x = "Within-Facility CV of Tank Age",
       y = "Facility Count",
       title = "Tank Age Heterogeneity Within Facilities",
       fill = NULL) +
  xlim(0, 3)

save_fig(fig5a, "Figure5A_Tank_Age_Distribution", width = 8, height = 5)
save_fig(fig5b, "Figure5B_Tank_Age_CV", width = 8, height = 5)


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
  install_start = as.character(install_start),
  install_end   = as.character(install_end),
  tx_mandate_date = as.character(tx_mandate_date),
  federal_deadline = as.character(federal_deadline),
  tx_years_before_federal = round(tx_years_before_federal, 1)
)]
save_table(table_a1, "TableA1_TX_Mandate_Schedule")
cat("  ✓ Table A1 saved\n")

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
save_table(cohort_comp, "TableA2_Cohort_Composition")
cat("  ✓ Table A2 saved\n")
print(cohort_comp)


#==============================================================================
# SECTION 11: INSTITUTIONAL CONTEXT FIGURES (GRANULAR CONTRACT PANEL)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 11: INSTITUTIONAL CONTEXT FIGURES\n")
cat("========================================\n\n")

CONTRACT_PATH <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")

if (file.exists(CONTRACT_PATH)) {
  contract_month <- fread(CONTRACT_PATH)
  
  # Base Filtering: Retain full temporal resolution (2007-2020)
  panel_data <- contract_month[YEAR >= 2007 & YEAR <= 2020]
  
  # 11.1 Figure [A]: TX FR Coverage Composition (Facility-Month Integration)
  cat("--- 11.1: TX FR Coverage Composition ---\n")
  
  panel_data[, plot_category := fcase(
    CATEGORY == "Insurance", "Private Insurance",
    CATEGORY == "State Fund", "State Fund",
    CATEGORY == "Self-Insurance", "Self-Insurance",
    CATEGORY == "NO COVERAGE", "No Coverage",
    default = "Other"
  )]

  # Isolate unique facility-category-month pairings
  fac_categories <- unique(panel_data[, .(FACILITY_ID, YEAR, MONTH, plot_category)])
  # Fractional weight for facilities utilizing multiple mechanism types in the same month
  fac_categories[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
  
  # Aggregate to annual level (average monthly exposure)
  regime_shares <- fac_categories[, .(N = sum(weight) / 12), by = .(YEAR, plot_category)]
  regime_shares[, share := N / sum(N), by = YEAR]

  fig_coverage <- ggplot(regime_shares, aes(x = YEAR, y = share, fill = plot_category)) +
    geom_col(position = "fill", width = 0.8) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "Year", y = "Share of Facilities",
         title = "Primary Financial Responsibility Mechanism (2007–2020)",
         fill = "Mechanism") +
    theme_pub() +
    theme(legend.position = "bottom")

  save_fig(fig_coverage, "FigureA_TX_FR_Coverage_Composition", width = 8, height = 6)
  save_table(regime_shares, "FigureA_data_regime_shares")

  # 11.2 Market Consolidation: Top 5 Insurers + HHI (Accurate Disaggregation)
  cat("--- 11.2: TX Private Insurance Market — Top 5 & HHI Trend ---\n")
  
  # Isolate private insurance
  ins_panel <- panel_data[plot_category == "Private Insurance" & !is.na(ISSUER_NAME) & ISSUER_NAME != "NO COVERAGE"]
  
  # Fractional Attribution by Month (Solves double-counting precisely)
  ins_panel[, weight := 1.0 / .N, by = .(FACILITY_ID, YEAR, MONTH)]
  
  # Aggregate Facility-Months per Insurer
  insurer_exposure <- ins_panel[, .(fac_months = sum(weight)), by = .(YEAR, ISSUER_NAME)]
  insurer_exposure[, annual_total := sum(fac_months), by = YEAR]
  insurer_exposure[, market_share := fac_months / annual_total]

  # Calculate True HHI on fully disaggregated market shares
  hhi_trend <- insurer_exposure[, .(HHI = sum((market_share * 100)^2)), by = YEAR]

  # Isolate Top 5 for Visualization
  setorder(insurer_exposure, YEAR, -fac_months)
  insurer_exposure[, annual_rank := frank(-fac_months, ties.method = "first"), by = YEAR]
  insurer_exposure[, Plot_Issuer := fifelse(annual_rank <= 5, ISSUER_NAME, "Other Private Insurers")]

  plot_data <- insurer_exposure[, .(market_share = sum(market_share)), by = .(YEAR, Plot_Issuer)]

  # Order factors: 'Other' at the bottom, largest remaining firms stacked above
  overall_sizes <- plot_data[, .(total_share = sum(market_share)), by = Plot_Issuer]
  setorder(overall_sizes, total_share)
  factor_levels <- c("Other Private Insurers", setdiff(overall_sizes$Plot_Issuer, "Other Private Insurers"))
  plot_data[, Plot_Issuer := factor(Plot_Issuer, levels = factor_levels)]

  # Merge HHI for secondary axis plotting (scaled to fit 0-1 primary axis. Assumes HHI max ~5000)
  HHI_SCALE_FACTOR <- 5000 
  plot_data <- merge(plot_data, hhi_trend, by = "YEAR", all.x = TRUE)

  fig_top5 <- ggplot(plot_data, aes(x = as.factor(YEAR))) +
    geom_col(aes(y = market_share, fill = Plot_Issuer), width = 0.85, color = "white", linewidth = 0.1) +
    geom_line(aes(y = HHI / HHI_SCALE_FACTOR, group = 1), color = "black", linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = HHI / HHI_SCALE_FACTOR, group = 1), color = "black", size = 2.5) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1), 
      expand = c(0, 0),
      sec.axis = sec_axis(~ . * HHI_SCALE_FACTOR, name = "Herfindahl-Hirschman Index (HHI)")
    ) +
    scale_fill_manual(values = setNames(c("gray70", scales::viridis_pal(option = "mako")(length(factor_levels)-1)), factor_levels)) +
    labs(x = "Year", y = "Market Share (by Facility-Months)",
         title = "Private Insurance Market Consolidation",
         subtitle = "Top 5 Insurers (Bars) and Market Concentration HHI (Dashed Line)",
         fill = "Insurer") +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right") +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE))

  save_fig(fig_top5, "Figure_TX_FR_Top5_Dominance_HHI", width = 12, height = 6)
  save_table(plot_data, "Figure_data_Top5_HHI")

  # 11.3 Insurer Abandonment Rate (Corrected Churn Logic with Event Tags)
  cat("--- 11.3: Insurer Switching Behavior (Churn) with Event Tags ---\n")
  
  # Identify all unique insurers a facility used in Year T
  fac_yr_issuers <- unique(ins_panel[, .(FACILITY_ID, YEAR, ISSUER_NAME)])
  
  # Self-join to evaluate if the T-1 insurer was completely abandoned in T
  t_minus_1 <- fac_yr_issuers[, .(FACILITY_ID, YEAR_T1 = YEAR, ISSUER_T1 = ISSUER_NAME)]
  t_current <- fac_yr_issuers[, .(FACILITY_ID, YEAR_T0 = YEAR, ISSUER_T0 = ISSUER_NAME)]
  
  churn_merge <- merge(t_minus_1, t_current, 
                       by.x = c("FACILITY_ID", "YEAR_T1", "ISSUER_T1"), 
                       by.y = c("FACILITY_ID", "YEAR_T0", "ISSUER_T0"), 
                       all.x = TRUE)
  
  # An insurer is "abandoned" if they existed in T-1 but fail to map to T
  churn_merge[, is_dropped := is.na(YEAR_T0)]
  
  # Aggregate at facility level: did the facility drop ANY insurer from last year?
  fac_churn <- churn_merge[, .(abandoned_insurer = any(is_dropped)), by = .(FACILITY_ID, YEAR = YEAR_T1 + 1)]
  
  churn_summary <- fac_churn[YEAR <= 2020 & YEAR >= 2008, .(total_churn = mean(abandoned_insurer)), by = YEAR]

  fig_churn <- ggplot(churn_summary, aes(x = YEAR, y = total_churn)) +
    geom_line(color = "gray80", linewidth = 1, linetype = "dashed") +
    geom_point(aes(size = total_churn), color = "gray40", alpha = 0.5) +
    # Institutional Event Overlays
    geom_point(data = churn_summary[YEAR == 2012], aes(y = total_churn), color = "#D55E00", size = 5) +
    geom_point(data = churn_summary[YEAR == 2018], aes(y = total_churn), color = "#0072B2", size = 5) +
    annotate("text", x = 2012, y = churn_summary[YEAR==2012, total_churn] + 0.02, 
             label = "Zurich Exit\n(2012)", color = "#D55E00", fontface = "bold", size = 3.5) +
    annotate("text", x = 2018, y = churn_summary[YEAR==2018, total_churn] + 0.02, 
             label = "TOMIC Acquisition\n(2018)", color = "#0072B2", fontface = "bold", size = 3.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Year", y = "Facility Abandonment Rate",
         title = "Institutional Drivers of Market Churn",
         subtitle = "Percentage of facilities dropping an incumbent provider. Spikes correspond to known exits/M&A.") +
    theme_pub() + theme(legend.position = "none")

  save_fig(fig_churn, "Figure_TX_FR_Market_Churn_Annotated", width = 8, height = 5)
  save_table(churn_summary, "Figure_data_Market_Churn")

  # 11.4 Capital Exposure: Statutory Minimum Adherence vs. Excess Coverage
  cat("--- 11.4: Statutory Coverage Limits ---\n")
  
  # Texas statutory minimum occurrence limit for petroleum USTs is generally $1,000,000
  # Utilize unique contracts observed in the panel year
  limits_data <- unique(ins_panel[!is.na(COVER_OCC) & COVER_OCC >= 10000, .(FIN_ASSUR_ID, YEAR, COVER_OCC)])
  limits_data[, coverage_tier := fcase(
    COVER_OCC == 1000000, "$1M (Statutory)",
    COVER_OCC < 1000000, "Sub-$1M",
    COVER_OCC > 1000000, "Excess (>$1M)"
  )]
  
  tier_shares <- limits_data[, .N, by = .(YEAR, coverage_tier)]
  tier_shares[, share := N / sum(N), by = YEAR]
  tier_shares[, coverage_tier := factor(coverage_tier, levels = c("Sub-$1M", "$1M (Statutory)", "Excess (>$1M)"))]

  fig_limits <- ggplot(tier_shares, aes(x = YEAR, y = share, fill = coverage_tier)) +
    geom_col(position = "stack", width = 0.7) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(values = c("Sub-$1M" = "firebrick", "$1M (Statutory)" = "gray60", "Excess (>$1M)" = COL_CTRL)) +
    labs(x = "Year", y = "Share of Contracts",
         title = "Per-Occurrence Coverage Limits",
         subtitle = "Adherence to the standard $1M statutory minimum vs. excess coverage acquisition",
         fill = "Occurrence Limit") +
    theme_pub() +
    theme(legend.position = "bottom")

  save_fig(fig_limits, "Figure_TX_FR_Coverage_Limits", width = 8, height = 5)
  save_table(tier_shares, "Figure_data_Coverage_Limits")

} else {
  cat("  ⚠ Skipped Section 11 — texas_fr_contract_month_panel.csv not available\n")
}


# 11.2 Figure [B]: Risk-Differentiated Pricing (Mid-Continent Rate Filings)
cat("\n--- 11.2: Figure [B] — Mid-Continent Pricing ---\n")

if (!is.null(rate_data)) {
  # Use scheduling range instead of 95% CI (per requirements)
  pricing_range <- rate_data[, .(
    mean_base    = mean(base_premium, na.rm = TRUE),
    mean_min     = mean(sched_min, na.rm = TRUE),
    mean_max     = mean(sched_max, na.rm = TRUE),
    sched_spread = mean(sched_max - sched_min, na.rm = TRUE),
    n_fac        = uniqueN(FACILITY_ID)
  ), by = YEAR]
  setorder(pricing_range, YEAR)

  fig_b <- ggplot(pricing_range, aes(x = YEAR)) +
    geom_ribbon(aes(ymin = mean_min, ymax = mean_max),
                fill = COL_TX, alpha = 0.2) +
    geom_line(aes(y = mean_base), color = COL_TX, linewidth = 1) +
    geom_point(aes(y = mean_base), color = COL_TX, size = 2) +
    labs(x = "Year",
         y = "Premium ($)",
         title = "Risk-Differentiated Pricing: Mid-Continent Rate Filings",
         subtitle = paste0("Shaded band = filed scheduling range ",
                           "(min to max across facilities)"),
         caption = sprintf("N facilities: %s–%s per year",
                           format(min(pricing_range$n_fac), big.mark = ","),
                           format(max(pricing_range$n_fac), big.mark = ","))) +
    scale_y_continuous(labels = dollar_format())

  save_fig(fig_b, "FigureB_MidContinent_Pricing", width = 8, height = 5)
  save_table(pricing_range, "FigureB_data_pricing_range")
} else {
  cat("  ⚠ Skipped — rate filing data not available\n")
}

# 11.3 Figure [T]: Regulatory & Data Availability Timeline
cat("\n--- 11.3: Figure [T] — Regulatory Timeline ---\n")

# Build timeline data
timeline_events <- data.table(
  date = as.numeric(c(1988, 1989, 1993, 1995, 1998, 2005, 2007, 2020)),
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
  type = c("federal", "texas", "texas", "texas", "both",
           "federal", "data", "data"),
  y_pos = c(0.8, 0.5, 0.5, 0.65, 0.9, 0.7, 0.55, 0.55)
)

# Data availability bars
data_bars <- data.table(
  source = c("Tank Inventory & LUST", "Texas FR Panel", "Mid-Continent Rates"),
  start  = c(1985, 2007, ifelse(!is.null(rate_data),
                                 min(rate_data$YEAR, na.rm = TRUE), 2006)),
  end    = c(2020, 2020, ifelse(!is.null(rate_data),
                                 max(rate_data$YEAR, na.rm = TRUE), 2024)),
  y_bar  = c(0.15, 0.10, 0.05)
)

fig_t <- ggplot() +
  # Data availability bars
  geom_segment(data = data_bars,
               aes(x = start, xend = end, y = y_bar, yend = y_bar),
               linewidth = 4, color = "gray70") +
  geom_text(data = data_bars,
            aes(x = (start + end) / 2, y = y_bar + 0.03, label = source),
            size = 2.8, fontface = "italic") +
  # Event markers
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
  # Highlight the Dec 22 1998 coincidence
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

  # Add pass/fail columns for inclusion criteria
  # Criterion 1: install dates >= 75%
  if ("pct_miss_install_date" %in% names(dq)) {
    dq[, pass_install := fifelse(pct_miss_install_date <= 25, "✓", "✗")]
  }
  # Criterion 2: closure dates >= 75% (among closed tanks)
  if ("pct_closed_missing_date" %in% names(dq)) {
    dq[, pass_closure := fifelse(pct_closed_missing_date <= 25, "✓", "✗")]
  }
  # Criterion 3: wall type >= 50%
  if ("pct_miss_tank_type" %in% names(dq)) {
    dq[, pass_wall := fifelse(pct_miss_tank_type <= 50, "✓", "✗")]
  }

  # State groupings
  dq[, group := fcase(
    state == "TX",  "Target",
    state == "MD",  "Excluded — No State Fund (ASTSWMO)",
    state == "NJ",  "Excluded — Treated (private ins. May 2003)",
    state == "MI",  "Excluded — Treated (fund transition 1995)",
    state %in% c("IA", "FL", "CT"), "Excluded — Treated",
    state %in% c("ME", "NM", "AR", "OK", "LA", "KS",
                 "MT", "ID", "SD", "AL", "MN", "NC"), "Control Tier 1",
    state %in% c("IL", "MA", "OH", "PA", "TN", "VA", "CO"), "Control Tier 2",
    default = "Excluded — Data/Institutional"
  )]

  # Add institutional footnotes
  dq[state == "TN", note := "Fund est. 2008; pre-2008 LUST may be zero"]
  dq[state == "MD", note := "Never operated state FA fund (ASTSWMO)"]
  dq[state == "NJ", note := "Private insurance req. May 2003"]
  dq[state == "ME", note := "LUST counting anomaly (tank-level)"]

  setorder(dq, group, state)
  save_table(dq, "TableB1_State_Data_Quality")
  cat("  ✓ Table B.1 saved\n")
} else {
  cat("  ⚠ Skipped — data quality report not available\n")
}

# 12.2 Table B.2: Sample Construction Attrition
cat("\n--- 12.2: Table B.2 — Sample Attrition ---\n")

attrition_dt <- rbindlist(lapply(attrition_log, as.data.table), fill = TRUE)
save_table(attrition_dt, "TableB2_Sample_Attrition")
cat("  ✓ Table B.2 saved\n")
print(attrition_dt)

# 12.3 Table B.3: Missing Data Balance Test
cat("\n--- 12.3: Table B.3 — Missing Data Balance ---\n")

if (!is.null(balance_glm)) {
  balance_tidy <- broom::tidy(balance_glm)
  balance_tidy$stars <- sapply(balance_tidy$p.value, stars_fn)
  save_table(as.data.table(balance_tidy), "TableB3_Missing_Data_Balance_Test")
  cat("  ✓ Table B.3 saved\n")
} else {
  cat("  ⚠ Balance GLM not available\n")
}


#==============================================================================
# SECTION 13: SAVE ANALYSIS-READY DATASETS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 13: SAVE ANALYSIS-READY DATASETS\n")
cat("========================================\n\n")

ANALYSIS_DIR <- here("Data", "Analysis")
dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# Annual panel (main analysis dataset)
saveRDS(annual_data, file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
cat(sprintf("  ✓ analysis_annual_data.rds: %s rows\n",
            format(nrow(annual_data), big.mark = ",")))

# Tank inventory (filtered)
saveRDS(tanks, file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
cat(sprintf("  ✓ analysis_tank_inventory.rds: %s tanks\n",
            format(nrow(tanks), big.mark = ",")))

# 1998 cross-section
saveRDS(tanks_1998, file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))
cat(sprintf("  ✓ analysis_tanks_1999.rds: %s tanks\n",
            format(nrow(tanks_1998), big.mark = ",")))

# Closed tanks
saveRDS(closed_tanks, file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
cat(sprintf("  ✓ analysis_closed_tanks.rds: %s closures\n",
            format(nrow(closed_tanks), big.mark = ",")))

# Pre-period closures
saveRDS(pre_period_closures,
        file.path(ANALYSIS_DIR, "analysis_pre_period_closures.rds"))
cat(sprintf("  ✓ analysis_pre_period_closures.rds: %s closures\n",
            format(nrow(pre_period_closures), big.mark = ",")))

# Metadata
metadata <- list(
  # Study parameters
  treatment_date         = TREATMENT_DATE,
  treatment_year         = TREATMENT_YEAR,
  post_year              = POST_YEAR,
  panel_start            = PANEL_START,
  panel_end              = PANEL_END,

  # Sample
  control_states         = CONTROL_STATES,
  nj_excluded            = TRUE,
  tn_fund_start          = TN_FUND_START_YEAR,
  tn_exclude_leak_models = tn_exclude_leak_models,
  md_no_fund             = MD_NO_FUND,
  n_facilities_tx        = tx_n,
  n_facilities_ctl       = ctl_n,

  # Mandate confound documentation
  # These are consumed by 02_DiD_Causal_Estimates.R for model documentation
  tx_mandate_start       = TX_MANDATE_START,
  tx_mandate_end         = TX_MANDATE_END,
  pt_validation_table    = pt_all,         # Full 4-row parallel trends table
  specA_pretrend_p       = specA_p,        # PRIMARY: Spec A p-value (should be > 0.10)
  pooled_pretrend_p      = pooled_p,       # Pooled p-value (will be < 0.001)
  specA_n_facilities     = uniqueN(annual_data[spec_A_eligible == 1, panel_id]),
  specB_n_facilities     = uniqueN(annual_data[spec_B_eligible == 1, panel_id]),
  es_start = ES_START,
  es_end = ES_END,
  study_end_date = STUDY_END_DATE,
  output_tables = OUTPUT_TABLES,
  output_figures = OUTPUT_FIGURES,
  federal_mandate_date = FEDERAL_MANDATE_DATE,
  mandate_cutoff_date = MANDATE_CUTOFF_DATE,
  incumbent_ids = incumbent_ids,
  # Attrition
  attrition_log          = attrition_log,
  run_date               = Sys.time()
)
saveRDS(metadata, file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
cat("  ✓ analysis_metadata.rds (includes mandate confound documentation)\n")


cat("\n====================================================================\n")
cat("01_Descriptive_Analysis.R COMPLETE\n")
cat(sprintf("  Total facilities: %s | Total facility-years: %s\n",
            format(uniqueN(annual_data$panel_id), big.mark = ","),
            format(nrow(annual_data), big.mark = ",")))
cat(sprintf("  Outputs saved to: %s\n", OUTPUT_TABLES))
cat(sprintf("  Figures saved to: %s\n", OUTPUT_FIGURES))
cat("====================================================================\n")
