################################################################################
# 02b_Panel_Build.R
# Texas UST Insurance Reform - Panel and Sample Construction
#
# LEGAL HISTORY NOTE (UST_Legal_History.md):
#   The upgrade mandate phase-in schedule is federal (40 CFR 280.21) and
#   IDENTICAL across all states. Texas 30 TAC 334.44/334.47 mirror it exactly
#   as a condition of State Program Approval.
#
#   Release detection compliance deadlines (280.21 / 334.47(b)(4)):
#     install year <= 1964:   deadline Dec 22, 1989
#     1965-1969:              deadline Dec 22, 1990
#     1970-1974:              deadline Dec 22, 1991
#     1975-1979:              deadline Dec 22, 1992
#     1980-1988:              deadline Dec 22, 1993
#   Spill/overfill (all pre-1989 installs):              Dec 22, 1994
#   Tank integrity + cathodic (existing/pre-1989 installs): Dec 22, 1998
#
# OUTPUTS (Data/Analysis/ - CSV):
#   PANELS:
#     panel_dt.csv               annual tank-year panel (all tank-years 1985-2020)
#     facility_panel.csv         annual facility-year panel (1985-2020)
#     facility_biography.csv     time-invariant facility characteristics
#     facility_make_model_reform.csv  Dec 1998 portfolio snapshot
#     exact_base.csv             exact-date Cox base (one row per tank)
#
#   MATCHED SAMPLES (8 total):
#     matched_tanks_reform_cem.csv    matched_tanks_reform_psm.csv
#     matched_tanks_birth_cem.csv     matched_tanks_birth_psm.csv
#     matched_facs_reform_cem.csv     matched_facs_reform_psm.csv
#     matched_facs_birth_cem.csv      matched_facs_birth_psm.csv
#
#   ID LOOKUPS (8): ids_<unit>_<pool>_<method>.csv
#
#   panel_meta.csv             scalar constants
#
# SECTION ORDER:
#   ---- DATA CONSTRUCTION (S1-S18) ----
#   S1   Setup & Constants
#   S2   Utility Functions (incl. make_annual_diff, run_psm_glmnet)
#   S3   Load Master Tanks
#   S4   Build Annual Tank-Year Panel
#   S5   Add Mandate Controls
#   S6   Define Tank Sample (panel_dt; first-year-churn FLAGGED)
#   S7   Cell Coverage Diagnostics
#   S8   Build Exact-Date Cox Base (first-year-churn FLAGGED)
#   S9   Analysis Sample Construction
#   S10  Tank Reform-Survivor Matching (CEM + PSM)
#   S11  Tank Birth-Cohort Matching (CEM + PSM) + bc_tanks for KM
#   S12  Facility-Year Panel (aggregated from tank_year_panel)
#   S13  Merge External Data (LUST + FR)
#   S14  Assemble Final Facility Panel
#   S15  Facility Reform-Survivor Matching (CEM 4-way + 5-way + PSM)
#   S16  Facility Birth-Cohort Matching (CEM + PSM)
#   S17  Post-Match Variable Construction
#   S18  Save All Outputs
#
#   ---- FIGURES (F1-F13) ----
#   F1   Mandate coverage by vintage and state
#   F2   Mandate response (closure rate in deadline year)
#   F3   Mandate static share vs response
#   F4   Tank population composition (overall + by state + bar versions)
#   F5   Fleet exit-rate diff bars (5-yr + 4-period versions)
#   F6   Cell coverage heatmap
#   F7   Cell size ECDF
#   F8   Cell balance scatter
#   F9   Cell unidentified (2-panel)
#   F10  PSM overlap histograms (4 panels)
#   F11  Birth-cohort KM survival curves
#   F12  Birth-cohort annual diff dot plots
#   F13  Birth-cohort all-vintage overlay difference
################################################################################


#### S1 Setup & Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(glmnet)
  library(survival)
  library(broom)
  library(ggplot2)
  library(scales)
  library(patchwork)
  library(here)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
dir.create(ANALYSIS_DIR,   recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters ----
POST_YEAR    <- 1999L
REFORM_DATE  <- as.IDate("1998-12-22")
REFORM_DAYS  <- as.numeric(as.Date("1998-12-22"))
REFORM_CAL   <- 1999.0          # for KM plot vertical lines
PANEL_START  <- 1985L
PANEL_END    <- 2020L
STUDY_END    <- as.IDate("2020-12-31")

# ---- Federal/Texas release detection compliance deadlines ----
RELEASE_DET_DEADLINE_YR <- list(
  pre_1965 = 1989L,
  y1965_69 = 1990L,
  y1970_74 = 1991L,
  y1975_79 = 1992L,
  y1980_88 = 1993L
)
SPILL_OVERFILL_DEADLINE_YR <- 1994L
INTEGRITY_DEADLINE_YR      <- 1998L

# ---- Study states ----
# PA excluded from STUDY_STATES: PA Act 16 (June 1995) drives a spurious
# 1995 SW closure spike that would bias the closure DiD. PA re-enters the
# panel for pricing/hazard ONLY via PRICING_STATES/PANEL_STATES below
# (ticket 046) -- CONTROL_STATES/STUDY_STATES themselves stay untouched so
# every existing STUDY_STATES-scoped consumer (DiD, matching, Cox base,
# structural sample) is unaffected by construction. See the T046 PA BLOCK
# near S18 for the parallel pricing/hazard-only pipeline.
CONTROL_STATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDY_STATES <- c("TX", CONTROL_STATES)

# States added for pricing/hazard only (ticket 046). NOT part of the
# closure-DiD or structural "observed" sample -- STUDY_STATES-gated code
# above is unaffected since it never reads these.
PRICING_STATES <- c("PA")
PANEL_STATES   <- c(STUDY_STATES, PRICING_STATES)

# ---- Colors ----
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
COL_PRE  <- "#E69F00"
COL_SW   <- "#08519c"
COL_DW   <- "#74c476"
WALL_COLORS <- c("Single-Walled" = COL_SW, "Double-Walled" = COL_DW)


#### S2 Utility Functions ####

log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_pct  <- function(x) sprintf("%.1f%%", x * 100)

theme_ust <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      axis.title        = element_text(size = base_size, face = "bold"),
      axis.text         = element_text(size = base_size - 1L),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "gray90", linewidth = 0.3),
      legend.position   = "bottom",
      legend.title      = element_text(size = base_size - 1L, face = "bold"),
      legend.text       = element_text(size = base_size - 1L),
      plot.margin       = margin(t = 6, r = 10, b = 6, l = 6)
    )
}

# ---- KM annual difference helper ----
# Takes a tidy KM data.table (with columns: time, estimate, std.error, group),
# extracts S_TX(t) and S_CTL(t) at Dec 31 of each calendar year in year_grid
# (step-function lookup: last estimate at or before yr + 0.99...), and returns
# the difference and combined Greenwood SE.
#
# CRITICAL: broom::tidy(survfit_obj) reports std.error as the Greenwood SE of
# the survival function S(t), NOT the variance. To combine independent groups:
# Var(diff) = Var(S_TX) + Var(S_CTL); SE(diff) = sqrt(SE_TX^2 + SE_CTL^2).
make_annual_diff <- function(km_dt, year_grid) {
  step_lookup <- function(g_dt, yr) {
    rec <- g_dt[time <= yr + 0.99]
    if (nrow(rec) == 0L) return(list(est = 1, se = 0))
    last <- rec[.N]
    list(est = last$estimate, se = last$std.error)
  }

  tx_dt  <- km_dt[group == "Texas"]
  ctl_dt <- km_dt[group == "Control States"]
  setorder(tx_dt,  time)
  setorder(ctl_dt, time)

  out <- rbindlist(lapply(year_grid, function(yr) {
    tx  <- step_lookup(tx_dt,  yr)
    ctl <- step_lookup(ctl_dt, yr)
    data.table(
      year    = yr,
      s_tx    = tx$est,
      s_ctl   = ctl$est,
      diff    = tx$est - ctl$est,
      se_diff = sqrt(tx$se^2 + ctl$se^2)
    )
  }))
  out[]
}

# ---- PSM helper: OOF elastic-net + Imbens-Rubin 1:1 Mahalanobis ----
# Used identically across all four PSM samples (tank-reform, tank-birth,
# fac-reform, fac-birth). Returns weights and pscores; caller merges them.
run_psm_glmnet <- function(dt, treat_col,
                           pscore_mm_formula, mahvars_formula, matchit_formula,
                           alpha = 0.5, n_fold = 10L, seed = 42L) {
  X <- sparse.model.matrix(pscore_mm_formula, data = as.data.frame(dt))
  y <- dt[[treat_col]]

  set.seed(seed)
  cv_fit <- cv.glmnet(
    x = X, y = y,
    family       = "binomial",
    alpha        = alpha,
    nfolds       = n_fold,
    type.measure = "deviance"
  )
  lambda_opt <- cv_fit$lambda.min

  set.seed(seed)
  fold_id <- sample(rep(seq_len(n_fold), length.out = nrow(X)))
  pscore_oof <- numeric(nrow(X))
  for (k in seq_len(n_fold)) {
    train_idx <- which(fold_id != k)
    test_idx  <- which(fold_id == k)
    fit_k <- glmnet(
      x = X[train_idx, ], y = y[train_idx],
      family = "binomial", alpha = alpha, lambda = lambda_opt
    )
    pscore_oof[test_idx] <- as.numeric(
      predict(fit_k, newx = X[test_idx, ], s = lambda_opt, type = "response")
    )
  }

  dt_psm <- copy(dt)
  dt_psm[, pscore       := pmax(pmin(pscore_oof, 0.999), 0.001)]
  dt_psm[, logit_pscore := log(pscore / (1 - pscore))]

  ir_caliper <- 0.2 * sd(dt_psm$logit_pscore)

  m_psm <- matchit(
    formula  = matchit_formula,
    data     = as.data.frame(dt_psm),
    method   = "nearest",
    distance = dt_psm$logit_pscore,
    mahvars  = mahvars_formula,
    ratio    = 1L,
    replace  = FALSE,
    caliper  = ir_caliper
  )

  list(
    dt           = dt_psm,
    weights      = m_psm$weights,
    lambda_opt   = lambda_opt,
    ir_caliper   = ir_caliper
  )
}


#### S3 Load Master Tanks ####

cat("\n========================================\n")
cat("S3: LOAD MASTER TANKS\n")
cat("========================================\n\n")

master_tanks <- fread(
  here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("Loaded: %s rows, %d columns", fmt_n(nrow(master_tanks)), ncol(master_tanks)))

master_tanks[, tank_installed_date := as.IDate(tank_installed_date)]
master_tanks[, tank_closed_date    := as.IDate(tank_closed_date)]

# Promote install cohort to integer once; use install_yr_int for all numeric ops
master_tanks[, install_yr_int := as.integer(mm_install_cohort)]

# Treatment assignment
master_tanks[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]

log_step(sprintf("TX=%s  CTL=%s  non-study=%s",
  fmt_n(master_tanks[texas_treated == 1L, .N]),
  fmt_n(master_tanks[texas_treated == 0L, .N]),
  fmt_n(master_tanks[is.na(texas_treated), .N])))

# Composite IDs - facility_id is NOT unique across states
master_tanks[, panel_id      := paste(facility_id, state, sep = "_")]
master_tanks[, tank_panel_id := paste(facility_id, state, tank_id, sep = "_")]

study_tanks <- master_tanks[
  !is.na(texas_treated) &
  !is.na(tank_installed_date) &
  state %in% STUDY_STATES
]
log_step(sprintf("Valid study tanks: %s", fmt_n(nrow(study_tanks))))
cat("\n")


#### S4 Build Annual Tank-Year Panel ####

cat("========================================\n")
cat("S4: BUILD ANNUAL TANK-YEAR PANEL\n")
cat("========================================\n\n")

# Release detection deadline year per tank
study_tanks[, release_det_deadline_yr := fcase(
  install_yr_int <= 1964L,         RELEASE_DET_DEADLINE_YR$pre_1965,
  install_yr_int %in% 1965L:1969L, RELEASE_DET_DEADLINE_YR$y1965_69,
  install_yr_int %in% 1970L:1974L, RELEASE_DET_DEADLINE_YR$y1970_74,
  install_yr_int %in% 1975L:1979L, RELEASE_DET_DEADLINE_YR$y1975_79,
  install_yr_int %in% 1980L:1988L, RELEASE_DET_DEADLINE_YR$y1980_88,
  default = NA_integer_
)]

# Make-model cell variables
study_tanks[, make_model_tank  := paste(mm_wall, mm_fuel, mm_capacity, mm_install_cohort, sep = "|")]
study_tanks[, make_model_noage := paste(mm_wall, mm_fuel, mm_capacity, sep = "|")]
study_tanks[, make_model_3dim  := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "|")]

study_tanks[
  mm_wall == "Unknown-Wall" | mm_fuel == "Unknown-Fuel" | is.na(mm_install_cohort),
  `:=`(make_model_tank  = NA_character_,
       make_model_noage = NA_character_,
       make_model_3dim  = NA_character_)
]

# Expand to annual tank-year panel
log_step(sprintf("Expanding to annual tank-year panel (%d-%d)...", PANEL_START, PANEL_END))

study_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date), year(tank_closed_date), PANEL_END)
)]
study_tanks[, `:=`(
  expand_start = pmax(install_yr, PANEL_START),
  expand_end   = pmin(close_yr_raw, PANEL_END)
)]

# Geographic and continuous columns carried into panel for downstream facility
# aggregation (S12). Selected from the columns commonly present in the
# harmonized master file. If any are missing in your master_tanks, R will
# error here rather than silently dropping.
panel_cols <- c(
  "tank_panel_id", "panel_id", "facility_id", "state", "texas_treated",
  "mm_wall", "mm_fuel", "mm_capacity",
  "mm_install_cohort", "install_yr_int",
  "make_model_tank", "make_model_noage", "make_model_3dim",
  "release_det_deadline_yr",
  "tank_installed_date", "tank_closed_date",
  "expand_start", "expand_end",
  "capacity", "county_name", "county_fips", "latitude", "longitude"
)

tank_year_panel <- study_tanks[
  expand_start <= expand_end, .SD, .SDcols = panel_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(!is.na(tank_closed_date) & year(tank_closed_date) == yrs))
}, by = panel_cols]

setorder(tank_year_panel, tank_panel_id, panel_year)

# First-year-churn FLAG (do not drop). Available downstream for filtering.
tank_year_panel[, first_year_churn := as.integer(
  panel_year == install_yr_int & closure_event == 1L
)]

log_step(sprintf("Panel: %s rows | %s tanks | %s facilities | %s first-year-churn flagged",
  fmt_n(nrow(tank_year_panel)),
  fmt_n(uniqueN(tank_year_panel$tank_panel_id)),
  fmt_n(uniqueN(tank_year_panel$panel_id)),
  fmt_n(sum(tank_year_panel$first_year_churn))))
cat("\n")


#### S5 Add Mandate Controls ####

cat("========================================\n")
cat("S5: ADD MANDATE CONTROLS\n")
cat("========================================\n\n")

tank_year_panel[, `:=`(
  post_1999 = as.integer(panel_year >= POST_YEAR),
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR),
  rel_year  = panel_year - POST_YEAR,
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  tank_age  = panel_year - install_yr_int
)]

# MANDATE 1: Release Detection. Pre-1989 installs only.
tank_year_panel[, mandate_release_det := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year >= 1989L &
  panel_year <= release_det_deadline_yr
)]

# MANDATE 2: Spill/Overfill. Pre-1989 installs only. Deadline Dec 22, 1994.
# 3-year pressure window for symmetry with integrity mandate.
tank_year_panel[, mandate_spill_overfill := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1992L:1994L
)]

# MANDATE 3: Tank Integrity. Existing/pre-1989 installs only. Deadline Dec 22, 1998.
tank_year_panel[, mandate_integrity := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1996L:1998L
)]

# Verification: all three mandate windows must equal zero post-1998
mandate_post_check <- tank_year_panel[
  panel_year >= 1999L,
  .(
    n_release_det    = sum(mandate_release_det),
    n_spill_overfill = sum(mandate_spill_overfill),
    n_integrity      = sum(mandate_integrity)
  )
]
stopifnot(
  "mandate_release_det nonzero post-1998"    = mandate_post_check$n_release_det    == 0L,
  "mandate_spill_overfill nonzero post-1998" = mandate_post_check$n_spill_overfill == 0L,
  "mandate_integrity nonzero post-1998"      = mandate_post_check$n_integrity      == 0L
)
log_step("Mandate overlap check passed.")

# Binned relative year for event studies [-14, 16]
tank_year_panel[, rel_year_bin := fcase(
  rel_year <= -14L, -14L,
  rel_year >=  16L,  16L,
  default = as.integer(rel_year)
)]

cat("\n")


#### S6 Define Tank Sample (panel_dt) ####

cat("========================================\n")
cat("S6: DEFINE TANK SAMPLE\n")
cat("========================================\n\n")

# All tank-years retained including installation-year rows.
# first_year_churn flag is available for downstream filtering.
# Cells with unknown wall/fuel (make_model_tank = NA) are excluded.

panel_dt <- tank_year_panel[
  !is.na(make_model_tank) &
  !is.na(did_term)        &
  state %in% STUDY_STATES &
  tstop > tstart
]

n_ty       <- nrow(panel_dt)
n_tanks    <- uniqueN(panel_dt$tank_panel_id)
n_tx_tanks <- panel_dt[texas_treated == 1L, uniqueN(tank_panel_id)]
n_ct_tanks <- panel_dt[texas_treated == 0L, uniqueN(tank_panel_id)]
n_cells    <- uniqueN(panel_dt$make_model_tank)

log_step(sprintf("panel_dt: %s tank-years | %s tanks (TX=%s | CTL=%s) | %s cells",
  fmt_n(n_ty), fmt_n(n_tanks), fmt_n(n_tx_tanks), fmt_n(n_ct_tanks), fmt_n(n_cells)))

cohort_comp <- panel_dt[, .(
  n_tanks = uniqueN(tank_panel_id),
  n_tx    = uniqueN(tank_panel_id[texas_treated == 1L]),
  n_ctl   = uniqueN(tank_panel_id[texas_treated == 0L])
), by = mm_install_cohort][order(mm_install_cohort)]

log_step("Install-year cohort composition:")
print(cohort_comp)
fwrite(cohort_comp, file.path(OUTPUT_TABLES, "Diag_InstallCohortComposition.csv"))
cat("\n")


#### S7 Cell Coverage Diagnostics ####

cat("========================================\n")
cat("S7: CELL COVERAGE DIAGNOSTICS\n")
cat("========================================\n\n")

cell_diag <- panel_dt[, .(
  n_total  = .N,
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  has_both = as.integer(sum(texas_treated == 1L) > 0L & sum(texas_treated == 0L) > 0L)
), by = .(make_model_tank, panel_year)]

cov_summ <- cell_diag[, .(
  total_cell_years      = .N,
  identified_cell_years = sum(has_both),
  pct_identified        = round(mean(has_both) * 100, 1),
  tank_years_total      = sum(n_total),
  tank_years_identified = sum(n_total * has_both),
  pct_ty_identified     = round(sum(n_total * has_both) / sum(n_total) * 100, 1)
)]

log_step(sprintf("Cell-years: %s total | %s identified (%s%%)",
  fmt_n(cov_summ$total_cell_years),
  fmt_n(cov_summ$identified_cell_years),
  cov_summ$pct_identified))
log_step(sprintf("Tank-years identified: %s / %s (%s%%)",
  fmt_n(cov_summ$tank_years_identified),
  fmt_n(cov_summ$tank_years_total),
  cov_summ$pct_ty_identified))

cov_by_cohort <- panel_dt[, .(
  n_tx     = sum(texas_treated == 1L),
  n_ctl    = sum(texas_treated == 0L),
  n_cells  = uniqueN(make_model_tank)
), by = mm_install_cohort][order(mm_install_cohort)]
log_step("Tank-years by install-year cohort:")
print(cov_by_cohort)

fwrite(cell_diag,     file.path(OUTPUT_TABLES, "Diag_CellCoverage_Detail.csv"))
fwrite(cov_summ,      file.path(OUTPUT_TABLES, "Diag_CellCoverage_Summary.csv"))
fwrite(cov_by_cohort, file.path(OUTPUT_TABLES, "Diag_CellCoverage_ByCohort.csv"))
cat("\n")


#### S8 Build Exact-Date Cox Base ####

cat("========================================\n")
cat("S8: BUILD EXACT-DATE COX BASE\n")
cat("========================================\n\n")

# One row per tank. Time axis in days since 1970-01-01.
# First-year-churn FLAGGED (not dropped) per the panel_dt convention.

exact_base <- study_tanks[
  !is.na(texas_treated)       &
  !is.na(make_model_tank)     &
  state %in% STUDY_STATES     &
  tank_installed_date < STUDY_END,
  .(tank_panel_id, panel_id, facility_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity,
    mm_install_cohort, install_yr_int,
    release_det_deadline_yr,
    make_model_tank, make_model_noage, make_model_3dim,
    tank_installed_date,
    t_enter  = as.numeric(tank_installed_date),
    t_exit   = as.numeric(pmin(
      fifelse(!is.na(tank_closed_date), tank_closed_date, STUDY_END),
      STUDY_END)),
    failure  = as.integer(
      !is.na(tank_closed_date) &
      tank_closed_date <= STUDY_END &
      tank_closed_date >= tank_installed_date)
  )
][t_exit > t_enter]

exact_base[, exit_yr_tmp := as.integer(format(as.Date(t_exit, origin = "1970-01-01"), "%Y"))]
exact_base[, first_year_churn := as.integer(exit_yr_tmp == install_yr_int & failure == 1L)]
exact_base[, exit_yr_tmp := NULL]

exact_base[, `:=`(
  age_enter = 0,
  age_exit  = as.numeric(t_exit - t_enter) / 365.25
)]

log_step(sprintf("exact_base: %s tanks | %s events | %s first-year-churn flagged",
  fmt_n(nrow(exact_base)),
  fmt_n(sum(exact_base$failure)),
  fmt_n(sum(exact_base$first_year_churn))))
cat("\n")


#### S9 Analysis Sample Construction ####

cat("========================================\n")
cat("S9: ANALYSIS SAMPLE CONSTRUCTION\n")
cat("========================================\n\n")

analysis_tanks <- copy(panel_dt[!is.na(make_model_tank)])

analysis_tanks[, `:=`(
  did_term  = texas_treated * as.integer(panel_year >= 1999L),
  did_short = texas_treated * as.integer(panel_year >= 1999L & panel_year <= 2004L),
  did_long  = texas_treated * as.integer(panel_year >= 2005L),
  rel_year  = panel_year - 1999L
)]
analysis_tanks[, rel_year_es := fcase(
  rel_year <= -12L, -12L,
  rel_year >= 15L,   15L,
  default = as.integer(rel_year)
)]
analysis_tanks[, rel_year_early := panel_year - 1998L]
analysis_tanks[, rel_year_early := pmax(pmin(rel_year_early, 15L), -12L)]
analysis_tanks[, did_term_early := texas_treated * as.integer(panel_year >= 1998L)]
analysis_tanks[, deadline_sw    := as.integer(panel_year == 1998L) *
                                    as.integer(mm_wall == "Single-Walled")]
analysis_tanks[, age_at_treatment := 1999L - install_yr_int]

log_step(sprintf("Analysis sample: TX=%s tanks | CTL=%s tanks | %s tank-years",
  fmt_n(analysis_tanks[texas_treated == 1L, uniqueN(tank_panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 0L, uniqueN(tank_panel_id)]),
  fmt_n(nrow(analysis_tanks))))
cat("\n")


#### S10 Tank Reform-Survivor Matching (CEM + PSM) ####

cat("========================================\n")
cat("S10: TANK | REFORM-SURVIVOR | CEM + PSM\n")
cat("========================================\n\n")

# Pool: tanks alive on Dec 22, 1998 (installed before AND open or closed after)
alive_at_reform <- study_tanks[
  tank_installed_date <= REFORM_DATE &
  (is.na(tank_closed_date) | tank_closed_date > REFORM_DATE),
  .(tank_panel_id)
]
log_step(sprintf("Tanks alive on %s: %s",
  format(REFORM_DATE), fmt_n(nrow(alive_at_reform))))

# Matching cross-section
tank_chars_reform <- unique(analysis_tanks[
  tank_panel_id %in% alive_at_reform$tank_panel_id,
  .(tank_panel_id, panel_id, state, texas_treated,
    mm_wall, mm_fuel, mm_capacity, mm_install_cohort, install_yr_int)
])
tank_chars_reform <- tank_chars_reform[
  !is.na(mm_wall) & !is.na(mm_fuel) &
  !is.na(mm_capacity) & !is.na(mm_install_cohort)
]

# Append continuous capacity and binary indicators for PSM
tank_indicators <- unique(study_tanks[, .(
  tank_panel_id, capacity,
  single_walled, double_walled,
  is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other
)])
tank_chars_reform <- merge(tank_chars_reform, tank_indicators,
                           by = "tank_panel_id", all.x = TRUE)

log_step(sprintf("Reform-pool: %s tanks (%s TX, %s CTL)",
  fmt_n(nrow(tank_chars_reform)),
  fmt_n(tank_chars_reform[texas_treated == 1L, .N]),
  fmt_n(tank_chars_reform[texas_treated == 0L, .N])))

# ---- CEM ----
tank_chars_reform_cem <- copy(tank_chars_reform)
tank_chars_reform_cem[, `:=`(
  mm_wall_f           = as.factor(mm_wall),
  mm_fuel_f           = as.factor(mm_fuel),
  mm_capacity_f       = as.factor(mm_capacity),
  mm_install_cohort_f = as.factor(mm_install_cohort)
)]

m_cem_tank_reform <- matchit(
  texas_treated ~ mm_wall_f + mm_fuel_f + mm_capacity_f + mm_install_cohort_f,
  data   = as.data.frame(tank_chars_reform_cem),
  method = "exact"
)
tank_chars_reform_cem[, cem_weight := m_cem_tank_reform$weights]
ids_tank_reform_cem <- tank_chars_reform_cem[cem_weight > 0, tank_panel_id]

log_step(sprintf("Tank-Reform-CEM matched: %s (TX=%s, CTL=%s)",
  fmt_n(length(ids_tank_reform_cem)),
  fmt_n(tank_chars_reform_cem[cem_weight > 0 & texas_treated == 1L, .N]),
  fmt_n(tank_chars_reform_cem[cem_weight > 0 & texas_treated == 0L, .N])))

matched_tanks_reform_cem <- merge(
  analysis_tanks[tank_panel_id %in% ids_tank_reform_cem],
  tank_chars_reform_cem[, .(tank_panel_id, cem_weight)],
  by = "tank_panel_id", all.x = TRUE
)

# ---- PSM ----
psm_data_tank_reform <- tank_chars_reform[
  !is.na(capacity) & !is.na(single_walled) & !is.na(is_gasoline)
]
psm_data_tank_reform[, mm_install_cohort_f := as.factor(mm_install_cohort)]

psm_mm_tank      <- ~ single_walled + double_walled +
                       is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other +
                       capacity + mm_install_cohort_f - 1
psm_mahvars_tank <- ~ single_walled + double_walled +
                       is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other +
                       capacity + mm_install_cohort_f
psm_matchit_tank <- texas_treated ~ single_walled + double_walled +
                       is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other +
                       capacity + mm_install_cohort_f

psm_res_tank_reform <- run_psm_glmnet(
  dt                = psm_data_tank_reform,
  treat_col         = "texas_treated",
  pscore_mm_formula = psm_mm_tank,
  mahvars_formula   = psm_mahvars_tank,
  matchit_formula   = psm_matchit_tank
)

psm_data_tank_reform <- psm_res_tank_reform$dt
psm_data_tank_reform[, psm_weight := psm_res_tank_reform$weights]
ids_tank_reform_psm <- psm_data_tank_reform[psm_weight > 0, tank_panel_id]

log_step(sprintf("Tank-Reform-PSM matched: %s (caliper=%.4f)",
  fmt_n(length(ids_tank_reform_psm)), psm_res_tank_reform$ir_caliper))

matched_tanks_reform_psm <- merge(
  analysis_tanks[tank_panel_id %in% ids_tank_reform_psm],
  psm_data_tank_reform[, .(tank_panel_id, psm_weight, pscore, logit_pscore)],
  by = "tank_panel_id", all.x = TRUE
)
cat("\n")


#### S11 Tank Birth-Cohort Matching (CEM + PSM) + bc_tanks for KM ####

cat("========================================\n")
cat("S11: TANK | BIRTH-COHORT | CEM + PSM\n")
cat("========================================\n\n")

# bc_tanks: ONE ROW PER TANK from study_tanks. Used both for matching AND
# for KM curves in F11-F13. Lower bound extended to 1950 to include all
# pre-reform "existing tank" cohorts subject to the rolling Chapter 334
# compliance schedule. Pre-1950 vintages excluded due to outlier ATTs and
# negligible population weights (see att_table_complete diagnostics).

bc_tanks <- study_tanks[
  !is.na(texas_treated) &
  state %in% STUDY_STATES &
  !is.na(tank_installed_date) &
  install_yr_int %between% c(1950L, 2018L) &
  !is.na(mm_wall) & mm_wall != "Unknown-Wall" &
  !is.na(mm_fuel) & mm_fuel != "Unknown-Fuel" &
  !is.na(mm_capacity) & !is.na(mm_install_cohort),
  .(
    tank_panel_id, panel_id, state, texas_treated,
    install_yr_int, mm_wall, mm_fuel, mm_capacity, mm_install_cohort,
    capacity, single_walled, double_walled,
    is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
    t_enter = as.numeric(tank_installed_date),
    t_exit  = as.numeric(pmin(
      fifelse(!is.na(tank_closed_date), tank_closed_date, STUDY_END),
      STUDY_END)),
    failure = as.integer(
      !is.na(tank_closed_date) &
      tank_closed_date <= STUDY_END &
      tank_closed_date >= tank_installed_date)
  )
][t_exit > t_enter]

# Drop installation-year exits (degenerate hazard for survival models)
bc_tanks[, `:=`(
  install_yr = as.integer(format(as.Date(t_enter, origin = "1970-01-01"), "%Y")),
  exit_yr    = as.integer(format(as.Date(t_exit,  origin = "1970-01-01"), "%Y"))
)]
bc_tanks <- bc_tanks[!(exit_yr == install_yr & failure == 1L)]

# Calendar-time vars for KM
bc_tanks[, `:=`(
  t_enter_cal = t_enter / 365.25 + 1970,
  t_exit_cal  = t_exit  / 365.25 + 1970,
  group       = factor(fifelse(texas_treated == 1L, "Texas", "Control States"),
                       levels = c("Texas", "Control States"))
)]

# 5-year vintage bins for KM facets (1950-2018, 14 bins)
# Extended from 1970 to 1950 to match bc_tanks lower bound
bc_tanks[, vintage5 := factor(fcase(
  install_yr_int %between% c(1950L, 1954L), "1950-1954",
  install_yr_int %between% c(1955L, 1959L), "1955-1959",
  install_yr_int %between% c(1960L, 1964L), "1960-1964",
  install_yr_int %between% c(1965L, 1969L), "1965-1969",
  install_yr_int %between% c(1970L, 1974L), "1970-1974",
  install_yr_int %between% c(1975L, 1979L), "1975-1979",
  install_yr_int %between% c(1980L, 1984L), "1980-1984",
  install_yr_int %between% c(1985L, 1989L), "1985-1989",
  install_yr_int %between% c(1990L, 1994L), "1990-1994",
  install_yr_int %between% c(1995L, 1999L), "1995-1999",
  install_yr_int %between% c(2000L, 2004L), "2000-2004",
  install_yr_int %between% c(2005L, 2009L), "2005-2009",
  install_yr_int %between% c(2010L, 2014L), "2010-2014",
  install_yr_int %between% c(2015L, 2018L), "2015-2018",
  default = NA_character_
), levels = c("1950-1954", "1955-1959", "1960-1964", "1965-1969",
              "1970-1974", "1975-1979", "1980-1984", "1985-1989", "1990-1994",
              "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018"))]

log_step(sprintf("bc_tanks: %s tanks (%s TX, %s CTL)",
  fmt_n(nrow(bc_tanks)),
  fmt_n(bc_tanks[texas_treated == 1L, .N]),
  fmt_n(bc_tanks[texas_treated == 0L, .N])))

# ---- CEM ----
bc_tanks[, `:=`(
  mm_wall_f           = as.factor(mm_wall),
  mm_fuel_f           = as.factor(mm_fuel),
  mm_capacity_f       = as.factor(mm_capacity),
  mm_install_cohort_f = as.factor(mm_install_cohort)
)]

m_cem_tank_birth <- matchit(
  texas_treated ~ mm_wall_f + mm_fuel_f + mm_capacity_f + mm_install_cohort_f,
  data   = as.data.frame(bc_tanks),
  method = "exact"
)
bc_tanks[, bc_cem_weight := m_cem_tank_birth$weights]
ids_tank_birth_cem <- bc_tanks[bc_cem_weight > 0, tank_panel_id]

log_step(sprintf("Tank-Birth-CEM matched: %s (TX=%s, CTL=%s)",
  fmt_n(length(ids_tank_birth_cem)),
  fmt_n(bc_tanks[bc_cem_weight > 0 & texas_treated == 1L, .N]),
  fmt_n(bc_tanks[bc_cem_weight > 0 & texas_treated == 0L, .N])))

# Build matched panel: merge weights onto panel_dt for matched IDs
matched_tanks_birth_cem <- merge(
  analysis_tanks[tank_panel_id %in% ids_tank_birth_cem],
  unique(bc_tanks[bc_cem_weight > 0, .(tank_panel_id, bc_cem_weight)]),
  by = "tank_panel_id", all.x = TRUE
)
setnames(matched_tanks_birth_cem, "bc_cem_weight", "cem_weight")

# ---- PSM ----
psm_data_tank_birth <- bc_tanks[
  !is.na(capacity) & !is.na(single_walled) & !is.na(is_gasoline)
]

psm_res_tank_birth <- run_psm_glmnet(
  dt                = psm_data_tank_birth,
  treat_col         = "texas_treated",
  pscore_mm_formula = psm_mm_tank,
  mahvars_formula   = psm_mahvars_tank,
  matchit_formula   = psm_matchit_tank
)

psm_data_tank_birth <- psm_res_tank_birth$dt
psm_data_tank_birth[, psm_weight := psm_res_tank_birth$weights]
ids_tank_birth_psm <- psm_data_tank_birth[psm_weight > 0, tank_panel_id]

log_step(sprintf("Tank-Birth-PSM matched: %s (caliper=%.4f)",
  fmt_n(length(ids_tank_birth_psm)), psm_res_tank_birth$ir_caliper))

matched_tanks_birth_psm <- merge(
  analysis_tanks[tank_panel_id %in% ids_tank_birth_psm],
  psm_data_tank_birth[, .(tank_panel_id, psm_weight, pscore, logit_pscore)],
  by = "tank_panel_id", all.x = TRUE
)

# Append BC weights to bc_tanks for KM use in F11-F13
bc_tanks <- merge(
  bc_tanks,
  psm_data_tank_birth[, .(tank_panel_id, bc_psm_weight = psm_weight,
                          pscore, logit_pscore)],
  by = "tank_panel_id", all.x = TRUE
)
bc_tanks[is.na(bc_psm_weight) & tank_panel_id %in% psm_data_tank_birth$tank_panel_id,
         bc_psm_weight := 0]
bc_tanks[, bc_cem_matched := bc_cem_weight > 0]
bc_tanks[, bc_psm_matched := !is.na(bc_psm_weight) & bc_psm_weight > 0]

cat("\n")


#### S12 Facility-Year Panel ####

cat("========================================\n")
cat("S12: FACILITY-YEAR PANEL\n")
cat("========================================\n\n")

# ---- T007 Phase 1: Churn-symmetry fix ----
# Build a single churn-free panel used for ALL three count aggregations
# (n_tanks_active, n_closures, n_installs).  first_year_churn == 1 tanks
# install and close in the same calendar year; they are economically
# meaningless and create a counting asymmetry when only excluded from
# closures but still counted in n_tanks_active and n_installs.
cat("T007 Phase1: Creating churn-filtered panel for S12 aggregations...\n")

n_total_ty  <- nrow(tank_year_panel)
n_churn_ty  <- tank_year_panel[first_year_churn == 1L, .N]
n_fy_churn  <- tank_year_panel[first_year_churn == 1L,
                                uniqueN(paste(panel_id, panel_year))]
n_fy_total  <- uniqueN(paste(tank_year_panel$panel_id,
                              tank_year_panel$panel_year))

log_step(sprintf("Churn filter stats:"))
log_step(sprintf("  Total tank-years in raw panel_dt:           %s",  fmt_n(n_total_ty)), 1)
log_step(sprintf("  Tank-years flagged as first_year_churn:     %s (%.2f%%)",
                  fmt_n(n_churn_ty), 100 * n_churn_ty / n_total_ty), 1)
log_step(sprintf("  Facility-years with >=1 churn tank-year:   %s (%.2f%% of %s facility-years)",
                  fmt_n(n_fy_churn), 100 * n_fy_churn / n_fy_total,
                  fmt_n(n_fy_total)), 1)

tank_year_panel_clean <- tank_year_panel[first_year_churn == 0L | is.na(first_year_churn)]
log_step(sprintf("  Rows in tank_year_panel_clean: %s (removed %s churn rows)",
                  fmt_n(nrow(tank_year_panel_clean)),
                  fmt_n(nrow(tank_year_panel) - nrow(tank_year_panel_clean))), 1)

# Churn-free install source for S12.3 (study_tanks has no first_year_churn flag)
study_tanks_no_churn <- study_tanks[
  is.na(tank_closed_date) | year(tank_installed_date) != year(tank_closed_date)
]
log_step(sprintf("  study_tanks_no_churn: %s of %s tanks (removed %s churn installs)",
                  fmt_n(nrow(study_tanks_no_churn)), fmt_n(nrow(study_tanks)),
                  fmt_n(nrow(study_tanks) - nrow(study_tanks_no_churn))), 1)
cat("\n")

# ---- S12.1 Core facility-year aggregation ----
# n_tanks_active = tanks present at ANY point during year, EXCLUDING first-year-churn
# tanks (tanks that install and close in the same calendar year).
# EOY stocks computed in S12.2 by subtracting closures.

cat("S12.1: Core stock aggregation (churn-filtered)...\n")

fac_stock <- tank_year_panel_clean[
  state %in% STUDY_STATES,
  .(
    n_tanks_active        = .N,
    n_sw_active           = sum(mm_wall == "Single-Walled", na.rm = TRUE),
    n_dw_active           = sum(mm_wall == "Double-Walled", na.rm = TRUE),
    n_unk_wall_active     = sum(mm_wall == "Unknown-Wall",  na.rm = TRUE),
    n_gasoline_active     = sum(mm_fuel == "Gasoline-Only", na.rm = TRUE),
    n_diesel_active       = sum(mm_fuel == "Diesel-Only",   na.rm = TRUE),
    n_other_fuel_active   = sum(!mm_fuel %in% c("Gasoline-Only", "Diesel-Only"), na.rm = TRUE),
    total_capacity        = sum(capacity, na.rm = TRUE),
    avg_tank_age          = mean(tank_age, na.rm = TRUE),
    max_tank_age          = fifelse(any(!is.na(tank_age)), max(tank_age, na.rm = TRUE), NA_real_),
    min_tank_age          = fifelse(any(!is.na(tank_age)), min(tank_age, na.rm = TRUE), NA_real_),
    # All closure measures exclude first-year-churn tanks (tanks that close
    # in the same calendar year they were installed). These installation-year
    # failures are not economically meaningful exits -- they represent failed
    # inspections at install, data entry errors, or rapid corrective removals
    # with no connection to the mandate or reform behavior under study.
    # first_year_churn == 1 flags are set in S4 (tank_year_panel construction).
    n_closures   = sum(closure_event[first_year_churn == 0L | is.na(first_year_churn)],
                       na.rm = TRUE),
    any_closure  = as.integer(
      sum(closure_event[first_year_churn == 0L | is.na(first_year_churn)],
          na.rm = TRUE) > 0L),
    n_sw_closures = sum(
      closure_event == 1L & mm_wall == "Single-Walled" &
      (first_year_churn == 0L | is.na(first_year_churn)),
      na.rm = TRUE),
    n_dw_closures = sum(
      closure_event == 1L & mm_wall == "Double-Walled" &
      (first_year_churn == 0L | is.na(first_year_churn)),
      na.rm = TRUE),
    any_sw_closure = as.integer(sum(
      closure_event == 1L & mm_wall == "Single-Walled" &
      (first_year_churn == 0L | is.na(first_year_churn)),
      na.rm = TRUE) > 0L),
    capacity_closed = sum(
      fifelse(closure_event == 1L &
              (first_year_churn == 0L | is.na(first_year_churn)),
              capacity, 0),
      na.rm = TRUE),
    any_mandate_release_det    = as.integer(any(mandate_release_det == 1L,    na.rm = TRUE)),
    any_mandate_spill_overfill = as.integer(any(mandate_spill_overfill == 1L, na.rm = TRUE)),
    any_mandate_integrity      = as.integer(any(mandate_integrity == 1L,      na.rm = TRUE)),
    n_tanks_identified  = sum(!is.na(make_model_tank)),
    n_cells_represented = uniqueN(make_model_tank, na.rm = TRUE),
    county_name   = first(county_name),
    county_fips   = first(county_fips),
    latitude      = first(latitude),
    longitude     = first(longitude),
    texas_treated = first(texas_treated),
    state         = first(state),
    facility_id   = first(facility_id)
  ),
  by = .(panel_id, panel_year)
]

log_step(sprintf("Facility-year stock: %s rows | %s facilities",
  fmt_n(nrow(fac_stock)), fmt_n(uniqueN(fac_stock$panel_id))))

# Diagnostic: how many closure-events were excluded due to first-year-churn
n_churn_events <- tank_year_panel[
  state %in% STUDY_STATES &
  closure_event == 1L &
  first_year_churn == 1L, .N
]
n_total_events <- tank_year_panel[
  state %in% STUDY_STATES &
  closure_event == 1L, .N
]
log_step(sprintf(
  "First-year-churn exclusion: %s of %s closure-events removed (%.1f%%)",
  fmt_n(n_churn_events),
  fmt_n(n_total_events),
  100 * n_churn_events / n_total_events
))

# ---- S12.2 End-of-year stocks ----
fac_stock[, `:=`(
  n_tanks_eoy  = n_tanks_active - n_closures,
  n_sw_eoy     = n_sw_active    - n_sw_closures,
  n_dw_eoy     = n_dw_active    - n_dw_closures,
  capacity_eoy = total_capacity - capacity_closed
)]
# _dec aliases
fac_stock[, `:=`(
  active_tanks_dec      = n_tanks_eoy,
  single_tanks_dec      = n_sw_eoy,
  double_tanks_dec      = n_dw_eoy,
  total_capacity_dec    = capacity_eoy,
  avg_tank_age_dec      = avg_tank_age,
  has_single_walled_dec = as.integer(n_sw_eoy > 0),
  has_double_walled_dec = as.integer(n_dw_eoy > 0)
)]
fac_stock[, `:=`(
  active_tanks = n_tanks_eoy,
  single_tanks = n_sw_eoy,
  double_tanks = n_dw_eoy,
  has_single_walled = as.integer(n_sw_active > 0),
  has_double_walled = as.integer(n_dw_active > 0),
  has_unknown_wall  = as.integer(n_unk_wall_active > 0),
  pct_sw       = n_sw_eoy / pmax(n_tanks_eoy, 1L),
  pct_dw       = n_dw_eoy / pmax(n_tanks_eoy, 1L),
  all_sw       = as.integer(n_sw_eoy == n_tanks_eoy & n_tanks_eoy > 0),
  all_dw       = as.integer(n_dw_eoy == n_tanks_eoy & n_tanks_eoy > 0),
  mixed_wall   = as.integer(n_sw_eoy > 0 & n_dw_eoy > 0),
  has_gasoline = as.integer(n_gasoline_active > 0),
  has_diesel   = as.integer(n_diesel_active > 0),
  is_motor_fuel = as.integer(n_gasoline_active > 0 | n_diesel_active > 0)
)]

# ---- S12.3 Installation events ----
# Uses study_tanks_no_churn (churn installs excluded) to match the
# first_year_churn exclusion applied in S12.1 and S12.1 closures.
cat("S12.3: Installation events (churn-filtered)...\n")
installs_by_fac_yr <- study_tanks_no_churn[
  !is.na(tank_installed_date),
  .(
    n_installs         = .N,
    n_dw_installs      = sum(mm_wall == "Double-Walled", na.rm = TRUE),
    n_sw_installs      = sum(mm_wall == "Single-Walled", na.rm = TRUE),
    capacity_installed = sum(capacity, na.rm = TRUE)
  ),
  by = .(panel_id, panel_year = year(tank_installed_date))
]
fac_stock <- merge(fac_stock, installs_by_fac_yr,
                   by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in c("n_installs", "n_dw_installs", "n_sw_installs", "capacity_installed"))
  fac_stock[is.na(get(v)), (v) := 0L]

# ---- T007 Phase 1, Step 1.3: Lead-year consistency check ----
# For each facility, n_tanks_active in year t+1 should equal n_tanks_eoy in
# year t (EOY stock becomes next-year's opening stock). Failures indicate
# residual panel noise (silent disappearances) that the churn fix can't address.
cat("T007 Phase1 Step 1.3: Lead-year consistency check...\n")
setorder(fac_stock, panel_id, panel_year)
fac_stock[, .n_active_lead := shift(n_tanks_active, -1L, type = "lead"), by = panel_id]
n_check  <- fac_stock[!is.na(.n_active_lead), .N]
n_fail   <- fac_stock[!is.na(.n_active_lead) & .n_active_lead != n_tanks_eoy, .N]
pct_fail <- if (n_check > 0L) 100 * n_fail / n_check else 0
log_step(sprintf("Lead consistency: %s of %s consecutive-year pairs pass (%.2f%% fail)",
                  fmt_n(n_check - n_fail), fmt_n(n_check), pct_fail))
if (pct_fail > 1) {
  fail_detail <- fac_stock[!is.na(.n_active_lead) & .n_active_lead != n_tanks_eoy,
                            .(state = first(state), gap = .n_active_lead - n_tanks_eoy),
                            by = .(panel_id, panel_year)][
    order(-abs(gap))]
  cat(sprintf("  WARNING: %.2f%% failure rate exceeds 1%% threshold.\n", pct_fail))
  cat("  Top-10 failing facility-years by |gap|:\n")
  print(head(fail_detail, 10L))
  warning(sprintf("T007 lead-consistency: %.2f%% failure rate — investigate residual panel noise",
                  pct_fail))
} else {
  log_step(sprintf("  PASS: failure rate %.2f%% < 1%% threshold", pct_fail), 1)
}
fac_stock[, .n_active_lead := NULL]   # cleanup temp column

# ---- S12.4 Closure type classification: replacement vs. permanent ----
# Forward-looking by construction. Outcome decomposition only - NOT a regressor.
cat("S12.4: Closure type classification...\n")
fac_max_install <- study_tanks[
  !is.na(tank_installed_date),
  .(fac_max_install_date = max(tank_installed_date, na.rm = TRUE)),
  by = panel_id
]
# Exclude first-year-churn tanks from closure type classification.
# A tank that closes in its installation year is not a replacement
# candidate and should not count toward permanent or replacement closures.
# first_year_churn is defined as close_yr == install_yr AND failure == 1,
# computed in S4. We replicate the definition here using the tank-level
# dates rather than the panel-year flag.
closed_tanks_fac <- study_tanks[
  !is.na(tank_closed_date) &
  year(tank_closed_date) != year(tank_installed_date),
  .(panel_id, tank_panel_id, tank_closed_date, tank_installed_date,
    mm_wall, mm_fuel, capacity)
]
closed_tanks_fac <- merge(closed_tanks_fac, fac_max_install, by = "panel_id", all.x = TRUE)
closed_tanks_fac[, is_replacement := as.integer(
  !is.na(fac_max_install_date) & fac_max_install_date > tank_closed_date
)]
closed_tanks_fac[is.na(is_replacement), is_replacement := 0L]
closed_tanks_fac[, is_sw_replacement := as.integer(
  is_replacement == 1L & mm_wall == "Single-Walled"
)]

closure_type_agg <- closed_tanks_fac[, .(
  n_closures_permanent   = sum(is_replacement == 0L),
  n_closures_replacement = sum(is_replacement == 1L),
  n_sw_replacement       = sum(is_sw_replacement, na.rm = TRUE),
  capacity_permanent     = sum(capacity[is_replacement == 0L], na.rm = TRUE),
  capacity_replacement   = sum(capacity[is_replacement == 1L], na.rm = TRUE)
), by = .(panel_id, panel_year = year(tank_closed_date))]

fac_stock <- merge(fac_stock, closure_type_agg,
                   by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in c("n_closures_permanent", "n_closures_replacement", "n_sw_replacement",
            "capacity_permanent", "capacity_replacement"))
  fac_stock[is.na(get(v)), (v) := 0L]

fac_stock[, `:=`(
  permanent_closure_year   = as.integer(n_closures_permanent > 0),
  replacement_closure_year = as.integer(n_closures_replacement > 0),
  single_to_double_year    = as.integer(n_sw_replacement > 0 & n_dw_installs > 0)
)]

log_step(sprintf("Closure types: %s permanent | %s replacement",
  fmt_n(fac_stock[, sum(n_closures_permanent)]),
  fmt_n(fac_stock[, sum(n_closures_replacement)])))

rm(fac_max_install, closed_tanks_fac, closure_type_agg)

# ---- S12.5 YoY changes ----
cat("S12.5: YoY changes...\n")
setorder(fac_stock, panel_id, panel_year)
fac_stock[, `:=`(
  lag_n_tanks   = shift(n_tanks_eoy,           1L, type = "lag"),
  lag_n_sw      = shift(n_sw_eoy,              1L, type = "lag"),
  lag_n_dw      = shift(n_dw_eoy,              1L, type = "lag"),
  lag_pct_sw    = shift(pct_sw,                1L, type = "lag"),
  lag_avg_age   = shift(avg_tank_age,          1L, type = "lag"),
  lag_capacity  = shift(capacity_eoy,          1L, type = "lag"),
  lag_has_sw    = shift(has_single_walled_dec, 1L, type = "lag"),
  lag_has_dw    = shift(has_double_walled_dec, 1L, type = "lag")
), by = panel_id]
fac_stock[, `:=`(
  net_tank_change    = n_tanks_eoy  - lag_n_tanks,
  delta_pct_sw       = pct_sw       - lag_pct_sw,
  delta_avg_age      = avg_tank_age - lag_avg_age,
  capacity_change    = capacity_eoy - lag_capacity,
  lost_all_sw        = as.integer(lag_has_sw == 1L & has_single_walled_dec == 0L),
  gained_dw          = as.integer(lag_has_dw == 0L & has_double_walled_dec == 1L),
  capacity_increased = as.integer(!is.na(lag_capacity) & capacity_eoy > lag_capacity),
  capacity_decreased = as.integer(!is.na(lag_capacity) & capacity_eoy < lag_capacity)
)]

# ---- S12.6 Facility biography (time-invariant) ----
# All variables are computed from the full study_tanks history at the
# facility level. These are the matching covariates for the FACILITY
# BIRTH-COHORT pool (S16). The pool includes ALL facilities regardless of
# whether they had tanks alive at the reform date - matching is done on
# time-invariant portfolio totals to study mandate pressure and parallel
# trends violations absent the snapshot-at-reform conditioning.
cat("S12.6: Facility biography...\n")
fac_bio <- study_tanks[, .(
  first_install_yr     = min(year(tank_installed_date), na.rm = TRUE),
  last_install_yr      = max(year(tank_installed_date), na.rm = TRUE),
  first_install_date   = min(tank_installed_date, na.rm = TRUE),
  last_install_date    = max(tank_installed_date, na.rm = TRUE),
  total_tanks_ever     = uniqueN(tank_panel_id),
  total_tanks_closed_ever = sum(!is.na(tank_closed_date)),
  total_capacity_ever  = sum(capacity, na.rm = TRUE),
  max_capacity_ever    = max(capacity, na.rm = TRUE),
  mean_capacity_ever   = mean(capacity, na.rm = TRUE),
  total_sw_ever        = sum(mm_wall == "Single-Walled", na.rm = TRUE),
  total_dw_ever        = sum(mm_wall == "Double-Walled", na.rm = TRUE),
  total_gasoline_ever  = sum(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  total_diesel_ever    = sum(mm_fuel == "Diesel-Only",   na.rm = TRUE),
  prop_sw_ever         = mean(mm_wall == "Single-Walled", na.rm = TRUE),
  prop_dw_ever         = mean(mm_wall == "Double-Walled", na.rm = TRUE),
  prop_gasoline_ever   = mean(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  prop_diesel_ever     = mean(mm_fuel == "Diesel-Only",   na.rm = TRUE),
  had_pre1965_tanks    = as.integer(any(year(tank_installed_date) <= 1964L, na.rm = TRUE)),
  had_pre1975_tanks    = as.integer(any(year(tank_installed_date) <= 1974L, na.rm = TRUE)),
  had_pre1989_tanks    = as.integer(any(year(tank_installed_date) <= 1988L, na.rm = TRUE)),
  all_post1988         = as.integer(all(year(tank_installed_date) >= 1989L, na.rm = TRUE)),
  all_post1989         = as.integer(all(year(tank_installed_date) >= 1990L, na.rm = TRUE)),
  ever_had_sw          = as.integer(any(mm_wall == "Single-Walled", na.rm = TRUE)),
  ever_had_dw          = as.integer(any(mm_wall == "Double-Walled", na.rm = TRUE)),
  texas_treated        = first(texas_treated),
  state                = first(state)
), by = panel_id]

# Replace NaN/Inf from all-NA reductions with NA (handles single-tank facs etc.)
for (col in setdiff(names(fac_bio), c("panel_id", "state"))) {
  if (is.numeric(fac_bio[[col]])) {
    set(fac_bio, which(!is.finite(fac_bio[[col]])), col, NA)
  }
}

# Time-invariant tank-count and SW-share bins for CEM matching
fac_bio[, total_tanks_bin := fcase(
  total_tanks_ever == 1L,                 "1",
  total_tanks_ever %between% c(2L, 3L),   "2-3",
  total_tanks_ever %between% c(4L, 6L),   "4-6",
  total_tanks_ever %between% c(7L, 10L),  "7-10",
  total_tanks_ever >= 11L,                "11+",
  default = NA_character_
)]
fac_bio[, sw_share_ever_bin := fcase(
  prop_sw_ever == 1,    "All SW",
  prop_sw_ever >  0.5,  "Majority SW",
  prop_sw_ever <= 0.5,  "Minority/No SW",
  default = NA_character_
)]

fac_bio[, fac_is_incumbent := as.integer(first_install_yr < 1999L)]
fac_bio[, fac_vintage := fcase(
  first_install_yr <  1975L,                    "Pre-1975",
  first_install_yr %between% c(1975L, 1984L),   "1975-1984",
  first_install_yr %between% c(1985L, 1988L),   "1985-1988",
  first_install_yr %between% c(1989L, 1998L),   "1989-1998",
  first_install_yr >= 1999L,                    "Post-Reform",
  default = "Unknown"
)]

# Birth-cohort bin for facility birth-cohort matching (S16)
fac_bio[, birth_cohort_bin := fcase(
  first_install_yr <= 1964L,                    "Pre-1965",
  first_install_yr %between% c(1965L, 1979L),   "1965-1979",
  first_install_yr %between% c(1980L, 1988L),   "1980-1988",
  first_install_yr %between% c(1989L, 1998L),   "1989-1998",
  first_install_yr >= 1999L,                    "Post-1998",
  default = NA_character_
)]

log_step(sprintf("Biography: %s facilities", fmt_n(nrow(fac_bio))))

# ---- S12.7 Reform-date portfolio snapshot ----
cat("S12.7: Reform-date portfolio snapshot...\n")
reform_tank_ids <- exact_base[
  t_enter <= REFORM_DAYS & t_exit > REFORM_DAYS, tank_panel_id
]
tanks_at_reform_fac <- study_tanks[
  tank_panel_id %in% reform_tank_ids,
  .(panel_id, tank_panel_id, mm_wall, mm_fuel, mm_capacity,
    capacity, install_yr_int,
    age_at_reform = as.numeric(REFORM_DATE - tank_installed_date) / 365.25)
]

fac_mm_reform <- tanks_at_reform_fac[, .(
  n_tanks_at_reform     = .N,
  n_sw_at_reform        = sum(mm_wall == "Single-Walled", na.rm = TRUE),
  n_dw_at_reform        = sum(mm_wall == "Double-Walled", na.rm = TRUE),
  total_capacity_reform = sum(capacity, na.rm = TRUE),
  oldest_age_reform     = max(age_at_reform, na.rm = TRUE),
  newest_age_reform     = min(age_at_reform, na.rm = TRUE),
  avg_age_reform        = mean(age_at_reform, na.rm = TRUE),
  oldest_install_yr_at_reform = as.integer(min(install_yr_int, na.rm = TRUE)),
  newest_install_yr_at_reform = as.integer(max(install_yr_int, na.rm = TRUE)),
  mean_capacity         = mean(capacity, na.rm = TRUE),
  sd_capacity           = sd(capacity,   na.rm = TRUE),
  prop_sw               = mean(mm_wall == "Single-Walled", na.rm = TRUE),
  prop_dw               = mean(mm_wall == "Double-Walled", na.rm = TRUE),
  prop_gasoline         = mean(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  prop_diesel           = mean(mm_fuel == "Diesel-Only",   na.rm = TRUE),
  fac_wall_reform = fcase(
    any(mm_wall == "Unknown-Wall"),  "Unknown",
    all(mm_wall == "Single-Walled"), "All-SW",
    all(mm_wall == "Double-Walled"), "All-DW",
    default = "Mixed"
  ),
  any_sw_at_reform = as.integer(any(mm_wall == "Single-Walled")),
  any_dw_at_reform = as.integer(any(mm_wall == "Double-Walled")),
  fac_fuel_reform = fcase(
    all(mm_fuel == "Gasoline-Only"), "Gasoline-Only",
    all(mm_fuel == "Diesel-Only"),   "Diesel-Only",
    all(mm_fuel == "Unknown-Fuel"),  "Unknown",
    default = "Mixed-Or-Other"
  )
), by = panel_id]

# Replace NaN/Inf from all-NA reductions with NA
for (col in setdiff(names(fac_mm_reform), "panel_id")) {
  if (is.numeric(fac_mm_reform[[col]])) {
    set(fac_mm_reform, which(!is.finite(fac_mm_reform[[col]])), col, NA)
  }
}

# CEM bins
fac_mm_reform[, n_tanks_bin := fcase(
  n_tanks_at_reform == 1L,                   "1",
  n_tanks_at_reform %between% c(2L, 3L),     "2-3",
  n_tanks_at_reform %between% c(4L, 6L),     "4-6",
  n_tanks_at_reform %between% c(7L, 10L),    "7-10",
  n_tanks_at_reform >= 11L,                   "11+",
  default = NA_character_
)]
fac_mm_reform[, n_sw_bin := fcase(
  n_sw_at_reform == 0L,                      "0",
  n_sw_at_reform == 1L,                      "1",
  n_sw_at_reform %between% c(2L, 3L),        "2-3",
  n_sw_at_reform >= 4L,                       "4+",
  default = NA_character_
)]
fac_mm_reform[, capacity_bin := fcase(
  is.na(total_capacity_reform) | total_capacity_reform == 0, NA_character_,
  total_capacity_reform < 10000,                              "Under-10k",
  total_capacity_reform %between% c(10000, 29999),            "10k-30k",
  total_capacity_reform %between% c(30000, 59999),            "30k-60k",
  total_capacity_reform %between% c(60000, 99999),            "60k-100k",
  total_capacity_reform >= 100000,                             "100k+",
  default = NA_character_
)]
fac_mm_reform[, oldest_age_bin := fcase(
  oldest_age_reform <   5,                "0-4yr",
  oldest_age_reform %between% c( 5,  9),  "5-9yr",
  oldest_age_reform %between% c(10, 14),  "10-14yr",
  oldest_age_reform %between% c(15, 19),  "15-19yr",
  oldest_age_reform %between% c(20, 24),  "20-24yr",
  oldest_age_reform >= 25,                "25yr+",
  default = NA_character_
)]
fac_mm_reform[, sw_share_bin := fcase(
  prop_sw == 1,    "All SW",
  prop_sw >  0.5,  "Majority SW",
  prop_sw <= 0.5,  "Minority/No SW",
  default = NA_character_
)]
fac_mm_reform[, make_model_fac := fifelse(
  !is.na(oldest_age_bin) & fac_wall_reform != "Unknown",
  paste(fac_wall_reform, fac_fuel_reform, oldest_age_bin, sep = "|"),
  NA_character_
)]

log_step(sprintf("Reform snapshot: %s facilities | identified cell: %s",
  fmt_n(nrow(fac_mm_reform)),
  fmt_n(fac_mm_reform[!is.na(make_model_fac), .N])))

rm(reform_tank_ids, tanks_at_reform_fac)

# ---- S12.8 DiD time variables ----
cat("S12.8: DiD variables...\n")
fac_stock[, `:=`(
  post_1999   = as.integer(panel_year >= POST_YEAR),
  did_term    = texas_treated * as.integer(panel_year >= POST_YEAR),
  did_short   = texas_treated * as.integer(panel_year >= 1999L & panel_year <= 2004L),
  did_long    = texas_treated * as.integer(panel_year >= 2005L),
  rel_year    = panel_year - POST_YEAR,
  rel_year_es = pmax(pmin(panel_year - POST_YEAR, 15L), -12L)
)]

# ---- S12.9 Exit/entry/survival flags ----
cat("S12.9: Exit/entry flags...\n")
setorder(fac_stock, panel_id, panel_year)
fac_stock[, last_panel_year  := max(panel_year), by = panel_id]
fac_stock[, first_panel_year := min(panel_year), by = panel_id]
fac_stock[, facility_exit := as.integer(
  panel_year == last_panel_year & last_panel_year < 2020L
)]
fac_stock[, facility_complete_closure := as.integer(
  n_closures > 0 & n_tanks_eoy == 0L
)]
fac_stock[, facility_entry := as.integer(
  panel_year == first_panel_year & first_panel_year > 1985L
)]
fac_stock[, post_reform_entrant := as.integer(first_panel_year >= 1999L)]
fac_stock[, years_in_panel := panel_year - first_panel_year]

# ---- S12.10 Cumulative variables ----
# All cumulatives are within-facility cumsums over the panel, ordered by year.
# These are time-VARYING facility-year history variables: at year t, they
# reflect the facility's portfolio history through year t. Available as
# regressors in panel models (no look-ahead bias since they sum past events).
cat("S12.10: Cumulatives...\n")

setorder(fac_stock, panel_id, panel_year)
fac_stock[, `:=`(
  # Closure history
  cumulative_closures        = cumsum(n_closures),
  cumulative_sw_closures     = cumsum(n_sw_closures),
  cumulative_dw_closures     = cumsum(n_dw_closures),
  cumulative_replacements    = cumsum(n_closures_replacement),
  cumulative_permanent       = cumsum(n_closures_permanent),
  cumulative_capacity_closed = cumsum(capacity_closed),
  ever_closed_tank           = as.integer(cumsum(n_closures) > 0),

  # Install history
  cumulative_installs           = cumsum(n_installs),
  cumulative_sw_installs        = cumsum(n_sw_installs),
  cumulative_dw_installs        = cumsum(n_dw_installs),
  cumulative_capacity_installed = cumsum(capacity_installed),

  # Net portfolio evolution
  cumulative_net_tanks = cumsum(n_installs - n_closures),

  # Wall-type history flags (running)
  ever_had_sw_to_date = cummax(has_single_walled),
  ever_had_dw_to_date = cummax(has_double_walled),
  ever_mixed_to_date  = cummax(mixed_wall),

  # Capacity peak to date
  max_active_tanks_to_date = cummax(n_tanks_active),
  max_capacity_eoy_to_date = cummax(capacity_eoy)
), by = panel_id]

# Fleet-size-ever through year t:
# active EOY + all closed to date = all tanks ever present at facility through t.
# Correct for pre-PANEL_START tanks: those appear in n_sw_eoy in 1985 and
# are captured by cumulative_sw_closures as they exit. Sum is exact.
fac_stock[, `:=`(
  sw_tanks_ever_by_t  = n_sw_eoy  + cumulative_sw_closures,
  dw_tanks_ever_by_t  = n_dw_eoy  + cumulative_dw_closures,
  tanks_ever_by_t     = n_tanks_eoy + cumulative_closures
), by = panel_id]

log_step(sprintf("Stock panel: %s rows | %s fac | %s exits | %s entries",
  fmt_n(nrow(fac_stock)), fmt_n(uniqueN(fac_stock$panel_id)),
  fmt_n(fac_stock[facility_exit == 1L, .N]),
  fmt_n(fac_stock[facility_entry == 1L, .N])))
cat("\n")

#### S13 Merge External Data ####

cat("========================================\n")
cat("S13: MERGE EXTERNAL DATA\n")
cat("========================================\n\n")

# ---- S13.1 LUST merge + leak-closure pairing ----
cat("S13.1: LUST...\n")
master_lust <- fread(
  here("Data", "Processed", "Master_Harmonized_LUST.csv"),
  na.strings = c("", "NA", "N/A")
)
master_lust[, `:=`(
  facility_id = toupper(trimws(as.character(facility_id))),
  state       = toupper(trimws(as.character(state)))
)]
master_lust[, panel_id    := paste(facility_id, state, sep = "_")]
master_lust[, report_date := as.IDate(report_date)]

leaks_fac_yr <- master_lust[
  !is.na(report_date) & state %in% STUDY_STATES,
  .(
    n_leak_reports   = .N,
    n_leak_incidents = uniqueN(report_date),
    leak_year        = 1L
  ),
  by = .(panel_id, panel_year = year(report_date))
]
fac_stock <- merge(fac_stock, leaks_fac_yr,
                   by = c("panel_id", "panel_year"), all.x = TRUE)
fac_stock[is.na(n_leak_reports), `:=`(
  n_leak_reports = 0L, n_leak_incidents = 0L, leak_year = 0L
)]
fac_stock[, n_leaks := n_leak_incidents]

log_step(sprintf("LUST: %s fac-years with leaks | %s reports | %s incidents",
  fmt_n(fac_stock[leak_year == 1L, .N]),
  fmt_n(fac_stock[, sum(n_leak_reports)]),
  fmt_n(fac_stock[, sum(n_leak_incidents)])))

# Leak-closure pairing across 4 windows (primary 0-60d, narrow 0-30d,
# wide 0-90d, regulatory 0-45d). All four preserved per current convention.
closures_for_lust <- study_tanks[
  !is.na(tank_closed_date),
  .(panel_id, closure_date = tank_closed_date, tank_install_date = tank_installed_date)
]
leaks_for_lust <- master_lust[
  !is.na(report_date) & state %in% STUDY_STATES,
  .(panel_id, report_date)
]
setkey(closures_for_lust, panel_id)
setkey(leaks_for_lust, panel_id)

lust_pairs <- leaks_for_lust[closures_for_lust, on = .(panel_id), allow.cartesian = TRUE]
n_pre <- nrow(lust_pairs)
lust_pairs <- lust_pairs[is.na(tank_install_date) | report_date >= tank_install_date]
log_step(sprintf("Pairs: %s -> %s after generation scoping",
  fmt_n(n_pre), fmt_n(nrow(lust_pairs))))

lust_pairs[, diff := as.numeric(report_date - closure_date)]
lust_pairs[, `:=`(
  is_rev_prim = diff >= 0    & diff <= 60,
  is_kno_prim = diff < -180,
  is_ind_prim = diff >= -180 & diff < 0,
  is_rev_nar  = diff >= 0    & diff <= 30,
  is_kno_nar  = diff < -365,
  is_rev_wid  = diff >= 0    & diff <= 90,
  is_kno_wid  = diff < -90,
  is_rev_reg  = diff >= 0    & diff <= 45,
  is_kno_reg  = diff < -180
)]

closure_lust_flags <- lust_pairs[, .(
  has_rev_prim = any(is_rev_prim), has_kno_prim = any(is_kno_prim),
  has_ind_prim = any(is_ind_prim),
  has_rev_nar  = any(is_rev_nar),  has_kno_nar  = any(is_kno_nar),
  has_rev_wid  = any(is_rev_wid),  has_kno_wid  = any(is_kno_wid),
  has_rev_reg  = any(is_rev_reg),  has_kno_reg  = any(is_kno_reg)
), by = .(panel_id, closure_date)]
closure_lust_flags[, panel_year := year(closure_date)]

annual_lust_flags <- closure_lust_flags[, .(
  tank_closure_revealed          = as.integer(any(has_rev_prim)),
  tank_closure_known_leak        = as.integer(any(has_kno_prim)),
  tank_closure_indeterminate     = as.integer(any(has_ind_prim)),
  tank_closure_revealed_narrow   = as.integer(any(has_rev_nar)),
  tank_closure_known_leak_narrow = as.integer(any(has_kno_nar)),
  tank_closure_revealed_wide     = as.integer(any(has_rev_wid)),
  tank_closure_known_leak_wide   = as.integer(any(has_kno_wid)),
  tank_closure_revealed_reg      = as.integer(any(has_rev_reg)),
  tank_closure_known_leak_reg    = as.integer(any(has_kno_reg))
), by = .(panel_id, panel_year)]

fac_stock <- merge(fac_stock, annual_lust_flags,
                   by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in grep("^tank_closure_", names(fac_stock), value = TRUE))
  fac_stock[is.na(get(v)), (v) := 0L]

fac_stock[, tank_closure_clean := as.integer(
  any_closure == 1L & tank_closure_revealed == 0L & tank_closure_known_leak == 0L
)]
fac_stock[, `:=`(
  lust_within_90d_of_closure = tank_closure_revealed_wide,
  leak_found_by_closure      = tank_closure_revealed
)]

# ---- Leak × Install pairing (parallel to leak × closure pairing above) ----
# A LUST report within 0-60 days AFTER a tank install at the same facility is
# treated as a "first-year-churn" leak suspect: the report is most likely an
# install-failure / pressure-test artifact, not a real environmental release.
# Symmetric to the closure-side `first_year_churn` exclusion in S12.1, but
# implemented as FLAGS (not drops) so downstream consumers can choose.
# Four windows mirror the closure-pairing scheme.
installs_for_lust <- study_tanks[
  !is.na(tank_installed_date),
  .(panel_id, tank_install_date = tank_installed_date)
]
setkey(installs_for_lust, panel_id)

leaks_install_pairs <- leaks_for_lust[installs_for_lust,
                                       on = .(panel_id),
                                       allow.cartesian = TRUE,
                                       nomatch = NULL]
leaks_install_pairs <- leaks_install_pairs[!is.na(report_date) &
                                              !is.na(tank_install_date)]
leaks_install_pairs[, days_after_install :=
                       as.numeric(report_date - tank_install_date)]
leaks_install_pairs[, `:=`(
  is_install_prim = days_after_install >= 0 & days_after_install <= 60,
  is_install_nar  = days_after_install >= 0 & days_after_install <= 30,
  is_install_wid  = days_after_install >= 0 & days_after_install <= 90,
  is_install_reg  = days_after_install >= 0 & days_after_install <= 45
)]

leak_install_flags <- leaks_install_pairs[, .(
  any_install_prim = as.integer(any(is_install_prim, na.rm = TRUE)),
  any_install_nar  = as.integer(any(is_install_nar,  na.rm = TRUE)),
  any_install_wid  = as.integer(any(is_install_wid,  na.rm = TRUE)),
  any_install_reg  = as.integer(any(is_install_reg,  na.rm = TRUE))
), by = .(panel_id, panel_year = year(report_date))]

fac_stock <- merge(fac_stock, leak_install_flags,
                   by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in c("any_install_prim", "any_install_nar",
            "any_install_wid", "any_install_reg")) {
  fac_stock[is.na(get(v)), (v) := 0L]
}

# Convenience flag (uses primary 0-60d window, matching closure-side default)
fac_stock[, leak_first_year_churn := any_install_prim]

log_step(sprintf(
  "Leak first-year-churn flags (0-60d): %s of %s leak-fac-years (%.1f%%)",
  fmt_n(fac_stock[leak_year == 1L & leak_first_year_churn == 1L, .N]),
  fmt_n(fac_stock[leak_year == 1L, .N]),
  100 * mean(fac_stock[leak_year == 1L, leak_first_year_churn == 1L])
))

rm(lust_pairs, closure_lust_flags, annual_lust_flags,
   closures_for_lust, leaks_for_lust,
   installs_for_lust, leaks_install_pairs, leak_install_flags)
log_step("LUST classification complete.")

setorder(fac_stock, panel_id, panel_year)
fac_stock[, `:=`(
  cumulative_leaks   = cumsum(n_leak_incidents),
  cumulative_reports = cumsum(n_leak_reports),
  ever_leaked        = as.integer(cumsum(n_leak_incidents) > 0),
  has_previous_leak  = as.integer(shift(cumsum(n_leak_incidents), 1L, fill = 0L) > 0)
), by = panel_id]

# Canonical first-leak event indicator. Downstream hazard estimation (01n)
# can filter on `leak_first_year_churn == 0L` to drop install-artifact leaks.
fac_stock[, event_first_leak := as.integer(leak_year == 1L &
                                            has_previous_leak == 0L)]

# ---- S13.2 Texas FR data ----
cat("\nS13.2: Texas FR...\n")
fa_monthly <- fread(here("Data", "Processed", "texas_fr_facility_month_panel.csv"))
fa_monthly[, `:=`(
  facility_id = toupper(trimws(as.character(FACILITY_ID))),
  panel_id    = paste(toupper(trimws(as.character(FACILITY_ID))), "TX", sep = "_")
)]

setorder(fa_monthly, panel_id, YEAR, MONTH)
fa_dec <- fa_monthly[, .SD[.N], by = .(panel_id, YEAR)]
fr_dec_vars <- intersect(
  c("uses_private", "uses_self", "fr_covered", "DETAIL_TYPE", "CATEGORY"),
  names(fa_dec)
)
fa_dec_slim <- fa_dec[, c("panel_id", "YEAR", fr_dec_vars), with = FALSE]
setnames(fa_dec_slim, "YEAR", "panel_year")
for (v in fr_dec_vars) setnames(fa_dec_slim, v, paste0(v, "_dec"))

fa_annual <- fa_monthly[, .(
  months_covered   = sum(fr_covered == TRUE, na.rm = TRUE),
  months_with_data = .N,
  share_private    = mean(uses_private, na.rm = TRUE),
  share_self       = mean(uses_self,    na.rm = TRUE),
  n_transitions    = sum(transition_month, na.rm = TRUE),
  any_transition   = as.integer(max(transition_month, na.rm = TRUE) > 0),
  n_gap_months     = sum(fr_covered == FALSE, na.rm = TRUE)
), by = .(panel_id, panel_year = YEAR)]
fa_annual[, `:=`(
  any_gap           = as.integer(n_gap_months > 0),
  fr_coverage_share = months_covered / 12,
  fr_stable         = as.integer(n_transitions == 0 & n_gap_months == 0)
)]

fac_stock <- merge(fac_stock, fa_dec_slim, by = c("panel_id", "panel_year"), all.x = TRUE)
fac_stock <- merge(fac_stock, fa_annual,   by = c("panel_id", "panel_year"), all.x = TRUE)

for (v in names(fa_dec_slim)[-(1:2)]) {
  if (is.character(fac_stock[[v]])) fac_stock[is.na(get(v)), (v) := "None"]
  else fac_stock[is.na(get(v)), (v) := 0]
}
for (v in c("months_covered", "months_with_data", "share_private", "share_self",
            "n_transitions", "any_transition", "n_gap_months", "any_gap",
            "fr_coverage_share", "fr_stable"))
  fac_stock[is.na(get(v)), (v) := 0]

zurich_lu <- fread(here("Data", "Processed", "zurich_2012_lookup.csv"))
zurich_lu[, panel_id := paste(toupper(trimws(as.character(FACILITY_ID))), "TX", sep = "_")]
fac_stock <- merge(fac_stock, unique(zurich_lu[, .(panel_id, had_zurich_2012)]),
                   by = "panel_id", all.x = TRUE)
fac_stock[is.na(had_zurich_2012), had_zurich_2012 := 0L]

fac_stock[state == "TX", dropped_by_zurich := as.integer(
  had_zurich_2012 == 1L & panel_year >= 2012L &
  (is.na(CATEGORY_dec) | CATEGORY_dec == "None")
)]
fac_stock[is.na(dropped_by_zurich), dropped_by_zurich := 0L]

log_step(sprintf("TX FR: %s fac-years with data",
  fmt_n(fac_stock[state == "TX" & months_with_data > 0, .N])))

rm(fa_monthly, fa_dec, fa_dec_slim, fa_annual, zurich_lu)

# ---- S13.3 Control-state FR fee data ----
cat("\nS13.3: Control-state FR fees...\n")
ctrl_fr <- fread(here("Data", "Raw", "state_fr_premium.csv"),
                 na.strings = c("", "NA"))
ctrl_fr[, state := toupper(trimws(state))]
setnames(ctrl_fr, "year", "panel_year")
ctrl_fr[, notes := NULL]

ctrl_fr_numeric <- c("fr_premium_per_tank_yr", "deductible_usd",
                     "coverage_per_occ_usd", "fund_epa_approved_fr", "fund_suspended")
for (v in ctrl_fr_numeric) ctrl_fr[is.na(get(v)), (v) := 0]
ctrl_fr[is.na(fr_mechanism),          fr_mechanism          := "state_fund"]
ctrl_fr[is.na(operator_fr_cost_type), operator_fr_cost_type := "automatic_zero"]

fac_stock <- merge(fac_stock, ctrl_fr, by = c("state", "panel_year"), all.x = TRUE)

log_step(sprintf("Control FR: %s fac-years matched | %s states",
  fmt_n(fac_stock[!is.na(fr_mechanism), .N]),
  uniqueN(fac_stock[!is.na(fr_mechanism), state])))

rm(ctrl_fr)
cat("\n")



#### S14 Assemble Final Facility Panel ####

cat("========================================\n")
cat("S14: ASSEMBLE FINAL FACILITY PANEL\n")
cat("========================================\n\n")

facility_panel <- merge(
  fac_stock,
  fac_bio[, .(panel_id, first_install_yr, last_install_yr,
              first_install_date, last_install_date,
              total_tanks_ever, total_tanks_closed_ever, total_capacity_ever,
              max_capacity_ever, mean_capacity_ever,
              total_sw_ever, total_dw_ever,
              total_gasoline_ever, total_diesel_ever,
              prop_sw_ever, prop_dw_ever, prop_gasoline_ever, prop_diesel_ever,
              had_pre1965_tanks, had_pre1975_tanks, had_pre1989_tanks,
              all_post1988, all_post1989, ever_had_sw, ever_had_dw,
              total_tanks_bin, sw_share_ever_bin,
              fac_is_incumbent, fac_vintage, birth_cohort_bin)],
  by = "panel_id", all.x = TRUE
)

facility_panel <- merge(
  facility_panel,
  fac_mm_reform[, .(panel_id, make_model_fac,
    fac_wall_reform, fac_fuel_reform, oldest_age_bin,
    n_tanks_at_reform, n_sw_at_reform, n_dw_at_reform,
    total_capacity_reform, oldest_age_reform,
    any_sw_at_reform, any_dw_at_reform,
    n_tanks_bin, n_sw_bin, capacity_bin, sw_share_bin,
    prop_sw, prop_dw, prop_gasoline, prop_diesel,
    mean_capacity, sd_capacity,
    oldest_install_yr_at_reform, newest_install_yr_at_reform)],
  by = "panel_id", all.x = TRUE
)

facility_panel[, `:=`(
  install_year = first_install_yr,
  cohort       = fifelse(fac_is_incumbent == 1L, "Incumbent", "Entrant"),
  is_incumbent = fac_is_incumbent
)]

# True years since facility's first-ever tank install.
# first_install_yr from fac_bio covers pre-PANEL_START history.
# For pre-1985 incumbents this differs substantially from years_in_panel,
# which is capped at 1985 by PANEL_START.
facility_panel[, yrs_since_first_install := as.integer(panel_year) - first_install_yr]

facility_panel[, wall_type := fcase(
  has_double_walled_dec == 1L & has_single_walled_dec == 0L, "Double-Walled",
  has_single_walled_dec == 1L & has_double_walled_dec == 0L, "Single-Walled",
  has_double_walled_dec == 1L & has_single_walled_dec == 1L, "Mixed",
  default = "Unknown"
)]

# Clean SW vs DW dichotomy for downstream binary analyses (DCM, descriptive
# tables, prose). NA for Mixed/Unknown so they're naturally excluded from
# binary-wall views without affecting the full-portfolio prediction sample
# in 01n / 05.
facility_panel[, wall_pure := fcase(
  wall_type == "Single-Walled", "Single-Walled",
  wall_type == "Double-Walled", "Double-Walled",
  default = NA_character_
)]
facility_panel[, is_pure_wall := as.integer(!is.na(wall_pure))]
facility_panel[, age_bins := cut(avg_tank_age_dec,
  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf),
  include.lowest = TRUE,
  labels = c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24+")
)]
facility_panel[, `:=`(
  exit_no_leak           = as.integer(facility_exit == 1L & leak_year == 0L),
  exit_with_leak         = as.integer(facility_exit == 1L & leak_year == 1L),
  exit_no_retrofit       = as.integer(facility_exit == 1L & replacement_closure_year == 0L),
  exit_with_retrofit     = as.integer(facility_exit == 1L & replacement_closure_year == 1L),
  leak_not_found_by_exit = as.integer(facility_exit == 1L & leak_year == 0L)
)]

log_step(sprintf("Facility panel: %s rows | %s fac | exits=%s | complete_closures=%s | entries=%s",
  fmt_n(nrow(facility_panel)),
  fmt_n(uniqueN(facility_panel$panel_id)),
  fmt_n(facility_panel[facility_exit == 1L, .N]),
  fmt_n(facility_panel[facility_complete_closure == 1L, .N]),
  fmt_n(facility_panel[facility_entry == 1L, .N])))
cat("\n")


#### S15 Facility Reform-Survivor Matching (CEM 4-way + 5-way + PSM) ####

cat("========================================\n")
cat("S15: FAC | REFORM-SURVIVOR | CEM + PSM\n")
cat("========================================\n\n")

# Pool: facilities with at least one tank alive on Dec 22, 1998
fac_alive_reform <- fac_mm_reform[
  !is.na(make_model_fac) & !is.na(n_tanks_bin) & !is.na(n_sw_bin)
]
fac_alive_reform <- merge(
  fac_alive_reform,
  unique(fac_stock[, .(panel_id, texas_treated)]),
  by = "panel_id"
)

n_tx_fac  <- fac_alive_reform[texas_treated == 1L, .N]
n_ctl_fac <- fac_alive_reform[texas_treated == 0L, .N]
log_step(sprintf("Reform-pool facilities: %s TX | %s CTL",
  fmt_n(n_tx_fac), fmt_n(n_ctl_fac)))

fac_alive_reform[, `:=`(
  n_tanks_bin_f  = as.factor(n_tanks_bin),
  n_sw_bin_f     = as.factor(n_sw_bin),
  fac_fuel_f     = as.factor(fac_fuel_reform),
  age_bin_f      = as.factor(oldest_age_bin),
  capacity_bin_f = as.factor(capacity_bin)
)]

# ---- CEM 4-way (primary) ----
m_fac_cem4 <- matchit(
  texas_treated ~ n_tanks_bin_f + n_sw_bin_f + fac_fuel_f + age_bin_f,
  data   = as.data.frame(fac_alive_reform),
  method = "exact"
)
fac_alive_reform[, fac_cem_weight := m_fac_cem4$weights]
ids_fac_reform_cem <- fac_alive_reform[fac_cem_weight > 0, panel_id]

log_step(sprintf("Fac-Reform-CEM4: %s matched (TX=%s, CTL=%s)",
  fmt_n(length(ids_fac_reform_cem)),
  fmt_n(fac_alive_reform[fac_cem_weight > 0 & texas_treated == 1L, .N]),
  fmt_n(fac_alive_reform[fac_cem_weight > 0 & texas_treated == 0L, .N])))

matched_facs_reform_cem <- merge(
  facility_panel[panel_id %in% ids_fac_reform_cem],
  fac_alive_reform[, .(panel_id, fac_cem_weight)],
  by = "panel_id", all.x = TRUE
)
setnames(matched_facs_reform_cem, "fac_cem_weight", "cem_weight")

# ---- CEM 5-way (+capacity) ----
fac_alive_cap <- fac_alive_reform[!is.na(capacity_bin)]
m_fac_cem5 <- matchit(
  texas_treated ~ n_tanks_bin_f + n_sw_bin_f + fac_fuel_f + age_bin_f + capacity_bin_f,
  data   = as.data.frame(fac_alive_cap),
  method = "exact"
)
fac_alive_cap[, fac_cem5_weight := m_fac_cem5$weights]
ids_fac_reform_cem5 <- fac_alive_cap[fac_cem5_weight > 0, panel_id]

log_step(sprintf("Fac-Reform-CEM5: %s matched (TX=%s, CTL=%s)",
  fmt_n(length(ids_fac_reform_cem5)),
  fmt_n(fac_alive_cap[fac_cem5_weight > 0 & texas_treated == 1L, .N]),
  fmt_n(fac_alive_cap[fac_cem5_weight > 0 & texas_treated == 0L, .N])))

# ---- PSM (rich continuous covariate set) ----
psm_data_fac_reform <- fac_alive_reform[
  !is.na(n_tanks_at_reform)         & !is.na(prop_sw)              &
  !is.na(prop_dw)                   & !is.na(mean_capacity)        &
  !is.na(sd_capacity)               & !is.na(oldest_install_yr_at_reform) &
  !is.na(newest_install_yr_at_reform) &
  !is.na(prop_gasoline)             & !is.na(prop_diesel)
]

psm_mm_fac <- ~ n_tanks_at_reform + prop_sw + prop_dw +
  mean_capacity + sd_capacity +
  oldest_install_yr_at_reform + newest_install_yr_at_reform +
  prop_gasoline + prop_diesel - 1
psm_mahvars_fac <- ~ n_tanks_at_reform + prop_sw + prop_dw +
  mean_capacity + sd_capacity +
  oldest_install_yr_at_reform + newest_install_yr_at_reform +
  prop_gasoline + prop_diesel
psm_matchit_fac <- texas_treated ~ n_tanks_at_reform + prop_sw + prop_dw +
  mean_capacity + sd_capacity +
  oldest_install_yr_at_reform + newest_install_yr_at_reform +
  prop_gasoline + prop_diesel

psm_res_fac_reform <- run_psm_glmnet(
  dt                = psm_data_fac_reform,
  treat_col         = "texas_treated",
  pscore_mm_formula = psm_mm_fac,
  mahvars_formula   = psm_mahvars_fac,
  matchit_formula   = psm_matchit_fac
)

psm_data_fac_reform <- psm_res_fac_reform$dt
psm_data_fac_reform[, psm_weight := psm_res_fac_reform$weights]
ids_fac_reform_psm <- psm_data_fac_reform[psm_weight > 0, panel_id]

log_step(sprintf("Fac-Reform-PSM matched: %s (caliper=%.4f)",
  fmt_n(length(ids_fac_reform_psm)), psm_res_fac_reform$ir_caliper))

matched_facs_reform_psm <- merge(
  facility_panel[panel_id %in% ids_fac_reform_psm],
  psm_data_fac_reform[, .(panel_id, psm_weight, pscore, logit_pscore)],
  by = "panel_id", all.x = TRUE
)
cat("\n")



#### S16 Facility Birth-Cohort Matching (CEM + PSM) ####

cat("========================================\n")
cat("S16: FAC | BIRTH-COHORT | CEM + PSM\n")
cat("========================================\n\n")

# Pool: ALL facilities (no reform-date alive restriction, no fallback fill).
# Matching covariates are TIME-INVARIANT facility attributes from fac_bio:
#   first_install_yr, total_tanks_ever, total_tanks_closed_ever,
#   total_capacity_ever, max_capacity_ever, ever_had_sw, ever_had_dw,
#   prop_sw_ever, prop_dw_ever, prop_gasoline_ever, prop_diesel_ever
# CEM dimensions: birth_cohort_bin x total_tanks_bin x sw_share_ever_bin
#
# RATIONALE: birth-cohort matching is meant to study mandate pressure and
# parallel trends violations absent the snapshot-at-reform conditioning.
# Using time-invariant attributes ensures the matching pool is symmetric
# across the entire sample window (1985-2018) regardless of which facilities
# happened to have tanks alive on Dec 22, 1998.

fac_birth_pool <- copy(fac_bio[
  !is.na(birth_cohort_bin)  &
  !is.na(total_tanks_bin)   &
  !is.na(sw_share_ever_bin)
])

log_step(sprintf("Birth-pool facilities: %s (%s TX, %s CTL)",
  fmt_n(nrow(fac_birth_pool)),
  fmt_n(fac_birth_pool[texas_treated == 1L, .N]),
  fmt_n(fac_birth_pool[texas_treated == 0L, .N])))

# ---- CEM (3 time-invariant dimensions) ----
fac_birth_pool[, `:=`(
  birth_cohort_bin_f  = as.factor(birth_cohort_bin),
  total_tanks_bin_f   = as.factor(total_tanks_bin),
  sw_share_ever_bin_f = as.factor(sw_share_ever_bin)
)]

m_fac_birth_cem <- matchit(
  texas_treated ~ birth_cohort_bin_f + total_tanks_bin_f + sw_share_ever_bin_f,
  data   = as.data.frame(fac_birth_pool),
  method = "exact"
)

fac_birth_pool[, cem_weight := m_fac_birth_cem$weights]
ids_fac_birth_cem <- fac_birth_pool[cem_weight > 0, panel_id]

log_step(sprintf("Fac-Birth-CEM matched: %s (TX=%s, CTL=%s)",
  fmt_n(length(ids_fac_birth_cem)),
  fmt_n(fac_birth_pool[cem_weight > 0 & texas_treated == 1L, .N]),
  fmt_n(fac_birth_pool[cem_weight > 0 & texas_treated == 0L, .N])))

matched_facs_birth_cem <- merge(
  facility_panel[panel_id %in% ids_fac_birth_cem],
  fac_birth_pool[, .(panel_id, cem_weight)],
  by = "panel_id", all.x = TRUE
)

# ---- PSM (rich continuous time-invariant covariates) ----
psm_data_fac_birth <- fac_bio[
  !is.na(first_install_yr)        & !is.na(total_tanks_ever)        &
  !is.na(total_tanks_closed_ever) & !is.na(total_capacity_ever)     &
  !is.na(max_capacity_ever)       & !is.na(prop_sw_ever)            &
  !is.na(prop_dw_ever)            & !is.na(prop_gasoline_ever)      &
  !is.na(prop_diesel_ever)        & !is.na(ever_had_sw)             &
  !is.na(ever_had_dw)
]

psm_mm_fac_birth <- ~ first_install_yr + total_tanks_ever + total_tanks_closed_ever +
  total_capacity_ever + max_capacity_ever +
  prop_sw_ever + prop_dw_ever +
  prop_gasoline_ever + prop_diesel_ever +
  ever_had_sw + ever_had_dw - 1
psm_mahvars_fac_birth <- ~ first_install_yr + total_tanks_ever + total_tanks_closed_ever +
  total_capacity_ever + max_capacity_ever +
  prop_sw_ever + prop_dw_ever +
  prop_gasoline_ever + prop_diesel_ever +
  ever_had_sw + ever_had_dw
psm_matchit_fac_birth <- texas_treated ~ first_install_yr + total_tanks_ever + total_tanks_closed_ever +
  total_capacity_ever + max_capacity_ever +
  prop_sw_ever + prop_dw_ever +
  prop_gasoline_ever + prop_diesel_ever +
  ever_had_sw + ever_had_dw

psm_res_fac_birth <- run_psm_glmnet(
  dt                = psm_data_fac_birth,
  treat_col         = "texas_treated",
  pscore_mm_formula = psm_mm_fac_birth,
  mahvars_formula   = psm_mahvars_fac_birth,
  matchit_formula   = psm_matchit_fac_birth
)

psm_data_fac_birth <- psm_res_fac_birth$dt
psm_data_fac_birth[, psm_weight := psm_res_fac_birth$weights]
ids_fac_birth_psm <- psm_data_fac_birth[psm_weight > 0, panel_id]

log_step(sprintf("Fac-Birth-PSM matched: %s (caliper=%.4f)",
  fmt_n(length(ids_fac_birth_psm)), psm_res_fac_birth$ir_caliper))

matched_facs_birth_psm <- merge(
  facility_panel[panel_id %in% ids_fac_birth_psm],
  psm_data_fac_birth[, .(panel_id, psm_weight, pscore, logit_pscore)],
  by = "panel_id", all.x = TRUE
)
cat("\n")


#### S17 Post-Match Variable Construction ####

cat("========================================\n")
cat("S17: POST-MATCH VARIABLES\n")
cat("========================================\n\n")

# Median age at treatment computed on primary tank-CEM-reform sample
med_age <- median(matched_tanks_reform_cem$age_at_treatment, na.rm = TRUE)
log_step(sprintf("Median age at treatment (Tank-Reform-CEM): %.0f years", med_age))

# Apply to all four tank-level matched samples
add_tank_post_vars <- function(dt) {
  if (!"age_at_treatment" %in% names(dt))
    dt[, age_at_treatment := 1999L - install_yr_int]
  dt[, `:=`(
    above_median_age = as.integer(age_at_treatment >= med_age),
    below_median_age = as.integer(age_at_treatment <  med_age),
    single_wall      = as.integer(mm_wall == "Single-Walled")
  )]
  dt[, `:=`(
    did_x_old    = did_term * above_median_age,
    did_x_sw     = did_term * single_wall,
    did_x_sw_old = did_term * single_wall * above_median_age,
    sw_x_old     = single_wall * above_median_age
  )]
  dt[]
}
matched_tanks_reform_cem <- add_tank_post_vars(matched_tanks_reform_cem)
matched_tanks_reform_psm <- add_tank_post_vars(matched_tanks_reform_psm)
matched_tanks_birth_cem  <- add_tank_post_vars(matched_tanks_birth_cem)
matched_tanks_birth_psm  <- add_tank_post_vars(matched_tanks_birth_psm)

# Control means from primary sample
ctrl_means <- matched_tanks_reform_cem[texas_treated == 0L, .(
  pre_reform = mean(closure_event[panel_year %between% c(1992L, 1998L)], na.rm = TRUE),
  post_all   = mean(closure_event[panel_year >= 1999L],                  na.rm = TRUE),
  post_short = mean(closure_event[panel_year %between% c(1999L, 2004L)], na.rm = TRUE),
  post_long  = mean(closure_event[panel_year >= 2005L],                  na.rm = TRUE)
)]
ctrl_mean_post <- ctrl_means$post_all

log_step(sprintf("Control closure rates per 1,000 tank-years (Tank-Reform-CEM):"))
log_step(sprintf("  Pre-reform (1992-1998):  %.1f", ctrl_means$pre_reform * 1000), 1)
log_step(sprintf("  Post-reform (all):       %.1f", ctrl_means$post_all   * 1000), 1)
log_step(sprintf("  Post short (1999-2004):  %.1f", ctrl_means$post_short * 1000), 1)
log_step(sprintf("  Post long  (2005-2020):  %.1f", ctrl_means$post_long  * 1000), 1)

ctrl_means_dt <- data.table(
  period = c("pre_1992_1998", "post_all", "post_1999_2004", "post_2005_2020"),
  rate   = unlist(ctrl_means)
)
fwrite(ctrl_means_dt, file.path(OUTPUT_TABLES, "Diag_ControlMeans.csv"))
cat("\n")


#### T046 PA BLOCK -- pricing/hazard-only rows for panel_dt + facility_panel ####
# Ticket 046. Fully parallel pipeline mirroring S3-S6, S12.1-S12.10, S13.1,
# and S14, scoped to PRICING_STATES ("PA") only. Does NOT touch study_tanks,
# tank_year_panel, panel_dt, fac_stock, or facility_panel, and does NOT edit
# any line in S3-S17 above. PA rows are unioned onto COPIES of the existing
# STUDY_STATES objects only at export time below, so every non-PA row and
# every existing STUDY_STATES-scoped consumer (S7-S11 diagnostics/Cox base,
# S15-S16 matching, matched-sample CSVs, and the F1-F13 figures below) sees
# the exact original objects, unaffected by construction (not just by test).
# MIRRORS S3-S6 / S13.1 / S12-S14 -- keep in sync if those sections change.

cat("========================================\n")
cat("T046: PA PRICING/HAZARD BLOCK\n")
cat("========================================\n\n")

assert_cols_match <- function(main_dt, pa_dt, main_name, pa_name) {
  main_types <- vapply(main_dt, function(x) class(x)[1], character(1))
  pa_types   <- vapply(pa_dt,   function(x) class(x)[1], character(1))
  missing_in_pa <- setdiff(names(main_dt), names(pa_dt))
  extra_in_pa   <- setdiff(names(pa_dt), names(main_dt))
  if (length(missing_in_pa) > 0L)
    stop(sprintf("%s missing columns present in %s: %s",
                 pa_name, main_name, paste(missing_in_pa, collapse = ", ")))
  if (length(extra_in_pa) > 0L)
    stop(sprintf("%s has extra columns not in %s: %s",
                 pa_name, main_name, paste(extra_in_pa, collapse = ", ")))
  common <- intersect(names(main_dt), names(pa_dt))
  mismatched <- common[main_types[common] != pa_types[common]]
  if (length(mismatched) > 0L)
    stop(sprintf("Type mismatch vs %s in columns: %s",
                 main_name, paste(mismatched, collapse = ", ")))
  invisible(TRUE)
}

#### T046.1 PA tank source (MIRRORS S3) ####
cat("T046.1: PA tank source...\n")

pa_tanks <- master_tanks[state %in% PRICING_STATES & !is.na(tank_installed_date)]
# texas_treated is a formula-safe input only (did_term = texas_treated *
# post_1999), NOT a DiD-eligibility signal. PA is quarantined from the
# closure-DiD/structural sample by state %in% STUDY_STATES (false for PA)
# and by closure_did_eligible == 0 below -- never by this value.
pa_tanks[, texas_treated := 0L]

log_step(sprintf("PA tanks: %s", fmt_n(nrow(pa_tanks))))

#### T046.2 PA annual tank-year panel (MIRRORS S4) ####
cat("T046.2: PA annual tank-year panel...\n")

pa_tanks[, release_det_deadline_yr := fcase(
  install_yr_int <= 1964L,         RELEASE_DET_DEADLINE_YR$pre_1965,
  install_yr_int %in% 1965L:1969L, RELEASE_DET_DEADLINE_YR$y1965_69,
  install_yr_int %in% 1970L:1974L, RELEASE_DET_DEADLINE_YR$y1970_74,
  install_yr_int %in% 1975L:1979L, RELEASE_DET_DEADLINE_YR$y1975_79,
  install_yr_int %in% 1980L:1988L, RELEASE_DET_DEADLINE_YR$y1980_88,
  default = NA_integer_
)]

pa_tanks[, make_model_tank  := paste(mm_wall, mm_fuel, mm_capacity, mm_install_cohort, sep = "|")]
pa_tanks[, make_model_noage := paste(mm_wall, mm_fuel, mm_capacity, sep = "|")]
pa_tanks[, make_model_3dim  := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "|")]
pa_tanks[
  mm_wall == "Unknown-Wall" | mm_fuel == "Unknown-Fuel" | is.na(mm_install_cohort),
  `:=`(make_model_tank  = NA_character_,
       make_model_noage = NA_character_,
       make_model_3dim  = NA_character_)
]

pa_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date), year(tank_closed_date), PANEL_END)
)]
pa_tanks[, `:=`(
  expand_start = pmax(install_yr, PANEL_START),
  expand_end   = pmin(close_yr_raw, PANEL_END)
)]

pa_ty_panel <- pa_tanks[
  expand_start <= expand_end, .SD, .SDcols = panel_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(!is.na(tank_closed_date) & year(tank_closed_date) == yrs))
}, by = panel_cols]

setorder(pa_ty_panel, tank_panel_id, panel_year)

pa_ty_panel[, first_year_churn := as.integer(
  panel_year == install_yr_int & closure_event == 1L
)]

log_step(sprintf("PA panel: %s rows | %s tanks | %s facilities | %s first-year-churn flagged",
  fmt_n(nrow(pa_ty_panel)),
  fmt_n(uniqueN(pa_ty_panel$tank_panel_id)),
  fmt_n(uniqueN(pa_ty_panel$panel_id)),
  fmt_n(sum(pa_ty_panel$first_year_churn))))

#### T046.3 PA mandate controls (MIRRORS S5) ####
cat("T046.3: PA mandate controls...\n")

pa_ty_panel[, `:=`(
  post_1999 = as.integer(panel_year >= POST_YEAR),
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR),
  rel_year  = panel_year - POST_YEAR,
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  tank_age  = panel_year - install_yr_int
)]

pa_ty_panel[, mandate_release_det := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year >= 1989L &
  panel_year <= release_det_deadline_yr
)]
pa_ty_panel[, mandate_spill_overfill := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1992L:1994L
)]
pa_ty_panel[, mandate_integrity := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1996L:1998L
)]

pa_mandate_post_check <- pa_ty_panel[
  panel_year >= 1999L,
  .(n_release_det    = sum(mandate_release_det),
    n_spill_overfill = sum(mandate_spill_overfill),
    n_integrity      = sum(mandate_integrity))
]
stopifnot(
  "PA mandate_release_det nonzero post-1998"    = pa_mandate_post_check$n_release_det    == 0L,
  "PA mandate_spill_overfill nonzero post-1998" = pa_mandate_post_check$n_spill_overfill == 0L,
  "PA mandate_integrity nonzero post-1998"      = pa_mandate_post_check$n_integrity      == 0L
)
log_step("PA mandate overlap check passed.")

pa_ty_panel[, rel_year_bin := fcase(
  rel_year <= -14L, -14L,
  rel_year >=  16L,  16L,
  default = as.integer(rel_year)
)]

#### T046.4 PA panel_dt rows (MIRRORS S6) ####
cat("T046.4: PA panel_dt rows...\n")

pa_panel_dt <- pa_ty_panel[
  !is.na(make_model_tank) &
  !is.na(did_term)        &
  tstop > tstart
]
log_step(sprintf("PA panel_dt: %s tank-years | %s tanks",
  fmt_n(nrow(pa_panel_dt)), fmt_n(uniqueN(pa_panel_dt$tank_panel_id))))

#### T046.5 PA churn-filtered facility-year stock (MIRRORS S12.1-S12.2) ####
cat("T046.5: PA facility-year stock...\n")

pa_ty_panel_clean <- pa_ty_panel[first_year_churn == 0L | is.na(first_year_churn)]
pa_tanks_no_churn <- pa_tanks[
  is.na(tank_closed_date) | year(tank_installed_date) != year(tank_closed_date)
]

pa_fac_stock <- pa_ty_panel_clean[
  ,
  .(
    n_tanks_active        = .N,
    n_sw_active           = sum(mm_wall == "Single-Walled", na.rm = TRUE),
    n_dw_active           = sum(mm_wall == "Double-Walled", na.rm = TRUE),
    n_unk_wall_active     = sum(mm_wall == "Unknown-Wall",  na.rm = TRUE),
    n_gasoline_active     = sum(mm_fuel == "Gasoline-Only", na.rm = TRUE),
    n_diesel_active       = sum(mm_fuel == "Diesel-Only",   na.rm = TRUE),
    n_other_fuel_active   = sum(!mm_fuel %in% c("Gasoline-Only", "Diesel-Only"), na.rm = TRUE),
    total_capacity        = sum(capacity, na.rm = TRUE),
    avg_tank_age          = mean(tank_age, na.rm = TRUE),
    max_tank_age          = fifelse(any(!is.na(tank_age)), max(tank_age, na.rm = TRUE), NA_real_),
    min_tank_age          = fifelse(any(!is.na(tank_age)), min(tank_age, na.rm = TRUE), NA_real_),
    n_closures   = sum(closure_event[first_year_churn == 0L | is.na(first_year_churn)],
                       na.rm = TRUE),
    any_closure  = as.integer(
      sum(closure_event[first_year_churn == 0L | is.na(first_year_churn)],
          na.rm = TRUE) > 0L),
    n_sw_closures = sum(
      closure_event == 1L & mm_wall == "Single-Walled" &
      (first_year_churn == 0L | is.na(first_year_churn)),
      na.rm = TRUE),
    n_dw_closures = sum(
      closure_event == 1L & mm_wall == "Double-Walled" &
      (first_year_churn == 0L | is.na(first_year_churn)),
      na.rm = TRUE),
    any_sw_closure = as.integer(sum(
      closure_event == 1L & mm_wall == "Single-Walled" &
      (first_year_churn == 0L | is.na(first_year_churn)),
      na.rm = TRUE) > 0L),
    capacity_closed = sum(
      fifelse(closure_event == 1L &
              (first_year_churn == 0L | is.na(first_year_churn)),
              capacity, 0),
      na.rm = TRUE),
    any_mandate_release_det    = as.integer(any(mandate_release_det == 1L,    na.rm = TRUE)),
    any_mandate_spill_overfill = as.integer(any(mandate_spill_overfill == 1L, na.rm = TRUE)),
    any_mandate_integrity      = as.integer(any(mandate_integrity == 1L,      na.rm = TRUE)),
    n_tanks_identified  = sum(!is.na(make_model_tank)),
    n_cells_represented = uniqueN(make_model_tank, na.rm = TRUE),
    county_name   = first(county_name),
    county_fips   = first(county_fips),
    latitude      = first(latitude),
    longitude     = first(longitude),
    texas_treated = first(texas_treated),
    state         = first(state),
    facility_id   = first(facility_id)
  ),
  by = .(panel_id, panel_year)
]

pa_fac_stock[, `:=`(
  n_tanks_eoy  = n_tanks_active - n_closures,
  n_sw_eoy     = n_sw_active    - n_sw_closures,
  n_dw_eoy     = n_dw_active    - n_dw_closures,
  capacity_eoy = total_capacity - capacity_closed
)]
pa_fac_stock[, `:=`(
  active_tanks_dec      = n_tanks_eoy,
  single_tanks_dec      = n_sw_eoy,
  double_tanks_dec      = n_dw_eoy,
  total_capacity_dec    = capacity_eoy,
  avg_tank_age_dec      = avg_tank_age,
  has_single_walled_dec = as.integer(n_sw_eoy > 0),
  has_double_walled_dec = as.integer(n_dw_eoy > 0)
)]
pa_fac_stock[, `:=`(
  active_tanks = n_tanks_eoy,
  single_tanks = n_sw_eoy,
  double_tanks = n_dw_eoy,
  has_single_walled = as.integer(n_sw_active > 0),
  has_double_walled = as.integer(n_dw_active > 0),
  has_unknown_wall  = as.integer(n_unk_wall_active > 0),
  pct_sw       = n_sw_eoy / pmax(n_tanks_eoy, 1L),
  pct_dw       = n_dw_eoy / pmax(n_tanks_eoy, 1L),
  all_sw       = as.integer(n_sw_eoy == n_tanks_eoy & n_tanks_eoy > 0),
  all_dw       = as.integer(n_dw_eoy == n_tanks_eoy & n_tanks_eoy > 0),
  mixed_wall   = as.integer(n_sw_eoy > 0 & n_dw_eoy > 0),
  has_gasoline = as.integer(n_gasoline_active > 0),
  has_diesel   = as.integer(n_diesel_active > 0),
  is_motor_fuel = as.integer(n_gasoline_active > 0 | n_diesel_active > 0)
)]

log_step(sprintf("PA facility-year stock: %s rows | %s facilities",
  fmt_n(nrow(pa_fac_stock)), fmt_n(uniqueN(pa_fac_stock$panel_id))))

#### T046.6 PA installs (MIRRORS S12.3) ####
cat("T046.6: PA installs...\n")

pa_installs_by_fac_yr <- pa_tanks_no_churn[
  !is.na(tank_installed_date),
  .(
    n_installs         = .N,
    n_dw_installs      = sum(mm_wall == "Double-Walled", na.rm = TRUE),
    n_sw_installs      = sum(mm_wall == "Single-Walled", na.rm = TRUE),
    capacity_installed = sum(capacity, na.rm = TRUE)
  ),
  by = .(panel_id, panel_year = year(tank_installed_date))
]
pa_fac_stock <- merge(pa_fac_stock, pa_installs_by_fac_yr,
                      by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in c("n_installs", "n_dw_installs", "n_sw_installs", "capacity_installed"))
  pa_fac_stock[is.na(get(v)), (v) := 0L]

#### T046.7 PA closure type classification (MIRRORS S12.4) ####
cat("T046.7: PA closure type classification...\n")

pa_fac_max_install <- pa_tanks[
  !is.na(tank_installed_date),
  .(fac_max_install_date = max(tank_installed_date, na.rm = TRUE)),
  by = panel_id
]
pa_closed_tanks_fac <- pa_tanks[
  !is.na(tank_closed_date) &
  year(tank_closed_date) != year(tank_installed_date),
  .(panel_id, tank_panel_id, tank_closed_date, tank_installed_date,
    mm_wall, mm_fuel, capacity)
]
pa_closed_tanks_fac <- merge(pa_closed_tanks_fac, pa_fac_max_install, by = "panel_id", all.x = TRUE)
pa_closed_tanks_fac[, is_replacement := as.integer(
  !is.na(fac_max_install_date) & fac_max_install_date > tank_closed_date
)]
pa_closed_tanks_fac[is.na(is_replacement), is_replacement := 0L]
pa_closed_tanks_fac[, is_sw_replacement := as.integer(
  is_replacement == 1L & mm_wall == "Single-Walled"
)]

pa_closure_type_agg <- pa_closed_tanks_fac[, .(
  n_closures_permanent   = sum(is_replacement == 0L),
  n_closures_replacement = sum(is_replacement == 1L),
  n_sw_replacement       = sum(is_sw_replacement, na.rm = TRUE),
  capacity_permanent     = sum(capacity[is_replacement == 0L], na.rm = TRUE),
  capacity_replacement   = sum(capacity[is_replacement == 1L], na.rm = TRUE)
), by = .(panel_id, panel_year = year(tank_closed_date))]

pa_fac_stock <- merge(pa_fac_stock, pa_closure_type_agg,
                      by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in c("n_closures_permanent", "n_closures_replacement", "n_sw_replacement",
            "capacity_permanent", "capacity_replacement"))
  pa_fac_stock[is.na(get(v)), (v) := 0L]

pa_fac_stock[, `:=`(
  permanent_closure_year   = as.integer(n_closures_permanent > 0),
  replacement_closure_year = as.integer(n_closures_replacement > 0),
  single_to_double_year    = as.integer(n_sw_replacement > 0 & n_dw_installs > 0)
)]

rm(pa_fac_max_install, pa_closed_tanks_fac, pa_closure_type_agg)

#### T046.8 PA YoY changes (MIRRORS S12.5) ####
cat("T046.8: PA YoY changes...\n")

setorder(pa_fac_stock, panel_id, panel_year)
pa_fac_stock[, `:=`(
  lag_n_tanks   = shift(n_tanks_eoy,           1L, type = "lag"),
  lag_n_sw      = shift(n_sw_eoy,              1L, type = "lag"),
  lag_n_dw      = shift(n_dw_eoy,              1L, type = "lag"),
  lag_pct_sw    = shift(pct_sw,                1L, type = "lag"),
  lag_avg_age   = shift(avg_tank_age,          1L, type = "lag"),
  lag_capacity  = shift(capacity_eoy,          1L, type = "lag"),
  lag_has_sw    = shift(has_single_walled_dec, 1L, type = "lag"),
  lag_has_dw    = shift(has_double_walled_dec, 1L, type = "lag")
), by = panel_id]
pa_fac_stock[, `:=`(
  net_tank_change    = n_tanks_eoy  - lag_n_tanks,
  delta_pct_sw       = pct_sw       - lag_pct_sw,
  delta_avg_age      = avg_tank_age - lag_avg_age,
  capacity_change    = capacity_eoy - lag_capacity,
  lost_all_sw        = as.integer(lag_has_sw == 1L & has_single_walled_dec == 0L),
  gained_dw          = as.integer(lag_has_dw == 0L & has_double_walled_dec == 1L),
  capacity_increased = as.integer(!is.na(lag_capacity) & capacity_eoy > lag_capacity),
  capacity_decreased = as.integer(!is.na(lag_capacity) & capacity_eoy < lag_capacity)
)]

#### T046.9 PA facility biography (MIRRORS S12.6) ####
cat("T046.9: PA facility biography...\n")

pa_fac_bio <- pa_tanks[, .(
  first_install_yr     = min(year(tank_installed_date), na.rm = TRUE),
  last_install_yr      = max(year(tank_installed_date), na.rm = TRUE),
  first_install_date   = min(tank_installed_date, na.rm = TRUE),
  last_install_date    = max(tank_installed_date, na.rm = TRUE),
  total_tanks_ever     = uniqueN(tank_panel_id),
  total_tanks_closed_ever = sum(!is.na(tank_closed_date)),
  total_capacity_ever  = sum(capacity, na.rm = TRUE),
  max_capacity_ever    = max(capacity, na.rm = TRUE),
  mean_capacity_ever   = mean(capacity, na.rm = TRUE),
  total_sw_ever        = sum(mm_wall == "Single-Walled", na.rm = TRUE),
  total_dw_ever        = sum(mm_wall == "Double-Walled", na.rm = TRUE),
  total_gasoline_ever  = sum(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  total_diesel_ever    = sum(mm_fuel == "Diesel-Only",   na.rm = TRUE),
  prop_sw_ever         = mean(mm_wall == "Single-Walled", na.rm = TRUE),
  prop_dw_ever         = mean(mm_wall == "Double-Walled", na.rm = TRUE),
  prop_gasoline_ever   = mean(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  prop_diesel_ever     = mean(mm_fuel == "Diesel-Only",   na.rm = TRUE),
  had_pre1965_tanks    = as.integer(any(year(tank_installed_date) <= 1964L, na.rm = TRUE)),
  had_pre1975_tanks    = as.integer(any(year(tank_installed_date) <= 1974L, na.rm = TRUE)),
  had_pre1989_tanks    = as.integer(any(year(tank_installed_date) <= 1988L, na.rm = TRUE)),
  all_post1988         = as.integer(all(year(tank_installed_date) >= 1989L, na.rm = TRUE)),
  all_post1989         = as.integer(all(year(tank_installed_date) >= 1990L, na.rm = TRUE)),
  ever_had_sw          = as.integer(any(mm_wall == "Single-Walled", na.rm = TRUE)),
  ever_had_dw          = as.integer(any(mm_wall == "Double-Walled", na.rm = TRUE)),
  texas_treated        = first(texas_treated),
  state                = first(state)
), by = panel_id]

for (col in setdiff(names(pa_fac_bio), c("panel_id", "state"))) {
  if (is.numeric(pa_fac_bio[[col]])) {
    set(pa_fac_bio, which(!is.finite(pa_fac_bio[[col]])), col, NA)
  }
}

pa_fac_bio[, total_tanks_bin := fcase(
  total_tanks_ever == 1L,                 "1",
  total_tanks_ever %between% c(2L, 3L),   "2-3",
  total_tanks_ever %between% c(4L, 6L),   "4-6",
  total_tanks_ever %between% c(7L, 10L),  "7-10",
  total_tanks_ever >= 11L,                "11+",
  default = NA_character_
)]
pa_fac_bio[, sw_share_ever_bin := fcase(
  prop_sw_ever == 1,    "All SW",
  prop_sw_ever >  0.5,  "Majority SW",
  prop_sw_ever <= 0.5,  "Minority/No SW",
  default = NA_character_
)]
pa_fac_bio[, fac_is_incumbent := as.integer(first_install_yr < 1999L)]
pa_fac_bio[, fac_vintage := fcase(
  first_install_yr <  1975L,                    "Pre-1975",
  first_install_yr %between% c(1975L, 1984L),   "1975-1984",
  first_install_yr %between% c(1985L, 1988L),   "1985-1988",
  first_install_yr %between% c(1989L, 1998L),   "1989-1998",
  first_install_yr >= 1999L,                    "Post-Reform",
  default = "Unknown"
)]
pa_fac_bio[, birth_cohort_bin := fcase(
  first_install_yr <= 1964L,                    "Pre-1965",
  first_install_yr %between% c(1965L, 1979L),   "1965-1979",
  first_install_yr %between% c(1980L, 1988L),   "1980-1988",
  first_install_yr %between% c(1989L, 1998L),   "1989-1998",
  first_install_yr >= 1999L,                    "Post-1998",
  default = NA_character_
)]

log_step(sprintf("PA biography: %s facilities", fmt_n(nrow(pa_fac_bio))))

#### T046.10 PA reform-date portfolio snapshot (MIRRORS S12.7) ####
# alive-at-reform computed directly from pa_tanks (same date logic as
# exact_base's t_enter/t_exit and S10's alive_at_reform), since exact_base
# itself is not built for PA (the Cox base stays STUDY_STATES-only, D5).
cat("T046.10: PA reform-date portfolio snapshot...\n")

pa_reform_tank_ids <- pa_tanks[
  tank_installed_date <= REFORM_DATE &
  (is.na(tank_closed_date) | tank_closed_date > REFORM_DATE),
  tank_panel_id
]
pa_tanks_at_reform_fac <- pa_tanks[
  tank_panel_id %in% pa_reform_tank_ids,
  .(panel_id, tank_panel_id, mm_wall, mm_fuel, mm_capacity,
    capacity, install_yr_int,
    age_at_reform = as.numeric(REFORM_DATE - tank_installed_date) / 365.25)
]

pa_fac_mm_reform <- pa_tanks_at_reform_fac[, .(
  n_tanks_at_reform     = .N,
  n_sw_at_reform        = sum(mm_wall == "Single-Walled", na.rm = TRUE),
  n_dw_at_reform        = sum(mm_wall == "Double-Walled", na.rm = TRUE),
  total_capacity_reform = sum(capacity, na.rm = TRUE),
  oldest_age_reform     = max(age_at_reform, na.rm = TRUE),
  newest_age_reform     = min(age_at_reform, na.rm = TRUE),
  avg_age_reform        = mean(age_at_reform, na.rm = TRUE),
  oldest_install_yr_at_reform = as.integer(min(install_yr_int, na.rm = TRUE)),
  newest_install_yr_at_reform = as.integer(max(install_yr_int, na.rm = TRUE)),
  mean_capacity         = mean(capacity, na.rm = TRUE),
  sd_capacity           = sd(capacity,   na.rm = TRUE),
  prop_sw               = mean(mm_wall == "Single-Walled", na.rm = TRUE),
  prop_dw               = mean(mm_wall == "Double-Walled", na.rm = TRUE),
  prop_gasoline         = mean(mm_fuel == "Gasoline-Only", na.rm = TRUE),
  prop_diesel           = mean(mm_fuel == "Diesel-Only",   na.rm = TRUE),
  fac_wall_reform = fcase(
    any(mm_wall == "Unknown-Wall"),  "Unknown",
    all(mm_wall == "Single-Walled"), "All-SW",
    all(mm_wall == "Double-Walled"), "All-DW",
    default = "Mixed"
  ),
  any_sw_at_reform = as.integer(any(mm_wall == "Single-Walled")),
  any_dw_at_reform = as.integer(any(mm_wall == "Double-Walled")),
  fac_fuel_reform = fcase(
    all(mm_fuel == "Gasoline-Only"), "Gasoline-Only",
    all(mm_fuel == "Diesel-Only"),   "Diesel-Only",
    all(mm_fuel == "Unknown-Fuel"),  "Unknown",
    default = "Mixed-Or-Other"
  )
), by = panel_id]

for (col in setdiff(names(pa_fac_mm_reform), "panel_id")) {
  if (is.numeric(pa_fac_mm_reform[[col]])) {
    set(pa_fac_mm_reform, which(!is.finite(pa_fac_mm_reform[[col]])), col, NA)
  }
}

pa_fac_mm_reform[, n_tanks_bin := fcase(
  n_tanks_at_reform == 1L,                   "1",
  n_tanks_at_reform %between% c(2L, 3L),     "2-3",
  n_tanks_at_reform %between% c(4L, 6L),     "4-6",
  n_tanks_at_reform %between% c(7L, 10L),    "7-10",
  n_tanks_at_reform >= 11L,                   "11+",
  default = NA_character_
)]
pa_fac_mm_reform[, n_sw_bin := fcase(
  n_sw_at_reform == 0L,                      "0",
  n_sw_at_reform == 1L,                      "1",
  n_sw_at_reform %between% c(2L, 3L),        "2-3",
  n_sw_at_reform >= 4L,                       "4+",
  default = NA_character_
)]
pa_fac_mm_reform[, capacity_bin := fcase(
  is.na(total_capacity_reform) | total_capacity_reform == 0, NA_character_,
  total_capacity_reform < 10000,                              "Under-10k",
  total_capacity_reform %between% c(10000, 29999),            "10k-30k",
  total_capacity_reform %between% c(30000, 59999),            "30k-60k",
  total_capacity_reform %between% c(60000, 99999),            "60k-100k",
  total_capacity_reform >= 100000,                             "100k+",
  default = NA_character_
)]
pa_fac_mm_reform[, oldest_age_bin := fcase(
  oldest_age_reform <   5,                "0-4yr",
  oldest_age_reform %between% c( 5,  9),  "5-9yr",
  oldest_age_reform %between% c(10, 14),  "10-14yr",
  oldest_age_reform %between% c(15, 19),  "15-19yr",
  oldest_age_reform %between% c(20, 24),  "20-24yr",
  oldest_age_reform >= 25,                "25yr+",
  default = NA_character_
)]
pa_fac_mm_reform[, sw_share_bin := fcase(
  prop_sw == 1,    "All SW",
  prop_sw >  0.5,  "Majority SW",
  prop_sw <= 0.5,  "Minority/No SW",
  default = NA_character_
)]
pa_fac_mm_reform[, make_model_fac := fifelse(
  !is.na(oldest_age_bin) & fac_wall_reform != "Unknown",
  paste(fac_wall_reform, fac_fuel_reform, oldest_age_bin, sep = "|"),
  NA_character_
)]

log_step(sprintf("PA reform snapshot: %s facilities | identified cell: %s",
  fmt_n(nrow(pa_fac_mm_reform)),
  fmt_n(pa_fac_mm_reform[!is.na(make_model_fac), .N])))

rm(pa_reform_tank_ids, pa_tanks_at_reform_fac)

#### T046.11 PA DiD time vars + exit/entry + cumulatives (MIRRORS S12.8-S12.10) ####
cat("T046.11: PA time variables, exit/entry, cumulatives...\n")

pa_fac_stock[, `:=`(
  post_1999   = as.integer(panel_year >= POST_YEAR),
  did_term    = texas_treated * as.integer(panel_year >= POST_YEAR),
  did_short   = texas_treated * as.integer(panel_year >= 1999L & panel_year <= 2004L),
  did_long    = texas_treated * as.integer(panel_year >= 2005L),
  rel_year    = panel_year - POST_YEAR,
  rel_year_es = pmax(pmin(panel_year - POST_YEAR, 15L), -12L)
)]

setorder(pa_fac_stock, panel_id, panel_year)
pa_fac_stock[, last_panel_year  := max(panel_year), by = panel_id]
pa_fac_stock[, first_panel_year := min(panel_year), by = panel_id]
pa_fac_stock[, facility_exit := as.integer(
  panel_year == last_panel_year & last_panel_year < 2020L
)]
pa_fac_stock[, facility_complete_closure := as.integer(
  n_closures > 0 & n_tanks_eoy == 0L
)]
pa_fac_stock[, facility_entry := as.integer(
  panel_year == first_panel_year & first_panel_year > 1985L
)]
pa_fac_stock[, post_reform_entrant := as.integer(first_panel_year >= 1999L)]
pa_fac_stock[, years_in_panel := panel_year - first_panel_year]

setorder(pa_fac_stock, panel_id, panel_year)
pa_fac_stock[, `:=`(
  cumulative_closures        = cumsum(n_closures),
  cumulative_sw_closures     = cumsum(n_sw_closures),
  cumulative_dw_closures     = cumsum(n_dw_closures),
  cumulative_replacements    = cumsum(n_closures_replacement),
  cumulative_permanent       = cumsum(n_closures_permanent),
  cumulative_capacity_closed = cumsum(capacity_closed),
  ever_closed_tank           = as.integer(cumsum(n_closures) > 0),
  cumulative_installs           = cumsum(n_installs),
  cumulative_sw_installs        = cumsum(n_sw_installs),
  cumulative_dw_installs        = cumsum(n_dw_installs),
  cumulative_capacity_installed = cumsum(capacity_installed),
  cumulative_net_tanks = cumsum(n_installs - n_closures),
  ever_had_sw_to_date = cummax(has_single_walled),
  ever_had_dw_to_date = cummax(has_double_walled),
  ever_mixed_to_date  = cummax(mixed_wall),
  max_active_tanks_to_date = cummax(n_tanks_active),
  max_capacity_eoy_to_date = cummax(capacity_eoy)
), by = panel_id]

pa_fac_stock[, `:=`(
  sw_tanks_ever_by_t  = n_sw_eoy  + cumulative_sw_closures,
  dw_tanks_ever_by_t  = n_dw_eoy  + cumulative_dw_closures,
  tanks_ever_by_t     = n_tanks_eoy + cumulative_closures
), by = panel_id]

log_step(sprintf("PA stock panel: %s rows | %s fac | %s exits | %s entries",
  fmt_n(nrow(pa_fac_stock)), fmt_n(uniqueN(pa_fac_stock$panel_id)),
  fmt_n(pa_fac_stock[facility_exit == 1L, .N]),
  fmt_n(pa_fac_stock[facility_entry == 1L, .N])))

#### T046.12 PA leak / leak-closure / leak-install pairing (MIRRORS S13.1) ####
# Real leak/hazard signal for PA -- researcher instruction (2026-07-01):
# 01n's never-leaked risk set needs it, so it is NOT left NA/0 like the fee.
# master_lust is already loaded in S13.1 above and still in scope here.
cat("T046.12: PA leak history...\n")

pa_leaks_fac_yr <- master_lust[
  !is.na(report_date) & state %in% PRICING_STATES,
  .(
    n_leak_reports   = .N,
    n_leak_incidents = uniqueN(report_date),
    leak_year        = 1L
  ),
  by = .(panel_id, panel_year = year(report_date))
]
pa_fac_stock <- merge(pa_fac_stock, pa_leaks_fac_yr,
                      by = c("panel_id", "panel_year"), all.x = TRUE)
pa_fac_stock[is.na(n_leak_reports), `:=`(
  n_leak_reports = 0L, n_leak_incidents = 0L, leak_year = 0L
)]
pa_fac_stock[, n_leaks := n_leak_incidents]

log_step(sprintf("PA LUST: %s fac-years with leaks | %s reports | %s incidents",
  fmt_n(pa_fac_stock[leak_year == 1L, .N]),
  fmt_n(pa_fac_stock[, sum(n_leak_reports)]),
  fmt_n(pa_fac_stock[, sum(n_leak_incidents)])))

pa_closures_for_lust <- pa_tanks[
  !is.na(tank_closed_date),
  .(panel_id, closure_date = tank_closed_date, tank_install_date = tank_installed_date)
]
pa_leaks_for_lust <- master_lust[
  !is.na(report_date) & state %in% PRICING_STATES,
  .(panel_id, report_date)
]
setkey(pa_closures_for_lust, panel_id)
setkey(pa_leaks_for_lust, panel_id)

pa_lust_pairs <- pa_leaks_for_lust[pa_closures_for_lust, on = .(panel_id), allow.cartesian = TRUE]
pa_lust_pairs <- pa_lust_pairs[is.na(tank_install_date) | report_date >= tank_install_date]

pa_lust_pairs[, diff := as.numeric(report_date - closure_date)]
pa_lust_pairs[, `:=`(
  is_rev_prim = diff >= 0    & diff <= 60,
  is_kno_prim = diff < -180,
  is_ind_prim = diff >= -180 & diff < 0,
  is_rev_nar  = diff >= 0    & diff <= 30,
  is_kno_nar  = diff < -365,
  is_rev_wid  = diff >= 0    & diff <= 90,
  is_kno_wid  = diff < -90,
  is_rev_reg  = diff >= 0    & diff <= 45,
  is_kno_reg  = diff < -180
)]

pa_closure_lust_flags <- pa_lust_pairs[, .(
  has_rev_prim = any(is_rev_prim), has_kno_prim = any(is_kno_prim),
  has_ind_prim = any(is_ind_prim),
  has_rev_nar  = any(is_rev_nar),  has_kno_nar  = any(is_kno_nar),
  has_rev_wid  = any(is_rev_wid),  has_kno_wid  = any(is_kno_wid),
  has_rev_reg  = any(is_rev_reg),  has_kno_reg  = any(is_kno_reg)
), by = .(panel_id, closure_date)]
pa_closure_lust_flags[, panel_year := year(closure_date)]

pa_annual_lust_flags <- pa_closure_lust_flags[, .(
  tank_closure_revealed          = as.integer(any(has_rev_prim)),
  tank_closure_known_leak        = as.integer(any(has_kno_prim)),
  tank_closure_indeterminate     = as.integer(any(has_ind_prim)),
  tank_closure_revealed_narrow   = as.integer(any(has_rev_nar)),
  tank_closure_known_leak_narrow = as.integer(any(has_kno_nar)),
  tank_closure_revealed_wide     = as.integer(any(has_rev_wid)),
  tank_closure_known_leak_wide   = as.integer(any(has_kno_wid)),
  tank_closure_revealed_reg      = as.integer(any(has_rev_reg)),
  tank_closure_known_leak_reg    = as.integer(any(has_kno_reg))
), by = .(panel_id, panel_year)]

pa_fac_stock <- merge(pa_fac_stock, pa_annual_lust_flags,
                      by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in grep("^tank_closure_", names(pa_fac_stock), value = TRUE))
  pa_fac_stock[is.na(get(v)), (v) := 0L]

pa_fac_stock[, tank_closure_clean := as.integer(
  any_closure == 1L & tank_closure_revealed == 0L & tank_closure_known_leak == 0L
)]
pa_fac_stock[, `:=`(
  lust_within_90d_of_closure = tank_closure_revealed_wide,
  leak_found_by_closure      = tank_closure_revealed
)]

pa_installs_for_lust <- pa_tanks[
  !is.na(tank_installed_date),
  .(panel_id, tank_install_date = tank_installed_date)
]
setkey(pa_installs_for_lust, panel_id)

pa_leaks_install_pairs <- pa_leaks_for_lust[pa_installs_for_lust,
                                             on = .(panel_id),
                                             allow.cartesian = TRUE,
                                             nomatch = NULL]
pa_leaks_install_pairs <- pa_leaks_install_pairs[!is.na(report_date) &
                                                    !is.na(tank_install_date)]
pa_leaks_install_pairs[, days_after_install :=
                          as.numeric(report_date - tank_install_date)]
pa_leaks_install_pairs[, `:=`(
  is_install_prim = days_after_install >= 0 & days_after_install <= 60,
  is_install_nar  = days_after_install >= 0 & days_after_install <= 30,
  is_install_wid  = days_after_install >= 0 & days_after_install <= 90,
  is_install_reg  = days_after_install >= 0 & days_after_install <= 45
)]

pa_leak_install_flags <- pa_leaks_install_pairs[, .(
  any_install_prim = as.integer(any(is_install_prim, na.rm = TRUE)),
  any_install_nar  = as.integer(any(is_install_nar,  na.rm = TRUE)),
  any_install_wid  = as.integer(any(is_install_wid,  na.rm = TRUE)),
  any_install_reg  = as.integer(any(is_install_reg,  na.rm = TRUE))
), by = .(panel_id, panel_year = year(report_date))]

pa_fac_stock <- merge(pa_fac_stock, pa_leak_install_flags,
                      by = c("panel_id", "panel_year"), all.x = TRUE)
for (v in c("any_install_prim", "any_install_nar",
            "any_install_wid", "any_install_reg")) {
  pa_fac_stock[is.na(get(v)), (v) := 0L]
}
pa_fac_stock[, leak_first_year_churn := any_install_prim]

rm(pa_lust_pairs, pa_closure_lust_flags, pa_annual_lust_flags,
   pa_closures_for_lust, pa_leaks_for_lust,
   pa_installs_for_lust, pa_leaks_install_pairs, pa_leak_install_flags)

setorder(pa_fac_stock, panel_id, panel_year)
pa_fac_stock[, `:=`(
  cumulative_leaks   = cumsum(n_leak_incidents),
  cumulative_reports = cumsum(n_leak_reports),
  ever_leaked        = as.integer(cumsum(n_leak_incidents) > 0),
  has_previous_leak  = as.integer(shift(cumsum(n_leak_incidents), 1L, fill = 0L) > 0)
), by = panel_id]
pa_fac_stock[, event_first_leak := as.integer(leak_year == 1L &
                                               has_previous_leak == 0L)]

log_step(sprintf("PA leak first-year-churn flags (0-60d): %s of %s leak-fac-years (%.1f%%)",
  fmt_n(pa_fac_stock[leak_year == 1L & leak_first_year_churn == 1L, .N]),
  fmt_n(pa_fac_stock[leak_year == 1L, .N]),
  100 * mean(pa_fac_stock[leak_year == 1L, leak_first_year_churn == 1L])))

# TX-FR and control-FR fee columns do not apply to PA. Match the existing
# non-TX default convention used elsewhere in this script for the TX-only
# FR columns (zeros/None), and leave the fee itself NA + flagged per ticket
# 046 decision #3 -- do not invent the PA USTIF fee.
pa_fac_stock[, `:=`(
  uses_private_dec = "None", uses_self_dec = "None",
  fr_covered_dec   = "None", DETAIL_TYPE_dec = "None", CATEGORY_dec = "None",
  months_covered    = 0L, months_with_data = 0L,
  share_private     = 0,  share_self       = 0,
  n_transitions     = 0L, any_transition   = 0L, n_gap_months = 0L,
  any_gap           = 0L, fr_coverage_share = 0, fr_stable = 0L,
  had_zurich_2012   = 0L, dropped_by_zurich = 0L,
  fr_mechanism           = NA_character_,
  operator_fr_cost_type  = NA_character_,
  fr_premium_per_tank_yr = NA_real_,
  deductible_usd          = NA_real_,
  coverage_per_occ_usd    = NA_real_,
  fund_epa_approved_fr    = NA_real_,
  fund_suspended          = NA_real_,
  fr_fee_unresolved       = 1L
)]

#### T046.13 PA final facility panel assembly (MIRRORS S14) ####
cat("T046.13: PA final facility panel assembly...\n")

pa_facility_panel <- merge(
  pa_fac_stock,
  pa_fac_bio[, .(panel_id, first_install_yr, last_install_yr,
              first_install_date, last_install_date,
              total_tanks_ever, total_tanks_closed_ever, total_capacity_ever,
              max_capacity_ever, mean_capacity_ever,
              total_sw_ever, total_dw_ever,
              total_gasoline_ever, total_diesel_ever,
              prop_sw_ever, prop_dw_ever, prop_gasoline_ever, prop_diesel_ever,
              had_pre1965_tanks, had_pre1975_tanks, had_pre1989_tanks,
              all_post1988, all_post1989, ever_had_sw, ever_had_dw,
              total_tanks_bin, sw_share_ever_bin,
              fac_is_incumbent, fac_vintage, birth_cohort_bin)],
  by = "panel_id", all.x = TRUE
)

pa_facility_panel <- merge(
  pa_facility_panel,
  pa_fac_mm_reform[, .(panel_id, make_model_fac,
    fac_wall_reform, fac_fuel_reform, oldest_age_bin,
    n_tanks_at_reform, n_sw_at_reform, n_dw_at_reform,
    total_capacity_reform, oldest_age_reform,
    any_sw_at_reform, any_dw_at_reform,
    n_tanks_bin, n_sw_bin, capacity_bin, sw_share_bin,
    prop_sw, prop_dw, prop_gasoline, prop_diesel,
    mean_capacity, sd_capacity,
    oldest_install_yr_at_reform, newest_install_yr_at_reform)],
  by = "panel_id", all.x = TRUE
)

pa_facility_panel[, `:=`(
  install_year = first_install_yr,
  cohort       = fifelse(fac_is_incumbent == 1L, "Incumbent", "Entrant"),
  is_incumbent = fac_is_incumbent
)]
pa_facility_panel[, yrs_since_first_install := as.integer(panel_year) - first_install_yr]
pa_facility_panel[, wall_type := fcase(
  has_double_walled_dec == 1L & has_single_walled_dec == 0L, "Double-Walled",
  has_single_walled_dec == 1L & has_double_walled_dec == 0L, "Single-Walled",
  has_double_walled_dec == 1L & has_single_walled_dec == 1L, "Mixed",
  default = "Unknown"
)]
pa_facility_panel[, wall_pure := fcase(
  wall_type == "Single-Walled", "Single-Walled",
  wall_type == "Double-Walled", "Double-Walled",
  default = NA_character_
)]
pa_facility_panel[, is_pure_wall := as.integer(!is.na(wall_pure))]
pa_facility_panel[, age_bins := cut(avg_tank_age_dec,
  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf),
  include.lowest = TRUE,
  labels = c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24+")
)]
pa_facility_panel[, `:=`(
  exit_no_leak           = as.integer(facility_exit == 1L & leak_year == 0L),
  exit_with_leak         = as.integer(facility_exit == 1L & leak_year == 1L),
  exit_no_retrofit       = as.integer(facility_exit == 1L & replacement_closure_year == 0L),
  exit_with_retrofit     = as.integer(facility_exit == 1L & replacement_closure_year == 1L),
  leak_not_found_by_exit = as.integer(facility_exit == 1L & leak_year == 0L)
)]

log_step(sprintf("PA facility panel: %s rows | %s fac | exits=%s | complete_closures=%s | entries=%s",
  fmt_n(nrow(pa_facility_panel)),
  fmt_n(uniqueN(pa_facility_panel$panel_id)),
  fmt_n(pa_facility_panel[facility_exit == 1L, .N]),
  fmt_n(pa_facility_panel[facility_complete_closure == 1L, .N]),
  fmt_n(pa_facility_panel[facility_entry == 1L, .N])))
cat("\n")

#### T046.14 Eligibility flags + PA union + D-ACCEPT diff report ####
cat("========================================\n")
cat("T046.14: PA UNION + ELIGIBILITY FLAGS + D-ACCEPT DIFF\n")
cat("========================================\n\n")

# Snapshot pre-PA panel_dt.csv / facility_panel.csv (if present on disk) as
# the D-ACCEPT baseline, before S18 overwrites them below.
pre_pa_dir <- file.path(ANALYSIS_DIR, "_pre_046_PA")
dir.create(pre_pa_dir, recursive = TRUE, showWarnings = FALSE)
panel_dt_path             <- file.path(ANALYSIS_DIR, "panel_dt.csv")
facility_panel_path       <- file.path(ANALYSIS_DIR, "facility_panel.csv")
panel_dt_prepa_path       <- file.path(pre_pa_dir, "panel_dt_prePA.csv")
facility_panel_prepa_path <- file.path(pre_pa_dir, "facility_panel_prePA.csv")

if (file.exists(panel_dt_path) && !file.exists(panel_dt_prepa_path)) {
  file.copy(panel_dt_path, panel_dt_prepa_path, overwrite = FALSE)
  log_step(sprintf("Backed up pre-PA baseline: %s", panel_dt_prepa_path))
}
if (file.exists(facility_panel_path) && !file.exists(facility_panel_prepa_path)) {
  file.copy(facility_panel_path, facility_panel_prepa_path, overwrite = FALSE)
  log_step(sprintf("Backed up pre-PA baseline: %s", facility_panel_prepa_path))
}

# Add eligibility flags to COPIES of the existing STUDY_STATES objects.
# panel_dt / facility_panel themselves are never mutated -- every consumer
# that runs after this point (figures F1-F13 below) still sees the exact
# original STUDY_STATES-only objects.
panel_dt_export <- copy(panel_dt)
panel_dt_export[, `:=`(
  panel_state_role     = "study",
  closure_did_eligible = 1L,
  hazard_eligible       = 1L
)]
pa_panel_dt[, `:=`(
  panel_state_role     = "pricing_only",
  closure_did_eligible = 0L,
  hazard_eligible       = 1L
)]
assert_cols_match(panel_dt_export, pa_panel_dt, "panel_dt", "pa_panel_dt")
panel_dt_final <- rbindlist(list(panel_dt_export, pa_panel_dt), use.names = TRUE)

facility_panel_export <- copy(facility_panel)
facility_panel_export[, `:=`(
  panel_state_role     = "study",
  closure_did_eligible = 1L,
  hazard_eligible       = 1L,
  fr_fee_unresolved     = 0L
)]
pa_facility_panel[, `:=`(
  panel_state_role     = "pricing_only",
  closure_did_eligible = 0L,
  hazard_eligible       = 1L
)]
assert_cols_match(facility_panel_export, pa_facility_panel, "facility_panel", "pa_facility_panel")
facility_panel_final <- rbindlist(list(facility_panel_export, pa_facility_panel), use.names = TRUE)

log_step(sprintf("panel_dt: %s study rows + %s PA rows = %s total",
  fmt_n(nrow(panel_dt_export)), fmt_n(nrow(pa_panel_dt)), fmt_n(nrow(panel_dt_final))))
log_step(sprintf("facility_panel: %s study rows + %s PA rows = %s total",
  fmt_n(nrow(facility_panel_export)), fmt_n(nrow(pa_facility_panel)), fmt_n(nrow(facility_panel_final))))

# ---- D-ACCEPT: non-PA rows byte-identical to the pre-PA baseline ----
build_diff_rows <- function(panel_name, before_path, after_dt, key_cols, exclude_cols) {
  if (!file.exists(before_path)) {
    log_step(sprintf("No pre-PA baseline for %s (first run) -- skipping identical_nonpa check.", panel_name))
    return(data.table(
      panel = panel_name, scope = "nonpa_identical_check", state = "ALL",
      n_rows_before = NA_integer_, n_rows_after = NA_integer_, identical_nonpa = NA
    ))
  }
  before_dt   <- fread(before_path, na.strings = c("", "NA", "N/A"))
  after_nonpa <- after_dt[!(state %in% PRICING_STATES)]

  row_counts <- rbindlist(list(
    before_dt[, .(n_rows_before = .N), by = state],
    after_dt[,  .(n_rows_after  = .N), by = state]
  ), fill = TRUE)
  row_counts <- row_counts[, .(
    n_rows_before = sum(n_rows_before, na.rm = TRUE),
    n_rows_after  = sum(n_rows_after,  na.rm = TRUE)
  ), by = state]
  row_counts[, `:=`(panel = panel_name, scope = "row_counts_by_state", identical_nonpa = NA)]

  common_cols <- setdiff(intersect(names(before_dt), names(after_nonpa)), exclude_cols)
  before_key  <- before_dt[, .SD, .SDcols = common_cols]
  after_key   <- after_nonpa[, .SD, .SDcols = common_cols]

  nonpa_states <- sort(unique(before_dt$state))
  identical_rows <- rbindlist(lapply(nonpa_states, function(st) {
    b <- before_key[state == st]
    a <- after_key[state == st]
    setorderv(b, key_cols); setorderv(a, key_cols)
    is_identical <- isTRUE(all.equal(b, a, check.attributes = FALSE))
    data.table(
      panel = panel_name, scope = "nonpa_identical_check", state = st,
      n_rows_before = nrow(b), n_rows_after = nrow(a),
      identical_nonpa = is_identical
    )
  }))

  rbindlist(list(row_counts, identical_rows), fill = TRUE)
}

diff_panel_dt <- build_diff_rows(
  "panel_dt", panel_dt_prepa_path, panel_dt_final,
  key_cols = c("tank_panel_id", "panel_year"),
  exclude_cols = c("panel_state_role", "closure_did_eligible", "hazard_eligible")
)
diff_facility_panel <- build_diff_rows(
  "facility_panel", facility_panel_prepa_path, facility_panel_final,
  key_cols = c("panel_id", "panel_year"),
  exclude_cols = c("panel_state_role", "closure_did_eligible", "hazard_eligible", "fr_fee_unresolved")
)

t046_diff <- rbindlist(list(diff_panel_dt, diff_facility_panel), fill = TRUE)
t046_diff <- t046_diff[, .(panel, scope, state, n_rows_before, n_rows_after, identical_nonpa)]
fwrite(t046_diff, file.path(OUTPUT_TABLES, "T_046_PA_Panel_Diff.csv"))

n_fail_diff <- t046_diff[scope == "nonpa_identical_check" & !is.na(identical_nonpa) & identical_nonpa == FALSE, .N]
if (n_fail_diff > 0L) {
  print(t046_diff[scope == "nonpa_identical_check" & identical_nonpa == FALSE])
  stop(sprintf("D-ACCEPT FAILED: %d non-PA state(s) not byte-identical to pre-PA baseline.", n_fail_diff))
}
log_step("D-ACCEPT PASSED: all non-PA states byte-identical to pre-PA baseline (or first run, no baseline).")
cat("\n")


#### S17b T007 Phase 1 Backup ####
# Preserve pre-T007 facility_panel.csv before overwriting.
cat("T007 Phase1: Backing up facility_panel.csv to _pre_T007/...\n")
pre_t007_dir <- file.path(ANALYSIS_DIR, "_pre_T007")
dir.create(pre_t007_dir, recursive = TRUE, showWarnings = FALSE)
fp_backup <- file.path(pre_t007_dir, "facility_panel.csv")
fp_current <- file.path(ANALYSIS_DIR, "facility_panel.csv")
if (file.exists(fp_current)) {
  file.copy(fp_current, fp_backup, overwrite = TRUE)
  log_step(sprintf("  Backed up: %s", fp_backup))
} else {
  log_step("  No existing facility_panel.csv to back up (first run).")
}


#### S18 Save Outputs ####

cat("========================================\n")
cat("S18: SAVE OUTPUTS\n")
cat("========================================\n\n")

# Panels
# panel_dt_final / facility_panel_final are the STUDY_STATES objects above
# unioned with the ticket-046 PA pricing/hazard rows (see T046 block) -- the
# in-memory panel_dt / facility_panel symbols themselves are untouched.
fwrite(panel_dt_final,                    file.path(ANALYSIS_DIR, "panel_dt.csv"))
fwrite(facility_panel_final,              file.path(ANALYSIS_DIR, "facility_panel.csv"))
fwrite(fac_bio,                           file.path(ANALYSIS_DIR, "facility_biography.csv"))
fwrite(fac_mm_reform,                     file.path(ANALYSIS_DIR, "facility_make_model_reform.csv"))
fwrite(exact_base,                        file.path(ANALYSIS_DIR, "exact_base.csv"))

# Tank-level matched samples
fwrite(matched_tanks_reform_cem,          file.path(ANALYSIS_DIR, "matched_tanks_reform_cem.csv"))
fwrite(matched_tanks_reform_psm,          file.path(ANALYSIS_DIR, "matched_tanks_reform_psm.csv"))
fwrite(matched_tanks_birth_cem,           file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv"))
fwrite(matched_tanks_birth_psm,           file.path(ANALYSIS_DIR, "matched_tanks_birth_psm.csv"))

# Facility-level matched samples
fwrite(matched_facs_reform_cem,           file.path(ANALYSIS_DIR, "matched_facs_reform_cem.csv"))
fwrite(matched_facs_reform_psm,           file.path(ANALYSIS_DIR, "matched_facs_reform_psm.csv"))
fwrite(matched_facs_birth_cem,            file.path(ANALYSIS_DIR, "matched_facs_birth_cem.csv"))
fwrite(matched_facs_birth_psm,            file.path(ANALYSIS_DIR, "matched_facs_birth_psm.csv"))

# ID lookups (8 tank+facility, plus the 5-way facility CEM)
fwrite(data.table(tank_panel_id = ids_tank_reform_cem),
       file.path(ANALYSIS_DIR, "ids_tank_reform_cem.csv"))
fwrite(data.table(tank_panel_id = ids_tank_reform_psm),
       file.path(ANALYSIS_DIR, "ids_tank_reform_psm.csv"))
fwrite(data.table(tank_panel_id = ids_tank_birth_cem),
       file.path(ANALYSIS_DIR, "ids_tank_birth_cem.csv"))
fwrite(data.table(tank_panel_id = ids_tank_birth_psm),
       file.path(ANALYSIS_DIR, "ids_tank_birth_psm.csv"))
fwrite(data.table(panel_id = ids_fac_reform_cem),
       file.path(ANALYSIS_DIR, "ids_fac_reform_cem.csv"))
fwrite(data.table(panel_id = ids_fac_reform_cem5),
       file.path(ANALYSIS_DIR, "ids_fac_reform_cem5.csv"))
fwrite(data.table(panel_id = ids_fac_reform_psm),
       file.path(ANALYSIS_DIR, "ids_fac_reform_psm.csv"))
fwrite(data.table(panel_id = ids_fac_birth_cem),
       file.path(ANALYSIS_DIR, "ids_fac_birth_cem.csv"))
fwrite(data.table(panel_id = ids_fac_birth_psm),
       file.path(ANALYSIS_DIR, "ids_fac_birth_psm.csv"))

# Scalar metadata
fwrite(
  data.table(
    scalar = c("med_age", "ctrl_mean_post", "n_ty", "n_tanks",
               "n_tx_tanks", "n_ct_tanks", "n_cells",
               "reform_date", "n_alive_at_reform",
               "n_tank_reform_cem", "n_tank_reform_psm",
               "n_tank_birth_cem",  "n_tank_birth_psm",
               "n_fac_reform_cem",  "n_fac_reform_cem5", "n_fac_reform_psm",
               "n_fac_birth_cem",   "n_fac_birth_psm"),
    value = c(med_age, ctrl_mean_post, n_ty, n_tanks,
              n_tx_tanks, n_ct_tanks, n_cells,
              as.numeric(REFORM_DATE),
              nrow(alive_at_reform),
              length(ids_tank_reform_cem), length(ids_tank_reform_psm),
              length(ids_tank_birth_cem),  length(ids_tank_birth_psm),
              length(ids_fac_reform_cem),  length(ids_fac_reform_cem5),
              length(ids_fac_reform_psm),
              length(ids_fac_birth_cem),   length(ids_fac_birth_psm))
  ),
  file.path(ANALYSIS_DIR, "panel_meta.csv")
)

log_step(sprintf("Saved to %s:", ANALYSIS_DIR))
log_step(sprintf("  panel_dt.csv                   - %s rows", fmt_n(nrow(panel_dt))), 1)
log_step(sprintf("  facility_panel.csv             - %s rows", fmt_n(nrow(facility_panel))), 1)
log_step(sprintf("  exact_base.csv                 - %s rows", fmt_n(nrow(exact_base))), 1)
log_step(sprintf("  matched_tanks_reform_cem.csv   - %s rows", fmt_n(nrow(matched_tanks_reform_cem))), 1)
log_step(sprintf("  matched_tanks_reform_psm.csv   - %s rows", fmt_n(nrow(matched_tanks_reform_psm))), 1)
log_step(sprintf("  matched_tanks_birth_cem.csv    - %s rows", fmt_n(nrow(matched_tanks_birth_cem))), 1)
log_step(sprintf("  matched_tanks_birth_psm.csv    - %s rows", fmt_n(nrow(matched_tanks_birth_psm))), 1)
log_step(sprintf("  matched_facs_reform_cem.csv    - %s rows", fmt_n(nrow(matched_facs_reform_cem))), 1)
log_step(sprintf("  matched_facs_reform_psm.csv    - %s rows", fmt_n(nrow(matched_facs_reform_psm))), 1)
log_step(sprintf("  matched_facs_birth_cem.csv     - %s rows", fmt_n(nrow(matched_facs_birth_cem))), 1)
log_step(sprintf("  matched_facs_birth_psm.csv     - %s rows", fmt_n(nrow(matched_facs_birth_psm))), 1)
log_step("  9 ID lookup files + panel_meta.csv", 1)

cat("\n========================================\n")
cat("DATA CONSTRUCTION COMPLETE - PROCEEDING TO FIGURES\n")
cat("========================================\n\n")


################################################################################
################################################################################
##                                                                            ##
##                              FIGURES                                       ##
##                                                                            ##
################################################################################
################################################################################


#### F1 Mandate Coverage by Vintage and State ####

cat("--- F1: Mandate coverage by vintage and state ---\n")

mand_summ_by_state <- rbind(
  tank_year_panel[mandate_release_det == 1L,
    .(mandate_type = "Release Detection", n_tanks = uniqueN(tank_panel_id)),
    by = .(install_yr_int, state)],
  tank_year_panel[mandate_spill_overfill == 1L,
    .(mandate_type = "Spill/Overfill", n_tanks = uniqueN(tank_panel_id)),
    by = .(install_yr_int, state)],
  tank_year_panel[mandate_integrity == 1L,
    .(mandate_type = "Tank Integrity", n_tanks = uniqueN(tank_panel_id)),
    by = .(install_yr_int, state)]
)
mand_long <- mand_summ_by_state[,
  .(n_tanks = sum(n_tanks)),
  by = .(install_yr_int, mandate_type,
         state_group = fifelse(state == "TX", "Texas", "Control"))]

mand_long[, x_label := fcase(
  install_yr_int <= 1960L, "1940-1960 (pooled)",
  default = as.character(install_yr_int)
)]
mand_long <- mand_long[, .(n_tanks = sum(n_tanks)),
                       by = .(x_label, mandate_type, state_group)]
all_labels <- c("1940-1960 (pooled)",
                as.character(sort(setdiff(as.integer(mand_long[x_label != "1940-1960 (pooled)", x_label]), NA))))
mand_long[, x_label := factor(x_label, levels = all_labels)]
mand_long[, mandate_label := fcase(
  mandate_type == "Release Detection", "Release Detection\n(Deadline: 1989-1993)",
  mandate_type == "Spill/Overfill",    "Spill/Overfill\n(Deadline: Dec 22, 1994)",
  mandate_type == "Tank Integrity",    "Tank Integrity\n(Deadline: Dec 22, 1998)"
)]

mand_plot <- ggplot(mand_long, aes(x = x_label, y = n_tanks, fill = state_group)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, position = "dodge") +
  facet_wrap(~ mandate_label, ncol = 1, scales = "fixed") +
  labs(x = "Install Year Cohort", y = "Number of Tanks Affected", fill = "State Group") +
  scale_fill_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  theme_ust() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        strip.text  = element_text(size = 10, face = "bold", hjust = 0),
        panel.spacing.y = unit(10, "pt"))

ggsave(file.path(OUTPUT_FIGURES, "Mandate_Coverage_by_Vintage.png"),
       mand_plot, width = 8, height = 9, dpi = 300)
log_step("Saved: Mandate_Coverage_by_Vintage.png")
cat("\n")


#### F2 Mandate Response (Closure Rate in Deadline Year) ####

cat("--- F2: Mandate response by vintage and state ---\n")

rd_response <- tank_year_panel[!is.na(release_det_deadline_yr), .(
  n_exposed         = uniqueN(tank_panel_id),
  n_closed_deadline = sum((closure_event == 1L) & (panel_year == release_det_deadline_yr))
), by = .(install_yr_int, release_det_deadline_yr, state)]
rd_response[, closure_rate := 100 * n_closed_deadline / n_exposed]
rd_response[, mandate_type := "Release Detection"]

so_response <- tank_year_panel[!is.na(release_det_deadline_yr) & panel_year == 1994L, .(
  n_exposed         = uniqueN(tank_panel_id),
  n_closed_deadline = sum(closure_event == 1L)
), by = .(install_yr_int, state)]
so_response[, closure_rate := 100 * n_closed_deadline / n_exposed]
so_response[, mandate_type := "Spill/Overfill"]

ti_response <- tank_year_panel[!is.na(release_det_deadline_yr) & panel_year == 1998L, .(
  n_exposed         = uniqueN(tank_panel_id),
  n_closed_deadline = sum(closure_event == 1L)
), by = .(install_yr_int, state)]
ti_response[, closure_rate := 100 * n_closed_deadline / n_exposed]
ti_response[, mandate_type := "Tank Integrity"]

response_long <- rbind(
  rd_response[, .(install_yr_int, state, mandate_type, n_exposed, closure_rate)],
  so_response[, .(install_yr_int, state, mandate_type, n_exposed, closure_rate)],
  ti_response[, .(install_yr_int, state, mandate_type, n_exposed, closure_rate)]
)
response_long[, state_group := fifelse(state == "TX", "Texas", "Control")]
response_long[, mandate_label := fcase(
  mandate_type == "Release Detection", "Release Detection\n(Deadline: 1989-1993)",
  mandate_type == "Spill/Overfill",    "Spill/Overfill\n(Deadline: Dec 22, 1994)",
  mandate_type == "Tank Integrity",    "Tank Integrity\n(Deadline: Dec 22, 1998)"
)]

response_plot <- ggplot(
  response_long[install_yr_int >= 1940L],
  aes(x = n_exposed, y = closure_rate, color = state_group, size = n_exposed)
) +
  geom_point(alpha = 0.6, stroke = 0.8) +
  facet_wrap(~ mandate_label, ncol = 1) +
  labs(x = "Number of Tanks Exposed (log scale)",
       y = "Closure Rate in Deadline Year (%)",
       color = "State Group", size = "Tank Count") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  theme_ust() +
  theme(strip.text = element_text(size = 10, face = "bold", hjust = 0),
        panel.spacing.y = unit(10, "pt"))

ggsave(file.path(OUTPUT_FIGURES, "Mandate_Response_by_Vintage.png"),
       response_plot, width = 8, height = 9, dpi = 300)
log_step("Saved: Mandate_Response_by_Vintage.png")
cat("\n")


#### F3 Mandate Static Share vs Response ####

cat("--- F3: Mandate static share vs response ---\n")

response_agg <- response_long[, .(
  n_exposed = sum(n_exposed, na.rm = TRUE),
  n_closed  = sum(n_exposed * closure_rate / 100, na.rm = TRUE)
), by = .(vintage_label = fcase(install_yr_int <= 1960L, "1940-1960",
                                default = as.character(install_yr_int)),
         state_group, mandate_type)]
response_agg[, closure_rate := 100 * n_closed / n_exposed]
response_agg[n_exposed == 0, closure_rate := 0]
response_agg[, mandate_label := fcase(
  mandate_type == "Release Detection", "Release Detection\n(Deadline: 1989-1993)",
  mandate_type == "Spill/Overfill",    "Spill/Overfill\n(Deadline: Dec 22, 1994)",
  mandate_type == "Tank Integrity",    "Tank Integrity\n(Deadline: Dec 22, 1998)"
)]

static_inventory <- response_agg[, .(baseline_tanks = max(n_exposed)),
                                 by = .(vintage_label, state_group)]
state_totals <- static_inventory[, .(total_state_tanks = sum(baseline_tanks)),
                                 by = state_group]
static_shares <- merge(static_inventory, state_totals, by = "state_group")
static_shares[, static_cohort_share := (baseline_tanks / total_state_tanks) * 100]

plot_data_static <- merge(response_agg,
  static_shares[, .(vintage_label, state_group, static_cohort_share)],
  by = c("vintage_label", "state_group"))
plot_data_static[, point_label := fcase(
  vintage_label == "1940-1960", "40-60",
  default = substr(as.character(vintage_label), 3, 4)
)]

scatter_static <- ggplot(plot_data_static,
    aes(x = static_cohort_share, y = closure_rate, color = state_group)) +
  geom_point(shape = 21, fill = "white", size = 11, stroke = 1.2, alpha = 0.8) +
  geom_text(aes(label = point_label), size = 3, color = "black", fontface = "bold") +
  facet_wrap(~ mandate_label, ncol = 1) +
  labs(x = "Fixed Cohort Share of Pre-1989 Inventory (%)",
       y = "Closure Rate in Deadline Year (%)",
       color = "State Group") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  scale_color_manual(values = c("Texas" = COL_TX, "Control" = COL_CTRL)) +
  theme_ust() +
  theme(strip.text = element_text(size = 10, face = "bold", hjust = 0),
        panel.spacing.y = unit(12, "pt"))

ggsave(file.path(OUTPUT_FIGURES, "Mandate_Static_Share_vs_Response.png"),
       scatter_static, width = 9, height = 10, dpi = 300)
log_step("Saved: Mandate_Static_Share_vs_Response.png")
cat("\n")


#### F4 Tank Population Composition ####

cat("--- F4: Tank population composition ---\n")

tank_year_panel[, close_yr_int := as.integer(expand_end)]
tank_chars <- unique(tank_year_panel[, .(tank_panel_id, state, install_yr_int, close_yr_int)])
composition_data <- tank_chars[, .(
  panel_year = seq(max(1990L, install_yr_int), min(2018L, close_yr_int))
), by = .(tank_panel_id, state, install_yr_int)]

composition_data[, install_decade := fcase(
  install_yr_int <= 1979L,         "1940-1979",
  install_yr_int %in% 1980L:1989L, "1980-1989",
  install_yr_int %in% 1990L:1999L, "1990-1999",
  install_yr_int %in% 2000L:2009L, "2000-2009",
  default = "2010+"
)]
composition_data[, state_group := fifelse(state == "TX", "Texas", "Control")]

decade_order <- c("1940-1979", "1980-1989", "1990-1999", "2000-2009", "2010+")
bw_palette <- c("1940-1979" = "#000000", "1980-1989" = "#404040",
                "1990-1999" = "#808080", "2000-2009" = "#c0c0c0",
                "2010+"     = "#f0f0f0")

# By state
comp_by_state <- composition_data[, .(n_tanks = uniqueN(tank_panel_id)),
                                  by = .(panel_year, state_group, install_decade)]
totals_by_state_year <- comp_by_state[, .(total = sum(n_tanks)),
                                      by = .(panel_year, state_group)]
comp_by_state <- merge(comp_by_state, totals_by_state_year,
                       by = c("panel_year", "state_group"))
comp_by_state[, share := 100 * n_tanks / total]
comp_by_state[, install_decade := factor(install_decade, levels = decade_order)]

comp_plot_state <- ggplot(comp_by_state,
    aes(x = panel_year, y = share, fill = install_decade, group = install_decade)) +
  geom_area(alpha = 0.85, color = "black", linewidth = 0.15, position = "stack") +
  facet_wrap(~ state_group, ncol = 1) +
  labs(x = "Year", y = "Share of Active Tank Inventory (%)", fill = "Install Period") +
  scale_x_continuous(breaks = seq(1990, 2018, 5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = bw_palette) +
  theme_ust() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text  = element_text(face = "bold", size = 11),
        panel.spacing.y = unit(12, "pt"))

ggsave(file.path(OUTPUT_FIGURES, "Tank_Population_Composition_by_State.png"),
       comp_plot_state, width = 10, height = 8, dpi = 300)
log_step("Saved: Tank_Population_Composition_by_State.png")
cat("\n")


#### F5 Cohort Exit-Rate Difference Bars ####

cat("--- F5: Cohort exit-rate difference bars ---\n")

tank_year_panel[, cohort_group := fcase(
  install_yr_int < 1980L,                   "Pre-1980",
  install_yr_int %between% c(1980L, 1988L), "1980-1988",
  install_yr_int %between% c(1989L, 1998L), "1989-1998",
  default = NA_character_
)]
cohort_exits <- tank_year_panel[
  !is.na(cohort_group) & panel_year %between% c(1980L, 2018L),
  .(n_active = .N, n_closed = sum(closure_event)),
  by = .(panel_year, state, cohort_group)
]
cohort_exits[, exit_rate := 100 * n_closed / n_active]
cohort_exits[, state_group := fifelse(state == "TX", "Texas", "Control")]
cohort_agg <- cohort_exits[, .(exit_rate = mean(exit_rate)),
                           by = .(panel_year, state_group, cohort_group)]
cohort_wide <- dcast(cohort_agg, panel_year + cohort_group ~ state_group, value.var = "exit_rate")
cohort_wide[, diff := Texas - Control]

cohort_palette <- c("Pre-1980" = "#000000", "1980-1988" = "#808080", "1989-1998" = "#c0c0c0")

cohort_wide[, period := fcase(
  panel_year %between% c(1980L, 1988L), "Pre-Mandate\n(1980-1988)",
  panel_year %between% c(1989L, 1998L), "Federal Mandates\n(1989-1998)",
  panel_year %between% c(1999L, 2008L), "Early Reform\n(1999-2008)",
  panel_year %between% c(2009L, 2018L), "Late Reform\n(2009-2018)"
)]
bar_data <- cohort_wide[!is.na(period), .(mean_diff = mean(diff, na.rm = TRUE)),
                        by = .(period, cohort_group)]
bar_data[, period := factor(period, levels = c(
  "Pre-Mandate\n(1980-1988)", "Federal Mandates\n(1989-1998)",
  "Early Reform\n(1999-2008)", "Late Reform\n(2009-2018)"
))]
bar_data[, cohort_group := factor(cohort_group, levels = c("Pre-1980", "1980-1988", "1989-1998"))]

fig_bar_periods <- ggplot(bar_data, aes(x = period, y = mean_diff, fill = cohort_group)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.65,
           color = "white", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  scale_fill_manual(values = cohort_palette,
    labels = c("Pre-1980" = "Pre-1980 (highest risk)",
               "1980-1988" = "1980-1988",
               "1989-1998" = "1989-1998 (placebo)")) +
  scale_y_continuous(labels = function(x) ifelse(x == 0, "0", paste0(x, " pp"))) +
  labs(x = NULL, y = "Texas minus Control States (percentage points)", fill = "Install Cohort") +
  theme_ust() +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(OUTPUT_FIGURES, "Fleet_ExitRate_Diff_Periods_Bar.png"),
       fig_bar_periods, width = 9, height = 5, dpi = 300)
log_step("Saved: Fleet_ExitRate_Diff_Periods_Bar.png")
cat("\n")


#### F6 Cell Coverage Heatmap ####

cat("--- F6: Cell coverage heatmap ---\n")

cell_year <- panel_dt[, .(
  n_tx    = sum(texas_treated == 1L),
  n_ctl   = sum(texas_treated == 0L),
  n_total = .N
), by = .(make_model_tank, panel_year, mm_install_cohort)]
cell_year[, has_both   := as.integer(n_tx > 0L & n_ctl > 0L)]
cell_year[, install_yr := as.integer(mm_install_cohort)]
cell_year_clean <- cell_year[
  !is.na(install_yr) & install_yr >= 1940L & install_yr <= 2019L
]
cell_year_clean[, cohort_bin := (install_yr %/% 5L) * 5L]

heat_dt <- cell_year_clean[, .(
  share_id = sum(n_total * has_both) / sum(n_total)
), by = .(cohort_bin, panel_year)]
heat_dt <- heat_dt[panel_year %between% c(PANEL_START, PANEL_END)]

heat_grid <- CJ(
  cohort_bin = sort(unique(heat_dt$cohort_bin)),
  panel_year = PANEL_START:PANEL_END
)
heat_dt <- merge(heat_grid, heat_dt, by = c("cohort_bin", "panel_year"), all.x = TRUE)

cohort_breaks <- seq(1940L, 2015L, 10L)

fig_heatmap <- ggplot(heat_dt, aes(x = factor(cohort_bin), y = panel_year, fill = share_id)) +
  geom_tile(color = "white", linewidth = 0.1) +
  geom_hline(yintercept = 1998.5, color = "#222222", linewidth = 0.9, linetype = "dashed") +
  scale_fill_gradientn(
    colours  = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#084594"),
    limits   = c(0, 1),
    labels   = label_percent(accuracy = 1),
    name     = "Share of Tank-Years\nIdentified",
    na.value = "gray92"
  ) +
  scale_x_discrete(breaks = as.character(cohort_breaks)) +
  scale_y_continuous(breaks = seq(PANEL_START, PANEL_END, 5L)) +
  labs(x = "Installation Year Cohort (5-Year Bins)", y = "Panel Year") +
  theme_ust() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5),
        panel.grid.major = element_blank(),
        legend.key.width = unit(1.6, "cm"),
        legend.key.height = unit(0.4, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "FigureApp_Cell_Heatmap.png"),
       fig_heatmap, width = 11, height = 6.5, dpi = 300)
log_step("Saved: FigureApp_Cell_Heatmap.png")
cat("\n")


#### F7 Cell Size ECDF ####

cat("--- F7: Cell size ECDF ---\n")

cell_level <- panel_dt[!is.na(make_model_tank), .(
  n_tx  = sum(texas_treated == 1L),
  n_ctl = sum(texas_treated == 0L)
), by = make_model_tank]
ecdf_long <- rbind(
  cell_level[n_tx  > 0L, .(tank_years = n_tx,  group = "Texas")],
  cell_level[n_ctl > 0L, .(tank_years = n_ctl, group = "Control States")]
)
ecdf_long[, group := factor(group, levels = c("Texas", "Control States"))]

med_tx  <- median(ecdf_long[group == "Texas",          tank_years])
med_ctl <- median(ecdf_long[group == "Control States", tank_years])

fig_ecdf <- ggplot(ecdf_long, aes(x = tank_years, color = group, linetype = group)) +
  stat_ecdf(linewidth = 1.0, pad = FALSE) +
  geom_vline(xintercept = med_tx,  color = COL_TX,   linewidth = 0.55, linetype = "dotted", alpha = 0.8) +
  geom_vline(xintercept = med_ctl, color = COL_CTRL, linewidth = 0.55, linetype = "dotted", alpha = 0.8) +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500, 1000, 5000, 50000),
                labels = label_comma(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1), breaks = seq(0, 1, 0.1)) +
  scale_color_manual(values = c("Texas" = COL_TX, "Control States" = COL_CTRL), name = NULL) +
  scale_linetype_manual(values = c("Texas" = "solid", "Control States" = "dashed"), name = NULL) +
  labs(x = "Tank-Years per Cell (log scale)", y = "Cumulative Share of Cells") +
  theme_ust() +
  theme(panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3))

ggsave(file.path(OUTPUT_FIGURES, "FigureApp_Cell_ECDF.png"),
       fig_ecdf, width = 7.5, height = 5, dpi = 300)
log_step("Saved: FigureApp_Cell_ECDF.png")
cat("\n")


#### F8 Cell Balance Scatter ####

cat("--- F8: Cell balance scatter ---\n")

cell_bal <- panel_dt[!is.na(make_model_tank), .(
  n_tx    = sum(texas_treated == 1L),
  n_ctl   = sum(texas_treated == 0L),
  mm_wall = mm_wall[1L]
), by = make_model_tank]
cell_bal_id <- cell_bal[n_tx > 0L & n_ctl > 0L]

ax_max <- max(cell_bal_id[, c(n_tx, n_ctl)]) * 1.5
ax_min <- 0.8

fig_balance <- ggplot(cell_bal_id, aes(x = n_ctl, y = n_tx, color = mm_wall)) +
  geom_abline(slope = 1,   intercept = 0, color = "#444444", linewidth = 0.75, linetype = "dashed") +
  geom_abline(slope = 2,   intercept = 0, color = "#bbbbbb", linewidth = 0.4,  linetype = "dotted") +
  geom_abline(slope = 0.5, intercept = 0, color = "#bbbbbb", linewidth = 0.4,  linetype = "dotted") +
  geom_point(alpha = 0.45, size = 1.6, stroke = 0) +
  scale_x_log10(limits = c(ax_min, ax_max),
                breaks = c(1, 10, 100, 1000, 10000, 100000),
                labels = label_comma(accuracy = 1)) +
  scale_y_log10(limits = c(ax_min, ax_max),
                breaks = c(1, 10, 100, 1000, 10000, 100000),
                labels = label_comma(accuracy = 1)) +
  scale_color_manual(values = WALL_COLORS, name = "Wall Type") +
  coord_fixed() +
  labs(x = "Control States: Tank-Years per Cell (log scale)",
       y = "Texas: Tank-Years per Cell (log scale)") +
  theme_ust() +
  theme(legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3))

ggsave(file.path(OUTPUT_FIGURES, "FigureApp_Cell_Balance.png"),
       fig_balance, width = 7, height = 7, dpi = 300)
log_step("Saved: FigureApp_Cell_Balance.png")
cat("\n")


#### F9 Cell Unidentified (2-panel) ####

cat("--- F9: Cell unidentified ---\n")

cell_level[, cell_type := fcase(
  n_tx > 0L & n_ctl > 0L,  "Identified",
  n_tx > 0L & n_ctl == 0L, "Texas-Only\n(no control match)",
  n_tx == 0L & n_ctl > 0L, "Control-Only\n(no Texas match)",
  default = "Other"
)]
cell_level[, n_total := n_tx + n_ctl]

ecdf_top <- rbind(
  cell_level[cell_type == "Identified",  .(n_total, status = "Identified")],
  cell_level[cell_type %in% c("Texas-Only\n(no control match)",
                              "Control-Only\n(no Texas match)"),
             .(n_total, status = "Unidentified")]
)
ecdf_top[, status := factor(status, levels = c("Identified", "Unidentified"))]

fig_top <- ggplot(ecdf_top, aes(x = n_total, color = status, linetype = status)) +
  stat_ecdf(linewidth = 1.0, pad = FALSE) +
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000),
                labels = label_comma(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1), breaks = seq(0, 1, 0.2)) +
  scale_color_manual(values = c("Identified" = "#222222", "Unidentified" = "#aaaaaa"), name = NULL) +
  scale_linetype_manual(values = c("Identified" = "solid", "Unidentified" = "dashed"), name = NULL) +
  labs(x = "Tank-Years per Cell (log scale)", y = "Cumulative Share of Cells") +
  theme_ust(base_size = 10) +
  theme(panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3))

unid_summ <- cell_level[
  cell_type %in% c("Texas-Only\n(no control match)", "Control-Only\n(no Texas match)"),
  .(n_cells = .N, n_tank_yrs = sum(n_total)), by = cell_type
]
unid_long <- rbind(
  unid_summ[, .(cell_type, metric = "Cells",      value = n_cells)],
  unid_summ[, .(cell_type, metric = "Tank-Years", value = n_tank_yrs)]
)
unid_long[, metric := factor(metric, levels = c("Cells", "Tank-Years"))]
unid_long[, cell_type_label := fifelse(
  cell_type == "Texas-Only\n(no control match)", "Texas-Only", "Control-Only"
)]
unid_long[, cell_type_label := factor(cell_type_label, levels = c("Texas-Only", "Control-Only"))]

fig_bot <- ggplot(unid_long, aes(x = cell_type_label, y = value, fill = cell_type_label)) +
  geom_col(width = 0.55, color = "white", linewidth = 0.3) +
  geom_text(aes(label = label_comma(accuracy = 1)(value)),
            vjust = -0.35, size = 3, fontface = "bold", color = "gray30") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_fill_manual(values = c("Texas-Only" = COL_TX, "Control-Only" = COL_CTRL), guide = "none") +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Unidentified Cell Type", y = NULL) +
  theme_ust(base_size = 10) +
  theme(panel.grid.major.x = element_blank(),
        strip.text = element_text(size = 10, face = "bold"))

fig_unid <- fig_top / fig_bot + plot_layout(heights = c(1.4, 1))

ggsave(file.path(OUTPUT_FIGURES, "FigureApp_Cell_Unidentified.png"),
       fig_unid, width = 8, height = 8, dpi = 300)
log_step("Saved: FigureApp_Cell_Unidentified.png")
cat("\n")


#### F10 Propensity Score Overlap Histograms ####

cat("--- F10: Propensity score overlap histograms ---\n")

plot_pscore <- function(psm_dt, label) {
  pdt <- psm_dt[, .(pscore = pscore,
                    treatment_grp = fifelse(texas_treated == 1L, "Texas", "Control States"))]
  pdt[, treatment_grp := factor(treatment_grp, levels = c("Texas", "Control States"))]

  ggplot(pdt, aes(x = pscore, fill = treatment_grp)) +
    geom_histogram(aes(y = after_stat(density)),
                   binwidth = 0.05, boundary = 0, alpha = 0.55,
                   color = "white", linewidth = 0.2, position = "identity") +
    geom_vline(xintercept = 0.5, color = "#222222", linewidth = 0.9, linetype = "dashed") +
    scale_fill_manual(values = c("Texas" = COL_TX, "Control States" = COL_CTRL), name = NULL) +
    scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
    labs(x = "Estimated Propensity Score (Pr[Texas])", y = "Density",
         subtitle = label) +
    theme_ust()
}

ggsave(file.path(OUTPUT_FIGURES, "FigureApp_PSM_Overlap_Tank_Reform.png"),
       plot_pscore(psm_data_tank_reform, "Tank | Reform-Survivor"),
       width = 8, height = 5, dpi = 300)
ggsave(file.path(OUTPUT_FIGURES, "FigureApp_PSM_Overlap_Tank_Birth.png"),
       plot_pscore(psm_data_tank_birth, "Tank | Birth-Cohort"),
       width = 8, height = 5, dpi = 300)
ggsave(file.path(OUTPUT_FIGURES, "FigureApp_PSM_Overlap_Fac_Reform.png"),
       plot_pscore(psm_data_fac_reform, "Facility | Reform-Survivor"),
       width = 8, height = 5, dpi = 300)
ggsave(file.path(OUTPUT_FIGURES, "FigureApp_PSM_Overlap_Fac_Birth.png"),
       plot_pscore(psm_data_fac_birth, "Facility | Birth-Cohort"),
       width = 8, height = 5, dpi = 300)
log_step("Saved: 4 PSM overlap histograms")
cat("\n")


#### F11-F13 Birth-Cohort KM Figures ####

cat("--- F11-F13: Birth-cohort KM figures ---\n")

# Sample definitions for KM loop
bc_sample_defs <- list(
  list(label = "Full Sample (Birth-Cohort Base)",
       fname_tag = "Full",
       filter_expr = NULL,
       use_weight  = FALSE),
  list(label = "BC-CEM Matched",
       fname_tag = "BC_CEM",
       filter_expr = "bc_cem_matched == TRUE",
       use_weight  = TRUE,
       weight_col  = "bc_cem_weight"),
  list(label = "BC-PSM Matched (1:1)",
       fname_tag = "BC_PSM",
       filter_expr = "bc_psm_matched == TRUE",
       use_weight  = FALSE)
)

bc_vintage_loop <- list(
  list(lbl = "Install Cohort 1985-1989", fname = "Vintage_1985_1989", v = "1985-1989"),
  list(lbl = "Install Cohort 1990-1994", fname = "Vintage_1990_1994", v = "1990-1994"),
  list(lbl = "Install Cohort 1995-1999", fname = "Vintage_1995_1999", v = "1995-1999"),
  list(lbl = "Install Cohort 2000-2004", fname = "Vintage_2000_2004", v = "2000-2004"),
  list(lbl = "Install Cohort 2005-2009", fname = "Vintage_2005_2009", v = "2005-2009"),
  list(lbl = "Install Cohort 2010-2014", fname = "Vintage_2010_2014", v = "2010-2014"),
  list(lbl = "Install Cohort 2015-2018", fname = "Vintage_2015_2018", v = "2015-2018"),
  list(lbl = "Full Sample 1985-2018",    fname = "FullSample",        v = "ALL")
)

# ---- F11: Survival curves TX vs Control ----
cat("\nF11: Survival curves...\n")

for (samp in bc_sample_defs) {
  for (item in bc_vintage_loop) {

    sub_dt <- if (item$v == "ALL") bc_tanks[!is.na(vintage5)] else bc_tanks[vintage5 == item$v]
    if (!is.null(samp$filter_expr)) sub_dt <- sub_dt[eval(parse(text = samp$filter_expr))]
    if (nrow(sub_dt) < 50 ||
        sub_dt[texas_treated == 1L, .N] == 0 ||
        sub_dt[texas_treated == 0L, .N] == 0) next

    surv_obj <- Surv(time = sub_dt$t_enter_cal,
                     time2 = sub_dt$t_exit_cal,
                     event = sub_dt$failure)

    if (isTRUE(samp$use_weight)) {
      wts <- sub_dt[[samp$weight_col]]
      fit <- survfit(surv_obj ~ group, data = sub_dt, weights = wts)
    } else {
      fit <- survfit(surv_obj ~ group, data = sub_dt)
    }

    dt_km <- as.data.table(broom::tidy(fit))
    dt_km[, group := fifelse(grepl("Texas", strata), "Texas", "Control States")]
    dt_km[, group := factor(group, levels = c("Texas", "Control States"))]

    x_lo <- if (item$v == "ALL") 1970 else as.integer(substr(item$v, 1, 4))
    x_hi <- 2021

    fig_surv <- ggplot(dt_km, aes(x = time, y = estimate, color = group, fill = group)) +
      geom_vline(xintercept = REFORM_CAL, color = "gray30",
                 linewidth = 0.8, linetype = "dashed") +
      geom_step(linewidth = 0.9) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
      annotate("text", x = REFORM_CAL + 0.2, y = 0.97,
               label = "Reform\nJan 1999", hjust = 0, vjust = 1, size = 3, color = "gray30") +
      scale_color_manual(values = c("Texas" = COL_TX, "Control States" = COL_CTRL), name = NULL) +
      scale_fill_manual( values = c("Texas" = COL_TX, "Control States" = COL_CTRL), name = NULL) +
      scale_x_continuous(limits = c(x_lo, x_hi),
                         breaks = seq(x_lo, x_hi, 5),
                         labels = as.character(seq(x_lo, x_hi, 5))) +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                         labels = percent_format(accuracy = 1)) +
      labs(x = "Calendar Year", y = "Survival Probability (Fraction Still Active)",
           title    = sprintf("Birth-Cohort KM Survival - %s", samp$label),
           subtitle = item$lbl) +
      theme_ust() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    fname <- sprintf("KM_BC_Survival_%s_%s.png", item$fname, samp$fname_tag)
    ggsave(file.path(OUTPUT_FIGURES, fname),
           fig_surv, width = 9, height = 6, dpi = 300)
    log_step(sprintf("Saved: %s", fname))
  }
}

# ---- F12: Annual difference dot plots ----
cat("\nF12: Annual difference dot plots...\n")

for (samp in bc_sample_defs) {
  for (item in bc_vintage_loop) {

    sub_dt <- if (item$v == "ALL") bc_tanks[!is.na(vintage5)] else bc_tanks[vintage5 == item$v]
    if (!is.null(samp$filter_expr)) sub_dt <- sub_dt[eval(parse(text = samp$filter_expr))]
    if (nrow(sub_dt) < 50 ||
        sub_dt[texas_treated == 1L, .N] == 0 ||
        sub_dt[texas_treated == 0L, .N] == 0) next

    surv_obj <- Surv(time = sub_dt$t_enter_cal,
                     time2 = sub_dt$t_exit_cal,
                     event = sub_dt$failure)

    if (isTRUE(samp$use_weight)) {
      wts <- sub_dt[[samp$weight_col]]
      fit <- survfit(surv_obj ~ group, data = sub_dt, weights = wts)
    } else {
      fit <- survfit(surv_obj ~ group, data = sub_dt)
    }

    dt_km <- as.data.table(broom::tidy(fit))
    dt_km[, group := fifelse(grepl("Texas", strata), "Texas", "Control States")]
    dt_km[, group := factor(group, levels = c("Texas", "Control States"))]

    y_lo <- if (item$v == "ALL") 1985L else as.integer(substr(item$v, 1, 4))
    year_grid <- seq(y_lo, 2020L)
    ann_dt <- make_annual_diff(dt_km, year_grid = year_grid)
    ann_dt[, period := fifelse(year < 1999, "Pre-Reform", "Post-Reform")]
    ann_dt[, period := factor(period, levels = c("Pre-Reform", "Post-Reform"))]

    fig_dot <- ggplot(ann_dt, aes(x = year, y = diff, color = period, fill = period)) +
      geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
      geom_vline(xintercept = REFORM_CAL, color = "gray30",
                 linewidth = 0.8, linetype = "dashed") +
      geom_errorbar(aes(ymin = diff - 1.96 * se_diff, ymax = diff + 1.96 * se_diff),
                    width = 0.3, linewidth = 0.4, alpha = 0.7) +
      geom_point(size = 2.5, shape = 21, alpha = 0.9) +
      scale_color_manual(values = c("Pre-Reform" = "gray50", "Post-Reform" = COL_TX),
                         name = NULL, guide = "none") +
      scale_fill_manual(values = c("Pre-Reform" = "gray50", "Post-Reform" = COL_TX), name = NULL) +
      scale_x_continuous(breaks = seq(y_lo, 2020, 5), labels = as.character(seq(y_lo, 2020, 5))) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
      labs(x = "Calendar Year",
           y = "S_Texas(t) minus S_Control(t)",
           title = sprintf("Birth-Cohort Annual Survival Difference - %s", samp$label),
           subtitle = sprintf("%s | Bars = 95%% CI (Greenwood SE)", item$lbl)) +
      theme_ust() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    fname <- sprintf("KM_BC_AnnualDiff_DotPlot_%s_%s.png", item$fname, samp$fname_tag)
    ggsave(file.path(OUTPUT_FIGURES, fname),
           fig_dot, width = 10, height = 6, dpi = 300)
    log_step(sprintf("Saved: %s", fname))
  }
}

# ---- F13: All-vintage overlay difference ----
cat("\nF13: All-vintage overlay difference...\n")

vintage_colors <- c(
  "1985-1989" = "#000000",
  "1990-1994" = "#404040",
  "1995-1999" = "#707070",
  "2000-2004" = "#a0a0a0",
  "2005-2009" = "#c8c8c8",
  "2010-2014" = "#d55e00",
  "2015-2018" = "#ff8c4a"
)
vintage_ltys <- c(
  "1985-1989" = "solid",
  "1990-1994" = "solid",
  "1995-1999" = "solid",
  "2000-2004" = "solid",
  "2005-2009" = "solid",
  "2010-2014" = "dashed",
  "2015-2018" = "dashed"
)

for (samp in bc_sample_defs) {

  diff_overlay <- rbindlist(lapply(
    setdiff(levels(bc_tanks$vintage5), NA),
    function(v) {
      sub_dt <- bc_tanks[vintage5 == v]
      if (!is.null(samp$filter_expr)) sub_dt <- sub_dt[eval(parse(text = samp$filter_expr))]
      if (nrow(sub_dt) < 50 ||
          sub_dt[texas_treated == 1L, .N] == 0 ||
          sub_dt[texas_treated == 0L, .N] == 0) return(NULL)

      surv_obj <- Surv(sub_dt$t_enter_cal, sub_dt$t_exit_cal, sub_dt$failure)
      fit <- if (isTRUE(samp$use_weight)) {
        survfit(surv_obj ~ group, data = sub_dt, weights = sub_dt[[samp$weight_col]])
      } else {
        survfit(surv_obj ~ group, data = sub_dt)
      }

      dt_km <- as.data.table(broom::tidy(fit))
      dt_km[, group := fifelse(grepl("Texas", strata), "Texas", "Control States")]
      dt_km[, group := factor(group, levels = c("Texas", "Control States"))]

      y_lo <- as.integer(substr(v, 1, 4))
      ann  <- make_annual_diff(dt_km, year_grid = seq(y_lo, 2020L))
      ann[, vintage5 := v]
      ann
    }
  ))
  diff_overlay[, vintage5 := factor(vintage5, levels = names(vintage_colors))]

  fig_overlay <- ggplot(diff_overlay,
      aes(x = year, y = diff, color = vintage5, linetype = vintage5)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
    geom_vline(xintercept = REFORM_CAL, color = "gray30",
               linewidth = 0.8, linetype = "dashed") +
    geom_ribbon(aes(ymin = diff - 1.96 * se_diff,
                    ymax = diff + 1.96 * se_diff,
                    fill = vintage5),
                alpha = 0.08, color = NA) +
    geom_line(linewidth = 0.85) +
    scale_color_manual(values = vintage_colors, name = "Install Cohort") +
    scale_fill_manual( values = vintage_colors, name = "Install Cohort") +
    scale_linetype_manual(values = vintage_ltys, name = "Install Cohort") +
    scale_x_continuous(limits = c(1970, 2021),
                       breaks = seq(1970, 2020, 5),
                       labels = as.character(seq(1970, 2020, 5))) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(x = "Calendar Year",
         y = "S_Texas(t) minus S_Control(t)",
         title = sprintf("Birth-Cohort Survival Difference by Vintage - %s", samp$label)) +
    theme_ust() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  fname <- sprintf("KM_BC_AllVintage_Overlay_Difference_%s.png", samp$fname_tag)
  ggsave(file.path(OUTPUT_FIGURES, fname),
         fig_overlay, width = 11, height = 7, dpi = 300)
  log_step(sprintf("Saved: %s", fname))
}


################################################################################
# APPEND AFTER F13 in 02b_Panel_Build.R
# Restores: F4 Overall + Bar figures, F5 5-year bar, 4 diagnostic CSVs
################################################################################


#### F4-Supp: Tank Population Composition (Overall + Bar versions) ####

cat("--- F4-Supp: Composition figures (Overall + Bar versions) ---\n")

# ---- Build comp_timeline_all (aggregate across all states) ----
# composition_data and decade_order already exist from F4.
# comp_by_state also already exists from F4 and is used for Bar_byState.

comp_timeline_all <- composition_data[, .(
  n_tanks_cohort = uniqueN(tank_panel_id)
), by = .(panel_year, install_decade)]

complete_grid_all <- CJ(
  panel_year    = 1990L:2018L,
  install_decade = decade_order         # character vector — CJ expands correctly
)
comp_timeline_all <- merge(
  comp_timeline_all, complete_grid_all,
  by = c("panel_year", "install_decade"), all = TRUE
)
comp_timeline_all[is.na(n_tanks_cohort), n_tanks_cohort := 0L]

total_by_year_all <- comp_timeline_all[,
  .(n_tanks_total = sum(n_tanks_cohort)), by = panel_year]
comp_timeline_all <- merge(comp_timeline_all, total_by_year_all, by = "panel_year")
comp_timeline_all[, share := 100 * n_tanks_cohort / n_tanks_total]
comp_timeline_all[, install_decade := factor(install_decade, levels = decade_order)]
setorder(comp_timeline_all, panel_year, install_decade)

# ---- Figure: Overall stacked area ----
composition_plot_all <- ggplot(
  comp_timeline_all,
  aes(x = panel_year, y = share, fill = install_decade, group = install_decade)
) +
  geom_area(alpha = 0.85, color = "black", linewidth = 0.15, position = "stack") +
  labs(x = "Year", y = "Share of Active Tank Inventory (%)", fill = "Install Period") +
  scale_x_continuous(limits = c(1990, 2018), breaks = seq(1990, 2018, 5)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = bw_palette) +
  theme_ust() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Tank_Population_Composition_Overall.png"),
       composition_plot_all, width = 10, height = 6, dpi = 300)
log_step("Saved: Tank_Population_Composition_Overall.png")

# ---- Figure: 6-period stacked bar (overall) ----
comp_timeline_all[, period := fcase(
  panel_year %between% c(1990L, 1994L), "1990-1994",
  panel_year %between% c(1995L, 1999L), "1995-1999",
  panel_year %between% c(2000L, 2004L), "2000-2004",
  panel_year %between% c(2005L, 2009L), "2005-2009",
  panel_year %between% c(2010L, 2014L), "2010-2014",
  panel_year %between% c(2015L, 2018L), "2015-2018"
)]

bar_comp <- comp_timeline_all[!is.na(period), .(
  mean_share = mean(share, na.rm = TRUE)
), by = .(period, install_decade)]

# Renormalize within each period so bars sum to 100
bar_comp[, mean_share := 100 * mean_share / sum(mean_share), by = period]
bar_comp[, period       := factor(period, levels = c(
  "1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018"
))]
bar_comp[, install_decade := factor(install_decade, levels = decade_order)]
bar_comp[, label      := ifelse(mean_share >= 5, sprintf("%.0f%%", mean_share), "")]
# White text on dark fills (1940-1979, 1980-1989), black on lighter fills
bar_comp[, text_color := ifelse(as.integer(install_decade) <= 2L, "white", "black")]

fig_bar_comp <- ggplot(bar_comp,
    aes(x = period, y = mean_share, fill = install_decade)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(aes(label = label, color = text_color),
            position = position_stack(vjust = 0.5),
            size = 3, fontface = "bold", show.legend = FALSE) +
  scale_fill_manual(values = bw_palette) +
  scale_color_identity() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Share of Active Tank Inventory (%)", fill = "Install Period") +
  theme_ust()

ggsave(file.path(OUTPUT_FIGURES, "Tank_Population_Composition_Bar.png"),
       fig_bar_comp, width = 8, height = 5, dpi = 300)
log_step("Saved: Tank_Population_Composition_Bar.png")

# ---- Figure: 6-period stacked bar by TX vs Control ----
# Uses comp_by_state already built in F4 (has panel_year, state_group,
# install_decade, share). Copy to avoid mutating the F4 object.

bar_comp_state <- copy(comp_by_state)[panel_year %between% c(1990L, 2018L)]
bar_comp_state[, period := fcase(
  panel_year %between% c(1990L, 1994L), "1990-1994",
  panel_year %between% c(1995L, 1999L), "1995-1999",
  panel_year %between% c(2000L, 2004L), "2000-2004",
  panel_year %between% c(2005L, 2009L), "2005-2009",
  panel_year %between% c(2010L, 2014L), "2010-2014",
  panel_year %between% c(2015L, 2018L), "2015-2018"
)]

bar_comp_state <- bar_comp_state[!is.na(period), .(
  mean_share = mean(share, na.rm = TRUE)
), by = .(period, state_group, install_decade)]

bar_comp_state[, mean_share := 100 * mean_share / sum(mean_share),
               by = .(period, state_group)]
bar_comp_state[, period := factor(period, levels = c(
  "1990-1994", "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2018"
))]
bar_comp_state[, install_decade := factor(install_decade, levels = decade_order)]
bar_comp_state[, state_group    := factor(state_group, levels = c("Control", "Texas"))]
bar_comp_state[, label := ifelse(mean_share >= 6, sprintf("%.0f%%", mean_share), "")]

fig_bar_comp_state <- ggplot(bar_comp_state,
    aes(x = period, y = mean_share, fill = install_decade)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 2.8, color = "white", fontface = "bold") +
  facet_wrap(~ state_group, ncol = 2) +
  scale_fill_manual(values = bw_palette) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Share of Active Tank Inventory (%)", fill = "Install Period") +
  theme_ust() +
  theme(strip.text      = element_text(face = "bold", size = 11),
        panel.spacing.x = unit(16, "pt"))

ggsave(file.path(OUTPUT_FIGURES, "Tank_Population_Composition_Bar_byState.png"),
       fig_bar_comp_state, width = 10, height = 6, dpi = 300)
log_step("Saved: Tank_Population_Composition_Bar_byState.png")
cat("\n")


#### F5-Supp: Fleet Exit-Rate Diff Bars (8-period, 5-year bins) ####

cat("--- F5-Supp: Fleet exit-rate diff bars (5-year bins) ---\n")

# cohort_wide and cohort_palette already exist from F5.
# Add period5 column without overwriting the existing period column.

cohort_wide[, period5 := fcase(
  panel_year %between% c(1980L, 1984L), "1980-1984",
  panel_year %between% c(1985L, 1989L), "1985-1989",
  panel_year %between% c(1990L, 1994L), "1990-1994",
  panel_year %between% c(1995L, 1999L), "1995-1999",
  panel_year %between% c(2000L, 2004L), "2000-2004",
  panel_year %between% c(2005L, 2009L), "2005-2009",
  panel_year %between% c(2010L, 2014L), "2010-2014",
  panel_year %between% c(2015L, 2018L), "2015-2018"
)]

bar_diff_5yr <- cohort_wide[!is.na(period5), .(
  mean_diff = mean(diff, na.rm = TRUE)
), by = .(period5, cohort_group)]

bar_diff_5yr[, period5 := factor(period5, levels = c(
  "1980-1984", "1985-1989", "1990-1994", "1995-1999",
  "2000-2004", "2005-2009", "2010-2014", "2015-2018"
))]
bar_diff_5yr[, cohort_group := factor(cohort_group,
  levels = c("Pre-1980", "1980-1988", "1989-1998"))]

fig_bar_diff_5yr <- ggplot(bar_diff_5yr,
    aes(x = period5, y = mean_diff, fill = cohort_group)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.65,
           color = "white", linewidth = 0.3) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 4.5, linetype = "dashed",
             color = "gray40", linewidth = 0.5) +
  scale_fill_manual(
    values = cohort_palette,
    labels = c("Pre-1980"  = "Pre-1980 (highest risk)",
               "1980-1988" = "1980\u20131988",
               "1989-1998" = "1989\u20131998 (placebo)")
  ) +
  scale_y_continuous(
    labels = function(x) ifelse(x == 0, "0", paste0(x, " pp"))
  ) +
  labs(x = NULL,
       y    = "Texas minus Control States (percentage points)",
       fill = "Install Cohort") +
  theme_ust() +
  theme(panel.grid.major.x = element_blank())

ggsave(file.path(OUTPUT_FIGURES, "Fleet_ExitRate_Diff_5yr_Bar.png"),
       fig_bar_diff_5yr, width = 9, height = 5, dpi = 300)
log_step("Saved: Fleet_ExitRate_Diff_5yr_Bar.png")
cat("\n")


#### Diagnostic CSVs (ported from old S5 / S10) ####

cat("--- Diagnostic CSVs ---\n")

# 1. Mandate coverage by install cohort and deadline year
#    (detailed tank-year counts per mandate window)
mand_summ <- tank_year_panel[
  install_yr_int >= 1940L & (
    mandate_release_det    == 1L |
    mandate_spill_overfill == 1L |
    mandate_integrity      == 1L),
  .(
    n_ty_release_det    = sum(mandate_release_det),
    n_ty_spill_overfill = sum(mandate_spill_overfill),
    n_ty_integrity      = sum(mandate_integrity),
    n_tanks             = uniqueN(tank_panel_id)
  ),
  by = .(mm_install_cohort, release_det_deadline_yr)
][order(mm_install_cohort)]

fwrite(mand_summ, file.path(OUTPUT_TABLES, "Diag_MandateCoverage.csv"))
log_step("Saved: Diag_MandateCoverage.csv")

# 2. CEM balance weights — tank-level, reform-survivor pool
#    tank_chars_reform_cem carries cem_weight from S10
fwrite(
  tank_chars_reform_cem[, .(tank_panel_id, cem_weight)],
  file.path(OUTPUT_TABLES, "Diag_CEM_Balance.csv")
)
log_step("Saved: Diag_CEM_Balance.csv")

# 3. PSM propensity scores — tank-level, reform-survivor pool
#    psm_data_tank_reform carries pscore, logit_pscore, psm_weight from S10
fwrite(
  psm_data_tank_reform[, .(tank_panel_id, psm_weight, pscore, logit_pscore)],
  file.path(OUTPUT_TABLES, "Diag_PSM_Pscores.csv")
)
log_step("Saved: Diag_PSM_Pscores.csv")

# 4. Missing capacity — 1987 SW TX tanks flagged for upstream registry review
#    tank_chars_reform is the reform-pool one-row-per-tank table from S10
missing_1987_sw <- tank_chars_reform[
  texas_treated == 1L      &
  mm_wall       == "Single-Walled" &
  mm_install_cohort == 1987L       &
  is.na(mm_capacity),
  .(tank_panel_id, panel_id)
]
fwrite(missing_1987_sw,
       file.path(OUTPUT_TABLES, "Diag_Missing_Capacity_1987_SW_TX.csv"))
log_step(sprintf("Saved: Diag_Missing_Capacity_1987_SW_TX.csv (%s tanks flagged)",
  fmt_n(nrow(missing_1987_sw))))

cat("\n")


cat("\n========================================\n")
cat("02b_Panel_Build.R COMPLETE\n")
cat("========================================\n\n")