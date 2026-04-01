################################################################################
# 02b_Panel_Build.R
# Texas UST Insurance Reform — Panel and Sample Construction
#
# LEGAL HISTORY NOTE (UST_Legal_History.md):
#   The upgrade mandate phase-in schedule is federal (40 CFR §280.21) and
#   IDENTICAL across all states. Texas 30 TAC §§334.44/334.47 mirror it exactly
#   as a condition of State Program Approval. Key implications:
#
#   (1) Pre-1989 install cohorts are NOT excluded — the mandate applied
#       identically to TX and all control states; they are not confounded.
#   (2) make_model_tank cells use the federal legal vintage bins since
#       those are the exact risk dimensions private insurers priced in 1999.
#   (3) mandate_active is built cohort-specifically from the legal schedule,
#       not as a coarse 1989-1993 flag.
#
#   Release detection compliance deadlines (§280.21 / §334.47(b)(4)):
#     install year <= 1964:   deadline Dec 22, 1989
#     1965-1969:              deadline Dec 22, 1990
#     1970-1974:              deadline Dec 22, 1991
#     1975-1979:              deadline Dec 22, 1992
#     1980-1988:              deadline Dec 22, 1993
#   Spill/overfill (all pre-1989 installs):   Dec 22, 1994
#   Tank integrity + cathodic (all installs):  Dec 22, 1998
#
# OUTPUTS (Data/Analysis/ — CSV):
#   panel_dt.csv          annual tank-year panel (primary analysis sample)
#   exact_base.csv        exact-date Cox base (one row per tank)
#   matched_tanks.csv     CEM-matched annual panel
#   matched_ids.csv       tank_panel_id of matched tanks
#   panel_meta.csv        scalar constants
#
# SECTIONS:
#   S1   Setup & Constants
#   S2   Utility Functions
#   S3   Load Master Tanks
#   S4   Build Annual Tank-Year Panel
#   S5   Add Mandate Controls (vintage-cohort specific, per legal schedule)
#   S6   Define Primary Sample (panel_dt)
#   S7   Cell Coverage Diagnostics
#   S8   Build Exact-Date Cox Base
#   S9   Analysis Sample Construction (SW-exposed facilities)
#   S10  CEM Matching -> matched_tanks
#   S11  Post-Match Variable Construction
#   S12  Save Outputs as CSV
################################################################################


#### S1 Setup & Constants ####

suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(ggplot2)
  library(scales)
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
POST_YEAR   <- 1999L
REFORM_DATE <- as.IDate("1998-12-22")
REFORM_DAYS <- as.numeric(as.Date("1998-12-22"))
STUDY_END   <- as.IDate("2020-12-31")

# ---- Federal/Texas release detection compliance deadlines ----
# 40 CFR §280.21 / 30 TAC §334.47(b)(4) — identical in both programs
RELEASE_DET_DEADLINE_YR <- list(
  pre_1965 = 1989L,   # install year <= 1964 (or unknown)
  y1965_69 = 1990L,   # 1965-1969
  y1970_74 = 1991L,   # 1970-1974
  y1975_79 = 1992L,   # 1975-1979
  y1980_88 = 1993L    # 1980-Dec 22, 1988
)
# post-1988 installs: comply from installation — no phase-in deadline
SPILL_OVERFILL_DEADLINE_YR <- 1994L   # §280.20(c) / §334.44(b)(1)(B); all pre-1989 installs
INTEGRITY_DEADLINE_YR      <- 1998L   # §280.21(b) / §334.44(b)(1)(A); all installs

# ---- Study states ----
# PA excluded: PA Act 16 (June 1995) drives a spurious 1995 SW closure spike
# unrelated to the Texas reform — confirmed by leave-one-out event study.
CONTROL_STATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Colors ----
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"
COL_PRE  <- "#E69F00"


#### S2 Utility Functions ####

log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_pct  <- function(x) sprintf("%.1f%%", x * 100)


#### S3 Load Master Tanks ####

cat("\n========================================\n")
cat("S3: LOAD MASTER TANKS\n")
cat("========================================\n\n")

master_tanks <- fread(
  here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("Loaded: %s rows, %d columns", fmt_n(nrow(master_tanks)), ncol(master_tanks)))

required_cols <- c("mm_wall", "mm_fuel", "mm_capacity", "mm_install_cohort",
                   "facility_id", "state", "tank_id",
                   "tank_installed_date", "tank_closed_date")
missing_cols <- setdiff(required_cols, names(master_tanks))
stopifnot("Required columns missing — re-run harmonization script." = length(missing_cols) == 0)

master_tanks[, tank_installed_date := as.IDate(tank_installed_date)]
master_tanks[, tank_closed_date    := as.IDate(tank_closed_date)]

# Treatment assignment — NA for non-study states
master_tanks[, texas_treated := fcase(
  state == "TX",              1L,
  state %in% CONTROL_STATES, 0L,
  default = NA_integer_
)]

log_step(sprintf("TX=%s  CTL=%s  non-study=%s",
  fmt_n(master_tanks[texas_treated == 1L, .N]),
  fmt_n(master_tanks[texas_treated == 0L, .N]),
  fmt_n(master_tanks[is.na(texas_treated), .N])))

# Composite identifiers — facility_id alone is NOT unique across states
master_tanks[, panel_id     := paste(facility_id, state, sep = "_")]
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

# ---- Install year as integer ----
study_tanks[, install_yr_int := as.integer(mm_install_cohort)]

# ---- Release detection deadline year per tank ----
# NA = post-1988 install; no phase-in; exempt from mandate window controls
study_tanks[, release_det_deadline_yr := fcase(
  install_yr_int <= 1964L,           RELEASE_DET_DEADLINE_YR$pre_1965,
  install_yr_int %in% 1965L:1969L,   RELEASE_DET_DEADLINE_YR$y1965_69,
  install_yr_int %in% 1970L:1974L,   RELEASE_DET_DEADLINE_YR$y1970_74,
  install_yr_int %in% 1975L:1979L,   RELEASE_DET_DEADLINE_YR$y1975_79,
  install_yr_int %in% 1980L:1988L,   RELEASE_DET_DEADLINE_YR$y1980_88,
  default = NA_integer_
)]

# ---- Make-model cell variables ----
# make_model_tank  : wall x fuel x capacity x install_year (primary cell)
#   Using actual install year (not bins) so that cell x year FE specifications
#   identify vintage-year effects through within-cell variation over time.
#   Private insurers priced at the individual cohort year level post-reform.
# make_model_noage : wall x fuel x capacity (for Cox age-HTE specs where
#   the age dimension must not be absorbed into the baseline hazard stratum)
# make_model_3dim  : wall x fuel x install_year (robustness — drops capacity)
study_tanks[, make_model_tank  := paste(mm_wall, mm_fuel, mm_capacity, mm_install_cohort, sep = "|")]
study_tanks[, make_model_noage := paste(mm_wall, mm_fuel, mm_capacity, sep = "|")]
study_tanks[, make_model_3dim  := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "|")]

# Null out cells for tanks with unknown wall, fuel, or missing install cohort
# — these cannot be matched and would create spurious cells
study_tanks[
  mm_wall == "Unknown-Wall" | mm_fuel == "Unknown-Fuel" | is.na(mm_install_cohort),
  `:=`(make_model_tank  = NA_character_,
       make_model_noage = NA_character_,
       make_model_3dim  = NA_character_)
]

# ---- Expand to annual tank-year panel (1985-2020) ----
log_step("Expanding to annual tank-year panel (1985-2020)...")

study_tanks[, `:=`(
  install_yr   = year(tank_installed_date),
  close_yr_raw = fifelse(!is.na(tank_closed_date), year(tank_closed_date), 2020L)
)]
study_tanks[, `:=`(
  expand_start = pmax(install_yr, 1985L),
  expand_end   = pmin(close_yr_raw, 2020L)
)]

panel_cols <- c(
  "tank_panel_id", "panel_id", "facility_id", "state", "texas_treated",
  "mm_wall", "mm_fuel", "mm_capacity",
  "mm_install_cohort", "install_yr_int",
  "make_model_tank", "make_model_noage", "make_model_3dim",
  "release_det_deadline_yr",
  "tank_installed_date", "tank_closed_date",
  "expand_start", "expand_end"
)

tank_year_panel <- study_tanks[
  expand_start <= expand_end, .SD, .SDcols = panel_cols
][, {
  yrs <- seq(expand_start, expand_end)
  .(panel_year    = yrs,
    closure_event = as.integer(!is.na(tank_closed_date) & year(tank_closed_date) == yrs))
}, by = panel_cols]

setorder(tank_year_panel, tank_panel_id, panel_year)

log_step(sprintf("Panel: %s rows | %s tanks | %s facilities",
  fmt_n(nrow(tank_year_panel)),
  fmt_n(uniqueN(tank_year_panel$tank_panel_id)),
  fmt_n(uniqueN(tank_year_panel$panel_id))))
cat("\n")


#### S5 Add Mandate Controls ####

cat("========================================\n")
cat("S5: ADD MANDATE CONTROLS\n")
cat("========================================\n\n")

# Core DiD time variables
tank_year_panel[, `:=`(
  post_1999 = as.integer(panel_year >= POST_YEAR),
  did_term  = texas_treated * as.integer(panel_year >= POST_YEAR),
  rel_year  = panel_year - POST_YEAR,
  tstart    = panel_year - 1L,
  tstop     = panel_year,
  tank_age  = panel_year - install_yr_int
)]

# ---- Release detection mandate window ----
# A tank-year is "in mandate" if the tank's cohort was in its federal
# compliance transition window: from 1989 (when Ch. 334 took effect)
# through the cohort's specific release detection deadline year.
# Both TX and control states faced this identically.
tank_year_panel[, release_det_mandate := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year >= 1989L &
  panel_year <= release_det_deadline_yr
)]

# ---- Spill/overfill mandate window ----
# All pre-1989 install tanks; §280.20(c) / §334.44(b)(1)(B); deadline Dec 22, 1994
tank_year_panel[, spill_overfill_mandate := as.integer(
  !is.na(release_det_deadline_yr) &
  panel_year %in% 1993L:1994L
)]

# ---- Tank integrity / cathodic protection window ----
# All UST systems; §280.21(b) / §334.44(b)(1)(A); deadline Dec 22, 1998
# Use 1996-1998 as the pressure window (3 years leading up to deadline)
tank_year_panel[, integrity_mandate := as.integer(
  panel_year %in% 1996L:1998L
)]

# ---- Combined mandate pressure flag ----
# Include as a covariate to absorb mandate-driven closure confound in regressions.
# Any of the three federal compliance windows active in panel_year for this cohort.
tank_year_panel[, mandate_active := as.integer(
  release_det_mandate == 1L |
  spill_overfill_mandate == 1L |
  integrity_mandate == 1L
)]

# ---- Binned relative year for event studies ----
tank_year_panel[, rel_year_bin := fcase(
  rel_year <= -8L, -8L,
  rel_year >= 15L, 15L,
  default = as.integer(rel_year)
)]

# ---- Mandate coverage summary ----
mand_summ <- tank_year_panel[mandate_active == 1L, .(
  n_ty    = .N,
  n_tanks = uniqueN(tank_panel_id)
), by = .(mm_install_cohort, release_det_deadline_yr)][order(mm_install_cohort)]

log_step("Mandate coverage (tank-years with mandate_active == 1):")
print(mand_summ)
fwrite(mand_summ, file.path(OUTPUT_TABLES, "Diag_MandateCoverage.csv"))
cat("\n")


#### S6 Define Primary Sample (panel_dt) ####

cat("========================================\n")
cat("S6: DEFINE PRIMARY SAMPLE\n")
cat("========================================\n\n")

# Include all vintage bins — pre-1989 cohorts retained.
# Drop installation-year rows (panel_year == install year): high closure
# rates there reflect failed inspections, not economic exit decisions.
# Cells with unknown wall/fuel (make_model_tank = NA) are excluded —
# they cannot be matched and break cell x year FE identification.

panel_dt <- tank_year_panel[
  !is.na(make_model_tank) &
  !is.na(did_term)        &
  state %in% STUDY_STATES &
  tstop > tstart          &
  panel_year > install_yr_int
]

stopifnot("Non-study states in panel_dt" =
  length(setdiff(unique(panel_dt$state), STUDY_STATES)) == 0)

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

log_step("\nInstall-year cohort composition:")
print(cohort_comp)
fwrite(cohort_comp, file.path(OUTPUT_TABLES, "Diag_InstallCohortComposition.csv"))

# Exclusion accounting
full_eligible <- tank_year_panel[
  !is.na(did_term) & state %in% STUDY_STATES &
  tstop > tstart & panel_year > install_yr_int
]
excl_na_cell <- full_eligible[is.na(make_model_tank), .N]
log_step(sprintf("\nExcluded for unknown cell: %s / %s (%s)",
  fmt_n(excl_na_cell), fmt_n(nrow(full_eligible)),
  fmt_pct(excl_na_cell / nrow(full_eligible))))
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

log_step("\nTank-years by install-year cohort:")
print(cov_by_cohort)

fwrite(cell_diag,     file.path(OUTPUT_TABLES, "Diag_CellCoverage_Detail.csv"))
fwrite(cov_summ,      file.path(OUTPUT_TABLES, "Diag_CellCoverage_Summary.csv"))
fwrite(cov_by_cohort, file.path(OUTPUT_TABLES, "Diag_CellCoverage_ByCohort.csv"))
cat("\n")


#### S8 Build Exact-Date Cox Base ####

cat("========================================\n")
cat("S8: BUILD EXACT-DATE COX BASE\n")
cat("========================================\n\n")

# One row per tank. Time axis in days since 1970-01-01 (numeric).
# survSplit at REFORM_DAYS is done in the analysis script.
# All vintage bins included — no cohort cutoff.

exact_base <- study_tanks[
  !is.na(texas_treated)   &
  !is.na(make_model_tank) &
  state %in% STUDY_STATES &
  !is.na(tank_installed_date) &
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

# Drop installation-year exits (same churn exclusion as panel)
exact_base[, exit_yr_tmp := as.integer(format(as.Date(t_exit, origin = "1970-01-01"), "%Y"))]
exact_base <- exact_base[!(exit_yr_tmp == install_yr_int & failure == 1L)]
exact_base[, exit_yr_tmp := NULL]

exact_base[, `:=`(
  age_enter = 0,
  age_exit  = as.numeric(t_exit - t_enter) / 365.25
)]

log_step(sprintf("exact_base: %s tanks | %s events | %s install-year cohorts",
  fmt_n(nrow(exact_base)),
  fmt_n(sum(exact_base$failure)),
  uniqueN(exact_base$mm_install_cohort)))
cat("\n")


#### S9 Analysis Sample Construction ####

cat("========================================\n")
cat("S9: ANALYSIS SAMPLE CONSTRUCTION\n")
cat("========================================\n\n")

# Restrict to facilities with at least one Single-Walled tank in panel_dt.
# SW tanks carried the highest private-insurer risk premium post-reform.
# This keeps the analysis focused on facilities most directly exposed
# to the insurance regime change. All vintage bins at those facilities
# are included; mandate_active absorbs compliance-window pressure.

sw_exposed_facs <- panel_dt[mm_wall == "Single-Walled", unique(panel_id)]
log_step(sprintf("SW-exposed facilities: %s", fmt_n(length(sw_exposed_facs))))

analysis_tanks <- panel_dt[panel_id %in% sw_exposed_facs]

# Treatment and event-study variables
analysis_tanks[, `:=`(
  did_term  = texas_treated * as.integer(panel_year >= 1999L),
  did_short = texas_treated * as.integer(panel_year >= 1999L & panel_year <= 2004L),
  did_long  = texas_treated * as.integer(panel_year >= 2005L),
  rel_year  = panel_year - 1999L
)]

# rel_year_es: binned [-8, 10]
analysis_tanks[, rel_year_es := fcase(
  rel_year <= -8L, -8L,
  rel_year >= 10L, 10L,
  default = as.integer(rel_year)
)]

# Anticipation spec variables (reform date = 1998)
analysis_tanks[, rel_year_early := panel_year - 1998L]
analysis_tanks[, rel_year_early := pmax(pmin(rel_year_early, 10L), -8L)]
analysis_tanks[, did_term_early := texas_treated * as.integer(panel_year >= 1998L)]
analysis_tanks[, deadline_sw    := as.integer(panel_year == 1998L) *
                                    as.integer(mm_wall == "Single-Walled")]

# Age at treatment date (years from install to 1999)
analysis_tanks[, age_at_treatment := 1999L - install_yr_int]

log_step(sprintf("Analysis sample: TX=%s fac / %s tanks | CTL=%s fac / %s tanks | %s tank-years",
  fmt_n(analysis_tanks[texas_treated == 1L, uniqueN(panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 1L, uniqueN(tank_panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 0L, uniqueN(panel_id)]),
  fmt_n(analysis_tanks[texas_treated == 0L, uniqueN(tank_panel_id)]),
  fmt_n(nrow(analysis_tanks))))

age_by_cohort <- analysis_tanks[, .(
  n_tanks     = uniqueN(tank_panel_id),
  median_age  = as.integer(median(age_at_treatment))
), by = mm_install_cohort][order(mm_install_cohort)]
log_step("\nAge at treatment by install cohort:")
print(age_by_cohort)
cat("\n")


#### S10 CEM Matching ####

cat("========================================\n")
cat("S10: CEM MATCHING\n")
cat("========================================\n\n")

# One row per tank; match on the four insurer-pricing dimensions:
#   wall type, fuel type, capacity category, install year (mm_install_cohort).
# Using actual install year (not bins) as the fourth cell dimension keeps the
# CEM cells consistent with make_model_tank. CEM coarsens the install year
# automatically within its matching algorithm.

tank_chars <- unique(analysis_tanks[, .(
  tank_panel_id, panel_id, state, texas_treated,
  mm_wall, mm_fuel, mm_capacity, mm_install_cohort, age_at_treatment
)])

# MatchIt requires factor inputs for CEM
tank_chars[, `:=`(
  mm_wall          = as.factor(mm_wall),
  mm_fuel          = as.factor(mm_fuel),
  mm_capacity      = as.factor(mm_capacity),
  mm_install_cohort = as.factor(mm_install_cohort)
)]

m_cem <- matchit(
  texas_treated ~ mm_wall + mm_fuel + mm_capacity + mm_install_cohort,
  data   = as.data.frame(tank_chars),
  method = "cem"
)

cat("CEM balance summary:\n")
print(summary(m_cem, un = FALSE))

tank_chars[, cem_weight := m_cem$weights]
matched_ids <- tank_chars[cem_weight > 0, tank_panel_id]

matched_tanks <- merge(
  analysis_tanks[tank_panel_id %in% matched_ids],
  tank_chars[, .(tank_panel_id, cem_weight)],
  by = "tank_panel_id",
  all.x = TRUE
)

log_step(sprintf("\nMatched TX:  %s / %s tanks (%.1f%%)",
  fmt_n(matched_tanks[texas_treated == 1L, uniqueN(tank_panel_id)]),
  fmt_n(tank_chars[texas_treated == 1L, .N]),
  100 * matched_tanks[texas_treated == 1L, uniqueN(tank_panel_id)] /
        tank_chars[texas_treated == 1L, .N]))

log_step(sprintf("Matched CTL: %s / %s tanks (%.1f%%)",
  fmt_n(matched_tanks[texas_treated == 0L, uniqueN(tank_panel_id)]),
  fmt_n(tank_chars[texas_treated == 0L, .N]),
  100 * matched_tanks[texas_treated == 0L, uniqueN(tank_panel_id)] /
        tank_chars[texas_treated == 0L, .N]))

cem_balance <- as.data.table(summary(m_cem)$sum.all, keep.rownames = "variable")
fwrite(cem_balance, file.path(OUTPUT_TABLES, "Diag_CEM_Balance.csv"))
cat("\n")


#### S11 Post-Match Variable Construction ####

cat("========================================\n")
cat("S11: POST-MATCH VARIABLES\n")
cat("========================================\n\n")

# Median age at treatment: computed on matched sample
med_age <- median(matched_tanks$age_at_treatment, na.rm = TRUE)
cat(sprintf("Median age at treatment (matched sample): %.0f years\n\n", med_age))

matched_tanks[, `:=`(
  above_median_age = as.integer(age_at_treatment >= med_age),
  below_median_age = as.integer(age_at_treatment <  med_age),
  single_wall      = as.integer(mm_wall == "Single-Walled")
)]

matched_tanks[, `:=`(
  did_x_old    = did_term * above_median_age,
  did_x_sw     = did_term * single_wall,
  did_x_sw_old = did_term * single_wall * above_median_age,
  sw_x_old     = single_wall * above_median_age
)]

age_label <- sprintf("Above Median Age (>= %.0f yr)", med_age)

# Control group means for regression table footnotes
ctrl_means <- matched_tanks[texas_treated == 0L, .(
  pre_reform     = mean(closure_event[panel_year %between% c(1992L, 1998L)], na.rm = TRUE),
  post_all       = mean(closure_event[panel_year >= 1999L], na.rm = TRUE),
  post_short     = mean(closure_event[panel_year %between% c(1999L, 2004L)], na.rm = TRUE),
  post_long      = mean(closure_event[panel_year >= 2005L], na.rm = TRUE)
)]

cat("Control closure rates (per 1,000 tank-years):\n")
cat(sprintf("  Pre-reform (1992-1998):  %.1f\n", ctrl_means$pre_reform * 1000))
cat(sprintf("  Post-reform (all):       %.1f\n", ctrl_means$post_all   * 1000))
cat(sprintf("  Post short (1999-2004):  %.1f\n", ctrl_means$post_short * 1000))
cat(sprintf("  Post long  (2005-2020):  %.1f\n", ctrl_means$post_long  * 1000))

ctrl_mean_post <- ctrl_means$post_all

ctrl_means_dt <- data.table(
  period = c("pre_1992_1998", "post_all", "post_1999_2004", "post_2005_2020"),
  rate   = unlist(ctrl_means)
)
fwrite(ctrl_means_dt, file.path(OUTPUT_TABLES, "Diag_ControlMeans.csv"))
cat("\n")


#### S12 Save Outputs as CSV ####

cat("========================================\n")
cat("S12: SAVE OUTPUTS\n")
cat("========================================\n\n")

fwrite(panel_dt,      file.path(ANALYSIS_DIR, "panel_dt.csv"))
fwrite(exact_base,    file.path(ANALYSIS_DIR, "exact_base.csv"))
fwrite(matched_tanks, file.path(ANALYSIS_DIR, "matched_tanks.csv"))

fwrite(
  data.table(tank_panel_id = matched_ids),
  file.path(ANALYSIS_DIR, "matched_ids.csv")
)

fwrite(
  data.table(
    key   = c("med_age", "ctrl_mean_post", "n_ty", "n_tanks",
              "n_tx_tanks", "n_ct_tanks", "n_cells"),
    value = c(med_age, ctrl_mean_post, n_ty, n_tanks,
              n_tx_tanks, n_ct_tanks, n_cells),
    label = c(age_label, rep("", 6))
  ),
  file.path(ANALYSIS_DIR, "panel_meta.csv")
)

cat(sprintf("Saved to %s:\n", ANALYSIS_DIR))
cat("  panel_dt.csv       —", fmt_n(nrow(panel_dt)),      "rows\n")
cat("  exact_base.csv     —", fmt_n(nrow(exact_base)),    "rows\n")
cat("  matched_tanks.csv  —", fmt_n(nrow(matched_tanks)), "rows\n")
cat("  matched_ids.csv    —", fmt_n(length(matched_ids)), "IDs\n")
cat("  panel_meta.csv     — scalar constants\n")

cat("\n========================================\n")
cat("02b_Panel_Build.R COMPLETE\n")
cat("========================================\n\n")
