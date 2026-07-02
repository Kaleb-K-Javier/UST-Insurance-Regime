################################################################################
# 02b_PA_Append.R
# Ticket 049 -- standalone PA append onto the existing panel (no full 02b re-run)
#
# WHAT THIS DOES: loads Data/Analysis/panel_dt.csv + facility_panel.csv AS-IS
# (study-state rows are never recomputed), builds PA (PRICING/HAZARD-eligible
# only, NOT part of the closure-DiD/structural sample) from master_tanks
# (Data/Processed/Master_Harmonized_UST_Tanks.csv) + the PA-specific LUST
# source (Data/Raw/state_databases/Pennsylvania/PA_Harmonized_LUST.csv,
# written by 14_Clean_PA.r -- NOT Master_Harmonized_LUST.csv, whose PA rows
# are downstream of the same source and inherit its bugs; see the T046.0
# comment below), unions PA onto the loaded panels, and OVERWRITES the
# canonical panel_dt.csv / facility_panel.csv.
#
# RUN ORDER (D5): 02b_Tank_level_Panel_Build.R (rare, base-data changes) THEN
# this script (fast, adds PA). This script's output OVERWRITES the canonical
# panel; a future full 02b run drops PA until this script is re-run again.
#
# SCOPE (D1, ticket 049): PA ONLY. NOT NM -- NM's unknown-wall build needs
# net-new design (cell-membership rules, make_model formation without a known
# wall) that has to be decided jointly with the 01n model-side changes; that
# is its own ticket. The idempotency guard below defensively also drops any
# pre-existing NM rows so it stays correct once NM does land, but this script
# builds PA rows only.
#
# Supersedes ticket 046's in-02b T046 block (commit a4014ce, branch
# origin/pa-046) -- same PA logic (flags, non-destructive, real PA leaks,
# type-safe union), lifted here as a standalone append.
################################################################################

SCRIPT_NAME <- "02b_PA_Append"

.log_path <- here::here("logs", paste0(SCRIPT_NAME, "_",
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output")
sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, SCRIPT_NAME, R.version.string, getwd()))

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

options(scipen = 999)
setDTthreads(14)

OUTPUT_TABLES <- here("Output", "Tables")
ANALYSIS_DIR  <- here("Data", "Analysis")
dir.create(OUTPUT_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(ANALYSIS_DIR,  recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters (mirrors 02b_Tank_level_Panel_Build.R S1) ----
POST_YEAR   <- 1999L
REFORM_DATE <- as.IDate("1998-12-22")
PANEL_START <- 1985L
PANEL_END   <- 2020L

RELEASE_DET_DEADLINE_YR <- list(
  pre_1965 = 1989L,
  y1965_69 = 1990L,
  y1970_74 = 1991L,
  y1975_79 = 1992L,
  y1980_88 = 1993L
)

# States built by this script (D1: PA only).
PRICING_STATES <- c("PA")
# States defensively dropped by the idempotency guard (D1 note: NM is not
# built here, but the guard stays correct once it is built elsewhere).
GUARD_DROP_STATES <- c("PA", "NM")

# ---- Utility functions ----
log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)

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

# Bug-B fix: coerce every shared PA column to the loaded main panel's class
# BEFORE assert_cols_match/rbindlist, so real-world type drift between an
# on-disk main panel and a freshly-built PA panel doesn't hard-stop the union
# on a coercible difference (e.g. integer vs numeric).
coerce_shared_cols <- function(main_dt, pa_dt) {
  shared <- intersect(names(main_dt), names(pa_dt))
  for (col in shared) {
    cls <- class(main_dt[[col]])[1]
    if (class(pa_dt[[col]])[1] != cls) {
      set(pa_dt, j = col, value = as(pa_dt[[col]], cls))
    }
  }
  invisible(pa_dt)
}

# Idempotency guard: drop any pre-existing PA (and defensively NM) rows from
# a loaded main panel before appending, so re-running this script never
# double-appends. Handles the flag column not existing yet (true on a panel
# that predates this script's first run).
guard_drop_prior_rows <- function(dt, dt_name) {
  has_role  <- "panel_state_role" %in% names(dt)
  drop_idx  <- dt$state %in% GUARD_DROP_STATES
  if (has_role) {
    drop_idx <- drop_idx | (!is.na(dt$panel_state_role) & dt$panel_state_role == "pricing_only")
  }
  n_drop <- sum(drop_idx)
  log_step(sprintf("%s: dropping %s pre-existing PA/NM rows (idempotency guard)",
    dt_name, fmt_n(n_drop)))
  dt[!drop_idx]
}

# Geographic/continuous columns carried into the tank-year panel for
# downstream facility aggregation (mirrors 02b S4's panel_cols).
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


#### STEP 1: LOAD EXISTING PANELS ####

cat("========================================\n")
cat("STEP 1: LOAD EXISTING PANELS\n")
cat("========================================\n\n")

panel_dt_path       <- file.path(ANALYSIS_DIR, "panel_dt.csv")
facility_panel_path <- file.path(ANALYSIS_DIR, "facility_panel.csv")

panel_dt_main <- fread(panel_dt_path, na.strings = c("", "NA", "N/A"))
log_step(sprintf("Loaded panel_dt.csv: %s rows, %d cols",
  fmt_n(nrow(panel_dt_main)), ncol(panel_dt_main)))

facility_panel_main <- fread(facility_panel_path, na.strings = c("", "NA", "N/A"))
log_step(sprintf("Loaded facility_panel.csv: %s rows, %d cols",
  fmt_n(nrow(facility_panel_main)), ncol(facility_panel_main)))

# Snapshot pre-PA baseline copies for the D-ACCEPT diff. Write-once: only
# if not already present, so a re-run never overwrites the true pre-PA
# baseline with an already-appended panel.
pre_pa_dir <- file.path(ANALYSIS_DIR, "_pre_046_PA")
dir.create(pre_pa_dir, recursive = TRUE, showWarnings = FALSE)
panel_dt_prepa_path       <- file.path(pre_pa_dir, "panel_dt_prePA.csv")
facility_panel_prepa_path <- file.path(pre_pa_dir, "facility_panel_prePA.csv")

if (!file.exists(panel_dt_prepa_path)) {
  file.copy(panel_dt_path, panel_dt_prepa_path, overwrite = FALSE)
  log_step(sprintf("Backed up pre-PA baseline: %s", panel_dt_prepa_path))
} else {
  log_step(sprintf("Pre-PA baseline already present, not overwriting: %s", panel_dt_prepa_path))
}
if (!file.exists(facility_panel_prepa_path)) {
  file.copy(facility_panel_path, facility_panel_prepa_path, overwrite = FALSE)
  log_step(sprintf("Backed up pre-PA baseline: %s", facility_panel_prepa_path))
} else {
  log_step(sprintf("Pre-PA baseline already present, not overwriting: %s", facility_panel_prepa_path))
}
cat("\n")


#### STEP 2: BUILD PA (lifts T046.1-T046.13 from commit a4014ce) ####

cat("========================================\n")
cat("STEP 2: BUILD PA ROWS\n")
cat("========================================\n\n")

#### T046.0 PA tank source setup (mirrors 02b S3, general preprocessing only) ####
cat("T046.0: Load master_tanks...\n")

master_tanks <- fread(
  here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"),
  na.strings = c("", "NA", "N/A")
)
master_tanks[, tank_installed_date := as.IDate(tank_installed_date)]
master_tanks[, tank_closed_date    := as.IDate(tank_closed_date)]
master_tanks[, install_yr_int      := as.integer(mm_install_cohort)]
master_tanks[, panel_id            := paste(facility_id, state, sep = "_")]
master_tanks[, tank_panel_id       := paste(facility_id, state, tank_id, sep = "_")]

log_step(sprintf("Loaded master_tanks: %s rows, %d cols",
  fmt_n(nrow(master_tanks)), ncol(master_tanks)))

# PA leak source: Data/Raw/state_databases/Pennsylvania/PA_Harmonized_LUST.csv
# (written by 14_Clean_PA.r) -- NOT Master_Harmonized_LUST.csv. The master
# file's PA rows are downstream of this same source and were, before the
# safe_parse_date() epoch-ms fix in 14_Clean_PA.r, entirely date-less (every
# PA LUST record undated because Releases.csv's Reported_Date is epoch-ms and
# was being run through excel_numeric_to_date() -- see 14_Clean_PA.r). Read
# here, before any STUDY_STATES filter exists anywhere in this script (this
# script never constructs STUDY_STATES at all -- Bug-A fix).
# facility_id here is the PA DEP permit format ("XX-XXXXX"), which matches
# master_tanks' PA facility_id directly -- no crosswalk needed.
cat("T046.0: Load PA LUST source (PA_Harmonized_LUST.csv)...\n")

pa_lust_raw <- fread(
  here("Data", "Raw", "state_databases", "Pennsylvania", "PA_Harmonized_LUST.csv"),
  na.strings = c("", "NA", "N/A")
)
pa_lust_raw[, `:=`(
  facility_id = toupper(trimws(as.character(facility_id))),
  state       = toupper(trimws(as.character(state)))
)]
pa_lust_raw[, panel_id    := paste(facility_id, state, sep = "_")]
pa_lust_raw[, report_date := as.IDate(report_date)]

log_step(sprintf("Loaded PA LUST source: %s rows, %d cols",
  fmt_n(nrow(pa_lust_raw)), ncol(pa_lust_raw)))
cat("\n")

#### T046.1 PA tank source (MIRRORS S3) ####
cat("T046.1: PA tank source...\n")

pa_tanks <- master_tanks[state %in% PRICING_STATES & !is.na(tank_installed_date)]
# texas_treated is a formula-safe input only (did_term = texas_treated *
# post_1999), NOT a DiD-eligibility signal. PA is quarantined from the
# closure-DiD/structural sample by state %in% STUDY_STATES (false for PA,
# since STUDY_STATES is never even constructed in this script) and by
# closure_did_eligible == 0 below -- never by this value.
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
# Real leak/hazard signal for PA -- researcher instruction (per ticket 046):
# 01n's never-leaked risk set needs it, so it is NOT left NA/0 like the fee.
# pa_lust_raw loaded fresh in T046.0 above from the PA-specific source (not
# Master_Harmonized_LUST.csv -- see T046.0 comment), filtered to
# PRICING_STATES defensively (the source file is PA-only already).
cat("T046.12: PA leak history...\n")

pa_leaks_fac_yr <- pa_lust_raw[
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

n_pa_leak_fac_years <- pa_fac_stock[leak_year == 1L, .N]
log_step(sprintf("PA LUST: %s fac-years with leaks | %s reports | %s incidents",
  fmt_n(n_pa_leak_fac_years),
  fmt_n(pa_fac_stock[, sum(n_leak_reports)]),
  fmt_n(pa_fac_stock[, sum(n_leak_incidents)])))
if (n_pa_leak_fac_years == 0L) {
  stop("PA leak join produced 0 leak fac-years. Check that Data/Raw/state_databases/Pennsylvania/PA_Harmonized_LUST.csv has populated report_date values -- this requires 14_Clean_PA.r's safe_parse_date() epoch-ms fix to have been applied AND 14_Clean_PA.r re-run on the server to regenerate that file. STOP.")
}

pa_closures_for_lust <- pa_tanks[
  !is.na(tank_closed_date),
  .(panel_id, closure_date = tank_closed_date, tank_install_date = tank_installed_date)
]
pa_leaks_for_lust <- pa_lust_raw[
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
# non-TX default convention used elsewhere for the TX-only FR columns
# (zeros/None), and leave the fee itself NA + flagged per D4 -- do not
# invent the PA USTIF fee.
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


#### STEP 3: IDEMPOTENCY GUARD ####

cat("========================================\n")
cat("STEP 3: IDEMPOTENCY GUARD\n")
cat("========================================\n\n")

panel_dt_main       <- guard_drop_prior_rows(panel_dt_main,       "panel_dt_main")
facility_panel_main <- guard_drop_prior_rows(facility_panel_main, "facility_panel_main")
cat("\n")


#### STEP 4: ELIGIBILITY FLAGS ####

cat("========================================\n")
cat("STEP 4: ELIGIBILITY FLAGS\n")
cat("========================================\n\n")

panel_dt_main[, `:=`(
  panel_state_role     = "study",
  closure_did_eligible = 1L,
  hazard_eligible       = 1L
)]
pa_panel_dt[, `:=`(
  panel_state_role     = "pricing_only",
  closure_did_eligible = 0L,
  hazard_eligible       = 1L
)]

facility_panel_main[, `:=`(
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

log_step("Flags set: study rows (study, 1, 1); PA rows (pricing_only, 0, 1).")
cat("\n")


#### STEP 5: TYPE-SAFE UNION (Bug-B fix) ####

cat("========================================\n")
cat("STEP 5: TYPE-SAFE UNION\n")
cat("========================================\n\n")

coerce_shared_cols(panel_dt_main, pa_panel_dt)
assert_cols_match(panel_dt_main, pa_panel_dt, "panel_dt_main", "pa_panel_dt")
panel_dt_final <- rbindlist(list(panel_dt_main, pa_panel_dt), use.names = TRUE)
log_step(sprintf("panel_dt: %s study rows + %s PA rows = %s total",
  fmt_n(nrow(panel_dt_main)), fmt_n(nrow(pa_panel_dt)), fmt_n(nrow(panel_dt_final))))

coerce_shared_cols(facility_panel_main, pa_facility_panel)
assert_cols_match(facility_panel_main, pa_facility_panel, "facility_panel_main", "pa_facility_panel")
facility_panel_final <- rbindlist(list(facility_panel_main, pa_facility_panel), use.names = TRUE)
log_step(sprintf("facility_panel: %s study rows + %s PA rows = %s total",
  fmt_n(nrow(facility_panel_main)), fmt_n(nrow(pa_facility_panel)), fmt_n(nrow(facility_panel_final))))
cat("\n")


#### STEP 6: WRITE + VERIFY ####

cat("========================================\n")
cat("STEP 6: WRITE + VERIFY\n")
cat("========================================\n\n")

fwrite(panel_dt_final,       panel_dt_path)
fwrite(facility_panel_final, facility_panel_path)
log_step(sprintf("Wrote: %s", panel_dt_path))
log_step(sprintf("Wrote: %s", facility_panel_path))

# ---- D-ACCEPT: non-PA rows identical to the pre-PA baseline ----
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
log_step("D-ACCEPT PASSED: all non-PA states identical to pre-PA baseline (or first run, no baseline).")
cat("\n")


#### SUMMARY ####

cat("========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

log_step(sprintf("PA tank-years added: %s", fmt_n(nrow(pa_panel_dt))))
log_step(sprintf("PA facility-years added: %s", fmt_n(nrow(pa_facility_panel))))
log_step(sprintf("PA leak fac-years: %s | leak reports: %s | leak incidents: %s",
  fmt_n(pa_facility_panel[leak_year == 1L, .N]),
  fmt_n(pa_facility_panel[, sum(n_leak_reports)]),
  fmt_n(pa_facility_panel[, sum(n_leak_incidents)])))

log_step("Final panel_dt row counts by state:")
print(panel_dt_final[, .N, by = state][order(state)])

log_step("Final facility_panel row counts by state:")
print(facility_panel_final[, .N, by = state][order(state)])

cat(sprintf("\n02b_PA_Append.R complete. Log: %s\n", .log_path))
