###############################################################################
# UST_Pipeline_Diagnostics.R
# ==========================
# Standalone diagnostic script for the UST harmonization pipeline.
# Loads raw data, runs all merge and data quality checks, and writes
# a full log to both console and a timestamped text file.
#
# Run this AFTER 04_Master_Build.R has been run at least once so that
# master_tanks and epa_dt exist, OR source this script standalone —
# it will reload everything from scratch.
#
# Output: Diagnostics/UST_Diagnostics_YYYY-MM-DD_HHMMSS.txt
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# 0. Setup
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(stringr)
library(here)
library(lubridate)

# Create output dir
diag_dir <- here("Diagnostics")
if (!dir.exists(diag_dir)) dir.create(diag_dir, recursive = TRUE)

log_path <- file.path(diag_dir, 
  paste0("UST_Diagnostics_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".txt"))

# ── Tee: write to both console and file simultaneously ───────────────────────
log_lines <- character(0)

log <- function(...) {
  msg <- paste0(...)
  message(msg)
  log_lines <<- c(log_lines, msg)
}

section <- function(title) {
  bar <- strrep("=", 79)
  log("")
  log(bar)
  log("  ", title)
  log(bar)
}

subsection <- function(title) {
  log("")
  log(strrep("-", 60))
  log("  ", title)
  log(strrep("-", 60))
}

log("UST PIPELINE DIAGNOSTICS")
log("Run timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log("Project root:  ", here())


# ══════════════════════════════════════════════════════════════════════════════
# 1. Load Data
# ══════════════════════════════════════════════════════════════════════════════

section("1. LOADING RAW DATA")

# ── Harmonized tank files ────────────────────────────────────────────────────
tank_files <- list.files(
  path       = here("Data", "Raw", "state_databases"),
  pattern    = "_Harmonized_UST_tanks.csv$",
  recursive  = TRUE,
  full.names = TRUE
)
log("Tank files found: ", length(tank_files))
for (f in tank_files) log("  ", basename(dirname(f)), "/", basename(f))

lust_files <- list.files(
  path       = here("Data", "Raw", "state_databases"),
  pattern    = "_Harmonized_LUST.csv$",
  recursive  = TRUE,
  full.names = TRUE
)
log("\nLUST files found: ", length(lust_files))
for (f in lust_files) log("  ", basename(dirname(f)), "/", basename(f))

load_state_file <- function(fpath) {
  fread(fpath, colClasses = "character", na.strings = c("", "NA", "NULL"))
}

master_tanks <- rbindlist(lapply(tank_files, load_state_file), fill = TRUE, use.names = TRUE)
master_lust  <- rbindlist(lapply(lust_files, load_state_file), fill = TRUE, use.names = TRUE)

log("\nTotal tank rows loaded:  ", format(nrow(master_tanks), big.mark = ","))
log("Total LUST rows loaded:  ", format(nrow(master_lust),  big.mark = ","))
log("States in master_tanks:  ", paste(sort(unique(master_tanks$state)), collapse = ", "))

# ── Cast numeric columns ─────────────────────────────────────────────────────
cols_numeric <- c("latitude", "longitude", "capacity",
                  "single_walled", "double_walled", "unknown_walled",
                  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")

for (col in cols_numeric) {
  if (!col %in% names(master_tanks)) master_tanks[, (col) := NA_real_]
}
master_tanks[, (cols_numeric) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))),
             .SDcols = cols_numeric]
master_tanks[, tank_installed_date := as.Date(tank_installed_date)]
master_tanks[, tank_closed_date    := as.Date(tank_closed_date)]

master_tanks[, status_std := fcase(
  grepl("Closed|Remov|Aband|Perm", tank_status, ignore.case = TRUE), "Closed",
  default = "Open"
)]

# ── Load EPA file ─────────────────────────────────────────────────────────────
epa_path <- here("Data", "Raw", "Facilities.csv")

if (!file.exists(epa_path)) {
  log("\nERROR: EPA Facilities.csv not found at: ", epa_path)
  log("Skipping all EPA merge diagnostics.")
  epa_loaded <- FALSE
} else {
  raw_headers <- names(fread(epa_path, nrows = 0))
  log("\nEPA file columns: ", paste(raw_headers, collapse = ", "))

  id_col <- grep("facility.*id", raw_headers, ignore.case = TRUE, value = TRUE)[1]
  st_col <- grep("^state$",      raw_headers, ignore.case = TRUE, value = TRUE)[1]
  if (is.na(st_col)) st_col <- grep("state", raw_headers, ignore.case = TRUE, value = TRUE)[1]

  log("EPA ID column detected:    ", id_col)
  log("EPA State column detected: ", st_col)

  epa_dt <- fread(epa_path,
                  select      = c(id_col, st_col, "Latitude", "Longitude"),
                  colClasses  = "character")
  setnames(epa_dt, c("epa_id", "epa_state_full", "lat_epa", "long_epa"))

  state_map <- data.table(epa_state_full = state.name, state_abbr = state.abb)
  epa_dt    <- merge(epa_dt, state_map, by = "epa_state_full", all.x = TRUE)

  log("\nEPA rows loaded:           ", format(nrow(epa_dt), big.mark = ","))
  log("EPA rows with lat/long:    ",
      format(nrow(epa_dt[!is.na(lat_epa) & !is.na(long_epa)]), big.mark = ","))
  epa_loaded <- TRUE
}


# ══════════════════════════════════════════════════════════════════════════════
# 2. Native Coordinate Coverage (Pre-EPA Merge)
# ══════════════════════════════════════════════════════════════════════════════

section("2. NATIVE COORDINATE COVERAGE (pre-EPA merge)")

native_geo <- master_tanks[, .(
  total        = .N,
  has_lat      = sum(!is.na(latitude)),
  pct_has_lat  = round(sum(!is.na(latitude)) / .N * 100, 1),
  miss_lat     = sum(is.na(latitude)),
  pct_miss_lat = round(sum(is.na(latitude)) / .N * 100, 1)
), by = state][order(pct_miss_lat)]

log(capture.output(print(native_geo)))

states_no_native <- native_geo[has_lat == 0, state]
log("\nStates with ZERO native coordinates (fully dependent on EPA backfill):")
log("  ", paste(states_no_native, collapse = ", "))


# ══════════════════════════════════════════════════════════════════════════════
# 3. EPA Merge ID Format Crosscheck
# ══════════════════════════════════════════════════════════════════════════════

section("3. EPA MERGE ID FORMAT CROSSCHECK")

if (epa_loaded) {

  # Build merge IDs exactly as 04_Master_Build.R does
  master_tanks[, merge_id := NA_character_]
  group_prefix <- c("TX", "CO", "LA", "ME", "NJ", "NM", "TN", "AR", "PA")
  master_tanks[state %in% group_prefix, merge_id := paste0(state, facility_id)]
  master_tanks[state == "MI", merge_id := paste0("MI", str_remove(facility_id, "^0+"))]
  master_tanks[state == "AL", merge_id := paste0("AL", as.integer(str_extract(facility_id, "[^-]+$")))]

  epa_lookup <- unique(
    epa_dt[!is.na(lat_epa) & !is.na(long_epa),
           .(merge_id = epa_id,
             lat_epa  = as.numeric(lat_epa),
             long_epa = as.numeric(long_epa))],
    by = "merge_id"
  )

  master_tanks <- merge(master_tanks, epa_lookup, by = "merge_id", all.x = TRUE)
  master_tanks[, latitude_final  := fcoalesce(latitude,  lat_epa)]
  master_tanks[, longitude_final := fcoalesce(longitude, long_epa)]

  # ── Per-state summary ─────────────────────────────────────────────────────
  subsection("3a. Match rates by state (all states with merge logic)")

  diag_states <- c("TX", "CO", "LA", "ME", "NJ", "NM", "TN", "AR", "PA", "MI", "AL")

  epa_diag <- master_tanks[state %in% diag_states, .(
    total            = .N,
    native_lat       = sum(!is.na(latitude)),
    epa_matched      = sum(!is.na(lat_epa)),
    final_lat        = sum(!is.na(latitude_final)),
    pct_final        = round(sum(!is.na(latitude_final)) / .N * 100, 1),
    merge_id_sample  = merge_id[1]
  ), by = state][order(state)]

  log(capture.output(print(epa_diag)))

  epa_id_sample <- epa_dt[state_abbr %in% diag_states,
                           .(epa_id_sample = epa_id[1]), by = state_abbr]
  log("\nEPA ID format samples (for manual crosscheck):")
  log(capture.output(print(epa_id_sample[order(state_abbr)])))

  # ── AL deep-dive ──────────────────────────────────────────────────────────
  subsection("3b. Alabama ID format deep-dive")

  al_master_sample <- master_tanks[state == "AL",
    .(facility_id, merge_id_current = merge_id,
      strip_zeros   = paste0("AL", str_remove(facility_id, "^0+")),
      numeric_only  = paste0("AL", gsub("[^0-9]", "", facility_id)),
      last_segment  = paste0("AL", str_extract(facility_id, "[^-]+$"))
    )][sample(.N, min(15, .N))]

  log("Master AL samples (facility_id and candidate merge IDs):")
  log(capture.output(print(al_master_sample)))

  al_epa_sample <- epa_dt[state_abbr == "AL", .(epa_id)][sample(.N, min(15, .N))]
  log("\nEPA AL ID samples:")
  log(capture.output(print(al_epa_sample)))

  al_epa_ids <- epa_dt[state_abbr == "AL", epa_id]
  al_master  <- master_tanks[state == "AL"]
  n_al       <- nrow(al_master)

  log("\nAL match rate by strategy:")
  al_strategies <- list(
    current      = al_master$merge_id,
    strip_zeros  = paste0("AL", str_remove(al_master$facility_id, "^0+")),
    numeric_only = paste0("AL", gsub("[^0-9]", "", al_master$facility_id)),
    last_segment = paste0("AL", str_extract(al_master$facility_id, "[^-]+$")),
    raw_prefix   = paste0("AL", al_master$facility_id)
  )
  for (nm in names(al_strategies)) {
    n_match <- length(intersect(al_strategies[[nm]], al_epa_ids))
    log(sprintf("  %-20s : %6d / %6d  (%5.1f%%)", nm, n_match, n_al, n_match / n_al * 100))
  }

  # ── PA deep-dive ──────────────────────────────────────────────────────────
  subsection("3c. Pennsylvania ID format deep-dive")

  pa_master_sample <- master_tanks[state == "PA",
    .(facility_id, merge_id_current = merge_id,
      strip_zeros   = paste0("PA", str_remove(facility_id, "^0+")),
      numeric_only  = paste0("PA", gsub("[^0-9]", "", facility_id)),
      last_segment  = paste0("PA", str_extract(facility_id, "[^-]+$")),
      first_segment = paste0("PA", str_extract(facility_id, "^[^-]+"))
    )][sample(.N, min(15, .N))]

  log("Master PA samples:")
  log(capture.output(print(pa_master_sample)))

  pa_epa_sample <- epa_dt[state_abbr == "PA", .(epa_id)][sample(.N, min(15, .N))]
  log("\nEPA PA ID samples:")
  log(capture.output(print(pa_epa_sample)))

  pa_epa_ids <- epa_dt[state_abbr == "PA", epa_id]
  pa_master  <- master_tanks[state == "PA"]
  n_pa       <- nrow(pa_master)

  log("\nPA match rate by strategy:")
  pa_strategies <- list(
    current       = pa_master$merge_id,
    strip_zeros   = paste0("PA", str_remove(pa_master$facility_id, "^0+")),
    numeric_only  = paste0("PA", gsub("[^0-9]", "", pa_master$facility_id)),
    last_segment  = paste0("PA", str_extract(pa_master$facility_id, "[^-]+$")),
    first_segment = paste0("PA", str_extract(pa_master$facility_id, "^[^-]+")),
    raw_prefix    = paste0("PA", pa_master$facility_id)
  )
  for (nm in names(pa_strategies)) {
    n_match <- length(intersect(pa_strategies[[nm]], pa_epa_ids))
    log(sprintf("  %-20s : %6d / %6d  (%5.1f%%)", nm, n_match, n_pa, n_match / n_pa * 100))
  }

  master_tanks[, c("merge_id", "lat_epa", "long_epa") := NULL]

} else {
  master_tanks[, latitude_final  := latitude]
  master_tanks[, longitude_final := longitude]
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. LUST Report Date Diagnostics
# ══════════════════════════════════════════════════════════════════════════════

section("4. LUST REPORT DATE DIAGNOSTICS")

for (st in sort(unique(master_lust$state))) {
  sub <- master_lust[state == st & !is.na(report_date) & report_date != ""]

  if (nrow(sub) == 0) {
    log(st, ": No non-empty report_date values to sample")
    next
  }

  sample_vals  <- head(sub$report_date, 5)
  n_total      <- nrow(master_lust[state == st])
  n_nonempty   <- nrow(sub)

  # Try parsing with the full order set
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      sample_vals,
      orders = c("dby", "dBY", "dmy", "dmY", "mdy", "ymd", "mdY", "Ymd"),
      quiet  = TRUE
    )
  )

  parse_ok <- sum(!is.na(parsed))

  # Full parse attempt on all non-empty values
  all_parsed <- suppressWarnings(
    lubridate::parse_date_time(
      sub$report_date,
      orders = c("dby", "dBY", "dmy", "dmY", "mdy", "ymd", "mdY", "Ymd"),
      quiet  = TRUE
    )
  )
  pct_parseable <- round(sum(!is.na(all_parsed)) / n_total * 100, 1)

  log(sprintf(
    "%-4s  total=%-6d  non_empty=%-6d  sample_parse=%d/5  full_pct_parseable=%5.1f%%  sample_vals=[%s]",
    st, n_total, n_nonempty, parse_ok, pct_parseable,
    paste(sample_vals, collapse = " | ")
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
# 5. Tank Status & Closed Date Logic Check
# ══════════════════════════════════════════════════════════════════════════════

section("5. TANK STATUS & CLOSED DATE DIAGNOSTICS")

subsection("5a. Raw tank_status values by state (top 5 per state)")

for (st in sort(unique(master_tanks$state))) {
  top_status <- master_tanks[state == st, .N, by = tank_status][order(-N)][1:min(5, .N)]
  log(st, ": ", paste(
    apply(top_status, 1, function(r) paste0('"', r[1], '"=', r[2])),
    collapse = ", "
  ))
}

subsection("5b. status_std mapping coverage by state")

std_check <- master_tanks[, .(
  total       = .N,
  n_closed    = sum(status_std == "Closed", na.rm = TRUE),
  n_open      = sum(status_std == "Open",   na.rm = TRUE),
  n_na_status = sum(is.na(tank_status) | tank_status == ""),
  pct_closed_missing_closed_date = {
    nc <- sum(status_std == "Closed", na.rm = TRUE)
    if (nc > 0) round(sum(status_std == "Closed" & is.na(tank_closed_date)) / nc * 100, 1)
    else NA_real_
  }
), by = state][order(state)]

log(capture.output(print(std_check)))


# ══════════════════════════════════════════════════════════════════════════════
# 6. Substance Flag Diagnostics
# ══════════════════════════════════════════════════════════════════════════════

section("6. SUBSTANCE FLAG DIAGNOSTICS")

subst_check <- master_tanks[, .(
  total        = .N,
  all_zero_na  = sum(
    (is.na(is_gasoline)     | is_gasoline     == 0) &
    (is.na(is_diesel)       | is_diesel       == 0) &
    (is.na(is_oil_kerosene) | is_oil_kerosene == 0) &
    (is.na(is_jet_fuel)     | is_jet_fuel     == 0) &
    (is.na(is_other)        | is_other        == 0)
  ),
  all_na_only  = sum(
    is.na(is_gasoline) & is.na(is_diesel) &
    is.na(is_oil_kerosene) & is.na(is_jet_fuel) & is.na(is_other)
  ),
  pct_no_subst = round(sum(
    (is.na(is_gasoline)     | is_gasoline     == 0) &
    (is.na(is_diesel)       | is_diesel       == 0) &
    (is.na(is_oil_kerosene) | is_oil_kerosene == 0) &
    (is.na(is_jet_fuel)     | is_jet_fuel     == 0) &
    (is.na(is_other)        | is_other        == 0)
  ) / .N * 100, 1)
), by = state][order(state)]

log(capture.output(print(subst_check)))


# ══════════════════════════════════════════════════════════════════════════════
# 7. Final Coverage Summary
# ══════════════════════════════════════════════════════════════════════════════

section("7. FINAL COORDINATE COVERAGE (post-EPA merge)")

final_geo <- master_tanks[, .(
  total         = .N,
  final_lat     = sum(!is.na(latitude_final)),
  pct_final_lat = round(sum(!is.na(latitude_final)) / .N * 100, 1),
  miss_lat      = sum(is.na(latitude_final)),
  pct_miss_lat  = round(sum(is.na(latitude_final)) / .N * 100, 1)
), by = state][order(pct_miss_lat)]

log(capture.output(print(final_geo)))


# ══════════════════════════════════════════════════════════════════════════════
# 8. Write Log File
# ══════════════════════════════════════════════════════════════════════════════

section("8. SAVING LOG")

writeLines(log_lines, log_path)
message("\nDiagnostic log saved to:")
message("  ", log_path)