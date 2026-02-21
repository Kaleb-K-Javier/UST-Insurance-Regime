## UST Raw Data Diagnostic Script
## Purpose: Load all raw inputs, inspect structure/IDs, verify EPA merge readiness
## Covers: EPA national files, all custom-state raw files, coordinate diagnostics
## Also documents/fixes the calc_missing() lat_col scoping bug

# 0. Setup ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)
library(DBI)
library(odbc)

log_file <- here("Diagnostics",
                 paste0("UST_Raw_Diagnostic_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".txt"))
if (!dir.exists(here("Diagnostics"))) dir.create(here("Diagnostics"), recursive = TRUE)

con_log <- file(log_file, open = "wt")

# Helper: write to console AND file
log <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = con_log)
}
logsep <- function(title = "") {
  line <- paste0(rep("=", 79), collapse = "")
  log(line)
  if (nchar(title) > 0) { log("  ", title); log(line) }
}
subsection <- function(title) {
  line <- paste0(rep("-", 60), collapse = "")
  log(line)
  log("  ", title)
  log(line)
}

peek <- function(dt, label, n = 5) {
  log("\n[HEAD ", n, " rows — ", label, "]")
  log(capture.output(print(head(dt, n))))
  log("\n[GLIMPSE — ", label, "]")
  log(capture.output(dplyr::glimpse(as.data.frame(dt))))
}

logsep("UST RAW DATA DIAGNOSTIC")
log("Run timestamp: ", format(Sys.time()))
log("Project root:  ", here())

# ============================================================
#  SECTION 1: EPA NATIONAL FILES
# ============================================================
logsep("1. EPA NATIONAL FILES (USTs.csv, Facilities.csv, Releases.csv)")

# --- 1a. Facilities.csv (the coordinate source) ---
subsection("1a. Facilities.csv")
fac_path <- here("Data", "Raw", "Facilities.csv")
log("Path: ", fac_path, " | Exists: ", file.exists(fac_path))

if (file.exists(fac_path)) {
  epa_fac <- fread(fac_path, colClasses = "character") %>% clean_names() %>% as.data.table()
  log("Rows: ", nrow(epa_fac), " | Cols: ", ncol(epa_fac))
  log("Column names: ", paste(names(epa_fac), collapse = ", "))
  peek(epa_fac, "Facilities.csv")

  # Coordinate coverage
  epa_fac[, lat_num := suppressWarnings(as.numeric(latitude))]
  epa_fac[, lon_num := suppressWarnings(as.numeric(longitude))]
  log("\nCoordinate diagnostics:")
  log("  Rows with non-NA lat:    ", sum(!is.na(epa_fac$lat_num)))
  log("  Rows with lat > 90:      ", sum(epa_fac$lat_num > 90, na.rm = TRUE), " (likely Mercator)")
  log("  Rows with lat < -90:     ", sum(epa_fac$lat_num < -90, na.rm = TRUE))
  log("  Rows with lat in [-90,90]: ", sum(between(epa_fac$lat_num, -90, 90), na.rm = TRUE))
  log("  Lat range: ", min(epa_fac$lat_num, na.rm=T), " to ", max(epa_fac$lat_num, na.rm=T))
  log("  Lon range: ", min(epa_fac$lon_num, na.rm=T), " to ", max(epa_fac$lon_num, na.rm=T))

  # ID format by state
  log("\nFacility_ID format samples by state (10 states):")
  sample_states <- c("Alabama", "Arkansas", "Louisiana", "Maine", "Michigan",
                     "New Jersey", "New Mexico", "Pennsylvania", "Tennessee", "Texas")
  for (st in sample_states) {
    ids <- epa_fac[state == st, facility_id][1:min(5, sum(epa_fac$state == st))]
    log(sprintf("  %-15s: %s", st, paste(ids, collapse = " | ")))
  }

  # State coverage
  log("\nEPA Facilities rows by state (top 20):")
  st_counts <- epa_fac[, .N, by = state][order(-N)][1:20]
  log(capture.output(print(st_counts)))
}

# --- 1b. USTs.csv ---
subsection("1b. USTs.csv")
ust_path <- here("Data", "Raw", "USTs.csv")
log("Path: ", ust_path, " | Exists: ", file.exists(ust_path))

if (file.exists(ust_path)) {
  epa_usts <- fread(ust_path, colClasses = "character") %>% clean_names() %>% as.data.table()
  log("Rows: ", nrow(epa_usts), " | Cols: ", ncol(epa_usts))
  log("Column names: ", paste(names(epa_usts), collapse = ", "))
  peek(epa_usts, "USTs.csv")

  # Check join key
  log("\nfacility_id overlap with Facilities.csv: ",
      length(intersect(epa_usts$facility_id, epa_fac$facility_id)),
      " / ", length(unique(epa_usts$facility_id)), " unique UST facility_ids")

  # Status distribution
  if ("tank_status" %in% names(epa_usts)) {
    log("\nTank status distribution (top 10):")
    log(capture.output(print(epa_usts[, .N, by = tank_status][order(-N)][1:10])))
  }

  # Date columns — check formats
  date_cols <- grep("date|installed|removal", names(epa_usts), value = TRUE, ignore.case = TRUE)
  log("\nDate-like columns: ", paste(date_cols, collapse = ", "))
  for (dc in date_cols[1:min(3, length(date_cols))]) {
    sample_vals <- epa_usts[!is.na(get(dc)) & get(dc) != "", get(dc)][1:5]
    log(sprintf("  %s sample: %s", dc, paste(sample_vals, collapse = " | ")))
  }
}

# --- 1c. Releases.csv (LUST) ---
subsection("1c. Releases.csv (LUST)")
rel_path <- here("Data", "Raw", "Releases.csv")
log("Path: ", rel_path, " | Exists: ", file.exists(rel_path))

if (file.exists(rel_path)) {
  epa_rel <- fread(rel_path, colClasses = "character") %>% clean_names() %>% as.data.table()
  log("Rows: ", nrow(epa_rel), " | Cols: ", ncol(epa_rel))
  log("Column names: ", paste(names(epa_rel), collapse = ", "))
  peek(epa_rel, "Releases.csv")

  # Date format check
  date_cols_r <- grep("date|reported", names(epa_rel), value = TRUE, ignore.case = TRUE)
  for (dc in date_cols_r[1:min(3, length(date_cols_r))]) {
    sample_vals <- epa_rel[!is.na(get(dc)) & get(dc) != "", get(dc)][1:5]
    log(sprintf("  %s sample: %s", dc, paste(sample_vals, collapse = " | ")))
  }

  log("\nReleases by state (top 15):")
  log(capture.output(print(epa_rel[, .N, by = state][order(-N)][1:15])))
}

# ============================================================
#  SECTION 2: CUSTOM STATE RAW FILES
# ============================================================
logsep("2. CUSTOM STATE RAW FILES")

# Helper to safely peek a file
safe_peek_csv <- function(path, label) {
  if (file.exists(path)) {
    dt <- tryCatch(fread(path, nrows = 5, colClasses = "character"), error = function(e) NULL)
    if (!is.null(dt)) peek(dt, label)
  } else {
    log("  MISSING: ", path)
  }
}

# --- 2a. Alabama ---
subsection("2a. Alabama")
al_path <- here("Data", "Raw", "state_databases", "Alabama")
log("AL directory: ", al_path)

if (dir.exists(al_path)) {
  log("Files found: ", paste(list.files(al_path), collapse = ", "))

  # Tanks Excel
  tank_xl <- file.path(al_path, "UST_UTanks (2).xlsx")
  if (file.exists(tank_xl)) {
    al_tanks_raw <- read_excel(tank_xl, col_types = "text", n_max = 5) %>% clean_names() %>% as.data.table()
    peek(al_tanks_raw, "AL UST_UTanks.xlsx")
    # Show permit number format
    all_tanks <- read_excel(tank_xl, col_types = "text") %>% clean_names() %>% as.data.table()
    log("\nAL permit_number samples (first 10):")
    log(capture.output(print(all_tanks[1:10, .(permit_number)])))
    log("\nAL facility_id (from harmonized) vs EPA format:")
    # The harmonized file uses permit_number as facility_id
    log("  Harmonized facility_id format: ", all_tanks$permit_number[1:5])
    log("  EPA AL ID format (from Facilities.csv): ", 
        if(exists("epa_fac")) epa_fac[state == "Alabama", facility_id][1:5] else "epa_fac not loaded")
  }

  # Sites Excel
  sites_xl <- file.path(al_path, "UST_Sites (2).xlsx")
  if (file.exists(sites_xl)) {
    al_sites_raw <- read_excel(sites_xl, col_types = "text", n_max = 5) %>% clean_names() %>% as.data.table()
    peek(al_sites_raw, "AL UST_Sites.xlsx")
  }
}

# --- 2b. Arkansas ---
subsection("2b. Arkansas")
ar_path <- here("Data", "Raw", "state_databases", "Arkansas")
log("AR files: ", paste(list.files(ar_path), collapse = ", "))

# Harmonized output
safe_peek_csv(file.path(ar_path, "AR_Harmonized_UST_tanks.csv"), "AR Harmonized Tanks")
safe_peek_csv(file.path(ar_path, "AR_Harmonized_LUST.csv"), "AR Harmonized LUST")

# Check report_date after fix
ar_lust_check <- tryCatch(fread(file.path(ar_path, "AR_Harmonized_LUST.csv"), colClasses="character"), error=function(e) NULL)
if (!is.null(ar_lust_check)) {
  log("\nAR LUST report_date sample (checking ISO fix worked):")
  log(capture.output(print(ar_lust_check[1:10, .(facility_id, report_date)])))
  n_parseable <- sum(!is.na(as.Date(ar_lust_check$report_date)), na.rm=TRUE)
  log(sprintf("AR LUST parseable dates: %d / %d (%.1f%%)", n_parseable, nrow(ar_lust_check), n_parseable/nrow(ar_lust_check)*100))
}

# --- 2c. Louisiana ---
subsection("2c. Louisiana")
la_path <- here("Data", "Raw", "state_databases", "Louisiana")
log("LA files: ", paste(list.files(la_path), collapse = ", "))
safe_peek_csv(file.path(la_path, "LA_Harmonized_UST_tanks.csv"), "LA Harmonized Tanks")

# --- 2d. Maine ---
subsection("2d. Maine")
me_path <- here("Data", "Raw", "state_databases", "Maine")
log("ME files: ", paste(list.files(me_path), collapse = ", "))
safe_peek_csv(file.path(me_path, "ME_Harmonized_UST_tanks.csv"), "ME Harmonized Tanks")

# --- 2e. Michigan ---
subsection("2e. Michigan")
mi_path <- here("Data", "Raw", "state_databases", "Michigan")
log("MI files: ", paste(list.files(mi_path), collapse = ", "))
safe_peek_csv(file.path(mi_path, "MI_Harmonized_UST_tanks.csv"), "MI Harmonized Tanks")

# Check MI facility_id format
mi_tanks <- tryCatch(fread(file.path(mi_path, "MI_Harmonized_UST_tanks.csv"), nrows=20, colClasses="character"), error=function(e) NULL)
if (!is.null(mi_tanks)) {
  log("\nMI facility_id samples (10):")
  log(capture.output(print(mi_tanks[1:10, .(facility_id)])))
  log("EPA MI ID samples:")
  if (exists("epa_fac")) log(capture.output(print(epa_fac[state=="Michigan", .(facility_id)][1:10])))
}

# --- 2f. New Jersey ---
subsection("2f. New Jersey")
nj_path <- here("Data", "Raw", "state_databases", "New Jersey")
log("NJ files: ", paste(list.files(nj_path), collapse = ", "))
safe_peek_csv(file.path(nj_path, "NJ_Harmonized_UST_tanks.csv"), "NJ Harmonized Tanks")

# --- 2g. New Mexico ---
subsection("2g. New Mexico")
nm_path <- here("Data", "Raw", "state_databases", "New Mexico")
log("NM files: ", paste(list.files(nm_path), collapse = ", "))
safe_peek_csv(file.path(nm_path, "NM_Harmonized_UST_tanks.csv"), "NM Harmonized Tanks")

# --- 2h. Oklahoma ---
subsection("2h. Oklahoma")
ok_path <- here("Data", "Raw", "state_databases", "Oklahoma")
log("OK files: ", paste(list.files(ok_path), collapse = ", "))
safe_peek_csv(file.path(ok_path, "OK_Harmonized_UST_tanks.csv"), "OK Harmonized Tanks")

# --- 2i. Pennsylvania ---
subsection("2i. Pennsylvania")
pa_path <- here("Data", "Raw", "state_databases", "Pennsylvania")
log("PA files: ", paste(list.files(pa_path), collapse = ", "))
safe_peek_csv(file.path(pa_path, "PA_Harmonized_UST_tanks.csv"), "PA Harmonized Tanks")

# Check epa_key column
pa_tanks <- tryCatch(fread(file.path(pa_path, "PA_Harmonized_UST_tanks.csv"), nrows=20, colClasses="character"), error=function(e) NULL)
if (!is.null(pa_tanks)) {
  log("\nPA column names: ", paste(names(pa_tanks), collapse=", "))
  log("PA epa_key present: ", "epa_key" %in% names(pa_tanks))
  if ("epa_key" %in% names(pa_tanks)) {
    log("PA epa_key samples:")
    log(capture.output(print(pa_tanks[1:10, .(facility_id, epa_key)])))
    # Full file check
    pa_full <- tryCatch(fread(file.path(pa_path, "PA_Harmonized_UST_tanks.csv"), colClasses="character"), error=function(e) NULL)
    if (!is.null(pa_full)) {
      log(sprintf("PA epa_key populated: %d / %d (%.1f%%)", sum(!is.na(pa_full$epa_key)), nrow(pa_full), sum(!is.na(pa_full$epa_key))/nrow(pa_full)*100))
      # Cross-check against EPA file
      if (exists("epa_fac")) {
        pa_epa_ids <- epa_fac[state == "Pennsylvania", facility_id]
        n_match <- length(intersect(pa_full$epa_key, pa_epa_ids))
        log(sprintf("PA epa_key -> EPA Facilities matches: %d / %d (%.1f%%)", n_match, nrow(pa_full), n_match/nrow(pa_full)*100))
        log("EPA PA ID samples: ", paste(pa_epa_ids[1:8], collapse=" | "))
        log("PA epa_key samples: ", paste(pa_full$epa_key[!is.na(pa_full$epa_key)][1:8], collapse=" | "))
      }
    }
  }
}

# Check PA linkage table
pa_link_path <- here("Data", "Raw", "state_databases", "Pennsylvania", "facility_linkage_table.csv")
if (!file.exists(pa_link_path)) {
  # Try the absolute path used in 09_Clean_PA.R
  pa_link_path <- "C:/Users/kalebkja/PA_UST_Auction_Analysis/data/external/padep/facility_linkage_table.csv"
}
if (file.exists(pa_link_path)) {
  pa_link <- fread(pa_link_path, nrows=10, colClasses="character") %>% clean_names()
  peek(pa_link, "PA facility_linkage_table.csv")
}

# --- 2j. Tennessee ---
subsection("2j. Tennessee")
tn_path <- here("Data", "Raw", "state_databases", "Tennessee")
log("TN files: ", paste(list.files(tn_path), collapse = ", "))
safe_peek_csv(file.path(tn_path, "TN_Harmonized_UST_tanks.csv"), "TN Harmonized Tanks")

# Check TN facility_id vs EPA
tn_tanks <- tryCatch(fread(file.path(tn_path, "TN_Harmonized_UST_tanks.csv"), nrows=10, colClasses="character"), error=function(e) NULL)
if (!is.null(tn_tanks) && exists("epa_fac")) {
  log("\nTN facility_id samples: ", paste(tn_tanks$facility_id[1:8], collapse=" | "))
  log("EPA TN ID samples:      ", paste(epa_fac[state=="Tennessee", facility_id][1:8], collapse=" | "))
}

# --- 2k. Texas ---
subsection("2k. Texas")
tx_path <- here("Data", "Raw", "state_databases", "Texas")
log("TX files: ", paste(list.files(tx_path), collapse = ", "))
safe_peek_csv(file.path(tx_path, "TX_Harmonized_UST_tanks.csv"), "TX Harmonized Tanks")
safe_peek_csv(file.path(tx_path, "TX_Harmonized_LUST.csv"), "TX Harmonized LUST")

# TX LUST date check after fix
tx_lust_check <- tryCatch(fread(file.path(tx_path, "TX_Harmonized_LUST.csv"), colClasses="character"), error=function(e) NULL)
if (!is.null(tx_lust_check)) {
  n_parseable <- sum(!is.na(as.Date(tx_lust_check$report_date)), na.rm=TRUE)
  log(sprintf("\nTX LUST parseable dates: %d / %d (%.1f%%)", n_parseable, nrow(tx_lust_check), n_parseable/nrow(tx_lust_check)*100))
  log("TX LUST report_date samples: ", paste(tx_lust_check$report_date[!is.na(tx_lust_check$report_date)][1:8], collapse=" | "))
}

# --- 2l. Washington DC (the duplicate WA bug) ---
subsection("2l. Washington DC — Duplicate WA Bug Check")
dc_path <- here("Data", "Raw", "state_databases", "Washington DC")
log("DC directory exists: ", dir.exists(dc_path))
if (dir.exists(dc_path)) {
  log("Files: ", paste(list.files(dc_path), collapse=", "))
  dc_file <- file.path(dc_path, "WA_Harmonized_UST_tanks.csv")
  if (file.exists(dc_file)) {
    dc_tanks <- fread(dc_file, nrows=5, colClasses="character")
    log("DC harmonized file 'state' column values: ", paste(dc_tanks$state, collapse=" | "))
    log("BUG PRESENT: ", all(dc_tanks$state == "WA"), " (should be DC, not WA)")
    log("Fix needed in DC cleaning script: change state='WA' to state='DC'")
  }
}

# ============================================================
#  SECTION 3: EPA MERGE ID CROSSCHECK (ALL STATES)
# ============================================================
logsep("3. EPA MERGE ID CROSSCHECK — ALL CUSTOM STATES")

if (exists("epa_fac")) {
  subsection("3a. ID format comparison: harmonized facility_id vs EPA facility_id")

  # Build EPA lookup by state
  epa_by_state <- epa_fac[, .(
    n_epa = .N,
    sample_ids = paste(facility_id[1:min(5,.N)], collapse=" | ")
  ), by = state]

  check_states <- list(
    list(abbr="AL", full="Alabama",     file=file.path(here("Data/Raw/state_databases/Alabama"),    "AL_Harmonized_UST_tanks.csv")),
    list(abbr="AR", full="Arkansas",    file=file.path(here("Data/Raw/state_databases/Arkansas"),   "AR_Harmonized_UST_tanks.csv")),
    list(abbr="CO", full="Colorado",    file=file.path(here("Data/Raw/state_databases/Colorado"),   "CO_Harmonized_UST_tanks.csv")),
    list(abbr="LA", full="Louisiana",   file=file.path(here("Data/Raw/state_databases/Louisiana"),  "LA_Harmonized_UST_tanks.csv")),
    list(abbr="ME", full="Maine",       file=file.path(here("Data/Raw/state_databases/Maine"),      "ME_Harmonized_UST_tanks.csv")),
    list(abbr="MI", full="Michigan",    file=file.path(here("Data/Raw/state_databases/Michigan"),   "MI_Harmonized_UST_tanks.csv")),
    list(abbr="NJ", full="New Jersey",  file=file.path(here("Data/Raw/state_databases/New Jersey"), "NJ_Harmonized_UST_tanks.csv")),
    list(abbr="NM", full="New Mexico",  file=file.path(here("Data/Raw/state_databases/New Mexico"), "NM_Harmonized_UST_tanks.csv")),
    list(abbr="OK", full="Oklahoma",    file=file.path(here("Data/Raw/state_databases/Oklahoma"),   "OK_Harmonized_UST_tanks.csv")),
    list(abbr="TN", full="Tennessee",   file=file.path(here("Data/Raw/state_databases/Tennessee"),  "TN_Harmonized_UST_tanks.csv")),
    list(abbr="TX", full="Texas",       file=file.path(here("Data/Raw/state_databases/Texas"),      "TX_Harmonized_UST_tanks.csv"))
  )

  for (s in check_states) {
    if (!file.exists(s$file)) { log(s$abbr, ": harmonized file missing"); next }
    dt <- fread(s$file, nrows=200, colClasses="character")
    epa_ids <- epa_fac[state == s$full, facility_id]

    if (length(epa_ids) == 0) { log(s$abbr, ": no EPA rows found for ", s$full); next }

    # Test candidate merge keys
    fac_ids <- dt$facility_id
    n_total  <- length(fac_ids)

    strategies <- list(
      raw            = fac_ids,
      prefix_state   = paste0(s$abbr, fac_ids),
      strip_prefix   = gsub(paste0("^", s$abbr), "", fac_ids),
      numeric_only   = paste0(s$abbr, gsub("[^0-9]", "", fac_ids)),
      epa_key        = if ("epa_key" %in% names(dt)) dt$epa_key else rep(NA_character_, n_total)
    )

    log(sprintf("\n%s (n_sample=%d, EPA n=%d):", s$abbr, n_total, length(epa_ids)))
    log(sprintf("  Harmonized facility_id sample: %s", paste(fac_ids[1:min(5,n_total)], collapse=" | ")))
    log(sprintf("  EPA ID sample:                 %s", paste(epa_ids[1:min(5,length(epa_ids))], collapse=" | ")))
    for (nm in names(strategies)) {
      cand <- strategies[[nm]]
      if (all(is.na(cand))) { log(sprintf("  %-20s: N/A (column absent)", nm)); next }
      n_match <- length(intersect(na.omit(cand), epa_ids))
      log(sprintf("  %-20s: %5d / %5d  (%5.1f%%)", nm, n_match, n_total, n_match/n_total*100))
    }
  }
}

# ============================================================
#  SECTION 4: COORDINATE SYSTEM DIAGNOSTICS
# ============================================================
logsep("4. COORDINATE SYSTEM DIAGNOSTICS (pct_miss_lat > 100% bug)")

log("BUG IDENTIFIED: calc_missing() in 04_Master_Build.R captures lat_col as a full-table")
log("vector OUTSIDE the by= grouping. sum(is.na(lat_col)) counts all NAs in the master")
log("table, while .N is just the group size — producing impossible >100% values.\n")
log("FIXED calc_missing() function (drop-in replacement for 04_Master_Build.R):\n")

fixed_calc <- '
# FIXED calc_missing — uses column NAMES, not pre-captured vectors
# The lat/long column is referenced as a string and evaluated inside the j expression
# so it correctly subsets by the by= groups.
calc_missing <- function(dt) {
  lat_col_name <- if ("latitude_final" %in% names(dt)) "latitude_final" else "latitude"

  dt[, .(
    total_tanks = .N,

    pct_closed_missing_date = {
      n_closed <- sum(status_std == "Closed", na.rm = TRUE)
      if (n_closed > 0) round(sum(status_std == "Closed" & is.na(tank_closed_date)) / n_closed * 100, 1)
      else NA_real_
    },
    pct_missing_install_date  = round(sum(is.na(tank_installed_date)) / .N * 100, 1),

    # FIX: reference column by name string so it subsets correctly within by=
    pct_miss_lat     = round(sum(is.na(.SD[[lat_col_name]])) / .N * 100, 1),
    pct_miss_county  = round(sum(is.na(county_name) | county_name == "") / .N * 100, 1),
    pct_miss_capacity = round(sum(is.na(capacity)) / .N * 100, 1),
    pct_miss_tank_status = round(sum(is.na(tank_status) | tank_status == "") / .N * 100, 1),
    pct_miss_tank_substance = round(sum(
      (is.na(is_gasoline)     | is_gasoline     == 0) &
      (is.na(is_diesel)       | is_diesel       == 0) &
      (is.na(is_oil_kerosene) | is_oil_kerosene == 0) &
      (is.na(is_jet_fuel)     | is_jet_fuel     == 0) &
      (is.na(is_other)        | is_other        == 0)
    ) / .N * 100, 1),
    pct_miss_tank_type = round(sum(
      is.na(single_walled) | unknown_walled == 1 | (single_walled == 0 & double_walled == 0)
    ) / .N * 100, 1),
    pct_miss_fips = round(sum(is.na(county_fips) | county_fips == "") / .N * 100, 1)

  ), by = .(study_group, state), .SDcols = lat_col_name]
}
'
log(fixed_calc)

# Also check coordinate ranges in harmonized files
subsection("4b. Coordinate range check in harmonized files (flag Mercator survivors)")
all_tank_files <- list.files(here("Data", "Raw", "state_databases"),
                              pattern = "_Harmonized_UST_tanks.csv$",
                              recursive = TRUE, full.names = TRUE)

coord_check <- rbindlist(lapply(all_tank_files, function(f) {
  dt <- tryCatch(fread(f, select = c("state", "latitude", "longitude"),
                       colClasses = "character"), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0) return(NULL)
  dt[, lat_n := suppressWarnings(as.numeric(latitude))]
  dt[, .(
    state = state[1],
    n_rows = .N,
    n_has_lat = sum(!is.na(lat_n)),
    n_mercator = sum(abs(lat_n) > 90, na.rm=TRUE),
    n_valid_deg = sum(between(lat_n, -90, 90), na.rm=TRUE),
    lat_min = min(lat_n, na.rm=TRUE),
    lat_max = max(lat_n, na.rm=TRUE)
  )]
}), fill=TRUE)

log("States with Mercator coordinates surviving in harmonized files:")
log(capture.output(print(coord_check[n_mercator > 0])))
log("\nFull coordinate coverage table:")
log(capture.output(print(coord_check[order(-n_mercator)])))

# ============================================================
#  SECTION 5: SUMMARY — REMAINING ACTION ITEMS
# ============================================================
logsep("5. SUMMARY OF REMAINING ISSUES")

log("Based on quality report and diagnostics:\n")
log("CRITICAL (blocks analysis):")
log("  1. calc_missing() lat_col scoping bug -> fix in 04_Master_Build.R (code above)")
log("  2. Washington DC writes state='WA' -> fix DC cleaning script, rerun, rerun master")
log("  3. PA EPA merge only 9.2% -> investigate why epa_key values not matching Facilities.csv")
log("     (linkage table uses efacts/site_id but EPA Facilities.csv may use different IDs)")
log("")
log("HIGH PRIORITY (material gaps in control states):")
log("  4. AL EPA merge still 0% -> verify 04_Master_Build.R AL merge_id line was saved/rerun")
log("  5. TN 41.6% missing lat -> check TN facility_id vs EPA ID format alignment")
log("  6. OK 62.7% missing lat -> check OK facility_id vs EPA ID format")
log("  7. CO 76.2% missing lat -> check CO facility_id vs EPA ID format")
log("  8. ME 148%+ (Mercator bug OR calc_missing scoping bug) -> fix calc_missing first")
log("")
log("MEDIUM PRIORITY:")
log("  9. PA LUST 15653/15653 missing report_date -> PA has no LUST date column? Check schema")
log(" 10. KY LUST 13115/13115 missing -> same issue")
log(" 11. AR substance 9.9% missing (3,358 rows with NA status) -> check AR cleaning logic")

log("\nDiagnostics complete. Log saved to: ", log_file)
close(con_log)
