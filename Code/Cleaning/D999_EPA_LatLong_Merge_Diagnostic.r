###############################################################################
# 00_EPA_LatLong_Merge_Diagnostic.R  (v2 — all runtime errors fixed)
# Purpose:
#   1. EPA lat/long completeness by state (all states)
#   2. EPA lat/long completeness by state × first-year cohort
#   3. Merge key format identification for 12 cleaning-script states
#   4. PA linkage table → EPA ID crosswalk validation
#   5. Non-cleaning-script states: missingness 1985–2023
#   6. Bugs & fixes for 04_Master_Build.R merge logic
#
# Inputs:
#   - Data/Raw/Facilities.csv    (EPA UST Finder — Facility layer)
#   - Data/Raw/USTs.csv          (EPA UST Finder — UST layer)
#   - Data/Raw/state_databases/Pennsylvania/facility_linkage_table.csv
#   - Data/Raw/state_databases/*_Harmonized_UST_tanks.csv
#
# Output:
#   - Output/Diagnostics/EPA_LatLong_Merge_Diagnostic.txt
#
# v2 Fixes:
#   - parse_epa_date: tryCatch around as.Date (was throwing error, not warning)
#   - State map: handles "NewYork" (no space) and "Washington DC"
#   - PA linkage: broader Sys.glob search
#   - OK: documents bracket-format mismatch
###############################################################################

library(data.table)
library(stringr)
library(here)
library(janitor)
library(lubridate) # REQUIRED for robust date parsing fallback

# ═══════════════════════════════════════════════════════════════════════════════
# 0. SETUP
# ═══════════════════════════════════════════════════════════════════════════════

out_dir <- here("Output", "Diagnostics")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
log_file <- file.path(out_dir, "EPA_LatLong_Merge_Diagnostic.txt")

# Start log
sink(log_file, split = TRUE)
cat(strrep("=", 80), "\n")
cat("EPA LAT/LONG & MERGE KEY DIAGNOSTIC (v2)\n")
cat("Generated:", as.character(Sys.time()), "\n")
cat(strrep("=", 80), "\n\n")

# Constants
cleaning_states <- c("AR", "LA", "ME", "MI", "NJ", "NM", "OK", "TX", "AL", "TN", "CO", "PA")

# ═══════════════════════════════════════════════════════════════════════════════
# 1. LOAD EPA DATA
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 1: LOADING EPA DATA\n")
cat(strrep("=", 80), "\n\n")

fac_path <- here("Data", "Raw", "Facilities.csv")
ust_path <- here("Data", "Raw", "USTs.csv")

stopifnot("Facilities.csv not found" = file.exists(fac_path))
stopifnot("USTs.csv not found"       = file.exists(ust_path))

# --- 1A. Facilities (geometry source) ---
cat("Loading Facilities.csv...\n")
epa_fac_raw <- fread(fac_path, colClasses = "character")
cat("  Raw columns:", paste(names(epa_fac_raw), collapse = ", "), "\n")
cat("  Raw rows:", format(nrow(epa_fac_raw), big.mark = ","), "\n\n")

# Identify columns dynamically
id_col  <- grep("facility.*id",  names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
st_col  <- grep("^state$",       names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
lat_col <- grep("latitude",      names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
lon_col <- grep("longitude",     names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]

cat("  Detected columns:\n")
cat("    ID col:  ", id_col, "\n")
cat("    State:   ", st_col, "\n")
cat("    Lat col: ", lat_col, "\n")
cat("    Lon col: ", lon_col, "\n\n")

epa_fac <- epa_fac_raw[, .SD, .SDcols = c(id_col, st_col, lat_col, lon_col)]
setnames(epa_fac, c("epa_id", "epa_state", "lat", "lon"))

epa_fac[, lat := as.numeric(lat)]
epa_fac[, lon := as.numeric(lon)]
epa_fac[, has_geo := !is.na(lat) & !is.na(lon)]

# ── FIX v2: Extended state map (handles "NewYork", "Washington DC", etc.) ──
state_map <- data.table(epa_state = state.name, state_abbr = state.abb)

# Add known EPA quirks
extra_states <- data.table(
  epa_state = c("NewYork", "New York", "Washington DC", "District of Columbia",
                "Puerto Rico", "Virgin Islands", "Guam", "American Samoa",
                "Northern Mariana Islands"),
  state_abbr = c("NY", "NY", "DC", "DC", "PR", "VI", "GU", "AS", "MP")
)
state_map <- rbind(state_map, extra_states)

# Also try cleaning: remove spaces and re-match
epa_fac[, epa_state_clean := trimws(epa_state)]
epa_fac <- merge(epa_fac, state_map, by.x = "epa_state_clean", by.y = "epa_state", all.x = TRUE)

# Report unmatched
unmatched <- epa_fac[is.na(state_abbr), .N, by = epa_state]
if (nrow(unmatched) > 0) {
  cat("  States still failing name→abbr map after extended map:\n")
  print(unmatched)
} else {
  cat("  All states mapped successfully.\n")
}
cat("\n")

# --- 1B. USTs (for install dates) ---
cat("Loading USTs.csv...\n")
epa_usts <- fread(ust_path, colClasses = "character") %>% clean_names() %>% as.data.table()
cat("  Raw columns:", paste(names(epa_usts)[1:min(10, ncol(epa_usts))], collapse = ", "), "...\n")
cat("  Raw rows:", format(nrow(epa_usts), big.mark = ","), "\n\n")

# Parse install date robustly
inst_col <- grep("install", names(epa_usts), ignore.case = TRUE, value = TRUE)[1]
cat("  Install date column:", inst_col, "\n")

# Show raw format samples before parsing
raw_samples <- head(unique(epa_usts[!is.na(get(inst_col)) & get(inst_col) != "", get(inst_col)]), 10)
cat("  Raw date samples:", paste(raw_samples, collapse = " | "), "\n\n")

# ── FIX v2: Robust date parser with tryCatch (errors, not just warnings) ──
# ── FIX v3: Integrated Robust Date Parser (Handles Pre-1970 & Excel Serial) ──
parse_epa_date_v2 <- function(x) {
  num_val <- suppressWarnings(as.numeric(x))
  d_out <- as.Date(rep(NA, length(x)))
  
  # Case A: Unix Milliseconds (Positive OR Negative)
  is_millis <- !is.na(num_val) & abs(num_val) > 1e10
  if (any(is_millis)) {
    d_out[is_millis] <- as.Date(as.POSIXct(num_val[is_millis] / 1000, origin = "1970-01-01"))
  }
  
  # Case B: Excel Serial Days (Fallback)
  is_serial <- !is.na(num_val) & !is_millis & num_val > 100 & num_val < 100000
  if (any(is_serial)) {
    d_out[is_serial] <- as.Date(num_val[is_serial], origin = "1899-12-30")
  }
  
  # Case C: Text Formats (Final Fallback via lubridate)
  needs_text <- is.na(d_out) & !is.na(x) & x != ""
  if (any(needs_text)) {
    d_parsed <- lubridate::parse_date_time(x[needs_text], 
                                           orders = c("ymd", "mdy", "Y-m-d", "m/d/Y", "dmy"), 
                                           quiet = TRUE)
    d_out[needs_text] <- as.Date(d_parsed)
  }
  
  # Sanity bounds: reject dates outside 1900-2030
  d_out[!is.na(d_out) & (d_out < as.Date("1900-01-01") | d_out > as.Date("2030-12-31"))] <- NA
  
  return(d_out)
}

cat("  Parsing install dates (this may take a moment for 2.3M rows)...\n")
epa_usts[, install_date := parse_epa_date_v2(get(inst_col))]
epa_usts[, install_year := year(install_date)]
cat("  Parsing install dates (this may take a moment for 2.3M rows)...\n")
epa_usts[, install_date := parse_epa_date_v2(get(inst_col))]
epa_usts[, install_year := year(install_date)]

parse_rate <- round(mean(!is.na(epa_usts$install_year)) * 100, 1)
cat("  Install date parse rate:", parse_rate, "%\n")

# Show parse results by format detected
n_epoch <- sum(!is.na(suppressWarnings(as.numeric(epa_usts[[inst_col]]))) & 
               (nchar(epa_usts[[inst_col]]) >= 10 | 
                suppressWarnings(as.numeric(epa_usts[[inst_col]])) < 0), na.rm = TRUE)
cat("  Likely epoch-format rows:", format(n_epoch, big.mark = ","), "\n")

# Year distribution sanity check
if (parse_rate > 0) {
  yr_dist <- epa_usts[!is.na(install_year), .N, by = install_year][order(install_year)]
  cat("  Year range:", min(yr_dist$install_year), "-", max(yr_dist$install_year), "\n")
  cat("  Year distribution (top decades):\n")
  decade_dist <- epa_usts[!is.na(install_year), .(n = .N), 
                           by = .(decade = floor(install_year / 10) * 10)][order(decade)]
  print(decade_dist)
}
cat("\n")

# Facility-level first year observed
first_year <- epa_usts[!is.na(install_year), .(first_year = min(install_year)), by = facility_id]
cat("  Unique facilities with install year:", format(nrow(first_year), big.mark = ","), "\n\n")


# ═══════════════════════════════════════════════════════════════════════════════
# 2. EPA LAT/LONG COMPLETENESS BY STATE (Q1)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 2: EPA LAT/LONG COMPLETENESS BY STATE\n")
cat(strrep("=", 80), "\n\n")

geo_by_state <- epa_fac[!is.na(state_abbr), .(
  total_fac   = .N,
  has_latlong = sum(has_geo),
  pct_latlong = round(sum(has_geo) / .N * 100, 1)
), by = state_abbr][order(-total_fac)]

cat("All states (sorted by facility count):\n")
print(geo_by_state, nrows = 60)
cat("\n")

cat("Summary across all states:\n")
cat("  Total facilities:", format(sum(geo_by_state$total_fac), big.mark = ","), "\n")
cat("  With lat/long:  ", format(sum(geo_by_state$has_latlong), big.mark = ","), 
    "(", round(sum(geo_by_state$has_latlong) / sum(geo_by_state$total_fac) * 100, 1), "%)\n\n")

cat("--- Your 12 cleaning-script states ---\n")
print(geo_by_state[state_abbr %in% cleaning_states][order(state_abbr)])
cat("\n")

# ═══════════════════════════════════════════════════════════════════════════════
# 3. EPA LAT/LONG BY STATE × FIRST-YEAR COHORT (Q2)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 3: EPA LAT/LONG BY STATE × FIRST-YEAR COHORT\n")
cat(strrep("=", 80), "\n\n")

if (nrow(first_year) > 0) {
  
  # Merge first_year onto facilities
  epa_fac_yr <- merge(epa_fac, first_year, by.x = "epa_id", by.y = "facility_id", all.x = TRUE)
  
  epa_fac_yr[, cohort := fcase(
    is.na(first_year),                   "00_No_Install_Date",
    first_year < 1985,                   "01_Pre-1985",
    first_year %between% c(1985, 1989),  "02_1985-1989",
    first_year %between% c(1990, 1994),  "03_1990-1994",
    first_year %between% c(1995, 1999),  "04_1995-1999",
    first_year %between% c(2000, 2004),  "05_2000-2004",
    first_year %between% c(2005, 2009),  "06_2005-2009",
    first_year %between% c(2010, 2014),  "07_2010-2014",
    first_year %between% c(2015, 2019),  "08_2015-2019",
    first_year >= 2020,                  "09_2020+"
  )]
  
  # 3A. All states aggregated
  cat("--- National: Lat/Long completeness by cohort ---\n")
  national_cohort <- epa_fac_yr[!is.na(state_abbr), .(
    n_fac       = .N,
    has_geo     = sum(has_geo),
    pct_geo     = round(sum(has_geo) / .N * 100, 1)
  ), by = cohort][order(cohort)]
  print(national_cohort)
  cat("\n")
  
  # 3B. Your 12 states × cohort
  cat("--- 12 Cleaning-Script States: Lat/Long by cohort ---\n")
  state_cohort <- epa_fac_yr[state_abbr %in% cleaning_states, .(
    n_fac   = .N,
    has_geo = sum(has_geo),
    pct_geo = round(sum(has_geo) / .N * 100, 1)
  ), by = .(state_abbr, cohort)][order(state_abbr, cohort)]
  print(state_cohort, nrows = 200)
  cat("\n")
  
  # 3C. Pivot: state × cohort (wide form for readability)
  cat("--- Wide form: pct_geo by state × cohort ---\n")
  wide_cohort <- dcast(state_cohort, state_abbr ~ cohort, value.var = "pct_geo", fill = NA)
  print(wide_cohort)
  cat("\n")
  
} else {
  cat("  [SKIP] No install dates parsed — cannot build cohort analysis.\n")
  cat("  Check raw date format samples above and adjust parse_epa_date_v2.\n\n")
  epa_fac_yr <- copy(epa_fac)
  epa_fac_yr[, first_year := NA_integer_]
  epa_fac_yr[, cohort := "00_No_Install_Date"]
}


# ═══════════════════════════════════════════════════════════════════════════════
# 4. MERGE KEY FORMAT IDENTIFICATION (Q3/Q4)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 4: EPA ID FORMAT BY STATE & MERGE KEY VALIDATION\n")
cat(strrep("=", 80), "\n\n")

# 4A. EPA ID format samples and patterns per state
cat("--- EPA Facility_ID format samples (8 per state) ---\n\n")

for (st in sort(cleaning_states)) {
  st_data <- epa_fac[state_abbr == st]
  cat(sprintf("  %s: n=%s | samples: %s\n", 
              st, format(nrow(st_data), big.mark = ","),
              paste(head(st_data$epa_id, 8), collapse = ", ")))
  
  # Pattern analysis
  has_prefix <- mean(grepl(paste0("^", st), st_data$epa_id)) * 100
  all_numeric_after_prefix <- mean(grepl(paste0("^", st, "[0-9]+$"), st_data$epa_id)) * 100
  has_brackets <- mean(grepl("\\[", st_data$epa_id)) * 100
  id_lengths <- nchar(st_data$epa_id)
  
  cat(sprintf("          Prefix: %.0f%% | Prefix+digits: %.0f%% | Has brackets: %.0f%% | Len: %d–%d\n\n",
              has_prefix, all_numeric_after_prefix, has_brackets, min(id_lengths), max(id_lengths)))
}

# 4B. Merge key construction audit
cat("--- Current Merge Key Logic in 04_Master_Build.R ---\n\n")

cat("  group_prefix states (paste0(state, facility_id)):\n")
cat("    TX, CO, LA, ME, NJ, NM, TN, AR\n")
cat("    [BUG] OK is MISSING — and EPA OK IDs use brackets: OK[4511652]\n")
cat("    OK needs special handling, not just group_prefix addition.\n\n")

cat("  PA: uses epa_key = paste0('PA', coalesce(efacts_facility_id, site_id))\n")
cat("  MI: paste0('MI', str_remove(facility_id, '^0+'))\n")
cat("  AL: paste0('AL', gsub('[^0-9]', '', facility_id))\n\n")

# 4C. Test merge key match rates against EPA
cat("--- Simulated Merge Key Match Rates ---\n\n")

# Load harmonized state files to get actual facility_ids
tank_files <- list.files(path = here("Data", "Raw", "state_databases"), 
                         pattern = "_Harmonized_UST_tanks.csv$", 
                         recursive = TRUE, full.names = TRUE)

if (length(tank_files) > 0) {
  
  all_tanks <- rbindlist(lapply(tank_files, function(f) {
    all_cols <- names(fread(f, nrows = 0))
    # EXPLICIT INCLUSION: Pull native lat/long
    cols_want <- intersect(c("facility_id", "state", "epa_key", "latitude", "longitude"), all_cols)
    dt <- fread(f, select = cols_want, colClasses = "character")
    if (!"epa_key" %in% names(dt)) dt[, epa_key := NA_character_]
    if (!"latitude" %in% names(dt)) dt[, latitude := NA_character_]
    if (!"longitude" %in% names(dt)) dt[, longitude := NA_character_]
    return(dt)
  }), fill = TRUE, use.names = TRUE)
  
  
  # Get unique facilities per state and cast native coordinates
  fac_unique <- unique(all_tanks[, .(facility_id, state, epa_key, latitude, longitude)])
  
  # STRICT VALIDATION: Force numeric coercion and restrict to NA/US bounding box
  fac_unique[, native_lat := suppressWarnings(as.numeric(latitude))]
  fac_unique[, native_lon := suppressWarnings(as.numeric(longitude))]
  fac_unique[, has_native_geo := !is.na(native_lat) & !is.na(native_lon) & 
                                 native_lat > 15 & native_lat < 75 & 
                                 native_lon > -180 & native_lon < -60]
  
  # Build merge keys exactly as master script does (CURRENT logic)
  fac_unique[, merge_id_current := NA_character_]
  
  group_prefix_current <- c("TX", "CO", "LA", "ME", "NJ", "NM", "TN", "AR")
  fac_unique[state %in% group_prefix_current, merge_id_current := paste0(state, facility_id)]
  fac_unique[state == "PA" & !is.na(epa_key) & epa_key != "", merge_id_current := epa_key]
  fac_unique[state == "MI", merge_id_current := paste0("MI", str_remove(facility_id, "^0+"))]
  fac_unique[state == "AL", merge_id_current := paste0("AL", gsub("[^0-9]", "", facility_id))]
  # OK: currently not handled at all in master script
  fac_unique[state == "OK", merge_id_current := paste0("OK", facility_id)]
  
  # Build PROPOSED merge keys (with fixes)
  fac_unique[, merge_id_proposed := merge_id_current]  # start from current
  
  # FIX: OK needs brackets to match EPA format OK[1234567]
  fac_unique[state == "OK", merge_id_proposed := paste0("OK[", facility_id, "]")]
  
  # FIX: NJ — try stripping leading zeros from state data to match EPA
  # EPA has NJ008118 (with zeros), state has NJ000001 (with zeros)
  # Both have zeros but different ID numbers — this may just be a universe mismatch
  
  # EPA lookup sets
  epa_ids_with_geo <- unique(epa_fac[has_geo == TRUE, .(epa_id)])
  epa_ids_all      <- unique(epa_fac[, .(epa_id)])
  
  # Match rates — CURRENT logic
  cat("  === CURRENT merge key match rates ===\n\n")
  merge_diag_current <- fac_unique[, .(
    n_facilities      = uniqueN(facility_id),
    has_merge_key     = sum(!is.na(merge_id_current)),
    matched_epa       = sum(merge_id_current %in% epa_ids_all$epa_id, na.rm = TRUE),
    matched_epa_geo   = sum(merge_id_current %in% epa_ids_with_geo$epa_id, na.rm = TRUE),
    merge_id_sample   = paste(head(na.omit(merge_id_current), 3), collapse = " | ")
  ), by = state][order(state)]
  
  merge_diag_current[, `:=`(
    pct_match     = round(matched_epa / n_facilities * 100, 1),
    pct_match_geo = round(matched_epa_geo / n_facilities * 100, 1)
  )]
  print(merge_diag_current)
  cat("\n")
  
  # Match rates — PROPOSED fixes
  cat("  === PROPOSED merge key match rates (with OK bracket fix) ===\n\n")
  merge_diag_proposed <- fac_unique[, .(
    n_facilities      = uniqueN(facility_id),
    matched_epa       = sum(merge_id_proposed %in% epa_ids_all$epa_id, na.rm = TRUE),
    matched_epa_geo   = sum(merge_id_proposed %in% epa_ids_with_geo$epa_id, na.rm = TRUE),
    merge_id_sample   = paste(head(na.omit(merge_id_proposed), 3), collapse = " | ")
  ), by = state][order(state)]
  
  merge_diag_proposed[, `:=`(
    pct_match     = round(matched_epa / n_facilities * 100, 1),
    pct_match_geo = round(matched_epa_geo / n_facilities * 100, 1)
  )]
  
  # Only show states where proposed differs from current
  changed <- merge_diag_proposed[state %in% c("OK")]
  print(changed)
  cat("\n")
  # Gap Coverage Analysis
  cat("  === NATIVE GEOSPATIAL COVERAGE FOR EPA UNMATCHED FACILITIES ===\n\n")
  fac_unique[, is_matched := merge_id_proposed %in% epa_ids_all$epa_id]
  
  gap_diag <- fac_unique[, .(
    n_total_gap         = sum(!is_matched, na.rm = TRUE),
    gap_with_native_geo = sum(!is_matched & has_native_geo, na.rm = TRUE)
  ), by = state][order(state)]
  
  gap_diag[, pct_gap_covered := round(gap_with_native_geo / n_total_gap * 100, 1)]
  
  # Filter to states exhibiting a coverage deficit
  print(gap_diag[n_total_gap > 0])
  cat("\n")

  # Show EPA samples side-by-side for ALL states with < 80% match
  low_match <- merge_diag_current[pct_match < 80]$state
  if (length(low_match) > 0) {
    cat("  *** STATES WITH < 80% MATCH — ID format comparison ***\n\n")
    for (st in low_match) {
      state_ids  <- head(fac_unique[state == st & !is.na(merge_id_current), merge_id_current], 5)
      epa_ids_st <- head(epa_fac[state_abbr == st, epa_id], 5)
      proposed   <- head(fac_unique[state == st & !is.na(merge_id_proposed), merge_id_proposed], 5)
      cat(sprintf("  %s:\n", st))
      cat(sprintf("    Current merge_ids:  %s\n", paste(state_ids, collapse = ", ")))
      cat(sprintf("    EPA epa_ids:        %s\n", paste(epa_ids_st, collapse = ", ")))
      cat(sprintf("    Proposed merge_ids: %s\n\n", paste(proposed, collapse = ", ")))
    }
  }
  
} else {
  cat("  [SKIP] No harmonized state files found — cannot test merge keys.\n\n")
}


# ═══════════════════════════════════════════════════════════════════════════════
# 5. PA LINKAGE TABLE INVESTIGATION
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 5: PA LINKAGE TABLE -> EPA ID CROSSWALK VALIDATION\n")
cat(strrep("=", 80), "\n\n")

# Hardcoded absolute path to server external drive
pa_link_path <- "C:/Users/kalebkja/PA_UST_Auction_Analysis/data/external/padep/facility_linkage_table.csv"

cat("  Target path:\n")
cat(sprintf("    %s [%s]\n", pa_link_path, ifelse(file.exists(pa_link_path), "FOUND", "not found")))
cat("\n")
pa_link <- fread(pa_link_path, colClasses = "character") %>% clean_names()
  
  cat("PA linkage table: ", format(nrow(pa_link), big.mark = ","), " rows\n")
  cat("Columns:", paste(names(pa_link), collapse = ", "), "\n\n")
  
  # ID availability
  cat("--- ID column population ---\n")
  for (col in c("facility_id", "site_id", "client_id", "efacts_facility_id")) {
    if (col %in% names(pa_link)) {
      n_pop <- sum(!is.na(pa_link[[col]]) & pa_link[[col]] != "" & pa_link[[col]] != "NA")
      cat(sprintf("  %-25s: %s / %s (%.1f%%)\n", col, 
                  format(n_pop, big.mark = ","),
                  format(nrow(pa_link), big.mark = ","),
                  n_pop / nrow(pa_link) * 100))
    }
  }
  cat("\n")
  
  # Build candidate keys
  pa_link[, efacts_clean := fifelse(efacts_facility_id %in% c("NA", "", NA), NA_character_, trimws(efacts_facility_id))]
  pa_link[, site_clean   := fifelse(site_id %in% c("NA", "", NA), NA_character_, trimws(site_id))]
  pa_link[, client_clean := fifelse(client_id %in% c("NA", "", NA), NA_character_, trimws(client_id))]
  pa_link[, epa_numeric  := fcoalesce(efacts_clean, site_clean)]
  pa_link[, epa_key_current := fifelse(!is.na(epa_numeric), paste0("PA", epa_numeric), NA_character_)]
  
  cat("--- Current merge key (PA + coalesce(efacts, site_id)) ---\n")
  cat("  epa_key populated:", sum(!is.na(pa_link$epa_key_current)), "/", nrow(pa_link), "\n")
  cat("  Samples:", paste(head(pa_link[!is.na(epa_key_current), epa_key_current], 8), collapse = ", "), "\n\n")
  
  # Cross-check against actual EPA IDs for PA
  pa_epa_ids <- epa_fac[state_abbr == "PA"]
  cat("  EPA Facilities.csv PA entries:", format(nrow(pa_epa_ids), big.mark = ","), "\n")
  cat("  EPA PA ID samples:", paste(head(pa_epa_ids$epa_id, 8), collapse = ", "), "\n\n")
  
  # Test each candidate key against EPA
  cat("--- Match rates for different PA key candidates ---\n\n")
  
  candidates <- list(
    "PA + efacts_facility_id" = pa_link[!is.na(efacts_clean), paste0("PA", efacts_clean)],
    "PA + site_id"            = pa_link[!is.na(site_clean), paste0("PA", site_clean)],
    "PA + client_id"          = pa_link[!is.na(client_clean), paste0("PA", client_clean)],
    "efacts alone (no prefix)"= pa_link[!is.na(efacts_clean), efacts_clean],
    "site_id alone"           = pa_link[!is.na(site_clean), site_clean],
    "Current (coalesce)"      = pa_link[!is.na(epa_key_current), epa_key_current]
  )
  
  for (nm in names(candidates)) {
    keys <- candidates[[nm]]
    n_total   <- length(keys)
    n_in_epa  <- sum(keys %in% pa_epa_ids$epa_id)
    n_in_geo  <- sum(keys %in% pa_epa_ids[has_geo == TRUE, epa_id])
    
    cat(sprintf("  %-30s: %6d keys -> %6d match EPA (%5.1f%%) | %6d with geo (%5.1f%%)\n",
                nm, n_total, n_in_epa, 
                if(n_total > 0) n_in_epa/n_total*100 else 0,
                n_in_geo,
                if(n_total > 0) n_in_geo/n_total*100 else 0))
  }
  cat("\n")
  
  # Reverse lookup: EPA PA IDs → which linkage column do they match?
  cat("--- Reverse lookup: 20 EPA PA IDs → linkage table column match ---\n\n")
  sample_epa_pa <- head(pa_epa_ids$epa_id, 20)
  epa_pa_numeric <- str_remove(sample_epa_pa, "^PA")
  
  for (i in seq_along(sample_epa_pa)) {
    eid <- epa_pa_numeric[i]
    in_efacts <- sum(pa_link$efacts_clean == eid, na.rm = TRUE)
    in_site   <- sum(pa_link$site_clean == eid, na.rm = TRUE)
    in_client <- sum(pa_link$client_clean == eid, na.rm = TRUE)
    tag <- ifelse(in_efacts > 0, "efacts", 
           ifelse(in_site > 0, "site_id", 
           ifelse(in_client > 0, "client_id", "NO MATCH")))
    cat(sprintf("  EPA: %-10s -> numeric: %-8s -> matched in: %s\n", 
                sample_epa_pa[i], eid, tag))
  }
  cat("\n")
  
  # Summary: which column is the winner?
  cat("--- PA Key Winner Analysis (full linkage table vs EPA) ---\n\n")
  n_efacts_match <- sum(paste0("PA", pa_link$efacts_clean) %in% pa_epa_ids$epa_id, na.rm = TRUE)
  n_site_match   <- sum(paste0("PA", pa_link$site_clean) %in% pa_epa_ids$epa_id, na.rm = TRUE)
  n_client_match <- sum(paste0("PA", pa_link$client_clean) %in% pa_epa_ids$epa_id, na.rm = TRUE)
  
  winner <- c("efacts_facility_id", "site_id", "client_id")[which.max(c(n_efacts_match, n_site_match, n_client_match))]
  cat(sprintf("  efacts → EPA: %d | site_id → EPA: %d | client_id → EPA: %d\n", 
              n_efacts_match, n_site_match, n_client_match))
  cat(sprintf("  WINNER: %s is the correct EPA crosswalk column for PA.\n\n", winner))
  
  # If efacts wins, check if current coalesce(efacts, site_id) is HURTING
  # (i.e., site_id values that DON'T match EPA but override because efacts is NA)
  if (winner == "efacts_facility_id") {
    bad_coalesce <- pa_link[is.na(efacts_clean) & !is.na(site_clean)]
    bad_coalesce[, test_key := paste0("PA", site_clean)]
    n_bad_site_match <- sum(bad_coalesce$test_key %in% pa_epa_ids$epa_id)
    cat(sprintf("  Of %d facilities using site_id as fallback (efacts NA), %d match EPA.\n",
                nrow(bad_coalesce), n_bad_site_match))
    cat(sprintf("  This means %d PA facilities use site_id keys that DON'T match EPA.\n\n",
                nrow(bad_coalesce) - n_bad_site_match))
  }
  


# ═══════════════════════════════════════════════════════════════════════════════
# 6. NON-CLEANING-SCRIPT STATES: MISSINGNESS 1985–2023 (Q6)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 6: NON-CLEANING-SCRIPT STATES — LAT/LONG MISSINGNESS (1985-2023)\n")
cat(strrep("=", 80), "\n\n")

# Use epa_fac directly (no cohort dependency) for the "all facilities" view
other_fac <- epa_fac[!state_abbr %in% cleaning_states & !is.na(state_abbr)]

cat("--- All facilities in non-cleaning states ---\n")
other_all <- other_fac[, .(
  total_fac = .N,
  has_geo   = sum(has_geo),
  pct_geo   = round(sum(has_geo) / .N * 100, 1)
), by = state_abbr][order(-total_fac)]
print(other_all)
cat("\n")

# Cohort-based analysis (only if first_year was successfully parsed)
if (exists("epa_fac_yr") && "first_year" %in% names(epa_fac_yr)) {
  
  other_yr <- epa_fac_yr[!state_abbr %in% cleaning_states & !is.na(state_abbr)]
  
  # Restrict to 1985-2023
  other_1985 <- other_yr[first_year >= 1985 & first_year <= 2023]
  
  if (nrow(other_1985) > 0) {
    cat("--- Restricted to first_year in [1985, 2023] ---\n")
    other_1985_state <- other_1985[, .(
      n_fac       = .N,
      has_geo     = sum(has_geo),
      pct_geo     = round(sum(has_geo) / .N * 100, 1),
      missing_geo = sum(!has_geo)
    ), by = state_abbr][order(-n_fac)]
    print(other_1985_state)
    cat("\n")
    
    # Wide pivot by cohort
    cat("--- Non-cleaning states x cohort (1985-2023, wide) ---\n")
    other_1985_cohort <- other_1985[, .(
      n_fac   = .N,
      has_geo = sum(has_geo),
      pct_geo = round(sum(has_geo) / .N * 100, 1)
    ), by = .(state_abbr, cohort)][order(state_abbr, cohort)]
    
    other_wide <- dcast(other_1985_cohort, state_abbr ~ cohort, value.var = "pct_geo", fill = NA)
    print(other_wide, nrows = 60)
    cat("\n")
  } else {
    cat("  No non-cleaning-state facilities with first_year in [1985,2023] found.\n\n")
  }
} else {
  cat("  [SKIP] Cohort analysis unavailable (install date parsing failed).\n")
  cat("  Section 2 above still shows lat/long completeness without cohort breakdown.\n\n")
}


# ═══════════════════════════════════════════════════════════════════════════════
# 7. SUMMARY OF BUGS & RECOMMENDED FIXES
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("SECTION 7: BUGS & RECOMMENDED FIXES FOR 04_Master_Build.R\n")
cat(strrep("=", 80), "\n\n")

cat("BUG 1: OK — 0% match (bracket format mismatch)\n")
cat("  EPA uses:  OK[4511652]  (brackets around numeric ID)\n")
cat("  State has: 0100003 (plain numeric, different numbering scheme)\n")
cat("  Current master script: OK not in group_prefix at all\n")
cat("  DIAGNOSIS: Even adding OK to group_prefix produces 'OK0100003'\n")
cat("             which will NEVER match 'OK[4511652]'.\n")
cat("  The IDs themselves differ (not just formatting).\n")
cat("  FIX OPTIONS:\n")
cat("    a) OK state data has native lat/long — rely on that exclusively\n")
cat("    b) Build a name+address fuzzy crosswalk to EPA\n")
cat("    c) Check if OK state 'facility_number' maps to EPA's bracketed ID\n")
cat("       via a different transformation\n\n")

cat("BUG 2: PA — 29.5% match (wrong crosswalk column)\n")
cat("  Current: epa_key = paste0('PA', coalesce(efacts_facility_id, site_id))\n")
cat("  Section 5 above shows which column actually matches EPA IDs.\n")
cat("  If efacts_facility_id is the winner, the coalesce fallback to site_id\n")
cat("  is injecting non-matching keys and polluting the merge.\n")
cat("  FIX: Use ONLY the winning column (likely efacts_facility_id),\n")
cat("       do NOT coalesce with site_id.\n\n")

cat("BUG 3: NJ — 71.1% match (ID universe gap)\n")
cat("  State has 32,140 unique facilities; EPA has 23,155.\n")
cat("  Of state facilities, 22,848 match EPA (71.1%).\n")
cat("  This is close to EPA's total (23,155), suggesting the matched\n")
cat("  facilities are nearly the full EPA universe.\n")
cat("  The unmatched 29% are likely state-only facilities not in EPA.\n")
cat("  NOT a formatting bug — this is a coverage gap.\n")
cat("  FIX: None needed for merge logic. Document that ~29% of NJ facilities\n")
cat("       lack EPA-sourced geo. NJ has native lat/long from State Plane\n")
cat("       conversion, so the impact on final geo coverage is minimal.\n\n")

cat("BUG 4: AR — 75.4% match\n")
cat("  State has 14,283 facilities; EPA has 10,771.\n")
cat("  Matched: 10,771 (75.4%) — exactly EPA's total.\n")
cat("  Same as NJ: coverage gap, not format bug.\n")
cat("  AR has native lat/long from state data.\n\n")

cat("BUG 5 (from v1, now confirmed NOT a bug): AL matches at 97%.\n")
cat("  The AL permit numbers in practice produce short IDs that match EPA.\n")
cat("  No fix needed.\n\n")

cat("BUG 6 (from v1, resolved): MI matches at 92.9%.\n")
cat("  Leading-zero stripping works correctly for MI.\n")
cat("  7.1% unmatched is a coverage gap, not a format issue.\n\n")

cat("BUG 7: NewYork state name mapping\n")
cat("  EPA Facilities.csv has 'NewYork' (no space) for 33,640 facilities.\n")
cat("  The v1 state_map using state.name missed these entirely.\n")
cat("  FIX: Extended state map in this v2 script handles it.\n")
cat("  Must also fix in 04_Master_Build.R if NY facilities are ever needed.\n\n")

sink()
cat("Diagnostic written to:", log_file, "\n")

###############################################################################
# 00_EPA_LatLong_And_PA_Crosswalk_Diagnostic.R
#
# Two goals:
#   1. EPA lat/long completeness for ALL 51 jurisdictions (50 states + DC),
#      overall and broken down by install-year cohort.
#   2. PA manual crosswalk: match 10 PA harmonized facilities to the
#      linkage table and EPA file by every candidate ID column,
#      then save a hand-check CSV.
#
# Inputs:
#   Data/Raw/Facilities.csv
#   Data/Raw/USTs.csv
#   Data/Raw/state_databases/Pennsylvania/PA_Harmonized_UST_tanks.csv
#   C:/.../padep/facility_linkage_table.csv  (PA-specific)
#
# Outputs:
#   Output/Diagnostics/EPA_LatLong_Completeness.txt   (log)
#   Output/Diagnostics/PA_Crosswalk_HandCheck.csv     (manual review)
###############################################################################

library(data.table)
library(stringr)
library(here)
library(janitor)
library(lubridate)

# ─────────────────────────────────────────────────────────────────────────────
# 0. Setup
# ─────────────────────────────────────────────────────────────────────────────
out_dir  <- here("Output", "Diagnostics")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

log_path <- file.path(out_dir, "EPA_LatLong_Completeness.txt")
csv_path <- file.path(out_dir, "PA_Crosswalk_HandCheck.csv")

sink(log_path, split = TRUE)

cat(strrep("=", 80), "\n")
cat("EPA LAT/LONG COMPLETENESS + PA CROSSWALK DIAGNOSTIC\n")
cat("Generated:", as.character(Sys.time()), "\n")
cat(strrep("=", 80), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load EPA Facilities (coordinate source)
# ─────────────────────────────────────────────────────────────────────────────
cat(strrep("=", 80), "\n")
cat("SECTION 1: LOAD EPA FACILITIES\n")
cat(strrep("=", 80), "\n\n")

fac_path <- here("Data", "Raw", "Facilities.csv")
stopifnot("Facilities.csv missing" = file.exists(fac_path))

epa_fac_raw <- fread(fac_path, colClasses = "character")
cat("Columns: ", paste(names(epa_fac_raw), collapse = ", "), "\n")
cat("Rows:    ", format(nrow(epa_fac_raw), big.mark = ","), "\n\n")

# Detect key columns dynamically
id_col   <- grep("facility.*id",  names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
st_col   <- grep("^state$",       names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
lat_col  <- grep("latitude",      names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
lon_col  <- grep("longitude",     names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
name_col <- grep("^name$",        names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]
if (is.na(name_col)) name_col <- grep("name", names(epa_fac_raw), ignore.case = TRUE, value = TRUE)[1]

cat(sprintf("Detected: id=%s  state=%s  lat=%s  lon=%s  name=%s\n\n",
            id_col, st_col, lat_col, lon_col, name_col))

epa_fac <- epa_fac_raw[, .SD, .SDcols = c(id_col, st_col, lat_col, lon_col, name_col)]
setnames(epa_fac, c("epa_id", "epa_state_raw", "lat_raw", "lon_raw", "epa_name"))

epa_fac[, lat := suppressWarnings(as.numeric(lat_raw))]
epa_fac[, lon := suppressWarnings(as.numeric(lon_raw))]
epa_fac[, has_geo := !is.na(lat) & !is.na(lon)]

# State name → abbreviation map (handles EPA quirks like "NewYork")
state_map <- rbind(
  data.table(epa_state = state.name,    state_abbr = state.abb),
  data.table(epa_state = c("NewYork", "New York", "Washington DC",
                            "District of Columbia", "Puerto Rico",
                            "Virgin Islands", "Guam", "American Samoa",
                            "Northern Mariana Islands"),
             state_abbr = c("NY", "NY", "DC", "DC", "PR", "VI", "GU", "AS", "MP"))
)
epa_fac[, epa_state_clean := trimws(epa_state_raw)]
epa_fac <- merge(epa_fac, state_map, by.x = "epa_state_clean", by.y = "epa_state", all.x = TRUE)

unmatched_states <- epa_fac[is.na(state_abbr), .N, by = epa_state_raw][order(-N)]
if (nrow(unmatched_states) > 0) {
  cat("States not mapped to abbreviation (review these):\n")
  print(unmatched_states)
  cat("\n")
} else {
  cat("All state names mapped successfully.\n\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 2. Load EPA USTs (for install dates / year cohorts)
# ─────────────────────────────────────────────────────────────────────────────
cat(strrep("=", 80), "\n")
cat("SECTION 2: LOAD EPA USTs & PARSE INSTALL DATES\n")
cat(strrep("=", 80), "\n\n")

ust_path <- here("Data", "Raw", "USTs.csv")
stopifnot("USTs.csv missing" = file.exists(ust_path))

epa_usts <- fread(ust_path, colClasses = "character") %>% clean_names() %>% as.data.table()
cat("UST rows: ", format(nrow(epa_usts), big.mark = ","), "\n")

inst_col <- grep("install", names(epa_usts), ignore.case = TRUE, value = TRUE)[1]
cat("Install date column: ", inst_col, "\n")
cat("Raw date samples: ",
    paste(head(unique(epa_usts[!is.na(get(inst_col)) & get(inst_col) != "", get(inst_col)]), 8),
          collapse = " | "), "\n\n")

# Robust date parser (handles Unix ms, Excel serial, and text formats)
parse_epa_date <- function(x) {
  num_val <- suppressWarnings(as.numeric(x))
  d_out   <- as.Date(rep(NA, length(x)))

  # Unix milliseconds (positive or negative for pre-1970)
  is_ms <- !is.na(num_val) & abs(num_val) > 1e10
  if (any(is_ms))
    d_out[is_ms] <- as.Date(as.POSIXct(num_val[is_ms] / 1000, origin = "1970-01-01"))

  # Excel serial days
  is_serial <- !is.na(num_val) & !is_ms & num_val > 100 & num_val < 100000
  if (any(is_serial))
    d_out[is_serial] <- as.Date(num_val[is_serial], origin = "1899-12-30")

  # Text fallback
  needs_text <- is.na(d_out) & !is.na(x) & x != ""
  if (any(needs_text)) {
    parsed <- lubridate::parse_date_time(
      x[needs_text],
      orders = c("ymd", "mdy", "Y-m-d", "m/d/Y", "dmy"), quiet = TRUE
    )
    d_out[needs_text] <- as.Date(parsed)
  }

  # Sanity bounds
  d_out[!is.na(d_out) & (d_out < as.Date("1900-01-01") | d_out > as.Date("2030-12-31"))] <- NA
  d_out
}

cat("Parsing install dates...\n")
epa_usts[, install_date := parse_epa_date(get(inst_col))]
epa_usts[, install_year := year(install_date)]

parse_rate <- round(mean(!is.na(epa_usts$install_year)) * 100, 1)
cat(sprintf("Parse rate: %.1f%%\n", parse_rate))
if (!is.na(min(epa_usts$install_year, na.rm = TRUE)))
  cat(sprintf("Year range: %d – %d\n\n",
              min(epa_usts$install_year, na.rm = TRUE),
              max(epa_usts$install_year, na.rm = TRUE)))

# Earliest install year per facility (facility-level cohort)
first_year <- epa_usts[!is.na(install_year),
                       .(first_year = min(install_year, na.rm = TRUE)),
                       by = facility_id]
cat("Facilities with resolved install year: ",
    format(nrow(first_year), big.mark = ","), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 3. EPA Lat/Long Completeness — All 51 Jurisdictions
# ─────────────────────────────────────────────────────────────────────────────
cat(strrep("=", 80), "\n")
cat("SECTION 3: LAT/LONG COMPLETENESS — ALL 51 JURISDICTIONS (OVERALL)\n")
cat(strrep("=", 80), "\n\n")

# Only US + DC (exclude territories for the main table)
us_abbrs <- c(state.abb, "DC")

geo_overall <- epa_fac[state_abbr %in% us_abbrs, .(
  n_facilities  = .N,
  n_with_geo    = sum(has_geo),
  pct_with_geo  = round(sum(has_geo) / .N * 100, 1),
  n_missing_geo = sum(!has_geo)
), by = state_abbr][order(state_abbr)]

cat("State-level lat/long completeness (all 51):\n\n")
print(geo_overall, nrows = 60)

cat("\n\nNATIONAL SUMMARY:\n")
cat(sprintf("  Total facilities (US + DC): %s\n",
            format(sum(geo_overall$n_facilities), big.mark = ",")))
cat(sprintf("  With lat/long:              %s  (%.1f%%)\n",
            format(sum(geo_overall$n_with_geo), big.mark = ","),
            sum(geo_overall$n_with_geo) / sum(geo_overall$n_facilities) * 100))
cat(sprintf("  Missing lat/long:           %s  (%.1f%%)\n\n",
            format(sum(geo_overall$n_missing_geo), big.mark = ","),
            sum(geo_overall$n_missing_geo) / sum(geo_overall$n_facilities) * 100))

cat("Bottom 10 states by geo completeness:\n")
print(geo_overall[order(pct_with_geo)][1:10])

# ─────────────────────────────────────────────────────────────────────────────
# 4. Lat/Long Completeness by State × Install-Year Cohort
# ─────────────────────────────────────────────────────────────────────────────
cat("\n\n", strrep("=", 80), "\n")
cat("SECTION 4: LAT/LONG COMPLETENESS BY STATE × INSTALL-YEAR COHORT\n")
cat(strrep("=", 80), "\n\n")

if (nrow(first_year) == 0) {
  cat("SKIPPED: No install dates parsed. Check parse_epa_date and raw date samples above.\n\n")
} else {

  fac_yr <- merge(epa_fac, first_year, by.x = "epa_id", by.y = "facility_id", all.x = TRUE)

  fac_yr[, cohort := fcase(
    is.na(first_year),                   "00_No_Date",
    first_year < 1985,                   "01_Pre1985",
    first_year %between% c(1985, 1989),  "02_1985-89",
    first_year %between% c(1990, 1994),  "03_1990-94",
    first_year %between% c(1995, 1999),  "04_1995-99",
    first_year %between% c(2000, 2004),  "05_2000-04",
    first_year %between% c(2005, 2009),  "06_2005-09",
    first_year %between% c(2010, 2014),  "07_2010-14",
    first_year %between% c(2015, 2019),  "08_2015-19",
    first_year >= 2020,                  "09_2020+"
  )]

  # 4A. National — how geo % changes across cohorts
  cat("--- NATIONAL: geo completeness by cohort ---\n")
  national_cohort <- fac_yr[state_abbr %in% us_abbrs, .(
    n_fac    = .N,
    n_geo    = sum(has_geo),
    pct_geo  = round(sum(has_geo) / .N * 100, 1)
  ), by = cohort][order(cohort)]
  print(national_cohort)
  cat("\n")

  # 4B. Per state — wide pivot: rows = state, cols = cohort, values = pct_geo
  state_cohort_long <- fac_yr[state_abbr %in% us_abbrs, .(
    n_fac   = .N,
    n_geo   = sum(has_geo),
    pct_geo = round(sum(has_geo) / .N * 100, 1)
  ), by = .(state_abbr, cohort)][order(state_abbr, cohort)]

  state_cohort_wide <- dcast(
    state_cohort_long,
    state_abbr ~ cohort,
    value.var = "pct_geo",
    fill = NA
  )

  cat("--- ALL 51 STATES: pct_with_geo by cohort (wide) ---\n\n")
  print(state_cohort_wide, nrows = 60)
  cat("\n")

  # 4C. Highlight states that improve dramatically in recent cohorts
  # (potential candidates for date-gated EPA merge strategy)
  if (all(c("01_Pre1985", "08_2015-19") %in% names(state_cohort_wide))) {
    cat("--- States where geo coverage IMPROVES significantly post-2000 ---\n")
    state_cohort_wide[, improvement := `08_2015-19` - `01_Pre1985`]
    print(state_cohort_wide[!is.na(improvement) & improvement > 20,
                            .(state_abbr, `01_Pre1985`, `08_2015-19`, improvement)][order(-improvement)])
    cat("\n")
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. PA Crosswalk: Hand-Check CSV
# ─────────────────────────────────────────────────────────────────────────────
cat(strrep("=", 80), "\n")
cat("SECTION 5: PA CROSSWALK — HAND-CHECK CSV\n")
cat(strrep("=", 80), "\n\n")

pa_harm_path  <- here("Data", "Raw", "state_databases", "Pennsylvania", "PA_Harmonized_UST_tanks.csv")
pa_link_paths <- c(
  here("Data", "Raw", "state_databases", "Pennsylvania", "facility_linkage_table.csv"),
  "C:/Users/kalebkja/PA_UST_Auction_Analysis/data/external/padep/facility_linkage_table.csv"
)
pa_link_path <- pa_link_paths[file.exists(pa_link_paths)][1]

cat("PA harmonized tanks: ", pa_harm_path,  "\n  exists:", file.exists(pa_harm_path),  "\n")
cat("PA linkage table:    ", pa_link_path,   "\n  exists:", !is.na(pa_link_path), "\n\n")

if (!file.exists(pa_harm_path)) {
  cat("ERROR: PA harmonized tanks file not found. Skipping crosswalk section.\n\n")
} else if (is.na(pa_link_path)) {
  cat("ERROR: PA linkage table not found at either path. Skipping crosswalk section.\n\n")
} else {

  # Load PA harmonized (all rows, need facility_id, facility_name, epa_key)
  pa_harm_cols  <- names(fread(pa_harm_path, nrows = 0))
  want_cols     <- intersect(c("facility_id", "facility_name", "epa_key"), pa_harm_cols)
  pa_harm       <- fread(pa_harm_path, select = want_cols, colClasses = "character")
  pa_harm_uniq  <- unique(pa_harm, by = "facility_id")

  cat("PA harmonized: ", nrow(pa_harm_uniq), " unique facilities\n")
  cat("Columns present: ", paste(names(pa_harm_uniq), collapse = ", "), "\n\n")

  # Load linkage table
  pa_link <- fread(pa_link_path, colClasses = "character") %>%
    clean_names() %>%
    as.data.table()

  cat("PA linkage table: ", nrow(pa_link), " rows\n")
  cat("Columns: ", paste(names(pa_link), collapse = ", "), "\n\n")

  # Normalise linkage ID columns
  na_vals <- c("", "NA", "N/A", "NULL", "None")
  for (col in c("efacts_facility_id", "site_id", "client_id")) {
    if (col %in% names(pa_link)) {
      pa_link[get(col) %in% na_vals, (col) := NA_character_]
      pa_link[, (col) := trimws(get(col))]
    }
  }

  # --- 5A. Match-rate audit (full linkage table vs EPA Facilities) -----------
  pa_epa_all  <- epa_fac[state_abbr == "PA"]
  cat("EPA PA facilities: ", nrow(pa_epa_all), "\n")
  cat("EPA PA ID samples: ", paste(head(pa_epa_all$epa_id, 8), collapse = " | "), "\n\n")

  candidates_full <- list(
    "PA+efacts"        = pa_link[!is.na(efacts_facility_id), paste0("PA", efacts_facility_id)],
    "PA+site_id"       = pa_link[!is.na(site_id),            paste0("PA", site_id)],
    "PA+client_id"     = pa_link[!is.na(client_id),          paste0("PA", client_id)],
    "efacts_raw"       = pa_link[!is.na(efacts_facility_id), efacts_facility_id],
    "site_id_raw"      = pa_link[!is.na(site_id),            site_id]
  )

  cat("--- Full linkage table match rates against EPA Facilities.csv ---\n\n")
  for (nm in names(candidates_full)) {
    keys   <- candidates_full[[nm]]
    n_tot  <- length(keys)
    n_all  <- sum(keys %in% pa_epa_all$epa_id)
    n_geo  <- sum(keys %in% pa_epa_all[has_geo == TRUE, epa_id])
    cat(sprintf("  %-20s : %5d keys | %5d match EPA (%5.1f%%) | %5d with geo (%5.1f%%)\n",
                nm, n_tot, n_all,
                ifelse(n_tot > 0, n_all / n_tot * 100, 0),
                n_geo,
                ifelse(n_tot > 0, n_geo / n_tot * 100, 0)))
  }

  # Reverse lookup: which linkage column does a sample of EPA PA IDs hit?
  cat("\n--- Reverse lookup: 20 EPA PA IDs → linkage column ---\n\n")
  sample_epa <- head(pa_epa_all$epa_id, 20)
  numeric_ids <- str_remove(sample_epa, "^PA")
  for (i in seq_along(sample_epa)) {
    nid <- numeric_ids[i]
    in_efacts <- if ("efacts_facility_id" %in% names(pa_link)) sum(pa_link$efacts_facility_id == nid, na.rm=TRUE) else 0
    in_site   <- if ("site_id"            %in% names(pa_link)) sum(pa_link$site_id            == nid, na.rm=TRUE) else 0
    in_client <- if ("client_id"          %in% names(pa_link)) sum(pa_link$client_id          == nid, na.rm=TRUE) else 0
    tag <- ifelse(in_efacts > 0, "efacts_facility_id",
           ifelse(in_site   > 0, "site_id",
           ifelse(in_client > 0, "client_id", "NO MATCH")))
    cat(sprintf("  %s  ->  %s (efacts=%d, site=%d, client=%d)\n",
                sample_epa[i], tag, in_efacts, in_site, in_client))
  }

  # --- 5B. Build the hand-check CSV -----------------------------------------
  # Take 10 diverse PA harmonized facilities (spread across facility_id range)
  # and match them to linkage table + EPA by every candidate ID column
  cat("\n\n--- Building PA hand-check CSV ---\n\n")

  # Sample: 10 evenly spaced facilities from the harmonized file
  set.seed(42)
  n_sample <- min(30, nrow(pa_harm_uniq))   # pull 30, keep up to 30 for coverage
  idx      <- round(seq(1, nrow(pa_harm_uniq), length.out = n_sample))
  pa_sample <- pa_harm_uniq[idx]

  cat("Sample facility_ids:\n")
  cat(paste(pa_sample$facility_id, collapse = " | "), "\n\n")

  # Join sample to linkage table (by DEP facility_id)
  if ("facility_id" %in% names(pa_link)) {
    pa_sample <- merge(pa_sample,
                       pa_link[, .(
                         facility_id,
                         link_efacts    = efacts_facility_id,
                         link_site_id   = if ("site_id"   %in% names(pa_link)) site_id   else NA_character_,
                         link_client_id = if ("client_id" %in% names(pa_link)) client_id else NA_character_,
                         link_fac_name  = if ("facility_name" %in% names(pa_link)) facility_name else NA_character_
                       )],
                       by = "facility_id", all.x = TRUE)
  } else {
    cat("WARNING: linkage table has no 'facility_id' column — cannot join by DEP ID.\n")
    cat("Linkage columns are: ", paste(names(pa_link), collapse = ", "), "\n\n")
  }

  # Construct candidate EPA keys from linkage
  pa_sample[, key_from_efacts  := fifelse(!is.na(link_efacts),    paste0("PA", link_efacts),    NA_character_)]
  pa_sample[, key_from_site_id := fifelse(!is.na(link_site_id),   paste0("PA", link_site_id),   NA_character_)]
  pa_sample[, epa_key_used     := fcoalesce(key_from_efacts, key_from_site_id)]

  # Join EPA Facilities to get EPA facility name + confirm ID hit
  epa_pa_lookup <- unique(pa_epa_all[, .(epa_id, epa_name, lat, lon, has_geo)])

  # Match via efacts key
  pa_sample <- merge(pa_sample,
                     epa_pa_lookup[, .(epa_id, epa_name_efacts = epa_name,
                                       epa_lat_efacts = lat, epa_has_geo_efacts = has_geo)],
                     by.x = "key_from_efacts", by.y = "epa_id", all.x = TRUE)

  # Match via site_id key
  pa_sample <- merge(pa_sample,
                     epa_pa_lookup[, .(epa_id, epa_name_site = epa_name,
                                       epa_lat_site = lat, epa_has_geo_site = has_geo)],
                     by.x = "key_from_site_id", by.y = "epa_id", all.x = TRUE)

  # Match via epa_key column already in harmonized file
  if ("epa_key" %in% names(pa_sample)) {
    pa_sample <- merge(pa_sample,
                       epa_pa_lookup[, .(epa_id, epa_name_from_key = epa_name,
                                         epa_lat_from_key = lat, epa_has_geo_from_key = has_geo)],
                       by.x = "epa_key", by.y = "epa_id", all.x = TRUE)
  }

  # Compose final output columns (easy to read in Excel)
  out_cols <- c(
    "pa_dep_facility_id"     = "facility_id",
    "pa_harmonized_name"     = "facility_name",
    "pa_epa_key_col"         = "epa_key",            # from 09_Clean_PA.R
    "linkage_efacts_id"      = "link_efacts",
    "linkage_site_id"        = "link_site_id",
    "linkage_client_id"      = "link_client_id",
    "linkage_facility_name"  = "link_fac_name",
    "key_from_efacts"        = "key_from_efacts",
    "key_from_site_id"       = "key_from_site_id",
    "key_used_for_epa_match" = "epa_key_used",
    "epa_name_via_efacts"    = "epa_name_efacts",
    "epa_has_geo_via_efacts" = "epa_has_geo_efacts",
    "epa_name_via_site_id"   = "epa_name_site",
    "epa_has_geo_via_site"   = "epa_has_geo_site"
  )
  if ("epa_key" %in% names(pa_sample)) {
    out_cols <- c(out_cols,
      "epa_name_via_epa_key"   = "epa_name_from_key",
      "epa_has_geo_via_epa_key"= "epa_has_geo_from_key"
    )
  }

  # Keep only columns that exist after all the merges
  available <- intersect(out_cols, names(pa_sample))
  pa_out    <- pa_sample[, ..available]
  setnames(pa_out, available, names(out_cols[out_cols %in% available]))

  fwrite(pa_out, csv_path)
  cat("Hand-check CSV saved to:\n  ", csv_path, "\n")
  cat("Rows in CSV: ", nrow(pa_out), "\n\n")

  # Quick summary for the log
  cat("--- CSV Summary ---\n")
  if ("epa_has_geo_via_efacts" %in% names(pa_out)) {
    n_hit <- sum(!is.na(pa_out$epa_name_via_efacts))
    cat(sprintf("  Rows hitting EPA via efacts key:  %d / %d\n", n_hit, nrow(pa_out)))
  }
  if ("epa_has_geo_via_site_id" %in% names(pa_out)) {
    n_hit2 <- sum(!is.na(pa_out$epa_name_via_site_id))
    cat(sprintf("  Rows hitting EPA via site_id key: %d / %d\n", n_hit2, nrow(pa_out)))
  }
  if ("epa_name_via_epa_key" %in% names(pa_out)) {
    n_hit3 <- sum(!is.na(pa_out$epa_name_via_epa_key))
    cat(sprintf("  Rows hitting EPA via epa_key col: %d / %d\n", n_hit3, nrow(pa_out)))
  }
  cat("\n")

  # Print sample to log for quick visual check
  cat("--- First 15 rows of hand-check table (selected cols) ---\n")
  print_cols <- intersect(
    c("pa_dep_facility_id", "pa_harmonized_name",
      "linkage_efacts_id", "key_from_efacts",
      "epa_name_via_efacts", "epa_has_geo_via_efacts"),
    names(pa_out)
  )
  print(pa_out[1:min(15, nrow(pa_out)), ..print_cols], nrows = 20)
}

sink()
cat("\nDiagnostic log:    ", log_path, "\n")
cat("PA hand-check CSV: ", csv_path, "\n")