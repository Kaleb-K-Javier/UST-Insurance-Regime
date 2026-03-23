#### 01_census_geography.R ####
# Module 1: Assign Census tract GEOID and block group GEOID to each facility.
#
# Purpose:
#   Census tract (11-digit) and block group (12-digit) GEOIDs are the merge
#   keys for RUCA codes, EJScreen, ACS population density, and any other
#   tract/BG-level product. This module must run before Modules 4 and 5.
#
# Method:
#   Point-in-polygon spatial join of facility coordinates to TIGER/Line
#   tract and block group shapefiles. Uses vintage-matched boundaries:
#     - 2010 TIGER for pre-2020 analysis
#     - 2020 TIGER for 2020+ analysis
#   Default: 2020 vintage (most facilities are geocoded to current addresses).
#
# Input:
#   Master_Harmonized_UST_Tanks.csv (via load_master_coords())
#
# Output:
#   gis_01_census_geography.csv
#     Columns: facility_id, state, tract_geoid_2020, bg_geoid_2020,
#              tract_geoid_2010, bg_geoid_2010

source(here::here("Code", "GIS", "00_gis_config.R"))

library(tigris)
options(tigris_use_cache = TRUE)

#### S1 Configuration ####

# Which Census vintage(s) to assign
# Both are needed for the vintage-matched RUCA crosswalk (Obj 1)
CENSUS_VINTAGES <- c(2020L, 2010L)

# FIPS codes for study states (tigris uses FIPS, not abbreviations)
state_fips_map <- tigris::fips_codes[, c("state", "state_code")]
state_fips_map <- unique(state_fips_map)
setDT(state_fips_map)
setnames(state_fips_map, c("state_abbr", "state_fips"))
study_fips <- state_fips_map[state_abbr %in% STUDY_STATES, state_fips]

#### S2 Load Facility Coordinates ####

log_gis("Loading facility coordinates...")
fac_dt <- load_master_coords()
fac_sf <- coords_to_sf(fac_dt, crs_out = CRS_WGS84)  # Keep WGS84 for tract join

#### S3 Download and Join Census Boundaries ####

run_census_join <- function(vintage, fac_sf) {
  log_gis(sprintf("Processing Census %d vintage...", vintage))
  
  suffix <- as.character(vintage)
  
  # --- 3a: Download tract boundaries ---
  log_gis(sprintf("Downloading %d tract boundaries for %d states...",
                  vintage, length(study_fips)), 1)
  
  tracts_list <- lapply(study_fips, function(fp) {
    tryCatch(
      tigris::tracts(state = fp, cb = TRUE, year = vintage, class = "sf"),
      error = function(e) {
        log_gis(sprintf("Failed to download tracts for FIPS %s: %s", fp, e$message), 2)
        return(NULL)
      }
    )
  })
  tracts_sf <- do.call(rbind, Filter(Negate(is.null), tracts_list))
  tracts_sf <- st_transform(tracts_sf, CRS_WGS84)
  
  log_gis(sprintf("Loaded %s tracts", format(nrow(tracts_sf), big.mark = ",")), 1)
  
  # --- 3b: Download block group boundaries ---
  log_gis(sprintf("Downloading %d block group boundaries for %d states...",
                  vintage, length(study_fips)), 1)
  
  bgs_list <- lapply(study_fips, function(fp) {
    tryCatch(
      tigris::block_groups(state = fp, cb = TRUE, year = vintage, class = "sf"),
      error = function(e) {
        log_gis(sprintf("Failed to download BGs for FIPS %s: %s", fp, e$message), 2)
        return(NULL)
      }
    )
  })
  bgs_sf <- do.call(rbind, Filter(Negate(is.null), bgs_list))
  bgs_sf <- st_transform(bgs_sf, CRS_WGS84)
  
  log_gis(sprintf("Loaded %s block groups", format(nrow(bgs_sf), big.mark = ",")), 1)
  
  # --- 3c: Spatial join — tracts ---
  log_gis("Joining facilities to tracts...", 1)
  
  # Select only GEOID from tracts to keep the join clean
  tracts_slim <- tracts_sf[, "GEOID"]
  joined_tracts <- st_join(fac_sf, tracts_slim, join = st_within, left = TRUE)
  
  tract_col <- paste0("tract_geoid_", suffix)
  
  # Extract as data.table
  tract_result <- data.table(
    facility_id = joined_tracts$facility_id,
    state       = joined_tracts$state,
    geoid       = joined_tracts$GEOID
  )
  setnames(tract_result, "geoid", tract_col)
  
  # Handle duplicates from overlapping polygons (rare with cb=TRUE, but defensive)
  tract_result <- unique(tract_result, by = c("facility_id", "state"))
  
  # --- 3d: Spatial join — block groups ---
  log_gis("Joining facilities to block groups...", 1)
  
  bgs_slim <- bgs_sf[, "GEOID"]
  joined_bgs <- st_join(fac_sf, bgs_slim, join = st_within, left = TRUE)
  
  bg_col <- paste0("bg_geoid_", suffix)
  
  bg_result <- data.table(
    facility_id = joined_bgs$facility_id,
    state       = joined_bgs$state,
    geoid       = joined_bgs$GEOID
  )
  setnames(bg_result, "geoid", bg_col)
  bg_result <- unique(bg_result, by = c("facility_id", "state"))
  
  # --- 3e: Merge tract + BG ---
  result <- merge(tract_result, bg_result, by = c("facility_id", "state"), all = TRUE)
  
  # --- 3f: Diagnostics ---
  n_total <- nrow(result)
  n_tract_na <- sum(is.na(result[[tract_col]]))
  n_bg_na    <- sum(is.na(result[[bg_col]]))
  
  log_gis(sprintf("Census %d results: %s facilities", vintage,
                  format(n_total, big.mark = ",")), 1)
  log_gis(sprintf("  Tract GEOID missing: %s (%.1f%%)",
                  format(n_tract_na, big.mark = ","),
                  100 * n_tract_na / n_total), 2)
  log_gis(sprintf("  BG GEOID missing: %s (%.1f%%)",
                  format(n_bg_na, big.mark = ","),
                  100 * n_bg_na / n_total), 2)
  
  return(result)
}


#### S4 Execute Joins ####

results_list <- list()

for (v in CENSUS_VINTAGES) {
  results_list[[as.character(v)]] <- run_census_join(v, fac_sf)
}

# Merge all vintages into single table
final_dt <- results_list[[1]]
if (length(results_list) > 1) {
  for (i in 2:length(results_list)) {
    final_dt <- merge(final_dt, results_list[[i]],
                      by = c("facility_id", "state"), all = TRUE)
  }
}


#### S5 Validation ####

log_gis("Running validation checks...")

# Check 1: GEOID format (tract = 11 digits, BG = 12 digits)
for (v in CENSUS_VINTAGES) {
  tc <- paste0("tract_geoid_", v)
  bc <- paste0("bg_geoid_", v)
  
  bad_tract <- sum(nchar(final_dt[[tc]]) != 11, na.rm = TRUE)
  bad_bg    <- sum(nchar(final_dt[[bc]]) != 12, na.rm = TRUE)
  
  if (bad_tract > 0) warning(sprintf("%d tract GEOIDs have wrong length for %d", bad_tract, v))
  if (bad_bg > 0)    warning(sprintf("%d BG GEOIDs have wrong length for %d", bad_bg, v))
}

# Check 2: BG should nest within tract (first 11 chars of BG = tract)
for (v in CENSUS_VINTAGES) {
  tc <- paste0("tract_geoid_", v)
  bc <- paste0("bg_geoid_", v)
  
  valid <- !is.na(final_dt[[tc]]) & !is.na(final_dt[[bc]])
  if (sum(valid) > 0) {
    mismatched <- sum(substr(final_dt[[bc]][valid], 1, 11) != final_dt[[tc]][valid])
    if (mismatched > 0) {
      warning(sprintf("%d facilities have BG GEOID inconsistent with tract GEOID for %d",
                      mismatched, v))
    } else {
      log_gis(sprintf("Census %d: All BG GEOIDs nest correctly within tracts", v), 1)
    }
  }
}

# Check 3: Coverage by state
log_gis("Coverage by state (2020 vintage):")
coverage <- final_dt[, .(
  total    = .N,
  has_tract = sum(!is.na(tract_geoid_2020)),
  pct      = round(100 * sum(!is.na(tract_geoid_2020)) / .N, 1)
), by = state][order(pct)]
print(coverage)


#### S6 Save ####

write_gis_lookup(final_dt, "gis_01_census_geography.csv")

log_gis("Module 01 complete.")
