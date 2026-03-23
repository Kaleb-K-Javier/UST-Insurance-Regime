#### 00_gis_config.R ####
# Shared configuration for all GIS enrichment modules.
# Source this at the top of every script in Code/GIS/.
#
# Convention:
#   - All spatial operations use EPSG:5070 (Albers Equal Area) for distance/area
#   - All coordinate storage uses EPSG:4326 (WGS84)
#   - All outputs keyed on (facility_id, state) as character
#   - All outputs saved to GIS_OUT_DIR as CSV lookup tables

library(data.table)
library(sf)
library(here)

#### S1 Paths ####

# Input: master harmonized tank file (output of 10_Master_Cleaning)
MASTER_TANKS_PATH <- here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv")

# Input: raw reference data directory
GIS_RAW_DIR <- here("Data", "Raw", "GIS")

# Output: all GIS lookup tables land here
GIS_OUT_DIR <- here("Data", "Processed", "GIS")
if (!dir.exists(GIS_OUT_DIR)) dir.create(GIS_OUT_DIR, recursive = TRUE)

# Scratch directory for large intermediate files (rasters, geodatabases)
GIS_SCRATCH_DIR <- here("Data", "Scratch", "GIS")
if (!dir.exists(GIS_SCRATCH_DIR)) dir.create(GIS_SCRATCH_DIR, recursive = TRUE)

#### S2 CRS Constants ####

CRS_WGS84  <- 4326L
CRS_ALBERS <- 5070L   # NAD83 / Conus Albers (meters) — for distance calculations
CRS_MERC   <- 3857L   # Web Mercator — only used for detecting unconverted coords

#### S3 Study States ####

# Full set of states in the panel (26 states per memory)
# Target + Tier 1 controls + Tier 2 controls
STUDY_STATES <- c(
  "TX",                                                           # Target
  "AL", "AR", "CO", "LA", "ME", "NJ", "NM", "OK", "PA", "TN",   # Tier 1
  "ID", "KS", "KY", "MD", "MA", "MN", "NC", "ND", "OH", "VA",   # Tier 2
  "AZ", "CT", "FL", "IA", "MI"                                   # Treated/excluded
)

#### S4 Helper Functions ####

load_master_coords <- function(states = STUDY_STATES) {
  # Load master tank file, deduplicate to facility level, filter to valid coords.
  # Returns data.table with one row per (facility_id, state) with lat/lon.
  
  dt <- fread(MASTER_TANKS_PATH, select = c(
    "facility_id", "state", "latitude", "longitude"
  ), colClasses = c(facility_id = "character", state = "character"))
  
  # Filter to study states
  dt <- dt[state %in% states]
  
  # Collapse to facility level (take first non-NA coordinate per facility)
  dt <- dt[!is.na(latitude) & !is.na(longitude)]
  dt <- dt[, .(
    latitude  = latitude[1],
    longitude = longitude[1]
  ), by = .(facility_id, state)]
  
  # Coordinate quality gate
  dt <- dt[
    between(latitude, 24, 50) &    # CONUS latitude bounds
    between(longitude, -125, -66)   # CONUS longitude bounds
  ]
  
  message(sprintf(
    "Loaded %s facilities with valid coordinates across %d states",
    format(nrow(dt), big.mark = ","), dt[, uniqueN(state)]
  ))
  
  return(dt)
}


coords_to_sf <- function(dt, crs_out = CRS_ALBERS) {
  # Convert data.table with latitude/longitude to sf object.
  # Input must have columns: latitude, longitude
  # Returns sf object in crs_out projection.
  
  sf_obj <- st_as_sf(dt, coords = c("longitude", "latitude"), crs = CRS_WGS84)
  if (crs_out != CRS_WGS84) {
    sf_obj <- st_transform(sf_obj, crs_out)
  }
  return(sf_obj)
}


write_gis_lookup <- function(dt, filename, key_cols = c("facility_id", "state")) {
  # Write a GIS lookup table to the standard output directory.
  # Validates key uniqueness before writing.
  
  stopifnot(all(key_cols %in% names(dt)))
  
  n_dups <- nrow(dt) - uniqueN(dt, by = key_cols)
  if (n_dups > 0) {
    warning(sprintf(
      "%s: %d duplicate keys detected. Deduplicating (keeping first).",
      filename, n_dups
    ))
    dt <- unique(dt, by = key_cols)
  }
  
  out_path <- file.path(GIS_OUT_DIR, filename)
  fwrite(dt, out_path)
  message(sprintf("Wrote %s (%s rows, %d cols)",
                  filename,
                  format(nrow(dt), big.mark = ","),
                  ncol(dt)))
  
  return(invisible(out_path))
}


log_gis <- function(msg, level = 0) {
  # Consistent logging with indentation
  prefix <- paste0(strrep("  ", level), ifelse(level > 0, "- ", ""))
  message(sprintf("[GIS] %s%s", prefix, msg))
}
