#### 03_urban_rural.R ####
# Module 3: Urban/rural classification of each UST facility.
#
# Two complementary measures (SCOPE Q2):
#   PRIMARY   USDA RUCA 2010 (gradient, tract-level) — joined via the 2010 tract
#             GEOID from Module 01. Primary RUCA: 1-3 metro, 4-6 micro, 7-9 small
#             town, 10 rural.
#   SECONDARY Census 2020 Urban Area (binary) — point-in-polygon of the facility
#             against the national 2020 UA layer (tigris).
# No Dewey. Depends on: gis_01_census_geography.csv (tract_geoid_2010) + the RUCA
# workbook in Data/Raw/GIS/.
#
# Output: gis_03_urban_rural.csv
#   facility_id, state, ruca_primary, ruca_secondary, ruca_class, ruca_metro,
#   tract_pop_density_2010, urban_area_2020, ua_name_2020

source(here::here("Code", "GIS", "00_gis_config.R"))
library(tigris)
options(tigris_use_cache = TRUE)
suppressMessages(library(readxl))

RUCA_PATH  <- file.path(GIS_RAW_DIR, "ruca2010_revised.xlsx")
CENSUS_OUT <- file.path(GIS_OUT_DIR, "gis_01_census_geography.csv")
stopifnot("Module 01 output missing — run 01_census_geography.R first" = file.exists(CENSUS_OUT))
stopifnot("RUCA workbook missing in Data/Raw/GIS/" = file.exists(RUCA_PATH))

#### S1 RUCA (read; header is row 2 — skip the errata row) ####
log_gis("Reading USDA RUCA 2010...")
raw <- as.data.table(read_excel(RUCA_PATH, sheet = "Data", skip = 1, .name_repair = "minimal"))
nm <- names(raw)
col_tract <- nm[grepl("Tract FIPS",          nm, ignore.case = TRUE)][1]
col_pri   <- nm[grepl("Primary RUCA",        nm, ignore.case = TRUE)][1]
col_sec   <- nm[grepl("Secondary RUCA",      nm, ignore.case = TRUE)][1]
col_den   <- nm[grepl("Population Density",   nm, ignore.case = TRUE)][1]
stopifnot(!is.na(col_tract), !is.na(col_pri), !is.na(col_sec), !is.na(col_den))

ruca <- raw[, .(
  tract_geoid_2010       = sprintf("%011.0f", as.numeric(trimws(as.character(get(col_tract))))),
  ruca_primary           = as.numeric(get(col_pri)),
  ruca_secondary         = as.numeric(get(col_sec)),
  tract_pop_density_2010 = as.numeric(get(col_den))
)]
ruca <- ruca[!is.na(ruca_primary) & nchar(tract_geoid_2010) == 11]
ruca[, ruca_class := fifelse(ruca_primary %in% 1:3, "metro",
                      fifelse(ruca_primary %in% 4:6, "micro",
                       fifelse(ruca_primary %in% 7:9, "small_town",
                        fifelse(ruca_primary == 10,   "rural", NA_character_))))]
ruca[, ruca_metro := as.integer(ruca_primary %in% 1:3)]
log_gis(sprintf("RUCA tracts: %s | class dist: %s", format(nrow(ruca), big.mark = ","),
                paste(names(table(ruca$ruca_class)), table(ruca$ruca_class), sep = "=", collapse = " ")), 1)

#### S2 Census 2020 Urban Areas (binary, point-in-poly) ####
log_gis("Downloading Census 2020 Urban Areas...")
ua <- tigris::urban_areas(year = 2020, class = "sf")
ua <- st_transform(ua, CRS_WGS84)
ua_name_col <- grep("^NAME", names(ua), value = TRUE)[1]
ua_slim <- ua[, ua_name_col]
names(ua_slim)[1] <- "ua_name_2020"

fac <- load_master_coords()
fac_sf <- coords_to_sf(fac, crs_out = CRS_WGS84)
log_gis("Joining facilities to urban areas...", 1)
joined <- st_join(fac_sf, ua_slim, join = st_within, left = TRUE)
ua_dt <- data.table(facility_id = joined$facility_id, state = joined$state,
                    ua_name_2020 = joined$ua_name_2020)
ua_dt <- unique(ua_dt, by = c("facility_id", "state"))
ua_dt[, urban_area_2020 := as.integer(!is.na(ua_name_2020))]

#### S3 Merge RUCA (via tract GEOID) + UA, key on facility ####
cen <- fread(CENSUS_OUT, select = c("facility_id", "state", "tract_geoid_2010"),
             colClasses = c(facility_id = "character", state = "character",
                            tract_geoid_2010 = "character"))
out <- merge(cen, ruca[, .(tract_geoid_2010, ruca_primary, ruca_secondary,
                           ruca_class, ruca_metro, tract_pop_density_2010)],
             by = "tract_geoid_2010", all.x = TRUE)
out <- merge(out, ua_dt, by = c("facility_id", "state"), all.x = TRUE)
out[, tract_geoid_2010 := NULL]

#### S4 Diagnostics + save ####
log_gis(sprintf("RUCA matched: %.1f%% | in 2020 UA: %.1f%%",
                100 * mean(!is.na(out$ruca_primary)),
                100 * mean(out$urban_area_2020 == 1, na.rm = TRUE)))
write_gis_lookup(out, "gis_03_urban_rural.csv")
log_gis("Module 03 complete.")
