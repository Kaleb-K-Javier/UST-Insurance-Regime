#### 09_demographics_ej.R ####
# Module 9: tract-level demographics for each UST facility — the "who / where is
# impacted" layer. ACS 5-year (2018-2022) joined to facilities via the 2020 tract
# GEOID from Module 01.
#   median household income, tract population, % minority (1 - non-Hispanic white),
#   % below poverty, median home value.
# EJScreen environmental-justice indices are a separate EPA block-group download
# (TODO, see end). No Dewey.
#
# Key: read from Code/Analysis/census_api_key (in-script, so this runs under
#      `Rscript --vanilla`). The key is NEVER printed.
#
# Output: gis_09_demographics.csv
#   facility_id, state, tract_geoid_2020, acs_year, med_hh_income, tract_pop,
#   pct_minority, pct_poverty, med_home_value

source(here::here("Code", "GIS", "00_gis_config.R"))
suppressMessages(library(tidycensus))

ACS_YEAR <- 2022L   # ACS 5-year 2018-2022

KEY_PATH <- here::here("Code", "Analysis", "census_api_key")
stopifnot("census_api_key file missing" = file.exists(KEY_PATH))
.key <- trimws(readLines(KEY_PATH, warn = FALSE)[1])
stopifnot("census key looks too short" = nchar(.key) >= 20)
census_api_key(.key)   # session only (install = FALSE); does not echo the key

CENSUS_OUT <- file.path(GIS_OUT_DIR, "gis_01_census_geography.csv")
stopifnot("Module 01 output missing — run 01 first" = file.exists(CENSUS_OUT))

#### S1 Pull ACS (tract, study states) ####
acs_vars <- c(
  med_hh_income  = "B19013_001",   # median household income
  pop            = "B01003_001",   # total population
  race_total     = "B03002_001",   # race/ethnicity universe
  white_nh       = "B03002_003",   # non-Hispanic white alone
  pov_univ       = "B17001_001",   # poverty status universe
  pov_below      = "B17001_002",   # income below poverty
  med_home_value = "B25077_001"    # median value, owner-occupied
)

log_gis(sprintf("Pulling ACS %d 5-year (tract) for %d states...", ACS_YEAR, length(STUDY_STATES)))
acs <- get_acs(geography = "tract", variables = acs_vars, state = STUDY_STATES,
               year = ACS_YEAR, survey = "acs5", output = "wide", geometry = FALSE)
setDT(acs)

acs[, pct_minority := fifelse(race_totalE > 0, 1 - white_nhE / race_totalE, NA_real_)]
acs[, pct_poverty  := fifelse(pov_univE  > 0, pov_belowE / pov_univE,       NA_real_)]
acs_slim <- acs[, .(
  tract_geoid_2020 = GEOID,
  med_hh_income    = med_hh_incomeE,
  tract_pop        = popE,
  pct_minority,
  pct_poverty,
  med_home_value   = med_home_valueE
)]
log_gis(sprintf("ACS tracts pulled: %s", format(nrow(acs_slim), big.mark = ",")), 1)

#### S2 Join to facilities via 2020 tract GEOID ####
cen <- fread(CENSUS_OUT, select = c("facility_id", "state", "tract_geoid_2020"),
             colClasses = c(facility_id = "character", state = "character",
                            tract_geoid_2020 = "character"))
out <- merge(cen, acs_slim, by = "tract_geoid_2020", all.x = TRUE)
out[, acs_year := ACS_YEAR]
setcolorder(out, c("facility_id", "state", "tract_geoid_2020", "acs_year",
                   "med_hh_income", "tract_pop", "pct_minority", "pct_poverty",
                   "med_home_value"))

#### S3 Diagnostics + save ####
log_gis(sprintf("Matched ACS income: %.1f%% | median of tract med_hh_income: $%s",
                100 * mean(!is.na(out$med_hh_income)),
                format(round(median(out$med_hh_income, na.rm = TRUE)), big.mark = ",")))
write_gis_lookup(out, "gis_09_demographics.csv")
log_gis("Module 09 complete.")

# TODO EJScreen: EPA publishes EJScreen at BLOCK-GROUP level (separate download,
# https://www.epa.gov/ejscreen) — join via bg_geoid_2020 from Module 01 for the
# environmental-justice indices (PM2.5, traffic proximity, EJ index percentiles).
