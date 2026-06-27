#### 10_merge_gis_lookups.R ####
# Join all non-Dewey GIS facility lookups into ONE facility-level covariate table
# for the reduced form. Spine = gis_01 (every GIS-covered facility in the 18-state
# estimation-sample state set). Adds panel_id (the analysis key) from the crosswalk
# so it joins straight to the DiD panel on panel_id (or facility_id+state).
#
# Output: Data/Processed/GIS/gis_facility_covariates.csv  (one row per facility_id+state)
#   keys: facility_id, state, panel_id
#   census: tract/bg GEOID (2020 & 2010)
#   competition: n_400m, n_800m, n_1609m
#   urban/rural: ruca_primary/secondary/class/metro, tract_pop_density_2010,
#                urban_area_2020, ua_name_2020
#   demographics: med_hh_income, tract_pop, pct_minority, pct_poverty, med_home_value

source(here::here("Code", "GIS", "00_gis_config.R"))
G <- GIS_OUT_DIR
rd <- function(f, sel = NULL) fread(file.path(G, f),
       colClasses = c(facility_id = "character", state = "character"), select = sel)

cen <- rd("gis_01_census_geography.csv")
nb  <- rd("gis_02_neighbor_counts.csv")
ur  <- rd("gis_03_urban_rural.csv")
# drop gis_09's tract_geoid_2020 (already in gis_01) to avoid a merge collision
dem <- rd("gis_09_demographics.csv",
          sel = c("facility_id", "state", "med_hh_income", "tract_pop",
                  "pct_minority", "pct_poverty", "med_home_value"))

out <- Reduce(function(a, b) merge(a, b, by = c("facility_id", "state"), all.x = TRUE),
              list(cen, nb, ur, dem))

# attach panel_id (analysis key) from the crosswalk
xw <- fread(here::here("Data", "Analysis", "_facility_xwalk_coords.csv"),
            select = c("panel_id", "facility_id", "state"),
            colClasses = c(panel_id = "character", facility_id = "character", state = "character"))
xw <- unique(xw, by = c("facility_id", "state"))
out <- merge(out, xw, by = c("facility_id", "state"), all.x = TRUE)
setcolorder(out, c("facility_id", "state", "panel_id"))

stopifnot(uniqueN(out, by = c("facility_id", "state")) == nrow(out))
write_gis_lookup(out, "gis_facility_covariates.csv")
log_gis(sprintf("Merged covariates: %s facilities | %d cols | panel_id matched %.1f%% | states %d",
                format(nrow(out), big.mark = ","), ncol(out),
                100 * mean(!is.na(out$panel_id)), uniqueN(out$state)))
log_gis(sprintf("ACS income non-NA %.1f%% | RUCA class non-NA %.1f%% | neighbors non-NA %.1f%%",
                100*mean(!is.na(out$med_hh_income)), 100*mean(!is.na(out$ruca_class)),
                100*mean(!is.na(out$n_1609m))))
