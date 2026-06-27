#### 11_make_hte_vars.R ####
# Facility-level GIS HTE variables, FIXED AT TREATMENT (Census 2000; TX reform 1999),
# keyed on panel_id. Population / rural / urban / income / composition are measured
# at BASELINE (pre-treatment ⇒ not endogenous to the reform) — the correct vintage
# for HTE splits on a 1999-2020 panel (researcher 2026-06-25: "value at treatment").
#
# Competition is NOT here: it moves in t. The edge LINKAGES are exported separately
# (12_export_neighbor_edges.R) so active-competitors-in-year-t are computed ex post.
#
# Built locally; written to Output/ (git-tracked; Data/ is ignored) so it syncs to
# the server via `git pull` with no GIS run there.
# Output: Output/GIS/gis_hte_vars.csv  (one row per panel_id)
#   keys: panel_id, facility_id, state, tract_geoid_2000
#   pop:  pop_2000, pop_density_2000, pop_density_tertile (low/mid/high), low_pop_density
#   rural/urban: pct_rural_2000, rural_2000 (1 = tract majority-rural in 2000)
#   demog: med_hh_income_2000 (1999$), pct_minority_2000, pct_poverty_2000

source(here::here("Code", "GIS", "00_gis_config.R"))
suppressMessages({library(tidycensus); library(tigris); library(sf)})
options(tigris_use_cache = TRUE)
census_api_key(trimws(readLines(here::here("Code", "Analysis", "census_api_key"), warn = FALSE)[1]))

YEAR <- 2000L
SQM_PER_SQMI <- 2589988.110336

xw <- fread(here::here("Data", "Analysis", "_facility_xwalk_coords.csv"),
            colClasses = c(panel_id = "character", facility_id = "character", state = "character"))
xw <- xw[is.finite(latitude) & is.finite(longitude) &
         latitude %between% c(24, 50) & longitude %between% c(-125, -66)]

sf1_vars <- c(pop = "P001001", urban = "P002002", rural = "P002005",
              tot_eth = "P004001", nhwhite = "P004005")
sf3_vars <- c(medinc = "P053001", povtot = "P087001", povbelow = "P087002")

per_state <- function(st) {
  trsf <- get_decennial("tract", state = st, year = YEAR, sumfile = "sf1",
                        output = "wide", geometry = TRUE, variables = sf1_vars)
  trsf <- st_transform(trsf, CRS_ALBERS)
  trsf$area_sqmi <- as.numeric(st_area(trsf)) / SQM_PER_SQMI
  s3 <- as.data.table(get_decennial("tract", state = st, year = YEAR, sumfile = "sf3",
                        output = "wide", geometry = FALSE, variables = sf3_vars))
  tr <- as.data.table(st_drop_geometry(trsf))
  tr <- merge(tr, s3, by = "GEOID", all.x = TRUE)

  fx <- xw[state == st]
  if (!nrow(fx)) return(NULL)
  pts <- st_transform(st_as_sf(fx, coords = c("longitude", "latitude"), crs = CRS_WGS84), CRS_ALBERS)
  jn  <- st_join(pts, trsf["GEOID"], join = st_within, left = TRUE)
  res <- data.table(panel_id = fx$panel_id, facility_id = fx$facility_id, state = st,
                    tract_geoid_2000 = jn$GEOID)
  merge(res, tr[, .(tract_geoid_2000 = GEOID, pop, urban, rural, tot_eth, nhwhite,
                    area_sqmi, medinc, povtot, povbelow)],
        by = "tract_geoid_2000", all.x = TRUE)
}

log_gis(sprintf("Census %d baseline (at-treatment) for %d states...", YEAR, length(STUDY_STATES)))
out <- rbindlist(lapply(STUDY_STATES, function(s) { log_gis(s, 1); per_state(s) }),
                 use.names = TRUE, fill = TRUE)

out[, pop_2000          := pop]
out[, pop_density_2000  := fifelse(area_sqmi > 0, pop / area_sqmi, NA_real_)]
out[, pct_rural_2000    := fifelse(pop > 0, rural / pop, NA_real_)]
out[, rural_2000        := as.integer(pct_rural_2000 > 0.5)]
out[, pct_minority_2000 := fifelse(tot_eth > 0, 1 - nhwhite / tot_eth, NA_real_)]
out[, med_hh_income_2000 := medinc]
out[, pct_poverty_2000  := fifelse(povtot > 0, povbelow / povtot, NA_real_)]
qd <- quantile(out$pop_density_2000, c(1/3, 2/3), na.rm = TRUE)
out[, pop_density_tertile := fcase(is.na(pop_density_2000), NA_character_,
      pop_density_2000 <= qd[1], "low", pop_density_2000 <= qd[2], "mid", default = "high")]
out[, low_pop_density := as.integer(pop_density_tertile == "low")]

fin <- unique(out[, .(panel_id, facility_id, state, tract_geoid_2000,
                      pop_2000, pop_density_2000, pop_density_tertile, low_pop_density,
                      pct_rural_2000, rural_2000,
                      med_hh_income_2000, pct_minority_2000, pct_poverty_2000)], by = "panel_id")

hte_dir <- here::here("Output", "GIS"); if (!dir.exists(hte_dir)) dir.create(hte_dir, recursive = TRUE)
fwrite(fin, file.path(hte_dir, "gis_hte_vars.csv"))
log_gis(sprintf("HTE (Census 2000 at-treatment): %s facilities | tract-matched %.1f%% | rural=%.1f%% | low_pop=%.1f%%",
        format(nrow(fin), big.mark = ","), 100*mean(!is.na(fin$tract_geoid_2000)),
        100*mean(fin$rural_2000, na.rm = TRUE), 100*mean(fin$low_pop_density, na.rm = TRUE)))
