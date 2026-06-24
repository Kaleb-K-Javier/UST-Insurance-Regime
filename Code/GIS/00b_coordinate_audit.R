#### 00b_coordinate_audit.R ####
# Deliverable 0 (prerequisite for ALL distance modules):
#   Per-state facility-level coordinate coverage. A distance task (neighbors,
#   wells) is only trustworthy where coordinates are present, so this audit gates
#   modules 02 and 04. No Dewey; reads the raw master (pre-dedup) so the % is the
#   true geocoding rate, not the post-filter rate.
#
# Output: gis_00_coord_coverage.csv
#   state, n_fac, n_geo, pct_geo, study_state

source(here::here("Code", "GIS", "00_gis_config.R"))

log_gis("Loading raw master for coverage audit...")
dt <- fread(MASTER_TANKS_PATH,
            select = c("facility_id", "state", "latitude", "longitude"),
            colClasses = c(facility_id = "character", state = "character"))
dt[, lat := as.numeric(latitude)]
dt[, lon := as.numeric(longitude)]

# Facility is "geocoded" if ANY of its tank rows carries a valid CONUS coordinate
fac <- dt[, .(geo = any(is.finite(lat) & is.finite(lon) &
                        lat %between% c(24, 50) & lon %between% c(-125, -66))),
          by = .(facility_id, state)]

cov <- fac[, .(n_fac = .N, n_geo = sum(geo)), by = state][order(state)]
cov[, pct_geo := round(100 * n_geo / n_fac, 1)]
cov[, study_state := state %in% STUDY_STATES]

fwrite(cov, file.path(GIS_OUT_DIR, "gis_00_coord_coverage.csv"))

ss <- cov[study_state == TRUE]
log_gis(sprintf("Saved gis_00_coord_coverage.csv | %d states | study-state coverage %.1f%% (%s/%s)",
                nrow(cov),
                100 * sum(ss$n_geo) / sum(ss$n_fac),
                format(sum(ss$n_geo), big.mark = ","),
                format(sum(ss$n_fac), big.mark = ",")))
# Flag thin study states (< 95%) for the distance-module caveat
thin <- ss[pct_geo < 95][order(pct_geo)]
if (nrow(thin) > 0) {
  log_gis("Study states below 95% coverage (interpret distance metrics with caution):", 1)
  print(thin[, .(state, n_fac, n_geo, pct_geo)])
}
log_gis("Module 00b complete.")
