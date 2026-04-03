#### 01_SafeGraph_Crosswalk.R ####
# Build UST panel -> SafeGraph Global Places crosswalk.
#
# Strategy:
#   For each UST facility with valid coordinates, find the k=5 nearest
#   SafeGraph POIs within the same state using a k-d tree (FNN package).
#   Exact Haversine distances computed in meters for the k candidates only.
#   No NAICS pre-filter -- all POI types are candidates; researcher applies
#   thresholds post-hoc using match_rank, dist_m, and match_confidence.
#
# Inputs:
#   - Master harmonized tank file (via load_master_coords())
#   - SafeGraph Global Places parquet files (local Dewey download)
#
# Outputs:
#   - GIS/ust_safegraph_xwalk.csv       (full crosswalk, no geometry)
#   - GIS/ust_safegraph_xwalk_geom.csv  (polygon WKT for within-100m matches)
#
# Key: (facility_id, state, placekey) -- up to k=5 rows per facility
#
# Section order:
#   S1  Parameters
#   S2  Load UST coords + facility name
#   S3  Load SafeGraph from parquet (no WKT)
#   S4  FNN k-d tree spatial match
#   S5  Attach SafeGraph attributes
#   S6  Radius flags (within_50m, within_100m)
#   S7  Name similarity scoring (needs S6 flags)
#   S8  Column order and sort
#   S9  Diagnostics
#   S10 Pull polygon WKT for within-100m matches
#   S11 Write outputs
#
# Requires: 00_gis_config.R sourced first.

source(here::here("Code", "GIS", "00_gis_config.R"))

library(DBI)
library(duckdb)
library(FNN)
library(geodist)
library(stringdist)

#### S1 Parameters ####

SG_PARQUET_DIR <- "C:/Users/kalebkja/dewey-downloads/dewey-downloads/dewey-downloads/safegraph_places"
K_NEAREST      <- 5L
RADIUS_TIGHT_M <- 50
RADIUS_BROAD_M <- 100

SAMPLE_STATES <- c(
  "AR", "CO", "ID", "KS", "KY", "LA", "MA", "MD", "ME",
  "MN", "MO", "NC", "OH", "OK", "SD", "TN", "VA"
)

#### S2 Load UST Facility Coordinates ####

log_gis("Loading UST facility coordinates")
ust <- load_master_coords(states = SAMPLE_STATES)
log_gis(sprintf("UST facilities: %s", format(nrow(ust), big.mark = ",")), level = 1)
log_gis(sprintf("Columns: %s", paste(names(ust), collapse = ", ")), level = 1)

#### S3 Load SafeGraph from Parquet via DuckDB ####
# Polygon WKT excluded -- pulled separately for confirmed matches only.

log_gis("Loading SafeGraph Global Places from parquet")

sg_path           <- file.path(SG_PARQUET_DIR, "*.snappy.parquet")
sg_path           <- gsub("\\\\", "/", sg_path)
sample_states_sql <- paste0("'", SAMPLE_STATES, "'", collapse = ",")

con <- dbConnect(duckdb(), dbdir = ":memory:")
dbExecute(con, "SET threads = 4;")
dbExecute(con, "SET memory_limit = '8GB';")

sg <- as.data.table(dbGetQuery(con, sprintf("
  SELECT
    PLACEKEY              AS placekey,
    LOCATION_NAME         AS location_name,
    STREET_ADDRESS        AS street_address,
    CITY                  AS city,
    REGION                AS region,
    POSTAL_CODE           AS postal_code,
    NAICS_CODE            AS naics_code,
    TOP_CATEGORY          AS top_category,
    SUB_CATEGORY          AS sub_category,
    OPENED_ON             AS opened_on,
    CLOSED_ON             AS closed_on,
    TRACKING_CLOSED_SINCE AS tracking_closed_since,
    GEOMETRY_TYPE         AS geometry_type,
    POLYGON_CLASS         AS polygon_class,
    IS_SYNTHETIC          AS is_synthetic,
    WKT_AREA_SQ_METERS    AS wkt_area_sq_meters,
    LATITUDE              AS latitude,
    LONGITUDE             AS longitude
  FROM read_parquet('%s')
  WHERE ISO_COUNTRY_CODE = 'US'
    AND REGION IN (%s)
    AND LATITUDE  IS NOT NULL
    AND LONGITUDE IS NOT NULL
", sg_path, sample_states_sql)))

dbDisconnect(con, shutdown = TRUE)

log_gis(sprintf("SafeGraph POIs loaded: %s", format(nrow(sg), big.mark = ",")), level = 1)
log_gis(sprintf("SafeGraph object size: %s", format(object.size(sg), units = "MB")), level = 1)

#### S4 K-Nearest Spatial Match via FNN k-d Tree ####
# One state at a time to bound peak memory.
# FNN::get.knnx builds a k-d tree on SafeGraph coords, queries UST.
# O(n log n). k-d tree in degree space; exact meter distances via geodist.

log_gis("Running k=5 nearest neighbor join via FNN k-d tree (state by state)")

results <- vector("list", length(SAMPLE_STATES))

for (i in seq_along(SAMPLE_STATES)) {

  st     <- SAMPLE_STATES[i]
  ust_st <- ust[state  == st]
  sg_st  <- sg[region == st]

  if (nrow(ust_st) == 0 || nrow(sg_st) == 0) {
    log_gis(sprintf("%s: skipped (UST=%d, SG=%d)",
                    st, nrow(ust_st), nrow(sg_st)), level = 1)
    next
  }

  k <- min(K_NEAREST, nrow(sg_st))

  knn_result <- FNN::get.knnx(
    data  = as.matrix(sg_st[,  .(longitude, latitude)]),
    query = as.matrix(ust_st[, .(longitude, latitude)]),
    k     = k
  )

  n_ust <- nrow(ust_st)

  dt <- data.table(
    ust_row    = rep(seq_len(n_ust), each = k),
    sg_row     = as.vector(t(knn_result$nn.index)),
    match_rank = rep(seq_len(k), times = n_ust)
  )

  dt[, facility_id   := ust_st$facility_id[ust_row]]
  dt[, state         := st]
  dt[, facility_name := ust_st$facility_name[ust_row]]
  dt[, ust_lat       := ust_st$latitude[ust_row]]
  dt[, ust_lon       := ust_st$longitude[ust_row]]

  dt[, placekey := sg_st$placekey[sg_row]]
  dt[, sg_lat   := sg_st$latitude[sg_row]]
  dt[, sg_lon   := sg_st$longitude[sg_row]]

  dt[, dist_m := round(geodist::geodist_vec(
    x1      = ust_lon, y1 = ust_lat,
    x2      = sg_lon,  y2 = sg_lat,
    paired  = TRUE,
    measure = "haversine"
  ), 1)]

  dt[, c("ust_row", "sg_row") := NULL]

  results[[i]] <- dt

  log_gis(sprintf(
    "%s: %s UST | k=%d | %s pairs | dist p50=%.0fm p99=%.0fm",
    st,
    format(n_ust,    big.mark = ","),
    k,
    format(nrow(dt), big.mark = ","),
    median(dt$dist_m),
    quantile(dt$dist_m, 0.99)
  ), level = 1)

  rm(knn_result, dt, ust_st, sg_st)
  gc()
}

xwalk_raw <- rbindlist(results, use.names = TRUE, fill = TRUE)
rm(results)
gc()

log_gis(sprintf("Total candidate pairs: %s", format(nrow(xwalk_raw), big.mark = ",")))

#### S5 Attach SafeGraph Attributes ####

log_gis("Attaching SafeGraph attributes to candidate pairs")

sg_attrs <- sg[, .(
  placekey, location_name, street_address, city, region,
  postal_code, naics_code, top_category, sub_category,
  opened_on, closed_on, tracking_closed_since,
  geometry_type, polygon_class, is_synthetic, wkt_area_sq_meters
)]

xwalk <- merge(xwalk_raw, sg_attrs, by = "placekey", all.x = TRUE)
rm(xwalk_raw, sg_attrs)
gc()

#### S6 Radius and Polygon Quality Flags ####
# Must run before S7 name scoring -- name scoring uses within_50m/within_100m.

xwalk[, within_50m  := dist_m <= RADIUS_TIGHT_M]
xwalk[, within_100m := dist_m <= RADIUS_BROAD_M]

xwalk[, low_confidence_polygon :=
        (!is.na(is_synthetic)  & is_synthetic  == TRUE) |
        (!is.na(polygon_class) & polygon_class == "SHARED_POLYGON")]

# tracking_closed_since is Date type.
# 2019-07-01 is SafeGraph coverage start date, not a true closure signal.
xwalk[, confirmed_closure :=
        (!is.na(tracking_closed_since) &
         tracking_closed_since > as.Date("2019-07-01"))]

#### S7 Name Similarity Scoring ####
# Jaro-Winkler distance: 0 = identical, 1 = completely different.
# Robust to abbreviations and word-order variation in business names.
# Requires within_50m and within_100m from S6.

log_gis("Computing Jaro-Winkler name similarity scores")

clean_name <- function(x) {
  x <- toupper(x)
  x <- gsub("[^A-Z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

xwalk[, ust_name_clean := clean_name(facility_name)]
xwalk[, sg_name_clean  := clean_name(location_name)]

xwalk[, name_dist_jw := fifelse(
  !is.na(ust_name_clean) & !is.na(sg_name_clean) &
    nchar(ust_name_clean) > 0 & nchar(sg_name_clean) > 0,
  stringdist(ust_name_clean, sg_name_clean, method = "jw"),
  NA_real_
)]

# jw < 0.10: near-identical  (e.g. "SHELL" vs "SHELL OIL")
# jw < 0.20: plausible match (e.g. "BP" vs "BP STATION")
# jw >= 0.20: likely different business
xwalk[, name_match_flag := fcase(
  name_dist_jw <  0.10, "strong",
  name_dist_jw <  0.20, "plausible",
  name_dist_jw >= 0.20, "weak",
  is.na(name_dist_jw),  "missing"
)]

# Composite spatial + name confidence
# high:     within 50m  AND strong name match
# medium:   within 100m AND strong or plausible name match
# low:      within 100m but weak or missing name
# no_match: beyond 100m
xwalk[, match_confidence := fcase(
  within_50m  == TRUE & name_match_flag == "strong",                   "high",
  within_100m == TRUE & name_match_flag %in% c("strong", "plausible"), "medium",
  within_100m == TRUE,                                                  "low",
  default = "no_match"
)]

xwalk[, c("ust_name_clean", "sg_name_clean") := NULL]

#### S8 Column Order and Key Sort ####

setcolorder(xwalk, c(
  "facility_id", "state", "placekey", "match_rank", "dist_m",
  "within_50m", "within_100m", "match_confidence",
  "ust_lat", "ust_lon",
  "sg_lat", "sg_lon",
  "facility_name", "location_name",
  "name_dist_jw", "name_match_flag",
  "street_address", "city", "region", "postal_code",
  "naics_code", "top_category", "sub_category",
  "opened_on", "closed_on", "tracking_closed_since", "confirmed_closure",
  "geometry_type", "polygon_class", "is_synthetic",
  "low_confidence_polygon", "wkt_area_sq_meters"
))

setorder(xwalk, state, facility_id, match_rank)

#### S9 Diagnostics ####

log_gis("Match diagnostics")

n_total_ust   <- ust[, uniqueN(paste(facility_id, state))]
n_matched_50  <- xwalk[within_50m  == TRUE, uniqueN(paste(facility_id, state))]
n_matched_100 <- xwalk[within_100m == TRUE, uniqueN(paste(facility_id, state))]

log_gis(sprintf("UST facilities:           %s",
                format(n_total_ust, big.mark = ",")), level = 1)
log_gis(sprintf("Matched within  50m:      %s (%.1f%%)",
                format(n_matched_50,  big.mark = ","),
                100 * n_matched_50  / n_total_ust), level = 1)
log_gis(sprintf("Matched within 100m:      %s (%.1f%%)",
                format(n_matched_100, big.mark = ","),
                100 * n_matched_100 / n_total_ust), level = 1)

log_gis("Distance distribution -- rank=1 (meters):", level = 1)
print(xwalk[match_rank == 1, .(
  p10 = quantile(dist_m, 0.10),
  p25 = quantile(dist_m, 0.25),
  p50 = quantile(dist_m, 0.50),
  p75 = quantile(dist_m, 0.75),
  p90 = quantile(dist_m, 0.90),
  p99 = quantile(dist_m, 0.99),
  max = max(dist_m)
)])

log_gis("Name match distribution (rank=1):", level = 1)
print(xwalk[match_rank == 1, .N, by = name_match_flag][order(-N)])

log_gis("Composite confidence distribution (rank=1):", level = 1)
print(xwalk[match_rank == 1, .N, by = match_confidence][order(-N)])

log_gis("High confidence sample (rank=1):", level = 1)
print(xwalk[match_rank == 1 & match_confidence == "high",
            .(facility_id, state, facility_name, location_name,
              dist_m, name_dist_jw, naics_code)][1:20])

log_gis("Top NAICS codes -- rank=1 nearest:", level = 1)
print(xwalk[match_rank == 1, .N, by = .(naics_code, top_category)][order(-N)][1:20])

log_gis("State-level match rates (within 100m):", level = 1)
state_diag <- merge(
  ust[, .(n_ust = .N), by = state],
  xwalk[within_100m == TRUE & match_rank == 1, .(n_matched = .N), by = state],
  by = "state", all.x = TRUE
)
state_diag[is.na(n_matched), n_matched := 0L]
state_diag[, match_rate := round(100 * n_matched / n_ust, 1)]
print(state_diag[order(-match_rate)])

#### S10 Pull Polygon WKT for Within-100m Matches ####

log_gis("Pulling polygon WKT for within-100m matches")

matched_keys <- xwalk[within_100m == TRUE, unique(placekey)]
log_gis(sprintf("Unique PLACEKEYs within 100m: %s",
                format(length(matched_keys), big.mark = ",")), level = 1)

con <- dbConnect(duckdb(), dbdir = ":memory:")
dbExecute(con, "SET threads = 4;")
dbExecute(con, "SET memory_limit = '8GB';")
dbWriteTable(con, "matched_keys", data.table(placekey = matched_keys))

sg_wkt <- as.data.table(dbGetQuery(con, sprintf("
  SELECT PLACEKEY AS placekey, POLYGON_WKT AS polygon_wkt
  FROM read_parquet('%s')
  WHERE PLACEKEY IN (SELECT placekey FROM matched_keys)
", sg_path)))

dbDisconnect(con, shutdown = TRUE)
log_gis(sprintf("WKT records retrieved: %s",
                format(nrow(sg_wkt), big.mark = ",")), level = 1)

#### S11 Write Outputs ####

log_gis("Writing outputs")

# Primary crosswalk -- no geometry
write_gis_lookup(
  xwalk,
  "ust_safegraph_xwalk.csv",
  key_cols = c("facility_id", "state", "placekey")
)

# Geometry file -- within-100m matches only
xwalk_geom <- merge(
  xwalk[within_100m == TRUE,
        .(facility_id, state, placekey, match_rank, dist_m,
          within_50m, within_100m, match_confidence)],
  sg_wkt,
  by    = "placekey",
  all.x = TRUE
)
setorder(xwalk_geom, state, facility_id, match_rank)

write_gis_lookup(
  xwalk_geom,
  "ust_safegraph_xwalk_geom.csv",
  key_cols = c("facility_id", "state", "placekey")
)

log_gis("SafeGraph crosswalk complete")