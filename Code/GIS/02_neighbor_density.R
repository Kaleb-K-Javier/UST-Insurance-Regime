#### 02_neighbor_density.R ####
# Module 2: Local competition / neighbor density around each UST facility.
#
# Method (CA recipe, ca_gis_neighbors.R, generalized to national scale):
#   Project facility points to EPSG:5070 (Albers Equal Area, meters). Run the
#   within-distance self-join STATE BY STATE to bound the O(n^2) cost. A neighbor
#   is any OTHER facility within radius r. Emit (a) per-facility counts at
#   {400, 800, 1609} m (~0.25 / 0.5 / 1 mile) and (b) the within-state edge list
#   (facility_i, facility_j, dist_m) at the 1-mile cap for spillover work.
#   Distances are planar in EPSG:5070 (metric, equal-area); error < 0.1% at <2 km.
#   Needs only lat/long -> NO Dewey.
#
# Inputs:  Master_Harmonized_UST_Tanks.csv (via load_master_coords())
# Outputs:
#   gis_02_neighbor_counts.csv : facility_id, state, n_400m, n_800m, n_1609m
#   gis_02_neighbor_edges.csv  : state, facility_id_i, facility_id_j, dist_m  (<=1609m, i<j)

source(here::here("Code", "GIS", "00_gis_config.R"))

RADII <- c(n_400m = 400, n_800m = 800, n_1609m = 1609.34)   # ~0.25 / 0.5 / 1 mile
MAX_R <- max(RADII)

log_gis("Loading facility coordinates...")
fac <- load_master_coords()
fac <- fac[, .(facility_id, state, latitude, longitude)]

counts_list <- list()
edges_list  <- list()

states <- sort(unique(fac$state))
for (s in states) {
  sub <- fac[state == s]
  n   <- nrow(sub)
  if (n < 2L) {
    counts_list[[s]] <- data.table(facility_id = sub$facility_id, state = s,
                                   n_400m = 0L, n_800m = 0L, n_1609m = 0L)
    next
  }

  sf_s <- coords_to_sf(sub, crs_out = CRS_ALBERS)
  nb   <- sf::st_is_within_distance(sf_s, sf_s, dist = MAX_R)   # includes self
  xy   <- sf::st_coordinates(sf_s)

  # Directed pairs (i<j) within MAX_R, with planar metric distance
  ii <- rep.int(seq_len(n), lengths(nb))
  jj <- unlist(nb, use.names = FALSE)
  keep <- ii < jj
  ii <- ii[keep]; jj <- jj[keep]
  d  <- sqrt((xy[ii, 1] - xy[jj, 1])^2 + (xy[ii, 2] - xy[jj, 2])^2)
  ok <- d <= MAX_R
  ii <- ii[ok]; jj <- jj[ok]; d <- d[ok]

  edges_list[[s]] <- data.table(
    state          = s,
    facility_id_i  = sub$facility_id[ii],
    facility_id_j  = sub$facility_id[jj],
    dist_m         = round(d, 1)
  )

  # Symmetrise to count neighbors per facility at each radius
  idx <- c(ii, jj); dd <- c(d, d)
  agg <- data.table(idx = idx, d = dd)[, .(
    n_400m  = sum(d <= RADII["n_400m"]),
    n_800m  = sum(d <= RADII["n_800m"]),
    n_1609m = sum(d <= RADII["n_1609m"])
  ), by = idx]

  out <- data.table(idx = seq_len(n), facility_id = sub$facility_id, state = s)
  out <- merge(out, agg, by = "idx", all.x = TRUE)
  setnafill(out, fill = 0L, cols = c("n_400m", "n_800m", "n_1609m"))
  out[, idx := NULL]
  counts_list[[s]] <- out

  log_gis(sprintf("%s: %s facilities | %s edges <=1mi | median n_1609m=%d",
                  s, format(n, big.mark = ","),
                  format(nrow(edges_list[[s]]), big.mark = ","),
                  as.integer(median(out$n_1609m))), 1)
}

counts <- rbindlist(counts_list, use.names = TRUE)
edges  <- rbindlist(edges_list,  use.names = TRUE)

# Validation
stopifnot(uniqueN(counts, by = c("facility_id", "state")) == nrow(counts))
stopifnot(all(counts$n_400m <= counts$n_800m), all(counts$n_800m <= counts$n_1609m))
stopifnot(all(edges$dist_m >= 0), all(edges$dist_m <= MAX_R + 1))

write_gis_lookup(counts, "gis_02_neighbor_counts.csv")
fwrite(edges, file.path(GIS_OUT_DIR, "gis_02_neighbor_edges.csv"))
log_gis(sprintf("Edges written: %s rows (<=1 mile, i<j)", format(nrow(edges), big.mark = ",")))
log_gis(sprintf("Counts: %s facilities | mean within 1mi = %.1f",
                format(nrow(counts), big.mark = ","), mean(counts$n_1609m)))
log_gis("Module 02 complete.")
