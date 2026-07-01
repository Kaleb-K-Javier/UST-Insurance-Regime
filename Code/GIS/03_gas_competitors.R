#### 03_gas_competitors.R ####
# Motor-fuel COMPETITOR count within 1 mile per facility, for the thin_market HTE.
# n_1609m (Module 02) counts ALL USTs within a mile; competition = other GASOLINE retailers only.
# Method: take the 1-mile all-UST edge list, keep neighbors that are gasoline retailers, count per
#   focal facility. is_gas = "ever had a gasoline tank" (facility_gas_flag.csv, built on the server
#   from facility_biography.total_gasoline_ever). Left-joins to the analysis sample by panel_id.
#
# Inputs (all local):
#   Data/Processed/GIS/gis_02_neighbor_edges.csv  (state, facility_id_i, facility_id_j, dist_m; <=1609m, i<j)
#   Data/Analysis/_facility_xwalk_coords.csv      (panel_id, facility_id, state)
#   Output/GIS/facility_gas_flag.csv              (panel_id, is_gas)   [server-derived, one-off]
# Output:
#   Output/GIS/gis_gas_competitors.csv  (panel_id, n_all_1609m, n_gas_1609m, is_gas)  -- committed lookup

suppressPackageStartupMessages({ library(data.table); library(here) })
R1MI <- 1609.34

ed <- fread(here("Data", "Processed", "GIS", "gis_02_neighbor_edges.csv"),
            colClasses = list(character = c("state", "facility_id_i", "facility_id_j")))
ed <- ed[dist_m <= R1MI]
xw <- unique(fread(here("Data", "Analysis", "_facility_xwalk_coords.csv"),
                   select = c("panel_id", "facility_id", "state"),
                   colClasses = list(character = c("panel_id", "facility_id", "state"))),
             by = c("facility_id", "state"))
gf <- fread(here("Output", "GIS", "facility_gas_flag.csv"),
            colClasses = list(character = "panel_id"))

# map each endpoint (facility_id, state) -> panel_id
ed <- merge(ed, xw[, .(facility_id, state, pid_i = panel_id)],
            by.x = c("facility_id_i", "state"), by.y = c("facility_id", "state"), all.x = TRUE)
ed <- merge(ed, xw[, .(facility_id, state, pid_j = panel_id)],
            by.x = c("facility_id_j", "state"), by.y = c("facility_id", "state"), all.x = TRUE)
cat(sprintf("edges=%s | endpoint i mapped=%.1f%% | j mapped=%.1f%%\n",
            format(nrow(ed), big.mark = ","),
            100 * mean(!is.na(ed$pid_i)), 100 * mean(!is.na(ed$pid_j))))

# undirected edge (i<j) contributes a neighbor to BOTH endpoints
dd <- rbindlist(list(ed[, .(focal = pid_i, nbr = pid_j)],
                     ed[, .(focal = pid_j, nbr = pid_i)]))
dd <- dd[!is.na(focal)]
dd[gf, nbr_gas := i.is_gas, on = c(nbr = "panel_id")]   # neighbor's gasoline flag (NA if unmapped)

cnt <- dd[, .(n_all_1609m = .N,
              n_gas_1609m = sum(nbr_gas == 1L, na.rm = TRUE)), by = focal]
setnames(cnt, "focal", "panel_id")

# base = analysis coord universe; isolated facilities -> 0; attach the facility's OWN is_gas
out <- merge(unique(xw[, .(panel_id)]), cnt, by = "panel_id", all.x = TRUE)
setnafill(out, fill = 0L, cols = c("n_all_1609m", "n_gas_1609m"))
out <- merge(out, gf, by = "panel_id", all.x = TRUE)
out[is.na(is_gas), is_gas := 0L]

fwrite(out, here("Output", "GIS", "gis_gas_competitors.csv"))
cat(sprintf("gis_gas_competitors.csv: %s facilities | mean n_all=%.1f mean n_gas=%.1f\n",
            format(nrow(out), big.mark = ","), mean(out$n_all_1609m), mean(out$n_gas_1609m)))

# distribution of the competition measure (drives the thin_market cutoff)
x <- out$n_gas_1609m
cat("\nn_gas_1609m quantiles (ALL facilities):\n")
print(round(quantile(x, c(0, .1, .25, .5, .6, .667, .75, .9, .95, .99, 1)), 1))
for (k in 0:5) cat(sprintf("  share n_gas <= %d : %.3f  (== %d : %.3f)\n", k, mean(x <= k), k, mean(x == k)))
xg <- out[is_gas == 1L, n_gas_1609m]
cat(sprintf("\namong GASOLINE focals (n=%s):\n", format(length(xg), big.mark = ",")))
print(round(quantile(xg, c(0, .25, .5, .6, .667, .75, .9, .95, 1)), 1))
for (k in 0:3) cat(sprintf("  share n_gas <= %d : %.3f\n", k, mean(xg <= k)))
