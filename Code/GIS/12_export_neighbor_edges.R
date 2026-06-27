#### 12_export_neighbor_edges.R ####
# Export facility neighbor EDGE LINKAGES (which facility is matched to which, +
# distance), tagged with panel_id on BOTH ends, so the RF/structural session can
# build TIME-VARYING competition (active competitors in year t) EX POST by crossing
# these edges with the panel's active-by-year status. Geography is time-invariant;
# only active status varies, so the edge set is a one-time export.
#
# Written to Output/ (git-tracked) as parquet (~4M rows; compact) + a tiny csv head
# so the schema is human-readable. Server reads parquet via arrow.
# Output: Output/GIS/gis_neighbor_edges.parquet (+ gis_neighbor_edges_sample.csv)
#   cols: panel_id_i, panel_id_j, state, dist_m   (undirected, i<j, <= 1 mile)

source(here::here("Code", "GIS", "00_gis_config.R"))
suppressMessages(library(arrow))

ed <- fread(file.path(GIS_OUT_DIR, "gis_02_neighbor_edges.csv"),
            colClasses = c(state = "character", facility_id_i = "character", facility_id_j = "character"))
xw <- fread(here::here("Data", "Analysis", "_facility_xwalk_coords.csv"),
            select = c("panel_id", "facility_id", "state"),
            colClasses = c(panel_id = "character", facility_id = "character", state = "character"))
xw <- unique(xw, by = c("facility_id", "state"))

ed <- merge(ed, xw[, .(facility_id_i = facility_id, state, panel_id_i = panel_id)],
            by = c("facility_id_i", "state"), all.x = TRUE)
ed <- merge(ed, xw[, .(facility_id_j = facility_id, state, panel_id_j = panel_id)],
            by = c("facility_id_j", "state"), all.x = TRUE)

out <- ed[, .(panel_id_i, panel_id_j, state, dist_m = round(dist_m, 1))]
odir <- here::here("Output", "GIS"); if (!dir.exists(odir)) dir.create(odir, recursive = TRUE)
arrow::write_parquet(out, file.path(odir, "gis_neighbor_edges.parquet"))
fwrite(head(out, 50), file.path(odir, "gis_neighbor_edges_sample.csv"))

log_gis(sprintf("Edges: %s rows | both panel_id matched %.1f%% | unmatched endpoints %s -> Output/GIS/gis_neighbor_edges.parquet",
        format(nrow(out), big.mark = ","),
        100 * mean(!is.na(out$panel_id_i) & !is.na(out$panel_id_j)),
        format(sum(is.na(out$panel_id_i) | is.na(out$panel_id_j)), big.mark = ",")))
