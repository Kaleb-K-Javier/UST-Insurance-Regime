#### qc_spotcheck_sample.R ####
# A ~100-facility spot-check sample drawn from the REDUCED-FORM ESTIMATION SAMPLE
# (make-model primary facilities), NOT the full master universe. Lets you verify
# the regression sample: (1) does the lat/long land on the actual UST site?
# (2) do neighbor EDGES connect to genuinely adjacent facilities? — tagged with
# treated/control, make-model cell, wall/fuel/age-bin, and closure.
#
# Sample definition (mirrors 01m_MakeModelSample / 01h funnel Stage 4):
#   make_model_fac present & oldest_age_bin present & wall!=Unknown & fuel!=Unknown.
# Sources: facility_make_model_reform.csv (cells), facility_biography.csv (treated/
#   state/closure), _facility_xwalk_coords.csv (panel_id->facility_id+lat/long).
#
# Outputs (Data/Processed/GIS/qc/): gis_qc_sample.csv + gis_qc_map.html. No API key.
# NOTE: gis_02 competition/edges cover the 26 GIS STUDY_STATES, which currently MISS
#   MO & SD (in the estimation sample) — those show NA neighbor cols / no edges until
#   the GIS layers are re-run on the estimation-sample state set.

source(here::here("Code", "GIS", "00_gis_config.R"))
suppressMessages(library(jsonlite))
set.seed(42)
N_SAMPLE <- 100L
EDGE_MAX <- 805
qc_dir <- file.path(GIS_OUT_DIR, "qc"); if (!dir.exists(qc_dir)) dir.create(qc_dir, recursive = TRUE)
ANA <- here::here("Data", "Analysis")

## 1) estimation sample (make-model primary) ----
mm <- fread(file.path(ANA, "facility_make_model_reform.csv"), colClasses = c(panel_id = "character"))
mm[, mm_primary := !is.na(make_model_fac) & make_model_fac != "" & !is.na(oldest_age_bin) &
     !grepl("Unknown", fac_wall_reform) & !grepl("Unknown", fac_fuel_reform)]
mm <- mm[mm_primary == TRUE, .(panel_id, make_model_fac, fac_wall_reform,
                               fac_fuel_reform, oldest_age_bin, n_tanks_at_reform)]

bio <- fread(file.path(ANA, "facility_biography.csv"),
             colClasses = c(panel_id = "character", state = "character"))
bio[, facility_closed := as.integer(total_tanks_ever > 0 & total_tanks_closed >= total_tanks_ever)]
bio <- bio[, .(panel_id, texas_treated, state, total_tanks_ever, total_tanks_closed, facility_closed)]

xw <- fread(file.path(ANA, "_facility_xwalk_coords.csv"),
            colClasses = c(panel_id = "character", facility_id = "character", state = "character"))
xw <- xw[, .(panel_id, facility_id, lat = latitude, lon = longitude)]

est <- merge(mm, bio, by = "panel_id")
est <- merge(est, xw, by = "panel_id")
est <- est[is.finite(lat) & is.finite(lon) & lat %between% c(24, 50) & lon %between% c(-125, -66)]

## facility names from master ----
nm <- fread(MASTER_TANKS_PATH, select = c("facility_id", "state", "facility_name"),
            colClasses = c(facility_id = "character", state = "character"))
nm <- unique(nm, by = c("facility_id", "state"))
est <- merge(est, nm, by = c("facility_id", "state"), all.x = TRUE)
est[, skey := paste(facility_id, state)]

## neighbor counts (gis_02) where available ----
nb <- fread(file.path(GIS_OUT_DIR, "gis_02_neighbor_counts.csv"),
            colClasses = c(facility_id = "character", state = "character"))
est <- merge(est, nb, by = c("facility_id", "state"), all.x = TRUE)

log_gis(sprintf("Estimation sample: %s facilities (TX=%s, control=%s) | with gis_02 coverage: %.1f%%",
                format(nrow(est), big.mark = ","), format(sum(est$texas_treated == 1), big.mark = ","),
                format(sum(est$texas_treated == 0), big.mark = ","),
                100 * mean(!is.na(est$n_1609m))))

## 2) stratified sample: 40 TX + 60 control ----
take <- function(d, n) if (nrow(d) <= n) d else d[sample(.N, n)]
samp <- rbind(take(est[texas_treated == 1], 40L), take(est[texas_treated == 0], 60L))
samp <- unique(samp, by = "panel_id"); if (nrow(samp) > N_SAMPLE) samp <- samp[sample(.N, N_SAMPLE)]
samp[, group := fifelse(texas_treated == 1, "TX (treated)", "control")]

## 3) links + nearest neighbor (gis_02 edges; state-aware key) ----
samp[, gmaps   := sprintf("https://www.google.com/maps/search/?api=1&query=%.6f,%.6f", lat, lon)]
samp[, gsat    := sprintf("https://www.google.com/maps/@%.6f,%.6f,19z/data=!3m1!1e3", lat, lon)]
samp[, gstreet := sprintf("https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=%.6f,%.6f", lat, lon)]

ed <- fread(file.path(GIS_OUT_DIR, "gis_02_neighbor_edges.csv"),
            colClasses = c(state = "character",
                           facility_id_i = "character", facility_id_j = "character"))
ed[, skey_i := paste(facility_id_i, state)][, skey_j := paste(facility_id_j, state)]
sset <- samp$skey
ed_s <- ed[skey_i %in% sset | skey_j %in% sset]
long <- rbind(ed_s[skey_i %in% sset, .(skey = skey_i, nbr = facility_id_j, state, dist_m)],
              ed_s[skey_j %in% sset, .(skey = skey_j, nbr = facility_id_i, state, dist_m)])
nn <- long[order(dist_m), .SD[1L], by = skey]
samp <- merge(samp, nn[, .(skey, nearest_nbr = nbr, nearest_dist_m = round(dist_m, 1))],
              by = "skey", all.x = TRUE)

## 4) recording sheet ----
samp[, `:=`(qc_on_site = "", qc_point_quality = "", qc_edge_ok = "", qc_treated_ok = "", qc_notes = "")]
ord <- c("panel_id", "facility_id", "state", "group", "texas_treated", "facility_name",
         "lat", "lon", "make_model_fac", "fac_wall_reform", "fac_fuel_reform", "oldest_age_bin",
         "facility_closed", "n_tanks_at_reform", "n_400m", "n_800m", "n_1609m",
         "nearest_nbr", "nearest_dist_m", "gmaps", "gsat", "gstreet",
         "qc_on_site", "qc_point_quality", "qc_edge_ok", "qc_treated_ok", "qc_notes")
fwrite(samp[, ..ord], file.path(qc_dir, "gis_qc_estsample.csv"))

## 5) overview satellite map (color by fuel; closed=white dashed ring; popup=treated+cell) ----
# edge endpoint coords from a master facility_id+state -> lat/lon lookup
mc <- fread(MASTER_TANKS_PATH, select = c("facility_id", "state", "latitude", "longitude"),
            colClasses = c(facility_id = "character", state = "character"))
mc[, `:=`(lat = as.numeric(latitude), lon = as.numeric(longitude))]
mc <- unique(mc[is.finite(lat) & is.finite(lon)], by = c("facility_id", "state"))
mc[, skey := paste(facility_id, state)]
clu <- mc[, .(skey, lat, lon)]

ed_draw <- ed_s[dist_m <= EDGE_MAX]
ci <- clu[data.table(skey = ed_draw$skey_i), on = "skey"]
cj <- clu[data.table(skey = ed_draw$skey_j), on = "skey"]
edge_feats <- data.table(lat1 = ci$lat, lon1 = ci$lon, lat2 = cj$lat, lon2 = cj$lon)
edge_feats <- edge_feats[is.finite(lat1) & is.finite(lon1) & is.finite(lat2) & is.finite(lon2)]

pts <- samp[, .(facility_id, state, facility_name, lat, lon, group,
                fac_fuel_reform, facility_closed, make_model_fac, n_1609m, nearest_dist_m, gmaps)]
pts_json   <- toJSON(pts, dataframe = "rows", auto_unbox = TRUE, na = "null")
edges_json <- toJSON(edge_feats, dataframe = "rows", auto_unbox = TRUE)
html <- sprintf('<!DOCTYPE html><html><head><meta charset="utf-8"><title>UST RF estimation-sample QC</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>html,body,#map{height:100%%;margin:0} .legend{background:#fff;padding:6px 8px;font:12px sans-serif;line-height:18px}</style>
</head><body><div id="map"></div><script>
var pts=%s, edges=%s;
var map=L.map("map").setView([33,-95],6);
L.tileLayer("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
 {maxZoom:19,attribution:"Esri World Imagery"}).addTo(map);
edges.forEach(function(e){L.polyline([[e.lat1,e.lon1],[e.lat2,e.lon2]],{color:"yellow",weight:1,opacity:0.7}).addTo(map);});
function fcol(f){return f=="Gasoline-Only"?"#22ff22":(f=="Diesel-Only"?"#ff4444":"#ffd400");}
var g=L.featureGroup();
pts.forEach(function(p){var c=fcol(p.fac_fuel_reform); var closed=p.facility_closed==1;
 var m=L.circleMarker([p.lat,p.lon],{radius:6,fillColor:c,fillOpacity:closed?0.45:0.95,color:closed?"#fff":"#000",weight:closed?2.5:1,dashArray:closed?"2 3":null});
 m.bindPopup("<b>"+(p.facility_name||"(no name)")+"</b><br>"+p.facility_id+" ("+p.state+") &middot; <b>"+p.group+"</b><br>"+
  (closed?"<span style=\\"color:#c30\\">CLOSED</span>":"<span style=\\"color:#080\\">OPEN</span>")+
  " | fuel="+p.fac_fuel_reform+"<br>cell="+p.make_model_fac+"<br>nbrs@1mi="+(p.n_1609m==null?"NA (state not in GIS yet)":p.n_1609m)+
  " | nearest: "+(p.nearest_dist_m||"?")+" m<br><a href=\\""+p.gmaps+"\\" target=\\"_blank\\">open in Google Maps</a>");
 m.addTo(g);});
g.addTo(map); map.fitBounds(g.getBounds().pad(0.2));
var lg=L.control({position:"bottomright"});
lg.onAdd=function(){var d=L.DomUtil.create("div","legend");
 d.innerHTML="<b>RF estimation-sample QC</b><br><span style=\\"color:#22ff22\\">&#9679;</span> gasoline-only"+
 "<br><span style=\\"color:#ffd400\\">&#9679;</span> mixed/other<br><span style=\\"color:#ff4444\\">&#9679;</span> diesel-only"+
 "<br>&#9711; white dashed ring = closed facility<br><span style=\\"color:#cc0\\">&#9472;</span> neighbor edge &le;0.5mi";return d;};
lg.addTo(map);
</script></body></html>', pts_json, edges_json)
writeLines(html, file.path(qc_dir, "gis_qc_estsample_map.html"))

log_gis(sprintf("QC sample (estimation): %d facilities (TX=%d, control=%d) | %d closed | %d w/o gis_02 (MO/SD) | %d edges",
                nrow(samp), sum(samp$texas_treated == 1), sum(samp$texas_treated == 0),
                sum(samp$facility_closed), sum(is.na(samp$n_1609m)), nrow(edge_feats)))
log_gis("Wrote qc/gis_qc_estsample.csv and qc/gis_qc_estsample_map.html")
