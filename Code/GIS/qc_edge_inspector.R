#### qc_edge_inspector.R ####
# A clearer per-facility edge QC tool. Instead of all 100 sample facilities + every
# edge on one zoomed-out map, this builds an interactive inspector: pick a facility
# from the list -> map zooms to it (satellite) and shows ONLY its neighbors, the
# connecting edges (bright line + black casing for contrast), distance on hover, and
# 0.25/0.5/1-mile rings so each neighbor's ring is obvious. Near-zero edges (<25 m)
# are flagged as likely duplicates. Self-contained (Leaflet CDN + Esri imagery), no key.
#
# Reuses the same 100 facilities as gis_qc_sample.csv (incl. facility_closed flag).
# Output: Data/Processed/GIS/qc/gis_qc_edge_inspector.html

source(here::here("Code", "GIS", "00_gis_config.R"))
suppressMessages(library(jsonlite))

qc_dir <- file.path(GIS_OUT_DIR, "qc")
samp <- fread(file.path(qc_dir, "gis_qc_estsample.csv"),
              colClasses = c(facility_id = "character", state = "character"))
samp <- samp[, .(facility_id, state, facility_name, lat, lon,
                 n_400m, n_800m, n_1609m, facility_closed, group, make_model_fac)]

NBR_CAP <- 60L   # show up to the nearest 60 neighbors per facility (legibility)

## facility coord+name lookup (for neighbor endpoints) ----
m <- fread(MASTER_TANKS_PATH,
           select = c("facility_id", "state", "facility_name", "latitude", "longitude"),
           colClasses = c(facility_id = "character", state = "character"))
m[, lat := as.numeric(latitude)][, lon := as.numeric(longitude)]
m <- m[is.finite(lat) & is.finite(lon) & lat %between% c(24, 50) & lon %between% c(-125, -66)]
coords <- m[, .(facility_name = facility_name[1], lat = lat[1], lon = lon[1]),
            by = .(facility_id, state)]

## edges touching the sample, neighbor = other endpoint, with neighbor coords/name ----
ed <- fread(file.path(GIS_OUT_DIR, "gis_02_neighbor_edges.csv"),
            colClasses = c(state = "character",
                           facility_id_i = "character", facility_id_j = "character"))
ids <- samp$facility_id
elong <- rbind(
  ed[facility_id_i %in% ids, .(facility_id = facility_id_i, state, nbr = facility_id_j, dist_m)],
  ed[facility_id_j %in% ids, .(facility_id = facility_id_j, state, nbr = facility_id_i, dist_m)]
)
fc <- coords[, .(nbr = facility_id, state, nlat = lat, nlon = lon, nname = facility_name)]
elong <- merge(elong, fc, by = c("nbr", "state"), all.x = TRUE)
setorder(elong, facility_id, state, dist_m)

## assemble per-facility JSON ----
facs <- lapply(seq_len(nrow(samp)), function(k) {
  f <- samp[k]
  e <- elong[facility_id == f$facility_id & state == f$state]
  if (nrow(e) > NBR_CAP) e <- e[seq_len(NBR_CAP)]
  list(
    id = f$facility_id, name = f$facility_name, state = f$state,
    lat = f$lat, lon = f$lon,
    n400 = f$n_400m, n800 = f$n_800m, n1609 = f$n_1609m,
    closed = f$facility_closed, group = f$group, cell = f$make_model_fac,
    nbrs = data.frame(lat = e$nlat, lon = e$nlon,
                      dist = round(e$dist_m), name = e$nname)
  )
})
json <- toJSON(facs, dataframe = "rows", auto_unbox = TRUE, na = "null")

## HTML (R format string single-quoted; JS/HTML use double quotes; %% = literal %) ----
tmpl <- '<!DOCTYPE html><html><head><meta charset="utf-8"><title>UST edge inspector</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>
html,body{height:100%%;margin:0;font:13px system-ui,sans-serif}
#wrap{display:flex;height:100%%}
#side{width:330px;overflow:auto;background:#111;color:#ddd}
#side h3{margin:8px}
#list{list-style:none;margin:0;padding:0}
#list li{padding:6px 9px;border-bottom:1px solid #222;cursor:pointer}
#list li:hover{background:#1d3a5f}
#list li.sel{background:#2a5f8f}
#main{flex:1;display:flex;flex-direction:column;min-width:0}
#hdr{padding:6px 10px;background:#000;color:#eee;min-height:34px}
#map{flex:1}
.cl{color:#ff8a8a}.op{color:#8aff8a}
</style></head><body><div id="wrap">
<div id="side"><h3>QC sample (%d)</h3><ul id="list"></ul></div>
<div id="main"><div id="hdr"></div><div id="map"></div></div>
</div><script>
var DATA=%s;
var ESRI="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}";
var map=L.map("map").setView([31.5,-99],5);
L.tileLayer(ESRI,{maxZoom:20,attribution:"Esri World Imagery"}).addTo(map);
var layer=L.layerGroup().addTo(map);
function ring(c,r,col){return L.circle(c,{radius:r,color:col,fill:false,weight:1,dashArray:"4 5",opacity:0.9});}
function focus(i){
 layer.clearLayers();
 var f=DATA[i], c=[f.lat,f.lon], mind=1e9;
 ring(c,1609,"#ffffff").addTo(layer); ring(c,800,"#ffcc00").addTo(layer); ring(c,400,"#33ccff").addTo(layer);
 (f.nbrs||[]).forEach(function(n){
  mind=Math.min(mind,n.dist);
  var col=n.dist<=400?"#22ff22":(n.dist<=800?"#ffd400":"#ff8800");
  L.polyline([c,[n.lat,n.lon]],{color:"#000",weight:3.5,opacity:0.65}).addTo(layer);
  L.polyline([c,[n.lat,n.lon]],{color:col,weight:1.6,opacity:0.95}).addTo(layer);
  L.circleMarker([n.lat,n.lon],{radius:5,color:"#000",weight:1,fillColor:col,fillOpacity:0.95})
    .addTo(layer).bindTooltip(n.dist+" m"+(n.name?(" \\u00b7 "+n.name):""));
 });
 var fcol=f.closed==1?"#ff2d55":"#ff2d55";
 L.circleMarker(c,{radius:8,color:f.closed==1?"#fff":"#000",weight:f.closed==1?3:2,dashArray:f.closed==1?"2 3":null,fillColor:fcol,fillOpacity:1})
   .addTo(layer).bindTooltip("FOCAL: "+(f.name||f.id),{permanent:false});
 map.setView(c,17);
 var dup=((f.nbrs||[]).length>0 && mind<25)?(" &#9888; nearest "+Math.round(mind)+" m (possible DUPLICATE)"):"";
 var stat=f.closed==1?"<span class=cl>[CLOSED]</span>":"<span class=op>[OPEN]</span>";
 document.getElementById("hdr").innerHTML="<b>"+(f.name||"(no name)")+"</b> ("+f.state+") "+f.id+" "+stat+
  " &middot; "+(f.group||"")+" &middot; cell "+(f.cell||"?")+
  " &mdash; neighbors within 1 mi: "+((f.nbrs||[]).length)+" | counts "+f.n400+"/"+f.n800+"/"+f.n1609+
  " @ 0.25/0.5/1mi (rings: <span style=\\"color:#33ccff\\">cyan</span>/<span style=\\"color:#ffcc00\\">amber</span>/white)"+
  "<span style=\\"color:#ff6\\">"+dup+"</span> &middot; "+
  "<a target=\\"_blank\\" style=\\"color:#9cf\\" href=\\"https://www.google.com/maps/search/?api=1&query="+f.lat+","+f.lon+"\\">Google Maps</a>";
 var items=document.querySelectorAll("#list li"); items.forEach(function(el){el.className="";}); items[i].className="sel";
}
var ul=document.getElementById("list");
DATA.forEach(function(f,i){var li=document.createElement("li");
 li.innerHTML=(i+1)+". "+(f.name||"(no name)")+" ("+f.state+") "+(f.closed==1?"<span class=cl>&#10005;</span>":"")+
   " <span style=\\"color:#888\\">["+((f.nbrs||[]).length)+"]</span>";
 li.onclick=function(){focus(i);}; ul.appendChild(li);});
focus(0);
</script></body></html>'

html <- sprintf(tmpl, nrow(samp), json)
writeLines(html, file.path(qc_dir, "gis_qc_estsample_inspector.html"))
log_gis(sprintf("Edge inspector: %d facilities (%d closed) | neighbor cap %d -> qc/gis_qc_estsample_inspector.html",
                nrow(samp), sum(samp$facility_closed), NBR_CAP))
