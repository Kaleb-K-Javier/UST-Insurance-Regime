#!/usr/bin/env python
# poi_firstpass_classify.py
# UGLY FIRST PASS: classify every UST facility as gas_station / private_retail /
# government / unmatched, by matching it to the nearest SafeGraph Global Places POI.
#
# This is deliberately rough (a grid-bin nearest-neighbour, the same coarse spatial
# primitive we already flagged as SUSPECT for the real crosswalk). It is good enough
# for a first eyeball cut of "what kinds of facilities are in the sample" and it
# records dist_m so we can SEE match quality. Do NOT treat its matches as final.
#
# RUN ON THE SERVER (where duckdb + the raw POI parquet live), from the repo root:
#   $env:DEWEY_Z_ROOT = "C:\Users\kalebkja\dewey-downloads"   # if not already set
#   uv run --with duckdb python Code\GIS\poi_firstpass_classify.py
#
# Companion doc: Code/GIS/poi_firstpass_classify.md
#
# ---- R->Python reading guide (researcher thinks in R) -------------------------
#   duckdb.connect()                ~ a throwaway in-memory data.table/DB session
#   con.execute(SQL).fetchdf()      ~ as.data.frame( DBI::dbGetQuery(...) )
#   read_parquet('glob/*.parquet')  ~ arrow::open_dataset() then collect lazily
#   read_csv_auto(..., all_varchar) ~ fread(..., colClasses="character")
#   the big COPY (...) TO 'x.csv'   ~ fwrite( <result of one big data.table join> )
# We push ALL the work into ONE duckdb SQL statement (like one chained data.table
# expression) instead of looping in Python — duckdb is the engine, Python is glue.
# -------------------------------------------------------------------------------

import os
import sys
import duckdb

# ---- config (env-overridable) -------------------------------------------------
Z_ROOT   = os.environ.get("DEWEY_Z_ROOT", r"C:\Users\kalebkja\dewey-downloads")
POI_GLOB = os.path.join(Z_ROOT, "global-places-poi-geometry", "*.parquet")
MASTER   = os.environ.get(
    "UST_MASTER_CSV",
    r"C:\Users\kalebkja\ust_ins_move_to_github\Data\Processed\Master_Harmonized_UST_Tanks.csv")
OUT_CSV  = os.environ.get("POI_FIRSTPASS_OUT",
                          os.path.join("Data", "Processed", "GIS", "poi_firstpass_class.csv"))

GRID_DELTA = 0.0025     # ~0.28 km lat cells; we expand +/- N_RING cells for capture
N_RING     = 2          # +/-2 cells ~ up to ~0.7 km; generous on purpose (geocode slack)
BUFFER_M   = 1000.0     # keep a match only if nearest POI <= this many metres

POI_FS = POI_GLOB.replace(os.sep, "/")
SRC_FS = MASTER.replace(os.sep, "/")
OUT_FS = OUT_CSV.replace(os.sep, "/")

con = duckdb.connect()
con.execute("SET threads=8;")


# ---- schema detection: find a column by case-insensitive alias --------------
# R analogue: names(dt)[ tolower(names(dt)) %in% aliases ][1]
def cols_of(relation_sql):
    rows = con.execute(f"DESCRIBE SELECT * FROM {relation_sql} LIMIT 0").fetchall()
    return [r[0] for r in rows]

def pick(have, *aliases, required=True, label=""):
    low = {c.lower(): c for c in have}
    for a in aliases:
        if a.lower() in low:
            return low[a.lower()]
    if required:
        print(f"!! could not find a column for {label or aliases[0]}.")
        print(f"   tried: {aliases}")
        print(f"   available: {have}")
        sys.exit(1)
    return None

print("=== detecting schemas ===")
poi_cols = cols_of(f"read_parquet('{POI_FS}')")
mas_cols = cols_of(f"read_csv_auto('{SRC_FS}', all_varchar=true, ignore_errors=true, sample_size=-1)")
print(f"POI columns ({len(poi_cols)}): {poi_cols}")
print(f"MASTER columns ({len(mas_cols)}): {mas_cols[:40]}{' ...' if len(mas_cols) > 40 else ''}")

# POI columns (SafeGraph Global Places naming; case-insensitive)
P_PK   = pick(poi_cols, "placekey", "place_key", required=False, label="placekey")
P_NAME = pick(poi_cols, "location_name", "name", label="location_name")
P_BRD  = pick(poi_cols, "brands", "brand", required=False, label="brands")
P_NAI  = pick(poi_cols, "naics_code", "naics", required=False, label="naics_code")
P_NAI2 = pick(poi_cols, "naics_code_2022", "naics_2022", required=False, label="naics_2022")
P_TOP  = pick(poi_cols, "top_category", "category", required=False, label="top_category")
P_SUB  = pick(poi_cols, "sub_category", "subcategory", required=False, label="sub_category")
P_LAT  = pick(poi_cols, "latitude", "lat")
P_LON  = pick(poi_cols, "longitude", "lon", "lng", "long")

# MASTER columns
M_FID  = pick(mas_cols, "facility_id", "facilityid", "fac_id")
M_ST   = pick(mas_cols, "state", "state_code", "st", required=False, label="state")
M_NAME = pick(mas_cols, "facility_name", "name", required=False, label="facility_name")
M_LAT  = pick(mas_cols, "latitude", "lat")
M_LON  = pick(mas_cols, "longitude", "lon", "lng", "long")

# columns that may be absent -> NULL literal so the SQL still runs
def col_or_null(c):
    return c if c else "NULL"

p_naics_expr = f"coalesce({col_or_null(P_NAI2)}, {col_or_null(P_NAI)})" \
    if (P_NAI or P_NAI2) else "NULL"

st_sel   = M_ST   if M_ST   else "'' AS state"
name_sel = M_NAME if M_NAME else "'' AS facility_name"

# ---- classification token lists (edit freely; this is the "ugly" part) -------
GOV_TOKENS = ["city of", "town of", "county", "state of", "u.s.", "united states",
              "usps", "post office", "school", "district", "dept of", "department of",
              "municipal", " fire ", "police", "sheriff", "d.o.t", " dot ",
              "transit", "transportation authority", "public works", "township",
              "village of", "borough", "university", "college", "naval", "air force",
              "army", "federal", "national guard", "correctional", "prison"]
GAS_TOKENS = ["chevron", "texaco", "shell", "exxon", "mobil", " bp ", "arco", " 76 ",
              "phillips 66", "conoco", "valero", "marathon", "sinclair", "sunoco",
              "citgo", " gulf", "speedway", "circle k", "quiktrip", "kwik", "wawa",
              "sheetz", "casey", "pilot", "flying j", "love's", "loves", "racetrac",
              "fuel", "gasoline", "petroleum", "service station", "travel center",
              "truck stop", "cardlock", "fuel mart", "food mart", "convenience"]
GAS_NAICS  = ["447110", "447190", "457110", "457120"]  # 2017 + 2022 gas-station NAICS


def ilike_any(expr, tokens):
    # R analogue: grepl(paste(tokens, collapse="|"), x, ignore.case=TRUE)
    parts = [f"lower({expr}) LIKE '%{t.lower()}%'" for t in tokens]
    return "(" + " OR ".join(parts) + ")"


def hav(lat1, lon1, lat2, lon2):
    return (f"2*6371000*asin(sqrt(pow(sin(radians(({lat2}-{lat1})/2)),2) + "
            f"cos(radians({lat1}))*cos(radians({lat2}))*pow(sin(radians(({lon2}-{lon1})/2)),2)))")


# neighbour offsets -1..+1.. for the grid ring
offs = ",".join(f"({d})" for d in range(-N_RING, N_RING + 1))

# POI name/brand text used for token tests
poi_txt = f"coalesce({P_NAME},'') || ' ' || coalesce({col_or_null(P_BRD)},'')"

gas_rule = (f"({p_naics_expr} IN ({','.join(repr(x) for x in GAS_NAICS)})"
            f" OR { ilike_any(col_or_null(P_TOP), ['gasoline','fuel']) if P_TOP else 'FALSE'}"
            f" OR {ilike_any('poi_txt', GAS_TOKENS)})")
gov_rule = (f"({ilike_any('fac_name', GOV_TOKENS)}"
            f" OR {ilike_any('poi_txt', GOV_TOKENS)}"
            f" OR {p_naics_expr} LIKE '92%')")

print("\n=== running grid-bin nearest-POI match + classify (one duckdb pass) ===")
print(f"    POI:    {POI_FS}")
print(f"    MASTER: {SRC_FS}")
print(f"    grid={GRID_DELTA} ring=+/-{N_RING} buffer={BUFFER_M:.0f} m  -> {OUT_FS}")

os.makedirs(os.path.dirname(OUT_FS) or ".", exist_ok=True)

sql = f"""
COPY (
  WITH
  -- 1. facility points, deduped to (facility_id, state).  R: unique(dt, by=...)
  fac0 AS (
    SELECT {M_FID} AS facility_id, {st_sel}, {name_sel},
           TRY_CAST({M_LAT} AS DOUBLE) AS flat, TRY_CAST({M_LON} AS DOUBLE) AS flon
    FROM read_csv_auto('{SRC_FS}', all_varchar=true, ignore_errors=true, sample_size=-1)
  ),
  fac AS (
    SELECT facility_id, any_value(state) AS state, any_value(facility_name) AS facility_name,
           any_value(flat) AS flat, any_value(flon) AS flon
    FROM fac0
    WHERE flat BETWEEN 24 AND 50 AND flon BETWEEN -125 AND -66
    GROUP BY facility_id
  ),
  -- 2. facility -> the (2*ring+1)^2 grid cells around it.  R: CJ(offsets) join
  off AS (SELECT * FROM (VALUES {offs}) AS t(d)),
  fac_cells AS (
    SELECT f.facility_id, f.state, f.facility_name, f.flat, f.flon,
           (CAST(floor(f.flat/{GRID_DELTA}) AS BIGINT)+a.d) || '_' ||
           (CAST(floor(f.flon/{GRID_DELTA}) AS BIGINT)+b.d) AS cell
    FROM fac f, off a, off b
  ),
  -- 3. POIs, binned to their own cell.  Only the cols we need.
  poi AS (
    SELECT {col_or_null(P_PK)}   AS poi_placekey,
           {P_NAME}              AS poi_name,
           {col_or_null(P_BRD)}  AS poi_brands,
           {p_naics_expr}        AS poi_naics,
           {col_or_null(P_TOP)}  AS poi_top,
           {col_or_null(P_SUB)}  AS poi_sub,
           TRY_CAST({P_LAT} AS DOUBLE) AS plat,
           TRY_CAST({P_LON} AS DOUBLE) AS plon,
           (CAST(floor(TRY_CAST({P_LAT} AS DOUBLE)/{GRID_DELTA}) AS BIGINT) || '_' ||
            CAST(floor(TRY_CAST({P_LON} AS DOUBLE)/{GRID_DELTA}) AS BIGINT)) AS cell
    FROM read_parquet('{POI_FS}')
    WHERE TRY_CAST({P_LAT} AS DOUBLE) BETWEEN 24 AND 50
      AND TRY_CAST({P_LON} AS DOUBLE) BETWEEN -125 AND -66
  ),
  -- 4. join on cell, distance, keep nearest POI per facility within buffer
  joined AS (
    SELECT fc.facility_id, fc.state, fc.facility_name,
           p.poi_placekey, p.poi_name, p.poi_brands, p.poi_naics, p.poi_top, p.poi_sub,
           {hav('fc.flat','fc.flon','p.plat','p.plon')} AS dist_m,
           row_number() OVER (PARTITION BY fc.facility_id
                              ORDER BY {hav('fc.flat','fc.flon','p.plat','p.plon')}) AS rk
    FROM fac_cells fc JOIN poi p USING (cell)
  ),
  nearest AS (SELECT * FROM joined WHERE rk = 1 AND dist_m <= {BUFFER_M}),
  -- 5. attach nearest back to ALL facilities (left join -> unmatched stay)
  final AS (
    SELECT f.facility_id, f.state, f.facility_name AS fac_name,
           n.poi_placekey, n.poi_name, n.poi_brands, n.poi_naics, n.poi_top, n.poi_sub,
           n.dist_m,
           (coalesce(n.poi_name,'') || ' ' || coalesce(n.poi_brands,'')) AS poi_txt
    FROM fac f LEFT JOIN nearest n USING (facility_id)
  )
  -- 6. the ugly 3-way (+unmatched) waterfall: gov -> gas -> private_retail
  SELECT facility_id, state, fac_name,
         poi_placekey, poi_name, poi_brands, poi_naics, poi_top, poi_sub,
         round(dist_m, 1) AS dist_m,
         CASE
           WHEN poi_placekey IS NULL AND NOT {ilike_any('fac_name', GOV_TOKENS)} THEN 'unmatched'
           WHEN {gov_rule} THEN 'government'
           WHEN poi_placekey IS NULL THEN 'unmatched'
           WHEN {gas_rule} THEN 'gas_station'
           ELSE 'private_retail'
         END AS class3,
         CASE
           WHEN poi_placekey IS NULL AND NOT {ilike_any('fac_name', GOV_TOKENS)} THEN 'no_poi'
           WHEN {gov_rule} THEN 'gov_token_or_naics92'
           WHEN poi_placekey IS NULL THEN 'no_poi'
           WHEN {gas_rule} THEN 'gas_naics_cat_or_brand'
           ELSE 'matched_other_retail'
         END AS rule_fired
  FROM final
) TO '{OUT_FS}' (FORMAT CSV, HEADER)
"""

con.execute(sql)

# ---- summary -----------------------------------------------------------------
summ = con.execute(f"""
  SELECT class3, count(*) AS n,
         round(100.0*count(*)/sum(count(*)) OVER (), 1) AS pct,
         round(median(dist_m), 1) AS med_dist_m
  FROM read_csv_auto('{OUT_FS}') GROUP BY class3 ORDER BY n DESC
""").fetchdf()
total = con.execute(f"SELECT count(*) FROM read_csv_auto('{OUT_FS}')").fetchone()[0]
con.close()

print("\n=== FIRST-PASS CLASS COUNTS ===")
print(summ.to_string(index=False))
print(f"\nfacilities total: {total:,}")
print(f"written: {OUT_CSV}")
print("NOTE: ugly first pass — grid-bin nearest match (suspect), token rules. "
      "Spot-check dist_m and a sample of each class before trusting.")
