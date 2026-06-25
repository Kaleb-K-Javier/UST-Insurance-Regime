# poi_firstpass_classify — what it does + how to run

**Script:** `Code/GIS/poi_firstpass_classify.py` (Python + duckdb)
**Purpose:** an UGLY FIRST PASS at "what kinds of facilities are in the sample" — tag every UST facility as **gas_station / private_retail / government / unmatched** by matching it to the nearest SafeGraph Global Places POI.

This is intentionally rough. It uses the same coarse **grid-bin nearest-neighbour** spatial primitive we already flagged as suspect for the real crosswalk (see memory `project_safegraph_reduce_rebuild`). It is for eyeballing the composition, not for final operator classification. It records `dist_m` so you can see how good (or bad) each match is.

## What it actually does (step by step)
1. **Detects column names** in both files case-insensitively (so it survives SafeGraph upper/lowercase drift). Prints the schemas it found; if it can't find a needed column it stops and shows you what's available.
2. **Facility points** — reads the tank-level master CSV, dedupes to one row per `facility_id`, keeps lat/long inside CONUS (24–50 N, −125 to −66 W).
3. **Grid match** — bins facilities and POIs to `0.0025°` cells, expands each facility to a `±2`-cell ring (~0.7 km), joins POIs on cell, computes haversine distance, keeps the **nearest POI per facility within `BUFFER_M` (1000 m)**.
4. **Classify (waterfall, first match wins):**
   - `government` — facility name OR matched POI name/brand hits a government token (`city of`, `county`, `usps`, `school district`, `dept of`, `fire`, `police`, `university`, …) OR matched NAICS starts `92`.
   - `gas_station` — matched NAICS ∈ {447110, 447190, 457110, 457120} OR top-category like gasoline/fuel OR name/brand hits a fuel token (chevron, shell, exxon, valero, speedway, circle k, "fuel", "truck stop", …).
   - `private_retail` — matched a POI but it's neither government nor gas.
   - `unmatched` — no POI within the buffer (and no government name signal).
5. **Writes** `Data/Processed/GIS/poi_firstpass_class.csv` with: `facility_id, state, fac_name, poi_placekey, poi_name, poi_brands, poi_naics, poi_top, poi_sub, dist_m, class3, rule_fired`, and prints class counts + median dist per class.

## How to run (on the SERVER — that's where duckdb + the POI parquet live)
From the repo root, with the raw Places already at `…\dewey-downloads\global-places-poi-geometry\`:
```powershell
$env:DEWEY_Z_ROOT = "C:\Users\kalebkja\dewey-downloads"   # if not already set this shell
uv run --with duckdb python Code\GIS\poi_firstpass_classify.py
```
- `uv run --with duckdb` supplies Python 3.13 + duckdb on the fly — no install needed (same engine as the Dewey puller).
- It **reads only** (raw POI parquet + master CSV) and **writes one CSV**. It does NOT download anything and never touches your API key.
- Output goes under `Data/` which is gitignored, so it won't be committed.

## Python/duckdb gotchas (vs R)
- **One big SQL, not a loop.** All the work is a single duckdb `COPY (… ) TO 'file.csv'`. Read it like one long chained `data.table` expression: CTEs `fac → fac_cells → poi → joined → nearest → final → SELECT`. There is no row-by-row Python.
- **`read_parquet('…/*.parquet')`** lazily scans all 144 files; duckdb pushes the `WHERE` down, so it never loads 21 GB into RAM. Analogue: `arrow::open_dataset()` + `filter()` + `collect()`.
- **Strings:** duckdb `LIKE` is case-sensitive, so the token tests wrap both sides in `lower(...)` (= `grepl(..., ignore.case=TRUE)`).
- **Threads/memory:** `SET threads=8`. If it OOMs on the server, add `con.execute("SET memory_limit='24GB';")` near the top, or drop `N_RING` to 1.
- **Tuning knobs** (top of the script): `GRID_DELTA`, `N_RING` (ring size), `BUFFER_M` (max match distance), and the `GOV_TOKENS` / `GAS_TOKENS` / `GAS_NAICS` lists — edit these freely; they ARE the "ugly" heuristics.

## First things to check in the output
- The printed **class counts** — does the gas-station share look sane (most USTs are retail fuel)?
- **`dist_m` distribution** — if lots of matches are 500–1000 m, the grid match is grabbing whatever's nearby, not the facility's own POI → confirms the coordinate-quality / buffer problem and motivates the real distance-based rebuild.
- A **sample of each class** (especially `private_retail` and `government`) — spot-check 20 rows; that tells you how much the token lists need work.

## Known limits (by design)
- Grid bins ≠ circles; the `±2` ring is generous to fight geocode error but will also grab neighbours. `dist_m` is your guard.
- Token-based government/gas detection is crude and will mislabel edge cases. It's a first cut, not the validated 4-bin operator class (that's module `05_operator_class`).
- One POI per facility (nearest). No multi-candidate auditing yet — that's the real crosswalk's job.
