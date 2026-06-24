# NEXT SESSION — SafeGraph POI download + facility-type classification
# Paste the block below into a fresh Opus session to onboard it.

---

You are the ARCHITECT (Opus) for the UST Insurance project. Today's goal: **strategize, plan, test, and work out the end-to-end workflow to (1) Dewey-download SafeGraph Global Places POIs and (2) spatially match them to EPA UST facilities to classify each facility by TYPE (operator class / facility type).** We are NOT trying to finish all of it today — we want a clean, tested workflow plan from download → crosswalk → classification, with the open decisions resolved.

## LANGUAGE — not locked to R for this GIS work (researcher's call, 2026-06-24)
This GIS/POI work may use whatever language is best for the job (Python with geopandas/duckdb-spatial is a strong candidate — the Dewey tooling is already Python). It does NOT have to be R. The existing `01_SafeGraph_Crosswalk.R` is a reference for the matching LOGIC, not a mandate to stay in R. **Whenever a script is in a non-R language, two things are REQUIRED every time:**
  1. **R→<language> mapping comments inline in the code** — annotate the logic in R terms so the researcher (who thinks in R) can follow what each block does (e.g. `# R: data.table[, .N, by=state]  ->  pandas groupby`).
  2. **A companion markdown doc** explaining what the script does end-to-end AND any new-language-specific setup/gotchas that must happen (install/env/CRS libs/how to run). One .md per script (or per module).
Pick the language deliberately and tell the researcher why; then honor (1) and (2). (Note: CLAUDE.md still describes an R-only style + R1 coder loop — that predates this rule; flag it rather than assume R-only.)

## First, get oriented (read these, in order)
1. `CLAUDE.md` (repo root) — workflow rules, token budget, ARCHITECT→CODER(R1)→REVIEWER(Sonnet) loop, R style, approval gates.
2. `Code/GIS/SCOPE_GIS_Dewey_Linkage.md` — THE design doc. §3 data architecture (download big → reduce on server → analyze small), §5 dataset inventory, §7.1 Task-4 operator-class 4-bin waterfall, §8 build plan/waves, §9 open decisions.
3. `Code/GIS/dewey/HANDOFF_GIS_DEWEY.md` — workstream state: what's built, env vars, dataset registry (safegraph_places READY), data facts.
4. `Code/GIS/dewey/README_DEWEY_PULL.md` — how the pull is run.
5. `Code/GIS/dewey/dewey_pull_reduce.py` — the download+reduce script (stream a chunk → duckdb grid-filter to POIs near a UST facility → write small). `safegraph_places` registry entry is ready.
6. `Code/GIS/00_gis_config.R` — GIS config: CRS_ALBERS=EPSG:5070, STUDY_STATES, keys=(facility_id,state), helpers (load_master_coords, coords_to_sf, write_gis_lookup). Outputs → Data/Processed/GIS/.
7. `Code/GIS/01_SafeGraph_Crosswalk.R` — EXISTING facility→SafeGraph k-NN (FNN k=5) + Jaro-Winkler name matcher → ust_safegraph_xwalk.csv. **This is the anchor to reuse.** Has known bug F1 (hard-coded stale SG_PARQUET_DIR — must repoint to the reduced download dir).

## Current repo state (just synced)
- Local, GitHub `main`, and the server (ucbare2) are all at commit `7d33f20` — fully synced. All the GIS/Dewey tooling above is committed.
- Topology: code authored on LOCAL Windows clone (`C:\Users\kaleb\Documents\ust_ins_move_to_github`); the Dewey download + heavy parquet reduce RUN ON THE SERVER (`C:\Users\kalebkja\ust_ins_move_to_github`), which is where the data and DEWEY_API_KEY live. Both track origin/main.

## IMPORTANT — we are REBUILDING the spatial filter, not reusing it (researcher decision 2026-06-24)
The raw FULL national SafeGraph Global Places is ALREADY DOWNLOADED (researcher pulled it via the Dewey CLL): 144 parquet files, ~21 GB, at `C:\Users\kalebkja\dewey-downloads\global-places-poi-geometry\` (→ D:; gitignored). The `_reduced_near_ust\safegraph_places_near_ust\` dir is EMPTY — we are NOT using the old reduce. **The existing near-UST reduce in `dewey_pull_reduce.py` is SUSPECT and must be rebuilt + validated, not trusted.** Why it's wrong (verified by reading the code, [dewey_pull_reduce.py:214](Code/GIS/dewey/dewey_pull_reduce.py:214) `build_facility_grid` + [:259](Code/GIS/dewey/dewey_pull_reduce.py:259) `_reduce_files`):
  - It is a GRID-BIN membership test (cell = `floor(lat/0.0025)_floor(lon/0.0025)`, +3×3 neighbor block), NOT a distance buffer. Bins ≠ circles → hard edge effects, lopsided capture near cell corners.
  - Capture radius is anisotropic/latitude-dependent: 0.0025° ≈ 0.278 km N–S but `0.278×cos(lat)` E–W (~0.25 km south, ~0.18 km north) — the "≈0.5 km" comment overstates it.
  - REAL RISK: that radius can be tighter than the EPA geocoding error. Where facility coords are coarse (ZIP/city-centroid; TX/AZ/CT flagged uneven), the true SafeGraph POI is dropped and the k-NN can never recover it → match loss correlated with coordinate quality.
  - REBUILD = true distance filter (project to meters / duckdb `ST_DWithin`) with a deliberate buffer + a CAPTURE-RATE validation (what share of known gas-station facilities find a POI; sensitivity to buffer). Keep the raw so the buffer can be set/measured, not assumed.

## The workflow we're designing (and who does what)
- **STEP A — DATA: DONE.** Raw national Places is on the server (path above). No further download needed. (Old framing — `dewey_pull_reduce.py run safegraph_places` — is retired for this purpose; we read the raw directly.)
- **STEP B — SPATIAL FILTER + CROSSWALK (rebuild):** read the 144 raw parquet directly (duckdb/geopandas), facility → POI match with a TRUE distance method (not the grid bins) + name Jaro-Winkler, keep ALL candidates + is_best, scored by dist_m. Validate capture rate. `01_SafeGraph_Crosswalk.R` is a LOGIC reference (its k-NN/JW recipe), NOT to be run as-is — it has the same bin-era assumptions + bug F1 (stale `SG_PARQUET_DIR` triple-`dewey-downloads` path). Language: researcher's choice (Python likely; see LANGUAGE rule above — R→lang comments + a .md required). SafeGraph Global Places carries BRANDS, NAICS (2017 + 2022 vintages), TOP_CATEGORY/SUB_CATEGORY, lat/long, POLYGON_WKT.
- **STEP C — CLASSIFY (R, new module `05_operator_class.R`):** use matched POI BRAND/NAICS/category + facility_name + fuel flags to label each facility. Operator 4-bin waterfall (SCOPE §7.1): Government/other → Fleet/non-retail → Branded major → Independent retail. Output `gis_05_operator_class.csv`. Needs a hand-validation sample.

## Locked decisions (don't relitigate)
- Spatial filter = GEOGRAPHY (near-UST DISTANCE buffer — rebuilt; NOT the old grid bins), NEVER NAICS (USTs include fleet/muni/farm sites).
- Crosswalk keeps all candidates + non-exclusive is_best; match = spatial k-NN + name JW (+ address where available).
- No CERS owner_type in the EPA panel → classification leans on SafeGraph BRAND + name heuristics + fuel flags + a researcher-reviewed brand dictionary (`Code/Helpers/brand_dictionary.R`, NEW).
- Retired code → Archive/, never delete.

## Open decisions to resolve early (SCOPE §9)
- Q1 (HARD PREREQ): per-state lat/long coverage in the Master file, especially TX (target state). Thin coords bias the spatial match. Audit coverage before trusting the crosswalk.
- Q4: approve the 4-bin operator waterfall + brand dictionary; how much hand-validation before it feeds the slide?
- Q5: DONE — raw national Places already staged on the server (D:, gitignored). No download decision left.

## Start by
1. Reading the files above + the existing reduce code; confirm you understand WHY the old grid-bin filter is suspect (bins-not-circles, anisotropic radius, loss correlated with geocode error).
2. Proposing the REBUILT spatial filter: a true distance buffer over the raw 144 parquet (duckdb `ST_DWithin` / projected meters), with a capture-rate validation plan. Decide the buffer + how to measure what it drops.
3. Flagging Q1 (per-state coordinate-quality audit, esp. TX) as the gate — it's the same root cause as the reduce's loss, so do it first and let it inform the buffer.

Do NOT write/run estimation code or commit anything yet — plan and test the workflow first, then we cut tickets in spec_template.md format (number from 028 upward; check `.claude/TICKETS/` for the current max).
