# Dewey download + reduce (server-side)

Goal: get the **UST-relevant** Dewey data onto the server **without ever staging
~1 TB**, and **without dropping non-retail operators** (trucking, municipal,
government, farm). We filter by **geography, not NAICS**.

### Filter = proximity to UST facilities (not industry)
A lat/long **grid** is built from the geocoded master file
(`Master_Harmonized_UST_Tanks.csv`, 97.5% geocoded, facility level). We keep any
Dewey POI whose cell is at/adjacent to a facility cell (~0.5 km capture) — every
operator type, gas or not. Works even for **Advan WPP, which has no `PLACEKEY`**
(it keys on `ID_STORE`/lat-long).

### Download tool = the `deweypy` CLI (docs.deweydata.io/docs/dewey-client)
- No-install (recommended), via `uv`:
  `uvx --python 3.13 --from deweypy dewey --api-key KEY speedy-download PROJECT_ID`
- Or installed: `pip install deweypy` then `python -m deweypy ... speedy-download PROJECT_ID`
- Date partitioning: `--partition-key-after YYYY-MM-DD --partition-key-before YYYY-MM-DD`
- `PROJECT_ID` = the id at the **end of the dataset's API URL, after `…/data/`**.
- ⚠ Do NOT `pip install meetdewey` — unrelated document-RAG product, not Dewey Data.

`dewey_pull_reduce.py` orchestrates that CLI: **for each monthly window** it
downloads just that window to a scratch dir → duckdb grid-filters it → writes a
small `part_*.parquet` → wipes scratch. **Peak disk = one month**, not 1 TB.

Files:
- `probe_system.sh`       run FIRST on the server; reports python/uv/deweypy/duckdb/disk/mount
- `dewey_pull_reduce.py`  grid build + CLI orchestration + reduce
- `run_dewey_pull.sh`     env + dry-run driver
- `requirements.txt`      duckdb (+ pandas/pyarrow); deweypy auto via uvx

## Step 0 — probe the server
```bash
bash probe_system.sh        # paste the output back; it sets the choices below
```

## Step 1 — provide credentials + ids
1. **API key** — app.deweydata.io → **Connections → Add Connection** → copy →
   `export DEWEY_API_KEY="...."` (never in the repo).
2. **PROJECT_IDs** — open each dataset → API/Get-Data tab → copy the id at the end
   of the URL after `…/data/` → paste into `DATASETS[...]["project_id"]`. Datasets:
   `global-places-poi-geometry`, `weekly-patterns-plus`, `spend` (property handled separately).
3. **Paths/mode** — `export DEWEY_Z_ROOT="/mnt/.../dewey-downloads"`,
   optionally `export UST_MASTER_CSV=/path/...`, and `export DEWEY_RUN=uvx` (or `pip`).

## Step 2 — run (ucbare2 Windows server, via uv)
Confirmed env: `uv` 0.10.12, `uv run python` → 3.13, `uv run --with duckdb` → duckdb 1.5.3.
Downloads MUST live on **C:** (server policy; D: not allowed). That is fine: stream-and-
reduce never stages the full dataset — peak disk = one month (~10 GB) + small reduced
outputs. PowerShell:
```powershell
$env:DEWEY_API_KEY  = "dwy_...."                         # don't commit it
$env:DEWEY_Z_ROOT   = "C:\Users\kalebkja\dewey-downloads"   # on C: (D: not allowed)
$env:UST_MASTER_CSV = "C:\Users\kalebkja\ust_ins_move_to_github\Data\Processed\Master_Harmonized_UST_Tanks.csv"
$env:DEWEY_RUN      = "uvx"
# DRY RUN — build grid + print the month plan, no download:
uv run --with duckdb python Code\GIS\dewey\dewey_pull_reduce.py
# REAL pull of one dataset (after PROJECT_IDs are set in the .py):
uv run --with duckdb python Code\GIS\dewey\dewey_pull_reduce.py run weekly_patterns_plus
```
Or just: `powershell -ExecutionPolicy Bypass -File Code\GIS\dewey\run_dewey_pull.ps1`.
Output: `C:\Users\kalebkja\dewey-downloads\_reduced_near_ust\<dataset>_near_ust\part_*.parquet`.
The downloader runs entirely through `uvx`/`uv run` — no system python or pip installs.
DISK NOTE: a 1-month WPP test measures per-month reduced size; if the broad 0.5 km grid
keeps too much for C:, tighten `GRID_DELTA` or filter WPP/Spend to the crosswalk-matched
POIs only (≈ one POI per facility) — see README "Downstream".

## Tunables (top of `dewey_pull_reduce.py`)
- `DEWEY_RUN` (`uvx`|`pip`), `NUM_WORKERS` (default 8)
- `GRID_DELTA` (~0.25 km cells, ~0.5 km capture with neighbours)
- `STUDY_STATES` (coarse REGION pre-filter; `None` = all US)
- per-dataset `keep_cols`, partition `start`/`end`
- Monthly windows bound peak disk; shrink to weekly if disk is tight (probe tells us).

## Property data (Cotality) — separate
`property-data` has no lat/long (joins by `ADDRID`) → can't grid-filter. Use the
Infutor ADDRID bridge (Infutor already on Z) or customize-by-state; public county
parcels are the robust fallback for commercial value. After the crosswalk.

## Downstream
1. `_reduced_near_ust/safegraph_places_near_ust/` → repoint
   `Code/GIS/01_SafeGraph_Crosswalk.R` `SG_PARQUET_DIR` here, run the crosswalk.
2. WPP + Spend reduced parquet → spatial-join to facilities (WPP by lat/long,
   no placekey) → foot-traffic / spend panels.
3. Crosswalk brand + facility name/fuel flags → operator 4-bin slide.

## Verify-before-trust (docs/PyPI/GitHub blocked to me; from the official quickstart)
The `deweypy speedy-download` + `--partition-key-*` flags are from the Dewey Client
quickstart. Confirm on the server with a 1-month test that files land **under the
script's scratch cwd**; if `speedy-download` needs an explicit output-dir flag,
add it in `_dewey_cmd()`. The dry run plans windows and downloads nothing.
