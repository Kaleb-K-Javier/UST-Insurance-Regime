#!/usr/bin/env python3
"""
dewey_pull_reduce.py
====================
Orchestrate Dewey's **deweypy CLI** to download datasets in date-partition CHUNKS,
reduce each chunk to POIs NEAR UST facilities (geography, NOT NAICS), and delete
the raw chunk. Peak disk = one chunk, never the full ~1 TB.

Why geography not NAICS: UST facilities are not all gas stations (trucking depots,
municipal/government motor pools, farms, airports). An industry filter would drop
the fleet/government operators we need. So we keep ANY POI type whose lat/long sits
near a UST facility, using a grid built from the geocoded master file.

Download tool (docs.deweydata.io/docs/dewey-client): the deweypy CLI.
  no-install : uvx --python 3.13 --from deweypy dewey --api-key KEY speedy-download PROJECT_ID
  installed  : python -m deweypy --api-key KEY speedy-download PROJECT_ID
  date range : --partition-key-after YYYY-MM-DD --partition-key-before YYYY-MM-DD
  threads    : --num-workers 8 (default)
PROJECT_ID = the id at the end of the dataset's API URL, after ".../data/".

Pipeline per dataset:
  partitioned (WPP, Spend): for each monthly window -> CLI download that window to
      a clean scratch dir -> duckdb grid-filter -> write small part -> wipe scratch.
  unpartitioned (Places):   one download -> grid-filter -> write -> wipe scratch.

Requires duckdb (python) for the reduce. Set env vars (see README). Run on server.
"""

import os
import glob
import shutil
import subprocess
import datetime as dt
import duckdb

# ----------------------------------------------------------------------------
# Environment
# ----------------------------------------------------------------------------
API_KEY    = os.environ.get("DEWEY_API_KEY")   # required only for a real pull, not the dry run
Z_ROOT     = os.environ.get("DEWEY_Z_ROOT", "Z:/C_Drive_Portal/dewey-downloads")
MASTER_CSV = os.environ.get(
    "UST_MASTER_CSV",
    os.path.join(os.path.dirname(__file__), "..", "..", "..",
                 "Data", "Processed", "Master_Harmonized_UST_Tanks.csv"))
OUT_ROOT     = os.path.join(Z_ROOT, "_reduced_near_ust")
SCRATCH      = os.path.join(Z_ROOT, "_scratch")
GRID_PARQUET = os.path.join(OUT_ROOT, "facility_grid.parquet")

DEWEY_RUN  = os.environ.get("DEWEY_RUN", "uvx")     # "uvx" (no install) or "pip"
NUM_WORKERS = os.environ.get("DEWEY_WORKERS", "8")

GRID_DELTA = 0.0025          # ~0.25 km cells; +8 neighbours => ~0.5 km capture
STUDY_STATES = ["TX","AL","AR","CO","LA","ME","NJ","NM","OK","PA","TN",
                "ID","KS","KY","MD","MA","MN","NC","ND","OH","VA",
                "AZ","CT","FL","IA","MI"]            # coarse REGION pre-filter; None = all US

# product PROJECT_ID = tail of each dataset's API URL after ".../data/"
DATASETS = {
    "safegraph_places": dict(
        project_id="prj_ycsuakbe__fldr_b7faazxwmt47zdme8", partitioned=False,
        lat="LATITUDE", lon="LONGITUDE", region="REGION",
        keep_cols=["PLACEKEY","LOCATION_NAME","BRANDS","STORE_ID","NAICS_CODE",
                   "NAICS_CODE_2022","TOP_CATEGORY","SUB_CATEGORY","LATITUDE",
                   "LONGITUDE","STREET_ADDRESS","CITY","REGION","POSTAL_CODE",
                   "OPENED_ON","CLOSED_ON","POLYGON_WKT","WKT_AREA_SQ_METERS"]),
    "weekly_patterns_plus": dict(     # Advan — keys on ID_STORE, NO placekey
        project_id="PASTE_WPP_PROJECT_ID", partitioned=True,
        start="2017-01-01", end=None,
        lat="LATITUDE", lon="LONGITUDE", region="REGION",
        keep_cols=["ID_STORE","PERSISTENT_ID","BRAND","LOCATION_NAME","REGION",
                   "NAICS_CODE","STREET_ADDRESS","LATITUDE","LONGITUDE",
                   "DATE_RANGE_START","VISIT_COUNTS","VISITOR_COUNTS",
                   "MEDIAN_DWELL","DISTANCE_FROM_HOME"]),
    "safegraph_spend": dict(
        project_id="PASTE_SPEND_PROJECT_ID", partitioned=True,
        start="2019-01-01", end=None,
        lat="LATITUDE", lon="LONGITUDE", region="REGION",
        keep_cols=["PLACEKEY","BRANDS","REGION","NAICS_CODE","LATITUDE","LONGITUDE",
                   "SPEND_DATE_RANGE_START","RAW_TOTAL_SPEND","RAW_NUM_TRANSACTIONS",
                   "RAW_NUM_CUSTOMERS","MEDIAN_SPEND_PER_CUSTOMER",
                   "MEDIAN_SPEND_PER_TRANSACTION","ONLINE_SPEND"]),
    "attom_tax_assessor": dict(          # ATTOM parcels — GEOCODED, so grid-filter works
        project_id="PASTE_ATTOM_TAX_ASSESSOR_PROJECT_ID", partitioned=False,
        lat="LATITUDE", lon="LONGITUDE", region="SITUSSTATECODE",
        keep_cols=["ATTOMID","PARCELNUMBERFORMATTED","PARCELNUMBERRAW",
                   "PARCELNUMBERALTERNATE","PARCELNUMBERPREVIOUS","LATITUDE","LONGITUDE",
                   "SITUSSTATECODE","SITUSCOUNTY","SITUSSTATECOUNTYFIPS",
                   "PROPERTYJURISDICTIONNAME","CBSACODE","CBSANAME",
                   "TAXASSESSEDVALUETOTAL","TAXASSESSEDVALUELAND","TAXASSESSEDVALUEIMPROVEMENTS",
                   "TAXMARKETVALUETOTAL","TAXMARKETVALUELAND","TAXMARKETVALUEIMPROVEMENTS",
                   "TAXMARKETVALUEYEAR"]),  # widen via sample: property-use, sqft, owner, site address
    # Hub: facility --spatial--> ATTOM parcel => ATTOMID + APN.
    #   ATTOMID joins ATTOM siblings (AVM modeled value, Recorder sales, Assessor History).
    #   APN (PARCELNUMBER*) joins external parcel data (county assessor, Regrid, Cotality).
    # Cotality property-data (ADDRID, no lat/long) becomes a fallback, not the primary path.
}


# ----------------------------------------------------------------------------
def build_facility_grid(force=False):
    if os.path.exists(GRID_PARQUET) and not force:
        return GRID_PARQUET
    os.makedirs(OUT_ROOT, exist_ok=True)
    src, dst = MASTER_CSV.replace(os.sep, "/"), GRID_PARQUET.replace(os.sep, "/")
    con = duckdb.connect()
    con.execute(f"""
        COPY (
          WITH fac AS (
            SELECT DISTINCT
              CAST(floor(TRY_CAST(latitude  AS DOUBLE)/{GRID_DELTA}) AS BIGINT) AS ilat,
              CAST(floor(TRY_CAST(longitude AS DOUBLE)/{GRID_DELTA}) AS BIGINT) AS ilon
            FROM read_csv_auto('{src}', all_varchar=true, ignore_errors=true)
            WHERE TRY_CAST(latitude AS DOUBLE) BETWEEN 24 AND 50
              AND TRY_CAST(longitude AS DOUBLE) BETWEEN -125 AND -66
          ),
          off AS (SELECT * FROM (VALUES (-1),(0),(1)) AS t(d)),
          grid AS (SELECT DISTINCT (fac.ilat+a.d) AS ilat, (fac.ilon+b.d) AS ilon
                   FROM fac, off a, off b)
          SELECT (ilat || '_' || ilon) AS cell FROM grid
        ) TO '{dst}' (FORMAT PARQUET)""")
    n = con.execute(f"SELECT count(*) FROM read_parquet('{dst}')").fetchone()[0]
    con.close()
    print(f"[grid] {n:,} facility cells -> {GRID_PARQUET}")
    return GRID_PARQUET


def _month_windows(start, end):
    s = dt.date.fromisoformat(start)
    e = dt.date.fromisoformat(end) if end else dt.date.today()
    cur = dt.date(s.year, s.month, 1)
    while cur <= e:
        nxt = dt.date(cur.year + (cur.month == 12), (cur.month % 12) + 1, 1)
        yield cur.isoformat(), (nxt - dt.timedelta(days=1)).isoformat()
        cur = nxt


def _dewey_cmd(project_id, after=None, before=None):
    if DEWEY_RUN == "uvx":
        base = ["uvx", "--python", "3.13", "--from", "deweypy", "dewey"]
    else:
        base = ["python", "-m", "deweypy"]
    cmd = base + ["--api-key", API_KEY, "speedy-download", project_id,
                  "--num-workers", NUM_WORKERS]
    if after:  cmd += ["--partition-key-after", after]
    if before: cmd += ["--partition-key-before", before]
    return cmd


def _run_cli(cmd):
    safe = ["***" if c == API_KEY else c for c in cmd]
    print("  RUN:", " ".join(safe))
    subprocess.run(cmd, cwd=SCRATCH, check=True)        # deweypy writes under cwd


def _reduce_scratch(cfg, out_path):
    files = glob.glob(os.path.join(SCRATCH, "**", "*.parquet"), recursive=True)
    if not files:
        print("    (no parquet in this window)"); return 0
    cols  = ", ".join(cfg["keep_cols"])
    flist = ", ".join("'" + f.replace(os.sep, "/") + "'" for f in files)
    grid  = GRID_PARQUET.replace(os.sep, "/")
    cell  = (f"(CAST(floor(TRY_CAST({cfg['lat']} AS DOUBLE)/{GRID_DELTA}) AS BIGINT) || '_' || "
             f" CAST(floor(TRY_CAST({cfg['lon']} AS DOUBLE)/{GRID_DELTA}) AS BIGINT))")
    where = [f"{cell} IN (SELECT cell FROM read_parquet('{grid}'))"]
    if STUDY_STATES and cfg.get("region"):
        where.insert(0, f"{cfg['region']} IN ({','.join(repr(s) for s in STUDY_STATES)})")
    con = duckdb.connect(); con.execute("SET threads=4;")
    con.execute(f"""COPY (SELECT {cols}
                         FROM read_parquet([{flist}], union_by_name=true)
                         WHERE {' AND '.join(where)})
                    TO '{out_path.replace(os.sep,'/')}' (FORMAT PARQUET)""")
    n = con.execute(f"SELECT count(*) FROM read_parquet('{out_path.replace(os.sep,'/')}')").fetchone()[0]
    con.close()
    return n


def _wipe_scratch():
    shutil.rmtree(SCRATCH, ignore_errors=True); os.makedirs(SCRATCH, exist_ok=True)


def pull_reduce(name, dry_run=True):
    cfg = DATASETS[name]
    if cfg["project_id"].startswith("PASTE"):
        raise AssertionError(f"set project_id for '{name}' (README: API URL tail after data/)")
    build_facility_grid()
    out_dir = os.path.join(OUT_ROOT, f"{name}_near_ust"); os.makedirs(out_dir, exist_ok=True)

    windows = (list(_month_windows(cfg["start"], cfg["end"])) if cfg["partitioned"]
               else [(None, None)])
    print(f"[{name}] {len(windows)} window(s); DEWEY_RUN={DEWEY_RUN}")
    if dry_run:
        print(f"[{name}] DRY RUN — no download. First window: {windows[0]}. "
              f"pull_reduce('{name}', dry_run=False) to run.")
        return
    if not API_KEY:
        raise SystemExit("set DEWEY_API_KEY before a real pull")
    for a, b in windows:
        _wipe_scratch()
        _run_cli(_dewey_cmd(cfg["project_id"], after=a, before=b))
        tag = (a or "all")
        n = _reduce_scratch(cfg, os.path.join(out_dir, f"part_{tag}.parquet"))
        print(f"  [{name}] {tag}: kept {n:,} near-UST rows")
        _wipe_scratch()
    print(f"[{name}] DONE -> {out_dir} (POIs near UST facilities, all types)")


if __name__ == "__main__":
    import sys
    build_facility_grid()
    # Real pull of ONE dataset:   ... dewey_pull_reduce.py run <dataset>
    if len(sys.argv) >= 3 and sys.argv[1] == "run":
        pull_reduce(sys.argv[2], dry_run=False)
    else:
        # default: DRY RUN of all (build grid + print the month plan, no download)
        for ds in ("safegraph_places", "weekly_patterns_plus", "safegraph_spend", "attom_tax_assessor"):
            try:
                pull_reduce(ds, dry_run=True)
            except (AssertionError, KeyError) as e:
                print("SKIP:", e)
