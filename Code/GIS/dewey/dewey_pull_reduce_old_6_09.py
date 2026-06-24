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
  local (ATTOM):            read from existing server path -> grid-filter -> write.

Requires duckdb (python) for the reduce. Set env vars (see README). Run on server.
"""

import os
import re
import glob
import shutil
import subprocess
import datetime as dt
import duckdb

import logging

def _setup_logging(out_root):
    """Write logs to both console and a timestamped file in out_root."""
    os.makedirs(out_root, exist_ok=True)
    ts  = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
    log_path = os.path.join(out_root, f"run_{ts}.log")
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        datefmt="%H:%M:%S",
        handlers=[
            logging.FileHandler(log_path, encoding="utf-8"),
            logging.StreamHandler(),           # still prints to console
        ]
    )
    logging.info(f"Log file: {log_path}")
    return log_path

# $env:DEWEY_API_KEY = 'akv1_dYMxDzorT7dLUTWISTtPRGvhOKQwgISURZP'
# ----------------------------------------------------------------------------
# Environment
# ----------------------------------------------------------------------------
API_KEY = os.environ.get("DEWEY_API_KEY")   # required only for a real pull, not the dry run

Z_ROOT = os.environ.get("DEWEY_Z_ROOT")
if not Z_ROOT:
    raise SystemExit(
        "ERROR: DEWEY_Z_ROOT is not set.\n"
        r"  PowerShell: $env:DEWEY_Z_ROOT = 'C:\Users\kalebkja\dewey-downloads'"
    )

MASTER_CSV = os.environ.get(
    "UST_MASTER_CSV",
    os.path.join(os.path.dirname(__file__), "..", "..", "..",
                 "Data", "Processed", "Master_Harmonized_UST_Tanks.csv"))

OUT_ROOT     = os.path.join(Z_ROOT, "_reduced_near_ust")
SCRATCH      = os.path.join(Z_ROOT, "_scratch")
GRID_PARQUET = os.path.join(OUT_ROOT, "facility_grid.parquet")

DEWEY_RUN   = os.environ.get("DEWEY_RUN", "uvx")   # "uvx" (no install) or "pip"
NUM_WORKERS = os.environ.get("DEWEY_WORKERS", "8")

GRID_DELTA   = 0.0025        # ~0.25 km cells; +8 neighbours => ~0.5 km capture
STUDY_STATES = ["TX","AL","AR","CO","LA","ME","NJ","NM","OK","PA","TN",
                "ID","KS","KY","MD","MA","MN","NC","ND","OH","VA",
                "AZ","CT","FL","IA","MI"]   # coarse REGION pre-filter; None = all US

# ----------------------------------------------------------------------------
# Dataset registry
#   project_id  : tail of Dewey API URL after ".../data/" (None = local source)
#   local_dir   : path to pre-downloaded files (set when project_id is None)
#   partitioned : True = download one month at a time
#   lat/lon     : column names for grid filter
#   region      : column name for state pre-filter
#   keep_cols   : columns to retain in reduced output
# ----------------------------------------------------------------------------
DATASETS = {
    "safegraph_places": dict(
        project_id="prj_ycsuakbe__fldr_b7faazxwmt47zdme8",
        local_dir=None,
        partitioned=False,
        lat="LATITUDE", lon="LONGITUDE", region="REGION",
        keep_cols=["PLACEKEY","LOCATION_NAME","BRANDS","STORE_ID","NAICS_CODE",
                   "NAICS_CODE_2022","TOP_CATEGORY","SUB_CATEGORY","LATITUDE",
                   "LONGITUDE","STREET_ADDRESS","CITY","REGION","POSTAL_CODE",
                   "OPENED_ON","CLOSED_ON","POLYGON_WKT","WKT_AREA_SQ_METERS"]),

    "weekly_patterns_plus": dict(   # Advan — keys on ID_STORE, NO placekey
        project_id="prj_ycsuakbe__fldr_94nvmmhcmp9rrr6mc",
        local_dir=None,
        partitioned=True,
        start="2017-01-01", end=None,
        lat="LATITUDE", lon="LONGITUDE", region="REGION",
        keep_cols=["ID_STORE","PERSISTENT_ID","BRAND","LOCATION_NAME","REGION",
                   "NAICS_CODE","STREET_ADDRESS","LATITUDE","LONGITUDE",
                   "DATE_RANGE_START","VISIT_COUNTS","VISITOR_COUNTS",
                   "MEDIAN_DWELL","DISTANCE_FROM_HOME"]),

    "safegraph_spend": dict(
        project_id="prj_ycsuakbe__fldr_fhx7b4q9kudjwmk4r",
        local_dir=None,
        partitioned=True,
        start="2019-01-01", end=None,
        lat="LATITUDE", lon="LONGITUDE", region="REGION",
        keep_cols=["PLACEKEY","BRANDS","REGION","NAICS_CODE","LATITUDE","LONGITUDE",
                   "SPEND_DATE_RANGE_START","RAW_TOTAL_SPEND","RAW_NUM_TRANSACTIONS",
                   "RAW_NUM_CUSTOMERS","MEDIAN_SPEND_PER_CUSTOMER",
                   "MEDIAN_SPEND_PER_TRANSACTION","ONLINE_SPEND"]),

    "attom_tax_assessor": dict(
        project_id=None,                            # already on server — no download needed
        local_dir=r"D:\Dewey\ATTOM\Assessor History",
        partitioned=False,
        lat="LATITUDE", lon="LONGITUDE",            # confirmed DOUBLE in schema
        region="SITUSSTATECODE",
        keep_cols=[
            # --- identity & time ---
            "ATTOMID",
            "ASSESSORHISTORYYEAR",
            "TAXYEARASSESSED",
            "TAXFISCALYEAR",
            "PUBLICATIONDATE",
            "ASSRLASTUPDATED",
            "LASTASSESSORTAXROLLUPDATE",
            # --- geography ---
            "LATITUDE",
            "LONGITUDE",
            "SITUSSTATECODE",
            "SITUSCOUNTY",
            "SITUSSTATECOUNTYFIPS",
            "CBSACODE",
            "CBSANAME",
            "MSACODE",
            "MSANAME",
            "PROPERTYJURISDICTIONNAME",
            "CENSUSFIPSPLACECODE",
            "CENSUSTRACT",
            "NEIGHBORHOODCODE",
            # --- situs address ---
            "PROPERTYADDRESSFULL",
            "PROPERTYADDRESSHOUSENUMBER",
            "PROPERTYADDRESSSTREETNAME",
            "PROPERTYADDRESSCITY",
            "PROPERTYADDRESSSTATE",
            "PROPERTYADDRESSZIP",
            # --- property type & use ---
            "PROPERTYUSESTANDARDIZED",
            "ZONEDCODELOCAL",
            "COMPANYFLAG",
            "STATUSOWNEROCCUPIEDFLAG",
            "OWNERTYPEDESCRIPTION1",
            "OWNERTYPEDESCRIPTION2",
            "OWNERSHIPVESTINGRELATIONCODE",
            # --- size ---
            "AREABUILDING",
            "AREALOTSF",
            "AREALOTACRES",
            "YEARBUILT",
            "UNITSCOUNT",
            "BUILDINGSCOUNT",
            # --- assessed & market values ---
            "TAXASSESSEDVALUETOTAL",
            "TAXASSESSEDVALUELAND",
            "TAXASSESSEDVALUEIMPROVEMENTS",
            "TAXASSESSEDIMPROVEMENTSPERC",
            "TAXMARKETVALUETOTAL",
            "TAXMARKETVALUELAND",
            "TAXMARKETVALUEIMPROVEMENTS",
            "TAXMARKETVALUEYEAR",
            "TAXBILLEDAMOUNT",
            # --- sales & transfers ---
            "ASSESSORLASTSALEAMOUNT",
            "ASSESSORLASTSALEDATE",
            "LASTOWNERSHIPTRANSFERDATE",
            "LASTOWNERSHIPTRANSFERDOCUMENTNUMBER",
            "GEOQUALITYCODE",
        ]),
}


# ----------------------------------------------------------------------------
# Grid build
# ----------------------------------------------------------------------------
def build_facility_grid(force=False):
    if os.path.exists(GRID_PARQUET) and not force:
        return GRID_PARQUET
    os.makedirs(OUT_ROOT, exist_ok=True)
    src = MASTER_CSV.replace(os.sep, "/")
    dst = GRID_PARQUET.replace(os.sep, "/")
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


# ----------------------------------------------------------------------------
# Shared reduce logic (works for both scratch downloads and local files)
# ----------------------------------------------------------------------------
def _reduce_files(cfg, files, out_path):
    """Grid-filter + state-filter an explicit list of parquet files -> out_path."""
    if not files:
        print("    (no parquet files found)"); return 0
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
    n = con.execute(
        f"SELECT count(*) FROM read_parquet('{out_path.replace(os.sep,'/')}')"
    ).fetchone()[0]
    con.close()
    return n


def _reduce_scratch(cfg, out_path):
    """Convenience wrapper: reduce whatever is currently in SCRATCH."""
    files = glob.glob(os.path.join(SCRATCH, "**", "*.parquet"), recursive=True)
    if not files:
        print("    (no parquet in this window)"); return 0
    return _reduce_files(cfg, files, out_path)


# ----------------------------------------------------------------------------
# Dewey CLI download helpers
# ----------------------------------------------------------------------------
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
    if after:  cmd += ["--partition-key-after",  after]
    if before: cmd += ["--partition-key-before", before]
    return cmd


def _run_cli(cmd):
    safe = ["***" if c == API_KEY else c for c in cmd]
    print("  RUN:", " ".join(safe))
    subprocess.run(cmd, cwd=SCRATCH, check=True)    # deweypy writes under cwd


def _wipe_scratch():
    shutil.rmtree(SCRATCH, ignore_errors=True)
    os.makedirs(SCRATCH, exist_ok=True)


# ----------------------------------------------------------------------------
# Main pipeline: Dewey download path
# ----------------------------------------------------------------------------
def pull_reduce(name, dry_run=True):
    """Download from Dewey, reduce each window, wipe scratch. For remote datasets."""
    cfg = DATASETS[name]
    pid = cfg.get("project_id")
    if not pid:
        raise AssertionError(
            f"'{name}' has no project_id — use pull_reduce_local() instead")
    if pid.startswith("PASTE"):
        raise AssertionError(
            f"set project_id for '{name}' (README: API URL tail after data/)")

    build_facility_grid()
    out_dir = os.path.join(OUT_ROOT, f"{name}_near_ust")
    os.makedirs(out_dir, exist_ok=True)

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
        _run_cli(_dewey_cmd(pid, after=a, before=b))
        tag = a or "all"
        n = _reduce_scratch(cfg, os.path.join(out_dir, f"part_{tag}.parquet"))
        print(f"  [{name}] {tag}: kept {n:,} near-UST rows")
        _wipe_scratch()
    print(f"[{name}] DONE -> {out_dir} (POIs near UST facilities, all types)")


# ----------------------------------------------------------------------------
# Main pipeline: local file path (ATTOM and any future pre-downloaded datasets)
# ----------------------------------------------------------------------------
_STATE_PAT = re.compile(r'^([A-Z]{2})(_.*)?$')


def _local_files_by_state(local_dir, study_states=None):
    """
    Walk ATTOM-style state subfolders (TX, TX_post17, TX_pre16, CA_14_23 …).
    Extracts the 2-letter prefix from each folder name and filters to study_states.
    Returns (file_list, summary_lines).
    """
    if not os.path.exists(local_dir):
        raise SystemExit(
            f"local_dir not found: {local_dir}\n"
            f"  Check the path or ensure the drive is mounted.")
    files, summary = [], []
    for entry in sorted(os.scandir(local_dir), key=lambda e: e.name):
        if not entry.is_dir():
            continue
        m = _STATE_PAT.match(entry.name)
        if not m:
            continue
        state = m.group(1)
        if study_states and state not in study_states:
            continue
        found = glob.glob(os.path.join(entry.path, "**", "*.parquet"), recursive=True)
        files.extend(found)
        summary.append(f"  {entry.name:20s} ({state})  {len(found):4d} files")
    return files, summary


def pull_reduce_local(name):
    """Reduce a dataset that is already downloaded on the server — no CLI step."""
    cfg = DATASETS[name]
    local_dir = cfg.get("local_dir")
    if not local_dir:
        raise AssertionError(f"'{name}' has no local_dir — use pull_reduce() instead")

    build_facility_grid()

    files, summary = _local_files_by_state(local_dir, study_states=STUDY_STATES)
    print(f"[{name}] LOCAL source: {local_dir}")
    print(f"[{name}] Study-state folders matched ({len(summary)}):")
    for s in summary:
        print(s)
    print(f"[{name}] Total files to scan: {len(files)}")
    if not files:
        raise SystemExit(
            f"[{name}] No .parquet files found in study-state folders.\n"
            f"  Check STUDY_STATES or local_dir path.")

    out_dir  = os.path.join(OUT_ROOT, f"{name}_near_ust")
    os.makedirs(out_dir, exist_ok=True)
    out_path = os.path.join(out_dir, "part_all.parquet")

    print(f"[{name}] Running grid-filter reduce …")
    n = _reduce_files(cfg, files, out_path)
    print(f"[{name}] DONE — kept {n:,} near-UST rows -> {out_path}")


# ----------------------------------------------------------------------------
# Entry point
# ----------------------------------------------------------------------------
if __name__ == "__main__":
    import sys
    build_facility_grid()

    if len(sys.argv) >= 3 and sys.argv[1] == "run":
        name = sys.argv[2]
        if name not in DATASETS:
            raise SystemExit(f"Unknown dataset '{name}'. Choose from: {list(DATASETS)}")
        cfg = DATASETS[name]
        if cfg.get("local_dir"):
            pull_reduce_local(name)         # pre-downloaded on server
        else:
            pull_reduce(name, dry_run=False) # download from Dewey
    else:
        # Default: DRY RUN — build grid + print plan for every dataset, no download
        for ds, cfg in DATASETS.items():
            if cfg.get("local_dir"):
                # Local datasets: just report path, no dry-run concept needed
                exists = os.path.exists(cfg["local_dir"])
                status = "OK" if exists else "NOT FOUND"
                print(f"[{ds}] LOCAL [{status}]: {cfg['local_dir']}")
            else:
                try:
                    pull_reduce(ds, dry_run=True)
                except (AssertionError, KeyError) as e:
                    print(f"SKIP {ds}: {e}")