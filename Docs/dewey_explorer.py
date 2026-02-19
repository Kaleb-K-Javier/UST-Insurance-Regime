#!/usr/bin/env python3
"""
================================================================================
DEWEY DATA EXPLORER — Full Schema + Sample + Profiling for Research Team
================================================================================
Purpose:  Scan every Dewey dataset you have access to. For each one, pull:
            - Complete column schema (name, type)
            - 5-row sample (head)
            - Column-level profiling (nulls, uniques, min/max, example values)
            - Estimated row count (from metadata)
          
          Output goes to:
            - Console (live progress)
            - dewey_explorer_report.txt  (full text report for team review)
            - Individual CSVs per dataset (full 5-row samples with all columns)
            - dewey_explorer_summary.csv (one-row-per-column across all datasets)

Setup:    pip install deweypy duckdb pandas tabulate
          (or: uv pip install deweypy duckdb pandas tabulate)

Usage:    python dewey_explorer.py

IMPORTANT: You need folder IDs from the Dewey UI. For each dataset:
           1. Go to https://app.deweydata.io
           2. Navigate to the dataset
           3. Click "API Access" or "Data Access"
           4. Copy the folder ID from the URL: 
              api.deweydata.io/api/v1/external/data/fldr_XXXXXXX
                                                    ^^^^^^^^^^^^^
           5. Paste it below in the DATASETS dict
================================================================================
"""

import os
import sys
import datetime
import json
import csv
from pathlib import Path
from collections import OrderedDict

# ===========================================================================
# CONFIGURATION — EDIT THIS SECTION
# ===========================================================================

DEWEY_API_KEY = "YOUR_API_KEY_HERE"

OUTPUT_DIR = Path("./dewey_explorer_output")

# How many rows to pull as a sample from each dataset
SAMPLE_ROWS = 20

# How many files to scan per dataset (more = slower but better coverage)
MAX_FILES_PER_DATASET = 10

# For profiling: how many rows to scan for null/unique stats
# (larger = more accurate but slower; uses remote parquet scans)
PROFILE_ROWS = 10000

# ---------------------------------------------------------------------------
# DATASETS TO EXPLORE
# ---------------------------------------------------------------------------
# Add every dataset you want to inspect. The script will pull full schemas
# and samples for each one — no pre-filtering, no assumptions about which
# columns matter. Your team reviews everything.
#
# FORMAT:
#   "short_name": {
#       "folder_id": "fldr_XXXXXXX",   <-- from Dewey UI
#       "label": "Human-readable name",
#       "format": "parquet" or "csv.gz",
#       "notes": "Any context for the team"
#   }
#
# TIP: Start by adding folder IDs for 2-3 datasets. Run the script.
#      Then add more and re-run — it appends to the report.
# ---------------------------------------------------------------------------

DATASETS = OrderedDict([
    # === POI / PLACES ===
    ("safegraph_places", {
        "folder_id": None,
        "label": "SafeGraph — Global Places & Geometry",
        "format": "parquet",
        "notes": "POI directory: brand names, NAICS, lat/lon, open/close dates",
    }),
    ("safegraph_spend", {
        "folder_id": None,
        "label": "SafeGraph — Spend Patterns",
        "format": "parquet",
        "notes": "Card transaction aggregates at POI level",
    }),
    ("dataplor_poi", {
        "folder_id": None,
        "label": "Dataplor — Global POI",
        "format": "parquet",
        "notes": "Alternative POI source, global coverage",
    }),

    # === FOOT TRAFFIC ===
    ("advan_weekly_patterns", {
        "folder_id": None,
        "label": "Advan Research — Weekly Patterns Plus",
        "format": "parquet",
        "notes": "Foot traffic at POI level, weekly. placekey linkage.",
    }),
    ("advan_neighborhood", {
        "folder_id": None,
        "label": "Advan Research — Neighborhood Patterns Plus",
        "format": "parquet",
        "notes": "CBG-level foot traffic aggregates",
    }),
    ("pass_by_visits", {
        "folder_id": None,
        "label": "pass_by — Retail Store Visits",
        "format": "parquet",
        "notes": "AI-powered foot traffic, 3M+ POIs, free with subscription",
    }),
    ("pass_by_visitors", {
        "folder_id": None,
        "label": "pass_by — Retail Store Visitors",
        "format": "parquet",
        "notes": "Visitor demographics/home locations",
    }),
    ("veraset_visits", {
        "folder_id": None,
        "label": "Veraset — Visits (institutional only)",
        "format": "parquet",
        "notes": "Device-level mobility, institutional license required",
    }),

    # === COMPANY / FIRMOGRAPHICS ===
    ("brightquery_benchmarking", {
        "folder_id": None,
        "label": "BrightQuery — Company Benchmarking",
        "format": "parquet",
        "notes": "Private company financials by EIN. Critical for revenue.",
    }),
    ("brightquery_public", {
        "folder_id": None,
        "label": "BrightQuery — Public Companies",
        "format": "parquet",
        "notes": "Public company financials/employment/benefits",
    }),
    ("brightquery_industry", {
        "folder_id": None,
        "label": "BrightQuery — Industry Statistics",
        "format": "parquet",
        "notes": "NAICS-level industry aggregates",
    }),
    ("brightquery_sba", {
        "folder_id": None,
        "label": "BrightQuery — SBA Loans",
        "format": "parquet",
        "notes": "Small business loan data — could flag financially stressed operators",
    }),
    ("veridion_firmographics", {
        "folder_id": None,
        "label": "Veridion — Firmographics (Company Core Profiles)",
        "format": "parquet",
        "notes": "134M+ companies: NAICS, employees, revenue range, ownership",
    }),
    ("rhetorik_company", {
        "folder_id": None,
        "label": "Rhetorik — Company Data",
        "format": "parquet",
        "notes": "Technographic + office data, may have subsidiary info",
    }),
    ("rhetorik_office", {
        "folder_id": None,
        "label": "Rhetorik — Office Data",
        "format": "parquet",
        "notes": "Physical office/location data with company linkage",
    }),
    ("people_data_labs_company", {
        "folder_id": None,
        "label": "People Data Labs — Company Insights",
        "format": "parquet",
        "notes": "Aggregated employee insights per company",
    }),

    # === TRANSACTIONS / CONSUMER ===
    ("consumer_edge_ticker", {
        "folder_id": None,
        "label": "Consumer Edge — Company/Ticker-Level Transactions",
        "format": "parquet",
        "notes": "Card spend aggregated to company/ticker level",
    }),
    ("consumer_edge_naics", {
        "folder_id": None,
        "label": "Consumer Edge — Macro Spend by NAICS",
        "format": "parquet",
        "notes": "Aggregate consumer spend breakout by NAICS code",
    }),
    ("consumer_edge_behavioral", {
        "folder_id": None,
        "label": "Consumer Edge — Behavioral/Demographic Transactions",
        "format": "parquet",
        "notes": "Card transaction data with consumer demographics",
    }),
    ("consumer_edge_brand_tracker", {
        "folder_id": None,
        "label": "Consumer Edge — Brand Tracker",
        "format": "parquet",
        "notes": "Brand-level consumer metrics",
    }),
    ("pdi_cstore", {
        "folder_id": None,
        "label": "PDI Technologies — C-Store Transactions",
        "format": "parquet",
        "notes": "SKU-level POS data from independent c-stores. In-store only.",
    }),

    # === REAL ESTATE / PROPERTY ===
    ("attom_tax_assessor", {
        "folder_id": None,
        "label": "ATTOM — Tax Assessor 5.0",
        "format": "parquet",
        "notes": "Property characteristics + assessed values. 155M+ properties.",
    }),
    ("attom_assessor_history", {
        "folder_id": None,
        "label": "ATTOM — Assessor History",
        "format": "parquet",
        "notes": "Historical annual assessment snapshots ~1998-present",
    }),
    ("attom_avm", {
        "folder_id": None,
        "label": "ATTOM — Automated Valuation Model",
        "format": "parquet",
        "notes": "Market value estimates with confidence scores",
    }),
    ("attom_recorder", {
        "folder_id": None,
        "label": "ATTOM — Recorder (Deed/Mortgage Transactions)",
        "format": "parquet",
        "notes": "Property transaction records — sale prices, buyer/seller",
    }),
    ("attom_preforeclosure", {
        "folder_id": None,
        "label": "ATTOM — Pre-Foreclosure History",
        "format": "parquet",
        "notes": "Foreclosure filings — financial distress signal for operators",
    }),
    ("attom_community", {
        "folder_id": None,
        "label": "ATTOM — Community Information",
        "format": "parquet",
        "notes": "Neighborhood-level demographics and characteristics",
    }),
    ("resimplifi_cre", {
        "folder_id": None,
        "label": "REsimplifi — Commercial Real Estate Listings",
        "format": "parquet",
        "notes": "Commercial property listings — gas station properties?",
    }),
    ("verisk_property", {
        "folder_id": None,
        "label": "Verisk — Property Data",
        "format": "parquet",
        "notes": "98% US property coverage, may include commercial",
    }),

    # === CLIMATE / ENVIRONMENT ===
    ("climatecheck_risk", {
        "folder_id": None,
        "label": "ClimateCheck — US Climate Risk Data",
        "format": "parquet",
        "notes": "Property-level climate risk scores (flood, fire, heat, storm, drought)",
    }),
    ("naturequant_naturescore", {
        "folder_id": None,
        "label": "NatureQuant — NatureScore",
        "format": "parquet",
        "notes": "Nature access quality — potential environmental amenity control",
    }),
    ("custom_weather", {
        "folder_id": None,
        "label": "Custom Weather — Historical Climate Data",
        "format": "parquet",
        "notes": "US weather data 2018-present. Precipitation affects groundwater.",
    }),

    # === LABOR / WORKFORCE ===
    ("linkup_jobs", {
        "folder_id": None,
        "label": "LinkUp — Job Records",
        "format": "parquet",
        "notes": "Job postings — could signal store openings/closings",
    }),
    ("wagescape_salary", {
        "folder_id": None,
        "label": "WageScape — Job Postings with Salary",
        "format": "parquet",
        "notes": "Wage data — labor cost proxy for gas station operations",
    }),

    # === CONSTRUCTION / PERMITS ===
    ("builty_permits", {
        "folder_id": None,
        "label": "Builty — US Building Permits",
        "format": "parquet",
        "notes": "Construction permits — could capture UST installation/removal permits",
    }),

    # === FINANCE ===
    ("7chord_bonds", {
        "folder_id": None,
        "label": "7 Chord — Predictive Bond Prices",
        "format": "parquet",
        "notes": "Corporate bond pricing — financial distress for large operators",
    }),

    # === GOVERNMENT ===
    ("lobbying_data", {
        "folder_id": None,
        "label": "LobbyingData — US Lobbying Data",
        "format": "parquet",
        "notes": "Lobbying mapped to bills — petroleum industry lobbying?",
    }),

    # === OTHER / EXPLORATORY ===
    ("warn_layoffs", {
        "folder_id": None,
        "label": "WARN Database — Layoff Data",
        "format": "parquet",
        "notes": "Mass layoff notices — large operator distress events",
    }),
])


# ===========================================================================
# ENGINE — DO NOT EDIT BELOW UNLESS DEBUGGING
# ===========================================================================

def install_deps():
    """Ensure all dependencies are available."""
    required = ["deweypy", "duckdb", "pandas", "tabulate"]
    missing = []
    for pkg in required:
        try:
            __import__(pkg)
        except ImportError:
            missing.append(pkg)
    if missing:
        print(f"Installing: {', '.join(missing)}")
        os.system(f"{sys.executable} -m pip install {' '.join(missing)} --quiet")


def authenticate():
    from deweypy.auth import set_api_key
    set_api_key(DEWEY_API_KEY)


def get_file_urls(folder_id, max_files=MAX_FILES_PER_DATASET):
    """Retrieve file URLs from Dewey API."""
    from deweypy.download.synchronous import get_dataset_files
    try:
        urls = get_dataset_files(folder_id, to_list=True)
        return urls[:max_files], len(urls)
    except Exception as e:
        return None, str(e)


def profile_dataset(urls, n_profile=PROFILE_ROWS, n_sample=SAMPLE_ROWS):
    """
    Full inspection of a parquet dataset:
      - Schema (all columns + types)
      - Sample rows (head)
      - Per-column profiling: null_count, null_pct, n_unique, min, max, 
        top_3_values, avg (numeric), example (text)
      - Estimated total row count from file metadata
    """
    import duckdb
    import pandas as pd

    con = duckdb.connect()
    con.execute("INSTALL httpfs; LOAD httpfs;")

    result = {
        "schema": [],
        "sample_df": None,
        "profile": [],
        "total_files": 0,
        "est_row_count": None,
        "errors": [],
    }

    try:
        # ----- SCHEMA -----
        schema_df = con.execute(
            "DESCRIBE SELECT * FROM read_parquet($urls)",
            {"urls": urls[:1]}
        ).df()
        result["schema"] = list(zip(
            schema_df["column_name"].tolist(),
            schema_df["column_type"].tolist()
        ))
        all_cols = schema_df["column_name"].tolist()

        # ----- SAMPLE (head) -----
        sample_df = con.execute(
            f"SELECT * FROM read_parquet($urls) LIMIT {n_sample}",
            {"urls": urls[:3]}
        ).df()
        result["sample_df"] = sample_df

        # ----- ESTIMATED ROW COUNT -----
        try:
            count_df = con.execute(
                "SELECT COUNT(*) as cnt FROM read_parquet($urls)",
                {"urls": urls[:3]}
            ).df()
            per_file = count_df["cnt"].iloc[0] / min(3, len(urls))
            result["est_row_count"] = int(per_file * len(urls))
            result["exact_count_sampled_files"] = int(count_df["cnt"].iloc[0])
        except:
            result["est_row_count"] = "unknown"

        # ----- PER-COLUMN PROFILING -----
        # Pull a profiling sample
        profile_df = con.execute(
            f"SELECT * FROM read_parquet($urls) USING SAMPLE {n_profile}",
            {"urls": urls[:5]}
        ).df()

        for col_name, col_type in result["schema"]:
            col_profile = {
                "column": col_name,
                "type": col_type,
                "null_count": None,
                "null_pct": None,
                "n_unique": None,
                "min": None,
                "max": None,
                "mean": None,
                "top_values": None,
                "example": None,
            }

            try:
                series = profile_df[col_name] if col_name in profile_df.columns else None
                if series is not None:
                    n_total = len(series)
                    n_null = int(series.isna().sum())
                    col_profile["null_count"] = n_null
                    col_profile["null_pct"] = round(100.0 * n_null / max(n_total, 1), 1)

                    non_null = series.dropna()
                    col_profile["n_unique"] = int(non_null.nunique())

                    # Example values (first 3 non-null unique)
                    examples = non_null.unique()[:3]
                    col_profile["example"] = [str(v)[:80] for v in examples]

                    # Top 3 most frequent values
                    if len(non_null) > 0:
                        vc = non_null.value_counts().head(3)
                        col_profile["top_values"] = [
                            f"{str(v)[:50]} ({c})" for v, c in zip(vc.index, vc.values)
                        ]

                    # Numeric stats
                    if pd.api.types.is_numeric_dtype(series):
                        col_profile["min"] = str(non_null.min()) if len(non_null) else None
                        col_profile["max"] = str(non_null.max()) if len(non_null) else None
                        col_profile["mean"] = str(round(non_null.mean(), 4)) if len(non_null) else None

                    # Date/string min/max
                    elif pd.api.types.is_datetime64_any_dtype(series):
                        col_profile["min"] = str(non_null.min()) if len(non_null) else None
                        col_profile["max"] = str(non_null.max()) if len(non_null) else None

                    elif pd.api.types.is_string_dtype(series) or pd.api.types.is_object_dtype(series):
                        # For strings: show length stats
                        lengths = non_null.astype(str).str.len()
                        col_profile["min"] = f"len={int(lengths.min())}" if len(lengths) else None
                        col_profile["max"] = f"len={int(lengths.max())}" if len(lengths) else None
                        col_profile["mean"] = f"avg_len={round(lengths.mean(), 1)}" if len(lengths) else None

            except Exception as e:
                col_profile["example"] = [f"PROFILE_ERROR: {str(e)[:60]}"]

            result["profile"].append(col_profile)

    except Exception as e:
        result["errors"].append(str(e))

    finally:
        con.close()

    return result


def format_report_section(dataset_key, config, inspection):
    """Format a single dataset's results as readable text."""
    lines = []
    sep = "═" * 100

    lines.append(sep)
    lines.append(f"  DATASET: {config['label']}")
    lines.append(f"  Key:     {dataset_key}")
    lines.append(f"  Notes:   {config.get('notes', '')}")
    lines.append(f"  Format:  {config.get('format', 'parquet')}")
    lines.append(sep)

    if inspection.get("errors"):
        for err in inspection["errors"]:
            lines.append(f"  *** ERROR: {err}")
        lines.append("")
        return "\n".join(lines)

    # Row count estimate
    lines.append(f"  Estimated total rows: {inspection.get('est_row_count', '?'):,}" 
                 if isinstance(inspection.get('est_row_count'), int) 
                 else f"  Estimated total rows: {inspection.get('est_row_count', '?')}")
    lines.append(f"  Total columns: {len(inspection.get('schema', []))}")
    lines.append(f"  Files scanned: {inspection.get('total_files', '?')}")
    lines.append("")

    # Full schema + profiling table
    lines.append("  COLUMN PROFILING")
    lines.append("  " + "─" * 96)
    header = f"  {'#':<4} {'Column Name':<35} {'Type':<15} {'Nulls%':<8} {'Unique':<8} {'Min':<15} {'Max':<15}"
    lines.append(header)
    lines.append("  " + "─" * 96)

    for i, prof in enumerate(inspection.get("profile", []), 1):
        null_pct = f"{prof['null_pct']}%" if prof['null_pct'] is not None else "?"
        n_uniq = str(prof['n_unique']) if prof['n_unique'] is not None else "?"
        mn = str(prof['min'])[:14] if prof['min'] is not None else ""
        mx = str(prof['max'])[:14] if prof['max'] is not None else ""
        line = f"  {i:<4} {prof['column']:<35} {prof['type']:<15} {null_pct:<8} {n_uniq:<8} {mn:<15} {mx:<15}"
        lines.append(line)

        # Show top values and examples indented below
        if prof.get("top_values"):
            tv = " | ".join(prof["top_values"][:3])
            lines.append(f"         Top: {tv[:90]}")
        if prof.get("example"):
            ex = " | ".join(prof["example"][:3])
            lines.append(f"         Ex:  {ex[:90]}")
        if prof.get("mean") and prof["mean"]:
            lines.append(f"         Mean: {prof['mean']}")

    lines.append("  " + "─" * 96)
    lines.append("")

    # Sample rows (head)
    if inspection.get("sample_df") is not None:
        lines.append(f"  SAMPLE DATA ({len(inspection['sample_df'])} rows)")
        lines.append("  " + "─" * 96)
        try:
            from tabulate import tabulate
            sample_str = tabulate(
                inspection["sample_df"].head(10),
                headers="keys",
                tablefmt="simple",
                showindex=False,
                maxcolwidths=30,
            )
            for line in sample_str.split("\n"):
                lines.append(f"  {line}")
        except ImportError:
            sample_str = inspection["sample_df"].head(10).to_string(
                max_rows=10, max_cols=15, max_colwidth=30
            )
            for line in sample_str.split("\n"):
                lines.append(f"  {line}")
        lines.append("")

    lines.append("")
    return "\n".join(lines)


def build_summary_csv(all_results):
    """Build a single CSV with one row per column across all datasets."""
    rows = []
    for dataset_key, config, inspection in all_results:
        for prof in inspection.get("profile", []):
            rows.append({
                "dataset": dataset_key,
                "dataset_label": config["label"],
                "column_name": prof["column"],
                "column_type": prof["type"],
                "null_pct": prof.get("null_pct"),
                "n_unique": prof.get("n_unique"),
                "min": prof.get("min"),
                "max": prof.get("max"),
                "mean": prof.get("mean"),
                "top_values": " | ".join(prof["top_values"][:3]) if prof.get("top_values") else "",
                "examples": " | ".join(prof["example"][:3]) if prof.get("example") else "",
            })
    return rows


# ===========================================================================
# MAIN
# ===========================================================================

def main():
    if DEWEY_API_KEY == "YOUR_API_KEY_HERE":
        print("=" * 70)
        print("  DEWEY DATA EXPLORER — SETUP REQUIRED")
        print("=" * 70)
        print()
        print("  1. Get your API key from: https://app.deweydata.io/settings")
        print("     Paste it into DEWEY_API_KEY at line ~33")
        print()
        print("  2. Get folder IDs from the Dewey UI for each dataset:")
        print("     Dataset page → 'API Access' → copy fldr_XXXXXXX")
        print("     Paste into the DATASETS dict starting at line ~65")
        print()
        print("  AVAILABLE DATASETS TO CONFIGURE:")
        print("  " + "─" * 66)
        for key, cfg in DATASETS.items():
            status = "✓ READY" if cfg["folder_id"] else "○ needs folder_id"
            print(f"    {status:<16} {key:<30} {cfg['label']}")
        print()
        print("  Start with just 2-3 datasets. You can re-run to add more.")
        print()
        sys.exit(0)

    install_deps()
    authenticate()
    print("✓ Authenticated with Dewey\n")

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Partition datasets
    configured = {k: v for k, v in DATASETS.items() if v["folder_id"]}
    skipped = {k: v for k, v in DATASETS.items() if not v["folder_id"]}

    print(f"Configured: {len(configured)} datasets")
    print(f"Skipped (no folder_id): {len(skipped)} datasets\n")

    if not configured:
        print("No datasets have folder IDs set. Edit the DATASETS dict and re-run.")
        sys.exit(1)

    all_results = []
    report_lines = []

    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    report_lines.append("=" * 100)
    report_lines.append(f"  DEWEY DATA EXPLORER — FULL RECONNAISSANCE REPORT")
    report_lines.append(f"  Generated: {timestamp}")
    report_lines.append(f"  Datasets scanned: {len(configured)}")
    report_lines.append(f"  Sample rows per dataset: {SAMPLE_ROWS}")
    report_lines.append(f"  Profile sample: {PROFILE_ROWS:,} rows")
    report_lines.append("=" * 100)
    report_lines.append("")

    for i, (key, config) in enumerate(configured.items(), 1):
        print(f"[{i}/{len(configured)}] {config['label']}")
        print(f"    Folder: {config['folder_id']}")

        # Discover files
        print(f"    → Discovering files...")
        urls, total_or_err = get_file_urls(config["folder_id"])

        if urls is None:
            print(f"    ✗ Error: {total_or_err}")
            inspection = {"errors": [total_or_err]}
            all_results.append((key, config, inspection))
            report_lines.append(format_report_section(key, config, inspection))
            continue

        total_files = total_or_err
        print(f"    → Found {total_files} files (scanning {min(len(urls), MAX_FILES_PER_DATASET)})")

        # Profile
        if config.get("format") == "csv.gz":
            print(f"    ⚠ CSV format — download locally first, then convert to parquet.")
            print(f"      Use: python -m deweypy download {config['folder_id']} --output ./dewey_data/{key}/")
            inspection = {"errors": ["CSV format requires local download first"]}
            all_results.append((key, config, inspection))
            report_lines.append(format_report_section(key, config, inspection))
            continue

        print(f"    → Pulling schema + profiling ({PROFILE_ROWS:,} row sample)...")
        inspection = profile_dataset(urls, n_profile=PROFILE_ROWS, n_sample=SAMPLE_ROWS)
        inspection["total_files"] = total_files

        n_cols = len(inspection.get("schema", []))
        est_rows = inspection.get("est_row_count", "?")
        print(f"    ✓ {n_cols} columns, ~{est_rows:,} est. rows" if isinstance(est_rows, int)
              else f"    ✓ {n_cols} columns, est. rows unknown")

        if inspection.get("errors"):
            for err in inspection["errors"]:
                print(f"    ⚠ {err}")

        # Save sample CSV
        if inspection.get("sample_df") is not None:
            sample_path = OUTPUT_DIR / f"{key}_sample.csv"
            inspection["sample_df"].to_csv(sample_path, index=False)
            print(f"    → Sample saved: {sample_path.name}")

        all_results.append((key, config, inspection))
        report_lines.append(format_report_section(key, config, inspection))
        print()

    # -----------------------------------------------------------------------
    # SUMMARY TABLE AT TOP OF REPORT
    # -----------------------------------------------------------------------
    summary_header = []
    summary_header.append("")
    summary_header.append("  QUICK SUMMARY")
    summary_header.append("  " + "─" * 96)
    summary_header.append(f"  {'Dataset':<30} {'Columns':<8} {'Est. Rows':<15} {'Status':<15} {'Notes'}")
    summary_header.append("  " + "─" * 96)

    for key, config, insp in all_results:
        n_cols = len(insp.get("schema", []))
        est = insp.get("est_row_count", "?")
        est_str = f"{est:,}" if isinstance(est, int) else str(est)
        status = "✓ OK" if not insp.get("errors") else "✗ ERROR"
        summary_header.append(
            f"  {key:<30} {n_cols:<8} {est_str:<15} {status:<15} {config.get('notes', '')[:40]}"
        )

    summary_header.append("  " + "─" * 96)
    summary_header.append("")
    summary_header.append("")

    # Insert summary after report header
    report_lines = report_lines[:8] + summary_header + report_lines[8:]

    # -----------------------------------------------------------------------
    # SKIPPED DATASETS LIST
    # -----------------------------------------------------------------------
    if skipped:
        report_lines.append("=" * 100)
        report_lines.append("  DATASETS NOT YET CONFIGURED (need folder_id)")
        report_lines.append("=" * 100)
        for key, config in skipped.items():
            report_lines.append(f"    ○ {key:<35} {config['label']}")
        report_lines.append("")

    # -----------------------------------------------------------------------
    # WRITE OUTPUTS
    # -----------------------------------------------------------------------

    # Main text report
    report_text = "\n".join(report_lines)
    report_path = OUTPUT_DIR / "dewey_explorer_report.txt"
    with open(report_path, "w", encoding="utf-8") as f:
        f.write(report_text)
    print(f"\n{'=' * 60}")
    print(f"  Full report: {report_path.resolve()}")

    # Summary CSV (one row per column across all datasets)
    summary_rows = build_summary_csv(all_results)
    if summary_rows:
        summary_csv_path = OUTPUT_DIR / "dewey_explorer_columns.csv"
        with open(summary_csv_path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=summary_rows[0].keys())
            writer.writeheader()
            writer.writerows(summary_rows)
        print(f"  Column summary CSV: {summary_csv_path.resolve()}")

    print(f"  Sample CSVs: {OUTPUT_DIR.resolve()}/<dataset>_sample.csv")
    print(f"{'=' * 60}")
    print()
    print("NEXT STEP: Share dewey_explorer_report.txt + dewey_explorer_columns.csv")
    print("           with your research team and paste back to Claude.")


if __name__ == "__main__":
    main()
