#!/usr/bin/env python
# poi_probe.py — quick health + schema check for a parquet glob.
# Usage (server):
#   uv run --with duckdb python Code\GIS\poi_probe.py "C:\path\to\folder\*.parquet"
#   uv run --with duckdb python Code\GIS\poi_probe.py "C:\path\to\folder"   (auto-adds **/*.parquet)
#
# Prints: file count, PAR1 truncation check, and the column schema.
# R analogue: arrow::open_dataset(glob)$schema + a sapply() integrity loop.
import os, sys, glob, struct, duckdb

arg = sys.argv[1] if len(sys.argv) > 1 else os.environ.get("POI_GLOB", "")
if not arg:
    print("give a glob or folder, e.g. ...poi_probe.py \"C:\\dir\\*.parquet\""); sys.exit(1)

# if a folder was passed (no wildcard), search it recursively
if not any(ch in arg for ch in "*?"):
    pat = os.path.join(arg, "**", "*.parquet")
else:
    pat = arg
files = glob.glob(pat, recursive=True)
print(f"pattern: {pat}")
print(f"files matched: {len(files)}")
if not files:
    sys.exit(0)

# PAR1 trailing-magic integrity (truncated/incomplete download check)
bad = []
for f in files:
    try:
        sz = os.path.getsize(f)
        if sz < 8:
            bad.append((os.path.basename(f), "tiny")); continue
        with open(f, "rb") as fh:
            fh.seek(-4, os.SEEK_END)
            if fh.read(4) != b"PAR1":
                bad.append((os.path.basename(f), "no PAR1"))
    except Exception as e:
        bad.append((os.path.basename(f), str(e)[:30]))
print(f"truncated/bad: {len(bad)} of {len(files)}")
for b in bad[:15]:
    print("   bad:", b[0], "-", b[1])

# schema from the first healthy file
good = [f for f in files if os.path.basename(f) not in {b[0] for b in bad}]
if good:
    con = duckdb.connect()
    g = good[0].replace(os.sep, "/")
    cols = con.execute(f"DESCRIBE SELECT * FROM read_parquet('{g}') LIMIT 0").fetchall()
    n = con.execute(f"SELECT count(*) FROM read_parquet('{g}')").fetchone()[0]
    con.close()
    print(f"\nschema of {os.path.basename(good[0])} ({n:,} rows):")
    for c in cols:
        print(f"   {c[0]:<28} {c[1]}")
else:
    print("\n(no healthy file to read schema from)")
