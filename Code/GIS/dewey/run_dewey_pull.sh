#!/usr/bin/env bash
# run_dewey_pull.sh — server driver. Downloads each dataset ONE MONTH AT A TIME via
# the deweypy CLI, grid-filters to POIs near UST facilities, deletes the raw month.
# Peak disk = one month, never ~1 TB. Run probe_system.sh FIRST.
set -euo pipefail
cd "$(dirname "$0")"

# 1) credentials + paths (or export beforehand)
export DEWEY_API_KEY="${DEWEY_API_KEY:?export DEWEY_API_KEY=... (Connections > Add Connection)}"
export DEWEY_Z_ROOT="${DEWEY_Z_ROOT:-/mnt/zdrive/C_Drive_Portal/dewey-downloads}"
export DEWEY_RUN="${DEWEY_RUN:-uvx}"     # uvx (no install) or pip
# export UST_MASTER_CSV="/path/to/Master_Harmonized_UST_Tanks.csv"

# 2) one-time deps for the REDUCE step (downloader runs via uvx, no install)
#    python -m pip install -r requirements.txt
#    uv install:  https://docs.astral.sh/uv/getting-started/installation/

# 3) DRY RUN: builds the facility grid + prints the per-dataset window plan (no download)
python dewey_pull_reduce.py

# 4) When PROJECT_IDs are set + the plan looks right, run the real pulls:
#    python - <<'PY'
#    import dewey_pull_reduce as d
#    d.pull_reduce("safegraph_places",     dry_run=False)
#    d.pull_reduce("weekly_patterns_plus", dry_run=False)
#    d.pull_reduce("safegraph_spend",      dry_run=False)
#    PY
echo "Dry run done. Set PROJECT_IDs + dry_run=False to fetch. See README_DEWEY_PULL.md"
