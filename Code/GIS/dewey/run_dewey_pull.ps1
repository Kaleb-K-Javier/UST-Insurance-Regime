# run_dewey_pull.ps1 — driver for the ucbare2 Windows server.
# Downloads each dataset ONE MONTH at a time via the deweypy CLI (uvx),
# grid-filters to POIs near UST facilities (uv + duckdb), deletes the raw month.
# Peak disk = one month. Everything runs through uv — no installs needed.
#
# Run from the repo root:  powershell -ExecutionPolicy Bypass -File Code\GIS\dewey\run_dewey_pull.ps1

# --- 1. credentials (needed only for the REAL pull in step 4, not the dry run) ---
if (-not $env:DEWEY_API_KEY) { Write-Warning "DEWEY_API_KEY not set - dry run is fine; set it before step 4 (real pull)." }

# --- 2. paths: large data now lives on D: (admin move, 2026-06; C: is small/shared).
#         dewey-downloads was moved to D:\shares\Users\kalebkja\... with a JUNCTION left
#         behind, so this C: path still works unchanged and now physically lands on D:.
#         Keep using the C: junction path here — do not hardcode a guessed D: path. ---
$env:DEWEY_Z_ROOT   = "C:\Users\kalebkja\dewey-downloads"   # C: junction -> physically D:
$env:UST_MASTER_CSV = "C:\Users\kalebkja\ust_ins_move_to_github\Data\Processed\Master_Harmonized_UST_Tanks.csv"
$env:DEWEY_RUN      = "uvx"          # deweypy CLI via uvx (no install)
$script = "Code\GIS\dewey\dewey_pull_reduce.py"

# --- 3. DRY RUN: build facility grid + print the month-by-month plan (no download) ---
uv run --with duckdb python $script

# --- 4. Real pulls (uncomment once the 3 PROJECT_IDs are set in dewey_pull_reduce.py).
#        Each runs to completion; the big one (WPP) streams one month at a time. ---
# uv run --with duckdb python $script run safegraph_places
# uv run --with duckdb python $script run weekly_patterns_plus
# uv run --with duckdb python $script run safegraph_spend

Write-Host "Dry run complete. Set the 3 PROJECT_IDs + `$env:DEWEY_API_KEY, then run step 4. See README_DEWEY_PULL.md"
