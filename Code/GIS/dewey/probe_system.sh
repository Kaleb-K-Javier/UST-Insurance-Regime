#!/usr/bin/env bash
# ============================================================================
# probe_system.sh — report the state of the machine that will do the Dewey work.
# Run this on the SERVER (or wherever the download will run) and paste the FULL
# output back. It decides: uvx-one-liner vs pip install, the real Z mount path,
# free disk (=> download chunk size), and whether duckdb is available for the
# reduce step. Read-only; changes nothing.
# ============================================================================
echo "===== OS / shell / user ====="
uname -a 2>/dev/null; echo "SHELL=$SHELL"; whoami; echo "PWD=$(pwd)"

echo "===== python ====="
for p in python python3; do
  if command -v "$p" >/dev/null 2>&1; then printf "%s -> " "$p"; "$p" --version 2>&1; fi
done

echo "===== uv / uvx (enables the no-install one-liner) ====="
command -v uv  >/dev/null 2>&1 && { printf "uv  -> "; uv --version 2>&1; } || echo "uv: MISSING"
command -v uvx >/dev/null 2>&1 && echo "uvx: present" || echo "uvx: MISSING"

echo "===== python packages ====="
if command -v python3 >/dev/null 2>&1; then
python3 - <<'PY' 2>/dev/null
import importlib.util as u
for m in ["deweypy","duckdb","polars","pandas","pyarrow"]:
    print(f"{m}: {'present' if u.find_spec(m) else 'MISSING'}")
PY
else echo "python3 not on PATH"; fi

echo "===== duckdb CLI ====="
command -v duckdb >/dev/null 2>&1 && { printf "duckdb -> "; duckdb --version 2>&1; } || echo "duckdb CLI: MISSING (python duckdb is fine too)"

echo "===== cpu / mem ====="
nproc 2>/dev/null | sed 's/^/cores: /'; free -h 2>/dev/null || vm_stat 2>/dev/null | head -3

echo "===== disk on cwd ====="
df -h . 2>/dev/null

echo "===== candidate Dewey / Z paths (free space + contents) ====="
for d in "$DEWEY_Z_ROOT" /mnt/zdrive /mnt/z /z "$HOME/dewey-downloads" "Z:/C_Drive_Portal/dewey-downloads"; do
  if [ -n "$d" ] && [ -e "$d" ]; then
    echo "FOUND: $d"; df -h "$d" 2>/dev/null; ls -la "$d" 2>/dev/null | head -n 8; echo
  fi
done

echo "===== master UST file ====="
for f in "$UST_MASTER_CSV" "Data/Processed/Master_Harmonized_UST_Tanks.csv" \
         "$HOME/ust_ins_move_to_github/Data/Processed/Master_Harmonized_UST_Tanks.csv"; do
  [ -n "$f" ] && [ -e "$f" ] && ls -la "$f"
done

echo "===== reachability to Dewey ====="
(curl -sSI -m 8 https://app.deweydata.io >/dev/null 2>&1 && echo "app.deweydata.io: reachable") \
  || echo "app.deweydata.io: NOT reachable / no curl"
echo "===== done — paste everything above ====="
