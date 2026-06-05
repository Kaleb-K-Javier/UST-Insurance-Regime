# probe_system.ps1 — Windows/PowerShell state probe for the Dewey work. Read-only.
# Run on the server:  powershell -ExecutionPolicy Bypass -File Code\GIS\dewey\probe_system.ps1
# Paste the full output back. Decides: which python uv uses, duckdb-via-uv works,
# free disk per volume (=> chunk size), where dewey-downloads live, master file path.

"===== OS / user ====="
[System.Environment]::OSVersion.VersionString
"host=$(hostname)   user=$(whoami)   pwd=$(Get-Location)"
"PowerShell=$($PSVersionTable.PSVersion.ToString())"

"===== python on PATH ====="
foreach ($p in 'python','python3','py') {
  $c = Get-Command $p -ErrorAction SilentlyContinue
  if ($c) { "{0} -> {1}" -f $p, $c.Source; & $p --version 2>&1 } else { "$p: MISSING" }
}

"===== uv / uvx ====="
$u = Get-Command uv  -ErrorAction SilentlyContinue; if ($u) { "uv  -> $($u.Source)"; uv --version 2>&1 }
$x = Get-Command uvx -ErrorAction SilentlyContinue; if ($x) { "uvx -> $($x.Source)" } else { "uvx: MISSING" }

"===== duckdb via uv (first run fetches it, ~1 min) ====="
uv run --with duckdb python -c "import duckdb; print('duckdb', duckdb.__version__)" 2>&1

"===== disk volumes (free vs size) ====="
Get-Volume | Where-Object DriveLetter | Select-Object DriveLetter, FileSystemLabel,
  @{n='FreeGB';e={[math]::Round($_.SizeRemaining/1GB)}},
  @{n='SizeGB';e={[math]::Round($_.Size/1GB)}} | Format-Table -AutoSize

"===== dewey-downloads candidates ====="
foreach ($d in @("$env:USERPROFILE\dewey-downloads",
                 "C:\Users\kalebkja\dewey-downloads",
                 "D:\shares\Users\kalebkja\C_Drive_Portal\dewey-downloads",
                 "Z:\C_Drive_Portal\dewey-downloads")) {
  if (Test-Path $d) { "FOUND $d"; Get-ChildItem $d -ErrorAction SilentlyContinue |
      Select-Object Name | Format-Table -AutoSize }
}

"===== master UST file ====="
foreach ($f in @(".\Data\Processed\Master_Harmonized_UST_Tanks.csv",
                 "C:\Users\kalebkja\ust_ins_move_to_github\Data\Processed\Master_Harmonized_UST_Tanks.csv")) {
  if (Test-Path $f) { Get-Item $f | Select-Object FullName,
      @{n='MB';e={[math]::Round($_.Length/1MB)}} | Format-Table -AutoSize }
}

"===== reachability to Dewey ====="
try { Invoke-WebRequest https://app.deweydata.io -UseBasicParsing -TimeoutSec 8 -Method Head | Out-Null; "app.deweydata.io: reachable" }
catch { "app.deweydata.io: NOT reachable -> $($_.Exception.Message)" }
"===== done ====="
