# T007 Phase 2-3-4 runner
# Run after Phase 1 (02b) completes. Executes in sequence.
# Usage: from repo root: .\Code\Dynamic_Model\T007_run_phases_2_3_4.ps1

Set-Location "C:\Users\kaleb\Documents\ust_ins_move_to_github"

Write-Host "=== T007 Phase 2: 04b clean action definitions ===" -ForegroundColor Cyan
Rscript Code/Dynamic_Model/04b_Replacement_Panel_Prep.R
if ($LASTEXITCODE -ne 0) { Write-Host "PHASE 2 FAILED" -ForegroundColor Red; exit 1 }
Write-Host "Phase 2 DONE" -ForegroundColor Green

Write-Host "=== T007 Phase 3: 04o 6p+FE clean estimation ===" -ForegroundColor Cyan
Rscript Code/Dynamic_Model/04o_6paramFE_Profile_Clean.R
if ($LASTEXITCODE -ne 0) { Write-Host "PHASE 3 FAILED" -ForegroundColor Red; exit 1 }
Write-Host "Phase 3 DONE" -ForegroundColor Green

Write-Host "=== T007 Phase 4: 04o TX-FF counterfactual ===" -ForegroundColor Cyan
Rscript Code/Dynamic_Model/04o_CF_TX_FlatFee.R
if ($LASTEXITCODE -ne 0) { Write-Host "PHASE 4 FAILED" -ForegroundColor Red; exit 1 }
Write-Host "Phase 4 DONE" -ForegroundColor Green

Write-Host "=== T007 Phases 2-4 all complete ===" -ForegroundColor Green
