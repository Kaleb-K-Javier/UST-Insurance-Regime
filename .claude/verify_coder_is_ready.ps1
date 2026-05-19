<#
.SYNOPSIS
    Pre-flight environment checker for the UST OpenRouter coder agent.

.DESCRIPTION
    Validates that all required directories, API keys, and files are in place
    before running the run_coder.ps1 script.
#>

$ErrorActionPreference = "Continue"
$allPassed = $true

Write-Host ""
Write-Host "══════════════════════════════════════════════════" -ForegroundColor Cyan
Write-Host "  CODER AGENT PRE-FLIGHT CHECK" -ForegroundColor Cyan
Write-Host "══════════════════════════════════════════════════" -ForegroundColor Cyan
Write-Host ""

# 1. Check OpenRouter API Key
Write-Host "Checking Environment Variables..." -ForegroundColor Yellow
if ([string]::IsNullOrWhiteSpace($env:OPENROUTER_API_KEY)) {
    Write-Host "[FAIL] OPENROUTER_API_KEY is missing from your PowerShell environment." -ForegroundColor Red
    $allPassed = $false
} else {
    Write-Host "[PASS] OPENROUTER_API_KEY is set." -ForegroundColor Green
}

# 2. Check Required Directories
Write-Host "`nChecking Required Directories..." -ForegroundColor Yellow
$requiredDirs = @(
    ".claude\TICKETS",          # Required for spec_template.md tickets
    "logs",                     # Required for R script log sinks
    "Output\Estimation_Results" # Required for .rds outputs
)

foreach ($dir in $requiredDirs) {
    if (-Not (Test-Path $dir)) {
        Write-Host "[FIX ] Creating missing directory: $dir" -ForegroundColor Gray
        New-Item -ItemType Directory -Path $dir -Force | Out-Null
        Write-Host "[PASS] $dir (Created)" -ForegroundColor Green
    } else {
        Write-Host "[PASS] $dir" -ForegroundColor Green
    }
}

# 3. Check Required Base Files
Write-Host "`nChecking Required Base Files..." -ForegroundColor Yellow
$requiredFiles = @(
    "CLAUDE.md",
    ".claude\run_coder.ps1",
    "Code\Helpers\cpp_engine.cpp",
    "Code\Helpers\improved_estimator_OPTIMIZED.r"
)

foreach ($file in $requiredFiles) {
    if (-Not (Test-Path $file)) {
        Write-Host "[FAIL] Missing critical file: $file" -ForegroundColor Red
        $allPassed = $false
    } else {
        Write-Host "[PASS] $file exists." -ForegroundColor Green
    }
}

# 4. Final Output
Write-Host ""
Write-Host "══════════════════════════════════════════════════" -ForegroundColor Cyan
if ($allPassed) {
    Write-Host "  ALL CHECKS PASSED. CODER IS READY TO RUN." -ForegroundColor Green
    Write-Host "  Command: .\.claude\run_coder.ps1 -TicketID <ID>" -ForegroundColor White
} else {
    Write-Host "  ENVIRONMENT CHECK FAILED." -ForegroundColor Red
    Write-Host "  Please fix the red errors above before running the coder." -ForegroundColor Red
}
Write-Host "══════════════════════════════════════════════════" -ForegroundColor Cyan
Write-Host ""