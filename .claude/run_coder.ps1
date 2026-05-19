<#
.SYNOPSIS
    Airgapped R1 coding session runner for UST research pipeline.

.DESCRIPTION
    Opens an interactive OpenRouter session pointed at a specific ticket.
    Logs the full session transcript and cleans it for Sonnet review.
    R1 reads the pseudocode, asks R-implementation questions, writes code, runs it.

.PARAMETER TicketID
    Ticket identifier, e.g. "005"

.PARAMETER Attempt
    Attempt number. Default 1. Increment on retry.

.EXAMPLE
    .\.claude\run_coder.ps1 -TicketID 005
    .\.claude\run_coder.ps1 -TicketID 005 -Attempt 2
#>

param(
    [Parameter(Mandatory=$true)]
    [string]$TicketID,

    [int]$Attempt = 1
)

$ErrorActionPreference = "Stop"

# -- Model routing --------------------------------------------------------------
# Routes Claude Code CLI through OpenRouter to DeepSeek R1.
# Auth: ANTHROPIC_AUTH_TOKEN is what OpenRouter requires (not ANTHROPIC_API_KEY).
# ANTHROPIC_API_KEY must be explicitly empty or the CLI falls back to Anthropic.
# Base URL: https://openrouter.ai/api -- no /v1 suffix (per OpenRouter docs).
$env:ANTHROPIC_BASE_URL             = "https://openrouter.ai/api"
$env:ANTHROPIC_AUTH_TOKEN           = $env:OPENROUTER_API_KEY   # set in your PowerShell profile
$env:ANTHROPIC_API_KEY              = ""                         # must be explicit empty string
$env:ANTHROPIC_DEFAULT_SONNET_MODEL = "deepseek/deepseek-r1"    # R1 as the "sonnet" slot
$env:CLAUDE_CODE_SUBAGENT_MODEL     = ""                         # prevent recursive spawning

# -- Paths ----------------------------------------------------------------------
$ticketDir  = ".claude\TICKETS"
$specFile   = "$ticketDir\${TicketID}_spec.md"
$rawLog     = "$ticketDir\${TicketID}_attempt${Attempt}_raw.txt"
$cleanLog   = "$ticketDir\${TicketID}_transcript.txt"  # Sonnet always reads this name

if (-Not (Test-Path $specFile)) {
    Write-Error "Ticket spec not found: $specFile -- run architect first"
    exit 1
}

if ($Attempt -gt 2) {
    Write-Error "Attempt $Attempt exceeds retry cap of 2. Escalate to Opus."
    exit 1
}

# -- Session banner -------------------------------------------------------------
Write-Host ""
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "  OPENROUTER R1 SESSION -- Ticket $TicketID  Attempt $Attempt" -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Spec:       $specFile"
Write-Host "Transcript: $cleanLog"
Write-Host "Model:      $($env:ANTHROPIC_DEFAULT_SONNET_MODEL)"
Write-Host ""
Write-Host "R1 will read the pseudocode and ask you R-implementation questions." -ForegroundColor Yellow
Write-Host "Answer R-specific questions only (indexing, syntax, NA handling)." -ForegroundColor Yellow
Write-Host "If R1 asks about economics or math -- type STOP and close this session." -ForegroundColor Red
Write-Host ""

# -- Launch session -------------------------------------------------------------
Start-Transcript -Path $rawLog -Force

$systemPrompt = @"
You are an expert R and Rcpp developer implementing structural econometrics code.
You are NOT an economist. Do not make economic decisions.

Your job:
1. Read the pseudocode spec in $specFile carefully.
2. Ask the researcher clarifying questions about R implementation ONLY:
   - Indexing conventions (0-based vs 1-based in R vs Rcpp)
   - NA handling at specific steps
   - data.table syntax preferences
   - Which existing functions to call vs reimplement
   - Output format and file paths
   DO NOT ask about economics, model specification, or math.
3. Once clarifications are resolved, implement the pseudocode exactly.
4. Run the code. Report: did it run (Y/N), key output numbers, any errors.
5. Do not modify anything beyond what the spec requires.
6. Do not silently catch errors (no tryCatch returning NULL).

Working directory is the repo root. Use here() for all paths.
Constants are in CLAUDE.md -- never change tolerance values.
"@

if ($Attempt -gt 1) {
    $retryNote = "The previous attempt review is in the Attempt Log section of the spec -- read it to understand what failed and what the correction requires."
} else {
    $retryNote = ""
}
$userPrompt = "Read $specFile. This is attempt $Attempt. $retryNote Start by confirming you have read the spec, then ask me any R-implementation questions before writing any code."

claude --append-system-prompt $systemPrompt $userPrompt

Stop-Transcript

# -- Clean transcript -----------------------------------------------------------
Write-Host ""
Write-Host "Cleaning transcript..." -ForegroundColor Gray
(Get-Content $rawLog -Raw) -replace "\x1B\[[0-9;]*[a-zA-Z]", "" | Set-Content $cleanLog
Remove-Item $rawLog

# -- Done -----------------------------------------------------------------------
Write-Host ""
Write-Host "==================================================" -ForegroundColor Green
Write-Host "  SESSION COMPLETE" -ForegroundColor Green
Write-Host "  Transcript saved: $cleanLog" -ForegroundColor Green
Write-Host "==================================================" -ForegroundColor Green
Write-Host ""
Write-Host "Next: return to Claude Desktop and run the reviewer agent:" -ForegroundColor Yellow
Write-Host "  'Review ticket $TicketID attempt $Attempt'" -ForegroundColor White
Write-Host ""
