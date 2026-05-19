<#
.SYNOPSIS
    Reviewer session runner for UST research pipeline -- Anthropic Pro account / OAuth routing.

.DESCRIPTION
    Opens an interactive Sonnet session that runs the Reviewer agent
    (see .claude/agents/reviewer.md). Mechanically verifies the R/Rcpp
    code against the pseudocode in the ticket spec. Writes the review
    result (PASS / TRANSLATION_FAIL / PSEUDOCODE_FAIL) to the ticket's
    Attempt Log.

    NOT a coder. NOT an estimator. Read-only on source files.
    Cannot fix bugs -- only classifies them and (if TRANSLATION_FAIL)
    writes the narrow correction prompt for the next coder attempt.

.PARAMETER TicketID
    Ticket identifier, e.g. "002"

.PARAMETER Attempt
    Attempt number being reviewed. Default 1.

.EXAMPLE
    .\.claude\run_reviewer_pro_api.ps1 -TicketID 002
    .\.claude\run_reviewer_pro_api.ps1 -TicketID 002 -Attempt 2
#>

param(
    [Parameter(Mandatory=$true)]
    [string]$TicketID,

    [int]$Attempt = 1
)

$ErrorActionPreference = "Stop"

# -- Output-token cap -----------------------------------------------------------
# Intentionally NOT capped on Pro-account routing. See run_coder_pro_api.ps1
# header comment and memory file feedback_no_token_cap_pro_routing.md.

# -- Paths ----------------------------------------------------------------------
$ticketDir       = ".claude\TICKETS"
$specFile        = "$ticketDir\${TicketID}_spec.md"
$coderTranscript = "$ticketDir\${TicketID}_transcript.txt"
$coderRawLog     = "$ticketDir\${TicketID}_attempt${Attempt}_raw.txt"
$reviewRaw       = "$ticketDir\${TicketID}_review_attempt${Attempt}_raw.txt"
$reviewClean     = "$ticketDir\${TicketID}_review_attempt${Attempt}.txt"
$reviewerAgent   = ".claude\agents\reviewer.md"

if (-Not (Test-Path $specFile)) {
    Write-Error "Ticket spec not found: $specFile"
    exit 1
}

if (-Not (Test-Path $reviewerAgent)) {
    Write-Error "Reviewer agent doc not found: $reviewerAgent"
    exit 1
}

# Reviewer expects ${ID}_transcript.txt. If only the raw Start-Transcript
# log exists (cleanup step in run_coder_pro_api.ps1 did not run), strip
# ANSI codes into the canonical transcript path before launching the review.
if (-Not (Test-Path $coderTranscript)) {
    if (Test-Path $coderRawLog) {
        Write-Host "Cleaning coder raw log into canonical transcript..." -ForegroundColor Gray
        (Get-Content $coderRawLog -Raw) -replace "\x1B\[[0-9;]*[a-zA-Z]", "" `
            | Set-Content $coderTranscript
        Write-Host "  -> $coderTranscript" -ForegroundColor Gray
    } else {
        Write-Host "WARNING: no transcript found ($coderTranscript or $coderRawLog)." -ForegroundColor Yellow
        Write-Host "         Reviewer will work from git diff + spec only." -ForegroundColor Yellow
    }
}

# -- Session banner -------------------------------------------------------------
Write-Host ""
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host "  ANTHROPIC PRO REVIEWER -- Ticket $TicketID  Attempt $Attempt" -ForegroundColor Cyan
Write-Host "==================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Spec:       $specFile"
Write-Host "Transcript: $coderTranscript"
Write-Host "Routing:    Anthropic Pro account (OAuth)"
Write-Host "MaxTokens:  uncapped (Pro account; see header comment)"
Write-Host ""
Write-Host "Reviewer reads spec + transcript + git diff." -ForegroundColor Yellow
Write-Host "Output: PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL appended to spec." -ForegroundColor Yellow
Write-Host "Reviewer cannot Write to source files. Mechanical checklist only." -ForegroundColor Red
Write-Host ""

# -- Launch session -------------------------------------------------------------
Start-Transcript -Path $reviewRaw -Force

# Reviewer system prompt: full agent doc + scope reinforcement
$agentDoc     = Get-Content $reviewerAgent -Raw
$systemPrompt = @"
$agentDoc

ADDITIONAL OPERATIONAL CONSTRAINTS FOR THIS RUN:
- Do not Write or Edit any file in Code/ or Output/.
- You MAY Edit only $specFile to append the Attempt Log block per the
  agent doc.
- If you find criteria you cannot mechanically verify, mark them
  UNVERIFIABLE rather than guessing PASS or FAIL.
- Working directory is the repo root. Use here() conventions when
  reading R files. Use git diff (Bash tool) to see what changed.
"@

$userPrompt = "Review ticket $TicketID attempt $Attempt. Read $specFile, then $coderTranscript (if present), then `git diff` for the files changed in this ticket. Run the mechanical checklist from your agent doc and append the result to the Attempt Log section of $specFile. Print the final REVIEW COMPLETE block to stdout."

claude --append-system-prompt $systemPrompt $userPrompt

Stop-Transcript

# -- Clean transcript -----------------------------------------------------------
Write-Host ""
Write-Host "Cleaning review transcript..." -ForegroundColor Gray
(Get-Content $reviewRaw -Raw) -replace "\x1B\[[0-9;]*[a-zA-Z]", "" `
    | Set-Content $reviewClean
Remove-Item $reviewRaw

# -- Done -----------------------------------------------------------------------
Write-Host ""
Write-Host "==================================================" -ForegroundColor Green
Write-Host "  REVIEW SESSION COMPLETE" -ForegroundColor Green
Write-Host "  Review transcript saved: $reviewClean" -ForegroundColor Green
Write-Host "==================================================" -ForegroundColor Green
Write-Host ""
Write-Host "Next:" -ForegroundColor Yellow
Write-Host "  Read the Attempt Log at the bottom of $specFile" -ForegroundColor White
Write-Host "  - PASS              -> commit and move to the next ticket" -ForegroundColor White
Write-Host "  - TRANSLATION_FAIL  -> launch coder attempt 2 with the correction prompt" -ForegroundColor White
Write-Host "  - PSEUDOCODE_FAIL   -> open new Opus session; spec needs revision" -ForegroundColor White
Write-Host ""
