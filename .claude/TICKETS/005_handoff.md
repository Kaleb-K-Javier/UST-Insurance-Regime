# Handoff: Ticket 005 → R1 (Sonnet 4.6 Pro coder session)

## How to launch

From the repo root in PowerShell:

```powershell
.\.claude\run_coder_pro_api.ps1 -TicketID 005
```

The runner now globs `${TicketID}_*.md` for the spec, so it will pick up
`005_02b_stepped_did.md` automatically. On retry: `-Attempt 2`.

## What Sonnet will receive automatically (from the runner)

- System prompt: "You are an R/Rcpp implementer, not an economist. Ask R
  implementation questions only. No tryCatch returning NULL."
- User prompt: "Read .claude/TICKETS/005_02b_stepped_did.md. This is
  attempt 1. Start by confirming you have read the spec, then ask me any
  R-implementation questions before writing any code."

## What Sonnet will likely ask (be ready with these answers)

These are R-implementation questions the spec deliberately leaves under-
specified so the coder can adapt to whatever the actual server R
environment looks like.

**1. Path to `matched_tanks_birth_cem`.**
   The spec says "same path as 02b S2". Open `Code/Analysis/02b_tank_closure_analysis.R`,
   find the `S2 Load Datasets` block (around line 104), and look for the
   `readRDS(...)` or `fread(...)` that loads `matched_tanks_birth_cem`.
   Provide that exact path to Sonnet. Likely candidates: somewhere under
   `Data/Analysis/` or `Output/Cleaned/`.

**2. Existing `run_boot_cox` return shape.**
   The spec asks Sonnet to re-export this function from 02b into the new
   helper, with a contract that includes `SE_boot, CI_lo, CI_hi, p_boot,
   B, n_clusters`. The existing function (02b line ~857) may return a
   different shape. If so, Sonnet should write a thin adapter inside
   `reduced_form_utils.R` that calls the original and remaps the output.

**3. `fwildclusterboot` API minor variations.**
   The argument name for the restriction matrix in `boottest()` varies
   slightly across package versions. If `R = c(...)` does not work in
   the server's installed version, try `param = c(...)` with a named
   numeric vector; the spec lists both possibilities.

**4. Output paths exist?**
   The spec uses `OUTPUT_TABLES` and `OUTPUT_FIGURES` constants from the
   helper file. Make sure `Output/Tables/`, `Output/Figures/`, and
   `Output/Estimation_Results/` already exist. If any don't,
   `dir.create(..., recursive = TRUE, showWarnings = FALSE)` in the
   helper or main script — Sonnet can pick where.

**5. Server core count.**
   Set `BOOT_NTHREADS <- 32L` (or whatever the server has). The spec
   says `parallel::detectCores() - 1L` as a default; if the server has
   reserved cores for other users, hard-code a smaller number.

## Things Sonnet should NOT ask (these are economic decisions, settled)

If Sonnet asks any of these, type STOP and close the session — that
means the spec failed and I need to revise it:

- Should the sample include CEM weights?
- Should we drop the Pre-89 / Post-88 sample splits?
- Should col (5) of OLS use cell + year or cell × year?
- Is Option B (strata + factor) the right Cox mirror for cols 5–7?
- Should the 3-row format show p-value instead of bootstrap SE?
- Is `B = 9999` enough?

All these are already settled in the spec or in prior conversation.

## What Sonnet should produce

7 files (per Step 1 of Spec Deliverables):

  1. `Code/Helpers/reduced_form_utils.R` — NEW shared helper
  2. `Code/Analysis/02c_Stepped_DiD.R` — NEW main script
  3. `Output/Tables/T_Stepped_DiD_OLS.tex` — 7-col stepped OLS
  4. `Output/Tables/T_Stepped_DiD_Cox.tex` — 7-col stepped Cox
  5. `Output/Tables/T_Stepped_DiD_Bootstrap_Diagnostics.csv` — 14-row long format
  6. `Output/Estimation_Results/Stepped_DiD_Fits_active_at_treatment.rds` — fits for the .qmd to reload
  7. `logs/02c_Stepped_DiD_<TIMESTAMP>.log` — auto-generated

## After Sonnet finishes

The transcript will be saved at `.claude/TICKETS/005_transcript.txt`.

Run the reviewer agent next:

> "Review ticket 005 attempt 1"

The reviewer reads the spec + transcript + outputs, runs the
acceptance-criteria checks, and returns one of:
  - **PASS** — outputs ready; move to Ticket 006
  - **TRANSLATION_FAIL** — Sonnet misread the pseudocode; retry with correction
  - **PSEUDOCODE_FAIL** — spec is ambiguous; escalate back to me to fix

Retry cap is 2 attempts per CLAUDE.md. If attempt 2 also fails, the
escalation back to Opus is automatic.

## Operator pre-flight checklist

Run these BEFORE launching the R1 session so you can answer the likely
questions instantly:

```r
# 1. Confirm matched_tanks_birth_cem path
grep -n "matched_tanks_birth_cem" Code/Analysis/02b_tank_closure_analysis.R | head -20

# 2. Confirm run_boot_cox return shape
# Open 02b around line 857, look at what it returns

# 3. Confirm fwildclusterboot is installed on server
# In R: requireNamespace("fwildclusterboot", quietly = FALSE)

# 4. Confirm output directories exist
for d in Output/Tables Output/Figures Output/Estimation_Results logs; do
  [ -d "$d" ] && echo "OK: $d" || echo "MISSING: $d"
done

# 5. Confirm server core count
# In R: parallel::detectCores()
```

## Notes for handoffs 006, 007, 008

When Ticket 005 passes review:
- 006 (event studies) has no dependencies beyond the helper file from 005
- 007 (vintage HTE) depends on 005's helper + extends `run_wcb_cox`
- 008 (HonestDiD) depends on 006's `.rds` output

Launch order: 005 → 006 → (007 ∥ 008 in parallel, both need 006).
