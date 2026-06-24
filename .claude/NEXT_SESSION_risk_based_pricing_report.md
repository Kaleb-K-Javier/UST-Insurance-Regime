# SESSION KICKOFF — Risk-Based Pricing & Leak-Risk Report
# Drafted: 2026-05-20 (end of the reduced-form report session)
# Intended use: paste the body below as the FIRST message in a new
#               Claude Code session (Opus). Trim the meta-header before pasting.

---

Load CLAUDE.md, .claude/HANDOFF.md, and the project memory MEMORY.md.

## Session goal

Build **`Reports/Paper/Risk_Based_Pricing_Report.qmd`** — a Quarto-to-PDF
report for advisor meetings, structured like
`Reports/Paper/Dynamic_Model_Fit_Report.qmd` and
`Reports/Paper/Reduced_Form_Results_Report.qmd` (already in the repo).

The report consolidates two related but distinct strands of work:
  1. **Leak-risk analysis** — LUST DiD (already produced by `02a`), plus
     any leak-risk prediction / hazard model results.
  2. **Risk-based pricing** — Texas's transition from financial-responsibility
     mandate (FF) to risk-based premium regime (RB), the premium analysis,
     market structure (HHI, top-5 dominance), premium-vs-hazard fit,
     premium-vs-expected-loss fit, cost-distribution diagnostics.

Format: NOT paper prose. Advisor-meeting register. Each result gets its
own section: header → equation in LaTeX → spec/controls bullet block →
table → figure → 1-2 sentence interpretation.

## Workflow (inherited from the prior session — DO NOT reinvent)

This project uses a 3-session architect→coder→reviewer loop (per CLAUDE.md).
You are the **architect** (Opus) in this new session. Coders are Sonnet 4.6
via Anthropic Pro account routing.

  - Tickets live in `.claude/TICKETS/NNN_<topic>.md` (continue numbering
    from 009). For naming and section structure, copy
    `.claude/TICKETS/005_02b_stepped_did.md` — it is the most complete
    recent example.
  - Each ticket spec MUST enumerate every deliverable's contents
    (rows, columns, types) — see memory `feedback_spec_enumerate_deliverables.md`.
  - Coder is invoked with `.\.claude\run_coder_pro_api.ps1 -TicketID NNN`.
  - Approval gates from CLAUDE.md: get explicit user approval before
    handing pseudocode to R1 OR running any estimation script OR changing
    any tolerance.

## Methodology defaults (inherit from the prior session)

These are settled across the project — do NOT re-litigate:

  - **Sample**: active-at-treatment, unweighted. Helper
    `build_active_at_treatment_sample()` already exists in
    `Code/Helpers/reduced_form_utils.R`. Use it.
  - **Cluster**: state level (G = 18).
  - **Bootstrap**: wild cluster bootstrap, score variant (Kline & Santos
    2012). B = 9,999, Rademacher weights, seed 20260519. Wrappers
    `run_wcb_ols()` and `run_wcb_cox()` already exist in
    `Code/Helpers/reduced_form_utils.R`. Reuse, do not rewrite.
  - **Report scaffolding**: copy `Reports/Paper/Reduced_Form_Results_Report.qmd`
    as the template — preamble, kable styling, file-existence guards on
    every artifact-loading chunk, section-per-page layout.
  - **3-row coefficient cells**: estimate / `(model SE)` / `[boot SE]`.
    Hand-written LaTeX (etable and gt do not natively support this).
  - **Citations for the bootstrap**: see `Docs/bootstrap_inference_references.txt`.

## Environment (verified 2026-05-20)

  - **Local dev machine** (this one): Windows, Claude runs here, edits
    happen here. Z:\ is a READ-ONLY mount of the analysis server's
    filesystem.
  - **Analysis server**: UCBARE2, Windows, R 4.4.3, 16 cores, user
    kalebkja. Repo at `C:\Users\kalebkja\ust_ins_move_to_github\`. Data
    INSIDE the repo at `Data/Analysis/`. NOT visible as Z: from the
    server (the server IS Z).
  - **Data path pattern** in any new script (works in both environments
    via env-var fallback):
      ```r
      ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR",
                                  here::here("Data", "Analysis"))
      PANEL_FILE   <- Sys.getenv("UST_PANEL_FILE",
                                  "matched_tanks_birth_cem.csv")
      ```
    Local override for canonical 4.6 GB file:
      `Sys.setenv(UST_ANALYSIS_DIR = "Z:/ust_ins_move_to_github/Data/Analysis")`
  - **Push/pull**: edit local → git push → server git pull → run on
    server → outputs visible via Z: locally OR via separate git pull of
    the outputs back. `.claude/*` files are local-only by convention
    (see memory `feedback_claude_files_local_only.md`).

## Inputs to read first (in order)

  1. `Reports/Paper/Reduced_Form_Results_Report.qmd` — the model template
     for sectioning, preamble, file-existence guards, table macros.
  2. `Reports/Paper/Dynamic_Model_Fit_Report.qmd` — the older model
     template (structural side).
  3. `Reports/Slides/04_Risk_Based_Pricing_and_USTs_REFACTORED.qmd` —
     enumerates which `.tex` and `.png/.pdf` artifacts the prior slide
     deck pulled in for risk-based pricing. Many already exist on disk;
     the report can pull them directly.
  4. `Code/Analysis/02a_DiD_facility_behavior.R` — source of the LUST
     DiD outputs (`T_LUST_A_Total.tex`, `T_LUST_B_Decomp.tex`) and the
     descriptive portfolio results.
  5. `Code/Helpers/reduced_form_utils.R` — helpers you will likely
     extend (do NOT duplicate functionality across new scripts).
  6. `.claude/TICKETS/005_02b_stepped_did.md` — the most complete recent
     ticket spec; copy the format.
  7. `Docs/bootstrap_inference_references.txt` — bootstrap citations.

## First task in the new session

DO NOT write any tickets or .qmd code yet. First:

  (a) Inventory what artifacts already exist for the risk-based pricing
      side: walk `Output/Tables/` and `Output/Figures/` for files
      starting with `04e_*`, `Figure_Premium_*`, `Figure_CV_*`,
      `Figure_TX_FR_*`, `Figure_cost_*`, etc. Most are already on disk
      from prior slide-deck work.
  (b) Identify what's MISSING — likely the analytic foundation for the
      premium-vs-hazard / premium-vs-EL plots needs a clean script;
      the leak-risk prediction model may need re-estimation; HHI
      computation may already be in `02a` or somewhere in `Code/`.
  (c) Propose a section outline (8-12 sections, one per result), in
      the same shape as the reduced-form report's TOC. Get user
      approval BEFORE drafting any ticket.

## Open questions to surface for the user EARLY (before ticket-drafting)

  - Does the risk-based pricing analysis need new estimation (architect
    → R1 → reviewer tickets), or only re-rendering of existing
    artifacts (architect can use the "presentation polish" exception
    per CLAUDE.md)?
  - Same sample (active-at-treatment, unweighted) or different?
    Premium analyses might pool differently (e.g., all incumbents).
  - Should leak-risk and risk-based pricing be two separate reports,
    or one combined report as currently scoped?
  - Are there pending R1 tickets from the prior session (006-008) that
    must land first because they share helpers? Check
    `.claude/HANDOFF.md` for the latest state.

## State at session-end of the prior session

  - Tickets 005-008 drafted; T005 attempt 2 ran on server overnight (or
    is still in flight — check the latest log in `logs/02c_*.log`).
  - `Code/Helpers/reduced_form_utils.R` is the shared library; extending
    it (don't fork it) is the pattern for new scripts.
  - `Reports/Paper/Reduced_Form_Results_Report.qmd` is the in-progress
    sibling report; review its structure before mirroring it.

When you have read items 1-6 above and produced a proposed section
outline, present it to the user and ask for sign-off before doing
anything else.
