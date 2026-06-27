# TICKET 034 — Reduced-form setting/data number gaps (§2 / §3) for the editor
# Created: 2026-06-26
# Status: DONE (answered directly from local data 2026-06-26 — no coder run needed)
# Answers: Reports/Paper/_RF_SettingData_Answers.md
# Attempt: 0
# Type: DATA PULL + CODE TRACE (descriptive numbers; no estimation change). DATA CODER seat
#       (run_coder_pro_api.ps1). Runs LOCALLY — exact_base.csv and facility_panel.csv are in
#       the local repo (Data/Analysis/), local R 4.5.2 at "C:/Program Files/R/R-4.5.2".
# Purpose: the editor needs verified numbers to close gaps in the RF setting/data section
#       (§2/§3). Each answer below feeds a specific paragraph. ANSWER ONLY — do not edit the
#       paper. Return each item in the exact format requested; print to console + a small CSV.

═══════════════════════════════════════════════════
CANONICAL SOURCES (read from these only)
═══════════════════════════════════════════════════
- Tank-level:    Data/Analysis/exact_base.csv  (panel_id, texas_treated, mm_wall,
                 install_yr_int, t_enter, t_exit, age_enter)
- Facility-year: Data/Analysis/facility_panel.csv  (panel_year, texas_treated, active_tanks,
                 total_capacity, n_tanks_at_reform, fac_is_incumbent, n_leaks, n_leak_incidents)
- Builders/specs: Code/Analysis/02b_Tank_level_Panel_Build.R · headline tank DiD
                 Code/Analysis/02c_Stepped_DiD.R (Cox) · summary table
                 Code/Analysis/T_Baseline_Characteristics_Slide.R
- Reform day: REFORM <- as.Date("1998-12-22"). Pre-reform window: define EXPLICITLY (propose
  panel_year in 1994:1997 — i.e. the four full pre-reform years; state whatever you use) and
  report the window with every rate.

ARCHITECT NOTES (leads, not answers):
- Q6 is ambiguous: TWO 02c files exist. 02c_Stepped_DiD.R is Cox (headline per editor).
  02c_Closure_Conditional_Enriched.R has CONTROL_STATES = c("AR","CO","ID","KS","KY","LA",
  "MA","MD","ME",...) — includes CO/KY/MD, NOT the paper's 17. TRACE the state vector the
  HEADLINE closure DiD actually uses (follow what 02c_Stepped_DiD.R loads / filters, incl. any
  sourced sample object), and report it verbatim. Do not assume.
- Data is LOCAL: run with "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe --vanilla" from repo
  root, reading the two CSVs directly. No renv, no server.

═══════════════════════════════════════════════════
DELIVERABLES — answer each, exact return format
═══════════════════════════════════════════════════
Q1 — Reconcile facility / observation counts. [§3 ¶1, ¶8, §4.3]
  Two samples are conflated in the draft (matched-DiD 117,250 vs alive-at-reform 252,670).
  Return:
   (a) HEADLINE DiD estimation sample (the one 02c_Stepped_DiD.R fits): N facilities, N tanks,
       N tank-years. (b) ALIVE-AT-REFORM descriptive sample (T_Baseline_Characteristics_Slide.R:
       FYa = sel(fp)): N facilities, N tanks, N facility-years. (c) one-line plain definition of
       each. (d) Is 117,250 still current for any of these — yes/no + which.

Q2 — Pre-reform tank-closure rate, TX vs control. [§3 ¶8; confirms §4.4 base]
  Annual tank-closure rate over the pre-reform window, TX and control SEPARATELY (not pooled).
  Closure outcome = the tank-panel closure indicator used by the headline DiD (state which
  column/definition). Return: TX rate, control rate, the window, N tank-years each. Note whether
  TX_pre ≈ control_pre ≈ ~2.0% (the cited control baseline).

Q3 — Pre-reform leak-discovery rate, TX vs control. [§3 ¶8]
  Annual leak-discovery rate = n_leak_incidents per facility-year (facility_panel.csv),
  panel_year < 1998, TX vs control SEPARATELY. Run it TWO ways: (i) incumbents only
  (fac_is_incumbent==1) and (ii) all facilities. Return: TX rate, control rate, window, sample
  (incumbent/all), N each. Note: do NOT use the full-sample rel_rate (1.4/0.5/1.6) — it is
  post-contaminated.

Q4 — Pre-reform parallel tracking. [decides where the "parallel" claim lives]
  Return BOTH if available: (a) the pre-period group means from Q2/Q3 (so the editor can write
  "both ≈ X%"), and (b) the event-study pre-trend coefficients + joint p-value for the headline
  closure ES. Identify which artifact holds the ES pre-trend coefs (likely
  Output/Figures/Fig_ES_Full.pdf + its underlying coef table — name the table/CSV).

Q5 — Retrofit / wall-change share. [§2 ¶1 + intro; load-bearing]
  Share of active facilities that EVER changed a tank's wall (retrofit) prior to terminal
  closure. Return the share with explicit numerator/denominator, computed BOTH TX-only and
  all-states, and the window. Confirm whether the intro's "0.003% of active Texas facilities"
  and §2's "0.003% of active facilities" are the SAME number or two different ones. Source:
  wall histories in exact_base.csv / 02b; possibly T013_Transition_and_State_Leakage_Diagnostic.R.

Q6 — Control-state vector. [§3 ¶2]  ***RESOLVED FROM CODE 2026-06-26 — no run needed.***
  The whole RF pipeline (headline tank + facility) uses ONE vector, defined identically in
  02a_DiD_OLS.R:111, 02b_tank_closure_analysis.R:71, AND 02b_Tank_level_Panel_Build.R:123:
    CONTROL_STATES = AR, CO, ID, KS, KY, LA, MA, MD, ME, MN, MO, NC, OH, OK, SD, TN, VA  (17)
    STUDY_STATES   = TX + those 17 = 18 states  (matches the RF's G=18 clusters)
  CO is IN; so are KY, MD, MO. The paper's stated list is WRONG: it lists AL/IL/MT/NM/PA (the
  code DROPS these — see 02a_DiD_OLS.R:116 comment: IL/NM/AL/MT missing wall data; PA closure-
  only/commented) and OMITS CO/KY/MD/MO. §3 ¶2 must be rewritten to the 17 above.
  WORKER: skip this; just CONFIRM the matched panel's unique(state) equals these 18 (1-line check)
  as a belt-and-suspenders, and report any state present in the data but not in this list.

Q7 — Dollar facts in §2. [§2 ¶1–¶2]
  For each, classify DATA PULL vs LITERATURE CITE and give the source string:
   - new double-walled replacement ≈ $55,000/tank
   - install ≈ $23k single-wall / $39k double-wall (canvas attributes to GAO 1987)
  grep the repo for these figures (55000 / 23000 / 39000 and "GAO"); if a registry/claims field
  backs $55k, name the column + file; else mark it a cite and give the reference.

═══════════════════════════════════════════════════
BUILD ITEMS — only if the researcher asks (default: SKIP, just answer Q1–Q7)
═══════════════════════════════════════════════════
B1 — Pre-reform "Panel B" rows in the summary table. Build ONLY if a rate is wanted in the
   baseline table (else Q2–Q4 feed §4.4 prose instead). T_Baseline_Characteristics_Slide.R loads
   panel_year + n_leak_incidents already; add a block AFTER line 92 with pre-1998 closure + leak
   rate, TX vs control; closure needs the tank-panel closure indicator joined in. This REPLACES
   the misleading full-sample rel_rate row (line 92).
B2 — §4.4 falsification table T_Falsification_RiskConcentration (did × age × wall, run OUTSIDE
   the cell×year FE). Flagged "not yet on the server." If wanted: confirm/locate the builder +
   output path, then run. (Defer unless the editor needs §4.4 ¶3 now.)
B3 — Emit both Ns with definitions (Q1) to a tiny CSV so §3 ¶1/¶8 and §4.3 can be made
   internally consistent.

═══════════════════════════════════════════════════
OUTPUT
═══════════════════════════════════════════════════
Print every answer to console AND write Output/Tables/T_RF_SettingData_Answers.csv with one row
per item (item, value, group [TX/control/all], window, n, source_note). No paper edits. No
estimation/tolerance changes. Hard errors surface (no tryCatch->NULL).

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
