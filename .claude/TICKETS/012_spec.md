# TICKET 012 — Pipeline conservation & fidelity diagnostics (D3–D6)
# Created: 2026-06-01
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Assignee: R1 (read-only analytics — NO estimation, NO model changes, NO panel edits)

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
T012 + T013 (Code/Analysis/) found that the 32-cell single-tank state coding
leaks where multi-tank portfolio dynamics occur (DW exits routed to SW cells;
16.2% of closures mis-celled; 30% of replaces don't reset age). Those covered
the STATE-TIMING tripwires (D1) and the closure cell-leakage (D2), plus
exit-absorbing (D7-exit, passes) and regime-timing (D8, passes).

This ticket completes the invariant-audit suite with the four remaining
mechanical checks that guard the rest of the pipeline 02b -> 04b -> 04 structural:
  D3 action conservation (02b flags -> 04b actions)
  D4 stock-flow count identity (the T007 churn-bug class)
  D5 cell-primitive vs cell-empirical (P_vec/h_vec fidelity)
  D6 selection-on-action (does the premium filter drop exits?)

READ-ONLY. One new script. No edits to any existing script, panel, or fit.

═══════════════════════════════════════════════════
ENVIRONMENT
═══════════════════════════════════════════════════
Rscript: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
Working dir: repo root (here::here())
Packages: data.table, ggplot2, here, scales
Use the standard CLAUDE.md logging block, cat("=== SECTION ===") headers,
hard error propagation, and OUTPUT RULES (never print full matrices/panels;
summaries + saved CSVs only).

New file: Code/Analysis/T014_Pipeline_Conservation_Diagnostics.R

Inputs (verify file.exists first; stop with a clear message if missing):
  Data/Analysis/dcm_obs_panel_observed.csv        (04b estimation sample)
  Data/Analysis/facility_panel.csv                (02b facility-year; fread SELECT only)
  Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
     (for fit$cache$P_vec, h_vec, hazard_loss, state_lut)

facility_panel SELECT columns (only these):
  panel_id, panel_year, texas_treated, n_tanks_active, n_tanks_eoy,
  n_installs, n_closures, any_closure, facility_complete_closure,
  replacement_closure_year, all_sw, all_dw, mixed_wall, premium

SCALE: 1 model unit = $10,000. fit$cache$P_vec is in model units; multiply by
10000 for $/tank-year. Read SCALE_FACTOR <- 10000L.

═══════════════════════════════════════════════════
D3 — ACTION CONSERVATION (02b flags -> 04b actions)
═══════════════════════════════════════════════════
Merge obs (04b) with facility_panel (02b flags) on (panel_id, panel_year),
inner join on the estimation sample (all.x = FALSE).

Classify each row's 02b event type:
  no_closure                : any_closure == 0
  partial_closure           : any_closure == 1 & facility_complete_closure == 0
  complete_exit_no_install  : facility_complete_closure == 1 & replacement_closure_year == 0
  complete_exit_with_install: facility_complete_closure == 1 & replacement_closure_year == 1

Classify each row's 04b action from y_it / I_replace:
  Maintain : y_it == 0
  Exit     : y_it == 1 & (is.na(I_replace) | I_replace == 0)
  Replace  : y_it == 1 & I_replace == 1

DELIVERABLE  Output/Tables/T014_D3_ActionConservation.csv
  Rows: one per (event_02b × action_04b) combination that occurs, PLUS a final
        row event_02b="__RESIDUAL_unmapped__" (closures with any_closure==1 that
        fall into none of the four event types).
  Cols (exact, with types):
        event_02b        chr   (one of the 4 types or the residual label)
        action_04b       chr   (Maintain | Exit | Replace)
        n                int
        pct_of_rows      num   (100 * n / total merged rows, 2 dp)
        pct_of_closures  num   (100 * n / total any_closure==1 rows, 2 dp; 0 for no_closure)

CONSERVATION ASSERTION: every any_closure==1 row maps to exactly one event_02b
type; residual_unmapped count MUST be 0. Print PASS/FAIL and the residual count.
Print the full (event_02b × action_04b) matrix to the log.

═══════════════════════════════════════════════════
D4 — STOCK-FLOW COUNT IDENTITY
═══════════════════════════════════════════════════
On facility_panel. Build gap-safe beginning-of-year stock (prior-year EOY,
shift KEY forward 1 yr, do NOT row-shift):
  prior <- fp[, .(panel_id, panel_year = panel_year + 1L, boy_stock = n_tanks_eoy)]
  fp <- merge(fp, prior, by=c("panel_id","panel_year"), all.x=TRUE)
  # rows with no prior-year row (left-censored first appearance) -> identity not
  # testable; EXCLUDE them and log the count.
Identity (only on rows with non-NA boy_stock):
  resid = n_tanks_eoy - (boy_stock + n_installs - n_closures)
  violation = abs(resid) > 0
size_bin = fcase(n_tanks_active==1,"1", ==2,"2", ==3,"3", >=4,"4+", default="unknown")

DELIVERABLE  Output/Tables/T014_D4_StockFlowIdentity.csv
  Rows: 5 — one per size_bin {1,2,3,4+} plus a final "all" row.
  Cols (exact, types):
        size_bin          chr
        n_obs             int   (rows with testable identity)
        n_violations      int
        pct_violation     num   (100 * n_violations / n_obs, 2 dp)
        mean_abs_resid    num   (mean |resid| over violating rows, 3 dp; 0 if none)
Print overall pct_violation and whether it is concentrated at large facilities
(compare 4+ vs 1). Cross-reference: memory says T007 found ~2.6% fail; state
whether this reproduces.

═══════════════════════════════════════════════════
D5 — CELL PRIMITIVE vs CELL-EMPIRICAL
═══════════════════════════════════════════════════
fit$cache$P_vec, $h_vec, $hazard_loss are length-32 (model state ordering by
s_idx). state_lut gives (s_idx, A_bin, w_state, rho_state).
Empirical premium per cell from obs (estimation sample), premium in model units:
  emp <- obs[!is.na(premium), .(P_emp = mean(premium), n_obs = .N), by = s_idx]
Compare primitive vs empirical premium IN DOLLARS (× SCALE_FACTOR).
  (h_vec / hazard_loss have NO direct per-row empirical analog in obs; do NOT
   fabricate one. Report the primitive h_vec and hazard_loss columns for
   reference only, no empirical comparison, and state this in the log.)

DELIVERABLE  Output/Tables/T014_D5_PrimitiveVsEmpirical.csv
  Rows: 32 (one per s_idx).
  Cols (exact, types):
        s_idx             int
        A_bin             int
        wall              chr   ("SW"/"DW")
        regime            chr   ("FF"/"RB")
        n_obs             int
        P_primitive_USD   num   (P_vec[s] * 10000, 0 dp)
        P_empirical_USD   num   (P_emp     * 10000, 0 dp)
        P_abs_diff_USD    num   (abs diff, 0 dp)
        h_vec             num   (primitive, 5 dp, reference only)
        hazard_loss_USD   num   (hazard_loss[s] * 10000, 0 dp, reference only)

DELIVERABLE  Output/Figures/T014_D5_Primitive_vs_Empirical.png  (10×6, dpi 150)
  45-degree scatter: x = P_empirical_USD, y = P_primitive_USD, color = regime,
  shape = wall; add geom_abline(slope=1). Title "Cell premium: primitive vs
  empirical". Points off the 45-line = a cell-aggregation / merge / scale error.
Print max P_abs_diff_USD and list any cell with abs diff > $25.

═══════════════════════════════════════════════════
D6 — SELECTION-ON-ACTION (premium filter)
═══════════════════════════════════════════════════
QUESTION: does restricting to premium-non-NA rows drop closures (esp. exits)
differentially, putting kappa/K on a selected subsample?

The estimation sample IS the premium-non-NA rows. The comparison set is the
SAME obs file WITHOUT the premium filter (obs has rows with NA premium too —
verify by counting is.na(premium)). If the obs file has no NA-premium rows,
state that and instead compare to facility-panel-derived closure rates by
matching on (panel_id, panel_year); enumerate which path was taken in the log.

Compute action shares two ways, by (w_state, rho_state) cell:
  full   = obs (all rows)
  est    = obs[!is.na(premium)]
  for each: P_close = mean(y_it==1), P_E = mean(y_it==1 & (is.na(I_replace)|I_replace==0)),
            P_R = mean(y_it==1 & !is.na(I_replace) & I_replace==1)

DELIVERABLE  Output/Tables/T014_D6_SelectionOnAction.csv
  Rows: 4 — one per (w_state × rho_state) cell {SW-FF, SW-RB, DW-FF, DW-RB}.
  Cols (exact, types):
        wall            chr
        regime          chr
        n_full          int
        n_est           int
        P_close_full    num (5 dp)
        P_close_est     num (5 dp)
        P_E_full        num (5 dp)
        P_E_est         num (5 dp)
        P_R_full        num (5 dp)
        P_R_est         num (5 dp)
Print the max |P_close_est - P_close_full| and |P_E_est - P_E_full| across cells;
flag if the premium filter changes the exit share by > 0.005 in any cell
(would indicate selection on the exit margin).

═══════════════════════════════════════════════════
SECTION — RESEARCHER SUMMARY (print + cat)
═══════════════════════════════════════════════════
  D3 conservation : PASS/FAIL, residual_unmapped count
  D4 stock-flow   : overall pct_violation, 4+ vs 1 ratio
  D5 primitive    : max premium abs diff ($), n cells > $25
  D6 selection    : max exit-share shift under premium filter; selection yes/no

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
  - Script runs end-to-end under R-4.5.2 with hard error propagation.
  - All 4 CSVs written with EXACTLY the columns/types enumerated above.
  - T014_D5 figure written.
  - D3 conservation residual printed (expected 0; if nonzero, do NOT silently
    pass — report the offending rows).
  - No estimation, no model changes, no edits to existing scripts/panels/fits.
