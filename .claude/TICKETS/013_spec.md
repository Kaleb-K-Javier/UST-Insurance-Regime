# TICKET 013 — BOY (decision-time) state stamping in 04b + re-estimate
# Created: 2026-06-01
# Status: CLOSED — PASS (researcher accepted 2026-06-02). BOY fit shipped as the
#         corrected baseline. The C3 "maintain off-support 0.685%" is a DOCUMENTED
#         KNOWN LIMITATION of the single-tank abstraction, NOT a failure (see
#         Closure note below). Welfare CF re-run (017) warranted by Step-4 movement.
# Attempt: 1 (reviewed 2026-06-02)
# Assignee: R1
# Gate: estimation-logic change (feeds Output/Estimation_Results) — NOT polish.

═══════════════════════════════════════════════════
MOTIVATION (what T012/T013 proved)
═══════════════════════════════════════════════════
04b stamps each decision-row's structural state from SAME-YEAR (post-action)
facility aggregates:
  - A_bin   from avg_tank_age_dec  (line ~114): a December/EOY portfolio average.
  - w_state from wall_type         (line ~121): the EOY worst-tank class.
Because these are read AFTER the year's closures/installs, the decision-row state
reflects the post-action portfolio. Consequences (quantified):
  - 16.2% of closures are filed under the WRONG decision-time wall cell (T013 D2).
  - Every DW full-exit is reclassified SW (0 tanks at EOY) -> DW cells lose their
    exits -> artificial P(R|cl)=1; SW exit margin 11% contaminated by DW exits.
  - DW-replace cell is only 16.6% genuine all-DW; 83.4% are mis-celled SW->DW
    retrofits (so "K_DW" is barely about DW facilities).
  - Age runs BACKWARD under Maintain (avg age drops when an old tank closes):
    0.67% of maintain transitions are model-forbidden (T013 D1).

FIX: stamp A_bin and w_state from the BEGINNING-OF-YEAR portfolio (prior-year
December snapshot = decision-time state), gap-safe. This is the cell a decision
is made IN; it is the correct conditioning state for a forward-looking choice.

SCOPE NOTE (do not over-reach): this ticket fixes WHICH CELL a decision is in.
It does NOT fix F_replace's single-tank reset assumption (T013 D1 replace
off-support = 30%, multi-tank avg-age does not reset on a one-tank swap). That is
the portfolio-state question, deferred to Ticket 014. Do not touch F_replace or
reset_state_index logic here beyond the BOY consistency in 1.4 below.

═══════════════════════════════════════════════════
ENVIRONMENT
═══════════════════════════════════════════════════
Edit: Code/Dynamic_Model/04b_Replacement_Panel_Prep.R  (state-stamping block only)
Re-run: 04b, then the 6p+FE estimation (04o_6paramFE_Profile_Clean.R-equivalent)
Rscript: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
Tolerances: UNCHANGED (tol_theta, tol_P, npl_iter, sigma2, beta, ccp_damping — all
as in current cfg). This ticket changes panel INPUTS only, not estimator settings.
ARCHIVE the superseded panel/primitives/fit (do not delete) per repo convention:
  move existing dcm_obs_panel_observed.csv, DCM_Primitives_Replacement_observed.rds,
  Model_Replacement_6paramFE_profile_clean_observed.rds to
  Data/Analysis/_pre_BOY/ and Output/Estimation_Results/_pre_BOY/ respectively.

═══════════════════════════════════════════════════
STEP 1 — BOY STATE STAMPING IN 04b
═══════════════════════════════════════════════════
The fac_panel is keyed (panel_id, panel_year), one row per active facility-year.
avg_tank_age_dec and wall_type are December (EOY) snapshots.

1.1  Build gap-safe prior-year (BOY) snapshots BEFORE the pre-1999 filter
     (the snapshots must be built on the FULL panel so year t can see year t-1):
       setorder(fac_panel, panel_id, panel_year)
       boy <- fac_panel[, .(panel_id, panel_year = panel_year + 1L,
                            avg_age_boy  = avg_tank_age_dec,
                            wall_type_boy = wall_type)]
       fac_panel <- merge(fac_panel, boy, by = c("panel_id","panel_year"),
                          all.x = TRUE)
     Do NOT row-shift (panel has gaps at zero-tank years).

1.2  Fallback for rows with no prior-year row (left-censored first appearance,
     or a post-gap reappearance): use the SAME-YEAR value and count them.
       n_boy_fallback <- fac_panel[is.na(avg_age_boy), .N]
       fac_panel[is.na(avg_age_boy),   avg_age_boy   := avg_tank_age_dec]
       fac_panel[is.na(wall_type_boy), wall_type_boy := wall_type]
     Log n_boy_fallback (count + pct of fac_panel rows).

1.3  Stamp A_bin and w_state from the BOY snapshots (replace current lines
     ~114 and ~121). Keep rho_state EXACTLY as-is (year-based; T013 confirmed 0
     regime leakage):
       fac_panel[, A_bin := as.integer(cut(avg_age_boy, AGE_BREAKS,
                            labels = 1:N_AGE, right = FALSE, include.lowest = TRUE))]
       fac_panel[is.na(A_bin), A_bin := N_AGE]
       fac_panel[, w_state := fcase(wall_type_boy == "Double-Walled", 2L, default = 1L)]
       fac_panel[, s_idx := state_idx(A_bin, w_state, rho_state)]

1.4  Cell-level primitives (h_cell, L_cell, premium cell medians) and any
     facility-year-to-cell aggregation that currently uses the EOY (A_bin,
     w_state) MUST use the new BOY (A_bin, w_state) so primitives and the obs
     panel share one stamping. Audit every `by = .(A_bin, w_state)` and every
     s_idx use downstream of 1.3 — they inherit BOY automatically once 1.3 runs
     before them. VERIFY ordering: 1.3 must execute before Steps 2/3's cell
     aggregations. (reset_state_index in the replacer block keeps its current
     next-year-wall logic — out of scope; flag in the log that it is unchanged.)

1.5  ADD a BOY size column to the output panel for downstream use (not yet used
     in estimation — Ticket 015 will consume it):
       boy_stock from prior-year n_tanks_eoy, gap-safe (same join pattern as 1.1;
       fallback to n_tanks_active). size_bin := fcase(==1"1",==2"2",==3"3",>=4"4+").
     Carry size_bin into the saved obs panel columns.

═══════════════════════════════════════════════════
STEP 2 — REGENERATE PANEL + PRIMITIVES, RE-ESTIMATE
═══════════════════════════════════════════════════
2.1  Run 04b end-to-end -> regenerates dcm_obs_panel_observed.csv (+ size_bin col)
     and DCM_Primitives_Replacement_observed.rds on BOY stamping.
2.2  Re-estimate the 6p+FE profile fit (rerun the 04o Phase-3 estimation path)
     -> Model_Replacement_6paramFE_profile_clean_observed.rds (BOY).
     Warm-start from the existing (pre-BOY) theta_hat. Converged==TRUE required.

═══════════════════════════════════════════════════
STEP 3 — ACCEPTANCE: RE-RUN THE INVARIANT TRIPWIRES
═══════════════════════════════════════════════════
Re-run T012 and T013 on the BOY panel/fit. ACCEPTANCE CRITERIA (hard):
  [ ] T013 D2 closure mis-cell rate: drops from 16.2% to < 1.0%.
  [ ] T013 D2 DW-cell replace "genuine all-DW" share: rises from 16.6% toward
      the true DW share (expect >> 50%; the SW->DW retrofits move OUT of DW cells).
  [ ] T013 D1 MAINTAIN off-support: DW->SW count drops to ~0 (a few residual
      cross-year composition changes allowed); maintain off-support mass < 0.2%.
  [ ] T013 D1 EXIT re-entry stays 0; regime change stays 0.
  [ ] T013 D1 REPLACE off-support: report the new value (NOT expected to hit 0 —
      F_replace single-tank reset is Ticket 014; document the residual).
  [ ] All P_hat rows sum to 1; estimator converged.

═══════════════════════════════════════════════════
STEP 4 — DELIVERABLE: BEFORE/AFTER PARAMETER MOVEMENT
═══════════════════════════════════════════════════
Output/Tables/T013fix_Theta_PreBOY_vs_BOY.csv — quantify how much the bug moved
the structural estimates.
  Rows: 8 — kappa_SW, kappa_DW, K_SW, K_DW, gamma_price, gamma_risk,
            log_likelihood, N_obs.
  Cols (exact, types):
    parameter     chr
    pre_BOY       num   (kappa in USD = *10000; K = exp(K_log); gammas raw)
    BOY           num
    delta         num
    pct_change    num   (NA for LL/N_obs)
  Print the table and a one-line verdict: "BOY stamping moved K_DW by X%,
  kappa_SW by Y%; welfare CF re-run is/ is not warranted."

NOTE: the welfare CF (04o_CF_TX_FlatFee) re-run is a FOLLOW-ON, not this ticket —
do it once the BOY fit passes Step 3 and Step 4 shows material movement.

═══════════════════════════════════════════════════
ARCHITECT CLARIFICATIONS (attempt 1 Q&A — binding)
═══════════════════════════════════════════════════
Q2  age_trans / F_maintain INHERITS BOY (leave §6 code untouched; it consumes the
    now-BOY A_bin). The transition kernel must describe the evolution of the state
    the model conditions on; that state is now BOY age. Residual backward-age moves
    (which F_maintain's stay/+1 structure cannot represent) are exactly what the
    Step-3 maintain-off-support gate measures — report, do not engineer away.
Q3  reset_state_index: leave byte-for-byte; shift(BOY w_state) is the correct
    next-period decision-time reset target. Do NOT keep a separate EOY w_state.
Q4  size source = prior-year n_tanks_eoy; ADD "n_tanks_eoy" to KEEP_COLS (matches
    T012 BOY-stock; consumed by Ticket 015). Fallback to prior-year active_tanks
    only if n_tanks_eoy absent from source; LOG which. size_bin CHARACTER with
    explicit bins: fcase(==1"1",==2"2",==3"3",>=4L"4+",default NA_character_).
Q5  output panel adds size_bin AND boy_stock (raw count). Skip avg_age_boy /
    wall_type_boy (redundant with the now-BOY A_bin/w_state).
Q7  re-run 04o AS-IS (warm start = T005 profile fit; NPL converges to the same
    optimum, so do not edit 04o Section 6). Archive the existing
    04o_Theta_Comparison_T005_vs_T007.csv/.tex to _pre_BOY/ BEFORE re-running;
    the regenerated copy is mislabeled (now T005-vs-BOY) — ignore it, the Step-4
    table is authoritative. Note in log.
Q8  Step-4 script = Code/Analysis/T013fix_Theta_PreBOY_vs_BOY.R. pre_BOY column
    from archived _pre_BOY/Model_Replacement_6paramFE_profile_clean_observed.rds
    (the pre-BOY CLEAN fit, NOT T005). Reuse 04o make_cmp_row transforms.
Q9  acceptance = run T012 then T013 (D2 reads T012_C4b), report the six checkboxes
    from their outputs; no new diagnostic code.

═══════════════════════════════════════════════════
WHAT NOT TO DO
═══════════════════════════════════════════════════
  - Do NOT change any tolerance/constant/estimator setting.
  - Do NOT touch F_replace, reset_state_index reset logic, or the Bellman/CCP code.
  - Do NOT add size to the state space or as an FE here (Ticket 015, pending the
    FE-vs-state design decision).
  - Do NOT delete superseded artifacts — archive to _pre_BOY/.

## Attempt Log

### Attempt 1 — 2026-06-02
Transcript: 013_transcript.txt (PowerShell host noise only — workflow bug W6;
  coder session was backgrounded. Skipped per reviewer agent guidance.)
Result: PSEUDOCODE_FAIL

Implementation faithfulness (git diff 04b + T013fix script): the BOY stamping
is implemented exactly as specified — 1.1 +1L key-join (not row-shift), 1.2
same-year fallback + count log, 1.3 A_bin/w_state from BOY snapshots, 1.4 s_idx
before downstream cell aggregation + reset_state_index left byte-for-byte, 1.5
size_bin/boy_stock added to obs panel. Pipeline re-ran end-to-end (04b 17:18 →
04o 17:20 → T012/T013 17:22 → T013fix 17:25). _pre_BOY archive is the May-28
T007-clean fit, so the Step-4 comparison isolates BOY stamping (no T007
action-def confound). Step-4 table is correct: 8 rows in spec order, exact
cols/types, kappa×10000, K=exp(K_log), pct_change NA for LL/N_obs, verdict line
present ("welfare CF re-run IS warranted"). The code is NOT the problem.

Criteria (Step 3):
- [✓] C1 D2 closure mis-cell rate <1.0% (from 16.2%): PASS — 0.0% (0 of 40,172).
- [✓] C2 D2 DW-cell replace genuine all-DW >>50% (from 16.6%): PASS — 100.0%.
- [ ] C3 D1 maintain off-support, DW→SW ~0 AND mass <0.2%: PARTIAL/FAIL —
      DW→SW=81 (~0, OK), BUT total maintain off-support mass = 0.685% > 0.2%.
      Decomposition: wall-flips (DW→SW 81 + SW→DW 1,825 = 1,906 = 0.089%) ARE
      under 0.2%; the gate is breached entirely by age-backward moves (14,121
      rows = 0.66%), unchanged from the pre-fix 0.67% (motivation L22-24).
- [✓] C4 D1 exit re-entry 0; regime change 0: PASS — both 0.
- [✓] C5 D1 replace off-support reported: PASS — 98.26% (report-only; T014).
- [✓] C6 P_hat rows sum to 1; converged: PASS — 04o converged TRUE iter 10,
      LL=-227238.316; estimator's rowSums(P) assertion would have halted else.

Why PSEUDOCODE_FAIL (not TRANSLATION_FAIL): R1 translated correctly; C3 cannot
be met by anything in scope. Q2 explicitly states the age-backward residual
"is exactly what the Step-3 maintain-off-support gate measures — report, do not
engineer away" and defers the F_replace/portfolio-age fix to Ticket 014. So the
spec's own Q2 contradicts the Step-3 hard gate "maintain off-support mass <
0.2%": BOY stamping provably cannot reduce the ~0.66% age component (it was
0.67% pre-fix, 0.66% post-fix). The substantive target BOY targets — wall
mis-celling — is fully achieved (D2 0%, wall maintain off-support 0.089%).

Question for Opus:
  "Does the Step-3 C3 gate 'maintain off-support mass < 0.2%' apply to the wall/
  composition component BOY can fix (0.089% — PASS) or to total maintain
  off-support including the irreducible age-backward residual (0.685% — which Q2
  itself says to report and defers to Ticket 014)? If the latter, the gate is
  unachievable in this ticket's scope and should be restated as a wall-only
  threshold plus a reported age residual; if so, Attempt 1 PASSES as-is. Do NOT
  re-run R1 — the implementation is faithful; only the acceptance gate needs
  reconciling."

### Closure — 2026-06-02 (researcher decision; resolves the C3 question)
Result: **PASS — CLOSED.** The reviewer's open question is answered by the
researcher: C3 is NOT a failure. The ticket's job was to fix WHICH CELL a
decision is in (BOY decision-time stamping); on that it succeeded completely —
D2 wall mis-celling 16.2% → 0%, DW-replace genuine all-DW 16.6% → 100%, wall
maintain off-support 0.089%, exit/regime leakage 0, estimator converged.

The residual 0.685% maintain off-support and 98% replace off-support are now
RECLASSIFIED as DOCUMENTED KNOWN LIMITATIONS of the model's single-tank state
abstraction — the thing this ticket SURFACED, not something it was supposed to
remove:
  - AGE-BACKWARD under Maintain (~0.66%) is a REAL limitation. Treating a
    facility as a single-tank manager forces the state to be a portfolio
    AVERAGE age; a partial closure of an old tank lowers the average, which the
    stay/+1 F_maintain kernel cannot represent. This is intrinsic to the
    single-tank choice and only the portfolio (histogram-state) model removes
    it (Ticket 014 / Scale-Incorporation sketch).
  - PARTIAL-CLOSURE / REPLACE off-support (98%) is the OPEN QUESTION of whether
    the data can support a richer scale/portfolio model (the partial-closure and
    replace MARGINS, F^R/F^C, kappa1 size-gradient). That is a
    can-the-data-identify-it question, not a coding defect — diagnostics in the
    T014 scoping (HANDOFF 2026-06-02) decide lite-vs-full.

These limitations are now written into the standing documentation (paper
identification audit, scale-incorporation sketch, HANDOFF, memory) to be raised
with advisors/committee. The Step-4 movement (gamma_price −96.7%, K_DW +33.9%,
LL not comparable across stampings) makes the welfare CF re-run (Ticket 017)
warranted. R1 was NOT re-run; the implementation was faithful on the first
attempt.
