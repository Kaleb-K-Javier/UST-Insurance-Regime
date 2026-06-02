# TICKET 016 — Relax BOTH gamma bounds symmetrically (remove unjustified magnitude limits)
# Created: 2026-06-01
# Status: AWAITING_IMPLEMENTATION (researcher approved 2026-06-01)
# Attempt: 0
# Assignee: R1
# Gate: estimator-settings change (bounds) — researcher explicitly approved.
# Depends on: Ticket 013 (BOY panel + fit) already landed.

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
gamma_price is box-constrained to [-3, -1] and gamma_risk to [0, 0.15]. The
magnitude limits (the -1 floor on |gamma_price|, the 0.15 ceiling on gamma_risk)
are unjustified priors. The only defensible restriction on each is its SIGN, which
is definitional: gamma_price <= 0 (premium is a cost) and gamma_risk >= 0 (expected
loss is a bad). Both gammas should be bounded SYMMETRICALLY — same magnitude range,
opposite sign — and otherwise free.

NOTE (2026-06-02 correction): the [-3,-1]/[0,0.15] ranges were NOT optim bounds —
they were post-hoc warning() range-checks in the 04o driver (04o_...Clean.R:179-182).
The actual box the 013 fit used was gamma_price [-20,5], gamma_risk [-5,10] (config
defaults). So the gammas were never sign-locked. pre-BOY -1.114 and post-BOY -2.192
were both interior optima, not pinned.

RESEARCHER INSTRUCTION (UPDATED 2026-06-02): do NOT constrain the sign of either
gamma. Let both search a wide symmetric range, both signs allowed:
    gamma_price in [-20, 20]
    gamma_risk  in [-20, 20]
The data places each gamma with no sign or magnitude prior. Estimates (~ -2 and
~0.1) sit far inside this box; just let it run and report where they land.

═══════════════════════════════════════════════════
ENVIRONMENT
═══════════════════════════════════════════════════
Rscript: C:\Program Files\R\R-4.5.2\bin\Rscript.exe
Re-estimate the BOY 6p+FE fit on the EXISTING BOY panel + primitives (Ticket 013
outputs — do NOT re-run 04b). Everything else UNCHANGED: tol_theta, tol_P,
npl_iter, sigma2, beta, ccp_damping, warm start, kappa/K — all exactly as the
Ticket-013 04o run. The ONLY changes are the two gamma boxes.

═══════════════════════════════════════════════════
STEP 1 — WIDEN BOTH BOUNDS (driver-level override; do NOT edit the shared config fn)
═══════════════════════════════════════════════════
In a NEW driver cloned from 04o (Code/Dynamic_Model/04p_6paramFE_Profile_GammaFree.R),
AFTER cfg is created by create_estimation_config_replacement_6p_fe_profile(...),
override BOTH with a wide sign-free box:
    cfg$gamma_price_bounds <- c(-20, 20)
    cfg$gamma_risk_bounds  <- c(-20, 20)
Confirm in the log that the estimator consumes cfg$gamma_price_bounds AND
cfg$gamma_risk_bounds as the optim lower/upper for the two gammas (the box is
enforced — pre-BOY gamma_price pinned at -1.114 proves it). If the estimator does
NOT read these config fields for the gamma bounds, STOP and report where the gamma
bounds are set.

═══════════════════════════════════════════════════
STEP 2 — RE-ESTIMATE + SAVE (separate fit; do not overwrite the 013 fit)
═══════════════════════════════════════════════════
Re-run the 6p+FE profile estimation on the BOY panel with the relaxed bounds.
Save to: Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds
Acceptance: converged == TRUE; all P_hat rows sum to 1. Report the estimated gammas
as-is — no pinning gate. (If a gamma lands at +/-20 it will show in the table and we
revisit; do not block on it.)

═══════════════════════════════════════════════════
STEP 3 — DELIVERABLE: bound-sensitivity comparison
═══════════════════════════════════════════════════
Output/Tables/T016_Gamma_BoundSensitivity.csv
  Rows: 7 — kappa_SW, kappa_DW, K_SW, K_DW, gamma_price, gamma_risk, log_likelihood.
  Cols (exact, types):
    parameter     chr
    BOY_013       num   (the Ticket-013 fit value; kappa in USD=*10000,
                         K=exp(K_log), gammas raw)
    BOY_gammafree num
    delta         num   (BOY_gammafree - BOY_013)
  Print a one-line summary of how far each gamma moved (delta and % change).
  No pinning logicals, no verdict prose. Notes line: the 013 fit used the config-
  default box gamma_price [-20,5], gamma_risk [-5,10]; this run widened both to
  [-20,20] (sign-free).

═══════════════════════════════════════════════════
STEP 4 — IF CLEAN, PROMOTE TO CANONICAL
═══════════════════════════════════════════════════
If Step 2 converged (P_hat rows sum to 1), the gammafree fit is the new canonical
BOY fit; the welfare CF and identification appendix will point at it. Do NOT delete the boxed 013 fit — keep both. Log the decision.

WHAT NOT TO DO: do not re-run 04b; do not change any tolerance, npl_iter, sigma2,
beta, ccp_damping, warm start, or the kappa/K bounds; do not touch F_replace or the
Bellman/CCP code. The two gamma boxes are the only change.

## Attempt Log

### Attempt 1 — 2026-06-02
Transcript: skipped (PowerShell host transcript only, workflow bug W6 — not needed; verdict clear from driver + log + git).
Reviewer: Sonnet (reviewer seat)
Result: PASS

Note on spec format: this ticket has no numbered "ACCEPTANCE CRITERIA (C1, C2...)" block.
Not failed on that ground — the requirements are discretely and unambiguously enumerated
across Steps 1-4 (bounds override, save path, table shape, promotion rule), so the
faithfulness check is fully mechanical. Reviewer derived criteria C1-C7 from those steps.
Recommend future settings-change specs include an explicit numbered ACCEPTANCE block.

Criteria:
- [x] C1 (Step 1) NEW driver 04p cloned from 04o; both gamma boxes overridden to
      c(-20,20) AFTER cfg built; shared config fn NOT edited: PASS
      (04p:131-148; create_estimation_config_replacement_6p_fe_profile untouched).
- [x] C2 (Step 1) Estimator consumes cfg$gamma_price_bounds AND cfg$gamma_risk_bounds
      as optim lower/upper for the two gammas: PASS
      (improved_estimator_OPTIMIZED.r:4787-4792 build lower_b/upper_b from those fields
      at struct positions 5/6; passed to optim lower=/upper= at 4808-4809. Log confirms
      before-override [-20,5]/[-5,10] -> after-override [-20,20]/[-20,20]).
- [x] C3 (Step 2) Re-estimate BOY panel, relaxed bounds, save to
      Model_Replacement_6paramFE_profile_clean_observed_gammafree.rds; converged==TRUE;
      all P_hat rows sum to 1; gammas reported as-is, no pinning gate: PASS
      (log: converged TRUE iter 9, LL=-227239.784, P_hat rowSums==1 PASS; saved;
      gamma_price=-2.1632, gamma_risk=0.1137, both deep interior of [-20,20]).
- [x] C4 (Step 2) All other settings UNCHANGED vs 04o (tol_theta/tol_P 1e-5,
      max_npl_iter 200, sigma2 1.0, beta 0.95, ccp_damping 0.6, warm start, kappa/K
      bounds): PASS (cfg block identical to 04o; only the two gamma override lines added).
- [x] C5 (Step 3) T016_Gamma_BoundSensitivity.csv: 7 rows (kappa_SW, kappa_DW, K_SW,
      K_DW, gamma_price, gamma_risk, log_likelihood); cols parameter(chr)/BOY_013(num)/
      BOY_gammafree(num)/delta(num); kappa in USD (*1e4), K=exp(K_log), gammas raw;
      one-line gamma movement summary (delta + %); notes line present; no pinning
      logicals/verdict prose: PASS (CSV verified; delta = gammafree - 013 checks out).
- [x] C6 (Step 4) Converged + P_hat sum -> promote to canonical; boxed 013 fit NOT
      deleted; decision logged: PASS (Section 10; 013 .rds retained on disk).
- [x] C7 (WHAT NOT TO DO) No 04b re-run; no tolerance/npl/sigma2/beta/ccp/warm/kappa-K
      change; no F_replace/Bellman/CCP edit: PASS (git: 04p is the only T016 artifact;
      improved_estimator_OPTIMIZED.r / cpp_engine.cpp working-tree mods predate T016
      = the T005 6p suite, and introduce no gamma-bound or tolerance change for T016).

Known, non-blocking (researcher pre-noted): optim code 52 at NPL inner iters 2-9
(L-BFGS-B line-search; outer NPL converged dTh->0, dP=6.0e-6 < tol_P 1e-5); MO zero
exits -> alpha_MO pinned at +20 (structural, gamma-bound-independent).
