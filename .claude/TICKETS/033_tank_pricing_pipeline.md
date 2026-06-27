# TICKET 033 — Tank-level M1 pricing pipeline: 01p de-confounded hazard + 07f tank cross-subsidy
# Created: 2026-06-26
# Status: AWAITING_REVIEW
# Attempt: 1
# Type: ESTIMATOR (hazard) + FIGURES. Code authored directly by Opus (no R1 leg).
#       Reviewer: MECHANICAL check of the two scripts against the criteria below.

═══════════════════════════════════════════════════
WHAT THIS COVERS (two new files, both on main through 727d94c)
═══════════════════════════════════════════════════
1. Code/Analysis/Descrptive Facts/01p_Pricing_Hazard.R
     De-confounded per-tank leak hazard for fair-premium pricing. Separate from 01n
     (which stays the observability model). Trains on the detection era with a
     calendar/period control so the wall/age coefficients are not contaminated by the
     detection trend (recorded rate/1k ~0 pre-1985 -> ~15 in 1990-95 -> ~3 by 2020).
     Predicts the (age x wall) cell schedule at a fixed reference year + single-tank
     exposure. NON-DESTRUCTIVE: new outputs only, 01n untouched.
2. Code/Analysis/07f_Tank_CrossSubsidy.R
     Tank-level cross-subsidy figures off that schedule. Uniform-premium framing
     (no "subsidy" language), severity-bootstrap CI band, single-year + pooled-by-state
     + fee-vs-fair, each emitted in BOTH wall modes (age x wall AND wall-collapsed).

DESIGN CONTEXT (for the reviewer — not criteria):
  - The inverse-frequency weights + Platt scaling from 01n were REMOVED in 01p: the
    weighted glmnet scale collapsed the Platt slope to ~0 -> flat predictions. An
    unweighted logistic elastic net is self-calibrated on the rare outcome.
  - Age-period-cohort: age + period are kept; vintage is NOT added separately (collinear).
  - Wall is detection-confounded (DW>=SW is monitoring, not risk); both wall modes are
    produced so the researcher chooses later. Not a criterion to judge.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA — 01p_Pricing_Hazard.R
═══════════════════════════════════════════════════
C1  Training sample filter is exactly: panel_year in [DETECTION_START=1990, TRAIN_END=2016]
    AND has_previous_leak==0 AND is_make_model==1 AND has_single_walled in {0,1} AND
    non-NA active_tanks, total_capacity, event_first_leak.
C2  Outcome Y = event_first_leak (binary).
C3  FEAT_FORMULA = ~ (age_bin + has_single_walled + active_tanks + total_capacity +
    has_gasoline_year + has_diesel_year)^2 + state_f + year_f  — i.e. the saturated
    age x wall interaction IS present AND a period control year_f IS present (main effect).
C4  NO class weights: there is no `weights=` argument on any cv.glmnet/glmnet call.
    NO Platt: there is no glm(Y ~ lp_oos ...); predictions use predict(fit, type="response").
C5  Cell schedule is predicted on a grid at: year_f = REFERENCE_YEAR(=2008),
    active_tanks = 1, total_capacity = median single-tank capacity, has_gasoline_year = 1,
    has_diesel_year = 0; for every state level, plus a NATIONAL row = mean over states.
C6  Writes Data/Analysis/dcm_cell_hazard_pricing.csv with columns EXACTLY
    (state, age_bin, has_single_walled, lambda). One row per (state in {study states} ∪
    {NATIONAL}) x 9 age bins x 2 wall = (n_states+1)*18 rows. lambda numeric in (0,1).
C7  Writes Data/Analysis/analysis_hazard_predictions_pricing.csv with columns EXACTLY
    (panel_id, panel_year, state, has_single_walled, age_bin, pred_pricing); pred_pricing in (0,1).
C8  Saves Data/Analysis/analysis_pricing_hazard_model.rds containing fit + feature_cols +
    metadata; it MUST NOT contain a `platt` element (removed).
C9  Non-destructive: the script does NOT write analysis_hazard_predictions_full.csv and
    does NOT write/modify any 01n output or 01n itself.
C10 Hard error propagation: no `tryCatch(expr, error=function(e) NULL)` and no
    `try(..., silent=TRUE)`. Logging block (sink to logs/) present. PRICING_SMOKE env knob
    only subsamples CV / reduces folds — it does not change outputs' schema.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA — 07f_Tank_CrossSubsidy.R
═══════════════════════════════════════════════════
C11 Reads the schedule Data/Analysis/dcm_cell_hazard_pricing.csv (HAZ_CELL) and STOPS with
    a clear error if absent (no silent fallback).
C12 Severity: SEV_STATES = {CO,NM,PA,TN} (LA excluded); net rule net_nom = (state=="NM" ?
    max(total_cost - 10000, 0) : total_cost); net_2023 deflated; S_bar = mean(net_2023);
    nonparametric bootstrap with N_BOOT=1000 -> S_lo,S_hi = 2.5/97.5 percentiles.
C13 Cell hazard: HAZARD_SCOPE=="national" uses the NATIONAL rows. cell_aw keyed by
    (age_bin, has_single_walled); cell_age = mean(lambda) by age_bin (wall collapsed).
C14 Tank panel: FIG_STATES = {CO,LA,NM,TN}; mm_wall in {Single-Walled, Double-Walled};
    age_bin computed from tank_age via AGE_BIN_BREAKS with AGE_BIN_LABELS matching the
    schedule's labels; both lookups merged; for each mode m, fair_m = lambda_m * S_bar and
    fairlo_m/fairhi_m = lambda_m * S_lo/S_hi.
C15 Per-tank fee: fr_premium_per_tank_yr joined by (panel_id, panel_year); NA -> 0.
C16 Uniform-premium framing: the strings "subsidy", "subsidized", "subsidizers" appear in
    NO figure title/subtitle/caption/axis label. tau = mean(fair premium) over the shown tanks.
C17 BOTH wall modes (WALL_MODES = agewall, ageonly) are emitted for all three figure
    families: Fig_TankCrossSub_Single_<ST>_{AgeWall,AgeOnly} (ST in FIG_STATES),
    Fig_TankCrossSub_Pooled_Panel_{AgeWall,AgeOnly}, Fig_TankPaidShare_Pooled_{AgeWall,AgeOnly};
    each saved as .png (and .pdf attempted).
C18 Writes Output/Tables/tank_crosssub_state_summary.csv (cols: scope, wall_mode, state, N,
    uniform_prem, share_over, spread_pct, fee_pct_fair) and
    Data/Analysis/tank_crosssub_tankyears.csv (cols: tank_panel_id, panel_id, state,
    panel_year, tank_age, mm_wall, age_bin, lambda_agewall, lambda_ageonly, fair_agewall,
    fair_ageonly, fee_tank).
C19 save_fig is lock-resilient (PNG always; PDF wrapped in tryCatch that only messages on
    failure — this is the ONE allowed tryCatch). Logging block present. No other silent
    error catching.

═══════════════════════════════════════════════════
HOW TO VERIFY
═══════════════════════════════════════════════════
- `git show 740a8cd 727d94c` / `git diff d05bdc7..727d94c` for the two files, and read both
  scripts directly. No run transcript exists (Opus-authored); skip the transcript step.
- Criteria are mechanical (string/structure checks on the code). Mark anything you cannot
  verify from the code as UNVERIFIABLE rather than guessing.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first review]
