# TICKET 037 — Two-part fair premium (leak rate × cleanup cost), first-principles rebuild
# Created: 2026-06-27
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Type: ESTIMATORS (frequency rebuild + severity align + combine) + per-unit fitted
#       outputs with CIs. No DCM/Bellman/C++. Supersedes the rushed 01p first-release model.

═══════════════════════════════════════════════════
FIRST-PRINCIPLES STATEMENT (what we are computing)
═══════════════════════════════════════════════════
Fair (break-even) premium for a tank-year = expected fund cost it generates:

      fair premium per tank-year  =  (leaks per tank-year)  ×  ($ per cleanup)
                                  =        λ(age,wall)        ×     S(age,wall)

- Frequency λ: leaks per TANK-year, by age×wall. Map site→tank by EXPOSURE: a site with
  N tanks gets N independent shots at a leak, so per-tank rate = leaks ÷ tank-years.
  Single-tank sites identify it exactly; multi-tank sites contribute under equal-shots.
- Severity S: average $ per CLEANUP, by age×wall. No tank mapping needed — a cleanup is
  one event with one bill; we attach it to the age×wall of the site where it occurred.
- Units cancel: (leaks / tank-yr) × ($ / leak) = $ / tank-yr. Facility = Σ over its tanks.

This REPLACES the prior λ (facility FIRST-release logistic, never-leaked set, single-tank
grid hack), which (a) counted only a site's first leak ever and (b) was site-level patched
to per-tank. The new λ counts EVERY leak as a rate per tank-year over ALL facility-years.

═══════════════════════════════════════════════════
CROSS-VALIDATION & NO LEAKAGE — APPLIES TO ALL PARTS (non-negotiable)
═══════════════════════════════════════════════════
Every fitted value that feeds a cell average, a figure, or a downstream estimate MUST be an
OUT-OF-SAMPLE (cross-validated) prediction. NO in-sample `predict(fit, newx=X)` for any value
that gets used. Mechanics (elastic net analogue of random-forest OOB = CV "prevalidation",
Tibshirani & Efron 2002):

1. GROUPED CV FOLDS BY FACILITY, STRATIFIED BY STATE. Assign foldid at the FACILITY
   (panel_id) level — ALL of a facility's rows share a fold (a facility spans up to 27 years;
   random row-folds would leak its other years into its own prediction). AND stratify by
   state: split EACH state's facilities across all K folds, so every state appears in every
   training fold. (Otherwise a fold could hold all of a small state's facilities -> that
   state's fixed effect is unestimable when the fold is held out -> garbage OOS predictions
   for it. Critical for small states.) Implementation: within each state, randomly assign its
   facilities to folds 1..K; combine. For severity, same (group by facility, stratify by state).
   K = 10, deterministic seed.
2. PREVALIDATED OOS PREDICTIONS. Fit with `cv.glmnet(..., foldid=fold, keep=TRUE)`; take the
   prevalidated predictions `fit.preval[, idx(lambda.min)]` — each row predicted from the fold
   it was HELD OUT of. These (not the in-sample fit) are the per-unit fitted values.
   - Poisson with offset: fit.preval is on the LINK scale incl. offset; OOS expected count =
     exp(preval_link); OOS PER-TANK rate mu_i = exp(preval_link_i) / active_tanks_i. (Verify
     glmnet's offset handling on a tiny check before trusting it.)
   - Severity Poisson (no offset): OOS expected cost = exp(preval_link).
3. CELL SCHEDULES are built by averaging the OOS (prevalidated) per-unit predictions over the
   estimation sample — NOT a synthetic grid, NOT in-sample fits. Tanks in panel_dt then inherit
   their cell's OOS-averaged value (leakage-free at the cell level).
4. ALPHA + LAMBDA TUNING. Share ONE foldid vector across all alpha candidates (fair comparison);
   pick (alpha, lambda.min) by CV deviance; use that model's fit.preval for the OOS values.
   GOLD-STANDARD option (note, implement if cheap): nested CV — outer facility-folds for OOS,
   inner folds for (alpha,lambda) — removes the mild alpha-selection optimism. At minimum,
   report whether nested vs single-CV changes the cell schedule materially.
5. BOOTSTRAP (for CIs) is SEPARATE from OOS point estimates and must ALSO be cluster-resampled
   by FACILITY (resample facilities with replacement, carry all their rows), refit at fixed
   (alpha,lambda), re-predict, re-average -> percentile CIs. Never resample facility-years
   independently (breaks the panel structure).
6. lambda.min for point predictions; report lambda.1se as a robustness line.
7. fit.preval IS ON THE LINK SCALE (confirmed: glmnet vignette passes fit.preval + family to
   assess.glmnet). Poisson: OOS expected count = exp(fit.preval[,idx]); per-tank rate =
   exp(fit.preval[,idx]) / active_tanks (verify the offset is inside the link with a 3-row check).
   Binomial-style families: inverse-logit. Severity Poisson: exp(fit.preval).
8. HONEST FIT METRICS FOR THE PAPER: get cross-validated goodness-of-fit straight from the
   prevalidated array — assess.glmnet(fit.preval, newy=Y, family=...) -> OOS deviance / mse /
   (pseudo-)R^2 (severity) and OOS deviance (frequency). These are the predictability numbers to
   report — no in-sample inflation. (Feeds the risk-predictability-stats need for the intro.)

Note on terminology: we use penalized regression, so "OOB" = the CV/prevalidation analogue;
there is no random forest here. If a tree model is ever used, use its true OOB predictions.

═══════════════════════════════════════════════════
PERFORMANCE & PARALLELISM (Windows server)
═══════════════════════════════════════════════════
The frequency model is ~5.5M rows; the bootstrap is B≥200 refits. Speed it up:
1. SPARSE design matrix: build X with Matrix::sparse.model.matrix (the age/wall/state/year
   dummies are mostly zeros). glmnet is much faster + far less memory on sparse X. This is the
   single biggest win for the 5.5M-row frequency fit and makes parallel folds memory-feasible.
2. PARALLEL on Windows = PSOCK, not fork. The glmnet docs' doMC/registerDoMC example is
   Unix-only. On the Windows server use:
     library(doParallel); cl <- makeCluster(K); registerDoParallel(cl)
     cv.glmnet(..., parallel = TRUE)   # parallelizes the K folds
     ... ; stopCluster(cl)
   Knob NWORKERS (default min(K, detectCores()-1)); watch memory (each PSOCK worker copies X —
   sparse X keeps this affordable; cap workers if RAM-bound, server has 64GB).
3. PARALLEL BOOTSTRAP (the big win): distribute the B refits across workers (parLapply/foreach);
   each worker resamples facilities, refits glmnet at FIXED (alpha,lambda) — one fit, not a path
   — predicts the cells, returns the draw. clusterExport the static design once.
4. Single fit at fixed (alpha,lambda) is cheap; the path-CV is the cost — keep alpha grid modest
   ({0,.25,.5,.75,1}) and let lambda default.

═══════════════════════════════════════════════════
SAVE TRAINED MODELS (reload without refitting)
═══════════════════════════════════════════════════
Researcher requirement: after fitting ON THE SERVER, save everything needed to recompute cell
schedules / fitted values / figures WITHOUT re-running the fits. Each *_model.rds must contain:
  - the cv.glmnet object INCLUDING fit.preval + foldid (so OOS predictions are reproducible),
  - the final glmnet fit at (alpha,lambda),
  - best_alpha, best_lambda, the feature formula + colnames(X), factor levels,
  - the bootstrap draws matrix (per-cell B columns) so CIs need no refit,
  - assess.glmnet OOS metrics, sample sizes, seed, timestamp.
Files: analysis_leak_rate_model.rds, analysis_severity_model.rds, and a combined
  analysis_fair_premium_models.rds (or keep separate). Saved to Data/Analysis on the server;
  the researcher pulls them back to recompute estimates locally.

═══════════════════════════════════════════════════
USES / PAPER ALIGNMENT (02_JMP_Draft.qmd — what these feed)
═══════════════════════════════════════════════════
- §4.1 "Observable risk": fig-cell-risk (SW-vs-DW leak rate by age), fig-lift, fig-calibration,
  fig-cell-gof, ROC/PR AUC — ALL from the frequency model's OOS predictions + assess.glmnet.
  "Risk is predictable" + "SW rises with age, DW ~flat" come from here. NOTE the new model
  REVISES current text: "first-release probability / pre-reform / logistic" -> "annual leak
  rate per tank-year / detection-era / count-offset"; and re-check the "DW flat" claim
  (production showed both rise, SW higher). Decision flagged for researcher (one all-years model
  for predictability too, vs the paper's current pre-reform-only).
- §4.1 cross-subsidy: fair premium = freq × severity. *** Current text says cleanup cost is
  flat in age/wall so uses pooled severity — the severity model CONTRADICTS this (SW ~1.48x DW);
  fair-premium gradient now comes from BOTH. All cross-subsidy numbers update. ***
- §4.2 "Pricing observable risk": fig-actuarial-alignment (empirical risk vs TX premium) = the
  CURRENT comparison; ADD the fair-premium(λ·S)-vs-real-TX-premium version (researcher wants both).
- §5 DCM: reuses the cell schedules — BUT at the DCM's OWN granularity, not the figure bins.
  DCM-INPUT SCHEDULE (additional outputs, built on the structural state space):
    * AGE bins = PM03 AGE_BREAKS = c(0,5,10,15,20,25,30,35,Inf) -> 8 five-year bins
      (NOT the 9 three-year figure bins; the estimators must re-bin to these for the DCM file).
    * WALL = SW/DW (16 MARG cells: SW_8(oldest)..SW_1, DW_8..DW_1).
    * CAPACITY = N_G=4 bins (NEW: the portfolio model now tracks capacity/size in the env, so the
      hazard/severity should vary by capacity bin G, not just age×wall). Both 01r and 01q already
      carry total_capacity as a feature, so predict by (age × wall × G).
  OPEN QUESTIONS for the structural-model agent (researcher offered pointers — these pin the DCM
  output shape; figures/Parts 1-3 do NOT depend on them and proceed now):
    Q1. Exact 4 capacity-bin (G) breaks — from facility total_cap_capped (winsorized 60k, PM01)?
        quantile or fixed edges?
    Q2. Is the hazard primitive indexed by capacity (age×wall×G) or only (age×wall)? (Memory says
        capacity enters REVENUE via capbar_G; confirm whether the kernel's hazard slot is G-indexed.)
    Q3. National vs state- and/or regime(FF/RB)-specific hazard/severity for the DCM? (04b built it
        national, regime-replicated; the new env may want state.)
    Q4. Exact cell/sidx ordering the kernel expects when it reads the hazard vector (so the CSV
        rows line up with the state index).
  Output (once Q1-Q4 known): dcm_cell_hazard_pricing_struct.csv / dcm_cell_severity_pricing_struct.csv
  on the DCM bins. Keep the figure-bin schedules (dcm_cell_*_pricing.csv) for the M1 figures.

═══════════════════════════════════════════════════
PART 1 — FREQUENCY: 01r_Leak_Rate.R  (NEW; archive 01p)
═══════════════════════════════════════════════════
Move Code/Analysis/Descrptive Facts/01p_Pricing_Hazard.R ->
  Code/Analysis/Descrptive Facts/Archive/01p_Pricing_Hazard_legacy_firstrelease.R (retired-code rule).
Write Code/Analysis/Descrptive Facts/01r_Leak_Rate.R.

DATA: Data/Processed/facility_leak_behavior_annual.csv.
SAMPLE: panel_year ∈ [1990, 2016] (detection era); active_tanks > 0; make-model
  (fac_wall != "Unknown-Wall" & fac_fuel != "Unknown-Fuel" & non-NA age); non-NA
  active_tanks, total_capacity, n_leaks; state ∈ ALL_STUDY_STATES.
  *** NO has_previous_leak filter *** (recurrent: every facility-year, every leak counts).

MODEL (Poisson elastic net / PPML — count with exposure offset):
  outcome  Y = n_leaks  (facility-year leak count; ≈0/1, rare 2+)
  offset   = log(active_tanks)                       # exposure -> per-tank RATE
  features FEAT = ~ (age_bin + has_single_walled + active_tanks + total_capacity
                     + has_gasoline_year + has_diesel_year)^2 + state_f + year_f
  glmnet family="poisson", offset=log(active_tanks). α∈{0,.25,.5,.75,1} tuned by CV on a
  subsample; single fit on full data at best (α,λ). predict type="response" with offset.
  age_bin: 9 bins (use panel age_bins if labels match AGE_BIN_LABELS, else from avg_tank_age).

PER-TANK RATE = exp(features·β)  (the offset makes the linear predictor a per-tank log-rate).
  Fitted per-tank rate for facility-year i: mu_i = predict(..., newoffset=0, type="response").

CELL SCHEDULE — *** fitted-values-then-average *** (NOT a synthetic reference grid):
  For each cell c=(state, age_bin, wall): lambda(c) = mean over facility-years in c of mu_i.
  NATIONAL row per (age_bin, wall) = exposure-weighted mean of mu_i over ALL states' fac-yrs
  (i.e. tank-year-weighted: weight = active_tanks).
  CI: cluster bootstrap by FACILITY (resample facilities, refit at fixed α,λ, re-predict,
  re-average) B≥200 -> lambda_lo, lambda_hi (2.5/97.5).

DIAGNOSTICS (print):
  - raw per-tank rate = sum(n_leaks)/sum(active_tanks) on the sample (expect ~0.0045);
    compare to mean(mu_i). They should be close (calibration check).
  - single-tank-only cell rates vs pooled cell rates (the equal-shots assumption check);
    print correlation / a few cells side by side.
  - national lambda by age×wall (per 1000 tank-yr): expect rising-with-age, SW≥DW.

OUTPUTS (Data/Analysis/):
  dcm_cell_hazard_pricing.csv   OVERWRITES; cols EXACTLY
        state, age_bin, has_single_walled, lambda, lambda_lo, lambda_hi
        rows = (n_states + 1[NATIONAL]) × 9 age × 2 wall. lambda ∈ (0, 0.5).
  analysis_leak_rate_predictions.csv   per facility-year fitted rate; cols EXACTLY
        panel_id, panel_year, state, has_single_walled, age_bin, active_tanks, mu_tank
        (mu_tank = per-tank-year rate). (no CI per row needed; CI lives at cell level.)
  analysis_leak_rate_model.rds   fit + feature_cols + best(α,λ) + metadata.
SMOKE knob LEAKRATE_SMOKE=1 subsamples CV + cuts bootstrap B (schema unchanged).

═══════════════════════════════════════════════════
PART 2 — SEVERITY: 01q_Severity_Model.R  (EXISTS; one alignment edit)
═══════════════════════════════════════════════════
Keep the PPML cost model. CHANGE only the NATIONAL aggregation to match Part 1's
fitted-values-then-average convention:
  NATIONAL sev_hat per (age_bin, wall) = CLAIM-weighted average of fitted Ŝ over ALL claims
  in that (age_bin, wall) cell (pool states), NOT a simple mean of per-state cell predictions.
  Bootstrap CI recomputed on that claim-pooled average. Per-state rows unchanged.
ADD: analysis_severity_predictions.csv — per claim fitted; cols EXACTLY
  state, age_bin, has_single_walled, sev_hat_claim.
Output dcm_cell_severity_pricing.csv cols unchanged (state, age_bin, has_single_walled,
  sev_hat, sev_lo, sev_hi).

═══════════════════════════════════════════════════
PART 3 — COMBINE: 08_Fair_Premium.R  (NEW)
═══════════════════════════════════════════════════
Join the two cell schedules and emit per-cell and per-unit fair premiums WITH CIs.
Frequency sample (facility-years) and severity sample (claims) are INDEPENDENT, so combine
by joint draw: fair = λ · S; CI via independent product bootstrap OR delta method
  Var(λS) = S²·Var(λ) + λ²·Var(S)  ->  fair_lo/hi.
(Use the cell CIs already produced; treat lambda and S as independent.)

OUTPUTS (Data/Analysis/):
  dcm_cell_fair_premium.csv   cols EXACTLY
        state, age_bin, has_single_walled, lambda, sev_hat, fair, fair_lo, fair_hi
        ((n_states+1) × 18 rows; fair = lambda·sev_hat in $/tank-yr).
  analysis_fair_premium_tank.csv     per tank-year (join panel_dt tanks to NATIONAL cells):
        tank_panel_id, panel_id, state, panel_year, age_bin, has_single_walled,
        lambda, sev_hat, fair, fair_lo, fair_hi
  analysis_fair_premium_facility.csv per facility-year (sum tanks within panel_id×year):
        panel_id, state, panel_year, n_tanks, fair, fair_lo, fair_hi
PRINT: national fair-premium schedule by age×wall ($/tank-yr) + blended mean.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
CV / NO-LEAKAGE (BOTH 01r and 01q):
- [ ] foldid assigned at FACILITY (panel_id) level — all of a facility's rows share a fold
      (grouped CV); deterministic seed; K=10. (NOT random per-row folds.)
- [ ] alpha tuned by re-calling cv.glmnet across alpha with the SAME foldid (per glmnet docs).
- [ ] cv.glmnet called with keep=TRUE; cell schedules + reported fitted values use the
      prevalidated fit.preval (OOS), NOT in-sample predict(fit, newx=X).
- [ ] fit.preval handled on the link scale (exp() for Poisson; /active_tanks for the rate);
      a 3-row offset sanity check is printed.
- [ ] OOS goodness-of-fit reported via assess.glmnet(fit.preval, ...) (CV deviance etc.).
- [ ] Bootstrap CIs cluster-resample by facility (not by facility-year).

01r (frequency):
- [ ] glmnet family="poisson" with offset=log(active_tanks) on outcome n_leaks; NO weights; NO Platt.
- [ ] Sample is ALL detection-era (1990–2016) facility-years (NO has_previous_leak filter); active_tanks>0.
- [ ] FEAT formula as specified (age×wall interaction present; year_f present).
- [ ] Cell lambda = fitted-values-then-average (mean of per-fac-yr mu over the cell), NOT a
      single synthetic-grid prediction; NATIONAL = tank-year(exposure)-weighted.
- [ ] Cluster bootstrap by facility -> lambda_lo/hi; B≥200 (smoke may reduce).
- [ ] Calibration print: mean(mu_i) ≈ sum(n_leaks)/sum(active_tanks) (raw ~0.0045); within ~15%.
- [ ] Single-tank-vs-pooled cell check printed.
- [ ] dcm_cell_hazard_pricing.csv has the 6 cols incl lambda_lo/hi; (n_states+1)*18 rows.
- [ ] analysis_leak_rate_predictions.csv cols exact. 01p moved to Archive/.
- [ ] Hard errors only (no tryCatch->NULL/try silent); logging block; SMOKE knob schema-safe.
01q (severity align):
- [ ] NATIONAL sev_hat = claim-pooled fitted average (not simple per-state mean); per-state rows unchanged.
- [ ] analysis_severity_predictions.csv written with the 4 cols.
08 (combine):
- [ ] fair = lambda·sev_hat per cell; fair_lo/hi from independent combination of the two cell CIs.
- [ ] dcm_cell_fair_premium.csv (8 cols), analysis_fair_premium_tank.csv, analysis_fair_premium_facility.csv
      written with EXACT columns above; facility fair = sum of its tanks' fair.
- [ ] National blended fair-premium printed (~$1–6k/tank-yr range expected; sanity, not a hard bound).

═══════════════════════════════════════════════════
KNOWN LIMITATIONS (note; do not block)
═══════════════════════════════════════════════════
- Site→tank uses equal-shots-per-tank (exact for single-tank sites; assumption for multi-tank).
  We report the single-tank-vs-pooled check as evidence it holds.
- λ uses facility-aggregate age/wall (releases not attributable to a specific tank); single-tank
  sites carry the clean signal.
- Severity uncapped (fund coverage limit not yet applied) — overstates fund liability; parked.
- Detection still adjusted by calendar year only (wall-specific detection device data sparse; parked).
- Severity sample = per-incident states CO/NM/PA/TN (LA site-aggregated, UT tiny); figure
  states borrow NATIONAL severity for LA.

═══════════════════════════════════════════════════
DOWNSTREAM (separate follow-on tickets, not this one)
═══════════════════════════════════════════════════
- Rewire 07f/07g to read analysis_fair_premium_tank/_facility (fair + CI) instead of λ×flat-S̄.
- 06 TX fair-vs-real premium uses dcm_cell_fair_premium.csv (TX rows).
- DCM consumes dcm_cell_hazard_pricing.csv (new λ) + dcm_cell_severity_pricing.csv.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
