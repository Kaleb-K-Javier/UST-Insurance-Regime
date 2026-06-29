# Ticket 042 — First-leak pricing hazard: portfolio features + uniform-regime sample + per-use CIs

**Owner:** (unassigned) · **Status:** READY · **Created:** 2026-06-28

## Goal
Produce the production **first-leak hazard** that feeds the insurer-pricing / §4.1 analyses:

> `h(t | x) = P(facility's first confirmed release in year t | survived release-free to t, observable portfolio x)`

This is the underwriter's object ("for a facility that hasn't leaked, what's the chance of a claim this year?"). It is the single first-stage prediction that feeds **five** downstream uses (see §Downstream). The job is to (a) enrich its covariates with the full tank portfolio, (b) widen the sample to all uniform-regime years, (c) fix a CV-leakage hole, and (d) attach the *correct, per-use* uncertainty.

## Decision history — DO NOT relitigate
1. **Use the first-leak hazard, NOT a recurrent Poisson leak-rate.** The recurrent rate (script `01r_Leak_Rate.R`) came out flat across age because of **survivorship** — the risky old single-walled tanks already leaked and were pulled from the ground, so the surviving old fleet looks safe. That is the wrong object for a *forward-looking* fair premium and it erases the risk heterogeneity the cross-subsidy / risk-based-pricing argument depends on. **`01r` is SUPERSEDED → move it to `Code/Analysis/Descrptive Facts/Archive/`.** (Per repo convention: archive, don't delete.)
2. **Model = elastic-net logistic** (the `USE_ELNET` branch already in `01n`). **GRF stays OFF** — too slow, not needed. The net at λ≈0 is effectively the unpenalized MLE, which is what makes the analytic CIs below valid.
3. **Releases are recorded at the FACILITY level only** — we never observe which tank leaked. So the model is facility-level, and the per-tank rate-card schedule is read off by **averaging predictions within (age × wall) cells** (`Figure_CV_CellRisk`, already in `01n`), NOT by a structural per-tank decomposition. Do not build the "any-tank-leaks" decomposition.

## Home
Extend **`Code/Analysis/Descrptive Facts/01n_CVValidation.R`** (the canonical §4.1 model). Preserve all existing figures/outputs/columns; the changes below are additive. Keep `USE_GRF = FALSE`, `USE_ELNET = TRUE`, `PRIMARY_MODEL = "ELNET"`.

## Changes

### 1. Sample — regime-clean, uniform-premium expansion
Current `01n` rule (S2): `panel_year < POST_YEAR (1999)` for **all** states. Change to:
- **TX:** `panel_year < 1999` (pre-reform — before firms respond to risk-based pricing).
- **Control states (the 19 uniform-fee states in `CONTROL_STATES`): ALL years.** They never adopt risk-based pricing, so every year is uniform-regime and clean for the regime-neutral physical hazard. (A control state's fund sunsetting does not make it risk-based, so it does not contaminate the leak hazard.)
- Keep all other filters: never-yet-leaked risk set (`has_previous_leak == 0`), `is_make_model == 1`, `has_single_walled ∈ {0,1}`, non-missing `active_tanks`/`total_capacity`/`event_first_leak`.
- **Detection-era floor + calendar control:** add a `panel_year >= 1990` floor and a **year control** (`factor(panel_year)` in the elnet design) so improving detection over time does not masquerade as rising/falling risk. (01p/01r used this; 01n currently has no lower bound and no year term.)
- The full-panel scoring block (for the DCM downstream, currently scores pre+post 1999) stays — it scores every feature combination regardless of the training cut.

### 2. Features — add the full tank portfolio
Current elnet design (S4): `~ (factor(age_bin) + has_single_walled + active_tanks + total_capacity + has_gasoline_year + has_diesel_year)^2 + factor(state)` — all **facility aggregates**.
ADD, built from the facility's **per-tank roster** (make / vintage / age / wall / capacity / fuel are buildable from the tank-level data):
- **Composition counts and shares** of the facility's tanks by: wall (SW/DW), age bin, **install-vintage cohort**, manufacturer/make class, capacity class, fuel.
- **Summary stats:** oldest-tank age, youngest-tank age, age spread, fraction single-walled, number of distinct vintages, number of distinct makes.
- The **vintage / install-cohort** features are the key de-confounders for the DW>SW artifact — they let wall stop proxying install era (young SW = 1980s under-detected vs young DW = 2000s monitored). This is the fix the researcher wants.
- Feed everything into the saturated elastic-net design; regularization handles the width. **Enumerate the exact final feature list in a comment block** in the script.

### 3. CV folds — fix the leakage
Replace `01n`'s random `nfolds` k-fold with **facility-grouped, state-stratified folds** (port the pattern from `01r` STEP 2): assign each *facility* to a fold within its state, so all of a facility's rows share a fold and every state appears in every training fold. Pass as `foldid` to both the alpha-tuning `cv.glmnet` and the full-data `keep=TRUE` `cv.glmnet`. (Today a facility's years can split across folds → mild leakage in the OOS predictions.)

### 4. Uncertainty — DIFFERENT treatment per downstream use
Do **not** compute one interval and reuse it. Do **not** use a 300× refit bootstrap for the cell CIs.
- **§4.1 cell-risk schedule (age × wall) + the SW-vs-DW contrast:** cluster-robust (by `panel_id`) **confidence interval on the predicted cell mean** and on the **SW−DW difference**. Compute via the **analytic infinitesimal jackknife = cluster-robust sandwich + delta method** on the cell-mean predictions (valid because the net sits at λ≈0 ≈ unpenalized logistic MLE — refit an unpenalized `glm`/`fixest::feglm` on the selected design, cluster by facility, delta-method onto the cell predictions). Reference contrast = predict at a fixed reference, vary only wall, delta-method CIs.
- **§4.1 predictive metrics (AUC, PR-AUC, lift, calibration deciles):** facility-**cluster resample** over the **fixed OOS predictions** (no model refit) → percentile CIs on each metric.
- **§4.2 fair premium / cross-subsidy (separate script `07*`):** combine the hazard cell CI with the **severity `S_bar` bootstrap CI** — propagate BOTH into the fair-premium band and the transfer-% numbers. (Severity CI already exists: ~$363k–$392k.)
- **§5 structural primitive:** OUT OF SCOPE here. The structural session bootstraps the whole two-step pipeline (first-stage hazard + second-stage estimator) together, so first-stage uncertainty is carried by that outer bootstrap, not propagated here.

### 5. Outputs — preserve `01n`'s, add CIs
- `Figure_CV_CellRisk` (age × wall) — **add CI bands** from the analytic IJ.
- New: `Table_CV_CellRisk_RefContrast` / figure — SW vs DW by age bin with `est, lo, hi` (the "SW>DW at every age" figure with CIs).
- `analysis_hazard_predictions_full.csv` — OOS per fac-yr, pre+post, unchanged role (DCM downstream).
- ROC / PR / calibration / lift / GoF — attach the metric CIs from the cluster resample.
- `analysis_cv_data_fac_year.rds`, `analysis_cv_data_fac.rds`, `analysis_elnet_model.rds` — keep.

## Downstream uses (context — which figure each feeds)
1. §4.1 SW-vs-DW gradient `fig-cell-risk` + 45° GoF `fig-cell-gof`  → cell-mean CI + contrast CI.
2. §4.1 lift / calibration / AUC                                    → metric CIs over fixed OOS preds.
3. §4.2 fair premium = hazard × severity → cross-subsidy + transfer% → hazard CI ⊕ severity CI.
4. §4.2 actuarial alignment `fig-actuarial-alignment`              → same cell-mean CI as #1.
5. §5 structural hazard primitive                                  → 2-step bootstrap (out of scope).

## Verification gates
- **Calibration:** cell-GoF 45° (model cell mean vs observed cell rate) holds; Platt/calibration check passes.
- **DW>SW fixed:** single-walled ≥ double-walled at the priced age bins once vintage features are in (the artifact was the vintage/detection confound).
- **Age gradient present:** first-leak hazard RISES with age (bathtub) — distinct from the flat recurrent rate; confirms the conditioning recovers wear-out.
- **CI sanity:** analytic cell CIs are reasonable; reference-contrast CIs don't blow up in thin old-DW cells (they're model-based there — note it).
- Sample expansion: confirm control states now contribute post-1999 facility-years; TX capped at <1999.

## Do NOT
- Do not use the recurrent Poisson rate (`01r`) — superseded, archive it.
- Do not turn on GRF.
- Do not use a refit (resampling) bootstrap for the cell CIs — use the analytic IJ / delta method.
- Do not restrict control states to pre-1999.
- Do not build the structural any-tank-leaks decomposition (releases are facility-level; use cell-risk averaging).
