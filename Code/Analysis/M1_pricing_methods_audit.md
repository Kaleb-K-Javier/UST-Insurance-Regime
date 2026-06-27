# M1 Pricing Pipeline — Methods Audit (code ↔ math ↔ explanation)

Purpose: let the researcher verify exactly what each script computes, paired with the
math and a plain-language explanation for the draft. NOT manuscript prose — these are
methods notes for verification; the editor agent turns them into the paper.

Last updated: 2026-06-27 (Opus). Covers commits through `f6e7d6a`.

---

## 0. The object we are building

A **fair (actuarially break-even) premium** for an underground storage tank, equal to
expected fund-payable loss per tank-year:

```
fair_premium(x) = λ(x) · S(x)
                = P(release this year | x) · E[net cleanup cost | a release, x]
```

This is a **two-part model**: a *frequency* model λ (how often) and a *severity* model S
(how much, given it happens). The two are estimated on **different samples** because cost
is only observed when a release occurs:

| part | script | unit / sample | N | outcome |
|---|---|---|---|---|
| frequency λ | `01p_Pricing_Hazard.R` | facility-year, detection era 1990–2016 | ~2.3M | `event_first_leak` (0/1) |
| severity S | `01q_Severity_Model.R` | realized claim (release) | ~7,334 | `net_2023` ($, ≥0) |

Both are predicted onto the same **(age bin × wall) cells** (9 age × 2 wall = 18), per
state and pooled "NATIONAL". `S̄` flat-severity (old) is replaced by `Ŝ(cell)`.

---

## 1. Frequency model — `01p_Pricing_Hazard.R`

**Data:** `Data/Processed/facility_leak_behavior_annual.csv` (facility-year panel).
**Sample (training):** `panel_year ∈ [1990, 2016]` (detection era; pre-1985 recorded ≈0
because detection wasn't mandated, post-2016 right-censored), `has_previous_leak == 0`
(at risk for a *first* release), make-model rows, non-missing covariates.

**Outcome:** `Y = event_first_leak` — facility's first confirmed release in that year.

**Estimand:**
```
λ(x, year_ref) = P(first release in a year | single tank, covariates x, detection regime = year_ref)
```

**Model (code):** unweighted **logistic elastic net** (`glmnet`, `family="binomial"`),
saturated features with a calendar control:
```
~ (age_bin + has_single_walled + active_tanks + total_capacity
   + has_gasoline_year + has_diesel_year)^2  + state_f + year_f
```
- `(...)^2` = all pairwise interactions (incl. age×wall).
- `year_f` (calendar) is the **de-confounder**: recorded-release rate rose ~0→15/1k as
  detection was mandated (1989–93) then declined. Without it, age/wall proxy *when* a tank
  was observed. With it in training, age/wall coefficients are net of the detection trend.
- α, λ chosen by CV on a 200k subsample; **single fit on the full data** at that (α, λ).
- **No class weights, no Platt** (an earlier weighting+Platt step collapsed predictions to
  the base rate; unweighted logistic is self-calibrated on the rare outcome).

**Prediction (the cell schedule):** predict on a grid of (state × age_bin × wall) at a
**fixed reference**: `year_f = 2008` (mature detection), `active_tanks = 1` (single-tank →
a genuine per-tank hazard), `total_capacity = 3000`, gasoline. Holding the year fixed sets
the detection regime; the *cross-sectional* age/wall gradient is what is priced.

**Outputs:**
- `dcm_cell_hazard_pricing.csv` — `state, age_bin, has_single_walled, lambda` (per state + NATIONAL).
- `analysis_hazard_predictions_pricing.csv` — per facility-year fitted λ at observed covariates.
- `analysis_pricing_hazard_model.rds` — fitted model + metadata.

**Plain explanation (draft):** "We estimate the annual probability that a tank has a
confirmed release as a flexible (elastic-net) function of tank age, wall type, facility
size, fuel, and state, controlling for calendar year to absorb the secular rise in release
*detection*. We then read off the implied per-tank hazard for each age×wall cell at a fixed
(modern) detection regime."

**Caveats:**
- First-release hazard in the never-leaked set ⇒ a **lower bound** on the annual release rate.
- Age + period are identified; cohort (= period − age) is *not* separable, so the age
  effect legitimately absorbs construction-vintage quality (fine for pricing).
- Wall: after the calendar control, full-sample shows **SW > DW** in all 9 bins (~3–5%);
  the earlier DW≥SW was a small-sample (smoke) artifact. Residual wall–detection
  confounding (DW interstitial monitoring detects more) likely *attenuates* the SW–DW gap,
  so 3–5% is a floor. Detection-method field exists in raw TX (`DET_*`) but is sparse — a
  parked refinement.

---

## 2. Severity model — `01q_Severity_Model.R`

**Data:** `Data/Processed/incident_level_claims.csv` (one row per realized claim/release).
**Sample:** states with per-incident cost records — **CO, NM, PA, TN** (~7,334 claims).
LA excluded (claim costs are facility totals, not per-release — under review). UT excluded
(no facility universe). Sub-deductible $0 claims kept.

**Outcome:** `net_2023` — fund-payable cleanup cost net of deductible, in 2023 $:
```
net_nom  = (state=="NM") ? max(total_cost − 10000, 0) : total_cost      # NM recorded gross; others already net
net_2023 = net_nom · (total_cost_2023 / total_cost)                     # CPI deflate to 2023
```

**Estimand:** `Ŝ(age_bin, wall, [state]) = E[net cleanup cost | a release occurs]`.

**Model (code):** **Poisson elastic net (PPML, log link)** — `glmnet(family="poisson")` on
cost (scaled to $1k for stability):
```
~ (age_bin + has_single_walled + active_tanks + total_capacity)^2 + state_f
```
- PPML targets **E[cost] directly**, is robust to the heavy right tail, and handles the
  sub-deductible zeros (no log(0) problem, no Duan smearing). Predict `type="response"`.
- α, λ by 10-fold CV; single fit at best (α, λ).
- **No fuel** (not in the claims file) — severity features are age, wall, size, state.

**Prediction:** grid (state × age_bin × wall) at single-tank reference (`active_tanks=1`,
`total_capacity=3000`) → `Ŝ(cell)`. NATIONAL = mean over states.

**Uncertainty:** nonparametric **bootstrap** — resample the 7,334 claims B=800×, refit at
fixed (α, λ), re-predict the grid → 2.5/97.5 percentiles per cell (`sev_lo`, `sev_hi`).

**Outputs:**
- `dcm_cell_severity_pricing.csv` — `state, age_bin, has_single_walled, sev_hat, sev_lo, sev_hi`.
- `analysis_severity_model.rds` — fitted model + metadata.

**Plain explanation (draft):** "Conditional on a release, we model the fund-payable cleanup
cost as a Poisson-pseudo-likelihood (PPML) elastic-net function of the same observables, and
read off expected cost per age×wall cell. PPML is used because cost is a skewed nonnegative
mean with a heavy tail and occasional sub-deductible zeros."

**Caveats:**
- **Small N (7,334)** ⇒ parsimonious + heavy shrinkage; rich interactions not identified.
- Severity is **not flat** (the old single-S̄ assumption missed this): SW ≈ 1.48× DW
  (no secondary containment → bigger spill — a *causal* channel, unlike the hazard's
  detection issue), and state levels differ (NM ≈ half of CO).
- No coverage cap yet: funds pay only up to a per-occurrence limit, so uncapped Ŝ
  **overstates** the fund's expected payout; capping at the real limit (`coverage_per_occ_usd`)
  would lower it ~20–45% (parked refinement). The *headline* (fee ≈ 1% of fair) is robust to it.

---

## 3. Combining into the fair premium

```
TANK fair premium       = λ(cell) · Ŝ(cell)                    # per tank-year
FACILITY fair premium   = Σ_{tanks} λ(cell_t) · Ŝ(cell_t)      # size-consistent: scales with tank count
```
Both use the **NATIONAL** age×wall schedules by default (so LA, which lacks own-state
hazard/severity at the tank level, borrows national). Cells: tank's `tank_age → age_bin`
(`AGE_BIN_BREAKS`) and `mm_wall → has_single_walled`.

**Uniform premium** (the comparator in the cross-subsidy figure):
```
τ = mean fair premium over the units shown    # the revenue-neutral flat charge
```

**Actual fee** (the comparator in the paid-share figure):
```
fee = (tank)  fr_premium_per_tank_yr
    = (facil) active_tanks · fr_premium_per_tank_yr
```

---

## 4. Figures — `07f` (tank) and `07g` (facility)

Both, per state, pooled over years (+ a single-year 2005 cut):
- **Cross-subsidy** (`Fig_*CrossSub_*`): units ranked low→high fair premium; area between
  fair premium and τ. Blue = pays *less* than the uniform premium (subsidizer under a flat
  charge), terracotta = pays *more*. Shows the redistribution a flat fee creates.
- **Paid-share** (`Fig_*PaidShare_*`): same fair-premium curve vs the *actual* fee (a sliver
  on the floor). The gap = the gas-tax-funded shortfall. Shows the *level* problem.
- 07f emits two wall modes (AgeWall / AgeOnly); 07g caps the y-view at the 98th pct
  (a few mega multi-tank facilities have huge summed premiums).

**States:** tank & facility figures are **CO/LA/TN** (NM absent from `panel_dt`).
**CI band:** currently severity-only (flat S̄ bootstrap in 07f/07g as written) — see §6.

---

## 5. Downstream consumers (researcher's map)

1. **Risk figures** — λ from 01p ("predict leak work").
2. **TX premium vs risk** — preferred: fair-premium-vs-real-premium; acceptable for now:
   risk curve over age vs the Mid-Continent schedule (`06_Actuarial_Alignment.R`, to be
   rebuilt with Ŝ instead of the old Duan-smeared cost).
3. **Cross-subsidy** — tank (07f) or facility (07g); choice pending.
4. **DCM** — the cell schedules (λ, Ŝ) feed the structural model's primitives.

---

## 6. Fitted values + SEs — current state and plan

**Goal (researcher):** every unit gets a *predicted fitted value* with an SE/CI — that is
the point of building prediction models.

**Current:** 01q cell schedule has bootstrap CIs; **01p cell schedule has none**; the
figure bands use only the flat-S̄ bootstrap.

**Plan:**
1. Add the **same bootstrap CI to 01p's** cell hazard (resample training years, refit at
   fixed α/λ, re-predict grid → `lambda_lo/hi`).
2. Emit **per-unit fitted values** (tank and facility): fitted `λ`, `Ŝ`, and
   `fair = λ·Ŝ`, each with a CI. Because the hazard sample (facility-years) and severity
   sample (claims) are independent, combine via the **product bootstrap / delta method**:
   `Var(λŜ) ≈ Ŝ²Var(λ) + λ²Var(Ŝ)` (CIs from joint resampling of the two cell bootstraps).
3. Switch the 07f/07g bands to this combined fair-premium CI, and write the per-unit fitted
   table for the DCM.

---

## 7. Open decisions / issues to resolve

- **LA / full sample — RESOLVED 2026-06-27.** Claims file = 6 states. CO/PA/TN/NM/UT are
  per-incident (1.1–1.3 rows/facility, distinct lust_ids). **LA is site-aggregated**:
  *exactly* 1.00 rows/facility (no multi-incident facilities) and median cost ~2× CO →
  one aggregated cleanup per facility, not per-release. Keep LA OUT of per-incident
  severity training (mixes units, biases up); LA figures borrow national Ŝ (already so).
  **UT is per-incident but tiny (88 claims)** — addable for completeness; not a figure
  state. Full usable per-incident severity sample = CO/NM/PA/TN(+UT) ≈ 7,422. Frequency is
  already the full 2.3M panel; severity is small *by construction* (cost observed only on
  the ~7.4k realized releases).
- **Tank vs facility** cross-subsidy as the headline.
- **Coverage cap** on severity (fund liability vs gross expected loss).
- **Wall** in figures: AgeWall vs AgeOnly (now both defensible; ~2% on τ).
- **Detection-method feature** (parked) to fully de-confound the wall hazard.
- **State-specific vs national** severity (NM ≈ half) — national used now; state is richer
  but LA has no claims.
