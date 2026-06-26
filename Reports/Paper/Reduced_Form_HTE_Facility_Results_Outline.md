# Reduced-Form Analysis — Results Outline (specifications + code)

**Scope.** The 1998 Texas risk-based-pricing reform's effect on UST closure, on the
active-at-treatment, birth-CEM-matched sample. Tank-level results are the headline;
the facility-level block carries the *same* identification up to the portfolio.

**Inference (current).** Analytic **cluster-robust SEs by state** (G = 18). The wild
cluster bootstrap (proper one-treated-cluster inference) and HonestDiD sensitivity
are **deferred** for speed — re-enable for final tables.

## 0. Notation

| symbol | meaning |
|---|---|
| $i,f,t$ | tank, facility, calendar year |
| $c$ | cell = `make_model_noage` × install cohort (wall×fuel×capacity×cohort) |
| $k = t-1998$ | event time (`rel_year`); reform = Dec 22 1998 |
| $\text{TX}_f$ | `texas_treated`; $\text{Post}_t=\mathbf 1\{t\ge1999\}$; $\text{did}=\text{TX}\cdot\text{Post}$ |
| $\alpha_f$ | facility FE (`panel_id`) |
| $\delta_{ct}$ | cell×year FE (`cell_vintage_year_fe`) |
| $M_{it}$ | RCRA mandate dummies (release-det / spill-overfill / integrity) |
| $Z_f$ | facility attribute (gas-station, rural, low-population = bottom pop. quartile) |
| $s_{fct}=n_{fct}/N_{ft}$ | facility $f$'s share of active tanks in cell $c$ at $t$ |

---

## 1. Tank-level static DiD — headline ATT

$$
\text{closure}_{it}=\beta\,\text{did}_{it}+\gamma'M_{it}+\alpha_f+\delta_{ct}+\varepsilon_{it}
$$

$\beta$ = average effect of the reform on a tank's annual closure probability. Identified
from TX-vs-control tanks within the same make-model-cohort×year cell.

```r
# Code/Analysis/02c_Stepped_DiD.R (headline) ; 02b_tank_closure_analysis.R:3138
feols(closure_event ~ did_term +
        mandate_release_det + mandate_spill_overfill + mandate_integrity |
        panel_id + cell_vintage_year_fe,
      data = data_active, cluster = ~state)
```

---

## 2. Tank-level event study

$$
\text{closure}_{it}=\sum_{k\ne-1}\beta_k\,\mathbf 1\{k_{it}=k\}\cdot\text{TX}_f+\gamma'M_{it}+\alpha_f+\delta_{ct}+\varepsilon_{it}
$$

$\beta_k$ = differential closure of TX vs control at event time $k$, relative to $k=-1$.
Pre-period $\beta_{k<0}\approx0$ is the parallel-trends test; $\beta_{k\ge0}$ is the path.

```r
# Code/Analysis/02h_HTE_EventStudy_FirstPass.R  (pooled; slide-style figure)
feols(closure_event ~ i(rel_year, texas_treated, ref = -1L) +
        mandate_release_det + mandate_spill_overfill + mandate_integrity |
        panel_id + cell_vintage_year_fe,
      data = data_active, cluster = ~state)
# -> Fig_ES_HTE_Pooled (blue pre / grey event / red post, hollow circles)
```

---

## 3. Tank-level HTE — triple-difference (interaction, NOT split-sample)

$$
\text{closure}_{it}=\beta\,\text{did}_{it}+\beta_Z\,(\text{did}_{it}\cdot Z_f)+\gamma'M_{it}+\alpha_f+\delta_{ct}+\varepsilon_{it}
$$

$Z_f$ absorbed by $\alpha_f$. $\beta$ = effect for $Z=0$; $\beta+\beta_Z$ = effect for $Z=1$;
$\beta_Z$ = the differential (the HTE). Common FEs — one shared cell×year baseline.
$Z\in\{$ gas-station, rural (RUCA≥7), low population (bottom pop. quartile vs upper three) $\}$. (Competition parked;
GIS neighbor counts not trustworthy yet.)

```r
# Code/Analysis/02g_HTE_GIS_FirstPass.R
d[, did_Z := did_term * Z]
feols(closure_event ~ did_term + did_Z +
        mandate_release_det + mandate_spill_overfill + mandate_integrity |
        panel_id + cell_vintage_year_fe,
      data = d, cluster = ~state)   # report did_term and did_Z
```

---

## 4. Carrying the cell FE up to the facility ($\hat Y^0$ crosswalk)

The tank cell×year FE becomes, at the facility, the **composition-weighted baseline**:

$$
\hat\delta_{ct}=\mathbb E[\text{closure}\mid c,t,\ \text{untreated}],\qquad
\hat Y^0_{ft}=\sum_c s_{fct}\,\hat\delta_{ct},\qquad
\tau_{ft}=\overline{\text{closure}}_{ft}-\hat Y^0_{ft}
$$

$\hat\delta_{ct}$ uses **untreated only** (controls all years + TX pre-1999) — no treatment
variation. $\hat Y^0_{ft}$ is the predicted no-reform closure share given the facility's tank
mix; $\tau_{ft}$ is the imputation residual. Left-joined to the facility panel on
`(panel_id, panel_year)`.

```r
# Code/Analysis/02k_Facility_CellFE_Crosswalk.R
unt <- tk[texas_treated == 0L | panel_year < 1999L]          # untreated
b1 <- unt[, .(b1=mean(closure_event), n1=.N), by=.(make_model_noage, install_yr_int, panel_year)]
# (+ coarser fallbacks b2, b3 for sparse cells) -> tank_base = fcoalesce(...)
xw <- tk[, .(closure_share_tank = mean(closure_event),
             Yhat0 = mean(tank_base)), by = .(panel_id, panel_year)]
xw[, tau := closure_share_tank - Yhat0]    # -> facility_cellfe_xwalk.csv
```

---

## 5. Facility-level causal portfolio ATT

**Identifying assumption:** parallel trends conditional on tank mix (same as tank level,
aggregated). Outcomes $Y_{ft}\in\{$ `closure_share`, `any_closure`, `facility_exit`,
`perm_share`, `repl_share` $\}$.

**Route A — direct TWFE (cell FE as a control):**
$$
Y_{ft}=\beta\,\text{did}_{ft}+\mu\,\hat Y^0_{ft}+\gamma'M_{ft}+\alpha_f+\lambda_t+\varepsilon_{ft}
$$

**Route B — imputation / predicted-rate:**
$$
\tau_{ft}=\beta\,\text{did}_{ft}+\alpha_f+\lambda_t+\varepsilon_{ft}
$$
(control $\tau\approx0$ by construction, so $\beta$ is the TX-post jump = ATT.)

```r
# Code/Analysis/02j_Facility_Portfolio_DiD.R   (RHS = "+ Yhat0 + <mandates>")
# Route A:
feols(closure_share ~ did_term + Yhat0 + any_mandate_release_det +
        any_mandate_spill_overfill + any_mandate_integrity |
        panel_id + panel_year, data = fy, cluster = ~state)
# Route B:
feols(tau ~ did_term | panel_id + panel_year, data = fy, cluster = ~state)
```

---

## 6. Facility-level event study

$$
Y_{ft}=\sum_{k\ne-1}\beta_k\,\mathbf 1\{k=k_{ft}\}\cdot\text{TX}_f+\mu\,\hat Y^0_{ft}+\gamma'M_{ft}+\alpha_f+\lambda_t+\varepsilon_{ft}
$$

Same object as the tank ES, at the facility unit, with the exact cell baseline as control.

```r
# Code/Analysis/02j_Facility_Portfolio_DiD.R  (slide-style figure)
feols(closure_share ~ i(rel_year, texas_treated, ref = -1L) + Yhat0 + <mandates> |
        panel_id + panel_year, data = fy, cluster = ~state)
# -> Fig_ES_Facility_Portfolio
```

---

## 7. Facility-level HTE (interaction)

$$
Y_{ft}=\beta\,\text{did}_{ft}+\beta_Z(\text{did}_{ft}\cdot Z_f)+\mu\,\hat Y^0_{ft}+\gamma'M_{ft}+\alpha_f+\lambda_t+\varepsilon_{ft}
$$

$Z\in\{$ gas-station (`has_gasoline`), low population (bottom pop. quartile), rural $\}$. $\beta_Z$ = differential
portfolio response.

```r
# Code/Analysis/02j_Facility_Portfolio_DiD.R
d[, did_Z := did_term * Z]
feols(closure_share ~ did_term + did_Z + Yhat0 + <mandates> |
        panel_id + panel_year, data = d, cluster = ~state)
```

---

## 8. Outputs

| file | what |
|---|---|
| `Output/Tables/T_Stepped_DiD_OLS.tex` | tank static ATT (headline) |
| `Output/Figures/Fig_ES_HTE_Pooled.*` | tank event study |
| `Output/Tables/T_HTE_GIS_FirstPass.csv` | tank HTE (interaction) |
| `Data/Analysis/facility_cellfe_xwalk.csv` | $\hat Y^0_{ft}$ / $\tau_{ft}$ crosswalk |
| `Output/Tables/T_Facility_Portfolio_ATT.csv` | facility ATT, routes A & B, all outcomes |
| `Output/Tables/T_Facility_Portfolio_HTE.csv` | facility HTE (interaction) |
| `Output/Figures/Fig_ES_Facility_Portfolio.*` | facility event study |

**Deferred:** wild cluster bootstrap (final inference), HonestDiD sensitivity, the
route-B 45° predicted-rate figure, and an interaction event study for group dynamics.
