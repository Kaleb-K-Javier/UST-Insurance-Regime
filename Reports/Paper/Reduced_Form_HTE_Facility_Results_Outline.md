# Reduced-Form Analysis — Results Outline (specifications + code)

**Scope.** The 1998 Texas risk-based-pricing reform's effect on UST closure, on the
active-at-treatment, birth-CEM-matched sample. Tank-level results are the headline;
the facility-level block carries the *same* identification up to the portfolio.

**Inference (current).** Analytic **cluster-robust SEs by state** (G = 18). The wild
cluster bootstrap (proper one-treated-cluster inference) and HonestDiD sensitivity
are **deferred** for speed — re-enable for final tables.

**Run.** All four scripts run on the server (ucbare2) off the in-repo panels — no Z paths.
Sample (headline birth-CEM panel): **9,761,982 tank-years | 117,250 facilities | 18 states.**
The RCRA mandate dummies are absorbed by the cell×year FE (fixest drops them for
collinearity) — they do not move the estimates. Rural / low-population HTE skip on the
server (GIS lookups are local-only); run those locally or copy the lookups over.

> **Facility block updated 2026-07-01 (Ticket 038).** The facility-level analysis was rebuilt as the
> unweighted **02l roll-up** (`Code/Analysis/02l_Facility_Rollup_Mirror.R`) with two FE versions —
> `oldtank_size` (headline) and `full_portfolio` (robust) — plus the two-panel Section-3 summary/balance
> table (`Code/Analysis/02m_Facility_Rollup_Baseline.R`). This **supersedes** the 02j $\hat Y^0$-route
> facility results described in §§4–7 and their `T_Facility_Portfolio_*` files. §8 lists the current output
> files and where they live (pulled to the local repo `Output/` on 2026-07-01). The 02j-route numbers still
> quoted in the narrative below are stale and should be refreshed from the new `T_Facility_Rollup_*` tables.

---

## Results (verified on server 2026-06-26 — cluster-robust by state)

**Tank-level HTE (verified, 02g):**

| cut | baseline ATT ($Z=0$) | differential $\beta_Z$ | $p$ |
|---|---|---|---|
| Gas station (any gasoline tank) | $+0.0197$ (non-gas) | $-0.0051$ | 0.22 |
| Pure gasoline (all tanks gasoline) | $+0.0068$ (mixed) | $\mathbf{+0.0431}$ | $<0.001$ |

The closure response is concentrated in **pure-gasoline facilities** — they close about
**+4.3 pp more** than mixed-fuel facilities (pure-gasoline total ≈ +5.0 pp). "Any gasoline"
(84% of facilities) does **not** separate the response. That is the gas-vs-non-gas result:
the bite is on the cleanest motor-fuel-retail / steepest-premium-gradient cell, not on
gasoline presence per se.

**Facility-level causal portfolio (02j) — verified.** Sample: **3.44M facility-years, 18
states** (active-at-treatment via the $\hat Y^0$ join). Facility + portfolio-mix×year FE
(`make_model_fac × year`) + $\hat Y^0$ control, cluster by state, no bootstrap.

*Portfolio ATTs (all positive, significant):*

| margin | ATT | note |
|---|---|---|
| closure share | $+0.0162$ | Route A (mix×year FE). Route B imputation $\tau$ = $+0.0118$ — footnote the gap. |
| facility exit | $+0.0125$ | |
| downsize (fewer tanks **and** gallons) | $+0.0058$* | *provisional* — being repartitioned (Ticket 031) |
| consolidate (fewer tanks, ~same gallons) | *pending re-run* | new margin: viable/modernization move |
| reconfigure-up (fewer tanks, +gallons) | *pending re-run* | rare upgrade flag |
| replacement (rare, 1-for-1) | $+0.0029$ | measured by `repl_share` |
| capacity cut | $+0.0148$ | |

\* The old `downsize` flag was tank-count-only and lumped together capacity-preserving
*consolidation* with genuine *contraction*. Ticket 031 splits it by net gallons (relative-5%
band): **downsize** = fewer tanks **and** fewer gallons; **consolidate** = fewer tanks, ~same
gallons; **reconfigure-up** = fewer tanks, more gallons. The $+0.0058$ here is the old
combined number — it will be replaced by separate consolidate / downsize ATTs after the 02j
re-run. The DCM scoping shows consolidate and downsize are the two dominant heterogeneous
actions; true 1-for-1 replacement is rare.

**The sorting story (the payoff).** The reform pushed **marginal** facilities to **exit**
and **viable** ones to **downsize / replace**:

- **Size (DCM total-capacity bins, monotone):** G1 (<9k gal) **exit +6.6 pp / closure +6.3 pp**;
  large bins ≈ 0 exit but **downsize** (G4 +1.2 pp) and **replace** (G4 +0.8 pp). Small exit,
  large reinvest.
- **Vintage (ref 1989–98):** pre-1975 **exit +2.5 pp**, no downsize/replace; 1989–98 **reinvest**
  (downsize +1.7, replace +0.7), ≈ 0 exit. Old exit, young reinvest.
- **Fuel ($did\times$gas):** gas stations drive every margin — closure +0.018, exit +0.007,
  downsize +0.020, replace +0.009. The response is entirely in gas stations.
- **Spatial (Census-2000, fixed at treatment):** rural & thin-market facilities **exit less**
  (rural −0.42 pp, thin −1.6 pp on exit) = captive demand, stay open; low-income & high-poverty
  facilities **exit more but downsize/replace less** = permanent exit, no reinvestment.

*Numbers above fill from* `T_Facility_Portfolio_ATT_Pub.tex`, the `T_Facility_HTE_*_Pub.tex`
*set, and `T_Facility_SizeHTE_Pub.tex` / `T_Facility_VintageHTE_Pub.tex`. The long-format size
and vintage CSVs (`T_Facility_SizeHTE_byMargin.csv`, `T_Facility_VintageHTE_byMargin.csv`) are
being added to 02j (Ticket 031) so every size/vintage × margin coefficient is machine-readable
alongside `T_Facility_HTE_byMargin.csv`.*

**Event studies.**

- **Closure ES (causal):** flat pre-period, sharp break at the reform — clean parallel trends.
  This is the design's identification check. `Fig_ES_Facility_Portfolio`.
- **Replacement ES (causal):** moderate; pre-period acceptable. `Fig_ES_Facility_Replace`.
- **Downsize ES (descriptive):** pre-trend is elevated (≈ post), so parallel trends is **not**
  credible for this margin. The downsize *ATT* stays in the portfolio table (it shares closure's
  static-DiD identification), but the downsize *figure* is presented as descriptive/suggestive
  only, with an explicit pre-trend caveat. `Fig_ES_Facility_Downsize`.
- Pooled tank ES: `Fig_ES_HTE_Pooled` (headline slide style).

**Caveat.** Single treated state → cluster-robust SEs are the fast first pass. The
pure-gasoline tank result and the size/vintage sorting gradients are large; any borderline
coefficient needs the **wild cluster bootstrap** (deferred) before it goes in a final table.

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

FE is the **portfolio-mix × year** fixed effect `cell_fac_year = make_model_fac × panel_year`
(47 mixes × years) — the facility twin of the tank `cell_vintage_year_fe` — *plus* `Yhat0` kept
as the composition-weighted baseline control. So $\lambda_t$ above is really $\delta^{\text{fac}}_{m(f),t}$
(mix × year).

```r
# Code/Analysis/02j_Facility_Portfolio_DiD.R   (RHS = "+ Yhat0 + <mandates>")
# Route A — facility mix x year FE + Yhat0 control:
feols(closure_share ~ did_term + Yhat0 + <mandates> | panel_id + cell_fac_year,
      data = fy, cluster = ~state)
# Route B — imputation (tau already nets the composition baseline):
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

## 8. Outputs — where the current result files live

All paths repo-relative. Every file is generated on the server (ucbare2) and pulled to the local repo
`Output/` for inspection (facility roll-up + figures pulled 2026-07-01). The facility block below is the
**02l roll-up** and **supersedes** the old 02j $\hat Y^0$-route files.

**Tank-level (headline, still current):**

| file | what |
|---|---|
| `Output/Tables/T_Stepped_DiD_OLS.tex` | tank static ATT (headline) |
| `Output/Figures/Fig_ES_HTE_Pooled.*` | tank event study |
| `Output/Tables/T_HTE_GIS_FirstPass.csv` | tank HTE (interaction) |

**Facility roll-up — current** (`Code/Analysis/02l_Facility_Rollup_Mirror.R`; FE versions
`oldtank_size` = headline, `full_portfolio` = robust):

| file | what |
|---|---|
| `Output/Tables/T_Facility_Rollup_ATT.csv` | facility ATT, 7 margins × 2 FE (14 rows) |
| `Output/Tables/T_Facility_Rollup_ES_Coefs.csv` | facility event-study coefficients (360 rows) |
| `Output/Tables/T_Facility_HTE.csv` | facility HTE, 7 dimensions (90 rows) |
| `Output/Tables/T_Facility_Bootstrap_SEs.csv` | WCB SEs — placeholder, `wcb_*` all NA (boottest OOM; analytic SEs stand) |
| `Output/Tables/T_DiD_Facility_Stepped_{oldtank_size,full_portfolio}.tex` | any_closure stepped DiD, per FE |
| `Output/Tables/T_Facility_Rollup_ATT_Pub_{oldtank_size,full_portfolio}.tex` | all-margin ATT, per FE |
| `Output/Figures/Fig_ES_Facility_<margin>_{oldtank_size,full_portfolio}.{pdf,png}` | ES figures; margin ∈ {any_closure, downsize, consolidate, any_replace, reconfigure_up} |

**Facility Section-3 summary / balance** (`Code/Analysis/02m_Facility_Rollup_Baseline.R`):

| file | what |
|---|---|
| `Output/Tables/T_Facility_Rollup_Balance_PanelA.csv` | Panel A: TX-vs-control balance, before/after CEM, SMDs |
| `Output/Tables/T_Facility_Rollup_BaseRates_PanelB.csv` | Panel B: pre-reform outcome base rates (Section-4 denominators) |
| `Output/Tables/T_Facility_Rollup_Summary.tex` | combined two-panel booktabs table |

**Superseded (02j $\hat Y^0$ route — kept for reference, NOT the current facility results):**
`Output/Tables/T_Facility_Portfolio_ATT.csv`, `T_Facility_Portfolio_HTE.csv`,
`Output/Figures/Fig_ES_Facility_Portfolio.*`, `Data/Analysis/facility_cellfe_xwalk.csv`
($\hat Y^0/\tau$). Also stale: the pre-rename `_A`/`_B`-suffixed 02l files/figures (now `oldtank_size` /
`full_portfolio`) and the CamelCase `Fig_ES_Facility_{Consolidate,Downsize,ReconfigureUp,Replace}.*`.

**Deferred:** wild cluster bootstrap (final inference — currently OOMs on this FE design), HonestDiD
sensitivity, the route-B 45° predicted-rate figure.
