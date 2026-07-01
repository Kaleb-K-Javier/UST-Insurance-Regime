# Section 3/4 facility results — file paths + data for the editor
# For 02_JMP_Draft_for_render.qmd. This is a DATA LEDGER (paths + raw numbers), not prose.
# Every facility exhibit is the 02l roll-up / 02m summary — the 02j Yhat0-route files are DEAD.
# Verified server runs: 02l 2026-06-28/29, 02m 2026-07-01.

## A. FILE PATHS — fix these in the draft

USE (current):
| exhibit | path |
|---|---|
| §3 summary/balance table (two-panel) | `Output/Tables/T_Facility_Rollup_Summary.tex` (\input) |
|   ↳ machine-readable Panel A / B | `Output/Tables/T_Facility_Rollup_Balance_PanelA.csv` / `..._BaseRates_PanelB.csv` |
| §4 portfolio ATTs (7 margins × 2 FE) | `Output/Tables/T_Facility_Rollup_ATT.csv` |
| §4 stepped any_closure table (per FE) | `Output/Tables/T_DiD_Facility_Stepped_{oldtank_size,full_portfolio}.tex` |
| §4 all-margin pub table (per FE) | `Output/Tables/T_Facility_Rollup_ATT_Pub_{oldtank_size,full_portfolio}.tex` |
| §4 event-study figures | `Output/Figures/Fig_ES_Facility_<margin>_{oldtank_size,full_portfolio}.{pdf,png}` ; margin ∈ {any_closure, downsize, consolidate, any_replace, reconfigure_up} |
| §4 HTE coefficients | `Output/Tables/T_Facility_HTE.csv` |

REMOVE / REPLACE (superseded 02j route — do NOT cite):
`T_Facility_Portfolio_ATT.csv`, `T_Facility_Portfolio_HTE.csv`, `Fig_ES_Facility_Portfolio.*`,
`facility_cellfe_xwalk.csv` (Ŷ⁰/τ). Also dead: the `_A`/`_B`-suffixed and CamelCase
`Fig_ES_Facility_{Consolidate,Downsize,ReconfigureUp,Replace}.*` figures.

FE-version labels for prose: `oldtank_size` = **headline** (oldest-tank cell × size bin); `full_portfolio`
= **robustness** (full tank multiset). Report oldtank_size as the main column.

## B. DATA — §3 summary / balance (T_Facility_Rollup_Summary.tex; report the AFTER-CEM column)

Sample: 117,250 matched facilities (TX 21,438 / control 95,812); 3,443,560 facility-years; 18 states.
CEM pruned only 2.1% of the eligible birth-cohort pool (before 119,824 → after 117,250); balance is
essentially unchanged before→after (matching confirms common support, it does not close large gaps).

Panel A (TX / Control / SMD, after-CEM):
- Tanks per station: 3.42 / 3.47 / −0.02
- Total capacity (000 gal): 26.5 / 55.2 / −0.01  *(outlier-driven; SMD ≈0 is not meaningful — prefer MEDIAN or winsorize)*
- Mean tank age at reform (yrs): 16.5 / 18.1 / **−0.19**  *(TX ~1.6 yrs younger — the one size/vintage imbalance; CEM does not fix it, the cell×year FE conditions on it)*
- Single-walled share: 0.90 / 0.91 / −0.04
- Pre-1989 vintage share: 0.78 / 0.74 / +0.09
- Motor-fuel share (ever sold gasoline): 0.74 / 0.80 / −0.14
- Gas competitors within 1 mi (mean): 19.0 / 12.5 / **+0.39**  *(MEDIAN 14 / 8 — largest imbalance: TX stations are in denser retail markets; unaffected by CEM)*

Honest framing: the birth-cohort + active-at-1998 restriction already yields good balance on size/wall/vintage
(|SMD|<0.1); the exceptions are tank age (−0.19), motor-fuel share (−0.14), and — the biggest — local gas
competition (+0.39). Identification rests on parallel trends + the composition×year FE, not on raw balance.

## C. DATA — §4 pre-reform base rates (Panel B; control_pre = the denominator each ATT is quoted against)

| margin | control_pre | note for prose |
|---|---|---|
| any_closure | **0.0239** | "+1.66 pp on a 2.4% control base" (≈ +70%) |
| facility_exit | 0.0000 | base is 0 by construction (active-1998 incumbents) — quote against control_POST = **0.0171** |
| downsize | 0.0129 | |
| consolidate | 0.0032 | |
| reconfigure_up | 0.0023 | |
| any_replace | 0.0115 | |
| cap_decrease | 0.0168 | |
(control pre-reform facility-years N = 1,188,935. For exact raw-DiD use the CSV — the .tex raw_did column is rounded to 2 dp.)

## D. DATA — §4 portfolio ATTs (T_Facility_Rollup_ATT.csv), oldtank_size / full_portfolio, pp

| margin | oldtank_size (headline) | full_portfolio (robust) |
|---|---|---|
| any_closure | +1.66*** | +2.92*** |
| facility_exit | +1.63** | +2.50** |
| cap_decrease | +1.64*** | +2.72*** |
| downsize | +0.06 (n.s.) | +0.31*** |
| reconfigure_up | +0.01 (n.s.) | +0.19*** |
| any_replace | +0.12 (n.s.) | +0.42*** |
| consolidate | −0.06** | −0.05** |
Size-transition margins wash out under oldtank_size BY CONSTRUCTION (size is in the FE); they load under
full_portfolio. Inference: analytic cluster-robust SE by state (G=18) — PROVISIONAL (single treated cluster
over-rejects; wild-cluster bootstrap OOMs, deferred).

## E. §4 HTE (T_Facility_HTE.csv, 100 rows; fe_version = oldtank_size)

Dimensions: cap_G, vintage, has_gasoline, rural, low_pop, low_income, high_pov, **thin_market**.
Sorting story (levels): small (<9k gal) / old (pre-1975) / gasoline stations EXIT; large / newer stations
DOWNSIZE + REPLACE.

**Thin-market (local gasoline competition).** `thin_market = 1{≤1 gasoline competitor within 1 mile}` (n_gas_1609m
from Code/GIS/03_gas_competitors.R) — **17.4%** of the sample. Interaction-only; reference = competitive markets
(thin_market=0). Values from T_Facility_HTE.csv rows where dimension="thin_market":

| margin | competitive (thin=0) | differential in thin mkts (thin=1) | implied thin effect |
|---|---|---|---|
| any_closure | +1.82 pp*** | **−1.52 pp*** | +0.31 pp |
| facility_exit | +1.79 pp*** | **−1.51 pp*** | +0.28 pp |
| downsize | +0.08 pp (n.s.) | −0.18 pp*** | −0.11 pp |
| consolidate | −0.07 pp** | +0.07 pp*** | ≈0 |
| any_replace | +0.14 pp (n.s.) | −0.15 pp*** | ≈0 |

Reading: the reform's closure/exit effect is concentrated in competitive markets; captive (thin-market) stations
barely respond (~+0.3 pp vs ~+1.8 pp), a −1.5 pp differential on both closure and exit — consistent with the
rural result (captive demand → stay open). Caveats: `thin_market` uses "ever-gasoline" neighbors (biography), a
slightly different fuel definition than the has_gasoline HTE dim; n_gas is mildly undercounted (~86% edge-endpoint
mapping); analytic cluster-robust SE with a single treated cluster (TX) → provisional, WCB deferred.
