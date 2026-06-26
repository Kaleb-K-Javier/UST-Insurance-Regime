# Recipe — How the fixed effects are built and what the regressions run
# (Reduced-form facility/tank DiD. For the editor agent. Self-contained.)

**Read this before writing the methods/results prose.** The fixed-effects design here is
**not** a textbook two-way (unit + calendar-year) setup. It uses a *cell × year* fixed effect
built from the portfolio composition, plus a separate composition baseline carried as a
control. Getting the wording right matters — the identification claim depends on it. Every
construct below names the exact variable and the file/line where it is built, so you can
verify rather than trust.

---

## 0. One-paragraph summary

We estimate the effect of Texas's **22 December 1998** switch to risk-based UST insurance
pricing on what facilities do to their tanks. Treated = Texas; controls = 17 other states.
The sample is **incumbents active at the reform**, matched on birth characteristics
(coarsened exact matching, "birth-CEM"), and **followed forward** so exit is measured, not
selected out. Identification compares a treated and a control unit **of the same portfolio
type, in the same calendar year**. That "same portfolio type × year" comparison is what the
non-standard fixed effect encodes.

---

## 1. Ingredients — the panels the FEs are built from

| panel | file | unit | built by |
|---|---|---|---|
| Tank panel | `Data/Analysis/matched_tanks_birth_cem.csv` | tank × year | `Code/Analysis/02b_Tank_level_Panel_Build.R` |
| Facility panel | `Data/Analysis/matched_facs_birth_cem.csv` | facility × year | `Code/Analysis/02b_Tank_level_Panel_Build.R` |
| Baseline crosswalk | `Data/Analysis/facility_cellfe_xwalk.csv` | facility × year | `Code/Analysis/02k_Facility_CellFE_Crosswalk.R` |

Tank regressions run on the tank panel (`02g`, `02h`, headline `02c`); facility regressions
run on the facility panel (`02j`), with the crosswalk left-joined for the baseline control.

Treatment timing, common to both panels:
- `post = 1{panel_year >= 1999}`  ·  `texas_treated = 1{state == TX}`  ·  `did = texas_treated × post`
- `rel_year = panel_year - 1998` (event time; reform year 1998 → `rel_year = 0` is 1998's
  measured outcome; the design uses `ref = -1`, i.e. 1997, as the omitted base period).

---

## 2. The "cell" — the heart of the design

A **cell** is a group of like tanks/facilities. The fixed effect is *cell × calendar year*,
so each cell gets its own time path and treated-vs-control is compared **within** it.

### 2a. Tank cell (fine)
`make_model_noage = paste(mm_wall, mm_fuel, mm_capacity)`  — wall material × fuel type ×
capacity class, *no age*  (`02b:316`). The tank cell × year FE is
`cell_vintage_year_fe = make_model_noage × install_cohort × panel_year`. So a tank is compared
to other tanks of the **same wall/fuel/capacity and install cohort, in the same year**.

### 2b. Facility cell (coarse, by design)
`make_model_fac = paste(fac_wall_reform, fac_fuel_reform, oldest_age_bin)`  (`02b:1315`),
**measured at the reform and held fixed**. Its three parts:
- `fac_wall_reform` — the facility's wall-material **mix** at reform (e.g. all single-wall,
  majority single-wall, …).
- `fac_fuel_reform` — the facility's fuel **mix** at reform (gasoline / mixed / non-gas).
- `oldest_age_bin` — age band of the facility's **oldest** tank at reform, 6 bands
  (0–4 / 5–9 / 10–14 / 15–19 / 20–24 / 25+ yr)  (`02b:1300-1307`).

This yields **≈47 distinct portfolio-mix signatures** (per the build). The facility cell ×
year FE is
`cell_fac_year = .GRP by (make_model_fac, panel_year)`  (`02j:90`).

**Critical point for the prose — the facility cell is *coarse on purpose*.** It captures the
facility's *type* (wall-mix | fuel-mix | age profile), **not** the exact multiset of its tank
make-model cells. The fine composition is handled separately (§3). The reason: the exact
facility composition is nearly unique per facility, so using it as an FE would be collinear
with the facility fixed effect and absorb all the treatment variation. Coarsening to ~47
types keeps the cell × year FE meaningful (many facilities share each signature) while the
fine detail enters as a control.

---

## 3. `Yhat0` — the fine composition, carried as a *control* (not an FE)

Built in `02k`. For **untreated** observations only (all control-state years + Texas
*pre-1999*), compute the cell-level closure rate
`δ_ct = mean(closure_event)` by `(make_model_noage, install_cohort, panel_year)`, with coarser
fallbacks for sparse cells. Then for each facility-year, weight those cell rates by the
facility's tank mix:

  **Yhat0_ft = Σ_c s_fct · δ_ct** — the closure share you'd predict for this facility's tank
  mix *if the reform had not happened* (composition-weighted untreated baseline).

  **τ_ft = (observed facility closure share) − Yhat0_ft** — the imputation residual.

`Yhat0` enters the facility regressions as a right-hand-side **control**; `τ` is the outcome
in the imputation route (§5, Route B). Untreated-only construction means `Yhat0` carries **no
treatment variation** — it is a clean composition adjustment.

---

## 4. The fixed-effects design — what is absorbed, and what "identified" means

Facility regressions use, in every block (`02j:148`):

  **FE = `panel_id` (facility) + `cell_fac_year` (portfolio-mix × year)**
  **control = `+ Yhat0 + mandates`**  (`02j:147`)

What each piece does, in words:
- `panel_id` absorbs every fixed facility difference (location, operator, baseline propensity
  to close).
- `cell_fac_year` absorbs **a separate time shock for each portfolio type** — e.g. "old,
  majority-single-wall gasoline facilities everywhere moved together in 2003." This is the
  non-standard part: the calendar-year effect is **allowed to differ by portfolio type**,
  instead of one common year effect for all.
- `Yhat0` nets out the facility's fine tank-mix composition (the detail the coarse cell omits).
- `mandates` = the 1988 RCRA upgrade dummies (`any_mandate_release_det`,
  `any_mandate_spill_overfill`, `any_mandate_integrity`). They are common to TX and controls
  (same federal deadline) and are typically **dropped by `fixest` for collinearity** with the
  cell × year FE — include them, but they do not move the estimates.

**Identifying comparison (say it this way):** two facilities of the **same portfolio-mix type,
in the same calendar year**, one in Texas (treated) and one in a control state. The same
mix × year cell contains many facilities across states, so `cell_fac_year` is *not* collinear
with the facility FE. Parallel-trends is therefore conditional on portfolio mix.

The **tank** regressions are the exact analogue: FE = `panel_id` (tank) + `cell_vintage_year_fe`
(tank cell × year), so a treated tank is compared to control tanks of the same
wall/fuel/capacity/cohort in the same year.

---

## 5. The regressions — exactly what runs

Notation: `Y_ft` = a facility-year outcome (§6); `Z_f` = a facility attribute; `did = TX × post`.
SE clustered by **state** (18 clusters) throughout. `feols` (package `fixest`).

**(A) Static ATT — headline (Route A, direct two-way-FE):**
```
Y_ft ~ did + Yhat0 + mandates | panel_id + cell_fac_year
```
β on `did` = average effect of the reform on that portfolio action, within mix × year.

**(B) Imputation / predicted-rate — robustness (Route B):**
```
tau_ft ~ did | panel_id + panel_year
```
`τ` already nets the composition baseline, so this route uses a **plain** facility + calendar-
year FE (not the mix × year cell). Control units have `τ ≈ 0` by construction, so β is the
Texas-post jump = ATT. Report A and B side by side; if they differ, the mix × year FE is what
moved A (footnote it).

**(C) Event study (causal path / parallel-trends check):**
```
Y_ft ~ i(rel_year, texas_treated, ref = -1) + Yhat0 + mandates | panel_id + cell_fac_year
```
β_k = treated-vs-control gap at event time k, relative to k = −1 (1997). Flat β_{k<0} = clean
pre-trend; β_{k≥0} = the post path. Same FE as the static ATT (so the ES is on the mix × year
design, *not* plain year).

**(D) Heterogeneity — interaction, NOT split-sample (triple-difference):**
```
Y_ft ~ did + did_Z + Yhat0 + mandates | panel_id + cell_fac_year      (did_Z = did × Z_f)
```
β on `did` = effect for the Z = 0 group; β on `did_Z` = the differential for Z = 1 (the HTE).
Common FEs — one shared cell × year baseline for both groups. (Split-sample is deliberately
avoided: it would re-estimate the FEs separately per group.) Z ∈ {gas-station, rural, low
population, low income, high poverty, thin market}.

**(E) Size / vintage heterogeneity — categorical interaction:**
```
Y_ft ~ i(cap_G, did, ref = G1) + did + Yhat0 + mandates | panel_id + cell_fac_year
```
`cap_G` = DCM total-capacity bin (G1 < 9k / G2 9–20k / G3 20–30k / G4 30k+ gallons). The
`did` coefficient = baseline ATT at the reference bin (G1); each `i(cap_G, did)::Gx` coefficient
= the differential for bin Gx. Vintage version replaces `cap_G` with the cohort factor
(reference = 1989–98). Machine-readable coefficients land in
`T_Facility_SizeHTE_byMargin.csv` / `T_Facility_VintageHTE_byMargin.csv`.

---

## 6. The outcomes — the facility portfolio margins

All are facility-year outcomes built in `02j` (lines ~83–87, plus the Ticket-031 repartition):
- `closure_share` = n_closures / n_tanks_active (capped at 1)
- `any_closure`, `facility_exit`
- **`downsize`** = fewer tanks **and** fewer gallons (genuine contraction)
- **`consolidate`** = fewer tanks, **~same** gallons (within a 5% relative band) — a viability /
  modernization move
- **`reconfigure_up`** = fewer tanks but **more** gallons (rare upgrade)
- `repl_share` = replacement closures / tanks (true 1-for-1 swap; **rare**)
- `cap_decrease` = any capacity drop (boolean)

**Why the downsize split matters for the write-up.** The portfolio response is a *sorting*
story: the reform pushed **marginal** facilities to **exit** and **viable** ones to
**downsize / consolidate**. `downsize` and `consolidate` are the two dominant heterogeneous
contraction actions (per the DCM scoping); true 1-for-1 replacement is uncommon. The split is
by **net gallons** so "shed capacity" (downsize) is separated from "same capacity, fewer
tanks" (consolidate). Each margin runs through (A)–(E) above on the same FE design.

---

## 7. Sample and inference (state these plainly)

- **Sample:** incumbents **active at the reform** (operating 22 Dec 1998, installed < 1999),
  **birth-CEM matched**, followed forward (exit measured, not selected). Facility birth-CEM
  match: birth-cohort × total-tanks-bin × single-wall-share-bin. Tank birth-CEM match:
  make-model (wall+fuel+capacity) × install cohort. No cohort dropped; the 1988 federal
  upgrade mandate is common to TX + controls and differenced out.
- **Headline counts:** tank panel ≈ 9.76M tank-years / 117,250 facilities / 18 states;
  facility panel ≈ 3.44M facility-years / 18 states.
- **Inference:** analytic **cluster-robust SEs by state** (G = 18) — the fast first pass.
  Texas is the *single* treated cluster, so the **wild cluster bootstrap** (proper one-treated-
  cluster inference) and **HonestDiD** sensitivity are **deferred** — re-enable for final
  tables. Flag any borderline coefficient as needing the bootstrap before it goes in a final
  table.

---

## 8. Wording cheat-sheet — the non-standard bits to get right (and pitfalls)

**Do say:**
- "facility and **portfolio-mix × year** fixed effects" (not just "two-way fixed effects").
- "identified from treated-vs-control facilities **of the same portfolio type, in the same
  year**."
- "the facility's fine tank composition enters as a **control** (`Yhat0`), a composition-
  weighted untreated baseline, because using it as a fixed effect would be collinear with the
  facility effect."
- "parallel trends **conditional on portfolio mix**."
- "the event study uses the **same** mix × year design as the static estimate."

**Don't say / avoid these mistakes:**
- Don't call `cell_fac_year` a plain year effect — it is *type-specific* year effects.
- Don't describe `make_model_fac` as the facility's exact tank composition — it is the coarse
  ≈47-type signature (wall-mix | fuel-mix | oldest-age band) measured at reform.
- Don't say `Yhat0` is an outcome — it is a control (the residual `τ` is the Route-B outcome).
- Don't present Route A and Route B as the same estimator — A is direct two-way-FE on the
  mix × year cell; B is imputation on the baseline-netted residual with plain year FE.
- Don't present HTE as split-sample — it is a single interaction regression with shared FEs.
- Don't claim every event study is causal — closure's ES is clean; downsize's pre-trend is
  weak (present that figure as descriptive); consolidate's is to be judged from its own figure.

---

**File map:** panels + cells `02b`; baseline crosswalk `02k`; facility regressions/outputs
`02j`; tank HTE `02g`; tank event study `02h`; headline tank ATT `02c`.
