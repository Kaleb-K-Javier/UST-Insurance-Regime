# Reduced-form setting/data — answers to the §2/§3 number gaps (Ticket 034)
# Computed locally from Data/Analysis/{facility_panel.csv, exact_base.csv} 2026-06-26.
# Pre-reform window = 1994–1997 (four full years before the Dec 22 1998 reform).
# texas_treated: 1 = Texas, 0 = the 17 controls. For the editor — verified, with flags.

## DECISIONS LOCKED (2026-06-27)
- **Headline sample = birth-CEM MATCHED** (facility 3.44M facility-years; tank 9.76M tank-years /
  117,250 facilities). All-incumbents is a **robustness only** (see "Sample robustness" below).
- **Event studies use `panel_id + cell_fac_year`** (facility + portfolio-mix × year FE) — the same
  FE×year design as the static DiD, on both tank and facility.
- Consolidation is an **informative null**; the sorting story is the payoff.

## Sample robustness (Ticket 035 — matched vs all-incumbents)
Re-ran the facility portfolio DiD on all alive-at-reform incumbents (4.82M facility-years, FE-only),
vs the matched headline. Result: **the sorting is fully robust** — every size/vintage gradient holds
(small→exit, large→downsize/replace, consolidate null) with the same shape and significance. Pooled
ATTs attenuate ~25% on all-incumbents and pooled downsize/repl significance softens (their gradients
stay strong). The **closure pre-trend is cleaner under matching even net of the mix×year FE** — so
matched is the headline; all-incumbents goes in one robustness sentence ("results hold, attenuated,
sorting unchanged"). Files: `T_Facility_SampleCompare_{ATT,SizeHTE,VintageHTE}.csv`,
`Fig_ES_Facility_*_allinc.*`.

## Q1 — Sample counts (two DIFFERENT samples; the draft conflated them)

**Alive-at-reform (incumbent) descriptive sample** — facilities with a tank operating on
Dec 22 1998, and all their facility-years (this is what `T_Baseline_Characteristics_Slide.R` builds):

| | Full (18 st) | Texas | Controls |
|---|---|---|---|
| Facilities | 252,670 | 54,379 | 198,291 |
| Tanks at reform | 330,311 | 60,651 | 269,660 |
| Incumbent facility-years | 4,824,293 | 898,006 | 3,926,287 |
| (Full panel, incl. non-incumbents) | 5,061,154 | 963,295 | 4,097,859 |

**Matched birth-CEM DiD estimation samples** (from the run outputs — the matched panels are not
local; these are the regression samples):
- Tank headline DiD: **9,761,982 tank-years / 117,250 facilities / 18 states.**
- Facility portfolio DiD: **3,443,560 facility-years / 18 states** (`T_Facility_Portfolio_ATT.csv` n).

**Reconciliation:** 117,250 (matched DiD facilities) and 252,670 (alive-at-reform facilities) are
**different samples** — matched-regression vs. descriptive-coverage. 117,250 is current and correct
for the *tank DiD*; 252,670 is correct for the *baseline-characteristics table*. The draft's
"117,250 facilities … 9.76M tank-years" is the matched tank DiD; do not also call it the
alive-at-reform panel. §3 ¶1, ¶8, §4.3 should name which sample each count refers to.

## Q2 — Pre-reform tank-closure rate, TX vs control (incumbents, 1994–97)

| closure definition | Texas | Controls |
|---|---|---|
| gross (all closures / active tank-yrs) | 6.35% | 5.89% |
| permanent only | 5.56% | 5.01% |
| replacement only | 0.79% | 0.88% |
| any-closure (share of facility-yrs) | 7.63% | 7.32% |

**TX and control are close on every definition (within ~0.3–0.5 pp)** → comparable pre-reform levels.
**FLAG:** none of these equals the cited "2.0% control baseline." That 2.0% is **not** the
facility-panel `n_closures` rate — it must be the tank-level headline `closure_event` (a narrower
hazard, e.g. clean/permanent closures among at-risk tanks) in `02b_tank_closure_analysis.R`. Either
trace the 2.0% to that exact outcome or restate it as the ~5–6% gross rate. Do not present 2.0% and
6% as the same object.

## Q3 — Pre-reform leak-discovery rate, TX vs control (share of facility-yrs with a leak, 1994–97)

| sample | Texas | Controls |
|---|---|---|
| incumbents | 0.59% | 2.41% |
| all facilities | 0.59% | 2.41% |
| (full-window incumbents, the contaminated `rel_rate`) | 0.59% | 1.67% |

**FLAG — these are NOT parallel.** Control leak-discovery is ~4× Texas pre-reform (2.41% vs 0.59%).
The cited "1.73% incumbent base" ≈ the **control** full-window rate (1.67%), **not** Texas (0.59%) or a
pooled baseline. The TX–control gap is almost certainly a **recording/reporting difference** (Texas
records far fewer leak incidents), not a real risk difference. **Do not claim the two groups have
parallel leak rates**, and don't attribute 1.73% to Texas.

## Q4 — Do the groups track pre-reform? (where the "parallel" claim should live)

- **Closure:** roughly comparable levels (TX 6.35% vs control 5.89%). Defensible.
- **Leak discovery:** **not** comparable (0.59% vs 2.41%).
- **Recommendation (matches the editor's option a):** move the "parallel before the reform" claim out
  of the §3 ¶8 *levels* sentence and into **§4.4, anchored on the event-study pre-trend** (which tests
  *trends*, not levels). Use the closure event study (`Fig_ES_Facility_Portfolio` / the tank
  `Fig_ES_Full`) flat pre-period as the parallel-trends evidence. Drop the leak-rate parallel claim
  entirely — the levels differ 4×.

## Q5 — Retrofit / wall-change share (the 0.003%)

- **0.003%** is a **TX-registry true in-place wall retrofit** figure (TX-only, window **1998–2021**) —
  per the editing-notes pull (`# Paper editing notes.md:224`: "retrofit share 0.003% (TX registry
  1998–2021)"). It is a genuine near-zero: almost no existing tank is physically re-lined SW→DW.
- **Distinct construct — do not conflate:** the facility-panel `single_to_double_year` (a facility's
  tank *mix* shifting from SW to DW, which includes replacing a SW tank with a new DW one) is **~2.05%
  of Texas facilities / 2.25% of controls** — two orders of magnitude larger, and a different thing.
- **§2 wording fix:** §2 ¶1 says "0.003% of active **facilities**"; the source is the **Texas** registry,
  so it should read "active **Texas** facilities" — same number as the intro, just make the scope explicit.

## Q6 — Control states  ✓ CONFIRMED FROM CODE + DATA

The pipeline uses ONE vector (`02a_DiD_OLS.R:111` = `02b_tank_closure_analysis.R:71` =
`02b_Tank_level_Panel_Build.R:123`), and the data confirms it exactly:
**TX + 17 controls = AR, CO, ID, KS, KY, LA, MA, MD, ME, MN, MO, NC, OH, OK, SD, TN, VA (18 states).**
`unique(state)` in `facility_panel.csv` = these 18, no extras. **CO/KY/MD/MO are IN; AL/IL/MT/NM/PA are
NOT.** The paper's §3 ¶2 list is wrong and must be rewritten to these 17.

## Q7 — Dollar facts (both are literature cites, not data pulls)

- **Install ≈ $23k single-wall / $39k double-wall:** **literature cite — GAO 1987** (editing notes mark
  it `[cite]`; attribute to the GAO 1987 report).
- **Double-wall replacement ≈ $55,000/tank:** **literature cite** (editing notes mark it `[cite]` at
  line 236) — but **no specific reference is named** beyond "[cite]"; attach a source before it goes in
  (it is not a registry/claims data field).
