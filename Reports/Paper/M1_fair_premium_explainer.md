# M1 Fair Premium — Output Manifest & Method (for results review)

> **Paper integration is DEFERRED until all results are reviewed.** This doc is not paper
> prose. It exists so you can (a) understand the method and (b) find every output to review.
> Once results are signed off, the editor maps them into the draft.

---

## The idea (plain)

A **fair premium** is the break-even charge for a tank: what it's expected to cost the fund
each year. It's two things multiplied:

> **fair premium per tank-year = (how often a tank leaks) × (what a cleanup costs)**

- **Frequency** = leaks per tank-year, by age and wall. A site with N tanks gets N "shots";
  per-tank rate = leaks ÷ tank-years. Single-tank sites pin it exactly. Counts **every** leak.
- **Severity** = average $ per cleanup, by age and wall. No tank split needed (a cleanup is one
  bill); attach it to the age/wall of the site involved.
- **Honesty:** every fitted value is **out-of-sample** (cross-validated, leave-the-facility-out,
  stratified by state) — no in-sample inflation. Each prediction carries a bootstrap CI.

Blended fair premium ≈ a few thousand $/tank-year; the actual fund fee covers ~1% of it.

---

## OUTPUT MANIFEST — where every result will live

All produced ON THE SERVER (`Data/Analysis/`, `Output/Figures/`, `Output/Tables/`); pull back
to review. Status: **[built]** = exists, **[rewire]** = exists, switching to the two-part model,
**[to build]** = new.

### 1. "Risk is predictable" (the underwriter can foresee leaks)
| output | path | what |
|---|---|---|
| OOS fit metrics | `Output/Tables/leak_rate_oos_fit.csv` | cross-validated deviance, ROC/PR AUC, pseudo-R² **[to build]** |
| lift curve | `Output/Figures/Fig_LeakRate_Lift.*` | top-decile risk concentration **[to build]** |
| calibration | `Output/Figures/Fig_LeakRate_Calibration.*` | predicted vs observed **[to build]** |
| cell goodness-of-fit | `Output/Figures/Fig_LeakRate_CellGoF.*` | predicted vs raw by cell (45°) **[to build]** |

### 2. "Single-walled gets riskier with age than double-walled"
| output | path | what |
|---|---|---|
| leak-rate cell schedule | `Data/Analysis/dcm_cell_hazard_pricing.csv` | per-tank leak rate by age×wall (+CI) **[rewire→01r]** |
| age×wall figure | `Output/Figures/Fig_LeakRate_AgeWall.*` | SW vs DW leak rate over age (+CI band) **[to build]** |

### 3. Cross-subsidy / fair premium (claims-data states)
| output | path | what |
|---|---|---|
| severity cell schedule | `Data/Analysis/dcm_cell_severity_pricing.csv` | avg cleanup $ by age×wall (+CI) **[built, align]** |
| fair-premium cell schedule | `Data/Analysis/dcm_cell_fair_premium.csv` | λ·S by cell (+CI) **[to build, 08]** |
| per-tank fair premium | `Data/Analysis/analysis_fair_premium_tank.csv` | one row/tank-year (+CI) **[to build]** |
| per-facility fair premium | `Data/Analysis/analysis_fair_premium_facility.csv` | one row/facility-year (+CI) **[to build]** |
| tank cross-subsidy figs | `Output/Figures/Fig_TankCrossSub_*`, `Fig_TankPaidShare_*` | fair vs uniform; fee vs fair **[rewire]** |
| facility cross-subsidy figs | `Output/Figures/Fig_FacCrossSub_*`, `Fig_FacPaidShare_*` | same, facility level **[rewire]** |
| summaries | `Output/Tables/tank_crosssub_state_summary.csv`, `fac_crosssub_state_summary.csv` | τ, % over, fee/fair by state **[rewire]** |

### 4. Texas premiums track risk (both versions you wanted)
| output | path | what |
|---|---|---|
| empirical-risk vs TX premium | `Output/Figures/Fig_actuarial_alignment.*` | current version (risk curve vs schedule) **[rewire, 06]** |
| **fair-premium vs real TX premium** | `Output/Figures/Fig_TX_fair_vs_real.*` | our λ·S estimate vs what TX insurers charge **[to build]** |
| TX premium by age | `Output/Figures/Fig_premium_by_age.*` | the Mid-Continent schedule **[built]** |

### 5. DCM structural inputs
| output | path | what |
|---|---|---|
| leak-rate cells | `Data/Analysis/dcm_cell_hazard_pricing.csv` | feeds the structural hazard primitive |
| severity cells | `Data/Analysis/dcm_cell_severity_pricing.csv` | feeds the structural cost |

### 6. Saved trained models (reload without refitting)
| output | path | what |
|---|---|---|
| frequency model | `Data/Analysis/analysis_leak_rate_model.rds` | cv fit + fit.preval + foldid + bootstrap draws + OOS metrics |
| severity model | `Data/Analysis/analysis_severity_model.rds` | same, for cost |

---

## Caveats to keep in mind while reviewing

1. **Site→tank** rests on "each tank equally likely to leak" — exact for single-tank sites,
   even-split for multi-tank. We report a single-tank-vs-pooled check that it holds.
2. We can't see **which** tank leaked at a multi-tank site, so a tank's rate = "rate among
   tanks of that age/wall," not a serial-number value.
3. The premium is high because cleanups are **expensive** (average carries rare $1M+ ones), not
   because tanks leak often (~0.45%/yr).
4. **Severity is uncapped** (fund coverage limit not yet applied) — a mild over-estimate of the
   fund's liability; doesn't change the "fee ≈ 1% of fair" conclusion.
5. Severity claims states = CO/NM/PA/TN (LA is site-aggregated → borrows national; UT tiny).

---

## Companion docs
- Technical audit (code ↔ math): `Code/Analysis/M1_pricing_methods_audit.md`
- Build spec / acceptance criteria: `.claude/TICKETS/037_two_part_fair_premium.md`
