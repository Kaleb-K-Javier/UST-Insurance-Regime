# M1 Fair Premium — Editor Brief (plain-language method + where it lives)

For the editor: this is the method in plain English plus a map of which script/output
produces each piece, and the caveats readers need. Turn this into manuscript prose — it is
written to be explained, not pasted.

---

## The one idea

A **fair premium** is the price that makes the fund break even on a tank: charge each tank,
each year, exactly what it is expected to cost. That expected cost is two things multiplied:

> **fair premium per tank-year  =  (how often a tank leaks)  ×  (what a cleanup costs)**

We estimate each piece from the data, let both vary with what we can observe about a tank
(its age and wall type), and multiply. Everything else is detail.

---

## Piece 1 — how often a tank leaks (frequency)

We count cleanups and divide by exposure: **leaks ÷ tank-years**. Three choices, each with a
plain reason:

- **Count every leak, not just a site's first.** The fund pays for a cleanup every time one
  happens, and a tank that has leaked before is *more* likely to leak again (in our data,
  about 2.6× more). Pricing each year fairly means counting them all.
- **Measure it per tank.** A leak is recorded for a *site*, but a fee is charged per *tank*,
  and we can't always tell which tank at a multi-tank site leaked. So we treat each tank as
  having its own, equal chance of leaking: a site with N tanks gets N "shots" per year, and
  the per-tank rate is the site's leaks spread across its tanks. **Single-tank sites pin this
  down exactly** (the site *is* the tank); multi-tank sites split evenly.
- **Use only the years leaks were actually being found.** Before the late-1980s federal
  detection mandate, almost no leaks were recorded — not because tanks were safe but because
  nobody was monitoring. We measure over the detection era and adjust for the calendar year,
  so we're capturing real risk, not changing detection.

**Result:** about **0.45% per tank per year** on average, rising with age, slightly higher
for single-walled tanks.

## Piece 2 — what a cleanup costs (severity)

We have the actual cleanup bills. Two choices:

- **Use the average bill, not the typical one.** Most cleanups run ~$110k, but a handful run
  into the millions, and the fund must be able to cover those. The average that carries the
  rare large cleanups is about **$378k**.
- **Only the fund's share** — the bill minus the deductible (we do this now; capping at the
  fund's coverage limit is a planned refinement that would lower it somewhat).

Cost needs **no per-tank mapping**: a cleanup is one event with one bill, and we simply
attach it to the age and wall type of the site where it happened. Single-walled cleanups run
~50% higher (no second wall to contain the spill).

## Putting them together

> fair premium per tank-year = (leaks per tank-year) × ($ per cleanup)
> ≈ 0.0045 × $378,000 ≈ **$1,700 per tank per year** (blended; higher for old single-walled)

The "leak" cancels, leaving dollars per tank-year. A **facility's** premium is just the sum
over its tanks. Against this, the **actual fund fee is a tiny fraction** (≈1% of fair) — the
rest is covered by gas-tax revenue. That gap is the headline.

---

## Why a "model" at all (and what kind)

If we just chopped the data into age×wall buckets and averaged, thin buckets — especially on
the cost side (only ~7,400 cleanups) — would bounce around randomly. The model is **a smart,
smooth average**: it borrows strength across buckets and adjusts for size, fuel, and state at
once. It does **not** change the idea — we still want "leaks per tank-year" and "average
bill." (Technically: a penalized Poisson regression for the rate, with tank-count as
exposure; a Poisson pseudo-likelihood regression for the average cost.)

---

## Where each piece lives (code map)

| piece | script | output file | what it is |
|---|---|---|---|
| leak rate λ | `Code/Analysis/Descrptive Facts/01r_Leak_Rate.R` | `Data/Analysis/dcm_cell_hazard_pricing.csv` | leaks per tank-year by age×wall (+ CI) |
| cleanup cost S | `Code/Analysis/Descrptive Facts/01q_Severity_Model.R` | `Data/Analysis/dcm_cell_severity_pricing.csv` | average $ per cleanup by age×wall (+ CI) |
| fair premium λ·S | `Code/Analysis/08_Fair_Premium.R` | `dcm_cell_fair_premium.csv`, `analysis_fair_premium_tank.csv`, `analysis_fair_premium_facility.csv` | per-tank / per-facility fair premium (+ CI) |
| cross-subsidy figure | `Code/Analysis/07f` (tank) / `07g` (facility) | `Output/Figures/Fig_*CrossSub_*` | fair premium vs the uniform (flat) premium |
| price-too-low figure | same | `Output/Figures/Fig_*PaidShare_*` | fair premium vs the actual fee |
| full audit (code↔math) | `Code/Analysis/M1_pricing_methods_audit.md` | — | technical companion |
| build spec | `.claude/TICKETS/037_two_part_fair_premium.md` | — | what the coder builds |

Every predicted number carries a confidence interval, from resampling the data (a bootstrap).

---

## Caveats readers should be told (plainly)

1. **The site→tank step rests on one assumption** — that each tank at a site is equally
   likely to leak. It is exact for single-tank sites; for multi-tank sites we split evenly.
   We check it by confirming the single-tank-only rate matches the pooled rate.
2. **We can't see which tank leaked** at a multi-tank site, so "a tank's leak rate" means
   "the rate among tanks of that age and wall," not a serial-number-level number.
3. **The premium is high because cleanups are expensive** (the average carries rare
   million-dollar cleanups), not because tanks leak constantly (~0.45%/yr per tank).
4. **The fund's coverage cap isn't applied yet** — so the fair premium is a mild
   *over*estimate of the fund's own liability; the "fee ≈ 1% of fair" conclusion holds
   regardless.
5. **Single-walled costs more *and* leaks (slightly) more**, so SW tanks carry a higher fair
   premium on both counts.

---

## What this CHANGES in the current draft (02_JMP_Draft.qmd) — editor action list

The new two-part model touches §4.1–4.2 and §5. Specific edits:

- **§4.1 hazard wording + numbers.** Current text says "annual **first-release** probability,"
  fit "on **pre-reform** years," via "penalized **logistic** regression," with double-walled
  "**flat**." The rebuilt model is an **annual leak rate per tank-year** (counts every leak),
  fit on the **detection era (1990–2016)**, a **count model with tank-count exposure**.
  → Update the method sentence; refill the `[X]` rate numbers in `fig-cell-risk`; and
  **re-check "double-walled stays flat"** — the rebuilt rates show both walls rising with age
  with single-walled higher, not DW flat. (Open decision: use this one all-years model for the
  §4.1 predictability story too, vs the current pre-reform-only.)
- **§4.1 severity claim — must change.** Current: *"cleanup cost barely depends on a tank's age
  or wall type, so I use a single pooled cleanup cost."* The severity model shows **single-walled
  cleanups cost ~1.48× double-walled** (no second wall → bigger spill). So the fair premium now
  varies through **both** how often a tank leaks **and** what its cleanup costs. Rewrite the
  `[^csmethod]` footnote and the sentence; the cross-subsidy dollar figures (fees, transfers,
  shares) all refresh.
- **§4.1 predictability numbers** (lift, calibration, ROC/PR AUC, cell-GoF) come from
  `assess.glmnet` on the out-of-sample (prevalidated) predictions — honest, leave-it-out values.
- **§4.2 add a figure.** Keep `fig-actuarial-alignment` (empirical risk vs Texas premium); ADD a
  **fair-premium-vs-actual-Texas-premium** panel (our λ·S estimate against what TX insurers
  charge). Both wanted.
- **§5 DCM** consumes the rebuilt cell schedules.

Everything is out-of-sample / no-leakage by construction (grouped cross-validation by facility,
stratified by state), which matches the draft's existing "scored by a model that left it out"
claim.

## The one-paragraph version (for an intro/abstract)

We price each tank at its actuarially fair premium — the break-even charge equal to its
expected annual cost to the fund — as the product of two estimated quantities: how often a
tank of its age and wall type leaks (about 0.45% per tank-year, from leak counts per
tank-year of exposure over 1990–2016, adjusted for detection), and what a resulting cleanup
costs the fund (about $378k on average, net of the deductible). The fair premium averages a
few thousand dollars per tank-year and rises sharply with age; the actual fund fee covers
only about 1% of it, with the remainder financed by gasoline taxes.
