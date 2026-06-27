# M03_revenue_lookup.R — Revenue lookup `R_rev` (Ticket 028)

## What it does
Builds `R_rev[G, state, era] = capbar_G × era-averaged fuel margin / SCALE` and attaches
it to `PM_Lookups.rds` (adds **one** element; all existing elements left byte-identical).
This is the measured operating-revenue object the portfolio DCM reads as `ψ·R` (ticket 029).

## Inputs
- `Data/Macro/wholesale_margin_state_year.csv` (M02 / ticket 026): uses **nominal** `margin_usd_gal`.
- `Data/Analysis/panel_dt.csv`: per-tank `capacity` (winsorized at 60k, summed to facility-year).
- `Data/Analysis/pm_panel.csv`: `g`, `era`, `G`, `excl_*` (one row per facility-year).
- `Output/Estimation_Results/PM_Lookups.rds`: target (gains `R_rev`) + `excluded_states` (KS, MD).

## Outputs
- `R_rev` added to `PM_Lookups.rds` — `dim 4 × n_state × 3`, dimnames `G / g / era`.
- `Data/Macro/R_rev_long.csv`: long form (`G, g, era, R`).

## Method
- `margin_{g,era}` = mean over years-in-era of nominal `margin_usd_gal` (`era_of_year` byte-matches PM01/PM02: `≤2013→2006`, `≤2018→2014`, else `2019`).
- `capbar_G` = mean `total_cap_capped` over INCLUDED facility-years (all `excl_*==0`, `G` present, `cap>0`), by `G`. **Global**, era-invariant.
- `R_rev[G,g,era] = capbar_G × margin_{g,era} / 10000`.
- KS/MD (`excluded_states`): zero-filled if margin missing; they never enter the likelihood.

## `kappa = 1` normalization (important)
`R` uses tank **capacity** as the size proxy, not annual throughput. Actual revenue =
throughput × margin = (turnover × capacity) × margin. Turnover (`kappa`) is normalized to
**1**, so the estimated `ψ` **absorbs turnover**: `ψ_hat` carries the turnover factor
(~30–50). For a real-dollar reading, divide `ψ_hat` by true turnover. Units normalization,
not a modeling assumption.

## Run / propagation
`Rscript Code/Macro/M03_revenue_lookup.R` — on the machine whose `PM_Lookups.rds` the ψ fit reads.
NOTE: `PM_Lookups.rds` and everything under `Data/` are **gitignored** (do not sync via git).
To use `R_rev` on another machine (e.g. the server), either run this script there, or carry
`R_rev_long.csv` over and re-attach it to that machine's `PM_Lookups.rds`.
