# README: 07_Cross_Subsidy_Figure.R

## What it does

Constructs the PERC milestone M1 "cross-subsidy figure." For each UST facility
in the 5 figure states (CO, LA, NM, PA, TN), computes the actuarially fair
premium PP = λ·S̄ and compares it to the state-specific break-even flat fee
τ (= state mean PP). Produces one pooled transfer-curve figure and two summary
tables.

## Break-even τ framing

The flat fee is anchored at **τ = mean(PP) within each state** (break-even
fund). This isolates cross-subsidy (dispersion of PP around τ) from the
gas-tax-financed level gap between actual statutory fees and τ. Actual
statutory fees are a later overlay, out of scope here.

The pooled figure uses a grand-mean τ across all 5 states as the visual
reference. Per-state τ values are in `cross_subsidy_state_summary.csv`.

## Gross/net cost harmonization

| State | Treatment                          | Deductible |
|-------|------------------------------------|------------|
| CO    | Net: total_cost = (C−D)+ already   | $10,000    |
| TN    | Net: total_cost = (C−D)+ already   | $10,000    |
| PA    | Net: total_cost = (C−D)+ already   | $5,000     |
| NM    | Gross: net = max(total_cost−$10k, 0) | $10,000  |
| UT    | Net (as-is); excluded from figure  | $10,000    |
| LA    | Facility-totals (not per-release)  | $5,000     |

Verified 2026-06-25: CO/TN/PA/UT records already carry the fund-payable net
cost. NM records carry gross cost C and must be netted.

## LA and UT exclusions

- **LA**: Claims file contains one row per facility (totals), not per-release
  rows. Excluded from severity pool. LA facilities use the pooled S̄ in PP.
- **UT**: No tank characteristics in the incident file → excluded from both
  the severity pool and the figure entirely.

## Severity

Pooled S̄ = mean(net_cost_2023) across CO, NM, PA, TN claims. Sub-deductible
zeros (net_cost = 0) are **kept** in the mean — they represent releases where
the state fund paid nothing (the insured absorbed the full cost within the
deductible). Excluding them would overstate expected fund-payable severity.

The age-bin severity breakdown in Step 2 is printed for robustness but **not**
used in PP. PP uses flat S̄ for all facilities.

## Known limitations

- Sub-deductible zeros pre-filtered for PA and TN in the upstream 11_Build
  pipeline; their severity is slightly overstated relative to the true (C−D)+
  distribution. Recovery from raw records is planned for a later pass.
- λ is the **first-release** hazard — a mild lower bound on total release
  frequency (some tanks have multiple releases).
- TN deductible varies $10k–$20k across policies; this script uses $10k
  uniformly.
- UT excluded because the incident file lacks tank characteristics needed to
  assign facilities to the state's facility universe.
- `age_bins` column omitted from `cross_subsidy_facility.csv` in this version
  (retrimmed spec: no per-cell severity, no age join from facility panel).

## Execution order

`01n_CVValidation.R` → `05_Claims_Analysis.R` → `07_Cross_Subsidy_Figure.R`
