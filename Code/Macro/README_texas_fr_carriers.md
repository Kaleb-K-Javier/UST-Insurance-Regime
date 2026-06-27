# Texas FR carriers — pricing coverage for the premium rebuild (Ticket 036)

## What this documents
Each Texas facility's **actual insurer over time** (from the financial-responsibility
panel), and **how much of it we can price** with carrier rate filings in hand. This is
the feasibility check for replacing the single mis-assigned Mid-Continent premium card
with carrier-specific premiums (`gamma_p` rebuild).

Source: `Code/Macro/fr_carrier_diagnostics.R` run on
`Data/Processed/texas_fr_facility_month_panel.csv` (22.3 GB; DuckDB). All shares below are
**facility-years** (modal carrier per facility-year). `ISSUER_NAME` is the FR *mechanism*,
**not** carrier-among-the-insured — most facility-years carry no private policy at all.

## The three tranches
Every TX facility-year falls in exactly one tranche:

1. **Not insured / no premium** — `NO COVERAGE`, `SELF` (financial test / local-gov), `PARENT`
   guarantee, state fund. These face no carrier premium and are **outside** the structural
   premium sample.
2. **Insured & PRICEABLE** — a real carrier we hold a rate engine for. Gets a transcribed card.
3. **Insured & UNPRICEABLE** — a real carrier with no filing in hand. Imputed (market mean /
   residual-carrier card), flagged, never dropped.

### Tranche shares (2008–2020 window; n = 214,574 facility-years, 168,868 insured)

| Tranche | Share of **all** fac-yrs | Share of **insured** |
|---|---:|---:|
| 1. Not insured (NO COVERAGE / SELF / PARENT / state fund) | 21.3% | — |
| 2. Insured & **priceable** | 55.8% | **70.9%** |
| 3. Insured & unpriceable (impute) | 22.9% | 29.1% |

Across the **full** panel (all years) the not-insured tranche is ~78% — it is dominated by
the pre-2006 state-fund era; see the ramp below.

## Share we can price, BY YEAR
The FR insurance data is near-empty pre-2008 (state-fund tail) and densifies after; priceable
share is highest early in the risk-based era and falls as the market fragments to surplus-lines
carriers we cannot price.

| Year | All fac-yrs | Insured % (data coverage) | **Priceable % of insured** |
|---:|---:|---:|---:|
| 2006 | 9,451 | 3.7 | 91.3 |
| 2007 | 13,047 | 35.1 | 82.9 |
| 2008 | 16,123 | 69.0 | 82.0 |
| 2009 | 16,346 | 70.5 | 81.0 |
| 2010 | 16,357 | 73.3 | 80.4 |
| 2011 | 16,354 | 72.4 | 79.3 |
| 2012 | 16,427 | 76.3 | 79.0 |
| 2013 | 16,428 | 81.5 | 75.4 |
| 2014 | 16,411 | 78.3 | 71.3 |
| 2015 | 16,744 | 84.1 | 64.3 |
| 2016 | 16,700 | 84.6 | 62.0 |
| 2017 | 16,699 | 82.0 | 63.8 |
| 2018 | 16,657 | 84.5 | 62.4 |
| 2019 | 16,629 | 86.0 | 62.6 |
| 2020 | 16,699 | 79.7 | 65.5 |
| 2021 | 16,740 | 83.3 | 66.7 |
| 2022 | 16,747 | 83.1 | 65.5 |

**Read:** usable carrier data starts **2008** (insured jumps 35%→69%); priceable share of the
insured runs **~80–82% in 2008–2013, settling to ~62–66% in 2015–2022.** A clear majority
throughout — versus today's "100% mis-assigned Mid-Continent."

## Who is in each tranche
Pooled share **of insured** facility-years (the carriers themselves):

**Tranche 2 — priceable (filings in hand), ≈ 70%**

| Carrier (FR `ISSUER_NAME`) | Filing | % of insured |
|---|---|---:|
| Mid-Continent Ins Co | in hand | 33.9 |
| Tank Owners Members Ins Co (TOMICS) | SERFF 2017 | 13.3 |
| ACE American + Illinois Union (ACE family) | ACE Iowa manuals (proxy) | 7.9 |
| Great American Alliance Ins Co | rate manual 2020 | 5.7 |
| Zurich American Ins Co | Iowa rates (level proxy) | 5.4 |
| AIG family — Chartis / AIG / Commerce & Industry | C&I 2015 redlined | 3.1 |

**Tranche 3 — unpriceable (impute), ≈ 30%**

| Carrier | % of insured |
|---|---:|
| Colony Ins Co | 7.4 |
| Ironshore Specialty Ins Co | 7.1 |
| Old Republic Ins Co | 3.2 |
| Crum & Forster Specialty Ins | 2.7 |
| Indian Harbor Ins Co | 2.2 |
| Nautilus / Tudor / Navigators / Liberty Surplus / Hudson / Chubb Custom / Admiral / Scottsdale / … (tail) | ≈ 4 |

**Colony (7.4%) + Ironshore (7.1%) ≈ 15% of insured = the single biggest unpriceable bloc**
(researcher: rate filings unreachable). They are the imputation floor and are concentrated in
the otherwise well-covered later years, which is why priceability falls after ~2014.

The ~70% priceable figure is a mild **lower bound**: a few small carriers that belong to
priceable families were not pattern-matched (e.g. `WESTCHESTER FIRE` = ACE/Chubb,
`STEADFAST` = Zurich; ~0.1% combined).

## Carrier dynamics (the premium variation the rebuild recovers)
- **Zurich exits ~2014** — 2,649 insured fac-yrs in 2008 → 477 (2014) → 21 (2015) → ~0. Its
  book dispersed mostly to **priceable** carriers (switch destinations: Mid-Continent 587,
  ACE 438, Great American 339; vs Colony 297 unpriceable).
- **TOMICS merges into Mid-Continent ~2019–2020** — Tank Owners 2,045 (2018) → 634 (2019) →
  **1** (2020), while Mid-Continent jumps 4,504 → 6,533 → 7,432. `TANK OWNERS → MID-CONTINENT`
  is the #1 year-over-year switch (2,212).

These shifts give the three model eras (`≤2013` / `2014–18` / `2019+`) genuinely different
carrier mixes, which is exactly the cross-era premium variation the single-card model erased.

## Coverage limits
Per-occurrence / aggregate limit fields are **`max_COVER_OCC`** / **`max_COVER_AGG`** (not
"occurrence"/"amount"). The diagnostic's Table E prints the pooled distribution + modal limit
by year — used to decide whether to fix the limit at the modal value or carry it as a premium
dimension.

## Reproduce
```powershell
# server, repo root
git fetch origin; git checkout origin/main -- Code/Macro/fr_carrier_diagnostics.R
& "C:\Program Files\R\R-4.4.3\bin\x64\Rscript.exe" Code/Macro/fr_carrier_diagnostics.R
```
Window is `YEAR_LO`/`YEAR_HI` (default 2000–2022) at the top of the script; family patterns are
in `fam()`. The 20 GB FR panel is gitignored — run on the machine that holds it (server).
