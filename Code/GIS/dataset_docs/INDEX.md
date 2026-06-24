# Dataset documentation — revenue / demand-control inputs for the structural model

Pulled 2026-06-23 to support the **maintain-payoff revenue proxy** `R(G',z,m)` (Option A;
see ticket 025/026/027 and memory `project_revenue_gasprice_option_a`). Goal of these
datasets: give the maintain flow utility **county-level demand heterogeneity** via
`R ≈ margin × county_gasoline_quantity`, and provide the household demand structure / scale
checks behind it.

Retrieval note: this machine's Chrome MCP is locked to `bls.gov`; `census.gov`, `bts.gov`,
`ornl.gov` were fetched with `curl`. BTS and BLS block curl, so LATCH and CES were pulled
from the **Internet Archive Wayback Machine** (snapshots noted). Verify against the live
sites before publication.

---

## NHTS — National Household Travel Survey (the survey under LATCH)
Source: https://nhts.ornl.gov  (2017 NHTS)
- `nhts/NHTS_2017_UsersGuide.pdf` — survey design, weighting, what's collected.
- `nhts/NHTS_2017_Codebook_v1.1.pdf` — variable codebook (Household file).
KEY FACT (geography wall): the **public NHTS has no county** — household geography tops out
at state (`HHSTATE`), Census division/region, urban-rural, and MSA-size class. County/tract
is restricted-use only. So raw NHTS can't be joined to facilities below state; the
county/tract product you want is **LATCH** (below).

## LATCH — Local Area Transportation Characteristics for Households  ★ primary for county demand
Source: BTS https://www.bts.gov/latch (2017, transferred from NHTS 2017 via ACS).
- `latch/latch_2017.csv` — **the data: one row per US Census tract (73,056 tracts)**.
- `latch/LATCH_methodology_2017.txt` — how NHTS travel is modeled onto tracts (small-area est.).
- `latch/LATCH_data.txt` — data-page description + file/dictionary listing.
- `latch/NHTS_BTS_data_dictionary.html` — BTS dictionary landing shell (JS; use live site for the interactive dict).
KEY FACT: LATCH gives **average weekday household VMT per tract** — exactly the demand driver.
Column glossary (from the CSV header):
  - `geocode` = 11-digit 2010 Census tract GEOID
  - `est_vmiles` = avg weekday household **vehicle-miles/day**  ← gasoline-demand driver
  - `est_pmiles` / `est_ptrp` / `est_vtrp` = person-miles / person-trips / vehicle-trips per day
  - `hh_cnt`, `tot_pop` = ACS households / population in the tract (`m_*` = margin of error)
  - `median_hh_inc`, `pct_veh_0/1/2_more`, worker counts, and `*_Nmem_Nveh` cells = demand by household size × vehicles
GAP: the formal column dictionary (BTS "appendix D") did not archive cleanly in Wayback; the
CSV header + methodology cover it. It's a **~2017 cross-section** (and a 2009 vintage) — good
for cross-county demand heterogeneity, NOT annual time variation.

COUNTY-DEMAND RECIPE (what feeds R):
  1. `tract_VMT = est_vmiles × hh_cnt`  → sum tracts to county → `county_VMT`
  2. `county_gallons = county_VMT / fleet_mpg`  (or keep VMT as a demand index)
  3. anchor to EIA: scale so Σcounty_gallons in a state = SEDS state gallons (ticket 025)
  4. `R_proxy = margin_state,t (ticket 026) × county_gallons`   ← use **margin**, not retail price

## DARTE — Database of Road Transportation Emissions
Source: ORNL DAAC, ds_id 1735, https://doi.org/10.3334/ORNLDAAC/1735
- `darte/DARTE_V2_UserGuide.pdf` (+ `.txt`) — methodology + variables.
CORRECTION (verified in the guide): the **published V2 product is a 1-km grid of on-road CO2**
(1980–2017), *built from* county×functional-class VMT and county gasoline/diesel gallons,
scaled to FHWA Highway Statistics MF-21 state totals. So **county-year fuel is recoverable**
(aggregate the grid to county polygons, or go to HPMS / the V1 county series directly) but it
is **NOT a ready county-VMT CSV** — there is an aggregation step. This is the route to add
*annual* county-demand variation that LATCH (cross-section) lacks. Underlying source = FHWA HPMS.

## EIA SEDS — State Energy Data System (state gasoline level; ticket 025)
Source: https://www.eia.gov/state/seds/
- `seds/SEDS_use_technical_notes.pdf` — consumption methodology (gallons).
- `seds/SEDS_price_petroleum_notes.pdf` — retail price methodology.
Role: state-year gasoline **price, consumption (gallons), expenditure** for all 50+DC, the
LEVEL the county allocation is anchored to. (Live SEDS pages 503'd to curl; PDFs fetched.)

## CES — Consumer Expenditure Survey PUMD (household gas expenditure; secondary)
Source: https://www.bls.gov/cex/  (see memory `reference_bls_ces_pumd_geography`)
- `ces/CES_PUMD_dictionary.xlsx` — variable dictionary incl. UCC codes (gasoline 470111; FMLI `GASMOCQ`).
- `ces/CES_income_imputation_guide.pdf` — income-variable guidance.
KEY FACT (geography wall): **no county/tract/ZIP** — finest is MSA(23)/division/state(only
CA,FL,NY,TX, research weights 2017–23, sunset May 2026). So CES is a coarse demand-structure /
validation layer, NOT a county revenue measure (which is exactly why LATCH+DARTE+SEDS carry
the local allocation).
