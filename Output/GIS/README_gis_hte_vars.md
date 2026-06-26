# GIS handoff for reduced-form HTE — two files
# Built: 2026-06-26 (local; server has no GIS stack). Written under Output/ because
# Data/ is gitignored but Output/**/*.csv (and these) ARE tracked, so they reach the
# server/other session via `git pull` — no GIS run there.

Join everything to the facility-year panel by **`panel_id`** (= `facility_id`+`state`).
Both files cover the 18 estimation-sample states (TX + 17 controls).

## 1. gis_hte_vars.csv — census HTE splits, FIXED AT TREATMENT (Census 2000)
One row per facility (264,914; 99.9% tract-matched). Population / rural / urban /
income / composition measured at **Census 2000** — i.e. PRE-reform (TX reform 1999),
so they are valid HTE split variables (not endogenous to the treatment). NOTE these
are baseline values, intentionally NOT time-varying (split on what a facility *was*
before the reform). Median `med_hh_income_2000` ≈ $34k (1999$) confirms the vintage.

Columns:
- `panel_id`, `facility_id`, `state`, `tract_geoid_2000`
- population: `pop_2000`, `pop_density_2000` (per sq mi), `pop_density_tertile`
  (low/mid/high), `low_pop_density` (1 = bottom tertile)
- rural/urban: `pct_rural_2000`, `rural_2000` (1 = tract majority-rural in 2000; ~31.6%)
- demographics: `med_hh_income_2000` (1999$), `pct_minority_2000` (1 − non-Hisp-white
  share), `pct_poverty_2000`

## 2. gis_neighbor_edges.parquet — for TIME-VARYING competition (compute ex post)
~4.0M undirected edges (i<j) within 1 mile; one-time export because geography is
time-invariant. Cols: `panel_id_i`, `panel_id_j`, `state`, `dist_m`.
(Plus `gis_neighbor_edges_sample.csv` = 50-row schema preview.)

**How to build active competitors in year t:** cross these edges with the panel's
active-by-year status (already on the server). For facility i in year t, count
neighbors j with `dist_m <= R` that are ACTIVE in t (≥1 open tank). Choose R
(e.g. 400/800/1609 m) at analysis time. This is the time-varying competition the
static count couldn't give.

## Caveats
- HTE vars are the facility's **tract** attributes (point-in-polygon), so they inherit
  geocode precision; lat/long QC is a known issue deferred to ~July 2026
  (Code/GIS/QC_GEOCODE_PLAN.md). Rural/pop splits are fairly robust to small error.
- ~0.1% of facilities don't fall in a 2000 tract → NA (edge/coastal cases).
- 18 RF states only. EJScreen EJ indices not included.
