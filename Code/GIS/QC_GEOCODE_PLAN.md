# GIS QC — geocode / lat-long quality plan
# Created: 2026-06-24 | Status: PLANNING (provenance-first; remediation deferred)
# Spatial-matching algorithm is fine; the issue is COORDINATE PRECISION, and it must
# be checked PER PROVENANCE (EPA-sourced vs state-sourced coords differ).

## 1. Problem (what the spot-check surfaced)
Facility→facility matching works, but some lat/longs are low-precision, biasing every
distance layer (competition `02`, pending wells `04`) and the "does the pin land on
the site?" check.
- **Coordinate stacking / co-location.** OK: 3.4% of facilities share an exact
  coordinate (226 points, 472 facilities; worst = 4). Verified: VFW Post 4465
  (`2011644`) + Snacker's LLC (`2013449`) ~33 m apart = two REAL distinct facilities
  (key = `facility_id+state`, NOT a dup id), same/near geocode.
- **"In nowhere" points** = ZIP/city/county/state CENTROID geocodes, not the address.

## 2. Provenance mechanics (from 10_Master_Cleaning_and_Harmonization.r, Step 2)
- EPA UST Finder coords (`Data/Raw/Facilities.csv`, server-only) merged via state-
  specific `merge_id`; then **native coords take PRIORITY, EPA fills gaps**:
  `latitude_final = fcoalesce(latitude, lat_epa)` (lines 154-156).
- Winning source is NOT retained (`lat_epa` dropped line 176; `latitude := latitude_final`
  line 453) ⇒ provenance must be RE-STAMPED.
- Recoverable with one line at the coalesce:
  `coord_source = state_native (native non-NA) / epa_finder (only lat_epa) / missing`.
- Implication: native-coord states (AR LA ME NJ NM OK AL TN CO …) ≈ state_native;
  **TX & MI historically lacked native ⇒ TX (TREATED) is largely epa_finder** →
  Route 2 (EPA flags) matters most for the treated group.

## 3. The three QC routes (your structure)

### ROUTE 1 — PROVENANCE  (foundation; do first)
Add `coord_source` to 10_Master_Cleaning at the coalesce, AND retain a
`coord_native_epa_dist_m` = distance(native, EPA) for facilities where BOTH exist
(currently thrown away — it's a free cross-check). Re-run (server-side, where
Facilities.csv lives) ⇒ every facility stamped state_native / epa_finder / missing.

### ROUTE 2 — EPA-SOURCED  (coord_source == epa_finder)
Use EPA's OWN geocode-quality flags from Facilities.csv: `Coordinate_Source`,
`Address_Match_Type` (+ EPA geo-quality code). These label each EPA point
address-matched (good) vs centroid/approximate (bad) — no re-geocoding. Pull
alongside `lat_epa`. Especially load-bearing for TX.

### ROUTE 3 — STATE-SOURCED  (coord_source == state_native)
No standard quality flag, so AUTOMATE the hand-check via multi-source agreement
(this is the "automate the hand-checking" path — humans only see disagreements):
  a. **Cross-source distance** — native vs EPA where an EPA coord exists; big gap =
     flag. EPA acts as an independent referee. [primary automation]
  b. **County-containment** — implied county = `gis_01$tract_geoid_2020[1:5]` vs the
     recorded `county_fips`; mismatch = "in nowhere". [FREE, runs locally NOW]
  c. **Re-geocode-and-compare** — Census batch geocoder (free, no key) from the EPA
     address; disagreement vs stored coord = flag.
  d. **Stacked-coordinate** flag (+ `gis_02` near-zero edges separate dupes from real).
  Rule: ≥2 independent sources agree ⇒ trust; residual disagreements ⇒ the (small)
  manual/coworker review queue.

## 4. Triage output
A per-facility `geocode_quality` field (good / epa_centroid / county_mismatch /
cross_source_disagree / stacked / needs_review) + a by-state summary (is it a
TX-target problem or a few control states?), added as a column to
`gis_facility_covariates.csv` so analyses can drop/condition on it.

## 5. Dependencies, sequencing, open decisions
- **Enabler = server EPA `Facilities.csv`** for Routes 1, 2, 3a, 3c (lat_epa +
  quality flags + address). Either re-run 10_Master_Cleaning on the server with the
  additions, or copy Facilities.csv local.
- **Route 3b (county-containment) runs NOW locally** — the free first cut at "in nowhere".
- Wells `04` (WQP + FRS, both reachable) PARKED until geocode quality is assessed.

Open decisions:
  1. Run the free county-containment scan now to size "in nowhere" across the 18 states?
  2. Re-run 10_Master_Cleaning with `coord_source` + retained native-vs-EPA distance +
     pulled EPA quality flags (server)?  (unblocks Routes 1 & 2 and 3a)
  3. Re-geocode scope (Route 3c): all flagged, estimation-sample only, or distance-critical only?
