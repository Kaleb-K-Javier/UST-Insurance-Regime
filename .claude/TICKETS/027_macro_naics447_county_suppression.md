# TICKET 027 — County gas-retail dollars (NAICS 447): SUPPRESSION AUDIT FIRST, then conditional pull
# Created: 2026-06-17
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
A county-level dollar proxy for local gas-retail activity (payroll, wages, establishment
counts for NAICS 447, Gasoline Stations) gives the cross-sectional, within-state variation
that disciplines how much revenue matters — the Gowrisankaran-Rysman (2012) / Sweeting
(2013) template of an aggregate moment pinning a sensitivity. BUT Census disclosure
suppression can gut county-level NAICS data, and per researcher instruction suppression is
CRUCIAL to whether this layer is usable at all. So this ticket is AUDIT-FIRST: measure
suppression in our study-state counties before committing to the full pull. Ungated data
pull.

═══════════════════════════════════════════════════
DATA SOURCES
═══════════════════════════════════════════════════
- County Business Patterns (CBP), NAICS 447: establishments, employment, annual payroll
  ($1,000). County×year, 1998–latest. Bulk FTP (no key) or Census API (CENSUS_API_KEY).
- QCEW (BLS), NAICS 447: employment, total quarterly wages ($). County×quarter, 1990–latest.
  BLS QCEW open-data CSVs (no key).
Study states only: TX + AR CO ID KS KY LA MA MD ME MN MO NC OH OK SD TN VA.
Note (do not assert as fact, MEASURE): NAICS 447 is a common multi-establishment industry,
so county suppression MAY be milder than for narrow industries — the audit decides.

═══════════════════════════════════════════════════
PHASE 1 — SUPPRESSION AUDIT  (gating: run, then STOP and report before Phase 2)
═══════════════════════════════════════════════════
Deliverable: Data/Macro/naics447_suppression_audit.csv — one row per (state, year, source).
  state                       chr
  year                        int
  source                      chr   "CBP" | "QCEW"
  n_counties_total            int   counties in state
  n_counties_with_447         int   counties with ≥1 NAICS-447 establishment
  n_counties_dollar_usable    int   payroll/wages present and NOT suppressed/flag-only
  share_usable                num   n_counties_dollar_usable / n_counties_with_447
  n_panel_facilities_in_suppressed  int  UST facilities (our panel) located in suppressed
                                        counties — join on county FIPS if panel geocodes
                                        are available; else NA with a note
Plus a written RECOMMENDATION (put in this ticket's ATTEMPT LOG and a one-paragraph note):
  is the county dollar layer usable for the study states? State the threshold used. If not
  usable, recommend a fallback (state-level only; or aggregate suppressed counties up to
  CBSA / commuting zone; or use establishment counts only where dollars are suppressed).

═══════════════════════════════════════════════════
PHASE 2 — CONDITIONAL PULL  (only if Phase 1 recommendation = usable)
═══════════════════════════════════════════════════
Deliverable: Data/Macro/naics447_county_year.csv — one row per (county_fips, year).
  county_fips        chr   5-digit zero-padded
  state              chr
  year               int   CBP 1998–2020; QCEW 1994–2020
  estab_447          int   establishment count
  emp_447            num   employment (NA if suppressed)
  payroll_447_kusd   num   annual payroll, $1,000 nominal (CBP)
  wages_447_usd      num   total annual wages, $ nominal (QCEW, summed over quarters)
  suppressed_flag    lgl   TRUE if dollar value suppressed/imputed
  source             chr

═══════════════════════════════════════════════════
STEPS
═══════════════════════════════════════════════════
Step 1 — Phase 1 audit for all study states × years × {CBP, QCEW}; compute share_usable.
  assert: one row per (state, year, source); shares in [0,1].
Step 2 — Write audit CSV; write the usability recommendation (with threshold) to ATTEMPT LOG.
  STOP here for researcher decision unless recommendation is clearly usable.
Step 3 — (conditional) Phase 2 pull for study states; flag suppressed cells.
  assert: suppressed cells flagged, never zero-filled or dropped; county_fips zero-padded chr.
Step 4 — Write county CSV; print n_rows, county/year coverage, suppressed share.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Macro/M03_naics447_county.R
- data.table; county_fips as zero-padded character (never numeric — leading zeros).
- QCEW open-data area CSVs are large: filter to NAICS 447 and study-state FIPS early.
- CENSUS_API_KEY from Sys.getenv if API route; else bulk FTP. Never hardcode keys.
- Hard error propagation; logging per CLAUDE.md.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (binary)
═══════════════════════════════════════════════════
- [ ] Phase 1 audit CSV produced for all study states × years × {CBP, QCEW}, share_usable computed.
- [ ] Written usability recommendation present with the threshold stated.
- [ ] Phase 2 run ONLY if recommendation = usable.
- [ ] If Phase 2 run: one row per (county_fips, year); enumerated columns/types; suppressed flagged.
- [ ] Suppressed cells never silently zero-filled or dropped.
- [ ] Outputs in Data/Macro/; no silent error catching; log written.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
