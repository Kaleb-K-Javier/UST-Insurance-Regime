# TICKET 026 — Wholesale gasoline price + state↔region crosswalk + fuel margin
# Created: 2026-06-17
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
Retail price is a poor measure of what a station earns — most of retail price is the cost
of the fuel itself. The fuel MARGIN (retail − wholesale − tax) is the real per-gallon
revenue driver and the cleanest "what the firm earns" dollar variable to feed the revenue
state (Option A). EIA publishes wholesale/spot gasoline prices only at the region (PADD) /
hub level, so converting them to a state-level margin requires an explicit, documented
state→region crosswalk. Per researcher instruction, the crosswalk is a DISTINCT data task
with judgment calls (some states sit between hubs) and is isolated in this ticket so it can
be reviewed on its own before the margin is trusted. Pure data pull — ungated.

═══════════════════════════════════════════════════
DATA SOURCES
═══════════════════════════════════════════════════
- EIA wholesale gasoline price. Options (coder picks, documents): "Refiner Gasoline Prices
  by Sales Type — Sales for Resale" by PADD (monthly); and/or spot prices by hub (NY Harbor,
  Gulf Coast, LA, Chicago). Prefer no-key bulk PET files; API v2 acceptable (EIA_API_KEY).
- Standard PADD definitions: PADD1 East Coast (1A/1B/1C), PADD2 Midwest, PADD3 Gulf Coast,
  PADD4 Rocky Mountain, PADD5 West Coast.
Window: 1994–2020. Map all 50+DC; the study states (TX + AR CO ID KS KY LA MA MD ME MN MO
NC OH OK SD TN VA) MUST each carry a justification note.

═══════════════════════════════════════════════════
DELIVERABLES (enumerated) — TWO FILES
═══════════════════════════════════════════════════
A) Data/Macro/state_region_crosswalk.csv — one row per state (50 + DC).
     state                 chr   2-letter USPS
     padd                  int   1–5
     padd_sub              chr   "1A"/"1B"/"1C" or NA
     wholesale_series_id   chr   exact EIA series/series-key used for this state
     wholesale_hub         chr   human label, e.g. "Gulf Coast"
     mapping_note          chr   justification — REQUIRED non-empty for every study state,
                                 especially borderline/override states
B) Data/Macro/wholesale_margin_state_year.csv — one row per (state, year), 1994–2020.
     state                       chr
     year                        int
     wholesale_price_usd_gal     num   annual mean of monthly wholesale, $/gal nominal
     retail_price_usd_gal        num   joined from Ticket 025
     gas_tax_state_usd_gal       num   from Ticket 025
     margin_usd_gal              num   = retail − wholesale − tax
     margin_real_usd_gal_2020    num   deflated to 2020$
     source_wholesale            chr   series + vintage

═══════════════════════════════════════════════════
STEPS
═══════════════════════════════════════════════════
Step 1 — Build crosswalk (deliverable A): assign every state to a PADD; choose the wholesale
  series for each (PADD-level by default; hub override where a hub better represents the
  state's wholesale market, written in mapping_note).
  assert: every state has exactly one wholesale_series_id; every STUDY state has non-empty mapping_note.
Step 2 — Pull EIA wholesale, collapse monthly→annual mean, 1994–2020.
Step 3 — Join wholesale to states via crosswalk; join retail+tax from Ticket 025; compute margin.
  assert: margin recomputes exactly as retail − wholesale − tax.
Step 4 — Validate: no NA margin for study states 1999–2020; report negative-margin share
  (negatives can occur briefly — do NOT silently drop or floor them).
Step 5 — Write both files. Print the FULL crosswalk to console for researcher review.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Macro/M02_wholesale_margin.R
- Crosswalk may be authored as a committed CSV or an in-script data.table, but must be
  human-readable and reviewable (it carries the judgment calls).
- here(), data.table; never hardcode API keys; hard error propagation; logging per CLAUDE.md.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (binary)
═══════════════════════════════════════════════════
- [ ] Crosswalk covers all 50 + DC; each state exactly one wholesale_series_id.
- [ ] Every study state (TX + 17) has a non-empty mapping_note.
- [ ] margin == retail − wholesale − tax on recompute (within 1e-9).
- [ ] No NA margin for study states 1999–2020.
- [ ] Negative-margin share reported, not dropped/floored.
- [ ] Both files saved to Data/Macro/; full crosswalk printed to console.
- [ ] No silent error catching; log written.

═══════════════════════════════════════════════════
DISTINCT-TASK FLAG
═══════════════════════════════════════════════════
Deliverable A (the crosswalk) is the crux of this ticket and must be reviewed by Kaleb
before the margin file (B) is used downstream.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
