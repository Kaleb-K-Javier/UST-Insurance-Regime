# TICKET 025 — State-level gasoline dollars: EIA SEDS price/consumption/expenditure + state fuel tax
# Created: 2026-06-17
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
Workstream: bring firm revenue into the structural model (Option A — revenue as an
exogenous Markov state). The model today has no revenue measure; operating payoff is a
normalized constant. GOAL is to MEASURE how much firms weight revenue vs. insurance
prices — not to test that they are equal. This ticket pulls the cleanest free, complete,
full-span state-level dollar series: EIA SEDS gives, per state-year, retail gasoline
price, gallons consumed, and total expenditure (= price × consumption). State motor-fuel
tax is pulled alongside because it is needed to turn retail price into a margin
(Ticket 026) and to decompose price. This is a pure data pull — no estimation, no Bellman,
ungated.

═══════════════════════════════════════════════════
DATA SOURCES
═══════════════════════════════════════════════════
1. EIA State Energy Data System (SEDS), motor gasoline (price, consumption, expenditure).
   Annual, all 50 states + DC, 1970–latest. Prefer the no-key "Complete SEDS" bulk CSV;
   EIA API v2 (dataset = SEDS) with key from Sys.getenv("EIA_API_KEY") is acceptable.
   SEDS price is typically published in $/million Btu; EIA also publishes $/gal in some
   series. Prefer a native $/gal series if available; otherwise convert from $/MMBtu using
   the motor-gasoline heat-content factor (~0.1203 MMBtu/gal — VERIFY against current EIA
   documentation, do not hardcode blindly) and validate against any published $/gal.
2. State motor-gasoline excise tax rate, $/gal, by state-year, 1994–2020. Source options
   (coder picks one, documents it): FHWA Highway Statistics MF-121T, Urban-Brookings/Tax
   Policy Center state gas-tax series, or API/Census state tax tables.

Pull window: 1994–2020 (back to 1994 so a 5-yr trailing average is definable at 1999).
Pull ALL 50 states + DC (trivial; makes the later panel join robust to the exact study-state
set — 02b currently lists TX + 17 controls).

═══════════════════════════════════════════════════
DELIVERABLE (enumerated)
═══════════════════════════════════════════════════
Output file: Data/Macro/seds_gasoline_state_year.csv
One row per (state, year). Expected ~51 states × 27 years ≈ 1,377 rows (report actual).
Columns (snake_case; state types listed):
  state                          chr   2-letter USPS (50 + DC)
  year                           int   1994–2020
  gas_price_retail_usd_gal       num   retail motor-gasoline price, $/gal, nominal
  gas_consumption_mgal           num   motor-gasoline consumption, million gallons
  gas_expenditure_musd           num   total motor-gasoline expenditure, $million nominal
  gas_price_real_usd_gal_2020    num   retail price deflated to 2020$ (state deflator used)
  gas_tax_state_usd_gal          num   state gasoline excise tax, $/gal, nominal
  gas_price_pretax_usd_gal       num   = gas_price_retail_usd_gal − gas_tax_state_usd_gal
  source_price                   chr   e.g. "EIA_SEDS_2026-06"
  source_tax                     chr   tax source label + vintage
Keep ANNUAL raw values only. Do NOT pre-compute a moving average here — the transition
design (annual vs MA, # bins) is intentionally left open until the data is inspected
(see memory: do not pre-judge whether a 5-yr MA is "too smooth").

═══════════════════════════════════════════════════
STEPS (mechanically unambiguous)
═══════════════════════════════════════════════════
Step 1 — Pull SEDS:
  Output: state, year, price ($/gal), consumption (Mgal), expenditure ($M), all states 1994+.
  Convert $/MMBtu→$/gal if needed; validate against published $/gal where available.
  assert: no missing state-years 1994–2020 for the 50+DC; price in [0.50, 6.00] $/gal nominal.
Step 2 — Pull state gas tax: state, year, tax ($/gal), 1994–2020.
  assert: tax in [0.00, 0.70] $/gal; no missing study-state-years.
Step 3 — Merge on (state, year); compute pretax price and real price (state the CPI/deflator).
  assert: expenditure ≈ price × consumption within 1% on overlapping cells.
Step 4 — Write CSV. Print: n_rows, year range, n_states, NA counts per column.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Macro/M01_SEDS_tax.R
- here(), data.table; httr2/jsonlite if API route; readr for bulk CSV.
- NEVER hardcode an API key. Read EIA_API_KEY via Sys.getenv; if unset, use the no-key bulk CSV.
- Hard error propagation only (no tryCatch(...->NULL), no try(silent=TRUE)).
- Logging block per CLAUDE.md for any run > 1 min.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (binary)
═══════════════════════════════════════════════════
- [ ] One row per (state, year); all enumerated columns present with stated units.
- [ ] $/gal conversion validated vs EIA published $/gal: max abs diff < $0.03/gal on overlap.
- [ ] expenditure ≈ price × consumption within 1%.
- [ ] No NA in price/consumption/tax for study states (TX + 17 controls) over 1999–2020.
- [ ] No moving-average / derived smoothing column in the output (annual raw only).
- [ ] Output saved to Data/Macro/seds_gasoline_state_year.csv.
- [ ] No silent error catching; log written to logs/.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
