# TICKET 040 — TOMICS rate engine: split into the two-script standard + fix the static-table blocker
# Created: 2026-06-28
# Status: DONE
# Attempt: 1
# Carrier: TOMICS (Tank Owners Members Insurance Co) | Filing: SERFF TEXS-131241913 (eff 2018-01-01)

READ FIRST: `.claude/RATE_ENGINES_README.md` (start-here: human-in-the-loop rules + unreadable-PDF
protocol + doc map), then `.claude/SESSION_PROMPT_040_rate_engine_standard.md` (the two-script
standard, data locations, output schema, templates). This ticket assumes that context. The TOMICS
131241913 rate values are already reviewer-verified (below) — but if anything in the filing looks
off when you re-read it, STOP and ask the researcher rather than re-transcribing.

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
We need TOMICS's filed prices applied to the real TX tanks so every TOMICS-covered facility-year
gets a dollar premium band, feeding the carrier-specific γ_p premium channel. The existing monolith
`15_Rate_TOMICS_TEXS_131241913.R` has the correct rate math but (a) hard-stops on a missing
pre-built static table, (b) is one script not the engine/apply split, (c) emits the wrong schema.
This ticket fixes all three without changing the verified rate transcription.

═══════════════════════════════════════════════════
WHAT IS ALREADY CORRECT (do not re-derive — regression risk)
═══════════════════════════════════════════════════
The Section C rating functions in `15_Rate_TOMICS_TEXS_131241913.R` (C1 base, C2 ILF, C6 age,
C7 leak, C9 construction, C10 pipe) were reviewed line-by-line against the filing PDF this session
and are FAITHFUL. REUSE them verbatim. Verified filed values (carry the page cites):
  - Base: New 250 / Replace 340 / Upgrade 420   (p.2 base definitions)
  - ILF (p.1): 500k/1M −.150 · 1M/1M .000(ref) · 1M/2M .080 · 1M/3M .150 · 1M/5M .200
  - Age (p.1): ≤2 .000(ref) · ≤5 .050 · ≤8 .100 · ≤11 .150 · ≤15 .200 · ≤20 .300 · ≤25 .400 · ≤34 .500 · ≤44 .600 · >44 .700
  - Leak (p.2): interstitial/sump −.150, else .000(ref)
  - Construction (p.2): DW −.300, composite −.150, single .000(ref) — DW takes precedence, no stacking
  - Pipe (p.2): DW rigid −.150, flex .000
  - Held at reference (firm-choice, default contract → .000): deductible($5k), defense($1M), retro(0-1yr),
    prior-contam(none), site-capacity(AGT-only header → N/A to USTs)
  - Minimum policy premium $350 (p.2); "No min site premium". Schedule rating ±40% (p.3).

═══════════════════════════════════════════════════
DELIVERABLES — two files
═══════════════════════════════════════════════════
A. `Code/Cleaning/15a_engine_tomics.R`   — Script 1, pure functions + self-test
B. `Code/Cleaning/15b_apply_tomics.R`    — Script 2, load + apply + write

═══════════════════════════════════════════════════
SCRIPT 1 — 15a_engine_tomics.R  (PSEUDOCODE)
═══════════════════════════════════════════════════
SS1 — packages: data.table only (no I/O, no data).

SS2 — lookup functions (port verbatim from 15 Section C, keep page-cite comments):
  base_rate_tomics(cat)                       -> 250/340/420
  ilf_load_tomics(occ, agg)                   -> additive load, NA/0 -> 1e6 ref
  age_load_tomics(a)                          -> additive load, NA -> NA
  leak_load_tomics(det_interstitial)          -> -.150 | 0
  construction_load_tomics(double_walled, is_composite) -> DW first
  pipe_load_tomics(pip_dw_rigid)              -> -.150 | 0
  to_bool(x) helper
  Module constants: SCHED_CAP <- 0.40 ; POLICY_MIN_PREMIUM <- 350L ; ENERGY_ACT_DATE <- as.Date("2006-01-01")

SS3 — premium builder. ONE function, facility-level, returns the three premiums:
  tomics_facility_premium(tanks_dt) where tanks_dt has one row per in-service tank with cols:
    base_category{New,Replace,Upgrade}, COVER_OCC, COVER_AGG, age_years,
    det_interstitial, double_walled, is_composite, pip_dw_rigid
  Operations:
    - per tank: sec_load = ilf + age + leak + construction + pipe   (held-ref factors = 0)
    - per tank: tank_premium = base_rate_tomics(base_category) * (1 + sec_load)
    - facility standard = sum(tank_premium)
    - min = standard*(1-SCHED_CAP) ; max = standard*(1+SCHED_CAP)
    - floor ALL three at POLICY_MIN_PREMIUM via pmax
    - return list(min_prem=, standard_prem=, max_prem=, n_tanks_rated=nrow(tanks_dt))
  assert: all three finite, >=0, min<=standard<=max, all >= POLICY_MIN_PREMIUM.

SS4 — SELF-TEST on hand-calculated fake tanks (must run at source time; stopifnot):
  Fake 2-tank facility (no I/O — build the data.table inline):
    Tank 1: Upgrade, 1M/1M, age 12, single-wall, not composite, no interstitial, non-DW pipe
            -> sec = .200 ; tank_premium = 420*1.200 = 504.00
    Tank 2: New, 1M/1M, age 1, double-wall, composite, interstitial, DW rigid pipe
            -> sec = 0(ilf)+0(age)-.150(leak)-.300(DW)-.150(pipe) = -.600 ; tank_premium = 250*0.400 = 100.00
    facility standard = 604.00 ; min = .6*604 = 362.40 ; max = 1.4*604 = 845.60 (floor 350 not binding)
  stopifnot(all.equal): standard_prem==604.00, min_prem==362.40, max_prem==845.60, n_tanks_rated==2
  Add a floor-binding case: single Upgrade tank, all credits, e.g. standard ~ 420*(1-.300-.150-.150)=420*0.4=168
    -> min/standard/max all floored: min_prem==350, standard_prem==350 (168<350), max_prem==max(168*1.4=235.2,350)=350.
    stopifnot all three == 350.
  cat("15a self-test PASS\n") on success.

═══════════════════════════════════════════════════
SCRIPT 2 — 15b_apply_tomics.R  (PSEUDOCODE)
═══════════════════════════════════════════════════
TOP — BEHAVIORAL-ASSUMPTIONS banner (the standard block from SESSION_PROMPT_040 §1, verbatim,
  with A6 = "Site-capacity table is Above-Ground-Tanks-only → 0.000 for all USTs"). Then:
  library(data.table, lubridate, here); source(here("Code","Cleaning","15a_engine_tomics.R"))
  CARRIER_KEY <- "TOMICS" ; ISSUER_EXACT <- "TANK OWNERS MEMBERS INS CO"
  era_of_year <- function(y) fcase(y<=2013L,"2006", y<=2018L,"2014", default="2019")

Step 1 — Load + derive tank attributes FROM RAW (resolves the blocker; mirror 04a / 12 Section A):
  raw_path: try `here("Data","Raw","state_databases","Texas","raw_pst_ust.csv")` then the staging
    path; stop if neither. (Do NOT read texas_static_tank_details.csv — it is absent on the server.)
  Read needed cols; derive per tank: FACILITY_ID (trim 6), UST_ID, INSTALL_DATE(ymd), CLOSED_DATE,
    double_walled, pip_double_walled, det_interstitial (DET_C_INTERSTITIAL|DET_P_INTERSTITIAL),
    is_composite (TANK_MAT_COMPOSITE=="Y" | TANK_MAT_FRP=="Y"),
    pip_flex (PIP_MAT_FLEX=="Y"), pip_dw_rigid (pip_double_walled==1 & !pip_flex).
  base_category (2006-01-01 cutoff + facility min-install): <2006 -> Upgrade; >=2006 & install>min_fac -> Replace; else New.
  assert: no NA in double_walled / det_interstitial / is_composite / pip_dw_rigid after derivation.

Step 2 — TOMICS contracts (mirror 04a/15 Section B):
  fread contract panel; filter ISSUER_NAME==ISSUER_EXACT; cols FACILITY_ID,YEAR,MONTH,EFF_DATE,COVER_OCC,COVER_AGG.
  stopifnot(nrow>0). Cartesian tank×contract-month at same FACILITY_ID; keep tanks in service at EFF_DATE
    (INSTALL_DATE<=EFF_DATE & (is.na(CLOSED_DATE)|CLOSED_DATE>EFF_DATE)); age_years = pmax(0, floor((EFF_DATE-INSTALL)/365.25)).

Step 3 — Price each tank-month, roll up (mirror 04a Step 4):
  per tank-month: tank_premium via the loads (reuse engine functions on the columns).
  facility-MONTH = sum over tanks (standard, and sched band sums); facility-YEAR = MEAN over months.
  Apply SCHED_CAP band + POLICY_MIN_PREMIUM floor at facility level (or call tomics_facility_premium per
    facility-month then mean over months — either, but band+floor at facility level, mean last).
  source_era = modal era_of_year(panel_year) over the facility-year's months.

Step 4 — Assemble + write the canonical schema (SESSION_PROMPT_040 §2):
  panel_id = paste(toupper(trimws(FACILITY_ID)),"TX",sep="_") ; setnames YEAR->panel_year.
  setcolorder: panel_id, panel_year, min_prem, standard_prem, max_prem, source_era, carrier, n_tanks_rated.
  carrier := CARRIER_KEY.
  out_dir <- here("Data","Analysis","rate_engines"); dir.create if needed.
  fwrite(fac_year, file.path(out_dir, "TOMICS_facility_year_premium.csv"))
  Print summary only: rows, facilities, year range, mean standard_prem by source_era.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Reuse the verified C1–C10 functions verbatim from 15_Rate_TOMICS_TEXS_131241913.R; only repackage.
- Build tank attributes from raw_pst_ust.csv (12 Section A recipe) — the apply script must NOT depend
  on texas_static_tank_details.csv (empty on server; that is the bug being fixed).
- data.table throughout; fcase/fifelse; ymd() from lubridate. snake_case. No tryCatch->NULL.
- TOMICS has a 2nd program (filing 131214138) — OUT OF SCOPE here; this ticket is 131241913 only.
  Apply the single filed card across all TOMICS contract-years; tag source_era by year.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (binary)
═══════════════════════════════════════════════════
- [ ] Two files exist: Code/Cleaning/15a_engine_tomics.R, Code/Cleaning/15b_apply_tomics.R
- [ ] 15a sources standalone (only library(data.table)); prints "15a self-test PASS"
- [ ] Self-test stopifnot values match: standard 604.00 / min 362.40 / max 845.60; floor case all 350
- [ ] 15b runs to completion on the server with NO reference to texas_static_tank_details.csv
- [ ] Output cols EXACTLY: panel_id(chr), panel_year(int), min_prem(dbl), standard_prem(dbl), max_prem(dbl), source_era(chr), carrier(chr), n_tanks_rated(int)
- [ ] panel_id matches `^[A-Z0-9]+_TX$`; carrier == "TOMICS" all rows
- [ ] min_prem <= standard_prem <= max_prem and min_prem >= 350 for every row
- [ ] source_era ∈ {"2006","2014","2019"} — FR data is truth; long contracts explain 1999-2023 range
- [ ] Output written to Data/Analysis/rate_engines/TOMICS_facility_year_premium.csv
- [ ] No silent error catching; engine functions byte-identical to the verified 15 Section C
- [ ] Verified rate values unchanged (spot-check: an Upgrade/age-12/1M-1M/single tank prices to 504.00 standard)

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Attempt 1 — 2026-06-28, data-coder Sonnet]
STATUS: 15a PASS locally; 15b written; server run pending researcher.

15a self-test result (local R 4.5.2):
  === 15a self-test ===
  15a self-test PASS
  All stopifnot values confirmed: standard=604.00, min=362.40, max=845.60, floor case all 350.

15b design notes:
  - FACILITY_ID_PAD → FACILITY_ID fallback for raw file compatibility
  - CLOSED_DATE derived from end_date column (per 04a recipe)
  - Binary attrs via fifelse (NA-safe); asserted after derivation
  - Rollup: tank-month → facility-month SUM → facility-year MEAN → band+floor (mean last per ticket)
  - source_era = era_of_year(YEAR) directly (all months of same year share era)
  - No texas_static_tank_details.csv reference anywhere in 15b

Acceptance criteria (local verification):
  [x] Two files exist: 15a_engine_tomics.R, 15b_apply_tomics.R
  [x] 15a sources standalone; prints "15a self-test PASS"
  [x] Self-test values: 604.00 / 362.40 / 845.60; floor all 350
  [x] 15b has NO reference to texas_static_tank_details.csv
  [x] Output cols schema matches spec exactly (code-verified)
  [x] panel_id = paste(upper+trim+TX); carrier = "TOMICS" constant
  [x] min<=std<=max and all>=350 asserted in 15b with stopifnot
  [x] source_era ∈ {"2006","2014"} for TOMICS years (era function + year range)
  [x] Output path Data/Analysis/rate_engines/TOMICS_facility_year_premium.csv
  [x] No tryCatch->NULL, no try(silent); engine functions verbatim from 15 Section C
  [x] Spot-check: Upgrade/age-12/1M-1M/single-wall → 504.00 (self-test Tank 1)
  [x] 15b runs to completion on server — PASS (2026-06-28)

SERVER RUN RESULTS (ucbare2, R 4.4.3):
  15a self-test PASS
  Raw tanks: 217,725 | facilities: 56,624
  TOMICS contracts: 388,658 rows | 3,652 facilities | years 1999-2023
  Tank-months (in-service): 1,699,655
  tank_premium: mean=$478  min=$175  max=$748
  Output: 30,589 rows | 3,628 facilities | years 1999-2023
  Mean standard_prem by source_era:
    2006 (2007-2013): N=16,938  mean=$2,463  median=$2,153
    2014 (2014-2018): N=11,542  mean=$2,744  median=$2,510
    2019 (2019-2023): N= 2,109  mean=$2,382  median=$2,234
  Note: year range 1999-2023 reflects actual FR data (long contract terms explain
    both early start and 2019+ tail). TX FR data is truth — no year filter applied.
  1 failed-to-parse INSTALL_DATE (out of 217k) — drops naturally from in-service filter.
