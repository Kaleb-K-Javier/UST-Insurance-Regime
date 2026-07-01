# TICKET 036 — Carrier rate engines -> rebuild TX facility premiums (recover gamma_p variation)
# Created: 2026-06-27
# Status: READY (data-engineering build; ungated). Pull-in-progress on 2 carriers.
# Type: DATA CONSTRUCTION. Builds a per-facility, per-year PREMIUM from carrier-specific
#       rate manuals + the FR carrier assignment. Replaces the single Mid-Continent card.
# Seat: data coder. Non-R OK for PDF parsing (python pdfplumber/camelot likely) — per
#       CLAUDE.md, inline R-mapping comments + a companion .md.

═══════════════════════════════════════════════════
WHY (the identification problem this fixes)
═══════════════════════════════════════════════════
gamma_p (premium response) is currently UNIDENTIFIED, not zero: the structural premium is
ONE Mid-Continent card by age x wall x era, so cross-state variation is FE-absorbed and
within-state variation is thin. The TX FR panel shows Mid-Continent is only ~38% of insured
facility-years; ~60% are on TOMICS / Ironshore / Colony / Great American / Zurich / ACE/AIG,
with heavy CHURN (Zurich collapses 2008->2014, TOMICS after 2019; Mid-Continent + Ironshore
absorb them). Assigning each facility its ACTUAL carrier's premium is BOTH a correction and
the source of within-TX premium variation that identifies gamma_p — via (1) carrier switches
within a facility (supply-driven, exogenous, survives even a facility FE), (2) coverage-limit
differences, (3) cross-carrier level differences, (4) carrier re-pricing over time. None of
this is touched by the state FE.

═══════════════════════════════════════════════════
INPUTS
═══════════════════════════════════════════════════
- Rate filings (PDFs) Data/Raw/Rate_Filings/:
    TOMICS 2017  (TEXS_131241913, TEXS_131214138) + SUPPORT docs w/ REAL FIRM DATA (validate the engine against these)
    Commerce & Industry Ins Co (AIG) 6/2015  (redlined -> back-cast prior rates; + "forward")
    Great American Ins Co rate manual 2020 (+ "comparison new product to other insurers" = direct cross-carrier evidence)
    ZURICH AMERICAN 2009 (TX exit; USE IOWA rates as a LEVEL-PROXY for TX — researcher OK, mostly a level diff)
    Mid-Continent (researcher has it; add to folder)   Ironshore + Colony: PULL IN PROGRESS
- FR panel Data/Processed/texas_fr_facility_month_panel.csv (20GB; DuckDB, NOT fread):
    FACILITY_ID, YEAR, ISSUER_NAME (carrier), Coverage Amount per Occurrence/Aggregate (limits),
    Coverage Effective Begin/Expiration Date. (premium_prepaid is a Y/N FLAG — no $; confirmed via dict
    "Docs/petroleum-storage-tank-data-specifications (10).docx".)
- Structural facility panel (age x wall x capacity, panel_id): Data/Analysis/panel_dt.csv / pm_panel.csv.

═══════════════════════════════════════════════════
STEPS
═══════════════════════════════════════════════════
1. PDF -> RATE ENGINE per carrier. Each filing -> premium = f(age, wall, capacity/tank, coverage
   limit, year, territory). Encode base rate + factor tables as a function/lookup. VALIDATE each
   engine against the support-doc real-firm examples (TOMICS) — engine must reproduce them.
2. CARRIER-NAME CROSSWALK: FR ISSUER_NAME <-> filing company <-> corporate family. e.g.
   Commerce & Industry = AIG (maps to AIG/Chartis/AMERICAN INTL rows); ACE family; GREAT AMERICAN
   ALLIANCE; ZURICH AMERICAN; MID-CONTINENT; TANK OWNERS MEMBERS. Build the lookup explicitly.
3. FR SWITCHING PANEL: facility -> (carrier, coverage limit, begin/end date) over time (DuckDB).
4. APPLY: facility-year premium = its carrier's engine(facility chars, limit = $1M, year). COLONY +
   IRONSHORE (~15% of insured; rate filings UNREACHABLE) and the small unpriceable tail -> IMPUTE:
   premium = share-weighted PRICEABLE-market mean for the matching age x wall x era cell; set
   premium_imputed = TRUE (keep the real carrier label for reference); do NOT drop. Expect ~70% of
   insured fac-years on a real engine, ~30% imputed.
5. OUTPUTS — TWO artifacts (so ticket 039 ingests cleanly; see 039 for the exact consumer):
   (a) CARDS: Data/Analysis/rate_engines/<carrier>_engine.csv per priceable carrier — cols
       carrier, wall, age_bin, era, premium_usd_per_tank_yr, limit_usd=1000000, source_doc. Feeds PM02 pbar.
   (b) ASSIGNMENT: Data/Analysis/tx_facility_premium_rebuilt.csv — cols panel_id, panel_year, carrier,
       coverage_limit, premium_rebuilt_usd, premium_imputed (+ companion .md). Feeds PM03.
   CONVENTIONS (locked for the 036<->039 interface):
     - year column = panel_year (calendar; matches pm_panel for the join key (panel_id, panel_year)).
     - carrier = NORMALIZED key in {MID_CONTINENT, TOMICS, GREAT_AMERICAN, ZURICH, ACE, AIG, IMPUTED}.
       036 owns the FR ISSUER_NAME -> key crosswalk (step 2); 039 does NO string cleanup.
     - ENGINE-CARD (a) format — PM02 fails otherwise (verified vs the draft tomics_131214138 file):
         * FILENAME = <CARRIER_KEY>_engine.csv using the canonical key (e.g. TOMICS_engine.csv) —
           NOT filing-numbered/lowercase (tomics_131214138_engine.csv won't match PM02's loop).
         * carrier column = the same canonical key; wall in {SW,DW}; age_bin = the 8 band-label strings
           "0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+" (PM02 AGE_BIN_MAP keys);
           era in {"2006","2014","2019"} ONLY — no "2014_PENDING" / placeholders.
         * ERA COVERAGE: emit rows for EVERY era the carrier was active in (per fr_carrier_diagnostics),
           not just the filing year — e.g. TOMICS active 2007-2018 -> emit BOTH era 2006 AND 2014
           (one filing's rates carry across the carrier's active eras unless it re-filed). 039/PM02
           only needs (era,carrier) cards for the combos that actually occur in pm_agg_counts.
     - premium_imputed = INTEGER 0/1 (0 = real engine, 1 = imputed).
     - premiums in USD; PM02 divides the CARDS by SCALE (10000), like the current pbar. premium_rebuilt_usd
       in (b) is AUDIT/diagnostic only — the model path uses (a) cards + the (b) carrier assignment,
       NOT the per-facility premium (a facility spans multiple cells, so it can't recover a per-cell card).

═══════════════════════════════════════════════════
LOCKED DECISIONS
═══════════════════════════════════════════════════
- Zurich TX premium = Iowa Zurich rate as a level-proxy (researcher: mostly a level diff, fine for now).
- DEDUCTIBLE / OOP assumptions UNCHANGED. This ticket rebuilds only the PREMIUM term (gamma_p side);
  H*D stays as-is (gamma_r side untouched).
- Keep the structural age x wall cells; map filing rate dimensions onto them.
- COVERAGE LIMIT held FIXED at $1,000,000 (statutory minimum; 90.8% of insured fac-yrs, modal $1M
  every year 2001-2022, no drift -> per fr_carrier_diagnostics). Not a premium dimension. Optional
  refinement: apply the carrier's $2M rate factor to the ~8% on a $2M limit.
- COLONY + IRONSHORE are IMPUTED, NOT transcribed (filings UNREACHABLE, researcher 2026-06-27).
  Imputed premium = share-weighted priceable-market mean for the age x wall x era cell; flag
  premium_imputed = TRUE; never drop. Same rule for the small unpriceable tail (Old Republic /
  Crum & Forster / Indian Harbor / Nautilus / ...). ROBUSTNESS: re-fit gamma_p on real-engine
  fac-years only (premium_imputed = FALSE) to confirm gamma_p is not an imputation artifact.

═══════════════════════════════════════════════════
VALIDATION / ACCEPTANCE
═══════════════════════════════════════════════════
- [ ] Each engine reproduces its filing's real-firm support examples.
- [ ] Within-TX premium CV > 0 and the premium JUMPS at carrier switches are visible (the gamma_p
      signal exists). If carriers turn out "same design" (flat level diffs only), REPORT the small
      variation honestly — that itself answers the question.
- [ ] Coverage report: % of TX insured facility-years on a matched (not imputed) rate engine
      (expect ~70% per fr_carrier_diagnostics; ~30% imputed = Colony/Ironshore/tail). The
      premium_imputed flag is present so the estimator can include or exclude imputed rows.
- [ ] tx_facility_premium_rebuilt.csv + companion .md written; DuckDB used for the 20GB FR file.

═══════════════════════════════════════════════════
DOWNSTREAM (separate ticket)
═══════════════════════════════════════════════════
Feed tx_facility_premium_rebuilt into the structural premium (replace the pbar / ss$P_RB_all
construction in the PM pipeline), re-run PM08 two-gamma (PM08_PSI unset), and check whether
gamma_p moves off ~0 now that within-TX premium variation exists. The "Great American vs other
insurers" comparison doc is the early read on whether carriers price differently enough to matter.
