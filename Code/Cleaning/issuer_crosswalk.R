###############################################################################
# issuer_crosswalk.R — FR ISSUER_NAME -> carrier_key crosswalk (Ticket 053)
#
# Single source of truth for mapping texas_fr_contract_month_panel.csv's
# ISSUER_NAME to a priceable carrier_key (or IMPUTED). Sourced by
# 20_Build_Priced_Tank_Panel.R. Replaces every per-engine hardcoded
# ISSUER_MATCH constant (15b/16b/17b/18b/19b) — carrier assignment now
# happens ONCE, here.
#
# Researcher-confirmed 2026-07-02 (chat, ticket 053 clarifying-questions
# thread). Full distinct ISSUER_NAME list verified against the real
# texas_fr_contract_month_panel.csv (67 distinct strings, incl. blank).
#
# tier:
#   "priceable"        -> a carrier_key with a real rate engine (6 keys)
#   "imputed_carrier"  -> a real insurer with no engine (PM02 imputes)
#   "non_carrier"      -> self-insurance / guarantee / bank / utility / gov
#                         pool / other non-underwriter string (PM02 imputes)
# carrier_key is "IMPUTED" for both non-priceable tiers — tier is carried
# separately so a later PM02 step can exclude non_carrier rows if desired
# without re-deriving this table.
#
# Sources: library(data.table) only — no data, no I/O.
###############################################################################

library(data.table)

ISSUER_CROSSWALK <- data.table(
  issuer_name = c(
    # ── Priceable (6 carrier_keys) ──────────────────────────────────────────
    "MID-CONTINENT INS CO",
    "TANK OWNERS MEMBERS INS CO",
    "ZURICH AMERICAN INS CO",
    "STEADFAST INS CO",
    "CHARTIS SPECIALTY INS CO",
    "AMERICAN INTL SPECIALTY LINES",
    "AIG INS CO",
    "COMMERCE & INDUSTRY INS CO",
    "NATL UNION FIRE INS CO",
    "NEW HAMPSHIRE INS CO",
    "LEXINGTON INS CO",
    "ACE AMERICAN INS CO",
    "ILLINOIS UNION INS CO",
    "WESTCHESTER FIRE INS CO",
    "CHUBB CUSTOM INS CO",
    "ACE SAFETY INDEM CO",
    "GREAT AMERICAN ALLIANCE INS CO",
    # ── IMPUTED / tier=imputed_carrier (real insurer, no engine) ────────────
    "IRONSHORE SPECIALTY INS CO",
    "COLONY INS CO",
    "OLD REPUBLIC INS CO",
    "CRUM & FORSTER SPECIALTY INS",
    "INDIAN HARBOR INS CO",
    "NAUTILUS INS CO",
    "TUDOR INS CO",
    "AMERICAN SAFETY INDEM CO",
    "NAVIGATORS INS CO",
    "LIBERTY SURPLUS INS CO",
    "HUDSON SPECIALTY INS CO",
    "STICO MUTUAL INS CO",
    "ADMIRAL INS CO",
    "SCOTTSDALE INS CO",
    "AEGIS INS SVCS CO",
    "BEAZLEY INS CO",
    "ALLIED WORLD ASSURANCE CO",
    "LIBERTY INTL UNDERWRITERS",
    "TRAVELERS CAS & SUR CO AMER",
    "ENDURANCE AMER SPECIALTY INS",
    "AMWINS PGM UNDERWRITERS",
    "CHEROKEE INS CO",
    "GREENWICH INS CO",
    "MT HAWLEY INS CO",
    "PROFESSIONAL LIABILITY INS CO",
    "NATIONWIDE INS CO",
    "AMERICAN SAFETY CASUALTY",
    "STATE FARM GENERAL INS CO",
    "BURLINGTON INS CO",
    "CENTURY SURETY INS CO",
    "MONTPELIER INS CO",
    "FARMERS INS EXCH",
    "HALLMARK SPECIALTY INS CO",
    "SENECA INS CO",
    # ── IMPUTED / tier=non_carrier (not an underwriting carrier) ────────────
    "SELF(FIN TEST OR LOCAL GOV FT)",
    "OTHER",
    "PARENT (GUARANTEE)",
    "",
    "SWANTER & GORDON",
    "ASSOC ELEC & GAS INS SVCS LTD",
    "FED RURAL ELEC INS EXCH",
    "ALLIED INS SVCS INC",
    "ENTERGY TEXAS INC",
    "BANCA INTESA",
    "GLOBAL FIN & INS SVCS",
    "AEP TEXAS NORTH CO",
    "TX MUNI LEAGUE INTERGOV",
    "TX ASSOC OF COUNTIES",
    "JP MORGAN CHASE BANK NA",
    "BANK OF AMERICA"
  ),
  carrier_key = c(
    # priceable (17 issuer strings -> 6 keys)
    "MID_CONTINENT", "TOMICS", "ZURICH", "ZURICH",
    "AIG", "AIG", "AIG", "AIG", "AIG", "AIG", "AIG",
    "ACE", "ACE", "ACE", "ACE", "ACE",
    "GREAT_AMERICAN",
    # imputed_carrier (34 issuer strings -> IMPUTED)
    rep("IMPUTED", 34L),
    # non_carrier (16 issuer strings -> IMPUTED)
    rep("IMPUTED", 16L)
  ),
  tier = c(
    rep("priceable", 17L),
    rep("imputed_carrier", 34L),
    rep("non_carrier", 16L)
  )
)

stopifnot(nrow(ISSUER_CROSSWALK) == 67L)
stopifnot(!anyDuplicated(ISSUER_CROSSWALK$issuer_name))
stopifnot(all(ISSUER_CROSSWALK$tier %in% c("priceable", "imputed_carrier", "non_carrier")))
stopifnot(all(ISSUER_CROSSWALK[tier == "priceable", carrier_key] %in%
              c("MID_CONTINENT", "TOMICS", "ZURICH", "AIG", "ACE", "GREAT_AMERICAN")))
stopifnot(all(ISSUER_CROSSWALK[tier != "priceable", carrier_key] == "IMPUTED"))

###############################################################################
## standardize_issuer_to_carrier — resolve ISSUER_NAME -> carrier_key/tier   ##
## Hard-stops on ANY value not in ISSUER_CROSSWALK (fleet-wide safety net —  ##
## catches future FR-data drift, e.g. a new issuer string, instead of        ##
## silently mis-assigning it to IMPUTED). Case/whitespace-normalized match.  ##
###############################################################################
standardize_issuer_to_carrier <- function(issuer_name_vec) {
  key <- toupper(trimws(issuer_name_vec))
  idx <- match(key, ISSUER_CROSSWALK$issuer_name)

  unresolved <- unique(key[is.na(idx)])
  if (length(unresolved) > 0L) {
    counts <- table(key[key %in% unresolved])
    stop(sprintf(
      paste0(
        "standardize_issuer_to_carrier: %d unmapped ISSUER_NAME value(s) not in ",
        "ISSUER_CROSSWALK:\n%s\n",
        "Add each to ISSUER_CROSSWALK (issuer_crosswalk.R) with the researcher's ",
        "explicit carrier_key/tier before re-running — do not guess."),
      length(unresolved),
      paste(sprintf("  %-40s n=%d", names(counts), as.integer(counts)), collapse = "\n")
    ))
  }

  data.table(
    issuer_name = issuer_name_vec,
    carrier_key = ISSUER_CROSSWALK$carrier_key[idx],
    tier        = ISSUER_CROSSWALK$tier[idx]
  )
}

###############################################################################
## write_issuer_crosswalk_qc — resolve a real ISSUER_NAME vector, write the  ##
## QC crosswalk CSV (issuer_name, carrier_key, tier, n_rows), print the      ##
## resolved carrier_key x n_rows table. Returns the resolved data.table      ##
## (one row per input row, for the caller to cbind/merge back in).           ##
###############################################################################
write_issuer_crosswalk_qc <- function(issuer_name_vec, qc_path) {
  resolved <- standardize_issuer_to_carrier(issuer_name_vec)

  qc <- resolved[, .(n_rows = .N), by = .(issuer_name, carrier_key, tier)]
  setorder(qc, carrier_key, -n_rows)
  fwrite(qc, qc_path)
  cat(sprintf("  Issuer crosswalk QC: %s (%d distinct issuer strings)\n",
              qc_path, nrow(qc)))
  cat("  Resolved carrier_key x n_rows:\n")
  print(resolved[, .(n_rows = .N), by = carrier_key][order(-n_rows)])

  resolved
}

###############################################################################
## Self-test (runs at source time)                                           ##
###############################################################################
cat("=== issuer_crosswalk self-test ===\n")

stopifnot(standardize_issuer_to_carrier("MID-CONTINENT INS CO")$carrier_key == "MID_CONTINENT")
stopifnot(standardize_issuer_to_carrier("chubb custom ins co")$carrier_key == "ACE")
stopifnot(standardize_issuer_to_carrier(" ZURICH AMERICAN INS CO ")$carrier_key == "ZURICH")
stopifnot(standardize_issuer_to_carrier("STEADFAST INS CO")$carrier_key == "ZURICH")
stopifnot(standardize_issuer_to_carrier("LEXINGTON INS CO")$carrier_key == "AIG")
stopifnot(standardize_issuer_to_carrier("GREAT AMERICAN ALLIANCE INS CO")$carrier_key == "GREAT_AMERICAN")
stopifnot(standardize_issuer_to_carrier("SELF(FIN TEST OR LOCAL GOV FT)")$carrier_key == "IMPUTED")
stopifnot(standardize_issuer_to_carrier("SELF(FIN TEST OR LOCAL GOV FT)")$tier == "non_carrier")
stopifnot(standardize_issuer_to_carrier("IRONSHORE SPECIALTY INS CO")$tier == "imputed_carrier")
stopifnot(standardize_issuer_to_carrier("")$carrier_key == "IMPUTED")
cat("  known-issuer resolution OK\n")

# Vectorized: mixed known + unknown does NOT silently drop the unknown one
multi <- standardize_issuer_to_carrier(c("MID-CONTINENT INS CO", "ACE AMERICAN INS CO"))
stopifnot(nrow(multi) == 2L)
stopifnot(all(multi$carrier_key == c("MID_CONTINENT", "ACE")))
cat("  vectorized multi-row resolution OK\n")

# Hard-stop guard: an unmapped issuer string halts rather than defaulting
err <- tryCatch(standardize_issuer_to_carrier("TOTALLY MADE UP ISSUER XYZ"),
                error = function(e) e)
stopifnot(inherits(err, "error"))
stopifnot(grepl("unmapped ISSUER_NAME", conditionMessage(err)))
cat("  unmapped-issuer hard-stop OK\n")

# SWANTER & GORDON (13 rows in the real FR panel, 2026-07-02 diagnostic) was
# the live real-data trigger for this guard during 053 development -- it was
# genuinely uncovered by the researcher's three tier lists at the time. Now
# RESOLVED (researcher, 2026-07-02): tier=non_carrier, does not read as an
# underwriting carrier, immaterial at 13 rows. Confirm it resolves cleanly.
stopifnot(standardize_issuer_to_carrier("SWANTER & GORDON")$carrier_key == "IMPUTED")
stopifnot(standardize_issuer_to_carrier("SWANTER & GORDON")$tier == "non_carrier")
cat("  SWANTER & GORDON resolves to IMPUTED/non_carrier (researcher-confirmed 2026-07-02) OK\n")

cat("issuer_crosswalk self-test PASS\n")
