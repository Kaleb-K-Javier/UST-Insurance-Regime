###############################################################################
# 19b_apply_great_american.R — Great American (STP Program) premium: read
#                                shared panel, price, write output
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Great American Insurance Company (STP Program)
# CARRIER_KEY:    "GREAT_AMERICAN"
# ISSUER MATCH:   RESOLVED (researcher-confirmed 2026-07-02, supersedes the
#                 pre-053 "PENDING CONFIRMATION" banner) — centralized in
#                 issuer_crosswalk.R. The pre-053 candidate string "GREAT
#                 AMERICAN INS CO" NEVER MATCHED any real ISSUER_NAME in the
#                 FR panel (confirmed via the real ISSUER_NAME distribution,
#                 2026-07-02) — the real string is "GREAT AMERICAN ALLIANCE
#                 INS CO" (182,126 contract-months). This script has never
#                 actually rated a single Great American contract until this
#                 fix. No sibling Great American pool NAICs are folded in
#                 (per the pre-053 NAIC CAUTION — not re-litigated, single
#                 exact-string match only).
#
# TICKET 053 REFACTOR: no longer loads raw_pst_ust.csv / panel_dt.csv / the
# FR contract panel itself. Reads Data/Analysis/rate_engines/
# _priced_tank_panel.csv, filters to carrier == "GREAT_AMERICAN" AND
# panel_year >= 2021 (GA's STP program is 2021+ only — re-implemented here
# explicitly since price_ga_tank() has no eff_date hard-stop of its own,
# same convention as the pre-053 script), calls price_ga_tank() (19a),
# rolls up, writes the 4 files.
#
# WALL: universal PM-state rule now (RATE_ENGINE_BUILD_TARGET.md invariant
#                 #6 — "Mixed-Wall follows the panel"): the shared panel's
#                 `wall` is ALWAYS "SW" or "DW" (SW iff mm_wall contains
#                 "single", else DW — Mixed-Wall/Unknown-Wall fall through to
#                 DW, same as every other engine). This SUPERSEDES the
#                 pre-053 19b's carrier-specific behavior of dropping Mixed-
#                 Wall/Unknown-Wall/unmatched tank-months from both File 1
#                 and File 2 — every insured GA tank-year now gets a
#                 construction code and prices into both files. This is the
#                 ticket's own explicit locked design (invariant #6), not a
#                 new judgment call made here.
#
# CONSTRUCTION / CONTENTS / LEAK / LINE-LEAK MAPPING: unchanged from the
#                 pre-053 19b (same HITL-flagged proposed mappings, see the
#                 pre-053 script's banner for the full raw-field rationale),
#                 now reading from the shared panel's finer-attr columns.
#                 has_cp comes from 20_'s centralized CP-scheme auto-detect
#                 (replaces this script's own now-redundant detect_cp_scheme).
#
# TANK CAPACITY: shared panel's `capacity` fed directly to price_ga_tank();
#                 tank-years with missing/out-of-[1,30000] capacity are
#                 DROPPED before pricing (capacity_factor_ga() hard-stops
#                 outside that filed range), drop count/pct printed.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   GREAT_AMERICAN_facility_year_premium.csv (min==standard==max, no
#     schedule band filed), GREAT_AMERICAN_cell_era_card.csv,
#   GREAT_AMERICAN_multitank_credit.csv, GREAT_AMERICAN_min_premium.csv
#   (= $225 flat, every year)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(here)

source(here("Code", "Cleaning", "19a_engine_great_american.R"))
# inherits: BASE_RATE_GA, MIN_PREMIUM_GA, REF_COVER_OCC, REF_COVER_AGG,
#           REF_DEDUCTIBLE, GA_MIN_FILING_DATE
# inherits: deductible_factor_ga, ilf_factor_ga, multiple_tank_factor_ga,
#           apply_floor_ga, price_ga_tank

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "19b_apply_great_american_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 19b_apply_great_american.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 19b_apply_great_american: GA premium apply (Ticket 053 shared-panel) ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "GREAT_AMERICAN"
out_dir     <- here("Data", "Analysis", "rate_engines")
panel_path  <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

REF_DED_ILF_FACTOR <- deductible_factor_ga(REF_DEDUCTIBLE) * ilf_factor_ga(REF_COVER_OCC, REF_COVER_AGG)
stopifnot(isTRUE(all.equal(REF_DED_ILF_FACTOR, 1.00, tolerance = 1e-8)))
cat(sprintf("  Reference deductible/ILF factor = %.4f (OK)\n", REF_DED_ILF_FACTOR))

min_premium_of_year <- function(panel_year) rep(MIN_PREMIUM_GA, length(panel_year))


###############################################################################
## STEP 1 — Read shared panel, filter to GA + 2021+ (GA's STP program is   ##
##          2021+ only — re-implemented explicitly, price_ga_tank() has no ##
##          eff_date hard-stop of its own)                                 ##
###############################################################################
cat("=== STEP 1: read shared panel, filter carrier + 2021+ ===\n")

if (!file.exists(panel_path))
  stop("Shared priced-tank panel not found at: ", panel_path,
       " — run 20_Build_Priced_Tank_Panel.R first.")

panel <- fread(panel_path)
ust_tm <- panel[carrier == CARRIER_KEY]
stopifnot(nrow(ust_tm) > 0L)

n_pre_2021 <- ust_tm[panel_year < 2021L, .N]
ust_tm <- ust_tm[panel_year >= 2021L]
cat(sprintf("  Dropped %d pre-2021 tank-years (GA's STP program is 2021+ only)\n", n_pre_2021))
stopifnot(nrow(ust_tm) > 0L)

cat(sprintf("  %s rows for carrier=%s (2021+) | %d facilities | years %d-%d\n",
            format(nrow(ust_tm), big.mark = ","), CARRIER_KEY,
            uniqueN(ust_tm$panel_id), min(ust_tm$panel_year), max(ust_tm$panel_year)))

# Capacity range QA/drop (capacity_factor_ga hard-stops outside [1,30000])
n_pre_cap <- nrow(ust_tm)
n_bad_cap <- ust_tm[is.na(capacity) | capacity < 1 | capacity > 30000, .N]
ust_tm <- ust_tm[!is.na(capacity) & capacity >= 1 & capacity <= 30000]
cat(sprintf("  Dropped %d / %d tank-years (%.2f%%) with missing/out-of-[1,30000] capacity\n",
            n_bad_cap, n_pre_cap, 100 * n_bad_cap / n_pre_cap))
stopifnot(nrow(ust_tm) > 0L)


###############################################################################
## STEP 2 — Assign construction / contents / leak / line-leak codes         ##
###############################################################################
cat("=== STEP 2: assign rating codes ===\n")

map_ga_construction_code <- function(wall, is_fiberglass, is_composite, has_cp) {
  fcase(
    is_composite == 1L,                              "FRP",
    is_fiberglass == 1L & wall == "SW",               "SWF",
    is_fiberglass == 1L & wall == "DW",               "DWF",
    wall == "DW",                                      "DWS",
    wall == "SW" & has_cp == 1L,                        "SWSC",
    wall == "SW" & has_cp == 0L,                        "SWS",
    default = NA_character_
  )
}
ust_tm[, construction_code := map_ga_construction_code(
  wall, is_fiberglass_tank, is_composite_tank, has_cp)]
stopifnot(!anyNA(ust_tm$construction_code))
cat("  construction_code distribution:\n"); print(ust_tm[, .N, by = construction_code][order(-N)])

map_ga_contents_code <- function(is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel) {
  fcase(
    is_gasoline == 1L,     "GASOLINE",
    is_diesel == 1L,       "DIESEL",
    is_jet_fuel == 1L,     "AVIATION_FUEL",
    is_oil_kerosene == 1L, "KEROSENE",
    default = "GASOLINE"
  )
}
ust_tm[, contents_code := map_ga_contents_code(is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel)]
cat("  contents_code distribution:\n"); print(ust_tm[, .N, by = contents_code][order(-N)])

map_ga_leak_code <- function(sec_cont, interstitial, atg, sir, vapor, gw) {
  fcase(
    sec_cont == 1L,      "CONTINUOUS_IN_TANK",
    interstitial == 1L,  "INTERSTITIAL",
    atg == 1L,            "ATG",
    sir == 1L,             "SIR",
    vapor == 1L,            "VAPOR",
    gw == 1L,                "GROUNDWATER",
    default = "MANUAL"
  )
}
ust_tm[, leak_code := map_ga_leak_code(
  det_c_sec_cont, det_c_interstitial, det_c_atg, det_c_sir, det_c_vapor, det_c_gw)]
cat("  leak_code distribution:\n"); print(ust_tm[, .N, by = leak_code][order(-N)])

ust_tm[, line_leak_code := fifelse(det_p_lld == 1L, "YES", "NO")]
cat("  line_leak_code distribution:\n"); print(ust_tm[, .N, by = line_leak_code][order(-N)])


###############################################################################
## STEP 3 — Price each tank-year via price_ga_tank() (19a)                  ##
###############################################################################
cat("=== STEP 3: price tank-years ===\n")

ust_tm[, tank_premium := price_ga_tank(
  capacity_gal = capacity, age_years = age_years,
  construction_code = construction_code, contents_code = contents_code,
  leak_code = leak_code, line_leak_code = line_leak_code
)]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))
cat(sprintf("  tank_premium: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium), min(ust_tm$tank_premium), max(ust_tm$tank_premium)))


###############################################################################
## STEP 3b — File 2: cell-year card                                         ##
###############################################################################
cat("=== STEP 3b: build cell-year card ===\n")

cell_card <- ust_tm[, .(premium_usd_per_tank_yr = mean(tank_premium, na.rm = TRUE)),
                     by = .(wall, age_bin, panel_year)]
cell_card[, carrier := CARRIER_KEY]
setcolorder(cell_card, c("carrier", "wall", "age_bin", "panel_year", "premium_usd_per_tank_yr"))

stopifnot(all(is.finite(cell_card$premium_usd_per_tank_yr)))
stopifnot(all(cell_card$premium_usd_per_tank_yr > 0))
stopifnot(all(cell_card$age_bin %in% 1:8))
stopifnot(all(cell_card$wall %in% c("SW", "DW")))

card_path <- file.path(out_dir, paste0(CARRIER_KEY, "_cell_era_card.csv"))
fwrite(cell_card, card_path)
cat(sprintf("  cell-year card: %s (%d rows)\n", card_path, nrow(cell_card)))


###############################################################################
## STEP 4 — Facility-year rollup: Multiple Tank Factor, floor -> File 1     ##
##          (NO schedule band -> min = standard = max, per 19a)             ##
###############################################################################
cat("=== STEP 4: facility-year rollup ===\n")

fac_year <- ust_tm[, .(
  fac_unmodified = sum(tank_premium, na.rm = TRUE),
  n_tanks_rated  = uniqueN(ust_id)
), by = .(panel_id, panel_year)]

fac_year[, mtf := multiple_tank_factor_ga(n_tanks_rated)]
fac_year[, fac_standard := fac_unmodified * mtf]
fac_year[, floor_y := min_premium_of_year(panel_year)]

fac_year[, `:=`(
  min_prem      = pmax(fac_standard, floor_y),
  standard_prem = pmax(fac_standard, floor_y),
  max_prem      = pmax(fac_standard, floor_y)
)]

stopifnot(all(is.finite(fac_year$standard_prem)))
stopifnot(all(fac_year$standard_prem >= fac_year$floor_y))
stopifnot(all(fac_year$min_prem == fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem == fac_year$max_prem))

fac_year[, carrier := CARRIER_KEY]

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated)]
setorder(out1, panel_id, panel_year)

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))


###############################################################################
## STEP 5 — File 3: Multiple Tank Factor bands. Top band extends the filed  ##
##          101-200's 0.700 flat to 9999 — ASSUMPTION, flagged (19a).       ##
###############################################################################
cat("=== STEP 5: Multiple Tank Factor bands ===\n")

band_edges <- data.table(
  n_min = c(1L, 11L, 26L, 51L, 101L),
  n_max = c(10L, 25L, 50L, 100L, 9999L)
)
band_edges[, credit_mult := multiple_tank_factor_ga(n_min)]

active_years <- sort(unique(out1$panel_year))
credit_tbl <- CJ(panel_year = active_years, n_min = band_edges$n_min)[
  band_edges, on = "n_min"
][, carrier := CARRIER_KEY]
setcolorder(credit_tbl, c("carrier", "panel_year", "n_min", "n_max", "credit_mult"))
setorder(credit_tbl, panel_year, n_min)

for (yr in active_years) {
  yb <- credit_tbl[panel_year == yr][order(n_min)]
  n_bands <- nrow(yb)
  stopifnot(yb$n_min[1L] == 1L, yb$n_max[n_bands] == 9999L)
  stopifnot(all(yb$n_min[-1L] == yb$n_max[-n_bands] + 1L))
}

out3_path <- file.path(out_dir, paste0(CARRIER_KEY, "_multitank_credit.csv"))
fwrite(credit_tbl, out3_path)
cat(sprintf("  Multiple Tank Factor bands: %s (%d rows)\n", out3_path, nrow(credit_tbl)))


###############################################################################
## STEP 6 — File 4: minimum premium per active year (flat $225, no era)     ##
###############################################################################
cat("=== STEP 6: minimum premium per year ===\n")

min_prem_tbl <- data.table(panel_year = active_years)
min_prem_tbl[, min_premium_usd := min_premium_of_year(panel_year)]
min_prem_tbl[, carrier := CARRIER_KEY]
setcolorder(min_prem_tbl, c("carrier", "panel_year", "min_premium_usd"))

stopifnot(all(min_prem_tbl$min_premium_usd > 0))

out4_path <- file.path(out_dir, paste0(CARRIER_KEY, "_min_premium.csv"))
fwrite(min_prem_tbl, out4_path)
cat(sprintf("  min premium: %s (%d rows)\n", out4_path, nrow(min_prem_tbl)))

cat(sprintf("\nRun completed: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=== 19b DONE ===\n")
