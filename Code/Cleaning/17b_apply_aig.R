###############################################################################
# 17b_apply_aig.R — AIG (Commerce & Industry Insurance Company) premium:
#                    read shared panel, price, write output
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Commerce & Industry Insurance Company (AIG)
# CARRIER_KEY:    "AIG"
# ISSUER POOLING: now handled centrally by issuer_crosswalk.R / 20_'s STEP 3
#                 — SEVEN issuer strings pool to AIG (expanded 2026-07-02 from
#                 the original three): "CHARTIS SPECIALTY INS CO",
#                 "AIG INS CO", "COMMERCE & INDUSTRY INS CO",
#                 "AMERICAN INTL SPECIALTY LINES", "NATL UNION FIRE INS CO",
#                 "NEW HAMPSHIRE INS CO", "LEXINGTON INS CO" — all priced
#                 with this one Commerce & Industry manual (researcher-
#                 confirmed 2026-07-02; supersedes the pre-053 3-issuer pool).
#                 File 1 still flags every AIG row pooled_manual_flag=1L /
#                 manual_note so a downstream PM08_REAL_ENGINE_ONLY switch
#                 can re-fit excluding pooled-manual carriers.
#
# TICKET 053 REFACTOR: no longer loads raw_pst_ust.csv / panel_dt.csv / the
# FR contract panel itself. Reads Data/Analysis/rate_engines/
# _priced_tank_panel.csv (built ONCE by 20_Build_Priced_Tank_Panel.R),
# filters to carrier == "AIG", calls price_aig_tank() (17a), rolls up,
# writes the 4 files.
#
# COVERAGE LIMIT / DEDUCTIBLE: $1,000,000/$1,000,000 + $5,000 deductible held
#                 at reference for BOTH File 1 and File 2 (036-locked
#                 invariant 1). LIMIT_DED_FACTOR asserted == 1.00 below.
#
# CONSTRUCTION / LEAK / PIPING CODE MAPPING: unchanged from the pre-053 17b
#                 (map_aig_construction_code / map_aig_leak_code / map_aig_
#                 pipe_construction_code / map_aig_pipe_leak_code below,
#                 copied verbatim) — same HITL-flagged proposed mappings,
#                 now reading from the shared panel's finer-attr columns
#                 instead of re-deriving them from raw_pst_ust.csv. wall
#                 comes from the shared panel's `wall` (mm_wall-sourced),
#                 same single source of truth as every other engine.
#
# 2015 FILING SWITCH: era assigned from the shared panel's representative
#                 eff_date (era_of_date_aig()) — the earliest FR contract-
#                 month EFF_DATE observed that (facility, year, AIG). This
#                 is a YEAR-grain simplification vs. the pre-053 script's
#                 exact tank-MONTH grain (which could split a single
#                 calendar year's contract into OLD-then-NEW sub-periods);
#                 the shared panel's grain is (tank, panel_year, carrier)
#                 per Ticket 053's design, so one era per tank-year.
#
# MINIMUM PREMIUM FLOOR: panel_year >= 2015 -> $525 (NEW), else $500 (OLD) —
#                 unchanged from the pre-053 PROPOSED rule (min_premium_of_
#                 year()), still a calendar-year simplification for 2015
#                 itself, still flagged.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   AIG_facility_year_premium.csv (File 1 — + pooled_manual_flag, manual_note)
#   AIG_cell_era_card.csv, AIG_multitank_credit.csv, AIG_min_premium.csv
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(here)

source(here("Code", "Cleaning", "17a_engine_aig.R"))
# inherits: SCHED_CAP, NEW_ERA_CUTOFF, MIN_PREMIUM_NEW, MIN_PREMIUM_OLD,
#           REF_COVER_OCC, REF_COVER_AGG, REF_DEDUCTIBLE
# inherits: era_of_date_aig, ilf_aig, deductible_credit_aig,
#           multi_tank_credit_aig, price_aig_tank

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "17b_apply_aig_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 17b_apply_aig.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 17b_apply_aig: AIG premium apply (Ticket 053 shared-panel) ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "AIG"
out_dir     <- here("Data", "Analysis", "rate_engines")
panel_path  <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

LIMIT_DED_FACTOR <- ilf_aig(REF_COVER_OCC, REF_COVER_AGG) - deductible_credit_aig(REF_DEDUCTIBLE)
stopifnot(isTRUE(all.equal(LIMIT_DED_FACTOR, 1.00, tolerance = 1e-8)))
cat(sprintf("  Invariant-1 check: LIMIT_DED_FACTOR at reference contract = %.4f (OK)\n",
            LIMIT_DED_FACTOR))

min_premium_of_year <- function(panel_year) {
  fifelse(panel_year >= 2015L, MIN_PREMIUM_NEW, MIN_PREMIUM_OLD)
}


###############################################################################
## STEP 1 — Read shared panel, filter to AIG                                ##
###############################################################################
cat("=== STEP 1: read shared panel, filter carrier ===\n")

if (!file.exists(panel_path))
  stop("Shared priced-tank panel not found at: ", panel_path,
       " — run 20_Build_Priced_Tank_Panel.R first.")

panel <- fread(panel_path)
ust_tm <- panel[carrier == CARRIER_KEY]
stopifnot(nrow(ust_tm) > 0L)
cat(sprintf("  %s rows for carrier=%s | %d facilities | years %d-%d\n",
            format(nrow(ust_tm), big.mark = ","), CARRIER_KEY,
            uniqueN(ust_tm$panel_id), min(ust_tm$panel_year), max(ust_tm$panel_year)))


###############################################################################
## STEP 2 — AIG-specific rating-code mapping (unchanged mapping logic,      ##
##          copied verbatim from the pre-053 17b — see its HITL FLAGS)      ##
###############################################################################
cat("=== STEP 2: derive AIG rating codes ===\n")

ust_tm[, era := era_of_date_aig(eff_date)]

map_aig_construction_code <- function(wall, composite, frp, steel, has_cp) {
  fcase(
    wall == "DW",                  "DW",
    composite == 1L | frp == 1L,   "FIBERGLASS_CLAD",
    steel == 1L & has_cp == 1L,    "CATHODIC",
    default = "BARE_STEEL"
  )
}
ust_tm[, construction_code := map_aig_construction_code(
  wall, is_composite_tank, is_fiberglass_tank, is_steel_tank, has_cp)]

map_aig_leak_code <- function(interstitial, atg, gw, vapor, sir) {
  fcase(
    interstitial == 1L,       "INTERSTITIAL",
    atg == 1L,                 "ATG",
    gw == 1L | vapor == 1L,    "MONITORING_WELLS",
    sir == 1L,                 "SIR",
    default = "NONE"
  )
}
ust_tm[, leak_code := map_aig_leak_code(det_c_interstitial, det_c_atg, det_c_gw, det_c_vapor, det_c_sir)]

map_aig_pipe_construction_code <- function(pip_double, pip_jacketed, pip_frp, pip_flex, pip_steel, pip_has_cp) {
  fcase(
    pip_double == 1L | pip_jacketed == 1L, "DW_PIPING",
    pip_frp == 1L | pip_flex == 1L,        "FIBERGLASS_PIPING",
    pip_steel == 1L & pip_has_cp == 1L,    "CATHODIC_PIPING",
    default = "BARE_STEEL_PIPING"
  )
}
ust_tm[, pipe_construction_code := map_aig_pipe_construction_code(
  pip_double, pip_jacketed, pip_frp, pip_flex, pip_steel, pip_has_cp)]

map_aig_pipe_leak_code <- function(interstitial, lld, gw, vapor, pipe_type, suction_exempt) {
  fcase(
    interstitial == 1L,                          "INTERSTITIAL",
    lld == 1L,                                    "MECHANICAL_LLD",
    gw == 1L | vapor == 1L,                       "EXTERNAL_WELLS",
    pipe_type == "Suction" | suction_exempt == 1L, "SUCTION_CHECK_VALVE",
    default = "NONE"
  )
}
ust_tm[, pipe_leak_code := map_aig_pipe_leak_code(
  det_p_interstitial, det_p_lld, det_p_gw, det_p_vapor, pipe_type, det_p_suction_exempt)]

cat("  construction_code distribution:\n");     print(ust_tm[, .N, by = construction_code][order(-N)])
cat("  leak_code distribution:\n");              print(ust_tm[, .N, by = leak_code][order(-N)])
cat("  pipe_construction_code distribution:\n"); print(ust_tm[, .N, by = pipe_construction_code][order(-N)])
cat("  pipe_leak_code distribution:\n");          print(ust_tm[, .N, by = pipe_leak_code][order(-N)])


###############################################################################
## STEP 3 — Price each tank-year via price_aig_tank() (17a)                 ##
###############################################################################
cat("=== STEP 3: price tank-years (tank_unmodified) ===\n")

ust_tm[, tank_premium := price_aig_tank(
  age_years = age_years, era = era, construction_code = construction_code,
  leak_code = leak_code, pipe_construction_code = pipe_construction_code,
  pipe_leak_code = pipe_leak_code
)]

stopifnot(all(is.finite(ust_tm$tank_premium)))
stopifnot(all(ust_tm$tank_premium >= 0))
cat(sprintf("  tank_premium: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_premium), min(ust_tm$tank_premium), max(ust_tm$tank_premium)))


###############################################################################
## STEP 3b — File 2: cell-year card                                         ##
###############################################################################
cat("=== STEP 3b: build cell-year card ===\n")

cell_card <- ust_tm[!is.na(wall), .(premium_usd_per_tank_yr = mean(tank_premium, na.rm = TRUE)),
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
## STEP 4 — Facility-year rollup: multi-tank credit off actual observed     ##
##          tank count, band + floor -> File 1                             ##
###############################################################################
cat("=== STEP 4: facility-year rollup, credit, band, floor ===\n")

fac_year <- ust_tm[, .(
  fac_unmodified = sum(tank_premium, na.rm = TRUE),
  n_tanks_rated  = uniqueN(ust_id)
), by = .(panel_id, panel_year)]

fac_year[, iv_credit := multi_tank_credit_aig(n_tanks_rated)]
fac_year[, fac_credited := fac_unmodified * (1 - iv_credit) * LIMIT_DED_FACTOR]
fac_year[, floor_y := min_premium_of_year(panel_year)]

fac_year[, `:=`(
  min_prem      = pmax(fac_credited * (1 - SCHED_CAP), floor_y),
  standard_prem = pmax(fac_credited,                    floor_y),
  max_prem      = pmax(fac_credited * (1 + SCHED_CAP), floor_y)
)]

stopifnot(all(fac_year$min_prem      >= fac_year$floor_y))
stopifnot(all(fac_year$standard_prem >= fac_year$floor_y))
stopifnot(all(fac_year$max_prem      >= fac_year$floor_y))
stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem))

fac_year[, carrier := CARRIER_KEY]
fac_year[, pooled_manual_flag := 1L]
fac_year[, manual_note        := "AIG group, C&I manual"]

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated, pooled_manual_flag, manual_note)]
setorder(out1, panel_id, panel_year)

stopifnot(all(out1$pooled_manual_flag == 1L))
stopifnot(all(out1$manual_note == "AIG group, C&I manual"))

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))


###############################################################################
## STEP 5 — File 3: multi-tank credit bands (from 17a's ladder, computed   ##
##          not transcribed, to guarantee agreement with the engine)        ##
###############################################################################
cat("=== STEP 5: multi-tank credit bands ===\n")

band_edges <- data.table(
  n_min = c(1L, 6L, 15L, 30L, 50L, 75L, 100L, 150L),
  n_max = c(5L, 14L, 29L, 49L, 74L, 99L, 149L, 9999L)
)
band_edges[, credit_mult := 1 - multi_tank_credit_aig(n_min)]

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
cat(sprintf("  multi-tank credit: %s (%d rows)\n", out3_path, nrow(credit_tbl)))


###############################################################################
## STEP 6 — File 4: minimum premium per active year                        ##
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
cat("=== 17b DONE ===\n")
