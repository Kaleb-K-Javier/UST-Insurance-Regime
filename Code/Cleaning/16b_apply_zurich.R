###############################################################################
# 16b_apply_zurich.R — Zurich premium: read shared panel, price, write output
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Zurich American Insurance Company
# CARRIER_KEY:    "ZURICH"
# RATE STATE:     Iowa (applied to TX tanks as Iowa-proxy; territory = 1.00)
#
# TICKET 053 REFACTOR: no longer loads raw_pst_ust.csv / panel_dt.csv /
# texas_compartment.csv / the FR contract panel / TX_LUST.csv / texas_
# facility.csv itself. Reads Data/Analysis/rate_engines/
# _priced_tank_panel.csv (built ONCE by 20_Build_Priced_Tank_Panel.R),
# filters to carrier == "ZURICH", calls price_zurich_tank() (16a), rolls up,
# writes the 4 files.
#
# COVERAGE LIMIT: $1M/$1M occ/agg held fixed — ILF = 1.000 (reference,
#                 not a lookup — Zurich's own filing convention, unlike
#                 TOMICS which varies ILF by actual contract limits).
#
# RATE ERA:       Assigned from the shared panel's representative eff_date
#                 (earliest FR contract-month EFF_DATE observed that
#                 (facility, year, carrier) — see 20_'s STEP 3):
#                   EFF_DATE < 2008-11-01              -> r2005
#                   2008-11-01 <= EFF_DATE < 2010-03-01 -> r2008
#                   2010-03-01 <= EFF_DATE < 2011-10-01 -> r2010
#                   EFF_DATE >= 2011-10-01               -> r2011
#
# SCHEDULE:       Held at 0% -> standard_prem; +/-40% band -> min/max
#                 (ticket 048 Part C: TX-written policies use +/-40%, not
#                 Iowa's filed +/-25% schedule-rating cap).
#
# LOCATION FACTORS DROPPED FROM FILE 1 TOO (researcher-confirmed 2026-07-02,
#                 053 clarifying-Q answer): zurich_loc_type_load() and
#                 zurich_prior_release_load() are still CALLED (same pure
#                 a-engine functions, unchanged math — no re-litigating
#                 16a), but on REFERENCE inputs (fac_type = "" -> the
#                 default/0% branch; had_prior_lust = FALSE, lust_open =
#                 FALSE -> 0%) rather than joining texas_facility.csv /
#                 TX_LUST.csv. Both factors are therefore no-op multipliers
#                 (x1.00) in File 1's real bill now — this IS a behavior
#                 change vs. the pre-053 16b (which computed real loads from
#                 those two joins); confirmed acceptable because PM02 reads
#                 File 1 for (panel_id, panel_year, carrier) assignment only,
#                 never the premium levels. zurich_size_credit() (the
#                 multi-tank count credit, File 3) is UNCHANGED — that is
#                 not a location factor and stays keyed on the carrier's
#                 actual observed tank count.
#
# CORROSION PROTECTION: corr_present now sources cathodic-protection status
#                 from the shared panel's `has_cp` (20_'s auto-detected
#                 CORR_TANK_CP scheme, Y/N vs C/E/B/N/U) instead of a
#                 hardcoded `CORR_TANK_CP %in% c("C","E","B")` check — the
#                 pre-053 16b assumed the C/E/B/N/U scheme unconditionally
#                 and would have silently under-counted cathodic protection
#                 if the server data actually used Y/N (exactly the bug
#                 class 053 exists to catch). is_jacketed_tank/is_composite_
#                 tank/is_fiberglass_tank -> non-corrodible, unchanged.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/ — RATE_ENGINE_BUILD_TARGET.md):
#   ZURICH_facility_year_premium.csv, ZURICH_cell_era_card.csv,
#   ZURICH_multitank_credit.csv, ZURICH_min_premium.csv
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(here)

source(here("Code", "Cleaning", "16a_engine_zurich.R"))
# inherits: SCHED_CAP, POLICY_MIN_PREMIUM, era_from_eff_date, zurich_size_credit,
#           zurich_loc_type_load, zurich_prior_release_load, price_zurich_tank

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "16b_apply_zurich_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 16b_apply_zurich.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 16b_apply_zurich: Zurich premium apply (Ticket 053 shared-panel) ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "ZURICH"
out_dir     <- here("Data", "Analysis", "rate_engines")
panel_path  <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


###############################################################################
## STEP 1 — Read shared panel, filter to Zurich                             ##
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
## STEP 2 — Zurich-specific rating-input derivation                         ##
###############################################################################
cat("=== STEP 2: derive Zurich rating inputs ===\n")

ust_tm[, rate_era      := era_from_eff_date(eff_date)]
ust_tm[, double_walled := as.integer(wall == "DW")]
ust_tm[, has_leak_det  := as.integer(det_c_interstitial == 1L | det_p_interstitial == 1L)]
ust_tm[, corr_present  := (
  is_fiberglass_tank == 1L | is_composite_tank == 1L | is_jacketed_tank == 1L | has_cp == 1L
)]

cat("  rate_era distribution:\n"); print(ust_tm[, .N, by = rate_era][order(rate_era)])


###############################################################################
## STEP 3 — Price each tank-year via price_zurich_tank() (16a)              ##
###############################################################################
cat("=== STEP 3: price tank-years ===\n")

ust_tm[, tank_premium := price_zurich_tank(
  age_years       = age_years,
  double_walled   = double_walled,
  rate_era        = rate_era,
  capacity_gal    = capacity,
  has_leak_det    = has_leak_det,
  spill_comply_y  = spill_comply_y,
  corr_present    = corr_present,
  is_gasoline     = is_gasoline == 1L,
  is_diesel       = is_diesel == 1L,
  is_oil_kero     = is_oil_kerosene == 1L,
  is_jet_fuel     = is_jet_fuel == 1L
)]

n_na_prem <- sum(is.na(ust_tm$tank_premium))
if (n_na_prem > 0L) warning(sprintf("%d tank-years with NA tank_premium", n_na_prem))
cat(sprintf("  tank_premium: min=%.0f  median=%.0f  max=%.0f  NA=%d\n",
            min(ust_tm$tank_premium, na.rm = TRUE),
            median(ust_tm$tank_premium, na.rm = TRUE),
            max(ust_tm$tank_premium, na.rm = TRUE),
            n_na_prem))


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
## STEP 4 — Facility-year rollup: sum tanks, size credit (actual tank       ##
##          count), location/prior-release held at reference (see banner), ##
##          band + floor -> File 1                                         ##
###############################################################################
cat("=== STEP 4: facility-year rollup ===\n")

ust_tm[, n_total_tanks := .N, by = .(panel_id, panel_year)]

fac_year_raw <- ust_tm[, .(
  tank_sum      = sum(tank_premium, na.rm = TRUE),
  n_tanks_rated = uniqueN(ust_id),
  n_total_tanks = first(n_total_tanks)
), by = .(panel_id, panel_year)]

n_fac <- nrow(fac_year_raw)
fac_year_raw[, fac_raw :=
  tank_sum
  * (1 + zurich_size_credit(n_total_tanks))
  * (1 + zurich_loc_type_load(rep("", n_fac), n_total_tanks))          # reference fac_type -> 0% (see banner)
  * (1 + zurich_prior_release_load(rep(FALSE, n_fac), rep(FALSE, n_fac)))  # reference -> 0% (see banner)
]

fac_year_raw[, `:=`(
  standard_prem = pmax(fac_raw,                   POLICY_MIN_PREMIUM),
  min_prem      = pmax(fac_raw * (1 - SCHED_CAP), POLICY_MIN_PREMIUM),
  max_prem      = pmax(fac_raw * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)
)]
fac_year_raw[, carrier := CARRIER_KEY]

stopifnot(all(is.finite(fac_year_raw$standard_prem)))
stopifnot(all(fac_year_raw$min_prem      <= fac_year_raw$standard_prem))
stopifnot(all(fac_year_raw$standard_prem <= fac_year_raw$max_prem))
stopifnot(all(fac_year_raw$standard_prem >= POLICY_MIN_PREMIUM))

out1 <- fac_year_raw[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                          carrier, n_tanks_rated)]
setorder(out1, panel_id, panel_year)

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))


###############################################################################
## STEP 5 — File 3: multi-tank credit bands (from 16a's zurich_size_credit,##
##          computed not transcribed, to guarantee agreement with engine)   ##
###############################################################################
cat("=== STEP 5: multi-tank credit (File 3) ===\n")

band_edges <- data.table(
  n_min = c(1L, 4L, 7L, 11L, 16L, 26L, 46L),
  n_max = c(3L, 6L, 10L, 15L, 25L, 45L, 9999L)
)
band_edges[, credit_mult := 1 + zurich_size_credit(n_min)]

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

credit_path <- file.path(out_dir, paste0(CARRIER_KEY, "_multitank_credit.csv"))
fwrite(credit_tbl, credit_path)
cat(sprintf("  multi-tank credit: %s (%d rows)\n", credit_path, nrow(credit_tbl)))


###############################################################################
## STEP 6 — File 4: minimum premium ($425, p.Z-IA-2 §VI)                    ##
###############################################################################
cat("=== STEP 6: minimum premium (File 4) ===\n")

min_prem_tbl <- data.table(
  carrier         = CARRIER_KEY,
  panel_year      = active_years,
  min_premium_usd = POLICY_MIN_PREMIUM
)
setcolorder(min_prem_tbl, c("carrier", "panel_year", "min_premium_usd"))
stopifnot(all(min_prem_tbl$min_premium_usd > 0))

min_prem_path <- file.path(out_dir, paste0(CARRIER_KEY, "_min_premium.csv"))
fwrite(min_prem_tbl, min_prem_path)
cat(sprintf("  min premium: %s (%d rows)\n", min_prem_path, nrow(min_prem_tbl)))

cat(sprintf("\nRun completed: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("=== 16b DONE ===\n")
