###############################################################################
# 21b_apply_mid_continent.R — Mid-Continent Casualty premium: read shared
#                               panel, price, write output (Ticket 053)
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Mid-Continent Casualty Company
# CARRIER_KEY:    "MID_CONTINENT"
# ISSUER MATCH:   "MID-CONTINENT INS CO" (confirmed via the real ISSUER_NAME
#                 distribution, 2026-07-02 — 1,198,286 contract-months, by
#                 far the largest single issuer in the FR panel).
#
# TICKET 053 / TICKET 048 FIX: this is the FIRST time Mid-Continent is priced
# off ITS OWN FR contracts like every other carrier. The pre-053 pipeline
# (Code/Dynamic_Model/04a_TX_Premium_All_1999_onwards.R) imputed a Mid-
# Continent premium for EVERY TX facility regardless of actual carrier ("the
# all-TX 04a problem" — ticket 048). That script is UNTOUCHED by this ticket
# (still available for its documented 1999-2005 backward-extrapolation use
# case) — this script is a NEW, separate two-script engine (21a/21b) that
# prices only facilities the FR panel shows were ACTUALLY insured by
# Mid-Continent, reading from the shared priced-tank panel like the other
# five carriers.
#
# RATE MATH: byte-identical to 04a's four era tables (see 21a header for the
#            NA-handling deviation: hard-stop on unrecognized tank_status
#            instead of 04a's silent rowSums(na.rm=TRUE) zero).
#
# RATE ERA: assigned from the shared panel's representative eff_date (the
#            earliest FR contract-month EFF_DATE observed that (facility,
#            year, Mid-Continent)) via era_of_eff_date_midcont() (21a).
#
# ILF / DEDUCTIBLE / COVERAGE FORM: Mid-Continent's OWN filed ILF table
#            varies by ACTUAL contract cover_occ/cover_agg (same convention
#            as TOMICS, unlike AIG/ACE/GA/Zurich which hold at reference) —
#            the shared panel's cover_occ/cover_agg are used directly, with
#            NA -> $1M/$1M per ilf_factor_*_midcont()'s own fallback.
#            cover_form and deductible are NOT fields in the FR contract
#            panel at all (Mid-Continent's own filing captures them, the TX
#            FR data does not) -> held at the filing reference ("A" / $5,000)
#            for every row, matching 04a's synthetic-panel convention.
#
# TANK STATUS: from the shared panel's tank_status (raw STATUS -> canonical
#            label, derived once in 20_). Insured tanks should overwhelmingly
#            be "In Use"; any other/unrecognized value now hard-stops (21a's
#            deliberate deviation from 04a) rather than silently zeroing.
#
# CONSTRUCTION / LEAK / PIPING INPUTS: derived from the shared panel's finer
#            attrs (is_steel_tank, is_fiberglass_tank, has_cp, pip_*, det_c_*,
#            det_p_*) using the SAME field definitions 04a's Section 1 used —
#            is_steel_cathodic = is_steel_tank & has_cp (has_cp now via 20_'s
#            centralized CP-scheme auto-detect, not 04a's hardcoded Y/N
#            check — see 21a header); is_reinforced_fiberglass =
#            is_fiberglass_tank; is_double_walled_steel = (wall=="DW") &
#            is_steel_tank; det_interstitial/det_ATG/det_vapor/
#            det_groundwater combine BOTH the C (tank) and P (piping) side
#            flags, exactly as 04a's Section 1 items 8/9 did.
#
# MULTI-TANK CREDIT: NONE — neither 04a nor the 12_/13_/14_ era scripts it
#            mirrors define a facility-level tank-count discount (04a
#            Section 7). File 3 is a trivial 1..9999 = 1.0 band.
#
# MINIMUM PREMIUM: $500 — HITL FLAG, UNRESOLVED. Carried verbatim from 04a's
#            own Section 8 comment: "PROPOSED $500/facility-month floor, per
#            POLICY_MIN_PREMIUM <- 500 in 12_/13_/14_Rate_MidCont_*.R
#            (identical across all four era scripts). HITL FLAG: that
#            constant is never actually applied as a pmax() floor anywhere
#            in 04a's own fac_year rollup — confirm $500 against the
#            underlying filing before treating this file as authoritative."
#            This script DOES apply it as a pmax() floor (21a's
#            POLICY_MIN_PREMIUM, same convention as every other carrier) —
#            flagging clearly that the $500 figure itself is still
#            unverified against the actual Mid-Continent filing.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   MID_CONTINENT_facility_year_premium.csv, MID_CONTINENT_cell_era_card.csv,
#   MID_CONTINENT_multitank_credit.csv (trivial 1.0 band),
#   MID_CONTINENT_min_premium.csv (= $500 flat, HITL-flagged, every year)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(here)

source(here("Code", "Cleaning", "21a_engine_mid_continent.R"))
# inherits: BASE_RATE_MC, SCHED_CAP, POLICY_MIN_PREMIUM, REF_COVER_FORM,
#           REF_DEDUCTIBLE, era_of_eff_date_midcont, price_mid_continent_tank

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "21b_apply_mid_continent_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 21b_apply_mid_continent.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 21b_apply_mid_continent: Mid-Continent premium apply (Ticket 053) ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "MID_CONTINENT"
out_dir     <- here("Data", "Analysis", "rate_engines")
panel_path  <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


###############################################################################
## STEP 1 — Read shared panel, filter to Mid-Continent                      ##
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
## STEP 2 — Mid-Continent-specific rating-input derivation (04a Section 1,  ##
##          mirrored onto the shared panel's finer attrs)                   ##
###############################################################################
cat("=== STEP 2: derive Mid-Continent rating inputs ===\n")

ust_tm[, era := era_of_eff_date_midcont(eff_date)]

ust_tm[, is_steel_cathodic        := as.integer(is_steel_tank == 1L & has_cp == 1L)]
ust_tm[, is_reinforced_fiberglass := is_fiberglass_tank]
ust_tm[, is_double_walled_steel   := as.integer(wall == "DW" & is_steel_tank == 1L)]

ust_tm[, det_interstitial := as.integer(det_c_interstitial == 1L | det_p_interstitial == 1L)]
ust_tm[, det_ATG          := as.integer(det_c_atg == 1L | det_p_lld == 1L)]
ust_tm[, det_vapor        := as.integer(det_c_vapor == 1L | det_p_vapor == 1L)]
ust_tm[, det_groundwater  := as.integer(det_c_gw == 1L | det_p_gw == 1L)]

ust_tm[, pip_steel_cathodic := as.integer(pip_steel == 1L & pip_has_cp == 1L)]
ust_tm[, pip_fiberglass     := pip_frp]
ust_tm[, pip_double_walled  := pip_double]

# Deductible / coverage form: not fields in the FR contract panel — held at
# the filing reference for every row (04a's own synthetic-panel convention;
# see banner). cover_occ/cover_agg DO vary (Mid-Continent's own filed ILF
# table is contract-limit-dependent, same as TOMICS).
ust_tm[, cover_form := REF_COVER_FORM]
ust_tm[, deductible := REF_DEDUCTIBLE]

cat("  era distribution:\n"); print(ust_tm[, .N, by = era][order(era)])
cat("  tank_status distribution:\n"); print(ust_tm[, .N, by = tank_status][order(-N)])


###############################################################################
## STEP 3 — Price each tank-year via price_mid_continent_tank() (21a)       ##
###############################################################################
cat("=== STEP 3: price tank-years ===\n")

ust_tm[, tank_premium := price_mid_continent_tank(
  age_years = age_years, tank_status = tank_status, era = era,
  is_steel_cathodic = is_steel_cathodic,
  is_reinforced_fiberglass = is_reinforced_fiberglass,
  is_double_walled_steel = is_double_walled_steel,
  det_interstitial = det_interstitial, det_ATG = det_ATG,
  det_vapor = det_vapor, det_groundwater = det_groundwater,
  pip_steel_cathodic = pip_steel_cathodic, pip_fiberglass = pip_fiberglass,
  pip_double_walled = pip_double_walled,
  cover_occ = cover_occ, cover_agg = cover_agg,
  deductible = deductible, cover_form = cover_form
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
## STEP 4 — Facility-year rollup: sum tanks (NO multi-tank credit — see     ##
##          banner), band + floor -> File 1                                ##
###############################################################################
cat("=== STEP 4: facility-year rollup, band, floor ===\n")

fac_year <- ust_tm[, .(
  fac_raw       = sum(tank_premium, na.rm = TRUE),
  n_tanks_rated = uniqueN(ust_id)
), by = .(panel_id, panel_year)]

fac_year[, `:=`(
  standard_prem = pmax(fac_raw,                   POLICY_MIN_PREMIUM),
  min_prem      = pmax(fac_raw * (1 - SCHED_CAP), POLICY_MIN_PREMIUM),
  max_prem      = pmax(fac_raw * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)
)]
fac_year[, carrier := CARRIER_KEY]

stopifnot(all(is.finite(fac_year$standard_prem)))
stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem))
stopifnot(all(fac_year$standard_prem >= POLICY_MIN_PREMIUM))

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated)]
setorder(out1, panel_id, panel_year)

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))


###############################################################################
## STEP 5 — File 3: multi-tank credit (trivial — neither 04a nor the       ##
##          12_/13_/14_ era scripts define a facility tank-count discount) ##
###############################################################################
cat("=== STEP 5: multi-tank credit (File 3, trivial) ===\n")

active_years <- sort(unique(out1$panel_year))
credit_tbl <- data.table(
  carrier     = CARRIER_KEY,
  panel_year  = active_years,
  n_min       = 1L,
  n_max       = 9999L,
  credit_mult = 1.0
)
setcolorder(credit_tbl, c("carrier", "panel_year", "n_min", "n_max", "credit_mult"))
stopifnot(all(credit_tbl$n_min == 1L), all(credit_tbl$n_max == 9999L))

credit_path <- file.path(out_dir, paste0(CARRIER_KEY, "_multitank_credit.csv"))
fwrite(credit_tbl, credit_path)
cat(sprintf("  multi-tank credit: %s (%d rows)\n", credit_path, nrow(credit_tbl)))


###############################################################################
## STEP 6 — File 4: minimum premium ($500 — HITL FLAG, unresolved, see      ##
##          banner)                                                        ##
###############################################################################
cat("=== STEP 6: minimum premium (File 4) — $500 HITL FLAG, unresolved ===\n")

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
cat("=== 21b DONE ===\n")
