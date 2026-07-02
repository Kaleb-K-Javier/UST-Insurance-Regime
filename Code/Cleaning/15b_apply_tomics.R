###############################################################################
# 15b_apply_tomics.R — TOMICS premium: read shared panel, price, write output
# Filing:  SERFF TEXS-131241913  (eff. 2018-01-01)
# Carrier: Tank Owners Members Insurance Company
#
# TICKET 053 REFACTOR: no longer loads raw_pst_ust.csv / panel_dt.csv / the
# FR contract panel itself. Reads Data/Analysis/rate_engines/
# _priced_tank_panel.csv (built ONCE by 20_Build_Priced_Tank_Panel.R),
# filters to carrier == "TOMICS", calls price_tomics_tank() (15a), rolls up,
# writes the 4 files. All raw/panel/FR plumbing lives in 20_ now.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/ — RATE_ENGINE_BUILD_TARGET.md):
#   TOMICS_facility_year_premium.csv (File 1)
#   TOMICS_cell_era_card.csv         (File 2 — keyed on calendar panel_year.)
#   TOMICS_multitank_credit.csv      (File 3 — trivial: TOMICS filing has no
#     facility tank-count discount; one 1..9999 = 1.0 band per active year.)
#   TOMICS_min_premium.csv           (File 4 — $350 filing minimum, p.2, every
#     active year.)
###############################################################################

# ╔══════════════════════════════════════════════════════════════════════════════╗
# ║  BEHAVIORAL ASSUMPTIONS — TOMICS filing TEXS-131241913 (unchanged from     ║
# ║  the pre-053 15b — see 15a for the rating-table provenance)                ║
# ╠══════════════════════════════════════════════════════════════════════════════╣
# ║ A1  Policy limits: cover_occ/cover_agg from the shared panel (the FR       ║
# ║     contract's own limits) used DIRECTLY — TOMICS is the one carrier      ║
# ║     whose filed ILF table varies by the tank's ACTUAL contract limits,    ║
# ║     not the 036-locked reference (unlike AIG/ACE/GA/Zurich, which hold    ║
# ║     limits at reference throughout). NA/0 -> 1M/1M reference (ILF load    ║
# ║     0.000), per ilf_load_tomics()'s own fifelse default.                  ║
# ║ A2  Schedule rating: +/-40% band (p.3). standard_prem = schedule-neutral. ║
# ║     All three floored at $350 filing minimum policy premium (p.2).        ║
# ║ A3  Deductible / A4 defense sub-limit / A5 retro date / A6 site-capacity  ║
# ║     / A7 prior contamination: held at filing reference, 0.000 load each   ║
# ║     (not in the shared panel; not TOMICS rating inputs beyond the above). ║
# ║ A8  TOMICS 2nd program (filing 131214138) OUT OF SCOPE — one filed card   ║
# ║     applied to all TOMICS contract-years the shared panel carries.        ║
# ╚══════════════════════════════════════════════════════════════════════════════╝

library(data.table)
library(here)

source(here("Code", "Cleaning", "15a_engine_tomics.R"))
# inherits: SCHED_CAP, POLICY_MIN_PREMIUM, ENERGY_ACT_DATE
# inherits: base_rate_tomics, ilf_load_tomics, age_load_tomics,
#           leak_load_tomics, construction_load_tomics, pipe_load_tomics,
#           price_tomics_tank

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "15b_apply_tomics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 15b_apply_tomics.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 15b_apply_tomics: TOMICS premium apply (Ticket 053 shared-panel) ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "TOMICS"
out_dir     <- here("Data", "Analysis", "rate_engines")
panel_path  <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


###############################################################################
## STEP 1 — Read shared panel, filter to TOMICS                             ##
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
## STEP 2 — TOMICS-specific rating-code mapping (from the shared finer      ##
##          attrs — same mapping logic as the pre-053 script's STEP 1)      ##
###############################################################################
cat("=== STEP 2: derive TOMICS rating inputs ===\n")

ust_tm[, det_interstitial := as.integer(det_c_interstitial == 1L | det_p_interstitial == 1L)]
ust_tm[, is_composite     := as.integer(is_composite_tank == 1L | is_fiberglass_tank == 1L)]
ust_tm[, pip_dw_rigid     := as.integer(pip_double == 1L & pip_flex == 0L)]
ust_tm[, double_walled    := as.integer(wall == "DW")]

ust_tm[, base_category := fcase(
  tank_installed_date <  ENERGY_ACT_DATE,                                            "Upgrade",
  tank_installed_date >= ENERGY_ACT_DATE & tank_installed_date >  facility_min_install, "Replace",
  tank_installed_date >= ENERGY_ACT_DATE & tank_installed_date <= facility_min_install, "New",
  default = "Upgrade"   # NA tank_installed_date -> Upgrade
)]

cat("  base_category distribution:\n")
print(ust_tm[, .N, by = base_category][order(-N)])


###############################################################################
## STEP 3 — Price each tank-year via price_tomics_tank() (15a)              ##
###############################################################################
cat("=== STEP 3: price tank-years ===\n")

ust_tm[, tank_premium := price_tomics_tank(
  base_category    = base_category,
  COVER_OCC        = cover_occ,
  COVER_AGG        = cover_agg,
  age_years        = age_years,
  det_interstitial = det_interstitial,
  double_walled    = double_walled,
  is_composite     = is_composite,
  pip_dw_rigid     = pip_dw_rigid
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
## STEP 4 — Facility-year rollup (no month grain anymore — the shared panel ##
##          is already at (tank, panel_year) grain), band + floor -> File 1 ##
###############################################################################
cat("=== STEP 4: facility-year rollup, band, floor ===\n")

fac_year <- ust_tm[, .(
  standard_prem_pre = sum(tank_premium, na.rm = TRUE),
  n_tanks_rated      = uniqueN(ust_id)
), by = .(panel_id, panel_year)]

fac_year[, `:=`(
  min_prem      = pmax(standard_prem_pre * (1 - SCHED_CAP), POLICY_MIN_PREMIUM),
  standard_prem = pmax(standard_prem_pre,                    POLICY_MIN_PREMIUM),
  max_prem      = pmax(standard_prem_pre * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)
)]
fac_year[, standard_prem_pre := NULL]
fac_year[, carrier := CARRIER_KEY]

stopifnot(all(fac_year$min_prem      >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$standard_prem >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$max_prem      >= POLICY_MIN_PREMIUM))
stopifnot(all(fac_year$min_prem      <= fac_year$standard_prem))
stopifnot(all(fac_year$standard_prem <= fac_year$max_prem))

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated)]
setorder(out1, panel_id, panel_year)

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))


###############################################################################
## STEP 5 — File 3: multi-tank credit (trivial — TOMICS filing has no       ##
##          facility tank-count discount)                                  ##
###############################################################################
cat("=== STEP 5: multi-tank credit (File 3) ===\n")

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
## STEP 6 — File 4: minimum premium ($350, p.2 of the filing)               ##
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
cat("=== 15b DONE ===\n")
