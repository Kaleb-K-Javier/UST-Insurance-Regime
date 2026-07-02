###############################################################################
# 18b_apply_ace.R — ACE (ACE American Insurance Company, later Chubb)
#                    premium: read shared panel, price, write output
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        ACE American Insurance Company (Chubb acquired ACE in 2016;
#                 one unified manual throughout, no era switch).
# CARRIER_KEY:    "ACE"
# ISSUER MATCH:   RESOLVED (researcher-confirmed 2026-07-02, supersedes the
#                 pre-053 "PENDING CONFIRMATION" banner) — now handled
#                 centrally by issuer_crosswalk.R / 20_'s STEP 3. Five real
#                 issuer strings pool to ACE: "ACE AMERICAN INS CO",
#                 "ILLINOIS UNION INS CO", "WESTCHESTER FIRE INS CO",
#                 "CHUBB CUSTOM INS CO", "ACE SAFETY INDEM CO". The pre-053
#                 bare-word candidates "ACE"/"CHUBB" never matched any real
#                 ISSUER_NAME in the FR panel (confirmed via the real
#                 ISSUER_NAME distribution, 2026-07-02) — "CHUBB CUSTOM INS
#                 CO" is the real string; this script has never actually
#                 rated a single Chubb-issued contract until this fix.
#
# TICKET 053 REFACTOR: no longer loads raw_pst_ust.csv / panel_dt.csv / the
# FR contract panel itself. Reads Data/Analysis/rate_engines/
# _priced_tank_panel.csv (built ONCE by 20_Build_Priced_Tank_Panel.R),
# filters to carrier == "ACE", calls price_ace_tank() (18a), rolls up,
# writes the 4 files.
#
# COVERAGE LIMIT / DEDUCTIBLE: $1,000,000/$1,000,000 + $5,000 deductible —
#                 the shared panel's cover_occ/cover_agg are not wired into
#                 pricing (18a's tank-level formula has no ILF/deductible
#                 term at all — a true no-op, same as the pre-053 script).
#
# SCHEDULE:       +/-40% (SCHED_CAP) -> min_prem/max_prem around
#                 standard_prem, facility-year level. File 2 schedule-
#                 neutral (no band).
#
# WALL SOURCE / CONSTRUCTION MAPPING: unchanged from the pre-053 18b —
#                 STIP3 (50%) <- wall=="DW" OR is_composite_tank==1;
#                 FIBERGLASS (20%) <- is_fiberglass_tank==1 (pure FRP);
#                 BARE_STEEL (0%) <- default. wall now comes straight from
#                 the shared panel (already mm_wall-sourced by 20_'s single
#                 join) instead of a per-script panel_dt.csv join.
#
# OUTPUT (4 files, Data/Analysis/rate_engines/):
#   ACE_facility_year_premium.csv, ACE_cell_era_card.csv,
#   ACE_multitank_credit.csv, ACE_min_premium.csv (= $350 flat, every year)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(here)

source(here("Code", "Cleaning", "18a_engine_ace.R"))
# inherits: SCHED_CAP, MIN_PREMIUM, multi_tank_credit_ace, price_ace_tank

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "18b_apply_ace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 18b_apply_ace.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 18b_apply_ace: ACE premium apply (Ticket 053 shared-panel) ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY <- "ACE"
out_dir     <- here("Data", "Analysis", "rate_engines")
panel_path  <- file.path(out_dir, "_priced_tank_panel.csv")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

min_premium_of_year <- function(panel_year) rep(MIN_PREMIUM, length(panel_year))


###############################################################################
## STEP 1 — Read shared panel, filter to ACE                                ##
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
## STEP 2 — ACE construction bucket                                         ##
###############################################################################
cat("=== STEP 2: construction bucket ===\n")

ust_tm[, construction_code := fcase(
  wall == "DW" | is_composite_tank == 1L,   "STIP3",
  is_fiberglass_tank == 1L,                  "FIBERGLASS",
  default = "BARE_STEEL"
)]
cat("  construction_code distribution:\n")
print(ust_tm[, .N, by = construction_code][order(-N)])


###############################################################################
## STEP 3 — Price each tank-year via price_ace_tank() (18a)                 ##
###############################################################################
cat("=== STEP 3: price tank-years ===\n")

ust_tm[, tank_standard := price_ace_tank(age_years = age_years, construction_code = construction_code)]

stopifnot(all(is.finite(ust_tm$tank_standard)))
stopifnot(all(ust_tm$tank_standard >= 0))
cat(sprintf("  tank_standard: mean=$%.0f  min=$%.0f  max=$%.0f\n",
            mean(ust_tm$tank_standard), min(ust_tm$tank_standard), max(ust_tm$tank_standard)))


###############################################################################
## STEP 3b — File 2: cell-year card                                         ##
###############################################################################
cat("=== STEP 3b: build cell-year card ===\n")

cell_card <- ust_tm[!is.na(wall), .(premium_usd_per_tank_yr = mean(tank_standard, na.rm = TRUE)),
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
## STEP 4 — Facility-year rollup: multi-tank credit, band + floor -> File 1##
###############################################################################
cat("=== STEP 4: facility-year rollup, credit, band, floor ===\n")

fac_year <- ust_tm[, .(
  fac_unmodified = sum(tank_standard, na.rm = TRUE),
  n_tanks_rated  = uniqueN(ust_id)
), by = .(panel_id, panel_year)]

fac_year[, iv_credit := multi_tank_credit_ace(n_tanks_rated)]
fac_year[, fac_credited := fac_unmodified * (1 - iv_credit)]
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

out1 <- fac_year[, .(panel_id, panel_year, min_prem, standard_prem, max_prem,
                      carrier, n_tanks_rated)]
setorder(out1, panel_id, panel_year)

out1_path <- file.path(out_dir, paste0(CARRIER_KEY, "_facility_year_premium.csv"))
fwrite(out1, out1_path)
cat(sprintf("  Facility-year rows: %d | facilities: %d | years: %d-%d\n",
            nrow(out1), uniqueN(out1$panel_id), min(out1$panel_year), max(out1$panel_year)))
cat(sprintf("  Saved: %s\n", out1_path))


###############################################################################
## STEP 5 — File 3: multi-tank credit bands (computed from 18a's ladder)    ##
###############################################################################
cat("=== STEP 5: multi-tank credit bands ===\n")

band_edges <- data.table(
  n_min = c(1L, 10L, 20L, 50L, 75L, 101L, 201L, 301L),
  n_max = c(9L, 19L, 49L, 74L, 100L, 200L, 300L, 9999L)
)
band_edges[, credit_mult := 1 - multi_tank_credit_ace(n_min)]

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
## STEP 6 — File 4: minimum premium per active year (flat $350, no era)     ##
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
cat("=== 18b DONE ===\n")
