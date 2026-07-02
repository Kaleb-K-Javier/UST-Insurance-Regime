###############################################################################
# 16b_apply_zurich.R — Apply Zurich rate engine to Texas FR contracts
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# BEHAVIORAL ASSUMPTIONS BANNER
# ══════════════════════════════════════════════════════════════════════════════
# CARRIER:        Zurich American Insurance Company
# CARRIER_KEY:    "ZURICH"
# ISSUER_NAME:    "ZURICH AMERICAN INS CO"  (FR panel exact match)
# RATE STATE:     Iowa (applied to TX tanks as Iowa-proxy; territory = 1.00)
#
# COVERAGE LIMIT: $1M / $1M occ/agg held fixed — ILF = 1.000 (reference).
#                 Per 036 locked decision; actual contract limits ignored.
#
# RATE ERA:       Assigned from policy EFF_DATE (NOT panel_year):
#                   EFF_DATE < 2008-11-01              → r2005
#                   2008-11-01 ≤ EFF_DATE < 2010-03-01 → r2008
#                   2010-03-01 ≤ EFF_DATE < 2011-10-01 → r2010
#                   EFF_DATE ≥ 2011-10-01               → r2011
#                 Pre-2005-06-01 contracts (if any) use r2005 as fallback.
#
# ILF / SIR:      Reference ($1M/$1M, no SIR) → 1.000.
# DEDUCTIBLE:     Filing default ($5K) held → no load.
# SCHEDULE:       Held at 0% → standard_prem; ±40% band → min_prem / max_prem
#                 (ticket 048 Part C: the filed ±25% cap is Iowa's rate-page
#                 cap; TX-written policies use ±40% — see SCHED_CAP in 16a).
# CONTACT PERSON: No (§X.2) → 0% (behavioral; no data on compliance).
#
# LOCATION TYPE:  Mapped from FACILITY_TYPE in texas_facility.csv.
#                 "All Other / unknown" → 0% (Refer to Company → reference).
#                 *** VERIFY FACILITY_TYPE codes against the data dictionary
#                     before running — see zurich_loc_type_load() in 16a. ***
#
# CORROSION:      FRP / composite / jacketed tanks → corr_present = TRUE (0%).
#                 CORR_TANK_CP ∈ {C, E, B}         → corr_present = TRUE (0%).
#                 All others (N, U, blank, NA)      → corr_present = FALSE (+10%).
#                 Pre-2010 CORR_TANK_CP is sparse; corr loads overstated then.
#
# OVERFILL:       SPILL_COMPLY == "Y" → 0%; all else → +10%.
#                 Source: texas_compartment.csv joined on UST_ID (not FACILITY_ID).
#
# CONTENTS:       Priority: gasoline > diesel > oil_kero > jet_fuel > ref (0%).
#                 is_oil_kerosene rated as Fuel Oil (-8%), modal product.
#
# PRIOR RELEASE:  TX_LUST.csv, per facility × EFF_DATE:
#                   No LUST before EFF_DATE           → 0%
#                   LUST, NFA completed before EFF_DATE → +10% (Claim Closed)
#                   LUST, NFA missing or ≥ EFF_DATE   → +20% (Claim Open)
#
# OUTPUT (4 files, Data/Analysis/rate_engines/ — RATE_ENGINE_BUILD_TARGET.md):
#   ZURICH_facility_year_premium.csv (File 1 — panel_id, panel_year, min_prem,
#     standard_prem, max_prem, carrier, n_tanks_rated — no source_era.)
#   ZURICH_cell_era_card.csv         (File 2 — carrier, wall, age_bin,
#     panel_year, premium_usd_per_tank_yr — keyed on calendar year, NOT era;
#     tank-level standalone STANDARD premium, no size/loc/release/floor.)
#   ZURICH_multitank_credit.csv      (File 3 — §XI.1 Size of Risk Credit,
#     computed from zurich_size_credit(), not transcribed, to guarantee
#     agreement with the engine; bands tile 1..9999 exactly.)
#   ZURICH_min_premium.csv           (File 4 — $425 filing minimum, p.Z-IA-2
#     §VI, every active year.)
# ══════════════════════════════════════════════════════════════════════════════

library(data.table)
library(here)

source(here("Code", "Cleaning", "16a_engine_zurich.R"))

# ── Logging ──────────────────────────────────────────────────────────────────
.log_path <- here::here("logs", paste0(
  "16b_apply_zurich_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 16b_apply_zurich.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

cat("=== 16b_apply_zurich: Zurich premium apply ===\n")
cat(sprintf("Run started: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

CARRIER_KEY  <- "ZURICH"
ISSUER_MATCH <- "ZURICH AMERICAN INS CO"
out_dir      <- here("Data", "Analysis", "rate_engines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
## STEP 1: Load raw tank file                                                 ##
###############################################################################
cat("=== STEP 1: load raw_pst_ust ===\n")
raw_ust_path <- here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.csv")
stopifnot(file.exists(raw_ust_path))

ust_raw <- fread(raw_ust_path, select = c(
  "FACILITY_ID", "UST_ID", "TANK_ID", "INSTALL_DATE", "CLOSED_DATE",
  "CAPACITY",
  "CORR_TANK_CP",
  "is_fiberglass_tank", "is_composite_tank", "is_jacketed_tank",
  "DET_C_INTERSTITIAL", "DET_P_INTERSTITIAL",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel"
))
ust_raw[, FACILITY_ID := toupper(trimws(as.character(FACILITY_ID)))]
cat(sprintf("  raw_pst_ust: %d tanks\n", nrow(ust_raw)))

###############################################################################
## STEP 1a: mm_wall JOIN (panel_dt.csv, TX) — SINGLE source of truth for wall##
###############################################################################
cat("=== STEP 1a: mm_wall panel join ===\n")
# SUPERSEDES the old "use the pre-built double_walled dummy" fix (that dummy
# is retired fleet-wide, researcher 2026-07-01): every engine sources wall from
# the estimation panel's mm_wall column (Data/Analysis/panel_dt.csv) so the
# card cell matches the structural model's state cell.
panel_wall <- fread(here("Data", "Analysis", "panel_dt.csv"),
                     select = c("facility_id", "tank_id", "state", "mm_wall"))
panel_wall <- panel_wall[state == "TX"]
panel_wall[, state := NULL]
panel_wall <- unique(panel_wall, by = c("facility_id", "tank_id"))
# PM-state wall mapping — mirrors 04al_BOY_Composition_Build.R:67 (the BOY-
# composition input PM01/PM03 build the portfolio state space from): SW iff
# mm_wall contains "single" (case-insensitive), else DW. Mixed-Wall/Unknown-Wall
# fall through to DW here exactly as the panel does — not special-cased.
panel_wall[, wall := fifelse(grepl("single", mm_wall, ignore.case = TRUE), "SW", "DW")]
panel_wall[, mm_wall := NULL]
setnames(panel_wall, c("facility_id", "tank_id"), c("facility_id_key", "tank_id_key"))

# Join keys mirror 08_Clean_TX.R's own harmonization of panel_dt.csv's IDs:
# facility_id is standardize_numeric_id()'d (integer round-trip, leading zeros
# stripped; 08_Clean_TX.R:566) and tank_id is a straight as.character(TANK_ID),
# no zero-strip (08_Clean_TX.R:824). Straight join, no fallback / match-rate
# heuristics.
ust_raw[, facility_id_key := {
  v <- suppressWarnings(as.integer(as.character(FACILITY_ID)))
  fifelse(is.na(v), trimws(as.character(FACILITY_ID)), as.character(v))
}]
ust_raw[, tank_id_key := trimws(as.character(TANK_ID))]

ust_raw <- panel_wall[ust_raw, on = c("facility_id_key", "tank_id_key")]
cat(sprintf("  mm_wall panel join: %d / %d tanks matched (%.1f%%)\n",
            sum(!is.na(ust_raw$wall)), nrow(ust_raw), 100 * mean(!is.na(ust_raw$wall))))
ust_raw[, c("facility_id_key", "tank_id_key") := NULL]

###############################################################################
## STEP 1b: Join compartment file for SPILL_COMPLY (§VIII.3 overfill)        ##
## Joined on UST_ID — see 08_Clean_TX.R FIX 2                               ##
###############################################################################
cat("=== STEP 1b: join compartment for SPILL_COMPLY ===\n")
comp_path <- here("Data", "Raw", "state_databases", "Texas", "texas_compartment.csv")
stopifnot(file.exists(comp_path))

comp_raw <- fread(comp_path, select = c("UST_ID", "SPILL_COMPLY"))
comp_raw <- comp_raw[!is.na(SPILL_COMPLY) & SPILL_COMPLY != ""]
comp_raw <- comp_raw[, .(SPILL_COMPLY = SPILL_COMPLY[1L]), by = UST_ID]

ust_raw  <- comp_raw[ust_raw, on = "UST_ID"]   # left join: ust_raw gains SPILL_COMPLY
cat(sprintf("  SPILL_COMPLY coverage: %.1f%%\n",
            100 * mean(!is.na(ust_raw$SPILL_COMPLY))))

###############################################################################
## STEP 1c: Derive rating attributes                                          ##
###############################################################################
cat("=== STEP 1c: derive tank attributes ===\n")

ust_raw[, INSTALL_DATE := as.Date(INSTALL_DATE)]
ust_raw[, CLOSED_DATE  := as.Date(CLOSED_DATE)]

# Year-month integers for fast inequality joins
ust_raw[, ym_install := year(INSTALL_DATE) * 12L + month(INSTALL_DATE)]
ust_raw[, ym_close   := fifelse(
  is.na(CLOSED_DATE),
  .Machine$integer.max,
  year(CLOSED_DATE) * 12L + month(CLOSED_DATE)
)]

n_no_install <- sum(is.na(ust_raw$INSTALL_DATE))
ust_raw <- ust_raw[!is.na(INSTALL_DATE)]
cat(sprintf("  Dropped %d tanks with no INSTALL_DATE\n", n_no_install))

# §VIII.4 Corrosion protection
# FRP / composite / jacketed = inherently non-corrodible → Yes (0%)
# CORR_TANK_CP C=cathodic, E=epoxy/non-corrodible, B=both → Yes (0%)
# N, U, blank, NA → No (+10%)
ust_raw[, corr_present := (
  (as.integer(is_fiberglass_tank) == 1L |
   as.integer(is_composite_tank)  == 1L |
   as.integer(is_jacketed_tank)   == 1L) |
  (!is.na(CORR_TANK_CP) & CORR_TANK_CP %in% c("C", "E", "B"))
)]

# §VIII.2 Leak detection
ust_raw[, has_leak_det := (
  as.integer(DET_C_INTERSTITIAL) == 1L |
  as.integer(DET_P_INTERSTITIAL) == 1L
)]

# §VIII.3 Overfill
ust_raw[, spill_comply_y := (!is.na(SPILL_COMPLY) & SPILL_COMPLY == "Y")]

# Contents (§XI.2)
ust_raw[, is_gasoline := as.logical(as.integer(is_gasoline))]
ust_raw[, is_diesel   := as.logical(as.integer(is_diesel))]
ust_raw[, is_oil_kero := as.logical(as.integer(is_oil_kerosene))]
ust_raw[, is_jet_fuel := as.logical(as.integer(is_jet_fuel))]

cat(sprintf("  Ready: %d tanks\n", nrow(ust_raw)))
cat(sprintf("    corr_present=%.1f%%  has_leak_det=%.1f%%  spill_comply_y=%.1f%%\n",
            100*mean(ust_raw$corr_present, na.rm=TRUE),
            100*mean(ust_raw$has_leak_det, na.rm=TRUE),
            100*mean(ust_raw$spill_comply_y, na.rm=TRUE)))

###############################################################################
## STEP 2: Load FR contract month panel, filter to Zurich                    ##
###############################################################################
cat("=== STEP 2: load FR panel ===\n")
fr_path <- here("Data", "Processed", "texas_fr_contract_month_panel.csv")
stopifnot(file.exists(fr_path))

fr_z <- fread(fr_path, select = c(
  "FACILITY_ID", "YEAR", "MONTH", "ISSUER_NAME", "EFF_DATE"))
fr_z <- fr_z[ISSUER_NAME == ISSUER_MATCH]
fr_z[, FACILITY_ID := toupper(trimws(as.character(FACILITY_ID)))]
fr_z[, EFF_DATE    := as.Date(EFF_DATE)]
fr_z[, ym          := as.integer(YEAR) * 12L + as.integer(MONTH)]
fr_z[, rate_era    := era_from_eff_date(EFF_DATE)]

cat(sprintf("  Zurich FR rows: %d  |  facilities: %d  |  years: %d-%d\n",
            nrow(fr_z), uniqueN(fr_z$FACILITY_ID),
            min(fr_z$YEAR), max(fr_z$YEAR)))
cat("  Rate era distribution:\n")
print(fr_z[, .N, by = rate_era][order(rate_era)])

###############################################################################
## STEP 3: LUST prior-release status per (FACILITY_ID, EFF_DATE)             ##
## §X.1 Location Prior Release — mandatory factor                            ##
###############################################################################
cat("=== STEP 3: LUST prior-release ===\n")
lust_path <- here("Data", "Raw", "state_databases", "Texas", "TX_LUST.csv")
stopifnot(file.exists(lust_path))

lust_raw <- fread(lust_path, select = c("FACILITY_ID", "report_date", "nfa_date"))
lust_raw[, FACILITY_ID := toupper(trimws(as.character(FACILITY_ID)))]
lust_raw[, report_date := as.Date(report_date)]
lust_raw[, nfa_date    := as.Date(nfa_date)]
cat(sprintf("  LUST records: %d  |  facilities: %d\n",
            nrow(lust_raw), uniqueN(lust_raw$FACILITY_ID)))

# For each unique (FACILITY_ID, EFF_DATE) in Zurich panel:
# find all LUST records where report_date < EFF_DATE
policy_keys <- unique(fr_z[, .(FACILITY_ID, EFF_DATE)])

pre_lust <- lust_raw[policy_keys, on = "FACILITY_ID",
                     allow.cartesian = TRUE, nomatch = NULL]
pre_lust <- pre_lust[!is.na(report_date) & report_date < EFF_DATE]

lust_status <- pre_lust[, .(
  had_prior_lust = TRUE,
  lust_open      = any(is.na(nfa_date) | nfa_date >= EFF_DATE)
), by = .(FACILITY_ID, EFF_DATE)]

# Left join back: unmatched keys → no prior release
policy_keys <- lust_status[policy_keys, on = .(FACILITY_ID, EFF_DATE)]
policy_keys[is.na(had_prior_lust), `:=`(had_prior_lust = FALSE, lust_open = FALSE)]

cat(sprintf("  Prior release (any):  %d facility-policies (%.1f%%)\n",
            sum(policy_keys$had_prior_lust),
            100 * mean(policy_keys$had_prior_lust)))
cat(sprintf("    of which open (+20%%): %d  |  closed (+10%%): %d\n",
            sum(policy_keys$had_prior_lust & policy_keys$lust_open),
            sum(policy_keys$had_prior_lust & !policy_keys$lust_open)))

fr_z <- policy_keys[fr_z, on = .(FACILITY_ID, EFF_DATE)]

###############################################################################
## STEP 4: Facility type for location type load (§X.3)                       ##
###############################################################################
cat("=== STEP 4: load FACILITY_TYPE ===\n")
fac_path <- here("Data", "Raw", "state_databases", "Texas", "texas_facility.csv")
stopifnot(file.exists(fac_path))

fac_raw <- fread(fac_path, select = c("FACILITY_ID", "FACILITY_TYPE"))
fac_raw[, FACILITY_ID := toupper(trimws(as.character(FACILITY_ID)))]
fac_raw <- unique(fac_raw, by = "FACILITY_ID")

cat("  FACILITY_TYPE distribution (top 12):\n")
print(fac_raw[, .N, by = FACILITY_TYPE][order(-N)][seq_len(min(.N, 12L))])

fr_z <- fac_raw[fr_z, on = "FACILITY_ID"]
fr_z[is.na(FACILITY_TYPE), FACILITY_TYPE := ""]

###############################################################################
## STEP 5: Cross-join active tanks × facility-months                         ##
###############################################################################
cat("=== STEP 5: build tank × month panel ===\n")

# Right join: for each FR row, all ust_raw tanks with same FACILITY_ID
ust_tm <- ust_raw[fr_z, on = "FACILITY_ID", allow.cartesian = TRUE, nomatch = NULL]

# Keep only tanks active in the coverage month
ust_tm <- ust_tm[ym_install <= ym & ym_close > ym]
cat(sprintf("  Tank-month rows: %d\n", nrow(ust_tm)))

ust_tm[, n_total_tanks := .N, by = .(FACILITY_ID, ym)]
ust_tm[, age_years     := as.integer(floor((ym - ym_install) / 12L))]

###############################################################################
## STEP 5b: Per-tank premium (Rule XIII — consecutive multiplication)         ##
###############################################################################
cat("=== STEP 5b: per-tank premium ===\n")

ust_tm[, base_r := zurich_base_rate(age_years, as.integer(wall == "DW"), rate_era)]
ust_tm[, tank_premium :=
  base_r
  * (1 + zurich_cap_load(CAPACITY))
  * (1 + zurich_leak_load(has_leak_det))
  * (1 + zurich_overfill_load(spill_comply_y))
  * (1 + zurich_corr_load(corr_present))
  * (1 + zurich_contents_load(is_gasoline, is_diesel, is_oil_kero, is_jet_fuel))
]

n_na_prem <- sum(is.na(ust_tm$tank_premium))
if (n_na_prem > 0L) warning(sprintf("%d tank-months with NA tank_premium", n_na_prem))
cat(sprintf("  tank_premium: min=%.0f  median=%.0f  max=%.0f  NA=%d\n",
            min(ust_tm$tank_premium, na.rm=TRUE),
            median(ust_tm$tank_premium, na.rm=TRUE),
            max(ust_tm$tank_premium, na.rm=TRUE),
            n_na_prem))

###############################################################################
## STEP 5b2: File 2 — cell-year card (structural kernel, tank level, before ##
##           facility rollup — no size/loc/release credit, no floor)        ##
###############################################################################
cat("=== STEP 5b2: build cell-year card ===\n")

# wall carried through from ust_raw's mm_wall panel join (STEP 1a) — no
# re-derivation here.
ust_tm[, age_bin    := as.integer(cut(age_years,
                        c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
                        labels = 1:8, right = FALSE, include.lowest = TRUE))]
ust_tm[is.na(age_bin), age_bin := 8L]
ust_tm[, panel_year := as.integer(YEAR)]

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
## STEP 5c: Facility-month rollup — sum tanks, apply facility factors, band  ##
###############################################################################
cat("=== STEP 5c: facility-month rollup ===\n")

fac_month <- ust_tm[, .(
  tank_sum       = sum(tank_premium, na.rm = TRUE),
  n_tanks_rated  = as.integer(sum(!is.na(tank_premium))),
  n_total_tanks  = first(n_total_tanks),
  FACILITY_TYPE  = first(FACILITY_TYPE),
  had_prior_lust = first(had_prior_lust),
  lust_open      = first(lust_open),
  rate_era       = first(rate_era),
  YEAR           = first(YEAR)
), by = .(FACILITY_ID, ym)]

# Facility-level factors (Rule XIII: consecutive on the summed base)
fac_month[, fac_raw :=
  tank_sum
  * (1 + zurich_size_credit(n_total_tanks))
  * (1 + zurich_loc_type_load(FACILITY_TYPE, n_total_tanks))
  * (1 + zurich_prior_release_load(had_prior_lust, lust_open))
]

# Schedule band ±40% (TX; SCHED_CAP, see 16a) and floor $425
fac_month[, standard_prem := pmax(fac_raw,                   POLICY_MIN_PREMIUM)]
fac_month[, min_prem      := pmax(fac_raw * (1 - SCHED_CAP), POLICY_MIN_PREMIUM)]
fac_month[, max_prem      := pmax(fac_raw * (1 + SCHED_CAP), POLICY_MIN_PREMIUM)]

stopifnot(all(is.finite(fac_month$standard_prem)))
stopifnot(all(fac_month$min_prem      <= fac_month$standard_prem))
stopifnot(all(fac_month$standard_prem <= fac_month$max_prem))
stopifnot(all(fac_month$standard_prem >= POLICY_MIN_PREMIUM))
cat(sprintf("  Facility-month rows: %d\n", nrow(fac_month)))

###############################################################################
## STEP 6: Facility-month → facility-year (mean across months)               ##
###############################################################################
cat("=== STEP 6: facility-year ===\n")

fac_year <- fac_month[, .(
  min_prem      = mean(min_prem),
  standard_prem = mean(standard_prem),
  max_prem      = mean(max_prem),
  n_tanks_rated = as.integer(round(mean(n_tanks_rated)))
), by = .(FACILITY_ID, panel_year = YEAR)]

fac_year[, panel_id := paste(toupper(trimws(FACILITY_ID)), "TX", sep = "_")]
fac_year[, carrier  := CARRIER_KEY]

cat(sprintf("  Facility-year rows: %d  |  facilities: %d  |  years: %d-%d\n",
            nrow(fac_year), uniqueN(fac_year$panel_id),
            min(fac_year$panel_year), max(fac_year$panel_year)))
cat(sprintf("  standard_prem: mean=%.0f  median=%.0f  p5=%.0f  p95=%.0f\n",
            mean(fac_year$standard_prem),
            median(fac_year$standard_prem),
            quantile(fac_year$standard_prem, 0.05),
            quantile(fac_year$standard_prem, 0.95)))

###############################################################################
## STEP 7: Write output (File 1 — no source_era, per                        ##
##          RATE_ENGINE_BUILD_TARGET.md)                                    ##
###############################################################################
cat("=== STEP 7: write output ===\n")

out_cols <- c("panel_id", "panel_year", "min_prem", "standard_prem",
              "max_prem", "carrier", "n_tanks_rated")
out <- fac_year[, ..out_cols]
setorder(out, panel_id, panel_year)

out_path <- file.path(out_dir, "ZURICH_facility_year_premium.csv")
fwrite(out, out_path)
cat(sprintf("  Saved: %s (%d rows)\n", out_path, nrow(out)))

###############################################################################
## STEP 8: File 3 — multi-tank credit (§XI.1 Size of Risk Credit; computed  ##
##          from zurich_size_credit(), not transcribed, to guarantee        ##
##          agreement with the engine)                                     ##
###############################################################################
cat("=== STEP 8: multi-tank credit (File 3) ===\n")

band_edges <- data.table(
  n_min = c(1L, 4L, 7L, 11L, 16L, 26L, 46L),
  n_max = c(3L, 6L, 10L, 15L, 25L, 45L, 9999L)
)
band_edges[, credit_mult := 1 + zurich_size_credit(n_min)]

active_years <- sort(unique(out$panel_year))
credit_tbl <- CJ(panel_year = active_years, n_min = band_edges$n_min)[
  band_edges, on = "n_min"
][, carrier := CARRIER_KEY]
setcolorder(credit_tbl, c("carrier", "panel_year", "n_min", "n_max", "credit_mult"))
setorder(credit_tbl, panel_year, n_min)

# Tiling check: every year's bands cover 1..9999 with no gap/overlap
for (yr in active_years) {
  yb <- credit_tbl[panel_year == yr][order(n_min)]
  n_bands <- nrow(yb)
  stopifnot(yb$n_min[1L] == 1L, yb$n_max[n_bands] == 9999L)
  stopifnot(all(yb$n_min[-1L] == yb$n_max[-n_bands] + 1L))
}

credit_path <- file.path(out_dir, "ZURICH_multitank_credit.csv")
fwrite(credit_tbl, credit_path)
cat(sprintf("  multi-tank credit: %s (%d rows, %d active years x %d bands)\n",
            credit_path, nrow(credit_tbl), length(active_years), nrow(band_edges)))

###############################################################################
## STEP 9: File 4 — minimum premium ($425, p.Z-IA-2 §VI, every active year) ##
###############################################################################
cat("=== STEP 9: minimum premium (File 4) ===\n")

min_prem_tbl <- data.table(
  carrier         = CARRIER_KEY,
  panel_year      = active_years,
  min_premium_usd = POLICY_MIN_PREMIUM
)
setcolorder(min_prem_tbl, c("carrier", "panel_year", "min_premium_usd"))
stopifnot(all(min_prem_tbl$min_premium_usd > 0))
stopifnot(uniqueN(min_prem_tbl$panel_year) == nrow(min_prem_tbl))

min_prem_path <- file.path(out_dir, "ZURICH_min_premium.csv")
fwrite(min_prem_tbl, min_prem_path)
cat(sprintf("  min premium: %s (%d rows)\n", min_prem_path, nrow(min_prem_tbl)))

cat(sprintf("Run completed: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
