# PM02_Lookups.R — Portfolio model lookup tables
# TICKET 023 B2. Every exogenous number in ONE .rds, model units (dollars / 10,000).
# Outputs: Output/Estimation_Results/PM_Lookups.rds, logs/PM02_*.log
suppressPackageStartupMessages({ library(data.table); library(here) })

.log_path <- here::here("logs", paste0("PM02_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM02_Lookups\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# --- constants ---
SCALE    <- 10000L
DATA_DIR <- here("Data", "Analysis")
TAB_DIR  <- here("Output", "Tables")
RES_DIR  <- here("Output", "Estimation_Results")
ERAS     <- c("2006", "2014", "2019")     # 3 portfolio eras
# 04a ERA_BOUNDS (lines 82-87); 2021 byte-identical to 2019 (line 266)
era_of_year <- function(y) fcase(y <= 2013L, "2006", y <= 2018L, "2014", default = "2019")

# Canonical carrier keys (ticket 036 crosswalk; no cleanup in 039)
PRICEABLE_CARRIERS <- c("MID_CONTINENT", "TOMICS", "GREAT_AMERICAN", "ZURICH", "ACE", "AIG")
ALL_CARRIERS       <- c(PRICEABLE_CARRIERS, "IMPUTED")
# age_bin label in engine CSVs ("0-5" etc.) -> integer bin 1=youngest 8=oldest
AGE_BIN_MAP <- c("0-5"=1L, "5-10"=2L, "10-15"=3L, "15-20"=4L,
                 "20-25"=5L, "25-30"=6L, "30-35"=7L, "35+"=8L)
ENGINE_DIR  <- file.path(DATA_DIR, "rate_engines")

# MARG order: position 1=SW_8 (oldest) .. position 8=SW_1, 9=DW_8 .. 16=DW_1
MARG_WALL <- c(rep("SW", 8L), rep("DW", 8L))
MARG_BIN  <- c(8:1, 8:1)   # age_bin for each of the 16 positions

# 04a line 67 midpoints: bin 1=(0-5yr)->2.5, ..., bin 8=(35+yr)->37.5
AGE_MIDPTS <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5)

# ==============================================================================
# Filed rate-filing price functions — verbatim from 04a_TX_Premium_All_1999_onwards.R
# Factor functions (04a lines 218-270); 2021 byte-identical to 2019 (04a line 266).
# ==============================================================================
# ILF: NA/NA defaults -> 1.18 for all eras (04a ilf_factor_2006/2014/2019, NA dispatch)
ILF <- 1.18

age_factor_2006 <- function(a) {
  fcase(a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
        a <= 20,  0.10, a <= 25,  0.20, a >  25,  0.25)
}
age_factor_2014 <- function(a) {
  fcase(a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
        a <= 20,  0.10, a <= 25,  0.20, a <= 30,  0.25,
        a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
        a <= 50,  0.65, a >  50,  0.75)
}
age_factor_2019 <- function(a) {  # 04a line 244; 2021 identical (line 268)
  fcase(a <=  2, -0.20, a <=  4, -0.16, a <=  6, -0.12, a <=  8, -0.08,
        a <= 10, -0.04, a <= 15,  0.00, a <= 20,  0.10, a <= 25,  0.20,
        a <= 30,  0.25, a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
        a <= 50,  0.65, a >  50,  0.75)
}
# construction_factor: SW -> is_steel_cathodic=T, DW -> is_double_walled_steel=T (04a line 186)
# fcase(dw, -0.20, sc|frp, 0.00, default 0.00) -> SW: 0.00, DW: -0.20
construction_load <- ifelse(MARG_WALL == "DW", -0.20, 0.00)

# Filed price per cell per era (300 * ILF * (1 + age_load + cons_load)); 04a line 73 / AE08
filed_price_vec <- function(era_label) {
  midpts <- AGE_MIDPTS[MARG_BIN]
  af <- switch(era_label,
    "2006" = age_factor_2006(midpts),
    "2014" = age_factor_2014(midpts),
    "2019" = age_factor_2019(midpts))
  300 * ILF * (1 + af + construction_load)
}

# ==============================================================================
# L1 PRICES  pbar[16 cells, 3 eras]
# ==============================================================================
cat("=== L1: empirical cell prices (single-cell TX, 2006+) ===\n")
boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 2006L]

# single-cell TX facility-years (n_occ == 1) — X3E method from AE06
occ <- boylong[, .(n_occ = .N, wall = wall[1L], age_bin = age_bin[1L]),
               by = .(panel_id, panel_year, state)]
single_tx <- occ[n_occ == 1L & state == "TX"]
cat(sprintf("  single-cell TX facility-years 2006+: %s\n",
            format(nrow(single_tx), big.mark=",")))

mid <- fread(file.path(DATA_DIR, "tx_midcont_premium_all_1999_onwards.csv"),
             select = c("panel_id", "panel_year", "mean_tank_premium", "source_era"))
mid[, panel_id := as.character(panel_id)]
mid <- mid[is.finite(mean_tank_premium)]

# map 04a source_era labels -> 3-era portfolio labels
era_map_src <- c(era_2006 = "2006", era_2014 = "2014", era_2019 = "2019", era_2021 = "2019")
mid[, era := era_map_src[source_era]]

emp <- merge(single_tx, mid, by = c("panel_id", "panel_year"))
cat(sprintf("  matched to rate-filing premiums: %s\n", format(nrow(emp), big.mark=",")))

# cell-era empirical means
x3e <- emp[, .(n = .N, mean_premium = mean(mean_tank_premium)),
           by = .(era, wall, age_bin)]
setorder(x3e, era, wall, age_bin)
cat(sprintf("  cell-era combinations observed: %d\n", nrow(x3e)))

# Build filed prices for all 16 cells x 3 eras
filed_dt <- CJ(era = ERAS, pos = seq_len(16L))
filed_dt[, wall    := MARG_WALL[pos]]
filed_dt[, age_bin := MARG_BIN[pos]]
filed_dt[, filed   := mapply(function(era, pos) filed_price_vec(era)[pos], era, pos)]

# Merge empirical into filed_dt
cell_era <- merge(filed_dt, x3e[, .(era, wall, age_bin, n, mean_premium)],
                  by = c("era", "wall", "age_bin"), all.x = TRUE)
cell_era[is.na(n), n := 0L]
setorder(cell_era, era, pos)

# --- FILL RULE ---
# Compute sample-wide ratio (empirical / filed) on thick cells (n >= 200)
THIN_N <- 200L
ratio_era_dt <- cell_era[n >= THIN_N, .(
  ratio = sum(mean_premium * n) / sum(filed * n)
), by = era]
cat("  era-level empirical/filed ratios (thick cells only):\n"); print(ratio_era_dt)

# (a) pool across eras for thin cells
pooled <- emp[, .(n_pool = .N, mean_premium_pool = mean(mean_tank_premium)),
              by = .(wall, age_bin)]
cell_era <- merge(cell_era, pooled, by = c("wall", "age_bin"), all.x = TRUE)
cell_era[is.na(n_pool), `:=`(n_pool = 0L, mean_premium_pool = NA_real_)]

# (b) impute = filed * era ratio for cells that are thin even pooled
cell_era <- merge(cell_era, ratio_era_dt, by = "era", all.x = TRUE)

cell_era[, fill_source := fcase(
  n >= THIN_N,                        "empirical",
  n_pool >= THIN_N,                   "pooled",
  default                           = "filed_x_ratio")]
cell_era[, price_raw := fcase(
  fill_source == "empirical",         mean_premium,
  fill_source == "pooled",            mean_premium_pool,
  default                           = filed * ratio)]

# Report fill rule usage
cat("  fill rule usage (cell-era counts):\n")
print(cell_era[, .(n_cells = .N), by = .(era, fill_source)])

stopifnot(all(is.finite(cell_era$price_raw)), all(cell_era$price_raw > 0))
cell_era[, price := price_raw / SCALE]

# Build pbar matrix: 16 rows (MARG order) x 3 era columns
pbar <- matrix(NA_real_, nrow = 16L, ncol = 3L,
               dimnames = list(NULL, ERAS))
for (era_label in ERAS) {
  sub <- cell_era[era == era_label][order(pos)]
  pbar[, era_label] <- sub$price
}
stopifnot(all(is.finite(pbar)), all(pbar > 0), dim(pbar) == c(16L, 3L))
cat(sprintf("  pbar: dim %dx%d | range [%.4f, %.4f] (model units)\n",
            nrow(pbar), ncol(pbar), min(pbar), max(pbar)))

# ==============================================================================
# L1b CARRIER ENGINE CARDS  pbar_carrier[16 cells, 3 eras, 7 carriers]
# Per-tank card for each priceable carrier from transcription engine CSVs;
# IMPUTED = share-weighted priceable mean per (MARG cell, era).
# ==============================================================================
cat("=== L1b: carrier-indexed engine cards ===\n")

REBUILT_CSV <- file.path(DATA_DIR, "tx_facility_premium_rebuilt.csv")
stopifnot(
  "tx_facility_premium_rebuilt.csv missing — run TICKET 036 first" =
    file.exists(REBUILT_CSV)
)
cat(sprintf("  [GATE 036] tx_facility_premium_rebuilt.csv: OK\n"))

pbar_carrier <- array(NA_real_,
  dim      = c(16L, 3L, length(ALL_CARRIERS)),
  dimnames = list(NULL, ERAS, ALL_CARRIERS))

for (cr in PRICEABLE_CARRIERS) {
  eng_path <- file.path(ENGINE_DIR, paste0(cr, "_engine.csv"))
  stopifnot(
    sprintf("Engine CSV missing for %s — run TICKET 036 first: %s", cr, eng_path) =
      file.exists(eng_path)
  )
  eng <- fread(eng_path, select = c("carrier", "wall", "age_bin", "era",
                                    "premium_usd_per_tank_yr"))
  stopifnot(all(eng$carrier == cr))
  stopifnot(
    sprintf("Non-canonical era in %s engine: %s", cr,
            paste(setdiff(eng$era, ERAS), collapse=",")) =
      all(eng$era %in% ERAS)
  )
  stopifnot(
    sprintf("Unrecognised age_bin in %s engine: %s", cr,
            paste(setdiff(eng$age_bin, names(AGE_BIN_MAP)), collapse=",")) =
      all(eng$age_bin %in% names(AGE_BIN_MAP))
  )
  # map age_bin string -> integer; MARG pos: SW bin b -> pos 9-b; DW bin b -> pos 17-b
  eng[, bin := AGE_BIN_MAP[age_bin]]
  eng[, pos := ifelse(wall == "SW", 9L - bin, 17L - bin)]
  stopifnot(all(eng$pos >= 1L), all(eng$pos <= 16L))
  for (ri in seq_len(nrow(eng))) {
    pbar_carrier[eng$pos[ri], eng$era[ri], cr] <-
      eng$premium_usd_per_tank_yr[ri] / SCALE
  }
  cat(sprintf("  %s: %d era-cell entries loaded\n", cr, nrow(eng)))
}

# IMPUTED card: share-weighted priceable mean per (MARG pos, era)
# Weights from single-cell TX RB facility-years with a real engine premium
cat("  Computing IMPUTED card from observed carrier market shares...\n")
rebuilt <- fread(REBUILT_CSV,
                 select = c("panel_id", "panel_year", "carrier", "premium_imputed"))
rebuilt[, panel_id := as.character(panel_id)]
rebuilt <- rebuilt[premium_imputed == 0L & carrier %in% PRICEABLE_CARRIERS]

# Reuse single_tx (already built in L1: single-cell TX 2006+ obs with wall + age_bin)
share_dt <- merge(
  single_tx[, .(panel_id, panel_year, wall, age_bin,
                era = era_of_year(panel_year))],
  rebuilt, by = c("panel_id", "panel_year"))
share_agg <- share_dt[, .(n = .N), by = .(wall, age_bin, era, carrier)]
share_agg[, bin := as.integer(age_bin)]
share_agg[, pos := ifelse(wall == "SW", 9L - bin, 17L - bin)]

# Build long card table for join
card_long <- rbindlist(lapply(PRICEABLE_CARRIERS, function(cr) {
  data.table(pos     = rep(seq_len(16L), times = length(ERAS)),
             era     = rep(ERAS, each = 16L),
             carrier = cr,
             card    = as.vector(pbar_carrier[, , cr]))
}))
card_long <- card_long[!is.na(card)]

# Join shares x cards, compute weighted mean per (pos, era)
imp_dt <- merge(share_agg, card_long, by = c("pos", "era", "carrier"))
imp_by_cell <- imp_dt[, .(imputed_card = sum(n * card) / sum(n)), by = .(pos, era)]

# Fallback for cells with no share data: equal-weight mean over available carrier cards
all_cells <- CJ(pos = seq_len(16L), era = ERAS)
imp_by_cell <- merge(all_cells, imp_by_cell, by = c("pos", "era"), all.x = TRUE)
for (ri in seq_len(nrow(imp_by_cell))) {
  if (is.na(imp_by_cell$imputed_card[ri])) {
    p_i   <- imp_by_cell$pos[ri]
    era_i <- imp_by_cell$era[ri]
    avail <- pbar_carrier[p_i, era_i, PRICEABLE_CARRIERS]
    avail <- avail[!is.na(avail)]
    if (length(avail) > 0L)
      imp_by_cell$imputed_card[ri] <- mean(avail)
  }
}
for (ri in seq_len(nrow(imp_by_cell)))
  pbar_carrier[imp_by_cell$pos[ri], imp_by_cell$era[ri], "IMPUTED"] <-
    imp_by_cell$imputed_card[ri]

stopifnot(all(is.finite(pbar_carrier[, , "IMPUTED"])),
          all(pbar_carrier[, , "IMPUTED"] > 0))
cat(sprintf("  IMPUTED card range: [%.4f, %.4f] (model units)\n",
            min(pbar_carrier[, , "IMPUTED"]), max(pbar_carrier[, , "IMPUTED"])))
cat(sprintf("  pbar_carrier: dim %s | priceable NAs (missing era-cell combos): %d\n",
            paste(dim(pbar_carrier), collapse="x"),
            sum(is.na(pbar_carrier[, , PRICEABLE_CARRIERS]))))

# ==============================================================================
# L2 CONTRACTS  tau[state], D[state]
# ==============================================================================
cat("=== L2: contracts (tau = flat-fee premium, D = deductible) ===\n")
fac_contracts <- fread(file.path(DATA_DIR, "facility_panel.csv"),
                       select = c("panel_id", "panel_year", "state",
                                  "fr_premium_per_tank_yr", "deductible_usd"))
fac_contracts[, panel_id := as.character(panel_id)]
fac_contracts <- fac_contracts[panel_year >= 1999L]

# state time-averages: tau from non-NA premium rows; D from non-NA, non-zero deductible rows
# (deductible_usd == 0 means "no deductible in that facility-year record", not a real $0 deductible)
state_tau <- fac_contracts[!is.na(fr_premium_per_tank_yr),
                            .(tau_usd = mean(fr_premium_per_tank_yr, na.rm = TRUE),
                              n_obs   = .N),
                           by = state]
state_ded <- fac_contracts[!is.na(deductible_usd) & deductible_usd > 0,
                            .(D_usd = mean(deductible_usd, na.rm = TRUE)),
                           by = state]
state_avg <- merge(state_tau, state_ded, by = "state", all.x = TRUE)
state_avg[is.na(D_usd), D_usd := 0]   # states with no positive deductible records -> D=0
setorder(state_avg, state)

# TX: no flat-fee contract; deductible = $5,000 (spec constant)
if (!"TX" %in% state_avg$state) {
  state_avg <- rbind(state_avg, data.table(state = "TX", tau_usd = NA_real_,
                                           D_usd = 5000, n_obs = 0L))
} else {
  state_avg[state == "TX", `:=`(tau_usd = NA_real_, D_usd = 5000)]
}
state_avg[, excluded := as.integer(state %chin% c("KS", "MD"))]

# model units
state_avg[, tau := tau_usd / SCALE]
state_avg[, D   := D_usd   / SCALE]

# states with D == 0: zero-deductible funds (full coverage; L_OOP = 0 = D)
zero_D_states <- state_avg[excluded == 0L & D == 0, state]
if (length(zero_D_states) > 0)
  cat(sprintf("  NOTE: zero-deductible included states (D=0 valid; L_OOP=0): %s\n",
              paste(zero_D_states, collapse=", ")))
# assert D >= 0 all included states (0 is valid; min(D,L)=D=0 still holds)
stopifnot(all(state_avg[excluded == 0L, D] >= 0))
cat("  contracts (model units):\n"); print(state_avg)

# L_cleanup lower bound: UST cleanup costs >> deductibles (> $100k documented minimum)
L_cleanup_lb <- 100000 / SCALE
stopifnot(all(state_avg$D_usd < 100000, na.rm = TRUE))
cat(sprintf("  min(D, L_cleanup_lb=%.0f/unit) == D for all states: PASS\n",
            L_cleanup_lb))

tau_vec      <- setNames(state_avg$tau,      state_avg$state)
D_vec        <- setNames(state_avg$D,        state_avg$state)
excluded_states <- state_avg[excluded == 1L, state]
cat(sprintf("  excluded states: %s\n", paste(excluded_states, collapse=", ")))

# ==============================================================================
# L3 HAZARD  h_aw[16], H evaluator spec, L_OOP
# ==============================================================================
cat("=== L3: hazard h_aw ===\n")
prim  <- readRDS(file.path(RES_DIR, "DCM_Primitives_Replacement_observed.rds"))
h_vec <- prim$h_vec
stopifnot(length(h_vec) == 32L, all(is.finite(h_vec)), all(h_vec > 0), all(h_vec < 1))
# regime-averaged; h_vec[1:16] = FF states, h_vec[17:32] = RB states, indexed (w-1)*8+a
h_aw  <- (h_vec[1:16] + h_vec[17:32]) / 2
stopifnot(length(h_aw) == 16L, all(is.finite(h_aw)))
cat(sprintf("  h_aw range: [%.4f, %.4f]\n", min(h_aw), max(h_aw)))

# H evaluation map (exported as function spec, not data):
#   abar(n): weighted avg age midpoint across occupied cells -> bin via AGE_BREAKS c(0,5,...,35,Inf)
#   wall(n): SW if n_SW >= n_DW, else DW
#   H(n) = h_aw[(wall-1)*8 + abar_bin]   where wall: SW=1, DW=2
# L_OOP[state] = D[state] (since L_cleanup >> D always)

# midpoints for the H evaluator (exported into lookups for 024)
midpts <- AGE_MIDPTS
age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)

# ==============================================================================
# L4 AGE KERNEL  adv[8]
# ==============================================================================
cat("=== L4: age kernel adv ===\n")
ak <- fread(file.path(TAB_DIR, "AE_X4_Age_Transition.csv"))
stopifnot(all(c("bin_now", "bin_next", "prob") %in% names(ak)))
adv <- numeric(8L)
adv_rows <- ak[bin_next == bin_now + 1L]
adv[adv_rows$bin_now] <- adv_rows$prob
# bin 8: adv[8] = 0 (stays by construction)
stopifnot(length(adv) == 8L, all(adv >= 0), all(adv <= 1), adv[8] == 0)
cat(sprintf("  adv: %s\n", paste(round(adv, 4), collapse=" ")))

# ==============================================================================
# L5 G KERNEL  Gmat[G=1..4, G'=1..4, netbin]
# ==============================================================================
cat("=== L5: G transition kernel Gmat ===\n")
pm <- fread(file.path(DATA_DIR, "pm_panel.csv"))
pm[, panel_id := as.character(panel_id)]
setorder(pm, panel_id, panel_year)

# lead G
pm[, G_next := shift(G, type = "lead"), by = panel_id]
pm[, nyr    := shift(panel_year, type = "lead"), by = panel_id]

# netbin: 6 levels
# "-3" to "1" for work rows (k,m != 0,0 and != Exit), "nowork" for (0,0) Maintain
pm[, netbin := fcase(
  action == "X",                                          NA_character_,
  action == "0,0",                                        "nowork",
  default = as.character(pmax(-3L, pmin(1L, m - k))))]

# keep consecutive-year transitions with valid G, G_next, netbin
gtr <- pm[!is.na(G) & !is.na(G_next) & nyr == panel_year + 1L & !is.na(netbin)]
cat(sprintf("  G transition rows: %s\n", format(nrow(gtr), big.mark=",")))

# counts by (G, G_next, netbin)
gcounts <- gtr[, .(n = .N), by = .(G, G_next, netbin)]

# row totals by (G, netbin) for normalization
grow_n <- gcounts[, .(row_n = sum(n)), by = .(G, netbin)]
gcounts <- merge(gcounts, grow_n, by = c("G", "netbin"))
gcounts[, prob := n / row_n]

# thin row fallback: if row_n < 100, replace with pooled "work" row for that G
THIN_ROW <- 100L
thin_keys <- unique(gcounts[row_n < THIN_ROW, .(G, netbin)])
if (nrow(thin_keys) > 0) {
  cat("  thin (G, netbin) rows (n<100) falling back to pooled work row:\n")
  print(thin_keys)
}

# pooled work row per G (aggregate all non-nowork netbins)
pool_work <- gtr[netbin != "nowork", .(n = .N), by = .(G, G_next)]
pool_n    <- pool_work[, .(row_n = sum(n)), by = G]
pool_work <- merge(pool_work, pool_n, by = "G")
pool_work[, prob := n / row_n]

# Build Gmat as named list of 4x4 matrices (one per netbin level)
NETBINS <- c("-3", "-2", "-1", "0", "1", "nowork")
Gmat <- setNames(vector("list", length(NETBINS)), NETBINS)
for (nb in NETBINS) {
  mat <- matrix(0, nrow = 4L, ncol = 4L)
  for (g in 1:4) {
    row_data <- gcounts[G == g & netbin == nb]
    is_thin  <- nrow(row_data) == 0L || row_data$row_n[1L] < THIN_ROW
    if (is_thin && nb != "nowork") {
      # fall back to pooled work row
      row_data <- pool_work[G == g]
    } else if (is_thin && nb == "nowork") {
      # if nowork row is thin: fall back to uniform (shouldn't happen)
      warning(sprintf("nowork row for G=%d has n<100, using uniform fallback", g))
      mat[g, ] <- 0.25
      next
    }
    if (nrow(row_data) > 0) {
      for (gp in 1:4) {
        v <- row_data[G_next == gp, prob]
        mat[g, gp] <- if (length(v) == 1L) v else 0
      }
    }
  }
  Gmat[[nb]] <- mat
}

# assert rowSums == 1 for each matrix
for (nb in NETBINS) {
  rs <- rowSums(Gmat[[nb]])
  if (!all(abs(rs - 1) < 1e-8)) {
    warning(sprintf("Gmat[%s] row sums not 1: %s", nb, paste(round(rs, 6), collapse=",")))
    # re-normalize to machine precision
    Gmat[[nb]] <- Gmat[[nb]] / rowSums(Gmat[[nb]])
  }
}
cat("  Gmat row sums check: all within 1e-8 of 1\n")
cat("  Gmat diagonal (stay prob) by netbin:\n")
for (nb in NETBINS) cat(sprintf("    [%s]: %s\n", nb,
  paste(round(diag(Gmat[[nb]]), 3), collapse=" ")))

# ==============================================================================
# L6 SCALARS
# ==============================================================================
cat("=== L6: scalars ===\n")
BETA    <- 0.9957
gamma_E <- 0.5772156649
sigma   <- 1.0
cat(sprintf("  BETA=%.4f  SCALE=%d  gamma_E=%.10f  sigma=%.1f\n",
            BETA, SCALE, gamma_E, sigma))
stopifnot(BETA == 0.9957)  # locked value from spec

# ==============================================================================
# Assemble and save
# ==============================================================================
cat("=== SAVE: PM_Lookups.rds ===\n")
lookups <- list(
  pbar          = pbar,          # 16 x 3 numeric, model units (legacy / carrier-marginal fallback)
  pbar_carrier  = pbar_carrier,  # 16 x 3 x 7 array [MARG, ERAS, ALL_CARRIERS], model units
  tau           = tau_vec,       # named numeric (state -> tau/SCALE), TX=NA
  D             = D_vec,         # named numeric (state -> D/SCALE)
  excluded_states = excluded_states,
  h_aw          = h_aw,          # length-16 numeric (MARG order)
  midpts        = midpts,        # AGE_MIDPTS, length 8
  age_breaks    = age_breaks,    # c(0,5,...,35,Inf)
  adv           = adv,           # length-8 advance probabilities
  Gmat          = Gmat,          # named list of 4x4 matrices
  G_breaks      = readRDS(file.path(DATA_DIR, "pm_G_breaks.rds")),
  BETA          = BETA,
  SCALE         = SCALE,
  ERAS          = ERAS,
  ALL_CARRIERS  = ALL_CARRIERS
)
out_rds <- file.path(RES_DIR, "PM_Lookups.rds")
saveRDS(lookups, out_rds)
cat(sprintf("  saved %s\n", out_rds))

cat("\n=== SUMMARY ===\n")
cat(sprintf("  pbar dim: %dx%d  range: [%.4f, %.4f]\n",
            nrow(pbar), ncol(pbar), min(pbar), max(pbar)))
cat(sprintf("  D range (incl states): [%.4f, %.4f]\n",
            min(D_vec, na.rm=TRUE), max(D_vec, na.rm=TRUE)))
cat(sprintf("  h_aw range: [%.4f, %.4f]\n", min(h_aw), max(h_aw)))
cat(sprintf("  adv: %s\n", paste(round(adv, 4), collapse=" ")))
cat(sprintf("  BETA=%.4f\n", BETA))

cat("=== PM02 DONE ===\n")
