# ==============================================================================
# 04b_Replacement_Panel_Prep.R
# ==============================================================================
# PURPOSE
#   Build the estimation panel and 32-state DCM primitives for the replacement
#   model (Phase 1 of the spec). Produces two parallel outputs:
#
#     observed:  TX 2006+ (with actual / engine-imputed Mid-Continent premiums)
#                + all controls 1999+
#     extended:  TX 1999+ (with engine-imputed pre-2006 premiums applied flat
#                in nominal terms) + all controls 1999+
#
#   Both share identical state-space construction; they differ only in TX
#   row inclusion. Estimating on each yields a robustness comparison for
#   theta_hat (does adding the engine-imputed pre-2006 TX years move the
#   structural estimates?).
#
# STATE SPACE  (32 cells)
#
#   age_bin  ∈ {1..8}   5-year bins matching 05_Claims_Analysis.r's `age_bins`:
#                       [0,5), [5,10), [10,15), [15,20), [20,25), [25,30),
#                       [30,35), [35, Inf)
#   w_state  ∈ {1, 2}   1 = Single-Walled (SW or Mixed/Unknown)
#                       2 = Double-Walled
#   rho_state∈ {1, 2}   1 = FF (controls all years; TX pre-1999)
#                       2 = RB (TX post-1999)
#   s_idx = (rho-1)*16 + (w-1)*8 + age_bin   ∈ 1..32
#
# OUTPUTS
#
#   Output/Estimation_Results/DCM_Primitives_Replacement_observed.rds
#   Output/Estimation_Results/DCM_Primitives_Replacement_extended.rds
#       Each contains: state_lut, h_vec[32], L_vec[32], P_vec[32],
#                      F_maintain (32x32 sparse), P0_mat (32x2),
#                      age_trans, n_exit, n_replace, pct_replace
#
#   Data/Analysis/dcm_obs_panel_observed.csv
#   Data/Analysis/dcm_obs_panel_extended.csv
#       One row per facility-year with:
#         panel_id, panel_year, state, s_idx, A_bin, w_state, rho_state,
#         premium, y_it, I_replace, reset_state_index, P_close_init
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))

setDTthreads(0L)

OUT_PRIM <- here::here("Output", "Estimation_Results")
OUT_PNL  <- here::here("Data",  "Analysis")
dir.create(OUT_PRIM, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_PNL,  recursive = TRUE, showWarnings = FALSE)

cat("=================================================================\n")
cat("04b: Replacement-model panel + 32-state DCM primitives\n")
cat("=================================================================\n\n")

# ---- Constants ----
N_AGE    <- 8L
N_WALL   <- 2L
N_RHO    <- 2L
N_STATES <- N_AGE * N_WALL * N_RHO   # 32

AGE_BREAKS <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
AGE_LABELS <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")

PRE_REFORM_YEAR <- 1999L
TX_OBSERVED_YEAR <- 2006L

# State-index helper: takes integer (A_bin, w_state, rho_state), returns 1..32
state_idx <- function(A, w, rho) (rho - 1L) * 16L + (w - 1L) * 8L + A


# ==============================================================================
# 1. Load facility panel and assign state index
# ==============================================================================
cat("[1/8] Loading facility panel (prefer local T007-cleaned copy, fall back to Z)...\n")

# T007 Phase 2: use data_in() so the Phase 1-regenerated local facility_panel.csv
# is picked up when present; falls back to Z if no local copy exists.
fac_panel_path <- data_in("Data", "Analysis", "facility_panel.csv")
if (!file.exists(fac_panel_path))
  stop("facility_panel.csv not found: ", fac_panel_path)
cat(sprintf("  Source: %s\n", fac_panel_path))

# Pull only what we need (3.2 GB file).
# n_installs added for T007 Phase 2 last_install_yr_facility computation.
KEEP_COLS <- c("panel_id", "panel_year", "state", "texas_treated",
               "avg_tank_age_dec", "wall_type", "post_1999",
               "any_closure", "facility_exit", "facility_complete_closure",
               "replacement_closure_year", "n_installs",
               "fr_premium_per_tank_yr",
               "active_tanks", "has_single_walled_dec", "has_double_walled_dec",
               "n_tanks_eoy")   # T013: prior-year EOY stock -> BOY size_bin (1.5)

fac_panel <- fread(fac_panel_path, select = KEEP_COLS)

cat(sprintf("  rows = %s | facilities = %s | year range %d-%d\n",
            format(nrow(fac_panel), big.mark = ","),
            format(uniqueN(fac_panel$panel_id), big.mark = ","),
            min(fac_panel$panel_year, na.rm = TRUE),
            max(fac_panel$panel_year, na.rm = TRUE)))

# ------------------------------------------------------------------------------
# TICKET 013 — BOY (beginning-of-year = decision-time) state stamping.
#
# avg_tank_age_dec and wall_type are December/EOY snapshots. Reading them on the
# decision row stamps the POST-action portfolio: 16.2% of closures land in the
# wrong wall cell, every DW full-exit reclassifies SW (0 tanks at EOY), and age
# runs BACKWARD under Maintain (T013 D1/D2). Fix: stamp A_bin/w_state from the
# prior-year December snapshot = the state a forward-looking decision is made IN.
#
# Snapshots are built on the FULL panel BEFORE the pre-1999 filter so year t can
# see year t-1. The hazard re-aggregation in step 2 recomputes its own A_bin/
# w_state internally (from avg_tank_age_dec / has_single_walled), so it does not
# consume fac_panel$A_bin — building these on the full panel is for gap-safety,
# not the hazard merge.
#
# 1.1 Gap-safe prior-year (BOY) snapshots via a +1L key join (NOT a row-shift;
#     the panel has gaps at zero-tank years).
setorder(fac_panel, panel_id, panel_year)
boy <- fac_panel[, .(panel_id,
                     panel_year    = panel_year + 1L,
                     avg_age_boy   = avg_tank_age_dec,
                     wall_type_boy = wall_type,
                     boy_stock     = n_tanks_eoy)]
fac_panel <- merge(fac_panel, boy, by = c("panel_id", "panel_year"),
                   all.x = TRUE)

# 1.2 Fallback for rows with no prior-year row (left-censored first appearance
#     or post-gap reappearance): use the SAME-YEAR value and count them.
n_boy_fallback <- fac_panel[is.na(avg_age_boy), .N]
fac_panel[is.na(avg_age_boy),   avg_age_boy   := avg_tank_age_dec]
fac_panel[is.na(wall_type_boy), wall_type_boy := wall_type]
fac_panel[is.na(boy_stock),     boy_stock     := n_tanks_eoy]
cat(sprintf("  [T013] BOY snapshot: %s of %s fac-panel rows used same-year fallback (%.2f%%)\n",
            format(n_boy_fallback, big.mark = ","),
            format(nrow(fac_panel), big.mark = ","),
            100 * n_boy_fallback / nrow(fac_panel)))

# 1.3 Stamp A_bin and w_state from the BOY snapshots (decision-time state).
fac_panel[, A_bin := as.integer(cut(avg_age_boy, AGE_BREAKS,
                                    labels = 1:N_AGE, right = FALSE,
                                    include.lowest = TRUE))]
fac_panel[is.na(A_bin), A_bin := N_AGE]   # missing age -> oldest bin (conservative)

# w_state: 1 = SW (or Mixed/Unknown), 2 = DW. Per spec: collapse Mixed to SW
# because a single SW tank dominates the risk profile for a facility.
fac_panel[, w_state := fcase(
  wall_type_boy == "Double-Walled", 2L,
  default = 1L)]

# rho_state: 1 = FF, 2 = RB. Controls always FF; TX FF before 1999, RB after.
# UNCHANGED — year-based (T013 confirmed 0 regime leakage).
fac_panel[, rho_state := fcase(
  texas_treated == 1L & panel_year >= PRE_REFORM_YEAR, 2L,
  default = 1L)]

fac_panel[, s_idx := state_idx(A_bin, w_state, rho_state)]
stopifnot(all(fac_panel$s_idx %in% 1:N_STATES))

# 1.5 BOY portfolio-size bin (decision-time stock) for downstream use. Not used
#     in estimation here — Ticket 015 consumes it. Character label, explicit bins
#     so 0/NA aren't mislabeled.
fac_panel[, size_bin := fcase(
  boy_stock == 1L, "1",
  boy_stock == 2L, "2",
  boy_stock == 3L, "3",
  boy_stock >= 4L, "4+",
  default = NA_character_)]


# ==============================================================================
# 2. Hazard cell means from the full-panel predictions saved by 01n
# ==============================================================================
# 01n trains the elnet on pre-1999 (no leakage from the regime change), then
# scores every facility-year in the study sample (pre AND post 1999) using
# the trained model + Platt scaling. Cell means are computed on the post-1999
# subset because that's the within-cell composition our DCM estimation panel
# faces. Hazard is treated as time-invariant within (age, wall) — the regime
# change shifts who's still in each cell, not the underlying physical risk.
cat("[2/8] Loading full-panel hazard predictions from 01n...\n")

haz_path <- z_path("Data", "Analysis", "analysis_hazard_predictions_full.csv")
if (!file.exists(haz_path))
  stop("analysis_hazard_predictions_full.csv not found: ", haz_path,
       "\n  Run the updated 01n_CVValidation.R first.")

haz_full <- fread(haz_path)
cat(sprintf("  rows = %s | year range %d-%d\n",
            format(nrow(haz_full), big.mark = ","),
            min(haz_full$panel_year), max(haz_full$panel_year)))

# Use post-reform predictions for cell means (matches estimation sample).
haz_post <- haz_full[panel_year >= PRE_REFORM_YEAR &
                     !is.na(pred_elnet_full)]

# Need avg_tank_age_dec from fac_panel to apply the 5-year binning. 01n's
# 9 3-year bins (age_bin) don't align cleanly with our 8 5-year buckets.
haz_post <- merge(
  haz_post[, .(panel_id, panel_year, has_single_walled,
               pred = pred_elnet_full)],
  fac_panel[, .(panel_id, panel_year, avg_tank_age_dec)],
  by = c("panel_id", "panel_year"), all.x = FALSE
)
haz_post[, A_bin := as.integer(cut(avg_tank_age_dec, AGE_BREAKS,
                                   labels = 1:N_AGE, right = FALSE,
                                   include.lowest = TRUE))]
haz_post[is.na(A_bin), A_bin := N_AGE]
haz_post[, w_state := ifelse(has_single_walled == 1L, 1L, 2L)]

# Cell mean predicted hazard, regime-agnostic (replicated across rho below)
h_cell <- haz_post[, .(h_hat = mean(pred, na.rm = TRUE),
                       n_obs = .N),
                   by = .(A_bin, w_state)][order(A_bin, w_state)]
cat("  Hazard cells (per 1000 facility-years, post-1999):\n")
print(h_cell[, .(A_bin, w_state, h_per1k = round(h_hat * 1000, 2), n_obs)])

# Build 16-cell vector indexed by (w_state, A_bin); replicate across regimes
h_vec_aw <- numeric(N_AGE * N_WALL)
for (a in 1:N_AGE) for (w in 1:N_WALL) {
  v <- h_cell[A_bin == a & w_state == w, h_hat]
  h_vec_aw[(w - 1L) * N_AGE + a] <- if (length(v) == 1L) v else NA_real_
}
# Fill any NA cells with overall mean (should be rare)
if (anyNA(h_vec_aw)) {
  cat(sprintf("  WARNING: %d hazard cells empty; filling with grand mean\n",
              sum(is.na(h_vec_aw))))
  h_vec_aw[is.na(h_vec_aw)] <- mean(h_vec_aw, na.rm = TRUE)
}
# Replicate across rho: same hazard whether FF or RB (physical risk).
h_vec <- rep(h_vec_aw, times = N_RHO)
stopifnot(length(h_vec) == N_STATES, !anyNA(h_vec))

# Now apply the pre-reform filter for the rest of the script (estimation panel
# is 1999+ only; pre-1999 was needed only for the hazard merge above).
n_before <- nrow(fac_panel)
fac_panel <- fac_panel[panel_year >= PRE_REFORM_YEAR]
cat(sprintf("  Filter to panel_year >= %d: %s of %s rows kept\n",
            PRE_REFORM_YEAR,
            format(nrow(fac_panel), big.mark = ","),
            format(n_before,        big.mark = ",")))


# ==============================================================================
# 3. Loss vector from 05's dcm_state_loss_levels.csv
# ==============================================================================
cat("[3/8] Loading and aligning loss CSV...\n")

loss_path <- here::here("Output", "Tables", "dcm_state_loss_levels.csv")
if (!file.exists(loss_path))
  stop("dcm_state_loss_levels.csv missing — run 05_Claims_Analysis.r first.\n",
       "  expected at ", loss_path)

L_raw <- fread(loss_path)
# 05 binning: same 8 5-year buckets; sw_tercile in {"0-33%","34-66%","67-100%","Unknown (NM)"}
# Map sw_tercile -> w_state. "67-100%" = mostly SW (w=1), "0-33%" = mostly DW (w=2).
# 34-66% and Unknown are conservatively assigned to SW (w=1).
L_raw[, A_bin := match(age_bins, AGE_LABELS)]
L_raw[, w_state := fcase(
  sw_tercile == "0-33%",   2L,
  sw_tercile == "67-100%", 1L,
  default = 1L)]

# If multiple sw_tercile rows collapse into one (A_bin, w_state) cell,
# weight by n_claims.
L_cell <- L_raw[!is.na(A_bin), .(
  L_hat = sum(L_hat * n_claims, na.rm = TRUE) / sum(n_claims, na.rm = TRUE)
), by = .(A_bin, w_state)][order(A_bin, w_state)]

cat("  Loss cells (mean expected cost, 2023 USD):\n")
print(L_cell[, .(A_bin, w_state, L = round(L_hat, 0))])

L_vec_aw <- numeric(N_AGE * N_WALL)
for (a in 1:N_AGE) for (w in 1:N_WALL) {
  v <- L_cell[A_bin == a & w_state == w, L_hat]
  L_vec_aw[(w - 1L) * N_AGE + a] <- if (length(v) == 1L) v else NA_real_
}
if (anyNA(L_vec_aw)) {
  cat(sprintf("  WARNING: %d loss cells empty; filling with grand mean\n",
              sum(is.na(L_vec_aw))))
  L_vec_aw[is.na(L_vec_aw)] <- mean(L_vec_aw, na.rm = TRUE)
}
# Scale to model units. The estimator's flow utility was calibrated to ~1.0
# annual profit per facility-year, with premiums on the order of $300-$800/tank.
# Cleanup losses from 05 are ~$200K-$700K. Putting all dollar quantities on the
# same scale (we use $10,000 = 1 unit, matching 03_Welfare.R's SCALE_FACTOR)
# keeps gamma_risk on a comparable magnitude to gamma_price.
SCALE_FACTOR <- 10000
L_vec_aw <- L_vec_aw / SCALE_FACTOR
L_vec    <- rep(L_vec_aw, times = N_RHO)
stopifnot(length(L_vec) == N_STATES, !anyNA(L_vec))


# ==============================================================================
# 4. Premium variable (observation-level), then aggregate to state-level
# ==============================================================================
cat("[4/8] Building observation-level premium...\n")

# 4a. TX premium from 04a wrapper output
tx_prem_path <- here::here("Data", "Analysis",
                           "tx_midcont_premium_all_1999_onwards.csv")
if (!file.exists(tx_prem_path))
  stop("Run 04a_TX_Premium_All_1999_onwards.R first to produce ", tx_prem_path)

tx_prem <- fread(tx_prem_path)
cat(sprintf("  TX premium rows: %s\n", format(nrow(tx_prem), big.mark = ",")))

fac_panel <- merge(
  fac_panel,
  tx_prem[, .(panel_id, panel_year,
              tx_mean_tank_premium = mean_tank_premium,
              tx_is_imputed_pre2006 = is_imputed_pre2006)],
  by = c("panel_id", "panel_year"), all.x = TRUE
)

# 4b. Combined premium variable (per tank-year):
#   - TX rows: Mid-Continent mean_tank_premium from 04a
#   - Control rows: state_fr_premium fr_premium_per_tank_yr from 02b
fac_panel[, premium_per_tank := fcase(
  texas_treated == 1L & !is.na(tx_mean_tank_premium), as.double(tx_mean_tank_premium),
  texas_treated == 0L,                                as.double(fr_premium_per_tank_yr),
  default = NA_real_
)]
fac_panel[, premium_per_tank_scaled := premium_per_tank / SCALE_FACTOR]

cat("  Premium availability:\n")
print(fac_panel[, .(
  with_premium = sum(!is.na(premium_per_tank)),
  total        = .N,
  pct          = round(100 * mean(!is.na(premium_per_tank)), 1)
), by = .(state, texas_treated)][order(-texas_treated, state)])


# ==============================================================================
# 5. Build estimation observation panel
# ==============================================================================
cat("[5/8] Building observation-level estimation panel...\n")

# T007 Phase 2: clean action definitions per researcher's plain-English mapping.
#   Replace = closure where facility installs again later anywhere
#   Exit    = all tanks closed AND no future installs at this facility ever
#   Maintain = anything else (including partial closures)

setorder(fac_panel, panel_id, panel_year)

# Last year this facility installed anything (churn-excluded n_installs after Phase 1)
fac_panel[, last_install_yr_facility := {
  yrs <- panel_year[n_installs > 0L]
  if (length(yrs) == 0L) NA_integer_ else max(yrs)
}, by = panel_id]

# True exit: complete closure AND no future install (panel_year >= last install year)
fac_panel[, is_true_exit := as.integer(
  facility_complete_closure == 1L &
  (is.na(last_install_yr_facility) | panel_year >= last_install_yr_facility)
)]

# Retrofit/Replace: replacement_closure_year already correctly built in post-fix 02b
fac_panel[, is_retrofit := as.integer(replacement_closure_year == 1L)]

# Action mapping: only true exits and retrofits are "closure" actions
fac_panel[, y_it := as.integer(is_true_exit == 1L | is_retrofit == 1L)]

# I_replace: NA for Maintain, 1 for Retrofit, 0 for Exit
fac_panel[, I_replace := fcase(
  y_it == 0L,                                   NA_integer_,
  is_retrofit == 1L,                            1L,
  is_true_exit == 1L & is_retrofit == 0L,       0L,
  default = NA_integer_)]

# Next-year wall state (for reset_state_index of replacers)
setorder(fac_panel, panel_id, panel_year)
fac_panel[, w_state_next := shift(w_state, -1L, type = "lead"), by = panel_id]

fac_panel[, reset_state_index := fcase(
  I_replace == 1L & !is.na(w_state_next),
    state_idx(1L, w_state_next, rho_state),
  I_replace == 1L & is.na(w_state_next),
    state_idx(1L, w_state, rho_state),       # fallback: same wall type
  default = NA_integer_)]

# T013: reset_state_index logic UNCHANGED (F_replace single-tank reset is out of
# scope -> Ticket 014). w_state_next = shift(w_state) now references the BOY
# w_state, so the reset target is next period's decision-time wall — BOY-consistent
# (an SW->DW retrofitter lands in a DW state next period).
cat("  [T013] reset_state_index unchanged; now BOY-consistent via shift(w_state).\n")


# ==============================================================================
# 6. Age-transition matrix from observed Maintain-year transitions
# ==============================================================================
cat("[6/8] Estimating age transitions and building F_maintain...\n")

fac_panel[, A_bin_next := shift(A_bin, -1L, type = "lead"), by = panel_id]

age_trans <- fac_panel[
  !is.na(A_bin_next) & y_it == 0L,
  .(pi_up   = mean(A_bin_next > A_bin, na.rm = TRUE),
    pi_stay = mean(A_bin_next == A_bin, na.rm = TRUE),
    n_obs   = .N),
  by = A_bin][order(A_bin)]

# Bin N_AGE is absorbing (top-coded "35+", oldest bin)
age_trans[A_bin == N_AGE, `:=`(pi_up = 0, pi_stay = 1)]

# Sanity: pi_up + pi_stay = 1 (within numerical tolerance)
age_trans[, pi_sum := pi_up + pi_stay]
if (any(abs(age_trans$pi_sum - 1) > 0.05)) {
  cat("  WARNING: age transition probs don't sum to 1 cleanly — possible regression\n")
  cat("  (firms aging backwards). Normalizing.\n")
  age_trans[, `:=`(
    pi_up   = pi_up   / pi_sum,
    pi_stay = pi_stay / pi_sum
  )]
}
print(age_trans[, .(A_bin, pi_up = round(pi_up, 4),
                    pi_stay = round(pi_stay, 4), n_obs)])

# Build F_maintain [N_STATES x N_STATES]: only age advances under Maintain;
# wall and regime are time-invariant.
F_maintain <- Matrix(0, N_STATES, N_STATES, sparse = TRUE)
for (rho in 1:N_RHO) for (w in 1:N_WALL) for (a in 1:N_AGE) {
  s_from <- state_idx(a, w, rho)
  pu <- age_trans[A_bin == a, pi_up]
  ps <- age_trans[A_bin == a, pi_stay]

  F_maintain[s_from, s_from] <- ps
  if (a < N_AGE) {
    s_up <- state_idx(a + 1L, w, rho)
    F_maintain[s_from, s_up] <- pu
  }
}
stopifnot(all(abs(rowSums(F_maintain) - 1) < 1e-10))


# ==============================================================================
# 7. State-level premium vector (mean over observations in cell)
# ==============================================================================
cat("[7/8] Aggregating premium to state level...\n")

state_prem_obs <- fac_panel[!is.na(premium_per_tank_scaled), .(
  P_hat = mean(premium_per_tank_scaled, na.rm = TRUE),
  n_obs = .N
), by = s_idx][order(s_idx)]

P_vec <- numeric(N_STATES)
for (s in 1:N_STATES) {
  v <- state_prem_obs[s_idx == s, P_hat]
  P_vec[s] <- if (length(v) == 1L) v else NA_real_
}
# Empty cells (no observations): impute by using the (a,w) cell mean across
# the other regime, then fall back to grand mean. This keeps the estimator
# from feeding NaN into the value function.
empty_cells <- which(is.na(P_vec))
if (length(empty_cells) > 0L) {
  cat(sprintf("  %d state cells lack premium data — imputing\n",
              length(empty_cells)))
  for (s in empty_cells) {
    rho <- (s - 1L) %/% 16L + 1L
    rest <- s - (rho - 1L) * 16L         # (w-1)*8 + a, regime-agnostic
    other_rho <- 3L - rho
    s_other <- (other_rho - 1L) * 16L + rest
    P_vec[s] <- P_vec[s_other]
  }
  if (anyNA(P_vec)) {
    P_vec[is.na(P_vec)] <- mean(P_vec, na.rm = TRUE)
  }
}
stopifnot(!anyNA(P_vec), length(P_vec) == N_STATES)


# ==============================================================================
# 8. CCP initialization (smooth logit) and write outputs (two samples)
# ==============================================================================
cat("[8/8] Initializing CCPs and writing observed + extended outputs...\n")

# Smooth-logit CCP init at observation level. Avoids ln(0) in NPL Step A.
# Use only obs with valid premium so the model sees a coherent likelihood.
ccp_dat <- fac_panel[!is.na(premium_per_tank_scaled) & !is.na(y_it)]
ccp_fit <- glm(y_it ~ A_bin + I(A_bin^2) + factor(w_state) + factor(rho_state),
               data = ccp_dat, family = binomial)
ccp_dat[, P_close_init := pmax(pmin(predict(ccp_fit, type = "response"),
                                    1 - 1e-6), 1e-6)]

# Aggregate to state-level CCPs (used as P0 for the NPL inner loop)
P0_state <- ccp_dat[, .(P_close = mean(P_close_init)), by = s_idx]
all_states <- data.table(s_idx = 1:N_STATES)
P0_state <- merge(all_states, P0_state, by = "s_idx", all.x = TRUE)
P0_state[is.na(P_close), P_close := mean(ccp_dat$P_close_init)]
P0_mat <- cbind(maintain = 1 - P0_state$P_close,
                close    = P0_state$P_close)


# ---- Build state lookup ----
state_lut <- CJ(rho_state = 1:N_RHO, w_state = 1:N_WALL, A_bin = 1:N_AGE)
state_lut[, s_idx := state_idx(A_bin, w_state, rho_state)]
setorder(state_lut, s_idx)


# Closure composition (used to flag if K is weakly identified)
closure_summary <- function(dat, label) {
  n_exit    <- dat[y_it == 1L & I_replace == 0L, .N]
  n_replace <- dat[y_it == 1L & I_replace == 1L, .N]
  n_close   <- n_exit + n_replace
  pct_replace <- if (n_close > 0) n_replace / n_close else 0
  cat(sprintf("  %s closures: %d exit, %d replace (%.1f%%)\n",
              label, n_exit, n_replace, 100 * pct_replace))
  if (pct_replace < 0.05)
    cat("    WARNING: replacement share < 5% — K weakly identified\n")
  list(n_exit = n_exit, n_replace = n_replace, pct_replace = pct_replace)
}


write_sample <- function(dat, primitives_path, panel_path, label) {
  cat(sprintf("\n--- Sample: %s ---\n", label))
  cat(sprintf("  rows = %s | facilities = %s\n",
              format(nrow(dat), big.mark = ","),
              format(uniqueN(dat$panel_id), big.mark = ",")))

  # Recompute state-level premium on this subsample so observed and extended
  # samples don't share the same P_vec (they shouldn't — extended adds
  # imputed pre-2006 TX, which shifts the cell means).
  prem_s <- dat[!is.na(premium_per_tank_scaled),
                .(P_hat = mean(premium_per_tank_scaled)), by = s_idx]
  prem_s <- merge(all_states, prem_s, by = "s_idx", all.x = TRUE)
  P_vec_s <- prem_s$P_hat
  empties <- which(is.na(P_vec_s))
  if (length(empties)) {
    for (s in empties) {
      rho <- (s - 1L) %/% 16L + 1L
      rest <- s - (rho - 1L) * 16L
      other_rho <- 3L - rho
      P_vec_s[s] <- P_vec_s[(other_rho - 1L) * 16L + rest]
    }
    if (anyNA(P_vec_s)) P_vec_s[is.na(P_vec_s)] <- mean(P_vec_s, na.rm = TRUE)
  }

  comp <- closure_summary(dat, label)

  prims <- list(
    state_lut    = state_lut,
    h_vec        = h_vec,
    L_vec        = L_vec,
    P_vec        = P_vec_s,
    F_maintain   = F_maintain,
    P0_mat       = P0_mat,
    age_trans    = age_trans,
    n_exit       = comp$n_exit,
    n_replace    = comp$n_replace,
    pct_replace  = comp$pct_replace,
    sample_label = label,
    n_obs        = nrow(dat),
    n_facilities = uniqueN(dat$panel_id)
  )
  saveRDS(prims, primitives_path)
  cat(sprintf("  saved primitives -> %s\n", primitives_path))

  obs_out <- dat[, .(panel_id, panel_year, state, texas_treated,
                     s_idx, A_bin, w_state, rho_state,
                     premium = premium_per_tank_scaled,
                     y_it, I_replace, reset_state_index,
                     P_close_init, size_bin, boy_stock)]   # T013 1.5: BOY size
  fwrite(obs_out, panel_path)
  cat(sprintf("  saved obs panel -> %s\n", panel_path))
}


# T007 Phase 2: Back up T005-era observed outputs before overwriting
cat("[T007] Backing up T005-era observed panel and primitives to _pre_T007/...\n")
pre_t007_pnl  <- file.path(OUT_PNL,  "_pre_T007")
pre_t007_prim <- file.path(OUT_PRIM, "_pre_T007")
dir.create(pre_t007_pnl,  recursive = TRUE, showWarnings = FALSE)
dir.create(pre_t007_prim, recursive = TRUE, showWarnings = FALSE)

for (.src in c(file.path(OUT_PNL,  "dcm_obs_panel_observed.csv"),
               file.path(OUT_PRIM, "DCM_Primitives_Replacement_observed.rds"))) {
  if (file.exists(.src)) {
    .dst <- file.path(if (grepl("\\.rds$", .src)) pre_t007_prim else pre_t007_pnl,
                      basename(.src))
    file.copy(.src, .dst, overwrite = TRUE)
    cat(sprintf("  Backed up: %s\n", .dst))
  }
}

# T007 Phase 2: action-distribution diff log vs T005-era panel
cat("[T007] Computing y_it diff vs T005-era dcm_obs_panel_observed.csv...\n")
old_obs_path <- file.path(pre_t007_pnl, "dcm_obs_panel_observed.csv")
if (file.exists(old_obs_path)) {
  old_obs <- fread(old_obs_path, select = c("panel_id", "panel_year", "y_it", "I_replace"))
  # Use same sample filter as sample_observed below
  new_tmp <- ccp_dat[!(texas_treated == 1L & panel_year < TX_OBSERVED_YEAR),
                     .(panel_id, panel_year, y_it, I_replace)]
  merged_diff <- merge(old_obs, new_tmp, by = c("panel_id", "panel_year"),
                       suffixes = c("_old", "_new"), all = FALSE)
  n_flip_yit <- merged_diff[y_it_old != y_it_new, .N]
  cat(sprintf("  Matched rows: %s | y_it flipped: %s (%.2f%%)\n",
      format(nrow(merged_diff), big.mark = ","),
      format(n_flip_yit,        big.mark = ","),
      100 * n_flip_yit / nrow(merged_diff)))
  cat("  Old action distribution:\n")
  print(old_obs[, .(N = .N, pct = round(100 * .N / nrow(old_obs), 2)),
                by = y_it][order(y_it)])
  cat("  New action distribution:\n")
  print(new_tmp[, .(N = .N, pct = round(100 * .N / nrow(new_tmp), 2)),
                by = y_it][order(y_it)])
  rm(old_obs, new_tmp, merged_diff)
} else {
  cat("  No T005-era file found at backup path; skipping diff.\n")
}

# Sample A — observed only: exclude TX pre-2006 (insufficient premium data)
sample_observed <- ccp_dat[!(texas_treated == 1L & panel_year < TX_OBSERVED_YEAR)]

# Sample B — extended: keep TX 1999+ with imputed pre-2006 premiums from 04a
sample_extended <- ccp_dat   # already filtered to >= PRE_REFORM_YEAR

write_sample(
  sample_observed,
  file.path(OUT_PRIM, "DCM_Primitives_Replacement_observed.rds"),
  file.path(OUT_PNL,  "dcm_obs_panel_observed.csv"),
  "observed (TX 2006+, controls 1999+)"
)

write_sample(
  sample_extended,
  file.path(OUT_PRIM, "DCM_Primitives_Replacement_extended.rds"),
  file.path(OUT_PNL,  "dcm_obs_panel_extended.csv"),
  "extended (TX 1999+ with engine-imputed pre-2006, controls 1999+)"
)


cat("\n04b complete.\n")
