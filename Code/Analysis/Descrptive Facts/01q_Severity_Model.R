################################################################################
# 01q_Severity_Model.R
# ML severity model (the cost arm of the two-part fair premium). Companion to 01p.
#
# Predicts expected fund-payable cleanup cost per release, E[cost | release, cell],
# from observables — so fair premium = lambda_hat (01p) x S_hat (this) instead of a
# flat pooled severity. Severity is NOT flat: single-walled releases cost ~1.48x
# double-walled (no secondary containment -> bigger spill), and state cost levels
# differ a lot (NM ~ half of CO). This model captures that.
#
# MODEL: Poisson elastic net (PPML, log link) on net-of-deductible cost.
#   PPML targets E[cost] directly, is robust to the heavy right tail, handles the
#   sub-deductible zeros, and is the same glmnet machinery as 01p (family="poisson").
#   Cost is scaled to $1k for numerical stability; predictions scaled back to $.
#   Only ~7,334 claims -> parsimonious + elastic-net shrinkage doing real work.
#
# ESTIMAND: S_hat(age_bin, wall, [state]) = E[net cleanup cost | a release occurs],
#   predicted at single-tank reference exposure.
#
# OUTPUT (Data/Analysis/):
#   dcm_cell_severity_pricing.csv   state, age_bin, has_single_walled, sev_hat,
#                                   sev_lo, sev_hi   (per state + NATIONAL rows)
#   analysis_severity_model.rds     fitted glmnet + metadata
#
# RUN: Rscript "Code/Analysis/Descrptive Facts/01q_Severity_Model.R"
################################################################################

suppressPackageStartupMessages({ library(data.table); library(glmnet); library(here) })
source(here::here("Code", "Helpers", "data_paths.R"))
options(scipen = 999); set.seed(20260627L)

ANALYSIS_DIR <- here("Data", "Analysis"); dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# === LOGGING ===
.log_path <- here("logs", paste0("01q_Severity_Model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt"); sink(.log, type="output"); sink(.log, type="message", append=TRUE)
on.exit({ sink(type="output"); sink(type="message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 01q_Severity_Model.R\nR: %s\nWD: %s\n\n", .log_path, R.version.string, getwd()))

# === PARAMETERS ===
SEV_STATES <- c("CO", "NM", "PA", "TN")     # claim states (LA excluded: facility totals)
D_NM       <- 10000
REF_CAP    <- 3000                          # single-tank reference capacity (matches 01p)
N_BOOT     <- 800L
COST_UNIT  <- 1000                          # fit in $1k for numerical stability
AGE_BIN_LABELS <- c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23","24+")
FEAT_FORMULA <- ~ (age_bin + has_single_walled + active_tanks + total_capacity)^2 + state_f

align_cols <- function(M, cols) {
  out <- matrix(0, nrow(M), length(cols), dimnames = list(NULL, cols))
  cm <- intersect(colnames(M), cols); out[, cm] <- M[, cm]; out
}

# =============================================================================
cat("=== STEP 1: LOAD CLAIMS + NET-OF-DEDUCTIBLE COST ===\n")
# =============================================================================
inc <- fread(z_path("Data","Processed","incident_level_claims.csv"),
             select = c("state","total_cost","total_cost_2023","age_bins",
                        "has_single_walled","active_tanks","total_capacity"))
inc <- inc[state %in% SEV_STATES &
           !is.na(total_cost) & !is.na(total_cost_2023) & total_cost >= 0 & total_cost_2023 >= 0 &
           has_single_walled %in% c(0L,1L) & !is.na(active_tanks) & !is.na(total_capacity) &
           age_bins %in% AGE_BIN_LABELS]
inc[, net_nom  := fifelse(state == "NM", pmax(total_cost - D_NM, 0), total_cost)]
inc[, net_2023 := fifelse(net_nom == 0, 0, net_nom * (total_cost_2023 / total_cost))]
inc[, age_bin  := factor(age_bins, levels = AGE_BIN_LABELS)]
STATE_LEVELS <- sort(unique(inc$state))
inc[, state_f := factor(state, levels = STATE_LEVELS)]
cat(sprintf("Claims used: %d (states %s) | mean net = $%s\n",
            nrow(inc), paste(STATE_LEVELS, collapse=","), format(round(mean(inc$net_2023)), big.mark=",")))

Y <- inc$net_2023 / COST_UNIT
X <- model.matrix(FEAT_FORMULA, data = as.data.frame(inc))[, -1L]
cat(sprintf("Design matrix: %d rows x %d cols\n", nrow(X), ncol(X)))

# =============================================================================
cat("=== STEP 2: POISSON ELASTIC NET (PPML) — tune alpha + lambda ===\n")
# =============================================================================
ALPHAS <- c(0.0, 0.5, 1.0); FOLDS <- 10L
res <- lapply(ALPHAS, function(a) {
  set.seed(20260627L)
  f <- cv.glmnet(X, Y, family = "poisson", alpha = a, nfolds = FOLDS, type.measure = "deviance")
  cat(sprintf("  alpha=%.2f  lambda.min=%.5f  dev=%.4f\n", a, f$lambda.min, min(f$cvm)))
  list(alpha = a, lambda = f$lambda.min, dev = min(f$cvm))
})
best <- res[[which.min(sapply(res, `[[`, "dev"))]]
best_alpha <- best$alpha; best_lambda <- best$lambda
cat(sprintf(">> best alpha=%.2f  lambda=%.5f\n", best_alpha, best_lambda))
fit <- glmnet(X, Y, family = "poisson", alpha = best_alpha, lambda = best_lambda)
cat(sprintf("In-sample mean predicted cost: $%s (raw mean $%s)\n",
            format(round(mean(as.numeric(predict(fit, newx=X, type="response")))*COST_UNIT), big.mark=","),
            format(round(mean(inc$net_2023)), big.mark=",")))

# =============================================================================
cat("=== STEP 3: PREDICT SEVERITY CELL SCHEDULE (+ bootstrap CI) ===\n")
# =============================================================================
grid <- CJ(state = STATE_LEVELS, age_bin = AGE_BIN_LABELS, has_single_walled = c(0L,1L))
grid[, `:=`(active_tanks = 1L, total_capacity = REF_CAP)]
grid[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
grid[, state_f := factor(state, levels = STATE_LEVELS)]
Xg <- align_cols(model.matrix(FEAT_FORMULA, data = as.data.frame(grid))[, -1L, drop=FALSE], colnames(X))
grid[, sev_hat := as.numeric(predict(fit, newx = Xg, type = "response")) * COST_UNIT]

# Bootstrap: resample claims, refit at fixed (alpha,lambda), predict grid -> CI.
n <- nrow(X)
boot <- matrix(NA_real_, nrow(grid), N_BOOT)
for (b in seq_len(N_BOOT)) {
  ix <- sample.int(n, n, replace = TRUE)
  fb <- glmnet(X[ix,], Y[ix], family = "poisson", alpha = best_alpha, lambda = best_lambda)
  boot[, b] <- as.numeric(predict(fb, newx = Xg, type = "response")) * COST_UNIT
}
grid[, sev_lo := apply(boot, 1, quantile, 0.025)]
grid[, sev_hi := apply(boot, 1, quantile, 0.975)]

# NATIONAL = mean over states (one rate card); CI from the across-state-averaged boot draws
natl <- grid[, .(sev_hat = mean(sev_hat)), by = .(age_bin, has_single_walled)]
# national CI: average boot across states within each (age,wall) cell, then percentiles
gi <- grid[, .(idx = .I, age_bin, has_single_walled)]
natl_lohi <- gi[, {
  m <- colMeans(boot[idx, , drop = FALSE])
  .(sev_lo = quantile(m, 0.025), sev_hi = quantile(m, 0.975))
}, by = .(age_bin, has_single_walled)]
natl <- merge(natl, natl_lohi, by = c("age_bin","has_single_walled"))
natl[, state := "NATIONAL"]

cell_out <- rbind(grid[, .(state, age_bin, has_single_walled, sev_hat, sev_lo, sev_hi)],
                  natl[, .(state, age_bin, has_single_walled, sev_hat, sev_lo, sev_hi)])
cell_out[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(cell_out, state, age_bin, has_single_walled)
fwrite(cell_out, file.path(ANALYSIS_DIR, "dcm_cell_severity_pricing.csv"))
cat("Saved: dcm_cell_severity_pricing.csv\n")

cat("\nDIAGNOSTIC — national severity Ŝ by age x wall ($):\n")
d <- dcast(natl, age_bin ~ has_single_walled, value.var = "sev_hat")[match(AGE_BIN_LABELS, age_bin)]
setnames(d, c("0","1"), c("DW","SW"))
print(d[, .(age_bin, DW = round(DW), SW = round(SW), SW_over_DW = round(SW/DW, 2))])
cat("\nState severity (age-9-11 SW, single tank) for reference:\n")
print(grid[age_bin == "9-11" & has_single_walled == 1L, .(state, sev = round(sev_hat))][order(state)])

saveRDS(list(fit = fit, feature_cols = colnames(X), best_alpha = best_alpha, best_lambda = best_lambda,
             state_levels = STATE_LEVELS, age_bin_levels = AGE_BIN_LABELS, ref_cap = REF_CAP,
             cost_unit = COST_UNIT, n_claims = nrow(inc), timestamp = Sys.time()),
        file.path(ANALYSIS_DIR, "analysis_severity_model.rds"))

# =============================================================================
cat("=== STEP 4: SUMMARY ===\n")
# =============================================================================
cat(sprintf("Severity PPML | claims %d | alpha %.2f | lambda %.5f | ref tank cap %d\n",
            nrow(inc), best_alpha, best_lambda, REF_CAP))
cat("Done.\n")
