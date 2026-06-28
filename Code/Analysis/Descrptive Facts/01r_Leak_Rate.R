################################################################################
# 01r_Leak_Rate.R  —  FREQUENCY arm of the two-part fair premium (Ticket 037)
#
# First-principles per-tank leak RATE: leaks per tank-year, by age x wall.
#   - Outcome n_leaks (count, every leak), offset log(active_tanks) -> PER-TANK rate.
#   - ALL detection-era facility-years (1990-2016), recurrent (no never-leaked filter).
#   - Poisson elastic net (PPML) on a SPARSE design; calendar control year_f.
#   - NO LEAKAGE: grouped CV folds BY FACILITY, STRATIFIED BY STATE; per-unit fitted
#     values are PREVALIDATED (out-of-sample, cv.glmnet keep=TRUE -> fit.preval).
#   - Cell schedules = fitted-values-then-average (exposure/tank-year weighted).
#   - Facility-cluster bootstrap (parallel) -> cell CIs.
#   - Two binnings: FIGURE (9 three-year age bins) + DCM (8 five-year, keyed wall x age).
#
# OUTPUTS (Data/Analysis/):
#   dcm_cell_hazard_pricing.csv   figure bins: state,age_bin,has_single_walled,lambda,lambda_lo,lambda_hi (+NATIONAL)
#   dcm_cell_hazard_struct.csv    DCM bins (national): wall,age_bin(1..8),lambda,lambda_lo,lambda_hi  (16 rows)
#   analysis_leak_rate_predictions.csv  per fac-yr OOS rate: panel_id,panel_year,state,has_single_walled,age_bin,active_tanks,mu_tank
#   analysis_leak_rate_model.rds  final fit + foldid + bootstrap draws + OOS metrics (reload, no refit)
#
# SMOKE: LEAKRATE_SMOKE=1 subsamples facilities + trims bootstrap/alpha (schema unchanged).
# RUN:   & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" "Code/Analysis/Descrptive Facts/01r_Leak_Rate.R"
################################################################################

suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(glmnet); library(doParallel); library(here)
})
source(here::here("Code", "Helpers", "data_paths.R"))
options(scipen = 999); set.seed(20260627L); setDTthreads(0L)
ANALYSIS_DIR <- here("Data", "Analysis"); dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# === LOGGING ===
.log_path <- here("logs", paste0("01r_Leak_Rate_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt"); sink(.log, type="output"); sink(.log, type="message", append=TRUE)
on.exit({ sink(type="output"); sink(type="message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 01r_Leak_Rate.R\nR: %s\nWD: %s\n\n", .log_path, R.version.string, getwd()))

# === PARAMETERS ===
SMOKE          <- nzchar(Sys.getenv("LEAKRATE_SMOKE"))
DETECTION_START<- 1990L; TRAIN_END <- 2016L
K              <- if (SMOKE) 3L else 10L
ALPHAS         <- if (SMOKE) c(0.5, 1) else c(0, 0.25, 0.5, 0.75, 1)   # smoke drops slow ridge
NLAMBDA        <- if (SMOKE) 20L else 60L
NBOOT          <- if (SMOKE) 20L else 300L
SMOKE_NFAC     <- 8000L
NWORKERS       <- max(1L, min(parallel::detectCores() - 1L, as.integer(Sys.getenv("LEAKRATE_NWORKERS", 8L))))
CHUNK_ROWS     <- if (SMOKE) 25000L else as.integer(Sys.getenv("LEAKRATE_CHUNK_ROWS", 250000L))  # row-chunk size for memory-safe design build
CONTROL_STATES <- c("ME","NM","AR","OK","LA","KS","MT","ID","SD","AL","MN","NC","IL","MA","OH","PA","TN","VA","CO")
ALL_STUDY_STATES <- c("TX", CONTROL_STATES)
AGE9_LAB <- c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23","24+")   # figure bins
AGE8_BRK <- c(0,5,10,15,20,25,30,35,Inf)                                        # DCM bins (PM03)
FEAT <- ~ (age_bin + has_single_walled + active_tanks + total_capacity +
           has_gasoline_year + has_diesel_year)^2 + state_f + year_f

wmean <- function(x, w) sum(x * w) / sum(w)   # exposure(tank-year)-weighted mean of per-tank rate

# Memory-safe sparse design build. A one-shot sparse.model.matrix() on the full panel
# spikes RAM during CONSTRUCTION — the saturated (...)^2 + state_f + year_f interaction
# expansion builds large transient blocks even though the final dgCMatrix is small (~0.5GB).
# Build it in row-chunks with factor levels PINNED via xlev (+ drop.unused.levels=FALSE) so
# every chunk yields an IDENTICAL column set, drop the per-chunk intercept, then rbind. The
# transient peak scales with chunk_rows, not nrow(data). Row order is preserved (sequential
# chunks), so X stays aligned with y/off/foldid built from the same d.
build_design_chunked <- function(formula, data, xlev, chunk_rows) {
  n      <- nrow(data)
  starts <- seq.int(1L, n, by = chunk_rows)
  parts  <- vector("list", length(starts))
  ref_cols <- NULL
  for (i in seq_along(starts)) {
    lo <- starts[i]; hi <- min(lo + chunk_rows - 1L, n)
    mm <- sparse.model.matrix(formula, data = data[lo:hi],
                              xlev = xlev, drop.unused.levels = FALSE)[, -1L, drop = FALSE]
    if (is.null(ref_cols)) ref_cols <- colnames(mm) else stopifnot(identical(colnames(mm), ref_cols))
    parts[[i]] <- mm
    cat(sprintf("  [%s] design chunk %d/%d rows %s-%s: %d cols\n",
                format(Sys.time(),"%H:%M:%S"), i, length(starts),
                format(lo, big.mark=","), format(hi, big.mark=","), ncol(mm))); flush(.log)
    rm(mm); gc(verbose = FALSE)
  }
  X <- do.call(rbind, parts)
  rm(parts); gc(verbose = FALSE)
  stopifnot(nrow(X) == n, inherits(X, "CsparseMatrix"))
  X
}

# =============================================================================
cat("=== STEP 1: LOAD + SAMPLE (detection era, recurrent) ===\n")
# =============================================================================
PANEL <- z_path("Data","Processed","facility_leak_behavior_annual.csv")
keep <- c("panel_id","panel_year","n_leaks","active_tanks","total_capacity","has_single_walled",
          "has_gasoline_year","has_diesel_year","state","fac_wall","fac_fuel","age_bins","avg_tank_age")
d <- fread(PANEL, select = keep)
d <- d[panel_year >= DETECTION_START & panel_year <= TRAIN_END &
       !is.na(active_tanks) & active_tanks > 0 & !is.na(total_capacity) & !is.na(n_leaks) &
       has_single_walled %in% c(0L,1L) & !is.na(avg_tank_age) &
       fac_wall != "Unknown-Wall" & fac_fuel != "Unknown-Fuel" &
       age_bins %in% AGE9_LAB & state %in% ALL_STUDY_STATES]
if (SMOKE) {
  set.seed(20260627L); fk <- sample(unique(d$panel_id), min(uniqueN(d$panel_id), SMOKE_NFAC))
  d <- d[panel_id %in% fk]; cat(sprintf("*** SMOKE: %s facilities ***\n", format(length(fk), big.mark=",")))
}
d[, age_bin  := factor(age_bins, levels = AGE9_LAB)]                                   # 9-bin feature
d[, age8     := as.integer(cut(avg_tank_age, AGE8_BRK, labels = 1:8, right = FALSE, include.lowest = TRUE))]
d[is.na(age8), age8 := 8L]
d[, wall     := fifelse(has_single_walled == 1L, "SW", "DW")]
STATE_LEVELS <- sort(unique(d$state)); YEAR_LEVELS <- sort(unique(d$panel_year))
d[, state_f := factor(state, levels = STATE_LEVELS)]
d[, year_f  := factor(panel_year, levels = YEAR_LEVELS)]
cat(sprintf("Facility-years: %s | facilities: %s | states %d | leaks: %s | tank-yrs: %s\n",
            format(nrow(d), big.mark=","), format(uniqueN(d$panel_id), big.mark=","), uniqueN(d$state),
            format(sum(d$n_leaks), big.mark=","), format(sum(d$active_tanks), big.mark=",")))
raw_rate <- sum(d$n_leaks) / sum(d$active_tanks)
cat(sprintf("RAW per-tank rate = sum(n_leaks)/sum(active_tanks) = %.5f (calibration target)\n", raw_rate))

# =============================================================================
cat("=== STEP 2: SPARSE DESIGN + GROUPED/STRATIFIED FOLDS ===\n")
# =============================================================================
xlev <- list(age_bin = levels(d$age_bin), state_f = levels(d$state_f), year_f = levels(d$year_f))
cat(sprintf("Building sparse design in row-chunks of %s rows (memory-safe)...\n", format(CHUNK_ROWS, big.mark=",")))
X   <- build_design_chunked(FEAT, d, xlev, CHUNK_ROWS)
y   <- d$n_leaks
off <- log(d$active_tanks)
cat(sprintf("Sparse X: %s rows x %d cols (%.2f%% nonzero)\n",
            format(nrow(X), big.mark=","), ncol(X), 100*length(X@x)/(as.numeric(nrow(X))*ncol(X))))

# foldid: assign each FACILITY to a fold WITHIN its state (stratified), so all of a
# facility's rows share a fold AND every state appears in every training fold.
fac_tab <- unique(d[, .(panel_id, state)])
set.seed(20260627L)
fac_tab[, fold := { n <- .N; ((sample.int(n) - 1L) %% K) + 1L }, by = state]
d <- merge(d, fac_tab[, .(panel_id, fold)], by = "panel_id", sort = FALSE)
foldid <- d$fold
stopifnot(length(foldid) == nrow(X), all(table(foldid) > 0))
cat(sprintf("Folds: K=%d, grouped by facility, stratified by %d states\n", K, uniqueN(d$state)))

# =============================================================================
cat("=== STEP 3: ALPHA TUNE (shared foldid) -> best (alpha, lambda) ===\n")
# =============================================================================
cl <- makeCluster(NWORKERS); registerDoParallel(cl); on.exit(stopCluster(cl), add = TRUE)
cat(sprintf("Workers: %d\n", NWORKERS)); flush(.log)
# ALPHA is robust and the cvdev-vs-alpha curve is near-flat (p=108 << n; lambda.min ~ 0),
# and lambda is re-tuned on the FULL data in STEP 4 -> selecting alpha on a facility
# subsample is safe and ~1/TUNE_FRAC faster. Subsample FACILITIES (state-stratified, whole
# facilities) so the grouped folds stay valid and every (state,fold) cell is populated.
TUNE_FRAC <- as.numeric(Sys.getenv("LEAKRATE_TUNE_FRAC", "1"))
if (TUNE_FRAC < 1) {
  set.seed(20260627L)
  fac_keep  <- fac_tab[, .SD[sample(.N, max(1L, round(.N * TUNE_FRAC)))], by = state]$panel_id
  tune_rows <- which(d$panel_id %in% fac_keep)
  Xt <- X[tune_rows, , drop = FALSE]; yt <- y[tune_rows]; offt <- off[tune_rows]; foldt <- foldid[tune_rows]
  stopifnot(length(unique(foldt)) == K, all(table(foldt) > 0))
  cat(sprintf("ALPHA TUNE on %.0f%% facility subsample: %s rows / %s facilities (lambda still tuned on FULL in STEP 4)\n",
              100*TUNE_FRAC, format(length(tune_rows), big.mark=","), format(length(fac_keep), big.mark=","))); flush(.log)
} else {
  Xt <- X; yt <- y; offt <- off; foldt <- foldid
}
res <- lapply(ALPHAS, function(a) {
  t0 <- Sys.time()
  f <- cv.glmnet(Xt, yt, family="poisson", offset=offt, foldid=foldt, alpha=a,
                 nlambda=NLAMBDA, type.measure="deviance", parallel=TRUE)
  cat(sprintf("  [%s] alpha=%.2f  lambda.min=%.6f  cvdev=%.5f  (%.0fs)\n",
              format(Sys.time(),"%H:%M:%S"), a, f$lambda.min, min(f$cvm),
              as.numeric(difftime(Sys.time(), t0, units="secs")))); flush(.log)
  list(alpha=a, dev=min(f$cvm))
})
best_alpha <- ALPHAS[which.min(sapply(res, `[[`, "dev"))]
cat(sprintf(">> best alpha=%.2f (tuned on %s)\n", best_alpha,
            if (TUNE_FRAC < 1) sprintf("%.0f%% subsample", 100*TUNE_FRAC) else "full")); flush(.log)

# =============================================================================
cat("=== STEP 4: PREVALIDATED OOS PER-TANK RATE (keep=TRUE) ===\n")
# =============================================================================
cvb <- cv.glmnet(X, y, family="poisson", offset=off, foldid=foldid, alpha=best_alpha,
                 nlambda=NLAMBDA, type.measure="deviance", keep=TRUE, parallel=TRUE)
best_lambda <- cvb$lambda.min; idx <- which(cvb$lambda == best_lambda)
preval_link <- cvb$fit.preval[, idx]              # OOS linear predictor (incl. offset)
exp_count_oos <- exp(preval_link)                  # OOS expected leak COUNT
mu_oos <- exp_count_oos / d$active_tanks           # OOS PER-TANK rate
# 3-row offset sanity check
cat("3-row offset check (active_tanks | exp_count_oos | mu_tank):\n")
print(head(data.table(active_tanks=d$active_tanks, exp_count=round(exp_count_oos,4), mu=round(mu_oos,5)), 3))
cat(sprintf("COUNT calibration: mean(exp_count_oos)=%.5f vs mean(n_leaks)=%.5f\n",
            mean(exp_count_oos), mean(y)))
cat(sprintf("RATE  calibration: wmean(mu, tanks)=%.5f vs raw=%.5f  (want ~equal)\n",
            wmean(mu_oos, d$active_tanks), raw_rate))
cv_dev <- min(cvb$cvm); null_dev <- cvb$cvm[1]   # cvm[1] = largest lambda (most regularized ~ null); cvm[last] can diverge at tiny lambda
cat(sprintf("OOS CV deviance=%.5f | null(CV)=%.5f | pseudo-R2=%.4f | lambda.min=%.6f\n",
            cv_dev, null_dev, 1 - cv_dev/null_dev, best_lambda)); flush(.log)

# =============================================================================
cat("=== STEP 5: CELL SCHEDULES (fitted-values-then-average, exposure-weighted) ===\n")
# =============================================================================
d[, mu := mu_oos]
# FIGURE schedule (9-bin) per state + NATIONAL
fig_state <- d[, .(lambda = wmean(mu, active_tanks)), by = .(state, age_bin, has_single_walled)]
fig_natl  <- d[, .(state = "NATIONAL", lambda = wmean(mu, active_tanks)), by = .(age_bin, has_single_walled)]
# DCM schedule (8-bin, national, keyed wall x age 1..8)
dcm <- d[, .(lambda = wmean(mu, active_tanks)), by = .(wall, age_bin = age8)][order(wall, age_bin)]
cat("DCM national hazard (per 1000 tank-yr), wall x age1..8:\n")
print(dcast(dcm, age_bin ~ wall, value.var="lambda")[, .(age_bin, SW=round(SW*1000,2), DW=round(DW*1000,2))])

# Single-tank vs pooled check (equal-shots assumption)
st <- d[active_tanks==1L, .(lam_single = wmean(mu, active_tanks)), by = .(age_bin, has_single_walled)]
po <- d[,               .(lam_pooled = wmean(mu, active_tanks)), by = .(age_bin, has_single_walled)]
chk <- merge(st, po, by=c("age_bin","has_single_walled"))
cat(sprintf("Single-tank vs pooled cell rates: corr=%.3f (want high)\n", cor(chk$lam_single, chk$lam_pooled)))

# =============================================================================
cat("=== STEP 6: FACILITY-CLUSTER BOOTSTRAP (parallel) -> cell CIs ===\n")
# =============================================================================
fac_rows <- split(seq_len(nrow(d)), d$panel_id)   # row indices per facility
fac_ids  <- names(fac_rows)
fit_full <- glmnet(X, y, family="poisson", offset=off, alpha=best_alpha, lambda=best_lambda)
# keys for re-aggregation inside workers
keyf <- d[, paste(state, age_bin, has_single_walled, sep="|")]; keyn <- d[, paste(age_bin, has_single_walled, sep="|")]
keyd <- d[, paste(wall, age8, sep="|")]
act  <- d$active_tanks            # export the vector, not the whole table
invisible(clusterEvalQ(cl, { suppressPackageStartupMessages({library(glmnet); library(Matrix)}) }))
clusterExport(cl, c("X","y","off","fac_rows","fac_ids","best_alpha","best_lambda",
                    "act","keyf","keyn","keyd","wmean"), envir=environment())
boot1 <- function(b) {
  set.seed(b)
  idb <- unlist(fac_rows[sample(fac_ids, length(fac_ids), replace=TRUE)], use.names=FALSE)
  fb  <- glmnet(X[idb,], y[idb], family="poisson", offset=off[idb], alpha=best_alpha, lambda=best_lambda)
  mub <- as.numeric(predict(fb, newx=X[idb,], newoffset=rep(0, length(idb)), s=best_lambda, type="response"))
  w   <- act[idb]
  agg <- function(key) { kk <- key[idb]; tapply(seq_along(kk), kk, function(i) wmean(mub[i], w[i])) }
  list(fig=agg(keyf), natl=agg(keyn), dcm=agg(keyd))
}
cat(sprintf("Bootstrap B=%d across %d workers...\n", NBOOT, NWORKERS)); flush(.log)
bdraws <- parLapply(cl, seq_len(NBOOT), boot1)
ci <- function(cells, comp) {
  m <- sapply(bdraws, function(z) z[[comp]][cells]); # cells x B (NA if cell absent in a draw)
  data.table(lo = apply(m, 1, quantile, 0.025, na.rm=TRUE), hi = apply(m, 1, quantile, 0.975, na.rm=TRUE))
}
fig_state[, c("lo","hi") := ci(paste(state, age_bin, has_single_walled, sep="|"), "fig")]
fig_natl [, c("lo","hi") := ci(paste(age_bin, has_single_walled, sep="|"), "natl")]
dcm      [, c("lo","hi") := ci(paste(wall, age_bin, sep="|"), "dcm")]

# =============================================================================
cat("=== STEP 7: WRITE OUTPUTS ===\n")
# =============================================================================
fig <- rbind(fig_state[, .(state, age_bin, has_single_walled, lambda, lambda_lo=lo, lambda_hi=hi)],
             fig_natl [, .(state, age_bin, has_single_walled, lambda, lambda_lo=lo, lambda_hi=hi)])
fig[, age_bin := factor(age_bin, levels=AGE9_LAB)]; setorder(fig, state, age_bin, has_single_walled)
fwrite(fig, file.path(ANALYSIS_DIR, "dcm_cell_hazard_pricing.csv"))
fwrite(dcm[, .(wall, age_bin, lambda, lambda_lo=lo, lambda_hi=hi)],
       file.path(ANALYSIS_DIR, "dcm_cell_hazard_struct.csv"))
fwrite(d[, .(panel_id, panel_year, state, has_single_walled, age_bin, active_tanks, mu_tank = mu)],
       file.path(ANALYSIS_DIR, "analysis_leak_rate_predictions.csv"))
saveRDS(list(fit = fit_full, best_alpha = best_alpha, best_lambda = best_lambda,
             feature_formula = deparse(FEAT), feature_cols = colnames(X),
             state_levels = STATE_LEVELS, year_levels = YEAR_LEVELS, foldid = foldid,
             boot_draws = bdraws, raw_rate = raw_rate,
             oos = list(cv_dev = cv_dev, null_dev = null_dev, pseudo_r2 = 1 - cv_dev/null_dev),
             n_facyears = nrow(d), n_facilities = uniqueN(d$panel_id), seed = 20260627L,
             timestamp = Sys.time()),
        file.path(ANALYSIS_DIR, "analysis_leak_rate_model.rds"))
cat("Saved: dcm_cell_hazard_pricing.csv, dcm_cell_hazard_struct.csv, analysis_leak_rate_predictions.csv, analysis_leak_rate_model.rds\n")
cat("=== DONE ===\n")
