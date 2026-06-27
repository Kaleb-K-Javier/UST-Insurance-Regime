################################################################################
# 01p_Pricing_Hazard.R
# De-confounded per-tank leak hazard for fair-premium pricing + the DCM primitive
#
# WHY THIS EXISTS (separate from 01n):
#   01n estimates an OBSERVABILITY hazard: facility-year FIRST release, trained
#   PRE-1999, NO calendar control. Re-used as the DCM hazard primitive and the
#   fair-premium frequency, it has two problems for PRICING:
#     (1) DETECTION-ERA CONFOUND. Recorded releases are detection-censored:
#         rate/1k = ~0 pre-1985 -> ~15 in 1990-95 -> ~10 in 2000-10 -> ~3 by 2020
#         (right-censored tail). With no calendar control, age x wall partly
#         proxies WHEN a tank was observed. A "young single-walled" tank is a
#         1980s install seen under poor detection; a "young double-walled" tank
#         is a 2000s install seen under good detection -> spurious DW>=SW at young
#         ages.
#     (2) SIZE CONFOUND. A facility FIRST-release outcome with active_tanks as a
#         covariate, then cell-averaged, bakes portfolio size into the per-tank
#         rate the card actually prices.
#
# THE FIX (age-period-cohort aware):
#   period = cohort + age are collinear, so we CANNOT add age + vintage + calendar
#   together. We keep AGE and add PERIOD (calendar), and:
#     - train on the DETECTION ERA only (1990-2016: detection in force, pre the
#       right-censored tail), pooling FF + RB (hazard is physical, regime-agnostic,
#       per 04b),
#     - predict the (age x wall) cell schedule at a FIXED REFERENCE YEAR (mature
#       detection) and at SINGLE-TANK exposure (active_tanks = 1) -> a genuine
#       per-tank schedule with the detection trend held fixed. The age gradient
#       keeps its real "old tanks are old construction" content (that is priced);
#       only the detection ARTIFACT is removed.
#
# ESTIMAND:
#   h(t | x, year_ref) = P(first confirmed release in a year |
#                          single tank, covariates x, detection regime = year_ref)
#
# NON-DESTRUCTIVE: writes NEW files. Does NOT touch 01n or its outputs. The DCM
#   (04b) and the cross-subsidy figures repoint to these only when you choose to.
#
# OUTPUTS (Data/Analysis/):
#   dcm_cell_hazard_pricing.csv          per-tank schedule: state,age_bin,
#                                        has_single_walled,lambda (+ national rows)
#   analysis_hazard_predictions_pricing.csv  per-facility-year predictions at
#                                        OBSERVED covariates (descriptive/continuity)
#   analysis_pricing_hazard_model.rds    fitted elnet + metadata
#
# RUN: Rscript "Code/Analysis/Descrptive Facts/01p_Pricing_Hazard.R"
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(glmnet)
  library(doParallel)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))
options(scipen = 999)
set.seed(20260626L)
setDTthreads(0L)

ANALYSIS_DIR <- here("Data", "Analysis")
dir.create(ANALYSIS_DIR, recursive = TRUE, showWarnings = FALSE)

# === LOGGING ==================================================================
.log_path <- here("logs", paste0("01p_Pricing_Hazard_",
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: 01p_Pricing_Hazard.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# === STUDY PARAMETERS =========================================================
DETECTION_START <- 1990L   # detection in force; pre-1990 recorded rate ~0 (censored)
TRAIN_END       <- 2016L   # before the right-censored tail (leaks not yet surfaced)
REFERENCE_YEAR  <- 2008L   # mature, stable detection era to price at (in [start,end])
stopifnot(REFERENCE_YEAR >= DETECTION_START, REFERENCE_YEAR <= TRAIN_END)

CONTROL_STATES <- c("ME","NM","AR","OK","LA","KS","MT","ID","SD","AL",
                    "MN","NC","IL","MA","OH","PA","TN","VA","CO")
ALL_STUDY_STATES <- c("TX", CONTROL_STATES)

MM_WALL_EXCLUDE <- "Unknown-Wall"
MM_FUEL_EXCLUDE <- "Unknown-Fuel"

AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23","24+")
make_age_bin <- function(v)
  factor(cut(v, AGE_BIN_BREAKS, AGE_BIN_LABELS, right = FALSE, include.lowest = TRUE),
         levels = AGE_BIN_LABELS)

NUM_THREADS <- max(1L, parallel::detectCores() - 1L)

# Smoke test: PRICING_SMOKE=1 subsamples + trims CV for a ~2-min end-to-end check.
SMOKE <- nzchar(Sys.getenv("PRICING_SMOKE"))

# Saturated feature formula (01n's, + year_f period control). Pre-made factor
# columns (age_bin, state_f, year_f) so training and the prediction grid share
# identical dummy columns.
FEAT_FORMULA <- ~ (age_bin + has_single_walled + active_tanks + total_capacity +
                   has_gasoline_year + has_diesel_year)^2 + state_f + year_f

# Robust column aligner: returns a matrix with exactly `cols`, filling absent
# columns with 0 (grids/score-sets can lack some interaction columns).
align_cols <- function(M, cols) {
  out <- matrix(0, nrow(M), length(cols), dimnames = list(NULL, cols))
  common <- intersect(colnames(M), cols)
  out[, common] <- M[, common]
  out
}

# =============================================================================
cat("=== STEP 1: LOAD + SAMPLE (detection era) ===\n")
# =============================================================================
PANEL_CSV <- z_path("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_CSV)) stop("Panel CSV not found: ", PANEL_CSV)

keep_cols <- c("fac_wall","fac_fuel","has_single_walled","age_bins","avg_tank_age",
               "active_tanks","total_capacity","has_gasoline_year","has_diesel_year",
               "event_first_leak","has_previous_leak","state","panel_id","panel_year")
annual <- fread(PANEL_CSV, select = keep_cols)
cat(sprintf("Loaded: %s rows | %d-%d\n", format(nrow(annual), big.mark=","),
            min(annual$panel_year), max(annual$panel_year)))

annual <- annual[state %in% ALL_STUDY_STATES]

# age bin (reuse panel's age_bins if labels match, else recompute)
if ("age_bins" %in% names(annual) &&
    all(as.character(sort(unique(na.omit(annual$age_bins)))) %in% AGE_BIN_LABELS)) {
  annual[, age_bin := factor(age_bins, levels = AGE_BIN_LABELS)]
} else {
  annual[, age_bin := make_age_bin(avg_tank_age)]
}

annual[, is_make_model := as.integer(fac_wall != MM_WALL_EXCLUDE &
                                     fac_fuel != MM_FUEL_EXCLUDE & !is.na(age_bin))]

cv <- annual[panel_year >= DETECTION_START & panel_year <= TRAIN_END &
             has_previous_leak == 0L & is_make_model == 1L &
             has_single_walled %in% c(0L,1L) &
             !is.na(active_tanks) & !is.na(total_capacity) & !is.na(event_first_leak)]

if (SMOKE) {
  set.seed(20260626L)
  cv <- cv[sample(.N, min(.N, 300000L))]
  cat(sprintf("*** SMOKE MODE: subsampled CV to %s rows ***\n",
              format(nrow(cv), big.mark = ",")))
}

STATE_LEVELS <- sort(unique(cv$state))
YEAR_LEVELS  <- sort(unique(cv$panel_year))
stopifnot(REFERENCE_YEAR %in% YEAR_LEVELS)
cv[, state_f := factor(state, levels = STATE_LEVELS)]
cv[, year_f  := factor(panel_year, levels = YEAR_LEVELS)]
cv[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]

n_event <- sum(cv$event_first_leak); event_rate <- mean(cv$event_first_leak)
cat(sprintf("Pricing sample: %s fac-years | %s facilities | %d states | yr %d-%d\n",
            format(nrow(cv), big.mark=","), format(uniqueN(cv$panel_id), big.mark=","),
            uniqueN(cv$state), min(YEAR_LEVELS), max(YEAR_LEVELS)))
cat(sprintf("First-leak events: %s | event rate: %.5f\n",
            format(n_event, big.mark=","), event_rate))

# =============================================================================
cat("=== STEP 2: DESIGN MATRIX + CLASS WEIGHTS ===\n")
# =============================================================================
Y <- cv$event_first_leak
# No class-imbalance reweighting: a logistic elastic net is well-calibrated on the
# raw rare outcome (the intercept absorbs the base rate). 01n's inverse-frequency
# weights + Platt rescaling were a GRF device; here they put glmnet on a balanced
# scale that the Platt slope could not map back (slope -> 0 -> flat predictions).
X <- model.matrix(FEAT_FORMULA, data = as.data.frame(cv))[, -1L]
cat(sprintf("Design matrix: %s rows x %d cols\n", format(nrow(X), big.mark=","), ncol(X)))

# =============================================================================
cat("=== STEP 3: ELASTIC NET (alpha tune -> full CV) ===\n")
# =============================================================================
ELNET_FOLDS <- if (SMOKE) 3L else 5L     # 5-fold is plenty for lambda; full 2.3M x 10-fold was the slow pole
ELNET_ALPHA <- if (SMOKE) c(0.0, 0.5, 1.0) else c(0.0, 0.25, 0.5, 0.75, 1.0)
TUNE_N <- min(nrow(X), 200000L)          # alpha-tuning subsample
CV_N   <- min(nrow(X), 600000L)          # lambda-CV subsample (final fit is on FULL X)

cl <- makeCluster(NUM_THREADS); registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE)

# alpha tuning on a subsample (parallel across folds)
set.seed(20260626L); tune_idx <- sample(nrow(X), TUNE_N)
cat(sprintf("Tuning alpha over %d values, %d-fold CV on %s rows, %d threads...\n",
            length(ELNET_ALPHA), ELNET_FOLDS, format(TUNE_N, big.mark=","), NUM_THREADS)); flush(.log)
alpha_res <- lapply(ELNET_ALPHA, function(a) {
  set.seed(20260626L)
  f <- cv.glmnet(X[tune_idx,], Y[tune_idx], family="binomial", alpha=a,
                 nfolds=ELNET_FOLDS, type.measure="deviance", parallel=TRUE)
  cat(sprintf("  alpha=%.2f  lambda.min=%.6f  dev=%.5f\n", a, f$lambda.min, min(f$cvm))); flush(.log)
  list(alpha=a, dev=min(f$cvm))
})
best_alpha <- ELNET_ALPHA[which.min(sapply(alpha_res, `[[`, "dev"))]
cat(sprintf(">> best alpha: %.2f\n", best_alpha)); flush(.log)

# lambda CV at best alpha on a CAPPED subsample (parallel). Picking lambda does not
# need all rows; the final fit below uses the FULL data at the chosen lambda.
cat(sprintf("CV for lambda at best alpha on %s rows (parallel)...\n",
            format(CV_N, big.mark=","))); flush(.log)
set.seed(20260626L); cv_idx <- sample(nrow(X), CV_N)
cvfit <- cv.glmnet(X[cv_idx,], Y[cv_idx], family="binomial", alpha=best_alpha,
                   nfolds=ELNET_FOLDS, type.measure="deviance", parallel=TRUE)
best_lambda <- cvfit$lambda.min

# refit on FULL data at (best alpha, lambda). Unweighted binomial -> calibrated probs.
set.seed(20260626L)
fit <- glmnet(X, Y, family="binomial", alpha=best_alpha, lambda=best_lambda)
cat(sprintf("lambda.min=%.6f | in-sample median pred: %.5f (rate %.5f)\n",
            best_lambda,
            median(as.numeric(predict(fit, newx=X, type="response"))), event_rate)); flush(.log)

# helper: covariate frame -> per-tank hazard (calibrated probability)
predict_haz <- function(df) {
  M <- align_cols(model.matrix(FEAT_FORMULA, data = df)[, -1L, drop=FALSE], colnames(X))
  as.numeric(predict(fit, newx = M, type = "response"))
}

# =============================================================================
cat("=== STEP 4: PER-TANK CELL SCHEDULE (reference year, single tank) ===\n")
# =============================================================================
ref_cap <- cv[active_tanks == 1L, median(total_capacity, na.rm = TRUE)]
cat(sprintf("Reference: year=%d, active_tanks=1, total_capacity=%.0f, gasoline=1\n",
            REFERENCE_YEAR, ref_cap))

grid <- CJ(state = STATE_LEVELS, age_bin = AGE_BIN_LABELS, has_single_walled = c(0L,1L))
grid[, `:=`(active_tanks = 1L, total_capacity = ref_cap,
            has_gasoline_year = 1L, has_diesel_year = 0L)]
grid[, state_f  := factor(state, levels = STATE_LEVELS)]
grid[, year_f   := factor(REFERENCE_YEAR, levels = YEAR_LEVELS)]
grid[, age_bin  := factor(age_bin, levels = AGE_BIN_LABELS)]
grid[, lambda   := predict_haz(as.data.frame(grid))]

# national schedule = mean over states (one rate card)
natl <- grid[, .(state = "NATIONAL", lambda = mean(lambda)),
             by = .(age_bin, has_single_walled)]
cell_out <- rbind(grid[, .(state, age_bin, has_single_walled, lambda)], natl)
cell_out[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(cell_out, state, age_bin, has_single_walled)
fwrite(cell_out, file.path(ANALYSIS_DIR, "dcm_cell_hazard_pricing.csv"))
cat("Saved: dcm_cell_hazard_pricing.csv\n")

cat(sprintf("\nDIAGNOSTIC — national per-tank schedule at year %d (per 1000):\n", REFERENCE_YEAR))
diag <- dcast(natl, age_bin ~ has_single_walled, value.var = "lambda")[
  match(AGE_BIN_LABELS, age_bin)]
setnames(diag, c("0","1"), c("DW","SW"))
diag[, `:=`(DW = round(DW*1000,2), SW = round(SW*1000,2), SW_gt_DW = (SW > DW))]
print(diag[, .(age_bin, DW, SW, SW_gt_DW)])
cat(sprintf("  age bins with SW>DW: %d / %d (was ~6/9 in 01n)\n",
            sum(diag$SW_gt_DW), nrow(diag)))

# detection trend the model absorbed: one fixed cell across years
cat("\nDIAGNOSTIC — detection/period effect (SW 9-11, single tank, by year, per 1000):\n")
yr_grid <- data.table(age_bin = factor("9-11", levels=AGE_BIN_LABELS),
                      has_single_walled = 1L, active_tanks = 1L, total_capacity = ref_cap,
                      has_gasoline_year = 1L, has_diesel_year = 0L,
                      state_f = factor(STATE_LEVELS[1], levels=STATE_LEVELS),
                      year_f  = factor(YEAR_LEVELS, levels=YEAR_LEVELS))
yr_grid[, lambda := predict_haz(as.data.frame(yr_grid))]
print(yr_grid[as.integer(as.character(year_f)) %% 5 == 0,
              .(year = year_f, per1k = round(lambda*1000,2))])
flush(.log)   # stream the schedule + both diagnostics before the slow STEP 5 scoring

# =============================================================================
cat("=== STEP 5: PER-FACILITY-YEAR PREDICTIONS (observed covariates) ===\n")
# =============================================================================
# Descriptive / continuity file: hazard at each facility-year's OWN covariates,
# over the full panel. Scored at the FIT year if in range, else clamped to range
# (out-of-window years use the nearest in-sample period level).
score <- annual[has_previous_leak == 0L & is_make_model == 1L &
                has_single_walled %in% c(0L,1L) &
                !is.na(active_tanks) & !is.na(total_capacity) &
                state %in% STATE_LEVELS]
score[, state_f := factor(state, levels = STATE_LEVELS)]
score[, py_clamp := pmin(pmax(panel_year, min(YEAR_LEVELS)), max(YEAR_LEVELS))]
score[, year_f  := factor(py_clamp, levels = YEAR_LEVELS)]
score[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
score[, pred_pricing := predict_haz(as.data.frame(score))]
fwrite(score[, .(panel_id, panel_year, state, has_single_walled, age_bin, pred_pricing)],
       file.path(ANALYSIS_DIR, "analysis_hazard_predictions_pricing.csv"))
cat(sprintf("Saved: analysis_hazard_predictions_pricing.csv (%s rows)\n",
            format(nrow(score), big.mark=",")))

saveRDS(list(fit=fit, feature_cols=colnames(X),
             best_alpha=best_alpha, best_lambda=best_lambda,
             state_levels=STATE_LEVELS, year_levels=YEAR_LEVELS,
             age_bin_levels=AGE_BIN_LABELS, ref_year=REFERENCE_YEAR, ref_cap=ref_cap,
             detection_start=DETECTION_START, train_end=TRAIN_END,
             formula=deparse(FEAT_FORMULA), event_rate=event_rate, timestamp=Sys.time()),
        file.path(ANALYSIS_DIR, "analysis_pricing_hazard_model.rds"))

# =============================================================================
cat("=== STEP 6: SUMMARY ===\n")
# =============================================================================
cat(sprintf("Pricing hazard | sample %d-%d | ref year %d | alpha %.2f | lambda %.6f\n",
            DETECTION_START, TRAIN_END, REFERENCE_YEAR, best_alpha, best_lambda))
cat(sprintf("Cell schedule: dcm_cell_hazard_pricing.csv | SW>DW in %d/%d age bins\n",
            sum(diag$SW_gt_DW), nrow(diag)))
cat("Done.\n")
