# ==============================================================================
# Code/Analysis/T013fix_Theta_PreBOY_vs_BOY.R
# TICKET 013 Step 4 — Before/after parameter movement from BOY state stamping.
#
# Quantifies how much the EOY->BOY (decision-time) state-stamping fix moved the
# 6p+FE structural estimates. Apples-to-apples: BOTH fits are the cleaned
# 6p+FE-profile fit; only the panel stamping differs.
#
# Inputs:
#   pre_BOY: Output/Estimation_Results/_pre_BOY/Model_Replacement_6paramFE_profile_clean_observed.rds
#   BOY:     Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
#   N_obs:   Data/Analysis/_pre_BOY/dcm_obs_panel_observed.csv (pre) and
#            Data/Analysis/dcm_obs_panel_observed.csv          (BOY)
#
# Output:
#   Output/Tables/T013fix_Theta_PreBOY_vs_BOY.csv
#     8 rows: kappa_SW, kappa_DW, K_SW, K_DW, gamma_price, gamma_risk,
#             log_likelihood, N_obs
#     Cols (exact, types): parameter chr | pre_BOY num | BOY num | delta num |
#                          pct_change num (NA for log_likelihood / N_obs)
# ==============================================================================

# ---- SECTION 1: LOGGING ------------------------------------------------------
.log_path <- here::here("logs", paste0(
  "T013fix_Theta_PreBOY_vs_BOY_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: T013fix_Theta_PreBOY_vs_BOY.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

suppressPackageStartupMessages({ library(data.table); library(here) })

SCALE_FACTOR <- 10000L
OUT_TAB <- here::here("Output", "Tables")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

# ---- SECTION 2: LOAD BOTH FITS ----------------------------------------------
cat("=== SECTION 2: LOAD pre-BOY and BOY clean fits ===\n")

path_pre <- here::here("Output", "Estimation_Results", "_pre_BOY",
                       "Model_Replacement_6paramFE_profile_clean_observed.rds")
path_boy <- here::here("Output", "Estimation_Results",
                       "Model_Replacement_6paramFE_profile_clean_observed.rds")
stopifnot(file.exists(path_pre), file.exists(path_boy))

fit_pre <- readRDS(path_pre)
fit_boy <- readRDS(path_boy)

theta_pre <- fit_pre$theta_hat
theta_boy <- fit_boy$theta_hat

struct_names <- c("kappa_SW", "kappa_DW", "K_log_SW", "K_log_DW",
                  "gamma_price", "gamma_risk")
stopifnot(all(struct_names %in% names(theta_pre)),
          all(struct_names %in% names(theta_boy)))
cat(sprintf("  Both fits carry all 6 struct params. pre-BOY LL=%.3f | BOY LL=%.3f\n",
            fit_pre$log_likelihood, fit_boy$log_likelihood))
cat(sprintf("  pre-BOY converged: %s | BOY converged: %s\n",
            fit_pre$converged, fit_boy$converged))

# ---- SECTION 3: N_obs from the two obs panels --------------------------------
cat("=== SECTION 3: N_obs ===\n")
path_obs_pre <- here::here("Data", "Analysis", "_pre_BOY", "dcm_obs_panel_observed.csv")
path_obs_boy <- here::here("Data", "Analysis", "dcm_obs_panel_observed.csv")
n_pre <- if (file.exists(path_obs_pre)) nrow(fread(path_obs_pre, select = "panel_id")) else
         if (!is.null(fit_pre$n_obs)) fit_pre$n_obs else NA_integer_
n_boy <- if (file.exists(path_obs_boy)) nrow(fread(path_obs_boy, select = "panel_id")) else
         if (!is.null(fit_boy$n_obs)) fit_boy$n_obs else NA_integer_
cat(sprintf("  N_obs pre-BOY = %s | BOY = %s\n",
            format(n_pre, big.mark = ","), format(n_boy, big.mark = ",")))

# ---- SECTION 4: BUILD TABLE --------------------------------------------------
cat("=== SECTION 4: BUILD COMPARISON TABLE ===\n")

# Same transforms as 04o make_cmp_row: kappa x10000 -> USD, K = exp(K_log),
# gammas raw.
disp_val <- function(pname, v) {
  if (grepl("^K_log_", pname))      exp(v)
  else if (grepl("^kappa_", pname)) v * SCALE_FACTOR
  else                              v
}
disp_name <- function(pname) if (grepl("^K_log_", pname)) sub("K_log_", "K_", pname) else pname

param_row <- function(pname) {
  v_pre <- disp_val(pname, theta_pre[[pname]])
  v_boy <- disp_val(pname, theta_boy[[pname]])
  dlt   <- v_boy - v_pre
  pct   <- if (abs(v_pre) > 1e-10) 100 * dlt / abs(v_pre) else NA_real_
  data.table(parameter  = disp_name(pname),
             pre_BOY    = v_pre,
             BOY        = v_boy,
             delta      = dlt,
             pct_change = pct)
}

rows <- rbindlist(lapply(struct_names, param_row))

rows <- rbindlist(list(
  rows,
  data.table(parameter = "log_likelihood",
             pre_BOY = fit_pre$log_likelihood,
             BOY     = fit_boy$log_likelihood,
             delta   = fit_boy$log_likelihood - fit_pre$log_likelihood,
             pct_change = NA_real_),
  data.table(parameter = "N_obs",
             pre_BOY = as.numeric(n_pre),
             BOY     = as.numeric(n_boy),
             delta   = as.numeric(n_boy - n_pre),
             pct_change = NA_real_)
))

# Round numeric display columns (keep full precision out of the CSV noise)
rows[, `:=`(pre_BOY    = round(pre_BOY, 4),
            BOY        = round(BOY, 4),
            delta      = round(delta, 4),
            pct_change = round(pct_change, 2))]

cat("  T013fix pre-BOY vs BOY comparison:\n")
print(rows)

path_csv <- file.path(OUT_TAB, "T013fix_Theta_PreBOY_vs_BOY.csv")
fwrite(rows, path_csv)
cat(sprintf("  Saved: %s\n", path_csv))

# ---- SECTION 5: VERDICT ------------------------------------------------------
cat("=== SECTION 5: VERDICT ===\n")
pct_kdw  <- rows[parameter == "K_DW",     pct_change]
pct_kSW  <- rows[parameter == "kappa_SW", pct_change]
material <- any(abs(rows[parameter %in% c("kappa_SW","kappa_DW","K_SW","K_DW",
                                          "gamma_price","gamma_risk"), pct_change]) >= 10,
                na.rm = TRUE)
cat(sprintf(
  "  BOY stamping moved K_DW by %.1f%%, kappa_SW by %.1f%%; welfare CF re-run %s warranted.\n",
  pct_kdw, pct_kSW, if (material) "IS" else "is NOT"))

cat("\n=== T013fix COMPLETE ===\n")
