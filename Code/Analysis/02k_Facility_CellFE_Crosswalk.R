################################################################################
# Code/Analysis/02k_Facility_CellFE_Crosswalk.R
#
# Build the EXACT tank cell x year baseline and collapse it to facility-year, so
# the facility DiD/ES (02j) controls for the SAME cell FE as the tank-level
# analysis -- not the coarse pct_sw/avg_age proxy. Output is a small crosswalk to
# LEFT-JOIN onto matched_facs by (panel_id, panel_year).
#
# Cell = make_model_noage x install_yr_int  (== the cell dimension inside the
#        tank-level cell_vintage_year_fe = make_model_noage^install_yr^panel_year).
# delta_hat_ct = mean closure among UNTREATED tank-years (controls all years + TX
#        pre-1999) in cell c, year t. No treatment variation -> clean baseline.
#        Sparse fine cells fall back to (make_model_noage x year) then (year).
# Yhat0_ft = mean of delta_hat over the facility's active tanks
#          = sum_c s_fct * delta_hat_ct  (composition-weighted cell x year baseline).
# tau_ft   = closure_share_tank - Yhat0  (the predicted-rate / imputation residual).
#
# closure_share_tank is computed here from the SAME tank set that defines Yhat0,
# so route-B tau is internally consistent (use this, not matched_facs's share,
# for the imputation outcome).
#
# Output: Data/Analysis/facility_cellfe_xwalk.csv
#   panel_id, panel_year, n_active, closure_share_tank, Yhat0, tau
#
# Run on the headline tank panel:
#   UST_ANALYSIS_DIR=Z:/ust_ins_move_to_github/Data/Analysis  Rscript ... 02k...R
################################################################################

suppressPackageStartupMessages({ library(data.table); library(here) })

# === LOGGING ===
dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
.log_path <- here::here("logs", paste0("02k_Facility_CellFE_Crosswalk_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nR: %s\n\n", .log_path, R.version.string))

# === SETUP ===
cat("=== SETUP ===\n")
source(here::here("Code", "Helpers", "reduced_form_utils.R"))
rf_use_threads()
ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR", here::here("Data", "Analysis"))
PANEL_FILE   <- Sys.getenv("UST_PANEL_FILE",   "matched_tanks_birth_cem.csv")
OUT_PATH     <- here::here("Data", "Analysis", "facility_cellfe_xwalk.csv")
MIN_CELL_N   <- 20L   # min untreated obs for the fine cell mean, else fall back
stopifnot(file.exists(file.path(ANALYSIS_DIR, PANEL_FILE)))

# === STEP 1 — TANK PANEL + ACTIVE-AT-TREATMENT (same builder as the headline) ===
cat("=== STEP 1: LOAD + ACTIVE SAMPLE ===\n")
.hdr  <- names(fread(file.path(ANALYSIS_DIR, PANEL_FILE), nrows = 0L))
.cols <- c("tank_panel_id","panel_id","state","texas_treated","install_yr_int",
           "make_model_noage","cem_weight","panel_year","closure_event")
if ("first_year_churn" %in% .hdr) .cols <- c(.cols, "first_year_churn")
dt <- fread(file.path(ANALYSIS_DIR, PANEL_FILE), na.strings = c("","NA"), select = .cols)
if ("first_year_churn" %in% names(dt)) dt <- dt[first_year_churn == 0L | is.na(first_year_churn)]
tk <- build_active_at_treatment_sample(dt)
cat(sprintf("  tank-years: %s | facilities: %s\n",
            fmt_n(nrow(tk)), fmt_n(uniqueN(tk$panel_id))))

# === STEP 2 — UNTREATED CELL x YEAR BASELINE delta_hat (exact tank cell) ===
cat("=== STEP 2: delta_hat (untreated cell x year) ===\n")
tk[, untreated := as.integer(texas_treated == 0L | (texas_treated == 1L & panel_year < 1999L))]
unt <- tk[untreated == 1L]
b1 <- unt[, .(b1 = mean(closure_event), n1 = .N), by = .(make_model_noage, install_yr_int, panel_year)]
b2 <- unt[, .(b2 = mean(closure_event)),          by = .(make_model_noage, panel_year)]
b3 <- unt[, .(b3 = mean(closure_event)),          by = .(panel_year)]
tk <- merge(tk, b1, by = c("make_model_noage","install_yr_int","panel_year"), all.x = TRUE)
tk <- merge(tk, b2, by = c("make_model_noage","panel_year"),                  all.x = TRUE)
tk <- merge(tk, b3, by = "panel_year",                                        all.x = TRUE)
tk[, tank_base := fcoalesce(fifelse(!is.na(n1) & n1 >= MIN_CELL_N, b1, NA_real_), b2, b3)]
stopifnot(!anyNA(tk$tank_base))
cat(sprintf("  baseline source: fine(>=%d)=%.1f%% | any fallback=%.1f%%\n", MIN_CELL_N,
            100*mean(!is.na(tk$n1) & tk$n1 >= MIN_CELL_N),
            100*mean(is.na(tk$n1) | tk$n1 < MIN_CELL_N)))

# === STEP 3 — COLLAPSE TO FACILITY-YEAR ===
cat("=== STEP 3: COLLAPSE TO FACILITY-YEAR ===\n")
xw <- tk[, .(
  n_active           = .N,
  closure_share_tank = mean(closure_event),
  Yhat0              = mean(tank_base)
), by = .(panel_id, panel_year)]
xw[, tau := closure_share_tank - Yhat0]

stopifnot(uniqueN(xw, by = c("panel_id","panel_year")) == nrow(xw))
stopifnot(all(xw$closure_share_tank >= 0 & xw$closure_share_tank <= 1))
stopifnot(all(xw$Yhat0 >= 0 & xw$Yhat0 <= 1))
fwrite(xw, OUT_PATH)
cat(sprintf("  saved: %s (%s rows)\n", OUT_PATH, fmt_n(nrow(xw))))
cat(sprintf("  closure_share_tank mean: %.4f | Yhat0 mean: %.4f | corr(Y,Yhat0): %.3f\n",
            mean(xw$closure_share_tank), mean(xw$Yhat0),
            cor(xw$closure_share_tank, xw$Yhat0)))
cat("\n=== 02k COMPLETE ===\n")
