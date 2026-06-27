################################################################################
# Code/Analysis/02i_Facility_Year_Panel.R
#
# Build the FACILITY-YEAR panel for the causal-portfolio analysis (02j), by
# aggregating the active-at-treatment matched TANK panel up to (panel_id, year).
# This is the unit at which the cell FE must be carried as PORTFOLIO COMPOSITION.
#
# Identification (carried to 02j): parallel trends CONDITIONAL ON TANK MIX. The
# tank-level cell x year FE becomes, at the facility, the composition-weighted
# baseline sum_c s_fct * delta_ct. Two ways 02j uses what we build here:
#   (A) direct TWFE  : composition shares s_fct (sh_* cols) interacted with year.
#   (B) imputation   : Yhat0 = mean over the facility's tanks of the UNTREATED
#                      cell x year closure rate; tau = closure_share - Yhat0.
#
# Outcomes (facility-year portfolio actions):
#   closure_share = fraction of active tanks that closed that year  [primary]
#   any_closure   = 1 if >=1 active tank closed
#   exit          = 1 if ALL active tanks closed that year (full-closure proxy)
#
# Output: Data/Analysis/facility_year_active.csv  (one row per panel_id x year)
#   keys/treat : panel_id, state, panel_year, rel_year, texas_treated, post, did_term
#   counts     : N_active, n_clo
#   outcomes   : closure_share, any_closure, exit
#   route B    : Yhat0, tau
#   controls   : mandate_release_det, mandate_spill_overfill, mandate_integrity
#   shares     : sh_<wall>_<agebin>  (sum to 1 within facility-year)
#   covariates : gas_station, fuel_gasonly, geo_rural, dem_dense, dem_lowinc, pop_q
#
# R->logic note: this is data engineering (data.table), but stays in R per the
# estimator-language scope. Run on the headline birth-CEM panel:
#   UST_ANALYSIS_DIR=Z:/ust_ins_move_to_github/Data/Analysis  Rscript ... 02i...R
################################################################################

suppressPackageStartupMessages({ library(data.table); library(here) })

# === LOGGING ===
dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
.log_path <- here::here("logs", paste0("02i_Facility_Year_Panel_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nR: %s\n\n", .log_path, R.version.string))

# === SETUP ===
cat("=== SETUP ===\n")
source(here::here("Code", "Helpers", "reduced_form_utils.R"))
ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR", here::here("Data", "Analysis"))
PANEL_FILE   <- Sys.getenv("UST_PANEL_FILE",   "matched_tanks_birth_cem.csv")
GIS_DIR      <- here::here("Data", "Processed", "GIS")
XWALK_PATH   <- Sys.getenv("UST_XWALK_PATH",
                           here::here("Data", "Analysis", "_facility_xwalk_coords.csv"))
OUT_PATH     <- file.path(ANALYSIS_DIR, "facility_year_active.csv")
# Always write the panel into the LOCAL repo (Z is read-only / may be the source).
OUT_PATH_LOCAL <- here::here("Data", "Analysis", "facility_year_active.csv")
MANDATES <- c("mandate_release_det", "mandate_spill_overfill", "mandate_integrity")
MIN_CELL_N <- 20L   # min untreated obs for the fine route-B baseline cell

stopifnot(file.exists(file.path(ANALYSIS_DIR, PANEL_FILE)), file.exists(XWALK_PATH))

# === STEP 1 — LOAD TANK PANEL + ACTIVE-AT-TREATMENT SAMPLE ===
cat("=== STEP 1: LOAD + ACTIVE SAMPLE ===\n")
.hdr  <- names(fread(file.path(ANALYSIS_DIR, PANEL_FILE), nrows = 0L))
.cols <- c("tank_panel_id", "panel_id", "state", "texas_treated",
           "install_yr_int", "make_model_noage", "cem_weight",
           "panel_year", "closure_event")
has_fuel  <- "mm_fuel" %in% .hdr
if (has_fuel) .cols <- c(.cols, "mm_fuel")
mand_have <- intersect(MANDATES, .hdr)
.cols <- c(.cols, mand_have)
if ("first_year_churn" %in% .hdr) .cols <- c(.cols, "first_year_churn")

dt <- fread(file.path(ANALYSIS_DIR, PANEL_FILE), na.strings = c("", "NA"), select = .cols)
if ("first_year_churn" %in% names(dt)) dt <- dt[first_year_churn == 0L | is.na(first_year_churn)]
tk <- build_active_at_treatment_sample(dt)        # tank-year; adds rel_year etc.
cat(sprintf("  tank-years: %s | facilities: %s | states: %d\n",
            fmt_n(nrow(tk)), fmt_n(uniqueN(tk$panel_id)), uniqueN(tk$state)))

# === STEP 2 — TANK-LEVEL CELLS (wall x agebin) ===
cat("=== STEP 2: CELLS ===\n")
tk[, wall := tstrsplit(make_model_noage, "|", fixed = TRUE)[[1]]]
tk[, wall := fifelse(wall %in% c("SW", "DW"), wall, "UNK")]
tk[, age := pmax(panel_year - install_yr_int, 0L)]
tk[, agebin := cut(age, c(-Inf, 10, 20, Inf), labels = c("0to10", "11to20", "21plus"))]
tk[, cell_share := paste0(wall, "_", agebin)]                 # coarse: route-A shares
tk[, cell_b     := paste0(make_model_noage, "|", agebin)]     # fine:  route-B baseline

# === STEP 3 — ROUTE-B BASELINE delta_hat (UNTREATED cell x year means) ===
cat("=== STEP 3: ROUTE-B BASELINE (untreated cell x year) ===\n")
# Untreated = control states (all years) + Texas pre-1999. No treatment variation.
tk[, untreated := as.integer(texas_treated == 0L | (texas_treated == 1L & panel_year < 1999L))]
unt <- tk[untreated == 1L]
b1 <- unt[, .(b1 = mean(closure_event), n1 = .N), by = .(cell_b, panel_year)]
b2 <- unt[, .(b2 = mean(closure_event)),          by = .(make_model_noage, panel_year)]
b3 <- unt[, .(b3 = mean(closure_event)),          by = .(panel_year)]
tk <- merge(tk, b1, by = c("cell_b", "panel_year"),          all.x = TRUE)
tk <- merge(tk, b2, by = c("make_model_noage", "panel_year"), all.x = TRUE)
tk <- merge(tk, b3, by = "panel_year",                        all.x = TRUE)
# Use the fine cell mean when it has >= MIN_CELL_N untreated obs, else fall back.
tk[, tank_base := fcoalesce(fifelse(!is.na(n1) & n1 >= MIN_CELL_N, b1, NA_real_), b2, b3)]
cat(sprintf("  baseline source: fine=%.1f%% mm=%.1f%% year=%.1f%%\n",
            100 * mean(!is.na(tk$b1) & tk$n1 >= MIN_CELL_N),
            100 * mean(is.na(tk$b1) | tk$n1 < MIN_CELL_N) * mean(!is.na(tk$b2)),
            100 * mean(is.na(tk$tank_base))))
stopifnot(!anyNA(tk$tank_base))

# === STEP 4 — AGGREGATE TO FACILITY-YEAR ===
cat("=== STEP 4: AGGREGATE TO FACILITY-YEAR ===\n")
mand_expr <- if (length(mand_have))
  setNames(lapply(mand_have, function(m) bquote(max(.(as.name(m))))), mand_have) else list()

fy <- tk[, c(list(
  state         = state[1],
  texas_treated = texas_treated[1],
  rel_year      = rel_year[1],
  N_active      = .N,
  n_clo         = sum(closure_event),
  closure_share = mean(closure_event),
  Yhat0         = mean(tank_base)
), lapply(mand_expr, eval, envir = .SD)),
by = .(panel_id, panel_year)]
fy[, `:=`(
  any_closure = as.integer(n_clo > 0L),
  exit        = as.integer(n_clo == N_active),
  tau         = closure_share - Yhat0,
  post        = as.integer(panel_year >= 1999L)
)]
fy[, did_term := texas_treated * post]
cat(sprintf("  facility-years: %s | facilities: %s | mean N_active: %.2f\n",
            fmt_n(nrow(fy)), fmt_n(uniqueN(fy$panel_id)), mean(fy$N_active)))

# === STEP 5 — COMPOSITION SHARES s_fct (wall x agebin) ===
cat("=== STEP 5: COMPOSITION SHARES ===\n")
sh <- tk[, .N, by = .(panel_id, panel_year, cell_share)]
sh[, share := N / sum(N), by = .(panel_id, panel_year)]
shw <- dcast(sh, panel_id + panel_year ~ cell_share, value.var = "share", fill = 0)
sh_cols <- setdiff(names(shw), c("panel_id", "panel_year"))
setnames(shw, sh_cols, paste0("sh_", sh_cols))
fy <- merge(fy, shw, by = c("panel_id", "panel_year"), all.x = TRUE)
cat(sprintf("  share columns: %s\n", paste(paste0("sh_", sh_cols), collapse = ", ")))

# === STEP 6 — FACILITY COVARIATES (GIS + FUEL) ===
cat("=== STEP 6: COVARIATES ===\n")
xw <- fread(XWALK_PATH, select = c("panel_id", "facility_id", "state"),
            colClasses = list(character = c("panel_id", "facility_id", "state")))
g_ur <- fread(file.path(GIS_DIR, "gis_03_urban_rural.csv"),
              select = c("facility_id", "state", "ruca_primary"),
              colClasses = list(character = c("facility_id", "state")))
g_dm <- fread(file.path(GIS_DIR, "gis_09_demographics.csv"),
              select = c("facility_id", "state", "tract_pop", "med_hh_income"),
              colClasses = list(character = c("facility_id", "state")))
covf <- Reduce(function(a, b) merge(a, b, by = c("facility_id", "state"), all.x = TRUE),
               list(unique(xw), g_ur, g_dm))
covf[, geo_rural := as.integer(ruca_primary >= 7)]
covf[, pop_q := cut(tract_pop, quantile(tract_pop, c(0,.25,.5,.75,1), na.rm = TRUE),
                    labels = 1:4, include.lowest = TRUE)]
covf[, dem_dense  := as.integer(pop_q == "4")]
covf[, dem_lowinc := as.integer(med_hh_income < median(med_hh_income, na.rm = TRUE))]
cov_keep <- covf[, .(panel_id, geo_rural, dem_dense, dem_lowinc, pop_q)]

if (has_fuel) {
  fuelf <- tk[, .(
    gas_station  = as.integer(any(mm_fuel == "Gasoline-Only", na.rm = TRUE)),
    fuel_gasonly = as.integer(all(mm_fuel == "Gasoline-Only", na.rm = TRUE) &
                              any(mm_fuel == "Gasoline-Only", na.rm = TRUE))
  ), by = panel_id]
  cov_keep <- merge(cov_keep, fuelf, by = "panel_id", all = TRUE)
}
fy <- merge(fy, cov_keep, by = "panel_id", all.x = TRUE)

# === STEP 7 — VALIDATE + SAVE ===
cat("=== STEP 7: VALIDATE + SAVE ===\n")
stopifnot(uniqueN(fy, by = c("panel_id", "panel_year")) == nrow(fy))
stopifnot(all(fy$closure_share >= 0 & fy$closure_share <= 1))
stopifnot(all(fy$N_active >= 1L))
sh_all <- fy[, rowSums(.SD), .SDcols = patterns("^sh_")]
stopifnot(all(abs(sh_all - 1) < 1e-8))
fwrite(fy, OUT_PATH_LOCAL)
cat(sprintf("  saved: %s (%s rows, %d cols)\n", OUT_PATH_LOCAL, fmt_n(nrow(fy)), ncol(fy)))
cat(sprintf("  closure_share mean: %.4f | Yhat0 mean: %.4f | exit rate: %.4f\n",
            mean(fy$closure_share), mean(fy$Yhat0), mean(fy$exit)))
cat("\n=== 02i COMPLETE ===\n")
