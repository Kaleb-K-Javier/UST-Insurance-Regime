################################################################################
# Code/Analysis/02h_HTE_EventStudy_FirstPass.R
#
# Event-study figures for the HTE cuts, styled to MATCH the headline slide figure
# (Fig_ES_Full, from 02b_tank_closure_analysis.R::plot_es_clean) and running the
# SAME regression as the headline ES so the cuts are directly comparable.
#
# HEADLINE ES SPEC (02b_tank_closure_analysis.R:3143, reproduced exactly here):
#   closure_event ~ i(rel_year, texas_treated, ref = -1) +
#     mandate_release_det + mandate_spill_overfill + mandate_integrity |
#     panel_id + cell_vintage_year_fe          , cluster ~ state
#   - unit = tank-year; ref event-time = -1 (1997); primary.
#   - the three federal RCRA mandate dummies are additive controls (NOT
#     interacted) — they were the piece 02g/02h originally omitted.
#
# WHAT THE REGRESSION IS (math):
#   closure_{i,t} = sum_{k != -1} beta_k * 1{rel_year=k} * TX_i
#                   + nu' Mandate_{i,t} + alpha_{f(i)} + delta_{c(i),t} + eps
#   alpha_f = facility FE (panel_id); delta_{c,t} = cell x year FE
#   (cell_vintage_year_fe = make_model_noage x install-cohort, by year).
#   TX_i is absorbed by alpha_f; common event-time effects by delta_{c,t}.
#   beta_k = differential annual closure prob of TX vs control tanks at event
#   time k, relative to k=-1, within make-model-cohort x year. Pre-period
#   beta_k ~ 0 is the parallel-trends check; post beta_k is the dynamic effect.
#
# IDENTIFYING ASSUMPTION: parallel trends within make-model cell (tank-year) —
#   see 02g header for how this carries to the facility level.
#
# Figure (Output/Figures/, slide style):
#   Fig_ES_HTE_Pooled.{pdf,png}   pooled ES; reproduces the slide Fig_ES_Full
#   (group dynamics via INTERACTION ES are a follow-up — never split-sample the ES)
# Table: Output/Tables/T_HTE_EventStudy_Coefs.csv
#
# Run (dev laptop, panel on Z):
#   UST_ANALYSIS_DIR=Z:/ust_ins_move_to_github/Data/Analysis  Rscript ... 02h...R
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(here)
})

# === LOGGING ===
dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
.script_name <- "02h_HTE_EventStudy_FirstPass"
.log_path <- here::here("logs", paste0(.script_name, "_",
                                       format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\n\n", .log_path, .script_name, R.version.string))

# === SETUP ===
cat("=== SETUP ===\n")
source(here::here("Code", "Helpers", "reduced_form_utils.R"))
rf_use_threads()

ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR", here::here("Data", "Analysis"))
PANEL_FILE   <- Sys.getenv("UST_PANEL_FILE",   "matched_tanks_birth_cem.csv")
GIS_DIR      <- here::here("Data", "Processed", "GIS")
XWALK_PATH   <- Sys.getenv("UST_XWALK_PATH",
                           here::here("Data", "Analysis", "_facility_xwalk_coords.csv"))
ES_WIN  <- c(-13L, 22L)   # event-time span (matches the slide's -14..22 axis)
MANDATES <- c("mandate_release_det", "mandate_spill_overfill", "mandate_integrity")

stopifnot(file.exists(file.path(ANALYSIS_DIR, PANEL_FILE)), file.exists(XWALK_PATH))
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# === STEP 1 — SAMPLE (same builder + headline mandate controls) ===
cat("=== STEP 1: SAMPLE ===\n")
.hdr  <- names(fread(file.path(ANALYSIS_DIR, PANEL_FILE), nrows = 0L))
.cols <- c("tank_panel_id", "panel_id", "state", "texas_treated",
           "install_yr_int", "make_model_noage", "cem_weight",
           "panel_year", "closure_event")
has_fuel <- "mm_fuel" %in% .hdr
if (has_fuel) .cols <- c(.cols, "mm_fuel")
mand_have <- intersect(MANDATES, .hdr)
.cols <- c(.cols, mand_have)
if ("first_year_churn" %in% .hdr) .cols <- c(.cols, "first_year_churn")

dt <- fread(file.path(ANALYSIS_DIR, PANEL_FILE), na.strings = c("", "NA"), select = .cols)
if ("first_year_churn" %in% names(dt)) dt <- dt[first_year_churn == 0L | is.na(first_year_churn)]
data_active <- build_active_at_treatment_sample(dt)
data_active <- data_active[rel_year %between% ES_WIN]
cat(sprintf("  sample (rel_year in [%d,%d]): %s tank-years | %s facilities | mandate ctrls: %s\n",
            ES_WIN[1], ES_WIN[2], fmt_n(nrow(data_active)), fmt_n(uniqueN(data_active$panel_id)),
            if (length(mand_have)) paste(mand_have, collapse = ",") else "NONE (absent)"))

# === STEP 2 — FACILITY COVARIATES (gas station, fuel, rural) ===
cat("=== STEP 2: COVARIATES ===\n")
xw <- fread(XWALK_PATH, select = c("panel_id", "facility_id", "state"),
            colClasses = list(character = c("panel_id", "facility_id", "state")))
g_ur <- fread(file.path(GIS_DIR, "gis_03_urban_rural.csv"),
              select = c("facility_id", "state", "ruca_primary"),
              colClasses = list(character = c("facility_id", "state")))
fac  <- merge(unique(xw), g_ur, by = c("facility_id", "state"), all.x = TRUE)
fac[, geo_rural := as.integer(ruca_primary >= 7)]
fac_cov <- fac[, .(panel_id, geo_rural)]
if (has_fuel) {
  fuel_fac <- data_active[, .(
    gas_station  = as.integer(any(mm_fuel == "Gasoline-Only", na.rm = TRUE)),
    fuel_gasonly = as.integer(all(mm_fuel == "Gasoline-Only", na.rm = TRUE) &
                              any(mm_fuel == "Gasoline-Only", na.rm = TRUE))
  ), by = panel_id]
  fac_cov <- merge(fac_cov, fuel_fac, by = "panel_id", all = TRUE)
}
data_active <- merge(data_active, fac_cov, by = "panel_id", all.x = TRUE)

# === STEP 3 — EVENT-STUDY FITTER (headline spec) ===
cat("=== STEP 3: EVENT STUDIES ===\n")
rhs_mand <- if (length(mand_have)) paste("+", paste(mand_have, collapse = " + ")) else ""
ES_FML <- as.formula(sprintf(
  "closure_event ~ i(rel_year, texas_treated, ref = -1L) %s | panel_id + cell_vintage_year_fe",
  rhs_mand))

# Fit ES on a (sub)sample; return tidy coefs with a ref row (rel_year=-1, est=0)
# and a pre/event/post period label. Backslash-free name parse (fixed=TRUE).
es_coefs <- function(d, series) {
  m  <- feols(ES_FML, data = d, cluster = ~state)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  ct <- ct[grepl("rel_year::", term, fixed = TRUE)]
  ct[, rel_year := as.integer(tstrsplit(sub("rel_year::", "", term, fixed = TRUE),
                                        ":", fixed = TRUE)[[1]])]
  out <- ct[, .(series, rel_year, est = Estimate, se = `Std. Error`)]
  out <- rbind(out, data.table(series, rel_year = -1L, est = 0, se = 0))
  out[, `:=`(ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
             period = factor(fifelse(rel_year < 0L, "pre",
                              fifelse(rel_year == 0L, "event", "post")),
                             levels = c("pre", "event", "post")))]
  setorder(out, rel_year)[]
}
# Pooled ES only. Group dynamics (gas/rural) are NOT done by split-sample: that
# re-estimates panel_id + cell_vintage_year_fe within each group (different
# baselines, different cell support). The correct group ES is an INTERACTION
# event study (i(rel_year, texas_treated) + i(rel_year, texas_treated:Z), common
# FEs) — a follow-up; the static interaction HTE lives in 02g.
es_list <- list(pooled = es_coefs(data_active, "All facilities"))

# === STEP 4 — PLOT (exact slide style: plot_es_clean) + SAVE ===
cat("=== STEP 4: FIGURES ===\n")
COL_PRE <- "#3A6BBF"; COL_EVENT <- "#888888"; COL_POST <- "#BF3A3A"

plot_es <- function(co, stem, facet = FALSE) {
  p <- ggplot(co, aes(rel_year, est, colour = period)) +
    geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.45) +
    geom_vline(xintercept = -1.5, colour = "grey40", linetype = "dotted", linewidth = 0.5) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.25, linewidth = 0.5) +
    geom_point(size = 2.0, shape = 21, fill = "white", stroke = 1.2) +
    scale_colour_manual(values = c(pre = COL_PRE, event = COL_EVENT, post = COL_POST),
                        labels = c(pre = "Pre-treatment", event = "Event", post = "Post-treatment"),
                        name = NULL) +
    scale_x_continuous(breaks = seq(-14, 22, by = 2)) +
    labs(x = "Years relative to Dec 22 1998", y = "Coefficient") +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.3),
          axis.text = element_text(colour = "black"),
          panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
          panel.grid.minor = element_blank(), plot.margin = margin(8, 12, 8, 8))
  if (facet) p <- p + facet_wrap(~series, ncol = 1, scales = "fixed")
  save_gg(p, stem, width = 7.6, height = if (facet) 7.6 else 4.8)
  cat(sprintf("  saved %s\n", stem))
}

plot_es(es_list$pooled, "Fig_ES_HTE_Pooled")

all_co <- rbindlist(es_list, use.names = TRUE)
fwrite(all_co, file.path(OUTPUT_TABLES, "T_HTE_EventStudy_Coefs.csv"))
cat(sprintf("  -> Output/Tables/T_HTE_EventStudy_Coefs.csv (%d rows)\n", nrow(all_co)))

# Pre-trend readout: max |coef| and max |t| over rel_year < -1 per series.
pre <- all_co[rel_year < -1L, .(max_abs_pre = max(abs(est)),
                                max_pre_t = max(abs(est / pmax(se, 1e-12)))), by = series]
cat("=== PRE-TREND CHECK (rel_year < -1; flat ~ good) ===\n"); print(pre)
cat("\n=== 02h COMPLETE ===\n")
