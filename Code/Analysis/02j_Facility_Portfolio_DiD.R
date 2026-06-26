################################################################################
# Code/Analysis/02j_Facility_Portfolio_DiD.R
#
# CAUSAL PORTFOLIO EFFECTS at the facility-year level, on the EXISTING matched
# facility panel (matched_facs_birth_cem.csv from 02b_Tank_level_Panel_Build.R) —
# the facility analogue of matched_tanks_birth_cem. No rebuild: outcomes,
# composition, fuel, mandates, did, rel_year, cem all already in that file.
#
# Identification (mirrors the tank headline, aggregated): parallel trends
# CONDITIONAL ON TANK MIX. The tank cell x year FE is carried up EXACTLY via the
# 02k crosswalk Yhat0 (composition-weighted untreated cell x year closure rate),
# left-joined on (panel_id, panel_year) -- NOT the coarse pct_sw/avg_age proxy.
#   ROUTE A (control):    Y_ft ~ did_term + Yhat0 + mandates | panel_id + year
#   ROUTE B (imputation): tau = closure_share_tank - Yhat0 ; tau ~ did_term | ...
# Both use the same exact cell baseline as the tank-level cell_vintage_year_fe.
#
# Outcomes (portfolio actions, all per active-tank or 0/1):
#   closure_share = n_closures / n_tanks_active   [primary intensity margin]
#   any_closure   = 1 if >=1 tank closed
#   facility_exit = facility leaves (from panel)
#   perm_share    = n_closures_permanent / n_tanks_active
#   repl_share    = n_closures_replacement / n_tanks_active
#
# HTE = INTERACTION only (did_Z, common FEs): gas_station (=has_gasoline),
#   dem_dense (top tract-pop quartile, GIS), geo_rural (RUCA>=7, GIS).
# Inference: wild cluster bootstrap by state (reduced_form_utils).
#
# Yhat0 + tau come from 02k (one tank pass over matched_tanks_birth_cem). The
# 45-degree predicted-rate figure can be added once this lands.
#
# Outputs:
#   Output/Tables/T_Facility_Portfolio_ATT.csv   (route A, per outcome)
#   Output/Tables/T_Facility_Portfolio_HTE.csv   (interaction did_Z, per Z)
#   Output/Tables/T_Facility_Portfolio_ES_Coefs.csv
#   Output/Figures/Fig_ES_Facility_Portfolio.{pdf,png}   (slide style)
#
# Run (panel on Z):
#   UST_ANALYSIS_DIR=Z:/ust_ins_move_to_github/Data/Analysis  Rscript ... 02j...R
################################################################################

suppressPackageStartupMessages({
  library(data.table); library(fixest); library(ggplot2); library(here)
})

# === LOGGING ===
dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
.log_path <- here::here("logs", paste0("02j_Facility_Portfolio_DiD_",
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
FAC_FILE     <- Sys.getenv("UST_FAC_FILE", "matched_facs_birth_cem.csv")
GIS_DIR      <- here::here("Data", "Processed", "GIS")
XW_PATH      <- here::here("Data", "Analysis", "facility_cellfe_xwalk.csv")  # 02k output
stopifnot(file.exists(file.path(ANALYSIS_DIR, FAC_FILE)))
stopifnot("Run 02k first (exact tank cell-FE crosswalk)" = file.exists(XW_PATH))
# GIS lookups are LOCAL-only (Data/ gitignored) -> OPTIONAL (rural / low-pop HTE).
HAS_GIS <- file.exists(file.path(GIS_DIR, "gis_03_urban_rural.csv")) &&
           file.exists(file.path(GIS_DIR, "gis_09_demographics.csv"))
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
BOOT_B <- as.integer(Sys.getenv("UST_BOOT_B", "1999"))
ES_WIN <- c(-13L, 22L)
MAND   <- c("any_mandate_release_det", "any_mandate_spill_overfill", "any_mandate_integrity")

# === STEP 1 — LOAD MATCHED FACILITY PANEL ===
cat("=== STEP 1: LOAD ===\n")
.hdr <- names(fread(file.path(ANALYSIS_DIR, FAC_FILE), nrows = 0L))
keep <- intersect(c("panel_id","state","panel_year","texas_treated","cem_weight",
  "n_tanks_active","n_closures","any_closure","facility_exit",
  "n_closures_permanent","n_closures_replacement","pct_sw","avg_tank_age",
  "has_gasoline","is_motor_fuel","facility_id", MAND), .hdr)
fy <- fread(file.path(ANALYSIS_DIR, FAC_FILE), select = keep,
            colClasses = list(character = c("panel_id","state","facility_id")))
mand_have <- intersect(MAND, names(fy))

# Reconstruct did/rel/post on the 1999 cutoff (match the tank headline exactly).
fy[, post     := as.integer(panel_year >= 1999L)]
fy[, did_term := texas_treated * post]
fy[, rel_year := as.integer(panel_year) - 1998L]
# Portfolio-action outcomes (shares of active tanks).
fy[, closure_share := pmin(n_closures / pmax(n_tanks_active, 1L), 1)]
if ("n_closures_permanent"   %in% names(fy)) fy[, perm_share := pmin(n_closures_permanent   / pmax(n_tanks_active,1L), 1)]
if ("n_closures_replacement" %in% names(fy)) fy[, repl_share := pmin(n_closures_replacement / pmax(n_tanks_active,1L), 1)]
cat(sprintf("  facility-years: %s | facilities: %s | states: %d | TX share: %.3f\n",
            fmt_n(nrow(fy)), fmt_n(uniqueN(fy$panel_id)), uniqueN(fy$state), mean(fy$texas_treated)))
cat(sprintf("  closure_share mean: %.4f | any_closure: %.4f | facility_exit: %.4f\n",
            mean(fy$closure_share), mean(fy$any_closure, na.rm=TRUE),
            if ("facility_exit" %in% names(fy)) mean(fy$facility_exit, na.rm=TRUE) else NA_real_))

# Left-join the EXACT tank cell x year baseline (02k) by (panel_id, panel_year).
# Yhat0 = composition-weighted untreated cell x year closure rate; tau = the
# imputation residual. This makes the facility control = the SAME cell FE as the
# tank-level analysis (not the coarse pct_sw/avg_age proxy).
xw <- fread(XW_PATH, colClasses = list(character = "panel_id"))
fy <- merge(fy, xw[, .(panel_id, panel_year, Yhat0, closure_share_tank, tau)],
            by = c("panel_id", "panel_year"), all.x = TRUE)
cat(sprintf("  cell-FE xwalk join: Yhat0 matched %.1f%% | corr(Y, Yhat0)=%.3f\n",
            100 * mean(!is.na(fy$Yhat0)),
            cor(fy$closure_share, fy$Yhat0, use = "complete.obs")))
fy <- fy[!is.na(Yhat0)]   # keep facility-years with the cell-FE baseline

# === STEP 2 — GIS COVARIATES (rural, population) + gas-station ===
cat("=== STEP 2: COVARIATES ===\n")
if ("has_gasoline" %in% names(fy)) fy[, gas_station := has_gasoline]   # from panel (no GIS)
if (HAS_GIS) {
  g_ur <- fread(file.path(GIS_DIR, "gis_03_urban_rural.csv"),
                select = c("facility_id","state","ruca_primary"),
                colClasses = list(character = c("facility_id","state")))
  g_dm <- fread(file.path(GIS_DIR, "gis_09_demographics.csv"),
                select = c("facility_id","state","tract_pop"),
                colClasses = list(character = c("facility_id","state")))
  gis <- merge(g_ur, g_dm, by = c("facility_id","state"), all = TRUE)
  gis[, geo_rural  := as.integer(ruca_primary >= 7)]
  # Low population = bottom 25% of tract population (vs the upper three quartiles).
  gis[, dem_lowpop := as.integer(tract_pop <= quantile(tract_pop, 0.25, na.rm = TRUE))]
  fy <- merge(fy, gis[, .(facility_id, state, geo_rural, dem_lowpop)],
              by = c("facility_id","state"), all.x = TRUE)
  cat(sprintf("  geo_rural matched: %.1f%% | dem_lowpop matched: %.1f%%\n",
              100*mean(!is.na(fy$geo_rural)), 100*mean(!is.na(fy$dem_lowpop))))
} else cat("  GIS lookups absent -> rural / low-population HTE skipped.\n")
cat(sprintf("  gas_station mean: %.3f\n",
            if ("gas_station" %in% names(fy)) mean(fy$gas_station, na.rm=TRUE) else NA_real_))

# Cell-FE control = the EXACT tank cell x year baseline carried up (Yhat0, 02k),
# plus the mandate dummies. Replaces the coarse pct_sw/avg_age x year proxy.
ctrl <- c("Yhat0", mand_have)
RHS  <- paste("+", paste(ctrl, collapse = " + "))

# === STEP 3 — ROUTE A STATIC ATT (per outcome) ===
cat("=== STEP 3: ROUTE A STATIC ATT ===\n")
outs <- intersect(c("closure_share","any_closure","facility_exit","perm_share","repl_share"), names(fy))
att <- list(); mA_list <- list()
for (yv in outs) {
  m  <- feols(as.formula(sprintf("%s ~ did_term %s | panel_id + panel_year", yv, RHS)),
              data = fy, cluster = ~state)
  mA_list[[yv]] <- m
  ct <- coeftable(m); est <- ct["did_term","Estimate"]; se <- ct["did_term","Std. Error"]
  att[[yv]] <- data.table(outcome = yv, beta = est, se = se,
                          ci_lo = est-1.96*se, ci_hi = est+1.96*se, p = ct["did_term","Pr(>|t|)"], n = m$nobs)
  cat(sprintf("  [A] %-14s did=%+.4f (SE %.4f, p %.3f)\n", yv, est, se, ct["did_term","Pr(>|t|)"]))
}
# Route B (imputation): tau = closure_share_tank - Yhat0 already nets the cell-FE
# baseline, so a plain DiD on tau recovers the ATT (control tau ~ 0).
mB  <- feols(tau ~ did_term | panel_id + panel_year, data = fy, cluster = ~state)
ctB <- coeftable(mB); eB <- ctB["did_term","Estimate"]; sB <- ctB["did_term","Std. Error"]
att[["B_tau"]] <- data.table(outcome = "closure_share (route B: tau)", beta = eB, se = sB,
  ci_lo = eB-1.96*sB, ci_hi = eB+1.96*sB, p = ctB["did_term","Pr(>|t|)"], n = mB$nobs)
cat(sprintf("  [B] %-14s did=%+.4f (SE %.4f, p %.3f)\n", "tau", eB, sB, ctB["did_term","Pr(>|t|)"]))
fwrite(rbindlist(att), file.path(OUTPUT_TABLES, "T_Facility_Portfolio_ATT.csv"))

# Publication table: route-A outcomes (cols 1..k) + route-B imputation (last col).
ATT_HEAD <- c(closure_share="Closure share", any_closure="Any closure",
              facility_exit="Facility exit", perm_share="Permanent", repl_share="Replacement")
pub_etable(c(mA_list, list(B = mB)),
  file.path(OUTPUT_TABLES, "T_Facility_Portfolio_ATT_Pub.tex"),
  headers = c(unname(ATT_HEAD[names(mA_list)]), "Closure (impute)"),
  title   = "Effect of the 1998 Texas reform on facility portfolio actions",
  notes   = "Facility-year sample. Columns 1-5: two-way (facility, year) fixed-effects estimator controlling for the composition-weighted closure baseline. Final column: imputation (predicted-rate) estimator on the baseline-netted outcome. Standard errors clustered by state (18 clusters).")

# === STEP 4 — HTE (INTERACTION did_Z, common FEs) ===
cat("=== STEP 4: HTE (interaction) ===\n")
ZLAB <- c(gas_station = "Gas station", dem_lowpop = "Low population", geo_rural = "Rural")
hte <- list(); mH_list <- list()
for (z in intersect(c("gas_station","dem_lowpop","geo_rural"), names(fy))) {
  d <- fy[!is.na(get(z))]
  d[, did_Z := did_term * get(z)]
  m  <- feols(as.formula(sprintf("closure_share ~ did_term + did_Z %s | panel_id + panel_year", RHS)),
              data = d, cluster = ~state)
  mH_list[[z]] <- m
  ct <- coeftable(m); eZ <- ct["did_Z","Estimate"]; sZ <- ct["did_Z","Std. Error"]
  hte[[z]] <- data.table(dimension = z, did_term = ct["did_term","Estimate"], did_Z = eZ,
                         se = sZ, ci_lo = eZ-1.96*sZ, ci_hi = eZ+1.96*sZ, p = ct["did_Z","Pr(>|t|)"])
  cat(sprintf("  %-12s did=%+.4f  didxZ=%+.4f (p %.3f)\n",
              z, ct["did_term","Estimate"], eZ, ct["did_Z","Pr(>|t|)"]))
}
if (length(hte)) {
  fwrite(rbindlist(hte), file.path(OUTPUT_TABLES, "T_Facility_Portfolio_HTE.csv"))
  pub_etable(mH_list, file.path(OUTPUT_TABLES, "T_Facility_Portfolio_HTE_Pub.tex"),
    headers = unname(ZLAB[names(mH_list)]),
    title   = "Heterogeneous portfolio response to the 1998 Texas reform (facility-year)",
    notes   = "Dependent variable: facility closure share. Each column reports the average effect (Reform $\\times$ Post) and its interaction with the subgroup indicator. Facility and year fixed effects; composition-weighted baseline control. Standard errors clustered by state (18 clusters).")
}

# === STEP 5 — ROUTE A EVENT STUDY (slide style) ===
cat("=== STEP 5: EVENT STUDY ===\n")
fes <- fy[rel_year %between% ES_WIN]
m_es <- feols(as.formula(sprintf(
  "closure_share ~ i(rel_year, texas_treated, ref = -1L) %s | panel_id + panel_year", RHS)),
  data = fes, cluster = ~state)
ct <- as.data.table(coeftable(m_es), keep.rownames = "term")[grepl("rel_year::", term, fixed = TRUE)]
ct[, rel_year := as.integer(tstrsplit(sub("rel_year::", "", term, fixed = TRUE), ":", fixed = TRUE)[[1]])]
co <- ct[, .(rel_year, est = Estimate, se = `Std. Error`)]
co <- rbind(co, data.table(rel_year = -1L, est = 0, se = 0))
co[, `:=`(ci_lo = est - 1.96*se, ci_hi = est + 1.96*se,
          period = factor(fifelse(rel_year < 0L,"pre",fifelse(rel_year==0L,"event","post")),
                          levels = c("pre","event","post")))]
setorder(co, rel_year)
fwrite(co, file.path(OUTPUT_TABLES, "T_Facility_Portfolio_ES_Coefs.csv"))

p <- ggplot(co, aes(rel_year, est, colour = period)) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.45) +
  geom_vline(xintercept = -1.5, colour = "grey40", linetype = "dotted", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.25, linewidth = 0.5) +
  geom_point(size = 2.0, shape = 21, fill = "white", stroke = 1.2) +
  scale_colour_manual(values = c(pre="#3A6BBF", event="#888888", post="#BF3A3A"),
                      labels = c(pre="Pre-treatment", event="Event", post="Post-treatment"), name = NULL) +
  scale_x_continuous(breaks = seq(-14, 22, by = 2)) +
  labs(x = "Years relative to Dec 22 1998", y = "Effect on facility closure share") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"),
        axis.line = element_line(colour = "black", linewidth = 0.4),
        axis.ticks = element_line(colour = "black", linewidth = 0.3),
        axis.text = element_text(colour = "black"),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(), plot.margin = margin(8, 12, 8, 8))
save_gg(p, "Fig_ES_Facility_Portfolio", width = 7.6, height = 4.8)
cat("  saved Fig_ES_Facility_Portfolio\n")
pre <- co[rel_year < -1L, .(max_abs_pre = max(abs(est)), max_t = max(abs(est/pmax(se,1e-12))))]
cat(sprintf("  PRE-TREND: max|coef|=%.4f  max|t|=%.2f\n", pre$max_abs_pre, pre$max_t))
cat("\n=== 02j COMPLETE ===\n")
