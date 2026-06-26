################################################################################
# Code/Analysis/02j_Facility_Portfolio_DiD.R
#
# CAUSAL PORTFOLIO EFFECTS at the facility-year level, on the EXISTING matched
# facility panel (matched_facs_birth_cem.csv) — the facility analogue of
# matched_tanks_birth_cem.
#
# IDENTIFICATION mirrors the tank headline, aggregated. The tank cell x year FE
# (cell_vintage_year_fe = make_model_noage x install_yr x panel_year) has a clean
# FACILITY twin: make_model_fac x panel_year, where make_model_fac is the
# facility's portfolio-mix signature (wall-mix | fuel-mix | age-band; 47 values).
# So the facility spec is the SAME FE design as the tank one:
#     Y_ft ~ did_term + Yhat0 + mandates | panel_id + cell_fac_year
#   cell_fac_year = portfolio-mix x year FE  ("same mix, same year, TX vs control")
#   Yhat0 (02k composition-weighted untreated baseline) KEPT as an extra control.
# Route B (imputation) kept as a robustness: tau = closure_share_tank - Yhat0.
#
# MARGINS (facility portfolio actions): closure_share, any_closure, facility_exit,
#   downsize (closed some, did not fully exit), permanent, replacement, capacity-decrease.
#
# HTE (interaction only, common FEs):
#   (a) fuel/geo: gas_station, low-population (bottom pop quartile), rural.
#   (b) SIZE: did x capacity bin, DCM-aligned (TOTAL facility capacity at 9k/20k/30k =
#       G1..G4), on each behavior margin -> "what size of firm moves on which margin".
#
# Inference: analytic cluster-robust by state (G=18). Bootstrap/HonestDiD deferred.
#
# Outputs: Output/Tables/T_Facility_Portfolio_ATT_Pub.tex, _HTE_Pub.tex,
#   T_Facility_SizeHTE_Pub.tex (+ CSVs), Output/Figures/Fig_ES_Facility_Portfolio.*
#
# Run (panel on Z locally; in-repo on server — DO NOT set UST_ANALYSIS_DIR there).
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
XW_PATH   <- here::here("Data", "Analysis", "facility_cellfe_xwalk.csv")  # 02k output
GIS_HTE   <- here::here("Output", "GIS", "gis_hte_vars.csv")             # git-tracked, panel_id
EDGES     <- here::here("Output", "GIS", "gis_neighbor_edges.parquet")   # git-tracked (force-added)
stopifnot(file.exists(file.path(ANALYSIS_DIR, FAC_FILE)))
stopifnot("Run 02k first (exact tank cell-FE crosswalk)" = file.exists(XW_PATH))
HAS_GIS   <- file.exists(GIS_HTE)                                        # census splits (Census 2000)
HAS_EDGES <- file.exists(EDGES) && requireNamespace("arrow", quietly = TRUE)  # competition edges
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
BOOT_B <- as.integer(Sys.getenv("UST_BOOT_B", "1999"))   # unused (bootstrap deferred)
ES_WIN <- c(-13L, 22L)
MAND   <- c("any_mandate_release_det", "any_mandate_spill_overfill", "any_mandate_integrity")
CAP_BREAKS <- c(-Inf, 9000, 20000, 30000, Inf)           # DCM G breaks (per-tank gallons)
CAP_LABS   <- c("G1_lt9k", "G2_9to20k", "G3_20to30k", "G4_gt30k")

# === STEP 1 — LOAD MATCHED FACILITY PANEL ===
cat("=== STEP 1: LOAD ===\n")
.hdr <- names(fread(file.path(ANALYSIS_DIR, FAC_FILE), nrows = 0L))
keep <- intersect(c("panel_id","state","panel_year","texas_treated","cem_weight",
  "n_tanks_active","n_closures","any_closure","facility_exit",
  "n_closures_permanent","n_closures_replacement","net_tank_change","capacity_decreased",
  "make_model_fac","fac_vintage","total_capacity_reform","n_tanks_at_reform",
  "has_gasoline","facility_id", MAND), .hdr)
fy <- fread(file.path(ANALYSIS_DIR, FAC_FILE), select = keep,
            colClasses = list(character = c("panel_id","state","facility_id")))
mand_have <- intersect(MAND, names(fy))

fy[, post     := as.integer(panel_year >= 1999L)]
fy[, did_term := texas_treated * post]
fy[, rel_year := as.integer(panel_year) - 1998L]
# Portfolio-action outcomes.
fy[, closure_share := pmin(n_closures / pmax(n_tanks_active, 1L), 1)]
if ("n_closures_permanent"   %in% names(fy)) fy[, perm_share := pmin(n_closures_permanent   / pmax(n_tanks_active,1L), 1)]
if ("n_closures_replacement" %in% names(fy)) fy[, repl_share := pmin(n_closures_replacement / pmax(n_tanks_active,1L), 1)]
fy[, downsize := as.integer(n_closures > 0L & facility_exit == 0L)]   # closed some, stayed open
if ("capacity_decreased" %in% names(fy)) fy[, cap_decrease := as.integer(capacity_decreased)]

# Facility cell x year FE = portfolio-mix x year (tank cell_vintage_year_fe twin).
fy[, cell_fac_year := .GRP, by = .(make_model_fac, panel_year)]
# DCM-aligned size: TOTAL facility capacity (reform) at the structural G breaks --
# the DCM capacity state is built on total portfolio capacity, not per-tank.
fy[, cap_G := factor(cut(total_capacity_reform, CAP_BREAKS, labels = CAP_LABS), levels = CAP_LABS)]
if ("fac_vintage" %in% names(fy)) fy[, vintage := factor(fac_vintage)]   # age/cohort at treatment
cat(sprintf("  facility-years: %s | facilities: %s | states: %d | TX share: %.3f\n",
            fmt_n(nrow(fy)), fmt_n(uniqueN(fy$panel_id)), uniqueN(fy$state), mean(fy$texas_treated)))
cat(sprintf("  make_model_fac mixes: %d | cap_G dist: %s\n",
            uniqueN(fy$make_model_fac),
            paste(names(table(fy$cap_G)), round(prop.table(table(fy$cap_G)),3), sep="=", collapse=" ")))
cat(sprintf("  margins: closure=%.4f exit=%.4f downsize=%.4f cap_decr=%.4f\n",
            mean(fy$closure_share), mean(fy$facility_exit,na.rm=TRUE), mean(fy$downsize),
            if ("cap_decrease" %in% names(fy)) mean(fy$cap_decrease,na.rm=TRUE) else NA_real_))

# Left-join the 02k cell-FE crosswalk (Yhat0 baseline + tau).
xw <- fread(XW_PATH, colClasses = list(character = "panel_id"))
fy <- merge(fy, xw[, .(panel_id, panel_year, Yhat0, closure_share_tank, tau)],
            by = c("panel_id", "panel_year"), all.x = TRUE)
cat(sprintf("  Yhat0 matched %.1f%%\n", 100 * mean(!is.na(fy$Yhat0))))
fy <- fy[!is.na(Yhat0)]

# === STEP 2 — COVARIATES (gas-station from panel; rural/low-pop from GIS if present) ===
cat("=== STEP 2: COVARIATES (gas-station + spatial HTE, all panel_id-keyed) ===\n")
if ("has_gasoline" %in% names(fy)) fy[, gas_station := has_gasoline]

# Spatial HTE vars FIXED AT TREATMENT (Census 2000), git-tracked in Output/GIS/,
# keyed on panel_id -> reach the server via git pull (no GIS toolchain needed).
if (HAS_GIS) {
  gv <- fread(GIS_HTE, select = c("panel_id","rural_2000","low_pop_density",
              "med_hh_income_2000","pct_poverty_2000"),
              colClasses = list(character = "panel_id"))
  fy <- merge(fy, gv, by = "panel_id", all.x = TRUE)
  fy[, rural      := rural_2000]
  fy[, low_pop    := low_pop_density]
  fy[, low_income := as.integer(med_hh_income_2000 < median(med_hh_income_2000, na.rm = TRUE))]
  fy[, high_pov   := as.integer(pct_poverty_2000   > median(pct_poverty_2000,   na.rm = TRUE))]
  cat(sprintf("  census HTE matched: rural %.1f%% | low_pop %.1f%% | income %.1f%%\n",
              100*mean(!is.na(fy$rural)), 100*mean(!is.na(fy$low_pop)), 100*mean(!is.na(fy$low_income))))
} else cat("  gis_hte_vars.csv absent -> census spatial HTE skipped.\n")

# Competition: PRE-REFORM (fixed) count of active neighbors within 1 mi at 1998.
# Fixed at treatment -> a clean, non-endogenous split (matches the census-vars logic),
# and far cheaper than the time-varying cross. Restricted to gas retail in Step 4.
if (HAS_EDGES) {
  ed <- as.data.table(arrow::read_parquet(EDGES, col_select = c("panel_id_i","panel_id_j")))
  active98 <- fy[panel_year == 1998L & n_tanks_active > 0L, unique(panel_id)]
  el  <- rbind(ed[, .(p = panel_id_i, nb = panel_id_j)], ed[, .(p = panel_id_j, nb = panel_id_i)])
  nnb <- el[nb %in% active98, .(n_neigh98 = .N), by = p]
  fy  <- merge(fy, nnb, by.x = "panel_id", by.y = "p", all.x = TRUE)
  fy[is.na(n_neigh98), n_neigh98 := 0L]
  thr <- fy[gas_station == 1L, median(n_neigh98, na.rm = TRUE)]
  fy[, thin_market := as.integer(n_neigh98 <= thr)]
  cat(sprintf("  competition: %s edges | median active-98 neighbors (gas) = %.0f\n",
              fmt_n(nrow(ed)), thr))
} else cat("  edges parquet / arrow absent -> competition HTE skipped.\n")

# Control block: Yhat0 (KEPT) + mandate dummies. FE = facility + portfolio-mix x year.
RHS <- paste("+", paste(c("Yhat0", mand_have), collapse = " + "))
FE  <- "panel_id + cell_fac_year"

# === STEP 3 — ROUTE A STATIC ATT (per margin) + ROUTE B ===
cat("=== STEP 3: ROUTE A STATIC ATT ===\n")
outs <- intersect(c("closure_share","any_closure","facility_exit","downsize",
                    "perm_share","repl_share","cap_decrease"), names(fy))
att <- list(); mA_list <- list()
for (yv in outs) {
  m  <- feols(as.formula(sprintf("%s ~ did_term %s | %s", yv, RHS, FE)), data = fy, cluster = ~state)
  mA_list[[yv]] <- m
  ct <- coeftable(m); est <- ct["did_term","Estimate"]; se <- ct["did_term","Std. Error"]
  att[[yv]] <- data.table(outcome = yv, beta = est, se = se,
                          ci_lo = est-1.96*se, ci_hi = est+1.96*se, p = ct["did_term","Pr(>|t|)"], n = m$nobs)
  cat(sprintf("  [A] %-14s did=%+.4f (SE %.4f, p %.3f)\n", yv, est, se, ct["did_term","Pr(>|t|)"]))
}
# Route B (imputation): tau already nets the composition baseline.
mB  <- feols(tau ~ did_term | panel_id + panel_year, data = fy, cluster = ~state)
ctB <- coeftable(mB); eB <- ctB["did_term","Estimate"]; sB <- ctB["did_term","Std. Error"]
att[["B_tau"]] <- data.table(outcome = "closure_share (route B: tau)", beta = eB, se = sB,
  ci_lo = eB-1.96*sB, ci_hi = eB+1.96*sB, p = ctB["did_term","Pr(>|t|)"], n = mB$nobs)
cat(sprintf("  [B] %-14s did=%+.4f (SE %.4f, p %.3f)\n", "tau", eB, sB, ctB["did_term","Pr(>|t|)"]))
fwrite(rbindlist(att), file.path(OUTPUT_TABLES, "T_Facility_Portfolio_ATT.csv"))

ATT_HEAD <- c(closure_share="Closure share", any_closure="Any closure", facility_exit="Facility exit",
              downsize="Downsize", perm_share="Permanent", repl_share="Replacement", cap_decrease="Capacity cut")
pub_etable(c(mA_list, list(B = mB)),
  file.path(OUTPUT_TABLES, "T_Facility_Portfolio_ATT_Pub.tex"),
  headers = c(unname(ATT_HEAD[names(mA_list)]), "Closure (impute)"),
  title   = "Effect of the 1998 Texas reform on facility portfolio actions",
  notes   = "Facility-year sample. Columns 1-k: two-way (facility, portfolio-mix x year) fixed-effects estimator controlling for the composition-weighted baseline. Final column: imputation (predicted-rate) estimator on the baseline-netted outcome. SE clustered by state (18 clusters).")

# === STEP 4 — HTE (interaction) across DIMS x KEY MARGINS ===
cat("=== STEP 4: HTE (fuel / spatial / competition) x margins ===\n")
ZLAB <- c(gas_station="Gas station", rural="Rural", low_pop="Low population",
          low_income="Low income", high_pov="High poverty", thin_market="Thin market")
DIMS <- intersect(names(ZLAB), names(fy))
MARG <- intersect(c("closure_share","facility_exit","downsize","repl_share"), names(fy))
hte_bin <- function(yv, z) {
  d <- fy[!is.na(get(z))]
  if (z == "thin_market" && "gas_station" %in% names(d)) d <- d[gas_station == 1L]   # competition: gas retail only
  d[, did_Z := did_term * get(z)]
  feols(as.formula(sprintf("%s ~ did_term + did_Z %s | %s", yv, RHS, FE)), data = d, cluster = ~state)
}
hte_rows <- list(); mH <- list()
for (yv in MARG) for (z in DIMS) {
  m <- hte_bin(yv, z); mH[[paste(yv, z)]] <- m; ct <- coeftable(m)
  hte_rows[[paste(yv, z)]] <- data.table(margin = yv, dimension = z,
    did_term = ct["did_term","Estimate"], did_Z = ct["did_Z","Estimate"],
    se_Z = ct["did_Z","Std. Error"], p_Z = ct["did_Z","Pr(>|t|)"])
  cat(sprintf("  %-13s x %-11s didxZ=%+.4f (p %.3f)\n", yv, z, ct["did_Z","Estimate"], ct["did_Z","Pr(>|t|)"]))
}
fwrite(rbindlist(hte_rows), file.path(OUTPUT_TABLES, "T_Facility_HTE_byMargin.csv"))
for (yv in intersect(c("closure_share","facility_exit","repl_share"), MARG)) {
  ms <- mH[paste(yv, DIMS)]; names(ms) <- DIMS
  pub_etable(ms, file.path(OUTPUT_TABLES, sprintf("T_Facility_HTE_%s_Pub.tex", yv)),
    headers = unname(ZLAB[DIMS]),
    title   = sprintf("Heterogeneous response (facility-year): %s", ATT_HEAD[[yv]]),
    notes   = "Reform x Post and its interaction with each subgroup (one regression per column). Facility + portfolio-mix x year FE; composition-weighted baseline control. Thin-market column = gas retail only. SE clustered by state.")
}

# === STEP 5 — SIZE HTE (did x DCM total-capacity bin) x margins ===
cat("=== STEP 5: SIZE HTE (DCM total-capacity bins) ===\n")
cat_hte <- function(yv, fvar, ref) {
  d <- fy[!is.na(get(fvar))]
  feols(as.formula(sprintf("%s ~ i(%s, did_term, ref = '%s') + did_term %s | %s",
                           yv, fvar, ref, RHS, FE)), data = d, cluster = ~state)
}
SZM <- intersect(c("closure_share","facility_exit","downsize","repl_share","cap_decrease"), names(fy))
mS <- setNames(lapply(SZM, function(yv) cat_hte(yv, "cap_G", CAP_LABS[1])), SZM)
pub_etable(mS, file.path(OUTPUT_TABLES, "T_Facility_SizeHTE_Pub.tex"),
  headers = unname(ATT_HEAD[names(mS)]),
  title   = "Size heterogeneity (DCM total-capacity bins) by margin (facility-year)",
  notes   = "Reform x Post = smallest bin (G1, total < 9k gal); cap_G rows = differential for larger bins (9-20k / 20-30k / 30k+ total gallons). Facility + portfolio-mix x year FE. SE clustered by state.")
cat(sprintf("  size HTE: %d margins\n", length(mS)))

# === STEP 5b — VINTAGE-AT-TREATMENT HTE (who moves by age) x margins ===
cat("=== STEP 5b: VINTAGE HTE (age at treatment) ===\n")
VREF <- "1989-1998"   # newest pre-reform cohort = reference
if ("vintage" %in% names(fy) && VREF %in% levels(fy$vintage)) {
  mV <- setNames(lapply(SZM, function(yv) cat_hte(yv, "vintage", VREF)), SZM)
  pub_etable(mV, file.path(OUTPUT_TABLES, "T_Facility_VintageHTE_Pub.tex"),
    headers = unname(ATT_HEAD[names(mV)]),
    title   = "Vintage-at-treatment heterogeneity by margin (facility-year)",
    notes   = sprintf("Reform x Post = the %s cohort (reference); vintage rows = differential for older/younger cohorts. Facility + portfolio-mix x year FE. SE clustered by state.", VREF))
  cat(sprintf("  vintage HTE: %d margins (ref %s)\n", length(mV), VREF))
} else cat("  vintage var/ref absent -> vintage HTE skipped.\n")

# === STEP 6 — CAUSAL EVENT STUDIES on closure / downsize / replace ===
cat("=== STEP 6: EVENT STUDIES (closure / downsize / replace) ===\n")
fes <- fy[rel_year %between% ES_WIN]
es_one <- function(yv, ylab, stem) {
  m  <- feols(as.formula(sprintf("%s ~ i(rel_year, texas_treated, ref = -1L) %s | %s", yv, RHS, FE)),
              data = fes, cluster = ~state)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl("rel_year::", term, fixed = TRUE)]
  ct[, rel_year := as.integer(tstrsplit(sub("rel_year::", "", term, fixed = TRUE), ":", fixed = TRUE)[[1]])]
  co <- ct[, .(rel_year, est = Estimate, se = `Std. Error`)]
  co <- rbind(co, data.table(rel_year = -1L, est = 0, se = 0))
  co[, `:=`(ci_lo = est-1.96*se, ci_hi = est+1.96*se, margin = yv,
            period = factor(fifelse(rel_year<0L,"pre",fifelse(rel_year==0L,"event","post")),
                            levels = c("pre","event","post")))]
  setorder(co, rel_year)
  p <- ggplot(co, aes(rel_year, est, colour = period)) +
    geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.45) +
    geom_vline(xintercept = -1.5, colour = "grey40", linetype = "dotted", linewidth = 0.5) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.25, linewidth = 0.5) +
    geom_point(size = 2.0, shape = 21, fill = "white", stroke = 1.2) +
    scale_colour_manual(values = c(pre="#3A6BBF", event="#888888", post="#BF3A3A"),
                        labels = c(pre="Pre-treatment", event="Event", post="Post-treatment"), name = NULL) +
    scale_x_continuous(breaks = seq(-14, 22, by = 2)) +
    labs(x = "Years relative to Dec 22 1998", y = ylab) +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"),
          axis.line = element_line(colour = "black", linewidth = 0.4),
          axis.ticks = element_line(colour = "black", linewidth = 0.3),
          axis.text = element_text(colour = "black"),
          panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
          panel.grid.minor = element_blank(), plot.margin = margin(8, 12, 8, 8))
  save_gg(p, stem, width = 7.6, height = 4.8)
  pt <- co[rel_year < -1L, .(max_abs_pre = max(abs(est)), max_t = max(abs(est/pmax(se,1e-12))))]
  cat(sprintf("  %-13s -> %s | PRE max|coef|=%.4f max|t|=%.2f\n", yv, stem, pt$max_abs_pre, pt$max_t))
  co
}
es_all <- rbindlist(list(
  es_one("closure_share", "Effect on facility closure share", "Fig_ES_Facility_Portfolio"),
  if ("downsize"   %in% names(fy)) es_one("downsize",   "Effect on facility downsize",    "Fig_ES_Facility_Downsize"),
  if ("repl_share" %in% names(fy)) es_one("repl_share", "Effect on facility replacement", "Fig_ES_Facility_Replace")
), use.names = TRUE)
fwrite(es_all, file.path(OUTPUT_TABLES, "T_Facility_ES_Coefs_byMargin.csv"))
cat("\n=== 02j COMPLETE ===\n")
