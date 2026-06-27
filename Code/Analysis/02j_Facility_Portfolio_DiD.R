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
#   Ticket 035 additions: T_Facility_SampleCompare_ATT.csv,
#     T_Facility_SampleCompare_SizeHTE.csv, T_Facility_SampleCompare_VintageHTE.csv,
#     Fig_ES_Facility_{Portfolio,Downsize,Consolidate}_allinc.*
#
# Run (panel on Z locally; in-repo on server -- DO NOT set UST_ANALYSIS_DIR there).
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
FAC_PANEL    <- here::here("Data", "Analysis", "facility_panel.csv")   # all-incumbents (Ticket 035)
XW_PATH   <- here::here("Data", "Analysis", "facility_cellfe_xwalk.csv")  # 02k output
GIS_HTE   <- here::here("Output", "GIS", "gis_hte_vars.csv")             # git-tracked, panel_id
EDGES     <- here::here("Output", "GIS", "gis_neighbor_edges.parquet")   # git-tracked (force-added)
stopifnot(file.exists(file.path(ANALYSIS_DIR, FAC_FILE)))
stopifnot("Run 02k first (exact tank cell-FE crosswalk)" = file.exists(XW_PATH))
HAS_GIS   <- file.exists(GIS_HTE)
HAS_EDGES <- file.exists(EDGES) && requireNamespace("arrow", quietly = TRUE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)
BOOT_B <- as.integer(Sys.getenv("UST_BOOT_B", "1999"))   # unused (bootstrap deferred)
ES_WIN <- c(-13L, 22L)
MAND   <- c("any_mandate_release_det", "any_mandate_spill_overfill", "any_mandate_integrity")
CAP_BREAKS <- c(-Inf, 9000, 20000, 30000, Inf)           # DCM G breaks (per-tank gallons)
CAP_LABS   <- c("G1_lt9k", "G2_9to20k", "G3_20to30k", "G4_gt30k")
MARG_ALL   <- c("closure_share", "facility_exit", "downsize", "consolidate",
                "reconfigure_up", "repl_share", "cap_decrease")   # fixed vector for comparison

# === HELPER FUNCTIONS ===
cat("=== HELPERS ===\n")

# Build all outcome margin columns + cell_fac_year + did_term + rel_year in-place.
# Detects capacity column names internally. REL_THRESH from outer scope (0.05).
build_margins <- function(fy) {
  .hdr_bm    <- names(fy)
  .cap_chg_bm <- if ("capacity_change"      %in% .hdr_bm) "capacity_change" else
                 if ("capacity_change_year" %in% .hdr_bm) "capacity_change_year" else NULL
  .cap_lev_bm <- if ("total_capacity_dec" %in% .hdr_bm) "total_capacity_dec" else
                 if ("total_capacity"     %in% .hdr_bm) "total_capacity" else NULL
  if (is.null(.cap_chg_bm) || is.null(.cap_lev_bm))
    stop("build_margins: capacity_change / total_capacity_dec absent from input.")
  cat(sprintf("  build_margins: cap_change='%s'  cap_level='%s'\n", .cap_chg_bm, .cap_lev_bm))
  fy[, post     := as.integer(panel_year >= 1999L)]
  fy[, did_term := texas_treated * post]
  fy[, rel_year := as.integer(panel_year) - 1998L]
  fy[, closure_share := pmin(n_closures / pmax(n_tanks_active, 1L), 1)]
  if ("n_closures_permanent"   %in% .hdr_bm)
    fy[, perm_share := pmin(n_closures_permanent   / pmax(n_tanks_active, 1L), 1)]
  if ("n_closures_replacement" %in% .hdr_bm)
    fy[, repl_share := pmin(n_closures_replacement / pmax(n_tanks_active, 1L), 1)]
  setorder(fy, panel_id, panel_year)
  fy[, dC   := get(.cap_chg_bm)]
  fy[, Ceoy := get(.cap_lev_bm)]
  fy[, Clag := Ceoy - dC]
  fy[, rel_cap     := fifelse(Clag > 0, dC / Clag, NA_real_)]
  fy[, contraction := as.integer(facility_exit == 0L & net_tank_change < 0L & Clag > 0 & !is.na(dC))]
  fy[, consolidate    := as.integer(contraction == 1L & abs(rel_cap) <= REL_THRESH)]
  fy[, downsize       := as.integer(contraction == 1L & rel_cap <  -REL_THRESH)]
  fy[, reconfigure_up := as.integer(contraction == 1L & rel_cap >   REL_THRESH)]
  fy[is.na(consolidate),    consolidate    := 0L]
  fy[is.na(downsize),       downsize       := 0L]
  fy[is.na(reconfigure_up), reconfigure_up := 0L]
  stopifnot(fy[, max(consolidate + downsize + reconfigure_up)] <= 1L)
  stopifnot(fy[facility_exit == 1L,   sum(consolidate + downsize + reconfigure_up)] == 0L)
  stopifnot(fy[net_tank_change >= 0L, sum(consolidate + downsize + reconfigure_up)] == 0L)
  stopifnot(fy[is.na(dC),             sum(consolidate + downsize + reconfigure_up)] == 0L)
  fy[, contraction := NULL]
  n_elig <- fy[facility_exit == 0L & net_tank_change < 0L & Clag > 0 & !is.na(dC), .N]
  cat(sprintf("  contraction partition: n_eligible=%d | consolidate=%.4f | downsize=%.4f | reconfigure_up=%.4f\n",
              n_elig, mean(fy$consolidate), mean(fy$downsize), mean(fy$reconfigure_up)))
  if ("capacity_decreased" %in% .hdr_bm) fy[, cap_decrease := as.integer(capacity_decreased)]
  # cell_fac_year built per-sample so each sample's FE spans its own facilities
  fy[, cell_fac_year := .GRP, by = .(make_model_fac, panel_year)]
  fy
}

# feols loop over MARG_ALL. Returns data.table(margin, beta, se, p, n).
att_table <- function(fy_in, rhs) {
  rows <- list()
  for (mg in MARG_ALL) {
    if (!mg %in% names(fy_in)) next
    m  <- feols(as.formula(sprintf("%s ~ did_term %s | panel_id + cell_fac_year", mg, rhs)),
                data = fy_in, cluster = ~state)
    ct <- coeftable(m)
    rows[[mg]] <- data.table(margin = mg,
                             beta   = ct["did_term", "Estimate"],
                             se     = ct["did_term", "Std. Error"],
                             p      = ct["did_term", "Pr(>|t|)"],
                             n      = m$nobs)
  }
  rbindlist(rows)
}

# Event study for one margin. data/rhs/fe default to matched C1 globals so
# existing STEP 6 call sites are byte-identical.
es_one <- function(yv, ylab, stem, data = fy, rhs = RHS, fe = FE) {
  fes_loc <- data[rel_year %between% ES_WIN]
  m  <- feols(as.formula(sprintf("%s ~ i(rel_year, texas_treated, ref = -1L) %s | %s", yv, rhs, fe)),
              data = fes_loc, cluster = ~state)
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

# Parameterized categorical HTE (used in Step 9 for the comparison; Steps 5/5b keep
# the local cat_hte closure for C1 byte-identical safety).
cat_hte_param <- function(fy_in, yv, fvar, ref, rhs) {
  d <- fy_in[!is.na(get(fvar))]
  feols(as.formula(sprintf("%s ~ i(%s, did_term, ref = '%s') + did_term %s | %s",
                           yv, fvar, ref, rhs, FE)), data = d, cluster = ~state)
}

# === STEP 1 — LOAD MATCHED FACILITY PANEL ===
cat("=== STEP 1: LOAD ===\n")
.hdr <- names(fread(file.path(ANALYSIS_DIR, FAC_FILE), nrows = 0L))
cat("capacity-related cols in .hdr:\n")
print(grep("capac|tank_change|total_cap", .hdr, value = TRUE, ignore.case = TRUE))
.cap_chg <- if ("capacity_change"      %in% .hdr) "capacity_change" else
            if ("capacity_change_year" %in% .hdr) "capacity_change_year" else NULL
.cap_lev <- if ("total_capacity_dec" %in% .hdr) "total_capacity_dec" else
            if ("total_capacity"     %in% .hdr) "total_capacity" else NULL
if (is.null(.cap_chg) || is.null(.cap_lev))
  stop("capacity_change / total_capacity_dec absent from matched_facs_birth_cem.csv — carry them\n through facility_panel in 02b (built at 02b:1135 / 02b:995) and rebuild the matched panel.")
cat(sprintf("  resolved: cap_change='%s'  cap_level='%s'\n", .cap_chg, .cap_lev))
keep <- intersect(c("panel_id","state","panel_year","texas_treated","cem_weight",
  "n_tanks_active","n_closures","any_closure","facility_exit",
  "n_closures_permanent","n_closures_replacement","net_tank_change","capacity_decreased",
  "make_model_fac","fac_vintage","total_capacity_reform","n_tanks_at_reform",
  "has_gasoline","facility_id", MAND,
  .cap_chg, .cap_lev), .hdr)
fy <- fread(file.path(ANALYSIS_DIR, FAC_FILE), select = keep,
            colClasses = list(character = c("panel_id","state","facility_id")))
mand_have <- intersect(MAND, names(fy))
REL_THRESH <- 0.05   # 5% capacity band; flip to 0.10 here to retest

# Refactored margin construction (Step 1 pseudocode)
fy <- build_margins(fy)

# DCM-aligned size and vintage (per-sample, not inside build_margins)
fy[, cap_G := factor(cut(total_capacity_reform, CAP_BREAKS, labels = CAP_LABS), levels = CAP_LABS)]
if ("fac_vintage" %in% names(fy)) fy[, vintage := factor(fac_vintage)]
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

# Control strings. rhs_feonly = mandates only (no Yhat0), used for C2/C3 compare.
RHS        <- paste("+", paste(c("Yhat0", mand_have), collapse = " + "))
rhs_feonly <- paste("+", paste(mand_have, collapse = " + "))
FE         <- "panel_id + cell_fac_year"

# === STEP 3 — ROUTE A STATIC ATT (per margin) + ROUTE B ===
cat("=== STEP 3: ROUTE A STATIC ATT ===\n")
outs <- intersect(c("closure_share","any_closure","facility_exit","downsize",
                    "consolidate","reconfigure_up",
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
mB  <- feols(tau ~ did_term | panel_id + panel_year, data = fy, cluster = ~state)
ctB <- coeftable(mB); eB <- ctB["did_term","Estimate"]; sB <- ctB["did_term","Std. Error"]
att[["B_tau"]] <- data.table(outcome = "closure_share (route B: tau)", beta = eB, se = sB,
  ci_lo = eB-1.96*sB, ci_hi = eB+1.96*sB, p = ctB["did_term","Pr(>|t|)"], n = mB$nobs)
cat(sprintf("  [B] %-14s did=%+.4f (SE %.4f, p %.3f)\n", "tau", eB, sB, ctB["did_term","Pr(>|t|)"]))
fwrite(rbindlist(att), file.path(OUTPUT_TABLES, "T_Facility_Portfolio_ATT.csv"))

ATT_HEAD <- c(closure_share="Closure share", any_closure="Any closure", facility_exit="Facility exit",
              downsize="Downsize (fewer tanks & gallons)", consolidate="Consolidate (fewer tanks, ~same gal)",
              reconfigure_up="Reconfigure-up (fewer tanks, +gal)",
              perm_share="Permanent", repl_share="Replacement", cap_decrease="Capacity cut")
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
MARG <- intersect(c("closure_share","facility_exit","downsize","consolidate","repl_share"), names(fy))
hte_bin <- function(yv, z) {
  d <- fy[!is.na(get(z))]
  if (z == "thin_market" && "gas_station" %in% names(d)) d <- d[gas_station == 1L]
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
SZM <- intersect(c("closure_share","facility_exit","downsize","consolidate","reconfigure_up","repl_share","cap_decrease"), names(fy))
mS <- setNames(lapply(SZM, function(yv) cat_hte(yv, "cap_G", CAP_LABS[1])), SZM)
pub_etable(mS, file.path(OUTPUT_TABLES, "T_Facility_SizeHTE_Pub.tex"),
  headers = unname(ATT_HEAD[names(mS)]),
  title   = "Size heterogeneity (DCM total-capacity bins) by margin (facility-year)",
  notes   = "Reform x Post = smallest bin (G1, total < 9k gal); cap_G rows = differential for larger bins (9-20k / 20-30k / 30k+ total gallons). Facility + portfolio-mix x year FE. SE clustered by state.")
cat(sprintf("  size HTE: %d margins\n", length(mS)))

cat_hte_coefs <- function(model_list, dimension_label) {
  ref_level <- if (dimension_label == "cap_G") CAP_LABS[1] else VREF
  rows <- list()
  for (nm in names(model_list)) {
    m  <- model_list[[nm]]
    ct <- as.data.table(coeftable(m), keep.rownames = "term")
    if (nm == names(model_list)[1L])
      cat(sprintf("  [%s dim=%s] head(term): %s\n", nm, dimension_label,
                  paste(head(ct$term), collapse = " | ")))
    ref_row  <- ct[term == "did_term"]
    int_rows <- ct[grepl(paste0(dimension_label, "::"), term, fixed = TRUE)]
    stopifnot(nrow(ref_row) == 1L)
    lvls <- sub(":did_term$", "",
                sub(paste0("^", dimension_label, "::"), "", int_rows$term))
    out <- rbind(
      data.table(margin = nm, dimension = dimension_label, level = ref_level,
                 is_reference = TRUE,
                 estimate  = ref_row$Estimate,
                 std_error = ref_row[["Std. Error"]],
                 p_value   = ref_row[["Pr(>|t|)"]]),
      if (nrow(int_rows) > 0L)
        data.table(margin = nm, dimension = dimension_label, level = lvls,
                   is_reference = FALSE,
                   estimate  = int_rows$Estimate,
                   std_error = int_rows[["Std. Error"]],
                   p_value   = int_rows[["Pr(>|t|)"]])
    )
    rows[[nm]] <- out
  }
  rbindlist(rows)
}
sz_csv <- cat_hte_coefs(mS, "cap_G")
stopifnot(all(is.finite(sz_csv$estimate)))
fwrite(sz_csv, file.path(OUTPUT_TABLES, "T_Facility_SizeHTE_byMargin.csv"))
cat(sprintf("  T_Facility_SizeHTE_byMargin.csv: %d rows\n", nrow(sz_csv)))

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
  vint_csv <- cat_hte_coefs(mV, "vintage")
  stopifnot(all(is.finite(vint_csv$estimate)))
  fwrite(vint_csv, file.path(OUTPUT_TABLES, "T_Facility_VintageHTE_byMargin.csv"))
  cat(sprintf("  T_Facility_VintageHTE_byMargin.csv: %d rows\n", nrow(vint_csv)))
} else cat("  vintage var/ref absent -> vintage HTE skipped.\n")

# === STEP 6 — CAUSAL EVENT STUDIES on closure / downsize / replace ===
cat("=== STEP 6: EVENT STUDIES (closure / downsize / replace) ===\n")
fes <- fy[rel_year %between% ES_WIN]   # kept for .n_reconf_post check below
.n_reconf_post <- fes[texas_treated == 1L & rel_year >= 0L, sum(reconfigure_up)]
cat(sprintf("  reconfigure_up treated post events (ES window): %d\n", .n_reconf_post))
es_all <- rbindlist(list(
  es_one("closure_share",  "Effect on facility closure share",     "Fig_ES_Facility_Portfolio"),
  if ("downsize"    %in% names(fy)) es_one("downsize",    "Effect on facility downsize",      "Fig_ES_Facility_Downsize"),
  if ("consolidate" %in% names(fy)) es_one("consolidate", "Effect on facility consolidation", "Fig_ES_Facility_Consolidate"),
  if ("repl_share"  %in% names(fy)) es_one("repl_share",  "Effect on facility replacement",   "Fig_ES_Facility_Replace"),
  if ("reconfigure_up" %in% names(fy) && .n_reconf_post >= 200L)
    es_one("reconfigure_up", "Effect on facility reconfigure-up",  "Fig_ES_Facility_ReconfigureUp")
), use.names = TRUE)
fwrite(es_all, file.path(OUTPUT_TABLES, "T_Facility_ES_Coefs_byMargin.csv"))

# Matched-panel FE-only closure ES (NO Yhat0) — the EXACT facility mirror of the tank ES
# (Fig_ES_Full / Fig_ES_HTE_Pooled). Same matched sample (fy) and same panel_id + cell_fac_year
# FE as the headline, but Yhat0 dropped so it (a) tosses no facility-years (no 68% Yhat0 coverage
# cut) and (b) is the tank ES spec verbatim: closure_share ~ i(rel_year, texas_treated) + mandates.
es_one("closure_share", "Effect on facility closure share",
       "Fig_ES_Facility_Portfolio_noYhat0",
       data = fy, rhs = rhs_feonly, fe = FE)

# ═══════════════════════════════════════════════════════════════════════════════
# TICKET 035 — SAMPLE ROBUSTNESS: matched birth-CEM vs ALL incumbents
# Three configs: C1=matched_full (FE+Yhat0), C2=matched_feonly, C3=allinc_feonly.
# C1 outputs above are UNCHANGED (reference). Steps 7-11 are additive.
# ═══════════════════════════════════════════════════════════════════════════════

# === STEP 7 — LOAD ALL-INCUMBENTS SAMPLE ===
cat("=== STEP 7: LOAD ALL-INCUMBENTS SAMPLE ===\n")
stopifnot("facility_panel.csv not found — pull from server" = file.exists(FAC_PANEL))
.fp_hdr <- names(fread(FAC_PANEL, nrows = 0L))
allinc_keep <- intersect(c(
  "panel_id", "state", "panel_year", "texas_treated",
  "fac_is_incumbent", "n_tanks_at_reform",
  "n_tanks_active", "n_closures", "any_closure", "facility_exit",
  "n_closures_permanent", "n_closures_replacement", "net_tank_change", "capacity_decreased",
  "make_model_fac", "fac_vintage", "total_capacity_reform",
  "has_gasoline", "facility_id", MAND,
  "capacity_change", "total_capacity_dec"
), .fp_hdr)
fy_allinc <- fread(FAC_PANEL, select = allinc_keep,
                   colClasses = list(character = c("panel_id", "state", "facility_id")))

# Incumbent filter: active-at-reform (I1; OR logic per spec)
fy_allinc <- fy_allinc[fac_is_incumbent == 1L | n_tanks_at_reform > 0L]
cat(sprintf("  allinc raw: %s facility-years | %s facilities\n",
            fmt_n(nrow(fy_allinc)), fmt_n(uniqueN(fy_allinc$panel_id))))

fy_allinc <- build_margins(fy_allinc)   # same margins as matched (I1)

# Per-sample cap_G and vintage (I5)
fy_allinc[, cap_G := factor(cut(total_capacity_reform, CAP_BREAKS, labels = CAP_LABS), levels = CAP_LABS)]
if ("fac_vintage" %in% names(fy_allinc)) fy_allinc[, vintage := factor(fac_vintage)]

# GIS join for allinc (~93% coverage, same file keyed on panel_id)
if (HAS_GIS) {
  gv_ai <- fread(GIS_HTE, select = c("panel_id", "rural_2000", "low_pop_density",
                 "med_hh_income_2000", "pct_poverty_2000"),
                 colClasses = list(character = "panel_id"))
  fy_allinc <- merge(fy_allinc, gv_ai, by = "panel_id", all.x = TRUE)
  fy_allinc[, rural   := rural_2000]
  fy_allinc[, low_pop := low_pop_density]
  cat(sprintf("  allinc GIS: rural %.1f%%\n", 100 * mean(!is.na(fy_allinc$rural))))
}

mand_allinc       <- intersect(MAND, names(fy_allinc))
rhs_feonly_allinc <- paste("+", paste(mand_allinc, collapse = " + "))

cat(sprintf("  allinc post-margins: %s facility-years | %s facilities | TX share %.3f\n",
            fmt_n(nrow(fy_allinc)), fmt_n(uniqueN(fy_allinc$panel_id)), mean(fy_allinc$texas_treated)))
cat(sprintf("  allinc margins: closure=%.4f exit=%.4f downsize=%.4f\n",
            mean(fy_allinc$closure_share), mean(fy_allinc$facility_exit, na.rm=TRUE),
            mean(fy_allinc$downsize)))

# === STEP 8 — COMPARISON ATT (three configs, 7 MARG_ALL margins) ===
cat("=== STEP 8: COMPARISON ATT ===\n")
cmp <- rbind(
  att_table(fy,        RHS           )[, `:=`(config = "matched_full",   sample = "matched", spec = "FE+Yhat0")],
  att_table(fy,        rhs_feonly    )[, `:=`(config = "matched_feonly", sample = "matched", spec = "FE-only")],
  att_table(fy_allinc, rhs_feonly_allinc)[, `:=`(config = "allinc_feonly", sample = "allinc", spec = "FE-only")]
)
setcolorder(cmp, c("margin", "config", "sample", "spec", "beta", "se", "p", "n"))
stopifnot(nrow(cmp) == 3L * length(intersect(MARG_ALL, c(names(fy), names(fy_allinc)))))
stopifnot(all(is.finite(cmp$beta)), all(is.finite(cmp$se)))
fwrite(cmp, file.path(OUTPUT_TABLES, "T_Facility_SampleCompare_ATT.csv"))
cat(sprintf("  T_Facility_SampleCompare_ATT.csv: %d rows (%d configs x margins)\n",
            nrow(cmp), uniqueN(cmp$config)))

# === STEP 9 — SIZE/VINTAGE HTE COMPARE (C2 matched_feonly vs C3 allinc_feonly) ===
cat("=== STEP 9: HTE COMPARE (size + vintage, C2 vs C3) ===\n")
SZM_CMP <- intersect(MARG_ALL, intersect(names(fy), names(fy_allinc)))

# Size HTE compare
sz_c2 <- cat_hte_coefs(
  setNames(lapply(SZM_CMP, function(yv) cat_hte_param(fy,        yv, "cap_G", CAP_LABS[1], rhs_feonly)),       SZM_CMP),
  "cap_G")
sz_c2[, config := "matched_feonly"]
sz_c3 <- cat_hte_coefs(
  setNames(lapply(SZM_CMP, function(yv) cat_hte_param(fy_allinc, yv, "cap_G", CAP_LABS[1], rhs_feonly_allinc)), SZM_CMP),
  "cap_G")
sz_c3[, config := "allinc_feonly"]
sz_cmp <- rbind(sz_c2, sz_c3)
stopifnot(all(is.finite(sz_cmp$estimate)))
fwrite(sz_cmp, file.path(OUTPUT_TABLES, "T_Facility_SampleCompare_SizeHTE.csv"))
cat(sprintf("  T_Facility_SampleCompare_SizeHTE.csv: %d rows\n", nrow(sz_cmp)))

# Vintage HTE compare (guarded: need VREF level in both samples)
has_vint_matched <- "vintage" %in% names(fy)       && VREF %in% levels(fy$vintage)
has_vint_allinc  <- "vintage" %in% names(fy_allinc) && VREF %in% levels(fy_allinc$vintage)
if (has_vint_matched && has_vint_allinc) {
  vt_c2 <- cat_hte_coefs(
    setNames(lapply(SZM_CMP, function(yv) cat_hte_param(fy,        yv, "vintage", VREF, rhs_feonly)),       SZM_CMP),
    "vintage")
  vt_c2[, config := "matched_feonly"]
  vt_c3 <- cat_hte_coefs(
    setNames(lapply(SZM_CMP, function(yv) cat_hte_param(fy_allinc, yv, "vintage", VREF, rhs_feonly_allinc)), SZM_CMP),
    "vintage")
  vt_c3[, config := "allinc_feonly"]
  vt_cmp <- rbind(vt_c2, vt_c3)
  stopifnot(all(is.finite(vt_cmp$estimate)))
  fwrite(vt_cmp, file.path(OUTPUT_TABLES, "T_Facility_SampleCompare_VintageHTE.csv"))
  cat(sprintf("  T_Facility_SampleCompare_VintageHTE.csv: %d rows\n", nrow(vt_cmp)))
} else {
  cat(sprintf("  vintage compare skipped: matched=%s allinc=%s\n", has_vint_matched, has_vint_allinc))
}

# === STEP 10 — EVENT STUDIES: all-incumbents sample (FE-only) ===
cat("=== STEP 10: ALLINC EVENT STUDIES (FE-only) ===\n")
es_one("closure_share", "Effect on facility closure share",
       "Fig_ES_Facility_Portfolio_allinc",
       data = fy_allinc, rhs = rhs_feonly_allinc, fe = FE)
if ("downsize" %in% names(fy_allinc))
  es_one("downsize", "Effect on facility downsize",
         "Fig_ES_Facility_Downsize_allinc",
         data = fy_allinc, rhs = rhs_feonly_allinc, fe = FE)
if ("consolidate" %in% names(fy_allinc))
  es_one("consolidate", "Effect on facility consolidation",
         "Fig_ES_Facility_Consolidate_allinc",
         data = fy_allinc, rhs = rhs_feonly_allinc, fe = FE)

# === STEP 11 — CONSOLE SUMMARY ===
cat("=== STEP 11: CONSOLE SUMMARY ===\n")
cat(sprintf("%-20s  %-22s  %-22s  %-22s\n",
            "margin", "matched_full", "matched_feonly", "allinc_feonly"))
for (mg in MARG_ALL) {
  r <- cmp[margin == mg]
  if (nrow(r) == 0L) next
  fmt_one <- function(cfg) {
    rr <- r[config == cfg]
    if (nrow(rr) == 0L) return("      --      ")
    sprintf("%+.4f (p%.3f)", rr$beta, rr$p)
  }
  cat(sprintf("%-20s  %-22s  %-22s  %-22s\n",
              mg, fmt_one("matched_full"), fmt_one("matched_feonly"), fmt_one("allinc_feonly")))
}

cat("\n=== 02j COMPLETE ===\n")
