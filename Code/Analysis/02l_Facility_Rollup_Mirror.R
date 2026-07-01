# =============================================================================
# Code/Analysis/02l_Facility_Rollup_Mirror.R
# Facility-level portfolio analysis: faithful roll-up of matched tank analysis.
# Ticket 038. Do NOT edit 02b, 02j, or 02k.
# =============================================================================

SCRIPT_NAME <- "02l_Facility_Rollup_Mirror"

.log_path <- here::here("logs", paste0(SCRIPT_NAME, "_",
                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output")
sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, SCRIPT_NAME, R.version.string, getwd()))

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(here)
})
source(here("Code", "Helpers", "reduced_form_utils.R"))
rf_use_threads()

ANALYSIS_DIR <- here("Data", "Analysis")
REL_THRESH   <- 0.05
ES_WIN       <- c(-13L, 22L)
CAP_BREAKS   <- c(-Inf, 9000, 20000, 30000, Inf)
CAP_LABS     <- c("G1_lt9k", "G2_9to20k", "G3_20to30k", "G4_gt30k")
VREF         <- "1989-1998"
MAND_RHS     <- "+ mandate_release_det + mandate_spill_overfill + mandate_integrity"
MARGINS      <- c("any_closure", "facility_exit", "downsize", "consolidate",
                  "reconfigure_up", "any_replace", "cap_decrease")
ES_MARGINS   <- c("any_closure", "downsize", "consolidate", "any_replace")
HTE_MARGINS  <- c("any_closure", "facility_exit", "downsize", "consolidate", "any_replace")
GIS_PATH     <- here("Output", "GIS", "gis_hte_vars.csv")

ES_LABELS <- c(
  any_closure    = "P(any closure | facility-year)",
  downsize       = "P(downsize | facility-year)",
  consolidate    = "P(consolidate | facility-year)",
  any_replace    = "P(any replacement | facility-year)",
  reconfigure_up = "P(reconfigure-up | facility-year)"
)

# --- ES helper: ported verbatim from 02j:140-172 ---
es_one_02l <- function(yv, ylab, stem, data, rhs, fe) {
  fes_loc <- data[rel_year %between% ES_WIN]
  m <- feols(
    as.formula(sprintf("%s ~ i(rel_year, texas_treated, ref = -1L) %s | %s", yv, rhs, fe)),
    data = fes_loc, cluster = ~state
  )
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[
    grepl("rel_year::", term, fixed = TRUE)]
  ct[, rel_year := as.integer(
    tstrsplit(sub("rel_year::", "", term, fixed = TRUE), ":", fixed = TRUE)[[1]])]
  co <- ct[, .(rel_year, estimate = Estimate, se = `Std. Error`)]
  co <- rbind(co, data.table(rel_year = -1L, estimate = 0, se = 0))
  co[, `:=`(ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
            margin = yv,
            period = factor(fifelse(rel_year < 0L, "pre",
                            fifelse(rel_year == 0L, "event", "post")),
                            levels = c("pre", "event", "post")))]
  setorder(co, rel_year)
  p <- ggplot(co, aes(rel_year, estimate, colour = period)) +
    geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed", linewidth = 0.45) +
    geom_vline(xintercept = -1.5, colour = "grey40", linetype = "dotted", linewidth = 0.5) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.25, linewidth = 0.5) +
    geom_point(size = 2.0, shape = 21, fill = "white", stroke = 1.2) +
    scale_colour_manual(
      values = c(pre = "#3A6BBF", event = "#888888", post = "#BF3A3A"),
      labels = c(pre = "Pre-treatment", event = "Event", post = "Post-treatment"),
      name   = NULL) +
    scale_x_continuous(breaks = seq(-14, 22, by = 2)) +
    labs(x = "Years relative to Dec 22 1998", y = ylab) +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(legend.position     = "bottom",
          legend.key.width    = unit(1.2, "cm"),
          axis.line           = element_line(colour = "black", linewidth = 0.4),
          axis.ticks          = element_line(colour = "black", linewidth = 0.3),
          axis.text           = element_text(colour = "black"),
          panel.grid.major.y  = element_line(colour = "grey92", linewidth = 0.3),
          panel.grid.minor    = element_blank(),
          plot.margin         = margin(8, 12, 8, 8))
  save_gg(p, stem, width = 7.6, height = 4.8)
  pt <- co[rel_year < -1L,
           .(max_abs_pre = max(abs(estimate)),
             max_t       = max(abs(estimate / pmax(se, 1e-12))))]
  cat(sprintf("  %-18s -> %s | PRE max|coef|=%.4f max|t|=%.2f\n",
      yv, stem, pt$max_abs_pre, pt$max_t))
  co[, is_reference := (rel_year == -1L)]
  co[, .(margin, rel_year, estimate, se, ci_lo, ci_hi, is_reference)]
}

# --- HTE coeftable → long rows: ported from 02j:337-367 with explicit ref_label ---
hte_long <- function(model_list, dimension_label, ref_label) {
  rows <- list()
  for (nm in names(model_list)) {
    m        <- model_list[[nm]]
    ct       <- as.data.table(coeftable(m), keep.rownames = "term")
    ref_row  <- ct[term == "did_term"]
    int_rows <- ct[grepl(paste0(dimension_label, "::"), term, fixed = TRUE)]
    stopifnot(nrow(ref_row) == 1L)
    lvls <- sub(":did_term$", "",
                sub(paste0("^", dimension_label, "::"), "", int_rows$term))
    out <- rbind(
      data.table(margin       = nm,
                 dimension    = dimension_label,
                 level        = ref_label,
                 is_reference = TRUE,
                 estimate     = ref_row$Estimate,
                 std_error    = ref_row[["Std. Error"]],
                 p_value      = ref_row[["Pr(>|t|)"]]),
      if (nrow(int_rows) > 0L)
        data.table(margin       = nm,
                   dimension    = dimension_label,
                   level        = lvls,
                   is_reference = FALSE,
                   estimate     = int_rows$Estimate,
                   std_error    = int_rows[["Std. Error"]],
                   p_value      = int_rows[["Pr(>|t|)"]])
    )
    rows[[nm]] <- out
  }
  rbindlist(rows)
}

# =============================================================================
# SECTION 1 — RECONSTRUCT MATCHED TANK SAMPLE (mirror 02b:3965-3988)
# =============================================================================
cat("=== SECTION 1: RECONSTRUCT MATCHED TANK SAMPLE ===\n")

matched_tanks_birth_cem <- fread(
  file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv"), na.strings = c("", "NA"))
matched_tanks_birth_cem <- matched_tanks_birth_cem[
  is.na(first_year_churn) | first_year_churn == 0L]

data_C <- matched_tanks_birth_cem[install_yr_int < 1999L & cem_weight > 0]

tanks_open_1998        <- data_C[panel_year == 1998L & closure_event == 0L, unique(panel_id)]
tanks_installed_1998   <- data_C[install_yr_int == 1998L, unique(panel_id)]
facilities_active_1998 <- union(tanks_open_1998, tanks_installed_1998)
data_C_active          <- data_C[panel_id %in% facilities_active_1998]

cat(sprintf("  data_C_active: nrow=%s | facilities=%s | tanks=%s | TX_facilities=%s\n",
    format(nrow(data_C_active),                           big.mark = ","),
    format(uniqueN(data_C_active$panel_id),               big.mark = ","),
    format(uniqueN(data_C_active$tank_panel_id),          big.mark = ","),
    format(uniqueN(data_C_active[texas_treated == 1L]$panel_id), big.mark = ",")))

# =============================================================================
# SECTION 2 — FACILITY-YEAR SKELETON + CLOSURE OUTCOME (I4)
# =============================================================================
cat("=== SECTION 2: FACILITY-YEAR SKELETON ===\n")

# Roll the matched tanks up to facility-year. Closure outcome = any_closure (I4).
# Mandates are TANK-level (tank-specific UST upgrade-compliance timing), so they vary
# across tanks within a facility-year -> aggregate to the facility-year MEAN = the
# share of the facility's tanks subject to each mandate that year (a valid [0,1] control).
# texas_treated and state ARE facility-constant -> take the first value and assert it.
fy <- data_C_active[, .(
  any_closure            = as.integer(sum(closure_event, na.rm = TRUE) > 0),
  n_tanks_fy             = .N,
  n_tanks_fy_unique      = uniqueN(tank_panel_id),
  mandate_release_det    = mean(mandate_release_det,    na.rm = TRUE),
  mandate_spill_overfill = mean(mandate_spill_overfill, na.rm = TRUE),
  mandate_integrity      = mean(mandate_integrity,      na.rm = TRUE),
  texas_treated          = texas_treated[1L],
  state                  = state[1L],
  nu_tt                  = uniqueN(texas_treated),
  nu_st                  = uniqueN(state)
), by = .(panel_id, panel_year)]
stopifnot(all(fy$nu_tt == 1L), all(fy$nu_st == 1L))   # treatment & state ARE facility-constant
fy[, c("nu_tt", "nu_st") := NULL]
fy[, did_term := texas_treated * as.integer(panel_year >= 1999L)]
fy[, rel_year := as.integer(panel_year) - 1998L]
cat(sprintf("  mandates -> facility-year mean shares: rel=%.3f spill=%.3f integ=%.3f\n",
    mean(fy$mandate_release_det), mean(fy$mandate_spill_overfill), mean(fy$mandate_integrity)))

cat(sprintf("  fy skeleton: %s facility-years | %s facilities | %d states\n",
    format(nrow(fy), big.mark = ","),
    format(uniqueN(fy$panel_id), big.mark = ","),
    uniqueN(fy$state)))

# =============================================================================
# SECTION 3 — COMPOSITION FE FROM MATCHED ROSTER (I7)
# =============================================================================
cat("=== SECTION 3: COMPOSITION FE ===\n")

tc <- unique(data_C_active[, .(panel_id, tank_panel_id, make_model_noage, install_yr_int)],
             by = "tank_panel_id")
tc[, cell := paste(make_model_noage, install_yr_int, sep = "@")]   # oldest-tank cell = make/model (build) x install year (vintage)

# Per-facility keys anchored on the OLDEST tank (earliest install_yr; tie-break min make_model).
# Two FE specs (both keep exact make_model x install_yr cohort cells, no coarsening):
#   oldtank_size   (HEADLINE) = oldest tank's cell x station size bin  -> keeps ~99.5%
#   full_portfolio (ROBUST)   = the station's full tank lineup         -> ~58% (strictest; no size in FE)
setorder(tc, panel_id, install_yr_int, make_model_noage)
k_old <- tc[, .(oldest = cell[1L], n_tanks = .N), by = panel_id]
k_cmp <- tc[, .(full = paste(sort(cell), collapse = "|")), by = panel_id]
K <- k_old[k_cmp, on = "panel_id"]
K[, size_bin := cut(n_tanks, c(0, 1, 3, 6, Inf), labels = c("1", "2_3", "4_6", "7p"))]
K[, comp_oldtank_size   := paste(oldest, as.character(size_bin), sep = "||")]
K[, comp_full_portfolio := full]

fy <- K[, .(panel_id, comp_oldtank_size, comp_full_portfolio)][fy, on = "panel_id"]
fy[, cell_comp_year_fe_oldtank_size   := .GRP, by = .(panel_year, comp_oldtank_size)]
fy[, cell_comp_year_fe_full_portfolio := .GRP, by = .(panel_year, comp_full_portfolio)]

for (v in c("oldtank_size", "full_portfolio")) {
  comp_col <- paste0("comp_", v)
  fe_col   <- paste0("cell_comp_year_fe_", v)
  gsz      <- fy[, .(gsz = .N), by = c("panel_year", comp_col)]
  n_sing   <- sum(gsz$gsz == 1L)
  cat(sprintf("  FE %-16s: compositions=%d | comp*year cells=%d | singleton FY=%d (%.1f%%)\n",
      v, uniqueN(fy[[comp_col]]), uniqueN(fy[[fe_col]]), n_sing, 100 * n_sing / nrow(fy)))
}

# =============================================================================
# SECTION 4 — PORTFOLIO MARGINS FROM FACILITY_PANEL.CSV (I5)
# =============================================================================
cat("=== SECTION 4: PORTFOLIO MARGINS ===\n")

fp_cols <- c("panel_id", "panel_year", "facility_exit", "net_tank_change",
             "capacity_change", "total_capacity_dec", "n_closures_replacement",
             "capacity_decreased", "total_capacity_reform", "fac_vintage", "has_gasoline")
fp <- fread(file.path(ANALYSIS_DIR, "facility_panel.csv"),
            select = fp_cols, na.strings = c("", "NA"))

# Verbatim port from 02j:96-115 (REL_THRESH = 0.05)
fp[, dC   := capacity_change]
fp[, Clag := total_capacity_dec - dC]
fp[, rel_cap     := fifelse(Clag > 0, dC / Clag, NA_real_)]
fp[, contraction := as.integer(
  facility_exit == 0L & net_tank_change < 0L & Clag > 0 & !is.na(dC))]
fp[, consolidate    := as.integer(contraction == 1L & abs(rel_cap) <= REL_THRESH)]
fp[, downsize       := as.integer(contraction == 1L & rel_cap <  -REL_THRESH)]
fp[, reconfigure_up := as.integer(contraction == 1L & rel_cap >   REL_THRESH)]
fp[is.na(consolidate),    consolidate    := 0L]
fp[is.na(downsize),       downsize       := 0L]
fp[is.na(reconfigure_up), reconfigure_up := 0L]

fp[, any_replace := as.integer(n_closures_replacement > 0L)]
fp[is.na(any_replace), any_replace := 0L]
fp[, cap_decrease := as.integer(capacity_decreased)]
fp[is.na(cap_decrease), cap_decrease := 0L]

stopifnot(fp[, max(consolidate + downsize + reconfigure_up)] <= 1L)
stopifnot(fp[facility_exit == 1L,   sum(consolidate + downsize + reconfigure_up)] == 0L)
stopifnot(fp[net_tank_change >= 0L, sum(consolidate + downsize + reconfigure_up)] == 0L)
stopifnot(fp[is.na(dC),             sum(consolidate + downsize + reconfigure_up)] == 0L)
cat("  Partition mutually exclusive PASSED\n")

n_elig <- fp[facility_exit == 0L & net_tank_change < 0L & Clag > 0 & !is.na(dC), .N]
cat(sprintf("  contraction partition: n_eligible=%d | consolidate=%.4f | downsize=%.4f | reconfigure_up=%.4f\n",
    n_elig, mean(fp$consolidate), mean(fp$downsize), mean(fp$reconfigure_up)))

fp_margins <- fp[, .(panel_id, panel_year, facility_exit, downsize, consolidate,
                     reconfigure_up, any_replace, cap_decrease,
                     total_capacity_reform, fac_vintage, has_gasoline)]
fy <- merge(fy, fp_margins, by = c("panel_id", "panel_year"), all.x = TRUE)

n_no_match <- fy[is.na(facility_exit), .N]
cat(sprintf("  Facility-years with no facility_panel match: %d\n", n_no_match))
for (mg in c("facility_exit", "downsize", "consolidate", "reconfigure_up",
             "any_replace", "cap_decrease")) {
  cat(sprintf("  %-18s: n_nonNA=%d | base_rate=%.4f\n",
      mg, fy[!is.na(get(mg)), .N], mean(fy[[mg]], na.rm = TRUE)))
}
cat(sprintf("  %-18s: n_nonNA=%d | base_rate=%.4f\n",
    "any_closure", nrow(fy), mean(fy$any_closure, na.rm = TRUE)))

fy[, cap_G   := factor(cut(total_capacity_reform, CAP_BREAKS, labels = CAP_LABS),
                       levels = CAP_LABS)]
fy[, vintage := factor(fac_vintage)]

# =============================================================================
# SECTION 5 — STATIC DiD (7 margins x {A,B})
# =============================================================================
cat("=== SECTION 5: STATIC DiD ===\n")
models_static <- list()
att_rows      <- list()

for (v in c("oldtank_size", "full_portfolio")) {
  fe_str <- paste0("panel_id + cell_comp_year_fe_", v)
  cat(sprintf("[%s] version %s\n", format(Sys.time(), "%H:%M:%S"), v))
  for (mg in MARGINS) {
    dat <- fy[!is.na(get(mg))]
    fml <- as.formula(sprintf(
      "%s ~ did_term + mandate_release_det + mandate_spill_overfill + mandate_integrity | %s",
      mg, fe_str))
    mod <- feols(fml, data = dat, cluster = ~state)
    key <- paste(mg, v)
    models_static[[key]] <- mod
    ct <- coeftable(mod)
    att_rows[[key]] <- data.table(
      margin              = mg,
      fe_version          = v,
      beta                = ct["did_term", "Estimate"],
      se                  = ct["did_term", "Std. Error"],
      p_value             = ct["did_term", "Pr(>|t|)"],
      n_obs               = as.integer(mod$nobs),
      n_fac               = as.integer(mod$fixef_sizes["panel_id"]),
      n_singleton_dropped = as.integer(nrow(dat) - mod$nobs)
    )
    cat(sprintf("  [%s] %-18s|%s  beta=%+.4f se=%.4f p=%.3f n=%d\n",
        format(Sys.time(), "%H:%M:%S"), mg, v,
        att_rows[[key]]$beta, att_rows[[key]]$se, att_rows[[key]]$p_value, mod$nobs))
  }
}

att_dt <- rbindlist(att_rows)
stopifnot(nrow(att_dt) == length(MARGINS) * 2L)   # 7 margins x 2 FE specs (oldtank_size, full_portfolio) = 14
fwrite(att_dt, file.path(OUTPUT_TABLES, "T_Facility_Rollup_ATT.csv"))
cat(sprintf("  T_Facility_Rollup_ATT.csv: %d rows\n", nrow(att_dt)))

AC_NOTES <- paste0(
  "Primary closure outcome. \\textit{any\\_closure} $=1$ if the station had at least one",
  " matched tank close in that facility-year; $=0$ otherwise.",
  " Matched tank sample (birth-CEM, unweighted). SE clustered by state.")

for (v in c("oldtank_size", "full_portfolio")) {
  fe_str <- paste0("panel_id + cell_comp_year_fe_", v)
  m1 <- feols(any_closure ~ did_term, data = fy, cluster = ~state)
  m2 <- feols(any_closure ~ did_term | panel_id, data = fy, cluster = ~state)
  m3 <- feols(any_closure ~ did_term + mandate_release_det + mandate_spill_overfill +
                mandate_integrity | panel_id, data = fy, cluster = ~state)
  m4 <- models_static[[paste("any_closure", v)]]
  pub_etable(list(M1 = m1, M2 = m2, M3 = m3, M4 = m4),
             file    = file.path(OUTPUT_TABLES,
                                 sprintf("T_DiD_Facility_Stepped_%s.tex", v)),
             title   = sprintf("Any closure: stepped DiD (facility-year, FE-%s)", v),
             notes   = AC_NOTES,
             headers = c("(1)", "(2)", "(3)", "(4)"))
}

for (v in c("oldtank_size", "full_portfolio")) {
  mods_v        <- models_static[paste(MARGINS, v)]
  names(mods_v) <- MARGINS
  pub_etable(mods_v,
             file  = file.path(OUTPUT_TABLES,
                               sprintf("T_Facility_Rollup_ATT_Pub_%s.tex", v)),
             title = sprintf("Portfolio margins: DiD estimates (FE version %s)", v),
             notes = paste0(
               "\\textit{any\\_closure}: station had $\\geq$1 matched tank close that year.",
               " Reform $\\times$ Post $=$ TX $\\times$ post-1998.",
               " FE: facility + portfolio-composition$\\times$year.",
               " SE clustered by state."))
}
cat("  Pub tables written (stepped A/B, all-margins A/B)\n")

# =============================================================================
# SECTION 6 — EVENT STUDIES + FIGURES
# =============================================================================
cat("=== SECTION 6: EVENT STUDIES ===\n")

.n_reconf_post <- fy[
  rel_year >= 0L & rel_year <= ES_WIN[2L] &
  texas_treated == 1L & !is.na(reconfigure_up),
  sum(reconfigure_up, na.rm = TRUE)]
cat(sprintf("  reconfigure_up treated post events in ES window: %d\n", .n_reconf_post))
run_reconf_es   <- (.n_reconf_post >= 200L)
ES_MARGINS_RUN  <- if (run_reconf_es) c(ES_MARGINS, "reconfigure_up") else ES_MARGINS
if (!run_reconf_es)
  cat("  reconfigure_up ES SKIPPED (< 200 treated post events)\n")

es_coef_list <- list()
for (v in c("oldtank_size", "full_portfolio")) {
  fe_str <- paste0("panel_id + cell_comp_year_fe_", v)
  cat(sprintf("[%s] ES version %s\n", format(Sys.time(), "%H:%M:%S"), v))
  for (mg in ES_MARGINS_RUN) {
    stem <- sprintf("Fig_ES_Facility_%s_%s", mg, v)
    co   <- es_one_02l(yv   = mg,
                       ylab = ES_LABELS[mg],
                       stem = stem,
                       data = fy[!is.na(get(mg))],
                       rhs  = MAND_RHS,
                       fe   = fe_str)
    co[, fe_version := v]
    es_coef_list[[paste(mg, v)]] <- co[, .(margin, fe_version, rel_year, estimate, se,
                                            ci_lo, ci_hi, is_reference)]
  }
}

es_coef_dt <- rbindlist(es_coef_list)
fwrite(es_coef_dt, file.path(OUTPUT_TABLES, "T_Facility_Rollup_ES_Coefs.csv"))
cat(sprintf("  T_Facility_Rollup_ES_Coefs.csv: %d rows\n", nrow(es_coef_dt)))

# =============================================================================
# SECTION 7 — HTE (FE-version A only; interaction-only; cluster ~state)
# =============================================================================
cat("=== SECTION 7: HTE (headline FE = oldtank_size) ===\n")
FE_HEAD <- "panel_id + cell_comp_year_fe_oldtank_size"

hte_fit_factor <- function(mg, fvar, ref_str) {
  d <- fy[!is.na(get(mg)) & !is.na(get(fvar))]
  feols(as.formula(sprintf(
    "%s ~ i(%s, did_term, ref = '%s') + did_term %s | %s",
    mg, fvar, ref_str, MAND_RHS, FE_HEAD)), data = d, cluster = ~state)
}

hte_fit_numeric <- function(mg, fvar, ref_val) {
  d <- fy[!is.na(get(mg)) & !is.na(get(fvar))]
  feols(as.formula(sprintf(
    "%s ~ i(%s, did_term, ref = %s) + did_term %s | %s",
    mg, fvar, as.character(ref_val), MAND_RHS, FE_HEAD)), data = d, cluster = ~state)
}

hte_rows <- list()

cat("  Size HTE...\n")
mS <- setNames(lapply(HTE_MARGINS, hte_fit_factor, fvar = "cap_G",   ref_str = CAP_LABS[1]), HTE_MARGINS)
hte_rows[["cap_G"]] <- hte_long(mS, "cap_G", CAP_LABS[1])

if ("vintage" %in% names(fy) && VREF %in% levels(fy$vintage)) {
  cat("  Vintage HTE...\n")
  mV <- setNames(lapply(HTE_MARGINS, hte_fit_factor, fvar = "vintage", ref_str = VREF), HTE_MARGINS)
  hte_rows[["vintage"]] <- hte_long(mV, "vintage", VREF)
} else {
  cat("  vintage var/ref absent -> vintage HTE skipped\n")
}

cat("  Fuel HTE...\n")
mF <- setNames(lapply(HTE_MARGINS, hte_fit_numeric, fvar = "has_gasoline", ref_val = 0L), HTE_MARGINS)
hte_rows[["has_gasoline"]] <- hte_long(mF, "has_gasoline", "0")

if (file.exists(GIS_PATH)) {
  cat("  Spatial HTE...\n")
  # gis_hte_vars schema: rural_2000, low_pop_density (binary); med_hh_income_2000,
  # pct_poverty_2000 (continuous -> median-split binaries). No thin_market in file.
  .gis_hdr  <- names(fread(GIS_PATH, nrows = 0))
  .gis_want <- intersect(c("panel_id", "rural_2000", "low_pop_density",
                           "med_hh_income_2000", "pct_poverty_2000"), .gis_hdr)
  gis <- fread(GIS_PATH, select = .gis_want, na.strings = c("", "NA"))
  if ("rural_2000"         %in% names(gis)) gis[, rural      := as.integer(rural_2000)]
  if ("low_pop_density"    %in% names(gis)) gis[, low_pop    := as.integer(low_pop_density)]
  if ("med_hh_income_2000" %in% names(gis)) gis[, low_income := as.integer(med_hh_income_2000 < median(med_hh_income_2000, na.rm = TRUE))]
  if ("pct_poverty_2000"   %in% names(gis)) gis[, high_pov   := as.integer(pct_poverty_2000  > median(pct_poverty_2000,  na.rm = TRUE))]
  SPATIAL_DIMS <- intersect(c("rural", "low_pop", "low_income", "high_pov"), names(gis))
  gis <- gis[, c("panel_id", SPATIAL_DIMS), with = FALSE]
  fy  <- gis[fy, on = "panel_id"]
  for (sv in SPATIAL_DIMS) {
    if (uniqueN(na.omit(fy[[sv]])) < 2L) {
      cat(sprintf("  spatial %s: no usable variation -- skipped\n", sv)); next
    }
    mSp <- setNames(
      lapply(HTE_MARGINS, hte_fit_numeric, fvar = sv, ref_val = 0L), HTE_MARGINS)
    hte_rows[[sv]] <- hte_long(mSp, sv, "0")
    cat(sprintf("  [%s] spatial %s done\n", format(Sys.time(), "%H:%M:%S"), sv))
  }
} else {
  cat("  GIS absent -- skipping spatial HTE (gis_hte_vars.csv not found)\n")
}

hte_dt <- rbindlist(hte_rows, fill = TRUE)
hte_dt[, fe_version := "oldtank_size"]
fwrite(hte_dt, file.path(OUTPUT_TABLES, "T_Facility_HTE.csv"))
cat(sprintf("  T_Facility_HTE.csv: %d rows\n", nrow(hte_dt)))

# =============================================================================
# SECTION 8 — DEFERRED BOOTSTRAP (only after Sections 5-7 have written files)
# =============================================================================
cat("=== SECTION 8: DEFERRED BOOTSTRAP ===\n")
bootstrap_ran <- FALSE

if (!requireNamespace("fwildclusterboot", quietly = TRUE)) {
  cat(paste0("WARNING: fwildclusterboot not installed --",
             " skipping deferred bootstrap; all other outputs already written\n"))
} else {
  boot_rows <- list()
  for (v in c("oldtank_size", "full_portfolio")) {
    cat(sprintf("[%s] bootstrap version %s\n", format(Sys.time(), "%H:%M:%S"), v))
    fe_col <- paste0("cell_comp_year_fe_", v)
    for (mg in MARGINS) {
      key <- paste(mg, v)
      # boottest cannot handle a fixest model that dropped FE singletons during estimation
      # (errors on $fixef_removed). Refit on a singleton-free sample: iteratively drop singleton
      # panel_id and comp-year cells until stable. Singletons contribute nothing to the FE
      # estimate, so the did coef matches the Section-5 model; this just gives boottest a clean object.
      dat_est <- fy[!is.na(get(mg))]
      repeat {
        n0 <- nrow(dat_est)
        dat_est[, .np := .N, by = panel_id];   dat_est <- dat_est[.np > 1L]
        dat_est[, .nf := .N, by = c(fe_col)];   dat_est <- dat_est[.nf > 1L]
        if (nrow(dat_est) == n0) break
      }
      dat_est[, c(".np", ".nf") := NULL]
      mod <- feols(as.formula(sprintf(
        "%s ~ did_term + mandate_release_det + mandate_spill_overfill + mandate_integrity | panel_id + %s",
        mg, fe_col)), data = dat_est, cluster = ~state)
      # Guard: a per-model boottest failure must not halt the whole deferred bootstrap.
      # It is LOGGED (not silently swallowed) and recorded as NA, then the loop continues.
      bt  <- tryCatch(
        fwildclusterboot::boottest(mod, param = "did_term", clustid = ~state,
                                   B = 9999L, type = "rademacher"),
        error = function(e) {
          cat(sprintf("  WARN boottest FAILED %s|%s: %s\n", mg, v, conditionMessage(e)))
          NULL })
      boot_rows[[key]] <- data.table(
        margin      = mg,
        fe_version  = v,
        beta        = coef(mod)["did_term"],
        analytic_se = se(mod)["did_term"],
        wcb_p_value = if (is.null(bt)) NA_real_ else bt$p_val,
        wcb_ci_lo   = if (is.null(bt)) NA_real_ else bt$conf_int[1L],
        wcb_ci_hi   = if (is.null(bt)) NA_real_ else bt$conf_int[2L],
        B           = 9999L,
        n_clusters  = uniqueN(dat_est$state)
      )
      cat(sprintf("  [%s] %-18s|%s  beta=%+.4f wcb_p=%s\n",
          format(Sys.time(), "%H:%M:%S"), mg, v, coef(mod)["did_term"],
          if (is.null(bt)) "FAILED" else sprintf("%.3f", bt$p_val)))
    }
  }
  boot_dt <- rbindlist(boot_rows)
  stopifnot(nrow(boot_dt) == length(MARGINS) * 2L)   # 7 x 3 = 21
  fwrite(boot_dt, file.path(OUTPUT_TABLES, "T_Facility_Bootstrap_SEs.csv"))
  cat(sprintf("  T_Facility_Bootstrap_SEs.csv: %d rows\n", nrow(boot_dt)))
  bootstrap_ran <- TRUE
}

# =============================================================================
# SECTION 9 — CONSOLE SUMMARY
# =============================================================================
cat("\n=== SECTION 9: SUMMARY ===\n")
cat("--- ATT: A vs B ---\n")
for (mg in MARGINS) {
  ra <- att_dt[margin == mg & fe_version == "A"]
  rb <- att_dt[margin == mg & fe_version == "B"]
  br <- mean(fy[[mg]], na.rm = TRUE)
  cat(sprintf("  %-18s  A: beta=%+.4f p=%.3f  B: beta=%+.4f p=%.3f  base=%.4f\n",
      mg, ra$beta, ra$p_value, rb$beta, rb$p_value, br))
}

cat("\n--- Composition cardinalities ---\n")
for (v in c("oldtank_size", "full_portfolio")) {
  comp_col <- paste0("comp_", v)
  fe_col   <- paste0("cell_comp_year_fe_", v)
  gsz      <- fy[, .(gsz = .N), by = c("panel_year", comp_col)]
  n_sing   <- sum(gsz$gsz == 1L)
  cat(sprintf("  FE-%s: %d compositions | %d comp*year cells | %d singleton FY\n",
      v, uniqueN(fy[[comp_col]]), uniqueN(fy[[fe_col]]), n_sing))
}

cat("\n--- ES figures ---\n")
for (v in c("oldtank_size", "full_portfolio")) for (mg in ES_MARGINS_RUN)
  cat(sprintf("  Fig_ES_Facility_%s_%s.{pdf,png}\n", mg, v))

cat(sprintf("\nBootstrap: %s\n",
    if (bootstrap_ran) "COMPLETE (T_Facility_Bootstrap_SEs.csv)"
    else "SKIPPED (fwildclusterboot not installed)"))
cat(sprintf("Log: %s\n", .log_path))
