#==============================================================================
# 02_DiD_Main_MakeModel.R
# Texas UST Insurance Reform — Causal DiD Estimates
#
# PREREQUISITES: Run 00_RunAll.R (or 01m_MakeModelSample.R) first.
#
# SECTIONS:
#   S1   Setup & Data Loading
#   S2   Helper Functions
#   S3   Sample Construction & Diagnostics
#   S4   Parallel Trends Validation
#   S5   Age-at-Treatment HTE (DiD, Event Study, Tables)
#   S6   Age-Bin HTE Coefficient Plot (Figure 8)
#   S7   Robustness -- Age-Group-Specific Year FEs
#   S8   Hypothesis Tests (Survivorship, H1, H3, H4)
#   S9   Youngest Subsample (Table 5 + Figure 7A)
#   S10  Oldest Subsample  (Table 6 + Figure 7B)
#   S11  Reported Leak DiD (Table 7)
#   S12  H4: Wall Type on Broader SW Sample (Table 9)
#   S13  H3: Age at Closure OLS (Table 8 + Figure H3)  [descriptive; see S8 for primary]
#   S14  Theory-Evidence Summary (Table 10)
#   S15  Robustness (Tables B.5-B.7)
#   S16  Survival Models (Cox DiD -- basic; detailed H1/H4 Cox in S8)
#   S17  Diagnostic Data Export
#   S18  Publication LaTeX Tables
#
# NOTE ON H3: S13 runs OLS on closed_tanks (descriptive only -- selection bias
#   caveat applies). The primary H3 test is in S8d (duration model on full panel).
#==============================================================================


#==============================================================================
# S1: SETUP & DATA LOADING
#==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(here)
  library(ggplot2)
  library(broom)
  library(patchwork)
})

if (requireNamespace("fwildclusterboot", quietly = TRUE))
  library(fwildclusterboot)

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

USE_BOOTSTRAP <- FALSE
N_BOOTSTRAP   <- 9999
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")

meta <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
list2env(meta, envir = .GlobalEnv)

# Fallback: define any constants missing from metadata
if (!exists("OUTPUT_TABLES"))  OUTPUT_TABLES  <- here("Output", "Tables")
if (!exists("OUTPUT_FIGURES")) OUTPUT_FIGURES <- here("Output", "Figures")
if (!exists("POST_YEAR"))      POST_YEAR      <- 1999L
if (!exists("TREATMENT_YEAR")) TREATMENT_YEAR <- 1999L
if (!exists("ES_END"))         ES_END         <- 2018L
if (!exists("PANEL_START"))    PANEL_START    <- 1990L
if (!exists("PANEL_END"))      PANEL_END      <- 2018L
if (!exists("AGE_BIN_BREAKS")) AGE_BIN_BREAKS <- c(0, 5, 10, 15, 20, 25, Inf)
if (!exists("AGE_BIN_LABELS")) AGE_BIN_LABELS <- c("0-4","5-9","10-14","15-19","20-24","25+")
if (!exists("AGE_BIN_REF"))    AGE_BIN_REF    <- "0-4"

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

COL_TX    <- "#D55E00"
COL_CTRL  <- "#0072B2"
COL_YOUNG <- "#009E73"
COL_OLD   <- "#CC79A7"

theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(plot.title       = element_text(face = "bold", size = base_size + 2),
          plot.subtitle    = element_text(color = "grey40", size = base_size),
          panel.grid.minor = element_blank(),
          legend.position  = "bottom")
}
theme_set(theme_pub())

# Load datasets
annual_data    <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
tank_inventory <- readRDS(file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
closed_tanks   <- readRDS(file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
tanks_1999     <- readRDS(file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))

cat(sprintf("Loaded: %s rows | %s facilities | %s tanks | %s closures\n",
  format(nrow(annual_data),             big.mark = ","),
  format(uniqueN(annual_data$panel_id), big.mark = ","),
  format(nrow(tank_inventory),          big.mark = ","),
  format(nrow(closed_tanks),            big.mark = ",")))

#------------------------------------------------------------------------------
# Variable construction
#------------------------------------------------------------------------------

annual_data[, any_closure   := closure_event]
annual_data[, any_leak      := leak_year]
annual_data[, replace_event := as.integer(closure_event == 1 & exit_flag == 0)]

if (!"vintage_cohort" %in% names(annual_data)) {
  if (!"install_year" %in% names(tanks_1999)) {
    if ("install_date" %in% names(tanks_1999)) {
      tanks_1999[, install_year := year(install_date)]
    } else {
      stop("tanks_1999 has neither 'install_year' nor 'install_date'.")
    }
  }
  tanks_1999[, vc := fcase(
    install_year < 1965,                  "Pre-1965",
    install_year %between% c(1965, 1974), "1965-1974",
    install_year %between% c(1975, 1979), "1975-1979",
    install_year %between% c(1980, 1984), "1980-1984",
    install_year %between% c(1985, 1988), "1985-1988",
    default =                              "Post-1988"
  )]
  fac_vc <- tanks_1999[, .(vintage_cohort = names(which.max(table(vc)))), by = panel_id]
  annual_data <- merge(annual_data, fac_vc, by = "panel_id", all.x = TRUE)
  annual_data[is.na(vintage_cohort), vintage_cohort := "Post-1988"]
}

vc_levels <- c("Pre-1965","1965-1974","1975-1979","1980-1984","1985-1988","Post-1988")
annual_data[, vintage_cohort := factor(vintage_cohort, levels = vc_levels)]

annual_data[, mandate_window_3yr := as.integer(
  state == "TX" & spec_B_eligible == 1 & panel_year %between% c(1988L, 1994L))]

annual_data[, age_bin := factor(
  cut(avg_tank_age, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
      right = FALSE, include.lowest = TRUE),
  levels = AGE_BIN_LABELS)]
annual_data[, age_bin := relevel(age_bin, ref = AGE_BIN_REF)]

wall_col <- intersect(
  c("pct_single_wall","has_single_walled","any_single_walled"),
  names(annual_data))[1]

rel_min_full     <- 1990L - POST_YEAR
rel_max_full     <- ES_END - POST_YEAR
rel_min_youngest <- 1994L - POST_YEAR
rel_min_oldest   <- -5L

cat(sprintf("Event study window: pre [%d, -1] post [1, %d]\n",
            rel_min_full, rel_max_full))

annual_data[, age_treat_bin := cut(
  mean_age_1998,
  breaks = c(0, 5, 6, 9, Inf),
  labels = c("1-2 yrs (youngest)", "3-5 yrs", "6-8 yrs", "9+ yrs (oldest)"),
  right  = FALSE, include.lowest = TRUE
)]
annual_data[, age_treat_bin := relevel(
  factor(age_treat_bin), ref = "1-2 yrs (youngest)"
)]


#==============================================================================
# S2: HELPER FUNCTIONS
#==============================================================================

save_did_table <- function(models, headers, base_name, title,
                           tvar = "did_term", digits = 4) {
  results <- mapply(function(m, h) {
    ct  <- coeftable(summary(m, cluster = ~state))
    idx <- grep(tvar, rownames(ct))[1]
    data.frame(Model    = h,
               Estimate = round(ct[idx, "Estimate"],   digits),
               SE       = round(ct[idx, "Std. Error"], digits),
               t_stat   = round(ct[idx, "t value"],    3),
               p_value  = round(ct[idx, "Pr(>|t|)"],   4),
               N_obs    = nobs(m))
  }, models, headers, SIMPLIFY = FALSE)

  dt <- rbindlist(results)
  fwrite(dt, file.path(OUTPUT_TABLES, paste0(base_name, ".csv")))
  sink(file.path(OUTPUT_TABLES, paste0(base_name, ".txt")))
  cat(title, "\n"); print(as.data.frame(dt))
  lapply(seq_along(models), function(i) {
    cat(sprintf("\n--- %s ---\n", headers[[i]])); print(summary(models[[i]]))
  })
  sink()
  suppressWarnings(
    etable(models, title = title, tex = TRUE, digits = digits,
           file = file.path(OUTPUT_TABLES, paste0(base_name, ".tex")))
  )
  cat(sprintf("  Saved: %s\n", base_name))
  invisible(dt)
}

extract_did <- function(m, tvar = "did_term") {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  list(beta = ct[idx,"Estimate"], se = ct[idx,"Std. Error"],
       p    = ct[idx,"Pr(>|t|)"], n  = nobs(m))
}

# For split-sample event study models: no did_term, average post-period coefs
extract_att_from_es <- function(m) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep("rel_year_bin::[^-][0-9]*:texas_treated", rownames(ct))
  if (length(idx) == 0) return(list(beta = NA, se = NA, p = NA_real_, n = nobs(m)))
  list(
    beta = mean(ct[idx, "Estimate"],   na.rm = TRUE),
    se   = mean(ct[idx, "Std. Error"], na.rm = TRUE),
    p    = NA_real_,
    n    = nobs(m)
  )
}

stars_fn <- function(p) {
  if (is.na(p))  return("")
  if (p < 0.01)  return("$^{***}$")
  if (p < 0.05)  return("$^{**}$")
  if (p < 0.10)  return("$^{*}$")
  ""
}

plot_es <- function(model, title = "", subtitle = NULL,
                    ylab = "Effect on Pr(Tank Closure)",
                    ref_period = -1, color = "grey40",
                    xlim_lo = NULL, xlim_hi = NULL,
                    pre_trend_p = NULL, filename = NULL) {

  ct <- as.data.table(tidy(model, conf.int = TRUE))
  ct <- ct[grepl("rel_year|event_time", term)]
  ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- rbind(ct,
    data.table(term="ref", estimate=0, std.error=0, conf.low=0, conf.high=0,
               rel_year=ref_period), fill=TRUE)
  setorder(ct, rel_year)
  ct[, period := fcase(rel_year < 0, "Pre",
                        rel_year == ref_period, "Ref",
                        default = "Post")]

  if (!is.null(pre_trend_p) && is.null(subtitle))
    subtitle <- sprintf("Pre-trend F-test p = %.3f | ref = t%+d",
                        pre_trend_p, ref_period)

  xl <- if (is.null(xlim_lo)) min(ct$rel_year) else xlim_lo
  xh <- if (is.null(xlim_hi)) max(ct$rel_year) else xlim_hi

  p <- ggplot(ct[rel_year %between% c(xl, xh)],
              aes(x = rel_year, y = estimate)) +
    annotate("rect", xmin = -Inf, xmax = -0.5, ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                    fill = ifelse(rel_year < 0, "pre", "post")), alpha = 0.15) +
    geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_point(aes(color = period), size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = period),
                  width = 0.25, linewidth = 0.5) +
    scale_color_manual(values = c(Pre = "#4575B4", Post = "#D73027", Ref = "black"),
                       guide = "none") +
    scale_fill_manual(values = c(pre = "#4575B4", post = "#D73027"), guide = "none") +
    labs(title = title, subtitle = subtitle,
         x = "Years Relative to Treatment (1999)", y = ylab) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

  if (!is.null(filename)) {
    ggsave(filename, p, width = 14, height = 8, dpi = 200, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p, width = 14, height = 8,
           device = cairo_pdf)
  }
  invisible(list(plot = p, data = ct))
}

pt_pval <- function(m) {
  nms <- names(coef(m))
  pre <- nms[grepl("::-[2-9]|::-1[0-9]", nms)]
  if (length(pre) == 0) return(NA_real_)
  suppressWarnings(wald(m, keep = pre)$p)
}

texas_share_by_period <- function(dt, rel_year_col = "rel_year_bin",
                                  weight_col = NULL) {
  if (is.null(weight_col)) {
    dt[, .(texas_share = mean(texas_treated)), by = get(rel_year_col)]
  } else {
    dt[, .(texas_share = weighted.mean(texas_treated, get(weight_col))),
       by = get(rel_year_col)]
  }
}

pull_coef <- function(m, pattern) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  if (length(idx) == 0) return(list(b = "---", se = "---"))
  list(
    b  = sprintf("%.4f%s", ct[idx[1], "Estimate"], stars_fn(ct[idx[1], "Pr(>|t|)"])),
    se = sprintf("(%.4f)", ct[idx[1], "Std. Error"])
  )
}

extract_cox_coef <- function(m, pattern) {
  s   <- summary(m)$coefficients
  idx <- grep(pattern, rownames(s))
  if (length(idx) == 0) return(NULL)
  data.table(
    term = rownames(s)[idx],
    hr   = s[idx, "exp(coef)"],
    coef = s[idx, "coef"],
    se   = s[idx, "se(coef)"],
    z    = s[idx, "z"],
    p    = s[idx, "Pr(>|z|)"]
  )
}


#==============================================================================
# S3: SAMPLE CONSTRUCTION & DIAGNOSTICS
#==============================================================================

cat("\n=== S3: SAMPLE CONSTRUCTION ===\n")

main_sample <- annual_data[
  single_tanks  == active_tanks  &
  has_gasoline_year == 1         &
  install_year  >  1989          &
  install_year  <= 1997
]

sw_broader_sample <- annual_data[
  has_single_walled == 1 &
  install_year > 1989    &
  install_year <= 1997
]

youngest_sample <- main_sample[mean_age_1998 <= 5]
oldest_sample   <- main_sample[mean_age_1998 >  5]

main_sample[,      rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -8L)]
youngest_sample[,  rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), rel_min_youngest)]
oldest_sample[,    rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), rel_min_oldest)]
sw_broader_sample[,rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -8L)]

cat(sprintf("  Main (make-model):  %s obs | %s fac | %s TX | %s ctrl\n",
  format(nrow(main_sample),             big.mark = ","),
  format(uniqueN(main_sample$panel_id), big.mark = ","),
  format(uniqueN(main_sample[texas_treated == 1, panel_id]), big.mark = ","),
  format(uniqueN(main_sample[texas_treated == 0, panel_id]), big.mark = ",")))
cat(sprintf("  Youngest (<=5):     %s obs | %s fac\n",
  format(nrow(youngest_sample),             big.mark = ","),
  format(uniqueN(youngest_sample$panel_id), big.mark = ",")))
cat(sprintf("  Oldest (>5):        %s obs | %s fac\n",
  format(nrow(oldest_sample),             big.mark = ","),
  format(uniqueN(oldest_sample$panel_id), big.mark = ",")))
cat(sprintf("  Broader SW (H4):    %s obs | %s fac\n",
  format(nrow(sw_broader_sample),             big.mark = ","),
  format(uniqueN(sw_broader_sample$panel_id), big.mark = ",")))

# Age distribution plot
age_dist_1998 <- main_sample[panel_year == 1998,
  .(n = .N),
  by = .(age_bin_yr = cut(mean_age_1998, breaks = seq(0, 10, 1), right = FALSE),
         group = fifelse(texas_treated == 1, "Texas", "Control"))
]
age_dist_1998[, share := n / sum(n), by = group]

p_age_dist <- ggplot(age_dist_1998[!is.na(age_bin_yr)],
                     aes(x = age_bin_yr, y = share, fill = group)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c(Texas = COL_TX, Control = COL_CTRL)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title    = "Age Distribution at Treatment Date (1998)",
       subtitle = "Make-model sample",
       x = "Mean Tank Age in 1998 (1-year bins)",
       y = "Share of Facilities", fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_9_Age_Distribution_1998.png"),
       p_age_dist, width = 10, height = 6, dpi = 300, bg = "white")

age_share_table <- dcast(age_dist_1998, age_bin_yr ~ group, value.var = "share")
fwrite(age_share_table, file.path(OUTPUT_TABLES, "Diagnostic_Age_Shares_1998.csv"))

# Raw closure rates plot
raw_trends <- main_sample[
  panel_year %between% c(1992L, 2005L) & !is.na(mean_age_1998),
  .(closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac        = uniqueN(panel_id)),
  by = .(panel_year,
         group   = fifelse(texas_treated == 1, "Texas", "Control"),
         age_grp = fifelse(mean_age_1998 <= 5,
                           "Young (\u22645 yrs in 1998)",
                           "Old (>5 yrs in 1998)"))
][!is.na(age_grp)]

raw_trends[, group   := factor(group,   levels = c("Control", "Texas"))]
raw_trends[, age_grp := factor(age_grp, levels = c("Young (\u22645 yrs in 1998)",
                                                    "Old (>5 yrs in 1998)"))]

p_raw <- ggplot(
  raw_trends[n_fac >= 200],          # drop thin early cells (n < 200)
  aes(x = panel_year, y = closure_rate, color = group,
      linetype = age_grp, shape = age_grp)
) +
  annotate("rect", xmin = -Inf, xmax = 1998.5,
           ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.5) +
  geom_vline(xintercept = 1998.5, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  annotate("text", x = 1998.7, y = Inf, label = "Reform\n(1999)",
           hjust = 0, vjust = 1.3, size = 3.2, color = "grey30") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2, fill = "white", stroke = 0.8) +
  scale_color_manual(values = c(Control = "#0072B2", Texas = "#D55E00"), name = NULL) +
  scale_linetype_manual(
    values = c("Young (\u22645 yrs in 1998)" = "dashed",
               "Old (>5 yrs in 1998)"        = "solid"), name = NULL) +
  scale_shape_manual(
    values = c("Young (\u22645 yrs in 1998)" = 21,
               "Old (>5 yrs in 1998)"        = 19), name = NULL) +
  scale_x_continuous(breaks = seq(1992, 2005, 2),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(title    = "Raw Closure Rates by State Group and Age at Treatment",
       subtitle = paste0(
         "Make-model sample (motor fuel, single-tank, single-walled, 1990\u20131997 install).\n",
         "Age split at mean tank age in 1998 = 5 years. Cells with n < 200 dropped."),
       x = "Year", y = "Mean Pr(Closure)",
       caption = "Note: Unadjusted cell means. Facility and year FEs not applied.") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey30", size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.8, "cm")) +
  guides(
    color    = guide_legend(order = 1, nrow = 1,
                            override.aes = list(linewidth = 1.2)),
    linetype = guide_legend(order = 2, nrow = 1,
                            override.aes = list(linewidth = 1.2, color = "grey30")),
    shape    = guide_legend(order = 2, nrow = 1,
                            override.aes = list(color = "grey30"))
  )

ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_OldYoung.png"),
       p_raw, width = 11, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_OldYoung.pdf"),
       p_raw, width = 11, height = 6.5, device = cairo_pdf)
cat("  Saved: Figure_Raw_ClosureRates_OldYoung\n")


#==============================================================================
# S4: PARALLEL TRENDS VALIDATION
#==============================================================================

cat("\n=== S4: PARALLEL TRENDS VALIDATION ===\n")

m_pt_main <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample[panel_year < POST_YEAR], cluster = ~state
)

pt_results <- list(
  main    = pt_pval(m_pt_main),
  n_pre   = nrow(main_sample[panel_year < POST_YEAR])
)

cat(sprintf("  Pre-trend F-test p (main): %.4f\n", pt_results$main))
cat(sprintf("  Interpretation: %s\n",
  fifelse(!is.na(pt_results$main) & pt_results$main > 0.10,
          "PASS -- no evidence of differential pre-trends",
          "CHECK -- pre-trend may be present")))


#==============================================================================
# S5: AGE-AT-TREATMENT HTE
#
# THEORY MAP:
#   Prop 3(i):  ATT(young) ≈ 0  -- base did_term coef (old_at_treat = 0)
#   Prop 3(ii): ATT(old)   > 0  -- did_term + did_term:old_at_treat
#
# MODEL STRUCTURE (S5c triple-interaction event study):
#   i(rel_year_bin, texas_treated, ref=-1) = year-by-year ATT for YOUNG
#       coef names: rel_year_bin::<t>:texas_treated
#   i(rel_year_bin, tx_old, ref=-1)        = DIFFERENTIAL (old - young)
#       coef names: rel_year_bin::<t>:tx_old
#   tx_old = texas_treated * old_at_treat  (pre-computed; i() needs plain var)
#   Old path in plot = texas_treated coefs + tx_old coefs
#==============================================================================

cat("\n=== S5: AGE-AT-TREATMENT HTE ===\n")

#--- S5a: Binary classification ---
main_sample[, old_at_treat := as.integer(mean_age_1998 > 5)]
main_sample[, tx_old       := texas_treated * old_at_treat]

cat(sprintf("  Old (>5 yrs):   %s fac\n",
  format(uniqueN(main_sample[old_at_treat == 1, panel_id]), big.mark = ",")))
cat(sprintf("  Young (<=5):    %s fac\n",
  format(uniqueN(main_sample[old_at_treat == 0, panel_id]), big.mark = ",")))

#--- S5b: Simple DiD by group ---
m_did_young <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample[old_at_treat == 0], cluster = ~state)

m_did_old <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample[old_at_treat == 1], cluster = ~state)

m_did_interact <- feols(
  closure_event ~ did_term + did_term:old_at_treat | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_interact_agectrl <- feols(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state)

d_young <- extract_did(m_did_young)
d_old   <- extract_did(m_did_old)

pre_mean_young <- mean(
  main_sample[old_at_treat == 0 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)
pre_mean_old <- mean(
  main_sample[old_at_treat == 1 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)

cat(sprintf("  ATT(young): %.5f (p=%.4f) | pre-mean=%.4f\n",
  d_young$beta, d_young$p, pre_mean_young))
cat(sprintf("  ATT(old):   %.5f (p=%.4f) | pre-mean=%.4f\n",
  d_old$beta, d_old$p, pre_mean_old))
cat(sprintf("  Consistent with Prop 3: %s\n",
  fifelse(d_old$beta > d_young$beta, "YES", "NO")))

#--- S5c: Triple-interaction event study ---
m_es_hte <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state)

cn <- names(coef(m_es_hte))
cat(sprintf("  Young coefs: %d | Differential coefs: %d | Match: %s\n",
  sum(grepl(":texas_treated$", cn)),
  sum(grepl(":tx_old$",        cn)),
  fifelse(sum(grepl(":texas_treated$", cn)) == sum(grepl(":tx_old$", cn)),
          "YES", "CHECK")))

#--- S5d: HTE event study plot function ---
plot_es_hte <- function(model, title = "", subtitle_extra = NULL,
                        filename = NULL, xlim_lo = -8, xlim_hi = NULL,
                        col_young = COL_YOUNG, col_old = COL_OLD) {

  ct <- as.data.table(tidy(model, conf.int = TRUE))

  young <- ct[grepl(":texas_treated$", term) & grepl("rel_year_bin", term)]
  young[, rel_year := as.numeric(gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]
  young[, group    := "Young (\u22645 yrs in 1998)"]
  young[, `:=`(conf.low  = estimate - 1.96 * std.error,
               conf.high = estimate + 1.96 * std.error)]

  diff_dt <- ct[grepl(":tx_old$", term) & grepl("rel_year_bin", term)]
  diff_dt[, rel_year := as.numeric(gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]

  old <- merge(
    young[,   .(rel_year, base = estimate, base_se = std.error)],
    diff_dt[, .(rel_year, diff = estimate, diff_se = std.error)],
    by = "rel_year", all.x = TRUE)
  old[is.na(diff), `:=`(diff = 0, diff_se = 0)]
  old[, `:=`(
    estimate  = base + diff,
    conf.low  = (base + diff) - 1.96 * sqrt(base_se^2 + diff_se^2),
    conf.high = (base + diff) + 1.96 * sqrt(base_se^2 + diff_se^2),
    group     = "Old (>5 yrs in 1998)"
  )]

  ref_rows <- data.table(
    rel_year = -1L, estimate = 0, conf.low = 0, conf.high = 0,
    group = c("Young (\u22645 yrs in 1998)", "Old (>5 yrs in 1998)"))

  plot_dt <- rbind(young[, .(rel_year, estimate, conf.low, conf.high, group)],
                   old[,   .(rel_year, estimate, conf.low, conf.high, group)],
                   ref_rows, fill = TRUE)
  setorder(plot_dt, group, rel_year)

  sub <- "Single model | Pooled year FEs | Old path = base + differential"
  if (!is.null(subtitle_extra)) sub <- paste0(sub, "\n", subtitle_extra)
  xh  <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(plot_dt[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate, color = group, fill = group)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_fill_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = title, subtitle = sub,
         x = "Years Relative to Treatment (1999)",
         y = "\u0394Pr(Tank Closure)", color = NULL, fill = NULL) +
    theme_pub() +
    theme(legend.position = "bottom", panel.grid.major.x = element_blank())

  if (!is.null(filename)) {
    ggsave(filename, p, width = 14, height = 7, dpi = 200, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p, width = 14, height = 7,
           device = cairo_pdf)
    cat(sprintf("  Saved: %s\n", basename(filename)))
  }
  invisible(list(plot = p, data = plot_dt))
}

att_line <- sprintf(
  "ATT(young) = %.4f%s  |  ATT(old) = %.4f%s  |  Diff = %.4f",
  d_young$beta, stars_fn(d_young$p),
  d_old$beta,   stars_fn(d_old$p),
  d_old$beta - d_young$beta)

es_hte_out <- plot_es_hte(
  model          = m_es_hte,
  title          = "Figure 6: HTE Event Study \u2014 Old vs. Young at Treatment",
  subtitle_extra = att_line,
  xlim_lo        = -8,
  xlim_hi        = rel_max_full,
  filename       = file.path(OUTPUT_FIGURES, "Figure_6_ES_HTE_OldYoung.png"))

#--- S5e: Four-spec DiD table ---
m_did_pooled <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_agectrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_binary_hte <- feols(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state)

m_did_4bin_hte <- feols(
  closure_event ~ did_term:age_treat_bin + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state)

ct4 <- coeftable(summary(m_did_4bin_hte, cluster = ~state))
bin_labels_4 <- levels(main_sample$age_treat_bin)
cat("\n  Total ATT by age-at-treatment bin (Prop 3 monotonicity check):\n")
ests_4bin <- sapply(bin_labels_4, function(bl) {
  idx <- grep(paste0("did_term:age_treat_bin", bl), rownames(ct4), fixed = TRUE)
  if (length(idx) > 0) {
    cat(sprintf("    %s: %.5f (p=%.4f)\n",
      bl, ct4[idx, "Estimate"], ct4[idx, "Pr(>|t|)"]))
    return(ct4[idx, "Estimate"])
  }
  cat(sprintf("    %s: NOT FOUND\n", bl)); NA_real_
})
ests_4bin <- ests_4bin[!is.na(ests_4bin)]
cat(sprintf("  Monotonically rising: %s\n",
  fifelse(length(ests_4bin) > 1 & all(diff(ests_4bin) > 0), "YES", "NO")))

save_did_table(
  models    = list(m_did_pooled, m_did_agectrl, m_did_binary_hte, m_did_4bin_hte),
  headers   = c("Pooled DiD", "+ Age Control", "Binary HTE", "4-Bin HTE"),
  base_name = "Table4_AgeTreat_HTE_MakeModel",
  title     = "Table 4: Age-at-Treatment HTE -- Make-Model Sample")

#--- S5f: LaTeX tables (Table 3 + Table 4) ---
specs       <- list(m_did_pooled, m_did_agectrl, m_did_binary_hte, m_did_4bin_hte)
spec_labels <- c("Pooled", "+Age Ctrl", "Binary HTE", "4-Bin HTE")
did_rows    <- lapply(specs, pull_coef, pattern = "^did_term$")
old_rows    <- lapply(specs, pull_coef, pattern = "did_term:old_at_treat")

writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Age-at-Treatment Heterogeneous Treatment Effects (H1/H2)}",
  "\\label{tbl:age_treat_hte}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & (1) & (2) & (3) & (4)\\\\",
  sprintf(" & %s \\\\", paste(spec_labels, collapse = " & ")),
  "\\midrule",
  "\\textit{Panel A: Young group (age $\\leq5$ in 1998)} & & & & \\\\",
  sprintf("Texas $\\times$ Post & %s \\\\",
    paste(sapply(did_rows, `[[`, "b"), collapse = " & ")),
  sprintf(" & %s \\\\",
    paste(sapply(did_rows, `[[`, "se"), collapse = " & ")),
  "\\addlinespace",
  "\\textit{Panel B: Differential for Old group ($>5$ yrs)} & & & & \\\\",
  sprintf("$\\times$ Old at treatment & %s \\\\",
    paste(sapply(old_rows, `[[`, "b"), collapse = " & ")),
  sprintf(" & %s \\\\",
    paste(sapply(old_rows, `[[`, "se"), collapse = " & ")),
  "\\midrule",
  "Age control (panel bins) & No & Yes & Yes & Yes \\\\",
  "Facility FE & Yes & Yes & Yes & Yes \\\\",
  "Year FE & Yes & Yes & Yes & Yes \\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(nobs(specs[[1]]), big.mark = ","),
    format(nobs(specs[[2]]), big.mark = ","),
    format(nobs(specs[[3]]), big.mark = ","),
    format(nobs(specs[[4]]), big.mark = ",")),
  "\\bottomrule",
  paste0("\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
    "Make-model sample. Spec 4: each bin coef is the total ATT for that bin ",
    "(no base term -- age_treat_bin main effects absorbed by facility FE). ",
    "SE clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}"),
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "Table4_AgeTreat_HTE_MakeModel.tex"))

writeLines(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Treatment Effect by Age at Reform: Old vs.\\ Young Facilities (H2)}",
  "\\label{tbl:old_young_did}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Young only & Old only & Interact & Interact+AgeCtrl \\\\\\midrule",
  "\\textit{ATT estimates} & & & & \\\\",
  sprintf("Texas $\\times$ Post & %s & %s & %s & %s \\\\",
    sprintf("%.4f%s", d_young$beta, stars_fn(d_young$p)),
    sprintf("%.4f%s", d_old$beta,   stars_fn(d_old$p)),
    pull_coef(m_did_interact,         "^did_term$")$b,
    pull_coef(m_did_interact_agectrl, "^did_term$")$b),
  sprintf(" & (%.4f) & (%.4f) & %s & %s \\\\",
    d_young$se, d_old$se,
    pull_coef(m_did_interact,         "^did_term$")$se,
    pull_coef(m_did_interact_agectrl, "^did_term$")$se),
  sprintf("$\\times$ Old ($>$5 in 1998) & & & %s & %s \\\\",
    pull_coef(m_did_interact,         "old_at_treat")$b,
    pull_coef(m_did_interact_agectrl, "old_at_treat")$b),
  sprintf(" & & & %s & %s \\\\",
    pull_coef(m_did_interact,         "old_at_treat")$se,
    pull_coef(m_did_interact_agectrl, "old_at_treat")$se),
  "\\midrule",
  sprintf("Pre-reform mean & %.4f & %.4f & \\multicolumn{2}{c}{---} \\\\",
    pre_mean_young, pre_mean_old),
  "Facility + Year FE & Yes & Yes & Yes & Yes \\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(d_young$n, big.mark=","), format(d_old$n, big.mark=","),
    format(nobs(m_did_interact), big.mark=","),
    format(nobs(m_did_interact_agectrl), big.mark=",")),
  "\\bottomrule",
  paste0("\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
    "Make-model sample. SE clustered at state. ",
    "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}"),
  "\\end{tabular}\\end{table}"
), file.path(OUTPUT_TABLES, "Table3_OldYoung_DiD_MakeModel.tex"))

cat("  Saved: Table3_OldYoung_DiD_MakeModel.tex\n")
cat("  Saved: Table4_AgeTreat_HTE_MakeModel.tex\n")

cat("\n====================================================================\n")
cat("S5 COMPLETE\n")
cat("====================================================================\n")


#==============================================================================
# S6: AGE-BIN HTE COEFFICIENT PLOT (Figure 8)
#
# NOTE ON PRE-PERIOD FALSIFICATION:
#   texas_treated and age_treat_bin are both time-invariant at the facility
#   level. Their interaction is fully absorbed by panel_id FE and CANNOT be
#   estimated as a static coefficient. Figure 8 shows post-reform only.
#   Pre-trend validation is shown in Figure 6 (event study pre-period flat).
#==============================================================================

cat("\n=== S6: AGE-BIN HTE COEFFICIENT PLOT (Figure 8) ===\n")

build_hte_dt <- function(m, base_tvar, label,
                         age_var    = "age_treat_bin",
                         bin_levels = levels(main_sample$age_treat_bin)) {
  ct       <- as.data.table(tidy(m, conf.int = TRUE))
  int_rows <- ct[grepl(age_var,   term, fixed = TRUE) &
                 grepl(base_tvar, term, fixed = TRUE)]
  int_rows[, bin := gsub(paste0(".*", age_var), "", term)]
  base_row <- ct[term == base_tvar]
  bc  <- if (nrow(base_row) > 0) base_row$estimate[1]  else 0
  bse <- if (nrow(base_row) > 0) base_row$std.error[1] else 0
  if (nrow(base_row) > 0 && nrow(int_rows) > 0) {
    int_rows[, `:=`(estimate  = estimate  + bc,
                    conf.low  = conf.low  + bc,
                    conf.high = conf.high + bc)]
  }
  int_rows <- int_rows[, .(bin, estimate, conf.low, conf.high)]
  int_rows[, `:=`(sample = label, bin = factor(bin, levels = bin_levels))]
  int_rows
}

hte_main <- build_hte_dt(m_did_4bin_hte, base_tvar = "did_term",
                         label = "Post-Reform (Make-Model)")

p_hte <- ggplot(
  hte_main[!is.na(bin)],
  aes(x = bin, y = estimate, group = 1)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.6, color = COL_TX, alpha = 0.7) +
  geom_point(size = 3, color = COL_TX) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, linewidth = 0.5, color = COL_TX) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Figure 8: Treatment Effect on Closure by Age-at-Treatment Bin",
    subtitle = paste0(
      "Monotonically rising = Prop 3. Pre-trend validation in Figure 6.\n",
      "Each bin coef is total ATT (no base term; age_treat_bin absorbed by facility FE)."),
    x = "Mean Tank Age in 1998", y = "\u0394Pr(Closure) | Texas \u00d7 Post") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_8_HTE_AgeBin_MakeModel.png"),
       p_hte, width = 11, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_8_HTE_AgeBin_MakeModel.pdf"),
       p_hte, width = 11, height = 6, device = cairo_pdf)
cat("  Saved: Figure_8_HTE_AgeBin_MakeModel\n")

cat("\n====================================================================\n")
cat("S6 COMPLETE\n")
cat("====================================================================\n")


#==============================================================================
# S7: ROBUSTNESS -- AGE-GROUP-SPECIFIC YEAR FEs
#
# MOTIVATION: Shared panel_year FEs force a single delta_t estimated as a
# weighted average across Young and Old control facilities. But raw data shows
# Control Young and Control Old have very different pre-period trajectories.
# A shared year FE is not the right counterfactual for either group individually.
#
# METHOD A (split=~age_grp): Group-specific year FEs. Preferred visual.
# METHOD B (panel_year^age_grp): Equivalent single model. Reuses plot_es_hte().
#==============================================================================

cat("\n=== S7: ROBUSTNESS -- AGE-GROUP-SPECIFIC YEAR FEs ===\n")

main_sample[, age_grp := fifelse(mean_age_1998 <= 5, "Young", "Old")]

# Method A: split sample
models_split <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data = main_sample, split = ~age_grp, cluster = ~state)

cat("  Sub-model names: "); print(names(models_split))

nm_old   <- grep("Old",   names(models_split), value = TRUE)
nm_young <- grep("Young", names(models_split), value = TRUE)
m_split_old   <- models_split[[nm_old]]
m_split_young <- models_split[[nm_young]]

d_split_young <- extract_att_from_es(m_split_young)
d_split_old   <- extract_att_from_es(m_split_old)

cat(sprintf("  Method A ATT(young): %.5f | ATT(old): %.5f | Old>Young: %s\n",
  d_split_young$beta, d_split_old$beta,
  fifelse(d_split_old$beta > d_split_young$beta, "YES", "NO")))

plot_es_split <- function(m_young, m_old, title = "", subtitle_extra = NULL,
                          filename = NULL, xlim_lo = -8, xlim_hi = NULL,
                          col_young = COL_YOUNG, col_old = COL_OLD) {

  extract_path <- function(m, grp_label) {
    ct <- as.data.table(tidy(m, conf.int = TRUE))
    ct <- ct[grepl("rel_year_bin", term)]
    ct[, rel_year := as.numeric(gsub("rel_year_bin::(-?[0-9]+):.*", "\\1", term))]
    ct[, group    := grp_label]
    ct[, `:=`(conf.low  = estimate - 1.96 * std.error,
              conf.high = estimate + 1.96 * std.error)]
    ct[, .(rel_year, estimate, conf.low, conf.high, group)]
  }

  plot_dt <- rbind(
    extract_path(m_young, "Young (\u22645 yrs in 1998)"),
    extract_path(m_old,   "Old (>5 yrs in 1998)"),
    data.table(rel_year = -1L, estimate = 0, conf.low = 0, conf.high = 0,
               group = c("Young (\u22645 yrs in 1998)", "Old (>5 yrs in 1998)")),
    fill = TRUE)
  setorder(plot_dt, group, rel_year)

  sub <- "Method A: Split sample | Group-specific year FEs"
  if (!is.null(subtitle_extra)) sub <- paste0(sub, "\n", subtitle_extra)
  xh  <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(plot_dt[rel_year %between% c(xlim_lo, xh)],
              aes(x = rel_year, y = estimate, color = group, fill = group)) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) + geom_point(size = 2.8) +
    scale_color_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_fill_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = title, subtitle = sub,
         x = "Years Relative to Treatment (1999)",
         y = "\u0394Pr(Tank Closure)", color = NULL, fill = NULL) +
    theme_pub() +
    theme(legend.position = "bottom", panel.grid.major.x = element_blank())

  if (!is.null(filename)) {
    ggsave(filename, p, width = 14, height = 7, dpi = 200, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p, width = 14, height = 7,
           device = cairo_pdf)
    cat(sprintf("  Saved: %s\n", basename(filename)))
  }
  invisible(list(plot = p, data = plot_dt))
}

es_split_out <- plot_es_split(
  m_young        = m_split_young,
  m_old          = m_split_old,
  title          = "Figure B.1: Method A (Split Sample, Group-Specific Year FEs)",
  subtitle_extra = sprintf(
    "ATT(young)=%.4f  |  ATT(old)=%.4f  |  Diff=%.4f",
    d_split_young$beta, d_split_old$beta,
    d_split_old$beta - d_split_young$beta),
  xlim_lo  = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "FigureB1_Robustness_MethodA_SplitSample.png"))

# Method B: group-time interacted FEs
m_es_hte_grpfe <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year^age_grp,
  main_sample, cluster = ~state)

cn_b <- names(coef(m_es_hte_grpfe))
post_young_b <- mean(coef(m_es_hte_grpfe)[
  grepl(":texas_treated$", cn_b) & grepl("rel_year_bin::[^-]", cn_b)],
  na.rm = TRUE)
post_old_b <- post_young_b + mean(coef(m_es_hte_grpfe)[
  grepl(":tx_old$", cn_b) & grepl("rel_year_bin::[^-]", cn_b)],
  na.rm = TRUE)

es_hte_grpfe_out <- plot_es_hte(
  model          = m_es_hte_grpfe,
  title          = "Figure B.2: Method B (Group-Time Year FEs, Single Model)",
  subtitle_extra = sprintf("Avg post ATT(young)=%.4f  |  ATT(old)=%.4f",
                           post_young_b, post_old_b),
  xlim_lo  = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "FigureB2_Robustness_MethodB_GrpYearFE.png"))

# Primary spec post-period averages
post_young_primary <- mean(coef(m_es_hte)[
  grepl(":texas_treated$", names(coef(m_es_hte))) &
  grepl("rel_year_bin::[^-]", names(coef(m_es_hte)))], na.rm = TRUE)
post_old_primary <- post_young_primary + mean(coef(m_es_hte)[
  grepl(":tx_old$", names(coef(m_es_hte))) &
  grepl("rel_year_bin::[^-]", names(coef(m_es_hte)))], na.rm = TRUE)

comparison_tbl <- data.table(
  Spec     = c("Primary (shared year FE)", "Method A (split)", "Method B (grp-time FE)"),
  ATT_young = round(c(post_young_primary, d_split_young$beta, post_young_b), 5),
  ATT_old   = round(c(post_old_primary,   d_split_old$beta,   post_old_b),   5),
  Old_gt_Young = c(post_old_primary > post_young_primary,
                   d_split_old$beta > d_split_young$beta,
                   post_old_b       > post_young_b))
print(comparison_tbl)
fwrite(comparison_tbl,
  file.path(OUTPUT_TABLES, "TableB_Robustness_YearFE_Comparison.csv"))

p_compare <- (es_hte_out$plot       + labs(title = "Primary (Shared Year FE)")) /
             (es_split_out$plot     + labs(title = "Method A (Split Sample)"))  /
             (es_hte_grpfe_out$plot + labs(title = "Method B (Group-Time FE)")) +
  plot_annotation(
    title    = "Figure B.3: Year FE Specification Comparison",
    subtitle = "All three should show Old > Young post-1999. Divergence = shared FE contamination.",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10)))

ggsave(file.path(OUTPUT_FIGURES, "FigureB3_Robustness_YearFE_Comparison.png"),
       p_compare, width = 14, height = 21, dpi = 200, bg = "white")

cat("\n====================================================================\n")
cat("S7 COMPLETE\n")
cat("====================================================================\n")


#==============================================================================
# S8: HYPOTHESIS TESTS -- SURVIVORSHIP, H1 GRADIENT, H3 DURATION, H4 WALL
#
#   S8a  Survivorship diagnostic (pre-1999 attrition by age bin)
#   S8b  H1 binned scatter: closure rate vs age bin, four state-period lines
#   S8c  H1 Cox DiD with did_term:age_bin -- age gradient in hazard
#   S8d  H3 discrete-time hazard on full panel (PRIMARY H3 TEST)
#   S8e  H3 kernel density of age-at-closure by TX/control x pre/post
#   S8f  H4 Cox DiD on sw_broader_sample with did_term:wall_col
#   S8g  H4 Kaplan-Meier curves by wall type x state group x pre/post
#
# NOTE ON H3 vs S13:
#   S13 runs OLS on closed_tanks (descriptive -- selection bias caveat applies).
#   S8d is the primary H3 test: full at-risk panel, age as time scale.
#==============================================================================

cat("\n=== S8: HYPOTHESIS TESTS ===\n")

if (!requireNamespace("survival",  quietly = TRUE)) install.packages("survival")
if (!requireNamespace("ggsurvfit", quietly = TRUE)) install.packages("ggsurvfit")
library(survival)
library(ggsurvfit)

#------------------------------------------------------------------------------
# S8a: Survivorship Diagnostic
# Check how many old-at-treatment facilities exited BEFORE 1999 (pre-shock).
# High pre-1999 attrition in old bins biases H1 age gradient downward.
#------------------------------------------------------------------------------

cat("\n--- S8a: Survivorship Diagnostic ---\n")

surv_diag <- main_sample[,
  .(exited_pre99  = as.integer(any(closure_event == 1 & panel_year < POST_YEAR)),
    survived_1999 = as.integer(any(panel_year >= POST_YEAR)),
    age_treat_bin = first(age_treat_bin),
    texas         = first(texas_treated)),
  by = panel_id]

surv_summary <- surv_diag[!is.na(age_treat_bin),
  .(n_facilities = .N,
    n_exited_pre = sum(exited_pre99,  na.rm = TRUE),
    n_survived   = sum(survived_1999, na.rm = TRUE),
    pct_exit_pre = mean(exited_pre99, na.rm = TRUE)),
  by = .(age_treat_bin,
         texas = fifelse(texas == 1, "Texas", "Control"))
][order(texas, age_treat_bin)]

cat("\n  Pre-1999 attrition by age bin and state group:\n")
print(surv_summary)
fwrite(surv_summary,
  file.path(OUTPUT_TABLES, "Table_S8a_Survivorship_Diagnostic.csv"))

max_pre_exit <- max(surv_summary[
  age_treat_bin %in% levels(main_sample$age_treat_bin)[3:4], pct_exit_pre],
  na.rm = TRUE)
cat(sprintf("  Max pre-1999 exit rate in oldest bins: %.1f%%\n", 100 * max_pre_exit))
cat(sprintf("  Survivorship caveat material (>20%%): %s\n",
  fifelse(max_pre_exit > 0.20, "YES -- note in paper", "NO -- caveat minor")))

#------------------------------------------------------------------------------
# S8b: H1 Binned Scatter
# Closure rate vs age bin, four lines: TX pre, TX post, Control pre, Control post.
# H1 prediction: TX post-reform line has steeper positive slope than other three.
#------------------------------------------------------------------------------

cat("\n--- S8b: H1 Binned Scatter ---\n")

h1_scatter_dt <- main_sample[!is.na(age_treat_bin),
  .(closure_rate = mean(closure_event, na.rm = TRUE), n = .N),
  by = .(age_treat_bin,
         state_group = fifelse(texas_treated == 1, "Texas", "Control"),
         period      = fifelse(panel_year < POST_YEAR, "Pre-Reform", "Post-Reform"))
]
h1_scatter_dt[, period_group := factor(
  paste(state_group, period),
  levels = c("Control Pre-Reform","Control Post-Reform",
             "Texas Pre-Reform","Texas Post-Reform"))]

p_h1_scatter <- ggplot(
  h1_scatter_dt[!is.na(age_treat_bin)],
  aes(x = age_treat_bin, y = closure_rate,
      color = period_group, linetype = period_group, group = period_group)
) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_color_manual(
    values = c("Control Pre-Reform"="#0072B2","Control Post-Reform"="#56B4E9",
               "Texas Pre-Reform"  ="#D55E00","Texas Post-Reform"  ="#E69F00"),
    name = NULL) +
  scale_linetype_manual(
    values = c("Control Pre-Reform"="dashed","Control Post-Reform"="solid",
               "Texas Pre-Reform"  ="dashed","Texas Post-Reform"  ="solid"),
    name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Figure H1.1: Closure Rate by Age Bin (H1 Test)",
    subtitle = "H1 prediction: TX post-reform line has steeper positive slope than the other three.",
    x = "Tank Age in 1998 (Age-at-Treatment Bin)",
    y = "Mean Pr(Closure)",
    caption = "Unadjusted cell means. Make-model sample.") +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.key.width = unit(1.5, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "FigureH1_1_BinnedScatter_AgeClosure.png"),
       p_h1_scatter, width = 11, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureH1_1_BinnedScatter_AgeClosure.pdf"),
       p_h1_scatter, width = 11, height = 6.5, device = cairo_pdf)
cat("  Saved: FigureH1_1_BinnedScatter_AgeClosure\n")

#------------------------------------------------------------------------------
# S8c: H1 Cox DiD -- Age Gradient in the Hazard
# Estimand: did_term:age_bin interaction in Cox.
# Positive and rising interaction HR = age gradient steepens under RB = H1.
# Uses strata(panel_id) for within-facility baseline hazard.
# NOTE: age_bin is current panel age (time-varying), NOT age_treat_bin.
#------------------------------------------------------------------------------

cat("\n--- S8c: H1 Cox -- Age Gradient in Hazard ---\n")

setorder(main_sample, panel_id, panel_year)
main_sample[, surv_time := seq_len(.N), by = panel_id]

cox_h1_base <- coxph(
  Surv(surv_time, closure_event) ~
    did_term + did_term:age_bin + strata(panel_id),
  data = main_sample, cluster = main_sample$state, ties = "efron")

cox_h1_agectrl <- coxph(
  Surv(surv_time, closure_event) ~
    did_term + age_bin + did_term:age_bin + strata(panel_id),
  data = main_sample, cluster = main_sample$state, ties = "efron")

cat("\n  Cox H1 Spec 1 (base + gradient):\n")
print(summary(cox_h1_base)$coefficients)
cat("\n  Cox H1 Spec 2 (+ age_bin control):\n")
print(summary(cox_h1_agectrl)$coefficients)

cox_h1_coefs <- rbindlist(list(
  cbind(spec = "Spec1", extract_cox_coef(cox_h1_base,    "did_term")),
  cbind(spec = "Spec2", extract_cox_coef(cox_h1_agectrl, "did_term"))
), fill = TRUE)

interact_hrs <- cox_h1_coefs[spec == "Spec2" & grepl("did_term:age_bin", term)]
if (nrow(interact_hrs) > 1) {
  cat(sprintf("\n  Monotonically rising interaction HRs (H1): %s\n",
    fifelse(all(diff(interact_hrs$hr) > 0), "YES", "NO")))
}
fwrite(cox_h1_coefs,
  file.path(OUTPUT_TABLES, "Table_S8c_Cox_H1_AgeGradient.csv"))

if (nrow(interact_hrs) > 0) {
  p_cox_h1 <- ggplot(
    interact_hrs,
    aes(x    = term,
        y    = hr,
        ymin = exp(coef - 1.96 * se),
        ymax = exp(coef + 1.96 * se))
  ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    geom_point(size = 3, color = COL_TX) +
    geom_errorbar(width = 0.25, linewidth = 0.6, color = COL_TX) +
    scale_x_discrete(labels = function(x) gsub("did_term:age_bin", "", x)) +
    labs(title    = "Figure H1.2: Cox HR for did_term x Age Bin (H1 Test)",
         subtitle = "HR > 1 and rising = age gradient steepens under RB. strata(panel_id).",
         x = "Panel Age Bin",
         y = "Hazard Ratio (Texas x Post x Age Bin)",
         caption = "Spec 2 with age_bin main effect. SE clustered at state.") +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  ggsave(file.path(OUTPUT_FIGURES, "FigureH1_2_Cox_AgeGradient_HR.png"),
         p_cox_h1, width = 10, height = 6, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "FigureH1_2_Cox_AgeGradient_HR.pdf"),
         p_cox_h1, width = 10, height = 6, device = cairo_pdf)
  cat("  Saved: FigureH1_2_Cox_AgeGradient_HR\n")
}

#------------------------------------------------------------------------------
# S8d: H3 Duration Model -- Primary H3 Test
#
# WHY NOT OLS ON closed_tanks (existing S13):
#   Conditioning on closure having occurred = selection bias. Units that close
#   differ in covariate distributions across groups. The correct test uses the
#   FULL at-risk panel as the estimation sample.
#
# Three specs:
#   cloglog  -- discrete proportional hazard (correct likelihood)
#   LPM      -- facility FEs, age-scale, directly comparable to main DiD
#   Cox AFT  -- avg_tank_age as time axis (H3 is literally a time-to-event claim)
#
# H3 prediction: did_term > 0 = TX post-reform closure hazard is higher
#   at any given tank age (reform tips old-enough tanks into closure).
#------------------------------------------------------------------------------

cat("\n--- S8d: H3 Duration Model (PRIMARY H3 TEST) ---\n")

h3_dt <- main_sample[!is.na(avg_tank_age) & !is.na(did_term)]

# Spec 1: complementary log-log (discrete proportional hazard)
h3_cloglog <- glm(
  closure_event ~ did_term + age_bin + texas_treated,
  family = binomial(link = "cloglog"), data = h3_dt)

# Spec 2: LPM with facility FE -- comparable to main DiD
h3_lpm_agescale <- feols(
  closure_event ~ did_term + age_bin | panel_id,
  h3_dt, cluster = ~state)

# Spec 3: Cox with age as time scale
h3_cox_age <- coxph(
  Surv(avg_tank_age, closure_event) ~
    did_term + strata(panel_id),
  data = h3_dt, cluster = h3_dt$state, ties = "efron")

cat("\n  H3 cloglog did_term coef:\n")
print(coef(summary(h3_cloglog))["did_term", ])
cat("\n  H3 LPM did_term coef:\n")
print(coeftable(h3_lpm_agescale)["did_term", ])
cat("\n  H3 Cox (age scale) did_term HR:\n")
print(summary(h3_cox_age)$coefficients["did_term", ])

hr_h3 <- exp(coef(h3_cox_age)["did_term"])
cat(sprintf("\n  H3 Cox HR = %.4f: TX post-1999 closure hazard %.1f%% %s at any tank age.\n",
  hr_h3, abs(100 * (hr_h3 - 1)),
  fifelse(hr_h3 > 1, "HIGHER (H3 supported)", "lower (opposite of H3)")))

fwrite(
  as.data.table(coef(summary(h3_cloglog)), keep.rownames = "term"),
  file.path(OUTPUT_TABLES, "Table_S8d_H3_ClogLog_Duration.csv"))
cat("  Saved: Table_S8d_H3_ClogLog_Duration.csv\n")

#------------------------------------------------------------------------------
# S8e: H3 Kernel Density -- Age-at-Closure Distribution
# Distributional test: theory predicts higher mass at OLD ages post-reform in TX,
# not a uniform shift. Four densities: TX/control x pre/post.
# NOTE: conditional on closure event -- see S8d for unconditional primary test.
#------------------------------------------------------------------------------

cat("\n--- S8e: H3 Kernel Density -- Age-at-Closure ---\n")

h3_closed <- main_sample[
  closure_event == 1 & !is.na(avg_tank_age),
  .(avg_tank_age,
    state_group = fifelse(texas_treated == 1, "Texas", "Control"),
    period      = fifelse(panel_year < POST_YEAR, "Pre-Reform", "Post-Reform"))
]
h3_closed[, group := factor(paste(state_group, period), levels = c(
  "Control Pre-Reform","Control Post-Reform",
  "Texas Pre-Reform","Texas Post-Reform"))]

h3_stats <- h3_closed[,
  .(n      = .N,
    mean   = mean(avg_tank_age),
    median = median(avg_tank_age),
    p25    = quantile(avg_tank_age, 0.25),
    p75    = quantile(avg_tank_age, 0.75)),
  by = group][order(group)]

cat("\n  Age-at-closure summary statistics:\n"); print(h3_stats)
fwrite(h3_stats, file.path(OUTPUT_TABLES, "Table_S8e_H3_AgeAtClosure_Stats.csv"))

col_h3 <- c("Control Pre-Reform" ="#0072B2","Control Post-Reform"="#56B4E9",
            "Texas Pre-Reform"   ="#D55E00","Texas Post-Reform"  ="#E69F00")
lty_h3 <- c("Control Pre-Reform" ="dashed","Control Post-Reform"="solid",
            "Texas Pre-Reform"   ="dashed","Texas Post-Reform"  ="solid")

p_h3_density <- ggplot(h3_closed,
  aes(x = avg_tank_age, color = group, linetype = group)) +
  geom_density(linewidth = 0.9, adjust = 1.2) +
  geom_vline(data = h3_stats,
             aes(xintercept = median, color = group, linetype = group),
             linewidth = 0.4, alpha = 0.6) +
  scale_color_manual(values = col_h3, name = NULL) +
  scale_linetype_manual(values = lty_h3, name = NULL) +
  scale_x_continuous(breaks = seq(0, 20, 2)) +
  labs(
    title    = "Figure H3.1: Distribution of Tank Age at Closure (H3 Test)",
    subtitle = paste0(
      "H3 prediction: TX post-reform density has higher mass at old ages.\n",
      "Vertical lines = group medians. Conditional on closure -- see S8d for primary test."),
    x = "Tank Age at Closure (years)", y = "Density",
    caption = "Kernel density (adjust=1.2). Make-model sample.") +
  theme_pub() +
  theme(legend.key.width = unit(1.5, "cm"))

ggsave(file.path(OUTPUT_FIGURES, "FigureH3_1_KernelDensity_AgeAtClosure.png"),
       p_h3_density, width = 11, height = 6.5, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureH3_1_KernelDensity_AgeAtClosure.pdf"),
       p_h3_density, width = 11, height = 6.5, device = cairo_pdf)
cat("  Saved: FigureH3_1_KernelDensity_AgeAtClosure\n")

#------------------------------------------------------------------------------
# S8f: H4 Cox DiD -- Wall Type Gradient in Hazard
# REQUIRES sw_broader_sample (main_sample is SW-only -- H4 not identified there)
# H4: SW closes faster than DW post-reform in TX (premium burden on SW tanks).
#------------------------------------------------------------------------------

cat("\n--- S8f: H4 Cox -- Wall Type Gradient ---\n")

if (!exists("sw_broader_sample")) {
  cat("  WARNING: sw_broader_sample not found. Skipping S8f/S8g.\n")
} else if (!wall_col %in% names(sw_broader_sample)) {
  cat(sprintf("  WARNING: '%s' not in sw_broader_sample. Skipping.\n", wall_col))
} else {

  cat(sprintf("  sw_broader_sample: %s obs | %s fac\n",
    format(nrow(sw_broader_sample), big.mark = ","),
    format(uniqueN(sw_broader_sample$panel_id), big.mark = ",")))

  if (!"did_term" %in% names(sw_broader_sample))
    sw_broader_sample[,
      did_term := as.integer(texas_treated == 1 & panel_year >= POST_YEAR)]

  setorder(sw_broader_sample, panel_id, panel_year)
  sw_broader_sample[, surv_time := seq_len(.N), by = panel_id]

  fml_pool <- as.formula(sprintf(
    "Surv(surv_time, closure_event) ~ did_term + %s + did_term:%s",
    wall_col, wall_col))
  cox_h4_pool <- coxph(fml_pool, data = sw_broader_sample,
                       cluster = sw_broader_sample$state, ties = "efron")

  fml_strata <- as.formula(sprintf(
    "Surv(surv_time, closure_event) ~ did_term + did_term:%s + strata(panel_id)",
    wall_col))
  cox_h4_strata <- coxph(fml_strata, data = sw_broader_sample,
                         cluster = sw_broader_sample$state, ties = "efron")

  cat("\n  H4 Cox (pooled):\n"); print(summary(cox_h4_pool)$coefficients)
  cat("\n  H4 Cox (strata panel_id):\n"); print(summary(cox_h4_strata)$coefficients)

  fwrite(
    as.data.table(summary(cox_h4_strata)$coefficients, keep.rownames = "term"),
    file.path(OUTPUT_TABLES, "Table_S8f_Cox_H4_WallType.csv"))
  cat("  Saved: Table_S8f_Cox_H4_WallType.csv\n")

  #----------------------------------------------------------------------------
  # S8g: H4 Kaplan-Meier Curves
  # Four facets: TX pre / TX post / Control pre / Control post
  # Within each: KM curves for SW vs DW
  # H4 prediction: SW-DW gap widens in TX post-1999 only
  #----------------------------------------------------------------------------

  cat("\n--- S8g: H4 Kaplan-Meier Curves ---\n")

  sw_broader_sample[, period_group := paste0(
    fifelse(texas_treated == 1, "Texas", "Control"), " | ",
    fifelse(panel_year < POST_YEAR, "Pre-Reform", "Post-Reform"))]

  km_groups <- unique(sw_broader_sample$period_group)
  km_list <- lapply(km_groups, function(pg) {
    sub <- sw_broader_sample[period_group == pg]
    fit <- survfit(as.formula(sprintf(
      "Surv(surv_time, closure_event) ~ %s", wall_col)), data = sub)
    km_tidy <- as.data.table(broom::tidy(fit))
    km_tidy[, period_group := pg]
    km_tidy
  })

  km_dt <- rbindlist(km_list)
  km_dt[, period_group := factor(period_group, levels = c(
    "Control | Pre-Reform","Control | Post-Reform",
    "Texas | Pre-Reform","Texas | Post-Reform"))]

  p_km_h4 <- ggplot(km_dt,
    aes(x = time, y = estimate, color = strata, linetype = strata)) +
    geom_step(linewidth = 0.8) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata),
                alpha = 0.10, color = NA) +
    facet_wrap(~period_group, ncol = 2) +
    scale_color_manual(
      values = c("wall_col=0"="#0072B2","wall_col=1"="#D55E00"),
      labels = c("Double-wall","Single-wall"), name = NULL) +
    scale_linetype_manual(
      values = c("wall_col=0"="dashed","wall_col=1"="solid"),
      labels = c("Double-wall","Single-wall"), name = NULL) +
    scale_fill_manual(
      values = c("wall_col=0"="#0072B2","wall_col=1"="#D55E00"),
      labels = c("Double-wall","Single-wall"), name = NULL) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    labs(
      title    = "Figure H4.1: Kaplan-Meier Survival by Wall Type (H4 Test)",
      subtitle = "H4 prediction: SW-DW gap widens in TX post-1999 (bottom-right) only.",
      x = "Years in Panel", y = "Survival Pr(No Closure)",
      caption = "KM estimator. Broader SW/DW sample. Shading = 95% CI.") +
    theme_pub() +
    theme(strip.text = element_text(face = "bold", size = 10))

  ggsave(file.path(OUTPUT_FIGURES, "FigureH4_1_KM_WallType.png"),
         p_km_h4, width = 12, height = 9, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, "FigureH4_1_KM_WallType.pdf"),
         p_km_h4, width = 12, height = 9, device = cairo_pdf)
  cat("  Saved: FigureH4_1_KM_WallType\n")
}

cat("\n====================================================================\n")
cat("S8 COMPLETE\n")
cat("  S8a: Table_S8a_Survivorship_Diagnostic.csv\n")
cat("  S8b: FigureH1_1_BinnedScatter_AgeClosure\n")
cat("  S8c: FigureH1_2_Cox_AgeGradient_HR | Table_S8c_Cox_H1_AgeGradient.csv\n")
cat("  S8d: Table_S8d_H3_ClogLog_Duration.csv  [PRIMARY H3 TEST]\n")
cat("  S8e: FigureH3_1_KernelDensity_AgeAtClosure\n")
cat("  S8f: Table_S8f_Cox_H4_WallType.csv  [requires sw_broader_sample]\n")
cat("  S8g: FigureH4_1_KM_WallType         [requires sw_broader_sample]\n")
cat("====================================================================\n")


#==============================================================================
# S9: YOUNGEST SUBSAMPLE (Table 5 + Figure 7A)
#==============================================================================

cat("\n=== S9: YOUNGEST SUBSAMPLE (age <= 5 in 1998) ===\n")

m_youngest_simple <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  youngest_sample, cluster = ~state)
m_youngest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  youngest_sample, cluster = ~state)
m_youngest_age_hte <- feols(
  closure_event ~ did_term * age_bin | panel_id + panel_year,
  youngest_sample, cluster = ~state)

save_did_table(
  models    = list(m_youngest_simple, m_youngest_age_ctrl, m_youngest_age_hte),
  headers   = c("Simple DiD", "+ Age Control", "Age HTE"),
  base_name = "Table5_Youngest_MakeModel",
  title     = "Table 5: Youngest Subsample (age <= 5) -- H2 Young ATT")

model_es_youngest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  youngest_sample, cluster = ~state)

es_youngest_out <- plot_es(
  model_es_youngest,
  title       = "Youngest Facilities (age \u2264 5 in 1998)",
  subtitle    = sprintf("Lifecycle acceleration | Pre-trend p = %.3f",
                        pt_pval(model_es_youngest)),
  xlim_lo     = rel_min_youngest, xlim_hi = rel_max_full)

cat(sprintf("  Youngest DiD: beta=%.5f | p=%.4f\n",
  extract_did(m_youngest_simple)$beta, extract_did(m_youngest_simple)$p))


#==============================================================================
# S10: OLDEST SUBSAMPLE (Table 6 + Figure 7B)
#==============================================================================

cat("\n=== S10: OLDEST SUBSAMPLE (age > 5 in 1998) ===\n")

m_oldest_simple <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  oldest_sample, cluster = ~state)
m_oldest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  oldest_sample, cluster = ~state)

# Age HTE omitted for oldest subsample -- near-empty baseline bin creates
# mechanical collinearity (t-stats of 55-77 are a data artifact, not real)
cat("  NOTE: Age HTE suppressed (collinearity in baseline age bin).\n")

save_did_table(
  models    = list(m_oldest_simple, m_oldest_age_ctrl),
  headers   = c("Simple DiD", "+ Age Control"),
  base_name = "Table6_Oldest_MakeModel",
  title     = "Table 6: Oldest Subsample (age > 5) -- H2 Old ATT")

model_es_oldest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  oldest_sample, cluster = ~state)

es_oldest_out <- plot_es(
  model_es_oldest,
  title    = "Oldest Facilities (age > 5 in 1998)",
  subtitle = sprintf("Near-end-of-life | Pre-trend p = %.3f",
                     pt_pval(model_es_oldest)),
  xlim_lo  = rel_min_oldest, xlim_hi = rel_max_full)

cat(sprintf("  Oldest DiD: beta=%.5f | p=%.4f\n",
  extract_did(m_oldest_simple)$beta, extract_did(m_oldest_simple)$p))

p_combined_es <- (es_youngest_out$plot + labs(title = "A: Youngest (age \u2264 5)")) /
                 (es_oldest_out$plot   + labs(title = "B: Oldest (age > 5)")) +
  plot_annotation(
    title    = "Figure 7: HTE Event Studies -- Youngest vs. Oldest",
    subtitle = "Make-model sample. Sustained effect (A) vs. front-loaded effect (B).",
    theme    = theme(plot.title = element_text(face = "bold", size = 14)))

ggsave(file.path(OUTPUT_FIGURES, "Figure_7_ES_Youngest_Oldest.png"),
       p_combined_es, width = 14, height = 14, dpi = 200, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_7_ES_Youngest_Oldest.pdf"),
       p_combined_es, width = 14, height = 14, device = cairo_pdf)


#==============================================================================
# S11: REPORTED LEAK DiD (Table 7)
#==============================================================================

cat("\n=== S11: REPORTED LEAK DiD ===\n")

m_leak_main     <- feols(leak_year ~ did_term | panel_id + panel_year,
                         main_sample,     cluster = ~state)
m_leak_youngest <- feols(leak_year ~ did_term | panel_id + panel_year,
                         youngest_sample, cluster = ~state)
m_leak_oldest   <- feols(leak_year ~ did_term | panel_id + panel_year,
                         oldest_sample,   cluster = ~state)

save_did_table(
  models    = list(m_leak_main, m_leak_youngest, m_leak_oldest),
  headers   = c("Full Make-Model", "Youngest", "Oldest"),
  base_name = "Table7_Leak_MakeModel",
  title     = "Table 7: Reported Leak Probability -- Make-Model Sample")

model_es_leak <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state)

plot_es(model_es_leak,
  title    = "Effect of Insurance Privatization on Reported Leak Probability",
  subtitle = sprintf("Make-Model | Pre-trend p = %.3f", pt_pval(model_es_leak)),
  ylab     = "Effect on Pr(Reported Leak)",
  xlim_lo  = -8, xlim_hi = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "Figure_Leak_ES_MakeModel.png"))


#==============================================================================
# S12: H4 -- WALL TYPE ON BROADER SW SAMPLE (Table 9)
# NOTE: H4 cannot be tested in main_sample (SW-only by construction).
#==============================================================================

cat("\n=== S12: H4 WALL TYPE (Broader SW Sample) ===\n")
cat(sprintf("  Wall type variable: %s\n", wall_col))

fml_wall_broad <- as.formula(sprintf(
  "closure_event ~ did_term + did_term:%s + %s | panel_id + panel_year",
  wall_col, wall_col))
m_hte_wall_broad <- feols(fml_wall_broad, sw_broader_sample, cluster = ~state)

sw_broad_pre <- sw_broader_sample[panel_year < POST_YEAR]
m_h4_pre <- feols(as.formula(sprintf(
  "closure_event ~ texas_treated + texas_treated:%s + %s | panel_id + panel_year",
  wall_col, wall_col)), sw_broad_pre, cluster = ~state)

save_did_table(
  models    = list(m_hte_wall_broad, m_h4_pre),
  headers   = c("Broader SW Sample", "Pre-Period Falsification"),
  base_name = "Table9_H4_WallType_BroaderSW",
  title     = "Table 9: H4 Wall Type (Broader SW Sample)")

h4_term <- grep(paste0("did_term:", wall_col), names(coef(m_hte_wall_broad)), value = TRUE)
h4_coef <- coef(m_hte_wall_broad)[h4_term]
cat(sprintf("  H4 TX x Post x %s = %.5f | %s\n", wall_col, h4_coef,
  fifelse(!is.na(h4_coef) & h4_coef > 0, "CONSISTENT WITH H4", "Null/negative H4")))


#==============================================================================
# S13: H3 -- AGE AT CLOSURE OLS (Table 8 + Figure H3)
#
# NOTE: This is a DESCRIPTIVE analysis. OLS on closed_tanks conditions on
# the closure event having occurred, introducing selection bias. The primary
# causal H3 test is in S8d (duration model on full at-risk panel).
# Results here should be interpreted as "among closures that occur, does the
# average age shift?" not as "does the reform cause older tanks to close?"
#==============================================================================

cat("\n=== S13: H3 AGE AT CLOSURE (DESCRIPTIVE OLS) ===\n")
cat("  NOTE: Primary H3 causal test is in S8d. This section is descriptive.\n")

if (!"spec_A_eligible" %in% names(closed_tanks)) {
  closed_tanks <- merge(closed_tanks,
    unique(annual_data[, .(panel_id, spec_A_eligible)]),
    by = "panel_id", all.x = TRUE)
  closed_tanks[is.na(spec_A_eligible), spec_A_eligible := 0L]
}

sw_flag_1998 <- unique(annual_data[panel_year == TREATMENT_YEAR - 1L,
  .(panel_id, fac_all_sw_1998 = as.integer(get(wall_col) >= 0.5))])
if (!"fac_all_sw_1998" %in% names(closed_tanks))
  closed_tanks <- merge(closed_tanks, sw_flag_1998, by = "panel_id", all.x = TRUE)

mm_ids <- unique(main_sample$panel_id)
closed_tanks[, in_make_model := as.integer(panel_id %in% mm_ids)]

h3_data <- closed_tanks[
  !is.na(age_at_closure) & !is.na(county_fips_fac) &
  spec_A_eligible == 1   &
  closure_year %between% c(PANEL_START, PANEL_END)
][, `:=`(
  texas      = as.integer(state == "TX"),
  post       = as.integer(closure_year >= POST_YEAR),
  texas_post = as.integer(state == "TX") * as.integer(closure_year >= POST_YEAR)
)]

h3_mm    <- h3_data[in_make_model == 1]
h3_specA <- h3_data

cat(sprintf("  H3 make-model: %s closures | H3 Spec A: %s closures\n",
  format(nrow(h3_mm), big.mark = ","), format(nrow(h3_specA), big.mark = ",")))

m_h3_mm    <- feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
                    h3_mm, cluster = ~state)
m_h3_specA <- feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
                    h3_specA, cluster = ~state)
m_h3_pre   <- feols(age_at_closure ~ texas | county_fips_fac + closure_year,
                    h3_specA[post == 0], cluster = ~state)

save_did_table(
  models    = list(m_h3_mm, m_h3_specA, m_h3_pre),
  headers   = c("Make-Model Closures", "Spec A Closures", "Pre-Period Falsification"),
  base_name = "Table8_H3_AgeAtClosure",
  title     = "Table 8: H3 -- Age at Closure (Descriptive OLS -- see S8d for causal test)",
  tvar      = "texas_post")

age_ts <- closed_tanks[
  spec_A_eligible == 1 & closure_year %between% c(1990, 2018) & !is.na(age_at_closure),
  .(mean_age = mean(age_at_closure, na.rm = TRUE), n = .N),
  by = .(closure_year, group = fifelse(state == "TX", "Texas", "Control"))
]
setorder(age_ts, group, closure_year)
age_ts[, mean_age_smooth := frollmean(mean_age, n = 3, align = "center"), by = group]

p_h3 <- ggplot(age_ts, aes(x = closure_year, y = mean_age,
                            color = group, shape = group)) +
  geom_vline(xintercept = POST_YEAR - 0.5, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_line(aes(y = mean_age_smooth), linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = c(Texas = COL_TX, Control = COL_CTRL)) +
  scale_x_continuous(breaks = seq(1990, 2018, 4)) +
  labs(title    = "H3: Mean Age at Tank Closure Over Time",
       subtitle = "3-year rolling mean. Spec A. Post-reform TX rise = H3 (descriptive).",
       x = "Year of Closure", y = "Mean Tank Age at Closure (years)",
       color = NULL, shape = NULL)

ggsave(file.path(OUTPUT_FIGURES, "Figure_H3_AgeAtClosure.png"),
       p_h3, width = 10, height = 6, dpi = 300, bg = "white")


#==============================================================================
# S14: THEORY-EVIDENCE SUMMARY (Table 10)
#==============================================================================

cat("\n=== S14: THEORY-EVIDENCE SUMMARY ===\n")

extract_h <- function(m, pattern, label, prediction, null_expected = FALSE) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  if (length(idx) == 0)
    return(data.table(Hypothesis = label, Prediction = prediction,
                      Estimate = NA, SE = NA, P = NA, Verdict = "Term not found"))
  r <- ct[tail(idx, 1), ]
  p <- r["Pr(>|t|)"]
  verdict <- if (null_expected) {
    fifelse(p > 0.10, "Supported (null as expected)", "FAILED (pre-trend)")
  } else {
    fcase(is.na(p),                     "N/A",
          p < 0.05 & r["Estimate"] > 0, "Supported",
          p >= 0.10,                     "Not rejected (null)",
          default =                      "Partial")
  }
  data.table(Hypothesis = label, Prediction = prediction,
             Estimate = round(r["Estimate"],   5),
             SE       = round(r["Std. Error"], 5),
             P        = round(p, 4),
             Verdict  = verdict)
}

theory_tbl <- rbindlist(list(
  # H1/H2: age gradient -- use 4-bin HTE for oldest bin
  extract_h(m_did_4bin_hte, "did_term:age_treat_bin",
            "H1/H2 Age Gradient (4-bin HTE, oldest bin)",
            "ATT rising in age; oldest bin largest"),
  # H2: Young vs Old ATT
  extract_h(m_youngest_simple, "did_term",
            "H2 ATT(young, age<=5)",
            "ATT(young) ≈ 0 or small positive"),
  extract_h(m_oldest_simple, "did_term",
            "H2 ATT(old, age>5)",
            "ATT(old) > ATT(young)"),
  # H3: Age at closure (OLS descriptive)
  extract_h(m_h3_mm, "texas_post",
            "H3 Age-at-Closure Shift (make-model OLS, descriptive)",
            "TX post-reform closures older"),
  extract_h(m_h3_pre, "texas",
            "H3 Falsification (pre-period)",
            "No TX-control age gap pre-reform", null_expected = TRUE),
  # H4: Wall type
  extract_h(m_hte_wall_broad, paste0("did_term:", wall_col),
            "H4 Wall Sensitivity (broader SW sample)",
            "SW responds more than DW")
))

print(theory_tbl)
fwrite(theory_tbl, file.path(OUTPUT_TABLES, "Table10_Theory_Evidence_MakeModel.csv"))


#==============================================================================
# S15: ROBUSTNESS
#==============================================================================

cat("\n=== S15: ROBUSTNESS ===\n")

m_noMD_main     <- feols(closure_event ~ did_term | panel_id + panel_year,
                         main_sample[state != "MD"],     cluster = ~state)
m_noMD_youngest <- feols(closure_event ~ did_term | panel_id + panel_year,
                         youngest_sample[state != "MD"], cluster = ~state)
m_noMD_oldest   <- feols(closure_event ~ did_term | panel_id + panel_year,
                         oldest_sample[state != "MD"],   cluster = ~state)

save_did_table(
  models    = list(m_noMD_main, m_noMD_youngest, m_noMD_oldest),
  headers   = c("Full Make-Model (no MD)", "Youngest (no MD)", "Oldest (no MD)"),
  base_name = "TableB5_MD_Excluded_MakeModel",
  title     = "Table B.5: MD-Excluded Robustness")

m_mandate_main <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  main_sample, cluster = ~state)
m_mandate_youngest <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  youngest_sample, cluster = ~state)

save_did_table(
  models    = list(m_did_pooled, m_mandate_main, m_youngest_simple, m_mandate_youngest),
  headers   = c("Main (no mandate)", "Main + mandate", "Youngest", "Youngest + mandate"),
  base_name = "TableB6_Mandate_Sensitivity_MakeModel",
  title     = "Table B.6: Mandate Control Sensitivity")

main_tight <- annual_data[
  single_tanks == active_tanks & has_gasoline_year == 1 &
  install_year %between% c(1992L, 1997L)]
main_tight[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -7L)]

m_tight <- feols(closure_event ~ did_term | panel_id + panel_year,
                 main_tight, cluster = ~state)

save_did_table(
  models    = list(m_did_pooled, m_tight),
  headers   = c("1990-1997 install (primary)", "1992-1997 install (tight)"),
  base_name = "TableB7_InstallWindow_Sensitivity",
  title     = "Table B.7: Install Year Window Sensitivity")


#==============================================================================
# S16: SURVIVAL MODELS -- BASIC COX DiD
# Detailed H1/H4 Cox with interactions in S8c/S8f.
# This section: simple did_term HR on main_sample for robustness comparison.
#==============================================================================

cat("\n=== S16: COX DiD (Make-Model Sample) ===\n")

cox_main <- copy(main_sample)
cox_main[, `:=`(tstart    = panel_year - 1L,
                tstop     = panel_year,
                age_start = avg_tank_age - 1,
                age_stop  = avg_tank_age)]
cox_main <- cox_main[tstop > tstart]

m_cox_main_cal <- coxph(
  Surv(tstart, tstop, closure_event) ~ did_term + mandate_active,
  data = cox_main, cluster = state)

cox_main_age <- cox_main[age_start >= 0 & age_stop > age_start]
m_cox_main_age <- coxph(
  Surv(age_start, age_stop, closure_event) ~ did_term + mandate_active + panel_year,
  data = cox_main_age, cluster = state)

cox_results <- rbindlist(lapply(list(
  list(m = m_cox_main_cal, label = "Calendar Origin"),
  list(m = m_cox_main_age, label = "Age Origin")
), function(x) {
  s <- summary(x$m)$coefficients
  data.table(Model    = x$label,
             HR       = round(exp(s["did_term","coef"]),       4),
             coef     = round(s["did_term","coef"],            4),
             se       = round(s["did_term","se(coef)"],        4),
             p        = round(s["did_term","Pr(>|z|)"],        4),
             n_events = x$m$nevent)
}))

print(cox_results)
fwrite(cox_results, file.path(OUTPUT_TABLES, "Table_Cox_DiD_MakeModel.csv"))
cat("  NOTE: H1 age-gradient Cox in S8c | H4 wall-type Cox in S8f\n")


#==============================================================================
# S17: DIAGNOSTIC DATA EXPORT
#==============================================================================

cat("\n=== S17: DIAGNOSTIC EXPORT ===\n")

saveRDS(model_es_youngest, file.path(ANALYSIS_DIR, "mm_youngest_event_study.rds"))
saveRDS(model_es_oldest,   file.path(ANALYSIS_DIR, "mm_oldest_event_study.rds"))
saveRDS(m_did_pooled,      file.path(ANALYSIS_DIR, "mm_headline_did_model.rds"))
saveRDS(pt_results,        file.path(ANALYSIS_DIR, "mm_pt_validation_results.rds"))
saveRDS(m_es_hte,          file.path(ANALYSIS_DIR, "mm_hte_event_study.rds"))

fwrite(texas_share_by_period(main_sample),
  file.path(OUTPUT_TABLES, "Diag_TXShare_Main.csv"))
fwrite(texas_share_by_period(youngest_sample),
  file.path(OUTPUT_TABLES, "Diag_TXShare_Youngest.csv"))
fwrite(texas_share_by_period(oldest_sample),
  file.path(OUTPUT_TABLES, "Diag_TXShare_Oldest.csv"))

cat("  Exported: 5 model objects + 3 composition diagnostics\n")


#==============================================================================
# S18: PUBLICATION LaTeX TABLES
#==============================================================================

cat("\n=== S18: LaTeX TABLES ===\n")

write_tex <- function(lines, name) {
  writeLines(lines, file.path(OUTPUT_TABLES, paste0(name, ".tex")))
  cat(sprintf("  Saved: %s.tex\n", name))
}

# Cross-spec summary CSV (no LaTeX needed)
spec_summary <- data.table(
  Model = c("Make-Model Full","Youngest","Oldest",
            "Leak (Main)","Leak (Youngest)","Leak (Oldest)"),
  beta  = sapply(list(m_did_pooled, m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$beta),
  se    = sapply(list(m_did_pooled, m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$se),
  p     = sapply(list(m_did_pooled, m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$p))

print(spec_summary)
fwrite(spec_summary, file.path(OUTPUT_TABLES, "Cross_Spec_Summary_MakeModel.csv"))

# Age split table (youngest vs oldest)
cy <- lapply(list(m_youngest_simple, m_youngest_age_ctrl), extract_did)
co <- lapply(list(m_oldest_simple,   m_oldest_age_ctrl),   extract_did)

write_tex(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Age Heterogeneity -- Youngest vs.\\ Oldest Subsamples (H2)}",
  "\\label{tbl:mm_age_hte}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Youngest ($\\leq$5 yrs)}} & \\multicolumn{2}{c}{\\textbf{Oldest ($>$5 yrs)}}\\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4)\\\\ & Simple & +Age & Simple & +Age\\\\\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s\\\\",
    sprintf("%.4f", cy[[1]]$beta), stars_fn(cy[[1]]$p),
    sprintf("%.4f", cy[[2]]$beta), stars_fn(cy[[2]]$p),
    sprintf("%.4f", co[[1]]$beta), stars_fn(co[[1]]$p),
    sprintf("%.4f", co[[2]]$beta), stars_fn(co[[2]]$p)),
  sprintf(" & (%.4f) & (%.4f) & (%.4f) & (%.4f)\\\\",
    cy[[1]]$se, cy[[2]]$se, co[[1]]$se, co[[2]]$se),
  "\\midrule",
  "Age bin control & No & Yes & No & Yes\\\\",
  "Facility + Year FE & Yes & Yes & Yes & Yes\\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s\\\\",
    format(cy[[1]]$n,big.mark=","), format(cy[[2]]$n,big.mark=","),
    format(co[[1]]$n,big.mark=","), format(co[[2]]$n,big.mark=",")),
  "\\bottomrule",
  paste0("\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
    "Make-model sample split at mean tank age in 1998 = 5 years. ",
    "SE clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}"),
  "\\end{tabular}\\end{table}"
), "JMP_Table56_AgeSplit_MakeModel")

cat("\n====================================================================\n")
cat(sprintf("02_DiD_Main_MakeModel.R COMPLETE | %s\n", Sys.time()))
cat(sprintf("  Tables: %s\n  Figures: %s\n", OUTPUT_TABLES, OUTPUT_FIGURES))
cat("====================================================================\n")