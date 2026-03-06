#==============================================================================
# 02_DiD_Main_MakeModel.R
# Texas UST Insurance Reform -- Causal Evidence from DiD
#
# SAMPLE ARCHITECTURE:
#   Primary sample: Make-model restricted cohort
#     - Single-walled only (has_single_walled == 1)
#     - Motor fuel / gasoline only (has_gasoline_year == 1)
#     - Pure single-tank operators (single_tanks == active_tanks)
#     - Post-1989 install, <= 1997 (1990-1997 vintage)
#   Rationale: Within-cell comparison on wall type, fuel type, and tank count
#   eliminates composition-driven treatment effect heterogeneity. All facilities
#   in the sample share the same observable risk profile; the remaining source
#   of heterogeneity is age at treatment, which is the object of H1/H2.
#
#   NOTE on H4: The make-model restriction produces a wall-type-homogeneous
#   sample (all single-walled). H4 (wall sensitivity) is tested on the
#   broader single-walled sample as a robustness check -- see S10.
#
# IDENTIFICATION: Since all facilities are post-1989 vintage, the sample is
#   equivalent to Spec A (mandate-free). Primary FE is panel_id + panel_year.
#   No vintage-cohort x year FE required (degenerate -- single cohort bin).
#   mandate_active is included as a control in sensitivity specs.
#
# AGE HTE ARCHITECTURE (primary HTE):
#   Youngest (mean_age_1998 <= 5): lifecycle acceleration story
#   Oldest   (mean_age_1998 >  5): near-end-of-life acceleration story
#   These subsamples address within-cohort age imbalance (Texas skews younger)
#   and map directly to Propositions 2 and 3.
#
# OUTPUTS:
#   Table 3:   Headline DiD -- Main Make-Model Sample
#   Table 4:   Age Control & Age HTE (H1/H2) -- Full Make-Model Sample
#   Table 5:   Youngest Subsample DiD (H2: young ATT ≈ 0)
#   Table 6:   Oldest Subsample DiD  (H2: old ATT > 0)
#   Table 7:   Reported Leak DiD -- Make-Model Sample
#   Table 8:   H3: Age at Closure
#   Table 9:   H4: Wall Type (broader SW sample -- robustness)
#   Table 10:  Theory-Evidence Summary
#   Table B.4: Parallel Trends Validation
#   Table B.5: MD-Excluded Robustness
#   Figure 6:  Event Study -- Full Make-Model Sample
#   Figure 7:  Event Study -- Youngest vs Oldest (side-by-side)
#   Figure 8:  HTE Age Bin Plot
#   Figure 9:  Age Distribution at 1998 (diagnostic)
#   Figure H3: Age at Closure Time Series
#
# SECTIONS:
#   S1   Setup & Data Loading
#   S2   Helper Functions
#   S3   Sample Construction & Diagnostics
#   S4   Parallel Trends Validation
#   S5   Headline DiD (Table 3 + Figure 6)
#   S6   Age Control & HTE on Full Make-Model Sample (Table 4 + Figure 8)
#   S7   Youngest Subsample (Table 5 + Figure 7A)
#   S8   Oldest Subsample  (Table 6 + Figure 7B)
#   S9   Reported Leak DiD (Table 7)
#   S10  H4: Wall Type on Broader SW Sample (Table 9)
#   S11  H3: Age at Closure (Table 8 + Figure H3)
#   S12  Theory-Evidence Summary (Table 10)
#   S13  Robustness (Tables B.5)
#   S14  Survival Models (Cox DiD)
#   S15  Diagnostic Data Export
#   S16  Publication LaTeX Tables
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
ANALYSIS_DIR   <- here("Data", "Analysis")  # <-- ADDED: Define ANALYSIS_DIR early, matching 01_Descriptive_Analysis_2_24.r

meta <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
list2env(meta, envir = .GlobalEnv)

# ------------------------------------------------------------------
# FALLBACK: define any constants missing from metadata
# Adjust these values to match your study design if needed
# ------------------------------------------------------------------
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
# ------------------------------------------------------------------

meta <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
list2env(meta, envir = .GlobalEnv)

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

COL_TX      <- "#D55E00"
COL_CTRL    <- "#0072B2"
COL_YOUNG   <- "#009E73"
COL_OLD     <- "#CC79A7"

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

# vintage_cohort (kept for robustness specs and H4 broader sample)
if (!"vintage_cohort" %in% names(annual_data)) {

  # tanks_1999 may already have install_year parsed, or may have install_date
  if (!"install_year" %in% names(tanks_1999)) {
    if ("install_date" %in% names(tanks_1999)) {
      tanks_1999[, install_year := year(install_date)]
    } else {
      stop("tanks_1999 has neither 'install_year' nor 'install_date'. Check your data build.")
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

wall_col <- intersect(c("pct_single_wall","has_single_walled","any_single_walled"),
                      names(annual_data))[1]

# Event study relative year bounds -- set conservatively for the 1990-1997 cohort
# Pre-period is short (oldest install 1990, so max pre-period = 9 years before 1999)
# Post-period runs through end of panel
rel_min_full    <- 1990 - POST_YEAR   # = -9 (endpoint-binned in practice)
rel_max_full    <- ES_END - POST_YEAR
rel_min_youngest <- 1994 - POST_YEAR  # = -5
rel_min_oldest   <- -5L               # binned endpoint for thin early cells

cat(sprintf("Event study window: pre [%d, -1] post [1, %d]\n",
            rel_min_full, rel_max_full))

# Add to variable construction block (S1), after mean_age_1998 is available
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

# Texas share of estimation sample by event-study period (composition diagnostic)
texas_share_by_period <- function(dt, rel_year_col = "rel_year_bin",
                                  weight_col = NULL) {
  if (is.null(weight_col)) {
    dt[, .(texas_share = mean(texas_treated)), by = get(rel_year_col)]
  } else {
    dt[, .(texas_share = weighted.mean(texas_treated, get(weight_col))),
       by = get(rel_year_col)]
  }
}


#==============================================================================
# S3: SAMPLE CONSTRUCTION & DIAGNOSTICS
#==============================================================================

cat("\n=== S3: SAMPLE CONSTRUCTION ===\n")

#------------------------------------------------------------------------------
# PRIMARY SAMPLE: Make-model restricted
#   - Motor fuel only (has_gasoline_year == 1)
#   - Pure single-tank operators (single_tanks == active_tanks)
#   - Has single-walled tank (has_single_walled == 1) -- implied by above but
#     retained for clarity
#   - Post-1989 install, installed by 1997 (balanced pre-period)
#
# This gives us a same-cell comparison: every facility in the sample has
# identical observable risk profile on wall type, fuel type, and tank count.
# Remaining heterogeneity is age at treatment, which is the HTE object.
#------------------------------------------------------------------------------

main_sample <- annual_data[
  single_tanks  == active_tanks  &   # pure single-tank operators
  has_gasoline_year == 1         &   # motor fuel only
  install_year  >  1989          &   # post-mandate cohort
  install_year  <= 1997              # ensures meaningful pre-period
]

# Broader single-walled sample for H4 wall-type test (S10)
# Same vintage restriction but allows multi-tank and non-gasoline
sw_broader_sample <- annual_data[
  has_single_walled == 1         &
  install_year > 1989            &
  install_year <= 1997
]

# Age subsamples (primary HTE -- addresses within-cohort age imbalance)
# Texas is overrepresented in age <= 5 bins, underrepresented in > 5 bins
youngest_sample <- main_sample[mean_age_1998 <= 5]
oldest_sample   <- main_sample[mean_age_1998 >  5]

# Apply event study binning to all samples
main_sample[,     rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full),  -8L)]
youngest_sample[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full),  rel_min_youngest)]
oldest_sample[,   rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full),  rel_min_oldest)]
sw_broader_sample[,rel_year_bin:= pmax(pmin(rel_year_1999, rel_max_full),  -8L)]

cat(sprintf("  Main (make-model):     %s obs | %s facilities | %s TX | %s ctrl\n",
  format(nrow(main_sample),             big.mark = ","),
  format(uniqueN(main_sample$panel_id), big.mark = ","),
  format(uniqueN(main_sample[texas_treated == 1, panel_id]), big.mark = ","),
  format(uniqueN(main_sample[texas_treated == 0, panel_id]), big.mark = ",")))

cat(sprintf("  Youngest (age <= 5):   %s obs | %s facilities\n",
  format(nrow(youngest_sample),             big.mark = ","),
  format(uniqueN(youngest_sample$panel_id), big.mark = ",")))

cat(sprintf("  Oldest (age > 5):      %s obs | %s facilities\n",
  format(nrow(oldest_sample),             big.mark = ","),
  format(uniqueN(oldest_sample$panel_id), big.mark = ",")))

cat(sprintf("  Broader SW (H4):       %s obs | %s facilities\n",
  format(nrow(sw_broader_sample),             big.mark = ","),
  format(uniqueN(sw_broader_sample$panel_id), big.mark = ",")))

#------------------------------------------------------------------------------
# Diagnostic Figure: Age distribution at 1998 by Texas vs Control
# This motivates the youngest/oldest split and documents the within-cohort
# age imbalance that would contaminate a pooled event study
#------------------------------------------------------------------------------

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
  labs(
    title    = "Age Distribution at Treatment Date (1998)",
    subtitle = "Make-model sample: motor fuel, single-tank, single-walled, post-1989",
    x = "Mean Tank Age in 1998 (1-year bins)",
    y = "Share of Facilities",
    fill = NULL,
    caption = "Texas is overrepresented in young bins (1-4 yrs) and underrepresented in older bins (7-9 yrs).\nThis within-cohort age imbalance motivates the youngest/oldest split."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_9_Age_Distribution_1998.png"),
       p_age_dist, width = 10, height = 6, dpi = 300, bg = "white")

# Tabulate shares for the text
age_share_table <- dcast(age_dist_1998, age_bin_yr ~ group, value.var = "share")
cat("\n  Age share table at 1998 (Texas vs Control):\n")
print(age_share_table)
fwrite(age_share_table, file.path(OUTPUT_TABLES, "Diagnostic_Age_Shares_1998.csv"))

#==============================================================================
# Raw Closure Rates: Texas vs Control by Age Group
# Publication-quality version
#==============================================================================

# Compute cell means -- four trajectories matching the event study groups
raw_trends <- main_sample[
  panel_year %between% c(1992L, 2005L) & !is.na(mean_age_1998),
  .(closure_rate = mean(closure_event, na.rm = TRUE),
    n_fac        = uniqueN(panel_id)),
  by = .(
    panel_year,
    group   = fifelse(texas_treated == 1, "Texas", "Control"),
    age_grp = fifelse(mean_age_1998 <= 5, "Young (\u22645 yrs in 1998)",
                                          "Old (>5 yrs in 1998)")
  )
]

# Drop NA age_grp rows (facilities with missing mean_age_1998)
raw_trends <- raw_trends[!is.na(age_grp)]

# Factor ordering: Control before Texas (so legend reads Control / Texas)
raw_trends[, group   := factor(group,   levels = c("Control", "Texas"))]
raw_trends[, age_grp := factor(age_grp, levels = c("Young (\u22645 yrs in 1998)",
                                                    "Old (>5 yrs in 1998)"))]

# Colour + linetype scheme
# Colour  = Texas (orange) / Control (blue)  -- same as raw plot
# Linetype = Young (dashed) / Old (solid)    -- same as raw plot
col_vals  <- c(Control = "#0072B2", Texas = "#D55E00")
lty_vals  <- c("Young (\u22645 yrs in 1998)" = "dashed",
               "Old (>5 yrs in 1998)"        = "solid")
# Shape for points
shp_vals  <- c("Young (\u22645 yrs in 1998)" = 21,
               "Old (>5 yrs in 1998)"        = 19)

p_raw <- ggplot(
  raw_trends,
  aes(x        = panel_year,
      y        = closure_rate,
      color    = group,
      linetype = age_grp,
      shape    = age_grp)
) +
  # Pre-reform shading
  annotate("rect",
           xmin = -Inf, xmax = 1998.5,
           ymin = -Inf, ymax = Inf,
           fill = "grey92", alpha = 0.5) +
  # Reform line
  geom_vline(xintercept = 1998.5,
             linetype = "dashed", color = "grey30",
             linewidth = 0.6) +
  # Reform label
  annotate("text",
           x = 1998.7, y = Inf,
           label = "Reform\n(1999)",
           hjust = 0, vjust = 1.3,
           size = 3.2, color = "grey30") +
  # Lines + points
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2, fill = "white", stroke = 0.8) +
  # Scales
  scale_color_manual(
    values = col_vals,
    name   = NULL) +
  scale_linetype_manual(
    values = lty_vals,
    name   = NULL) +
  scale_shape_manual(
    values = shp_vals,
    name   = NULL) +
  scale_x_continuous(
    breaks = seq(1992, 2005, 2),
    expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.05))) +
  # Labels
  labs(
    title    = "Raw Closure Rates by State Group and Age at Treatment",
    subtitle = paste0(
      "Make-model sample (motor fuel, single-tank, single-walled, 1990\u20131997 install).\n",
      "Age split at mean tank age in 1998 = 5 years. Shaded region = pre-reform period."
    ),
    x        = "Year",
    y        = "Mean Pr(Closure)",
    caption  = "Note: Rates are unadjusted cell means. Facility and year fixed effects are not applied."
  ) +
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey30", size = 10,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(color = "grey50", size = 8,
                                    hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text        = element_text(size = 10),
    legend.position  = "bottom",
    legend.box       = "vertical",
    legend.margin    = margin(t = 2),
    legend.text      = element_text(size = 10),
    legend.key.width = unit(1.8, "cm")
  ) +
  # Merge color + linetype into one legend row each
  guides(
    color    = guide_legend(order = 1, nrow = 1,
                            override.aes = list(linewidth = 1.2)),
    linetype = guide_legend(order = 2, nrow = 1,
                            override.aes = list(linewidth = 1.2,
                                                color = "grey30")),
    shape    = guide_legend(order = 2, nrow = 1,
                            override.aes = list(color = "grey30"))
  )

ggsave(
  file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_OldYoung.png"),
  p_raw, width = 11, height = 6.5, dpi = 300, bg = "white"
)
ggsave(
  file.path(OUTPUT_FIGURES, "Figure_Raw_ClosureRates_OldYoung.pdf"),
  p_raw, width = 11, height = 6.5, device = cairo_pdf
)

cat("Saved: Figure_Raw_ClosureRates_OldYoung\n")


#==============================================================================
# S5 + S6: AGE-AT-TREATMENT HTE -- DiD, EVENT STUDY, TABLES
#
# STRUCTURE:
#   S5a  Binary old/young classification
#   S5b  Simple DiD by group (young ATT, old ATT, difference)
#   S5c  Triple-interaction event study (single model, pooled year FEs)
#   S5d  HTE plot function + Figure 6
#   S5e  Continuous age-bin HTE regression (Table 4)
#   S5f  Publication LaTeX tables
#   S6   Age-bin HTE coefficient plot (Figure 8)
#
# THEORY MAP:
#   Prop 3(i):  ATT(young) ≈ 0   -- base did_term coef (old_at_treat=0)
#   Prop 3(ii): ATT(old)   > 0   -- did_term + did_term:old_at_treat
#   HTE test:   interaction on old_at_treat > 0 and rising post-reform
#
# MODEL STRUCTURE:
#   i(rel_year_bin, texas_treated, ref=-1)  = year-by-year ATT for YOUNG
#       coef names: rel_year_bin::<t>:texas_treated
#   i(rel_year_bin, tx_old,        ref=-1)  = DIFFERENTIAL for OLD vs YOUNG
#       coef names: rel_year_bin::<t>:tx_old
#   tx_old = texas_treated * old_at_treat  (pre-computed -- fixest i() needs plain var)
#   Old path in plot = texas_treated coefs + tx_old coefs
#==============================================================================


#==============================================================================
# S5a: BINARY OLD/YOUNG CLASSIFICATION
#==============================================================================

cat("\n=== S5a: OLD/YOUNG CLASSIFICATION ===\n")

# Time-invariant binary: 1 if mean_age_1998 > 5
# Main effect absorbed by panel_id FE -- only interactions identified
main_sample[, old_at_treat := as.integer(mean_age_1998 > 5)]

# Pre-compute product for fixest i() -- i() requires a plain column, not an expression
main_sample[, tx_old := texas_treated * old_at_treat]

cat(sprintf("  Old (>5 yrs in 1998):    %s facilities\n",
  format(uniqueN(main_sample[old_at_treat == 1, panel_id]), big.mark = ",")))
cat(sprintf("  Young (<=5 yrs in 1998): %s facilities\n",
  format(uniqueN(main_sample[old_at_treat == 0, panel_id]), big.mark = ",")))
cat(sprintf("  Texas share -- Old: %.1f%% | Young: %.1f%%\n",
  100 * mean(main_sample[old_at_treat == 1 & panel_year == 1998, texas_treated]),
  100 * mean(main_sample[old_at_treat == 0 & panel_year == 1998, texas_treated])))


#==============================================================================
# S5b: SIMPLE DiD BY GROUP
# Young-only = ATT(old_at_treat == 0)  --> Prop 3(i): should be ≈ 0
# Old-only   = ATT(old_at_treat == 1)  --> Prop 3(ii): should be > 0
# Pooled interaction = single-model equivalent of the same comparison
#==============================================================================

cat("\n=== S5b: SIMPLE DiD BY GROUP ===\n")

m_did_young <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample[old_at_treat == 0], cluster = ~state
)

m_did_old <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample[old_at_treat == 1], cluster = ~state
)

# Single model: base coef = young ATT; interaction = old-young differential
m_did_interact <- feols(
  closure_event ~ did_term + did_term:old_at_treat | panel_id + panel_year,
  main_sample, cluster = ~state
)

# Same + age_bin to absorb within-panel aging trend
m_did_interact_agectrl <- feols(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state
)

d_young <- extract_did(m_did_young)
d_old   <- extract_did(m_did_old)

pre_mean_young <- mean(
  main_sample[old_at_treat == 0 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)
pre_mean_old <- mean(
  main_sample[old_at_treat == 1 & panel_year < TREATMENT_YEAR, closure_event],
  na.rm = TRUE)

cat(sprintf("  ATT(young): %.5f (SE=%.5f, p=%.4f) | pre-mean=%.4f | pct=%.1f%%\n",
  d_young$beta, d_young$se, d_young$p,
  pre_mean_young, 100 * d_young$beta / pre_mean_young))
cat(sprintf("  ATT(old):   %.5f (SE=%.5f, p=%.4f) | pre-mean=%.4f | pct=%.1f%%\n",
  d_old$beta, d_old$se, d_old$p,
  pre_mean_old, 100 * d_old$beta / pre_mean_old))
cat(sprintf("  Difference (old - young): %.5f\n",  d_old$beta - d_young$beta))
cat(sprintf("  Consistent with Prop 3:   %s\n",
  fifelse(d_old$beta > d_young$beta, "YES -- old > young", "NO")))


#==============================================================================
# S5c: TRIPLE-INTERACTION EVENT STUDY
#
# Two i() calls in one feols:
#   i(rel_year_bin, texas_treated, ref=-1) -- young path (tx_old=0)
#   i(rel_year_bin, tx_old,        ref=-1) -- differential (old - young)
#
# tx_old = texas_treated * old_at_treat is pre-computed above (S5a).
# fixest::i() requires a plain column name in the var argument,
# not an inline expression like texas_treated:old_at_treat.
#
# Coefficient naming by fixest:
#   Young path:        rel_year_bin::<t>:texas_treated
#   Differential:      rel_year_bin::<t>:tx_old
#==============================================================================

cat("\n=== S5c: TRIPLE-INTERACTION EVENT STUDY ===\n")

main_sample[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -8L)]

m_es_hte <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state
)

# Verify coefficient names match expected pattern before plotting
cn <- names(coef(m_es_hte))
n_young_coefs <- sum(grepl(":texas_treated$", cn))
n_old_coefs   <- sum(grepl(":tx_old$",        cn))
cat(sprintf("  Young path coefs (texas_treated): %d\n", n_young_coefs))
cat(sprintf("  Differential coefs (tx_old):      %d\n", n_old_coefs))
cat(sprintf("  Coef counts match:                %s\n",
  fifelse(n_young_coefs == n_old_coefs, "YES", "CHECK -- mismatch")))


#==============================================================================
# S5d: HTE PLOT FUNCTION + FIGURE 6
#
# Young path:  grep ":texas_treated$" from tidy() output
# Differential: grep ":tx_old$"
# Old path:    young + differential (SE propagated as sqrt(se1^2 + se2^2))
#==============================================================================

cat("\n=== S5d: HTE EVENT STUDY PLOT ===\n")

plot_es_hte <- function(model,
                        title          = "",
                        subtitle_extra = NULL,
                        filename       = NULL,
                        xlim_lo        = -8,
                        xlim_hi        = NULL,
                        col_young      = COL_YOUNG,
                        col_old        = COL_OLD) {

  ct <- as.data.table(tidy(model, conf.int = TRUE))

  # Young path: rel_year_bin::<t>:texas_treated  (base, tx_old = 0)
  young <- ct[grepl(":texas_treated$", term) & grepl("rel_year_bin", term)]
  young[, rel_year := as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", term))]
  young[, group    := "Young (\u22645 yrs in 1998)"]
  young[, `:=`(conf.low  = estimate - 1.96 * std.error,
               conf.high = estimate + 1.96 * std.error)]

  # Differential: rel_year_bin::<t>:tx_old
  diff_dt <- ct[grepl(":tx_old$", term) & grepl("rel_year_bin", term)]
  diff_dt[, rel_year := as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", term))]

  # Old path = young base + differential
  # SE propagated properly: sqrt(se_young^2 + se_diff^2)
  old <- merge(
    young[,   .(rel_year, base = estimate, base_se = std.error)],
    diff_dt[, .(rel_year, diff = estimate, diff_se = std.error)],
    by = "rel_year", all.x = TRUE
  )
  old[is.na(diff), `:=`(diff = 0, diff_se = 0)]
  old[, `:=`(
    estimate  = base + diff,
    conf.low  = (base + diff) - 1.96 * sqrt(base_se^2 + diff_se^2),
    conf.high = (base + diff) + 1.96 * sqrt(base_se^2 + diff_se^2),
    group     = "Old (>5 yrs in 1998)"
  )]

  # Reference period rows (t = -1, normalised to zero)
  ref_rows <- data.table(
    rel_year  = -1L, estimate = 0, conf.low = 0, conf.high = 0,
    group = c("Young (\u22645 yrs in 1998)", "Old (>5 yrs in 1998)")
  )

  plot_dt <- rbind(
    young[, .(rel_year, estimate, conf.low, conf.high, group)],
    old[,   .(rel_year, estimate, conf.low, conf.high, group)],
    ref_rows, fill = TRUE
  )
  setorder(plot_dt, group, rel_year)

  sub <- "Single model | Pooled year FEs | Old path = base + differential"
  if (!is.null(subtitle_extra)) sub <- paste0(sub, "\n", subtitle_extra)

  xh <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(
    plot_dt[rel_year %between% c(xlim_lo, xh)],
    aes(x = rel_year, y = estimate, color = group, fill = group)
  ) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_fill_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title    = title,
      subtitle = sub,
      x        = "Years Relative to Treatment (1999)",
      y        = "\u0394Pr(Tank Closure)",
      color    = NULL, fill = NULL
    ) +
    theme_pub() +
    theme(legend.position   = "bottom",
          panel.grid.major.x = element_blank())

  if (!is.null(filename)) {
    ggsave(filename, p, width = 14, height = 7, dpi = 200, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p,
           width = 14, height = 7, device = cairo_pdf)
    cat(sprintf("  Saved: %s\n", basename(filename)))
  }

  invisible(list(plot = p, data = plot_dt))
}

att_line <- sprintf(
  "ATT(young) = %.4f%s  |  ATT(old) = %.4f%s  |  Difference = %.4f",
  d_young$beta, stars_fn(d_young$p),
  d_old$beta,   stars_fn(d_old$p),
  d_old$beta - d_young$beta)

es_hte_out <- plot_es_hte(
  model          = m_es_hte,
  title          = "Figure 6: HTE Event Study \u2014 Old vs. Young at Treatment (Single Model)",
  subtitle_extra = att_line,
  xlim_lo        = -8,
  xlim_hi        = rel_max_full,
  filename       = file.path(OUTPUT_FIGURES, "Figure_6_ES_HTE_OldYoung.png")
)


#==============================================================================
# S5e: FOUR-SPEC DiD TABLE (Table 4)
# Spec 1: Pooled DiD                         -- baseline
# Spec 2: + age_bin control                  -- absorbs within-panel aging
# Spec 3: Binary HTE (old/young) + age_bin   -- maps to event study
# Spec 4: Four-bin age_treat_bin HTE         -- monotonic rise (Prop 3)
#
# Spec 4 note: age_treat_bin main effects are time-invariant so panel_id FE
# absorbs them. The collinearity NOTE in output is expected and benign.
# Only did_term:age_treat_bin interactions are identified.
#==============================================================================

cat("\n=== S5e: FOUR-SPEC DiD TABLE (Table 4) ===\n")

m_did_pooled <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  main_sample, cluster = ~state
)

m_did_agectrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  main_sample, cluster = ~state
)

m_did_binary_hte <- feols(
  closure_event ~ did_term + did_term:old_at_treat + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state
)

m_did_4bin_hte <- feols(
  closure_event ~ did_term:age_treat_bin + age_bin |
    panel_id + panel_year,
  main_sample, cluster = ~state
)

# Print total ATT by bin for monotonicity check (Prop 3)
ct4          <- coeftable(summary(m_did_4bin_hte, cluster = ~state))
bin_labels_4 <- levels(main_sample$age_treat_bin)

cat("\n  Total ATT by age-at-treatment bin:\n")
ests_4bin <- sapply(bin_labels_4, function(bl) {
  idx <- grep(paste0("did_term:age_treat_bin", bl), rownames(ct4), fixed = TRUE)
  if (length(idx) > 0) {
    cat(sprintf("    %s: %.5f (p=%.4f)\n",
      bl, ct4[idx, "Estimate"], ct4[idx, "Pr(>|t|)"]))
    return(ct4[idx, "Estimate"])
  }
  cat(sprintf("    %s: NOT FOUND\n", bl))
  NA_real_
})
ests_4bin <- ests_4bin[!is.na(ests_4bin)]
cat(sprintf("  Monotonically rising: %s\n",
  fifelse(length(ests_4bin) > 1 & all(diff(ests_4bin) > 0), "YES", "NO")))

save_did_table(
  models    = list(m_did_pooled, m_did_agectrl, m_did_binary_hte, m_did_4bin_hte),
  headers   = c("Pooled DiD", "+ Age Control", "Binary HTE", "4-Bin HTE"),
  base_name = "Table4_AgeTreat_HTE_MakeModel",
  title     = "Table 4: Age-at-Treatment HTE -- Make-Model Sample (H1/H2)"
)


#==============================================================================
# S5f: PUBLICATION LaTeX TABLES
#==============================================================================

cat("\n=== S5f: LaTeX TABLES ===\n")

# Helper: safely pull one coefficient row from a model
pull_coef <- function(m, pattern) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  if (length(idx) == 0) return(list(b = "---", se = "---"))
  list(
    b  = sprintf("%.4f%s", ct[idx[1], "Estimate"], stars_fn(ct[idx[1], "Pr(>|t|)"])),
    se = sprintf("(%.4f)", ct[idx[1], "Std. Error"])
  )
}

# ---- Table 4 ----
specs       <- list(m_did_pooled, m_did_agectrl, m_did_binary_hte, m_did_4bin_hte)
spec_labels <- c("Pooled", "+Age Ctrl", "Binary HTE", "4-Bin HTE")
did_rows    <- lapply(specs, pull_coef, pattern = "^did_term$")
old_rows    <- lapply(specs, pull_coef, pattern = "did_term:old_at_treat")

writeLines(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Age-at-Treatment Heterogeneous Treatment Effects (H1/H2)}",
  "\\label{tbl:age_treat_hte}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4)\\\\",
  sprintf(" & %s \\\\", paste(spec_labels, collapse = " & ")),
  "\\midrule",
  "\\textit{Panel A: Base effect (Young group, age $\\leq5$ in 1998)} & & & & \\\\",
  sprintf("Texas $\\times$ Post & %s \\\\",
    paste(sapply(did_rows, `[[`, "b"),  collapse = " & ")),
  sprintf(" & %s \\\\",
    paste(sapply(did_rows, `[[`, "se"), collapse = " & ")),
  "\\addlinespace",
  "\\textit{Panel B: Differential for Old group ($>5$ yrs in 1998)} & & & & \\\\",
  sprintf("$\\times$ Old at treatment & %s \\\\",
    paste(sapply(old_rows, `[[`, "b"),  collapse = " & ")),
  sprintf(" & %s \\\\",
    paste(sapply(old_rows, `[[`, "se"), collapse = " & ")),
  "\\midrule",
  "Age control (panel bins) & No & Yes & Yes & Yes \\\\",
  "4-bin age-at-treatment HTE & No & No & No & Yes \\\\",
  "Facility FE & Yes & Yes & Yes & Yes \\\\",
  "Year FE & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(nobs(specs[[1]]), big.mark = ","),
    format(nobs(specs[[2]]), big.mark = ","),
    format(nobs(specs[[3]]), big.mark = ","),
    format(nobs(specs[[4]]), big.mark = ",")),
  "\\bottomrule",
  paste0(
    "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
    "Make-model sample (motor fuel, single-tank, single-walled, 1990--1997 install). ",
    "Panel A = ATT for young tanks (age $\\leq5$ in 1998). ",
    "Panel B = additional effect for old tanks; total ATT for old = Panel A + Panel B. ",
    "Age control = current-panel-age bin dummies. ",
    "Age-at-treatment main effects are time-invariant and absorbed by panel FE ",
    "(collinearity note in estimation output is expected and benign). ",
    "SE clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}"),
  "\\end{tabular}",
  "\\end{table}"
), file.path(OUTPUT_TABLES, "Table4_AgeTreat_HTE_MakeModel.tex"))

# ---- Table 3 ----
writeLines(c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Treatment Effect by Age at Reform: Old vs.\\ Young Facilities (H2)}",
  "\\label{tbl:old_young_did}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) \\\\",
  " & Young only & Old only & Interact & Interact+AgeCtrl \\\\",
  "\\midrule",
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
  sprintf("\\%% effect & %.1f\\%% & %.1f\\%% & \\multicolumn{2}{c}{---} \\\\",
    100 * d_young$beta / pre_mean_young,
    100 * d_old$beta   / pre_mean_old),
  "\\midrule",
  "Age control (panel bins) & No & No & No & Yes \\\\",
  "Facility + Year FE & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(d_young$n,                    big.mark = ","),
    format(d_old$n,                      big.mark = ","),
    format(nobs(m_did_interact),         big.mark = ","),
    format(nobs(m_did_interact_agectrl), big.mark = ",")),
  "\\bottomrule",
  paste0(
    "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:} ",
    "Make-model sample. Split at mean tank age in 1998 = 5 years. ",
    "Cols (1)--(2): separate regressions per subgroup. ",
    "Cols (3)--(4): pooled model with interaction; base = Young ATT, ",
    "interaction = additional effect for Old group. ",
    "Prop.\\ 3(i): ATT(young) $\\approx 0$. ",
    "Prop.\\ 3(ii): ATT(old) $> 0$. ",
    "SE clustered at state. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}"),
  "\\end{tabular}",
  "\\end{table}"
), file.path(OUTPUT_TABLES, "Table3_OldYoung_DiD_MakeModel.tex"))

cat("  Saved: Table3_OldYoung_DiD_MakeModel.tex\n")
cat("  Saved: Table4_AgeTreat_HTE_MakeModel.tex\n")

cat("\n====================================================================\n")
cat("S5 COMPLETE\n")
cat("  Figure 6: HTE event study (old vs young, single model)\n")
cat("  Table 3:  Old/young ATT summary (4 specs)\n")
cat("  Table 4:  4-bin age-at-treatment HTE (monotonicity)\n")
cat("====================================================================\n")


#==============================================================================
# S6: AGE-BIN HTE COEFFICIENT PLOT (Figure 8)
#
# Takes the four per-bin coefficients from m_did_4bin_hte and plots them
# with 95% CIs. Overlays a pre-period falsification where all interactions
# should be ≈ 0 (no differential trend pre-reform by age-at-treatment).
#
# build_hte_dt() is a general helper that:
#   1. Finds all interaction rows matching base_tvar x age_var
#   2. Adds back the base term if it exists (for binary models)
#   3. For the 4-bin spec (no base term) each coef is already the total ATT
#==============================================================================

cat("\n=== S6: AGE-BIN HTE COEFFICIENT PLOT (Figure 8) ===\n")

# Pre-period falsification
# did_term is 0 everywhere in the pre-period, so use texas_treated instead
main_pre_period <- main_sample[panel_year < POST_YEAR]

m_age_hte_pre <- feols(
  closure_event ~ texas_treated:age_treat_bin | panel_id + panel_year,
  main_pre_period, cluster = ~state
)

build_hte_dt <- function(m, base_tvar, label,
                         age_var    = "age_treat_bin",
                         bin_levels = levels(main_sample$age_treat_bin)) {

  ct <- as.data.table(tidy(m, conf.int = TRUE))

  # Interaction rows: contain both base_tvar and age_var in the term name
  int_rows <- ct[grepl(age_var,    term, fixed = TRUE) &
                 grepl(base_tvar,  term, fixed = TRUE)]
  int_rows[, bin := gsub(paste0(".*", age_var), "", term)]

  # Base term (exists in binary spec, absent in 4-bin spec)
  base_row <- ct[term == base_tvar]
  bc  <- if (nrow(base_row) > 0) base_row$estimate[1]  else 0
  bse <- if (nrow(base_row) > 0) base_row$std.error[1] else 0

  # Add base back to interactions when base term exists (binary HTE pattern)
  if (nrow(base_row) > 0 && nrow(int_rows) > 0) {
    int_rows[, `:=`(
      estimate  = estimate  + bc,
      conf.low  = conf.low  + bc,
      conf.high = conf.high + bc
    )]
  }

  int_rows <- int_rows[, .(bin, estimate, conf.low, conf.high)]
  int_rows[, `:=`(sample = label,
                  bin    = factor(bin, levels = bin_levels))]
  int_rows
}
# Pre-period falsification for age-bin HTE CANNOT be estimated as a
# static coefficient -- texas_treated and age_treat_bin are both
# time-invariant, so their interaction is absorbed by panel_id FE.
# Pre-trend validation is shown in Figure 6 (event study pre-period).
# Figure 8 shows only post-reform HTE coefficients.

hte_main <- build_hte_dt(
  m_did_4bin_hte,
  base_tvar = "did_term",
  label     = "Post-Reform (Make-Model)"
)

# Plot post-reform only
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
      "Monotonically rising effect = Prop 3. ",
      "Pre-trend validation shown in Figure 6 event study.\n",
      "Pre-period falsification not estimable: age-at-treatment is ",
      "time-invariant and absorbed by facility FE."
    ),
    x = "Mean Tank Age in 1998",
    y = "\u0394Pr(Closure) | Texas \u00d7 Post"
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "Figure_8_HTE_AgeBin_MakeModel.png"),
       p_hte, width = 11, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_8_HTE_AgeBin_MakeModel.pdf"),
       p_hte, width = 11, height = 6, device = cairo_pdf)

cat("  Saved: Figure_8_HTE_AgeBin_MakeModel\n")

cat("\n====================================================================\n")
cat("S6 COMPLETE\n")
cat("  Figure 8: Age-at-treatment bin HTE coefficient plot\n")
cat("====================================================================\n")


#==============================================================================
# S7: ROBUSTNESS -- AGE-GROUP-SPECIFIC YEAR FEs
#
# MOTIVATION (co-author critique):
#   The primary spec (S5c) uses shared panel_year FEs across all facilities.
#   This forces a single macro shock delta_t for each year, estimated as a
#   weighted average across Young and Old control facilities. But the raw
#   data shows Control Young and Control Old have very different pre-period
#   trajectories (Control Young: 0.72->0.006 vs Control Old: 0.13->0.016).
#   A shared year FE is not the right counterfactual for either group.
#
# TWO SOLUTIONS:
#   Method A -- Split sample (feols split = ~age_grp)
#     Each sub-model gets its OWN panel_year FEs from its own data.
#     Control Young anchors the Young event study.
#     Control Old  anchors the Old event study.
#     Independent error variances per group.
#     Direct output: fixest_multi object, each element is a standard feols.
#
#   Method B -- Group-time interacted FEs (panel_year^age_grp)
#     Single pooled model but year FEs are saturated by age group.
#     Mathematically equivalent to Method A under uniform error variance.
#     Reuses plot_es_hte() directly -- same coef structure as primary spec.
#
# WHAT TO EXPECT:
#   If the shared year FE assumption in S5c was not badly violated, all three
#   specs (primary, Method A, Method B) should tell the same qualitative story.
#   Divergence would indicate the control group temporal trends differ enough
#   that the shared FE was contaminating the primary estimates.
#==============================================================================

cat("\n=== S7: ROBUSTNESS -- AGE-GROUP-SPECIFIC YEAR FEs ===\n")

# age_grp column needed for split= argument and panel_year^age_grp FE
# Values must exactly match the raw_trends plot labels for interpretability
main_sample[, age_grp := fifelse(mean_age_1998 <= 5, "Young", "Old")]

cat(sprintf("  age_grp: Old = %s obs | Young = %s obs\n",
  format(nrow(main_sample[age_grp == "Old"]),   big.mark = ","),
  format(nrow(main_sample[age_grp == "Young"]), big.mark = ",")))


#------------------------------------------------------------------------------
# METHOD A: Split sample
# feols split = ~age_grp fits one model per level of age_grp.
# Each sub-model has its own panel_year FEs -- the correct counterfactual.
# Clustered at state level within each sub-model.
#------------------------------------------------------------------------------

cat("\n--- Method A: Split Sample ---\n")

models_split <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data    = main_sample,
  split   = ~age_grp,
  cluster = ~state
)

# fixest_multi: sub-models named by split variable value
# Print names to confirm structure
cat("  Sub-model names:\n")
print(names(models_split))

# Extract sub-models by name for downstream use
# fixest names them "sample.var: age_grp == <value>"
nm_old   <- grep("Old",   names(models_split), value = TRUE)
nm_young <- grep("Young", names(models_split), value = TRUE)

m_split_old   <- models_split[[nm_old]]
m_split_young <- models_split[[nm_young]]
# extract_did() won't work on split models -- no did_term
# Compute ATT as mean of post-period i() coefficients instead
extract_att_from_es <- function(m) {
  ct  <- coeftable(summary(m, cluster = ~state))
  # Post-period rows: rel_year_bin >= 0, texas_treated interaction
  idx <- grep("rel_year_bin::[^-][0-9]*:texas_treated", rownames(ct))
  if (length(idx) == 0) return(list(beta = NA, se = NA, p = NA, n = nobs(m)))
  list(
    beta = mean(ct[idx, "Estimate"],   na.rm = TRUE),
    se   = mean(ct[idx, "Std. Error"], na.rm = TRUE),
    p    = NA_real_,   # no single p for average -- see note
    n    = nobs(m)
  )
}

d_split_young <- extract_att_from_es(m_split_young)
d_split_old   <- extract_att_from_es(m_split_old)


cat(sprintf("  Method A ATT(young): %.5f (p=%.4f)\n",
  d_split_young$beta, d_split_young$p))
cat(sprintf("  Method A ATT(old):   %.5f (p=%.4f)\n",
  d_split_old$beta,   d_split_old$p))
cat(sprintf("  Consistent with Prop 3: %s\n",
  fifelse(d_split_old$beta > d_split_young$beta, "YES", "NO")))


#------------------------------------------------------------------------------
# Method A plot: extract tidy() from each sub-model, combine, plot
# Each sub-model's coefficients ARE the absolute ATT path for that group --
# no reconstruction needed (unlike the triple-interaction primary spec).
#------------------------------------------------------------------------------

plot_es_split <- function(m_young, m_old,
                          title          = "",
                          subtitle_extra = NULL,
                          filename       = NULL,
                          xlim_lo        = -8,
                          xlim_hi        = NULL,
                          col_young      = COL_YOUNG,
                          col_old        = COL_OLD) {

  # Extract and label each group's event study path
  extract_path <- function(m, grp_label) {
    ct <- as.data.table(tidy(m, conf.int = TRUE))
    ct <- ct[grepl("rel_year_bin", term)]
    ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+):.*", "\\1", term))]
    ct[, group    := grp_label]
    ct[, `:=`(conf.low  = estimate - 1.96 * std.error,
              conf.high = estimate + 1.96 * std.error)]
    ct[, .(rel_year, estimate, conf.low, conf.high, group)]
  }

  young_path <- extract_path(m_young, "Young (\u22645 yrs in 1998)")
  old_path   <- extract_path(m_old,   "Old (>5 yrs in 1998)")

  # Reference period at t = -1
  ref_rows <- data.table(
    rel_year  = -1L, estimate = 0, conf.low = 0, conf.high = 0,
    group = c("Young (\u22645 yrs in 1998)", "Old (>5 yrs in 1998)")
  )

  plot_dt <- rbind(young_path, old_path, ref_rows, fill = TRUE)
  setorder(plot_dt, group, rel_year)

  sub <- "Method A: Split sample | Group-specific year FEs | Correct counterfactual per group"
  if (!is.null(subtitle_extra)) sub <- paste0(sub, "\n", subtitle_extra)

  xh <- if (is.null(xlim_hi)) max(plot_dt$rel_year) else xlim_hi

  p <- ggplot(
    plot_dt[rel_year %between% c(xlim_lo, xh)],
    aes(x = rel_year, y = estimate, color = group, fill = group)
  ) +
    annotate("rect", xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax = Inf, fill = "grey92", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey30", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.8) +
    scale_color_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_fill_manual(values = c(
      "Young (\u22645 yrs in 1998)" = col_young,
      "Old (>5 yrs in 1998)"        = col_old)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title    = title,
      subtitle = sub,
      x        = "Years Relative to Treatment (1999)",
      y        = "\u0394Pr(Tank Closure)",
      color    = NULL, fill = NULL
    ) +
    theme_pub() +
    theme(legend.position   = "bottom",
          panel.grid.major.x = element_blank())

  if (!is.null(filename)) {
    ggsave(filename, p, width = 14, height = 7, dpi = 200, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), p,
           width = 14, height = 7, device = cairo_pdf)
    cat(sprintf("  Saved: %s\n", basename(filename)))
  }

  invisible(list(plot = p, data = plot_dt))
}

att_line_split <- sprintf(
  "ATT(young) = %.4f%s  |  ATT(old) = %.4f%s  |  Difference = %.4f",
  d_split_young$beta, stars_fn(d_split_young$p),
  d_split_old$beta,   stars_fn(d_split_old$p),
  d_split_old$beta - d_split_young$beta)

es_split_out <- plot_es_split(
  m_young        = m_split_young,
  m_old          = m_split_old,
  title          = "Figure B.1: Robustness -- Method A (Split Sample, Group-Specific Year FEs)",
  subtitle_extra = att_line_split,
  xlim_lo        = -8,
  xlim_hi        = rel_max_full,
  filename       = file.path(OUTPUT_FIGURES, "FigureB1_Robustness_MethodA_SplitSample.png")
)


#------------------------------------------------------------------------------
# METHOD B: Group-time interacted FEs (panel_year^age_grp)
# Single pooled model. Year FEs are estimated separately per age group.
# Same triple-interaction coef structure as primary spec (S5c) -- so
# plot_es_hte() works directly without modification.
# The only change vs primary spec: panel_year -> panel_year^age_grp
#------------------------------------------------------------------------------

cat("\n--- Method B: Group-Time Interacted FEs ---\n")

m_es_hte_grpfe <- feols(
  closure_event ~
    i(rel_year_bin, texas_treated, ref = -1) +
    i(rel_year_bin, tx_old,        ref = -1) |
    panel_id + panel_year^age_grp,   # <-- key change vs primary spec
  main_sample, cluster = ~state
)

# Verify coef structure is identical to primary spec
cn_b <- names(coef(m_es_hte_grpfe))
cat(sprintf("  Young path coefs: %d | Differential coefs: %d\n",
  sum(grepl(":texas_treated$", cn_b)),
  sum(grepl(":tx_old$",        cn_b))))

# DiD summary from Method B for comparison
# (no did_term in event study model -- compute from post-period coefs)
post_young_b <- mean(
  coef(m_es_hte_grpfe)[grepl(":texas_treated$", cn_b) &
                        grepl("rel_year_bin::[^-]", cn_b)],
  na.rm = TRUE)
post_old_b <- post_young_b + mean(
  coef(m_es_hte_grpfe)[grepl(":tx_old$", cn_b) &
                        grepl("rel_year_bin::[^-]", cn_b)],
  na.rm = TRUE)

cat(sprintf("  Method B avg post-period ATT(young): %.5f\n", post_young_b))
cat(sprintf("  Method B avg post-period ATT(old):   %.5f\n", post_old_b))
cat(sprintf("  Consistent with Prop 3:              %s\n",
  fifelse(post_old_b > post_young_b, "YES", "NO")))

# Plot using the existing plot_es_hte() -- coef structure is identical
es_hte_grpfe_out <- plot_es_hte(
  model          = m_es_hte_grpfe,
  title          = "Figure B.2: Robustness -- Method B (Group-Time Year FEs, Single Model)",
  subtitle_extra = sprintf(
    "Avg post ATT(young) = %.4f  |  Avg post ATT(old) = %.4f",
    post_young_b, post_old_b),
  xlim_lo        = -8,
  xlim_hi        = rel_max_full,
  filename       = file.path(OUTPUT_FIGURES, "FigureB2_Robustness_MethodB_GrpYearFE.png")
)


#------------------------------------------------------------------------------
# THREE-WAY COMPARISON: Primary vs Method A vs Method B
# Tabulate avg post-period ATT for each group under each spec.
# Qualitative agreement across all three = robust HTE result.
#------------------------------------------------------------------------------

cat("\n--- Three-way comparison ---\n")

# Primary spec post-period averages
post_young_primary <- mean(
  coef(m_es_hte)[grepl(":texas_treated$", names(coef(m_es_hte))) &
                 grepl("rel_year_bin::[^-]", names(coef(m_es_hte)))],
  na.rm = TRUE)
post_old_primary <- post_young_primary + mean(
  coef(m_es_hte)[grepl(":tx_old$", names(coef(m_es_hte))) &
                 grepl("rel_year_bin::[^-]", names(coef(m_es_hte)))],
  na.rm = TRUE)

comparison_tbl <- data.table(
  Spec        = c("Primary (shared year FE)",
                  "Method A (split sample)",
                  "Method B (group-time FE)"),
  ATT_young   = round(c(post_young_primary,
                        d_split_young$beta,
                        post_young_b), 5),
  ATT_old     = round(c(post_old_primary,
                        d_split_old$beta,
                        post_old_b), 5),
  Old_gt_Young = c(post_old_primary  > post_young_primary,
                   d_split_old$beta  > d_split_young$beta,
                   post_old_b        > post_young_b)
)

print(comparison_tbl)
fwrite(comparison_tbl,
  file.path(OUTPUT_TABLES, "TableB_Robustness_YearFE_Comparison.csv"))

# Side-by-side figure: primary vs Method A vs Method B
p_compare <- (es_hte_out$plot      + labs(title = "Primary (Shared Year FE)")) /
             (es_split_out$plot    + labs(title = "Method A (Split Sample)"))  /
             (es_hte_grpfe_out$plot + labs(title = "Method B (Group-Time FE)")) +
  plot_annotation(
    title    = "Figure B.3: Robustness -- Year FE Specification Comparison",
    subtitle = "All three specs should show Old > Young post-1999. Divergence indicates shared FE contamination.",
    theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                     plot.subtitle = element_text(size = 10))
  )

ggsave(file.path(OUTPUT_FIGURES, "FigureB3_Robustness_YearFE_Comparison.png"),
       p_compare, width = 14, height = 21, dpi = 200, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "FigureB3_Robustness_YearFE_Comparison.pdf"),
       p_compare, width = 14, height = 21, device = cairo_pdf)

cat("  Saved: FigureB3_Robustness_YearFE_Comparison\n")
cat("  Saved: TableB_Robustness_YearFE_Comparison.csv\n")

cat("\n====================================================================\n")
cat("S7 COMPLETE\n")
cat("  Figure B.1: Method A event study (split sample)\n")
cat("  Figure B.2: Method B event study (group-time year FEs)\n")
cat("  Figure B.3: Three-way comparison panel\n")
cat("  Table  B.*: ATT comparison across specs\n")
cat("====================================================================\n")


#==============================================================================
# S7: YOUNGEST SUBSAMPLE (Table 5 + Figure 7A)
# Theory: lifecycle acceleration -- treatment effect small but sustained
# H2: ATT(young) ≈ 0 but positive and persistent (Proposition 3)
#==============================================================================

cat("\n=== S7: YOUNGEST SUBSAMPLE (age <= 5 in 1998) ===\n")

m_youngest_simple <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  youngest_sample, cluster = ~state
)

m_youngest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  youngest_sample, cluster = ~state
)

# Age HTE within youngest: note bins will be narrow -- interpret cautiously
m_youngest_age_hte <- feols(
  closure_event ~ did_term * age_bin | panel_id + panel_year,
  youngest_sample, cluster = ~state
)

save_did_table(
  models  = list(m_youngest_simple, m_youngest_age_ctrl, m_youngest_age_hte),
  headers = c("Simple DiD", "+ Age Control", "Age HTE"),
  base_name = "Table5_Youngest_MakeModel",
  title     = "Table 5: Youngest Subsample (mean_age_1998 <= 5) -- H2 Young ATT")

# Event study -- youngest
model_es_youngest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  youngest_sample, cluster = ~state
)

es_youngest_out <- plot_es(
  model_es_youngest,
  title       = "Youngest Facilities (age \u2264 5 in 1998)",
  subtitle    = sprintf("Lifecycle acceleration | Pre-trend p = %.3f",
                        pt_pval(model_es_youngest)),
  xlim_lo     = rel_min_youngest,
  xlim_hi     = rel_max_full,
  pre_trend_p = pt_pval(model_es_youngest)
)

cat(sprintf("  Youngest DiD: beta = %.5f | p = %.4f\n",
  extract_did(m_youngest_simple)$beta,
  extract_did(m_youngest_simple)$p))


#==============================================================================
# S8: OLDEST SUBSAMPLE (Table 6 + Figure 7B)
# Theory: near-end-of-life acceleration -- effect front-loaded, fades post-12yr
# H2: ATT(old) > 0, concentrated in early post-period
#==============================================================================

cat("\n=== S8: OLDEST SUBSAMPLE (age > 5 in 1998) ===\n")

m_oldest_simple <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  oldest_sample, cluster = ~state
)

m_oldest_age_ctrl <- feols(
  closure_event ~ did_term + age_bin | panel_id + panel_year,
  oldest_sample, cluster = ~state
)

# NOTE: Age HTE omitted for oldest subsample.
# Near-empty baseline age bin creates mechanical collinearity producing
# t-statistics of 55-77. This is a data artifact, not a real effect.
# The simple DiD and age control specs are the appropriate estimators here.
cat("  NOTE: Age HTE suppressed for oldest subsample (collinearity in baseline bin).\n")

save_did_table(
  models  = list(m_oldest_simple, m_oldest_age_ctrl),
  headers = c("Simple DiD", "+ Age Control"),
  base_name = "Table6_Oldest_MakeModel",
  title     = "Table 6: Oldest Subsample (mean_age_1998 > 5) -- H2 Old ATT")

# Event study -- oldest
model_es_oldest <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  oldest_sample, cluster = ~state
)

es_oldest_out <- plot_es(
  model_es_oldest,
  title       = "Oldest Facilities (age > 5 in 1998)",
  subtitle    = sprintf("Near-end-of-life acceleration | Pre-trend p = %.3f",
                        pt_pval(model_es_oldest)),
  xlim_lo     = rel_min_oldest,
  xlim_hi     = rel_max_full,
  pre_trend_p = pt_pval(model_es_oldest)
)

cat(sprintf("  Oldest DiD: beta = %.5f | p = %.4f\n",
  extract_did(m_oldest_simple)$beta,
  extract_did(m_oldest_simple)$p))

#------------------------------------------------------------------------------
# Figure 7: Youngest + Oldest event studies side-by-side
#------------------------------------------------------------------------------

p_combined_es <- (es_youngest_out$plot + labs(title = "A: Youngest (age \u2264 5)")) /
                 (es_oldest_out$plot   + labs(title = "B: Oldest (age > 5)")) +
  plot_annotation(
    title    = "Figure 7: HTE Event Studies -- Youngest vs. Oldest Subsamples",
    subtitle = "Make-model sample. Sustained effect (A) vs. front-loaded fading effect (B).",
    theme    = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(file.path(OUTPUT_FIGURES, "Figure_7_ES_Youngest_Oldest.png"),
       p_combined_es, width = 14, height = 14, dpi = 200, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "Figure_7_ES_Youngest_Oldest.pdf"),
       p_combined_es, width = 14, height = 14, device = cairo_pdf)


#==============================================================================
# S9: REPORTED LEAK DiD (Table 7)
#==============================================================================

cat("\n=== S9: REPORTED LEAK DiD ===\n")

m_leak_main <- feols(
  leak_year ~ did_term | panel_id + panel_year,
  main_sample, cluster = ~state
)

m_leak_youngest <- feols(
  leak_year ~ did_term | panel_id + panel_year,
  youngest_sample, cluster = ~state
)

m_leak_oldest <- feols(
  leak_year ~ did_term | panel_id + panel_year,
  oldest_sample, cluster = ~state
)

save_did_table(
  models  = list(m_leak_main, m_leak_youngest, m_leak_oldest),
  headers = c("Full Make-Model", "Youngest", "Oldest"),
  base_name = "Table7_Leak_MakeModel",
  title     = "Table 7: Reported Leak Probability -- Make-Model Sample")

# Leak event study (full make-model)
model_es_leak <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  main_sample, cluster = ~state
)

plot_es(
  model_es_leak,
  title    = "Effect of Insurance Privatization on Reported Leak Probability",
  subtitle = sprintf("Make-Model Sample | Pre-trend p = %.3f",
                     pt_pval(model_es_leak)),
  ylab     = "Effect on Pr(Reported Leak)",
  xlim_lo  = -8,
  xlim_hi  = rel_max_full,
  filename = file.path(OUTPUT_FIGURES, "Figure_Leak_ES_MakeModel.png")
)

cat(sprintf("  Leak DiD (full): beta = %.5f | p = %.4f\n",
  extract_did(m_leak_main)$beta,
  extract_did(m_leak_main)$p))


#==============================================================================
# S10: H4 -- WALL TYPE ON BROADER SW SAMPLE (Table 9)
# NOTE: H4 cannot be tested within the make-model sample because it is
# wall-type homogeneous (all single-walled by construction). We test H4 on
# the broader single-walled sample, which retains variation on whether
# a facility has mixed or pure single-wall portfolio.
# This serves as a robustness check, not the primary identification test.
#==============================================================================

cat("\n=== S10: H4 WALL TYPE (Broader SW Sample) ===\n")
cat(sprintf("  Wall type variable: %s\n", wall_col))
cat("  NOTE: H4 tested on sw_broader_sample (multi-tank/non-gasoline allowed).\n")
cat("  The make-model sample is wall-homogeneous; H4 is not identified within it.\n")

fml_wall_broad <- as.formula(sprintf(
  "closure_event ~ did_term + did_term:%s + %s | panel_id + panel_year",
  wall_col, wall_col))

m_hte_wall_broad <- feols(fml_wall_broad, sw_broader_sample, cluster = ~state)

# Pre-period falsification for H4
sw_broad_pre <- sw_broader_sample[panel_year < POST_YEAR]
m_h4_pre <- feols(as.formula(sprintf(
  "closure_event ~ texas_treated + texas_treated:%s + %s | panel_id + panel_year",
  wall_col, wall_col)), sw_broad_pre, cluster = ~state)

save_did_table(
  models  = list(m_hte_wall_broad, m_h4_pre),
  headers = c("Broader SW Sample", "Pre-Period Falsification"),
  base_name = "Table9_H4_WallType_BroaderSW",
  title     = "Table 9: H4 Wall Type Sensitivity (Broader SW Sample -- Robustness)")

h4_term <- grep(paste0("did_term:", wall_col), names(coef(m_hte_wall_broad)), value = TRUE)
h4_coef <- coef(m_hte_wall_broad)[h4_term]
cat(sprintf("  H4 TX x Post x %s = %.5f | %s\n", wall_col, h4_coef,
  fifelse(!is.na(h4_coef) & h4_coef > 0,
          "CONSISTENT WITH H4", "Null/negative H4")))


#==============================================================================
# S11: H3 -- AGE AT CLOSURE RIGHTWARD SHIFT (Table 8 + Figure H3)
#==============================================================================

cat("\n=== S11: H3 AGE AT CLOSURE ===\n")

# Attach spec_A_eligible and SW flag to closed_tanks
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

# Tag make-model facilities in closed_tanks
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

# Primary H3: make-model closures (tightest comparison)
h3_mm <- h3_data[in_make_model == 1]

# Broader H3: all Spec A (as before, for comparability)
h3_specA <- h3_data

cat(sprintf("  H3 make-model: %s closures | H3 Spec A: %s closures\n",
  format(nrow(h3_mm),    big.mark = ","),
  format(nrow(h3_specA), big.mark = ",")))

m_h3_mm    <- feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
                    h3_mm, cluster = ~state)
m_h3_specA <- feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
                    h3_specA, cluster = ~state)
m_h3_pre   <- feols(age_at_closure ~ texas      | county_fips_fac + closure_year,
                    h3_specA[post == 0], cluster = ~state)

save_did_table(
  models  = list(m_h3_mm, m_h3_specA, m_h3_pre),
  headers = c("Make-Model Closures", "Spec A Closures", "Pre-Period Falsification"),
  base_name = "Table8_H3_AgeAtClosure",
  title     = "Table 8: H3 -- Rightward Shift in Age at Closure",
  tvar      = "texas_post")

# Figure H3
age_ts <- closed_tanks[
  spec_A_eligible == 1 & closure_year %between% c(1990, 2018) &
  !is.na(age_at_closure),
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
  labs(
    title    = "H3: Mean Age at Tank Closure Over Time",
    subtitle = "3-year rolling mean. Spec A facilities. Post-reform TX rise = H3.",
    x = "Year of Closure", y = "Mean Tank Age at Closure (years)",
    color = NULL, shape = NULL
  )

ggsave(file.path(OUTPUT_FIGURES, "Figure_H3_AgeAtClosure.png"),
       p_h3, width = 10, height = 6, dpi = 300, bg = "white")


#==============================================================================
# S12: THEORY-EVIDENCE SUMMARY (Table 10)
#==============================================================================

cat("\n=== S12: THEORY-EVIDENCE SUMMARY ===\n")

extract_h <- function(m, pattern, label, prediction, null_expected = FALSE) {
  ct  <- coeftable(summary(m, cluster = ~state))
  idx <- grep(pattern, rownames(ct))
  if (length(idx) == 0)
    return(data.table(Hypothesis = label, Prediction = prediction,
                      Estimate = NA, SE = NA, P = NA,
                      Verdict = "Term not found"))
  r <- ct[tail(idx, 1), ]
  p <- r["Pr(>|t|)"]
  verdict <- if (null_expected) {
    fifelse(p > 0.10, "Supported (null as expected)", "FAILED (pre-trend)")
  } else {
    fcase(is.na(p),               "N/A",
          p < 0.05 & r["Estimate"] > 0, "Supported",
          p >= 0.10,               "Not rejected (null)",
          default =                "Partial")
  }
  data.table(Hypothesis = label, Prediction = prediction,
             Estimate = round(r["Estimate"],    5),
             SE       = round(r["Std. Error"],  5),
             P        = round(p,                4),
             Verdict  = verdict)
}

theory_tbl <- rbindlist(list(
  # H1/H2: Age sensitivity -- make-model sample
extract_h(m_closure_age_hte, "did_term:age_treat_bin",
          "H1/H2 Age Sensitivity (Make-Model, oldest bin)",
          "TE rising in age; near-zero for young tanks"),

extract_h(m_age_hte_pre, "texas_treated:age_treat_bin",
          "H1/H2 Falsification (pre-period, make-model)",
          "All interactions ≈ 0", null_expected = TRUE),
            # H2: Youngest vs oldest headline DiD
  extract_h(m_youngest_simple, "did_term",
            "H2 Young ATT (make-model, age<=5)",
            "ATT(young) ≈ 0 or small positive"),
  extract_h(m_oldest_simple, "did_term",
            "H2 Old ATT (make-model, age>5)",
            "ATT(old) > ATT(young)"),
  # H3: Age at closure
  extract_h(m_h3_mm, "texas_post",
            "H3 Rightward Shift (make-model closures)",
            "TX post-reform closures older"),
  extract_h(m_h3_pre, "texas",
            "H3 Falsification (pre-period)",
            "No TX-control age gap pre-reform", null_expected = TRUE),
  # H4: Wall type (broader sample)
  extract_h(m_hte_wall_broad, paste0("did_term:", wall_col),
            "H4 Wall Sensitivity (broader SW sample)",
            "SW responds more than DW (robustness)")
))

print(theory_tbl)
fwrite(theory_tbl, file.path(OUTPUT_TABLES, "Table10_Theory_Evidence_MakeModel.csv"))


#==============================================================================
# S13: ROBUSTNESS
#==============================================================================

cat("\n=== S13: ROBUSTNESS ===\n")

# MD excluded (make-model sample)
m_noMD_main     <- feols(closure_event ~ did_term | panel_id + panel_year,
                         main_sample[state != "MD"], cluster = ~state)
m_noMD_youngest <- feols(closure_event ~ did_term | panel_id + panel_year,
                         youngest_sample[state != "MD"], cluster = ~state)
m_noMD_oldest   <- feols(closure_event ~ did_term | panel_id + panel_year,
                         oldest_sample[state != "MD"], cluster = ~state)

save_did_table(
  models  = list(m_noMD_main, m_noMD_youngest, m_noMD_oldest),
  headers = c("Full Make-Model (no MD)", "Youngest (no MD)", "Oldest (no MD)"),
  base_name = "TableB5_MD_Excluded_MakeModel",
  title     = "Table B.5: MD-Excluded Robustness -- Make-Model Sample")

# Mandate control sensitivity
m_mandate_main <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  main_sample, cluster = ~state)
m_mandate_youngest <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  youngest_sample, cluster = ~state)

save_did_table(
  models  = list(m_closure_main, m_mandate_main,
                 m_youngest_simple, m_mandate_youngest),
  headers = c("Main (no mandate)", "Main + mandate_active",
              "Youngest (no mandate)", "Youngest + mandate_active"),
  base_name = "TableB6_Mandate_Sensitivity_MakeModel",
  title     = "Table B.6: Mandate Control Sensitivity -- Make-Model Sample")

# Install year window sensitivity: restrict to 1992-1997 (tighter pre-period)
main_tight <- annual_data[
  single_tanks  == active_tanks  &
  has_gasoline_year == 1         &
  install_year %between% c(1992L, 1997L)
]
main_tight[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max_full), -7L)]

m_tight <- feols(closure_event ~ did_term | panel_id + panel_year,
                 main_tight, cluster = ~state)

save_did_table(
  models  = list(m_closure_main, m_tight),
  headers = c("1990-1997 install (primary)", "1992-1997 install (tight)"),
  base_name = "TableB7_InstallWindow_Sensitivity",
  title     = "Table B.7: Install Year Window Sensitivity")


#==============================================================================
# S14: SURVIVAL MODELS (Cox DiD) -- Make-Model Sample
#==============================================================================

cat("\n=== S14: COX DiD (Make-Model Sample) ===\n")

cox_main <- copy(main_sample)
cox_main[, `:=`(tstart    = panel_year - 1L,
                tstop     = panel_year,
                age_start = avg_tank_age - 1,
                age_stop  = avg_tank_age)]
cox_main <- cox_main[tstop > tstart]

# Calendar time origin
m_cox_main_cal <- coxph(
  Surv(tstart, tstop, closure_event) ~ did_term + mandate_active,
  data = cox_main, cluster = state)

# Age origin (facility age as time axis)
cox_main_age <- cox_main[age_start >= 0 & age_stop > age_start]
m_cox_main_age <- coxph(
  Surv(age_start, age_stop, closure_event) ~ did_term + mandate_active + panel_year,
  data = cox_main_age, cluster = state)

cox_results <- rbindlist(lapply(list(
  list(m = m_cox_main_cal, label = "Calendar Origin (Make-Model)"),
  list(m = m_cox_main_age, label = "Age Origin (Make-Model)")
), function(x) {
  s <- summary(x$m)$coefficients
  data.table(
    Model    = x$label,
    HR       = round(exp(s["did_term", "coef"]),   4),
    coef     = round(s["did_term", "coef"],         4),
    se       = round(s["did_term", "se(coef)"],     4),
    p        = round(s["did_term", "Pr(>|z|)"],     4),
    n_events = x$m$nevent
  )
}))

print(cox_results)
fwrite(cox_results, file.path(OUTPUT_TABLES, "Table_Cox_DiD_MakeModel.csv"))


#==============================================================================
# S15: DIAGNOSTIC DATA EXPORT
#==============================================================================

cat("\n=== S15: DIAGNOSTIC EXPORT ===\n")

saveRDS(model_es_main,     file.path(ANALYSIS_DIR, "mm_headline_event_study.rds"))
saveRDS(model_es_youngest, file.path(ANALYSIS_DIR, "mm_youngest_event_study.rds"))
saveRDS(model_es_oldest,   file.path(ANALYSIS_DIR, "mm_oldest_event_study.rds"))
saveRDS(m_closure_main,    file.path(ANALYSIS_DIR, "mm_headline_did_model.rds"))
saveRDS(pt_results,        file.path(ANALYSIS_DIR, "mm_pt_validation_results.rds"))

# Texas composition by period diagnostic
tx_share_main     <- texas_share_by_period(main_sample)
tx_share_youngest <- texas_share_by_period(youngest_sample)
tx_share_oldest   <- texas_share_by_period(oldest_sample)

fwrite(tx_share_main,     file.path(OUTPUT_TABLES, "Diag_TXShare_Main.csv"))
fwrite(tx_share_youngest, file.path(OUTPUT_TABLES, "Diag_TXShare_Youngest.csv"))
fwrite(tx_share_oldest,   file.path(OUTPUT_TABLES, "Diag_TXShare_Oldest.csv"))

cat("  Exported: 5 model objects + 3 composition diagnostics\n")


#==============================================================================
# S16: PUBLICATION LaTeX TABLES
#==============================================================================

cat("\n=== S16: LaTeX TABLES ===\n")

write_tex <- function(lines, name) {
  writeLines(lines, file.path(OUTPUT_TABLES, paste0(name, ".tex")))
  cat(sprintf("  Saved: %s.tex\n", name))
}

# Table 3: Headline make-model
c3 <- lapply(list(m_closure_main, m_closure_main_mandate,
                  m_exit_main, m_rep_main), extract_did)
write_tex(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Policy Effects -- Make-Model Sample (Motor Fuel, Single-Tank, SW, 1990--1997)}",
  "\\label{tbl:mm_headline}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Closure}} & \\textbf{Exit$|$Cls} & \\textbf{Replace$|$Cls}\\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-4}\\cmidrule(lr){5-5}",
  " & (1) & (2) & (3) & (4)\\\\\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s\\\\",
    sprintf("%.4f", c3[[1]]$beta), stars_fn(c3[[1]]$p),
    sprintf("%.4f", c3[[2]]$beta), stars_fn(c3[[2]]$p),
    sprintf("%.4f", c3[[3]]$beta), stars_fn(c3[[3]]$p),
    sprintf("%.4f", c3[[4]]$beta), stars_fn(c3[[4]]$p)),
  sprintf("& (%.4f) & (%.4f) & (%.4f) & (%.4f)\\\\",
    c3[[1]]$se, c3[[2]]$se, c3[[3]]$se, c3[[4]]$se),
  "\\midrule",
  "Mandate control & No & Yes & No & No\\\\",
  "Facility FE & Yes & Yes & Yes & Yes\\\\",
  "Year FE & Yes & Yes & Yes & Yes\\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s\\\\",
    format(c3[[1]]$n, big.mark=","), format(c3[[2]]$n, big.mark=","),
    format(c3[[3]]$n, big.mark=","), format(c3[[4]]$n, big.mark=",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Sample restricted to single-walled, motor-fuel-only, single-tank facilities",
  "installed 1990--1997. All facilities share identical observable risk profile",
  "on wall type, fuel type, and tank count; remaining heterogeneity is age.",
  "SE clustered at state level.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), "JMP_Table3_Headline_MakeModel")

# Table 5 + 6: Youngest and Oldest side-by-side
cy <- lapply(list(m_youngest_simple, m_youngest_age_ctrl), extract_did)
co <- lapply(list(m_oldest_simple,   m_oldest_age_ctrl),   extract_did)
write_tex(c(
  "\\begin{table}[htbp]\\centering",
  "\\caption{Age Heterogeneity -- Youngest vs. Oldest Subsamples (H2)}",
  "\\label{tbl:mm_age_hte}",
  "\\begin{tabular}{lcccc}\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Youngest ($\\leq$5 yrs)}} & \\multicolumn{2}{c}{\\textbf{Oldest ($>$5 yrs)}}\\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  " & (1) & (2) & (3) & (4)\\\\",
  " & Simple & +Age Ctrl & Simple & +Age Ctrl\\\\\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s\\\\",
    sprintf("%.4f", cy[[1]]$beta), stars_fn(cy[[1]]$p),
    sprintf("%.4f", cy[[2]]$beta), stars_fn(cy[[2]]$p),
    sprintf("%.4f", co[[1]]$beta), stars_fn(co[[1]]$p),
    sprintf("%.4f", co[[2]]$beta), stars_fn(co[[2]]$p)),
  sprintf("& (%.4f) & (%.4f) & (%.4f) & (%.4f)\\\\",
    cy[[1]]$se, cy[[2]]$se, co[[1]]$se, co[[2]]$se),
  "\\midrule",
  "Age bin control & No & Yes & No & Yes\\\\",
  "Facility + Year FE & Yes & Yes & Yes & Yes\\\\\\midrule",
  sprintf("Observations & %s & %s & %s & %s\\\\",
    format(cy[[1]]$n, big.mark=","), format(cy[[2]]$n, big.mark=","),
    format(co[[1]]$n, big.mark=","), format(co[[2]]$n, big.mark=",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Make-model sample split at mean tank age in 1998 = 5 years.",
  "Youngest subsample: lifecycle acceleration (small sustained effect).",
  "Oldest subsample: near-end-of-life acceleration (front-loaded effect).",
  "Age HTE omitted for oldest subsample due to thin baseline bin collinearity.",
  "SE clustered at state level. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}\\end{table}"
), "JMP_Table56_AgeSplit_MakeModel")

# Cross-spec summary
spec_summary <- data.table(
  Model = c("Make-Model Full", "Make-Model + Mandate",
            "Youngest", "Oldest",
            "Leak (Main)", "Leak (Youngest)", "Leak (Oldest)"),
  beta  = sapply(list(m_closure_main, m_closure_main_mandate,
                      m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$beta),
  se    = sapply(list(m_closure_main, m_closure_main_mandate,
                      m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$se),
  p     = sapply(list(m_closure_main, m_closure_main_mandate,
                      m_youngest_simple, m_oldest_simple,
                      m_leak_main, m_leak_youngest, m_leak_oldest),
                 function(m) extract_did(m)$p))
print(spec_summary)
fwrite(spec_summary, file.path(OUTPUT_TABLES, "Cross_Spec_Summary_MakeModel.csv"))

cat("\n====================================================================\n")
cat(sprintf("02_DiD_Main_MakeModel.R COMPLETE | Tables: %s | Figures: %s\n",
            OUTPUT_TABLES, OUTPUT_FIGURES))
cat("====================================================================\n")