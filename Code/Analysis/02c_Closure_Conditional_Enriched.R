################################################################################
# 02c_Closure_Conditional_Enriched.R
#
# Builds an enriched conditional-on-closing DiD table for the slide deck.
# Sample: incumbent facilities (alive at Dec 22, 1998) x study years,
#         further restricted to facility-years with any_closure == 1.
# Reads panel: Z:/ust_ins_move_to_github/Data/Analysis/facility_panel.csv
# Writes:       Output/Tables/T2b_Enriched_Slide.tex
#
# Sibling to 02a_DiD_facility_behavior.R Section B6.3 (the original T2b);
# this version reports more outcomes and surfaces the pre-reform control
# mean so the TX x Post coefficient is interpretable as a deviation from
# baseline closing-firm behavior. Not causal (sample is endogenous to the
# reform via any_closure).
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(ggplot2)
  library(patchwork)
  library(here)
})
options(scipen = 999)
setDTthreads(14)

PANEL_PATH <- "Z:/ust_ins_move_to_github/Data/Analysis/facility_panel.csv"
OUT_TAB    <- here("Output", "Tables")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)

CONTROL_STATES <- c("AR", "CO", "ID", "KS", "KY", "LA", "MA", "MD", "ME",
                    "MN", "MO", "NC", "OH", "OK", "SD", "TN", "VA")
STUDY_STATES <- c("TX", CONTROL_STATES)
POST_YEAR    <- 1999L
PANEL_START  <- 1985L
PANEL_END    <- 2018L

cat("=== 02c: enriched conditional-on-closing DiD ===\n")
cat(sprintf("Reading panel from: %s\n", PANEL_PATH))

panel <- fread(
  PANEL_PATH,
  select = c("panel_id", "state", "panel_year", "texas_treated", "did_term",
             "rel_year_es",
             "fac_is_incumbent", "any_closure",
             "facility_complete_closure",
             "permanent_closure_year",
             "replacement_closure_year", "single_to_double_year",
             "n_dw_installs",
             "net_tank_change", "capacity_change",
             "leak_year", "tank_closure_revealed",
             "make_model_fac",
             "any_mandate_release_det", "any_mandate_spill_overfill",
             "any_mandate_integrity")
)
cat(sprintf("  read: %s rows\n", format(nrow(panel), big.mark = ",")))

panel <- panel[
  fac_is_incumbent == 1L &
  state %in% STUDY_STATES &
  !is.na(texas_treated)  &
  !is.na(did_term)       &
  panel_year %between% c(PANEL_START, PANEL_END)
]
cat(sprintf("  after incumbent + study-state + year filter: %s facility-years\n",
            format(nrow(panel), big.mark = ",")))

panel[, any_dw_install_year := as.integer(n_dw_installs > 0L)]

closing <- panel[any_closure == 1L]
cat(sprintf("  closing sample: %s facility-years (TX %s | ctrl %s)\n",
    format(nrow(closing),                   big.mark = ","),
    format(closing[texas_treated == 1L, .N], big.mark = ","),
    format(closing[texas_treated == 0L, .N], big.mark = ",")))

fit_outcome <- function(var, dat) {
  fml <- as.formula(paste0(
    var,
    " ~ did_term + any_mandate_release_det + any_mandate_spill_overfill",
    " + any_mandate_integrity | panel_id + make_model_fac^panel_year"))
  feols(fml, data = dat, cluster = ~state)
}
ctrl_mean <- function(var, dat) {
  dat[texas_treated == 0L & panel_year < POST_YEAR,
      mean(get(var), na.rm = TRUE)]
}

outcomes <- list(
  list(label = "Facility closes all tanks this year",
       var = "facility_complete_closure", fmt = "pct"),
  list(label = "Has a permanent closure (no replace)",
       var = "permanent_closure_year",    fmt = "pct"),
  list(label = "Has a replacement closure (upgrade margin)",
       var = "replacement_closure_year",  fmt = "pct"),
  list(label = "Any DW tank installed",
       var = "any_dw_install_year",       fmt = "pct"),
  list(label = "SW removed AND DW installed (SW$\\to$DW)",
       var = "single_to_double_year",     fmt = "pct"),
  list(label = "Net tank change (count)",
       var = "net_tank_change",           fmt = "num"),
  list(label = "Capacity change (gal)",
       var = "capacity_change",           fmt = "gal")
)

results <- lapply(outcomes, function(o) {
  cat(sprintf("  fitting %-44s ... ", o$label))
  m  <- fit_outcome(o$var, closing)
  cm <- ctrl_mean(o$var,   closing)
  b  <- coef(m)["did_term"]
  se <- se(m)["did_term"]
  p  <- pvalue(m)["did_term"]
  cat(sprintf("cm=%+9.4f  beta=%+9.4f  se=%.4f  p=%.4g\n", cm, b, se, p))
  list(label = o$label, fmt = o$fmt,
       cm = cm, beta = b, se = se, p = p, n = nobs(m))
})

stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "\\textsuperscript{***}"
  else if (p < 0.05) "\\textsuperscript{**}"
  else if (p < 0.10) "\\textsuperscript{*}"
  else ""
}
fmt_val <- function(x, fmt) {
  if (fmt == "pct")      sprintf("%.1f\\%%", 100 * x)
  else if (fmt == "num") sprintf("%+.3f", x)
  else if (fmt == "gal") sprintf("%+s",
                                 formatC(round(x), format = "d",
                                         big.mark = ","))
  else                   sprintf("%.4f", x)
}
fmt_signed <- function(x, fmt) {
  s <- if (x > 0) "$+$" else "$-$"
  v <- abs(x)
  if (fmt == "pct")      sprintf("%s%.1f\\,pp", s, 100 * v)
  else if (fmt == "num") sprintf("%s%.3f", s, v)
  else if (fmt == "gal") sprintf("%s%s", s,
                                 formatC(round(v), format = "d",
                                         big.mark = ","))
  else                   sprintf("%s%.4f", s, v)
}

fmt_se <- function(se, fmt) {
  if (fmt == "gal") formatC(round(se), format = "d", big.mark = ",")
  else              sprintf("%.3f", se)
}
tex_rows <- vapply(results, function(r) {
  sprintf("%s & %s & %s%s & (%s) \\\\",
          r$label, fmt_val(r$cm, r$fmt),
          fmt_signed(r$beta, r$fmt), stars(r$p),
          fmt_se(r$se, r$fmt))
}, character(1L))

n_obs <- format(results[[1]]$n, big.mark = ",")

tex_out <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome (conditional on closing) & Pre-reform ctrl mean & TX $\\times$ Post & (SE) \\\\",
  "\\midrule",
  tex_rows,
  "\\midrule",
  sprintf("$N$ closing facility-years & \\multicolumn{3}{c}{%s} \\\\", n_obs),
  "Sample & \\multicolumn{3}{c}{Incumbent (alive at Dec 22, 1998), any\\_closure $=1$} \\\\",
  "FE & \\multicolumn{3}{c}{Facility + make\\_model\\_fac $\\times$ year; SE clustered by state} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(tex_out, file.path(OUT_TAB, "T2b_Enriched_Slide.tex"))
cat(sprintf("\nSaved: %s\n", file.path(OUT_TAB, "T2b_Enriched_Slide.tex")))


################################################################################
# LUST DiD on incumbent (alive-at-reform) sample
#
# Same sample as the closure analysis ABOVE the any_closure == 1 restriction:
# fac_is_incumbent == 1, study states, panel years 1985-2018. Three outcomes:
#   Total leak discovery        = leak_year
#   Background leak             = leak_year == 1 & tank_closure_revealed == 0
#   Inspection-triggered leak   = tank_closure_revealed  (Primary 0-60d window)
# Same FE spec: panel_id + make_model_fac^panel_year, SE clustered by state.
################################################################################

cat("\n=== LUST DiD on alive-at-reform (incumbent) sample ===\n")
panel[, lust_standalone := as.integer(leak_year == 1L &
                                      tank_closure_revealed == 0L)]
cat(sprintf("  Incumbent sample: %s facility-years (TX %s | ctrl %s)\n",
    format(nrow(panel),                    big.mark = ","),
    format(panel[texas_treated == 1L, .N], big.mark = ","),
    format(panel[texas_treated == 0L, .N], big.mark = ",")))

lust_outcomes <- list(
  list(label = "Total leak discovery (0/1)",
       var = "leak_year"),
  list(label = "\\hspace{0.5em}Background",
       var = "lust_standalone"),
  list(label = "\\hspace{0.5em}Inspection-triggered",
       var = "tank_closure_revealed")
)

lust_results <- lapply(lust_outcomes, function(o) {
  cat(sprintf("  fitting %-44s ... ", o$label))
  m  <- fit_outcome(o$var, panel)
  cm <- ctrl_mean(o$var,   panel)
  b  <- coef(m)["did_term"]
  se <- se(m)["did_term"]
  p  <- pvalue(m)["did_term"]
  cat(sprintf("cm=%+9.4f  beta=%+9.4f  se=%.4f  p=%.4g\n", cm, b, se, p))
  list(label = o$label, cm = cm, beta = b, se = se, p = p, n = nobs(m))
})

# Slide-friendly tex: outcome | pre-reform ctrl mean | TX x Post | (SE)
fmt_lust <- function(x) sprintf("%.2f\\%%", 100 * x)
fmt_lust_signed <- function(x, p) {
  s   <- if (x > 0) "$+$" else "$-$"
  mag <- 100 * abs(x)
  sprintf("%s%.2f\\,pp%s", s, mag, stars(p))
}
lust_rows <- vapply(lust_results, function(r) {
  sprintf("%s & %s & %s & (%.4f) \\\\",
          r$label, fmt_lust(r$cm),
          fmt_lust_signed(r$beta, r$p), r$se)
}, character(1L))

n_lust <- format(lust_results[[1]]$n, big.mark = ",")

lust_tex <- c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome (incumbent sample) & Pre-reform ctrl mean & TX $\\times$ Post & (SE) \\\\",
  "\\midrule",
  lust_rows[1L],
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Decomposition (Primary 0--60d window):}} \\\\",
  lust_rows[2L:3L],
  "\\midrule",
  sprintf("$N$ facility-years & \\multicolumn{3}{c}{%s} \\\\", n_lust),
  "Sample & \\multicolumn{3}{c}{Incumbent (alive at Dec 22, 1998)} \\\\",
  "FE & \\multicolumn{3}{c}{Facility + make\\_model\\_fac $\\times$ year; SE clustered by state} \\\\",
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(lust_tex, file.path(OUT_TAB, "T_LUST_Incumbent_Slide.tex"))
cat(sprintf("Saved: %s\n", file.path(OUT_TAB, "T_LUST_Incumbent_Slide.tex")))


################################################################################
# LUST event studies on incumbent (alive-at-reform) sample
#
# Same FE spec as static DiD; replaces did_term with i(rel_year_es, texas_treated, ref=-1).
# Three outcomes (matching the LUST static table): leak_year, lust_standalone,
# tank_closure_revealed. Single 3-panel figure saved to Output/Figures/.
################################################################################

cat("\n=== LUST event studies on incumbent sample ===\n")
OUT_FIG <- here("Output", "Figures")
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

ES_REF      <- -1L
ES_X_MIN    <- -12L
ES_X_MAX    <-  15L

fit_es <- function(var, dat, ref = ES_REF) {
  fml <- as.formula(sprintf(
    "%s ~ i(rel_year_es, texas_treated, ref = %dL) + any_mandate_release_det + any_mandate_spill_overfill + any_mandate_integrity | panel_id + make_model_fac^panel_year",
    var, ref))
  feols(fml, data = dat, cluster = ~state)
}

extract_es <- function(m, ref = ES_REF) {
  nms  <- names(coef(m))
  pat  <- "^rel_year_es::(-?[0-9]+):texas_treated$"
  hits <- grep(pat, nms, value = TRUE)
  if (!length(hits)) {
    pat  <- "^texas_treated:rel_year_es::(-?[0-9]+)$"
    hits <- grep(pat, nms, value = TRUE)
  }
  yrs  <- as.integer(sub(pat, "\\1", hits))
  dt   <- data.table(rel_year = yrs,
                     estimate = coef(m)[hits],
                     se       = se(m)[hits])
  if (!ref %in% dt$rel_year)
    dt <- rbindlist(list(dt, data.table(rel_year = ref, estimate = 0, se = 0)))
  setorder(dt, rel_year)
  dt[, period := fcase(rel_year < 0, "pre",
                        rel_year == 0, "event",
                        default        = "post")]
  dt[, `:=`(ci_lo = estimate - 1.96 * se,
            ci_hi = estimate + 1.96 * se)]
  dt
}

plot_es_panel <- function(dt, y_label) {
  dt_plot <- dt[rel_year %between% c(ES_X_MIN, ES_X_MAX)]
  ggplot(dt_plot, aes(x = rel_year, y = estimate)) +
    geom_hline(yintercept = 0, colour = "grey55",
               linetype = "dashed", linewidth = 0.45) +
    geom_vline(xintercept = ES_REF - 0.5, colour = "grey40",
               linetype = "dotted", linewidth = 0.5) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = period),
                alpha = 0.13, colour = NA) +
    geom_line(aes(colour = period, group = 1), linewidth = 0.55) +
    geom_point(aes(colour = period), size = 1.9, shape = 21,
               fill = "white", stroke = 1.2) +
    scale_colour_manual(
      values = c(pre = "#3A6BBF", event = "#888888", post = "#BF3A3A"),
      labels = c(pre = "Pre",     event = "Event",   post = "Post"),
      name   = NULL) +
    scale_fill_manual(
      values = c(pre = "#3A6BBF", event = "#888888", post = "#BF3A3A"),
      guide  = "none") +
    scale_x_continuous(breaks = seq(ES_X_MIN, ES_X_MAX, by = 2)) +
    labs(x = "Years relative to Dec 22, 1998", y = y_label) +
    theme_classic(base_size = 11, base_family = "Times") +
    theme(legend.position    = "bottom",
          axis.line          = element_line(colour = "black", linewidth = 0.4),
          axis.ticks         = element_line(colour = "black", linewidth = 0.3),
          axis.text          = element_text(colour = "black"),
          panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
          panel.grid.minor   = element_blank(),
          plot.margin        = margin(8, 12, 8, 8))
}

lust_es_specs <- list(
  list(var = "leak_year",             label = "Total leak discovery"),
  list(var = "lust_standalone",       label = "Background (no closure 0--60d)"),
  list(var = "tank_closure_revealed", label = "Inspection-triggered (closure 0--60d)")
)

lust_es_dts <- lapply(lust_es_specs, function(o) {
  cat(sprintf("  ES fitting %-50s ... ", o$label))
  m  <- fit_es(o$var, panel)
  dt <- extract_es(m)
  cat(sprintf("done (range %+d..%+d)\n", min(dt$rel_year), max(dt$rel_year)))
  list(spec = o, dt = dt)
})

plots <- lapply(lust_es_dts, function(x) plot_es_panel(x$dt, x$spec$label))

combined <- plots[[1]] / plots[[2]] / plots[[3]] +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(file.path(OUT_FIG, "Fig_ES_LUST_Combined.pdf"),
       combined, width = 9, height = 11)
ggsave(file.path(OUT_FIG, "Fig_ES_LUST_Combined.png"),
       combined, width = 9, height = 11, dpi = 200)
cat(sprintf("Saved: %s\n", file.path(OUT_FIG, "Fig_ES_LUST_Combined.{pdf,png}")))

# Single-panel figures (one per outcome) for appendix slides
single_filenames <- c("Fig_ES_LUST_Total",
                      "Fig_ES_LUST_Background",
                      "Fig_ES_LUST_Inspection")
for (k in seq_along(plots)) {
  ggsave(file.path(OUT_FIG, paste0(single_filenames[k], ".pdf")),
         plots[[k]], width = 10, height = 5)
  ggsave(file.path(OUT_FIG, paste0(single_filenames[k], ".png")),
         plots[[k]], width = 10, height = 5, dpi = 200)
}
cat(sprintf("Saved: %s\n",
            paste(single_filenames, collapse = ", ")))
