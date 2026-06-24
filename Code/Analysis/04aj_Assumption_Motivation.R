# 04aj_Assumption_Motivation.R
# Ticket 019 — data-motivated assumptions for the portfolio/size DCM (M1, M2, M3).
# READ-ONLY DESCRIPTIVE. No structural estimation; no Output/Estimation_Results writes.
#   M1  size belongs in the state         (binary logit per margin, CCP ~ size | s_idx FE)
#   M2  action set: replace != downsize   (binary logit per margin, action ~ state x regime)
#   M3  replace transition = consolidation (Δcap/Δcount/Δwall by install vs no-install)
# All SEs CLUSTERED BY STATE via fixest CRVE. No bootstrap. No joint LR/Wald test.
# True-action coding reuses 04z_Model_vs_Reality.R's fcase VERBATIM (renamed after).

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(here)
})
need <- function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
need("fixest"); need("scales"); need("patchwork")
suppressPackageStartupMessages({ library(fixest); library(scales); library(patchwork) })

cat("=== 04aj ASSUMPTION MOTIVATION (M1/M2/M3) ===\n")
OUT_T <- here("Output", "Tables"); OUT_F <- here("Output", "Figures")
dir.create(OUT_T, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_F, recursive = TRUE, showWarnings = FALSE)

# theme / palettes (match 04ag/04ah/04ai)
REG_COL  <- c("FF" = "#E76F51", "RB" = "#2A9D8F")
SIZE_COL <- c("1" = "#bdd7e7", "2" = "#6baed6", "3" = "#3182bd", "4+" = "#08519c")
th <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank(), legend.position = "top")

star <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))))
texesc <- function(x) gsub("%", "\\\\%", gsub("_", "\\\\_", x))

# ---- pull a clustered coefficient row by exact or regex term name ----
ct_row <- function(fit, term, regex = FALSE) {
  ct <- coeftable(fit)
  rn <- rownames(ct)
  i  <- if (regex) grep(term, rn) else match(term, rn)
  i  <- i[!is.na(i)]
  if (length(i) != 1L)
    stop(sprintf("term '%s' not uniquely found; have: %s", term, paste(rn, collapse = ", ")))
  list(coef = ct[i, 1], se = ct[i, 2], p = ct[i, 4], or = exp(ct[i, 1]))
}

# ═══════════════════════════════════════════════════════════════════════
cat("\n=== PREP: true-action coding (reuse 04z fcase verbatim) ===\n")
# ═══════════════════════════════════════════════════════════════════════
obs <- fread(here("Data", "Analysis", "dcm_obs_panel_observed.csv"),
             select = c("panel_id", "panel_year", "state", "s_idx", "A_bin",
                        "w_state", "rho_state", "boy_stock"))
fp  <- fread(here("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "any_closure", "facility_complete_closure",
                        "replacement_closure_year", "permanent_closure_year", "n_installs",
                        "single_to_double_year"))
m <- merge(obs, fp, by = c("panel_id", "panel_year"), all.x = FALSE)
for (c0 in c("any_closure", "facility_complete_closure", "replacement_closure_year",
             "permanent_closure_year", "n_installs", "single_to_double_year"))
  m[is.na(get(c0)), (c0) := 0L]

# size filter + re-derived size_bin (spec is authoritative; ignore shipped column)
m <- m[boy_stock >= 1L]
m[, size_bin := factor(pmin(boy_stock, 4L), levels = 1:4, labels = c("1", "2", "3", "4+"))]

# --- 04z fcase, copied VERBATIM (do not edit branch logic) ---
m[, true_event := fcase(
  any_closure == 0L,                                              "No tank closed",
  replacement_closure_year == 1L & single_to_double_year == 1L,   "Replace: SW->DW upgrade",
  replacement_closure_year == 1L,                                 "Replace: same wall",
  facility_complete_closure == 1L,                                "Full exit (all tanks closed)",
  default                                                       = "Downsizing (kept operating)")]
m[, true_event := factor(true_event, levels = c(
  "No tank closed", "Replace: SW->DW upgrade", "Replace: same wall",
  "Full exit (all tanks closed)", "Downsizing (kept operating)"))]

# --- rename to ticket-019 short labels via a NAMED LOOKUP applied AFTER the fcase ---
LBL <- c("No tank closed"               = "No-close",
         "Replace: SW->DW upgrade"      = "Replace-upgrade",
         "Replace: same wall"           = "Replace-samewall",
         "Full exit (all tanks closed)" = "Exit",
         "Downsizing (kept operating)"  = "Downsize")
m[, event := factor(LBL[as.character(true_event)],
                    levels = c("No-close", "Exit", "Replace-upgrade", "Replace-samewall", "Downsize"))]
m[, regime := as.integer(rho_state == 2L)]                                  # RB = 1
# factor wall: DW reference so the SW contrast prints as 'w_stateSW' (matches spec f_k)
m[, w_state_f := factor(w_state, levels = c(2L, 1L), labels = c("DW", "SW"))]
m[, A_bin_f := factor(A_bin)]

MARGINS <- c("Exit", "Replace-upgrade", "Replace-samewall", "Downsize")

cat(sprintf("  rows after boy_stock>=1 filter: %s\n", format(nrow(m), big.mark = ",")))
cat("  5-category count table (should reproduce 04z margins):\n")
print(m[, .N, by = event][order(-N)])
cat(sprintf("  n states (clusters): %d\n", uniqueN(m$state)))
stopifnot(nrow(m) > 2.0e6, nlevels(m$size_bin) == 4L)

# ═══════════════════════════════════════════════════════════════════════
cat("\n=== M1: size gradient (binary logit per margin, s_idx FE) ===\n")
# ═══════════════════════════════════════════════════════════════════════
size_terms <- c("size_bin2", "size_bin3", "size_bin4+")
m1_rows <- list()
for (k in MARGINS) {
  m[, y_k := as.integer(event == k)]
  fit <- feglm(y_k ~ size_bin | s_idx, data = m, family = binomial(), cluster = ~state)
  cat(sprintf("  [M1] margin=%-16s nobs=%s\n", k, format(fit$nobs, big.mark = ",")))
  for (tm in size_terms) {
    r <- ct_row(fit, tm)
    m1_rows[[length(m1_rows) + 1L]] <- data.table(
      margin = k, size_term = tm, odds_ratio = r$or, coef = r$coef,
      se = r$se, p = r$p, n = fit$nobs)
  }
}
m1 <- rbindlist(m1_rows)
stopifnot(all(is.finite(m1$odds_ratio)))
fwrite(m1, file.path(OUT_T, "M1_Size_Logit.csv"))
cat("  saved M1_Size_Logit.csv\n"); print(m1[, .(margin, size_term, odds_ratio = round(odds_ratio, 3),
                                                  se = round(se, 4), p = round(p, 4))])

# --- M1 .tex (compact slide pivot: size term rows x margin OR columns, with stars) ---
m1[, cell := sprintf("%.2f%s", odds_ratio, star(p))]
m1w <- dcast(m1, size_term ~ margin, value.var = "cell")
setcolorder(m1w, c("size_term", MARGINS))
m1_tex <- c(
  "\\begin{center}\\small\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{tabular}{lrrrr}", "\\toprule",
  "\\textbf{Size (vs 1)} & \\textbf{Exit} & \\textbf{Repl.\\ upgrade} & \\textbf{Repl.\\ same-wall} & \\textbf{Downsize} \\\\",
  "\\midrule",
  apply(m1w, 1L, function(r) sprintf("%s & %s & %s & %s & %s \\\\",
        texesc(r[["size_term"]]), r[["Exit"]], r[["Replace-upgrade"]],
        r[["Replace-samewall"]], r[["Downsize"]])),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}{\\centering\\scriptsize Odds ratios from a binary logit per action margin (vs.\\ maintain), $s\\_idx$ (age$\\times$wall$\\times$regime) fixed effects; state-clustered SE ($G{=}18$). $^{*}p{<}.1,\\ ^{**}p{<}.05,\\ ^{***}p{<}.01$. ORs ordered away from $1$ across size bins $\\Rightarrow$ size shifts behavior at fixed state.\\par}")
writeLines(m1_tex, file.path(OUT_T, "M1_Size_Logit.tex"))

# --- M1 figure: empirical share of EACH margin by (A_bin, size_bin), facet margin ~ wall ---
ind_cols <- paste0("ind_", MARGINS)
for (k in MARGINS) m[, (paste0("ind_", k)) := as.integer(event == k)]
agg1 <- m[, lapply(.SD, mean), by = .(A_bin, size_bin, w_state_f), .SDcols = ind_cols]
agg1l <- melt(agg1, id.vars = c("A_bin", "size_bin", "w_state_f"),
              measure.vars = ind_cols, variable.name = "margin", value.name = "share")
agg1l[, margin := factor(sub("^ind_", "", margin), levels = MARGINS)]
pM1 <- ggplot(agg1l, aes(A_bin, share, color = size_bin, group = size_bin)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  facet_grid(margin ~ w_state_f, scales = "free_y") +
  scale_color_manual(values = SIZE_COL, name = "Facility size\n(active tanks)") +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Age bin", y = "Share of facility-years",
       title = "Behavior shifts with facility size at fixed (age, wall, regime)",
       subtitle = "Empirical action shares by age bin and size, faceted by margin (rows) and wall (cols). Free y-axes; shares are small.") +
  th
ggsave(file.path(OUT_F, "M1_CCP_by_Size.png"), pM1, width = 9, height = 9, dpi = 150)
cat("  saved M1_CCP_by_Size.png\n")

# ═══════════════════════════════════════════════════════════════════════
cat("\n=== M2: regime contrast (binary logit per margin, year FE) ===\n")
# ═══════════════════════════════════════════════════════════════════════
n_states_RB <- uniqueN(m[regime == 1L, state])
cat(sprintf("  n_states_RB = %d\n", n_states_RB))
m2_rows <- list()
for (k in MARGINS) {
  m[, y_k := as.integer(event == k)]
  fit <- feglm(y_k ~ A_bin_f + w_state_f + size_bin + regime + regime:w_state_f | panel_year,
               data = m, family = binomial(), cluster = ~state)
  e <- ct_row(fit, "^regime$", regex = TRUE)                   # e_k  (RB main effect)
  f <- ct_row(fit, "regime.*w_state_fSW|w_state_fSW.*regime", regex = TRUE)  # f_k  (RB x SW; fixest orders factor first)
  cat(sprintf("  [M2] margin=%-16s e_RB(coef)=%+.3f  f_RBxSW(coef)=%+.3f\n", k, e$coef, f$coef))
  m2_rows[[length(m2_rows) + 1L]] <- data.table(
    margin = k, term = "regime_RB", odds_ratio = e$or, coef = e$coef, se = e$se, p = e$p, n_states_RB = n_states_RB)
  m2_rows[[length(m2_rows) + 1L]] <- data.table(
    margin = k, term = "regimeRB:SW", odds_ratio = f$or, coef = f$coef, se = f$se, p = f$p, n_states_RB = n_states_RB)
}
m2 <- rbindlist(m2_rows)
fwrite(m2, file.path(OUT_T, "M2_Action_Logit.csv"))
cat("  saved M2_Action_Logit.csv\n"); print(m2[, .(margin, term, odds_ratio = round(odds_ratio, 3),
                                                    se = round(se, 4), p = round(p, 4))])
# opposite-sign test report (not a hard stop)
e_up <- m2[margin == "Replace-upgrade" & term == "regime_RB", coef]
e_dn <- m2[margin == "Downsize"        & term == "regime_RB", coef]
f_up <- m2[margin == "Replace-upgrade" & term == "regimeRB:SW", coef]
cat(sprintf("  TEST e_{Replace-upgrade}=%+.3f, e_{Downsize}=%+.3f -> opposite-sign: %s\n",
            e_up, e_dn, ifelse(sign(e_up) != sign(e_dn), "YES", "NO")))
cat(sprintf("  TEST f_{Replace-upgrade}=%+.3f (RB targets SW if >0)\n", f_up))

# --- M2 .tex (compact: margin rows, RB-OR and RB:SW-OR cols, with stars) ---
m2[, cell := sprintf("%.2f%s", odds_ratio, star(p))]
m2w <- dcast(m2, margin ~ term, value.var = "cell")
m2w <- m2w[match(MARGINS, margin)]
m2_tex <- c(
  "\\begin{center}\\small\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{tabular}{lrr}", "\\toprule",
  "\\textbf{Margin} & \\textbf{RB} & \\textbf{RB$\\times$SW} \\\\", "\\midrule",
  apply(m2w, 1L, function(r) sprintf("%s & %s & %s \\\\",
        texesc(r[["margin"]]), r[["regime_RB"]], r[["regimeRB:SW"]])),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  sprintf("\\vspace{0.1cm}{\\centering\\scriptsize Odds ratios, binary logit per margin (vs.\\ maintain) with year fixed effects and (age, wall, size) controls; state-clustered SE. RB identified cross-state within year (controls always FF). RB present in %d state(s) $\\Rightarrow$ few-treated-cluster caution on the RB main effect; the causal claim defers to the DiD. $^{*}p{<}.1,\\ ^{**}p{<}.05,\\ ^{***}p{<}.01$.\\par}", n_states_RB))
writeLines(m2_tex, file.path(OUT_T, "M2_Action_Logit.tex"))

# --- M2 figure: shares of {Exit, Replace-upgrade, Downsize} by A_bin, facet wall ~ regime ---
M2_MARG <- c("Exit", "Replace-upgrade", "Downsize")
m[, regime_lab := factor(regime, levels = c(0L, 1L), labels = c("FF", "RB"))]
agg2 <- m[, lapply(.SD, mean), by = .(A_bin, w_state_f, regime_lab),
          .SDcols = paste0("ind_", M2_MARG)]
agg2l <- melt(agg2, id.vars = c("A_bin", "w_state_f", "regime_lab"),
              measure.vars = paste0("ind_", M2_MARG), variable.name = "margin", value.name = "share")
agg2l[, margin := factor(sub("^ind_", "", margin), levels = M2_MARG)]
pM2 <- ggplot(agg2l, aes(A_bin, share, color = margin, group = margin)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  facet_grid(w_state_f ~ regime_lab, scales = "free_y") +
  scale_color_manual(values = c("Exit" = "#264653", "Replace-upgrade" = "#2A9D8F", "Downsize" = "#E9C46A"),
                     name = NULL) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Age bin", y = "Share of facility-years",
       title = "Action shares by regime: replace and downsize move oppositely",
       subtitle = "Empirical shares by age bin, faceted by wall (rows) and regime (cols). Free y-axes.") +
  th
ggsave(file.path(OUT_F, "M2_ActionShares_by_State_Regime.png"), pM2, width = 9, height = 6.5, dpi = 150)
cat("  saved M2_ActionShares_by_State_Regime.png\n")

# ═══════════════════════════════════════════════════════════════════════
cat("\n=== M3: replace transition = consolidation ===\n")
# ═══════════════════════════════════════════════════════════════════════
fpm <- fread(here("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "state", "net_tank_change", "capacity_change",
                        "n_installs", "any_closure", "single_to_double_year",
                        "facility_complete_closure", "total_capacity", "lag_capacity"))
for (c0 in c("n_installs", "any_closure", "single_to_double_year", "facility_complete_closure"))
  fpm[is.na(get(c0)), (c0) := 0L]

# events = operating AND any portfolio change (INCLUDES net-0 swaps; NO net_tank_change<0 filter)
ev <- fpm[facility_complete_closure == 0L & (n_installs > 0L | any_closure == 1L)]
ev[, consolidation := as.integer(n_installs > 0L)]
ev[, event_type := factor(consolidation, levels = c(1L, 0L), labels = c("consolidation", "pure_shrink"))]
# Δcapacity as % (guard lag_capacity>0); NA out bad-denominator rows for dcap pieces only
ev[, dcap_pct := NA_real_]
ev[!is.na(lag_capacity) & lag_capacity > 0, dcap_pct := 100 * capacity_change / lag_capacity]
cat("  event counts by type:\n"); print(ev[, .N, by = event_type])
cat("  dcap_pct NA'd (bad/zero lag_capacity) per event type:\n")
print(ev[is.na(dcap_pct), .N, by = event_type])
stopifnot(ev[event_type == "consolidation", .N] > 0L, ev[event_type == "pure_shrink", .N] > 0L)

# --- distribution table: event x dcount-bin {0,-1,-2,<=-3} ---
ev[, dcount_bin := fcase(net_tank_change == 0L, "0",
                         net_tank_change == -1L, "-1",
                         net_tank_change == -2L, "-2",
                         net_tank_change <= -3L, "<=-3",
                         default = NA_character_)]
cat(sprintf("  rows with net_tank_change>0 (outside dcount bins): %d\n", ev[is.na(dcount_bin), .N]))
N_evt <- ev[, .(N_event = .N), by = event_type]
dist <- ev[!is.na(dcount_bin), .(
  n = .N,
  median_dcap_pct = median(dcap_pct, na.rm = TRUE),
  p25_dcap = quantile(dcap_pct, 0.25, na.rm = TRUE),
  p75_dcap = quantile(dcap_pct, 0.75, na.rm = TRUE),
  share_cap_within10pct = mean(abs(dcap_pct) <= 10, na.rm = TRUE),
  share_SWtoDW = mean(single_to_double_year == 1L)),
  by = .(event_type, dcount_bin)]
dist <- merge(dist, N_evt, by = "event_type")
dist[, share := n / N_event][, N_event := NULL]
# enumerate all 8 cells in order
grid <- CJ(event_type = factor(c("consolidation", "pure_shrink"),
                               levels = c("consolidation", "pure_shrink")),
           dcount_bin = c("0", "-1", "-2", "<=-3"), sorted = FALSE)
dist <- dist[grid, on = .(event_type, dcount_bin)]
dist[is.na(n), `:=`(n = 0L, share = 0)]
setcolorder(dist, c("event_type", "dcount_bin", "n", "share", "median_dcap_pct",
                    "p25_dcap", "p75_dcap", "share_cap_within10pct", "share_SWtoDW"))
fwrite(dist, file.path(OUT_T, "M3_Transition_Distribution.csv"))
cat("  saved M3_Transition_Distribution.csv\n"); print(dist)

# --- M3 tests (Eq.4-6), SE clustered by state ---
f4 <- feols(dcap_pct ~ consolidation, data = ev, cluster = ~state)
f5 <- feols(net_tank_change ~ consolidation, data = ev, cluster = ~state)
f6 <- feglm(single_to_double_year ~ consolidation, data = ev, family = binomial(), cluster = ~state)
r4 <- ct_row(f4, "consolidation"); r5 <- ct_row(f5, "consolidation"); r6 <- ct_row(f6, "consolidation")
m3t <- data.table(
  outcome = c("dcap_pct", "net_tank_change", "single_to_double_year"),
  term = "consolidation",
  coef = c(r4$coef, r5$coef, r6$coef),
  se   = c(r4$se,   r5$se,   r6$se),
  p    = c(r4$p,    r5$p,    r6$p))
fwrite(m3t, file.path(OUT_T, "M3_Transition_Tests.csv"))
cat("  saved M3_Transition_Tests.csv\n"); print(m3t)

# --- M3 .tex for distribution table (8 rows, fits slide) ---
d3 <- copy(dist)
d3[, `:=`(share = sprintf("%.1f\\%%", 100 * share),
          median_dcap_pct = sprintf("%.1f", median_dcap_pct),
          share_SWtoDW = sprintf("%.1f\\%%", 100 * share_SWtoDW))]
m3d_tex <- c(
  "\\begin{center}\\small\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{tabular}{llrrrr}", "\\toprule",
  "\\textbf{Event} & \\textbf{$\\Delta$count} & \\textbf{n} & \\textbf{share} & \\textbf{med.\\ $\\Delta$cap} & \\textbf{SW$\\to$DW} \\\\",
  "\\midrule",
  apply(d3, 1L, function(r) sprintf("%s & %s & %s & %s & %s & %s \\\\",
        texesc(r[["event_type"]]), r[["dcount_bin"]], format(as.integer(r[["n"]]), big.mark = ","),
        r[["share"]], r[["median_dcap_pct"]], r[["share_SWtoDW"]])),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}{\\centering\\scriptsize Portfolio-change facility-years that keep operating. Consolidation $=$ any install in the year; pure-shrink $=$ none. Shares within event type; med.\\ $\\Delta$cap in \\%. Includes net-0 swaps.\\par}")
writeLines(m3d_tex, file.path(OUT_T, "M3_Transition_Distribution.tex"))

# --- M3 tests .tex ---
m3t_tex <- c(
  "\\begin{center}\\small\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{tabular}{llrr}", "\\toprule",
  "\\textbf{Outcome} & \\textbf{Term} & \\textbf{coef} & \\textbf{SE} \\\\", "\\midrule",
  apply(m3t, 1L, function(r) sprintf("%s & %s & %.3f & %.3f \\\\",
        texesc(r[["outcome"]]), texesc(r[["term"]]), as.numeric(r[["coef"]]), as.numeric(r[["se"]]))),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}{\\centering\\scriptsize Eq.\\ 4--6: $\\Delta$cap\\%, $\\Delta$count (feols) and SW$\\to$DW (logit) on consolidation. State-clustered SE ($G{=}18$).\\par}")
writeLines(m3t_tex, file.path(OUT_T, "M3_Transition_Tests.tex"))

# --- M3 summary .tex (slide-ready: by event type, medians + %SW->DW) ---
summ3 <- ev[, .(median_dcount = median(net_tank_change),
                median_dcap_pct = median(dcap_pct, na.rm = TRUE),
                share_SWtoDW = mean(single_to_double_year == 1L)), by = event_type]
summ3 <- summ3[match(c("consolidation", "pure_shrink"), event_type)]
m3s_tex <- c(
  "\\begin{center}\\small\\renewcommand{\\arraystretch}{1.25}",
  "\\begin{tabular}{lrrr}", "\\toprule",
  "\\textbf{Event type} & \\textbf{median $\\Delta$count} & \\textbf{median $\\Delta$cap\\%} & \\textbf{SW$\\to$DW} \\\\",
  "\\midrule",
  apply(summ3, 1L, function(r) sprintf("%s & %s & %.1f & %.1f\\%% \\\\",
        texesc(r[["event_type"]]), r[["median_dcount"]],
        as.numeric(r[["median_dcap_pct"]]), 100 * as.numeric(r[["share_SWtoDW"]]))),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  "\\vspace{0.1cm}{\\centering\\scriptsize Consolidation (install $+$ close) vs pure-shrink (close only) among operating facility-years with any portfolio change. SW$\\to$DW $=$ share with a single-to-double upgrade.\\par}")
writeLines(m3s_tex, file.path(OUT_T, "M3_Transition_Summary.tex"))
cat("  saved M3 .tex tables (distribution, tests, summary)\n")

# --- M3 figure: (a) dcap_pct density by event type, (b) net_tank_change bars by event type ---
evd <- ev[!is.na(dcap_pct) & dcap_pct > -100 & dcap_pct < 200]   # trim display tails only
pa <- ggplot(evd, aes(dcap_pct, fill = event_type, color = event_type)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c("consolidation" = "#2A9D8F", "pure_shrink" = "#E76F51"), name = NULL) +
  scale_color_manual(values = c("consolidation" = "#2A9D8F", "pure_shrink" = "#E76F51"), name = NULL) +
  labs(x = "Capacity change (% of lagged)", y = "Density",
       title = "(a) Capacity change", subtitle = "No sign imposed: negative, zero and positive mass") + th
nbar <- ev[!is.na(dcount_bin), .N, by = .(event_type, dcount_bin)]
nbar[, dcount_bin := factor(dcount_bin, levels = c("0", "-1", "-2", "<=-3"))]
nbar[, share := N / sum(N), by = event_type]
swann <- ev[, .(sw = mean(single_to_double_year == 1L)), by = event_type]
pb <- ggplot(nbar, aes(dcount_bin, share, fill = event_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_fill_manual(values = c("consolidation" = "#2A9D8F", "pure_shrink" = "#E76F51"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Net tank-count change", y = "Share within event type",
       title = "(b) Net count change",
       subtitle = sprintf("SW->DW: consolidation %.0f%%, pure-shrink %.0f%%",
                          100 * swann[event_type == "consolidation", sw],
                          100 * swann[event_type == "pure_shrink", sw])) + th
pM3 <- pa / pb + plot_layout(heights = c(1, 1))
ggsave(file.path(OUT_F, "M3_Transition_by_Action.png"), pM3, width = 9, height = 8, dpi = 150)
cat("  saved M3_Transition_by_Action.png\n")

cat("\n=== 04aj DONE ===\n")
