# ==============================================================================
# AE03_Transition_Evidence.R  -- TICKET 021, script 3 of 5
# ==============================================================================
# X4  capacity is a near-fixed type      (G fixed; frozen at M/R)
# X8  replace recycles capacity          (n^R transition, no reset)
# X9  new tanks are double-walled        (e_{DW,1} install; era fact)
# X10 the regime moves the margins       (downsize as action; regime replace)
# Consumes Data/Analysis/ae_frame.csv (AE01). READ-ONLY DESCRIPTIVE.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
need <- function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
need("fixest"); need("scales")
suppressPackageStartupMessages({ library(fixest); library(scales) })

# ---- logging ----
.log_path <- here::here("logs", paste0("AE03_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE03_Transition_Evidence.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# ============================ COMMON PUBLICATION BLOCK ========================
FIG_DIR <- here("Output", "Figures"); TAB_DIR <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
SIZE_COL <- c("1" = "#bdd7e7", "2" = "#6baed6", "3" = "#3182bd", "4+" = "#08519c")
SIZE_LEV <- c("1", "2", "3", "4+")
th_ae <- theme_minimal(base_size = 11) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_blank(), legend.position = "bottom",
        panel.grid.minor = element_blank())
save_fig <- function(p, stem, width = 7, height = 4.5) {
  ggsave(file.path(FIG_DIR, paste0(stem, ".png")), p, width = width, height = height, dpi = 300)
  ggsave(file.path(FIG_DIR, paste0(stem, ".pdf")), p, width = width, height = height)
  cat(sprintf("  saved %s .png/.pdf\n", stem))
}
BAD_SPECIAL <- "[%_$&#~^<>]"
assert_ascii_clean <- function(x, where) {
  v <- as.character(unlist(x)); hit <- grepl(BAD_SPECIAL, v)
  if (any(hit)) stop(sprintf("forbidden special char in %s: %s", where,
                             paste(unique(v[hit]), collapse = " | ")))
  invisible(TRUE)
}
write_booktabs <- function(dt, path, align, header, panels = NULL) {
  assert_ascii_clean(dt, paste("cells of", basename(path)))
  assert_ascii_clean(header, paste("header of", basename(path)))
  if (!is.null(panels)) assert_ascii_clean(unlist(panels), paste("panels of", basename(path)))
  ncol_t <- length(header)
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c(sprintf("\\begin{tabular}{%s}", align), "\\toprule",
               paste0(paste(header, collapse = " & "), " \\\\"), "\\midrule"), con)
  for (i in seq_len(nrow(dt))) {
    lbl <- if (!is.null(panels)) panels[[as.character(i)]] else NULL
    if (!is.null(lbl))
      writeLines(c(sprintf("\\multicolumn{%d}{l}{%s} \\\\", ncol_t, lbl), "\\midrule"), con)
    writeLines(paste0(paste(as.character(unlist(dt[i])), collapse = " & "), " \\\\"), con)
  }
  writeLines(c("\\bottomrule", "\\end{tabular}"), con)
  cat(sprintf("  saved %s\n", basename(path)))
}
star <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", ""))))
fmt_n <- function(x) format(as.integer(x), big.mark = ",")
# State SW-install ban dates (hardcoded; email-verified sheet read 2026-06-09)
STATE_BAN <- c(MA = "1989-01-01", ME = "1991-09-16", ID = "2007-02-23", AR = "2007-07-01",
               TN = "2007-07-24", AL = "2007-08-06", NC = "2007-11-01", MN = "2007-12-22",
               IL = "2008-02-01", OK = "2008-07-01", CO = "2008-08-01", LA = "2008-12-20",
               TX = "2009-01-01", ND = "2009-01-01", SD = "2009-01-01", MD = "2009-01-12",
               VA = "2010-09-15", OH = "2011-05-16", KY = "2012-04-01", KS = "2013-07-01",
               MO = "2017-07-01")
BAN_YEAR <- setNames(as.integer(substr(STATE_BAN, 1, 4)), names(STATE_BAN))
# =============================================================================

cat("=== AE03 TRANSITION EVIDENCE ===\n")
fr <- fread(file.path(DATA_DIR, "ae_frame.csv"))
fr[, panel_id := as.character(panel_id)]
fr[, size_bin := factor(size_bin, levels = SIZE_LEV)]

# =============================================================================
# X4. CAPACITY IS A NEAR-FIXED TYPE  (dcm spine; 04ag re-emitted + open cell B)
# =============================================================================
cat("=== X4: capacity stickiness (Panel A fixed-type, Panel B G-move) ===\n")
capf <- fread(file.path(DATA_DIR, "facility_panel.csv"),
              select = c("panel_id", "panel_year", "total_capacity"))
capf[, panel_id := as.character(panel_id)]
sx <- merge(fr[on_spine == 1L, .(panel_id, panel_year, state, action, size_bin, boy_stock)],
            capf, by = c("panel_id", "panel_year"), all.x = TRUE)
sx <- sx[boy_stock >= 1L & !is.na(total_capacity)]
setorder(sx, panel_id, panel_year)
sx[, count_bin := size_bin]
cap_brk <- quantile(sx$total_capacity, c(0, .25, .5, .75, 1), na.rm = TRUE)
sx[, cap_bin := cut(total_capacity, breaks = unique(cap_brk), include.lowest = TRUE,
                    labels = c("Q1", "Q2", "Q3", "Q4")[seq_len(length(unique(cap_brk)) - 1)])]

one_year_stay <- function(d, col) {
  d <- copy(d)
  d[, nx  := shift(get(col), type = "lead"), by = panel_id]
  d[, nyr := shift(panel_year, type = "lead"), by = panel_id]
  tr <- d[!is.na(nx) & nyr == panel_year + 1L]
  mean(as.character(tr[[col]]) == as.character(tr$nx))
}
stay_count <- one_year_stay(sx, "count_bin")
stay_cap   <- one_year_stay(sx, "cap_bin")
spell <- sx[, .(n_years = .N,
                chg_count = uniqueN(count_bin) > 1L,
                chg_cap   = uniqueN(cap_bin)   > 1L), by = panel_id]
mean_spell <- mean(spell$n_years)
x4A <- data.table(
  panel = "A",
  metric = c("one_year_stay", "mean_spell_yrs", "implied_never_change", "actual_never_change"),
  count    = c(stay_count, mean_spell, stay_count^mean_spell, mean(!spell$chg_count)),
  capacity = c(stay_cap,   mean_spell, stay_cap^mean_spell,   mean(!spell$chg_cap)))

# Panel B: capacity-quartile (G) move at downsize, by size
sx[, cap_next := shift(cap_bin, type = "lead"), by = panel_id]
sx[, nyr := shift(panel_year, type = "lead"), by = panel_id]
dn <- sx[action == "Downsize" & !is.na(cap_next) & nyr == panel_year + 1L]
dn[, gmove := as.integer(as.character(cap_bin) != as.character(cap_next))]
x4B <- dn[, .(panel = "B", metric = "g_move_at_downsize", group = as.character(size_bin),
              n = .N, share = round(mean(gmove), 4)), by = size_bin][order(size_bin)]
x4B[, size_bin := NULL]
# single combined deliverable CSV (Panel A metrics + Panel B G-move rows)
x4A_long <- melt(x4A, id.vars = c("panel", "metric"),
                 measure.vars = c("count", "capacity"),
                 variable.name = "group", value.name = "value")
x4A_long[, `:=`(group = as.character(group), n = NA_integer_)]
x4_csv <- rbind(x4A_long[, .(panel, metric, group, n, value)],
                x4B[, .(panel, metric, group, n, value = share)])
fwrite(x4_csv, file.path(TAB_DIR, "AE_X4_Capacity_Stickiness.csv"))
cat("  Panel A:\n"); print(x4A)
cat("  Panel B (G-move at downsize by size):\n"); print(x4B)
gmove_overall <- dn[, mean(gmove)]
cat(sprintf("  DECISION READ: overall G-move at downsize = %.3f -> %s\n", gmove_overall,
            ifelse(gmove_overall < 0.10, "SMALL (<10pct): G frozen everywhere",
                   "LARGE: G step-down kernel at D only")))

# X4 .tex (starred): Panel A (4 metrics x count/capacity) + Panel B (size x N, move share)
pa <- x4A[, .(Item = c("one year stay", "mean spell yrs", "implied never change",
                       "actual never change"),
              Tanks = c(sprintf("%.3f", count[1]), sprintf("%.1f", count[2]),
                        sprintf("%.3f", count[3]), sprintf("%.3f", count[4])),
              Capacity = c(sprintf("%.3f", capacity[1]), sprintf("%.1f", capacity[2]),
                           sprintf("%.3f", capacity[3]), sprintf("%.3f", capacity[4])))]
pb <- x4B[, .(Item = paste("size", group), Tanks = fmt_n(n),
              Capacity = sprintf("%.3f", share))]
x4tex <- rbind(pa, pb)
write_booktabs(x4tex, file.path(TAB_DIR, "AE_X4_Capacity_Stickiness.tex"),
               align = "lrr", header = c("Item", "Tanks or N", "Capacity or share"),
               panels = list("1" = "Panel A. Size stickiness, one-year vs whole-spell",
                             "5" = "Panel B. Capacity quartile move at downsize by size"))

# =============================================================================
# X8. REPLACE RECYCLES CAPACITY  (closure-years ONLY; M3 contamination fixed)
# =============================================================================
cat("=== X8: replace recycles capacity ===\n")
trf <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "net_tank_change", "capacity_change",
                        "lag_capacity"))
trf[, panel_id := as.character(panel_id)]
# population: operating AND any_closure==1 (NO install-only years)
ev <- merge(fr[facility_complete_closure == 0L & any_closure == 1L],
            trf, by = c("panel_id", "panel_year"), all.x = TRUE)
ev[, swap := as.integer(n_installs > 0L)]
ev[, event_type := factor(swap, levels = c(1L, 0L), labels = c("swap", "shrink"))]
ev[, dcap_pct := NA_real_]
ev[!is.na(lag_capacity) & lag_capacity > 0, dcap_pct := 100 * capacity_change / lag_capacity]
ev[, dcount_bin := fcase(net_tank_change == 0L, "0", net_tank_change == -1L, "-1",
                         net_tank_change == -2L, "-2", net_tank_change <= -3L, "<=-3",
                         default = NA_character_)]
# tank-level act: shed an SW and installed a DW
boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
shedsw <- boylong[wall == "SW", .(sw_shed = sum(n_shed)), by = .(panel_id, panel_year)]
ev <- merge(ev, shedsw, by = c("panel_id", "panel_year"), all.x = TRUE)
ev[is.na(sw_shed), sw_shed := 0L]
ev[, shedSW_instDW := as.integer(sw_shed > 0L & n_inst_dw > 0L)]
cat("  event counts by type:\n"); print(ev[, .N, by = event_type])
stopifnot(ev[event_type == "swap", .N] > 0L, ev[event_type == "shrink", .N] > 0L)

x8 <- ev[!is.na(dcount_bin), .(
  n = .N,
  share_within10pct      = round(mean(abs(dcap_pct) <= 10, na.rm = TRUE), 6),
  median_dcap_pct        = round(median(dcap_pct, na.rm = TRUE), 4),
  p25                    = round(quantile(dcap_pct, .25, na.rm = TRUE), 4),
  p75                    = round(quantile(dcap_pct, .75, na.rm = TRUE), 4),
  share_shedSW_instDW    = round(mean(shedSW_instDW), 6)),
  by = .(event_type, dcount_bin)]
grid8 <- CJ(event_type = factor(c("swap", "shrink"), levels = c("swap", "shrink")),
            dcount_bin = c("0", "-1", "-2", "<=-3"), sorted = FALSE)
x8 <- x8[grid8, on = .(event_type, dcount_bin)]
x8[is.na(n), n := 0L]
N_evt <- ev[!is.na(dcount_bin), .(N_event = .N), by = event_type]
x8 <- merge(x8, N_evt, by = "event_type")
x8[, share := round(n / N_event, 6)][, N_event := NULL]
setcolorder(x8, c("event_type", "dcount_bin", "n", "share", "median_dcap_pct",
                  "p25", "p75", "share_within10pct", "share_shedSW_instDW"))
setorder(x8, event_type, dcount_bin)
fwrite(x8, file.path(TAB_DIR, "AE_X8_Recycle.csv"))
cat("  saved AE_X8_Recycle.csv\n"); print(x8)

# X8 tests: feols dcap_pct ~ swap and net_tank_change ~ swap on FIXED population
f1 <- feols(dcap_pct ~ swap, data = ev, cluster = ~state)
f2 <- feols(net_tank_change ~ swap, data = ev, cluster = ~state)
ct1 <- coeftable(f1); ct2 <- coeftable(f2)
x8t <- data.table(
  outcome = c("dcap_pct", "net_tank_change"), term = "swap",
  coef = c(ct1["swap", 1], ct2["swap", 1]),
  se   = c(ct1["swap", 2], ct2["swap", 2]),
  p    = c(ct1["swap", 4], ct2["swap", 4]))
fwrite(x8t, file.path(TAB_DIR, "AE_X8_Recycle_Tests.csv"))
cat("  saved AE_X8_Recycle_Tests.csv\n"); print(x8t)

# X8 .tex (starred)
dcl <- c("0" = "0", "-1" = "-1", "-2" = "-2", "<=-3" = "-3 or more")
x8tex <- x8[, .(Event = as.character(event_type), `Count chg` = dcl[dcount_bin],
                N = fmt_n(n), Share = sprintf("%.3f", share),
                `Med cap chg` = sprintf("%.1f", median_dcap_pct),
                `Shed SW inst DW` = sprintf("%.3f", share_shedSW_instDW))]
write_booktabs(x8tex, file.path(TAB_DIR, "AE_X8_Recycle.tex"), align = "llrrrr",
               header = c("Event", "Count chg", "N", "Share", "Med cap chg", "Shed SW inst DW"))

# X8 figure: dcap density swap vs shrink (display trim noted in qmd caption)
evd <- ev[!is.na(dcap_pct) & dcap_pct > -100 & dcap_pct < 200]
pX8 <- ggplot(evd, aes(dcap_pct, fill = event_type, color = event_type)) +
  geom_density(alpha = 0.35, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  scale_fill_manual(values = c(swap = RB_COL, shrink = FF_COL), name = NULL) +
  scale_color_manual(values = c(swap = RB_COL, shrink = FF_COL), name = NULL) +
  labs(x = "Capacity change (pct of lagged)", y = "Density") +
  th_ae
save_fig(pX8, "AE_X8_Capacity_Density", width = 7, height = 4.5)

# =============================================================================
# X9. NEW TANKS ARE DOUBLE-WALLED  (Replace at has_SW; tank-level classes)
# =============================================================================
cat("=== X9: install decomposition ===\n")
shedwall <- boylong[, .(sw_shed = sum(n_shed[wall == "SW"]),
                        dw_shed = sum(n_shed[wall == "DW"])), by = .(panel_id, panel_year)]
rep9 <- merge(fr[action == "Replace" & has_SW == 1L],
              shedwall, by = c("panel_id", "panel_year"), all.x = TRUE)
for (cc in c("sw_shed", "dw_shed")) rep9[is.na(get(cc)), (cc) := 0L]
rep9[, inst_dw := as.integer(n_inst_dw > 0L)]
rep9[, inst_sw := as.integer((n_inst - n_inst_dw) > 0L)]
rep9[, shed_sw := as.integer(sw_shed > 0L)]
rep9[, install_class := fcase(
  shed_sw == 1L & inst_dw == 1L, "shed SW inst DW",
  shed_sw == 1L & inst_dw == 0L, "shed SW inst SW",
  default                       = "shed DW only")]
rep9[, ban_year := BAN_YEAR[state]]
rep9[, ban_timing := fifelse(!is.na(ban_year) & panel_year >= ban_year, "post ban", "pre ban")]
total_hasSW_rep <- nrow(rep9)
CLASS_LEV <- c("shed SW inst DW", "shed SW inst SW", "shed DW only")
x9 <- rep9[, .(n = .N), by = .(install_class, ban_timing)]
x9 <- merge(CJ(install_class = CLASS_LEV, ban_timing = c("pre ban", "post ban")),
            x9, by = c("install_class", "ban_timing"), all.x = TRUE)
x9[is.na(n), n := 0L]
x9[, share := round(n / total_hasSW_rep, 6)]
x9[, install_class := factor(install_class, levels = CLASS_LEV)]
setorder(x9, install_class, ban_timing)
stopifnot(sum(x9$n) == total_hasSW_rep)
fwrite(x9, file.path(TAB_DIR, "AE_X9_Install_Decomposition.csv"))
cat(sprintf("  saved AE_X9_Install_Decomposition.csv | has_SW replace events: %s\n",
            fmt_n(total_hasSW_rep)))
print(x9)
true_viol <- rep9[install_class == "shed DW only", .N] / total_hasSW_rep
postban_instsw <- rep9[ban_timing == "post ban" & install_class == "shed SW inst SW", .N] /
  max(rep9[ban_timing == "post ban", .N], 1L)
cat(sprintf("  TRUE-VIOLATION share (shed DW only) = %.4f | post-ban inst-SW share (tripwire) = %.4f\n",
            true_viol, postban_instsw))

x9tex <- x9[, .(`Install class` = as.character(install_class), `Ban timing` = ban_timing,
                N = fmt_n(n), `Share pct` = sprintf("%.2f", 100 * share))]
write_booktabs(x9tex, file.path(TAB_DIR, "AE_X9_Install_Decomposition.tex"), align = "llrr",
               header = c("Install class", "Ban timing", "N", "Share pct"))

# X9 figure: DW share of installs by year, TX vs Control; rug at ban dates
inst_yr <- fr[n_inst > 0L, .(n_inst = sum(n_inst), n_dw = sum(n_inst_dw)),
              by = .(panel_year, group)]
inst_yr[, dw_share := n_dw / n_inst]
ban_years_dt <- data.table(by = as.integer(substr(STATE_BAN, 1, 4)))
pX9 <- ggplot(inst_yr, aes(panel_year, dw_share, color = group)) +
  geom_rug(data = ban_years_dt, aes(x = by), inherit.aes = FALSE, sides = "b",
           color = "grey50", alpha = 0.6) +
  geom_line(linewidth = 1) + geom_point(size = 1.4) +
  scale_color_manual(values = c(TX = RB_COL, Control = FF_COL), name = NULL) +
  scale_y_continuous(labels = percent) +
  labs(x = "Year", y = "DW share of installs", color = NULL) +
  th_ae
save_fig(pX9, "AE_X9_Install_DW_Share", width = 7, height = 4.5)

# =============================================================================
# X10. THE REGIME MOVES THE MARGINS  (dcm spine; NO regime x wall interaction)
# =============================================================================
cat("=== X10: regime margins ===\n")
sp <- fr[on_spine == 1L]
sp[, A_bin_f := factor(A_bin)]
sp[, wall := factor(w_state, levels = c(1L, 2L), labels = c("SW", "DW"))]
sp[, size_bin := factor(size_bin, levels = SIZE_LEV)]
sp[, regime := regime_rb]
x10_one <- function(d, k, has_wall, lbl, samp_lbl) {
  d <- copy(d); d[, y_k := as.integer(action == k)]
  form <- if (has_wall) y_k ~ A_bin_f + wall + size_bin + regime | panel_year
          else          y_k ~ A_bin_f + size_bin + regime | panel_year
  fit <- feglm(form, data = d, family = binomial(), cluster = ~state)
  ct <- coeftable(fit)
  nrb <- uniqueN(d[regime == 1L, state])
  data.table(margin = lbl, sample = samp_lbl, odds_ratio = exp(ct["regime", 1]),
             se = ct["regime", 2], p = ct["regime", 4], n = fit$nobs, n_states_RB = nrb)
}
x10 <- rbindlist(list(
  x10_one(sp,                 "Downsize", TRUE,  "Downsize", "full"),
  x10_one(sp[has_SW == 1L],   "Replace",  FALSE, "Replace",  "has_SW"),
  x10_one(sp,                 "Exit",     TRUE,  "Exit",     "full")))
fwrite(x10, file.path(TAB_DIR, "AE_X10_Regime_Margins.csv"))
cat("  saved AE_X10_Regime_Margins.csv\n"); print(x10)

x10tex <- x10[, .(Margin = margin, Sample = fifelse(sample == "has_SW", "has SW", sample),
                  OR = paste0(sprintf("%.3f", odds_ratio), star(p)),
                  SE = sprintf("%.3f", se), N = fmt_n(n), `States RB` = fmt_n(n_states_RB))]
write_booktabs(x10tex, file.path(TAB_DIR, "AE_X10_Regime_Margins.tex"), align = "llrrrr",
               header = c("Margin", "Sample", "OR", "SE", "N", "States RB"))

cat("=== AE03 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
