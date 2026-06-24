# ==============================================================================
# AE02_State_Evidence.R  -- TICKET 021, script 2 of 5
# ==============================================================================
# X1  what facilities do            (4-action set + Expansion fencing)
# X2  size shifts behavior at state  (counts belong in the state)
# X5  tanks within a facility alike  (16 coarse cells)
# X12 state space is covered         (feasibility)
# Consumes Data/Analysis/ae_frame.csv (AE01). READ-ONLY DESCRIPTIVE.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
need <- function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
need("fixest"); need("scales")
suppressPackageStartupMessages({ library(fixest); library(scales) })

# ---- logging ----
.log_path <- here::here("logs", paste0("AE02_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE02_State_Evidence.R\nR: %s\nWD: %s\n\n",
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
ACTION_LEV <- c("Exit", "Replace", "Downsize", "Expansion", "Maintain")
# =============================================================================

cat("=== AE02 STATE EVIDENCE ===\n")
fr <- fread(file.path(DATA_DIR, "ae_frame.csv"))
fr[, panel_id := as.character(panel_id)]
fr[, size_bin := factor(size_bin, levels = SIZE_LEV)]
cat(sprintf("  frame rows: %s | on spine: %s\n",
            fmt_n(nrow(fr)), fmt_n(sum(fr$on_spine))))

# =============================================================================
# X1. WHAT FACILITIES DO  (full frame; physical)
# =============================================================================
cat("=== X1: action frequencies + simultaneity ===\n")
x1_one <- function(d, lab) {
  tot <- nrow(d)
  z <- d[, .(n = .N), by = action]
  z <- merge(data.table(action = ACTION_LEV), z, by = "action", all.x = TRUE)
  z[is.na(n), n := 0L]
  z[, `:=`(size_bin = lab, rate_pct = round(100 * n / tot, 4))]
  z[match(ACTION_LEV, action)]
}
x1 <- rbindlist(c(
  lapply(SIZE_LEV, function(s) x1_one(fr[size_bin == s], s)),
  list(x1_one(fr, "All"))))
x1 <- x1[, .(action, size_bin, n, rate_pct)]
fwrite(x1, file.path(TAB_DIR, "AE_X1_Action_Frequencies.csv"))
cat("  saved AE_X1_Action_Frequencies.csv\n"); print(dcast(x1, action ~ size_bin, value.var = "rate_pct"))

# simultaneity grid: sheds x installs (tank-level magnitudes from boyfy counts)
binc <- function(x) fcase(x == 0L, "0", x == 1L, "1", x == 2L, "2", x >= 3L, "3+")
fr[, shed_b := factor(binc(n_shed_total), levels = c("0", "1", "2", "3+"))]
fr[, inst_b := factor(binc(n_inst),       levels = c("0", "1", "2", "3+"))]
sim <- fr[, .(n = .N), by = .(sheds = shed_b, installs = inst_b)]
sim <- merge(CJ(sheds = factor(c("0", "1", "2", "3+"), levels = c("0", "1", "2", "3+")),
                installs = factor(c("0", "1", "2", "3+"), levels = c("0", "1", "2", "3+"))),
             sim, by = c("sheds", "installs"), all.x = TRUE)
sim[is.na(n), n := 0L]
sim[, share := round(n / sum(n), 6)]
setorder(sim, sheds, installs)
fwrite(sim, file.path(TAB_DIR, "AE_X1_Simultaneity.csv"))
n_sim_head <- fr[n_shed_total >= 2L & n_inst >= 2L, .N]
cat(sprintf("  saved AE_X1_Simultaneity.csv | facility-years with 2+ sheds AND 2+ installs: %s\n",
            fmt_n(n_sim_head)))

# X1 .tex: Panel A = All column 5 action rows; Panel B = simultaneity headline row
x1_all <- x1[size_bin == "All"]
panelB <- data.table(action = "2+ sheds and 2+ installs", size_bin = "All",
                     n = n_sim_head, rate_pct = round(100 * n_sim_head / nrow(fr), 4))
x1tex_dt <- rbind(x1_all, panelB)
x1tex_out <- x1tex_dt[, .(Action = action, N = fmt_n(n), `Rate pct` = sprintf("%.2f", rate_pct))]
write_booktabs(x1tex_out, file.path(TAB_DIR, "AE_X1_Action_Frequencies.tex"),
               align = "lrr", header = c("Action", "N", "Rate pct"),
               panels = list("1" = "Panel A. Action frequency, all facility-years",
                             "6" = "Panel B. Simultaneous shed and install"))

# =============================================================================
# X2. SIZE SHIFTS BEHAVIOR AT FIXED STATE  (dcm spine; 019-M1 design)
# =============================================================================
cat("=== X2: size logit + CCP by size ===\n")
sp <- fr[on_spine == 1L]
sp[, size_bin := factor(size_bin, levels = SIZE_LEV)]
X2_MARG <- c("Exit", "Downsize", "Replace")
size_terms <- c("size_bin2", "size_bin3", "size_bin4+")
size_lab   <- c("size 2", "size 3", "size 4 plus")
x2_rows <- list()
for (k in X2_MARG) {
  sp[, y_k := as.integer(action == k)]
  fit <- feglm(y_k ~ size_bin | s_idx, data = sp, family = binomial(), cluster = ~state)
  ct <- coeftable(fit)
  cat(sprintf("  [X2] margin=%-9s nobs=%s\n", k, fmt_n(fit$nobs)))
  for (j in seq_along(size_terms)) {
    tm <- size_terms[j]
    x2_rows[[length(x2_rows) + 1L]] <- data.table(
      margin = k, size_term = size_lab[j], odds_ratio = exp(ct[tm, 1]),
      se = ct[tm, 2], p = ct[tm, 4], n = fit$nobs)
  }
}
x2 <- rbindlist(x2_rows)
stopifnot(all(is.finite(x2$odds_ratio)))
fwrite(x2, file.path(TAB_DIR, "AE_X2_Size_Logit.csv"))
cat("  saved AE_X2_Size_Logit.csv\n")
print(x2[, .(margin, size_term, odds_ratio = round(odds_ratio, 3), se = round(se, 4), p = round(p, 4))])

# X2 .tex: size-term rows x margin OR columns, stars
x2[, cell := paste0(sprintf("%.2f", odds_ratio), star(p))]
x2w <- dcast(x2, size_term ~ margin, value.var = "cell")
x2w <- x2w[match(size_lab, size_term)]
setcolorder(x2w, c("size_term", X2_MARG))
x2tex_out <- x2w[, .(Size = size_term, Exit, Downsize, Replace)]
write_booktabs(x2tex_out, file.path(TAB_DIR, "AE_X2_Size_Logit.tex"),
               align = "lrrr", header = c("Size vs one", "Exit", "Downsize", "Replace"))

# X2 figure: empirical margin shares by (A_bin, size_bin), facet margin ~ wall
sp[, wall := factor(w_state, levels = c(1L, 2L), labels = c("SW", "DW"))]
for (k in X2_MARG) sp[, (paste0("ind_", k)) := as.integer(action == k)]
agg2 <- sp[!is.na(A_bin) & !is.na(wall),
           lapply(.SD, mean), by = .(A_bin, size_bin, wall),
           .SDcols = paste0("ind_", X2_MARG)]
agg2l <- melt(agg2, id.vars = c("A_bin", "size_bin", "wall"),
              measure.vars = paste0("ind_", X2_MARG), variable.name = "margin", value.name = "share")
agg2l[, margin := factor(sub("^ind_", "", margin), levels = X2_MARG)]
pX2 <- ggplot(agg2l, aes(A_bin, share, color = size_bin, group = size_bin)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.4) +
  facet_grid(margin ~ wall, scales = "free_y") +
  scale_color_manual(values = SIZE_COL, name = "Facility size", drop = FALSE) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(x = "Age bin", y = "Share of facility-years", color = "Facility size") +
  th_ae
save_fig(pX2, "AE_X2_CCP_by_Size", width = 7, height = 7)

# =============================================================================
# X5. WITHIN-FACILITY HOMOGENEITY  (dcm spine; 04ah re-emitted)
# =============================================================================
cat("=== X5: within-facility homogeneity ===\n")
hfac <- fread(file.path(DATA_DIR, "facility_panel.csv"),
              select = c("panel_id", "panel_year", "max_tank_age", "min_tank_age",
                         "all_sw", "all_dw", "mixed_wall", "active_tanks"))
hfac[, panel_id := as.character(panel_id)]
h <- merge(sp[, .(panel_id, panel_year, size_bin)], hfac,
           by = c("panel_id", "panel_year"), all.x = TRUE)
h <- h[!is.na(max_tank_age) & !is.na(min_tank_age)]
bin_of <- function(a) pmin(as.integer(floor(a / 5)) + 1L, 8L)
h[, age_homog  := bin_of(min_tank_age) == bin_of(max_tank_age)]
h[, wall_homog := (all_sw == 1L) | (all_dw == 1L)]
h[, both_homog := age_homog & wall_homog]
h[, age_range  := max_tank_age - min_tank_age]
x5 <- h[, .(n_fac_yrs = .N,
            pct_wall_homog = round(100 * mean(wall_homog), 1),
            pct_age_homog  = round(100 * mean(age_homog), 1),
            pct_both_homog = round(100 * mean(both_homog), 1),
            median_age_range = as.numeric(median(age_range))),
        by = size_bin][order(size_bin)]
fwrite(x5, file.path(TAB_DIR, "AE_X5_Homogeneity.csv"))
cat("  saved AE_X5_Homogeneity.csv\n"); print(x5)

x5l <- melt(x5[, .(size_bin, Wall = pct_wall_homog, `Age one bin` = pct_age_homog, Both = pct_both_homog)],
            id.vars = "size_bin", variable.name = "measure", value.name = "pct")
x5l[, measure := factor(measure, levels = c("Wall", "Age one bin", "Both"))]
pX5 <- ggplot(x5l, aes(size_bin, pct / 100, fill = measure)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  scale_fill_manual(values = c("Wall" = "#457B9D", "Age one bin" = FF_COL, "Both" = "#1D3557"),
                    name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  labs(x = "Facility size", y = "Share of facility-years homogeneous", fill = NULL) +
  th_ae
save_fig(pX5, "AE_X5_Homogeneity", width = 7, height = 4.5)

# =============================================================================
# X12. STATE SPACE IS COVERED  (E5 occupancy re-emitted; boylong full frame)
# =============================================================================
cat("=== X12: occupancy / coverage ===\n")
boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 1999L]
boylong[, cellname := sprintf("%s_%d", wall, age_bin)]
ALL_CELLS <- as.vector(t(outer(c("SW", "DW"), 1:8, function(w, b) sprintf("%s_%d", w, b))))
wcomp <- dcast(boylong, panel_id + panel_year ~ cellname, value.var = "n_boy", fill = 0)
for (mc in setdiff(ALL_CELLS, names(wcomp))) wcomp[, (mc) := 0L]
setcolorder(wcomp, c("panel_id", "panel_year", ALL_CELLS))
cm <- as.matrix(wcomp[, ..ALL_CELLS])
wcomp[, comp_key := apply(cm, 1L, paste, collapse = "-")]
wcomp[, n_occ := rowSums(cm > 0)]
wcomp[, N := rowSums(cm)]
keytab <- wcomp[, .(nfy = .N), by = comp_key][order(-nfy)]
keytab[, cumshare := cumsum(nfy) / sum(nfy)]
tot_fy <- nrow(wcomp)
cov_at <- function(k) round(sum(keytab$nfy[seq_len(min(k, nrow(keytab)))]) / tot_fy, 6)
n_for  <- function(pp) which(keytab$cumshare >= pp)[1]
x12 <- data.table(
  metric = c("n_compositions", "top10_cov", "top100_cov", "top500_cov",
             "n_for_95pct", "n_for_99pct", "share_single_cell",
             "share_N_gt4", "share_N_gt6", "p50_N", "p90_N", "p99_N"),
  value = c(nrow(keytab), cov_at(10), cov_at(100), cov_at(500),
            n_for(0.95), n_for(0.99), round(mean(wcomp$n_occ == 1L), 6),
            round(mean(wcomp$N > 4L), 6), round(mean(wcomp$N > 6L), 6),
            as.numeric(quantile(wcomp$N, 0.50)), as.numeric(quantile(wcomp$N, 0.90)),
            as.numeric(quantile(wcomp$N, 0.99))))
fwrite(x12, file.path(TAB_DIR, "AE_X12_Occupancy.csv"))
cat("  saved AE_X12_Occupancy.csv\n"); print(x12)

keytab[, rank := .I]
pX12 <- ggplot(keytab, aes(rank, cumshare)) +
  geom_line(color = RB_COL, linewidth = 0.9) +
  geom_hline(yintercept = c(0.95, 0.99), linetype = "dashed", color = "grey50") +
  scale_x_log10(labels = comma) + scale_y_continuous(labels = percent) +
  labs(x = "Composition rank (log scale)", y = "Cumulative share of facility-years") +
  th_ae
save_fig(pX12, "AE_X12_Coverage", width = 7, height = 4.5)

cat("=== AE02 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
