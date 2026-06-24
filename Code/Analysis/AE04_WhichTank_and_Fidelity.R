# ==============================================================================
# AE04_WhichTank_and_Fidelity.R  -- TICKET 021, script 4 of 5
# ==============================================================================
# X6  which tank goes        (the removal rule) -- conditional logit, 04ak exact
# X7  the rule predicts removals (validates rule + how-many) -- 04ao fidelity
# SELF-CONTAINED: copies the action-coding block verbatim (does not read ae_frame).
# The ONLY script allowed to read panel_dt. READ-ONLY DESCRIPTIVE.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
need <- function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
need("survival"); need("scales")
suppressPackageStartupMessages({ library(survival); library(scales) })

# ---- logging ----
.log_path <- here::here("logs", paste0("AE04_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE04_WhichTank_and_Fidelity.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# ============================ COMMON PUBLICATION BLOCK ========================
FIG_DIR <- here("Output", "Figures"); TAB_DIR <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
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
fmt_n <- function(x) format(as.integer(x), big.mark = ",")
# =============================================================================

cat("=== AE04 WHICH TANK + FIDELITY ===\n")

# =============================================================================
# X6. WHICH TANK GOES  (conditional logit; 04ak exact spec + pub outputs)
# =============================================================================
cat("=== X6: which tank (conditional logit) ===\n")
dt <- fread(file.path(DATA_DIR, "panel_dt.csv"),
            select = c("tank_panel_id", "panel_id", "panel_year", "state",
                       "mm_wall", "capacity", "tank_age", "closure_event"))
cat(sprintf("  tank-years read: %s\n", fmt_n(nrow(dt))))
dt <- dt[!is.na(tank_age) & !is.na(capacity) & capacity > 0 & !is.na(closure_event)]
dt[, sw := as.integer(grepl("single", mm_wall, ignore.case = TRUE))]

fy <- dt[, .(n_active = .N, n_closed = sum(closure_event)), by = .(panel_id, panel_year)]
tgt <- fy[n_active >= 2L & n_closed == 1L, .(panel_id, panel_year)]
sub <- dt[tgt, on = .(panel_id, panel_year)]
sub[, strata_id := paste(panel_id, panel_year)]
n_strata <- uniqueN(sub$strata_id); n_obs <- nrow(sub)
cat(sprintf("  n_strata (choice occasions) = %s | n_obs (alternatives) = %s\n",
            fmt_n(n_strata), fmt_n(n_obs)))
stopifnot(n_strata >= 19000L, n_strata <= 20500L)

# In-formula cluster(state); method="approximate" (single event per stratum -> same
# point estimates as exact, supports robust/clustered variance). Errors surface.
fit <- clogit(closure_event ~ tank_age + sw + I(capacity / 1000) +
                strata(strata_id) + cluster(state), data = sub, method = "approximate")
ct <- summary(fit)$coefficients
terms_in  <- c("tank_age", "sw", "I(capacity/1000)")
terms_out <- c("age per year", "SW", "capacity per 1000 gal")
se_col <- if ("robust se" %in% colnames(ct)) "robust se" else "se(coef)"
coef_v <- ct[terms_in, "coef"]; se_v <- ct[terms_in, se_col]; p_v <- ct[terms_in, "Pr(>|z|)"]
if (fit$nevent != n_strata) warning(sprintf("clogit events %d != n_strata %d", fit$nevent, n_strata))

x6 <- data.table(term = terms_out, odds_ratio = exp(coef_v), coef = coef_v,
                 se = se_v, p = p_v, n_strata = n_strata, n_obs = n_obs)
stopifnot(all(is.finite(x6$odds_ratio)))
fwrite(x6, file.path(TAB_DIR, "AE_X6_WhichTank.csv"))
cat("  saved AE_X6_WhichTank.csv\n"); print(x6)

x6tex <- x6[, .(Term = term, OR = sprintf("%.3f", odds_ratio),
                SE = sprintf("%.3f", se), p = sprintf("%.3f", p))]
write_booktabs(x6tex, file.path(TAB_DIR, "AE_X6_WhichTank.tex"), align = "lrrr",
               header = c("Closed tank attribute", "OR", "SE", "p"))

# forest plot (AER style)
x6f <- data.table(term = factor(terms_out, levels = rev(terms_out)),
                  or = x6$odds_ratio,
                  lo = exp(x6$coef - 1.96 * x6$se), hi = exp(x6$coef + 1.96 * x6$se))
pX6 <- ggplot(x6f, aes(or, term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, color = RB_COL, linewidth = 0.9) +
  geom_point(size = 3, color = "#264653") +
  labs(x = "Odds ratio (95 pct CI)", y = NULL) +
  th_ae
save_fig(pX6, "AE_X6_WhichTank_Forest", width = 7, height = 3.5)
rm(dt, fy, sub); gc()

# =============================================================================
# X7. THE RULE PREDICTS REMOVALS  (04ao fidelity; action coding copied verbatim)
# =============================================================================
cat("=== X7: removal fidelity ===\n")
boyfy <- fread(file.path(DATA_DIR, "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
boyfy <- boyfy[panel_year >= 1999L & N >= 1L]
boyfy[, size_bin := fcase(N == 1L, "1", N == 2L, "2", N == 3L, "3", N >= 4L, "4+",
                          default = NA_character_)]
fac <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "any_closure",
                        "facility_complete_closure", "n_installs"))
fac[, panel_id := as.character(panel_id)]
for (cc in c("any_closure", "facility_complete_closure", "n_installs"))
  fac[is.na(get(cc)), (cc) := 0L]
ev <- merge(boyfy, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
for (cc in c("any_closure", "facility_complete_closure", "n_installs"))
  ev[is.na(get(cc)), (cc) := 0L]
ev[, action := fcase(
  facility_complete_closure == 1L,                "Exit",
  any_closure == 1L & n_installs >  0L,           "Replace",
  any_closure == 1L & n_installs == 0L,           "Downsize",
  any_closure == 0L & n_installs >  0L,           "Expansion",
  default                                          = "Maintain")]

e7pop <- ev[action %chin% c("Downsize", "Replace") & n_shed_total >= 1L]
cat(sprintf("  X7 events: %s (Downsize=%s, Replace=%s)\n", fmt_n(nrow(e7pop)),
            fmt_n(e7pop[action == "Downsize", .N]), fmt_n(e7pop[action == "Replace", .N])))

boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
bl_e <- boylong[e7pop[, .(panel_id, panel_year)], on = c("panel_id", "panel_year"), nomatch = NULL]
bl_e[, cell := sprintf("%s_%d", wall, age_bin)]
MARG <- c(sprintf("SW_%d", 8:1), sprintf("DW_%d", 8:1))
marg_wall <- c(rep("SW", 8), rep("DW", 8)); marg_bin <- c(8:1, 8:1)
wB <- dcast(bl_e, panel_id + panel_year ~ cell, value.var = "n_boy",  fill = 0)
wR <- dcast(bl_e, panel_id + panel_year ~ cell, value.var = "n_shed", fill = 0)
for (mc in setdiff(MARG, names(wB))) wB[, (mc) := 0L]
for (mc in setdiff(MARG, names(wR))) wR[, (mc) := 0L]
e7 <- merge(e7pop[, .(panel_id, panel_year, action, size_bin, n_shed_total)],
            wB[, c("panel_id", "panel_year", MARG), with = FALSE],
            by = c("panel_id", "panel_year"))
Rmat <- as.matrix(merge(e7[, .(panel_id, panel_year)],
                        wR[, c("panel_id", "panel_year", MARG), with = FALSE],
                        by = c("panel_id", "panel_year"))[, ..MARG])
B <- as.matrix(e7[, ..MARG]); R <- Rmat
storage.mode(B) <- "double"; storage.mode(R) <- "double"
k <- rowSums(R)
stopifnot(all(k == e7$n_shed_total))
# predicted shed multiset under marginal order (greedy fill of first k)
CB <- t(apply(B, 1L, cumsum)); before <- CB - B
P <- pmin(pmax(k - before, 0), B)
stopifnot(all(rowSums(P) == k))
e7[, exact_match := as.integer(rowSums(abs(P - R)) == 0)]
is_sw <- marg_wall == "SW"; is_dw <- marg_wall == "DW"; KEPT <- B - R
e7[, wrong_wall := as.integer((rowSums(R[, is_dw, drop = FALSE]) > 0) &
                              (rowSums(KEPT[, is_sw, drop = FALSE]) > 0))]
ageband_viol <- function(Rw, Kw) {
  cs <- t(apply(Kw, 1L, function(z) cumsum(c(0, z[-8]))))
  rowSums((Rw > 0) & (cs > 0)) > 0
}
av_sw <- ageband_viol(R[, is_sw, drop = FALSE], KEPT[, is_sw, drop = FALSE])
av_dw <- ageband_viol(R[, is_dw, drop = FALSE], KEPT[, is_dw, drop = FALSE])
e7[, wrong_ageband := as.integer(av_sw | av_dw)]
e7[, k_shed := fifelse(n_shed_total == 1L, "1", "2+")]

x7 <- e7[!is.na(size_bin), .(
  n = .N,
  exact_match_share   = round(mean(exact_match), 6),
  wrong_wall_share    = round(mean(wrong_wall), 6),
  wrong_ageband_share = round(mean(wrong_ageband), 6)),
  by = .(action, size_bin, k_shed)]
x7[, action := factor(action, levels = c("Downsize", "Replace"))]
x7[, size_bin := factor(size_bin, levels = SIZE_LEV)]
x7[, k_shed := factor(k_shed, levels = c("1", "2+"))]
setorder(x7, action, size_bin, k_shed)
x7 <- x7[, .(action = as.character(action), size_bin = as.character(size_bin),
             k = as.character(k_shed), n, exact_match_share, wrong_wall_share, wrong_ageband_share)]
fwrite(x7, file.path(TAB_DIR, "AE_X7_Removal_Fidelity.csv"))
cat("  saved AE_X7_Removal_Fidelity.csv\n"); print(x7)
cat(sprintf("  POOLED exact-match=%.4f wrong-wall=%.4f wrong-ageband=%.4f\n",
            e7[, mean(exact_match)], e7[, mean(wrong_wall)], e7[, mean(wrong_ageband)]))

# .tex collapsed to action x size
x7c <- e7[!is.na(size_bin), .(n = .N, match = round(mean(exact_match), 3),
                              wrong_wall = round(mean(wrong_wall), 3)),
          by = .(action, size_bin)][order(action, size_bin)]
x7tex <- x7c[, .(Action = action, Size = size_bin, N = fmt_n(n),
                 Match = sprintf("%.3f", match), `Wrong wall` = sprintf("%.3f", wrong_wall))]
write_booktabs(x7tex, file.path(TAB_DIR, "AE_X7_Removal_Fidelity.tex"), align = "llrrr",
               header = c("Action", "Size", "N", "Match", "Wrong wall"))

# Shed vs block: Downsize events, n_shed vs size of top-priority cell
isdown <- e7$action == "Downsize"
Bd <- B[isdown, , drop = FALSE]; kd <- e7$n_shed_total[isdown]; szd <- e7$size_bin[isdown]
first_occ <- max.col(Bd > 0, ties.method = "first")
block <- Bd[cbind(seq_len(nrow(Bd)), first_occ)]
rel <- fcase(kd < block, "less", kd == block, "exactly", kd > block, "more")
svb <- data.table(size_bin = szd, rel = rel)[, .(n = .N), by = .(size_bin, rel)]
svb <- merge(CJ(size_bin = SIZE_LEV, rel = c("less", "exactly", "more")),
             svb, by = c("size_bin", "rel"), all.x = TRUE)
svb[is.na(n), n := 0L]
svb[, share := round(n / sum(n), 6), by = size_bin]
svb[, size_bin := factor(size_bin, levels = SIZE_LEV)]
svb[, rel := factor(rel, levels = c("less", "exactly", "more"))]
setorder(svb, size_bin, rel)
fwrite(svb, file.path(TAB_DIR, "AE_X7_Shed_vs_Block.csv"))
cat("  saved AE_X7_Shed_vs_Block.csv\n"); print(dcast(svb, size_bin ~ rel, value.var = "share"))

# Kernel heat: predicted-cell x realized-cell shed counts (marginal-order pairing)
Kpool <- matrix(0L, 16, 16)
for (i in seq_len(nrow(e7))) {
  pl <- rep.int(1:16, P[i, ]); rl <- rep.int(1:16, R[i, ])
  for (mm in seq_along(pl)) Kpool[pl[mm], rl[mm]] <- Kpool[pl[mm], rl[mm]] + 1L
}
kdt <- as.data.table(which(Kpool > 0, arr.ind = TRUE)); setnames(kdt, c("pidx", "ridx"))
kdt[, n := Kpool[cbind(pidx, ridx)]]
kdt[, share_within_pred := n / sum(n), by = pidx]
kdt[, pred := factor(MARG[pidx], levels = MARG)]
kdt[, real := factor(MARG[ridx], levels = rev(MARG))]
pX7 <- ggplot(kdt, aes(pred, real, fill = share_within_pred)) +
  geom_tile(color = "grey90") +
  scale_fill_gradient(low = "#F4F9F8", high = RB_COL, labels = percent, name = "Share within predicted") +
  labs(x = "Predicted shed cell (wall and age bin)", y = "Realized shed cell") +
  th_ae +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7))
save_fig(pX7, "AE_X7_Kernel_Heat", width = 7, height = 6.5)
cat(sprintf("  kernel diagonal share = %.4f\n", sum(diag(Kpool)) / sum(Kpool)))

cat("=== AE04 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
