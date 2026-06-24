# ==============================================================================
# AE08 -- researcher-directed corrective + completion pass (2026-06-12). Four jobs:
#  (1) HAZARD FIX: H = FACILITY-level ML hazard evaluated at the composition's
#      characteristics (avg-age bin x majority wall) -- the T014 standing decision.
#      The earlier 1-(1-h)^n union construction (AE05) invented per-tank hazards
#      the leak data does not contain. REGENERATES AE_X3_Premium_vs_Hazard and
#      AE_X11_{Premium_Regressions,Correlations} under the same filenames.
#  (2) AGE KERNEL: per-tank age-band transition matrix (L1 exhibit).
#  (3) MISS-COST: rule misses converted to premium dollars and hazard error.
#  (4) (k,m) MENU: removed x installed cross-tab (unified intensity action).
# READ-ONLY DESCRIPTIVE. panel_dt read declared (job 2 only; filter-first).
# ==============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here); library(scales) })
if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest", repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(library(fixest))

.log_path <- here::here("logs", paste0("AE08_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE08\n\n", .log_path))

FIG_DIR <- here("Output", "Figures"); TAB_DIR <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
th_ae <- theme_minimal(base_size = 11) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_blank(), legend.position = "bottom",
        panel.grid.minor = element_blank())
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

# ---- shared: rate card p_cell (verbatim era map from AE05) ----
BASE_RATE <- 300
to_bool <- function(x) !is.na(x) & as.logical(x)
construction_factor <- function(sc, frp, dw) fcase(to_bool(dw), -0.20, to_bool(sc) | to_bool(frp), 0.00, default = 0.00)
ilf_2006 <- function() 1.18; ilf_2014 <- function() 1.18
age_2006 <- function(a) fcase(a <= 5, -0.10, a <= 10, 0.00, a <= 15, 0.05, a <= 20, 0.10, a <= 25, 0.20, a > 25, 0.25)
age_2014 <- function(a) fcase(a <= 5, -0.10, a <= 10, 0.00, a <= 15, 0.05, a <= 20, 0.10, a <= 25, 0.20,
                              a <= 30, 0.25, a <= 35, 0.35, a <= 40, 0.45, a <= 45, 0.55, a <= 50, 0.65, a > 50, 0.75)
age_2019 <- function(a) fcase(a <= 2, -0.20, a <= 4, -0.16, a <= 6, -0.12, a <= 8, -0.08, a <= 10, -0.04,
                              a <= 15, 0.00, a <= 20, 0.10, a <= 25, 0.20, a <= 30, 0.25, a <= 35, 0.35,
                              a <= 40, 0.45, a <= 45, 0.55, a <= 50, 0.65, a > 50, 0.75)
AGE_MIDPTS <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5)
era_of_year <- function(y) fcase(y <= 2013L, "2006", y <= 2018L, "2014", y <= 2020L, "2019", default = "2021")
p_cell_fun <- function(wall, bin, era) {
  al <- switch(era, "2006" = age_2006(AGE_MIDPTS[bin]), "2014" = age_2014(AGE_MIDPTS[bin]),
               "2019" = age_2019(AGE_MIDPTS[bin]), "2021" = age_2019(AGE_MIDPTS[bin]))
  cl <- construction_factor(wall == "SW", FALSE, wall == "DW")
  300 * 1.18 * (1 + al + cl)   # 2019/2021 cf/df = 1.00 at defaults
}
p_cell <- CJ(wall = c("SW", "DW"), age_bin = 1:8, era = c("2006", "2014", "2019", "2021"))
p_cell[, p := mapply(p_cell_fun, wall, age_bin, era)]

# ---- shared: FACILITY-LEVEL hazard evaluation (the corrected construction) ----
AGE_BREAKS <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
bin_age <- function(a) { b <- as.integer(cut(a, AGE_BREAKS, labels = 1:8, right = FALSE, include.lowest = TRUE)); b[is.na(b)] <- 8L; b }
prim <- readRDS(here("Output", "Estimation_Results", "DCM_Primitives_Replacement_observed.rds"))
h_vec <- prim$h_vec
stopifnot(length(h_vec) == 32)
h_aw <- (h_vec[1:16] + h_vec[17:32]) / 2   # regime-averaged; (w-1)*8 + a, w=1 SW
H_eval <- function(avg_bin, maj_wall) {     # facility-level hazard at composition coords
  w <- fifelse(maj_wall == "SW", 1L, 2L)
  h_aw[(w - 1L) * 8L + avg_bin]
}

# ==============================================================================
# (1) HAZARD FIX: rebuild composition frame with corrected H; regenerate X3/X11
# ==============================================================================
cat("=== (1) hazard fix: H = facility ML hazard at composition coords ===\n")
boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 1999L]
boylong[, era := era_of_year(panel_year)]
bl <- merge(boylong, p_cell, by = c("wall", "age_bin", "era"), all.x = TRUE)
stopifnot(!anyNA(bl$p))
comp <- bl[, .(P_total_RB = sum(n_boy * p),
               N = sum(n_boy),
               avg_age = sum(n_boy * AGE_MIDPTS[age_bin]) / sum(n_boy),
               n_sw = sum(n_boy[wall == "SW"])),
           by = .(panel_id, panel_year, state)]
comp[, avg_bin := bin_age(avg_age)]
comp[, maj_wall := fifelse(n_sw >= N - n_sw, "SW", "DW")]   # ties -> SW (riskier)
comp[, H := H_eval(avg_bin, maj_wall)]
cat(sprintf("  composition frame: %s | H range %.4f-%.4f\n",
            fmt_n(nrow(comp)), min(comp$H), max(comp$H)))

boyfy <- fread(file.path(DATA_DIR, "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
fee <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "fr_premium_per_tank_yr"))
fee[, panel_id := as.character(panel_id)]
frame <- merge(comp, boyfy[, .(panel_id, panel_year, Q)], by = c("panel_id", "panel_year"), all.x = TRUE)
frame <- merge(frame, fee, by = c("panel_id", "panel_year"), all.x = TRUE)
frame[is.na(fr_premium_per_tank_yr), fr_premium_per_tank_yr := 0]
frame[, P_total_FF := fr_premium_per_tank_yr * N]
frame[, regime_rb := as.integer(state == "TX")]
frame[, P_index := fifelse(regime_rb == 1L, P_total_RB, P_total_FF)]
frame <- frame[is.finite(P_index) & is.finite(H) & !is.na(Q)]

# X3 scatter regenerated (same filename)
ck <- boylong[, .(comp_key = paste(sprintf("%s%d:%d", wall, age_bin, n_boy), collapse = "|")),
              by = .(panel_id, panel_year)]
fr2 <- merge(frame, ck, by = c("panel_id", "panel_year"), all.x = TRUE)
agg <- fr2[!is.na(comp_key), .(H = mean(H), P_index = mean(P_index), nfy = .N),
           by = .(comp_key, regime = fifelse(regime_rb == 1L, "RB", "FF"))]
pX3 <- ggplot(agg, aes(H, P_index, size = nfy, color = regime)) +
  geom_point(alpha = 0.35) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL), name = NULL) +
  scale_size_continuous(range = c(0.4, 6), labels = comma, name = "Facility-years") +
  labs(x = "Facility hazard (ML model at composition)", y = "Premium index (total)") +
  th_ae
ggsave(file.path(FIG_DIR, "AE_X3_Premium_vs_Hazard.png"), pX3, width = 7, height = 5, dpi = 300)
ggsave(file.path(FIG_DIR, "AE_X3_Premium_vs_Hazard.pdf"), pX3, width = 7, height = 5)
cat("  regenerated AE_X3_Premium_vs_Hazard (corrected H)\n")

# X11 regressions + correlations regenerated (same filenames)
reg_summ <- function(m, lhs, spec, n_use) {
  res <- residuals(m)
  data.table(spec = spec, r2 = round(r2(m, "r2"), 6), resid_sd = round(sd(res), 6),
             resid_sd_over_sdP = round(sd(res) / sd(lhs), 6),
             coef_regime = NA_real_, se_regime = NA_real_, n = n_use)
}
dRB <- frame[regime_rb == 1L]; dFF <- frame[regime_rb == 0L]
dFFp <- dFF[fr_premium_per_tank_yr > 0]
rRB  <- feols(P_total_RB ~ H + N + Q | panel_year, data = dRB)
rFF  <- feols(P_total_FF ~ H + N + Q | panel_year, data = dFF, cluster = ~state)
rFFp <- feols(P_total_FF ~ H + N + Q | panel_year, data = dFFp, cluster = ~state)
rPool <- feols(P_index ~ regime_rb + H + N + Q | panel_year, data = frame, cluster = ~state)
x11 <- rbindlist(list(reg_summ(rRB, dRB$P_total_RB, "RB", nrow(dRB)),
                      reg_summ(rFF, dFF$P_total_FF, "FF_all", nrow(dFF)),
                      reg_summ(rFFp, dFFp$P_total_FF, "FF_feepos", nrow(dFFp)),
                      reg_summ(rPool, frame$P_index, "pooled", nrow(frame))))
ctp <- coeftable(rPool)
x11[spec == "pooled", `:=`(coef_regime = round(ctp["regime_rb", 1], 6),
                           se_regime = round(ctp["regime_rb", 2], 6))]
fwrite(x11, file.path(TAB_DIR, "AE_X11_Premium_Regressions.csv"))
cat("  regenerated AE_X11_Premium_Regressions.csv (corrected H)\n"); print(x11)
x11_tex <- x11[, .(Spec = gsub("_", " ", spec), R2 = sprintf("%.3f", r2),
                   `Resid SD` = sprintf("%.1f", resid_sd),
                   `Resid over SD P` = sprintf("%.3f", resid_sd_over_sdP),
                   `Coef regime` = ifelse(is.na(coef_regime), "NA", sprintf("%.1f", coef_regime)),
                   `SE regime` = ifelse(is.na(se_regime), "NA", sprintf("%.1f", se_regime)),
                   N = fmt_n(n))]
write_booktabs(x11_tex, file.path(TAB_DIR, "AE_X11_Premium_Regressions.tex"),
               align = "lrrrrrr",
               header = c("Spec", "R2", "Resid SD", "Resid over SD P", "Coef regime", "SE regime", "N"))

q999 <- quantile(frame$Q, 0.999, na.rm = TRUE)
pairs <- list(c("P_index","H"), c("P_index","N"), c("P_index","Q"), c("H","N"), c("H","Q"), c("N","Q"))
plab <- c("P-H","P-N","P-Q","H-N","H-Q","N-Q")
cor_rows <- list()
for (samp in c("pooled","FF","RB")) {
  d <- switch(samp, pooled = frame, FF = dFF, RB = dRB)
  dtm <- d[Q <= q999]
  for (j in seq_along(pairs)) {
    a <- pairs[[j]][1]; b <- pairs[[j]][2]
    cor_rows[[length(cor_rows)+1L]] <- data.table(sample = samp, pair = plab[j],
      pearson = round(cor(d[[a]], d[[b]], use = "complete.obs"), 6),
      pearson_qtrim = round(cor(dtm[[a]], dtm[[b]], use = "complete.obs"), 6), n = nrow(d))
  }
}
fwrite(rbindlist(cor_rows), file.path(TAB_DIR, "AE_X11_Correlations.csv"))
cat("  regenerated AE_X11_Correlations.csv (corrected H)\n")

# ==============================================================================
# (2) AGE KERNEL: per-tank age-band transition (surviving tanks; deterministic aging)
# ==============================================================================
cat("=== (2) per-tank age-band kernel ===\n")
pdt <- fread(file.path(DATA_DIR, "panel_dt.csv"),
             select = c("panel_year", "tank_age", "closure_event"))
pdt <- pdt[panel_year >= 1999L & !is.na(tank_age) & closure_event == 0L]
pdt[, bin_now := bin_age(tank_age)]
pdt[, bin_next := bin_age(tank_age + 1)]   # aging is calendar time: next age known
ak <- pdt[, .(n = .N), by = .(bin_now, bin_next)]
ak[, prob := round(n / sum(n), 6), by = bin_now]
setorder(ak, bin_now, bin_next)
fwrite(ak, file.path(TAB_DIR, "AE_X4_Age_Transition.csv"))
akw <- dcast(ak, bin_now ~ bin_next, value.var = "prob", fill = 0)
cat("  saved AE_X4_Age_Transition.csv | stay probs:\n")
print(ak[bin_now == bin_next, .(bin_now, stay = prob)])
ak_tex <- ak[bin_now == bin_next | bin_next == bin_now + 1L]
ak_tex_w <- dcast(ak_tex, bin_now ~ fifelse(bin_next == bin_now, "Stay", "Advance"),
                  value.var = "prob", fill = 0)
ak_out <- ak_tex_w[, .(`Age band` = as.character(bin_now), Stay = sprintf("%.3f", Stay),
                       Advance = sprintf("%.3f", fifelse(is.na(Advance), 0, Advance)))]
write_booktabs(ak_out, file.path(TAB_DIR, "AE_X4_Age_Transition.tex"),
               align = "lrr", header = c("Age band", "Stay", "Advance"))
rm(pdt); invisible(gc())

# ==============================================================================
# (3) MISS-COST: premium-dollar and hazard error of removal-rule misses
# ==============================================================================
cat("=== (3) miss-cost of rule misses ===\n")
fr <- fread(file.path(DATA_DIR, "ae_frame.csv"))
fr[, panel_id := as.character(panel_id)]
e7pop <- fr[action %in% c("Downsize", "Replace") & n_shed_total >= 1L]
bl_e <- boylong[e7pop[, .(panel_id, panel_year)], on = c("panel_id", "panel_year"), nomatch = NULL]
bl_e[, cell := sprintf("%s_%d", wall, age_bin)]
MARG <- c(sprintf("SW_%d", 8:1), sprintf("DW_%d", 8:1))
marg_wall <- c(rep("SW", 8), rep("DW", 8)); marg_bin <- c(8:1, 8:1)
wB <- dcast(bl_e, panel_id + panel_year ~ cell, value.var = "n_boy", fill = 0)
wR <- dcast(bl_e, panel_id + panel_year ~ cell, value.var = "n_shed", fill = 0)
for (mc in setdiff(MARG, names(wB))) wB[, (mc) := 0L]
for (mc in setdiff(MARG, names(wR))) wR[, (mc) := 0L]
e8 <- merge(e7pop[, .(panel_id, panel_year, action, size_bin, n_shed_total)],
            wB[, c("panel_id", "panel_year", MARG), with = FALSE],
            by = c("panel_id", "panel_year"))
R <- as.matrix(merge(e8[, .(panel_id, panel_year)],
                     wR[, c("panel_id", "panel_year", MARG), with = FALSE],
                     by = c("panel_id", "panel_year"))[, ..MARG])
B <- as.matrix(e8[, ..MARG]); storage.mode(B) <- "double"; storage.mode(R) <- "double"
k <- rowSums(R)
CB <- t(apply(B, 1L, cumsum)); P <- pmin(pmax(k - (CB - B), 0), B)
e8[, miss := as.integer(rowSums(abs(P - R)) > 0)]
# premium error: era-matched card prices (UPPER bound -- the empirical card is flatter)
e8[, era := era_of_year(panel_year)]
pmatL <- as.matrix(dcast(p_cell, era ~ paste0(wall, "_", age_bin), value.var = "p")[
  match(e8$era, era), MARG, with = FALSE])
dP <- rowSums((R - P) * pmatL)            # realized-minus-predicted premium of REMOVED set
e8[, dP_abs := abs(dP)]
# hazard error: corrected facility-level evaluation on the two post-removal portfolios
post_stats <- function(Bm, Sm) {          # Sm = removed; returns avg bin + maj wall + N
  K <- Bm - Sm
  Nk <- rowSums(K)
  avg <- as.vector(K %*% AGE_MIDPTS[marg_bin]) / pmax(Nk, 1)
  nsw <- rowSums(K[, marg_wall == "SW", drop = FALSE])
  list(bin = bin_age(avg), wall = fifelse(nsw >= Nk - nsw, "SW", "DW"), N = Nk)
}
pp <- post_stats(B, P); pr <- post_stats(B, R)
ok <- pp$N > 0 & pr$N > 0                  # exits-by-shedding-everything drop
e8[, dH_abs := NA_real_]
e8[ok, dH_abs := abs(H_eval(pp$bin[ok], pp$wall[ok]) - H_eval(pr$bin[ok], pr$wall[ok]))]
mc <- e8[miss == 1L & !is.na(size_bin), .(
  n_misses = .N,
  share_dP_zero = round(mean(dP_abs < 1e-9), 4),
  mean_dP = round(mean(dP_abs), 2), p90_dP = round(quantile(dP_abs, .9), 2),
  mean_dH_pp = round(100 * mean(dH_abs, na.rm = TRUE), 4),
  p90_dH_pp = round(100 * quantile(dH_abs, .9, na.rm = TRUE), 4)),
  by = .(action, size_bin)][order(action, size_bin)]
fwrite(mc, file.path(TAB_DIR, "AE_X7_Miss_Cost.csv"))
cat("  saved AE_X7_Miss_Cost.csv\n"); print(mc)
cat(sprintf("  POOLED among misses: mean |dP| = $%.2f/yr | p90 = $%.2f | mean |dH| = %.4f pp\n",
            e8[miss == 1L, mean(dP_abs)], e8[miss == 1L, quantile(dP_abs, .9)],
            100 * e8[miss == 1L, mean(dH_abs, na.rm = TRUE)]))
mc_tex <- mc[, .(Action = action, Size = size_bin, Misses = fmt_n(n_misses),
                 `Mean premium err` = sprintf("%.0f", mean_dP),
                 `p90 premium err` = sprintf("%.0f", p90_dP),
                 `Mean hazard err pp` = sprintf("%.3f", mean_dH_pp))]
write_booktabs(mc_tex, file.path(TAB_DIR, "AE_X7_Miss_Cost.tex"), align = "llrrrr",
               header = c("Action", "Size", "Misses", "Mean premium err",
                          "p90 premium err", "Mean hazard err pp"))

# ==============================================================================
# (4) (k,m) MENU: removed x installed cross-tab
# ==============================================================================
cat("=== (4) (k,m) menu ===\n")
km <- fr[action %in% c("Downsize", "Replace"),
         .(n = .N), by = .(k = pmin(n_shed_total, 4L), m = pmin(n_inst, 4L))]
km[, share := round(n / sum(n), 6)]
setorder(km, k, m)
fwrite(km, file.path(TAB_DIR, "AE_X7_KM_Menu.csv"))
kmw <- dcast(km, k ~ m, value.var = "n", fill = 0)
setnames(kmw, as.character(0:4), c("m0", "m1", "m2", "m3", "m4"), skip_absent = TRUE)
km_out <- kmw[, .(`Tanks removed` = fcase(k == 4L, "4 plus", default = as.character(k)),
                  `Install 0` = fmt_n(m0), `Install 1` = fmt_n(m1), `Install 2` = fmt_n(m2),
                  `Install 3` = fmt_n(m3), `Install 4 plus` = fmt_n(m4))]
write_booktabs(km_out, file.path(TAB_DIR, "AE_X7_KM_Menu.tex"), align = "lrrrrr",
               header = c("Tanks removed", "Install 0", "Install 1", "Install 2",
                          "Install 3", "Install 4 plus"))
cat("  saved AE_X7_KM_Menu.csv/.tex\n")
print(dcast(km, k ~ m, value.var = "n", fill = 0))

cat("=== AE08 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
