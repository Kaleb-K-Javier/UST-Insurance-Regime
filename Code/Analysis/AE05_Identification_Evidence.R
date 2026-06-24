# ==============================================================================
# AE05_Identification_Evidence.R  -- TICKET 021, script 5 of 5
# ==============================================================================
# X3  the price schedule acts on (wall x age)  (joint cells)
# X11 premium variation identifies gamma_p     (identification)
# Rate-card block COPIED VERBATIM from 04an_Index_Separation.R (do NOT alter bodies).
# Consumes ae_frame indirectly via boyfy/boylong/facility_panel. READ-ONLY DESCRIPTIVE.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
need <- function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
need("fixest"); need("scales")
suppressPackageStartupMessages({ library(fixest); library(scales) })

# ---- logging ----
.log_path <- here::here("logs", paste0("AE05_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE05_Identification_Evidence.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# ============================ COMMON PUBLICATION BLOCK ========================
FIG_DIR <- here("Output", "Figures"); TAB_DIR <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
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
write_booktabs <- function(dt, path, align, header, panels = NULL, footer = NULL) {
  assert_ascii_clean(dt, paste("cells of", basename(path)))
  assert_ascii_clean(header, paste("header of", basename(path)))
  if (!is.null(panels)) assert_ascii_clean(unlist(panels), paste("panels of", basename(path)))
  if (!is.null(footer)) assert_ascii_clean(footer, paste("footer of", basename(path)))
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
  if (!is.null(footer)) {
    writeLines("\\midrule", con)
    for (ft in footer) writeLines(sprintf("\\multicolumn{%d}{l}{%s} \\\\", ncol_t, ft), con)
  }
  writeLines(c("\\bottomrule", "\\end{tabular}"), con)
  cat(sprintf("  saved %s\n", basename(path)))
}
fmt_n <- function(x) format(as.integer(x), big.mark = ",")
# =============================================================================

cat("=== AE05 IDENTIFICATION EVIDENCE ===\n")

# ==============================================================================
# 0. Rate-card factors -- COPIED VERBATIM from 04an / 04a (do NOT alter bodies)
# ==============================================================================
BASE_RATE <- 300
to_bool <- function(x) !is.na(x) & as.logical(x)
construction_factor <- function(is_steel_cathodic, is_reinforced_fiberglass,
                                is_double_walled_steel) {
  dw  <- to_bool(is_double_walled_steel); sc <- to_bool(is_steel_cathodic)
  frp <- to_bool(is_reinforced_fiberglass)
  fcase(dw, -0.20, sc | frp, 0.00, default = 0.00)
}
ilf_factor_2006 <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ); agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18, occ == 1e6 & agg == 2e6, 1.20, default = 1.00)
}
age_factor_2006 <- function(a) {
  fcase(is.na(a), NA_real_, a <= 5, -0.10, a <= 10, 0.00, a <= 15, 0.05,
        a <= 20, 0.10, a <= 25, 0.20, a > 25, 0.25)
}
ilf_factor_2014 <- function(occ, agg) {
  occ <- fifelse(is.na(occ), 1e6, occ); agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18, occ == 1e6 & agg == 2e6, 1.20, default = 1.18)
}
age_factor_2014 <- function(a) {
  fcase(is.na(a), NA_real_, a <= 5, -0.10, a <= 10, 0.00, a <= 15, 0.05,
        a <= 20, 0.10, a <= 25, 0.20, a <= 30, 0.25, a <= 35, 0.35, a <= 40, 0.45,
        a <= 45, 0.55, a <= 50, 0.65, a > 50, 0.75)
}
ilf_factor_2019 <- ilf_factor_2014
age_factor_2019 <- function(a) {
  fcase(is.na(a), NA_real_, a <= 2, -0.20, a <= 4, -0.16, a <= 6, -0.12, a <= 8, -0.08,
        a <= 10, -0.04, a <= 15, 0.00, a <= 20, 0.10, a <= 25, 0.20, a <= 30, 0.25,
        a <= 35, 0.35, a <= 40, 0.45, a <= 45, 0.55, a <= 50, 0.65, a > 50, 0.75)
}
deduct_factor_2019 <- function(ded) {
  fcase(ded <= 5e3 | is.na(ded), 1.00, ded <= 10e3, 0.92, ded <= 25e3, 0.80,
        ded <= 50e3, 0.60, ded > 50e3, 0.40)
}
coverage_factor_2019 <- function(form) {
  fcase(toupper(form) == "B", 1.50, toupper(form) == "C", 1.25, default = 1.00)
}
ilf_factor_2021 <- ilf_factor_2019; age_factor_2021 <- age_factor_2019
deduct_factor_2021 <- deduct_factor_2019; coverage_factor_2021 <- coverage_factor_2019
AGE_MIDPTS <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5)
era_of_year <- function(y) fcase(y <= 2013L, "era_2006", y <= 2018L, "era_2014",
                                 y <= 2020L, "era_2019", default = "era_2021")
ERA_LABEL <- c(era_2006 = "2006", era_2014 = "2014", era_2019 = "2019", era_2021 = "2021")
p_cell_one <- function(wall, bin, era) {
  ilf <- switch(era, era_2006 = ilf_factor_2006(1e6, 1e6), era_2014 = ilf_factor_2014(1e6, 1e6),
                era_2019 = ilf_factor_2019(1e6, 1e6), era_2021 = ilf_factor_2021(1e6, 1e6))
  age_load <- switch(era, era_2006 = age_factor_2006(AGE_MIDPTS[bin]),
                     era_2014 = age_factor_2014(AGE_MIDPTS[bin]),
                     era_2019 = age_factor_2019(AGE_MIDPTS[bin]),
                     era_2021 = age_factor_2021(AGE_MIDPTS[bin]))
  cons_load <- construction_factor(is_steel_cathodic = (wall == "SW"),
                                   is_reinforced_fiberglass = FALSE,
                                   is_double_walled_steel = (wall == "DW"))
  price <- BASE_RATE * ilf * (1 + age_load + cons_load)
  if (era %in% c("era_2019", "era_2021")) {
    cf <- switch(era, era_2019 = coverage_factor_2019("A"), era_2021 = coverage_factor_2021("A"))
    df <- switch(era, era_2019 = deduct_factor_2019(5e3), era_2021 = deduct_factor_2021(5e3))
    price <- price * cf * df
  }
  price
}
p_cell <- CJ(wall = c("SW", "DW"), age_bin = 1:8, era_tag = names(ERA_LABEL))
p_cell[, p_cell := mapply(p_cell_one, wall, age_bin, era_tag)]
p_cell[, era := ERA_LABEL[era_tag]]
out_pc <- p_cell[, .(wall, age_bin, era, p_cell = round(p_cell, 4))]

# =============================================================================
# X3. THE PRICE SCHEDULE ACTS ON (WALL x AGE)
# =============================================================================
cat("=== X3: rate card matrix + premium vs hazard ===\n")
rc06 <- dcast(out_pc[era == "2006"], wall ~ age_bin, value.var = "p_cell")
rc06 <- rc06[match(c("SW", "DW"), wall)]
fwrite(rc06, file.path(TAB_DIR, "AE_X3_RateCard.csv"))
cat("  saved AE_X3_RateCard.csv\n"); print(rc06)

rc06_tex <- copy(rc06)
for (cc in as.character(1:8)) rc06_tex[, (cc) := sprintf("%.0f", get(cc))]
setnames(rc06_tex, "wall", "Wall")
write_booktabs(rc06_tex, file.path(TAB_DIR, "AE_X3_RateCard.tex"),
               align = "lrrrrrrrr",
               header = c("Wall", as.character(1:8)),
               footer = "2014 and 2019 re-filings raise old-age loads; era table in qmd notes")

# ---- index construction (P_total_RB, H, P_total_FF, P_index) -- 04an Section 2 ----
prim <- readRDS(here("Output", "Estimation_Results", "DCM_Primitives_Replacement_observed.rds"))
h_vec <- prim$h_vec
stopifnot(length(h_vec) == 32, all(is.finite(h_vec)), all(h_vec > 0 & h_vec < 1))
h_aw <- (h_vec[1:16] + h_vec[17:32]) / 2
h_cell <- data.table(wall = rep(c("SW", "DW"), each = 8L), age_bin = rep(1:8, 2L), h = h_aw)

boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 1999L]
boylong[, era := ERA_LABEL[era_of_year(panel_year)]]
bl <- merge(boylong, out_pc[, .(wall, age_bin, era, p_cell)],
            by = c("wall", "age_bin", "era"), all.x = TRUE)
stopifnot(!anyNA(bl$p_cell))
bl <- merge(bl, h_cell, by = c("wall", "age_bin"), all.x = TRUE)
stopifnot(!anyNA(bl$h))
idx <- bl[, .(P_total_RB = sum(n_boy * p_cell), loghalf = sum(n_boy * log(1 - h))),
          by = .(panel_id, panel_year)]
idx[, H := 1 - exp(loghalf)][, loghalf := NULL]

boyfy <- fread(file.path(DATA_DIR, "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
boyfy <- boyfy[panel_year >= 1999L]
frame <- merge(boyfy[, .(panel_id, panel_year, state, N, Q, has_SW)], idx,
               by = c("panel_id", "panel_year"), all.x = TRUE)
fac <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "fr_premium_per_tank_yr"))
fac[, panel_id := as.character(panel_id)]
frame <- merge(frame, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
frame[is.na(fr_premium_per_tank_yr), fr_premium_per_tank_yr := 0]
frame[, P_total_FF := fr_premium_per_tank_yr * N]
frame[, regime_rb := as.integer(state == "TX" & panel_year >= 1999L)]
frame[, P_index := fifelse(regime_rb == 1L, P_total_RB, P_total_FF)]
frame <- frame[is.finite(P_total_RB) & is.finite(H)]
cat(sprintf("  index frame: %s | RB: %s | FF: %s\n", fmt_n(nrow(frame)),
            fmt_n(sum(frame$regime_rb == 1L)), fmt_n(sum(frame$regime_rb == 0L))))

# X3 figure: P_index vs H at composition level (restyled, AER)
ck <- boylong[, .(comp_key = paste(sprintf("%s%d:%d", wall, age_bin, n_boy), collapse = "|")),
              by = .(panel_id, panel_year)]
fr2 <- merge(frame, ck, by = c("panel_id", "panel_year"), all.x = TRUE)
agg <- fr2[!is.na(comp_key), .(H = mean(H), P_index = mean(P_index), nfy = .N),
           by = .(comp_key, regime = fifelse(regime_rb == 1L, "RB", "FF"))]
pX3 <- ggplot(agg, aes(H, P_index, size = nfy, color = regime)) +
  geom_point(alpha = 0.35) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL), name = NULL) +
  scale_size_continuous(range = c(0.4, 6), labels = comma, name = "Facility-years") +
  labs(x = "Facility hazard H(n)", y = "Premium index (total)") +
  th_ae
save_fig(pX3, "AE_X3_Premium_vs_Hazard", width = 7, height = 5)

# =============================================================================
# X11. PREMIUM VARIATION IDENTIFIES gamma_p
# =============================================================================
cat("=== X11: premium regressions + per-tank fit + correlations ===\n")
reg_summ <- function(m, lhs_vec, spec, n_use) {
  res <- residuals(m)
  data.table(spec = spec, r2 = round(r2(m, "r2"), 6), resid_sd = round(sd(res), 6),
             resid_sd_over_sdP = round(sd(res) / sd(lhs_vec), 6),
             coef_regime = NA_real_, se_regime = NA_real_, n = n_use)
}
dRB <- frame[regime_rb == 1L]
dFF <- frame[regime_rb == 0L]
dFFpos <- frame[regime_rb == 0L & fr_premium_per_tank_yr > 0]
# RB == TX == single state under >=1999 frame -> cluster degenerate; fit unclustered,
# coef/se_regime NA by construction (no regime term). [architect-ruled]
rRB  <- feols(P_total_RB ~ H + N + Q | panel_year, data = dRB)
rFF  <- feols(P_total_FF ~ H + N + Q | panel_year, data = dFF, cluster = ~state)
rFFp <- feols(P_total_FF ~ H + N + Q | panel_year, data = dFFpos, cluster = ~state)
rPool <- feols(P_index ~ regime_rb + H + N + Q | panel_year, data = frame, cluster = ~state)
tRB  <- reg_summ(rRB,  dRB$P_total_RB,    "RB",        nrow(dRB))
tFF  <- reg_summ(rFF,  dFF$P_total_FF,    "FF_all",    nrow(dFF))
tFFp <- reg_summ(rFFp, dFFpos$P_total_FF, "FF_feepos", nrow(dFFpos))
tPool <- reg_summ(rPool, frame$P_index,   "pooled",    nrow(frame))
ctp <- coeftable(rPool)
tPool[, coef_regime := round(ctp["regime_rb", 1], 6)]
tPool[, se_regime   := round(ctp["regime_rb", 2], 6)]
x11 <- rbindlist(list(tRB, tFF, tFFp, tPool))
fwrite(x11, file.path(TAB_DIR, "AE_X11_Premium_Regressions.csv"))
cat("  saved AE_X11_Premium_Regressions.csv\n"); print(x11)

# zero-fee state list + share (qmd Notes)
zf <- dFF[, .(fee = mean(fr_premium_per_tank_yr)), by = state][fee == 0]
zero_fee_states <- sort(zf$state)
share_zero_fee_fy <- dFF[fr_premium_per_tank_yr == 0, .N] / nrow(dFF)
cat(sprintf("  zero-fee FF states (n=%d): %s\n", length(zero_fee_states),
            paste(zero_fee_states, collapse = " ")))
cat(sprintf("  share of FF facility-years with zero fee: %.4f\n", share_zero_fee_fy))

x11_tex <- x11[, .(Spec = gsub("_", " ", spec), R2 = sprintf("%.3f", r2),
                   `Resid SD` = sprintf("%.1f", resid_sd),
                   `Resid over SD P` = sprintf("%.3f", resid_sd_over_sdP),
                   `Coef regime` = ifelse(is.na(coef_regime), "NA", sprintf("%.3f", coef_regime)),
                   `SE regime` = ifelse(is.na(se_regime), "NA", sprintf("%.3f", se_regime)),
                   N = fmt_n(n))]
write_booktabs(x11_tex, file.path(TAB_DIR, "AE_X11_Premium_Regressions.tex"),
               align = "lrrrrrr",
               header = c("Spec", "R2", "Resid SD", "Resid over SD P",
                          "Coef regime", "SE regime", "N"))

# per-tank rate-card fit (fixes the denominator channel)
mid <- fread(file.path(DATA_DIR, "tx_midcont_premium_all_1999_onwards.csv"),
             select = c("panel_id", "panel_year", "mean_tank_premium",
                        "n_tanks_rated", "source_era"))
mid[, panel_id := as.character(panel_id)]
mid <- mid[panel_year >= 2006L & is.finite(mean_tank_premium)]
rc <- merge(mid, frame[regime_rb == 1L, .(panel_id, panel_year, P_total_RB, N)],
            by = c("panel_id", "panel_year"))
rc[, simplified_per_tank := P_total_RB / N]
rc <- rc[is.finite(simplified_per_tank) & is.finite(mean_tank_premium)]
cat(sprintf("  matched TX 2006+ facility-years (per-tank fit): %s\n", fmt_n(nrow(rc))))
x11pt <- rc[, .(n = .N,
                pearson = round(cor(simplified_per_tank, mean_tank_premium), 6),
                r2 = round(cor(simplified_per_tank, mean_tank_premium)^2, 6),
                mean_actual = round(mean(mean_tank_premium), 4),
                mean_simplified = round(mean(simplified_per_tank), 4)),
            by = source_era][order(source_era)]
setnames(x11pt, "source_era", "era")
fwrite(x11pt, file.path(TAB_DIR, "AE_X11_RateCard_Fit_PerTank.csv"))
cat("  saved AE_X11_RateCard_Fit_PerTank.csv\n"); print(x11pt)

# index correlations pooled/FF/RB + Q-trim sensitivity
q_p999 <- quantile(frame$Q, 0.999, na.rm = TRUE)
qrow <- frame[which.max(Q)]
cat(sprintf("  Q max = %.4g at panel_id=%s year=%d (p99.9 cutoff=%.4g)\n",
            qrow$Q, qrow$panel_id, qrow$panel_year, q_p999))
pairs <- list(c("P_index", "H"), c("P_index", "N"), c("P_index", "Q"),
              c("H", "N"), c("H", "Q"), c("N", "Q"))
pair_lab <- c("P-H", "P-N", "P-Q", "H-N", "H-Q", "N-Q")
cor_rows <- list()
for (samp in c("pooled", "FF", "RB")) {
  d <- switch(samp, pooled = frame, FF = frame[regime_rb == 0L], RB = frame[regime_rb == 1L])
  dt_trim <- d[Q <= q_p999]
  for (j in seq_along(pairs)) {
    a <- pairs[[j]][1]; b <- pairs[[j]][2]
    cor_rows[[length(cor_rows) + 1L]] <- data.table(
      sample = samp, pair = pair_lab[j],
      pearson = round(cor(d[[a]], d[[b]], use = "complete.obs"), 6),
      pearson_qtrim = round(cor(dt_trim[[a]], dt_trim[[b]], use = "complete.obs"), 6),
      n = nrow(d))
  }
}
x11cor <- rbindlist(cor_rows)
fwrite(x11cor, file.path(TAB_DIR, "AE_X11_Correlations.csv"))
cat("  saved AE_X11_Correlations.csv\n"); print(x11cor)

cat("=== AE05 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
