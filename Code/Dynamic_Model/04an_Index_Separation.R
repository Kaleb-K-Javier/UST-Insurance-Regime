# ==============================================================================
# 04an_Index_Separation.R  -- TICKET 020, script 3 of 4  (E2)
# ==============================================================================
# E2 = index separation: is there enough independent variation in the
#   low-dimensional indices (P_index, H, N, Q) to identify the ~9 parameters,
#   and does the simplified 2-dim rate card track actual Mid-Continent premiums?
#
# Sample frame: panel_year >= 1999 (the estimation population). Under this frame
#   RB == TX (rho_state==2) and FF == control states (rho_state==1).
# READ-ONLY DESCRIPTIVE. No Output/Estimation_Results writes.
# ==============================================================================

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(scales)
})
if (!requireNamespace("fixest", quietly = TRUE)) install.packages("fixest", repos = "https://cloud.r-project.org")
suppressMessages(library(fixest))

cat("=== 04an INDEX SEPARATION (E2) ===\n")
cat(sprintf("Start: %s\n\n", format(Sys.time())))

FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
fig_dir <- here("Output", "Figures"); tab_dir <- here("Output", "Tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 019 .tex helper (same conventions as 04am)
# ------------------------------------------------------------------------------
esc_tex <- function(x) { x <- as.character(x); x <- gsub("\\\\", "", x)
  x <- gsub("%", "\\\\%", x); gsub("_", "\\\\_", x) }
write_tex_table <- function(dt, path, align, header, caption_note = NULL) {
  con <- file(path, open = "wt"); on.exit(close(con))
  writeLines(c("\\begin{table}[ht]", "\\centering", "\\small",
               "\\renewcommand{\\arraystretch}{1.15}",
               sprintf("\\begin{tabular}{%s}", align), "\\hline",
               paste0(paste(header, collapse = " & "), " \\\\"), "\\hline"), con)
  for (i in seq_len(nrow(dt)))
    writeLines(paste0(paste(esc_tex(unlist(dt[i])), collapse = " & "), " \\\\"), con)
  writeLines(c("\\hline", "\\end{tabular}"), con)
  if (!is.null(caption_note)) writeLines(sprintf("\\\\[2pt]{\\footnotesize %s}", caption_note), con)
  writeLines("\\end{table}", con)
}

# ==============================================================================
# 0. Rate-card factors -- COPIED VERBATIM from 04a_TX_Premium_All_1999_onwards.R
#    (lines 72, 175-270). Reviewer will diff: do NOT alter the bodies.
# ==============================================================================
BASE_RATE <- 300                                              # 04a:72

to_bool <- function(x) !is.na(x) & as.logical(x)              # 04a:175

construction_factor <- function(is_steel_cathodic, is_reinforced_fiberglass,
                                is_double_walled_steel) {     # 04a:186-192
  dw  <- to_bool(is_double_walled_steel)
  sc  <- to_bool(is_steel_cathodic)
  frp <- to_bool(is_reinforced_fiberglass)
  fcase(dw, -0.20, sc | frp, 0.00, default = 0.00)
}

ilf_factor_2006 <- function(occ, agg) {                       # 04a:210-216
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18,
        occ == 1e6 & agg == 2e6, 1.20,
        default = 1.00)
}
age_factor_2006 <- function(a) {                              # 04a:218-222
  fcase(is.na(a), NA_real_,
        a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
        a <= 20,  0.10, a <= 25,  0.20, a >  25,  0.25)
}
ilf_factor_2014 <- function(occ, agg) {                       # 04a:225-231
  occ <- fifelse(is.na(occ), 1e6, occ)
  agg <- fifelse(is.na(agg), 1e6, agg)
  fcase(occ == 1e6 & agg == 1e6, 1.18,
        occ == 1e6 & agg == 2e6, 1.20,
        default = 1.18)
}
age_factor_2014 <- function(a) {                              # 04a:233-239
  fcase(is.na(a), NA_real_,
        a <=  5, -0.10, a <= 10,  0.00, a <= 15,  0.05,
        a <= 20,  0.10, a <= 25,  0.20, a <= 30,  0.25,
        a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
        a <= 50,  0.65, a >  50,  0.75)
}
ilf_factor_2019 <- ilf_factor_2014                            # 04a:242
age_factor_2019 <- function(a) {                              # 04a:244-250
  fcase(is.na(a), NA_real_,
        a <=  2, -0.20, a <=  4, -0.16, a <=  6, -0.12, a <=  8, -0.08,
        a <= 10, -0.04, a <= 15,  0.00, a <= 20,  0.10, a <= 25,  0.20,
        a <= 30,  0.25, a <= 35,  0.35, a <= 40,  0.45, a <= 45,  0.55,
        a <= 50,  0.65, a >  50,  0.75)
}
deduct_factor_2019 <- function(ded) {                         # 04a:252-258
  fcase(ded <=  5e3 | is.na(ded), 1.00,
        ded <= 10e3,              0.92,
        ded <= 25e3,              0.80,
        ded <= 50e3,              0.60,
        ded >  50e3,              0.40)
}
coverage_factor_2019 <- function(form) {                      # 04a:260-264
  fcase(toupper(form) == "B", 1.50,
        toupper(form) == "C", 1.25,
        default = 1.00)
}
ilf_factor_2021      <- ilf_factor_2019                       # 04a:267-270
age_factor_2021      <- age_factor_2019
deduct_factor_2021   <- deduct_factor_2019
coverage_factor_2021 <- coverage_factor_2019

# 04b AGE_BREAKS midpoints (open top bin -> 37.5; price-flat above 25 anyway).
AGE_MIDPTS <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5)

# Modal-era year map (architect ruling; matches 04a ERA_BOUNDS source_era):
#   1999-2013 -> era_2006 ; 2014-2018 -> era_2014 ; 2019-2020 -> era_2019 ; 2021+ -> era_2021
era_of_year <- function(y) fcase(y <= 2013L, "era_2006", y <= 2018L, "era_2014",
                                 y <= 2020L, "era_2019", default = "era_2021")
ERA_LABEL <- c(era_2006 = "2006", era_2014 = "2014", era_2019 = "2019", era_2021 = "2021")

# ==============================================================================
# 1. Build the 16-cell x 4-era p_cell table
# ==============================================================================
cat("=== SECTION 1: p_cell table (16 cells x 4 eras) ===\n")
p_cell_one <- function(wall, bin, era) {
  ilf <- switch(era, era_2006 = ilf_factor_2006(1e6, 1e6),
                     era_2014 = ilf_factor_2014(1e6, 1e6),
                     era_2019 = ilf_factor_2019(1e6, 1e6),
                     era_2021 = ilf_factor_2021(1e6, 1e6))
  age_load <- switch(era, era_2006 = age_factor_2006(AGE_MIDPTS[bin]),
                          era_2014 = age_factor_2014(AGE_MIDPTS[bin]),
                          era_2019 = age_factor_2019(AGE_MIDPTS[bin]),
                          era_2021 = age_factor_2021(AGE_MIDPTS[bin]))
  cons_load <- construction_factor(is_steel_cathodic = (wall == "SW"),
                                   is_reinforced_fiberglass = FALSE,
                                   is_double_walled_steel = (wall == "DW"))
  price <- BASE_RATE * ilf * (1 + age_load + cons_load)
  # 2019/2021 also multiply by coverage-form and deductible factors; at spec
  # defaults (form "A", $5k deductible) both equal 1.00 -- applied explicitly
  # so the path stays diff-able against 04a's apply_engine (04a:316-324).
  if (era %in% c("era_2019", "era_2021")) {
    cf <- switch(era, era_2019 = coverage_factor_2019("A"), era_2021 = coverage_factor_2021("A"))
    df <- switch(era, era_2019 = deduct_factor_2019(5e3),   era_2021 = deduct_factor_2021(5e3))
    price <- price * cf * df
  }
  price
}
p_cell <- CJ(wall = c("SW", "DW"), age_bin = 1:8, era_tag = names(ERA_LABEL))
p_cell[, p_cell := mapply(p_cell_one, wall, age_bin, era_tag)]
p_cell[, era := ERA_LABEL[era_tag]]
out_pc <- p_cell[, .(wall, age_bin, era, p_cell = round(p_cell, 4))]
setorder(out_pc, wall, age_bin, era)
fwrite(out_pc, file.path(tab_dir, "T020_E2_Cell_Prices.csv"))
cat("  saved T020_E2_Cell_Prices.csv\n"); print(out_pc)

# ==============================================================================
# 2. h_cell (regime-averaged) and index construction per facility-year
# ==============================================================================
cat("=== SECTION 2: indices P_index, H, N, Q ===\n")
prim <- readRDS(here("Output", "Estimation_Results", "DCM_Primitives_Replacement_observed.rds"))
h_vec <- prim$h_vec
stopifnot(length(h_vec) == 32, all(is.finite(h_vec)), all(h_vec > 0 & h_vec < 1))
# index map: s_idx = (rho-1)*16 + (w-1)*8 + a ; w=1 SW, w=2 DW
h_ff <- h_vec[1:16]; h_rb <- h_vec[17:32]
cat(sprintf("  max|h_FF - h_RB| = %.3e (expected ~0; hazard is physical)\n",
            max(abs(h_ff - h_rb))))
h_aw <- (h_ff + h_rb) / 2                      # regime-averaged, length 16
h_cell <- data.table(wall = rep(c("SW", "DW"), each = 8L), age_bin = rep(1:8, 2L),
                     h = h_aw)

boylong <- fread(here("Data", "Analysis", "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 1999L]
boylong[, era := ERA_LABEL[era_of_year(panel_year)]]

# P_total_RB = sum n_cell * p_cell(era)
bl <- merge(boylong, out_pc[, .(wall, age_bin, era, p_cell)],
            by = c("wall", "age_bin", "era"), all.x = TRUE)
stopifnot(!anyNA(bl$p_cell))
# H = 1 - prod (1-h_cell)^n_cell
bl <- merge(bl, h_cell, by = c("wall", "age_bin"), all.x = TRUE)
stopifnot(!anyNA(bl$h))
idx <- bl[, .(P_total_RB = sum(n_boy * p_cell),
              loghalf    = sum(n_boy * log(1 - h))),
          by = .(panel_id, panel_year)]
idx[, H := 1 - exp(loghalf)][, loghalf := NULL]

boyfy <- fread(here("Data", "Analysis", "boy_composition_fy.csv"))
boyfy[, panel_id := as.character(panel_id)]
boyfy <- boyfy[panel_year >= 1999L]

frame <- merge(boyfy[, .(panel_id, panel_year, state, N, Q, has_SW)],
               idx, by = c("panel_id", "panel_year"), all.x = TRUE)

# FF premium: facility-level fr_premium_per_tank_yr (state-year rate from 02b)
fac <- fread(here("Data", "Analysis", "facility_panel.csv"),
             select = c("panel_id", "panel_year", "fr_premium_per_tank_yr"))
fac[, panel_id := as.character(panel_id)]
frame <- merge(frame, fac, by = c("panel_id", "panel_year"), all.x = TRUE)
n_fr_na <- frame[is.na(fr_premium_per_tank_yr), .N]
cat(sprintf("  fr_premium_per_tank_yr NA coerced to 0: %s rows\n", format(n_fr_na, big.mark = ",")))
frame[is.na(fr_premium_per_tank_yr), fr_premium_per_tank_yr := 0]
frame[, P_total_FF := fr_premium_per_tank_yr * N]

# regime: RB == TX & year>=1999 (collapses to TX under this frame)
frame[, regime_rb := as.integer(state == "TX" & panel_year >= 1999L)]
frame[, P_index := fifelse(regime_rb == 1L, P_total_RB, P_total_FF)]
frame <- frame[is.finite(P_total_RB) & is.finite(H)]
cat(sprintf("  frame facility-years: %s | RB(TX): %s | FF(control): %s\n",
            format(nrow(frame), big.mark = ","),
            format(sum(frame$regime_rb == 1L), big.mark = ","),
            format(sum(frame$regime_rb == 0L), big.mark = ",")))

# ==============================================================================
# 3. Index correlations (pooled / FF / RB)
# ==============================================================================
cat("=== SECTION 3: index correlations ===\n")
pairs <- list(c("P_index","H"), c("P_index","N"), c("P_index","Q"),
              c("H","N"), c("H","Q"), c("N","Q"))
pair_lab <- c("P-H","P-N","P-Q","H-N","H-Q","N-Q")
cor_rows <- list()
for (samp in c("pooled","FF","RB")) {
  d <- switch(samp, pooled = frame, FF = frame[regime_rb == 0L], RB = frame[regime_rb == 1L])
  for (j in seq_along(pairs)) {
    pe <- cor(d[[pairs[[j]][1]]], d[[pairs[[j]][2]]], use = "complete.obs")
    cor_rows[[length(cor_rows) + 1L]] <- data.table(sample = samp, pair = pair_lab[j],
                                                    pearson = round(pe, 6), n = nrow(d))
  }
}
e2_cor <- rbindlist(cor_rows)
fwrite(e2_cor, file.path(tab_dir, "T020_E2_Index_Correlations.csv"))
cat("  saved T020_E2_Index_Correlations.csv\n"); print(e2_cor)

# ==============================================================================
# 4. Premium regressions (fixest, cluster = ~state)
# ==============================================================================
cat("=== SECTION 4: premium regressions ===\n")
reg_summ <- function(m, lhs_vec, spec, sample) {
  res <- residuals(m)
  data.table(spec = spec, sample = sample, r2 = round(r2(m, "r2"), 6),
             resid_sd = round(sd(res), 6),
             resid_sd_over_sdP = round(sd(res) / sd(lhs_vec), 6),
             n = m$nobs)
}
dRB <- frame[regime_rb == 1L]; dFF <- frame[regime_rb == 0L]
# R1 is RB-only; under the >=1999 frame RB == TX == a SINGLE state, so
# cluster=~state is degenerate (one cluster). R1's reported columns
# (r2, resid_sd, n) need no vcov and its SE is NOT a deliverable (NA by spec),
# so R1 is fit unclustered. R2 (FF, many control states) and R3 (pooled) keep
# state-clustered SEs; only R3's se_regime is reported. [FLAG for architect.]
r1 <- feols(P_total_RB ~ H + N + Q | panel_year, data = dRB)
r2m<- feols(P_total_FF ~ H + N + Q | panel_year, data = dFF, cluster = ~state)
r3 <- feols(P_index ~ regime_rb + H + N + Q | panel_year, data = frame, cluster = ~state)

t1 <- reg_summ(r1, dRB$P_total_RB, "R1_RB", "RB"); t1[, `:=`(coef_regime = NA_real_, se_regime = NA_real_)]
t2 <- reg_summ(r2m, dFF$P_total_FF, "R2_FF", "FF"); t2[, `:=`(coef_regime = NA_real_, se_regime = NA_real_)]
t3 <- reg_summ(r3, frame$P_index, "R3_pooled", "pooled")
ct <- coeftable(r3)
t3[, coef_regime := round(ct["regime_rb", "Estimate"], 6)]
t3[, se_regime   := round(ct["regime_rb", "Std. Error"], 6)]
e2_reg <- rbindlist(list(t1, t2, t3), use.names = TRUE)
setcolorder(e2_reg, c("spec","sample","r2","resid_sd","resid_sd_over_sdP",
                      "coef_regime","se_regime","n"))
fwrite(e2_reg, file.path(tab_dir, "T020_E2_Premium_Regressions.csv"))
cat("  saved T020_E2_Premium_Regressions.csv\n"); print(e2_reg)
cat("  NOTE: R2_FF expected ~1 -- FF premium is mechanically fee x N.\n")

# starred .tex
e2_reg_tex <- e2_reg[, .(Spec = spec, `$R^2$` = sprintf("%.3f", r2),
                         `Resid SD` = sprintf("%.1f", resid_sd),
                         `Resid/SD(P)` = sprintf("%.3f", resid_sd_over_sdP),
                         N = format(n, big.mark = ","))]
write_tex_table(e2_reg_tex, file.path(tab_dir, "T020_E2_Premium_Regressions.tex"),
                align = "lrrrr",
                header = c("Spec", "$R^2$", "Resid SD", "Resid/SD(P)", "N"),
                caption_note = "Year FE; SE state-clustered. R2\\_FF is mechanically near 1 (fee $\\times$ N).")

# ==============================================================================
# 5. Rate-card fit vs actual Mid-Continent premium (TX 2006+)
# ==============================================================================
cat("=== SECTION 5: rate-card fit (TX 2006+) ===\n")
mid <- fread(here("Data", "Analysis", "tx_midcont_premium_all_1999_onwards.csv"),
             select = c("panel_id", "panel_year", "mean_tank_premium",
                        "n_tanks_rated", "source_era"))
mid[, panel_id := as.character(panel_id)]
mid[, actual := mean_tank_premium * n_tanks_rated]
mid <- mid[panel_year >= 2006L & is.finite(actual)]
rc <- merge(mid, frame[regime_rb == 1L, .(panel_id, panel_year, P_total_RB)],
            by = c("panel_id", "panel_year"))
cat(sprintf("  matched TX 2006+ facility-years: %s\n", format(nrow(rc), big.mark = ",")))
e2_rc <- rc[, .(n_fy = .N,
                pearson = round(cor(P_total_RB, actual), 6),
                r2 = round(cor(P_total_RB, actual)^2, 6),
                mean_actual = round(mean(actual), 4),
                mean_simplified = round(mean(P_total_RB), 4)),
            by = source_era][order(source_era)]
setnames(e2_rc, "source_era", "era")
fwrite(e2_rc, file.path(tab_dir, "T020_E2_RateCard_Fit.csv"))
cat("  saved T020_E2_RateCard_Fit.csv\n"); print(e2_rc)

# ==============================================================================
# 6. FIGURE: P_index vs H at composition level
# ==============================================================================
cat("=== SECTION 6: figure P_index vs H ===\n")
ck <- boylong[, .(comp_key = paste(sprintf("%s%d:%d", wall, age_bin, n_boy), collapse = "|")),
              by = .(panel_id, panel_year)]
fr2 <- merge(frame, ck, by = c("panel_id", "panel_year"), all.x = TRUE)
agg <- fr2[!is.na(comp_key), .(H = mean(H), P_index = mean(P_index), nfy = .N),
           by = .(comp_key, regime = fifelse(regime_rb == 1L, "RB", "FF"))]
p_e2 <- ggplot(agg, aes(H, P_index, size = nfy, color = regime)) +
  geom_point(alpha = 0.35) +
  scale_color_manual(values = c(FF = FF_COL, RB = RB_COL)) +
  scale_size_continuous(range = c(0.4, 6), labels = comma) +
  labs(x = "Facility hazard H(n)", y = "P_index (total premium)",
       color = "Regime", size = "Facility-years",
       title = "Two pricing worlds on the composition state") +
  theme_minimal(base_size = 13)
ggsave(file.path(fig_dir, "T020_E2_Premium_vs_Hazard.png"), p_e2,
       width = 7.5, height = 5.5, dpi = 150)
cat("  saved T020_E2_Premium_vs_Hazard.png\n")

cat("=== 04an DONE ===\n")
