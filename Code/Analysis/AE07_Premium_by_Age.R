# ==============================================================================
# AE07_Premium_by_Age.R -- researcher-directed (2026-06-12): the simple
# identifying-variation picture. x = age bin, y = mean ACTUAL TOTAL facility
# premium, by regime, faceted by facility size. Single-cell facility-years so
# the age coordinate is unambiguous. READ-ONLY DESCRIPTIVE.
# ==============================================================================
suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here); library(scales) })

.log_path <- here::here("logs", paste0("AE07_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE07\n\n", .log_path))

FIG_DIR <- here("Output", "Figures"); TAB_DIR <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
th_ae <- theme_minimal(base_size = 11) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_blank(), legend.position = "bottom",
        panel.grid.minor = element_blank())

cat("=== AE07 PREMIUM BY AGE (facility totals) ===\n")
boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 1999L]
occ <- boylong[, .(n_occ = .N, wall = wall[1L], age_bin = age_bin[1L], N = sum(n_boy)),
               by = .(panel_id, panel_year, state)]
single <- occ[n_occ == 1L]
cat(sprintf("  single-cell facility-years (all states): %s\n",
            format(nrow(single), big.mark = ",")))

# actual totals: TX = engine mean x rated count; controls = state fee x N
mid <- fread(file.path(DATA_DIR, "tx_midcont_premium_all_1999_onwards.csv"),
             select = c("panel_id", "panel_year", "mean_tank_premium", "n_tanks_rated"))
mid[, panel_id := as.character(panel_id)]
mid <- mid[is.finite(mean_tank_premium) & n_tanks_rated > 0]
mid[, total_actual := mean_tank_premium * n_tanks_rated]
fee <- fread(file.path(DATA_DIR, "facility_panel.csv"),
             select = c("panel_id", "panel_year", "fr_premium_per_tank_yr"))
fee[, panel_id := as.character(panel_id)]

d <- merge(single, mid[, .(panel_id, panel_year, total_actual)],
           by = c("panel_id", "panel_year"), all.x = TRUE)
d <- merge(d, fee, by = c("panel_id", "panel_year"), all.x = TRUE)
d[is.na(fr_premium_per_tank_yr), fr_premium_per_tank_yr := 0]
d[, regime := fifelse(state == "TX", "Risk-based (TX)", "Flat fee (controls)")]
d[, total_premium := fifelse(state == "TX", total_actual,
                             fr_premium_per_tank_yr * N)]
d <- d[!is.na(total_premium)]                 # TX rows without engine match drop
d <- d[!(state == "TX" & panel_year < 2006L)] # TX premium data starts 2006
d[, size_bin := fcase(N == 1L, "1 tank", N == 2L, "2 tanks", N == 3L, "3 tanks",
                      N >= 4L, "4 plus tanks", default = NA_character_)]
d[, size_bin := factor(size_bin, levels = c("1 tank", "2 tanks", "3 tanks", "4 plus tanks"))]
cat(sprintf("  plotted facility-years: %s (TX %s | control %s)\n",
            format(nrow(d), big.mark = ","),
            format(d[state == "TX", .N], big.mark = ","),
            format(d[state != "TX", .N], big.mark = ",")))

agg <- d[, .(mean_total = mean(total_premium), n = .N),
         by = .(age_bin, regime, wall, size_bin)]
agg <- agg[n >= 30]   # suppress cells with under 30 facility-years (display only)
fwrite(agg[order(regime, wall, size_bin, age_bin)],
       file.path(TAB_DIR, "AE_X3_Premium_by_Age.csv"))
cat("  saved AE_X3_Premium_by_Age.csv\n")

p <- ggplot(agg, aes(age_bin, mean_total, color = regime, linetype = wall)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.3) +
  facet_wrap(~size_bin, nrow = 1) +
  scale_color_manual(values = c("Risk-based (TX)" = RB_COL, "Flat fee (controls)" = FF_COL),
                     name = NULL) +
  scale_linetype_manual(values = c(SW = "solid", DW = "dashed"), name = NULL) +
  scale_x_continuous(breaks = c(2, 4, 6, 8)) +
  scale_y_continuous(labels = dollar) +
  labs(x = "Tank age bin (5-year bins)", y = "Mean total facility premium per year") +
  th_ae
ggsave(file.path(FIG_DIR, "AE_X3_Premium_by_Age.png"), p, width = 8.5, height = 3.6, dpi = 300)
ggsave(file.path(FIG_DIR, "AE_X3_Premium_by_Age.pdf"), p, width = 8.5, height = 3.6)
cat("  saved AE_X3_Premium_by_Age .png/.pdf\n")

cat("  TX mean total by age bin (pooled sizes, SW):\n")
print(d[state == "TX" & wall == "SW", .(mean_total = round(mean(total_premium), 1), n = .N),
        by = age_bin][order(age_bin)])
cat("=== AE07 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
