# 04ah_Within_Facility_Homogeneity.R
# Second condition for the no-histogram size-scaled model: is a facility internally
# HOMOGENEOUS enough that ONE representative age+wall suffices (so we never need the
# count histogram)? Stickiness (04ag) says size does not drift; homogeneity says the
# single representative tank is a faithful summary. We measure, by facility size:
#   - wall homogeneity  : all tanks same wall (all_sw or all_dw), not mixed
#   - age homogeneity   : all active tanks fall in ONE 5-year age bin (the state's bin)
#   - both              : the share the single-representative-tank state represents exactly
# Read-only. Frame = estimation sample (dcm_obs panel ids) joined to facility_panel.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04ah WITHIN-FACILITY HOMOGENEITY ===\n")
OUT_T <- here("Output","Tables"); OUT_F <- here("Output","Figures")

dcm <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","boy_stock","rho_state"))
fac <- fread(here("Data","Analysis","facility_panel.csv"),
             select = c("panel_id","panel_year","max_tank_age","min_tank_age",
                        "all_sw","all_dw","mixed_wall","active_tanks"))
dcm <- merge(dcm, fac, by = c("panel_id","panel_year"), all.x = TRUE); rm(fac)
dcm <- dcm[boy_stock >= 1L & !is.na(max_tank_age) & !is.na(min_tank_age)]
cat(sprintf("  facility-years: %s | facilities: %s\n",
            format(nrow(dcm), big.mark=","), format(uniqueN(dcm$panel_id), big.mark=",")))

dcm[, size := factor(pmin(boy_stock, 4L), levels = 1:4, labels = c("1","2","3","4+"))]
dcm[, regime := factor(rho_state, levels = c(1L,2L), labels = c("FF","RB"))]

# homogeneity measures
dcm[, age_range := max_tank_age - min_tank_age]
bin_of <- function(a) pmin(as.integer(floor(a / 5)) + 1L, 8L)   # 5-yr bins, capped at bin 8
dcm[, age_homog  := bin_of(min_tank_age) == bin_of(max_tank_age)] # all tanks in ONE state bin
dcm[, wall_homog := (all_sw == 1L) | (all_dw == 1L)]             # not mixed-wall
dcm[, both_homog := age_homog & wall_homog]

# ---- by facility size ----
cat("\n--- homogeneity by facility size (facility-year weighted) ---\n")
bysize <- dcm[, .(n_fac_yrs       = .N,
                  pct_wall_homog  = round(100*mean(wall_homog), 1),
                  pct_age_homog   = round(100*mean(age_homog), 1),
                  pct_both_homog  = round(100*mean(both_homog), 1),
                  median_age_range= as.numeric(median(age_range)),
                  mean_tanks      = round(mean(active_tanks), 2)), by = size][order(size)]
print(bysize)
fwrite(bysize, file.path(OUT_T, "04ah_Homogeneity_bySize.csv"))

# multi-tank-only headline (single-tank are trivially homogeneous)
multi <- dcm[boy_stock >= 2L]
cat(sprintf("\n  OVERALL (all facility-years): both-homogeneous = %.1f%%\n", 100*mean(dcm$both_homog)))
cat(sprintf("  MULTI-TANK only (boy_stock>=2): both-homogeneous = %.1f%% (n=%s fac-yrs)\n",
            100*mean(multi$both_homog), format(nrow(multi), big.mark=",")))
cat(sprintf("  share of facility-years that are single-tank: %.1f%%\n", 100*mean(dcm$boy_stock == 1L)))

# ---- figure: homogeneity share by size, three measures ----
plt <- melt(bysize[, .(size, Wall = pct_wall_homog, `Age (one bin)` = pct_age_homog,
                       Both = pct_both_homog)],
            id.vars = "size", variable.name = "measure", value.name = "pct")
plt[, measure := factor(measure, levels = c("Wall","Age (one bin)","Both"))]
p <- ggplot(plt, aes(size, pct/100, fill = measure)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3.1, color = "grey25") +
  scale_fill_manual(values = c("Wall" = "#457B9D", "Age (one bin)" = "#E76F51", "Both" = "#1D3557")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.05)) +
  labs(x = "Facility size (BOY tank count)", y = "Share of facility-years homogeneous",
       fill = NULL,
       title = "Within-facility homogeneity falls with size",
       subtitle = "Share of facility-years whose tanks share one wall / one 5-yr age bin / both. Single-tank are trivially homogeneous.") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), panel.grid.minor = element_blank())
ggsave(file.path(OUT_F, "04ah_Homogeneity_bySize.png"), p, width = 8.5, height = 5, dpi = 150)
cat("  saved 04ah_Homogeneity_bySize.png\n")

# ---- compact .tex ----
tex <- c("\\begin{center}\\small\\renewcommand{\\arraystretch}{1.2}",
  "\\begin{tabular}{lrrrr}", "\\toprule",
  "\\textbf{Size} & \\textbf{Wall homog.} & \\textbf{Age one-bin} & \\textbf{Both} & \\textbf{Median age range} \\\\",
  "\\midrule",
  bysize[, sprintf("%s & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.0f yr \\\\",
                   size, pct_wall_homog, pct_age_homog, pct_both_homog, median_age_range)],
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  sprintf("\\vspace{0.1cm}{\\centering\\scriptsize Facility-year weighted. ``Both'' is the share the single representative-tank state represents exactly. Single-tank facilities (%.0f\\%% of facility-years) are homogeneous by construction.\\par}",
          100*mean(dcm$boy_stock == 1L)))
writeLines(tex, file.path(OUT_T, "04ah_Homogeneity_Summary.tex"))
cat("  saved 04ah_Homogeneity_Summary.tex\n=== 04ah DONE ===\n")
