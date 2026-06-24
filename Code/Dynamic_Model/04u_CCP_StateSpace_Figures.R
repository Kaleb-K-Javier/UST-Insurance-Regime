# 04u_CCP_StateSpace_Figures.R
# Unified empirical-CCP figure across the state space, re-cuttable by any dimension.
# Design: x = age bin, y = empirical choice probability, FACET by action
#   (maintain / exit / replace) with free y-scales; color = the chosen cut;
#   point size = cell sample size.
# SOURCE: the canonical estimation panel dcm_obs_panel_observed.csv (BOY-stamped),
#   computing the model's three actions directly from y_it / I_replace so the
#   exit/replace split matches the structural fit exactly.
#   (Earlier this read T011_C1_CCPs_by_SizeBin_Cell.csv, which carried a stale DW
#    coding degeneracy -- every DW closure as replace, DW exit = 0. Fixed.)

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })

theme_set(theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), strip.text = element_text(face = "bold")))
age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")

dt <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
            select = c("A_bin","w_state","rho_state","size_bin","y_it","I_replace"))
dt[, wall     := ifelse(w_state == 1L, "SW", "DW")]
dt[, regime   := ifelse(rho_state == 2L, "RB", "FF")]
dt[, size_lab := factor(size_bin, levels = c("1","2","3","4+"),
                        labels = c("1 tank","2 tanks","3 tanks","4+ tanks"))]

make_ccp_fig <- function(data, cut_col, cut_label, palette = NULL, fname, title = NULL) {
  src <- data[!is.na(get(cut_col))]
  g <- src[, .(n  = .N,
               PM = mean(y_it == 0L),
               PE = mean(y_it == 1L & I_replace == 0L),
               PR = mean(y_it == 1L & I_replace == 1L)), by = c("A_bin", cut_col)]
  long <- melt(g, id.vars = c("A_bin", cut_col, "n"), measure.vars = c("PM","PE","PR"),
               variable.name = "action", value.name = "ccp")
  long[, action := factor(action, levels = c("PM","PE","PR"),
                          labels = c("Maintain","Exit","Replace"))]
  setnames(long, cut_col, "cut"); long[, cut := factor(cut)]
  p <- ggplot(long, aes(x = A_bin, y = ccp, color = cut)) +
    geom_line(aes(group = cut), linewidth = 0.7, alpha = 0.85) +
    geom_point(aes(size = n), alpha = 0.85) +
    facet_wrap(~ action, nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = 1:8, labels = age_labs) +
    scale_size_area(max_size = 7, guide = "none") +
    labs(x = "Tank age", y = "Empirical choice probability", color = cut_label,
         title = if (is.null(title)) paste0("Choice probabilities across the state space, by ", tolower(cut_label)) else title,
         subtitle = "Faceted by action (free y-scale); point size = cell sample size") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  if (!is.null(palette)) p <- p + scale_color_manual(values = palette)
  ggsave(here("Output","Figures", fname), p, width = 11, height = 4.2, dpi = 150)
  cat(sprintf("  saved %s\n", fname))
}

make_ccp_fig(dt, "wall",     "Wall type", c(SW = "#2A9D8F", DW = "#E76F51"), "04u_CCP_by_Wall.png")
make_ccp_fig(dt, "regime",   "Regime",    c(FF = "#E76F51", RB = "#2A9D8F"), "04u_CCP_by_Regime.png")
make_ccp_fig(dt, "size_lab", "Facility size", NULL, "04u_CCP_by_Size.png")
make_ccp_fig(dt[wall == "SW"], "regime", "Regime", c(FF = "#E76F51", RB = "#2A9D8F"),
             "04u_CCP_SW_byRegime.png", "Single-walled tanks: choice probabilities by regime")
make_ccp_fig(dt[wall == "DW"], "regime", "Regime", c(FF = "#E76F51", RB = "#2A9D8F"),
             "04u_CCP_DW_byRegime.png", "Double-walled tanks: choice probabilities by regime")

cat("=== 04u DONE (canonical dcm_obs coding) ===\n")
