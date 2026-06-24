# 04w_Recoded_CCP_Decomposition.R
# Shows how the single-tank model's MAINTAIN and REPLACE actions decompose once
# facility-level portfolio events are correctly coded. Same faceted CCP design as 04u.
#   REPLACE  : [model's current replace] vs [true replace = upgrade w/ install]
#              vs [shrink = tank removed, no install]
#   MAINTAIN : [model's current maintain] vs [true maintain = no closure]
#              vs [partial closure hidden inside maintain]
# Source: BOY estimation panel (dcm_obs_panel_observed.csv) merged with the
#   facility panel flags (facility_panel.csv) -- exactly the T011 audit merge.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })

theme_set(theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)))
age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")

obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","A_bin","w_state","y_it","I_replace","size_bin"))
fp  <- fread(here("Data","Analysis","facility_panel.csv"),
             select = c("panel_id","panel_year","any_closure","facility_complete_closure",
                        "replacement_closure_year","permanent_closure_year"))
m <- merge(obs, fp, by = c("panel_id","panel_year"), all.x = FALSE)
for (c0 in c("any_closure","facility_complete_closure","replacement_closure_year","permanent_closure_year"))
  m[is.na(get(c0)), (c0) := 0L]
m <- m[size_bin %in% c("1","2","3","4+")]
m[, size_lab := factor(size_bin, levels = c("1","2","3","4+"),
                       labels = c("1 tank","2 tanks","3 tanks","4+ tanks"))]
cat(sprintf("merged estimation sample: %d rows\n", nrow(m)))

# ---- action-space frequency (how rare is any movement?) ----
N <- nrow(m)
fr <- data.table(
  category = c("Maintain (model: no close)", "Exit (model: close, no install)",
               "Replace (model: close + install)",
               "True upgrade (closure + new install)", "Shrink (tank removed, no install)",
               "Any closure activity (>=1 tank closed)"),
  n = c(m[y_it == 0L, .N], m[y_it == 1L & I_replace == 0L, .N], m[y_it == 1L & I_replace == 1L, .N],
        m[replacement_closure_year == 1L, .N], m[permanent_closure_year == 1L, .N],
        m[any_closure == 1L, .N]))
fr[, pct := round(100 * n / N, 3)]
fwrite(fr, here("Output","Tables","04w_ActionFrequency.csv"))
cat(sprintf("N=%d\n", N)); print(fr)

size_pal <- c("1 tank"="#E76F51","2 tanks"="#8AB17D","3 tanks"="#2A9D8F","4+ tanks"="#9B5DE5")

plot_decomp <- function(agg, defn_levels, defn_labels, title, fname) {
  long <- melt(agg, id.vars = c("A_bin","size_lab","n"),
               measure.vars = defn_levels, variable.name = "defn", value.name = "p")
  long[, defn := factor(defn, levels = defn_levels, labels = defn_labels)]
  p <- ggplot(long, aes(x = A_bin, y = p, color = size_lab)) +
    geom_line(aes(group = size_lab), linewidth = 0.7, alpha = 0.85) +
    geom_point(aes(size = n), alpha = 0.85) +
    facet_wrap(~ defn, nrow = 1, scales = "free_y") +
    scale_x_continuous(breaks = 1:8, labels = age_labs) +
    scale_color_manual(values = size_pal) +
    scale_size_area(max_size = 6, guide = "none") +
    labs(x = "Tank age", y = "Empirical probability", color = "Facility size",
         title = title,
         subtitle = "Empirical probability by facility size; faceted (free y-scale); point size = cell sample size")
  ggsave(here("Output","Figures", fname), p, width = 11.5, height = 4.3, dpi = 150)
  cat(sprintf("  saved %s\n", fname))
}

# ---- REPLACE decomposition ----
agg_r <- m[, .(
  n = .N,
  replace_upg = mean(y_it == 1L & I_replace == 1L),
  full_exit   = mean(y_it == 1L & I_replace == 0L),
  downsizing  = mean(any_closure == 1L & facility_complete_closure == 0L & replacement_closure_year == 0L)
), by = .(A_bin, size_lab)]
plot_decomp(agg_r,
  c("replace_upg","full_exit","downsizing"),
  c("Upgrade  ->  model Replace","Full exit  ->  model Exit","Downsizing (kept operating)  ->  hidden in Maintain"),
  "The three closure events by facility size, and where the model puts them",
  "04w_Replace_Decomp.png")

# ---- MAINTAIN decomposition ----
agg_m <- m[, .(
  n = .N,
  model_maintain = mean(y_it == 0L),
  true_maintain  = mean(y_it == 0L & any_closure == 0L),
  partial_hidden = mean(y_it == 0L & any_closure == 1L)
), by = .(A_bin, size_lab)]
plot_decomp(agg_m,
  c("model_maintain","true_maintain","partial_hidden"),
  c("Model: current Maintain action","True maintain (no closure at all)","Partial closure hidden in Maintain"),
  "What the model calls \"maintain\": true maintain vs. hidden partial closures",
  "04w_Maintain_Decomp.png")

cat("=== 04w DONE ===\n")
