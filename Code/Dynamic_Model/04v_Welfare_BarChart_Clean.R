# 04v_Welfare_BarChart_Clean.R
# Jargon-free welfare bar chart for the talk, rebuilt from the saved 04r summary
# (no re-solving). Reads Output/Tables/04r_CF234_Welfare_Summary.csv.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })

d <- fread(here("Output","Tables","04r_CF234_Welfare_Summary.csv"))
d <- d[E_label == "HEALTH_PLUS_UNMEASURED" & scenario != "baseline"]   # headline E = $50k, CFs only

d[, comp := factor(fcase(
  component == "ProducerSurplus_USD", "Firm surplus",
  component == "ExternalDamage_USD",  "External damage",
  component == "GovtOutlay_USD",      "Govt outlay",
  component == "SocialWelfare_USD",   "Social welfare"),
  levels = c("Firm surplus","External damage","Govt outlay","Social welfare"))]
d[, cf := factor(fcase(
  scenario == "CF1_flatfee", "CF1\nFlat-fee",
  scenario == "CF2_subsidy", "CF2\nSubsidy",
  scenario == "CF3_pigou",   "CF3\nPigouvian",
  scenario == "CF4_mandate", "CF4\nMandate"),
  levels = c("CF1\nFlat-fee","CF2\nSubsidy","CF3\nPigouvian","CF4\nMandate"))]

pal <- c("Firm surplus" = "#003262", "External damage" = "#3B7A57",
         "Govt outlay" = "#FDB515", "Social welfare" = "#8B1A1A")

p <- ggplot(d, aes(x = cf, y = delta_USD, fill = comp)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.6) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Counterfactual welfare: change vs. the risk-based baseline",
       subtitle = "$ per facility, present value (E = $50k per expected release)",
       x = NULL, y = expression(Delta~"vs. baseline"), fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

ggsave(here("Output","Figures","04v_Welfare_BarChart_Clean.png"), p, width = 10, height = 5.6, dpi = 150)
ggsave(here("Output","Figures","04v_Welfare_BarChart_Clean.pdf"), p, width = 10, height = 5.6)
cat("  saved 04v_Welfare_BarChart_Clean.{png,pdf}\n=== 04v DONE ===\n")
