# 04s_Portfolio_ForbiddenMoves_Figure.R
# Polish-direct (presentation): redraw the T013 "off-support" flows figure with a
# plain-English title and readable age-range labels for the talk appendix.
# Pure consumer of Output/Tables/T013_D1_TopOffSupportFlows.csv. No estimation.

library(data.table)
library(ggplot2)
library(here)

theme_set(theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.major.y = element_blank()))

d <- fread(here("Output", "Tables", "T013_D1_TopOffSupportFlows.csv"))

age_range <- c("1" = "0-5", "2" = "5-10", "3" = "10-15", "4" = "15-20",
               "5" = "20-25", "6" = "25-30", "7" = "30-35", "8" = "35+")

parse_state <- function(x) {
  parts <- tstrsplit(x, "/", fixed = TRUE)
  list(age = age_range[sub("^A", "", parts[[1]])], wall = parts[[2]])
}
from_p <- parse_state(d$from)
to_p   <- parse_state(d$to)
d[, label := paste0(from_p$age, " → ", to_p$age, "  yr  (", from_p$wall, ")")]

d <- d[order(-N)][1:min(12L, .N)]
d[, label := factor(label, levels = rev(label))]

p <- ggplot(d, aes(x = N, y = label)) +
  geom_col(fill = "#E76F51", alpha = 0.9) +
  labs(x = "Number of facility-years", y = NULL,
       title = "Moves the data shows but the model rules out",
       subtitle = "A facility's average tank age fell while it kept operating")

outfile <- here("Output", "Figures", "04s_Portfolio_ForbiddenMoves.png")
ggsave(outfile, p, width = 9, height = 5.2, dpi = 150)
cat(sprintf("saved %s\n", basename(outfile)))
