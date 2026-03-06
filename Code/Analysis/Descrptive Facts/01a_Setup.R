#==============================================================================
# 01a_Setup.R
# Texas UST Insurance Reform — Shared Setup, Constants & Helpers
#
# SOURCE THIS FILE FIRST in every 01x module:
#   source(here::here("Code", "01a_Setup.R"))
#
# Defines: packages, paths, study params, color palette, theme,
#          canonical age bins, and all shared helper functions.
# Nothing here reads data or writes output.
#==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(ggplot2)
  library(patchwork)
  library(gridExtra)
  library(grid)
  library(broom)
  library(scales)
  library(stringr)
  library(kableExtra)
  library(here)
})

if (requireNamespace("pROC",    quietly = TRUE)) library(pROC)
if (requireNamespace("cmprsk",  quietly = TRUE)) library(cmprsk)
if (requireNamespace("ggrepel", quietly = TRUE)) library(ggrepel)

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

# ── Directories ───────────────────────────────────────────────────────────────
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data", "Analysis")   # final RDS → 02_DiD reads from here
INTERIM_DIR    <- here("Data", "Interim")    # intermediate RDS between 01x scripts

for (d in c(OUTPUT_TABLES, OUTPUT_FIGURES, ANALYSIS_DIR, INTERIM_DIR))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Run flags ─────────────────────────────────────────────────────────────────
RUN_FULL      <- TRUE    # enables k-fold CV in 01j; set FALSE for quick runs
USE_BOOTSTRAP <- FALSE   # Webb-6 wild cluster bootstrap; TRUE for final submission
N_BOOTSTRAP   <- 9999

# ── Study parameters ──────────────────────────────────────────────────────────
TREATMENT_DATE       <- as.IDate("1998-12-22")
TREATMENT_YEAR       <- 1998L
POST_YEAR            <- 1999L
PANEL_START          <- 1985L
PANEL_END            <- 2020L
ES_START             <- 1985L
ES_END               <- 2018L
STUDY_END_DATE       <- as.IDate("2020-12-31")
FEDERAL_MANDATE_DATE <- as.IDate("1998-12-22")
MANDATE_CUTOFF_DATE  <- as.IDate("1988-12-22")
TX_MANDATE_START     <- 1989L
TX_MANDATE_END       <- 1993L
TX_MANDATE_WINDOW_BROAD_START <- 1988L
TX_MANDATE_WINDOW_BROAD_END   <- 1994L

# ── Sample ────────────────────────────────────────────────────────────────────
CONTROL_STATES <- setdiff(
  c("ME","NM","AR","OK","LA","KS","MT","ID","SD","AL",
    "MN","NC","IL","MA","OH","PA","TN","VA","CO"),
  "NJ"    # NJ excluded: private insurance mandate 2003
)
TN_FUND_START_YEAR <- 2008L
MD_NO_FUND         <- TRUE

# ── Make-model cohort window ──────────────────────────────────────────────────
MM_INSTALL_START <- 1990L   # first full post-mandate vintage year
MM_INSTALL_END   <- 1997L   # ensures ≥ 2 pre-treatment years in panel

# Shading boundaries for bar charts (add cohort_shade_layer())
COHORT_SHADE_START <- MM_INSTALL_START - 0.5
COHORT_SHADE_END   <- MM_INSTALL_END   + 0.5

# ── Canonical 5-year age bins ─────────────────────────────────────────────────
AGE_BIN_BREAKS <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
AGE_BIN_LABELS <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35+")
AGE_BIN_REF    <- "0-4"

make_age_bin <- function(age_vec) {
  factor(
    cut(age_vec, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
        right = FALSE, include.lowest = TRUE),
    levels = AGE_BIN_LABELS, ordered = FALSE
  )
}
age_bin_for_reg <- function(ab) relevel(ab, ref = AGE_BIN_REF)

# ── Colors ────────────────────────────────────────────────────────────────────
COL_TX      <- "#D55E00"
COL_CTRL    <- "#0072B2"
COL_YOUNG   <- "#009E73"
COL_OLD     <- "#CC79A7"
COL_PAIR    <- c("Texas" = COL_TX, "Control" = COL_CTRL)
COL_MANDATE <- "gold"
COL_COHORT  <- "steelblue"   # analysis cohort window shading

# ── Publication theme ─────────────────────────────────────────────────────────
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.1),
                                      margin = margin(0,0,6,0)),
      plot.subtitle    = element_text(color = "grey40", size = rel(0.85),
                                      margin = margin(0,0,8,0)),
      plot.caption     = element_text(color = "grey50", size = rel(0.75),
                                      hjust = 0),
      axis.title       = element_text(face = "bold", size = rel(0.9)),
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = "gray85"),
      strip.text       = element_text(face = "bold")
    )
}
theme_set(theme_pub())

# ── Reusable ggplot layers ────────────────────────────────────────────────────

# Light blue shading over the 1990–1997 analysis cohort install window
cohort_shade_layer <- function(ymin = -Inf, ymax = Inf, alpha = 0.10) {
  list(
    annotate("rect",
             xmin = COHORT_SHADE_START, xmax = COHORT_SHADE_END,
             ymin = ymin, ymax = ymax,
             fill = COL_COHORT, alpha = alpha),
    annotate("text",
             x = (COHORT_SHADE_START + COHORT_SHADE_END) / 2,
             y = ymax, vjust = 1.4, size = 2.8, color = "steelblue4",
             label = "Analysis cohort\n(1990\u20131997)")
  )
}

# Dashed vertical line at Dec 1998 reform
treatment_vline <- function(color = "gray30", lwd = 0.7) {
  list(
    geom_vline(xintercept = TREATMENT_YEAR + 0.5,
               linetype = "dashed", color = color, linewidth = lwd),
    annotate("text", x = TREATMENT_YEAR + 0.7, y = Inf,
             vjust = 1.5, hjust = 0, size = 2.6, color = color,
             label = "Dec 1998\nreform")
  )
}

# Gold shading over the 1989–1993 TX phased mandate window
mandate_shade_layer <- function(ymin = -Inf, ymax = Inf, alpha = 0.12) {
  list(
    annotate("rect",
             xmin = TX_MANDATE_START - 0.5, xmax = TX_MANDATE_END + 0.5,
             ymin = ymin, ymax = ymax,
             fill = COL_MANDATE, alpha = alpha),
    annotate("text",
             x = (TX_MANDATE_START + TX_MANDATE_END) / 2,
             y = ymax, vjust = 1.4, size = 2.5, color = "goldenrod4",
             label = "TX mandate\nwindow")
  )
}

# ── File I/O helpers ──────────────────────────────────────────────────────────

# Save figure as both PNG and PDF
save_fig <- function(p, name, width = 10, height = 6, dpi = 300) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")),
         p, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")),
         p, width = width, height = height, device = cairo_pdf)
  cat(sprintf("  [fig] %s\n", name))
  invisible(p)
}

save_table <- function(dt, name) {
  fwrite(dt, file.path(OUTPUT_TABLES, paste0(name, ".csv")))
  cat(sprintf("  [tbl] %s.csv\n", name))
  invisible(dt)
}

write_tex <- function(kbl_obj, name) {
  writeLines(as.character(kbl_obj),
             file.path(OUTPUT_TABLES, paste0(name, ".tex")))
  cat(sprintf("  [tex] %s.tex\n", name))
  invisible(NULL)
}

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

# ── save_panels(): individual panels + patchwork in one call ──────────────────
#
# panels        : named list of ggplots; names become the panel letter suffix
#                 e.g. list(A = p1, B = p2) → saves base_name_PanelA, _PanelB
# base_name     : stem for individual panel file names
# combined_name : file name for patchwork (default: base_name_Combined)
# panel_width/height : dimensions for each individual panel
# combined_width/height : dimensions for the combined figure
# layout        : optional patchwork layout string or design matrix
# title/subtitle: patchwork-level annotation
# tag_levels    : "A" (letters) or "1" (numbers) for panel tags
#
save_panels <- function(panels,
                        base_name,
                        combined_name   = NULL,
                        panel_width     = 8,
                        panel_height    = 5,
                        combined_width  = NULL,
                        combined_height = NULL,
                        ncol            = NULL,
                        nrow            = NULL,
                        layout          = NULL,
                        title           = NULL,
                        subtitle        = NULL,
                        tag_levels      = "A") {

  stopifnot(is.list(panels), length(panels) >= 1)
  if (is.null(names(panels))) names(panels) <- LETTERS[seq_along(panels)]
  if (is.null(combined_name)) combined_name <- paste0(base_name, "_Combined")
  n <- length(panels)
  if (is.null(combined_width))  combined_width  <- panel_width  * min(n, 3)
  if (is.null(combined_height)) combined_height <- panel_height * ceiling(n / 3)

  # ── Individual panels ──
  for (nm in names(panels)) {
    save_fig(panels[[nm]], paste0(base_name, "_Panel", nm),
             width = panel_width, height = panel_height)
  }

  # ── Patchwork ──
  pw <- Reduce(`+`, panels)

  lo_args <- list()
  if (!is.null(ncol))   lo_args$ncol   <- ncol
  if (!is.null(nrow))   lo_args$nrow   <- nrow
  if (!is.null(layout)) lo_args$design <- layout
  if (length(lo_args) > 0) pw <- pw + do.call(plot_layout, lo_args)

  ann_args <- list(tag_levels = tag_levels,
                   theme = theme(plot.tag = element_text(face = "bold")))
  if (!is.null(title))    ann_args$title    <- title
  if (!is.null(subtitle)) ann_args$subtitle <- subtitle
  pw <- pw + do.call(plot_annotation, ann_args)

  save_fig(pw, combined_name,
           width = combined_width, height = combined_height)
  invisible(pw)
}

# ── Interim data helpers (pass objects between 01x scripts) ──────────────────
save_interim <- function(obj, name) {
  path <- file.path(INTERIM_DIR, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat(sprintf("  [interim] saved: %s\n", name))
  invisible(path)
}

load_interim <- function(name) {
  path <- file.path(INTERIM_DIR, paste0(name, ".rds"))
  if (!file.exists(path))
    stop(sprintf(
      "Interim file not found: %s\n  → Run earlier 01x scripts first.", path))
  readRDS(path)
}

cat("01a_Setup.R sourced\n")
