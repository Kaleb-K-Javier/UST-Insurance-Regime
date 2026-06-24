# ==============================================================================
# AE06_G_Transition_and_Empirical_Premiums.R -- TICKET 021 extension (researcher-
# directed, 2026-06-11): two exhibits the qmd review found missing.
#   X4C  Rust-style capacity-bin transition matrix BY ACTION (G x G' empirical
#        probabilities; the mileage-process analogue for gallons).
#   X3E  EMPIRICAL average per-tank premium by (wall x age bin) cell, from
#        single-cell TX facility-years (their facility mean premium IS the cell
#        premium) -- the identifying-variation exhibit + the model's p_c.
# READ-ONLY DESCRIPTIVE. Conventions identical to AE01-AE05.
# ==============================================================================

suppressPackageStartupMessages({ library(data.table); library(here) })

.log_path <- here::here("logs", paste0("AE06_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
cat(sprintf("LOG START %s\nScript: AE06\nR: %s\n\n", .log_path, R.version.string))

TAB_DIR <- here("Output", "Tables"); DATA_DIR <- here("Data", "Analysis")
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

cat("=== AE06: G transition by action + empirical cell premiums ===\n")

# ==============================================================================
# X4C. Capacity-bin transition matrix BY ACTION (Rust mileage-process analogue)
# ==============================================================================
cat("=== X4C: G x G' transition by action ===\n")
fr <- fread(file.path(DATA_DIR, "ae_frame.csv"))
fr[, panel_id := as.character(panel_id)]
capf <- fread(file.path(DATA_DIR, "facility_panel.csv"),
              select = c("panel_id", "panel_year", "total_capacity"))
capf[, panel_id := as.character(panel_id)]
sx <- merge(fr[on_spine == 1L, .(panel_id, panel_year, state, action)],
            capf, by = c("panel_id", "panel_year"), all.x = TRUE)
sx <- sx[!is.na(total_capacity) & total_capacity > 0]
setorder(sx, panel_id, panel_year)
# same quartile binning as AE03 X4 (spine sample quantiles)
cap_brk <- quantile(sx$total_capacity, c(0, .25, .5, .75, 1), na.rm = TRUE)
sx[, G := cut(total_capacity, breaks = unique(cap_brk), include.lowest = TRUE,
              labels = paste0("Q", seq_len(length(unique(cap_brk)) - 1)))]
sx[, G_next := shift(G, type = "lead"), by = panel_id]
sx[, nyr := shift(panel_year, type = "lead"), by = panel_id]
tr <- sx[!is.na(G_next) & nyr == panel_year + 1L &
         action %chin% c("Maintain", "Downsize", "Replace")]
x4c <- tr[, .(n = .N), by = .(action, G, G_next)]
x4c[, prob := round(n / sum(n), 6), by = .(action, G)]
setorder(x4c, action, G, G_next)
fwrite(x4c, file.path(TAB_DIR, "AE_X4_G_Transition.csv"))
cat("  saved AE_X4_G_Transition.csv\n")
cat("  diagonal mass by action:\n")
print(tr[, .(stay = round(mean(as.character(G) == as.character(G_next)), 4),
             n = .N), by = action])

# .tex: one row per (action, G): the full G' distribution across columns
wide <- dcast(x4c, action + G ~ G_next, value.var = "prob", fill = 0)
for (qq in c("Q1", "Q2", "Q3", "Q4")) if (!qq %in% names(wide)) wide[, (qq) := 0]
nrow_per <- wide[, .N, by = action]
wide_out <- wide[, .(From = as.character(G),
                     Q1 = sprintf("%.3f", Q1), Q2 = sprintf("%.3f", Q2),
                     Q3 = sprintf("%.3f", Q3), Q4 = sprintf("%.3f", Q4)), by = action]
acts <- c("Maintain", "Downsize", "Replace")
wide_out <- wide_out[order(match(action, acts))]
panel_rows <- cumsum(c(1, head(nrow_per[match(acts, action), N], -1)))
panels <- setNames(as.list(paste0("Panel ", LETTERS[1:3], ". ", acts)),
                   as.character(panel_rows))
write_booktabs(wide_out[, .(From, Q1, Q2, Q3, Q4)],
               file.path(TAB_DIR, "AE_X4_G_Transition.tex"),
               align = "lrrrr", header = c("From bin", "Q1", "Q2", "Q3", "Q4"),
               panels = panels)

# ==============================================================================
# X3E. Empirical average per-tank premium by cell (single-cell TX facilities)
# ==============================================================================
cat("=== X3E: empirical cell premiums from single-cell TX facility-years ===\n")
boylong <- fread(file.path(DATA_DIR, "boy_composition_long.csv"))
boylong[, panel_id := as.character(panel_id)]
boylong <- boylong[panel_year >= 2006L]
occ <- boylong[, .(n_occ = .N, wall = wall[1L], age_bin = age_bin[1L]),
               by = .(panel_id, panel_year, state)]
single <- occ[n_occ == 1L & state == "TX"]
cat(sprintf("  single-cell TX facility-years 2006+: %s\n", fmt_n(nrow(single))))

mid <- fread(file.path(DATA_DIR, "tx_midcont_premium_all_1999_onwards.csv"),
             select = c("panel_id", "panel_year", "mean_tank_premium", "source_era"))
mid[, panel_id := as.character(panel_id)]
mid <- mid[is.finite(mean_tank_premium)]
emp <- merge(single, mid, by = c("panel_id", "panel_year"))
cat(sprintf("  matched to engine premiums: %s\n", fmt_n(nrow(emp))))

x3e <- emp[, .(n = .N,
               mean_premium = round(mean(mean_tank_premium), 2),
               median_premium = round(median(mean_tank_premium), 2),
               sd_premium = round(sd(mean_tank_premium), 2)),
           by = .(era = source_era, wall, age_bin)]
setorder(x3e, era, wall, age_bin)
fwrite(x3e, file.path(TAB_DIR, "AE_X3_Empirical_Premium.csv"))
cat("  saved AE_X3_Empirical_Premium.csv\n")

# .tex: era_2006 matrix (wall rows x age-bin columns of mean premium, n beneath)
e06 <- x3e[era == "era_2006"]
mtx <- dcast(e06, wall ~ age_bin, value.var = "mean_premium")
ntx <- dcast(e06, wall ~ age_bin, value.var = "n")
for (b in as.character(1:8)) { if (!b %in% names(mtx)) mtx[, (b) := NA_real_]
                               if (!b %in% names(ntx)) ntx[, (b) := NA_integer_] }
mtx <- mtx[match(c("SW", "DW"), wall)]; ntx <- ntx[match(c("SW", "DW"), wall)]
rows <- list()
for (w in c("SW", "DW")) {
  mrow <- mtx[wall == w]; nrow_ <- ntx[wall == w]
  rows[[length(rows) + 1L]] <- data.table(t(c(w,
    sapply(as.character(1:8), function(b) {
      v <- mrow[[b]]; if (is.na(v)) "--" else sprintf("%.0f", v) }))))
  rows[[length(rows) + 1L]] <- data.table(t(c("(facility-years)",
    sapply(as.character(1:8), function(b) {
      v <- nrow_[[b]]; if (is.na(v)) "--" else fmt_n(v) }))))
}
x3e_tex <- rbindlist(rows)
write_booktabs(x3e_tex, file.path(TAB_DIR, "AE_X3_Empirical_Premium.tex"),
               align = "lrrrrrrrr",
               header = c("Wall", as.character(1:8)))
cat("  era_2006 empirical matrix:\n"); print(mtx); print(ntx)

cat("=== AE06 DONE ===\n")
sink(type = "message"); sink(type = "output"); close(.log)
