# TICKET 009 — Fit figure dot-sizing fix (in-place edit of 04o_CF_TX_FlatFee.R)
# Created: 2026-05-28
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
The T007 Phase 4 fit/CF figures use `size = n_cell` (raw cell counts)
for the empirical dots, with the legend hidden via
`scale_size_area(guide = "none")`. Readers can't tell how big each
cell is relative to the overall estimation sample.

Fix: switch the dot-size encoding to **cell share of total estimation
observations** (`100 * n_cell / sum(n_cell)`), show the legend with
percentage breaks, and clarify the subtitles. Apply the edit in place
to the existing `04o_CF_TX_FlatFee.R` script — DO NOT create a new
refresh script. On re-run, all 5 figures regenerate with the new
sizing.

═══════════════════════════════════════════════════
SCOPE
═══════════════════════════════════════════════════
- Single-file edit: `Code/Dynamic_Model/04o_CF_TX_FlatFee.R`
- Affects 5 figure outputs:
    Output/Figures/04o_Fit_6p_SW.png
    Output/Figures/04o_Fit_6p_DW.png
    Output/Figures/04o_Fit_6p_CF_Overlay_SW.png
    Output/Figures/04o_Fit_6p_CF_Overlay_DW.png
    Output/Figures/04o_Fit_6p_AllCells_Residuals.png
- NO new files
- NO changes to estimation, CF re-solve, welfare computation, or any
  other section of 04o — only the figure-generation blocks

═══════════════════════════════════════════════════
EDITS
═══════════════════════════════════════════════════

═══ Edit 1 — Compute `cell_pct` once, near the top of the figure block ═══

Wherever the figure-data data.table is first built (the `d` object
fed to the SW/DW fit ggplot — around line 700 of the current file),
add immediately after construction:

  # cell_pct: percent of total estimation observations contributed by
  # each state cell. Used for dot-area encoding so readers can see at a
  # glance which cells dominate identification vs. which are sparse.
  d[, cell_pct := 100 * n_cell / sum(unique(d[, .(s_idx, n_cell)])$n_cell)]

(Or equivalent — the denominator should be the total observations
across the 32 unique state cells in the estimation sample, NOT the
length of `d` after melting/long-format expansion. If d is already in
long format with multiple rows per cell, compute the denominator from
the unique (s_idx, n_cell) pairs.)

═══ Edit 2 — SW/DW fit figures (around lines 707-723) ═══

Replace:

  geom_point(aes(y = empirical, size = n_cell),
             shape = 21, fill = "white", stroke = 1.1, alpha = 0.9) +
  ...
  scale_size_area(max_size = 7, guide = "none") +

With:

  geom_point(aes(y = empirical, size = cell_pct),
             shape = 21, fill = "white", stroke = 1.1, alpha = 0.9) +
  ...
  scale_size_area(
    max_size = 7,
    name     = "Cell share (% of est. obs)",
    breaks   = c(0.5, 2, 5, 10, 20),
    labels   = function(x) sprintf("%.1f%%", x)
  ) +

And update the subtitle line in the same block from:
  subtitle = "Solid/dashed line = MODEL-IMPLIED P(action | state);  open circle = EMPIRICAL share  (size ∝ n obs)"
to:
  subtitle = "Lines = MODEL-IMPLIED P(action | state); open circles = EMPIRICAL share; circle area = cell's share of estimation sample"

Make sure the legend appears (default in ggplot is `legend.position =
"right"` from theme_minimal; if any local theme() call sets
`legend.position = "none"` for the fit-figure theme, change it to
`"right"` or `"bottom"`).

═══ Edit 3 — CF overlay figures (around lines 848-852) ═══

Same replacement as Edit 2 for the empirical-points block in the
CF overlay ggplot. Update its subtitle similarly to refer to "cell's
share of estimation sample" rather than raw obs.

═══ Edit 4 — AllCells_Residuals figure (around lines 745-765) ═══

This figure uses log-scale x-axis with raw `n_cell` as the variable.
Change to `cell_pct` on linear scale (or log10 if dynamic range
warrants — print the range to the log for the coder's call).

Replace:
  scale_x_log10(labels = scales::comma) +
  ...
  labs(... x = "Obs per cell (log scale)", ...)

With:
  scale_x_continuous(labels = function(x) sprintf("%.1f%%", x)) +
  ...
  labs(... x = "Cell share of estimation sample (%)", ...)

And in the data prep, map x to cell_pct.

If the dynamic range of cell_pct exceeds 50× (e.g., smallest cell 0.1%
and largest 20%), use log10 with explicit percentage labels:
  scale_x_log10(breaks = c(0.1, 0.5, 2, 5, 20),
                labels = function(x) sprintf("%.1f%%", x))

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════

CONTENT:
- [ ] Single file modified: Code/Dynamic_Model/04o_CF_TX_FlatFee.R
- [ ] NO new R scripts created
- [ ] cell_pct sums to ~100% across the 32 unique state cells (assert
      with `stopifnot(abs(sum(unique(d[, .(s_idx, cell_pct)])$cell_pct) - 100) < 0.1)`)
- [ ] Each of the 4 fit/overlay figures has a VISIBLE legend titled
      "Cell share (% of est. obs)" with breaks at 0.5%, 2%, 5%, 10%, 20%
- [ ] Each fit/overlay subtitle includes the phrase "circle area = cell's
      share of estimation sample"
- [ ] AllCells_Residuals figure x-axis label says "Cell share of
      estimation sample (%)" (not "Obs per cell")

CODE HYGIENE:
- [ ] No tryCatch returning NULL silently
- [ ] No changes outside the figure-generation blocks
- [ ] Existing assertions/sanity checks elsewhere in 04o still pass

RENDER:
- [ ] 04o_CF_TX_FlatFee.R runs end-to-end without errors
- [ ] All 5 figures overwrite at the existing paths with the new sizing
- [ ] Total runtime within 20 min (NPL + CF re-solve + plotting all
      regenerate from the cached fit; this is acceptable)

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- The CF re-solve and welfare computation will re-run too, but those
  are fast (~30s combined). Don't try to skip them; let 04o run
  end-to-end.
- If the user later wants a figures-only path (skip CF), that's a
  separate concern — not in this ticket.
- Locate the figure-generation blocks by searching for `geom_point`
  and `scale_size_area` in 04o; my line-number citations above are
  approximate.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each attempt. Leave blank until first attempt.]
