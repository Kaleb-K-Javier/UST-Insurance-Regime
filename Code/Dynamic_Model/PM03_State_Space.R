# PM03_State_Space.R — Portfolio model state space and transition skeleton
# TICKET 023 B3. Enumerate compositions; precompute rmap, imap, post, A_age.
# THIS FILE IS CHECKED HARDEST BY REVIEWER — see acceptance criteria.
# Outputs: Output/Estimation_Results/PM_StateSpace.rds,
#          Data/Analysis/pm_agg_counts.csv,  logs/PM03_*.log
suppressPackageStartupMessages({
  library(data.table); library(here); library(Matrix); library(partitions)
})

t_start <- proc.time()

.log_path <- here::here("logs", paste0("PM03_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM03_State_Space\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# --- constants (from TICKET 023) ---
N_BAR    <- 6L
K_BAR    <- 4L
M_BAR    <- 4L
N_CELLS  <- 16L
EXPECTED_C <- 74612L   # sum_{j=1..6} C(j+15,15)
DATA_DIR <- here("Data", "Analysis")
RES_DIR  <- here("Output", "Estimation_Results")

# MARG order: pos 1=SW_8 .. pos 8=SW_1, pos 9=DW_8 .. pos 16=DW_1
MARG_WALL <- c(rep("SW", 8L), rep("DW", 8L))
MARG_BIN  <- c(8:1, 8:1)   # age_bin for each position (bin 8 = oldest, bin 1 = youngest)

# AGE_BREAKS and midpoints (04a line 67 / AE08 line 79)
AGE_BREAKS <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
AGE_MIDPTS <- c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5)
midpts_16  <- AGE_MIDPTS[MARG_BIN]   # midpoint for each of the 16 MARG positions

bin_age <- function(a) {
  b <- as.integer(cut(a, AGE_BREAKS, labels = 1:8, right = FALSE, include.lowest = TRUE))
  b[is.na(b)] <- 8L
  b
}

# ==============================================================================
# 0. Load lookups needed for statics
# ==============================================================================
cat("=== SECTION 0: load lookups ===\n")
lk   <- readRDS(file.path(RES_DIR, "PM_Lookups.rds"))
pbar <- lk$pbar    # 16 x 3 (MARG order x eras)
adv  <- lk$adv     # length-8 advance probabilities; adv[8]=0

# ==============================================================================
# 1. Enumerate compositions: all 16-vectors n with 1 <= sum(n) <= N_BAR
#    using partitions::compositions (approved Q3 ruling)
# ==============================================================================
cat("=== SECTION 1: enumerate compositions ===\n")
t_enum <- proc.time()
comp_list <- vector("list", N_BAR)
for (j in seq_len(N_BAR)) {
  m_j <- as.matrix(compositions(j, N_CELLS))  # N_CELLS x C(j+15,15)
  comp_list[[j]] <- t(m_j)                     # C(j+15,15) x N_CELLS
  storage.mode(comp_list[[j]]) <- "integer"
  cat(sprintf("  j=%d: %d compositions\n", j, nrow(comp_list[[j]])))
}
comp_mat <- do.call(rbind, comp_list)
C <- nrow(comp_mat)
cat(sprintf("  Total compositions: %d (expected %d)\n", C, EXPECTED_C))
stopifnot(C == EXPECTED_C)

# Build hash: key -> comp_id
cat("  Building composition hash...\n")
keys <- apply(comp_mat, 1L, paste, collapse = "-")
comp_env <- new.env(hash = TRUE, parent = emptyenv(), size = C + 1000L)
for (i in seq_len(C)) comp_env[[keys[i]]] <- i
cat(sprintf("  Hash built: %d entries | enum time: %.1fs\n",
            C, (proc.time() - t_enum)["elapsed"]))

# ==============================================================================
# 2. Per-composition statics
# ==============================================================================
cat("=== SECTION 2: per-composition statics ===\n")
N_vec    <- rowSums(comp_mat)                             # tank count
n_SW_tot <- rowSums(comp_mat[, 1:8,  drop = FALSE])      # total SW tanks
n_DW_tot <- rowSums(comp_mat[, 9:16, drop = FALSE])      # total DW tanks

# weighted average age -> bin
avg_age  <- as.numeric(comp_mat %*% midpts_16) / N_vec   # safe: N>=1
abar_bin <- bin_age(avg_age)

# majority wall: SW if n_SW >= n_DW (ties -> SW)
majwall  <- ifelse(n_SW_tot >= n_DW_tot, "SW", "DW")
majwall_int <- as.integer(majwall == "DW") + 1L           # SW=1, DW=2

# h_aw index: (majwall-1)*8 + abar_bin (1-based, matches h_aw indexing)
h_idx <- (majwall_int - 1L) * 8L + abar_bin
stopifnot(all(h_idx >= 1L), all(h_idx <= 16L))

# P_RB by era: comp_mat %*% pbar (vectorized, C x 3)
P_RB_all <- comp_mat %*% pbar     # C x 3; colnames = c("2006","2014","2019")
cat(sprintf("  P_RB_2006 range: [%.4f, %.4f] (model units)\n",
            min(P_RB_all[,1]), max(P_RB_all[,1])))

# ==============================================================================
# 3. Removal map rmap[C, 0:K_BAR]
#    Greedy cumsum trick from AE04/AE08 (vectorized matrix form)
#    MARG order: SW_8 (oldest) first; remove up to k <= N-1 tanks
# ==============================================================================
cat("=== SECTION 3: removal map rmap ===\n")
t_rmap <- proc.time()
# pre-compute cumulative sums once
CB     <- t(apply(comp_mat, 1L, cumsum))     # C x 16 cumulative sums
before <- CB - comp_mat                       # C x 16: tanks before position p

rmap <- matrix(NA_integer_, nrow = C, ncol = K_BAR + 1L)
rmap[, 1L] <- seq_len(C)  # k=0: identity

for (ki in seq_len(K_BAR)) {
  # valid: can remove ki tanks (N >= ki+1)
  valid  <- (N_vec >= ki + 1L)
  # greedy allocation: taken[i,p] = min(max(ki - before[i,p], 0), comp_mat[i,p])
  taken  <- pmin(pmax(ki - before, 0L), comp_mat)
  n_new  <- comp_mat - taken                  # C x 16 post-removal compositions
  storage.mode(n_new) <- "integer"
  # generate keys and look up comp_ids for valid rows
  valid_idx <- which(valid)
  new_keys  <- apply(n_new[valid_idx, , drop = FALSE], 1L, paste, collapse = "-")
  ids <- vapply(new_keys, function(k) {
    v <- comp_env[[k]]; if (is.null(v)) NA_integer_ else as.integer(v)
  }, integer(1L))
  rmap[valid_idx, ki + 1L] <- ids
}
cat(sprintf("  rmap built (%dx%d) in %.1fs\n",
            nrow(rmap), ncol(rmap), (proc.time() - t_rmap)["elapsed"]))

# spot check: rmap[,2] (k=1) should shift oldest non-zero cell down by 1
# verify NA exactly where k > N-1
for (ki in seq_len(K_BAR)) {
  n_na      <- sum(is.na(rmap[, ki + 1L]))
  n_invalid <- sum(N_vec < ki + 1L)
  stopifnot(n_na == n_invalid)
}
cat("  rmap NA-pattern assertion PASS\n")

# ==============================================================================
# 4. Install map imap[C, 0:M_BAR]: add m DW_1 tanks (position 16 in MARG order)
# ==============================================================================
cat("=== SECTION 4: install map imap ===\n")
imap <- matrix(NA_integer_, nrow = C, ncol = M_BAR + 1L)
imap[, 1L] <- seq_len(C)  # m=0: identity

for (mi in seq_len(M_BAR)) {
  valid  <- (N_vec + mi <= N_BAR)
  n_new  <- comp_mat
  n_new[, N_CELLS] <- n_new[, N_CELLS] + mi  # add mi DW_1 tanks to position 16
  storage.mode(n_new) <- "integer"
  valid_idx <- which(valid)
  new_keys  <- apply(n_new[valid_idx, , drop = FALSE], 1L, paste, collapse = "-")
  ids <- vapply(new_keys, function(k) {
    v <- comp_env[[k]]; if (is.null(v)) NA_integer_ else as.integer(v)
  }, integer(1L))
  imap[valid_idx, mi + 1L] <- ids
}
cat(sprintf("  imap built (%dx%d)\n", nrow(imap), ncol(imap)))

# verify NA exactly where N + m > N_BAR
for (mi in seq_len(M_BAR)) {
  n_na      <- sum(is.na(imap[, mi + 1L]))
  n_invalid <- sum(N_vec + mi > N_BAR)
  stopifnot(n_na == n_invalid)
}
cat("  imap NA-pattern assertion PASS\n")

# ==============================================================================
# 5. Post-action map post[C, k, m] = imap[rmap[C,k], m]
# ==============================================================================
cat("=== SECTION 5: post-action map ===\n")
post <- array(NA_integer_, dim = c(C, K_BAR + 1L, M_BAR + 1L))
for (ki in 0L:K_BAR) {
  rmap_col <- rmap[, ki + 1L]
  valid_rm <- which(!is.na(rmap_col))
  for (mi in 0L:M_BAR) {
    post[valid_rm, ki + 1L, mi + 1L] <- imap[rmap_col[valid_rm], mi + 1L]
  }
}
# NA propagates: off-menu entries
n_valid_post <- sum(!is.na(post))
cat(sprintf("  post array: %d total | %d valid | %d NA (off-menu)\n",
            length(post), n_valid_post, sum(is.na(post))))

# ==============================================================================
# 6. Aging matrix A_age [C x C sparse]
#    Binomial aging: j_p ~ Binom(n_p, adv[bin_p]) movers from position p to p-1
#    (p=1 and p=9 are oldest per wall, adv[8]=0 so no movers from those positions)
#    Build in chunks of 5000 compositions; ~20-60 nonzeros per row expected.
# ==============================================================================
cat("=== SECTION 6: aging matrix A_age ===\n")
t_age <- proc.time()

CHUNK_SIZE <- 5000L
n_chunks   <- ceiling(C / CHUNK_SIZE)
triplet_chunks <- vector("list", n_chunks)

for (ch in seq_len(n_chunks)) {
  ch_start <- (ch - 1L) * CHUNK_SIZE + 1L
  ch_end   <- min(ch * CHUNK_SIZE, C)
  chunk_ids <- ch_start:ch_end

  rows_buf <- vector("list", length(chunk_ids))

  for (idx in seq_along(chunk_ids)) {
    c_idx <- chunk_ids[idx]
    n_vec <- comp_mat[c_idx, ]

    # advancing positions: n > 0 AND bin < 8 (adv[8]=0 so only bins 1..7 advance)
    adv_pos <- which(n_vec > 0L & MARG_BIN < 8L)

    if (length(adv_pos) == 0L) {
      # all tanks in oldest bins — composition stays
      rows_buf[[idx]] <- data.table(row = c_idx, col = c_idx, val = 1.0)
      next
    }

    K <- length(adv_pos)

    # delta matrix: 16 x K. position p loses j, position p-1 gains j.
    delta_mat <- matrix(0L, N_CELLS, K)
    for (qi in seq_len(K)) {
      p <- adv_pos[qi]
      delta_mat[p,     qi] <- -1L
      delta_mat[p - 1L, qi] <-  1L
    }

    # enumerate j combinations for each advancing position
    j_ranges <- lapply(adv_pos, function(p) 0L:n_vec[p])
    combos   <- as.matrix(do.call(expand.grid, j_ranges))
    n_combos <- nrow(combos)

    # outcome compositions: N_CELLS x n_combos (vectorized via matrix multiply)
    n_out_mat <- matrix(as.integer(n_vec), N_CELLS, n_combos) +
                 delta_mat %*% t(combos)
    storage.mode(n_out_mat) <- "integer"

    # key lookup for each outcome
    keys_out <- apply(n_out_mat, 2L, paste, collapse = "-")
    cols_out <- vapply(keys_out, function(k) {
      v <- comp_env[[k]]; if (is.null(v)) NA_integer_ else as.integer(v)
    }, integer(1L))

    # probabilities: pre-compute binomial tables, then index
    binom_tbls <- lapply(seq_len(K), function(qi) {
      p <- adv_pos[qi]
      dbinom(0L:n_vec[p], n_vec[p], adv[MARG_BIN[p]])
    })
    prob_mat <- matrix(0, n_combos, K)
    for (qi in seq_len(K)) prob_mat[, qi] <- binom_tbls[[qi]][combos[, qi] + 1L]
    vals_out <- apply(prob_mat, 1L, prod)

    rows_buf[[idx]] <- data.table(row = c_idx, col = cols_out, val = vals_out)
  }

  triplet_chunks[[ch]] <- rbindlist(rows_buf)
  if (ch %% 3L == 0L || ch == n_chunks) {
    elapsed <- (proc.time() - t_age)["elapsed"]
    cat(sprintf("  [%s] aging chunk %d/%d done | %.1fs elapsed\n",
                format(Sys.time(), "%H:%M:%S"), ch, n_chunks, elapsed))
  }
}

all_triplets <- rbindlist(triplet_chunks)
rm(triplet_chunks); invisible(gc())
cat(sprintf("  triplets: %s rows | any NA col: %s\n",
            format(nrow(all_triplets), big.mark=","), any(is.na(all_triplets$col))))
stopifnot(!anyNA(all_triplets$col))

# aggregate duplicate (row, col) pairs (can arise if multiple j-paths land on same outcome)
all_triplets <- all_triplets[, .(val = sum(val)), by = .(row, col)]
cat(sprintf("  triplets after dedup: %s\n", format(nrow(all_triplets), big.mark=",")))

A_age <- sparseMatrix(
  i    = all_triplets$row,
  j    = all_triplets$col,
  x    = all_triplets$val,
  dims = c(C, C)
)
rm(all_triplets); invisible(gc())

# assert row sums == 1 to tolerance 1e-10
rs <- rowSums(A_age)
max_rs_err <- max(abs(rs - 1))
cat(sprintf("  A_age row sum max error: %.2e (require < 1e-10)\n", max_rs_err))
stopifnot(max_rs_err < 1e-10)
cat(sprintf("  A_age: %dx%d sparse | nnz=%s | aging time %.1fs\n",
            nrow(A_age), ncol(A_age),
            format(Matrix::nnzero(A_age), big.mark=","),
            (proc.time() - t_age)["elapsed"]))

# ==============================================================================
# 7. Full state index  sidx = (G-1)*C + comp_id
# ==============================================================================
cat("=== SECTION 7: state index convention ===\n")
N_G    <- 4L
N_SIDX <- N_G * C   # 4 x 74,612 = 298,448
cat(sprintf("  Total operating states: %d (4 x %d)\n", N_SIDX, C))
cat("  sidx = (G-1)*C + comp_id  where G in {1,2,3,4}\n")
cat("  A_age is C x C (comp transitions); 024 composes with Gmat for full s-level transitions\n")

# ==============================================================================
# 8. Observed-support report: map pm_panel rows -> sidx
# ==============================================================================
cat("=== SECTION 8: observed-support report ===\n")
pm <- fread(file.path(DATA_DIR, "pm_panel.csv"))
pm[, panel_id := as.character(panel_id)]

CELL_COLS <- c(paste0("n_SW", 8:1), paste0("n_DW", 8:1))

# compute composition key for each pm_panel row
pm_cell_mat <- as.matrix(pm[, CELL_COLS, with = FALSE])
storage.mode(pm_cell_mat) <- "integer"
cat(sprintf("  computing %s composition keys...\n", format(nrow(pm), big.mark=",")))
pm_keys <- apply(pm_cell_mat, 1L, paste, collapse = "-")
pm_comp_id <- vapply(pm_keys, function(k) {
  v <- comp_env[[k]]; if (is.null(v)) NA_integer_ else as.integer(v)
}, integer(1L))
pm[, comp_id := pm_comp_id]

share_found <- mean(!is.na(pm$comp_id))
n_distinct_sidx <- uniqueN(pm[!is.na(comp_id) & !is.na(G),
                               .(sidx = (G - 1L) * C + comp_id)])
cat(sprintf("  facility-years with composition in enumeration: %.4f (expect ~0.981)\n",
            share_found))
cat(sprintf("  distinct visited (comp_id, G) pairs: %d\n", n_distinct_sidx))

# sidx for all rows with valid comp_id and G
pm[!is.na(comp_id) & !is.na(G), sidx := (G - 1L) * C + comp_id]

# ==============================================================================
# 9. Likelihood aggregation file: pm_agg_counts.csv
#    Included rows only (all excl_* == 0), valid sidx
# ==============================================================================
cat("=== SECTION 9: pm_agg_counts.csv ===\n")
incl <- pm[excl_state == 0L & excl_expansion == 0L & excl_bigN == 0L &
           excl_offmenu == 0L & excl_k0shed == 0L & !is.na(sidx)]
cat(sprintf("  included rows with valid sidx: %s (of %s total pm_panel rows)\n",
            format(nrow(incl), big.mark=","), format(nrow(pm), big.mark=",")))

agg_counts <- incl[, .(n_obs = .N), by = .(sidx, g, era, action)]
setorder(agg_counts, sidx, g, era, action)
# enforce types
agg_counts[, sidx   := as.integer(sidx)]
agg_counts[, g      := as.character(g)]
agg_counts[, era    := as.character(era)]
agg_counts[, action := as.character(action)]
agg_counts[, n_obs  := as.integer(n_obs)]

stopifnot(sum(agg_counts$n_obs) == nrow(incl))
cat(sprintf("  agg_counts: %s rows | sum(n_obs)=%s == included rows: PASS\n",
            format(nrow(agg_counts), big.mark=","),
            format(sum(agg_counts$n_obs), big.mark=",")))
cat("  agg_counts action distribution:\n")
print(agg_counts[, .(n_obs = sum(n_obs)), by = action][order(-n_obs)])

fwrite(agg_counts, file.path(DATA_DIR, "pm_agg_counts.csv"))
cat(sprintf("  saved %s\n", file.path(DATA_DIR, "pm_agg_counts.csv")))

# ==============================================================================
# 10. Save PM_StateSpace.rds
# ==============================================================================
cat("=== SECTION 10: save PM_StateSpace.rds ===\n")
state_space <- list(
  comp_mat  = comp_mat,    # C x 16 integer matrix (MARG order)
  C         = C,
  keys      = keys,        # length-C character keys
  # statics
  N_vec     = N_vec,
  n_SW_tot  = n_SW_tot,
  abar_bin  = abar_bin,
  majwall   = majwall,
  h_idx     = h_idx,
  P_RB_all  = P_RB_all,   # C x 3; colnames = ERAS
  # maps
  rmap      = rmap,        # C x (K_BAR+1) integer; col k+1 = post-removal for k tanks
  imap      = imap,        # C x (M_BAR+1) integer; col m+1 = post-install for m tanks
  post      = post,        # C x (K_BAR+1) x (M_BAR+1) integer
  # transition
  A_age     = A_age,       # C x C sparse (Matrix::dgCMatrix)
  # sidx convention
  N_G       = N_G,
  N_SIDX    = N_SIDX,
  # metadata
  N_BAR     = N_BAR,
  K_BAR     = K_BAR,
  M_BAR     = M_BAR,
  MARG_WALL = MARG_WALL,
  MARG_BIN  = MARG_BIN,
  AGE_BREAKS = AGE_BREAKS,
  AGE_MIDPTS = AGE_MIDPTS
)
out_rds <- file.path(RES_DIR, "PM_StateSpace.rds")
saveRDS(state_space, out_rds)
cat(sprintf("  saved %s\n", out_rds))

total_sec <- (proc.time() - t_start)["elapsed"]
cat(sprintf("\nPM03 total runtime: %.1f seconds (%.1f minutes)\n",
            total_sec, total_sec / 60))
cat("=== PM03 DONE ===\n")
