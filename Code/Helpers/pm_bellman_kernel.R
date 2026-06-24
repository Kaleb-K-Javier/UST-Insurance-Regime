# pm_bellman_kernel.R
# Portfolio model: Bellman value-solve kernel (Ticket 024a)
# NO side effects on source — pure function definitions only.

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
})

# Euler-Mascheroni constant and scale normalization (spec L6)
gamma_E <- 0.5772156649
sigma    <- 1.0

# ---------------------------------------------------------------------------
# netbin: net install bin label for action (k,m)
# Returns one of "-3","-2","-1","0","1","nowork"
# "nowork" iff k==0 AND m==0 (maintain); else pmax(-3, pmin(1, m-k))
# ---------------------------------------------------------------------------
netbin <- function(k, m) {
  stopifnot(length(k) == 1L, length(m) == 1L)
  if (k == 0L && m == 0L) return("nowork")
  as.character(pmax(-3L, pmin(1L, m - k)))
}

# ---------------------------------------------------------------------------
# feasible_actions: return data.table(k,m) for a facility with N tanks.
# Rule (023): 0<=k<=min(N-1,K_BAR), 0<=m<=M_BAR, N-k+m<=N_BAR, NOT(k==0&m>0).
# Exit X is always feasible but NOT returned here (WORK actions only).
# ---------------------------------------------------------------------------
feasible_actions <- function(N, K_BAR = 4L, M_BAR = 4L, N_BAR = 6L) {
  rows <- list()
  for (k in 0L:min(N - 1L, K_BAR)) {
    for (m in 0L:M_BAR) {
      if (k == 0L && m > 0L) next          # no pure-install without removal
      if (N - k + m > N_BAR)  next          # capacity cap
      rows[[length(rows) + 1L]] <- data.table(k = k, m = m)
    }
  }
  rbindlist(rows)
}

# ---------------------------------------------------------------------------
# build_kernel: precompute all per-action index vectors and Gmat selection.
# Returns a list (kernel object) used by comp_apply, Mv, solve_V.
# ---------------------------------------------------------------------------
build_kernel <- function(ss, lk) {
  C      <- ss$C
  A_age  <- ss$A_age          # C x C dgCMatrix
  rmap   <- ss$rmap           # C x (K_BAR+1), 1-based comp IDs or NA
  imap   <- ss$imap           # C x (M_BAR+1), 1-based comp IDs or NA
  N_vec  <- ss$N_vec          # length C, N(c) for each comp
  N_G    <- ss$N_G            # = 4
  Gmat   <- lk$Gmat           # named list of 4x4 matrices (row=from, col=to)

  K_BAR  <- ncol(rmap) - 1L
  M_BAR  <- ncol(imap) - 1L
  N_BAR  <- 6L

  stopifnot(inherits(A_age, "dgCMatrix"))
  stopifnot(nrow(A_age) == C, ncol(A_age) == C)
  stopifnot(length(N_vec) == C)
  stopifnot(N_G == 4L)
  stopifnot(all(c("nowork","0","1","-1","-2","-3") %in% names(Gmat)))

  # Verify imap column 1 = identity (k=0 no-op); imap[,1] == 1:C
  stopifnot(all(imap[, 1L] == seq_len(C)))

  # Verify rmap column 1 = identity (m=0 no-op); rmap[,1] == 1:C
  stopifnot(all(rmap[, 1L] == seq_len(C)))

  # Injectivity of imap: for each m>0, imap[,m+1] values (ignoring NA) are unique
  for (m in 1L:M_BAR) {
    vals <- imap[, m + 1L]
    vals <- vals[!is.na(vals)]
    stopifnot(length(vals) == length(unique(vals)))
  }

  # Build per-N feasible action tables
  unique_N <- sort(unique(N_vec))
  fa_by_N  <- setNames(lapply(unique_N, feasible_actions, K_BAR=K_BAR, M_BAR=M_BAR, N_BAR=N_BAR),
                        as.character(unique_N))

  # Build global WORK action set (unique (k,m) pairs across all N)
  work_all <- unique(rbindlist(fa_by_N))
  setkey(work_all, k, m)

  list(
    C       = C,
    A_age   = A_age,
    rmap    = rmap,
    imap    = imap,
    N_vec   = N_vec,
    N_G     = N_G,
    K_BAR   = K_BAR,
    M_BAR   = M_BAR,
    N_BAR   = N_BAR,
    Gmat    = Gmat,
    fa_by_N = fa_by_N,
    work_all= work_all
  )
}

# ---------------------------------------------------------------------------
# comp_apply: apply CompKer_(k,m) to X (C x ncol matrix).
# Implements D1 timing: remove (rmap) -> age (A_age) -> install (imap).
# Per the spec recipe (R-IMPLEMENTATION NOTES, EXACT RECIPE):
#   Step 1: INSTALL = input-side gather on X (BEFORE A_age) using imap[,m+1]
#   Step 2: AGE survivors via A_age %*% Y
#   Step 3: REMOVE = output-side gather on Z using rmap[,k+1]
# Infeasible rows (NA index) are zeroed at both steps (NA trap Q2).
# ---------------------------------------------------------------------------
comp_apply <- function(kernel, k, m, X) {
  stopifnot(is.matrix(X), nrow(X) == kernel$C)

  # Step 1: install gather (input side)
  if (m == 0L) {
    Y <- X
  } else {
    gi <- kernel$imap[, m + 1L]           # length C, 1-based or NA
    Y  <- X[gi, , drop = FALSE]           # C x ncol; NAs where gi is NA
    na_rows <- is.na(gi)
    if (any(na_rows)) Y[na_rows, ] <- 0
  }

  # Step 2: age
  Z <- kernel$A_age %*% Y                 # C x ncol (sparse %*% dense)

  # Step 3: remove gather (output side)
  if (k == 0L) {
    out <- Z
  } else {
    go  <- kernel$rmap[, k + 1L]          # length C, 1-based or NA
    out <- Z[go, , drop = FALSE]
    na_rows <- is.na(go)
    if (any(na_rows)) out[na_rows, ] <- 0
  }

  stopifnot(!anyNA(out))
  out
}

# ---------------------------------------------------------------------------
# Mv: structured matvec (I - beta M) rhs helper.
# Returns (M V)mat as C x 4 matrix.
# For each WORK action a=(k,m) feasible for at least one comp:
#   W_a = Vmat %*% t(Gmat_a)             # C x 4
#   Z_a = comp_apply(k, m, W_a)          # C x 4 (CompKer applied)
#   add P_a[,G] * Z_a[,G] column-wise    # elementwise multiply over C per G
# P is a named list: P[[action_str]][c, G] where action_str = "k,m" or "X".
# Only WORK actions (not "X") feed Mv.
# ---------------------------------------------------------------------------
Mv <- function(kernel, P, Vmat) {
  stopifnot(is.matrix(Vmat), nrow(Vmat) == kernel$C, ncol(Vmat) == kernel$N_G)

  result <- matrix(0.0, nrow = kernel$C, ncol = kernel$N_G)

  work_dt <- kernel$work_all
  for (i in seq_len(nrow(work_dt))) {
    k <- work_dt$k[i]
    m <- work_dt$m[i]
    akey <- paste0(k, ",", m)

    if (!akey %in% names(P)) next        # action not in P means P_a = 0 everywhere

    P_a  <- P[[akey]]                    # C x 4
    nb   <- netbin(k, m)
    Ga   <- kernel$Gmat[[nb]]            # 4x4, row=from, col=to

    # W_a = Vmat %*% t(Gmat_a):  contracts over TO-index G'
    W_a  <- Vmat %*% t(Ga)             # C x 4

    # Apply CompKer
    Z_a  <- comp_apply(kernel, k, m, W_a)  # C x 4

    # Accumulate: elementwise P_a * Z_a
    result <- result + P_a * Z_a
  }

  result
}

# ---------------------------------------------------------------------------
# solve_V: Anderson-accelerated fixed point for (I - beta M) V = R.
# Fixed-point map: F(V) = R + beta * M V  (contraction, rate <= beta = 0.9957)
# Residual: g(V) = F(V) - V
# Walker-Ni (2011) Type-1 Anderson(depth=5):
#   ΔG = column diffs of [g_{n-m},...,g_n]  (N x m)
#   ΔX = column diffs of [x_{n-m},...,x_n]
#   gamma = argmin ||ΔG gamma + g_n||_2  (via normal eqs with relative ridge)
#   V_{n+1} = F(V_n) - (ΔX + ΔG) gamma
# Relative ridge 1e-6*max(diag(gram)) ensures Gram is always well-conditioned.
# Returns list(V, iters, resid, converged). Hard error if not converged.
# ---------------------------------------------------------------------------
solve_V <- function(kernel, R, P, beta, tol = 1e-9, max_it = 10000L, anderson = 5L) {
  C   <- kernel$C
  N_G <- kernel$N_G
  N   <- C * N_G

  R_mat <- matrix(R, nrow = C)

  # Fixed-point map F(v) = R + beta * M v
  FP_fn <- function(v) {
    Vm <- matrix(v, nrow = C)
    as.vector(R_mat + beta * Mv(kernel, P, Vm))
  }

  V_vec <- as.vector(R) / (1 - beta)   # geometric series initial guess

  # Anderson history: columns are most-recent-last
  G_hist <- matrix(0.0, nrow = N, ncol = 0L)  # residuals
  X_hist <- matrix(0.0, nrow = N, ncol = 0L)  # iterates

  for (it in seq_len(max_it)) {
    Fv <- FP_fn(V_vec)
    gv <- Fv - V_vec              # residual g(V_n)

    resid_inf <- max(abs(gv))
    if (it %% 200L == 0L)
      cat(sprintf("  [%s] solve_V iter %d: resid=%.3e\n",
                  format(Sys.time(), "%H:%M:%S"), it, resid_inf))
    if (resid_inf < tol) {
      return(list(V = V_vec, iters = it, resid = resid_inf, converged = TRUE))
    }

    # Append current (iterate, residual) to history, then trim to depth
    G_hist <- cbind(G_hist, gv)
    X_hist <- cbind(X_hist, V_vec)
    if (ncol(G_hist) > anderson + 1L) {
      keep   <- seq(ncol(G_hist) - anderson, ncol(G_hist))
      G_hist <- G_hist[, keep, drop = FALSE]
      X_hist <- X_hist[, keep, drop = FALSE]
    }

    m_k <- ncol(G_hist) - 1L   # number of ΔG columns (history pairs)

    if (m_k == 0L) {
      V_vec <- Fv   # plain FP on very first step
    } else {
      # ΔG: N x m_k (consecutive column diffs of G_hist, including current g_n)
      # ΔX: N x m_k (same for iterates)
      nh  <- ncol(G_hist)
      dG  <- G_hist[, 2L:nh, drop = FALSE] - G_hist[, 1L:(nh - 1L), drop = FALSE]  # N x m_k
      dX  <- X_hist[, 2L:nh, drop = FALSE] - X_hist[, 1L:(nh - 1L), drop = FALSE]

      # Normal equations: (ΔG^T ΔG) gamma = -ΔG^T g_n
      gram     <- crossprod(dG)                       # m_k x m_k
      rhs_neg  <- -as.vector(crossprod(dG, gv))       # -(ΔG^T g_n), length m_k

      max_diag <- max(diag(gram))

      if (max_diag < 1e-28) {
        # dG effectively zero (residuals not changing) — plain FP is fine
        V_vec <- Fv
      } else {
        # Relative ridge: condition number bounded to ~1e6; floor 1e-10 prevents
        # tiny-but-nonzero gram from amplifying floating-point noise in rhs_neg
        ridge_val <- 1e-6 * max_diag + 1e-30   # relative only; no absolute floor
        gamma     <- solve(gram + ridge_val * diag(m_k), rhs_neg)

        # V_{n+1} = F(V_n) + (ΔX + ΔG) gamma  [Walker-Ni 2011: gamma = -(ΔG'ΔG)^{-1}ΔG'gv]
        V_step <- Fv + as.vector((dX + dG) %*% gamma)

        if (anyNA(V_step) || !all(is.finite(V_step))) {
          # Numerical blowup: restart Anderson history, take plain FP step
          G_hist <- matrix(0.0, nrow = N, ncol = 0L)
          X_hist <- matrix(0.0, nrow = N, ncol = 0L)
          V_vec  <- Fv
        } else {
          V_vec <- V_step
        }
      }
    }
  }

  stop(sprintf(
    "solve_V did not converge in %d iterations; final inf-norm residual = %.3e (tol=%.3e)",
    max_it, max(abs(FP_fn(V_vec) - V_vec)), tol
  ))
}
