// pm_matvec.cpp — C++ structured matvec for portfolio Bellman (Ticket 024c)
// ARMA_NO_DEBUG: correctness confirmed by 1e-12 gate in PM06 before this flag matters.
// ARMA_64BIT_WORD: required because C=74612, so C*C=5.57e9 exceeds 32-bit index limit.
// [[Rcpp::depends(RcppArmadillo)]]
#define ARMA_NO_DEBUG
#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
#include <vector>

struct MvOp {
  arma::sp_mat             A_age;       // C x C sparse age-advance matrix (deep copy)
  int                      C;
  int                      N_G;
  int                      n_actions;
  std::vector<std::vector<int>> g_in;  // [n_actions][C]: 0-based install gather, -1 = infeasible
  std::vector<std::vector<int>> g_out; // [n_actions][C]: 0-based remove  gather, -1 = infeasible
  std::vector<arma::mat>   GaT;        // [n_actions]: N_G x N_G pre-transposed Gmat
  std::vector<arma::mat>   P_a;        // [n_actions]: C x N_G action weight matrix
};

// pm_op_build: convert A_age once and cache all per-action index/weight data.
// imap0: C x (M_BAR+1) integer matrix, 0-based values, -1 for infeasible (from R).
// rmap0: C x (K_BAR+1) integer matrix, 0-based values, -1 for infeasible.
// k_vec, m_vec: length n_actions, 0-based action indices.
// GaT_list: List of n_actions 4x4 NumericMatrix (pre-transposed Gmat).
// P_list:   List of n_actions C x N_G NumericMatrix (CCP weights for WORK actions).
// [[Rcpp::export]]
SEXP pm_op_build(
  const arma::sp_mat&        A_age,
  const Rcpp::IntegerMatrix& imap0,
  const Rcpp::IntegerMatrix& rmap0,
  const Rcpp::IntegerVector& k_vec,
  const Rcpp::IntegerVector& m_vec,
  const Rcpp::List&          GaT_list,
  const Rcpp::List&          P_list,
  int C, int N_G
) {
  int n_actions = k_vec.size();
  MvOp* op      = new MvOp();
  op->A_age     = A_age;   // deep copy into struct; R's dgCMatrix released independently
  op->C         = C;
  op->N_G       = N_G;
  op->n_actions = n_actions;
  op->g_in .resize(n_actions);
  op->g_out.resize(n_actions);
  op->GaT  .resize(n_actions);
  op->P_a  .resize(n_actions);

  for (int a = 0; a < n_actions; a++) {
    int k = k_vec[a];   // 0-based: column k of rmap0 = remove gather for k removals
    int m = m_vec[a];   // 0-based: column m of imap0 = install gather for m installs

    op->g_in[a].resize(C);
    for (int c = 0; c < C; c++) op->g_in[a][c]  = imap0(c, m);

    op->g_out[a].resize(C);
    for (int c = 0; c < C; c++) op->g_out[a][c] = rmap0(c, k);

    op->GaT[a] = Rcpp::as<arma::mat>(GaT_list[a]);
    op->P_a[a] = Rcpp::as<arma::mat>(P_list[a]);
  }

  return Rcpp::XPtr<MvOp>(op, true);   // true = delete on GC
}

// pm_op_mv: one application of (M V): called ~300x per BiCGSTAB solve.
// x: flat C*N_G vector (column-major: G=0 in x[0..C-1], G=1 in x[C..2C-1], ...).
// Returns flat C*N_G vector with same layout.
// Zero-copy on input and output via NumericVector backing pointers.
// [[Rcpp::export]]
Rcpp::NumericVector pm_op_mv(SEXP ptr_sexp, Rcpp::NumericVector x) {
  Rcpp::XPtr<MvOp> op(ptr_sexp);
  const int C   = op->C;
  const int N_G = op->N_G;

  // Input view: no copy, column-major layout matches R's as.vector(matrix)
  const arma::mat Vmat(x.begin(), C, N_G, false, true);

  // Output: pre-allocate R vector, view as arma::mat to write in-place
  Rcpp::NumericVector ret(C * N_G);
  arma::mat result(ret.begin(), C, N_G, false, true);
  result.zeros();

  // Workspace: allocate once outside action loop to avoid 80 heap allocs per call
  arma::mat Wa(C, N_G), Y(C, N_G), Z(C, N_G), out_g(C, N_G);

  for (int a = 0; a < op->n_actions; a++) {
    const std::vector<int>& gi = op->g_in[a];
    const std::vector<int>& go = op->g_out[a];

    // W_a = Vmat * GaT[a]  (contracts over the to-state G dimension)
    Wa = Vmat * op->GaT[a];

    // Step 1: INSTALL gather (input side, imap): Y[c,:] = Wa[gi[c],:], 0 if gi[c]<0
    Y.zeros();
    for (int c = 0; c < C; c++) {
      int src = gi[c];
      if (src >= 0) Y.row(c) = Wa.row(src);
    }

    // Step 2: AGE via sparse A_age
    Z = op->A_age * Y;

    // Step 3: REMOVE gather (output side, rmap): out_g[c,:] = Z[go[c],:], 0 if go[c]<0
    out_g.zeros();
    for (int c = 0; c < C; c++) {
      int src = go[c];
      if (src >= 0) out_g.row(c) = Z.row(src);
    }

    // Accumulate: result += P_a[a] % out_g  (elementwise C x N_G)
    result += op->P_a[a] % out_g;
  }

  return ret;   // ret's backing memory holds the result; no additional copy
}

// pm_probe_arma: trivial probe used by the preflight gate in PM06.
// [[Rcpp::export]]
double pm_probe_arma(double a, double b) {
  arma::vec x = {a, b};
  return arma::sum(x);
}
