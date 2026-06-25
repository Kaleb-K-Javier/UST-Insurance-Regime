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

// =====================================================================
// BATCHED MATVEC (Ticket 024p Lever A)
// Groups actions by install count m.  For each m-group computes
//   Zbase_m = A_age * InstallGather_m(Vmat)   <- ONE sparse product
// then per action in the group:
//   Wa = Zbase_m * GaT_a                       <- cheap dense 4x4
//   result += P_a % RemoveGather_k(Wa)
// Reduces expensive sparse products from n_actions to n_distinct_m (<=5).
// Old pm_op_build / pm_op_mv are left intact as the within-file oracle.
// =====================================================================
#include <map>

struct ActionB {
  std::vector<int> g_out;   // [C]: 0-based remove gather, -1 = infeasible
  arma::mat        GaT;     // N_G x N_G (pre-transposed Gmat)
  arma::mat        P_a;     // C x N_G  (CCP weight matrix)
};

struct MGroupB {
  std::vector<int>     g_in;    // [C]: 0-based install gather, -1 = infeasible
  std::vector<ActionB> actions; // all actions sharing this m value
};

struct MvOpB {
  arma::sp_mat         A_age;
  int                  C;
  int                  N_G;
  std::vector<MGroupB> groups;  // one entry per distinct install count m
};

// pm_op_build_b: same interface as pm_op_build; groups actions by m before storing.
// [[Rcpp::export]]
SEXP pm_op_build_b(
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
  MvOpB* op = new MvOpB();
  op->A_age = A_age;
  op->C     = C;
  op->N_G   = N_G;

  std::map<int, int> m_to_grp;  // install count m (0-based) -> index in op->groups

  for (int a = 0; a < n_actions; a++) {
    int m = m_vec[a];
    int k = k_vec[a];

    if (m_to_grp.find(m) == m_to_grp.end()) {
      int grp_idx = (int)op->groups.size();
      m_to_grp[m] = grp_idx;
      MGroupB grp;
      grp.g_in.resize(C);
      for (int c = 0; c < C; c++) grp.g_in[c] = imap0(c, m);
      op->groups.push_back(std::move(grp));
    }
    int grp_idx = m_to_grp.at(m);

    ActionB act;
    act.g_out.resize(C);
    for (int c = 0; c < C; c++) act.g_out[c] = rmap0(c, k);
    act.GaT = Rcpp::as<arma::mat>(GaT_list[a]);
    act.P_a = Rcpp::as<arma::mat>(P_list[a]);
    op->groups[grp_idx].actions.push_back(std::move(act));
  }

  return Rcpp::XPtr<MvOpB>(op, true);
}

// pm_op_mv_b: batched matvec — see algebra above.
// Same calling convention as pm_op_mv: input/output are flat C*N_G vectors.
// [[Rcpp::export]]
Rcpp::NumericVector pm_op_mv_b(SEXP ptr_sexp, Rcpp::NumericVector x) {
  Rcpp::XPtr<MvOpB> op(ptr_sexp);
  const int C   = op->C;
  const int N_G = op->N_G;

  const arma::mat Vmat(x.begin(), C, N_G, false, true);

  Rcpp::NumericVector ret(C * N_G);
  arma::mat result(ret.begin(), C, N_G, false, true);
  result.zeros();

  // Workspace: allocated once per pm_op_mv_b call, reused across all groups/actions
  arma::mat Ym(C, N_G), Zbase(C, N_G), Wa(C, N_G), out_g(C, N_G);

  for (const MGroupB& grp : op->groups) {
    // Install gather: Ym[c,:] = Vmat[g_in[c],:]; row zeroed when g_in[c] < 0
    Ym.zeros();
    for (int c = 0; c < C; c++) {
      int src = grp.g_in[c];
      if (src >= 0) Ym.row(c) = Vmat.row(src);
    }

    // ONE sparse product for all actions in this m-group
    Zbase = op->A_age * Ym;

    for (const ActionB& act : grp.actions) {
      // Dense C x N_G matmul with pre-transposed 4x4 Gmat
      Wa = Zbase * act.GaT;

      // Remove gather: out_g[c,:] = Wa[g_out[c],:]; row zeroed when g_out[c] < 0
      out_g.zeros();
      for (int c = 0; c < C; c++) {
        int src = act.g_out[c];
        if (src >= 0) out_g.row(c) = Wa.row(src);
      }

      result += act.P_a % out_g;
    }
  }

  return ret;
}
