// ==============================================================================
// structural_estimation.cpp - RCPP ACCELERATION FOR NPL/EM-NPL ESTIMATION
// ==============================================================================
// PURPOSE: High-performance implementations of computational bottlenecks
// DEPENDENCIES: RcppArmadillo
// ==============================================================================

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// ==============================================================================
// PRIORITY 1: E-STEP FACILITY LIKELIHOOD COMPUTATION
// ==============================================================================
// Replaces lines 547-590 in improved_estimator_FINAL__1_.r
// 
// Computes log P(facility observations | type k) for all facilities and types
// This is the dominant computational bottleneck in EM estimation
// ==============================================================================

// [[Rcpp::export]]
NumericMatrix e_step_cpp(const IntegerMatrix& panel_data,
                         const List& P_list_R,
                         const IntegerVector& facility_ids,
                         int K,
                         int n_facilities) {
  
  // panel_data: [n_obs x 3] with columns: facility_id, state_idx, action_idx
  // P_list_R: List of K matrices, each [n_states x 3] with action probabilities
  // Returns: log_lik_matrix [n_facilities x K]
  
  int n_obs = panel_data.nrow();
  NumericMatrix log_lik_matrix(n_facilities, K);
  
  // Convert P_list to 3D array for fast access
  int n_states = as<NumericMatrix>(P_list_R[0]).nrow();
  cube P_cube(n_states, 3, K);
  for (int k = 0; k < K; k++) {
    NumericMatrix P_k = as<NumericMatrix>(P_list_R[k]);
    for (int s = 0; s < n_states; s++) {
      for (int a = 0; a < 3; a++) {
        P_cube(s, a, k) = P_k(s, a);
      }
    }
  }
  
  // Compute log-likelihood for each facility-type pair
  // Group observations by facility for efficiency
  for (int i = 0; i < n_obs; i++) {
    int fac_id = panel_data(i, 0);
    int state_idx = panel_data(i, 1) - 1;  // R is 1-indexed
    int action_idx = panel_data(i, 2) - 1; // R is 1-indexed
    
    // Find facility index in facility_ids vector
    int fac_idx = -1;
    for (int f = 0; f < n_facilities; f++) {
      if (facility_ids[f] == fac_id) {
        fac_idx = f;
        break;
      }
    }
    
    if (fac_idx >= 0) {
      // Add log probability for this observation under each type
      for (int k = 0; k < K; k++) {
        double prob = P_cube(state_idx, action_idx, k);
        // Numerical stability: clip very small probabilities
        if (prob < 1e-300) prob = 1e-300;
        log_lik_matrix(fac_idx, k) += std::log(prob);
      }
    }
  }
  
  return log_lik_matrix;
}

// ==============================================================================
// PRIORITY 2: AGGREGATE WEIGHTED COUNTS
// ==============================================================================
// Replaces lines 528-545 in improved_estimator_FINAL__1_.r
//
// Aggregates facility observations weighted by type posterior probabilities
// Used in EM M-step to create weighted pseudo-observations
// ==============================================================================

// [[Rcpp::export]]
NumericVector aggregate_weighted_counts_cpp(const IntegerMatrix& panel_data,
                                            const NumericVector& weights,
                                            const IntegerVector& facility_ids,
                                            int n_states) {
  
  // panel_data: [n_obs x 3]: facility_id, state_idx, action_idx  
  // weights: [n_facilities] posterior probability for this type
  // Returns: counts_vec [n_states * 3]
  
  int n_obs = panel_data.nrow();
  int n_facilities = facility_ids.size();
  NumericVector counts_vec(n_states * 3, 0.0);
  
  // Create facility weight lookup
  std::map<int, double> weight_map;
  for (int f = 0; f < n_facilities; f++) {
    weight_map[facility_ids[f]] = weights[f];
  }
  
  // Accumulate weighted counts
  for (int i = 0; i < n_obs; i++) {
    int fac_id = panel_data(i, 0);
    int state_idx = panel_data(i, 1) - 1;  // R is 1-indexed
    int action_idx = panel_data(i, 2) - 1; // R is 1-indexed
    
    double w = weight_map[fac_id];
    int idx = state_idx * 3 + action_idx;
    counts_vec[idx] += w;
  }
  
  return counts_vec;
}

// ==============================================================================
// PRIORITY 3: INCLUSIVE VALUE CALCULATION
// ==============================================================================
// Replaces lines 192-209 in improved_estimator_FINAL__1_.r
//
// Computes nested logit inclusive values with numerical stability
// Critical for nested logit CCP calculation
// ==============================================================================

// [[Rcpp::export]]
NumericVector compute_inclusive_value_cpp(const NumericVector& v_m,
                                          const NumericVector& v_r,
                                          double sigma2,
                                          const LogicalVector& feasible_r,
                                          double gamma_E) {
  
  int n = v_m.size();
  NumericVector I(n);
  double gamma = 0.5772156649; // Euler's constant
  
  for (int i = 0; i < n; i++) {
    if (feasible_r[i]) {
      // Retrofit is feasible: compute log-sum-exp with numerical stability
      double v_max = std::max(v_m[i], v_r[i]);
      double exp_m = std::exp((v_m[i] - v_max) / sigma2);
      double exp_r = std::exp((v_r[i] - v_max) / sigma2);
      I[i] = v_max + sigma2 * std::log(exp_m + exp_r) + sigma2 * gamma;
    } else {
      // Only maintain feasible
      I[i] = v_m[i] + sigma2 * gamma;
    }
  }
  
  return I;
}

// ==============================================================================
// PRIORITY 4: SIMULATION PANEL GENERATION
// ==============================================================================
// Replaces lines 204-306 in improved_generator_FINAL__1_.r
//
// Simulates panel data using precomputed transition maps
// Vectorized action sampling and state transitions
// ==============================================================================

// [[Rcpp::export]]
List simulate_panel_cpp(const IntegerVector& initial_states,
                        const IntegerVector& facility_types,
                        const List& P_list_R,
                        const IntegerVector& next_state_maintain,
                        const IntegerVector& next_state_stay,     // NEW: No-aging transitions
                        const NumericVector& probs_up,            // NEW: Prob of aging per state
                        const IntegerVector& next_state_retrofit,
                        int T_periods,
                        int seed) {
  
  // Set random seed for reproducibility
  Rcpp::Environment base_env("package:base");
  Rcpp::Function set_seed = base_env["set.seed"];
  set_seed(seed);
  
  int N_facilities = initial_states.size();
  int K = P_list_R.size();
  
  // Convert P_list to 3D array
  int n_states = as<NumericMatrix>(P_list_R[0]).nrow();
  cube P_cube(n_states, 3, K);
  for (int k = 0; k < K; k++) {
    NumericMatrix P_k = as<NumericMatrix>(P_list_R[k]);
    for (int s = 0; s < n_states; s++) {
      for (int a = 0; a < 3; a++) {
        P_cube(s, a, k) = P_k(s, a);
      }
    }
  }
  
  // Initialize state tracking
  IntegerVector current_states = clone(initial_states);
  LogicalVector active_mask(N_facilities, true);
  
  // Storage for panel data (pre-allocate conservatively)
  std::vector<int> fac_ids, periods, state_idxs, actions, types;
  fac_ids.reserve(N_facilities * T_periods);
  periods.reserve(N_facilities * T_periods);
  state_idxs.reserve(N_facilities * T_periods);
  actions.reserve(N_facilities * T_periods);
  types.reserve(N_facilities * T_periods);
  
  // Simulation loop
  for (int t = 0; t < T_periods; t++) {
    // Check if any facilities still active
    bool any_active = false;
    for (int f = 0; f < N_facilities; f++) {
      if (active_mask[f]) {
        any_active = true;
        break;
      }
    }
    if (!any_active) break;
    
    // Draw actions for active facilities
    for (int f = 0; f < N_facilities; f++) {
      if (!active_mask[f]) continue;
      
      int state = current_states[f] - 1;  // Convert to 0-indexed
      int type = facility_types[f] - 1;   // Convert to 0-indexed
      
      // Get probabilities for this state-type combination
      double p_maintain = P_cube(state, 0, type);
      double p_exit = P_cube(state, 1, type);
      double p_retrofit = P_cube(state, 2, type);
      
      // Draw action (1=maintain, 2=exit, 3=retrofit)
      double u = R::runif(0, 1);
      int action;
      if (u < p_maintain) {
        action = 1;
      } else if (u < p_maintain + p_exit) {
        action = 2;
      } else {
        action = 3;
      }
      
      // Store observation
      fac_ids.push_back(f + 1);  // Convert back to 1-indexed for R
      periods.push_back(t + 1);
      state_idxs.push_back(current_states[f]);
      actions.push_back(action);
      types.push_back(facility_types[f]);
      
      // Update state based on action
      if (action == 2) {
        // Exit: mark as inactive
        active_mask[f] = false;
      } else if (action == 1) {
        // Maintain: STOCHASTIC AGING
        // Facilities either age up (A→A+1) or stay (A→A) based on probability
        int current_idx_0 = current_states[f] - 1;  // Convert to 0-indexed
        double p_up = probs_up[current_idx_0];
        
        if (R::runif(0, 1) < p_up) {
          // Aging occurs: transition to next age state
          current_states[f] = next_state_maintain[current_idx_0];
        } else {
          // No aging: stay in current state
          current_states[f] = next_state_stay[current_idx_0];
        }
      } else if (action == 3) {
        // Retrofit: transition to age 1, double wall (regime preserved)
        current_states[f] = next_state_retrofit[current_states[f] - 1];
      }
    }
  }
  
  // Convert vectors to R objects
  return List::create(
    Named("facility_id") = IntegerVector(fac_ids.begin(), fac_ids.end()),
    Named("period") = IntegerVector(periods.begin(), periods.end()),
    Named("state_idx") = IntegerVector(state_idxs.begin(), state_idxs.end()),
    Named("action") = IntegerVector(actions.begin(), actions.end()),
    Named("true_type") = IntegerVector(types.begin(), types.end())
  );
}

// ==============================================================================
// UTILITY: LOG-SUM-EXP WITH NUMERICAL STABILITY
// ==============================================================================
// Helper function for various likelihood computations
// ==============================================================================

// [[Rcpp::export]]
double logSumExp_cpp(const NumericVector& x) {
  double max_val = max(x);
  
  // Clip extreme values to prevent overflow
  NumericVector x_clipped = pmax(pmin(x, 700.0), -700.0);
  
  double sum_exp = 0.0;
  for (int i = 0; i < x_clipped.size(); i++) {
    sum_exp += std::exp(x_clipped[i] - max_val);
  }
  
  return max_val + std::log(sum_exp);
}

// ==============================================================================
// UTILITY: MATRIX SPARSE MULTIPLICATION CHECK
// ==============================================================================
// Checks sparsity and recommends optimal multiplication strategy
// ==============================================================================

// [[Rcpp::export]]
List check_matrix_sparsity(const NumericMatrix& M) {
  int n_elements = M.nrow() * M.ncol();
  int n_nonzero = 0;
  
  for (int i = 0; i < M.nrow(); i++) {
    for (int j = 0; j < M.ncol(); j++) {
      if (std::abs(M(i,j)) > 1e-15) {
        n_nonzero++;
      }
    }
  }
  
  double sparsity = 1.0 - (double)n_nonzero / (double)n_elements;
  bool use_sparse = sparsity > 0.3;
  
  return List::create(
    Named("n_elements") = n_elements,
    Named("n_nonzero") = n_nonzero,
    Named("sparsity") = sparsity,
    Named("use_sparse") = use_sparse
  );
}