



# am_gauss_code_base.txt explainer
# GAUSS CODEBASE SUMMARY: NPL ESTIMATOR & DYNAMIC GAMES

## 1. CODEBASE PURPOSE
This combined file contains a GAUSS-based econometric toolkit for estimating dynamic discrete choice models. It focuses on the Nested Pseudo Likelihood (NPL) estimator. The code covers:
1.  [cite_start]**Single-Agent Models:** Specifically, replicating John Rust's bus engine replacement model (1987)[cite: 13].
2.  [cite_start]**Dynamic Games:** A Monte Carlo experiment for a market entry/exit game, replicating logic from Pesendorfer and Schmidt-Dengler (2008)[cite: 11, 44].

## 2. FILE STRUCTURE (IN COMBINED BLOCK)
The code block above is delimited by `/* === FILE: filename === */` headers.

### A. Execution Scripts (Entry Points)
* `npl_sing.e`: The main execution script for the Rust bus model. [cite_start]It handles data loading, discretization, and calls the NPL estimator (`npl_sing`)[cite: 13].
* `mcarlo_psd_261207.prg`: The main execution script for the Dynamic Games simulation. [cite_start]It is self-contained and includes procedures for equilibrium mapping (`equilmap`), data simulation (`simdygam`), and NPL iterations (`npldygam`)[cite: 44, 66].

### B. Core Procedures (Functions)
* `npl_sing.src`: Implements the NPL algorithm loop. [cite_start]It performs policy iteration by solving linear systems (using `crout` decomposition) and updating structural parameters via Conditional Logit[cite: 258, 276].
* `clogit.src`: A Maximum Likelihood estimator for McFadden's Conditional Logit model. [cite_start]It uses Newton's method with analytical gradients/Hessians[cite: 386].
* [cite_start]`multilog.src`: A Maximum Likelihood estimator for Multinomial Logit models[cite: 187].
* [cite_start]`nadaraya_cv.src`: Implements Nadaraya-Watson kernel regression with cross-validation for bandwidth selection (used for transition probability estimation)[cite: 222].
* [cite_start]`discthre.src`: A utility function to discretize continuous variables based on a vector of thresholds[cite: 247].

## 3. KEY ALGORITHMIC LOGIC
* **The NPL Loop:** The core logic in `npl_sing.src` iterates between updating the "value" of choices (policy evaluation) and updating the structural parameters (Pseudo-ML). [cite_start]It continues until the parameter estimates converge[cite: 270].
* **Linear Algebra:** The code relies heavily on GAUSS matrix operations. Note that the operator `/` in contexts like `numer./denom` implies element-wise division, but `inv(A)*B` or `A\B` logic is used for systems. [cite_start]The code explicitly uses `crout` and `qrsol` for solving linear systems `Ax = B`[cite: 277].
* [cite_start]**Discretization:** The bus model discretizes the continuous `accumulated mileage` state variable into 201 bins using `discthre`[cite: 21, 29].

## 4. DATA DEPENDENCIES
* The code references `bus1234.dat` (binary data) and `bus1234.dht` (header). [cite_start]These files are not included in the text block but are required for `npl_sing.e` to execute[cite: 23].

# em_NPL_code_base.txt explainer
# Project Codebase Summary: Dynamic Discrete Choice Estimation with Unobserved Heterogeneity

## 1. Project Overview
**Objective:** This codebase implements a structural dynamic discrete choice model to estimate firm entry/exit decisions. It handles observed and unobserved state variables using Conditional Choice Probabilities (CCPs) and the Expectation-Maximization (EM) algorithm.

**Econometric Context:**
* **Model:** Dynamic discrete choice (Hotz-Miller style CCP estimation).
* **Unobservables:** The code handles two types of unobserved heterogeneity:
    * **Permanent:** Modeled via EM algorithm (`PState` missing).
    * **Transitory (Markov):** Modeled via Hamilton Filter/Forward-Backward algorithm (`State` missing).

## 2. Key Variable Dictionary
* `N`: Number of cross-sectional observations (firms/markets).
* `T`: Number of time periods.
* `S`: Number of unobserved states.
* `Firm1`: N x T matrix (binary). 1 if firm is incumbent/enters, 0 if out.
* `LFirm`: N x T matrix. Lagged `Firm1` (state variable).
* `State`: Time-varying exogenous state of the economy (observed in Prob 1/2, unobserved in Prob 3/4).
* `PState`: Permanent state of the economy (observed in Prob 1/3, unobserved in Prob 2).
* `Y`: Continuous price variable dependent on states.
* `b`: Vector of structural parameters for the profit function.
* `b2`: Vector of parameters for the pricing equation (OLS).
* `FV`: Future Value terms derived from CCPs.
* `PType`: Posterior probability of an observation being a specific unobserved type.
* `xi / xi2`: Conditional Choice Probabilities (CCPs).

## 3. File Directory & Logic

### A. Data Generation
* **`assign3data1.m`**: Generates synthetic data for the assignment.
    * **Logic:** Simulates firm trajectories based on defined profit parameters, transition matrices (`p2`), and entry costs. Saves `dataassign32.mat`.

### B. Estimation Scripts (Main Drivers)
These scripts correspond to the specific tasks in the assignment PDF.
* **`assign3prob1.m`**: Benchmark estimation. `State` and `PState` are **observed**. Uses simple CCP estimation.
* **`assign3prob2.m`**: Estimation with **Missing Permanent State** (`PState`).
    * **Logic:** Uses an iterative EM algorithm. Calls `typeprob0806p` to update type probabilities (`PType`) and `fminunc` to maximize likelihood weighted by `PType`.
* **`assign3prob3.m`**: Estimation with **Missing Transitory State** (`State`).
    * **Logic:** Uses a Hamilton Filter (Hidden Markov Model). Calls `typeprob0806` to update posterior probabilities and transition matrices for the unobserved state.
* **`assign3prob4.m`**: Two-step estimation with **Missing Transitory State**.
    * **Logic:** First estimates the measurement model (pricing) and heterogeneity distribution, then estimates profit parameters.

### C. Likelihood & Optimization Functions
* **`logitCCP.m`**: Standard log-likelihood for the dynamic choice model given Future Values (`FV`).
* **`logitCCPu.m`**: Weighted log-likelihood using `PType` (posterior probabilities) for the EM algorithm steps.
* **`logitCCPucalc.m`**: Calculates the joint likelihood of the discrete choice (`Firm2`) and the continuous outcome (`Y2` - Price), assuming a normal error for prices. Used in the E-step.
* **`likeCCP2stage.m`**: Calculates likelihood contributions for the two-stage estimator in Problem 4.

### D. EM Algorithm & Filtering (The "E-Step")
* **`typeprob0806p.m`**: (For Problem 2) Performs the E-step for **Permanent** unobserved heterogeneity. Updates prior probabilities (`prior`) and calculates `PType` based on likelihoods.
* **`typeprob0806.m`**: (For Problem 3/4) Performs the E-step for **Time-Varying (Markov)** unobserved heterogeneity. Implements the forward-backward algorithm (Hamilton, 1990) to update transition matrices (`trans2`) and `PType`.

### E. CCP Updating & Utilities
* **`updateCCPu.m`**: Updates Future Value (`FV`) terms using data-based frequency counts weighted by `PType`.
* **`updateCCPu2.m`**: Updates Future Value (`FV`) terms using the structural model's predicted probabilities (fixed point approach).
* **`updateCCPu2stage.m`**: Specialized CCP update for the two-stage estimation in Problem 4.
* **`prob3.m`**: Calculates equilibrium choice probabilities by solving the fixed point of the value function iteration.
* **`wols.m`**: Weighted Ordinary Least Squares. Estimates pricing parameters `b2` and variance `var` using weights `PType`.

## 4. Algorithmic Flow (Typical Loop)
In Problems 2, 3, and 4, the code follows this general structure:
1.  **Initialize:** Guess parameters `b`, `b2`, priors, and transitions `p`.
2.  **E-Step:** Calculate the probability of being a specific type (`PType`) given current parameters using `typeprob0806` (transitory) or `typeprob0806p` (permanent).
3.  **Update CCPs:** Re-calculate Future Values (`FV`) based on new `PType` using `updateCCPu` (data) or `updateCCPu2` (model).
4.  **M-Step (Price):** Update pricing parameters (`b2`) using Weighted OLS (`wols`).
5.  **M-Step (Choice):** Update profit parameters (`b`) by maximizing the weighted likelihood `logitCCPu`.
6.  **Convergence:** Repeat 2-5 until `PType` or parameters stabilize.