# TICKET [ID] — [Economic question being answered]
# Created: [DATE]
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
[2-3 sentences: why this estimation / simulation / test.
 What does the result tell us economically?
 What decision does it inform?]

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════
[Equations as agreed with Opus — numbered for reference below.
 Be explicit about: what is estimated, what is held fixed,
 what the identification assumption is.]

Eq. 1:
Eq. 2:
...

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════
[This is what R1 reads. Must be mechanically unambiguous.
 An R developer who does not know the economics must be able
 to implement this exactly.]

Step 1 — [Name]:
  Input:  [name: type, dims]
  Output: [name: type, dims]
  Operations:
    - [explicit step — state indexing convention here]
    - [NA handling if relevant]
    - [reference equation: Eq. N]
  assert: [postcondition — what must be true after this step]

Step 2 — [Name]:
  Input:
  Output:
  Operations:
  assert:

[continue...]

Step N — Save output:
  Save to: Output/Estimation_Results/[model]_[sample]_[YYYYMMDD].rds
  Print summary (see CLAUDE.md output rules)

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
[Anything R-specific. Examples:]
- Call sourceCpp("Code/Helpers/cpp_engine.cpp") before estimation
- Use [existing function name] for [step] — do not reimplement
- data.table key: [key columns]
- Indexing: R is 1-based; Rcpp arrays in cpp_engine.cpp are 0-based
- [Any other R-specific constraint]

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
[Mechanical checks only. Sonnet verifies these against the code and output.
 Each must be binary — pass or fail with no interpretation required.]

- [ ] Function signature: [exact expected signature]
- [ ] Output object class: [e.g. list with names: theta_hat, converged, ll, iter]
- [ ] Output dimensions: [exact]
- [ ] Output saved to: [exact path]
- [ ] No silent error catching (no tryCatch returning NULL)
- [ ] Tolerances match CLAUDE.md: [list which: tol_theta, tol_P, etc.]
- [ ] sourceCpp called before estimation: Y/N
- [ ] [Any numeric check derivable from math, e.g. "rowSums(P) == 1 within 1e-8"]
- [ ] Log written to logs/ directory

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each R1 attempt. Leave blank until first attempt.]

### Attempt 1 — [DATE]
Transcript: [ID]_transcript.txt
Result: [PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL]

Criteria:
- [ ] [criterion]: [result]

[If TRANSLATION_FAIL]:
Correction prompt for R1:
  "[paste into new R1 session]"

[If PSEUDOCODE_FAIL]:
Question for Opus:
  "[one sentence — what is underspecified]"

---
### Attempt 2 — [DATE]
[same format]
