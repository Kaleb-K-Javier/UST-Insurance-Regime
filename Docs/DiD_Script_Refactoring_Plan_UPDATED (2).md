# UPDATED DiD Script Refactoring Plan
## Accounting for Panel Builder Capabilities

**Date:** January 28, 2026  
**Status:** Revised after reviewing `10_Build_Master_Annual_Panel_Refactored.R`

---

## Executive Summary - REVISED

After reviewing the panel creation script, **many critical data preparation steps are already handled upstream**. This dramatically simplifies the refactoring task.

### What's Already Done in Panel Builder ✓

| Issue | Status | Location in Panel Builder |
|-------|--------|---------------------------|
| **Missing Data Exclusion** | ✓ DONE | Lines 109-134: Strict filtering rules |
| **Incumbent Sample Definition** | ✓ DONE | Line 451: `is_incumbent` flag |
| **Sample Time Windows** | ✓ DONE | Lines 99-104: Panel bounded 1970-2025 |
| **Texas ID Standardization** | ✓ DONE | Lines 60-82: ID cleaning with zero-stripping |

### What Still Needs to Be Done in 02_DiD_Results.R

| Task | Priority | Reason |
|------|----------|--------|
| **Replace Parts A-H with Models 1-5** | HIGH | Wrong specification structure |
| **Create tank-level datasets** | HIGH | Panel builder only creates facility-year |
| **Implement competing risks** | HIGH | Models 5A/5B require new methodology |
| **Add Model-specific fixed effects** | MEDIUM | Current FE structure doesn't match memo |
| **Event studies for Models 1A, 3A** | MEDIUM | Need memo-specific specifications |
| **Robustness checks** | LOW | Definition B, leak windows, etc. |

---

## PART 1: What the Panel Builder Already Provides

### 1.1 Missing Data Handling ✓ COMPLETE

**Panel Builder Lines 109-134:**
```r
# Rule 1: Drop facility if ANY tank is 'Closed' but missing closure date
problem_closed <- master_tanks[
  tolower(tank_status) == "closed" & is.na(tank_closed_date),
  .(panel_id)
]

# Rule 2: Drop facility if ANY tank is 'Open' but missing install date
problem_open <- master_tanks[
  tolower(tank_status) == "open" & is.na(tank_installed_date),
  .(panel_id)
]
```

**This matches Memo Section 1.3 requirements exactly!** No additional work needed in 02 script.

---

### 1.2 Incumbent Sample Definition ✓ COMPLETE

**Panel Builder Line 451:**
```r
annual[, is_incumbent := as.integer(first_year < 1999)]
```

**Usage in 02 Script:**
```r
# Current line 116 already does this correctly:
annual_data <- annual_data[cohort == "Incumbent"]

# Just verify the column name matches:
annual_data <- annual_data[is_incumbent == 1]
```

**This is already correct!** The `is_incumbent` flag identifies facilities active before 1999.

---

### 1.3 Sample Time Boundaries ✓ COMPLETE

**Panel Builder Lines 99-104:**
```r
PANEL_START <- as.IDate("1970-01-01") 
PANEL_END   <- as.IDate("2025-12-31")
```

**Usage in 02 Script:** Just need to align the year filter:
```r
# Current line 106 uses 1980-2025
annual_data <- annual_data[panel_year >= 1980 & panel_year <= 2025]

# For pre-trend analysis, use 1990-2015:
annual_data <- annual_data[panel_year >= 1990 & panel_year <= 2015]
```

---

### 1.4 Treatment Variables ✓ MOSTLY DONE

**Panel Builder Lines 443-450:**
```r
annual[, `:=`(
  post_1999 = as.integer(panel_year >= 1999),
  texas_treated = as.integer(state == "TX"),
  is_incumbent = as.integer(first_year < 1999)
)]
```

**These are perfect!** The 02 script just needs to use them consistently.

---

## PART 2: What Still Needs to Be Done

### 2.1 Create Tank-Level Datasets (NEW - CRITICAL)

**Problem:** Panel builder only creates **facility-year** aggregated data. Models 2, 4, 5A, 5B require **tank-level** data.

**Solution:** Create a separate data preparation step in 02 script:

```r
#==============================================================================
# LOAD TANK-LEVEL DATA (For Models 2, 4, 5)
#==============================================================================
# Note: Panel builder aggregates to facility-year. We need tank-level inventory.

cat("\n--- Loading Tank-Level Inventory ---\n")

# Load Master Tanks (same source as panel builder uses)
tank_inventory <- fread(here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"))

# Apply SAME filters as panel builder
tank_inventory[, `:=`(
  facility_id = toupper(trimws(as.character(facility_id))),
  tank_id     = toupper(trimws(as.character(tank_id))),
  state       = toupper(trimws(as.character(state)))
)]
tank_inventory[, panel_id := paste(facility_id, state, sep = "_")]

# Apply incumbent filter (match facility-year panel)
incumbent_facilities <- unique(annual_data$panel_id)
tank_inventory <- tank_inventory[panel_id %in% incumbent_facilities]

# Create survival variables (for Cox models)
TREATMENT_DATE <- as.Date("1999-01-01")

tank_inventory[, `:=`(
  # Age at treatment
  age_at_treatment = as.numeric(difftime(
    TREATMENT_DATE, 
    tank_installed_date, 
    units = "days"
  )) / 365.25,
  
  # Time to closure (years from treatment)
  time_to_close = as.numeric(difftime(
    pmin(tank_closed_date, as.Date("2015-12-31"), na.rm = TRUE),
    TREATMENT_DATE,
    units = "days"
  )) / 365.25,
  
  # Event indicator
  event_closure = as.integer(!is.na(tank_closed_date) & 
                            tank_closed_date <= as.Date("2015-12-31")),
  
  # Risk factors
  texas = as.integer(state == "TX"),
  single_walled = as.integer(tolower(wall_type) == "single"),
  old_at_treatment = as.integer(age_at_treatment > 20),
  
  # Vintage cohorts
  vintage = fcase(
    tank_installed_date < as.Date("1980-01-01"), "Pre-1980",
    tank_installed_date < as.Date("1988-12-23"), "1980-1988",
    default = "1989-1998"
  )
)]

cat(sprintf("Tank inventory prepared: %s tanks\n", 
  format(nrow(tank_inventory), big.mark = ",")))
```

---

### 2.2 Create Closed-Tanks Dataset (For Models 3, 4)

```r
#==============================================================================
# CREATE CLOSED-TANKS-ONLY DATASET (Models 3A, 3B, 4)
#==============================================================================

closed_tanks <- tank_inventory[event_closure == 1]

# Add closure year for fixed effects
closed_tanks[, closure_year := as.integer(format(tank_closed_date, "%Y"))]

# Age at closure
closed_tanks[, age_at_closure := as.numeric(difftime(
  tank_closed_date, tank_installed_date, units = "days"
)) / 365.25]

# Post-treatment indicator
closed_tanks[, post := as.integer(closure_year >= 1999)]
closed_tanks[, texas_post := as.integer(texas == 1 & post == 1)]

cat(sprintf("Closed tanks dataset: %s tanks\n", 
  format(nrow(closed_tanks), big.mark = ",")))
```

---

### 2.3 Pre-1999 Exit Balance Test (NEW DIAGNOSTIC)

**This is the ONLY new diagnostic needed**, since missing data is already handled.

```r
#==============================================================================
# DIAGNOSTIC: Pre-1999 Exit Balance Test (Memo Section 1.1)
#==============================================================================

cat("\n--- Pre-1999 Exit Balance Test ---\n")

# Use facility-year panel
# Identify facilities that exited before 1999
facility_exit_status <- annual_data[, .(
  last_year = max(panel_year),
  first_year = min(panel_year),
  texas = first(texas_treated)
), by = panel_id]

facility_exit_status[, exited_pre_1999 := as.integer(last_year < 1999)]

# Regression: Did Texas facilities exit at different rates?
pre_exit_test <- feols(
  exited_pre_1999 ~ texas,
  data = facility_exit_status,
  cluster = "texas"  # Only 2 clusters, so inference is limited
)

cat("\nH0: Texas and Control had same exit rates before 1999\n")
print(summary(pre_exit_test))

# Interpretation
if (pre_exit_test$coefficients["texas"] > 0 & pre_exit_test$pvalue["texas"] < 0.05) {
  warning("Texas had HIGHER pre-1999 exit rate. Survivorship bias concern.")
} else if (pre_exit_test$coefficients["texas"] < 0 & pre_exit_test$pvalue["texas"] < 0.05) {
  warning("Texas had LOWER pre-1999 exit rate. Survivorship bias concern.")
} else {
  cat("No significant difference in pre-1999 exit rates (p > 0.05)\n")
}
```

---

## PART 3: Model Specifications (Simplified)

Since data prep is done upstream, we just need the right **model calls**:

### 3.1 Model 1A (Use Existing Panel)

```r
#==============================================================================
# MODEL 1A: Facility-Level Closure Probability
#==============================================================================
# Data: facility-year panel (already created by panel builder)
# Outcome: Any_Closure = 1 if facility closed ANY tank in year t

cat("\n====================================================================\n")
cat("MODEL 1A: Facility-Level Annual Closure Probability\n")
cat("====================================================================\n\n")

# Create binary outcome
annual_data[, Any_Closure := as.integer(n_closures > 0)]

# Create interaction term
annual_data[, texas_post := texas_treated * post_1999]

# Estimate
model_1a <- feols(
  Any_Closure ~ texas_post | panel_id + panel_year,
  data = annual_data,
  cluster = "state",
  lean = FALSE
)

cat("--- Model 1A Results ---\n")
print(summary(model_1a))

# Save
save_standard_did_table(
  models = list(model_1a),
  headers = c("Any Closure"),
  base_name = "Model_1A_Facility_Closure_Probability",
  title = "Model 1A: Treatment Effect on Annual Closure Probability",
  treatment_var = "texas_post",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP,
  digits = 6
)
```

**Note:** The outcome variable already exists in the panel! `n_closures` is created in panel builder Section 5.

---

### 3.2 Model 1A-ES (Event Study)

```r
#==============================================================================
# MODEL 1A-ES: Event Study
#==============================================================================

# Create relative year
annual_data[, rel_year := panel_year - 1999]

# Create event dummies (omit -1 as reference)
rel_years <- -10:10
for (k in rel_years) {
  if (k == -1) next  # Reference period
  
  varname <- ifelse(k < 0, 
                   sprintf("rel_m%d", abs(k)),
                   ifelse(k == 0, "rel_0", sprintf("rel_p%d", k)))
  
  annual_data[, (varname) := as.integer(texas_treated == 1 & rel_year == k)]
}

# Bin endpoints
annual_data[, rel_m10plus := as.integer(texas_treated == 1 & rel_year <= -10)]
annual_data[, rel_p10plus := as.integer(texas_treated == 1 & rel_year >= 10)]

# Formula
es_formula <- as.formula(paste(
  "Any_Closure ~",
  paste(c("rel_m10plus", paste0("rel_m", 9:2), "rel_0", 
          paste0("rel_p", 1:9), "rel_p10plus"), collapse = " + "),
  "| panel_id + panel_year"
))

# Estimate
model_1a_es <- feols(es_formula, data = annual_data, cluster = "state")

# Extract coefficients
es_coefs <- broom::tidy(model_1a_es, conf.int = TRUE) %>%
  filter(grepl("rel_", term)) %>%
  mutate(
    rel_year = case_when(
      term == "rel_m10plus" ~ -10,
      grepl("rel_m", term) ~ -as.numeric(gsub("rel_m", "", term)),
      term == "rel_0" ~ 0,
      grepl("rel_p", term) ~ as.numeric(gsub("rel_p|plus", "", term)),
      TRUE ~ NA_real_
    )
  ) %>%
  bind_rows(data.frame(term = "rel_m1", estimate = 0, std.error = 0,
                      conf.low = 0, conf.high = 0, rel_year = -1))

# F-test for pre-trends
pre_vars <- c("rel_m10plus", paste0("rel_m", 9:2))
pre_test <- wald(model_1a_es, keep = pre_vars, cluster = "state")

cat("\n--- Parallel Trends F-Test ---\n")
cat(sprintf("F-statistic: %.3f\n", pre_test$stat))
cat(sprintf("P-value: %.4f\n", pre_test$p.value))

# Plot
p_es <- ggplot(es_coefs, aes(x = rel_year, y = estimate)) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "#0072B2") +
  geom_line(color = "#0072B2", linewidth = 1) +
  geom_point(color = "#0072B2", size = 2) +
  labs(
    title = "Model 1A Event Study: Any Closure",
    subtitle = sprintf("Parallel trends F-test: p = %.3f", pre_test$p.value),
    x = "Years Relative to Treatment (1999)",
    y = "Coefficient (pp)",
    caption = "Reference: 1998. 95% CI shown."
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Model_1A_EventStudy.png"),
       p_es, width = 10, height = 6, dpi = 300, bg = "white")
```

---

### 3.3 Model 2: Tank-Level Closure with HTE

```r
#==============================================================================
# MODEL 2: Tank-Level Closure with Risk Interactions
#==============================================================================

cat("\n====================================================================\n")
cat("MODEL 2: Tank-Level Closure with Risk Heterogeneity\n")
cat("====================================================================\n\n")

# Model 2A: Age interaction
model_2a <- coxph(
  Surv(time_to_close, event_closure) ~ texas * old_at_treatment + 
    strata(county_fips),
  data = tank_inventory,
  cluster = state
)

cat("--- Model 2A: Age × Treatment ---\n")
print(summary(model_2a))

# Model 2B: Wall type interaction
model_2b <- coxph(
  Surv(time_to_close, event_closure) ~ texas * single_walled + 
    strata(county_fips),
  data = tank_inventory,
  cluster = state
)

cat("\n--- Model 2B: Wall Type × Treatment ---\n")
print(summary(model_2b))

# Model 2C: Vintage interactions
model_2c <- coxph(
  Surv(time_to_close, event_closure) ~ texas * vintage + 
    strata(county_fips),
  data = tank_inventory,
  cluster = state
)

cat("\n--- Model 2C: Vintage × Treatment ---\n")
print(summary(model_2c))

# Save all
save_cox_results(model_2a, "Model_2A_Tank_Closure_Age_HTE", 
                "Model 2A: Tank Closure - Age Heterogeneity")
save_cox_results(model_2b, "Model_2B_Tank_Closure_Wall_HTE",
                "Model 2B: Tank Closure - Wall Type Heterogeneity")
save_cox_results(model_2c, "Model_2C_Tank_Closure_Vintage_HTE",
                "Model 2C: Tank Closure - Vintage Heterogeneity")
```

---

### 3.4 Model 3A: Age at Closure (County FE)

```r
#==============================================================================
# MODEL 3A: Age at Closure (County FE)
#==============================================================================

cat("\n====================================================================\n")
cat("MODEL 3A: Age at Closure (Between-County Variation)\n")
cat("====================================================================\n\n")

# Baseline
model_3a_base <- feols(
  age_at_closure ~ texas_post | county_fips + closure_year,
  data = closed_tanks,
  cluster = "state"
)

# With controls
model_3a_controls <- feols(
  age_at_closure ~ texas_post + single_walled + log(capacity + 1) |
    county_fips + closure_year,
  data = closed_tanks,
  cluster = "state"
)

cat("--- Model 3A (Baseline) ---\n")
print(summary(model_3a_base))

cat("\n--- Model 3A (With Controls) ---\n")
print(summary(model_3a_controls))

# Interpretation
beta <- coef(model_3a_base)["texas_post"]
if (beta < 0) {
  cat(sprintf("\nTreatment reduced closure age by %.2f years\n", -beta))
  cat("→ Marginal facilities exited, closing younger tanks\n")
} else {
  cat(sprintf("\nTreatment increased closure age by %.2f years\n", beta))
  cat("→ Selective closure of older, high-risk tanks\n")
}

# Save
save_standard_did_table(
  models = list(model_3a_base, model_3a_controls),
  headers = c("Baseline", "With Controls"),
  base_name = "Model_3A_Age_at_Closure_County_FE",
  title = "Model 3A: Age at Closure (County FE)",
  treatment_var = "texas_post",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)
```

---

### 3.5 Model 3B: Age at Closure (Facility FE)

```r
#==============================================================================
# MODEL 3B: Age at Closure (Facility FE)
#==============================================================================

cat("\n====================================================================\n")
cat("MODEL 3B: Age at Closure (Within-Facility Variation)\n")
cat("====================================================================\n\n")

# CRITICAL: Restrict to facilities with closures in BOTH periods
closure_counts <- closed_tanks[, .(
  n_pre = sum(closure_year < 1999),
  n_post = sum(closure_year >= 1999)
), by = panel_id]

spanning_facilities <- closure_counts[n_pre >= 1 & n_post >= 1, panel_id]

model_3b_data <- closed_tanks[panel_id %in% spanning_facilities]

cat(sprintf("Facilities spanning pre/post: %s (%.1f%% of facilities)\n",
  format(length(spanning_facilities), big.mark = ","),
  100 * length(spanning_facilities) / uniqueN(closed_tanks$panel_id)))

cat(sprintf("Closure events: %s (%.1f%% of all closures)\n",
  format(nrow(model_3b_data), big.mark = ","),
  100 * nrow(model_3b_data) / nrow(closed_tanks)))

# Estimate (Note: Texas absorbed by facility FE)
model_3b <- feols(
  age_at_closure ~ post | panel_id + closure_year,
  data = model_3b_data,
  cluster = "state"
)

cat("\n--- Model 3B Results ---\n")
print(summary(model_3b))

cat("\nNote: Coefficient is β_Post (within Texas facilities only)\n")
cat("Texas treatment effect is absorbed by facility fixed effects\n")

# Save
save_standard_did_table(
  models = list(model_3b),
  headers = c("Within-Facility"),
  base_name = "Model_3B_Age_at_Closure_Facility_FE",
  title = "Model 3B: Age at Closure (Facility FE, Spanning Sample)",
  treatment_var = "post",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)
```

---

### 3.6 Models 4, 5A, 5B (Leak Mechanisms)

**These follow the same pattern as original refactoring plan.** Since they use the tank-level and closed-tanks datasets we created in Section 2, the code is identical to the original plan (Part 3.7-3.10).

---

## PART 4: Descriptive Tables (Use Panel Builder Output)

The panel builder already creates most of what we need! We just need to format it.

### 4.1 Table 1: Sample Description

```r
#==============================================================================
# TABLE 1: Sample Description
#==============================================================================

# The panel already has n_closures, active_tanks_dec, etc.
# Just aggregate to Texas vs Control

sample_desc <- annual_data[panel_year == 1999, .(
  Facilities = .N,
  Tanks = sum(active_tanks_dec, na.rm = TRUE),
  Single_Tank_Facilities = sum(active_tanks_dec == 1, na.rm = TRUE),
  Median_Tanks = median(active_tanks_dec, na.rm = TRUE),
  Mean_Tanks = mean(active_tanks_dec, na.rm = TRUE)
), by = .(State = ifelse(texas_treated == 1, "Texas", "Control"))]

# Add totals
totals <- annual_data[panel_year == 1999, .(
  State = "Total",
  Facilities = .N,
  Tanks = sum(active_tanks_dec, na.rm = TRUE),
  Single_Tank_Facilities = sum(active_tanks_dec == 1, na.rm = TRUE),
  Median_Tanks = median(active_tanks_dec, na.rm = TRUE),
  Mean_Tanks = mean(active_tanks_dec, na.rm = TRUE)
)]

table1 <- rbind(sample_desc, totals)

# Add percentages
table1[State != "Total", `:=`(
  Pct_Facilities = 100 * Facilities / sum(Facilities),
  Pct_Tanks = 100 * Tanks / sum(Tanks),
  Pct_Single = 100 * Single_Tank_Facilities / Facilities
)]

write.csv(table1, 
  file.path(OUTPUT_TABLES, "Table_1_Sample_Description.csv"),
  row.names = FALSE)

cat("Saved: Table_1_Sample_Description.csv\n")
```

---

## PART 5: Simplified Checklist

### Data Preparation (Already Done by Panel Builder)
- [x] Missing data exclusion (Lines 109-134 of panel builder)
- [x] Incumbent sample definition (Line 451 of panel builder)
- [x] Treatment variables created (Lines 443-450)
- [ ] Pre-1999 exit balance test (New diagnostic in 02 script)

### New Data Structures Needed in 02 Script
- [ ] Tank-level inventory (from Master_Harmonized_UST_Tanks.csv)
- [ ] Closed tanks only (subset of tank inventory)
- [ ] Facility-level survival (for Model 1B)

### Models to Estimate
- [ ] Model 1A + 1A-ES (facility-year panel)
- [ ] Model 1B (facility-level survival)
- [ ] Model 2A, 2B, 2C (tank-level with HTE)
- [ ] Model 3A + 3-ES (closed tanks, county FE)
- [ ] Model 3B (closed tanks, facility FE)
- [ ] Model 4 (revealed leaks)
- [ ] Model 5A, 5B (operational leaks, competing risks)

### Descriptive Analysis
- [ ] Table 1 (sample description)
- [ ] Table 2 (baseline characteristics)
- [ ] Table 3 (risk validation)
- [ ] Figure 4 (pre-trends)

### Robustness
- [ ] Leak window sensitivity
- [ ] County FE vs Facility FE comparison

---

## PART 6: Key Simplifications

**What we DON'T need to do anymore:**

1. ~~Implement missing data exclusion~~ → Already in panel builder
2. ~~Create incumbent filter~~ → Already in panel builder
3. ~~Calculate treatment variables~~ → Already in panel builder
4. ~~Handle Texas ID standardization~~ → Already in panel builder
5. ~~Build facility-year panel~~ → Already in panel builder

**What we DO need to do:**

1. Load tank-level data (use same source as panel builder)
2. Replace Parts A-H with Models 1-5
3. Add competing risk models (5A, 5B)
4. Add event studies with proper specifications
5. Format descriptive tables

---

## PART 7: Execution Strategy

**Phase 1: Load Tank-Level Data** (30 min)
- Load `Master_Harmonized_UST_Tanks.csv`
- Apply incumbent filter
- Create survival variables
- Test on 1% sample

**Phase 2: Estimate Core Models** (2-3 hours)
- Model 1A + 1A-ES (use existing panel)
- Model 3A + 3-ES (need closed tanks dataset)
- Model 2A-C (use tank inventory)

**Phase 3: Advanced Models** (3-4 hours)
- Model 1B (facility survival)
- Model 3B (within-facility)
- Models 4, 5A, 5B (leak mechanisms)

**Phase 4: Tables & Figures** (1-2 hours)
- Format existing panel data into Tables 1-3
- Create Figure 4 from panel

---

## END OF UPDATED PLAN

**Key Insight:** The panel builder is doing its job correctly. We just need to:
1. Use its output properly (facility-year panel)
2. Go back to the tank-level source data when needed (Models 2-5)
3. Replace the wrong model structure (Parts A-H → Models 1-5)

The heavy lifting is already done!
