# ==============================================================================
# TABLE: LEAK TIMING RELATIVE TO EXIT (OPERATIONAL VS. CLOSURE DISCOVERY)
# ==============================================================================

cat("Generating Leak Timing Summary Table...\n")
library(gt)
# 1. Prepare Filtered Data
# ------------------------------------------------------------------------------
# Filter to facilities that have BOTH a leak and an exit event
filtered_data1 <- filtered_data[
  leak_incident == 1 & exit_flag == 1,
  .(
    first_leak_date = min(date[leak_incident == 1], na.rm = TRUE),
    first_exit_date = min(date[exit_flag == 1], na.rm = TRUE),
    treatment_group = first(treatment_group)
  ),
  by = panel_id
]

# Define precedence
filtered_data1[, leak_preceded_exit := first_leak_date < first_exit_date]

# 2. Aggregate Counts
# ------------------------------------------------------------------------------
timing_summary <- filtered_data1[, .(
  N_Facilities = .N,
  N_Preceded = sum(leak_preceded_exit, na.rm = TRUE),
  N_Coincided = sum(!leak_preceded_exit, na.rm = TRUE)
), by = treatment_group]

# Calculate Percentages
timing_summary[, `:=`(
  Pct_Preceded = 100 * N_Preceded / N_Facilities,
  Pct_Coincided = 100 * N_Coincided / N_Facilities
)]

# 3. Create GT Table
# ------------------------------------------------------------------------------
t_leak_timing <- timing_summary[treatment_group %in% c("Texas", "Control")] %>%
  gt() %>%
  tab_header(
    title = "Leak Discovery Timing Relative to Facility Exit",
    subtitle = "Comparing Operational Discovery vs. Found-at-Closure"
  ) %>%
  cols_label(
    treatment_group = "Group",
    N_Facilities = "Total Exiting w/ Leak",
    N_Preceded = "Count",
    Pct_Preceded = "%",
    N_Coincided = "Count",
    Pct_Coincided = "%"
  ) %>%
  tab_spanner(
    label = "Operational Discovery (Leak < Exit)",
    columns = c(N_Preceded, Pct_Preceded)
  ) %>%
  tab_spanner(
    label = "Found at Closure (Leak ≥ Exit)",
    columns = c(N_Coincided, Pct_Coincided)
  ) %>%
  fmt_number(
    columns = c(N_Facilities, N_Preceded, N_Coincided),
    decimals = 0,
    sep_mark = ","
  ) %>%
  fmt_number(
    columns = c(Pct_Preceded, Pct_Coincided),
    decimals = 1
  ) %>%
  cols_align(
    align = "center",
    columns = -treatment_group
  )

# 4. Basic Save Command
# ------------------------------------------------------------------------------
# Saves directly to HTML using the standard gt::gtsave function
gtsave(t_leak_timing, filename = here("Output", "Tables", "T_leak_timing_analysis.html"))

cat("Table saved.\n")