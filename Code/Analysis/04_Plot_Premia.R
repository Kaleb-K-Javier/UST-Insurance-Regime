


library(data.table)
library(ggplot2)
library(stringr)
library(tidyverse)


# Set working directory to the folder containing the CSVs
data_dir <- "c:/Users/kaleb/Box/UST-Insurance/Rate FIllings/Mid-Continent Casualty Company ­– 23418"
# the new file path is : C:\Users\kalebkja\ust_ins_move_to_github\Data\Rate FIllings\Mid-Continent Casualty Company ­– 23418
# List all relevant CSV files
csv_files <- list.files(
  path = data_dir,
  pattern = "^texas_midcontinent_facility_year_premium_.*\\.csv$",
  full.names = TRUE
)

# List all relevant CSV files
# List all relevant CSV files

csv_tank_files <- list.files(
  path = data_dir,
  pattern = "texas_midcontinent_tank_month_premium_.*\\.csv$",
  full.names = TRUE
)


all_tank_data <- rbindlist(lapply(csv_tank_files, fread), fill = TRUE)


fa_monthly_contract <- fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/panel_merge_staging/fa_monthly_contract.csv")

#load the panel
panel <- fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/panel_merge_staging/panel.csv")
head(panel)
## create the following tables:
### rows are insurance companies
### columns are years
### values are number of unique facilities
fa_facility_counts <- fa_monthly_contract %>%
    filter(FACILITY_ID %in% panel$FACILITY_ID) %>%
    group_by(ISSUER_NAME, YEAR) %>%
    summarise(unique_facilities = n_distinct(FACILITY_ID), .groups = 'drop') %>%
    pivot_wider(names_from = YEAR, values_from = unique_facilities, values_fill = 0)

glimpse(fa_facility_counts)



# Output directory for plots
plot_dir <- file.path(data_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir)
# Combine all CSV files into one data.table
all_data <- rbindlist(lapply(csv_files, fread), fill = TRUE)
max(all_data$YEAR, na.rm = TRUE)

# Ensure YEAR column is numeric and drop rows without YEAR
if (!"YEAR" %in% names(all_data)) stop("No YEAR column found in data.")
all_data[, YEAR := as.numeric(YEAR)]
all_data <- all_data[!is.na(YEAR)]



# Compare number of unique Mid-Continent facilities in premia data vs contracts
midc_facilities_premia <- unique(all_data$FACILITY_ID)
midc_facilities_contract <- unique(
  fa_monthly_contract[
    tolower(ISSUER_NAME) %like% "mid-",
    FACILITY_ID
  ]
)

# Table: number of unique facilities per year in premia data
premia_fac_count <- all_data[, .(premia_facilities = uniqueN(FACILITY_ID)), by = YEAR]

# Table: number of unique facilities per year in contracts
contract_fac_count <- fa_monthly_contract[
  tolower(ISSUER_NAME) %like% "mid-",
  .(contract_facilities = uniqueN(FACILITY_ID)),
  by = YEAR
]

# Merge for plotting
compare_fac <- merge(
  premia_fac_count,
  contract_fac_count,
  by = "YEAR",
  all = TRUE
)

# Plot: Number of unique facilities per year in premia vs contracts
p_fac_compare <- ggplot(compare_fac, aes(x = YEAR)) +
  geom_line(aes(y = premia_facilities, color = "Premia Data"), size = 1) +
  geom_line(aes(y = contract_facilities, color = "Contract Data"), size = 1, linetype = "dashed") +
  labs(
    title = "Unique Mid-Continent Facilities: Premia vs Contract Data",
    x = "Year",
    y = "Number of Unique Facilities"
  ) +
  scale_color_manual(values = c("Premia Data" = "blue", "Contract Data" = "red")) +
  theme_minimal(base_size = 16)

p_fac_compare

ggsave(
  filename = file.path(plot_dir, "midcontinent_facility_count_comparison.png"),
  plot = p_fac_compare, width = 8, height = 5, dpi = 300
)



# Series 1: Original means
series_1 <- all_data[, .(
    mean_base_premium = mean(base_premium, na.rm = TRUE),
    mean_sched_min    = mean(sched_min, na.rm = TRUE),
    mean_sched_max    = mean(sched_max, na.rm = TRUE)
), by = YEAR]

# Series 2: Means with minimum 500
series_2 <- all_data[, .(
    mean_base_premium = mean(pmax(base_premium, 500), na.rm = TRUE),
    mean_sched_min    = mean(pmax(sched_min, 500), na.rm = TRUE),
    mean_sched_max    = mean(pmax(sched_max, 500), na.rm = TRUE)
), by = YEAR]

# Title years
year_range <- range(series_1$YEAR, na.rm = TRUE)
title_years <- paste0(year_range[1], "–", year_range[2])

# Plot 1: Original means
p1 <- ggplot(series_1, aes(x = YEAR)) +
    geom_line(aes(y = mean_base_premium, color = "Base Premium"), size = 1) +
    geom_line(aes(y = mean_sched_min, color = "Min Premium"), size = 1, linetype = "dashed") +
    geom_line(aes(y = mean_sched_max, color = "Max Premium"), size = 1, linetype = "dashed") +
    labs(
        title = paste("Mid‑Continent Facility Premia (", title_years, ")", sep = ""),
        x = "Year",
        y = "Mean Premium ($)"
    ) +
    scale_color_manual(values = c("Base Premium" = "blue", "Min Premium" = "green", "Max Premium" = "red")) +
    theme_minimal(base_size = 16)

p1


ggsave(
    filename = file.path(plot_dir, "all_series1.png"),
    plot = p1, width = 8, height = 5, dpi = 300
)


# Plot 2: Adjusted means
p2 <- ggplot(series_2, aes(x = YEAR)) +
    geom_line(aes(y = mean_base_premium, color = "Base Premium"), size = 1) +
    geom_line(aes(y = mean_sched_min, color = "Min Premium"), size = 1, linetype = "dashed") +
    geom_line(aes(y = mean_sched_max, color = "Max Premium"), size = 1, linetype = "dashed") +
    labs(
        title = paste("Mid‑Continent Facility Premia (", title_years, ")", sep = ""),
        x = "Year",
        y = "Mean Premium ($)"
    ) +
    scale_color_manual(values = c("Base Premium" = "blue", "Min Premium" = "green", "Max Premium" = "red")) +
    theme_minimal(base_size = 16)

p2

ggsave(
    filename = file.path(plot_dir, "all_series2.png"),
    plot = p2, width = 8, height = 5, dpi = 300
)

# Plot 3: Comparison of mean_base_premium
p3 <- ggplot() +
    geom_line(data = series_1, aes(x = YEAR, y = mean_base_premium, color = "Original Base Premium"), size = 1) +
    geom_line(data = series_2, aes(x = YEAR, y = mean_base_premium, color = "Adjusted Base Premium"), size = 1) +
    labs(
        title = paste("Comparison of Mean Base Premiums (", title_years, ")", sep = ""),
        x = "Year",
        y = "Mean Base Premium ($)"
    ) +
    scale_color_manual(values = c("Original Base Premium" = "blue", "Adjusted Base Premium" = "orange")) +
    theme_minimal(base_size = 16)

p3

ggsave(
    filename = file.path(plot_dir, "all_comparison.png"),
    plot = p3, width = 8, height = 5, dpi = 300
)

# Save all plots in a single PDF
ggsave(
    filename = file.path(plot_dir, "all_plots.pdf"),
    plot = gridExtra::grid.arrange(p1, p2, p3, ncol = 1),
    width = 8, height = 15, dpi = 300
)


# Series 3: Adjusted base premium with 95% confidence interval
series_3 <- all_data[, {
    vals <- pmax(base_premium, 500)
    m <- mean(vals, na.rm = TRUE)
    s <- sd(vals, na.rm = TRUE)
    n <- sum(!is.na(vals))
    se <- s / sqrt(n)
    ci <- 1.96 * se
    list(
        mean_base_premium = m,
        lower_ci = m - ci,
        upper_ci = m + ci
    )
}, by = YEAR]

# Plot 4: Adjusted base premium with 95% CI
p4 <- ggplot(series_3, aes(x = YEAR, y = mean_base_premium)) +
    geom_line(color = "orange", size = 1) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "orange", alpha = 0.2) +
    labs(
        title = paste("Adjusted Base Premium with 95% CI (", title_years, ")", sep = ""),
        x = "Year",
        y = "Mean Adjusted Base Premium ($)"
    ) +
    theme_minimal(base_size = 16)

p4


ggsave(
    filename = file.path(plot_dir, "all_series3_ci.png"),
    plot = p4, width = 8, height = 5, dpi = 300
)


# Series 4: Total premium collected each year
series_4 <- all_data[, .(
    total_premium = sum(base_premium, na.rm = TRUE)
), by = YEAR]

# Plot 5: Total premium collected by year
p5 <- ggplot(series_4, aes(x = YEAR, y = total_premium)) +
    geom_col(fill = "steelblue") +
    labs(
        title = paste("Total Premium Collected by Year (", title_years, ")", sep = ""),
        x = "Year",
        y = "Total Premium Collected ($)"
    ) +
    theme_minimal(base_size = 16)

p5
ggsave(
    filename = file.path(plot_dir, "all_total_premium.png"),
    plot = p5, width = 8, height = 5, dpi = 300
)





