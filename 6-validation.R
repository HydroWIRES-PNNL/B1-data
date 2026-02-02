# ==============================================================================
# 6-validation.R
# ==============================================================================
#
#   Validate B1 dataset outputs through data checks, visualizations, and
#   comparisons with previous versions and RectifHyd source data.
#
# Validation Steps:
#   1. Data integrity checks (missing values, ranges, consistency)
#   2. Comparison with RectifHyd v1.4.0 source data
#   3. Comparison with previous B1 versions (if available)
#   4. Temporal patterns and regional aggregations
#   5. Generate diagnostic plots and summary statistics
#
# Inputs:
#   - output/B1_data_1.4.0/B1_monthly.csv:b1_monthlyB1 output from 5a-B1-monthly.R
#   - output/B1_data_1.4.0/B1_weekly.csv: Weekly B1 output from 5b-B1-weekly.R
#   - data/RectifHyd_v1.4.0.csv: Source RectifHyd data
#   - previous_versions/B1_monthly_v*.csv: Previous versions (optional)
#
# Outputs:
#   - output/validation_report.txt: Text summary of validation results
#   - output/validation_plots/: Directory containing diagnostic plots
#
# Authors: Cameron Bracken (cameron.bracken@pnnl.gov)
# ==============================================================================

# %% Libraries
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
library(tidyverse)
library(glue)
s = glue::glue
library(paletteer)

source('utilities.R')

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000
)


# %% Configuration and paths
version = "1.4.0"
output_dir = "output"
b1_dir = file.path(output_dir, paste0("B1_data_", version))
validation_dir = file.path(output_dir, "validation_plots")

dir.create(output_dir, showWarnings = FALSE)
dir.create(validation_dir, showWarnings = FALSE, recursive = TRUE)

# Input paths
b1_monthly_file = file.path(b1_dir, "B1_monthly.csv")
b1_weekly_file = file.path(b1_dir, "B1_weekly.csv")
rectifhyd_file = "data/RectifHyd_v1.4.0.csv"

# Output paths
report_file = file.path(output_dir, "validation_report.txt")

# %% Load data
message("Loading B1 data...")
b1_monthly = read_csv(b1_monthly_file)
b1_weekly = read_csv(b1_weekly_file)
rectifhyd = read_csv(rectifhyd_file) |> rename(datetime = date)

# %% Initialize validation report
report = c(
  "==============================================================================",
  "B1 Dataset Validation Report",
  sprintf("Version: %s", version),
  sprintf("Generated: %s", Sys.time()),
  "==============================================================================",
  ""
)

# %% Section 1: Data Integrity Checks
report = c(report, "1. DATA INTEGRITY CHECKS", "")

#b1_monthlydata checks
n_plants_monthly = length(unique(b1_monthly$eia_id))
n_years_monthly = length(unique(b1_monthly$year))
n_records_monthly = nrow(b1_monthly)

report = c(
  report,
  sprintf("Monthly Dataset:"),
  sprintf("  - Total records: %s", format(n_records_monthly, big.mark = ",")),
  sprintf("  - Unique plants: %s", n_plants_monthly),
  sprintf(
    "  - Year range: %d-%d (%d years)",
    min(b1_monthly$year),
    max(b1_monthly$year),
    n_years_monthly
  ),
  ""
)

# Check for missing values
missing_cols_monthly = b1_monthly |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "column", values_to = "n_missing") |>
  filter(n_missing > 0) |>
  arrange(desc(n_missing))

if (nrow(missing_cols_monthly) > 0) {
  report = c(report, "  Missing values detected:")
  for (i in 1:min(5, nrow(missing_cols_monthly))) {
    report = c(
      report,
      sprintf(
        "    - %s: %s (%.1f%%)",
        missing_cols_monthly$column[i],
        format(missing_cols_monthly$n_missing[i], big.mark = ","),
        100 * missing_cols_monthly$n_missing[i] / n_records_monthly
      )
    )
  }
  report = c(report, "")
} else {
  report = c(report, "  No missing values in key columns", "")
}

# Weekly data checks
n_plants_weekly = length(unique(b1_weekly$eia_id))
n_years_weekly = length(unique(b1_weekly$year))
n_records_weekly = nrow(b1_weekly)

report = c(
  report,
  sprintf("Weekly Dataset:"),
  sprintf("  - Total records: %s", format(n_records_weekly, big.mark = ",")),
  sprintf("  - Unique plants: %s", n_plants_weekly),
  sprintf(
    "  - Year range: %d-%d (%d years)",
    min(b1_weekly$year),
    max(b1_weekly$year),
    n_years_weekly
  ),
  sprintf(
    "  - Weeks per year: ~%d",
    round(n_records_weekly / n_plants_weekly / n_years_weekly)
  ),
  ""
)

# %% Section 2: Physical Constraint Checks
report = c(report, "2. PHYSICAL CONSTRAINT CHECKS", "")

# Check if p_avg is within bounds
violations_monthly =
  b1_monthly |>
  mutate(
    exceeds_nameplate = p_avg > nameplate,
    min_above_avg = p_min > p_avg,
    max_below_avg = p_max < p_avg,
    min_above_max = p_min > p_max,
    negative_ador = ador < 0
  ) |>
  summarise(
    exceeds_nameplate = sum(exceeds_nameplate, na.rm = TRUE),
    min_above_avg = sum(min_above_avg, na.rm = TRUE),
    max_below_avg = sum(max_below_avg, na.rm = TRUE),
    min_above_max = sum(min_above_max, na.rm = TRUE),
    negative_ador = sum(negative_ador, na.rm = TRUE)
  )

report = c(
  report,
  "Monthly Constraint Violations:",
  sprintf("  - p_avg > nameplate: %d", violations_monthly$exceeds_nameplate),
  sprintf("  - p_min > p_avg: %d", violations_monthly$min_above_avg),
  sprintf("  - p_max < p_avg: %d", violations_monthly$max_below_avg),
  sprintf("  - p_min > p_max: %d", violations_monthly$min_above_max),
  sprintf("  - ador < 0: %d", violations_monthly$negative_ador),
  ""
)

# %% Section 3: Comparison with RectifHyd
report = c(report, "3. COMPARISON WITH RECTIFHYD v1.4.0", "")

# Join B1b1_monthlywith RectifHyd
comparison =
  b1_monthly |>
  select(eia_id, datetime, b1_target_mwh = target_mwh, nameplate, state) |>
  inner_join(
    rectifhyd |>
      select(eia_id, datetime, rectifhyd_mwh, recommended_data),
    by = join_by(eia_id, datetime)
  )

# Calculate differences
diff_summary =
  comparison |>
  mutate(
    diff_mwh = b1_target_mwh - rectifhyd_mwh,
    pct_diff = 100 * (b1_target_mwh - rectifhyd_mwh) / rectifhyd_mwh
  ) |>
  summarise(
    n_records = n(),
    mean_diff = mean(diff_mwh, na.rm = TRUE),
    median_diff = median(diff_mwh, na.rm = TRUE),
    max_diff = max(abs(diff_mwh), na.rm = TRUE),
    rmse = sqrt(mean(diff_mwh^2, na.rm = TRUE)),
    mean_pct_diff = mean(abs(pct_diff), na.rm = TRUE)
  )

report = c(
  report,
  sprintf("Energy Target Comparison (B1 vs RectifHyd):"),
  sprintf(
    "  - Records compared: %s",
    format(diff_summary$n_records, big.mark = ",")
  ),
  sprintf("  - Mean difference: %.2f MWh", diff_summary$mean_diff),
  sprintf("  - Median difference: %.2f MWh", diff_summary$median_diff),
  sprintf("  - Max absolute difference: %.2f MWh", diff_summary$max_diff),
  sprintf("  - RMSE: %.2f MWh", diff_summary$rmse),
  sprintf(
    "  - Mean absolute %% difference: %.2f%%",
    diff_summary$mean_pct_diff
  ),
  ""
)

# Identify largest discrepancies
largest_diffs =
  comparison |>
  mutate(abs_diff = abs(b1_target_mwh - rectifhyd_mwh)) |>
  arrange(desc(abs_diff)) |>
  head(5)

report = c(report, "  Top 5 largest discrepancies:")
for (i in 1:nrow(largest_diffs)) {
  report = c(
    report,
    sprintf(
      "    %d. Plant %d (%s), %s: %.0f MWh difference",
      i,
      largest_diffs$eia_id[i],
      largest_diffs$state[i],
      largest_diffs$datetime[i],
      largest_diffs$abs_diff[i]
    )
  )
}
report = c(report, "")

# %% Section 4: Regional Aggregations
report = c(report, "4. REGIONAL AGGREGATIONS", "")

# Western vs non-Western comparison
regional_summary =
  b1_monthly |>
  group_by(western, year) |>
  summarise(
    n_plants = length(unique(eia_id)),
    total_energy_gwh = sum(target_mwh, na.rm = TRUE) / 1000,
    avg_capacity_factor = mean(p_avg / nameplate, na.rm = TRUE),
    .groups = "drop"
  )

west_avg = regional_summary |>
  filter(western == TRUE) |>
  summarise(avg = mean(total_energy_gwh))
non_west_avg = regional_summary |>
  filter(western == FALSE) |>
  summarise(avg = mean(total_energy_gwh))

report = c(
  report,
  sprintf("Western US (WA, ID, CO, UT, NM, WY, MT, CA, OR, NV, AZ):"),
  sprintf("  - Average annual generation: %.0f GWh", west_avg$avg),
  sprintf("Non-Western US:"),
  sprintf("  - Average annual generation: %.0f GWh", non_west_avg$avg),
  ""
)

# %% Section 5: Temporal Patterns
report = c(report, "5. TEMPORAL PATTERNS", "")

#b1_monthly seasonality
seasonal_pattern =
  b1_monthly |>
  filter(western == TRUE) |>
  group_by(month) |>
  summarise(
    avg_generation_gwh = mean(target_mwh, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

peak_month = seasonal_pattern |>
  filter(avg_generation_gwh == max(avg_generation_gwh))
low_month = seasonal_pattern |>
  filter(avg_generation_gwh == min(avg_generation_gwh))

report = c(
  report,
  "Western USb1_monthlySeasonality:",
  sprintf(
    "  - Peak month: %s (%.0f GWh avg)",
    month.abb[peak_month$month],
    peak_month$avg_generation_gwh
  ),
  sprintf(
    "  - Low month: %s (%.0f GWh avg)",
    month.abb[low_month$month],
    low_month$avg_generation_gwh
  ),
  sprintf(
    "  - Peak/Low ratio: %.2f",
    peak_month$avg_generation_gwh / low_month$avg_generation_gwh
  ),
  ""
)
# section 6 compare previous versions ------
# %%: Comparison with Previous Versions
report = c(report, "6. COMPARISON WITH PREVIOUS VERSIONS", "")

previous_versions_info = tribble(
  ~path                             , ~version ,
  "previous_versions/B1_data_1.1.2" , "1.1.2"  ,
  "previous_versions/B1_data_1.2.0" , "1.2.0"  ,
  "previous_versions/B1_data_1.3.0" , "1.3.0"
)

available_versions =
  previous_versions_info |>
  filter(file.exists(path))

# %% read and compare versions
message("Loading previous versions for comparison...")

b1_versions_monthly =
  bind_rows(
    available_versions |>
      pmap_dfr(\(path, version) read_b1(path, version, timestep = "monthly")),
    b1_monthly |> mutate(version = version, datetime = as.Date(datetime))
  )

b1_versions_weekly =
  bind_rows(
    available_versions |>
      pmap_dfr(\(path, version) read_b1(path, version, timestep = "weekly")),
    b1_weekly |> mutate(version = version, datetime = as.Date(datetime))
  )

# %% annual generation by version
annual_by_version =
  b1_versions_monthly |>
  group_by(version, year) |>
  summarise(
    total_twh = sum(target_mwh, na.rm = TRUE) / 1e6,
    .groups = "drop"
  ) |>
  group_by(version) |>
  summarise(
    avg_annual_twh = mean(total_twh, na.rm = TRUE),
    .groups = "drop"
  )

report = c(report, "Annual Generation by Version (Average across years):")
for (i in seq_len(nrow(annual_by_version))) {
  report = c(
    report,
    sprintf(
      "  - v%s: %.2f TWh",
      annual_by_version$version[i],
      annual_by_version$avg_annual_twh[i]
    )
  )
}
report = c(report, "")

# %% plant counts by version
plant_counts =
  b1_versions_monthly |>
  group_by(version, year) |>
  summarise(n_plants = length(unique(eia_id)), .groups = "drop") |>
  group_by(version) |>
  summarise(avg_plants = mean(n_plants), .groups = "drop")

report = c(report, "Average Number of Plants by Version:")
for (i in seq_len(nrow(plant_counts))) {
  report = c(
    report,
    sprintf(
      "  - v%s: %.0f plants",
      plant_counts$version[i],
      plant_counts$avg_plants[i]
    )
  )
}
report = c(report, "")

# %% version comparison plots
message("Generating version comparison plots...")

version_compare_dir = file.path(validation_dir, "version_comparisons")
dir.create(version_compare_dir, showWarnings = FALSE, recursive = TRUE)

# setup color palette ------
versions = sort(unique(b1_versions_monthly$version))
version_palette = "ggsci::default_nejm"
version_colors = paletteer::paletteer_d(version_palette, n = length(versions))
names(version_colors) = versions

# %% aggregate timeseries plot
p_version_aggregate =
  b1_versions_monthly |>
  group_by(version, datetime) |>
  summarise(
    total_gwh = sum(target_mwh, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) |>
  ggplot(aes(datetime, total_gwh, color = version)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = version_colors) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "Total US Hydropower Generation Across B1 Versions",
    subtitle = "Monthly aggregated data",
    x = "",
    y = "Generation [GWh]",
    color = "B1 Version"
  )
print(p_version_aggregate)
ggsave(
  file.path(validation_dir, "07_version_comparison_aggregate.png"),
  p_version_aggregate,
  width = 10,
  height = 6
)

# per-plant comparison plots ------
pdf(
  file.path(version_compare_dir, "B1_version_compare_monthly_all_plants.pdf"),
  10,
  3,
  onefile = TRUE
)
b1_versions_monthly$eia_id |>
  unique() |>
  sort() |>
  walk(
    \(id) {
      plot_one_plant_versions(id, b1_versions_monthly, version_colors) |> print()
    },
    .progress = TRUE
  )
dev.off()

pdf(
  file.path(version_compare_dir, "B1_version_compare_weekly_all_plants.pdf"),
  10,
  3,
  onefile = TRUE
)
b1_versions_weekly$eia_id |>
  unique() |>
  sort() |>
  walk(
    \(id) {
      plot_one_plant_versions(id, b1_versions_weekly, version_colors) |> print()
    },
    .progress = TRUE
  )
dev.off()

report = c(
  report,
  sprintf(
    "Version comparison plots saved to: %s/",
    version_compare_dir
  ),
  ""
)


# plotting ------

# %% Generate diagnostic plots
message("Generating diagnostic plots...")

# Plot 1: monthly generation by region over time
p1 =
  b1_monthly |>
  group_by(datetime, western) |>
  summarise(
    total_gwh = sum(target_mwh, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) |>
  mutate(month_num = month(datetime)) |>
  ggplot(aes(datetime, total_gwh, color = western)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    "",
    values = c("TRUE" = "steelblue", "FALSE" = "coral"),
    labels = c("TRUE" = "Western US", "FALSE" = "Non-Western US")
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "Monthly Hydropower Generation by Region",
    subtitle = sprintf("B1 Dataset v%s", version),
    x = "",
    y = "Generation [GWh]"
  )
print(p1)
ggsave(
  file.path(validation_dir, "01_generation_timeseries.png"),
  p1,
  width = 10,
  height = 6
)

# %% Plot 2: Seasonal pattern comparison (Western US)
p2 =
  b1_monthly |>
  filter(western == TRUE) |>
  group_by(month, year) |>
  summarise(
    total_gwh = sum(target_mwh, na.rm = TRUE) / 1000,
    .groups = "drop"
  ) |>
  ggplot(aes(month, total_gwh, group = year)) +
  geom_line(alpha = 0.3, color = "steelblue") +
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    color = "darkblue",
    linewidth = 1.5
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal() +
  labs(
    title = "Western US Seasonal Pattern by Year",
    subtitle = "Individual years (light) and multi-year average (dark)",
    x = "",
    y = "Generation [GWh]"
  )
print(p2)
ggsave(
  file.path(validation_dir, "02_seasonal_pattern.png"),
  p2,
  width = 10,
  height = 6
)

# %% Plot 3: B1 vs RectifHyd comparison scatter
p3 =
  comparison |>
  # sample_n(min(10000, nrow(comparison))) |>
  ggplot(aes(rectifhyd_mwh, b1_target_mwh)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "B1 vs RectifHyd Energy Targets",
    x = "RectifHyd [MWh]",
    y = "B1 [MWh]"
  )
print(p3)
ggsave(
  file.path(validation_dir, "03_b1_vs_rectifhyd.png"),
  p3,
  width = 8,
  height = 8
)

# %% Plot 4: Constraint ranges by plant size
p4 =
  b1_monthly |>
  filter(year == 2020) |>
  group_by(eia_id, nameplate) |>
  summarise(
    p_avg = mean(p_avg, na.rm = TRUE),
    p_min = mean(p_min, na.rm = TRUE),
    p_max = mean(p_max, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    nameplate_bin = cut(
      nameplate,
      breaks = c(0, 10, 50, 100, 500, Inf),
      labels = c("<10 MW", "10-50 MW", "50-100 MW", "100-500 MW", ">500 MW")
    )
  ) |>
  filter(!is.na(nameplate_bin)) |>
  ggplot(aes(nameplate_bin)) +
  geom_boxplot(aes(y = p_avg), fill = "lightblue", alpha = 0.7) +
  geom_boxplot(aes(y = p_min), fill = "coral", alpha = 0.7) +
  geom_boxplot(aes(y = p_max), fill = "lightgreen", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Operating Constraints by Plant Size (2020)",
    subtitle = "p_avg (blue), p_min (coral), p_max (green)",
    x = "Nameplate Capacity",
    y = "Power [MW]"
  )
print(p4)
ggsave(
  file.path(validation_dir, "04_constraints_by_size.png"),
  p4,
  width = 10,
  height = 6
)

# %% Plot 5: Weekly vs B1 Monthly
weekly_agg =
  b1_weekly |>
  group_by(eia_id, year) |>
  summarise(weekly_total_mwh = sum(target_mwh, na.rm = TRUE), .groups = "drop")

monthly_agg =
  b1_monthly |>
  group_by(eia_id, year) |>
  summarise(monthly_total_mwh = sum(target_mwh, na.rm = TRUE), .groups = "drop")

p5 =
  weekly_agg |>
  inner_join(monthly_agg, by = c("eia_id", "year")) |>
  sample_n(min(5000, n())) |>
  ggplot(aes(monthly_total_mwh, weekly_total_mwh)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Annual Energy: B1 Monthly vs. Weekly Datasets",
    subtitle = "Should match exactly (red line = perfect agreement)",
    x = "Monthly Dataset [MWh/year]",
    y = "Weekly Dataset [MWh/year]"
  )
print(p5)
ggsave(
  file.path(validation_dir, "05_weekly_vs_monthly.png"),
  p5,
  width = 8,
  height = 8
)

# %% Plot 6: Capacity factor distribution
p6 =
  b1_monthly |>
  mutate(capacity_factor = p_avg / nameplate) |>
  filter(!is.na(capacity_factor), capacity_factor >= 0, capacity_factor <= 1) |>
  ggplot(aes(capacity_factor, fill = western)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  scale_fill_manual(
    "",
    values = c("TRUE" = "steelblue", "FALSE" = "coral"),
    labels = c("TRUE" = "Western US", "FALSE" = "Non-Western US")
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(
    title = "Distribution of Average Capacity Factors",
    subtitle = "All plants, all months",
    x = "Capacity Factor (p_avg / nameplate)",
    y = "Count"
  )
print(p6)
ggsave(
  file.path(validation_dir, "06_capacity_factor_distribution.png"),
  p6,
  width = 10,
  height = 6
)

# %% Write validation report
report = c(
  report,
  "==============================================================================",
  "VALIDATION COMPLETE",
  sprintf("Report saved to: %s", report_file),
  sprintf("Diagnostic plots saved to: %s/", validation_dir),
  "=============================================================================="
)

writeLines(report, report_file)
cat(paste(report, collapse = "\n"))

message("\nValidation complete!")
message("Report: ", report_file)
message("Plots: ", validation_dir, "/")
