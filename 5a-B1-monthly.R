# ==============================================================================
# Script: 5a-B1-monthly.R
# Project: B1-data - Hydropower Generation Constraints Dataset
# ==============================================================================
#
# Purpose:
#   Generate monthly B1 constraints dataset by combining RectifHyd monthly energy
#   values with PNW-derived parameters to calculate p_max, p_min, and ADOR.
#   Non-PNW plants use mode-averaged parameters (Run-of-River vs Storage).
#
# Approach:
#   1. Take RectifHyd and add max, min, ador using average of PNW parameters
#   2. Fill missing data with historical medians
#   3. Use observed USACE data where available
#
# Inputs:
#   - data/RectifHyd_v1.4.0.csv: Monthly hydropower generation data
#   - data/ORNL_EHAHydroPlant_PublicFY2024.xlsx: Plant metadata and modes
#   - output/PNW_28_max_min_ador_parameters.csv: Parameter values from 4-pnw-params.R
#   - data/eia_huc4.csv: EIA to huc4 crosswalk
#   - output/huc4_average_flows_imputed.csv: huc4 streamflow data from 1-huc4-streamflow.R
#   - output/pnw_daily_data.csv: PNW dam operational data from 3-hydropower.R
#
#
# Authors: Cameron Bracken (cameron.bracken@pnnl.gov)
#
# Version History:
#   1.4 Update - Jan 2026 - Cameron Bracken
# ==============================================================================

# %% Libraries
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
library(tidyverse)
library(glue)
s = glue::glue
# for writing parquet files
library(arrow)

source('utilities.R')

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000
)


# %% Configuration
start_year = 2001
end_year = 2024
output_prefix = "B1_data"
version = "1.4.0"

# Input paths
rectifhyd_file = "data/RectifHyd_v1.4.0.csv"
eha_file = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"
eia_huc4_file = "data/eia_huc4.csv"
huc4_flows_file = "output/huc4_average_flows_imputed.csv"
pnw_params_file = "output/PNW_28_max_min_ador_parameters.csv"

# Output paths
output_dir = "output"
b1_dir = file.path(output_dir, paste0(output_prefix, "_", version))

# Final output name
b1_monthly_fn = file.path(b1_dir, "B1_monthly.parquet")

# Create directories
dir.create(output_dir, showWarnings = FALSE)
dir.create(b1_dir, showWarnings = FALSE, recursive = TRUE)

# %% Calculate hours per month
hours_per_month =
  tibble(
    date = seq(ISOdate(2001, 1, 1), to = ISOdate(end_year, 12, 31), by = "day")
  ) |>
  mutate(
    date = lubridate::date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) |>
  group_by(year, month) |>
  summarise(n_days = n(), .groups = "drop") |>
  ungroup() |>
  mutate(n_hours = n_days * 24) |>
  select(year, month, n_hours)

# %% Load EHA metadata
HS =
  readxl::read_xlsx(eha_file, sheet = "Operational") |>
  select(
    eha_ptid = EHA_PtID,
    plant = PtName,
    eia_id = EIA_PtID,
    nameplate_mw = CH_MW,
    ba = BACode
  )

# %% Load crosswalks and flow data
eia_and_huc4 =
  read_csv(eia_huc4_file, show_col_types = FALSE, progress = FALSE) |>
  rename_all(tolower)

all_flows =
  read_csv(huc4_flows_file, show_col_types = FALSE, progress = FALSE)

all_flows_monthly =
  all_flows |>
  group_by(year, month, huc4, usgs_id) |>
  summarise(av_flow_cfs = mean(av_flow_cfs), .groups = "drop")

# %% Load RectifHyd data
rectifhyd =
  read_csv(rectifhyd_file, show_col_types = FALSE, progress = FALSE) |>
  mutate(month = monthi)

# %% Prepare monthly targets
monthly_targets =
  rectifhyd |>
  rename(rectifhyd_nameplate_mw = nameplate_mw) |>
  left_join(HS, by = join_by(eia_id, eha_ptid, plant)) |>
  mutate(target_mwh = recommended_mwh) |>
  select(
    eia_id,
    plant,
    eha_ptid,
    state,
    year,
    month,
    target_mwh,
    nameplate_mw,
    ba
  ) |>
  left_join(hours_per_month, by = c("year", "month")) |>
  mutate(target_mwh = if_else(target_mwh < 0, 0, target_mwh)) |>
  mutate(p_avg = target_mwh / n_hours) |>
  mutate(p_avg = if_else(p_avg > nameplate_mw, nameplate_mw, p_avg))

# %% Calculate replacement data for missing values
replacement_data =
  monthly_targets |>
  group_by(eia_id, month) |>
  summarise(p_avg_ = median(p_avg), .groups = "drop") |>
  ungroup()

# %% Fill missing plant-year-month combinations
eia_ids =
  monthly_targets |>
  pull(eia_id) |>
  unique()

full_frame =
  expand.grid(
    month = 1:12,
    year = 2001:end_year,
    eia_id = eia_ids
  ) |>
  as_tibble()

monthly_targets_filled =
  monthly_targets |>
  select(eia_id, eha_ptid, plant, state, nameplate_mw, ba) |>
  unique() |>
  left_join(full_frame, by = "eia_id") |>
  left_join(hours_per_month, by = c("month", "year")) |>
  left_join(
    monthly_targets,
    by = join_by(
      eia_id,
      eha_ptid,
      plant,
      state,
      nameplate_mw,
      month,
      year,
      n_hours,
      ba
    )
  ) |>
  left_join(replacement_data, by = c("eia_id", "month")) |>
  mutate(p_avg = if_else(is.na(p_avg), p_avg_, p_avg)) |>
  mutate(
    target_mwh = if_else(is.na(target_mwh), p_avg * n_hours, target_mwh)
  ) |>
  select(-p_avg_) |>
  filter(!state %in% c("AK", "HI"))

# %% Load plant modes
modes =
  readxl::read_xlsx(eha_file, sheet = "Operational") |>
  select(eia_id = EIA_PtID, mode = Mode) |>
  filter(!is.na(eia_id)) |>
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) |>
  filter(!duplicated(eia_id)) |>
  unique()

# %% Load PNW parameters and calculate mode averages
mma_params_general =
  read_csv(pnw_params_file, show_col_types = FALSE, progress = FALSE) |>
  left_join(modes, by = join_by(eia_id)) |>
  group_by(mode) |>
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  )

mma_params_pnw =
  read_csv(pnw_params_file, show_col_types = FALSE, progress = FALSE) |>
  select(-dam)

# %% Apply parameters and calculate constraints
monthly_final =
  bind_rows(
    monthly_targets_filled |>
      left_join(modes, by = "eia_id") |>
      filter(!(eia_id %in% mma_params_pnw[["eia_id"]])) |>
      left_join(mma_params_general, by = join_by(mode)),
    monthly_targets_filled |>
      left_join(mma_params_pnw, by = join_by(eia_id)) |>
      filter(eia_id %in% mma_params_pnw[["eia_id"]])
  ) |>
  mutate(
    p_max = p_avg + max_param * (nameplate_mw - p_avg),
    p_min = min_param * p_avg,
    ador = ador_param * (p_max - p_min)
  ) |>
  select(
    eia_id,
    plant,
    state,
    ba,
    year,
    month,
    target_mwh,
    nameplate = nameplate_mw,
    p_avg,
    p_min,
    p_max,
    ador
  ) |>
  left_join(eia_and_huc4, by = join_by(eia_id)) |>
  left_join(all_flows_monthly, by = join_by(year, month, huc4)) |>
  rename(huc4_flow_cfs = av_flow_cfs)

# %% Final output preparation
monthly_final |>
  mutate(
    eia_id = as.integer(eia_id),
    year = as.integer(year),
    month = as.integer(month),
    datetime = sprintf("%s-%02d-01", year, month)
  ) |>
  rename(eia_id = eia_id) |>
  mutate_if(is.double, \(x) round(x, 4)) |>
  mutate(
    Western = if_else(
      state %in%
        c("WA", "ID", "CO", "UT", "NM", "WY", "MT", "CA", "OR", "NV", "AZ"),
      TRUE,
      FALSE
    )
  ) |>
  arrange(-Western) |>
  janitor::clean_names(parsing_option = 3) |>
  write_parquet(b1_monthly_fn)

# %% Diagnostic plots
b1_monthly = read_parquet(b1_monthly_fn)

b1_monthly |>
  filter(western) |>
  group_by(year, month) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  ggplot(aes(month, energy_mwh / 1000, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("orange", "cornflowerblue")) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Energy [GWh]")

b1_monthly |>
  filter(western == TRUE) |>
  group_by(year, month) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  pivot_wider(id_cols = month, names_from = year, values_from = energy_mwh) |>
  mutate(pct_diff = (`2001` - `2009`) / `2009` * 100)
