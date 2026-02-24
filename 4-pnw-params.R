# ==============================================================================
# Script: 4-pnw-params.R
# Project: B1-data - Hydropower Generation Constraints Dataset
# ==============================================================================
#
# Purpose:
#   Analyzes 28 Pacific Northwest hydropower plants with high-quality USACE hourly
#   generation data (2001-2024) to derive statistical parameters for max/min
#   constraints and ADOR (Average Daily Operating Range).
#
# Inputs:
#   - output/pnw_hourly_power.csv: Hourly power data from 3-hydropower.R
#   - data/ORNL_EHAHydroPlant_PublicFY2024.xlsx: Plant metadata
#
# Outputs:
#   - output/PNW_28_max_min_ador_parameters.csv (monthly-based)
#   - output/PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv (weekly-based)
#   - output/USACE_weekly_parameters_28.csv (weekly targets for PNW plants)
#
# Authors: Cameron Bracken (cameron.bracken@pnnl.gov)
#
# Original code: Sean Turner (sean.turner@pnnl.gov)
#
# Version History:
#   v1.0   - Sean Turner - Original version
#   2023 Update - Cameron Bracken
#   2024 Update - Cameron Bracken
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
start_year = 2001
end_year = 2024

# Input paths
pnw_hourly_file = "output/pnw_hourly_power.csv"
eha_file = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"

# Output paths
output_dir = "output"
output_monthly_params = "output/PNW_28_max_min_ador_parameters.csv"
output_weekly_params = "output/PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv"
output_weekly_targets = "output/USACE_weekly_parameters_28.csv"

# Create output directory
dir.create(output_dir, showWarnings = FALSE)

# Validate input files exist
if (!file.exists(pnw_hourly_file)) {
  stop(
    "PNW hourly power file not found: ",
    pnw_hourly_file,
    "\nPlease run 3-hydropower.R first."
  )
}

if (!file.exists(eha_file)) {
  stop(
    "EHA plant file not found: ",
    eha_file,
    "\nPlease ensure ORNL EHA data is downloaded."
  )
}

# %% Prepare date sequences
date_time_sequence =
  tibble(
    date_time = seq(
      as.POSIXct(sprintf("%d-01-01 00:00:00", start_year)),
      as.POSIXct(sprintf("%d-12-31 23:00:00", end_year)),
      by = "hour"
    )
  )

sequence_2001_2020_monthly =
  date_time_sequence |>
  mutate(
    year = year(date_time),
    month = month(date_time, label = TRUE),
    date = date(date_time)
  ) |>
  select(year, month, date) |>
  unique()

sequence_2001_2020_weekly =
  date_time_sequence |>
  mutate(year = year(date_time)) |>
  group_by(year) |>
  group_split() |>
  map_dfr(\(x) {
    yr =
      x[["date_time"]][1] |>
      year()

    x |>
      mutate(week_commencing = floor_date(date(date_time), "week", 7)) |>
      mutate(
        week_commencing = if_else(
          year(week_commencing) < yr,
          ymd(paste0(yr, "-01-01")),
          week_commencing
        )
      ) |>
      mutate(date = date(date_time)) |>
      select(-date_time) |>
      unique()
  })

# %% Load nameplate capacities from EHA
nameplates =
  readxl::read_xlsx(eha_file, sheet = "Operational") |>
  select(eia_id = EIA_PtID, nameplate = CH_MW) |>
  filter(!is.na(eia_id)) |>
  unique()

# %% Define dam codes
dam_codes =
  tribble(
    ~dam  , ~eia_id ,
    "BON" , 3075L   ,
    "CHJ" , 3921L   ,
    "GCL" , 6163L   ,
    "IHR" , 3925L   ,
    "JDA" , 3082L   ,
    "LGS" , 3926L   ,
    "LMN" , 3927L   ,
    "LWG" , 6175L   ,
    "MCN" , 3084L   ,
    "PRD" , 3887L   ,
    "TDA" , 3895L   ,
    "LIB" , 6172L   ,
    "ALF" ,  851L   ,
    "BCL" , 3074L   ,
    "CGR" , 3076L   ,
    "DET" , 3077L   ,
    "DEX" , 3078L   ,
    "DWR" ,  840L   ,
    "FOS" , 6552L   ,
    "GPR" , 3080L   ,
    "HCR" , 3081L   ,
    "HGH" , 2203L   ,
    "LOP" , 3083L   ,
    "LOS" , 6174L   ,
    "RIS" , 6200L   ,
    "RRH" , 3883L   ,
    "WAN" , 3888L   ,
    "WEL" , 3886L
  )

eia_id_and_nameplate =
  dam_codes |>
  left_join(nameplates, by = "eia_id")

# %% Load hourly generation data
hourly_all_plants =
  pnw_hourly_file |>
  read_csv(show_col_types = FALSE, progress = FALSE) |>
  select(dam, date_time = datetime_pacific, power, eia_id) %>%
  split(.$dam) |>
  map(\(x) {
    date_time_sequence |>
      left_join(
        select(x, date_time, power),
        by = "date_time"
      ) |>
      mutate(
        dam = x[["dam"]][1],
        eia_id = x[["eia_id"]][1]
      ) |>
      left_join(
        select(eia_id_and_nameplate, eia_id, nameplate),
        by = "eia_id"
      ) |>
      # remove a few weird high points in PRD, WEL, WAN
      mutate(power = if_else(power > nameplate, NA_real_, power)) |>
      mutate(power = if_else(power <= 0, NA_real_, power)) |>
      tidyr::fill(power, .direction = "down")
  }) |>
  bind_rows()

# %% Compute daily statistics
daily_stats =
  hourly_all_plants |>
  na.omit() |>
  mutate(date = date(date_time)) |>
  group_by(date, dam) |>
  summarise(
    mwh = sum(power, na.rm = TRUE),
    max = max(power, na.rm = TRUE),
    min = min(power, na.rm = TRUE),
    dor = max - min,
    .groups = "drop"
  )

# %% Aggregate to monthly statistics
monthly_stats_PNW =
  daily_stats |>
  left_join(sequence_2001_2020_monthly, by = "date") |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    dor = if_else(is.infinite(dor), NA_real_, dor)
  ) |>
  group_by(year, month, dam) |>
  summarise(
    mwh = sum(mwh),
    max = max(max, na.rm = TRUE),
    min = min(min, na.rm = TRUE),
    ador = mean(dor, na.rm = TRUE),
    n_hours = n() * 24,
    .groups = "drop"
  )

# %% Aggregate to weekly statistics
weekly_stats_PNW =
  daily_stats |>
  left_join(sequence_2001_2020_weekly, by = "date") |>
  group_by(year, week_commencing, dam) |>
  summarise(
    mwh = sum(mwh),
    max = max(max, na.rm = TRUE),
    min = min(min, na.rm = TRUE),
    ador = mean(dor),
    n_hours = n() * 24,
    .groups = "drop"
  )

# %% Diagnostic plot
weekly_stats_PNW |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean = mwh / n_hours) |>
  filter(dam == "GPR") |>
  ggplot(aes(week_commencing, mean, group = dam)) +
  geom_line() +
  geom_line(aes(y = max)) +
  geom_line(aes(y = min)) +
  facet_wrap(~year, scales = "free_x")

# %% Compute monthly parameters
monthly_stats_PNW |>
  left_join(eia_id_and_nameplate, by = "dam") |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean = mwh / n_hours) |>
  mutate(
    max_param = (max - mean) / (nameplate - mean),
    min_param = min / mean,
    ador_param = ador / (max - min)
  ) |>
  group_by(dam) |>
  mutate(
    max_param_ = median(max_param, na.rm = TRUE),
    min_param_ = median(min_param, na.rm = TRUE),
    ador_param_ = median(ador_param, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    p_max = mean + max_param_ * (nameplate - mean),
    p_min = min_param_ * mean,
    p_ador = (max - min) * ador_param_
  ) |>
  select(
    dam,
    eia_id,
    max_param = max_param_,
    min_param = min_param_,
    ador_param = ador_param_
  ) |>
  unique() |>
  write_csv(output_monthly_params)

# %% Compute weekly parameters
weekly_stats_PNW |>
  left_join(eia_id_and_nameplate, by = "dam") |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean = mwh / n_hours) |>
  mutate(
    max_param = (max - mean) / (nameplate - mean),
    min_param = min / mean,
    ador_param = ador / (max - min)
  ) |>
  group_by(dam) |>
  mutate(
    max_param_ = median(max_param, na.rm = TRUE),
    min_param_ = median(min_param, na.rm = TRUE),
    ador_param_ = median(ador_param, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    p_max = mean + max_param_ * (nameplate - mean),
    p_min = min_param_ * mean,
    p_ador = (max - min) * ador_param_
  ) |>
  select(
    dam,
    eia_id,
    max_param = max_param_,
    min_param = min_param_,
    ador_param = ador_param_
  ) |>
  unique() |>
  write_csv(output_weekly_params)

# %% Diagnostic plot - hourly data
hourly_all_plants |>
  filter(year(date_time) == 2011) |>
  ggplot(aes(date_time, power)) +
  geom_line() +
  facet_wrap(~dam, scales = "free_y")

# %% Compute 2001 weekly statistics for USACE
hourly_with_weeks =
  hourly_all_plants |>
  filter(year(date_time) == 2001) |>
  mutate(
    week_commencing = if_else(
      day(date(date_time)) <= 6,
      ymd("2001-01-01"),
      floor_date(date(date_time), "week", 7)
    ),
    week = as.integer(factor(week_commencing))
  )

ador =
  hourly_with_weeks |>
  filter(week < 53) |>
  mutate(date = date(date_time)) |>
  group_by(dam, date, week, week_commencing) |>
  summarise(daily_range = max(power) - min(power), .groups = "drop") |>
  group_by(dam, week, week_commencing) |>
  summarise(ador = mean(daily_range), .groups = "drop")

USACE_weekly_parameters =
  hourly_with_weeks |>
  group_by(dam, week, week_commencing) |>
  summarise(
    p_max = max(power),
    p_min = min(power),
    p_avg = mean(power),
    n_hours = n(),
    .groups = "drop"
  ) |>
  left_join(ador, by = c("dam", "week", "week_commencing")) |>
  left_join(dam_codes, by = "dam") |>
  mutate(target_mwh = p_avg * n_hours) |>
  select(
    eia_id,
    week,
    week_commencing,
    n_hours,
    target_mwh,
    p_avg,
    p_max,
    p_min,
    ador
  ) |>
  mutate_if(is.numeric, \(x) round(x, 4))

# %% Write weekly parameters
readr::write_csv(USACE_weekly_parameters, output_weekly_targets)
