# ==============================================================================
# Script: 1-huc4-streamflow.R
# Project: B1-data - Hydropower Generation Constraints Dataset
# ==============================================================================
#
# Purpose:
#   Downloads USGS daily streamflow from gauges representing huc4 watersheds.
#   Uses missRanger to impute missing values across all gauges, ensuring complete
#   daily time series (2001-2024).
#
# Inputs:
#   - data/eia_huc4.csv: Crosswalk between EIA plant IDs and huc4 watersheds
#   - data/USGS_Streamgage_huc4.csv: USGS gauge to huc4 mapping
#
# Outputs:
#   - output/huc4_average_flows_imputed.csv: Complete daily flow data by huc4
#   - data/usgs/*.csv: Cached gauge flow data
#
# Notes:
#   - CB Oct 2023: Changed gauge 05587455 to 05587450 (line 140 issue)
#   - Flow data is cached locally to avoid re-downloading
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

library(dataRetrieval)
library(missRanger)

# %% Configuration
start_year = 2001
end_year = 2024

# Input paths
eia_huc4_file = "data/eia_huc4.csv"
usgs_streamgage_file = "data/USGS_Streamgage_huc4.csv"
usgs_cache_dir = "data/usgs"

# Output paths
output_dir = "output"
output_file = "output/huc4_average_flows_imputed.csv"

# %% Load EIA to huc4 crosswalk
eia_and_huc4 =
  read_csv(eia_huc4_file, show_col_types = FALSE)

# %% Download USGS daily flow data
huc4_average_flows =
  usgs_streamgage_file |>
  read_csv(show_col_types = FALSE) |>
  filter(!is.na(StreamGage_FEA1)) |>
  rename_all(tolower) |>
  select(huc4, usgs_id = streamgage_fea1) |>
  pmap_dfr(\(huc4, usgs_id) {
    message(usgs_id)

    gauge_fn = file.path(usgs_cache_dir, sprintf("%s.csv", usgs_id))

    if (!file.exists(gauge_fn)) {
      dir.create(usgs_cache_dir, showWarnings = FALSE)

      data_dl =
        dataRetrieval::readNWISdata(
          sites = usgs_id,
          service = "dv",
          parameterCd = "00060",
          asDateTime = FALSE,
          startDate = sprintf("%s-01-01", start_year),
          endDate = sprintf("%s-12-31", end_year)
        )

      gauge_flow =
        data_dl |>
        as_tibble() |>
        select(
          av_flow_cfs = X_00060_00003,
          date = dateTime
        ) |>
        mutate(
          day = day(date),
          month = month(date),
          year = year(date)
        ) |>
        group_by(day, month, year) |>
        summarise(
          av_flow_cfs = mean(av_flow_cfs, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(huc4 = as.character(!!huc4), usgs_id = !!usgs_id)

      write_csv(gauge_flow, gauge_fn, progress = FALSE)
    } else {
      gauge_flow = read_csv(
        gauge_fn,
        progress = FALSE,
        show_col_types = FALSE,
        col_types = "ddddcc"
      )
    }
    gauge_flow
  })

# %% Format huc4 codes
all_flows =
  huc4_average_flows |>
  mutate(huc4 = if_else(nchar(huc4) == 3, paste0(0, huc4), huc4))

# %% Impute missing flow data
all_flows_wide =
  all_flows |>
  distinct(year, month, day, huc4, usgs_id, .keep_all = TRUE) |>
  pivot_wider(
    id_cols = c(year, month, day),
    names_from = c(huc4, usgs_id),
    values_from = av_flow_cfs
  )

ymd_label =
  all_flows_wide |>
  select(year, month, day)

all_flow_wide_imputed =
  all_flows_wide |>
  select(-year, -month, -day) |>
  missRanger()

all_flow_imputed =
  ymd_label |>
  bind_cols(all_flow_wide_imputed) |>
  pivot_longer(
    -c(year, month, day),
    names_to = c("huc4", "usgs_id"),
    names_sep = "_",
    values_to = "av_flow_cfs"
  ) |>
  arrange(huc4, year, month, day)

# %% Write output
dir.create(output_dir, showWarnings = FALSE)
all_flow_imputed |>
  write_csv(output_file)
