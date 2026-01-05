# 2a-huc4-streamflow.R
#
# Created by Sean Turner, 2022
#
# Updated by Cameron Bracken, cameron.braken@pnnl.gov,
# 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2024 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2025 Update - Cameron Bracken cameron.bracken@pnnl.gov
#             - Added data imputation

# %%
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)
library(tidyverse)
library(dataRetrieval)
library(missRanger)
library(janitor)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

source('utilities.R')

#%%
check_env_set_usgs_api_key()

# %%
start_year = 1980
end_year = 2024

usgs_data_dir = "data/usgs_huc4_flow"
dir.create(usgs_data_dir, showWarnings = F)

# associate EIA plant code with huc4
# pref Dan's EHA POD dataset, EHA and HILLARI have a lot of issues
# but fall back to HILLARRI v3 with Sean's fixes
# run this script to regenerate the eia_huc4.csv file
# source('data/huc4_plants.R')
hillari = read_csv('data/hillarri_v3.csv') |>
  janitor::clean_names() |>
  mutate(huc4 = substr(huc_12, 1, 4)) |>
  rename(usgs_id_hillari = usgs_gage) |>
  mutate(usgs_id_hillari = str_split_i(usgs_id_hillari, '-', 2))

eia_and_huc4 <- read_csv('data/eia_huc4.csv') |>
  janitor::clean_names() |>
  full_join(
    hillari |>
      select(huc4_hillari = huc4, eia_id = eia_ptid) |>
      drop_na(eia_id),
    by = join_by(eia_id)
  ) |>
  mutate(huc4 = ifelse(is.na(huc4), huc4_hillari, huc4)) |>
  select(-huc4_hillari)


# %%
## CB Oct 2023, had to change line 140 from Data/USGS_Streamgage_huc4.csv
## 05587455 to 05587450, the gage was returning no streamflow data
##
## get USGS daily flow data averges
huc4_flow =
  read_csv("data/USGS_Streamgage_huc4.csv") |>
  clean_names() |>
  mutate(huc4 = sprintf('%04d', huc4)) |>
  select(huc4, usgs_id = stream_gage_fea1) |>
  na.omit() |>
  arrange(huc4) |>
  pmap(function(huc4, usgs_id) {
    gage_fn <- "%s/%s_%s_to_%s.csv" |>
      sprintf(usgs_data_dir, usgs_id, start_year, end_year)

    message(usgs_id, ' ', huc4, ' ', gage_fn)

    if (!file.exists(gage_fn)) {
      gage_flow = get_usgs(
        usgs_id,
        sprintf("%s-01-01", start_year),
        sprintf("%s-12-31", end_year)
      ) |>
        rename(av_flow_cfs = value) |>
        mutate(day = day(date), month = month(date), year = year(date)) |>
        mutate(
          huc4 = sprintf('%04d', as.integer(huc4)),
          usgs_id = sprintf('%08d', as.integer(usgs_id))
        )
      if ((gage_flow |> na.omit() |> nrow()) == 0) {
        # browser()
        warning('No data returned from USGS API, skipping...')
        return(NULL)
      }

      gage_flow |> write_csv(gage_fn, progress = F)
    } else {
      gage_flow <- read_csv(gage_fn, col_types = "ddddcc") |>
        mutate(
          huc4 = sprintf('%04d', as.integer(huc4)),
          usgs_id = sprintf('%08d', as.integer(usgs_id))
        )
    }
    gage_flow
  })
#bind_rows()

#%%
# impute missing flow data to create complete set
huc4_flow_imputed_wide_fn = 'data/huc4_flow_usgs_wide_imputed.csv'
if (!file.exists(huc4_flow_imputed_wide_fn)) {
  huc4_flow_wide =
    huc4_flow |>
    distinct(year, month, day, huc4, usgs_id, .keep_all = T) |>
    pivot_wider(
      id_cols = c(year, month, day),
      names_from = c(huc4, usgs_id),
      values_from = av_flow_cfs
    )

  ymd_label = huc4_flow_wide |> select(year, month, day)

  huc4_flow_wide_imputed_nodate =
    huc4_flow_wide |>
    select(-year, -month, -day) |>
    missRanger()

  huc4_flow_wide_imputed = ymd_label |>
    bind_cols(huc4_flow_wide_imputed_nodate)

  huc4_flow_wide_imputed |>
    write_csv(huc4_flow_imputed_wide_fn)
} else {
  huc4_flow_wide_imputed = read_csv(huc4_flow_imputed_wide_fn)
}

#%% format data to long
huc4_flow_imputed_long_fn = 'data/huc4_flow_usgs_long_imputed.csv'
huc4_flow_imputed = huc4_flow_wide_imputed |>
  pivot_longer(
    -c(year, month, day),
    names_to = c("huc4", "usgs_id"),
    names_sep = "_",
    values_to = "av_flow_cfs"
  ) |>
  arrange(huc4, year, month, day) |>
  mutate(date = sprintf('%s-%s-%s', year, month, day) |> as.Date())

huc4_flow_imputed |> write_csv(huc4_flow_imputed_long_fn)
