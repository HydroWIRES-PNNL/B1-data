# 2a-huc4-streamflow.R
#
# Created by Sean Turner, 2022
#
# Updated by Cameron Bracken, cameron.braken@pnnl.gov,
# 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2024 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2025 Update - Cameron Bracken cameron.bracken@pnnl.gov
#             - Added data imputation

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')

# %% additonal setup
check_env_set_usgs_api_key()

usgs_data_dir = config::get('usgs_data_dir')
dir.create(usgs_data_dir, showWarnings = F)

# associate EIA plant code with huc4
# pref Dan's EHA POD dataset, EHA and HILLARI have a lot of issues
# but fall back to HILLARRI v3 with Sean's fixes
# run this script to regenerate the eia_huc4.csv file
# source('data/huc4_plants.R')

# Read HILLARI v3 data which has been pre-filtered for just CONUS hydro plants
# see downloading script
hillari = file.path(data_dir, config::get('hilarri_csv')) |>
  read_csv() |>
  janitor::clean_names() |>
  mutate(huc4 = substr(huc_12, 1, 4)) |>
  rename(usgs_id_hillari = usgs_gage) |>
  mutate(usgs_id_hillari = str_split_i(usgs_id_hillari, '-', 2))

# eha = read_xlsx(eha_fn, sheet = "Operational") |>
#   janitor::clean_names(parsing_option = 3)

eia_and_huc4 <- read_csv('data/eia_huc4.csv') |>
  distinct_all() |>
  janitor::clean_names() |>
  full_join(
    hillari |>
      select(huc4_hillari = huc4, eia_id = eia_ptid) |>
      drop_na(eia_id) |>
      distinct(eia_id, .keep_all = T),
    by = join_by(eia_id)
  ) |>
  mutate(huc4 = ifelse(is.na(huc4), huc4_hillari, huc4)) |>
  select(-huc4_hillari)


# %%
## get USGS daily flow data averges
huc4_flow_daily =
  read_csv("data/USGS_Streamgage_huc4.csv") |>
  clean_names() |>
  mutate(huc4 = sprintf('%04d', huc4)) |>
  select(huc4, usgs_id = stream_gage_fea1) |>
  bind_rows(
    hillari |>
      filter(!is.na(usgs_id_hillari)) |>
      select(huc4, usgs_id = usgs_id_hillari)
  ) |>
  na.omit() |>
  arrange(huc4) |>
  pmap_dfr(function(huc4, usgs_id) {
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
      gage_flow <- read_csv(gage_fn) |> #, col_types = "ddddcc") |>
        mutate(
          huc4 = sprintf('%04d', as.integer(huc4)),
          usgs_id = sprintf('%08d', as.integer(usgs_id))
        )
    }
    gage_flow
  })

#%% monthly
# impute missing flow data to create complete set
if (!file.exists(huc4_flow_imputed_monthly_wide_fn)) {
  huc4_flow_wide =
    huc4_flow_daily |>
    group_by(year, month, huc4, usgs_id) |>
    summarise(av_flow_cfs = mean(av_flow_cfs)) |>
    # distinct(year, month, day, huc4, usgs_id, .keep_all = T) |>
    pivot_wider(
      id_cols = c(year, month),
      names_from = c(huc4, usgs_id),
      values_from = av_flow_cfs
    ) |>
    ungroup()

  ymd_label = huc4_flow_wide |> select(year, month)

  huc4_flow_wide_imputed_nodate =
    huc4_flow_wide |>
    select(-starts_with('year'), -starts_with('month')) |>
    missRanger() #num.trees = 50)

  huc4_flow_wide_imputed = ymd_label |>
    bind_cols(huc4_flow_wide_imputed_nodate)

  huc4_flow_wide_imputed |>
    write_csv(huc4_flow_imputed_monthly_wide_fn)
  message('Wrote imputed huc4 flows: ', huc4_flow_imputed_monthly_wide_fn)
} else {
  huc4_flow_wide_imputed = read_csv(huc4_flow_imputed_monthly_wide_fn)
  message('Read cached imputed huc4 flows: ', huc4_flow_imputed_monthly_wide_fn)
}

#%% format data to long
huc4_flow_monthly_imputed =
  huc4_flow_wide_imputed |>
  pivot_longer(
    -c(year, month),
    names_to = c("huc4", "usgs_id"),
    names_sep = "_",
    values_to = "av_flow_cfs"
  ) |>
  arrange(huc4, year, month) |>
  mutate(date = sprintf('%s-%s-01', year, month) |> as.Date())

huc4_flow_monthly_imputed |> write_csv(huc4_flow_imputed_monthly_fn)

#%% impute daily (takes a long time)
# impute missing flow data to create complete set
if (!file.exists(huc4_flow_imputed_daily_wide_fn)) {
  huc4_flow_wide =
    huc4_flow_daily |>
    group_by(year, month, day, huc4, usgs_id) |>
    summarise(av_flow_cfs = mean(av_flow_cfs)) |>
    distinct(year, month, day, huc4, usgs_id, .keep_all = T) |>
    pivot_wider(
      id_cols = c(year, month, day),
      names_from = c(huc4, usgs_id),
      values_from = av_flow_cfs
    ) |>
    ungroup()

  ymd_label = huc4_flow_wide |> select(year, month, day)

  huc4_flow_wide_imputed_nodate =
    huc4_flow_wide |>
    select(-year, -month, -day) |>
    missRanger() #num.trees = 50)

  huc4_flow_wide_imputed = ymd_label |>
    bind_cols(huc4_flow_wide_imputed_nodate)

  huc4_flow_wide_imputed |>
    write_csv(huc4_flow_imputed_daily_wide_fn)
  message('Wrote imputed huc4 flows: ', huc4_flow_imputed_daily_wide_fn)
} else {
  huc4_flow_wide_imputed = read_csv(huc4_flow_imputed_daily_wide_fn)
  message('Read cached imputed huc4 flows: ', huc4_flow_imputed_daily_wide_fn)
}

#%% format data to long
huc4_flow_daily_imputed =
  huc4_flow_wide_imputed |>
  pivot_longer(
    -c(year, month, day),
    names_to = c("huc4", "usgs_id"),
    names_sep = "_",
    values_to = "av_flow_cfs"
  ) |>
  arrange(huc4, year, month, day) |>
  mutate(date = sprintf('%s-%s-%s', year, month, day) |> as.Date())

huc4_flow_daily_imputed |> write_csv(huc4_flow_imputed_daily_fn)
