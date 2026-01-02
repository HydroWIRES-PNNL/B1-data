# read_eia_hydro_gen_pre_2001.R
#
# Created by Cameron Bracken, cameron.bracken@pnnl.gov, Dec 31, 2025
#

# %% packages and utility functions
library(conflicted)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(readxl)
library(sf)
library(foreign)
library(zoo)
library(tools)
library(arrow)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

source('utilities.R')

# %% settings
# directory for all data
data_dir = 'data'
# cutoff for excluding low capacity plant, set to zero to include everything
capacity_cutoff_MW <- 0
# which version of rfp to download
rfp_version = '1.1'


# %% get rfp data

## rfp gen data
## get the rectifydplus gen data
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
## v1.0: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## v1.1 https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/
if (rfp_version == '1.1') {
  # get RectifHydPlus version 1.1
  rfp_data_url = "https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_NetGen_MWh_v1.1.csv"
  rfp_fields_url = "https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_v1.1_field_descriptions.csv"
  rfp_data_fn = file.path(data_dir, basename(rfp_data_url))
  rfp_fields_fn = file.path(data_dir, basename(rfp_fields_url))
  if (!file.exists(rfp_data_fn)) {
    download.file(rfp_data_url, rfp_data_fn)
  }
  if (!file.exists(rfp_fields_fn)) {
    download.file(rfp_fields_url, rfp_fields_fn)
  }
} else if (rfp_version == '1.0') {
  # get the rectifydplus 1.0 data
  # hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
  # citation: https://www.nature.com/articles/s41597-025-05323-y
  rfp_data_dir = 'rectifhyd_plus_data' # will be a subdir of data_dir
  rfp_data_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus/RectifHydPlus.zip'
  rfp_data_path = download_unzip_rename(
    url = rfp_data_url,
    zip_fn = basename(rfp_data_url),
    extract_to_dir = data_dir,
    dir_rename_to = rfp_data_dir
  )
}

## get the rectifydplus inputs, which includes older EIA data, thanks Sean!
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
## v1.0: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## v1.1 https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/
if (rfp_version == '1.0') {
  ## version 1.0
  rfp_inputs_dir = 'rectifhyd_plus_1.0_inputs' # will be a subdir of data_dir
  rfp_inputs_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus/RectifHydPlus_inputs.zip'
} else if (rfp_version == '1.1') {
  ## version 1.1
  rfp_inputs_dir = 'rectifhyd_plus_1.1_inputs' # will be a subdir of data_dir
  rfp_inputs_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_inputs.zip'
}
rfp_inputs_zip_fn = rfp_inputs_url |>
  basename() |>
  file_path_sans_ext() |>
  paste0('_', rfp_version, '.zip')
rfp_inputs_path = download_unzip_rename(
  url = rfp_inputs_url,
  zip_fn = rfp_inputs_zip_fn,
  extract_to_dir = data_dir,
  dir_rename_to = rfp_inputs_dir
)

## get the rectifydplus misc, which includes older EIA data, thanks Sean!
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
if (rfp_version == '1.0') {
  ## version 1.0
  rfp_misc_dir = 'rectifhyd_plus_misc_1.0' # will be a subdir of data_dir
  rfp_misc_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus/RectifHydPlus_misc.zip'
} else if (rfp_version == '1.1') {
  ## version 1.1
  rfp_misc_dir = 'rectifhyd_plus_misc_1.1'
  # will be a subdir of data_dirsupl
  rfp_misc_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_misc.zip'
}
rfp_misc_zip_fn = rfp_misc_url |>
  basename() |>
  file_path_sans_ext() |>
  paste0('_', rfp_version, '.zip')
rfp_misc_path = download_unzip_rename(
  url = rfp_misc_url,
  zip_fn = rfp_misc_zip_fn,
  extract_to_dir = data_dir,
  dir_rename_to = rfp_misc_dir
)

## get the rectifydplus supplemental, only in version 1.1
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
if (rfp_version == 1.1) {
  rfp_supl_dir = 'rectifhyd_plus_supplemental' # will be a subdir of data_dir
  # version 1.1
  rfp_supl_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_supplemental.zip'
  rfp_supl_zip_fn = rfp_supl_url |>
    basename() |>
    file_path_sans_ext() |>
    paste0('_', rfp_version, '.zip')
  rfp_supl_path = download_unzip_rename(
    url = rfp_supl_url,
    zip_fn = rfp_supl_zip_fn,
    extract_to_dir = data_dir,
    dir_rename_to = rfp_supl_dir
  )
}

# get the latest rectifydplus code, which includes functions for reading older EIA data, thanks Sean!
# repo: https://code.ornl.gov/turnersw/rectifhydplus

if (rfp_version == '1.1') {
  rfp_code_dir = 'rectifhyd_plus_code_main' # will be a subdir of data_dir
  # rfp_code_dir = 'rectifhyd_plus_code_1.1' # will be a subdir of data_dir
  # main brainch of public code repo
  rfp_code_url = "https://code.ornl.gov/turnersw/rectifhydplus/-/archive/main/rectifhydplus-main.zip"
  # version of code released with the data v 1.1, prefer repo for latest updates
  # rfp_code_url = "https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_code.zip"
} else if (rfp_version == '1.0') {
  rfp_code_dir = 'rectifhyd_plus_code_1.0' # will be a subdir of data_dir
  rfp_code_url = "https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus/RectifHydPlus_code.zip"
}
rfp_code_zip_fn = rfp_code_url |>
  basename() |>
  file_path_sans_ext() |>
  paste0('_', rfp_version, '.zip')
rfp_code_path = download_unzip_rename(
  url = rfp_code_url,
  zip_fn = rfp_code_zip_fn,
  extract_to_dir = data_dir,
  dir_rename_to = rfp_code_dir
)
# remove git folder to avoid confusion, if it exists
unlink(file.path(rfp_code_path, '.git'), recursive = TRUE)


# %% read rectifhyd plus gen data
# use rectifyhplus functions for reading data, thanks Sean!
source(file.path(rfp_code_path, '/R/EIA_xl_readers.R'))
source(file.path(rfp_code_path, '/R/data reading and cleaning.R'))

eia_gen_dir = file.path(data_dir, rfp_inputs_dir, 'EIA/Generation/')
eia_plant_dir = file.path(data_dir, rfp_inputs_dir, 'EIA/Plant/')

# %% Read HILLARI data to find catidate EIA plants
# HILLARI data, it comes with the rfp inputs
# https://hydrosource.ornl.gov/data/datasets/hilarri-v3/
hydro_plant_db = read_HILARRI(
  file.path(rfp_inputs_path, 'HILARRI_v3/HILARRI_v3_preliminary.gpkg'),
  file.path(rfp_code_path, 'data/misc/HILARRI_changes.csv')
)
hydro_plants = hydro_plant_db |>
  # conus only for now
  filter(huc_02 %in% sprintf('%02d', 1:18)) |>
  filter(!is.na(eia_ptid))
target_plants = hydro_plants$eia_ptid |> unique() |> as.integer()

# rectifhyd target plants are based on EIA860, 2022
# rfp_target_plants = identify_target_plants(eia_gen_dir, eia_plant_dir)

# %% read different sources of gen data
# monthly

# read and format hydro data from EIA using rfp function for reading EIA data
# only pulls 1980 to 2022
eia_hydro_gen_monthly_1980_2022_wide =
  get_EIA_monthly_gen(
    eia_gen_dir,
    eia_plant_dir,
    # gnr_dir=eia_gen_dir,
    # plt_dir = eia_plant_dir,
    target_plants
  ) |>
  # now pivot to long form
  # eia_hydro_gen_monthly_1980_2022 = eia_hydro_gen_monthly_1980_2022_wide |>
  pivot_longer(
    all_of(month.abb),
    names_to = 'month',
    values_to = 'net_gen_mwh'
  ) |>
  janitor::clean_names(parsing_option = 3) |>
  mutate(datetime = ym(sprintf('%s-%s', year, month)), data_source = 'eia') |>
  select(
    eia_id,
    datetime,
    net_gen_mwh,
    nameplate_mw = nameplate,
    data_source
  ) |>
  # at least one plant, 3437, has mutiple rows, assume these
  # are multiple generators and add them up
  group_by(eia_id, datetime, data_source) |>
  summarise(net_gen_mwh = sum(net_gen_mwh), net_gen_mwh = sum(nameplate_mw))

# get PUDL data, has data back to 2001
eia_hydro_gen_monthly_pudl = get_eia_monthly_hydro_gen_pudl() |>
  mutate(data_source = 'eia_pudl')

# rfp monthly data
rfp_hydro_gen_monthly = get_rfp_monthly_hydro_gen(rfp_data_fn) |>
  mutate(data_source = 'rfp') |>
  # add capacity data for first and second eia id
  left_join(
    eia_hydro_gen_monthly_1980_2022 |>
      select(eia_id, datetime, nameplate_mw),
    by = join_by(eia_id, datetime)
  ) |>
  left_join(
    eia_hydro_gen_monthly_1980_2022 |>
      select(rfp_eia_id2 = eia_id, datetime, nameplate_mw2 = nameplate_mw),
    by = join_by(datetime, rfp_eia_id2)
  ) |>
  # correct split plants in rfp, the split plants have the combined gen so
  # split based on proportion of nameplate_mw
  # TODO find a better way to do this
  mutate(
    net_gen_mwh = case_when(
      rfp_eia_id_was_split ~ nameplate_mw /
        (nameplate_mw + nameplate_mw2) *
        net_gen_mwh,
      .default = net_gen_mwh
    )
  ) |>
  select(-c(rfp_eia_id2, nameplate_mw2))

# %% read different sources of gen data
## Annual
# use PUDL data, has data back to 2001 in some cases
eia_hydro_gen_annual_pudl = get_eia_annual_hydro_gen_pudl(target_plants) |>
  mutate(data_source = 'eia_pudl')

ferc_hydro_gen_annual = get_ferc_annual_hydro_gen() |>
  mutate(data_source = 'ferc')

rfp_gen_annual = rfp_hydro_gen_monthly |>
  mutate(year = year(datetime)) |>
  group_by(eia_id, year, data_source) |>
  summarise(
    datetime = first(datetime),
    rfp_eia_id_was_split = first(rfp_eia_id_was_split),
    net_gen_mwh = sum(net_gen_mwh, na.rm = T),
    nameplate_mw = first(nameplate_mw)
  )

eia_hydro_gen_annual = eia_hydro_gen_monthly_1980_2022 |>
  mutate(year = year(datetime)) |>
  group_by(eia_id, year, data_source) |>
  summarise(
    datetime = first(datetime),
    net_gen_mwh = sum(net_gen_mwh, na.rm = T),
    nameplate_mw = first(nameplate_mw)
  )

# %%
# Need to aggregate eia ids that are really the same plant
# write out the plants the aggregate later
eia_hydro_gen_annual_pudl |>
  distinct(eia_id, plant_name_eia, plant_id_pudl) |>
  group_by(plant_id_pudl) |>
  mutate(n_eia_ids_to_combine = n()) |>
  filter(n_eia_ids_to_combine > 1) |>
  write_csv(file.path(data_dir, 'eia_ids_to_combine.csv'))


# %% combine data sources
hydro_gen_monthly = eia_hydro_gen_monthly_1980_2022 |>
  bind_rows(eia_hydro_gen_monthly_pudl) |>
  bind_rows(rfp_hydro_gen_monthly) |>
  # drop_na(net_gen_mwh) |>
  mutate(net_gen_mw = net_gen_mwh / (days_in_month(datetime) * 24)) |>
  mutate(data_frequency = 'monthly') |>
  mutate(source_freq = paste0(data_source, '_', data_frequency))
hydro_gen_monthly |>
  write_csv(file.path(data_dir, 'hydro_gen_monthly_eia_pudl_rfp.csv'))

hydro_gen_annual = eia_hydro_gen_annual_pudl |>
  bind_rows(eia_hydro_gen_annual) |>
  bind_rows(ferc_hydro_gen_annual) |>
  bind_rows(rfp_gen_annual) |>
  # drop_na(net_gen_mwh) |>
  mutate(hours_in_year = 365 + (leap_year(datetime) |> as.integer()) * 24) |>
  mutate(net_gen_mw = net_gen_mwh / hours_in_year) |>
  mutate(data_frequency = 'annual') |>
  mutate(source_freq = paste0(data_source, '_', data_frequency))
hydro_gen_annual |>
  write_csv(file.path(data_dir, 'hydro_gen_annual_eia_pudl_rfp.csv'))

# hydro_gen = bind_rows(hydro_gen_monthly, hydro_gen_annual)

#%% diagnostics
message('The following plants have mutiple EIA IDs for one plant:')
eia_hydro_gen_annual_pudl |>
  distinct(eia_id, plant_name_eia, plant_id_pudl) |>
  ungroup() |>
  summarise(n = n(), eia_id = str_flatten_comma(eia_id), .by = plant_id_pudl) |>
  filter(n > 1) |>
  print()

# TODO more checks

# %% plots
multipage_pdf_by_group = function(data, group, fn = 'compare_gen.pdf') {
  pdf(fn, 6, 4, onefile = TRUE)
  data |>
    group_by(!!as.name(group)) |>
    group_split() |>
    map(
      function(hydro_df) {
        title = with(hydro_df, eia_id)
        # browser()
        p = hydro_df |>
          ggplot() +
          geom_line(aes(
            datetime,
            net_gen_mw,
            color = source_freq,
            linetype = source_freq
          )) +
          geom_step(
            aes(datetime, nameplate_mw, color = source_freq),
            linetype = 'dashed',
            size = 0.5
          ) +
          labs(x = '', y = 'Net Hydro Gen [MWh]', title = title) +
          theme_minimal()
        print(p)
      },
      .progress = TRUE
    ) -> shhhh
  dev.off()
  message('Wrote: ', fn)
}

hydro_gen_monthly %>%
  # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
  multipage_pdf_by_group('eia_id', 'figures/compare_gen_monthly.pdf')

hydro_gen_annual %>%
  # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
  multipage_pdf_by_group('eia_id', 'figures/compare_gen_annual.pdf')
