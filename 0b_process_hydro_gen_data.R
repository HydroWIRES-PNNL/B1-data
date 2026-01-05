# read_eia_hydro_gen_pre_2001.R
#
# Created by Cameron Bracken, cameron.bracken@pnnl.gov, Dec 31, 2025
#

# ----------------------------------------------------------------------------
# setup ----------------------------------------------------------------------
# ----------------------------------------------------------------------------

# %% load conflicted first to avoid warnings
library(conflicted)

# %% renv
## initial setup
# install.packages('renv')
# renv::init()
# vignette("renv")
# renv::snapshot()
library(renv)
renv::load()

# %% packages and utility functions
library(conflicted)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(readxl)
library(sf)
library(tools)
library(arrow)
library(janitor)
library(glue)
# shortcut for string interpolation like pythins f strings
s = glue::glue

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
rfp_version = '1.1' |> numeric_version()
rf_version = '1.3' |> numeric_version()


# output will be filtered to these years
# TODO add filtering everywhere
year_range = 1980:2024
start_year = min(year_range)
end_year = max(year_range)

# %% input data files and data tags
rf_version_tag = rf_version |>
  as.character() |>
  str_replace(fixed('.'), '_') %>%
  s('rf_', .)
rfp_version_tag = rfp_version |>
  as.character() |>
  str_replace(fixed('.'), '_') %>%
  s('rfp_', .)

# TODO download this as well
pudl_generators_fn = 'data/out_eia__monthly_generators.parquet'
pudl_plants_fn = 'data/out_eia__yearly_plants.parquet'

# ----------------------------------------------------------------------------
# data downloading -----------------------------------------------------------
# ----------------------------------------------------------------------------

# %%
message('Getting rectifhydplus data version: ', rfp_version)
## get the rectifydplus gen data
## TODO move this to a separate script or functions
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
## v1.0: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## v1.1 https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/
if (rfp_version == numeric_version('1.1')) {
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
} else if (rfp_version == numeric_version('1.0')) {
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

# %%
## get the rectifydplus inputs, which includes older EIA data, thanks Sean!
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
## v1.0: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## v1.1 https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/
if (rfp_version == numeric_version('1.0')) {
  ## version 1.0
  rfp_inputs_dir = 'rectifhyd_plus_1.0_inputs' # will be a subdir of data_dir
  rfp_inputs_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus/RectifHydPlus_inputs.zip'
} else if (rfp_version == numeric_version('1.1')) {
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

# %%
## get the rectifydplus misc, which includes older EIA data, thanks Sean!
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
if (rfp_version == numeric_version('1.0')) {
  ## version 1.0
  rfp_misc_dir = 'rectifhyd_plus_misc_1.0' # will be a subdir of data_dir
  rfp_misc_url = 'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus/RectifHydPlus_misc.zip'
} else if (rfp_version == numeric_version('1.1')) {
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

# %%
## get the rectifydplus supplemental, only in version 1.1
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
if (rfp_version == numeric_version('1.1')) {
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

# %%
# get the latest rectifydplus code, which includes functions for reading older EIA data, thanks Sean!
# repo: https://code.ornl.gov/turnersw/rectifhydplus

if (rfp_version == numeric_version('1.1')) {
  rfp_code_dir = 'rectifhyd_plus_code_main' # will be a subdir of data_dir
  # rfp_code_dir = 'rectifhyd_plus_code_1.1' # will be a subdir of data_dir
  # main brainch of public code repo, use this to pull in latest updates
  rfp_code_url = "https://code.ornl.gov/turnersw/rectifhydplus/-/archive/main/rectifhydplus-main.zip"
  # version of code released with the data v 1.1
  # rfp_code_url = "https://hydrosource.s3.us-east-2.amazonaws.com/files/data/datasets/rectifhydplus_v1-1/RectifHydPlus_code.zip"
} else if (rfp_version == numeric_version('1.0')) {
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


# %% download rectifhyd
message('Getting rectifhyd data version: ', rf_version)
rf_url =
  if (rf_version == numeric_version('1.4')) {
    'RectifHyd_v1.4.csv'
  } else if (rf_version == numeric_version('1.3')) {
    'https://zenodo.org/records/11584567/files/RectifHyd_v1.3.csv'
  } else if (rf_version == numeric_version('1.2.1')) {
    'https://zenodo.org/records/10011017/files/RectifHyd_v1.2.1.csv'
  } else if (rf_version < numeric_version('1.2.1')) {
    stop('Reading older rf versions than 1.2.1 is not supported.')
  }

rf_fn = file.path(data_dir, basename(rf_url))
if (!file.exists(rf_fn)) {
  download.file(rf_url, rf_fn)
} else {
  message(s('rectifhyd - {rf_fn} exists, delete it to re-download.'))
}


# ---------------------------------------------------------------------------
# data ingest ---------------------------------------------------------------
# ---------------------------------------------------------------------------

# %% Read hilarri and pudl data

# monkey patch the st_read function to hide output when it is
# called by the rfp function
st_read = function(...) {
  sf::st_read(..., quiet = TRUE)
}

# Read HILLARI v3 data to find catidate EIA plants
# HILLARI data, it comes with the rfp inputs
# https://hydrosource.ornl.gov/data/datasets/hilarri-v3/

eha_hydro_plants =
  read_HILARRI(
    file.path(rfp_inputs_path, 'HILARRI_v3/HILARRI_v3_preliminary.gpkg'),
    file.path(rfp_code_path, 'data/misc/HILARRI_changes.csv')
  ) |>
  suppressMessages() |>
  # conus only for now
  # filter(huc_02 %in% sprintf('%02d', 1:18)) |>
  # state filtering is more reliable
  filter(!(state %in% c('AK', 'HI', 'PR', 'DC'))) |>
  filter(!is.na(eia_ptid)) |>
  filter(prjct_type == 'Conventional hydropower')

# Read PUDL (eia) data
# pudl data filtered for just hydro and cleaned up columns
eia_hydro_generators = read_eia_pudl_generators_parquet_hydro(pudl_generators_fn)
# raw pudl data
eia_hydro_plants = read_parquet(pudl_plants_fn)
eia_generators_raw = read_parquet(pudl_generators_fn)
eia_hydro_generators_raw =
  eia_generators_raw |>
  filter(
    energy_source_code_1 == 'WAT',
    prime_mover_code == 'HY'
  )

# %% Determine target hydro plants from hilarri and pudl data
target_eia_ids_hilarri = eha_hydro_plants$eia_ptid |> unique() |> as.integer() |> sort()
target_eia_ids_eia_pudl = eia_hydro_generators_raw$plant_id_eia |>
  unique() |>
  as.integer() |>
  sort()

# these are all either super small (<1MW) or
# TODO figure out a coherent plan for these plants
# these are generally small and/or non operational plants
# but add them anyway for completeness
hilarri_eia_ids_not_in_pudl = setdiff(target_eia_ids_hilarri, target_eia_ids_eia_pudl)
# HILLARI is missing a bunch...
pudl_eia_ids_not_in_hilarri = setdiff(target_eia_ids_eia_pudl, target_eia_ids_hilarri)

# check if any of the eha plant are actually in pudl after
# pre-screening has been done
target_eia_ids_hilarri_additional =
  eia_hydro_generators_raw |>
  # eia_hydro_generators |>
  filter(plant_id_eia %in% pudl_eia_ids_not_in_hilarri) |>
  distinct(plant_id_eia, generator_id, .keep_all = T)

# some eia ids do not have pudl data, but we still want to include them to reconstruct with proxies
target_eia_ids = c(
  target_eia_ids_eia_pudl,
  target_eia_ids_hilarri_additional$plant_id_eia
) |>
  unique() |>
  sort()


# %% setup for filling missing data and combining plants

# all dataes we want to represent
complete_date_seq = seq.Date(
  s('{start_year}-01-01'),
  s('{end_year}-12-01'),
  by = 'month'
)

# some pudl plants have mutiple eia ids
pudl_eia_map_uncombined =
  eia_hydro_generators |>
  filter(eia_id %in% target_eia_ids) |>
  distinct(plant_id_pudl, eia_id)

pudl_eia_map_combined =
  pudl_eia_map_uncombined |>
  arrange(eia_id) |>
  group_by(plant_id_pudl) |>
  summarise(eia_id = str_flatten(eia_id, '_')) |>
  mutate(
    eia_id1 = str_split_i(eia_id, '_', 1),
    eia_id2 = str_split_i(eia_id, '_', 2),
    eia_id3 = str_split_i(eia_id, '_', 3)
  )

message('\nAggregating the following eia ids that have the same pudl id:')
pudl_eia_map_combined |> filter(!is.na(eia_id2)) |> print(n = 100)

# %%  other pudl/eia ids should to be combined since they are only
# split for reporting purposes (see rectifhydplus paper for details)
#
# The combined plants are in identified by the RHPID field when it has
# a forward slashsett
# eg. "id1/it2_desc"
rfpids_combined = read_csv(rfp_data_fn) |>
  filter(str_detect(RHPID, '/')) |>
  distinct(RHPID, .keep_all = T) |>
  select(1)

rfp_eia_pudl_map_combined = tibble(
  eia_id = rfpids_combined |>
    pull(RHPID) |>
    str_split_i('_', 1) |>
    str_replace('/', '_'),
  eia_id1 = eia_id |> str_split_i('_', 1),
  eia_id2 = eia_id |> str_split_i('_', 2)
) |>
  left_join(
    eia_hydro_generators |>
      distinct(plant_id_pudl1 = plant_id_pudl, eia_id1 = eia_id),
    by = join_by(eia_id1)
  ) |>
  left_join(
    eia_hydro_generators |>
      distinct(plant_id_pudl2 = plant_id_pudl, eia_id2 = eia_id),
    by = join_by(eia_id2)
  ) |>
  mutate(plant_id_pudl = paste0(plant_id_pudl1, '_', plant_id_pudl2)) |>
  filter(plant_id_pudl1 != plant_id_pudl2)

rfp_eia_pudl_map_uncombined = rfp_eia_pudl_map_combined |>
  pivot_longer(c(plant_id_pudl1, plant_id_pudl2)) |>
  rename(plant_id_pudl_new = plant_id_pudl, plant_id_pudl = value) |>
  select(-c(name)) |>
  pivot_longer(c(eia_id1, eia_id2)) |>
  rename(eia_id_new = eia_id, eia_id = value) |>
  select(-c(name))

# for testing
# pudl_eia_mapping = pudl_eia_map_uncombined
# rfp_pudl_eia_mapping = rfp_eia_pudl_map_combined

message('\nAggregating the following eia ids that :')
rfp_eia_pudl_map_uncombined |>
  filter(str_detect(plant_id_pudl_new, '_')) |>
  distinct(eia_id_new, .keep_all = TRUE) |>
  print(n = 100)

# %% read gen from eia spreadsheets
# read and format hydro data from EIA using rfp function for reading EIA data
# only pulls 1980 to 2022
message('\nReading and formatting eia spreadsheets.')


eia_raw_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_eia_pudl_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(eia_raw_complete_fn)) {
  eia_hydro_gen_monthly_1980_2022_complete =
    read_eia_spreadsheets_rfp(
      eia_gen_dir,
      eia_plant_dir,
      target_eia_ids
    ) |>
    # ignore warnings about converting character data in generation spreadheets
    suppressWarnings() |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined
    )
  eia_hydro_gen_monthly_1980_2022_complete |>
    write_csv(eia_raw_complete_fn)
} else {
  eia_hydro_gen_monthly_1980_2022_complete = read_csv(eia_raw_complete_fn)
}

# %%
# get PUDL data, has data back to 2001
message('\nReading and formatting pudl eia data.')
eia_pudl_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_eia_pudl_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(eia_pudl_complete_fn)) {
  eia_hydro_gen_monthly_pudl_complete =
    eia_hydro_generators |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined
    )
  eia_hydro_gen_monthly_pudl_complete |>
    write_csv(eia_pudl_complete_fn)
} else {
  eia_hydro_gen_monthly_pudl_complete = read_csv(eia_pudl_complete_fn)
}


#%% rectifhyd (not plus)
message(s('\nReading and formatting rectifhyd data version {rf_version}.'))

rf_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_{rf_version_tag}_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(rf_complete_fn)) {
  rf_hydro_gen_monthly_complete = read_rectifhyd(rf_fn) |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined,
      data_source_name = rf_version_tag
    )
  rf_hydro_gen_monthly_complete |>
    write_csv(rf_complete_fn)
} else {
  rf_hydro_gen_monthly_complete = read_csv(rf_complete_fn)
}


# %% rectifhyd plus
# rfp monthly data
message(s('\nReading and formatting rectifhydplus data version {rfp_version}.'))
rfp_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_{rfp_version_tag}_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(rfp_complete_fn)) {
  rfp_hydro_gen_monthly_complete = get_rfp_monthly_hydro_gen(rfp_data_fn) |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined,
      data_source_name = rfp_version_tag
    )
  rfp_hydro_gen_monthly_complete |>
    write_csv(rfp_complete_fn)
} else {
  rfp_hydro_gen_monthly_complete = read_csv(rfp_complete_fn)
}

# ---------------------------------------------------------------------------
# merging datasets ----------------------------------------------------------
# ---------------------------------------------------------------------------

# %% Combine all the monthly data sources, rfp, rf, eia, eia_pudl
message('\nCombinining data sources.')
hydro_gen_data_complete =
  eia_hydro_gen_monthly_pudl_complete |>
  left_join(
    rfp_hydro_gen_monthly_complete,
    by = join_by(eia_id, datetime),
    suffix = c('.pudl_eia', s('.rfp_{rfp_version}'))
  ) |>
  inner_join(
    eia_hydro_gen_monthly_1980_2022_complete |>
      inner_join(
        rf_hydro_gen_monthly_complete,
        by = join_by(eia_id, datetime),
        suffix = c('.eia', s('.rf_{rf_version}'))
      ),
    by = join_by(datetime, eia_id)
  ) |>
  # data source columns are now redundant
  select(-starts_with('data_source'))

hydro_gen_data_long = bind_rows(
  eia_hydro_gen_monthly_pudl_complete,
  rfp_hydro_gen_monthly_complete,
  eia_hydro_gen_monthly_1980_2022_complete,
  rf_hydro_gen_monthly_complete
) |>
  mutate(
    n_hours = days_in_month(datetime) * 24,
    net_gen_mw = net_gen_mwh / n_hours,
    nameplate_mwh = nameplate_mw * n_hours
  )

# ----------------------------------------------------------------------------
# diagnostics ----------------------------------------------------------------
# ----------------------------------------------------------------------------

#%% diagnostics
message()
message("The following plants have mutiple eia_id's for one plant:")
hydro_gen_data_complete |>
  ungroup() |>
  filter(if_any(starts_with("n_eia_ids_combined"), ~ . > 1)) |>
  distinct(eia_id, across(starts_with("plant_id_pudl"))) |>
  # arrange(eia_id) |>
  print(n = 100)

# %%
message()
message('reftifhyd plus combines eia plants, check these:')
# TODO Check rfp combined plants

rfpids_combined = read_csv(rfp_data_fn) |>
  filter(str_detect(RHPID, '/')) |>
  distinct(RHPID, .keep_all = T)
print(rfpids_combined |> select(1))

rfp_eia_pudl_combined_ids = tibble(
  eia_id = rfpids_combined |>
    pull(RHPID) |>
    str_split_i('_', 1) |>
    str_replace('/', '_'),
  eia_id1 = eia_id |> str_split_i('/', 1),
  eia_id2 = eia_id |> str_split_i('/', 2)
) |>
  left_join(eia_hydro_generators)

#%%
message()
message('If the eia ids were combined properly then this should have no rows:')
hydro_gen_data_complete |>
  group_by(plant_id_pudl.pudl_eia) |>
  mutate(
    has_change_eia = length(unique(n_eia_ids_combined.eia)),
    has_change_pudl_eia = length(unique(n_eia_ids_combined.pudl_eia)),
    has_change_rf = length(unique(n_eia_ids_combined.rf_1.3)),
    has_change_rfp = length(unique(n_eia_ids_combined.rfp_1.1))
  ) |>
  filter(if_any(starts_with("has_change"), ~ . > 1))

# %%
message()
eia_hydro_gen_monthly_rfp =
  read_eia_spreadsheets_rfp(
    eia_gen_dir,
    eia_plant_dir,
    target_eia_ids,
    complete_data = F
  ) |>
  mutate(
    sum_total = jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec,
    diff = sum_total - total,
    pdiff = diff / total
  ) |>
  filter(pdiff > 0.01)
message(
  'rectifhydplus - these annual values dont add up to the monthly totals within 1%:'
)
# TODO look into these and figure out a fix
eia_hydro_gen_monthly_rfp |>
  print(n = 100)

# %% check target plants
# TODO check why these were getting filtered from the pudl eia data
# target_eia_ids_hilarri_additional |>
#   select(
#     plant_id_eia,
#     generator_id,
#     plant_id_pudl,
#     planned_generator_retirement_date,
#     energy_source_code_1,
#     prime_mover_code,
#     generator_operating_date,
#     generator_retirement_date,
#     operational_status,
#     capacity_mw
#   ) |>
#   print(n = 1000)

message(
  'Checking target plants -- pudl should contain every target plant (should return zero rows):'
)
eia_hydro_generators_raw |> filter(!(plant_id_eia %in% target_eia_ids)) |> print()
eia_hydro_generators |> filter(!(plant_id_eia %in% target_eia_ids)) |> print()
hydro_gen_data_complete |> filter(!(plant_id_eia %in% target_eia_ids)) |> print()

message(
  'Checking target plants -- these eia ids are in rfp but not in pudl (should return zero rows):'
)
# rectifhyd target plants are based on EIA860, 2022
rfp_target_eia_ids = identify_target_eia_ids(eia_gen_dir, eia_plant_dir)
rfp_target_eia_ids |> filter(!(EIA_ID %in% target_eia_ids))

# TODO check annual vs monthly values all datasets
# TODO check nameplate vs observed gen
# TODO run more checks

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
            net_gen_mwh,
            color = data_source,
            linetype = data_source
          )) +
          geom_step(
            aes(datetime, nameplate_mw, linetype = data_source),
            linetype = 'solid',
            color = 'black',
            size = 1
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

hydro_gen_data_long %>%
  # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
  multipage_pdf_by_group('eia_id', 'figures/compare_gen_monthly.pdf') |>
  # ggplot warns aout dropping NA values, ignore
  suppressWarnings()


# %% read different sources of annual gen data
# TODO include annual data as well
## Annual
# use PUDL data, has data back to 2001 in some cases
# eia_hydro_gen_annual_pudl = get_eia_annual_hydro_gen_pudl(target_eia_ids) |>
#   mutate(data_source = 'eia_pudl')

# ferc_hydro_gen_annual = get_ferc_annual_hydro_gen() |>
#   mutate(data_source = 'ferc')

# rfp_gen_annual = rfp_hydro_gen_monthly |>
#   mutate(year = year(datetime)) |>
#   group_by(eia_id, year, data_source) |>
#   summarise(
#     datetime = first(datetime),
#     rfp_eia_id_was_split = first(rfp_eia_id_was_split),
#     net_gen_mwh = sum(net_gen_mwh, na.rm = T),
#     nameplate_mw = first(nameplate_mw)
#   )

# eia_hydro_gen_annual = eia_hydro_gen_monthly_1980_2022 |>
#   mutate(year = year(datetime)) |>
#   group_by(eia_id, year, data_source) |>
#   summarise(
#     datetime = first(datetime),
#     net_gen_mwh = sum(net_gen_mwh, na.rm = T),
#     nameplate_mw = first(nameplate_mw)
#   )

hydro_gen_data_long %>%
  mutate(year = year(datetime)) |>
  group_by(eia_id, year, data_source) |>
  summarise(
    net_gen_mwh = agg_na_rm_unless_all_na(net_gen_mwh, sum),
    nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw, max),
    datetime = ym(s('{year}-01'))
  ) |>
  # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
  multipage_pdf_by_group('eia_id', 'figures/compare_gen_annual.pdf') |>
  # ggplot warns aout dropping NA values, ignore
  suppressWarnings()
