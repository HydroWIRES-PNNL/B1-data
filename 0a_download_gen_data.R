# 0a_download_gen_data.R
#
# Downloads hydropower generation data from multiple sources
#
# Created by Cameron Bracken, cameron.bracken@pnnl.gov, Jan 04, 2026

# ----------------------------------------------------------------------------
# setup ----------------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% packages - load conflicted first to avoid warnings
library(conflicted)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(janitor)
library(glue)
# shortcut for string interpolation like pythins f strings
s = glue::glue

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 10000,
  dplyr.summarise.inform = FALSE
)

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


# ----------------------------------------------------------------------------
# USACE NWD data -------------------------------------------------------------
# ----------------------------------------------------------------------------

# %%
dam_codes <- read_csv('data/columbia_plants_eia_id.csv') |>
  rename(dam = usace_name, dam_name = usace)

# if (dam %in% c(
#   'BCL', 'CGR', 'DET',
#   'DEX', 'FOS', 'GPR',
#   'LOS', 'HCR', 'HGH',
#   'LOP', 'LOS'
# )) {
#   data_type <- 'CBT-REV'
# }

start_year <- 2001
end_year <- 2024
url_base <- "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
period <- "lookforward=0h0m&startdate=01/01/%s&enddate=12/31/%s" |>
  sprintf(start_year, end_year)
out_dir <- "data/usace"

dir.create(out_dir, showWarnings = F)

data_string <- c(
  power = ".Power.Total.1Hour.1Hour.",
  forebay = ".Elev-Forebay.Ave.~1Day.1Day.",
  outflow = ".Flow-Out.Ave.1Hour.1Hour.",
  inflow = ".Flow-In.Ave.~1Day.1Day."
)
cbt_type <- c(
  power = "CBT-RAW",
  forebay = "CBT-REV",
  outflow = "CBT-REV",
  inflow = "CBT-REV"
)

cbt_units <- c(
  power = "MW",
  forebay = "ft",
  outflow = "kcfs",
  inflow = "kcfs"
)

willamette_projects <- c(
  "BCL",
  "CGR",
  "DET",
  "DEX",
  "FOS",
  "GPR",
  "HCR",
  "LOP",
  "LOS"
)

# set up the query strings, some damn use different naming conventions
data_query <- expand.grid(dam = dam_codes$dam, variable = names(data_string)) |>
  left_join(dam_codes, by = "dam") |>
  left_join(
    data.frame(
      variable = names(data_string),
      data_string = data_string,
      cbt_type = cbt_type,
      cbt_units = cbt_units
    ),
    by = "variable"
  ) |>
  mutate(
    # Willamettes use a different postfix
    cbt_type = if_else(
      variable == "power" & dam %in% c(willamette_projects, "HGH"),
      "CBT-REV",
      cbt_type
    ),
    cbt_type = if_else(
      variable %in%
        c("inflow", "outflow", "forebay") &
        dam %in% willamette_projects,
      "Best",
      cbt_type
    ),
    # Bonneville only has instantaneous forebay
    data_string = if_else(
      variable == "forebay" & dam == "BON",
      ".Elev-Forebay.Inst.~1Day.0.",
      data_string
    ),
    cbt_type = if_else(
      variable == "forebay" & dam == "BON",
      "CBT-RAW",
      cbt_type
    ),
    # these dams only have daily outflow
    data_string = if_else(
      variable == "outflow" & dam %in% c("DET", "GPR", "LOP"),
      ".Flow-Out.Ave.~1Day.1Day.",
      data_string
    )
  )

# pull the data, some of the larger queries (inst forebay) might timeout, if so, just try it again
data_query %>%
  split(rownames(.)) |>
  map(function(r) {
    cbt_string <- with(r, paste0(dam, data_string, cbt_type))
    url <- paste0(
      url_base,
      cbt_string,
      ":units=",
      r$cbt_units,
      "&headers=true&timezone=GMT&",
      period
    )
    # message(url)
    message(cbt_string)
    output_fn <- sprintf("%s/%s.csv", out_dir, cbt_string)
    # save the output to a file if the file does not exist, otherwise read from file
    if (!file.exists(output_fn)) {
      x <- read_csv(url, show = F, progress = F)
      write_csv(x, output_fn)
    } else {
      x <- read_csv(output_fn, show = F, progress = F)
    }
    x |>
      rename_all(~ c("datetime", "value")) |>
      mutate(
        datetime = dmy_hm(datetime),
        cbt_string = cbt_string,
        variable = r$variable,
        dam = r$dam,
        units = r$cbt_units
      )
  }) |>
  bind_rows() -> cbt_data

cbt_data |>
  mutate(
    datetime_pacific = with_tz(datetime, "US/Pacific"),
    year = year(datetime_pacific),
    month = month(datetime_pacific),
    day = day(datetime_pacific),
    hour = hour(datetime_pacific),
    variable = paste0(variable, "_", units)
  ) |>
  group_by(year, month, day, variable, dam) |>
  summarise(value = mean(value, na.rm = T), .groups = "drop") |>
  select(year, month, day, dam, variable, value) |>
  arrange(dam, variable, year, month, day, value) |>
  left_join(dam_codes, by = join_by(dam)) |>
  filter(year >= start_year) -> cbt_data_daily

write_csv(cbt_data_daily, "data/pnw_daily_data.csv")

# "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
# "BON.Elev-Forebay.Inst.1Hour.0.CBT-REV:units=ft&headers=true&filename=&timezone=PST&"
# "lookforward=0h0m&startdate=01/01/2001+08:00&enddate=01/25/2024+08:00"
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# HILARRIv3 changes ----------------------------------------------------------
# ----------------------------------------------------------------------------
# %%
# Include some manual updates to hillari
# https://code.ornl.gov/turnersw/rectifhydplus/-/raw/main/data/misc/hillari_changes.csv
download.file(
  'https://code.ornl.gov/turnersw/rectifhydplus/-/raw/main/data/misc/HILARRI_changes.csv?ref_type=heads&inline=false',
  'data/hillarri_changes.csv'
)
hillari_changes = 'data/hillarri_changes.csv'

hillari = st_read(hillari_fn) |>
  select(-geom) |>
  left_join(
    read_csv(hillari_changes, col_types = "c"),
    by = "eia_ptid"
  ) |>
  mutate(
    usgs_gage = if_else(!is.na(usgs_gage_new), usgs_gage_new, usgs_gage)
  ) |>
  select(-usgs_gage_new) |>
  as_tibble() |>
  filter(!is.na(eia_ptid), prjct_type == 'Conventional hydropower')

write_csv(hillari, 'data/hillarri_v3.csv')


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
