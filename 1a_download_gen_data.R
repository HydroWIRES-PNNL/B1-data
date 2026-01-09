# 0a_download_gen_data.R
#
# Downloads hydropower generation data from multiple sources
#
# Created by Cameron Bracken, cameron.bracken@pnnl.gov, Jan 04, 2026

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')

# ----------------------------------------------------------------------------
# data downloading -----------------------------------------------------------
# ----------------------------------------------------------------------------

# %%
message('\nGetting rectifhydplus data version: ', rfp_version)
## get the rectifydplus gen data
## TODO move this to a separate script or functions
## hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## citation: https://www.nature.com/articles/s41597-025-05323-y
## v1.0: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
## v1.1 https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/
if (rfp_version == numeric_version('1.1')) {
  # get RectifHydPlus version 1.1
  rfp_data_url =
    paste0(
      "https://hydrosource.s3.us-east-2.amazonaws.com/files/",
      "data/datasets/rectifhydplus_v1-1/RectifHydPlus_NetGen_MWh_v1.1.csv"
    )
  rfp_fields_url =
    paste0(
      "https://hydrosource.s3.us-east-2.amazonaws.com/files/data/",
      "datasets/rectifhydplus_v1-1/RectifHydPlus_v1.1_field_descriptions.csv"
    )
  rfp_fields_fn = file.path(data_dir, basename(rfp_fields_url))
  if (!file.exists(rfp_data_fn)) {
    download.file(rfp_data_url, rfp_data_fn)
    message('Downloaded: ', rfp_data_fn)
  }
  if (!file.exists(rfp_fields_fn)) {
    download.file(rfp_fields_url, rfp_fields_fn)
    message('Downloaded: ', rfp_fields_fn)
  }
} else if (rfp_version == numeric_version('1.0')) {
  # get the rectifydplus 1.0 data
  # hydrosource: https://hydrosource.ornl.gov/data/datasets/rectifhydplus/
  # citation: https://www.nature.com/articles/s41597-025-05323-y
  rfp_data_url = paste0(
    'https://hydrosource.s3.us-east-2.amazonaws.com/files/',
    'data/datasets/rectifhydplus/RectifHydPlus.zip'
  )
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
  rfp_inputs_url = paste0(
    'https://hydrosource.s3.us-east-2.amazonaws.com/files/',
    'data/datasets/rectifhydplus/RectifHydPlus_inputs.zip'
  )
} else if (rfp_version == numeric_version('1.1')) {
  ## version 1.1
  rfp_inputs_url = paste0(
    'https://hydrosource.s3.us-east-2.amazonaws.com/files/',
    'data/datasets/rectifhydplus_v1-1/RectifHydPlus_inputs.zip'
  )
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
  rfp_misc_url = paste0(
    'https://hydrosource.s3.us-east-2.amazonaws.com/files/',
    'data/datasets/rectifhydplus/RectifHydPlus_misc.zip'
  )
} else if (rfp_version == numeric_version('1.1')) {
  ## version 1.1
  rfp_misc_url = paste0(
    'https://hydrosource.s3.us-east-2.amazonaws.com/files/',
    'data/datasets/rectifhydplus_v1-1/RectifHydPlus_misc.zip'
  )
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
  # version 1.1
  rfp_supl_url = paste0(
    'https://hydrosource.s3.us-east-2.amazonaws.com/files/data/',
    'datasets/rectifhydplus_v1-1/RectifHydPlus_supplemental.zip'
  )
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
  # version of code released with the data v 1.1
  rfp_code_url = paste0(
    "https://hydrosource.s3.us-east-2.amazonaws.com/files/",
    "data/datasets/rectifhydplus_v1-1/RectifHydPlus_code.zip"
  )
} else if (rfp_version == numeric_version('1.0')) {
  rfp_code_url = paste0(
    "https://hydrosource.s3.us-east-2.amazonaws.com/files/",
    "data/datasets/rectifhydplus/RectifHydPlus_code.zip"
  )
}
if (config::get('rfp_code_checkout_repo')) {
  # main brainch of public code repo, use this to pull in latest updates
  rfp_code_url = paste0(
    "https://code.ornl.gov/turnersw/rectifhydplus/",
    "-/archive/main/rectifhydplus-main.zip"
  )
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


# ----------------------------------------------------------------------------
# HILARRIv3 changes ----------------------------------------------------------
# ----------------------------------------------------------------------------
message('\nHILARRIv3 changes')
# %%
# Include some manual updates to hilarri
# https://code.ornl.gov/turnersw/rectifhydplus/-/raw/main/data/misc/hilarri_changes.csv
# download.file(
#   'https://code.ornl.gov/turnersw/rectifhydplus/-/raw/main/data/misc/HILARRI_changes.csv?ref_type=heads&inline=false',
#   'data/hillarri_changes.csv'
# )
# hilarri_changes = 'data/hillarri_changes.csv'
# hilarri_fn = 'data/HILARRI_v3/HILARRI_v3_preliminary.gpkg'

# data and changes come with the rfp input directory
hilarri_fn = file.path(rfp_inputs_path, 'HILARRI_v3/HILARRI_v3_preliminary.gpkg')
hilarri_changes = file.path(rfp_code_path, 'data/misc/HILARRI_changes.csv')

# could use the function here to do the same thing
# source(file.path(rfp_code_path, '/R/data reading and cleaning.R'))

hilarri_modified = hilarri_fn |>
  st_read(quiet = T) |>
  st_drop_geometry() |>
  left_join(
    read_csv(hilarri_changes, col_types = "c"),
    by = "eia_ptid"
  ) |>
  mutate(
    usgs_gage = if_else(!is.na(usgs_gage_new), usgs_gage_new, usgs_gage)
  ) |>
  select(-usgs_gage_new) |>
  as_tibble() |>
  filter(!is.na(eia_ptid), prjct_type == 'Conventional hydropower') |>
  suppressMessages() |>
  # conus only for now
  # filter(huc_02 %in% sprintf('%02d', 1:18)) |>
  # state filtering is more reliable
  filter(!(state %in% c('AK', 'HI', 'PR', 'DC'))) |>
  filter(!is.na(eia_ptid)) |>
  filter(prjct_type == 'Conventional hydropower')

hilarri_modified |>
  write_csv(s('{data_dir}/{config::get("hilarri_csv")}'))


# ----------------------------------------------------------------------------
# PUDL data ------------------------------------------------------------------
# ----------------------------------------------------------------------------
# TODO download this as well
# %%
pudl_generators_fn = 'data/out_eia__monthly_generators.parquet'
pudl_plants_fn = 'data/out_eia__yearly_plants.parquet'

# ----------------------------------------------------------------------------
# USACE NWD data -------------------------------------------------------------
# ----------------------------------------------------------------------------
message('\nUSACE NWD dam and reservoir data for the Columbia River Basin')

# %%

dam_codes <-
  s('{data_dir}/columbia_plants_eia_id.csv') |>
  read_csv() |>
  rename(dam = usace_name, dam_name = usace)

# if (dam %in% c(
#   'BCL', 'CGR', 'DET',
#   'DEX', 'FOS', 'GPR',
#   'LOS', 'HCR', 'HGH',
#   'LOP', 'LOS'
# )) {
#   data_type <- 'CBT-REV'
# }

# defined in config.yml
#start_year <- 2001
#end_year <- 2024
url_base <- "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
period <- "lookforward=0h0m&startdate=01/01/%s&enddate=01/02/%s" |>
  # pad the end year by a few days to account for any timezone shifts
  sprintf(start_year, end_year + 1)
out_dir <- file.path(data_dir, "usace")

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

# %% download the data
# set up the query strings, some damn use different naming conventions
data_query =
  expand.grid(dam = dam_codes$dam, variable = names(data_string)) |>
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

hourly_output_fn = s(
  '{data_dir}/usace_dam_data_hourly_{start_year}_{end_year}.csv'
)

if (!file.exists(hourly_output_fn)) {
  # pull the data, some of the larger queries (inst forebay)
  # might timeout, if so, just try it again
  cbt_data_hourly =
    data_query |>
    group_by(dam, variable) |>
    group_split() |>
    # split(rownames(.)) |>
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
      output_fn <- s("{out_dir}/{cbt_string}_{start_year}_{end_year}.csv")
      # save the output to a file if the file does not exist, otherwise read from file
      if (!file.exists(output_fn)) {
        message(s('Downloading data from: {url}'))
        x <- read_csv(url, show = F, progress = F)
        write_csv(x, output_fn)
        message(s('Wrote output file: {output_fn}'))
      } else {
        message(s('Reading cached file: {output_fn}'))
        x <- read_csv(output_fn, show = F, progress = F)
      }
      x |>
        rename_all(~ c("datetime_utc", "value")) |>
        mutate(
          datetime_utc = dmy_hm(datetime_utc, tz = 'UTC'),
          cbt_string = cbt_string,
          variable = r$variable,
          dam = r$dam,
          units = r$cbt_units
        )
    }) |>
    bind_rows()
  cbt_data_hourly |> write_csv(hourly_output_fn)
  message(s('Wrote output file: {hourly_output_fn}'))
} else {
  message(s('Reading cached file: {hourly_output_fn}'))
  cbt_data_hourly = read_csv(hourly_output_fn)
}

# aggregate hourly data to daily based on pacific time
# TODO some plants are not in pacific time zone
cbt_data_daily =
  cbt_data_hourly |>
  mutate(
    datetime_pacific = with_tz(datetime_utc, "US/Pacific"),
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
  filter(year >= start_year)

daily_output_fn = file.path(
  data_dir,
  s('usace_dam_data_daily_{start_year}_{end_year}.csv')
)

cbt_data_daily |> write_csv(daily_output_fn)
message(s('Wrote output file: {daily_output_fn}'))

# "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
# "BON.Elev-Forebay.Inst.1Hour.0.CBT-REV:units=ft&headers=true&filename=&timezone=PST&"
# "lookforward=0h0m&startdate=01/01/2001+08:00&enddate=01/25/2024+08:00"
