# Name: 2b-gage-streamflow.R
# Description: [B1-data] streamflow data retrieval
#
# Author: D. Broman, PNNL
# Modified by C. Bracken, PNNL, January 2025, v1.4.0 update
#  - Added caching based on start and end date to speed up subsequent runs

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')


# %% GLOBAL SETTINGS
# TODO move to input file
#- start and end dates for data retrieval (in YYYY-MM-DD format)
date_start = s("{start_year}-01-01")
date_end = s("{end_year}-12-31")
#- output directory
dir_data = config::get('eia_flow_dir') #"data/flow/"
#- diagnostics directory
dir_diag = file.path(dir_data, '/diag') #"data/flow/diag/"
dir_proc = file.path(dir_data, '/proc') #"data/flow/diag/"
#- ResOpsUS time_series_all data directory
dir_resops = file.path(resops_flow_dir, '/time_series_all')
# TODO track down where this is from originally and add it to the pipeline
sta_list_path = "data/gage-inputs/flow_to_EIA_crosswalk.csv"

#%%
# output fns
imputed_flow_fn =
  s("{dir_proc}/flow_all_eia_hydro_wide_{start_year}_to_{end_year}.csv")
imputed_flow_long_fn =
  s("{dir_proc}/flow_all_eia_hydro_long_{start_year}_to_{end_year}.csv")
metadata_fn = s("{dir_proc}/flow_metadata.csv")

# %%
#- parse station list
sta_list_raw = read_csv(sta_list_path) |>
  clean_names()

sta_list_proc = sta_list_raw |>
  dplyr::filter(update_flag == 1)

# create processing dir
dir.create(file.path(dir_data, "proc"), showWarnings = F)
dir.create(file.path(dir_data, "raw"), showWarnings = F)

# retrieve data from each source and save as file
#- usgs (updated)
sta_list_usgsu = sta_list_proc |>
  dplyr::filter(
    !is.na(usgs_id_turbinerelease) |
      !is.na(usgs_id_totalrelease) |
      !is.na(usgs_id_update)
  )

# next_if_file_exists = function(fn) {
#   if (file.exists(fn)) {
#     message('Not downloading, cached file exists: ', fn)
#     next
#   }
# }

# %%
# sta_list_usgsu = sta_list_usgsu |> filter(eia_id == 4229)
for (i in 1:nrow(sta_list_usgsu)) {
  #
  row_temp = sta_list_usgsu[i, ]
  # message(row_temp$eia_id)
  message(paste(
    "Starting processing for EIA ID",
    row_temp$eia_id,
    "from USGS",
    i,
    'of',
    nrow(sta_list_usgsu)
  ))
  # if (row_temp$eia_id == 50546) {
  #   browser()
  # }

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from USGS"
  ))

  #- identify type
  if (!is.na(row_temp$usgs_id_update)) {
    site_no_raw = row_temp$usgs_id_update
    flow_cat = "representative"
  }

  if (!is.na(row_temp$usgs_id_totalrelease)) {
    site_no_raw = row_temp$usgs_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$usgs_id_turbinerelease)) {
    site_no_raw = row_temp$usgs_id_turbinerelease
    flow_cat = "turbine"
  }

  #- if multiple sites used
  if (grepl("\\-|\\+|\\;", site_no_raw)) {
    site_no_list = unlist(str_split(site_no_raw, "\\-|\\+|\\;"))
    operator = ifelse(
      grepl("\\-", site_no_raw),
      "subtract",
      ifelse(grepl("\\+", site_no_raw), "add", "join")
    )

    # download data from each site
    dat_raw_list = list()
    for (j in 1:length(site_no_list)) {
      site_no = str_pad(site_no_list[j], width = 8, side = "left", pad = "0")
      dat_raw_list[[j]] = get_usgs(site_no, date_start, date_end)
      dat_raw_list[[j]]$site_no = paste0("s", j)
      # dat_raw_list[[j]] = dat_raw
      # dat_proc = bind_rows(dat_proc, dat_raw)
    }
    dat_raw = bind_rows(dat_raw_list)

    if (operator != "join") {
      #- two sites are added or subtracted
      dat_proc =
        dat_raw |>
        group_by(date) |>
        summarise(
          value = case_when(
            operator == "subtract" ~ value[1] - value[2],
            # TODO should be missing values be excluded (i.e. assumed zero)?
            operator == "add" ~ sum(value, na.rm = T),
            .default = sum(value, na.rm = T)
          )
        ) |>
        dplyr::select(date, value) |>
        mutate(
          eia_id = row_temp$eia_id,
          source = "USGS",
          data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
          flow_cat = flow_cat
        )
    } else {
      # browser()
      #- two sites are joined together
      # this probably means that a gage was renamed or moved but can still be used
      dat_proc =
        dat_raw |>
        pivot_wider(id_cols = c(date), names_from = site_no) |>
        # spread(site_no, value) |>
        mutate(value = ifelse(is.na(s1), s2, s1)) |>
        dplyr::select(date, value) |>
        mutate(
          eia_id = row_temp$eia_id,
          source = "USGS",
          data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
          flow_cat = flow_cat
        )
    }
  } else {
    #- if single site used
    site_no = str_pad(site_no_raw, width = 8, side = "left", pad = "0")
    dat_raw = get_usgs(site_no, date_start, date_end)
    dat_proc = dat_raw |>
      mutate(
        eia_id = row_temp$eia_id,
        source = "USGS",
        data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
        flow_cat = flow_cat
      )
  } # end site if else
  # write the data to a file
  dat_proc |> write_csv(fn)
} # end usgs update loop

# %%
#- cdec
sta_list_cdec = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "CDEC" | alt_source_totalrelease == "CDEC"
  )

for (i in 1:nrow(sta_list_cdec)) {
  row_temp = sta_list_cdec[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from CDEC"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_code_raw = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_code_raw = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }

  #- parse sta code and sens_code
  sta_code_proc = unlist(str_split(sta_code_raw, "\\;"))
  sta_code = sta_code_proc[1]
  sens_code = sta_code_proc[2]
  dur_code = sta_code_proc[3]

  dat_raw = get_cdec(
    sta_code,
    sens_code,
    dur_code = dur_code,
    date_start,
    date_end
  )
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "CDEC",
      data_id = paste0(sta_code, ";", sens_code, ";", "D"),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end cdec loop

# %%
#- rise
sta_list_rise = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "RISE" | alt_source_totalrelease == "RISE"
  )

for (i in 1:nrow(sta_list_rise)) {
  row_temp = sta_list_rise[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from RISE"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    item_id = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    item_id = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }

  dat_raw = get_rise(item_id, date_start, date_end)
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "RISE",
      data_id = item_id,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end rise loop

# %%
#- cdss
sta_list_cdss = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "CDSS" | alt_source_totalrelease == "CDSS"
  )

for (i in 1:nrow(sta_list_cdss)) {
  row_temp = sta_list_cdss[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from CDSS"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_abb = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_abb = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }

  dat_raw = get_cdss(sta_abb, date_start, date_end)
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "CDSS",
      data_id = sta_abb,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end cdss loop

# %%
#- pnh
sta_list_pnh = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "PNH" | alt_source_totalrelease == "PNH"
  )

for (i in 1:nrow(sta_list_pnh)) {
  row_temp = sta_list_pnh[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from Columbia-Pacific Northwest Hydromet"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_code_raw = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_code_raw = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }
  sta_code_proc = unlist(str_split(sta_code_raw, "\\;"))
  sta_code = sta_code_proc[1]
  par_code = sta_code_proc[2]

  dat_raw = get_pnh(sta_code, par_code, date_start, date_end)
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "PNH",
      data_id = paste0(sta_code, ";", par_code),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end pnh loop

# %%
#- mbh
sta_list_mbh = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "MBH" | alt_source_totalrelease == "MBH"
  )

for (i in 1:nrow(sta_list_mbh)) {
  row_temp = sta_list_mbh[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }
  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from Missouri Basin Hydromet"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_code_raw = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_code_raw = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }
  sta_code_proc = unlist(str_split(sta_code_raw, "\\;"))
  sta_code = sta_code_proc[1]
  par_code = sta_code_proc[2]

  # TODO add in support for multiple sites like USGS
  dat_raw = get_mbh(sta_code, par_code, date_start, date_end)
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "MBH",
      data_id = paste0(sta_code, ";", par_code),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end mbh loop

# %%
#- nwd
sta_list_nwd = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "NWD" | alt_source_totalrelease == "NWD"
  )

for (i in 1:nrow(sta_list_nwd)) {
  row_temp = sta_list_nwd[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from NWD Dataquery"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    item_id_raw = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    item_id_raw = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }

  item_id_proc = unlist(str_split(item_id_raw, "\\;"))
  item_id = item_id_proc[1]
  units = item_id_proc[2]

  #- TODO enhance this logic
  dur_code = ifelse(grepl("1Day", item_id), "D", "I")

  dat_raw = get_nwd(item_id, units, dur_code, date_start, date_end)
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "NWD",
      data_id = item_id,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end nwd loop

# %%
#- RO
sta_list_ro = sta_list_proc |>
  dplyr::filter(
    alt_source_turbinerelease == "RO" | alt_source_totalrelease == "RO"
  )

for (i in 1:nrow(sta_list_ro)) {
  row_temp = sta_list_ro[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from ResOpsUS"
  ))

  #- identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    grand_id = row_temp$alt_id_totalrelease
    flow_cat = "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    grand_id = row_temp$alt_id_turbinerelease
    flow_cat = "turbine"
  }
  dat_raw = read_csv(paste0(dir_resops, "/ResOpsUS_", grand_id, ".csv"))

  dat_proc = dat_raw |>
    dplyr::filter(date >= date_start, date <= date_end) |>
    mutate(value = outflow * 35.314666212661) |> # cms to cfs conversion
    dplyr::select(date, value) |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "ResOpsUS",
      data_id = grand_id,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end ro loop

# %%
#- usgs (basins)
sta_list_usgsb = sta_list_raw |>
  dplyr::filter(update_flag == 0, !is.na(usgs_id))

for (i in 1:nrow(sta_list_usgsb)) {
  row_temp = sta_list_usgsb[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$eia_id, date_start, date_end)

  if (file.exists(fn)) {
    message('Not downloading, cached file exists: ', fn)
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from USGS"
  ))

  site_no_raw = row_temp$usgs_id
  flow_cat = "basin"

  site_no = str_pad(site_no_raw, width = 8, side = "left", pad = "0")
  dat_raw = get_usgs(site_no, date_start, date_end)
  dat_proc = dat_raw |>
    mutate(
      eia_id = row_temp$eia_id,
      source = "USGS",
      data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end usgs basin loop

# %%
# ===========================================================
# gather data together in a single dataset
#- read in data
dat_flow = tibble()
flow_file_list = list.files(
  file.path(dir_data, 'raw/'),
  pattern = paste0(date_start, '_to_', date_end),
  full.names = T
)

message('Combining flow data.')
dat_flow =
  flow_file_list |>
  map(
    function(fn_in) {
      message(paste("Reading", fn_in))
      read_csv(fn_in) |> mutate(data_id = as.character(data_id))
    },
    .progress = T
  ) |>
  bind_rows() |>
  # TODO find out why there are dupe rows
  distinct_all() |>
  clean_names()

# %%
# TODO identify and remove bad values before imputing

#- build wide csv or read it in if it exists
if (!file.exists(imputed_flow_fn)) {
  message('Imputing missing gage flows.')
  dat_flow_wide_imputed =
    dat_flow |>
    mutate(eia_id = paste0("eia_", eia_id)) |>
    dplyr::select(date, eia_id, value) |>
    # distinct_all() |>
    pivot_wider(id_cols = date, names_from = eia_id) |>
    missRanger()
  write_csv(dat_flow_wide_imputed, imputed_flow_fn)
  message(s('Wrote file: {imputed_flow_fn}'))
} else {
  message(s('Reading cached file: {imputed_flow_fn}'))
  dat_flow_wide_imputed = read_csv(imputed_flow_fn)
}

# %%
#- write out tidy (long) csv
dat_flow_long =
  dat_flow_wide_imputed |>
  pivot_longer(-date, names_to = "eia_id") |>
  mutate(eia_id = gsub("eia_", "", eia_id) |> as.numeric()) |>
  rename(ave_flow_cfs = value) |>
  left_join(dat_flow, by = join_by(date, eia_id)) |>
  # set the source to imputed if no observed data
  mutate(source = ifelse(is.na(value), 'imputed_gage_flow', source)) |>
  select(-c(value, unit_of_measure)) |>
  rename(data_source = source)

dat_flow_long |>
  write_csv(imputed_flow_long_fn)

# %%
#- create metadata file
# dat_flow_meta =
#   dat_flow_long |>
#   select(-date, -ave_flow_cfs) |>
#   distinct_all()

# write_csv(dat_flow_meta, metadata_fn)

# %%
# ===========================================================
# diagnostic plots
if (create_figures) {
  message('Creating diagnostic plots.')

  dat_flow_long |>
    rename(datetime = date) |>
    multipage_pdf_timeseries_by_group_with_source(
      group = 'eia_id',
      y_var = 'ave_flow_cfs',
      y_lab = 'Daily Average Flow [cfs]',
      plot_y_step = FALSE,
      fn = file.path(figures_dir, 'gage_flow_by_eia_id.pdf')
    )

  p_sources =
    dat_flow_long |>
    group_by(date, data_source) |>
    summarise(n = n()) |>
    ggplot() +
    geom_area(aes(date, n, fill = data_source)) +
    theme_minimal() +
    scale_fill_paletteer_d("pals::kelly", direction = -1, name = 'Data Source') +
    labs(y = 'Number of gages')
  file.path(figures_dir, 'gage_flow_data_sources.pdf') |>
    ggsave(p_sources, width = 10, height = 5)

  message(s('Wrote diagnostics plots to: {figures_dir}'))
}
