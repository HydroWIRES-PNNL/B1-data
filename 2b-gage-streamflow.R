# Name: 2b-gage-streamflow.R
# Description: [B1-data] streamflow data retrieval
#
# Author: D. Broman, PNNL
# Modified by C. Bracken, PNNL, January 2025, v1.4.0 update
#  - Added caching based on sstart and end date to speed up subsequent runs

# %%
library(conflicted)
library(tidyverse)
library(missRanger)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

source("utilities.R")

#%%
check_env_set_usgs_api_key()

# %% GLOBAL SETTINGS
# TODO move to input file
#- start and end dates for data retrieval (in YYYY-MM-DD format)
date_start <- "1980-01-01"
date_end <- "2024-12-31"
#- output directory
dir_data <- "data/flow/"
#- diagnostics directory
dir_diag <- "data/flow/diag/"
#- ResOpsUS time_series_all data directory
dir_resops <- "data/ResOpsUS/time_series_all/"
# TODO track down where this is from originally and add it to the pipeline
sta_list_path <- "data/gage-inputs/flow_to_EIA_crosswalk.csv"

# %%
#- parse station list
sta_list_raw <- read_csv(sta_list_path)

sta_list_proc <- sta_list_raw %>%
  dplyr::filter(update_flag == 1)

# create processing dir
dir.create(file.path(dir_data, "proc"), showWarnings = F)
dir.create(file.path(dir_data, "raw"), showWarnings = F)

# retrieve data from each source and save as file
#- usgs (updated)
sta_list_usgsu <- sta_list_proc %>%
  dplyr::filter(
    !is.na(USGS_ID_turbinerelease) |
      !is.na(USGS_ID_totalrelease) |
      !is.na(USGS_ID_update)
  )

# %%
for (i in 1:nrow(sta_list_usgsu)) {
  #
  row_temp <- sta_list_usgsu[i, ]
  # message(row_temp$EIA_ID)
  message(paste(
    "Starting processing for EIA ID",
    row_temp$EIA_ID,
    "from USGS",
    i,
    'of',
    nrow(sta_list_usgsu)
  ))
  # if (row_temp$EIA_ID == 50546) {
  #   browser()
  # }

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from USGS"
  ))

  #- identify type
  if (!is.na(row_temp$USGS_ID_update)) {
    site_no_raw <- row_temp$USGS_ID_update
    flow_cat <- "representative"
  }

  if (!is.na(row_temp$USGS_ID_totalrelease)) {
    site_no_raw <- row_temp$USGS_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$USGS_ID_turbinerelease)) {
    site_no_raw <- row_temp$USGS_ID_turbinerelease
    flow_cat <- "turbine"
  }

  #- if multiple sites used
  if (grepl("\\-|\\+|\\;", site_no_raw)) {
    site_no_list <- unlist(str_split(site_no_raw, "\\-|\\+|\\;"))
    operator <- ifelse(
      grepl("\\-", site_no_raw),
      "subtract",
      ifelse(grepl("\\+", site_no_raw), "add", "join")
    )

    # download data from each site
    dat_proc <- tibble()
    for (j in 1:length(site_no_list)) {
      site_no <- str_pad(site_no_list[j], width = 8, side = "left", pad = "0")
      dat_raw <- get_usgs(site_no, date_start, date_end)
      dat_raw$site_no <- paste0("s", j)
      dat_proc <- bind_rows(dat_proc, dat_raw)
    }

    #- if two sites are added or subtracted
    if (operator != "join") {
      dat_proc <- dat_proc %>%
        spread(site_no, value) %>%
        group_by(date) %>%
        mutate(
          value = case_when(
            operator == "subtract" ~ s1 - s2,
            operator == "add" ~ s1 + s2,
            .default = s1 + s2
          )
        ) %>%
        dplyr::select(date, value) %>%
        mutate(
          EIA_ID = row_temp$EIA_ID,
          source = "USGS",
          data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
          flow_cat = flow_cat
        )

      #- if two sites are joined together
    } else {
      dat_proc <- dat_proc %>%
        spread(site_no, value) %>%
        mutate(value = ifelse(is.na(s1), s2, s1)) %>%
        dplyr::select(date, value) %>%
        mutate(
          EIA_ID = row_temp$EIA_ID,
          source = "USGS",
          data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
          flow_cat = flow_cat
        )
    }
    #- if single site used
  } else {
    site_no <- str_pad(site_no_raw, width = 8, side = "left", pad = "0")
    dat_raw <- get_usgs(site_no, date_start, date_end)
    dat_proc <- dat_raw %>%
      mutate(
        EIA_ID = row_temp$EIA_ID,
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
sta_list_cdec <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "CDEC" | alt_source_totalrelease == "CDEC"
  )

for (i in 1:nrow(sta_list_cdec)) {
  row_temp <- sta_list_cdec[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from CDEC"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    sta_code_raw <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    sta_code_raw <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }

  #- parse sta code and sens_code
  sta_code_proc <- unlist(str_split(sta_code_raw, "\\;"))
  sta_code <- sta_code_proc[1]
  sens_code <- sta_code_proc[2]
  dur_code <- sta_code_proc[3]

  dat_raw <- get_cdec(
    sta_code,
    sens_code,
    dur_code = dur_code,
    date_start,
    date_end
  )
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "CDEC",
      data_id = paste0(sta_code, ";", sens_code, ";", "D"),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end cdec loop

# %%
#- rise
sta_list_rise <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "RISE" | alt_source_totalrelease == "RISE"
  )

for (i in 1:nrow(sta_list_rise)) {
  row_temp <- sta_list_rise[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from RISE"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    item_id <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    item_id <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }

  dat_raw <- get_rise(item_id, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "RISE",
      data_id = item_id,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end rise loop

# %%
#- cdss
sta_list_cdss <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "CDSS" | alt_source_totalrelease == "CDSS"
  )

for (i in 1:nrow(sta_list_cdss)) {
  row_temp <- sta_list_cdss[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from CDSS"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    sta_abb <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    sta_abb <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }

  dat_raw <- get_cdss(sta_abb, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "CDSS",
      data_id = sta_abb,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end cdss loop

# %%
#- pnh
sta_list_pnh <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "PNH" | alt_source_totalrelease == "PNH"
  )

for (i in 1:nrow(sta_list_pnh)) {
  row_temp <- sta_list_pnh[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from Columbia-Pacific Northwest Hydromet"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    sta_code_raw <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    sta_code_raw <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }
  sta_code_proc <- unlist(str_split(sta_code_raw, "\\;"))
  sta_code <- sta_code_proc[1]
  par_code <- sta_code_proc[2]

  dat_raw <- get_pnh(sta_code, par_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "PNH",
      data_id = paste0(sta_code, ";", par_code),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end pnh loop

# %%
#- mbh
sta_list_mbh <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "MBH" | alt_source_totalrelease == "MBH"
  )

for (i in 1:nrow(sta_list_mbh)) {
  row_temp <- sta_list_mbh[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from Missouri Basin Hydromet"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    sta_code_raw <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    sta_code_raw <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }
  sta_code_proc <- unlist(str_split(sta_code_raw, "\\;"))
  sta_code <- sta_code_proc[1]
  par_code <- sta_code_proc[2]

  # TODO add in support for multiple sites like USGS
  dat_raw <- get_mbh(sta_code, par_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "MBH",
      data_id = paste0(sta_code, ";", par_code),
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end mbh loop

# %%
#- nwd
sta_list_nwd <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "NWD" | alt_source_totalrelease == "NWD"
  )

for (i in 1:nrow(sta_list_nwd)) {
  row_temp <- sta_list_nwd[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from NWD Dataquery"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    item_id_raw <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    item_id_raw <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }

  item_id_proc <- unlist(str_split(item_id_raw, "\\;"))
  item_id <- item_id_proc[1]
  units <- item_id_proc[2]

  #- TODO enhance this logic
  dur_code <- ifelse(grepl("1Day", item_id), "D", "I")

  dat_raw <- get_nwd(item_id, units, dur_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "NWD",
      data_id = item_id,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end nwd loop

# %%
#- RO
sta_list_ro <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "RO" | alt_source_totalrelease == "RO"
  )

for (i in 1:nrow(sta_list_ro)) {
  row_temp <- sta_list_ro[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from ResOpsUS"
  ))

  #- identify type
  if (!is.na(row_temp$alt_ID_totalrelease)) {
    grand_id <- row_temp$alt_ID_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_ID_turbinerelease)) {
    grand_id <- row_temp$alt_ID_turbinerelease
    flow_cat <- "turbine"
  }
  dat_raw <- read_csv(paste0(dir_resops, "ResOpsUS_", grand_id, ".csv"))

  dat_proc <- dat_raw %>%
    dplyr::filter(date >= date_start, date <= date_end) %>%
    mutate(value = outflow * 35.314666212661) %>% # cms to cfs conversion
    dplyr::select(date, value) %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "ResOpsUS",
      data_id = grand_id,
      flow_cat = flow_cat
    )
  dat_proc |> write_csv(fn)
} # end ro loop

# %%
#- usgs (basins)
sta_list_usgsb <- sta_list_raw %>%
  dplyr::filter(update_flag == 0, !is.na(USGS_ID))

for (i in 1:nrow(sta_list_usgsb)) {
  row_temp <- sta_list_usgsb[i, ]

  fn = flow_fn_with_dates(dir_data, row_temp$EIA_ID, date_start, date_end)
  if (file.exists(fn)) {
    next
  }

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$EIA_ID,
    "from USGS"
  ))

  site_no_raw <- row_temp$USGS_ID
  flow_cat <- "basin"

  site_no <- str_pad(site_no_raw, width = 8, side = "left", pad = "0")
  dat_raw <- get_usgs(site_no, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
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
dat_flow <- tibble()
flow_file_list <- list.files(
  file.path(dir_data, 'raw/'),
  pattern = paste0(date_start, '_to_', date_end),
  full.names = T
)

message('Combining flow data.')
dat_flow = flow_file_list |>
  map(
    function(fn) {
      # message(paste("Reading", fn))
      read_csv(fn) |> mutate(data_id = as.character(data_id))
    },
    .progress = T
  ) |>
  bind_rows()

# %%
#- build wide csv or read it in if it exists
imputed_flow_fn = paste0(
  dir_data,
  "/proc/flow_all_eia_hydro_wide_%s_to_%s.csv"
) |>
  sprintf(date_start, date_end)
if (!file.exists(imputed_flow_fn)) {
  dat_flow_wide <- dat_flow %>%
    mutate(EIA_ID = paste0("EIA_", EIA_ID)) %>%
    dplyr::select(date, EIA_ID, value) %>%
    distinct(date, EIA_ID, value) |>
    pivot_wider(id_cols = date, names_from = EIA_ID) |>
    missRanger()
  write_csv(dat_flow_wide, dat_flow_wide)
} else {
  dat_flow_wide = read_csv(imputed_flow_fn)
}

#- write out wide csv

# %%
#- write out tidy (long) csv
imputed_flow_long_fn = paste0(
  dir_data,
  "/proc/flow_all_eia_hydro_long_%s_to_%s.csv"
) |>
  sprintf(date_start, date_end)
dat_flow_wide |>
  pivot_longer(-date, names_to = "EIA_ID") |>
  mutate(EIA_ID = gsub("EIA_", "", EIA_ID)) |>
  write_csv(imputed_flow_long_fn)

# %%
#- create metadata file
dat_flow_meta <- dat_flow %>%
  dplyr::select(-date, -value) %>%
  distinct_all()

write_csv(dat_flow_meta, paste0(dir_data, "/proc/flow_metadata.csv"))

# %%
# ===========================================================
# diagnostic plots
message('Creating diagnostic plots.')
id_list <- unique(dat_flow$EIA_ID)

unique(dat_flow$EIA_ID) |>
  map(id_list, function(id_sel) {
    id_sel <- id_list[i]
    dat_flow_fl <- dat_flow %>%
      dplyr::filter(EIA_ID == id_sel)

    ggplot() +
      geom_line(data = dat_flow_fl, aes(x = date, y = value)) +
      theme_classic() +
      xlab("") +
      ylab("Flow (cfs)") +
      ggtitle(id_sel) +
      theme(text = element_text(family = "AvantGarde"))

    ggsave(paste0(dir_diag, id_sel, "_ts.png"), height = 4, width = 6)
  })
