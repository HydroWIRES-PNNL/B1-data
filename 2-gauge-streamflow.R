# ==============================================================================
# Script: 2-gauge-streamflow.R
# Project: B1-data - Hydropower Generation Constraints Dataset
# ==============================================================================
#
# Purpose:
#   Download daily streamflow/discharge data from multiple data sources for
#   hydropower plants. This flow data is used to disaggregate monthly energy
#   targets to daily resolution based on actual hydrological conditions.
#
# Inputs:
#   - data/gauge-inputs/flow_to_EIA_crosswalk.csv - mapping of EIA plant IDs
#     to gauge stations and data sources
#   - utilities.R - data retrieval functions for each source
#
# Outputs:
#   - data/flow/raw/*_flow.csv - raw daily flow data per plant
#   - data/flow/proc/flow_all_td.csv - combined processed flow data (tidy/long)
#   - data/flow/proc/flow_all_sp.csv - combined processed flow data (wide/spread)
#   - data/flow/proc/flow_metadata.csv - metadata about data sources
#
# Data Sources Supported:
#   - USGS NWIS (via dataRetrieval package)
#   - California CDEC (via cder package)
#   - Reclamation RISE API
#   - Colorado CDSS
#   - Reclamation Pacific Northwest Hydromet (PNH)
#   - Reclamation Missouri Basin Hydromet (MBH)
#   - Army Corps NWD Dataquery
#   - ResOpsUS reservoir release data
#
# Notes:
#   - Flow files are cached; delete data/flow/raw/ to force re-download
#   - Some sources have rate limits or data availability gaps
#   - The flow data is imputed to fill gaps using missRanger
#   - Diagnostic plots are output in the plots/ directory
#
# Authors: D. Broman (daniel.broman@pnnl.gov)
#          C. Bracken (cameron.bracken@pnnl.gov)
#
# Version History:
#   v1.3.0 - DB, May 2024 - initial version with automated downloading
#   v1.4.0 - CB, Jan 2025 - extended through 2024, code cleanup, documentation
# ==============================================================================

# %% Libraries
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
library(missRanger)
library(tidyverse)
source("utilities.R")

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000
)

# ===========================================================
# CONFIGURATION
# %% Date range for data retrieval
date_start <- "2001-01-01"
date_end <- "2024-12-31"

# %% Input paths
sta_list_path <- "data/gauge-inputs/flow_to_EIA_crosswalk.csv"
dir_resops <- "data/ResOpsUS/time_series_all/"

# %% Output paths
dir_data <- "data/flow/"
dir_data_raw <- "data/flow/raw/"
dir_data_proc <- "data/flow/proc/"
plots_dir <- "figures"
plot_file <- "figures/flow_proxies.pdf"
# ===========================================================

# %% parse station list

sta_list_raw <- read_csv(sta_list_path)

sta_list_proc <- sta_list_raw %>%
  dplyr::filter(update_flag == 1)

# create processing dir
dir.create(dir_data_proc, showWarnings = F, recursive = TRUE)
dir.create(dir_data_raw, showWarnings = F, recursive = TRUE)
dir.create(plots_dir, showWarnings = F)

# retrieve data from each source and save as file
# %% usgs (updated)
sta_list_usgsu <- sta_list_proc %>%
  dplyr::filter(
    !is.na(usgs_id_turbinerelease) |
      !is.na(usgs_id_totalrelease) |
      !is.na(usgs_id_update)
  )

for (i in 1:nrow(sta_list_usgsu)) {
  # XXX change back to 1
  row_temp <- sta_list_usgsu[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from USGS"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$usgs_id_update)) {
    site_no_raw <- row_temp$usgs_id_update
    flow_cat <- "representative"
  }

  if (!is.na(row_temp$usgs_id_totalrelease)) {
    site_no_raw <- row_temp$usgs_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$usgs_id_turbinerelease)) {
    site_no_raw <- row_temp$usgs_id_turbinerelease
    flow_cat <- "turbine"
  }

  # %% if multiple sites used
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

    # %% if two sites are added or subtracted
    if (operator != "join") {
      dat_proc <- dat_proc %>%
        spread(site_no, value) %>%
        group_by(date) %>%
        mutate(value = ifelse(operator == "subtract", s1 - s2, s1 + s2)) %>%
        dplyr::select(date, value) %>%
        mutate(
          eia_id = row_temp$eia_id,
          source = "USGS",
          data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
          flow_cat = flow_cat
        )

      # %% if two sites are joined together
    } else {
      dat_proc <- dat_proc %>%
        spread(site_no, value) %>%
        mutate(value = ifelse(is.na(s1), s2, s1)) %>%
        dplyr::select(date, value) %>%
        mutate(
          eia_id = row_temp$eia_id,
          source = "USGS",
          data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
          flow_cat = flow_cat
        )
    }

    write_csv(dat_proc, flow_fn)

    # %% if single site used
  } else {
    site_no <- str_pad(site_no_raw, width = 8, side = "left", pad = "0")
    dat_raw <- get_usgs(site_no, date_start, date_end)
    dat_proc <- dat_raw %>%
      mutate(
        eia_id = row_temp$eia_id,
        source = "USGS",
        data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
        flow_cat = flow_cat
      )
    write_csv(dat_proc, flow_fn)
  } # end site if else
} # end usgs update loop

# %% cdec
sta_list_cdec <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "CDEC" | alt_source_totalrelease == "CDEC"
  )

for (i in 1:nrow(sta_list_cdec)) {
  row_temp <- sta_list_cdec[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from CDEC"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_code_raw <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_code_raw <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }

  # %% parse sta code and sens_code
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
      eia_id = row_temp$eia_id,
      source = "CDEC",
      data_id = paste0(sta_code, ";", sens_code, ";", "D"),
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end cdec loop

# %% rise
sta_list_rise <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "RISE" | alt_source_totalrelease == "RISE"
  )

for (i in 1:nrow(sta_list_rise)) {
  row_temp <- sta_list_rise[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from RISE"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    item_id <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    item_id <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }

  dat_raw <- get_rise(item_id, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "RISE",
      data_id = item_id,
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end rise loop

# %% cdss
sta_list_cdss <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "CDSS" | alt_source_totalrelease == "CDSS"
  )

for (i in 1:nrow(sta_list_cdss)) {
  row_temp <- sta_list_cdss[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from CDSS"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_abb <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_abb <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }

  dat_raw <- get_cdss(sta_abb, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "CDSS",
      data_id = sta_abb,
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end cdss loop

# %% pnh
sta_list_pnh <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "PNH" | alt_source_totalrelease == "PNH"
  )

for (i in 1:nrow(sta_list_pnh)) {
  row_temp <- sta_list_pnh[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from Columbia-Pacific Northwest Hydromet"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_code_raw <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_code_raw <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }
  sta_code_proc <- unlist(str_split(sta_code_raw, "\\;"))
  sta_code <- sta_code_proc[1]
  par_code <- sta_code_proc[2]

  dat_raw <- get_pnh(sta_code, par_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "PNH",
      data_id = paste0(sta_code, ";", par_code),
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end pnh loop

# %% mbh
sta_list_mbh <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "MBH" | alt_source_totalrelease == "MBH"
  )

for (i in 1:nrow(sta_list_mbh)) {
  row_temp <- sta_list_mbh[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from Missouri Basin Hydromet"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    sta_code_raw <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    sta_code_raw <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }
  sta_code_proc <- unlist(str_split(sta_code_raw, "\\;"))
  sta_code <- sta_code_proc[1]
  par_code <- sta_code_proc[2]

  # TODO add in support for multiple sites like USGS
  dat_raw <- get_mbh(sta_code, par_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "MBH",
      data_id = paste0(sta_code, ";", par_code),
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end mbh loop

# %% nwd
sta_list_nwd <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "NWD" | alt_source_totalrelease == "NWD"
  )

for (i in 1:nrow(sta_list_nwd)) {
  row_temp <- sta_list_nwd[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from NWD Dataquery"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    item_id_raw <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    item_id_raw <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }

  item_id_proc <- unlist(str_split(item_id_raw, "\\;"))
  item_id <- item_id_proc[1]
  units <- item_id_proc[2]

  # %% TODO enhance this logic
  dur_code <- ifelse(grepl("1Day", item_id), "D", "I")

  dat_raw <- get_nwd(item_id, units, dur_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "NWD",
      data_id = item_id,
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end nwd loop

# %% RO
sta_list_ro <- sta_list_proc %>%
  dplyr::filter(
    alt_source_turbinerelease == "RO" | alt_source_totalrelease == "RO"
  )

for (i in 1:nrow(sta_list_ro)) {
  row_temp <- sta_list_ro[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from ResOpsUS"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  # %% identify type
  if (!is.na(row_temp$alt_id_totalrelease)) {
    grand_id <- row_temp$alt_id_totalrelease
    flow_cat <- "total"
  }

  if (!is.na(row_temp$alt_id_turbinerelease)) {
    grand_id <- row_temp$alt_id_turbinerelease
    flow_cat <- "turbine"
  }
  dat_raw <- read_csv(paste0(dir_resops, "ResOpsUS_", grand_id, ".csv"))

  dat_proc <- dat_raw %>%
    dplyr::filter(date >= date_start, date <= date_end) %>%
    mutate(value = outflow * 35.314666212661) %>% # cms to cfs conversion
    dplyr::select(date, value) %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "ResOpsUS",
      data_id = grand_id,
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end ro loop

# %% usgs (basins)
sta_list_usgsb <- sta_list_raw %>%
  dplyr::filter(update_flag == 0, !is.na(usgs_id))

for (i in 1:nrow(sta_list_usgsb)) {
  row_temp <- sta_list_usgsb[i, ]

  message(paste(
    "retrieving and processing EIA ID",
    row_temp$eia_id,
    "from USGS"
  ))

  flow_fn = paste0(dir_data, "/raw/", row_temp$eia_id, "_flow.csv")

  if (file.exists(flow_fn)) {
    message("data already exists delete to re-download: ", flow_fn)
    next
  }

  site_no_raw <- row_temp$usgs_id
  flow_cat <- "basin"

  site_no <- str_pad(site_no_raw, width = 8, side = "left", pad = "0")
  dat_raw <- get_usgs(site_no, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      eia_id = row_temp$eia_id,
      source = "USGS",
      data_id = paste0(site_no_raw, ";", "00060", ";", "00003"),
      flow_cat = flow_cat
    )
  write_csv(dat_proc, flow_fn)
} # end usgs basin loop

# ===========================================================
# gather data together in a single dataset

# %% read in data
dat_flow <- tibble()
flow_file_list <- list.files(
  file.path(dir_data, 'raw'),
  pattern = ".csv",
  full.names = T
)

dat_flow =
  flow_file_list |>
  map_dfr(
    \(flow_file) {
      read_csv(flow_file, show_col_types = FALSE) |>
        mutate(data_id = as.character(data_id))
    },
    .progress = T
  )

# %% build spread csv
dat_flow_sp <-
  dat_flow %>%
  mutate(eia_id = paste0("EIA_", eia_id)) %>%
  dplyr::select(date, eia_id, value) %>%
  # CB should be none or very few dupes, when I checked last
  # there was only one dupe:
  # # A tibble: 1 × 3
  #   date       eia_id      n
  #   <date>     <chr>   <int>
  # 1 2024-10-30 EIA_516     2
  distinct(date, eia_id, .keep_all = T) |>
  pivot_wider(id_cols = date, names_from = eia_id) |>
  missRanger()
# spread(eia_id, value, -date)

# %% write out spread csv
write_csv(dat_flow_sp, paste0(dir_data, "/proc/flow_all_sp.csv"))

# %% write out tidy (long) csv
dat_flow_imputed_long =
  dat_flow_sp |>
  pivot_longer(-date, names_to = "eia_id") |>
  mutate(eia_id = gsub("EIA_", "", eia_id) |> as.numeric()) |>
  right_join(
    dat_flow,
    by = join_by(date, eia_id),
    suffix = c('', '_unimputed')
  ) |>
  mutate(imputed = ifelse(is.na(value_unimputed), TRUE, FALSE))

dat_flow_imputed_long |>
  write_csv(paste0(dir_data, "/proc/flow_all_td.csv"))

# %% create metadata file
dat_flow_meta <- dat_flow %>%
  dplyr::select(-date, -value) %>%
  distinct()

write_csv(dat_flow_meta, paste0(dir_data, "/proc/flow_metadata.csv"))

# ===========================================================
# diagnostic plots
multipage_pdf_timeseries_by_group_with_source(
  dat_flow_imputed_long |> rename(datetime = date),
  'eia_id',
  y_var = 'value',
  y_lab = 'Flow [cfs]',
  plot_y_step = FALSE,
  fn = plot_file,
  data_source_col = 'flow_cat',
  show_imputed = TRUE
)
