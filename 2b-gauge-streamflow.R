# ===========================================================
# Name: 2a-streamflow.R
# Author: D. Broman, PNNL
# Last Modified: 2024-04-24
# Description: [B1-data] streamflow data retrieval
# ===========================================================
source("utilities.R")

# ===========================================================
# GLOBAL SETTINGS
# TODO move to input file
#- start and end dates for data retrieval (in YYYY-MM-DD format)
date_start <- "2001-01-01"
date_end <- "2023-12-31"
#- output directory
dir_data <- "data/flow"
#- ResOpsUS time_series_all data directory
dir_resops <- "/Volumes/data/ResOpsUS/time_series_all/"
# ===========================================================

#- parse station list
sta_list_path <- "gauge-inputs/flow_to_EIA_crosswalk.csv"

sta_list_raw <- read_csv(sta_list_path)

sta_list_proc <- sta_list_raw %>%
  dplyr::filter(update_flag == 1)

# create processing dir
dir.create(file.path(dir_data, "proc"), showWarnings = F)

# retrieve data from each source and save as file
#- usgs (updated)
sta_list_usgsu <- sta_list_proc %>%
  dplyr::filter(!is.na(USGS_ID_turbinerelease) | !is.na(USGS_ID_totalrelease) | !is.na(USGS_ID_update))

for (i in 1:nrow(sta_list_usgsu)) { # XXX change back to 1
  row_temp <- sta_list_usgsu[i, ]

  message(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from USGS"))

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
    operator <- ifelse(grepl("\\-", site_no_raw), "subtract",
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
        mutate(value = ifelse(operator == "subtract", s1 - s2, s1 + s2)) %>%
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

    write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))

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
    write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
  } # end site if else
} # end usgs update loop

#- cdec
sta_list_cdec <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "CDEC" | alt_source_totalrelease == "CDEC")

for (i in 1:nrow(sta_list_cdec)) {
  row_temp <- sta_list_cdec[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from CDEC"))

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

  dat_raw <- get_cdec(sta_code, sens_code, dur_code = dur_code, date_start, date_end)
  dat_proc <- dat_raw %>%
    mutate(
      EIA_ID = row_temp$EIA_ID,
      source = "CDEC",
      data_id = paste0(sta_code, ";", sens_code, ";", "D"),
      flow_cat = flow_cat
    )
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end cdec loop

#- rise
sta_list_rise <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "RISE" | alt_source_totalrelease == "RISE")

for (i in 1:nrow(sta_list_rise)) {
  row_temp <- sta_list_rise[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from RISE"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end rise loop

#- cdss
sta_list_cdss <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "CDSS" | alt_source_totalrelease == "CDSS")

for (i in 1:nrow(sta_list_cdss)) {
  row_temp <- sta_list_cdss[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from CDSS"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end cdss loop

#- pnh
sta_list_pnh <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "PNH" | alt_source_totalrelease == "PNH")

for (i in 1:nrow(sta_list_pnh)) {
  row_temp <- sta_list_pnh[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from Columbia-Pacific Northwest Hydromet"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end pnh loop

#- mbh
sta_list_mbh <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "MBH" | alt_source_totalrelease == "MBH")

for (i in 1:nrow(sta_list_mbh)) {
  row_temp <- sta_list_mbh[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from Missouri Basin Hydromet"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end mbh loop

#- nwd
sta_list_nwd <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "NWD" | alt_source_totalrelease == "NWD")

for (i in 1:nrow(sta_list_nwd)) {
  row_temp <- sta_list_nwd[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from NWD Dataquery"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end nwd loop

#- RO
sta_list_ro <- sta_list_proc %>%
  dplyr::filter(alt_source_turbinerelease == "RO" | alt_source_totalrelease == "RO")

for (i in 1:nrow(sta_list_ro)) {
  row_temp <- sta_list_ro[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from ResOpsUS"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end ro loop

#- usgs (basins)
sta_list_usgsb <- sta_list_raw %>%
  dplyr::filter(update_flag == 0, !is.na(USGS_ID))

for (i in 1:nrow(sta_list_usgsb)) {
  row_temp <- sta_list_usgsb[i, ]

  print(paste("retrieving and processing EIA ID", row_temp$EIA_ID, "from USGS"))

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
  write_csv(dat_proc, paste0(dir_data, row_temp$EIA_ID, "_flow.csv"))
} # end usgs basin loop

# ===========================================================
# gather data together in a single dataset

#- read in data
dat_flow <- tibble()
flow_file_list <- list.files(dir_data, pattern = ".csv")
for (i in 1:length(flow_file_list)) {
  flow_file_sel <- flow_file_list[i]

  message(paste("reading", flow_file_sel))
  dat_flow_temp <- read_csv(paste0(dir_data, flow_file_sel), show_col_types = FALSE)
  dat_flow_temp <- dat_flow_temp %>%
    mutate(data_id = as.character(data_id))

  dat_flow <- bind_rows(dat_flow, dat_flow_temp)
}

#- build spread csv
dat_flow_sp <- dat_flow %>%
  mutate(EIA_ID = paste0("EIA_", EIA_ID)) %>%
  dplyr::select(date, EIA_ID, value) %>%
  distinct(date, EIA_ID, value) |>
  pivot_wider(id_cols = date, names_from = EIA_ID) |>
  missRanger()
# spread(EIA_ID, value, -date)

#- write out spread csv
write_csv(dat_flow_sp, paste0(dir_data, "/proc/flow_all_sp.csv"))

#- write out tidy csv
dat_flow_sp |>
  pivot_longer(-date, names_to = "EIA_ID") |>
  mutate(EIA_ID = gsub("EIA_", "", EIA_ID)) |>
  write_csv(paste0(dir_data, "/proc/flow_all_td.csv"))

#- create metadata file
dat_flow_meta <- dat_flow %>%
  dplyr::select(-date, -value) %>%
  distinct()

write_csv(dat_flow_meta, paste0(dir_data, "/proc/flow_metadata.csv"))

# ===========================================================
# diagnostic plots
dir_diag <- "diag/"

id_list <- unique(dat_flow$EIA_ID)
for (i in 1:length(id_list)) {
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
}
