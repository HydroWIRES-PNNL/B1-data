#===========================================================
# Name: utilities.R
# Author: D. Broman, PNNL
# Last Modified: 2024-04-24
# Description: [B1-data] utilities
#===========================================================
require(tidyverse)
require(cder)
require(dataRetrieval)

#' get_usgs
#'
#' @description downloads data from USGS
#' @param site_no string USGS site number
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse dataRetrieval
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none
#'
get_usgs = function(site_no, date_start, date_end) {
  # TODO error handling
  # TODO par_cd and stat_cd hard-coded
  # https://help.waterdata.usgs.gov/codes-and-parameters/parameters
  par_cd = '00060' # daily discharge in cfs
  stat_cd = '00003' # daily mean

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')

  dat_raw = readNWISdv(
    siteNumbers = site_no,
    parameterCd = par_cd,
    startDate = date_start,
    endDate = date_end,
    statCd = stat_cd
  )

  # find column with data - NOT USED
  # grepl(paste0(par_cd, '_', stat_cd), names(dat_raw))

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_raw %>%
    dplyr::rename(value = X_00060_00003) %>%
    mutate(date = as.Date(Date)) %>%
    dplyr::select(date, value)

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}

#' get_pnh
#'
#' @description downloads data from Reclamation Columbia-Pacific Northwest Region Hydromet
#' @param sta_code string hydromet station id
#' @param par_code string hydromet parameter code
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @return
#' @export none

get_pnh = function(sta_code, par_code, date_start, date_end) {
  #- daily data
  url_head = 'https://www.usbr.gov/pn-bin/daily.pl?'

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  url = paste0(
    url_head,
    'station=',
    sta_code,
    '&pcode=',
    par_code,
    '&year=',
    year_start,
    '&month=',
    month_start,
    '&day=',
    day_start,
    '&year=',
    year_end,
    '&month=',
    month_end,
    '&day=',
    day_end,
    '&format=csv'
  )

  # TODO error handling
  dat_proc = read_csv(url, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  #- template tibble with complete timesteps
  # TODO modify to alter desired timestep; hard-coded to daily
  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_proc %>%
    setNames(c('date', 'value'))

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}

#' get_mbh
#'
#' @description downloads data from Reclamation Missouri Basin Region Hydromet
#' @param sta_code string hydromet station id
#' @param par_code string hydromet parameter code
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none

get_mbh = function(sta_code, par_code, date_start, date_end) {
  # https://www.usbr.gov/gp/hydromet/automated_retrieval.pdf
  #- daily data
  url_head = 'https://www.usbr.gov/gp-bin/webarccsv.pl?parameter='
  #- instantaneous data
  # url_head = 'http://www.usbr.gov/gp-bin/webdaycsv.pl?parameter='
  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  url = paste0(
    url_head,
    sta_code,
    '%20',
    par_code,
    '&syer=',
    year_start,
    '&smnth=',
    month_start,
    '&sdy=',
    day_start,
    '&eyer=',
    year_end,
    '&emnth=',
    month_end,
    '&edy=',
    day_end,
    '&format=2'
  )

  # TODO error handling

  # download.file(url, 'temp.txt')
  # dat_raw = read_lines('temp.txt')
  dat_raw = read_lines(url)
  hdr_line_max = which(grepl('BEGIN DATA', dat_raw))
  dat_line_max = which(grepl('END DATA', dat_raw))
  dat_proc = read_csv(
    url,
    skip = hdr_line_max,
    n_max = dat_line_max - hdr_line_max - 2,
    show_col_types = FALSE
  )

  # TODO check timestep and adjust as needed; check units and convert as needed

  #- template tibble with complete timesteps
  # TODO modify to alter desired timestep; hard-coded to daily
  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_proc %>%
    setNames(c('date', 'value')) %>%
    mutate(date = as.Date(date, format = '%m/%d/%Y'), value = as.numeric(value))

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}

#' get_rise
#'
#' @description downloads data from Reclamation Information Sharing Environment (RISE)
#' @param item_id integer RISE catalog id
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none

get_rise = function(item_id, date_start, date_end) {
  # type (format) currenly hard-coded to 'csv'
  url_head = 'https://data.usbr.gov/rise/api/result/download?type=csv&itemId='

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')

  url = paste0(url_head, item_id, '&after=', date_start, '&before=', date_end)
  # &order=ASC

  # TODO error handling
  dat_raw = read_lines(url)

  hdr_line_max = which(grepl('"#SERIES DATA#', dat_raw))
  dat_proc = read_csv(url, skip = hdr_line_max, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  #- template tibble with complete timesteps
  # TODO modify to alter desired timestep; hard-coded to daily
  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_proc %>%
    dplyr::rename(value = Result) %>%
    mutate(date = as.Date(`Datetime (UTC)`)) %>%
    dplyr::select(date, value)

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}

#' get_cdec
#'
#' @description downloads data from California Data Exchange (CDEC)
#' @param sta_code character CDEC station id
#' @param sens_code integer CDEC sensor code
#' @param dur_code character CDEC duration code
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse cder
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none

get_cdec = function(sta_code, sens_code, dur_code = 'D', date_start, date_end) {
  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')

  dat_raw = cdec_query(sta_code, sens_code, dur_code, date_start, date_end)

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_raw %>%
    dplyr::rename(value = Value) %>%
    mutate(date = as.Date(DateTime)) %>%
    dplyr::select(date, value)

  if (dur_code != 'D') {
    dat_proc = dat_proc %>%
      group_by(date) %>%
      dplyr::summarise(value = mean(value, na.rm = T))
  }

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}

#' get_cdss
#'
#' @description downloads data from Colorado's Decision Support Systems (CDSS)
#' @param sta_abb character CDSS station abbreviation
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none

get_cdss = function(sta_abb, date_start, date_end) {
  url_head = 'https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsday/?format=csvforced'

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  url = paste0(
    url_head,
    '&dateFormat=dateOnly',
    '&fields=stationNum%2Cabbrev%2CmeasType%2CmeasDate%2Cvalue%2CflagA%2CflagC%2CflagD%2CdataSource%2Cmodified%2CmeasUnit',
    '&encoding=deflate',
    '&abbrev=',
    sta_abb,
    '&min-measDate=',
    month_start,
    '%2F',
    day_start,
    '%2F',
    year_start,
    '&max-measDate=',
    month_end,
    '%2F',
    day_end,
    '%2F',
    year_end
  )

  dat_raw = read_lines(url)

  hdr_line_max = which(grepl('abbrev', dat_raw))
  dat_proc = read_csv(url, skip = hdr_line_max - 1, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  #- template tibble with complete timesteps
  # TODO modify to alter desired timestep; hard-coded to daily
  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_proc %>%
    dplyr::rename(date = measDate) %>%
    dplyr::select(date, value)

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}

#' get_nwd
#' @description downloads data from NWD Dataquery
#' @param item_id character NWD item string
#' @param units character [cfs, kcfs]
#' @param dur_code character [I (sub-daily), D (daily)]
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none

get_nwd = function(item_id, units, dur_code, date_start, date_end) {
  url_head = 'https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?'

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  # - template tibble with complete timesteps
  # TODO modify to alter desired timestep; hard-coded to daily
  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  #- calculate number of years requested
  yr_ct = round(
    as.numeric(difftime(
      dat_fmt$date[nrow(dat_fmt)],
      dat_fmt$date[1],
      units = 'days'
    )) /
      365
  )

  if (dur_code == 'I' & yr_ct > 10) {
    date_end_seq = unique(c(
      seq(
        from = date_start + years(10) - days(1),
        to = date_end,
        by = '10 years'
      ),
      date_end
    ))
    date_start_seq = c(
      date_start,
      date_end_seq[1:(length(date_end_seq) - 1)] + days(1)
    )

    dat_raw = tibble()
    for (b in 1:length(date_end_seq)) {
      date_start_sel = date_start_seq[b]
      date_end_sel = date_end_seq[b]
      year_start = year(date_start_sel)
      month_start = month(date_start_sel)
      day_start = day(date_start_sel)
      year_end = year(date_end_sel)
      month_end = month(date_end_sel)
      day_end = day(date_end_sel)

      url = paste0(
        url_head,
        'id=',
        item_id,
        '%3Aunits%3D',
        units,
        '&headers=true',
        '&timezone=PST',
        '&startdate=',
        month_start,
        '%2F',
        day_start,
        '%2F',
        year_start,
        '+06%3A00',
        '&enddate=',
        month_end,
        '%2F',
        day_end,
        '%2F',
        year_end,
        '+06%3A00'
      )

      # TODO error handling
      dat_raw_temp = read_csv(url, show_col_types = FALSE)

      dat_raw = bind_rows(dat_raw, dat_raw_temp)
    } #- end data retrieval loop

    dat_proc = dat_raw %>%
      setNames(c('date_time', 'value_raw')) %>%
      mutate(date = as.Date(date_time, format = '%d-%b-%Y %H:%M')) %>%
      group_by(date) %>%
      dplyr::summarise(value_agg = mean(value_raw, na.rm = T)) %>%
      group_by(date) %>%
      mutate(value = ifelse(units == 'kcfs', value_agg * 1000, value_agg)) %>%
      dplyr::select(date, value)
  } else if (dur_code == 'I' & yr_ct <= 10) {
    url = paste0(
      url_head,
      'id=',
      item_id,
      '%3Aunits%3D',
      units,
      '&headers=true',
      '&timezone=PST',
      '&startdate=',
      month_start,
      '%2F',
      day_start,
      '%2F',
      year_start,
      '+06%3A00',
      '&enddate=',
      month_end,
      '%2F',
      day_end,
      '%2F',
      year_end,
      '+06%3A00'
    )

    # TODO error handling
    dat_raw = read_csv(url, show_col_types = FALSE)

    dat_proc = dat_raw %>%
      setNames(c('date_time', 'value_raw')) %>%
      mutate(date = as.Date(date_time, format = '%d-%b-%Y %H:%M')) %>%
      group_by(date) %>%
      dplyr::summarise(value_agg = mean(value_raw, na.rm = T)) %>%
      group_by(date) %>%
      mutate(value = ifelse(units == 'kcfs', value_agg * 1000, value_agg)) %>%
      dplyr::select(date, value)
  } else if (dur_code == 'D') {
    url = paste0(
      url_head,
      'id=',
      item_id,
      '%3Aunits%3D',
      units,
      '&headers=true',
      '&timezone=PST',
      '&startdate=',
      month_start,
      '%2F',
      day_start,
      '%2F',
      year_start,
      '+06%3A00',
      '&enddate=',
      month_end,
      '%2F',
      day_end,
      '%2F',
      year_end,
      '+06%3A00'
    )

    # TODO error handling
    dat_raw = read_csv(url, show_col_types = FALSE)

    dat_proc = dat_raw %>%
      setNames(c('date_time', 'value_raw')) %>%
      mutate(date = as.Date(date_time, format = '%d-%b-%Y %H:%M')) %>%
      group_by(date) %>%
      mutate(value = ifelse(units == 'kcfs', value_raw * 1000, value_raw)) %>%
      dplyr::select(date, value)
  } # end if block

  dat_fmt = dat_fmt %>%
    left_join(dat_proc, by = 'date')

  return(dat_fmt)
}


download_unzip_rename = function(
  url,
  zip_fn,
  extract_to_dir = '.',
  dir_rename_to = NULL,
  delete_zip = FALSE,
  cache = TRUE,
  download_method = 'wget',
  overwrite = TRUE,
  ...
) {
  zip_path = file.path(extract_to_dir, zip_fn)
  if (cache & file.exists(zip_path)) {
    message(
      'download_unzip_rename: ',
      zip_path,
      ' exists, not downloading again, set cache=FALSE to re-download.'
    )
  } else {
    download.file(
      url,
      zip_path,
      method = download_method,
      overwrite = overwrite
    ) # might fail if not installed
  }
  # list the files in the zip and
  files_in_zip = unzip(zip_path, list = T)$Name |>
    str_split_i('/', 1) |>
    unique()
  if (length(files_in_zip) == 1) {
    output_dir_name = files_in_zip[1]

    unzip(zip_path, exdir = extract_to_dir)

    if (!is.null(dir_rename_to)) {
      unzipped_output_dir = file.path(extract_to_dir, output_dir_name)
      new_unzipped_output_dir = file.path(extract_to_dir, dir_rename_to)
      if (file.exists(new_unzipped_output_dir) & overwrite) {
        unlink(new_unzipped_output_dir, recursive = T)
      }
      file.rename(unzipped_output_dir, new_unzipped_output_dir)
    }
  } else {
    if (is.null(dir_rename_to)) {
      stop(
        'Your zip file has no top level directory, please set dir_rename_to to extract the contents into.'
      )
    }
    message(
      'Your zip file has no top level directory, creating based on the dir_rename_to argument.'
    )
    output_dir_name = file.path(extract_to_dir, dir_rename_to)
    new_unzipped_output_dir = output_dir_name
    dir.create(output_dir_name, showWarnings = FALSE)

    unzip(zip_path, exdir = new_unzipped_output_dir)
  }

  if (delete_zip) {
    unlink(zip_path)
  }
  invisible(new_unzipped_output_dir)
}


## Functions for reading EIA spreadsheets
## From rectifhydplus, by Sean Turner, ORNL
identify_target_plants <- function(gnr_dir, plt_dir) {
  # Identify full list of conventional HY plants to target for analysis...
  # ... using 2022 generator data (most recent complete EIA set).

  # read EIA data files and filter for CONUS
  # suppressWarnings(
  read_xlsx(
    paste0(
      gnr_dir,
      "f923_2022/",
      "EIA923_Schedules_2_3_4_5_M_12_2022_Final.xlsx"
    ),
    skip = 5,
    progress = FALSE,
    col_types = 'text'
  ) |>
    filter(
      `AER\r\nFuel Type Code` == "HYC",
      !(`Plant State` %in% c("AK", 'HI'))
    ) |>
    select(EIA_ID = `Plant Id`) |>
    distinct_all() |>
    mutate(EIA_ID = str_replace(EIA_ID, '\\.', '')) |>
    mutate(EIA_ID = as.integer(EIA_ID)) -> HYC_with_gen_2022
  # )

  # read EIA plant data files (containing nameplate) ...
  # ... and filter capacity cutoff and CONUS plants ...
  # with generation data
  # suppressWarnings(
  read_xlsx(
    paste0(plt_dir, "/eia8602022/", "3_1_Generator_Y2022.xlsx"),
    skip = 1,
    col_types = 'text'
  ) |>
    select(
      EIA_ID = `Plant Code`,
      nameplate = `Nameplate Capacity (MW)`,
      `Prime Mover`
    ) |>
    filter(`Prime Mover` == "HY") |>
    mutate(EIA_ID = as.integer(EIA_ID)) |>
    summarise(nameplate = sum(as.numeric(nameplate)), .by = EIA_ID) |>
    filter(
      EIA_ID %in% HYC_with_gen_2022[["EIA_ID"]],
      # capacity cutoff is a globally specefied variable

      nameplate >= capacity_cutoff_MW
    ) -> HYC_cap_2022
  # )

  # get target plant list
  HYC_cap_2022 |>
    select(EIA_ID) -> target_plants

  return(target_plants)
}


get_eia_monthly_hydro_gen_pudl = function(
  pudl_fn = 'data/out_eia__monthly_generators.parquet' #,
  # start_year = 2022
) {
  read_parquet(pudl_fn) |>
    # filter(plant_id_eia %in% eia_ids) |>
    #filter(year(report_date) > (start_year - 1)) |>
    # distinct(plant_id_eia, fuel_type_code_pudl) |>
    # exclude pumped storage
    filter(
      fuel_type_code_pudl == 'hydro',
      !is.na(net_generation_mwh),
      prime_mover_code == 'HY',
      operational_status == 'existing'
    ) |>
    select(
      eia_id = plant_id_eia,
      plant_name_eia,
      plant_id_pudl,
      generator_id,
      datetime = report_date,
      net_gen_mwh = net_generation_mwh,
      nameplate_mw = capacity_mw,
      operational_status
    ) |>
    group_by(
      eia_id,
      datetime,
      plant_name_eia,
      plant_id_pudl,
      operational_status
    ) |>
    summarise(net_gen_mwh = sum(net_gen_mwh), nameplate_mw = sum(nameplate_mw))
}

get_eia_annual_hydro_gen_pudl = function(
  eia_ids,
  pudl_fn = 'data/out_eia__yearly_generators.parquet'
) {
  read_parquet(pudl_fn) |>
    filter(plant_id_eia %in% eia_ids) |>
    # filter(year(report_date) >= start_year) |>
    # distinct(plant_id_eia, fuel_type_code_pudl) |>
    filter(
      fuel_type_code_pudl == 'hydro',
      !is.na(net_generation_mwh),
      prime_mover_code == 'HY',
      operational_status == 'existing'
    ) |>
    select(
      eia_id = plant_id_eia,
      plant_id_pudl,
      plant_name_eia,
      generator_id,
      datetime = report_date,
      net_gen_mwh = net_generation_mwh,
      nameplate_mw = capacity_mw
    ) |>
    group_by(eia_id, plant_name_eia, plant_id_pudl, datetime) |>
    summarise(
      net_gen_mwh = sum(net_gen_mwh, na.rm = T),
      nameplate_mw = sum(nameplate_mw, na.rm = T)
    )
}

get_ferc_annual_hydro_gen = function(
  pudl_fn = 'data/out_ferc1__yearly_hydroelectric_plants_sched406.parquet'
) {
  read_parquet(pudl_fn) |>
    mutate(datetime = ym(sprintf('%s-01', report_year))) |>
    filter(!is.na(net_generation_mwh)) |>
    select(
      plant_id_pudl,
      plant_name_ferc1,
      datetime,
      nameplate_mw = capacity_mw,
      net_gen_mwh = net_generation_mwh
    ) |>
    group_by(plant_id_pudl, plant_name_ferc1, datetime) |>
    summarise(
      net_gen_mwh = sum(net_gen_mwh, na.rm = T),
      nameplate_mw = sum(nameplate_mw, na.rm = T)
    )
}

get_rfp_monthly_hydro_gen = function(rfp_data_fn) {
  rfp = read_csv(rfp_data_fn) |>
    pivot_longer(
      -c(RHPID, year, quality_label),
      names_to = 'month',
      values_to = 'net_gen_mwh'
    ) |>
    mutate(
      # usually associated EIA id can be extracted from the RHPID,
      # but there are some with combined ids that need to be split,
      # and in that case use the first id of the two
      # TODO account for the
      eia_id = str_split_i(RHPID, '_', 1) |>
        str_split_i('/', 1) |>
        as.numeric(),
      rfp_eia_id2 = str_split_i(RHPID, '_', 1) |>
        str_split_i('/', 2) |>
        as.numeric(),
      rfp_eia_id_was_split = str_split_i(RHPID, '_', 1) |>
        as.numeric() |>
        is.na() |>
        Vectorize(isTRUE)() |>
        suppressWarnings(),
      datetime = ym(sprintf('%s-%s', year, month))
    ) |>
    select(datetime, net_gen_mwh, eia_id, rfp_eia_id2, rfp_eia_id_was_split)

  # flip the eia ids of the combined plants so we can disag them later based on capacity
  rfp |>
    bind_rows(
      rfp |>
        filter(!is.na(rfp_eia_id2)) |>
        mutate(
          eia_id_save = eia_id,
          eia_id = rfp_eia_id2,
          rfp_eia_id2 = eia_id_save
        ) |>
        select(-eia_id_save)
    )
}
