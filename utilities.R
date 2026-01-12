#===========================================================
# Name: utilities.R
# Author: C. Bracken, all functions unless otherwise noted
#         D. Broman, get_nwd, get_cdss, get_cdec, get_rise, get_mbh, get_pnh, get_usgs), PNNL
# Last Modified: 2024-04-24
# Description: [B1-data] utilities
#===========================================================
require(tidyverse)
require(cder)
require(dataRetrieval)
require(sf)
require(janitor)

#' get_usgs
#'
#' @description downloads data from USGS
#' @param site_no string USGS site number
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse dataRetrieval
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @author D. Broman, C. Bracken
#' @export none
#'
get_usgs = function(site_no, date_start, date_end, return_raw = FALSE) {
  # TODO error handling
  # TODO par_cd and stat_cd hard-coded
  # https://help.waterdata.usgs.gov/codes-and-parameters/parameters
  par_cd = '00060' # daily discharge in cfs
  stat_cd = '00003' # daily mean

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')

  # new USGS function read_waterdata_daily
  # requires environment variable API_USGS_PAT be set with an API key
  # edit CB, Jan 2025, move to new usgs api
  dat_raw = read_waterdata_daily(
    monitoring_location_id = paste0('USGS-', site_no), # new api needs "USGS-" prefix
    parameter_code = par_cd, # streamflow
    statistic_id = stat_cd, # mean
    time = c(date_start, date_end),
    # set timeout to max
    limit = NA,
  ) |>
    st_drop_geometry()

  if (!return_raw) {
    # TODO if the site has no daily data, check for other data (eg. hourly or 15 min)
    # dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))
    dat_fmt =
      tibble(date = seq(from = date_start, to = date_end, by = 'day')) |>
      group_by(date) |>
      left_join(
        # dat_raw |> select(date = time, value, unit_of_measure),
        dat_raw |>
          # some sites have mutiple data points per time step, so choose the latest availablle
          group_by(date = time) |>
          summarise(
            value = value[which.max(last_modified)],
            unit_of_measure = unit_of_measure[which.max(last_modified)]
          ),
        by = 'date'
      )
    return(dat_fmt)
  } else {
    return(return_raw)
  }
}

#' get_pnh
#'
#' @description downloads data from Reclamation Columbia-Pacific Northwest Region Hydromet
#' @param sta_code string hydromet station id
#' @param par_code string hydromet parameter code
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @author D. Broman
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
#' @author D. Broman
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
#' @author D. Broman
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
#' @author D. Broman
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
#' @author D. Broman
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
#' @author D. Broman, C. Bracken
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
      if (nrow(dat_raw_temp) == 0) {
        next
      } else {
        dat_raw = bind_rows(dat_raw, dat_raw_temp)
      }
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

#' Title
#'
#' @param url
#' @param zip_fn
#' @param extract_to_dir
#' @param dir_rename_to
#' @param delete_zip
#' @param cache
#' @param download_method
#' @param overwrite
#' @param ...
#'
#' @returns
#'
#' @export
#' @examples
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

    new_unzipped_output_dir =
      if (!is.null(dir_rename_to)) {
        new_unzipped_output_dir = file.path(extract_to_dir, dir_rename_to)
      } else {
        NULL
      }
    if (cache & file.exists(new_unzipped_output_dir)) {
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
      message(
        'download_unzip_rename: the target directory ',
        new_unzipped_output_dir,
        ' exists, not unzipping again, set cache=FALSE to re-unzip.'
      )
    }
  } else {
    if (is.null(dir_rename_to)) {
      stop(
        'download_unzip_rename: ',
        'Your zip file has no top level directory, please set dir_rename_to to extract the contents into.'
      )
    }
    message(
      'download_unzip_rename: ',
      'Your zip file has no top level directory, creating it based on the dir_rename_to argument.'
    )
    output_dir_name = file.path(extract_to_dir, dir_rename_to)
    new_unzipped_output_dir = output_dir_name
    if (!cache | !file.exists(new_unzipped_output_dir)) {
      message('download_unzip_rename: unzipping ', new_unzipped_output_dir)
      dir.create(output_dir_name, showWarnings = FALSE)
      unzip(zip_path, exdir = new_unzipped_output_dir)
    } else {
      message(
        'download_unzip_rename: the target directory ',
        new_unzipped_output_dir,
        ' exists, not unzipping again, set cache=FALSE to re-unzip.'
      )
    }
  }

  if (delete_zip) {
    unlink(zip_path)
  }
  invisible(new_unzipped_output_dir)
}


#' Title
#'
#' @param pudl_fn
#'
#' @returns
#'
#' @export
#' @examples
get_eia_monthly_hydro_gen_pudl = function(
  pudl_fn = 'data/out_eia__monthly_generators.parquet' #,
) {
  #
  generators = read_eia_pudl_generators_parquet_hydro(pudl_fn)

  full_date_seq = tibble(
    datetime = with(
      generators,
      seq.Date(min(report_date), max(report_date), by = 'month')
    )
  )

  generators_complete =
    generators |>
    select(
      eia_id = plant_id_eia,
      plant_id_pudl,
      generator_id,
      datetime = report_date,
      net_gen_mwh = net_generation_mwh,
      nameplate_mw = capacity_mw,
    ) |>
    group_by(eia_id, generator_id) |>
    group_split() |>
    map(function(single_generator) {
      single_generator |>
        right_join(full_date_seq, by = join_by(datetime)) |>
        # in a few cases, data for a generator is missing for some dates,
        # so fill it to get consistent ids when mutiple eia_ids are combined
        fill(eia_id, generator_id, plant_id_pudl, .direction = 'downup')
    }) |>
    bind_rows()

  generators_complete |>
    group_by(
      eia_id,
      datetime,
      plant_id_pudl,
    ) |>
    # aggregate generators to plants, but some plants have mutiple eia ids
    summarise(
      net_gen_mwh = sum(net_gen_mwh, na.rm = T),
      nameplate_mw = sum(nameplate_mw, na.rm = T)
    ) |>
    group_by(
      datetime,
      plant_id_pudl
    ) |>
    # now aggregate the plants the have multiple eia ids
    # and keep track of what was done
    mutate(
      # usually associated EIA id can be extracted from the RHPID,
      # but there are some with combined ids that need to be split,
      # and in that case use the first id of the two
      # TODO account for this throughout
      eia_id = str_flatten(eia_id, '_'),
      n_eia_ids_combined = length(eia_id),
      # str_split_i('/', 1) |>,
      eia_id1 = eia_id[1],
      eia_id2 = eia_id[2],
      eia_id3 = eia_id[3],
      # no hydro_plants in pudl data combine more than 3 eia_ids,
      # but add a 4th to be safe, but remove it later if its empty
      eia_id4 = eia_id[4],
      net_gen_mwh = sum(net_gen_mwh, na.rm = T),
      nameplate_mw = sum(nameplate_mw, na.rm = T),
      eia_id_was_combined = eia_id |>
        as.numeric() |>
        is.na() |>
        Vectorize(isTRUE)() |>
        suppressWarnings(),
      n_eia_ids_combined = length(eia_id)
    ) %>%
    # drop the 4th eia column if there are no entries
    {
      if (all(is.na(.$eia_id4))) {
        select(., -eia_id4)
      } else {
        .
      }
    }
}

#' Title
#'
#' @param rectifhyd_fn
#'
#' @returns
#'
#' @export
#' @examples
read_rectifhyd = function(rectifhyd_fn) {
  #
  rectifhyd_raw = read_csv(rectifhyd_fn)
  if (!('nameplate_mw' %in% names(rectifhyd_raw))) {
    message(paste0(
      'NOTE: This version of rectifhyd does not include capacity data,',
      ' adding a dummy column for compatability with other datasets.'
    ))
  }
  rectifhyd_raw |>
    janitor::clean_names(parsing_option = 3) |>
    mutate(
      eia_id = as.character(eia_id),
      datetime = sprintf('%s-%s', year, month) |> ym(),
      net_gen_mwh = ifelse(
        # use the best available data source
        recommended_data == 'RectifHyd',
        rectif_hyd_mwh,
        eia_mwh
      ),
      data_source = 'rf',
      nameplate_mw = NA,
      net_gen_mw = NA
    )
}

#' Title
#'
#' @param x
#'
#' @returns
#'
#' @export
#' @examples
agg_na_rm_unless_all_na = function(x, aggfun = sum) {
  ifelse(all(is.na(x)), NA, aggfun(x, na.rm = T))
}

#' Title
#'
#' @param df
#' @param cols
#'
#' @returns
#'
#' @export
#' @examples
cols_exists = function(df, cols) {
  cols %in% names(df)
}


#' Title
#'
#' @param df
#' @param cols
#'
#' @returns
#'
#' @export
#' @examples
col_class = function(df, col) {
  df |> pull(!!as.name(col)) |> class()
}


#' Title
#'
#' @param gen_data
#'
#' @returns
#'
#' @export
#' @examples
complete_and_combine_eia_ids = function(
  gen_data,
  complete_date_seq,
  target_plants,
  pudl_eia_mapping,
  rfp_pudl_eia_mapping,
  data_source_name = NULL
) {
  #
  # some datasets may not have nameplate capacity
  if (!(cols_exists(gen_data, 'nameplate_mw'))) {
    message('Adding dummy variable for nameplate capacity.')
    gen_data$nameplate_mw = NA
  }
  # setting data source name column
  if (!cols_exists(gen_data, 'data_source') & !is.null(data_source_name)) {
    message('Adding data_source variable: ', data_source_name)
    gen_data$data_source = data_source_name
  } else if (!is.null(data_source_name)) {
    # data source was set explicitly so use that
    gen_data$data_source = data_source_name
  } else {
    # fall back to the data source name specified in the dataset
    data_source_name = gen_data$data_source[1]
  }

  # full combination of all eia_ids and dates
  full_grid_date_eia_id = expand_grid(
    eia_id = unique(pudl_eia_mapping$eia_id),
    datetime = complete_date_seq
  )
  # add pudl id mapping if its not already present
  if (!cols_exists(gen_data, 'plant_id_pudl')) {
    message('plant_id_pudl is not in the dataset, merging it in.')
    full_grid_date_eia_id =
      left_join(
        full_grid_date_eia_id,
        pudl_eia_mapping,
        by = join_by(eia_id)
      )
  }

  # convert integer pudl id to character so it doesnt clash later
  if (cols_exists(gen_data, 'plant_id_pudl')) {
    if (col_class(gen_data, 'plant_id_pudl') != 'character') {
      gen_data = gen_data |> mutate(plant_id_pudl = as.character(plant_id_pudl))
    }
  }

  message(
    'Creating complete data based on ',
    s('{length(target_plants)} eia_ids and {length(complete_date_seq)} dates.')
  )
  # create a complete dataset where every timestep and eia id is represented
  # cant use the complete() function because some eia and pudl ids might be missing
  gen_data_complete =
    full_grid_date_eia_id |>
    left_join(gen_data, by = join_by(datetime, eia_id)) |>
    group_by(eia_id) |>
    fill(-c(datetime, eia_id, net_gen_mwh), .direction = 'downup') |>
    group_by(
      eia_id,
      datetime,
      plant_id_pudl
    ) |>
    # aggregate generators (if any) to plants,
    # but if all values are missing return NA
    # some plants may still have mutiple eia ids
    summarise(
      net_gen_mwh = agg_na_rm_unless_all_na(net_gen_mwh),
      nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw)
    )

  n_agg1 = pudl_eia_mapping |> group_by(plant_id_pudl) |> groups_gt1() |> nrow()
  message(s('Aggregating {n_agg1} eia plants with the same pudl id'))
  # now aggregate the pudl plants that have multiple eia ids
  # and keep track of what was done
  gen_data_agg1 = gen_data_complete |>
    group_by(
      datetime,
      plant_id_pudl
    ) |>
    summarise(
      eia_id1 = eia_id[1],
      eia_id2 = eia_id[2],
      eia_id3 = eia_id[3],
      # no hydro_plants in pudl data combine more than 3 eia_ids,
      # but could add a 4th to be safe, and remove it later if its empty
      # eia_id4 = eia_id[4],
      n_eia_ids_combined = length(eia_id),
      net_gen_mwh = agg_na_rm_unless_all_na(net_gen_mwh),
      nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw),
      eia_id_was_combined = ifelse(n_eia_ids_combined > 1, TRUE, FALSE),
      eia_id = str_flatten(eia_id, '_')
    )

  n_agg2 = rfp_eia_pudl_map_uncombined |>
    filter(str_detect(plant_id_pudl_new, '_')) |>
    distinct(eia_id_new, .keep_all = TRUE) |>
    nrow()
  message(
    s(
      'Aggregating {n_agg2} pairs of eia ids that have unique pudl ids, based on rfp ids.'
    )
  )
  # browser()
  # now aggregate the plants that have distinct eia and pudl ids but
  # are actually the same plant and keep track of what was done
  gen_data_agg2 =
    gen_data_agg1 |>
    left_join(
      rfp_pudl_eia_mapping,
      by = join_by(plant_id_pudl, eia_id)
    ) |>
    # create new columns with the combined ids to identiy which
    # plants need to be aggregated
    mutate(
      eia_id_new = ifelse(is.na(eia_id_new), eia_id, eia_id_new),
      plant_id_pudl_new = ifelse(
        is.na(plant_id_pudl_new),
        plant_id_pudl,
        plant_id_pudl_new
      )
    ) |>
    # filter(eia_id_new == '6479_6480') |>
    group_by(datetime, eia_id_new, plant_id_pudl_new) |>
    summarise(
      eia_id1 = ifelse(all(is.na(eia_id1)), eia_id[1], first(eia_id1)),
      eia_id2 = ifelse(all(is.na(eia_id2)), eia_id[2], first(eia_id2)),
      eia_id3 = first(eia_id3),
      plant_id_pudl1 = plant_id_pudl[1],
      plant_id_pudl2 = plant_id_pudl[2],
      net_gen_mwh = agg_na_rm_unless_all_na(net_gen_mwh),
      nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw),
      n_eia_ids_combined = n_eia_ids_combined[1],
      eia_id_was_combined = eia_id_was_combined[1],
      n_eia_ids_combined2 = length(eia_id),
      eia_id_was_combined2 = ifelse(n_eia_ids_combined2 > 1, TRUE, FALSE),
      eia_id = eia_id_new[1],
      plant_id_pudl = plant_id_pudl_new[1]
    ) |>
    # drop old id_columns and rename the new columns with the combined ids
    select(-c(eia_id, plant_id_pudl)) |>
    rename(eia_id = eia_id_new, plant_id_pudl = plant_id_pudl_new) |>
    mutate(data_source = data_source_name)

  return(gen_data_agg2)
}

#' Title
#'
#' @param eia_gen_dir
#' @param eia_plant_dir
#' @param target_plants
#'
#' @returns
#'
#' @export
#' @examples
read_eia_spreadsheets_rfp = function(
  eia_gen_dir,
  eia_plant_dir,
  target_plants,
  complete_data = TRUE,
  cache = TRUE,
  cache_dir = 'data'
) {
  cache_fn = s('{cache_dir}/eia_hydro_gen_monthly_1980_2022_wide.csv')
  if (cache & file.exists(cache_fn)) {
    message(s('Reading cached eia spreadhseet data from {cache_fn}'))
    eia_hydro_gen_monthly_1980_2022_wide = read_csv(cache_fn)
  } else {
    # TODO check on warnings about data names
    eia_hydro_gen_monthly_1980_2022_wide =
      get_EIA_monthly_gen(
        eia_gen_dir,
        eia_plant_dir,
        target_plants
      ) |>
      clean_names(parsing_option = 3)
    eia_hydro_gen_monthly_1980_2022_wide |> write_csv(cache_fn)
  }

  # this option returns the data with all the id columns,
  # even if they are NA
  if (complete_data) {
    # now pivot to long form
    eia_hydro_gen_monthly_1980_2022 = eia_hydro_gen_monthly_1980_2022_wide |>
      # ensure the eia data has a complete time range for each id
      complete(year, eia_id) |>
      pivot_longer(
        all_of(tolower(month.abb)),
        names_to = 'month',
        values_to = 'net_gen_mwh'
      ) |>
      mutate(datetime = ym(sprintf('%s-%s', year, month)), data_source = 'eia') |>
      select(
        eia_id,
        datetime,
        net_gen_mwh,
        nameplate_mw = nameplate,
        data_source
      ) |>
      # at least one plant, 3437, has mutiple rows, one row is all zeros
      # I assume these are multiple generators and add them up
      # TODO track down the duplicate, its probably in one of the spreadsheets
      group_by(eia_id, datetime, data_source) |>
      summarise(net_gen_mwh = sum(net_gen_mwh), nameplate_mw = sum(nameplate_mw)) |>
      # needs to be character to join with other datasets
      mutate(eia_id = as.character(eia_id))
  } else {
    eia_hydro_gen_monthly_1980_2022_wide
  }
}


#' Read a pudl eia parquet file and filter to only the conventional
#' hydro plants (exclude pumped storage) and return relevant columns.
#'
#' The file name should be either annual or monthly generator file.
#'
#'
#' @param pudl_fn
#'
#' @returns a tibble
#'
#' @export
#' @examples
read_eia_pudl_generators_parquet_hydro = function(pudl_fn) {
  pudl_eia_hydro = read_parquet(pudl_fn) |>
    # get just conventional hydro
    # https://www.eia.gov/electricity/monthly/pdf/AppendixC.pdf
    filter(
      # fuel_type_code_pudl == 'hydro',
      energy_source_code_1 == 'WAT',
      # !is.na(net_generation_mwh),
      prime_mover_code == 'HY'
      # operational_status == 'existing'
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
    mutate(
      eia_id = as.character(eia_id),
      plant_id_pudl = as.character(plant_id_pudl),
      data_source = 'eia_pudl'
    )
  return(pudl_eia_hydro)
}


#' Title
#'
#' @param eia_ids
#' @param pudl_fn
#'
#' @returns
#'
#' @export
#' @examples
get_eia_annual_hydro_gen_pudl = function(
  eia_ids,
  pudl_fn = 'data/out_eia__yearly_generators.parquet'
) {
  read_eia_pudl_generators_parquet_hydro(pudl_fn) |>
    group_by(eia_id, plant_name_eia, plant_id_pudl, datetime) |>
    summarise(
      net_gen_mwh = agg_na_rm_unless_all_na(net_gen_mwh),
      nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw)
    )
}

#' Title
#'
#' @param pudl_fn
#'
#' @returns
#'
#' @export
#' @examples
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

#' Title
#'
#' @param rfp_data_fn
#'
#' @returns
#'
#' @export
#' @examples
get_rfp_monthly_hydro_gen = function(rfp_data_fn) {
  rfp = read_csv(rfp_data_fn) |>
    pivot_longer(
      -c(RHPID, year, quality_label),
      names_to = 'month',
      values_to = 'net_gen_mwh'
    ) |>
    # rectifhyd plus combines eia plants into a new id
    # usually associated EIA id can be extracted from the RHPID,
    # but there are some with combined ids that neeed to be split to,
    # keep track of which ids were combined
    # TODO account for this everywhere
    mutate(eia_id = str_split_i(RHPID, '_', 1) |> str_replace('/', '_')) |>
    mutate(
      eia_id1 = str_split_i(eia_id, '_', 1),
      eia_id2 = str_split_i(eia_id, '_', 2),
      # no hydro_plants in rfp combine more than 2
      # eia_id3 = str_split_i(eia_id, '_', 3),
      n_eia_ids_combined = eia_id |> str_split('_') |> sapply(length),
      eia_id_was_combined = ifelse(n_eia_ids_combined > 1, T, F),
      datetime = ym(sprintf('%s-%s', year, month))
    ) |>
    select(
      datetime,
      net_gen_mwh,
      eia_id,
      eia_id1,
      eia_id2,
      eia_id_was_combined,
      n_eia_ids_combined,
    ) |>
    mutate(data_source = 'rfp')
  return(rfp)

  # # flip the eia ids of the combined plants so we can disag them later based on capacity
  # rfp |>
  #   bind_rows(
  #     rfp |>
  #       filter(!is.na(rfp_eia_id2)) |>
  #       mutate(
  #         eia_id_save = eia_id,
  #         eia_id = rfp_eia_id2,
  #         rfp_eia_id2 = eia_id_save
  #       ) |>
  #       select(-eia_id_save)
  #   )
}


#' Title
#'
#' @param dir_datam
#' @param id
#' @param date_start
#' @param date_end
#' @param subdir
#'
#' @returns
#'
#' @export
#' @examples
flow_fn_with_dates = function(
  dir_data,
  id,
  date_start,
  date_end,
  subdir = 'raw'
) {
  out_path = file.path(dir_data, subdir)
  paste0(
    file.path(out_path, id),
    "_",
    date_start,
    '_to_',
    date_end,
    "_flow.csv"
  )
}


#' Title
#'
#' @param df
#'
#' @returns
#'
#' @export
#' @examples
groups_gt1 <- function(df) {
  df %>%
    count() |>
    filter(n > 1)
}


#' Title
#'
#' @param env_fn
#'
#' @returns
#'
#' @export
#' @examples
check_env_set_usgs_api_key = function(env_fn = '.env') {
  env_fn = '.env'
  key_help_msg = "Did not find an environment file '%s' with the 'API_USGS_PAT' variable.
Please get a key from https://api.waterdata.usgs.gov/signup and 
set it in the file like this:
    API_USGS_PAT = 'api-key'
"
  if (!file.exists(env_fn)) {
    stop(key_help_msg |> sprintf(env_fn))
  } else {
    readRenviron(env_fn)
    usgs_api_key = Sys.getenv('API_USGS_PAT')
    if (usgs_api_key == "") {
      stop(key_help_msg |> sprintf(env_fn))
    }
  }
}


#' download previous B1 version data and unzip it
#'
#' @param path
#' @param version
#' @param timestep
#' @param ...
#'
#' @returns
#'
#' @export
#' @examples
read_b1 = function(path, version, timestep = 'monthly', ...) {
  #
  message('Reading B1 data version ', version, ' ', timestep)

  # TODO change to use compareVersion(version,"1.4.0")
  sv = numeric_version(version)
  if (sv < numeric_version('1.4.0')) {
    # multiple files, one per year
    subdir = list.files(path, full.names = TRUE) |>
      str_subset(regex(timestep, ignore_case = TRUE))
    # browser()
    subdir |>
      list.files('*', full.names = T) |>
      map(read_csv, .progress = TRUE) |>
      bind_rows() %>%
      {
        # older data did not have a consistent datetime column name
        if (timestep == 'weekly') {
          mutate(., datetime = week_start)
        } else if (timestep == 'monthly') {
          mutate(., datetime = ymd(sprintf('%s-%s-01', year, month)))
        } else {
          stop("Timestep must be 'monthly' or 'weekly'.")
        }
      } |>
      janitor::clean_names(parsing_option = 3) |>
      mutate(version = version)
  } else {
    fn = list.files(path, full.names = TRUE) |>
      str_subset(regex(timestep, ignore_case = TRUE))
    # one file for enture period
    read_csv(fn) |>
      mutate(version = version)
  }
}


#' Title
#'
#' @param url
#' @param zip_fn
#' @param exdir
#' @param dir_rename_to
#' @param delete_zip
#' @param cache
#' @param download_method
#' @param overwrite
#' @param ...
#'
#' @returns
#'
#' @export
#' @examples
download_unzip_rename_orig = function(
  url,
  zip_fn,
  exdir = '.',
  dir_rename_to = NULL,
  delete_zip = FALSE,
  cache = TRUE,
  download_method = 'wget',
  overwrite = TRUE,
  ...
) {
  # TODO merge with newer verison
  zip_path = file.path(exdir, zip_fn)
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
  output_dir_name = unzip(zip_path, list = T)$Name[1]
  unzip(zip_path, exdir = exdir)
  if (!is.null(dir_rename_to)) {
    unzipped_output_dir = file.path(exdir, output_dir_name)
    new_unzipped_output_dir = file.path(exdir, dir_rename_to)
    if (file.exists(new_unzipped_output_dir) & overwrite) {
      unlink(new_unzipped_output_dir, recursive = T)
    }
    file.rename(unzipped_output_dir, new_unzipped_output_dir)
  }
  if (delete_zip) {
    unlink(zip_path)
  }
}


#' Title
#'
#' @param data
#' @param group
#' @param y_var
#' @param y_lab
#' @param y_step_var
#' @param plot_y_step
#' @param fn
#' @param data_source_col
#'
#' @returns
#'
#' @export
#' @examples
multipage_pdf_timeseries_by_group_with_source =
  function(
    data,
    group,
    y_var = 'net_gen_mw',
    y_lab = 'Net Hydro Gen [aMW]',
    y_step_var = 'nameplate_mw',
    plot_y_step = TRUE,
    fn = 'compare_gen.pdf',
    data_source_col = 'data_source'
  ) {
    # browser()
    # set up fixed colors for all the data labels so they dont change between plots
    if (data_source_col != 'data_source') {
      data$data_source = data[[data_source_col]]
    }
    data_sources = unique(data$data_source)
    data_source_colors = ggthemes::colorblind_pal()(8)[1:length(data_sources) + 1]
    names(data_source_colors) = data_sources

    y_step_geom =
      if (plot_y_step) {
        geom_step(
          aes(datetime, nameplate_mw, color = data_source),
          linewidth = .4,
          group = 1
        )
      } else {
        geom_blank()
      }
    pdf(fn, 6, 4, onefile = TRUE)
    data |>
      group_by(!!as.name(group)) |>
      group_split() |>
      walk(
        function(hydro_df) {
          title = hydro_df[[group]][1] #with(hydro_df, eia_id)
          # browser()
          p = hydro_df |>
            ggplot() +
            geom_line(
              aes(datetime, !!as.name(y_var), color = data_source, group = 1),
              linewidth = .8
            ) +
            scale_color_manual(values = data_source_colors) +
            y_step_geom +
            labs(x = '', y = y_lab, title = title) +
            theme_minimal()
          print(p)
        },
        .progress = TRUE
      )
    message('Wrote: ', fn)
    on.exit({
      dev.off()
    })
    invisible()
  }


distinct_keep <- function(.data, ...) {
  dplyr::distinct(.data, ..., .keep_all = TRUE)
}
