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
get_usgs = function(site_no, date_start, date_end){
  # TODO error handling
  # TODO par_cd and stat_cd hard-coded
  # https://help.waterdata.usgs.gov/codes-and-parameters/parameters
  par_cd = '00060' # daily discharge in cfs
  stat_cd = '00003' # daily mean

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')

  dat_raw = readNWISdv(siteNumbers = site_no,
             parameterCd = par_cd,
             startDate = date_start,
             endDate = date_end,
             statCd = stat_cd)

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

get_pnh = function(sta_code, par_code, date_start, date_end){

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

  url = paste0(url_head,
               'station=', sta_code,
               '&pcode=', par_code,
               '&year=', year_start,
               '&month=', month_start,
               '&day=', day_start,
               '&year=', year_end,
               '&month=', month_end,
               '&day=', day_end,
               '&format=csv')

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

get_mbh = function(sta_code, par_code, date_start, date_end){
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

  url = paste0(url_head, sta_code, '%20', par_code, '&syer=', year_start,
               '&smnth=', month_start,
               '&sdy=', day_start,
               '&eyer=', year_end,
               '&emnth=', month_end,
               '&edy=', day_end,
               '&format=2')

  # TODO error handling

  # download.file(url, 'temp.txt')
  # dat_raw = read_lines('temp.txt')
  dat_raw = read_lines(url)
  hdr_line_max = which(grepl('BEGIN DATA', dat_raw))
  dat_line_max = which(grepl('END DATA', dat_raw))
  dat_proc = read_csv(url, skip = hdr_line_max, n_max = dat_line_max - hdr_line_max - 2, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  #- template tibble with complete timesteps
  # TODO modify to alter desired timestep; hard-coded to daily
  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_proc %>%
    setNames(c('date', 'value')) %>%
    mutate(date = as.Date(date, format = '%m/%d/%Y'),
           value = as.numeric(value))

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

get_rise = function(item_id, date_start, date_end){
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

get_cdec = function(sta_code, sens_code, dur_code = 'D', date_start, date_end){

  #- format dates
  date_start = as.Date(date_start, format = '%Y-%m-%d')
  date_end = as.Date(date_end, format = '%Y-%m-%d')

  dat_raw = cdec_query(sta_code, sens_code, dur_code, date_start, date_end)

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = 'day'))

  dat_proc = dat_raw %>%
    dplyr::rename(value = Value) %>%
    mutate(date = as.Date(DateTime)) %>%
    dplyr::select(date, value)

  if(dur_code == 'E'){
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

get_cdss = function(sta_abb, date_start, date_end){
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


  url = paste0(url_head,
              '&dateFormat=dateOnly',
              '&fields=stationNum%2Cabbrev%2CmeasType%2CmeasDate%2Cvalue%2CflagA%2CflagC%2CflagD%2CdataSource%2Cmodified%2CmeasUnit',
              '&encoding=deflate',
              '&abbrev=', sta_abb,
              '&min-measDate=', month_start, '%2F', day_start, '%2F', year_start,
              '&max-measDate=', month_end, '%2F', day_end, '%2F', year_end)

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


get_nwd = function(item_id, units, dur_code, date_start, date_end){
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
  yr_ct = round(as.numeric(difftime(dat_fmt$date[nrow(dat_fmt)], dat_fmt$date[1], units = 'days')) / 365)

  if(dur_code == 'I' & yr_ct > 10){
    date_end_seq = unique(c(seq(from = date_start + years(10) - days(1), to = date_end, by = '10 years'), date_end))
    date_start_seq = c(date_start, date_end_seq[1:(length(date_end_seq) - 1)] + days(1))

    dat_raw = tibble()
    for(b in 1:length(date_end_seq)){
      date_start_sel = date_start_seq[b]
      date_end_sel = date_end_seq[b]
      year_start = year(date_start_sel)
      month_start = month(date_start_sel)
      day_start = day(date_start_sel)
      year_end = year(date_end_sel)
      month_end = month(date_end_sel)
      day_end = day(date_end_sel)

      url = paste0(url_head,
                   'id=', item_id, '%3Aunits%3D', units,
                   '&headers=true',
                   '&timezone=PST',
                   '&startdate=', month_start, '%2F', day_start, '%2F', year_start, '+06%3A00',
                   '&enddate=', month_end, '%2F', day_end, '%2F', year_end, '+06%3A00')

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

  } else if(dur_code == 'I' & yr_ct <= 10){

    url = paste0(url_head,
                 'id=', item_id, '%3Aunits%3D', units,
                 '&headers=true',
                 '&timezone=PST',
                 '&startdate=', month_start, '%2F', day_start, '%2F', year_start, '+06%3A00',
                 '&enddate=', month_end, '%2F', day_end, '%2F', year_end, '+06%3A00')

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
  } else if(dur_code == 'D'){

    url = paste0(url_head,
                 'id=', item_id, '%3Aunits%3D', units,
                 '&headers=true',
                 '&timezone=PST',
                 '&startdate=', month_start, '%2F', day_start, '%2F', year_start, '+06%3A00',
                 '&enddate=', month_end, '%2F', day_end, '%2F', year_end, '+06%3A00')

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
