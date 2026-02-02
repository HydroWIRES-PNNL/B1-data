# ==============================================================================
# Script: utilities.R
# Project: B1-data - Hydropower Generation Constraints Dataset
# ==============================================================================
#
# Purpose:
#   Utility functions for downloading streamflow data from various sources.
#   Each get_* function retrieves daily discharge data and returns a standardized
#   tibble with columns: date, value (flow in cfs unless noted).
#
# Functions:
#   - get_usgs()  : USGS National Water Information System (NWIS)
#   - get_cdec()  : California Data Exchange Center
#   - get_rise()  : Reclamation Information Sharing Environment
#   - get_cdss()  : Colorado Decision Support Systems
#   - get_pnh()   : Reclamation Pacific Northwest Hydromet
#   - get_mbh()   : Reclamation Missouri Basin Hydromet
#   - get_nwd()   : Army Corps NWD Dataquery
#
# Notes:
#   - All functions return complete date sequences with NAs for missing data
#   - [TODO: Document rate limits and API quirks for each source]
#   - [TODO: Add error handling for failed downloads]
#
# Authors: D. Broman (PNNL)
#          Cameron Bracken (cameron.bracken@pnnl.gov)
# ==============================================================================

# %% Libraries
library(tidyverse)
library(cder)
library(dataRetrieval)
library(arrow)

# %% USGS data retrieval
#' get_usgs
#'
#' @description downloads data from USGS
#' @param site_no string USGS site number
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse dataRetrieval
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none
get_usgs = function(site_no, date_start, date_end) {
  # TODO error handling
  # TODO par_cd and stat_cd hard-coded
  # https://help.waterdata.usgs.gov/codes-and-parameters/parameters
  par_cd = "00060" # daily discharge in cfs
  stat_cd = "00003" # daily mean

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")

  dat_raw = readNWISdv(
    siteNumbers = site_no,
    parameterCd = par_cd,
    startDate = date_start,
    endDate = date_end,
    statCd = stat_cd
  )

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  dat_proc =
    dat_raw |>
    dplyr::rename(value = X_00060_00003) |>
    mutate(date = as.Date(Date)) |>
    dplyr::select(date, value)

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}

# %% Pacific Northwest Hydromet data retrieval
#' get_pnh
#'
#' @description downloads data from Reclamation Columbia-Pacific Northwest Region Hydromet
#' @param sta_code string hydromet station id
#' @param par_code string hydromet parameter code
#' @param date_start string (or date) start date in 'YYYY-MM-DD' format
#' @param date_end string (or date) end date in 'YYYY-MM-DD' format
#' @importFrom tidyverse
#' @return dat_fmt tibble with columns date (date in 'YYYY-MM-DD') and value (float)
#' @export none
get_pnh = function(sta_code, par_code, date_start, date_end) {
  url_head = "https://www.usbr.gov/pn-bin/daily.pl?"

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  url = paste0(
    url_head,
    "station=",
    sta_code,
    "&pcode=",
    par_code,
    "&year=",
    year_start,
    "&month=",
    month_start,
    "&day=",
    day_start,
    "&year=",
    year_end,
    "&month=",
    month_end,
    "&day=",
    day_end,
    "&format=csv"
  )

  # TODO error handling
  dat_proc = read_csv(url, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  dat_proc =
    dat_proc |>
    setNames(c("date", "value"))

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}

# %% Missouri Basin Hydromet data retrieval
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
  url_head = "https://www.usbr.gov/gp-bin/webarccsv.pl?parameter="

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  url = paste0(
    url_head,
    sta_code,
    "%20",
    par_code,
    "&syer=",
    year_start,
    "&smnth=",
    month_start,
    "&sdy=",
    day_start,
    "&eyer=",
    year_end,
    "&emnth=",
    month_end,
    "&edy=",
    day_end,
    "&format=2"
  )

  # TODO error handling
  dat_raw = read_lines(url)
  hdr_line_max = which(grepl("BEGIN DATA", dat_raw))
  dat_line_max = which(grepl("END DATA", dat_raw))
  dat_proc = read_csv(
    url,
    skip = hdr_line_max,
    n_max = dat_line_max - hdr_line_max - 2,
    show_col_types = FALSE
  )

  # TODO check timestep and adjust as needed; check units and convert as needed

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  dat_proc =
    dat_proc |>
    setNames(c("date", "value")) |>
    mutate(date = as.Date(date, format = "%m/%d/%Y"), value = as.numeric(value))

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}

# %% RISE data retrieval
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
  url_head = "https://data.usbr.gov/rise/api/result/download?type=csv&itemId="

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")

  url = paste0(url_head, item_id, "&after=", date_start, "&before=", date_end)

  # TODO error handling
  dat_raw = read_lines(url)

  hdr_line_max = which(grepl('"#SERIES DATA#', dat_raw))
  dat_proc = read_csv(url, skip = hdr_line_max, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  dat_proc =
    dat_proc |>
    dplyr::rename(value = Result) |>
    mutate(date = as.Date(`Datetime (UTC)`)) |>
    dplyr::select(date, value)

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}

# %% CDEC data retrieval
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
get_cdec = function(sta_code, sens_code, dur_code = "D", date_start, date_end) {
  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")

  dat_raw = cdec_query(sta_code, sens_code, dur_code, date_start, date_end)

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  dat_proc =
    dat_raw |>
    dplyr::rename(value = Value) |>
    mutate(date = as.Date(DateTime)) |>
    dplyr::select(date, value)

  if (dur_code == "E") {
    dat_proc =
      dat_proc |>
      group_by(date) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE))
  }

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}

# %% CDSS data retrieval
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
  url_head = "https://dwr.state.co.us/Rest/GET/api/v2/surfacewater/surfacewatertsday/?format=csvforced"

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  url = paste0(
    url_head,
    "&dateFormat=dateOnly",
    "&fields=stationNum%2Cabbrev%2CmeasType%2CmeasDate%2Cvalue%2CflagA%2CflagC%2CflagD%2CdataSource%2Cmodified%2CmeasUnit",
    "&encoding=deflate",
    "&abbrev=",
    sta_abb,
    "&min-measDate=",
    month_start,
    "%2F",
    day_start,
    "%2F",
    year_start,
    "&max-measDate=",
    month_end,
    "%2F",
    day_end,
    "%2F",
    year_end
  )

  dat_raw = read_lines(url)

  hdr_line_max = which(grepl("abbrev", dat_raw))
  dat_proc = read_csv(url, skip = hdr_line_max - 1, show_col_types = FALSE)

  # TODO check timestep and adjust as needed; check units and convert as needed

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  dat_proc =
    dat_proc |>
    dplyr::rename(date = measDate) |>
    dplyr::select(date, value)

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}

# %% NWD data retrieval
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
  url_head = "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?"

  date_start = as.Date(date_start, format = "%Y-%m-%d")
  date_end = as.Date(date_end, format = "%Y-%m-%d")
  year_start = year(date_start)
  month_start = month(date_start)
  day_start = day(date_start)
  year_end = year(date_end)
  month_end = month(date_end)
  day_end = day(date_end)

  dat_fmt = tibble(date = seq(from = date_start, to = date_end, by = "day"))

  yr_ct = round(
    as.numeric(difftime(
      dat_fmt$date[nrow(dat_fmt)],
      dat_fmt$date[1],
      units = "days"
    )) /
      365
  )

  if (dur_code == "I" & yr_ct > 10) {
    date_end_seq = unique(c(
      seq(
        from = date_start + years(10) - days(1),
        to = date_end,
        by = "10 years"
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
        "id=",
        item_id,
        "%3Aunits%3D",
        units,
        "&headers=true",
        "&timezone=PST",
        "&startdate=",
        month_start,
        "%2F",
        day_start,
        "%2F",
        year_start,
        "+06%3A00",
        "&enddate=",
        month_end,
        "%2F",
        day_end,
        "%2F",
        year_end,
        "+06%3A00"
      )

      # TODO error handling
      dat_raw_temp = read_csv(url, show_col_types = FALSE)
      dat_raw = bind_rows(dat_raw, dat_raw_temp)
    }

    dat_proc =
      dat_raw |>
      set_names(c("date_time", "value_raw")) |>
      mutate(date = as.Date(date_time, format = "%d-%b-%Y %H:%M")) |>
      group_by(date) |>
      dplyr::summarise(value_agg = mean(value_raw, na.rm = TRUE)) |>
      group_by(date) |>
      mutate(value = ifelse(units == "kcfs", value_agg * 1000, value_agg)) |>
      dplyr::select(date, value)
  } else if (dur_code == "I" & yr_ct <= 10) {
    url = paste0(
      url_head,
      "id=",
      item_id,
      "%3Aunits%3D",
      units,
      "&headers=true",
      "&timezone=PST",
      "&startdate=",
      month_start,
      "%2F",
      day_start,
      "%2F",
      year_start,
      "+06%3A00",
      "&enddate=",
      month_end,
      "%2F",
      day_end,
      "%2F",
      year_end,
      "+06%3A00"
    )

    # TODO error handling
    dat_raw = read_csv(url, show_col_types = FALSE)

    dat_proc =
      dat_raw |>
      setNames(c("date_time", "value_raw")) |>
      mutate(date = as.Date(date_time, format = "%d-%b-%Y %H:%M")) |>
      group_by(date) |>
      dplyr::summarise(value_agg = mean(value_raw, na.rm = TRUE)) |>
      group_by(date) |>
      mutate(value = ifelse(units == "kcfs", value_agg * 1000, value_agg)) |>
      dplyr::select(date, value)
  } else if (dur_code == "D") {
    url = paste0(
      url_head,
      "id=",
      item_id,
      "%3Aunits%3D",
      units,
      "&headers=true",
      "&timezone=PST",
      "&startdate=",
      month_start,
      "%2F",
      day_start,
      "%2F",
      year_start,
      "+06%3A00",
      "&enddate=",
      month_end,
      "%2F",
      day_end,
      "%2F",
      year_end,
      "+06%3A00"
    )

    # TODO error handling
    dat_raw = read_csv(url, show_col_types = FALSE)

    dat_proc =
      dat_raw |>
      setNames(c("date_time", "value_raw")) |>
      mutate(date = as.Date(date_time, format = "%d-%b-%Y %H:%M")) |>
      group_by(date) |>
      mutate(value = ifelse(units == "kcfs", value_raw * 1000, value_raw)) |>
      dplyr::select(date, value)
  }

  dat_fmt =
    dat_fmt |>
    left_join(dat_proc, by = "date")

  return(dat_fmt)
}


# %% Plotting utilities
#' multipage_pdf_timeseries_by_group_with_source
#'
#' @param data data frame
#' @param group grouping column name
#' @param y_var y variable name
#' @param y_lab y axis label
#' @param y_step_var step variable for nameplate
#' @param plot_y_step whether to plot step line
#' @param fn output filename
#' @param data_source_col column name for data source
#' @param show_imputed whether to show imputed points
#' @return invisible
#' @export
multipage_pdf_timeseries_by_group_with_source = function(
  data,
  group,
  y_var = "net_gen_mw",
  y_lab = "Net Hydro Gen [aMW]",
  y_step_var = "nameplate_mw",
  plot_y_step = TRUE,
  fn = "compare_gen.pdf",
  data_source_col = "data_source",
  show_imputed = FALSE
) {
  # set up fixed colors for all the data labels so they dont change between plots
  if (data_source_col != "data_source") {
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
        title = hydro_df[[group]][1]

        hydro_df_imputed = hydro_df |> filter(imputed)
        y_imputed =
          if (show_imputed & nrow(hydro_df_imputed) > 0) {
            geom_point(
              aes(datetime, !!as.name(y_var), shape = imputed),
              color = "red",
              size = 0.5,
              data = hydro_df_imputed
            )
          } else {
            geom_blank()
          }

        p =
          hydro_df |>
          ggplot() +
          geom_line(
            aes(datetime, !!as.name(y_var), color = data_source, group = 1),
            linewidth = .8
          ) +
          scale_color_manual(values = data_source_colors) +
          y_step_geom +
          y_imputed +
          labs(
            x = "",
            y = y_lab,
            title = title,
            subtitle = "(red dots indicate imputed data)"
          ) +
          theme_minimal()
        print(p)
      },
      .progress = TRUE
    )
  message("Wrote: ", fn)
  on.exit({
    dev.off()
  })
  invisible()
}

# %% Helper functions
#' distinct_keep
#'
#' @param .data data frame
#' @param ... columns to distinct by
#' @return data frame with distinct rows keeping all columns
#' @export
distinct_keep = function(.data, ...) {
  dplyr::distinct(.data, ..., .keep_all = TRUE)
}

#' write_output
#'
#' @param x data frame to write
#' @param fn filename without extension
#' @param output_dir output directory
#' @param format output format (csv or parquet)
#' @param round_to decimal places to round to
#' @return invisible
#' @export
write_output = function(x, fn, output_dir = ".", format = "csv", round_to = 4) {
  fn_path = paste0(file.path(output_dir, fn), ".", format)
  x |>
    mutate(across(where(is.double), \(val) round(val, round_to))) |>
    (\(df) {
      if (format == "csv") {
        write_csv(df, fn_path, na = "")
      } else if (format == "parquet") {
        write_parquet(df, fn_path)
      }
    })()
  message("Wrote: ", fn_path)
}

#' groups_gt1
#'
#' @param df grouped data frame
#' @return data frame with groups having more than 1 row
#' @export
groups_gt1 = function(df) {
  df |>
    count() |>
    filter(n > 1)
}

# %% Version comparison utilities
#' read_b1
#'
#' @description Reads B1 data from a directory, handling different versions
#' @param path string path to B1 data directory
#' @param version string version number (e.g., "1.4.0")
#' @param timestep string either "monthly" or "weekly"
#' @return tibble with B1 data including version column
#' @export
read_b1 = function(path, version, timestep = "monthly", ...) {
  message("Reading B1 data version ", version, " ", timestep)

  sv = numeric_version(version)
  if (sv < numeric_version("1.4.0")) {
    # multiple files per year (older versions)
    subdir =
      list.files(path, full.names = TRUE) |>
      str_subset(regex(timestep, ignore_case = TRUE))

    subdir |>
      list.files("*", full.names = TRUE) |>
      map(read_csv, show_col_types = FALSE, .progress = TRUE) |>
      bind_rows() %>%
      {
        if (timestep == "weekly") {
          mutate(., datetime = week_start)
        } else if (timestep == "monthly") {
          mutate(
            .,
            month = match(month, month.abb),
            datetime = ymd(sprintf("%s-%02d-01", year, month))
          )
        } else {
          stop("Timestep must be 'monthly' or 'weekly'.")
        }
      } |>
      janitor::clean_names(parsing_option = 3) |>
      mutate(
        version = version,
        datetime = as.Date(datetime)
      )
  } else {
    # single file for all years (v1.4.0+)
    file_path = file.path(path, paste0("B1_", timestep, ".csv"))

    if (!file.exists(file_path)) {
      message("Warning: File not found: ", file_path)
      return(tibble())
    }

    read_csv(file_path, show_col_types = FALSE) |>
      mutate(
        version = version,
        datetime = as.Date(datetime)
      )
  }
}

#' plot_one_plant_versions
#'
#' @description Plot generation timeseries for one plant across multiple B1 versions
#' @param id integer EIA plant ID
#' @param data data frame with columns: eia_id, datetime, target_mwh, version, plant
#' @param version_colors named vector of colors for each version (optional)
#' @param palette string paletteer palette name (e.g., "pals::kelly") if version_colors is NULL
#' @return ggplot object
#' @export
plot_one_plant_versions = function(id, data, version_colors = NULL, palette = NULL) {
  plant_name =
    data |>
    filter(eia_id == id) |>
    pull(plant) |>
    unique()

  if (is.null(version_colors)) {
    versions = unique(data$version)
    if (!is.null(palette)) {
      version_colors = paletteer::paletteer_d(palette, n = length(versions))
      names(version_colors) = versions
    } else {
      version_colors = ggthemes::colorblind_pal()(8)[seq_along(versions) + 1]
      names(version_colors) = versions
    }
  }

  p =
    data |>
    filter(eia_id == id) |>
    ggplot() +
    geom_line(aes(datetime, target_mwh, color = factor(version))) +
    theme_minimal() +
    scale_color_manual(values = version_colors) +
    labs(title = paste(plant_name, id), color = "B1 Data\nVersion")

  return(p)
}
