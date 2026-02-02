# ==============================================================================
# Script: 3-hydropower.R
# Project: B1-data - Hydropower Generation Constraints Dataset
# ==============================================================================
#
#   Download USACE dam operational data (power, forebay elevation, inflow, outflow)
#   from NWD web services for 28 PNW dams.Handles different data types (CBT-RAW,
#   CBT-REV, Best) and temporal resolutions. Aggregates to daily averages.
#
# Notes:
#   - Data is downloaded in UTC, converted to US/Pacific, then aggregated to daily
#   - Larger queries (inst forebay) might timeout - just retry if needed
#
# Authors: Cameron Bracken (cameron.bracken@pnnl.gov)
# ==============================================================================

# %% Libraries
library(conflicted)
conflicted::conflicts_prefer(dplyr::filter)
library(tidyverse)
library(glue)
s = glue::glue
library(paletteer)

source('utilities.R')

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000
)

# %% Configuration
start_year = 2001
end_year = 2024

# URL configuration
url_base = "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
period = "lookforward=0h0m&startdate=01/01/%s&enddate=12/31/%s" |>
  sprintf(start_year, end_year)

# Cache directory for raw downloads
cache_dir = "data/usace"

# Output paths
output_dir = "output"
hourly_power_file = "output/pnw_hourly_power.csv"
daily_data_file = "output/pnw_daily_data.csv"

# Create directories
dir.create(cache_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)

# %% Dam codes
dam_codes =
  tribble(
    ~dam  , ~eia_id ,
    "BON" , 3075L   ,
    "CHJ" , 3921L   ,
    "GCL" , 6163L   ,
    "IHR" , 3925L   ,
    "JDA" , 3082L   ,
    "LGS" , 3926L   ,
    "LMN" , 3927L   ,
    "LWG" , 6175L   ,
    "MCN" , 3084L   ,
    "PRD" , 3887L   ,
    "TDA" , 3895L   ,
    "LIB" , 6172L   ,
    "ALF" ,  851L   ,
    "BCL" , 3074L   ,
    "CGR" , 3076L   ,
    "DET" , 3077L   ,
    "DEX" , 3078L   ,
    "DWR" ,  840L   ,
    "FOS" , 6552L   ,
    "GPR" , 3080L   ,
    "HCR" , 3081L   ,
    "HGH" , 2203L   ,
    "LOP" , 3083L   ,
    "LOS" , 6174L   ,
    "RIS" , 6200L   ,
    "RRH" , 3883L   ,
    "WAN" , 3888L   ,
    "WEL" , 3886L
  )

# %% Data type specifications
data_string =
  c(
    power = ".Power.Total.1Hour.1Hour.",
    forebay = ".Elev-Forebay.Ave.~1Day.1Day.",
    outflow = ".Flow-Out.Ave.1Hour.1Hour.",
    inflow = ".Flow-In.Ave.~1Day.1Day."
  )

cbt_type =
  c(
    power = "CBT-RAW",
    forebay = "CBT-REV",
    outflow = "CBT-REV",
    inflow = "CBT-REV"
  )

cbt_units =
  c(
    power = "MW",
    forebay = "ft",
    outflow = "kcfs",
    inflow = "kcfs"
  )

willamette_projects = c(
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

# %% Build query parameters
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

# %% Download data
cbt_data =
  data_query |>
  group_by(dam, variable, eia_id) |>
  group_split() |>
  map_dfr(\(r) {
    cbt_string = with(r, paste0(dam, data_string, cbt_type))
    url = paste0(
      url_base,
      cbt_string,
      ":units=",
      r$cbt_units,
      "&headers=true&timezone=GMT&",
      period
    )
    message(cbt_string)
    output_fn = file.path(cache_dir, sprintf("%s.csv", cbt_string))

    # save the output to a file if the file does not exist, otherwise read from file
    if (!file.exists(output_fn)) {
      x = read_csv(url, show_col_types = FALSE, progress = FALSE)
      write_csv(x, output_fn)
    } else {
      x = read_csv(output_fn, show_col_types = FALSE, progress = FALSE)
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
  })

# %% Process and convert to Pacific time
cbt_data_pacific =
  cbt_data |>
  mutate(
    datetime_pacific = with_tz(datetime, "US/Pacific"),
    year = year(datetime_pacific),
    month = month(datetime_pacific),
    day = day(datetime_pacific),
    hour = hour(datetime_pacific),
    variable = paste0(variable, "_", units)
  ) |>
  left_join(dam_codes, by = join_by(dam)) |>
  filter(year >= start_year)

# %% Save hourly power data for PNW parameter analysis
cbt_data_hourly_power =
  cbt_data_pacific |>
  filter(variable == "power_MW") |>
  select(eia_id, dam, datetime_pacific, power = value) |>
  arrange(dam, datetime_pacific)

# %% Aggregate to daily
cbt_data_daily =
  cbt_data_pacific |>
  group_by(year, month, day, variable, dam, eia_id) |>
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
  select(year, month, day, dam, eia_id, variable, value) |>
  arrange(dam, variable, year, month, day, value)

# %% Write outputs
write_csv(cbt_data_hourly_power, hourly_power_file)
write_csv(cbt_data_daily, daily_data_file)
