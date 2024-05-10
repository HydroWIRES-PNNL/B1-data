library(tidyverse)

dam_codes <- tribble(
  ~dam, ~EIA_ID,
  "BON", 3075L,
  "CHJ", 3921L,
  "GCL", 6163L,
  "IHR", 3925L,
  "JDA", 3082L,
  "LGS", 3926L,
  "LMN", 3927L,
  "LWG", 6175L,
  "MCN", 3084L,
  "PRD", 3887L,
  "TDA", 3895L,
  "LIB", 6172L,
  "ALF", 851L,
  "BCL", 3074L,
  "CGR", 3076L,
  "DET", 3077L,
  "DEX", 3078L,
  "DWR", 840L,
  "FOS", 6552L,
  "GPR", 3080L,
  "HCR", 3081L,
  "HGH", 2203L,
  "LOP", 3083L,
  "LOS", 6174L,
  "RIS", 6200L,
  "RRH", 3883L,
  "WAN", 3888L,
  "WEL", 3886L
)

#
# if (dam %in% c(
#   'BCL', 'CGR', 'DET',
#   'DEX', 'FOS', 'GPR',
#   'LOS', 'HCR', 'HGH',
#   'LOP', 'LOS'
# )) {
#   data_type <- 'CBT-REV'
# }

start_year <- 2001
url_base <- "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
period <- "lookforward=0h0m&startdate=01/01/%s&enddate=01/25/2024" |> sprintf(start_year)
out_dir <- "data/usace"

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

willamette_projects <- c("BCL", "CGR", "DET", "DEX", "FOS", "GPR", "HCR", "LOP", "LOS")

# set up the query strings, some damn use different naming conventions
data_query <- expand.grid(dam = dam_codes$dam, variable = names(data_string)) |>
  left_join(dam_codes, by = "dam") |>
  left_join(data.frame(
    variable = names(data_string),
    data_string = data_string,
    cbt_type = cbt_type,
    cbt_units = cbt_units
  ), by = "variable") |>
  mutate(
    # Willamettes use a different postfix
    cbt_type = if_else(
      variable == "power" & dam %in% c(willamette_projects, "HGH"),
      "CBT-REV", cbt_type
    ),
    cbt_type = if_else(
      variable %in% c("inflow", "outflow", "forebay") & dam %in% willamette_projects,
      "Best", cbt_type
    ),
    # Bonneville only has instantaneous forebay
    data_string = if_else(
      variable == "forebay" & dam == "BON",
      ".Elev-Forebay.Inst.~1Day.0.", data_string
    ),
    cbt_type = if_else(
      variable == "forebay" & dam == "BON",
      "CBT-RAW", cbt_type
    ),
    # these dams only have daily outflow
    data_string = if_else(
      variable == "outflow" & dam %in% c("DET", "GPR", "LOP"),
      ".Flow-Out.Ave.~1Day.1Day.", data_string
    )
  )

# pull the data, some of the larger queries (inst forebay) might timeout, if so, just try it again
data_query %>%
  split(rownames(.)) |>
  map(function(r) {
    cbt_string <- with(r, paste0(dam, data_string, cbt_type))
    url <- paste0(
      url_base, cbt_string,
      ":units=", r$cbt_units,
      "&headers=true&timezone=GMT&", period
    )
    # message(url)
    message(cbt_string)
    output_fn <- sprintf("%s/%s.csv", out_dir, cbt_string)
    # save the output to a file if the file does not exist, otherwise read from file
    if (!file.exists(output_fn)) {
      x <- read_csv(url, show = F, progress = F)
      write_csv(x, output_fn)
    } else {
      x <- read_csv(output_fn, show = F, progress = F)
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
  }) |>
  bind_rows() -> cbt_data

cbt_data |>
  mutate(
    datetime_pacific = with_tz(datetime, "US/Pacific"),
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
  filter(year >= start_year) ->
cbt_data_daily

write_csv(cbt_data_daily, "data/pnw_daily_data.csv")

# "https://www.nwd-wc.usace.army.mil/dd/common/web_service/webexec/ecsv?id="
# "BON.Elev-Forebay.Inst.1Hour.0.CBT-REV:units=ft&headers=true&filename=&timezone=PST&"
# "lookforward=0h0m&startdate=01/01/2001+08:00&enddate=01/25/2024+08:00"
