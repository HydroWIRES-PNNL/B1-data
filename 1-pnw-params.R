# 1-pnw-params.R
#
# USACE monthly and weekly parameters!
#
# 2022 Original Version - Sean Turner sean.turner@ornl.gov
# 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2024 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2025 Update - Cameron Bracken cameron.bracken@pnnl.gov

# ----------------------------------------------------------------------------
# setup ----------------------------------------------------------------------
# ----------------------------------------------------------------------------

# %% load conflicted first to avoid warnings
library(conflicted)

# %% renv
## initial setup
# install.packages('renv')
# renv::init()
# vignette("renv")
# renv::snapshot()
library(renv)
renv::load()

# %% packages and utility functions
library(conflicted)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(glue)
library(sf)
library(tools)
library(janitor)
library(glue)
# shortcut for string interpolation like pythins f strings
s = glue::glue

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

source('utilities.R')

# %% settings
# directory for all data
data_dir = 'data'

# output will be filtered to these years
# TODO add filtering everywhere
year_range = 1980:2024
start_year = min(year_range)
end_year = max(year_range)

# %%
# prepare continuous date_time sequence
date_time_sequence = tibble(
  date_time = seq(
    as.POSIXct(s("{start_year}-01-01 00:00:00")),
    as.POSIXct(s("{end_year}-12-31 23:00:00")),
    by = "hour"
  )
)

sequence_monthly = date_time_sequence |>
  mutate(
    year = year(date_time),
    month = month(date_time, label = T),
    date = date(date_time)
  ) |>
  select(year, month, date) |>
  unique()

sequence_weekly = date_time_sequence |>
  mutate(year = year(date_time)) |>
  group_by(year) |>
  group_split() |>
  map_dfr(
    function(x) {
      x[["date_time"]][1] |> year() -> yr

      x |>
        mutate(week_commencing = floor_date(date(date_time), "week", 7)) |>
        mutate(
          week_commencing = if_else(
            year(week_commencing) < yr,
            ymd(paste0(yr, "-01-01")),
            week_commencing
          )
        ) |>
        mutate(date = date(date_time)) |>
        select(-date_time) |>
        unique()
    }
  )

# TODO update to use the pudl eia data
nameplates =
  hydrofixr::read_EIA_capabilities(
    data_dir = file.path(data_dir, "hydrofixr/")
  ) |>
  clean_names() |>
  select(eia_id, nameplate) |>
  unique()

eia_id_and_nameplate = hydrofixr:::dam_codes |>
  bind_rows(tibble(dam = "LIB", eia_id = 6172)) |>
  left_join(nameplates, by = join_by(eia_id))


hourly_all_plants =
  s("{data_dir}/hydrofixr/USACE_hourly_gen_raw_27_plants_MST_2001_2022.csv") |>
  read_csv() |>
  select(dam, date_time, power) |>
  group_by(dam) |>
  group_split() |>
  map(function(x) {
    date_time_sequence |>
      mutate(date_time = date_time - (8 * 60 * 60)) |>
      left_join(select(x, -dam), by = "date_time") |>
      mutate(dam = x[["dam"]][1]) |>
      left_join(eia_id_and_nameplate, by = "dam") |>
      # remove a few weird high points in PRD, WEL, WAN
      mutate(power = if_else(power > nameplate, NA_real_, power)) |>
      mutate(power = if_else(power <= 0, NA_real_, power)) |>
      tidyr::fill(power, .direction = "down")
  }) |>
  bind_rows()


daily_stats =
  hourly_all_plants |>
  na.omit() |>
  mutate(date = date(date_time)) |>
  group_by(date, dam) |>
  summarise(
    MWh = sum(power, na.rm = T),
    max = max(power, na.rm = T),
    min = min(power, na.rm = T),
    dor = max - min
  )

daily_stats |>
  left_join(sequence_monthly, by = "date") |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    dor = if_else(is.infinite(dor), NA_real_, dor)
  ) |>
  group_by(year, month, dam) |>
  summarise(
    MWh = sum(MWh),
    max = max(max, na.rm = T),
    min = min(min, na.rm = T),
    ador = mean(dor, na.rm = T),
    n_hours = n() * 24,
    .groups = "drop"
  ) -> monthly_stats_PNW

daily_stats |>
  left_join(sequence_weekly, by = "date") |>
  group_by(year, week_commencing, dam) |>
  summarise(
    MWh = sum(MWh),
    max = max(max, na.rm = T),
    min = min(min, na.rm = T),
    ador = mean(dor),
    n_hours = n() * 24,
    .groups = "drop"
  ) -> weekly_stats_PNW

weekly_stats_PNW |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean = MWh / n_hours) |>
  filter(dam == "GPR") |>
  ggplot(aes(week_commencing, mean, group = dam)) +
  geom_line() +
  geom_line(aes(y = max)) +
  geom_line(aes(y = min)) +
  facet_wrap(~year, scales = "free_x")


# get max, min, and ador params for each of the PNW 28 (single parameter, not varying monthly)
monthly_stats_PNW |>
  left_join(eia_id_and_nameplate) |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean = MWh / n_hours) |>
  mutate(
    max_param = (max - mean) / (nameplate - mean),
    min_param = min / mean,
    ador_param = ador / (max - min)
  ) |>
  group_by(dam) |>
  mutate(
    max_param_ = median(max_param, na.rm = T),
    min_param_ = median(min_param, na.rm = T),
    ador_param_ = median(ador_param, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    p_max = mean + max_param_ * (nameplate - mean),
    p_min = min_param_ * mean,
    p_ador = (max - min) * ador_param_
  ) |>
  # filter(dam == "DWR") |>
  # ggplot(aes(month, mean, group = dam)) + geom_line() + geom_line(aes(y = max)) +
  # geom_line(aes(y = min)) +facet_wrap(~year, scales = "free_x") +
  # geom_line(aes(y = p_max), col = "red") +
  # geom_line(aes(y = p_min), col = "red") +
  # geom_line(aes(y = ador), col = "blue") +
  # geom_line(aes(y = p_ador), col = "hotpink")
  select(
    dam,
    eia_id,
    max_param = max_param_,
    min_param = min_param_,
    ador_param = ador_param_
  ) |>
  unique() |>
  write_csv("data/PNW_28_max_min_ador_parameters.csv")


weekly_stats_PNW |>
  left_join(eia_id_and_nameplate) |>
  mutate(
    max = if_else(is.infinite(max), NA_real_, max),
    min = if_else(is.infinite(min), NA_real_, min),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean = MWh / n_hours) |>
  mutate(
    max_param = (max - mean) / (nameplate - mean),
    min_param = min / mean,
    ador_param = ador / (max - min)
  ) |>
  group_by(dam) |>
  mutate(
    max_param_ = median(max_param, na.rm = T),
    min_param_ = median(min_param, na.rm = T),
    ador_param_ = median(ador_param, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    p_max = mean + max_param_ * (nameplate - mean),
    p_min = min_param_ * mean,
    p_ador = (max - min) * ador_param_
  ) |>
  # filter(dam == "DWR") |>
  # ggplot(aes(month, mean, group = dam)) + geom_line() + geom_line(aes(y = max)) +
  # geom_line(aes(y = min)) +facet_wrap(~year, scales = "free_x") +
  # geom_line(aes(y = p_max), col = "red") +
  # geom_line(aes(y = p_min), col = "red") +
  # geom_line(aes(y = ador), col = "blue") +
  # geom_line(aes(y = p_ador), col = "hotpink")
  select(
    dam,
    eia_id,
    max_param = max_param_,
    min_param = min_param_,
    ador_param = ador_param_
  ) |>
  unique() |>
  write_csv("data/PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv")


#
#
hourly_all_plants |>
  filter(year(date_time) == 2011) |>
  # filter(plant %in% plants[19:28]) |>
  ggplot(aes(date_time, power)) +
  geom_line() +
  facet_wrap(~dam, scales = "free_y")


# compute statistics
hourly_all_plants |>
  filter(year(date_time) == 2001) |>
  mutate(
    week_commencing = if_else(
      day(date(date_time)) <= 6,
      ymd("2001-01-01"),
      floor_date(date(date_time), "week", 7)
    ),
    week = as.integer(factor(week_commencing))
    # week = format(date_time, '%U')
  ) -> hourly_with_weeks

hourly_with_weeks |>
  filter(week < 53) |>
  mutate(date = date(date_time)) |>
  group_by(dam, date, week, week_commencing) |>
  summarise(daily_range = max(power) - min(power), .groups = "drop") |>
  group_by(dam, week, week_commencing) |>
  summarise(ador = mean(daily_range), .groups = "drop") -> ador


hourly_with_weeks |>
  group_by(dam, week, week_commencing) |>
  summarise(
    p_max = max(power),
    p_min = min(power),
    p_avg = mean(power),
    n_hours = n(),
    .groups = "drop"
  ) |>
  left_join(ador, c("dam", "week", "week_commencing")) |>
  # ggplot(aes(week_commencing, p_avg)) +
  # geom_line() + facet_wrap(~dam, scales = "free_y") +
  # expand_limits(y = 0) +
  # geom_line(aes(y = p_max), col = "blue") +
  # geom_line(aes(y = p_min), col = "red") +
  # geom_line(aes(y = ador), col = "pink")
  left_join(
    hydrofixr:::dam_codes |> bind_rows(tibble(dam = "LIB", eia_id = 6172L)),
    by = "dam"
  ) |>
  mutate(target_MWh = p_avg * n_hours) |>
  select(
    eia_id,
    week,
    week_commencing,
    n_hours,
    target_MWh,
    p_avg,
    p_max,
    p_min,
    ador
  ) |>
  mutate_if(is.numeric, function(x) round(x, 4)) -> USACE_weekly_parameters


# USACE_weekly_parameters |>
# filter(eia_id == 3076) |>
# print(n = 53)

readr::write_csv(USACE_weekly_parameters, "data/USACE_weekly_parameters_28.csv")
