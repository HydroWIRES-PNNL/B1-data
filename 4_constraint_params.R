# 1-pnw-params.R
#
# USACE monthly and weekly parameters!
#
# 2022 Original Version - Sean Turner sean.turner@ornl.gov
# 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2024 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2025 Update - Cameron Bracken cameron.bracken@pnnl.gov

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')

# %%
# prepare continuous datetime sequences in local time for the CRB (mostly)
datetime_sequence = tibble(
  datetime = seq(
    as.POSIXct(s("{start_year}-01-01 00:00:00"), tz = 'UTC'),
    as.POSIXct(s("{end_year}-12-31 23:00:00"), tz = 'UTC'),
    by = "hour"
  )
)

sequence_monthly = datetime_sequence |>
  mutate(
    year = year(datetime),
    month = month(datetime),
    date = date(datetime)
  ) |>
  select(year, month, date) |>
  unique()

sequence_weekly = datetime_sequence |>
  mutate(year = year(datetime)) |>
  group_by(year) |>
  group_split() |>
  map_dfr(
    function(x) {
      x[["datetime"]][1] |> year() -> yr

      x |>
        mutate(week_commencing = floor_date(date(datetime), "week", 7)) |>
        mutate(
          week_commencing = if_else(
            year(week_commencing) < yr,
            ymd(paste0(yr, "-01-01")),
            week_commencing
          )
        ) |>
        mutate(date = date(datetime)) |>
        select(-datetime) |>
        unique()
    }
  )

# %% Pull in data for the CRB/PNW

# TODO make nameplate dynamic based on date
pudl_generators_fn = 'data/out_eia__yearly_generators.parquet'
nameplates = read_eia_pudl_generators_parquet_hydro(pudl_generators_fn) |>
  filter(year(datetime) == 2022) |>
  group_by(eia_id, plant_name_eia, datetime) |>
  mutate(nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw, sum)) |>
  ungroup() |>
  # only inlcude the first timestep
  distinct(eia_id, .keep_all = T) |>
  filter(!is.na(nameplate_mw)) |>
  select(eia_id, nameplate_mw)

dam_codes =
  s('{data_dir}/columbia_plants_eia_id.csv') |>
  read_csv(col_types = "ccc") |>
  rename(dam = usace_name, dam_name = usace)

eia_id_and_nameplate = nameplates |> left_join(dam_codes, by = join_by(eia_id))

# get the CRB hourly data, which was previously downloaded
cbt_hourly_complete_raw =
  s('{data_dir}/usace_dam_data_hourly_{start_year}_{end_year}.csv') |>
  read_csv()

# format the raw gen data and clean up some bad points
hourly_gen_all_plants =
  cbt_hourly_complete_raw |>
  filter(variable == 'power') |>
  rename(power_mw = value) |>
  select(dam, datetime_utc, power_mw) |>
  arrange(dam, datetime_utc, power_mw) |>
  complete(datetime_utc, dam) |>
  # mutate(datetime = with_tz(datetime_utc, 'US/Pacific')) |>
  left_join(eia_id_and_nameplate, by = "dam") |>
  mutate(power_mw = if_else(power_mw <= 0, 0, power_mw)) |>
  # could set these to nameplate or assume a data error and set to NA
  mutate(power_mw = if_else(power_mw > nameplate_mw, NA_real_, power_mw)) |>
  mutate(datetime_local = with_tz(datetime_utc, 'US/Pacific'))

# # should return no rows
# hourly_gen_all_plants |>
#   filter(power_mw > nameplate_mw * 1.25 | power_mw < 0)

# %% stats for daily/weekly/monthly
daily_stats =
  hourly_gen_all_plants |>
  na.omit() |>
  mutate(
    year = year(datetime_local),
    month = month(datetime_local),
    day = day(datetime_local)
  ) |>
  # mutate(date = date(datetime)) |>
  group_by(dam, year, month, day) |>
  summarise(
    mwh = sum(power_mw, na.rm = T),
    max_mw = max(power_mw, na.rm = T),
    min_mw = min(power_mw, na.rm = T),
    dor = max_mw - min_mw
  ) |>
  ungroup() |>
  mutate(date = as.Date(ISOdate(year, month, day, tz = 'US/Pacific'))) |>
  filter(year >= start_year, year <= end_year) |>
  select(-c(year, month, day))

# %% stats for daily/weekly/monthly
monthly_stats_pnw =
  daily_stats |>
  left_join(sequence_monthly, by = "date") |>
  mutate(
    max_mw = if_else(is.infinite(max_mw), NA_real_, max_mw),
    min_mw = if_else(is.infinite(min_mw), NA_real_, min_mw),
    dor = if_else(is.infinite(dor), NA_real_, dor)
  ) |>
  group_by(year, month, dam) |>
  summarise(
    mwh = sum(mwh),
    max_mw = max(max_mw, na.rm = T),
    min_mw = min(min_mw, na.rm = T),
    ador = mean(dor, na.rm = T),
    n_hours = n() * 24
  )

# %% stats for daily/weekly/monthly
weekly_stats_pnw =
  daily_stats |>
  left_join(sequence_weekly, by = "date") |>
  group_by(year, week_commencing, dam) |>
  summarise(
    mwh = sum(mwh, na.rm = T),
    max_mw = max(max_mw, na.rm = T),
    min_mw = min(min_mw, na.rm = T),
    ador = mean(dor, na.rm = T),
    n_hours = n() * 24,
  )

weekly_stats_pnw |>
  mutate(
    max_mw = if_else(is.infinite(max_mw), NA_real_, max_mw),
    min_mw = if_else(is.infinite(min_mw), NA_real_, min_mw),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean_mw = mwh / n_hours) |>
  filter(dam == "GCL") |>
  ggplot(aes(week_commencing, mean_mw, group = dam)) +
  geom_line() +
  geom_line(aes(y = max_mw)) +
  geom_line(aes(y = min_mw)) +
  facet_wrap(~year, scales = "free_x")


# %%
# get max, min, and ador params for each of the PNW 28 (single parameter, not varying monthly)
# monthly
# TODO make these dynamic by month
monthly_params =
  monthly_stats_pnw |>
  left_join(eia_id_and_nameplate, by = join_by(dam)) |>
  mutate(
    max_mw = if_else(is.infinite(max_mw), NA_real_, max_mw),
    min_mw = if_else(is.infinite(min_mw), NA_real_, min_mw),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean_mw = mwh / n_hours) |>
  mutate(
    max_param = (max_mw - mean_mw) / (nameplate_mw - mean_mw),
    min_param = min_mw / mean_mw,
    ador_param = ador / (max_mw - min_mw)
  ) |>
  group_by(dam) |>
  mutate(
    max_param_ = median(max_param, na.rm = T),
    min_param_ = median(min_param, na.rm = T),
    ador_param_ = median(ador_param, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    p_max = mean_mw + max_param_ * (nameplate_mw - mean_mw),
    p_min = min_param_ * mean_mw,
    p_ador = (max_mw - min_mw) * ador_param_
  ) |>
  # filter(dam == "DWR") |>
  # ggplot(aes(month, mean_mw, group = dam)) +
  # geom_line() +
  # geom_line(aes(y = max_mw)) +
  # geom_line(aes(y = min_mw)) +
  # facet_wrap(~year, scales = "free_x") +
  # geom_line(aes(y = p_max), col = "red") +
  # geom_line(aes(y = p_min), col = "red") +
  # geom_line(aes(y = ador), col = "blue") +
  # geom_line(aes(y = p_ador), col = "hotpink")
  select(
    dam,
    # month,
    eia_id,
    max_param = max_param_,
    min_param = min_param_,
    ador_param = ador_param_
  ) |>
  distinct_all()

monthly_params |>
  write_csv("data/b1_params_pnw_federal_dams_max_min_ador_monthly.csv")

# %%
# get max, min, and ador params for each of the PNW 28 (single parameter, not varying monthly)
# weekly
weekly_params =
  weekly_stats_pnw |>
  left_join(eia_id_and_nameplate, by = join_by(dam)) |>
  mutate(
    max_mw = if_else(is.infinite(max_mw), NA_real_, max_mw),
    min_mw = if_else(is.infinite(min_mw), NA_real_, min_mw),
    ador = if_else(is.infinite(ador), NA_real_, ador)
  ) |>
  mutate(mean_mw = mwh / n_hours) |>
  mutate(
    max_param = (max_mw - mean_mw) / (nameplate_mw - mean_mw),
    min_param = min_mw / mean_mw,
    ador_param = ador / (max_mw - min_mw)
  ) |>
  group_by(dam) |>
  mutate(
    max_param_ = median(max_param, na.rm = T),
    min_param_ = median(min_param, na.rm = T),
    ador_param_ = median(ador_param, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    p_max = mean_mw + max_param_ * (nameplate_mw - mean_mw),
    p_min = min_param_ * mean_mw,
    p_ador = (max_mw - min_mw) * ador_param_
  ) |>
  # filter(dam == "DWR") |>
  # ggplot(aes(week_commencing, mean_mw, group = dam)) +
  # geom_line() +
  # geom_line(aes(y = max_mw)) +
  # geom_line(aes(y = min_mw)) +
  # facet_wrap(~year, scales = "free_x") +
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
  distinct_all()

weekly_params |>
  write_csv("data/b1_params_pnw_federal_dams_max_min_ador_weekly.csv")

# %%
hourly_gen_all_plants |>
  filter(year(datetime_local) == 2011) |>
  # filter(plant %in% plants[19:28]) |>
  ggplot(aes(datetime_local, power_mw)) +
  geom_line() +
  facet_wrap(~dam, scales = "free_y")

# # compute statistics
# hourly_with_weeks = hourly_gen_all_plants |>
#   # filter(year(datetime_local) == 2001) |>
#   mutate(
#     week_commencing = if_else(
#       day(datetime_local) <= 6,
#       ymd(s("2001-01-01")),
#       floor_date(date(datetime_local), "week", 7)
#     ),
#     week = as.integer(factor(week_commencing))
#     # week = format(datetime, '%U')
#   )

# ador =
#   hourly_with_weeks |>
#   filter(week < 53) |>
#   mutate(date = date(datetime_local)) |>
#   group_by(dam, date, week, week_commencing) |>
#   summarise(
#     daily_range = max(power_mw, na.rm = T) - min(power_mw, na.rm = T),
#     .groups = "drop"
#   ) |>
#   group_by(dam, week, week_commencing) |>
#   summarise(ador = mean(daily_range, na.rm = T), .groups = "drop")

# USACE_weekly_parameters = hourly_with_weeks |>
#   group_by(dam, week, week_commencing) |>
#   summarise(
#     p_max = max(power_mw, na.rm = T),
#     p_min = min(power_mw, na.rm = T),
#     p_avg = mean(power_mw, na.rm = T),
#     n_hours = n(),
#     .groups = "drop"
#   ) |>
#   left_join(ador, by = join_by(dam, week, week_commencing)) |>
#   # ggplot(aes(week_commencing, p_avg)) +
#   # geom_line() + facet_wrap(~dam, scales = "free_y") +
#   # expand_limits(y = 0) +
#   # geom_line(aes(y = p_max), col = "blue") +
#   # geom_line(aes(y = p_min), col = "red") +
#   # geom_line(aes(y = ador), col = "pink")
#   left_join(dam_codes, by = "dam") |>
#   mutate(target_mwh = p_avg * n_hours) |>
#   select(
#     eia_id,
#     week,
#     week_commencing,
#     n_hours,
#     target_mwh,
#     p_avg,
#     p_max,
#     p_min,
#     ador
#   ) |>
#   mutate_if(is.numeric, function(x) round(x, 4))

# # USACE_weekly_parameters |>
# # filter(eia_id == 3076) |>
# # print(n = 53)

# readr::write_csv(USACE_weekly_parameters, "data/USACE_weekly_parameters_28.csv")
