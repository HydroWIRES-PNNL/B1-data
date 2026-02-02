# ==============================================================================
# 5b-B1-weekly.R
# ==============================================================================
#
#   Generate weekly hydropower dataset by disaggregating annual EIA data
#   to daily resolution using flow-based proxies, then aggregating to weekly
#   targets. Applies PNW-derived parameters to calculate p_max, p_min, and ADOR.
#
# Approach:
#   1. Disaggregate monthly energy to daily using flow patterns (huc4 or release)
#   2. Aggregate daily values to weekly resolution
#   3. Apply max, min, ador parameters (PNW-specific or mode-averaged)
#   4. Join huc4 flows and PNW dam data where available
#
# Authors: Cameron Bracken (cameron.bracken@pnnl.gov)
#
# Version History:
#   v1.4.0 Update - Jan 2026 - Cameron Bracken
# ==============================================================================

# %% Libraries
library(tidyverse)
# for writing parquet files
library(arrow)

# %% Configuration
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

start_year = 2001
end_year = 2024
output_prefix = "B1_data"
version = "1.4.0"

# Input paths
eha_file = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"
eia_huc4_file = "data/eia_huc4.csv"
flow_gauge_file = "data/flow/proc/flow_all_td.csv"
huc4_flows_file = "output/huc4_average_flows_imputed.csv"
pnw_params_weekly_file = "output/PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv"

# Output paths
output_dir = "output"
b1_dir = file.path(output_dir, paste0(output_prefix, "_", version))

# Final output name
b1_weekly_fn = file.path(b1_dir, "B1_weekly.parquet")
b1_monthly_fn = file.path(b1_dir, "B1_monthly.parquet")

# Create directories
dir.create(output_dir, showWarnings = FALSE)
dir.create(b1_dir, showWarnings = FALSE, recursive = TRUE)

# %% Create date/time sequences
date_time_sequence =
  tibble(
    date_time = seq(
      as.POSIXct(sprintf("%s-01-01 00:00:00", start_year)),
      as.POSIXct(sprintf("%s-12-31 23:00:00", end_year)),
      by = "hour"
    )
  )

sequence_monthly =
  date_time_sequence |>
  mutate(
    year = year(date_time),
    month = month(date_time),
    date = date(date_time)
  ) |>
  select(year, month, date) |>
  unique()

sequence_weekly =
  date_time_sequence |>
  mutate(year = year(date_time)) |>
  group_by(year) |>
  group_split() |>
  map_dfr(
    \(x) {
      yr = x[["date_time"]][1] |> year()

      weekdef = rep(1:53, each = 7)

      x |>
        mutate(date = date(date_time)) |>
        select(-date_time) |>
        unique() |>
        mutate(jweek = weekdef[1:n()]) |>
        group_by(jweek) |>
        mutate(
          n_hours = 24 * n(),
          week_start = min(date)
        )
    }
  )

# %% Load crosswalks and flow data
eia_and_huc4 =
  read_csv(eia_huc4_file) |>
  rename_all(tolower)

all_flows =
  read_csv(huc4_flows_file, show = FALSE, progress = FALSE)

flow_huc4 =
  read_csv(huc4_flows_file)

flow_gauge =
  read_csv(flow_gauge_file) |>
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) |>
  rename(av_flow_cfs = value)

# %% Load monthly B1 data
b1_monthly =
  read_parquet(b1_monthly_fn)

# %% Weekly disaggregation
weekly_targets_all_years =
  2001:end_year |>
  map(
    \(yr) {
      message(yr)

      wk_seq =
        sequence_weekly |>
        filter(year == yr) |>
        mutate(month = month(date))

      all_targets_yr_x =
        b1_monthly |>
        filter(year == yr) |>
        group_by(eia_id) |>
        group_split() |>
        map(
          \(x) {
            # browser()
            eia_id_ = x$eia_id |> unique()

            HUC = eia_and_huc4 |>
              filter(eia_id %in% eia_id_) %>%
              .[["huc4"]]

            if (length(HUC) == 0) {
              HUC = NA_character_
            }

            if (eia_id_ == 314) {
              HUC = NA_character_
            }

            plant_has_associated_huc4 = eia_id_ %in% eia_and_huc4$eia_id
            plant_has_release_flow = nrow(
              flow_gauge |> filter(eia_id == eia_id_, year == yr)
            ) >=
              365
            plant_has_huc4_flow = nrow(
              flow_huc4 |> filter(huc4 == HUC, year == yr)
            ) >=
              365

            if (
              !plant_has_associated_huc4 |
                !(plant_has_huc4_flow | plant_has_release_flow)
            ) {
              weekly_targets =
                x |>
                select(month, target_mwh) |>
                left_join(wk_seq, by = c("month")) |>
                group_by(month) |>
                mutate(n_days = n()) |>
                mutate(daily_gen_mwh = target_mwh / n_days) |>
                group_by(jweek, week_start) |>
                summarise(
                  target_mwh = sum(daily_gen_mwh),
                  n_hours = 24 * n(),
                  .groups = "drop"
                ) |>
                mutate(
                  year = yr,
                  eia_id = x[["eia_id"]][1],
                  nameplate = x[["nameplate"]][1],
                  plant = x[["plant"]][1],
                  state = x[["state"]][1]
                )

              return(weekly_targets)
            }

            flows_for_disag = if (plant_has_release_flow) {
              flow_gauge |>
                filter(eia_id == eia_id_) |>
                filter(year == yr)
            } else if (plant_has_huc4_flow) {
              flow_huc4 |>
                filter(huc4 == HUC) |>
                filter(year == yr)
            } else {
              stop("No flow data to disag")
            }

            daily_flow_allocation =
              flows_for_disag |>
              arrange(year, month, day) |>
              group_by(month, year) |>
              mutate(daily_allocation = av_flow_cfs / sum(av_flow_cfs)) |>
              mutate(
                daily_allocation = if_else(
                  is.nan(daily_allocation),
                  1 / n(),
                  daily_allocation
                )
              ) |>
              select(year, month, day, daily_allocation, av_flow_cfs) |>
              ungroup() |>
              mutate(month = month(month))

            weekly_targets =
              x |>
              select(month, target_mwh) |>
              left_join(daily_flow_allocation, by = join_by(month)) |>
              mutate(daily_gen_mwh = daily_allocation * target_mwh) |>
              mutate(date = ymd(paste0(year, "-", month, "-", day))) |>
              left_join(select(wk_seq, date, jweek, week_start), by = "date") |>
              group_by(jweek, week_start) |>
              summarise(
                target_mwh = sum(daily_gen_mwh),
                n_hours = 24 * n(),
                av_flow_cfs = mean(av_flow_cfs),
                .groups = "drop"
              ) |>
              mutate(
                year = yr,
                eia_id = x[["eia_id"]][1],
                nameplate = x[["nameplate"]][1],
                plant = x[["plant"]][1],
                state = x[["state"]][1]
              )

            return(weekly_targets)
          },
          .progress = TRUE
        ) |>
        bind_rows()

      return(all_targets_yr_x)
    },
    .progress = TRUE
  ) |>
  bind_rows() |>
  left_join(eia_and_huc4, by = join_by(eia_id)) |>
  left_join(all_flows |> distinct(huc4, usgs_id), by = join_by(huc4))

# %% Load plant modes
modes =
  readxl::read_xlsx(eha_file, sheet = "Operational") |>
  select(eia_id = EIA_PtID, mode = Mode) |>
  filter(!is.na(eia_id)) |>
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) |>
  filter(!duplicated(eia_id)) |>
  unique()

# %% Load weekly parameters and calculate mode averages
mma_params_general =
  read_csv(pnw_params_weekly_file, show = FALSE) |>
  left_join(modes, by = join_by(eia_id)) |>
  group_by(mode) |>
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  )

mma_params_pnw =
  read_csv(pnw_params_weekly_file, show = FALSE) |>
  select(-dam)

# %% Apply parameters and calculate constraints
weekly_final =
  bind_rows(
    weekly_targets_all_years |>
      left_join(modes, by = "eia_id") |>
      filter(!(eia_id %in% mma_params_pnw[["eia_id"]])) |>
      left_join(mma_params_general, by = join_by(mode)),
    weekly_targets_all_years |>
      left_join(mma_params_pnw, by = join_by(eia_id)) |>
      filter(eia_id %in% mma_params_pnw[["eia_id"]])
  ) |>
  mutate(target_mwh = if_else(target_mwh < 0, 0, target_mwh)) |>
  mutate(p_avg = target_mwh / n_hours) |>
  mutate(p_avg = if_else(p_avg > nameplate, nameplate, p_avg)) |>
  mutate(
    p_max = p_avg + max_param * (nameplate - p_avg),
    p_min = min_param * p_avg,
    ador = ador_param * (p_max - p_min)
  ) |>
  select(
    eia_id,
    huc4,
    usgs_id,
    plant,
    state,
    year,
    jweek,
    week_start,
    n_hours,
    target_mwh,
    nameplate,
    p_avg,
    p_min,
    p_max,
    ador,
    av_flow_cfs
  ) |>
  rename(huc4_flow_cfs = av_flow_cfs)

# %% Final output preparation
weekly_final |>
  mutate(
    eia_id = as.integer(eia_id),
    year = as.integer(year),
    datetime = week_start
  ) |>
  rename(eia_id = eia_id) |>
  mutate_if(is.double, \(x) round(x, 4)) |>
  mutate(
    Western = if_else(
      state %in%
        c("WA", "ID", "CO", "UT", "NM", "WY", "MT", "CA", "OR", "NV", "AZ"),
      TRUE,
      FALSE
    )
  ) |>
  arrange(-Western) |>
  janitor::clean_names(parsing_option = 3) |>
  arrange(eia_id, datetime) |>
  write_parquet(b1_weekly_fn)

# %% Diagnostic plots
weekly = read_parquet(b1_weekly_fn)

weekly |>
  filter(western == TRUE) |>
  group_by(year, week_start) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  mutate(week_start = `year<-`(week_start, 2000)) |>
  filter(week_start < as.Date("2000-12-31")) |>
  ggplot(aes(week_start, energy_mwh / 1000, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("orange", "cornflowerblue")) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Energy [GWh]")
