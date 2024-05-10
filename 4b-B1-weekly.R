# B1 weekly.

# break up monthly B1 datasets into weekly resolution.
library(tidyverse)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

end_year <- 2022
eha_fn <- "data/ORNL_EHAHydroPlant_FY2023_rev.xlsx"
output_dir <- "B1_weekly"

dir.create(output_dir, showWarnings = FALSE)

tibble(
  date_time = seq(as.POSIXct("2001-01-01 00:00:00"),
    as.POSIXct(sprintf("%s-12-31 23:00:00", end_year)),
    by = "hour"
  )
) -> date_time_sequence

date_time_sequence %>%
  mutate(
    year = year(date_time), month = month(date_time, label = T),
    date = date(date_time)
  ) %>%
  select(year, month, date) %>%
  unique() -> sequence_monthly

date_time_sequence %>%
  mutate(year = year(date_time)) %>%
  split(.$year) %>%
  map_dfr(
    function(x) {
      x[["date_time"]][1] %>% year() -> yr

      rep(1:53, each = 7) -> weekdef

      x %>%
        # mutate(week_commencing = floor_date(date(date_time), "week", 8)) %>%
        # mutate(week_commencing = if_else(year(week_commencing) < yr,
        #                                  ymd(paste0(yr, "-01-01")), week_commencing)) %>%
        mutate(date = date(date_time)) %>%
        select(-date_time) %>%
        unique() %>%
        mutate(jweek = weekdef[1:n()]) %>%
        group_by(jweek) %>%
        mutate(
          n_hours = 24 * n(),
          week_start = min(date)
        )
    }
  ) -> sequence_weekly

# read table of EIA ids associated with HUC4
EIA_and_HUC4 <- read_csv("data/eia_huc4.csv")

# read flow data created by `2-streamflow.R`
flow_huc4 <- read_csv("data/HUC4_average_flows_imputed.csv")
flow_gauge <- read_csv("data/flow/proc/flow_all_td.csv") |>
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) |>
  rename(av_flow_cfs = value)

# PNW dam data, daily forebay, inflow, outflow, power
pnw_dam_data <- read_csv("data/pnw_daily_data.csv") |>
  pivot_wider(id_cols = c(year, month, day, dam, EIA_ID), names_from = "variable") |>
  mutate(inflow_cfs = inflow_kcfs * 1000, outflow_cfs = outflow_kcfs * 1000)

# read monthly B1 data
monthly_all <-
  2001:end_year %>%
  map_dfr(function(yr) {
    read_csv(paste0("B1_monthly/B1_monthly_", yr, ".csv"), show = F) %>%
      mutate(month = factor(month, levels = month.abb, ordered = T))
  })


# disaggregate to daily and aggregate back to weekly
2001:end_year %>%
  map(function(yr) {
    message(yr)

    sequence_weekly %>%
      filter(year == yr) %>%
      mutate(month = month(date, label = T)) -> wk_seq

    monthly_all %>%
      filter(year == yr) %>%
      # filter(EIA_ID == 314) -> x
      split(.$EIA_ID) %>% # .[[14]] -> x
      map(function(x) {
        x$EIA_ID %>% unique() -> EIA_ID_

        # message("\t", yr, EIA_ID_)

        HUC <- EIA_and_HUC4 %>%
          filter(EIA_ID %in% EIA_ID_) %>%
          .[["HUC4"]]

        if (length(HUC) == 0) HUC <- NA_character_

        # CB: I have no idea why EIA 314 is special, the data looks fine to me
        # but maybe sean decided it was bogus
        if (EIA_ID_ == 314) HUC <- NA_character_


        plant_has_associated_huc4 <- EIA_ID_ %in% EIA_and_HUC4$EIA_ID
        plant_has_release_flow <- nrow(flow_gauge %>% filter(EIA_ID == EIA_ID_, year == yr)) >= 365
        plant_has_huc4_flow <- nrow(flow_huc4 %>% filter(HUC4 == HUC, year == yr)) >= 365

        # if the EIA ID does not have a HUC associated or if the flow
        # data is incomplete then just split evenly across each week
        if (!plant_has_associated_huc4 | !(plant_has_huc4_flow | plant_has_release_flow)) {
          x %>%
            select(month, target_MWh) %>%
            left_join(wk_seq,
              by = c("month")
            ) %>%
            group_by(month) %>%
            mutate(n_days = n()) %>%
            mutate(daily_gen_MWh = target_MWh / n_days) %>%
            group_by(jweek, week_start) %>%
            summarise(
              target_MWh = sum(daily_gen_MWh),
              n_hours = 24 * n(), .groups = "drop"
            ) %>%
            mutate(
              year = yr, EIA_ID = x[["EIA_ID"]][1],
              nameplate = x[["nameplate"]][1],
              plant = x[["plant"]][1],
              state = x[["state"]][1]
            ) ->
          weekly_targets

          return(weekly_targets)
        }

        dam_data <- pnw_dam_data |>
          filter(EIA_ID == EIA_ID_) |>
          select(year, month, day, forebay_ft, inflow_cfs, outflow_cfs)

        # determine which flows to use, release flows are preferable,
        # huc4 flows are the fallback
        flows_for_disag <- if (plant_has_release_flow) {
          flow_gauge %>%
            filter(EIA_ID == EIA_ID_) %>%
            filter(year == yr)
        } else if (plant_has_huc4_flow) {
          flow_huc4 %>%
            filter(HUC4 == HUC) %>%
            filter(year == yr)
        } else {
          # this should not happen since the case should be caught above
          stop("No flow data to disag")
        }

        daily_flow_allocation <- flows_for_disag %>%
          arrange(year, month, day) %>%
          group_by(month, year) %>%
          mutate(daily_allocation = av_flow_cfs / sum(av_flow_cfs)) %>%
          mutate(daily_allocation = if_else(is.nan(daily_allocation), 1 / n(), daily_allocation)) %>%
          select(year, month, day, daily_allocation, av_flow_cfs) %>%
          ungroup() %>%
          # join in dam data, might be empty
          left_join(dam_data, by = join_by(year, month, day)) %>%
          mutate(month = month(month, label = T))

        weekly_targets <- x %>%
          select(month, target_MWh) %>%
          left_join(daily_flow_allocation,
            by = c("month")
          ) %>%
          mutate(daily_gen_MWh = daily_allocation * target_MWh) %>%
          mutate(date = ymd(paste0(year, "-", month, "-", day))) %>%
          left_join(select(wk_seq, date, jweek, week_start), by = "date") %>%
          group_by(jweek, week_start) %>%
          summarise(
            target_MWh = sum(daily_gen_MWh),
            n_hours = 24 * n(),
            av_flow_cfs = mean(av_flow_cfs),
            forebay_ft = forebay_ft[1],
            inflow_cfs = mean(inflow_cfs, na.rm = T),
            outflow_cfs = mean(outflow_cfs, na.rm = T),
            .groups = "drop"
          ) %>%
          mutate(
            year = yr, EIA_ID = x[["EIA_ID"]][1],
            nameplate = x[["nameplate"]][1],
            plant = x[["plant"]][1],
            state = x[["state"]][1]
          )

        return(weekly_targets)
      }, .progress = TRUE) |>
      bind_rows() -> all_targets_yr_x

    return(all_targets_yr_x)
  }, .progress = TRUE) |>
  bind_rows() |>
  # add HUC4 and USGS_ID
  left_join(EIA_and_HUC4, by = join_by(EIA_ID)) |>
  left_join(all_flows |> distinct(HUC4, USGS_ID), by = join_by(HUC4)) ->
weekly_targets_all_years


readxl::read_xlsx(eha_fn, sheet = "Operational") %>%
  select(EIA_ID = EIA_PtID, mode = Mode) %>%
  filter(!is.na(EIA_ID)) %>%
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) %>%
  filter(!duplicated(EIA_ID)) %>%
  unique() -> modes

read_csv("PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv", show = F) %>%
  left_join(modes, by = join_by(EIA_ID)) %>%
  group_by(mode) %>%
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  ) ->
mma_params_general

read_csv("PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv", show = F) %>%
  select(-dam) ->
mma_params_pnw


bind_rows(
  weekly_targets_all_years %>%
    left_join(modes, by = "EIA_ID") %>%
    filter(!(EIA_ID %in% mma_params_pnw[["EIA_ID"]])) %>%
    left_join(mma_params_general, by = join_by(mode)),
  weekly_targets_all_years %>%
    left_join(mma_params_pnw, by = join_by(EIA_ID)) %>%
    filter(EIA_ID %in% mma_params_pnw[["EIA_ID"]])
) %>%
  # filter(EIA_ID == 3075) %>%
  mutate(target_MWh = if_else(target_MWh < 0, 0, target_MWh)) %>%
  mutate(p_avg = target_MWh / n_hours) %>%
  mutate(p_avg = if_else(p_avg > nameplate, nameplate, p_avg)) %>%
  mutate(
    p_max = p_avg + max_param * (nameplate - p_avg),
    p_min = min_param * p_avg,
    ador = ador_param * (p_max - p_min)
  ) %>%
  select(
    EIA_ID, HUC4, USGS_ID, plant, state, year, jweek,
    week_start, n_hours, target_MWh, nameplate, p_avg, p_min, p_max, ador,
    av_flow_cfs, forebay_ft, inflow_cfs, outflow_cfs
  ) |>
  rename(HUC4_flow_cfs = av_flow_cfs) ->
weekly_final

weekly_final %>%
  mutate(
    EIA_ID = as.integer(EIA_ID),
    year = as.integer(year)
  ) %>%
  # filter(EIA_ID == 153) %>%
  # ggplot(aes(jweek, p_avg, group = year)) + geom_line() + facet_wrap(~year) +
  # geom_line(aes(y = p_min), col = "red") +
  # geom_line(aes(y = p_max), col = "blue") +
  # geom_line(aes(y = ador), col = "pink")
  # filter(p_min < 0)
  # filter(p_min > p_max)
  # filter(p_min > (p_max - ador)) #240 cases
  # filter(p_min < 0) #2 cases
  # filter(p_min > p_avg) #483
  # filter(p_max < p_avg) #466 cases
  # filter(p_max < p_min) #....
  # filter(ador < 0)
  # filter(p_avg < 0)
  mutate_if(is.double, function(x) round(x, 4)) %>%
  mutate(Western = if_else(state %in% c(
    "WA", "ID", "CO", "UT",
    "NM", "WY", "MT", "CA",
    "OR", "NV", "AZ"
  ), TRUE, FALSE)) %>%
  arrange(-Western) %>%
  split(.$year) %>%
  map(function(x) {
    x %>%
      pull(year) %>%
      .[1] -> yr
    write_csv(x, paste0(output_dir, "/B1_weekly_", yr, ".csv"), na = "")
  }) -> shhh

weekly <- list.files(output_dir, full.names = T) |>
  map(function(x) read_csv(x, progress = F, show = F)) |>
  bind_rows()

# weekly |>
#   filter(Western == TRUE) |>
#   group_by(year, week_start) |>
#   summarise(energy_mwh = sum(target_MWh), .groups = "drop") |>
#   filter(year %in% c(2001, 2009)) |>
#   mutate(week_start = `year<-`(week_start, 2000)) |>
#   filter(week_start < as.Date("2000-12-31")) |>
#   ggplot(aes(week_start, energy_mwh / 1000, fill = factor(year))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual("", values = c("orange", "cornflowerblue")) +
#   scale_x_date(date_breaks = "month", date_labels = "%b") +
#   theme_bw() +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "", y = "Energy [GWh]")

#
# weekly |>
#   filter(Western == TRUE) |>
#   group_by(year, week_start) |>
#   summarise(energy_mwh = sum(target_MWh), .groups = "drop") |>
#   filter(year %in% c(2001, 2009)) |>
#   mutate(week_start = `year<-`(week_start, 2000)) |>
#   pivot_wider(id_cols = week_start, names_from = year, values_from = energy_mwh) |>
#   mutate(pct_diff = (`2001` - `2009`) / `2009` * 100) |>
#   print(n = 100)
