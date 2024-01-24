# B1 datasets

# Create the hydroWIRES B1 dataset from hydro923plus

# Approach:
# 1. For the monthly version, take hydro923plus and add max, min, ador using average of PNW parameters
# 2. For the weekly version, disaggregate using USGS flows, use same max, min, ador parameters.

library(tidyverse)

end_year <- 2023
rectifhyd_fn <- "data/RectifHyd_v1.2.1.csv"
eha_fn <- "data/ORNL_EHAHydroPlant_FY2023_rev.xlsx"
output_dir <- "B1_monthly"

dir.create(output_dir, showWarnings = FALSE)

# get hours per month for converting MWh to MW average
tibble(date = seq(ISOdate(2001, 1, 1), to = ISOdate(end_year, 12, 31), by = "day")) %>%
  mutate(
    date = lubridate::date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  group_by(year, month) %>%
  summarise(n_days = n(), .groups = "drop") %>%
  ungroup() %>%
  mutate(n_hours = n_days * 24) %>%
  left_join(tibble(month = 1:12, month_abb = month.abb), by = "month") %>%
  mutate(month = factor(month_abb, levels = month.abb)) %>%
  select(year, month, n_hours) -> hours_per_month

readxl::read_xlsx(eha_fn, sheet = "Operational") %>%
  select(EHA_PtID, plant = PtName, EIA_ID = EIA_PtID, nameplate_MW = CH_MW) ->
HS

all_flows <- read_csv("data/HUC4_average_flows_imputed.csv")
hydro923plus <- read_csv(rectifhyd_fn)

hydro923plus %>%
  left_join(HS, by = c("EIA_ID", "plant")) %>%
  mutate(target_MWh = if_else(recommended_data == "RectifHyd",
    RectifHyd_MWh,
    EIA_MWh
  )) %>%
  select(EIA_ID, plant, EHA_PtID, state, year, month, target_MWh, nameplate_MW) %>%
  left_join(hours_per_month, by = c("year", "month")) %>%
  mutate(target_MWh = if_else(target_MWh < 0, 0, target_MWh)) %>%
  mutate(p_avg = target_MWh / n_hours) %>%
  mutate(p_avg = if_else(p_avg > nameplate_MW, nameplate_MW, p_avg)) ->
monthly_targets

monthly_targets %>%
  group_by(EIA_ID, month) %>%
  summarise(p_avg_ = median(p_avg)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = month.abb, ordered = T)) -> replacement_data

monthly_targets %>%
  pull(EIA_ID) %>%
  unique() -> EIA_IDs

expand.grid(
  month = month.abb, year = 2001:end_year,
  EIA_ID = EIA_IDs
) %>%
  as_tibble() -> full_frame

monthly_targets %>%
  select(EIA_ID, EHA_PtID, plant, state, nameplate_MW) %>%
  unique() %>%
  left_join(full_frame, by = "EIA_ID") %>%
  left_join(hours_per_month, by = c("month", "year")) %>%
  left_join(monthly_targets,
    by = c("EIA_ID", "EHA_PtID", "plant", "state", "nameplate_MW", "month", "year", "n_hours")
  ) %>%
  left_join(replacement_data, by = c("EIA_ID", "month")) %>%
  mutate(p_avg = if_else(is.na(p_avg), p_avg_, p_avg)) %>%
  mutate(target_MWh = if_else(is.na(target_MWh), p_avg * n_hours, target_MWh)) %>%
  select(-p_avg_) %>%
  filter(!state %in% c("AK", "HI")) ->
monthly_targets_filled


readxl::read_xlsx(eha_fn, sheet = "Operational") %>%
  select(EIA_ID = EIA_PtID, mode = Mode) %>%
  filter(!is.na(EIA_ID)) %>%
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) %>%
  filter(!duplicated(EIA_ID)) %>%
  unique() -> modes

read_csv("PNW_28_max_min_ador_parameters.csv") %>%
  left_join(modes) %>%
  group_by(mode) %>%
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  ) ->
mma_params_general

read_csv("PNW_28_max_min_ador_parameters.csv") %>%
  select(-dam) ->
mma_params_pnw

bind_rows(
  monthly_targets_filled %>%
    left_join(modes, by = "EIA_ID") %>%
    filter(!(EIA_ID %in% mma_params_pnw[["EIA_ID"]])) %>%
    left_join(mma_params_general),
  monthly_targets_filled %>%
    left_join(mma_params_pnw) %>%
    filter(EIA_ID %in% mma_params_pnw[["EIA_ID"]])
) %>%
  mutate(
    p_max = p_avg + max_param * (nameplate_MW - p_avg),
    p_min = min_param * p_avg,
    ador = ador_param * (p_max - p_min)
  ) %>%
  select(EIA_ID, plant, state, year, month, target_MWh, nameplate = nameplate_MW, p_avg, p_min, p_max, ador) ->
monthly_final

# perform basic checks

monthly_final %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  mutate(
    EIA_ID = as.integer(EIA_ID),
    year = as.integer(year)
  ) %>%
  # filter(EIA_ID == 3075) %>%
  # ggplot(aes(month, p_avg, group = year)) + geom_line() + facet_wrap(~year) +
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
    write_csv(x, paste0(output_dir, "/B1_monthly_", yr, ".csv"))
  })
