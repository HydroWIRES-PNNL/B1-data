# 4a-B1-monthly.R
#
# Create the HydroWIRES B1 monthly dataset
#
# Monthly hydropower gen data is
#
# 2022 Original version by Sean Turner, PNNL
# 2024 Update - Nov 3 2025 - Cameron Bracken cameron.bracken@pnnl.gov
#
# Approach:
# 1. For the monthly version, take rectifhyd and add max, min, ador using average of PNW parameters
# 2. For the weekly version, disaggregate using USGS flows, use same max, min, ador parameters.

# %%
library(tidyverse)
library(conflicted)
library(arrow)

conflicts_prefer(dplyr::filter)
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

source('utilities.R')

# %%
start_year = 1980
end_year = 2024
hydro_gen_data_fn = "data/hydro_gen_monthly_eia_pudl_rfp.csv"
rectifhyd_fn = 'data/RectifHyd_v1.4.csv'

eha_fn = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"
output_prefix = "B1_data"
version = "1.4.0"


# %%
output_dir = paste0(output_prefix, "_", version)
dir.create(output_dir, showWarnings = FALSE)

# %%
eha =
  readxl::read_xlsx(eha_fn, sheet = "Operational") |>
  janitor::clean_names(parsing_option = 3) |>
  select(
    eha_ptid = eha_pt_id,
    # plant = pt_name,
    eia_id = eia_pt_id
    # state = state
    # nameplate_mw = CH_MW,
    # ba = baCode
  )

# read table of EIA ids associated with HUC4
eia_and_huc4 = read_csv("data/eia_huc4.csv", show = F, progress = F)

# %%
eia_plants_pudl = read_parquet('data/out_eia__yearly_plants.parquet') |>
  select(
    eia_id = plant_id_eia,
    datetime = report_date,
    plant_name_eia,
    lat = latitude,
    lon = longitude,
    state,
    county,
    timezone,
    ba = balancing_authority_code_eia,
    plant_id_pudl,
    utility_id_eia,
    utility_name_eia
  )

# TODO find when plants started and stopped service
generators = read_parquet('data/out_eia__yearly_generators.parquet')

hydro_plants_eia_pudl = generators |>
  filter(fuel_type_code_pudl == 'hydro', prime_mover_code == 'HY') |>
  group_by(plant_id_pudl, eia_id = plant_id_eia, report_date) |>
  # first sum all the generators that have a unique pudl and eia id
  summarise(
    n_generators_combined = n(),
    # for checking, will read the full data in later
    net_generatio_mwh = sum(net_generation_mwh, na.rm = T),
    capacity_mw = sum(capacity_mw, na.rm = T),
    ba = first(balancing_authority_code_eia, na_rm = T)
  ) |>
  # now sum all the eia_ids that correspond to the same plant
  # some EIA ids correspond to the same plant but are split for reporting
  # purposes, eg. hoover which is in two states
  summarise(
    eia_id = str_flatten(eia_id, '_'),
    # I've never seen more than 3 ids combined but add a 4th to be safe
    eia_id1 = unique(eia_id)[1],
    eia_id2 = unique(eia_id)[2],
    eia_id3 = unique(eia_id)[3],
    eia_id4 = unique(eia_id)[4],
    generator_id = str_flatten_comma(generator_id),
    n_generators_combined = n(),
    n_eia_combined = summarise(n = length(unique(eia_id))),
    # for checking, will read the full data in later
    net_generatio_mwh = sum(net_generation_mwh, na.rm = T),
    capacity_mw = sum(capacity_mw, na.rm = T),
    ba = first(balancing_authority_code_eia, na_rm = T)
  ) |>
  left_join()

# %%
# flows
huc4_flows = read_csv("data/huc4_flow_usgs_long_imputed.csv")
huc4_flows_monthly = huc4_flows |>
  group_by(year, month, huc4, usgs_id) |>
  summarise(av_flow_cfs = mean(av_flow_cfs), .groups = "drop") |>
  mutate(month = month.abb[month])

gage_flows =
  read_csv(
    'data/flow/proc/flow_all_eia_hydro_long_1980-01-01_to_2024-12-31.csv'
  ) |>
  janitor::clean_names(parsing_option = 3) |>
  mutate(year = year(date), month = month(date), day = day(date)) |>
  group_by(eia_id, year, month) |>
  summarise(av_flow_cfs = mean(value))


# %%
# get hours per month for converting MWh to MW average
hours_per_month =
  tibble(
    datetime = seq.Date(
      sprintf('%s-01-01', start_year) |> as.Date(),
      sprintf('%s-01-01', end_year) |> as.Date(),
      by = "month"
    )
  ) |>
  mutate(n_days = days_in_month(datetime), n_hours = n_days * 24)

# %%
# rectifhyd data
rectifhyd = read_csv(rectifhyd_fn) |>
  janitor::clean_names(parsing_option = 3) |>
  mutate(
    datetime = sprintf('%s-%s', year, month) |> ym(),
    net_gen_mwh = ifelse(
      recommended_data == 'RectifHyd',
      rectif_hyd_mwh,
      eia_mwh
    ),
    data_source = 'rf',
    nameplate_mw = 0,
    net_gen_mw = 0
  ) |>
  select(eia_id, datetime, net_gen_mwh, data_source, nameplate_mw, net_gen_mw)

# pre-formatted gen data from eia, pudl and rectifhydplus
hydro_gen_raw = read_csv(hydro_gen_data_fn) |>
  bind_rows(rectifhyd)

# pre-formatted gen data from eia, pudl and rectifhydplus
hydro_gen =
  hydro_gen_raw |>
  # TODO check why pudl plant id is missing in so many cases
  select(
    -c(
      rfp_eia_id_was_split,
      plant_id_pudl,
      plant_name_eia,
      source_freq,
      data_frequency,
      net_gen_mw
    )
  ) |>
  pivot_wider(
    id_cols = everything(),
    names_from = data_source,
    values_from = c(net_gen_mwh, nameplate_mw)
  ) |>
  # drop_na(net_gen_mwh_eia) |>
  # create a single historical record, using a hierarchy of sources
  # the three sources are eia, eia_pudl, and rfp
  mutate(
    # for nameplate prefer pudl, then rfp, then eia
    nameplate_mw = case_when(
      !is.na(nameplate_mw_eia_pudl) ~ nameplate_mw_eia_pudl,
      .default = NA
    ),
    nameplate_mw = case_when(
      is.na(nameplate_mw) & !is.na(nameplate_mw_rfp) ~ nameplate_mw_rfp,
      .default = nameplate_mw
    ),
    nameplate_mw = case_when(
      is.na(nameplate_mw) & !is.na(nameplate_mw_eia) ~ nameplate_mw_eia,
      .default = nameplate_mw
    ),
    # for gen prefer rfp, rf, pudl, then eia
    # TODO handle split rfp cases
    # TODO add data source tracking
    net_gen_mwh = case_when(
      !is.na(net_gen_mwh_rfp) ~ net_gen_mwh_rfp,
      .default = NA
    ),
    gen_source = ifelse(!is.na(net_gen_mwh_rfp), 'rfp', NA),
    net_gen_mwh = case_when(
      is.na(net_gen_mwh) & !is.na(net_gen_mwh_rf) ~ net_gen_mwh_rf,
      .default = net_gen_mwh
    ),
    gen_source = ifelse(
      is.na(net_gen_mwh) & !is.na(net_gen_mwh_rf),
      'rf',
      gen_source
    ),
    net_gen_mwh = case_when(
      is.na(net_gen_mwh) & !is.na(net_gen_mwh_eia_pudl) ~ net_gen_mwh_eia_pudl,
      .default = net_gen_mwh
    ),
    gen_source = ifelse(
      is.na(net_gen_mwh) & !is.na(net_gen_mwh_eia_pudl),
      'eia_pudl',
      gen_source
    ),
    net_gen_mwh = case_when(
      is.na(net_gen_mwh) & !is.na(net_gen_mwh_eia) ~ net_gen_mwh_eia,
      .default = net_gen_mwh
    ),
    gen_source = ifelse(
      is.na(net_gen_mwh) & !is.na(net_gen_mwh_eia),
      'eia',
      gen_source
    )
  ) |>
  select(eia_id, datetime, nameplate_mw, net_gen_mwh, gen_source) |>
  left_join(hours_per_month, by = join_by(datetime)) |>
  mutate(target_mwh = if_else(net_gen_mwh < 0, 0, net_gen_mwh)) |>
  mutate(p_avg = target_mwh / n_hours) |>
  mutate(p_avg = if_else(p_avg > nameplate_mw, nameplate_mw, p_avg))

# %%
eia_ids = hydro_gen |>
  pull(eia_id) |>
  unique()

full_frame =
  expand.grid(
    datetime = hours_per_month$datetime,
    eia_id = eia_ids
  ) |>
  as_tibble()

# %%
# TODO bring in more plant metdata
hydro_gen_with_metadata = hydro_gen |>
  left_join(
    eha |> filter(!is.na(eia_id)) |> distinct(eia_id, .keep_all = TRUE),
    by = join_by(eia_id)
  ) |>
  left_join(eia_plants_pudl, join_by(eia_id, datetime)) |>
  group_by(eia_id) |>
  # fill incomplete plant metadata
  fill(
    -c(nameplate_mw, net_gen_mwh, gen_source, n_days, n_hours),
    .direction = 'downup'
  ) |>
  mutate(net_gen_mw = net_gen_mwh / n_hours, .after = net_gen_mwh)


#%%
rectifhyd_monthly_targets_filled =
  full_frame |>
  left_join(
    hydro_gen_with_metadata |> select(-c(n_hours, n_days)),
    by = join_by(datetime, eia_id)
  ) |>
  # select(eia_id, eha_ptid, state, nameplate_mw, ba) |>
  # distinct() |>
  left_join(hours_per_month) |>
  left_join(
    rectifhyd_monthly_targets,
    by = join_by(
      eia_id,
      eha_ptid,
      plant,
      state,
      nameplate_mw,
      month,
      year,
      n_hours,
      ba
    )
  ) |>
  left_join(replacement_data, by = c("eia_id", "month")) |>
  mutate(p_avg = if_else(is.na(p_avg), p_avg_, p_avg)) |>
  mutate(
    target_mwh = if_else(is.na(target_mwh), p_avg * n_hours, target_mwh)
  ) |>
  select(-p_avg_) |>
  filter(!state %in% c("AK", "HI"))


readxl::read_xlsx(eha_fn, sheet = "Operational") |>
  select(eia_id = EIA_PtID, mode = Mode) |>
  filter(!is.na(eia_id)) |>
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) |>
  filter(!duplicated(eia_id)) |>
  unique() -> modes

read_csv("PNW_28_max_min_ador_parameters.csv", show = F, progress = F) |>
  left_join(modes, by = join_by(eia_id)) |>
  group_by(mode) |>
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  ) -> mma_params_general

read_csv("PNW_28_max_min_ador_parameters.csv", show = F, progress = F) |>
  select(-dam) -> mma_params_pnw

bind_rows(
  rectifhyd_monthly_targets_filled |>
    left_join(modes, by = "eia_id") |>
    filter(!(eia_id %in% mma_params_pnw[["eia_id"]])) |>
    left_join(mma_params_general, by = join_by(mode)),
  rectifhyd_monthly_targets_filled |>
    left_join(mma_params_pnw, by = join_by(eia_id)) |>
    filter(eia_id %in% mma_params_pnw[["eia_id"]])
) |>
  mutate(
    p_max = p_avg + max_param * (nameplate_mw - p_avg),
    p_min = min_param * p_avg,
    ador = ador_param * (p_max - p_min)
  ) |>
  select(
    eia_id,
    plant,
    state,
    ba,
    year,
    month,
    target_mwh,
    nameplate = nameplate_mw,
    p_avg,
    p_min,
    p_max,
    ador
  ) |>
  left_join(eia_and_huc4, by = join_by(eia_id)) |>
  left_join(huc4_flows_monthly, by = join_by(year, month, HUC4)) |>
  rename(HUC4_flow_cfs = av_flow_cfs) -> monthly_final

# perform basic checks

monthly_final |>
  left_join(pnw_dam_data, by = join_by(year, month, eia_id)) |>
  mutate(month = factor(month, levels = month.abb)) |>
  mutate(
    eia_id = as.integer(eia_id),
    year = as.integer(year),
    datetime = sprintf('%s-%02d-01', year, `names=`(1:12, month.abb)[month])
  ) |>
  rename(eia_id = eia_id) |>
  # filter(eia_id == 3075) |>
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
  mutate_if(is.double, function(x) round(x, 4)) |>
  mutate(
    Western = if_else(
      state %in%
        c(
          "WA",
          "ID",
          "CO",
          "UT",
          "NM",
          "WY",
          "MT",
          "CA",
          "OR",
          "NV",
          "AZ"
        ),
      TRUE,
      FALSE
    )
  ) |>
  arrange(-Western) |>
  janitor::clean_names(parsing_option = 3) |>
  write_csv(paste0(output_dir, "/B1_monthly.csv"), na = "") -> shh


monthly = list.files(output_dir, '*monthly*', full.names = T) |>
  map(function(x) read_csv(x, progress = F, show = F)) |>
  bind_rows()

monthly |>
  filter(western == TRUE) |>
  group_by(year, month) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  ggplot(aes(month, energy_mwh / 1000, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("orange", "cornflowerblue")) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Energy [GWh]")

monthly |>
  filter(western == TRUE) |>
  group_by(year, month) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  pivot_wider(id_cols = month, names_from = year, values_from = energy_mwh) |>
  mutate(pct_diff = (`2001` - `2009`) / `2009` * 100)
