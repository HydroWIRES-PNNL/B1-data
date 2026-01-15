# 5b_B1_weekly.R
#
# Develop the B1-data weekly resolution
#
# by Cameron Bracken, PNNL 2025, cameron.bracken@pnnl.gov
#
# Original weekly disag code developed by Sean Turner
# 2024 Update - Dec 2025 - Cameron Bracken cameron.bracken@pnnl.gov
# 1.4.0 Update - Jan 9 2026, Cameron Bracken cameron.bracken@pnnl.gov

# ----------------------------------------------------------------------------
# setup ----------------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')

#%% settings
# TODO move to global config
eha_fn = "data/ORNL_EHAHydroPlant_FY2023_rev.xlsx"

output_dir = b1_dir
dir.create(output_dir, showWarnings = FALSE)


# ----------------------------------------------------------------------------
# read data ------------------------------------------------------------------
# ----------------------------------------------------------------------------
#%%
# read table of eia ids associated with huc4
# TODO find why some eia_ids have mutiple huc4
eia_and_huc4 = read_csv("data/eia_huc4.csv") |> distinct_keep(eia_id) |> rename(eia_id1 = eia_id)

# read flow data created by `2a and 2b scripts`
flow_huc4 = read_csv(huc4_flow_imputed_daily_fn) |>
  drop_na(ave_flow_cfs) |>
  distinct_keep(huc4, date)

flow_gage =
  read_csv(release_gage_flow_eia_fn) |>
  drop_na(ave_flow_cfs) |>
  distinct_keep(eia_id, date) |>
  mutate(
    year = year(date),
    month = month(date),
    day = day(date)
  ) |>
  # rename(av_flow_cfs = value) |>
  clean_names() |>
  na.omit() |>
  rename(eia_id1 = eia_id)

# PNW dam data, daily forebay, inflow, outflow, power
pnw_dam_data =
  read_csv(s("data/usace_dam_data_daily_{start_year}_{end_year}.csv")) |>
  pivot_wider(
    id_cols = c(year, month, day, dam, eia_id),
    names_from = "variable"
  ) |>
  mutate(inflow_cfs = inflow_kcfs * 1000, outflow_cfs = outflow_kcfs * 1000) |>
  rename(eia_id1 = eia_id)

# read monthly B1 data
b1_monthly = read_parquet(file.path(output_dir, 'B1_monthly.parquet'))
annual_gen = read_csv('data/annual_gen_imputed_long.csv')
b1_monthly_with_annual = b1_monthly |>
  left_join(annual_gen |> select(-datetime, -data_source), by = join_by(eia_id, year))
b1_plants = b1_monthly |> distinct(eia_id, eia_id1, eia_id2, eia_id3)
b1_annual = b1_monthly_with_annual |>
  group_by(eia_id, year) |>
  summarise(
    annual_nameplate_mw = max(nameplate_mw, na.rm = T),
    annual_p_ave = agg_na_rm_unless_all_na(p_ave, mean),
    sources = str_flatten_comma(unique(data_source)),
    annual_p_ave_i = first(annual_p_ave_i),
    n_hours_annual = sum(n_hours),
    eia_id1 = first(eia_id1),
    eia_id2 = first(eia_id2),
    eia_id3 = first(eia_id3),
  ) |>
  mutate(annual_cf = ifelse(annual_nameplate_mw == 0, 0, annual_p_ave / annual_nameplate_mw)) |>
  # drop plants with no data, if any
  # TODO look into these
  filter(!all(is.na(annual_p_ave))) |>
  mutate(plant_ave_cf = mean(annual_cf, na.rm = T))
#annual_gen |> left_join(b1_plants, by = join_by(eia_id))

# %% date sequences
# TODO move these to global or functions since they are used in multiple scripts
date_time_sequence =
  tibble(
    date_time = seq(
      as.POSIXct(s("{start_year}-01-01 00:00:00")),
      as.POSIXct(s("{end_year}-12-31 23:00:00")),
      by = "hour"
    )
  )

date_sequence_daily =
  tibble(
    date = seq(
      as.Date(s("{start_year}-01-01")),
      as.Date(s("{end_year}-12-31")),
      by = "day"
    )
  )

sequence_monthly =
  date_time_sequence |>
  mutate(
    year = year(date_time),
    month = month(date_time, label = T),
    date = date(date_time)
  ) |>
  select(year, month, date) |>
  unique()


sequence_daily =
  date_time_sequence |>
  mutate(
    year = year(date_time),
    month = month(date_time, label = T),
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
    function(x) {
      yr = x[["date_time"]][1] |> year()

      weekdef = rep(1:53, each = 7)

      x |>
        # this was the original way it was done in rectifhyd
        # define weeks based on the day of the week,
        # in which case the first week of the year would have less days
        # mutate(week_commencing = floor_date(date(date_time), "week", 7)) |>
        # mutate(week_commencing = if_else(year(week_commencing) < yr,
        #                                  ymd(paste0(yr, "-01-01")), week_commencing)) |>
        # at some point it was decide that fixed week boundaries would be preferable
        # this defines fixed week boundaries starting from the first day of the year
        # the last period will always have only 1 or 2 days (in a leap year)
        # an alternate way would be to have the week boundaries be fixed at a day of the year
        # in which case leap years would have one period with 8 days
        mutate(date = date(date_time)) |>
        select(-date_time) |>
        unique() |>
        mutate(jweek = weekdef[1:n()]) |>
        group_by(jweek) |>
        mutate(
          n_hours = 24 * n(),
          week_start = min(date),
          jday = yday(date)
        )
    }
  ) |>
  ungroup() |>
  mutate(month = month(date), day = day(date))

#%% Daily average gen proxy
ave_gen_daily =
  b1_monthly |>
  group_by(eia_id) |>
  group_split() %>%
  # .[[1]] -> plant
  map_dfr(\(plant) {
    # compute an average monthly gen profile to
    plant_non_proxy = plant |>
      filter(!str_detect(data_source, 'proxy')) #|>

    # interp to daily then agg to weekly and take the average
    sequence_weekly |>
      # interpolate monthly to daily
      mutate(p_ave_interp = with(plant_non_proxy, approx(datetime, p_ave, date))$y) |>
      # shift the julian day in leap years (cut off last day)
      # mutate(jday = ifelse(leap_year(year) & jday > 60, jday - 1, jday)) |>
      group_by(month, day) |>
      summarise(p_ave_gen_proxy = agg_na_rm_unless_all_na(p_ave_interp, mean)) |>
      mutate(
        eia_id = plant$eia_id[1],
        eia_id1 = plant$eia_id1[1],
        eia_id2 = plant$eia_id2[1],
        eia_id3 = plant$eia_id3[1]
      )
  }) |>
  # TODO interpolate instead of copy the last day
  # example interpolation
  # mutate(date = ymd(s('2000-{month}-{day}'))) |>
  # arrange(date, eia_id) |>
  # group_by(date, eia_id) |>
  # mutate(p_ave_gen_proxy = na.approx(p_ave_gen_proxy, x = date, na.rm = FALSE)) |>
  # # mutate(date = ymd(s('2000-{month}-{day}'))) |>
  # # select(-date) |>
  # ungroup()
  # fill  leap days
  mutate(p_ave_gen_proxy = ifelse(month == 2 & day == 29, NA, p_ave_gen_proxy)) |>
  fill(p_ave_gen_proxy)

# %% Determine the best disag proxy at each site
# TODO see if any combined plants dont match flow based on the first eia id

message('Determining best disag proxies.')
# quick lookup tables to see if a proxy exists or not
ave_gen_daily_lookup =
  bind_rows(
    # break out the combined plants in case we need an explicit match
    ave_gen_daily |> filter(!is.na(eia_id1)),
    ave_gen_daily |> filter(!is.na(eia_id2)) |> mutate(eia_id1 = eia_id2),
    ave_gen_daily |> filter(!is.na(eia_id3)) |> mutate(eia_id1 = eia_id3)
  ) |>
  drop_na(p_ave_gen_proxy) |>
  distinct(month, day, eia_id, eia_id1) |>
  mutate(plant_has_ave_obs_gen = TRUE)

flow_gage_lookup =
  flow_gage |>
  select(eia_id1, date) |>
  mutate(plant_has_release_flow = TRUE)

flow_huc4_lookup =
  flow_huc4 |>
  select(huc4, date) |>
  left_join(eia_and_huc4, by = join_by(huc4), relationship = 'many-to-many') |>
  # only need to know if there is at least one gage
  distinct(eia_id1, date, .keep_all = TRUE) |>
  na.omit() |>
  mutate(plant_has_huc4_flow = TRUE)

# this one is used later in the disag
flow_huc4_lookup_annual =
  flow_huc4_lookup |>
  mutate(year = year(date)) |>
  distinct(huc4, eia_id1, year)

# %% combine data sources info
flow_daily_data_sources =
  expand_grid(
    date = date_sequence_daily$date,
    eia_id1 = unique(c(b1_plants$eia_id1, b1_plants$eia_id2, b1_plants$eia_id3))
  ) |>
  left_join(flow_gage_lookup, by = join_by(date, eia_id1)) |>
  left_join(flow_huc4_lookup, by = join_by(date, eia_id1)) |>
  mutate(month = month(date), day = day(date)) |>
  left_join(ave_gen_daily_lookup, by = join_by(eia_id1, month, day)) |>
  ungroup() |>
  # fill in missing cases
  mutate_if(is.logical, coalesce, FALSE) |>
  # join in data source info
  left_join(
    flow_gage |> select(date, eia_id1, data_source_gage = data_source),
    by = join_by(date, eia_id1)
  ) |>
  left_join(
    flow_huc4 |> select(date, huc4, data_source_huc4 = data_source),
    by = join_by(date, huc4)
  ) |>
  # set flags for imputed flow data
  mutate(
    imputed_gage_flow = case_when(
      plant_has_release_flow & str_detect(data_source_gage, 'imputed') ~ TRUE,
      .default = FALSE
    ),
    imputed_huc4_flow = case_when(
      plant_has_huc4_flow & str_detect(data_source_huc4, 'imputed') ~ TRUE,
      .default = FALSE
    ),
  ) |>
  # fill in missing cases (again)
  mutate_if(is.logical, coalesce, FALSE) |>
  mutate(
    best_proxy = case_when(
      # first choice, gage at the dam (various flavors, see the release gage script for details)
      plant_has_release_flow ~ 'release_flow_proxy',
      # second choice, gage at the huc4 outlet
      plant_has_huc4_flow ~ 'huc4_flow_proxy',
      # third choice, average observed gen profile across all years
      plant_has_ave_obs_gen ~ 'ave_gen_proxy',
      .default = 'none'
    )
  )
# this is an example of how to do short circuting nested if staetments
# # cant use case_when here because it does not short circut
# # TODO set a preference list that includes imputed values
# mutate(
#   best_proxy = if_else(
#     # first choice gage flow at the dam
#     plant_has_release_flow & !imputed_gage_flow,
#     'release_flow_proxy',
#     if_else(
#       # second choice gage flow at the huc4 dam outlet
#       imputed_gage_flow & plant_has_huc4_flow,
#       'huc4_flow_proxy',
#       if_else(
#         # third choice imputed gage flow at the dam
#         !plant_has_release_flow & imputed_gage_flow & plant_has_huc4_flow,
#         'imputed_release_flow_proxy',
#         if_else(
#           # fourth choice imputed gage flow at the huc4 outlet
#           !plant_has_release_flow & imputed_huc4_flow,
#           'imputed_huc4_flow_proxy',
#           # fifth choice average gen profile
#           'ave_gen_proxy'
#         )
#       )
#     )
#   )
# )

flow_daily_data_sources |> write_csv('data/daily_disag_proxy_info.csv')

best_proxies = flow_daily_data_sources |>
  mutate(year = year(date)) |>
  group_by(eia_id, year) |>
  summarise(best_proxy = names(which.max(table(best_proxy))))

#%% disaggregate to daily and aggregate back to weekly

if (!file.exists(weekly_target_prelim_fn)) {
  # CB: Takes about 30 min on my M4 laptop
  # TODO speed this up, perhaps entirely using joins
  weekly_targets_prelim =
    start_year:end_year |>
    map_dfr(
      function(yr) {
        message(yr)

        wk_seq =
          sequence_weekly |>
          filter(year == yr) |>
          mutate(month = month(date))

        all_targets_yr_x =
          b1_annual |>
          filter(year == yr) |>
          group_by(eia_id) |>
          group_split() %>%
          # .[[1]] -> x
          map_dfr(
            \(x) {
              # browser()
              # eia_id is the character string version that might have combined eia_ids
              eia_id_ = x$eia_id |> unique()
              # eia_id1 is the numeric version that is the first combined id
              # split out so that it matches the flow datasets
              # TODO match the flow data with combined eia_ids
              eia_id1_ = x$eia_id1 |> unique()
              # eia_ids = eia_id_ |> unique() |> str_split_1('_')

              the_best_proxy = best_proxies |>
                filter(eia_id == eia_id_, year == yr) |>
                pull(best_proxy)

              # if (verbose) {
              #   message("\t", yr, ' ', eia_id_)
              # }
              if (length(the_best_proxy) == 0) {
                browser()
              }
              # message('The best proxy is: ', the_best_proxy)

              # get proxies using the best one previously selected
              # the proxies are daily and then are aggregated to weekly
              proxy_for_disag =
                if (the_best_proxy == 'huc4_flow_proxy') {
                  if (verbose) {
                    message(yellow('\tUsing', the_best_proxy, 'for', eia_id_, 'in', yr))
                  }
                  huc4_ = flow_huc4_lookup_annual |>
                    filter(eia_id1 == eia_id1_, year == yr) |>
                    pull(huc4) |>
                    unique()
                  flow_huc4 |>
                    filter(huc4 == huc4_) |>
                    filter(year == yr)
                } else if (the_best_proxy == 'release_flow_proxy') {
                  if (verbose) {
                    message(green('\tUsing', the_best_proxy, 'for', eia_id_, 'in', yr))
                  }
                  flow_gage |>
                    filter(eia_id1 == eia_id1_, year == yr) |>
                    # append the flow category to the existing data source
                    # for more granular info
                    mutate(data_source = s('{data_source}_{flow_cat}'))
                } else if (the_best_proxy == 'ave_gen_proxy') {
                  if (verbose) {
                    message(red('\tUsing', the_best_proxy, 'for', eia_id_, 'in', yr))
                  }
                  ave_gen_daily |>
                    filter(eia_id1 == eia_id1_) |>
                    rename(ave_flow_cfs = p_ave_gen_proxy) |>
                    mutate(
                      year = yr,
                      data_source = 'average_observed_gen'
                    )
                } else if (the_best_proxy == 'none') {
                  # the eia ID does not have a huc associated or if the flow
                  # data is incomplete then just split evenly across each week
                  if (verbose) {
                    message(red('\tNo proxy available, assigning flat to ', eia_id_))
                  }
                  wk_seq |>
                    select(year, month, day) |>
                    mutate(ave_flow_cfs = 1, data_source = NA_character_)
                } else {
                  stop('This error should not occur, time to take a pizza break.')
                }

              # disag from annual to daily
              daily_disag =
                proxy_for_disag |>
                arrange(year, month, day) |>
                mutate(
                  p_disag_daily = ave_flow_cfs / sum(ave_flow_cfs),
                  # get the annual average gen value, it may have been imputed,
                  # but the annual gen data is mostly complete.
                  # convert to total energy
                  annual_target_mwh = with(x, annual_p_ave * n_hours_annual)
                ) |>
                mutate(
                  p_disag_daily = if_else(
                    is.nan(p_disag_daily),
                    1 / n(),
                    p_disag_daily
                  ),
                  daily_target_mwh = annual_target_mwh * p_disag_daily
                ) |>
                select(year, month, day, p_disag_daily, daily_target_mwh) |>
                ungroup()

              weekly_targets_plant_year =
                daily_disag |>
                left_join(wk_seq, by = join_by(year, month, day)) |>
                group_by(jweek, week_start) |>
                summarise(
                  target_mwh = sum(daily_target_mwh),
                  n_hours = 24 * n()
                ) |>
                mutate(
                  year = yr,
                  eia_id = eia_id_,
                  # TODO split nameplate by weeks when it changes part way through the year
                  nameplate_mw = x$annual_nameplate_mw[1],
                  p_ave = target_mwh / n_hours,
                  disag_proxy = the_best_proxy,
                  disag_data_source = proxy_for_disag$data_source[1]
                )

              return(weekly_targets_plant_year)
            },
            .progress = ifelse(verbose, F, T)
          )

        return(all_targets_yr_x)
      },
      .progress = ifelse(verbose, F, T)
    )
  weekly_targets_prelim |> write_csv(weekly_target_prelim_fn)
  message('Wrote preliminary weekly target file: ', weekly_target_prelim_fn)
} else {
  message('Reading cached preliminary weekly target file: ', weekly_target_prelim_fn)
  weekly_targets_prelim = read_csv(weekly_target_prelim_fn)
}

# %%
# need to scale the proxy-based gen when it exceeds the nameplate cap
# using this approach from rectifhyd
#   PNNL_MW = fraction * netgen_annual / n_hours,
#   PNNL_MWh = PNNL_MW / max(PNNL_MW) * nameplate_MW * n_hours,
#   PNNL_MW = PNNL_MWh / n_hours
message('Scaling weekly disag based on nameplate values.')
weekly_targets =
  weekly_targets_prelim
# |>
#   group_by(eia_id, year) |>
#   group_split() |>
#   map_dfr(
#     \(plant_year) {
#       #
#       p_max = max(plant_year$p_ave)
#       buffer = 1.25

#       if (is.na(p_max) || p_max == 0 || all(p_max < plant_year$nameplate_mw * buffer)) {
#         return(plant_year)
#       }
#       plant_year |>
#         mutate(
#           target_mwh = p_ave / p_max * nameplate_mw * n_hours,
#           p_ave = target_mwh / n_hours
#         )
#     },
#     .progress = T
#   )

# ----------------------------------------------------------------------------
# finalize -------------------------------------------------------------------
# ----------------------------------------------------------------------------
#%% compute p_min, p_max, and ador from the generic parameters

# eia_id in the b1_monthly data contains merged ids, and so is character type
# eia_id1, eia_id2, and eia_id3, contain the split out columns, but when
# merging in data only eia_id1 is needed because the other ids refer to the same plant
eha =
  read_xlsx('data/ORNL_EHAHydroPlant_PublicFY2024.xlsx', 'Operational') %>%
  set_names(tolower(names(.))) |>
  clean_names(parsing_option = 3) |>
  # mutate(eia_id = as.character(eia_ptid)) |>
  select(eia_id1 = eia_ptid, eha_ptid, dam_own, mode, huc) |>
  mutate(eia_id = as.character(eia_id1))

modes = eha |>
  drop_na(eia_id) |>
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) |>
  distinct(eia_id, .keep_all = T) |>
  distinct(eia_id, eia_id1, mode)

b1_params_weekly =
  read_csv('data/b1_params_pnw_federal_dams_max_min_ador_weekly.csv') |>
  rename(eia_id1 = eia_id) |>
  mutate(eia_id = as.character(eia_id1))

b1_params_general =
  b1_params_weekly |>
  left_join(modes, by = join_by(eia_id1)) |>
  group_by(mode) |>
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  )

# params for specefic dams where data is available
b1_params_pnw = b1_params_weekly |>
  select(-c(dam, eia_id)) |>
  rename_with(
    function(name) {
      paste0(name, '_pnw')
    },
    ends_with("_param")
  )


b1_metadata =
  if (output_format == 'csv') {
    read_csv(s('{output_dir}/B1_metadata.csv')) |>
      select(-c(nameplate_mw))
    # write_csv(., fn_path, na = "")
  } else if (output_format == 'parquet') {
    read_parquet(s('{output_dir}/B1_metadata.parquet')) |>
      select(-c(nameplate_mw))
  }

b1_weekly =
  weekly_targets |>
  mutate(month = month(week_start)) |>
  left_join(b1_metadata, by = join_by(eia_id, year, month)) |>
  # fill in metadata first by year, then by plant this assumes
  # the metadata columns are constant between known changes
  # TODO check if this causes any issues
  group_by(eia_id, year) |>
  fill(-c(target_mwh, p_ave), .direction = 'downup') |>
  group_by(eia_id) |>
  fill(-c(target_mwh, p_ave), .direction = 'downup') |>
  left_join(b1_params_general, by = join_by(mode)) |>
  left_join(b1_params_pnw, by = join_by(eia_id1)) |>
  # select the dam specefic parameters where available
  mutate(
    max_param = ifelse(is.na(max_param_pnw), max_param, max_param_pnw),
    min_param = ifelse(is.na(min_param_pnw), min_param, min_param_pnw),
    ador_param = ifelse(is.na(ador_param_pnw), ador_param, ador_param_pnw),
  ) |>
  select(-ends_with('_pnw')) |>
  # filter(eia_id == 3075) |>
  mutate(target_mwh = if_else(target_mwh < 0, 0, target_mwh)) |>
  mutate(p_ave = target_mwh / n_hours) |>
  mutate(p_ave = if_else(p_ave > nameplate_mw, nameplate_mw, p_ave)) |>
  mutate(
    p_max = p_ave + max_param * (nameplate_mw - p_ave),
    p_min = min_param * p_ave,
    ador = ador_param * (p_max - p_min)
  )


# data,
# group,
# y_var = 'net_gen_mw',
# y_lab = 'Net Hydro Gen [aMW]',
# y_step_var = 'nameplate_mw',
# plot_y_step = TRUE,
# fn = 'compare_gen.pdf',
# data_source_col = 'data_source'
# %%
if (create_figures) {
  b1_weekly %>%
    mutate(datetime = week_start) |>
    # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
    multipage_pdf_timeseries_by_group_with_source(
      'eia_id',
      'p_ave',
      fn = 'figures/complete_gen_weekly_final.pdf'
    ) |>
    # ggplot warns about dropping NA values, ignore
    suppressWarnings()
}

#%% basic checks, more in the validation script
b1_weekly |> filter(p_min < 0)
b1_weekly |> filter(p_min > p_max)
b1_weekly |> filter(p_min > (p_max - ador))
b1_weekly |> filter(p_min > p_ave)
b1_weekly |> filter(p_max < p_ave - .001)
b1_weekly |> filter(p_max < p_min)
b1_weekly |> filter(ador < 0)
b1_weekly |> filter(ador < 0)

# TODO investigate these
b1_weekly |> filter(p_ave > nameplate_mw * (1.25 + 0.01))

#%% write data

b1_weekly |>
  select(-c(datetime, max_param, min_param, ador_param)) |>
  select(
    week_start,
    jweek,
    year,
    month,
    n_hours,
    target_mwh,
    p_ave,
    nameplate_mw,
    everything()
  ) |>
  write_output("B1_weekly", output_dir, output_format)
