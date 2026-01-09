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

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')

# %%
hydro_gen_data_fn = s(
  '{data_dir}/hydro_gen_multisource_monthly_{start_year}_{end_year}_long.csv'
)
# rectifhyd_fn = 'data/RectifHyd_v1.4.csv'

# eha_fn = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"
output_prefix = "B1_data"
version = "1.4.0"


# %%
output_dir = paste0(output_prefix, "_", version)
dir.create(output_dir, showWarnings = FALSE)


# ----------------------------------------------------------------------------
# input data -----------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% Read hilarri and pudl data

# Read HILLARI v3 data which has been pre-filtered for just CONUS hydro plants
# see downloading script
hilarri_hydro_plants =
  file.path(data_dir, config::get('hilarri_csv')) |>
  read_csv() #col_types = list(eia_ptid = col_character()))

eha = read_xlsx('data/ORNL_EHAHydroPlant_PublicFY2024.xlsx', 'Operational') %>%
  set_names(tolower(names(.))) |>
  clean_names(parsing_option = 3) |>
  # mutate(eia_id = as.character(eia_ptid)) |>
  select(eia_id = eia_ptid, eha_ptid, dam_own, mode, huc)

eia_eha =
  bind_rows(
    hilarri_hydro_plants |>
      select(eia_id1 = eia_ptid, eha_ptid),
    eha |>
      select(eia_id1 = eia_id, eha_ptid)
  ) |>
  distinct(eia_id1, .keep_all = TRUE) |>
  na.omit()

# read table of EIA ids associated with HUC4
eia_and_huc4 = read_csv("data/eia_huc4.csv", show = F, progress = F)

# TODO find when plants started and stopped service
eia_hydro_plants_pudl = read_parquet('data/out_eia__yearly_plants.parquet') |>
  mutate(eia_id = as.character(plant_id_eia))

# %%
# flow based disag proportions
if (!file.exists(flow_based_disag_fn) & !cache) {
  message('Computing flow based disaggregation proportions.')
  flow_disag = read_csv(release_fractions_fn) |>
    full_join(
      read_csv(huc4_fractions_fn) |> rename(fraction_huc4 = fraction),
      by = join_by(eha_ptid, year, month)
    ) |>
    drop_na(eha_ptid) |>
    left_join(eia_eha |> distinct(eha_ptid, .keep_all = T), by = join_by(eha_ptid)) |>
    # pick the best proxy (release preferred)
    mutate(
      flow_proxy = ifelse(is.na(fraction_release), 'huc4_flow_proxy', 'release_flow_proxy'),
      fraction = ifelse(is.na(fraction_release), fraction_huc4, fraction_release)
    ) |>
    drop_na(eia_id1) |>
    mutate(datetime = ym(s('{year}-{month}'))) |>
    distinct(eia_id1, datetime, .keep_all = TRUE) |>
    group_by(eia_id1, year) |>
    mutate(
      n_hours = days_in_month(datetime) * 24 |> as.numeric(),
      n_hour_annual = sum(n_hours),
      p_disag_max = n_hours / max(c(n_hour_annual, 8760))
    ) |>
    group_split() |>
    map_dfr(
      function(x_yr) {
        # browser()
        #
        proxy_smoothed = FALSE
        proxy_scaled = FALSE
        proxy_failed = FALSE
        counter = 0
        np_buffer = 1.25

        # message(unique(x_yr$eia_id1), " ", unique(x_yr$year))
        # we apply a loess smoothing spline to the provisional allocation factors
        # (low degree of smoothing; span = 0.2), repeating until generation is more
        # reasonable with nameplate capacity not exceeded and no monthly factor
        # greater than 0.25, which would imply a quarter of annual generation
        # occurring in a single month. This threshold was selected based on an
        # analysis of existing monthly generating data across observed plants
        # (0.25 is very rarely exceeded). Final adjusted factors are then multiplied
        # by observed annual generation to create a rectified set of monthly generation
        # estimates that sum to observed annual generation at each plant.
        repeat {
          max_fraction = max(x_yr[["fraction"]])
          any_frac_over_np_limit = any(x_yr[["fraction"]] > (x_yr[["p_disag_max"]] * np_buffer))

          counter = counter + 1

          if (max_fraction > 0.25 | any_frac_over_np_limit) {
            #| np_test == TRUE) {
            # browser()
            proxy_smoothed = TRUE

            # if (counter == 1) message("Smoothing fractions")

            # message(counter)
            if (nrow(x_yr) < 12) {
              # if less than 12 months, split evenly
              return(
                x_yr |>
                  mutate(
                    fraction = n_hours / 8760,
                    proxy_scaled = FALSE,
                    proxy_smoothed = FALSE,
                    proxy_failed = TRUE
                  )
              )
            }

            # stick together the same year on the back and the front for smoothing
            x_yr =
              x_yr |>
              mutate(
                fraction_smoothed = zoo::rollmean(c(rep(0, 6), fraction, rep(0, 6)), 13)
              ) |>
              mutate(fraction = fraction_smoothed / sum(fraction_smoothed)) |>
              select(-fraction_smoothed)
            # frac = x_yr$fraction
            # print(frac)
          }

          if (counter >= 50) {
            browser()
            # bail out if the smoothing is taking too long
            # message("Using flat monthly flow proxy distribution")
            x_yr = x_yr %>%
              mutate(fraction = p_disag_max / sum(p_disag_max))
            # TODO replace these with average proxies later
            proxy_scaled = TRUE
            proxy_failed = TRUE
            break
          } else {
            fraction_max_lt_limit = max(x_yr[["fraction"]]) < 0.25
            all_fractions_lt_npmax = all(x_yr[["fraction"]] < (x_yr[["p_disag_max"]] * np_buffer))

            if (fraction_max_lt_limit & all_fractions_lt_npmax) {
              # herustics look good
              # usually only takes 3 or less smoothings iterations
              # if (counter > 3) {
              #   message(unique(x_yr$eia_id1), " ", unique(x_yr$year), ' ', counter)
              # }
              break
            } else {
              # try again
              cycle
            }
            #else {
            #  message(unique(x_yr$eia_id1), " ", unique(x_yr$year))
            #  browser()
            #  stop("Flow based proxy scaling failed: ", counter)
            #}
          }
        }

        return(
          x_yr %>%
            mutate(
              proxy_smoothed = !!proxy_smoothed,
              proxy_scaled = !!proxy_scaled,
              proxy_failed = !!proxy_failed
            )
        )
      },
      .progress = TRUE
    ) |>
    rename(p_disag1 = fraction) |>
    select(-starts_with('fraction'))

  flow_disag |> write_csv(flow_based_disag_fn)
  message('Wrote flow based disag proportions: ', flow_based_disag_fn)
} else {
  message('Reading cached flow based disag proportions: ', flow_based_disag_fn)
  flow_disag = read_csv(flow_based_disag_fn)
}

# check
flow_disag |>
  group_by(year, eia_id1) |>
  mutate(sump = sum(p_disag1)) |>
  filter(abs(sump - 1) > 0.00001)


# ----------------------------------------------------------------------------
# data source selection ------------------------------------------------------
# ----------------------------------------------------------------------------

# %%
# gen data
# pre-formatted gen data from eia, pudl,rectifhyd, and rectifhydplus
hydro_gen_multisource_raw = read_csv(hydro_gen_data_long_fn)

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
# select the source of gen data
# these are the names of the data sources in order of preference
data_source_names = c(rfp_version_tag, 'eia_pudl', 'eia_raw', rf_version_tag)
n_sources = length(data_source_names)

message(
  'Selecting the source of gen data based on this preference order: ',
  str_flatten_comma(data_source_names)
)


hydro_gen_source_selected =
  hydro_gen_multisource_raw |>
  # filter(eia_id == '100') |>
  # filter(eia_id == '6163') |>
  # filter(eia_id == '10687_2546') |>
  mutate(
    data_source_f = factor(data_source, levels = data_source_names, ordered = T)
  ) |>
  group_by(datetime, eia_id) |>
  # very important to sort the data by source in the order of preference
  arrange(datetime, eia_id, data_source_f) |>
  mutate(
    best_gen_ind = which(!is.na(net_gen_mwh))[1],
    best_np_ind = which(!is.na(nameplate_mw))[1]
  ) |>
  mutate(
    best_gen_ind = ifelse(!is.na(best_gen_ind), best_gen_ind, n_sources + 1),
    best_np_ind = ifelse(!is.na(best_np_ind), best_np_ind, n_sources + 1)
  ) |>
  summarise(
    # best_gen_ind = which(!is.na(net_gen_mwh))[1],
    data_source_selected = data_source_names[best_gen_ind[1]],
    net_gen_mwh = net_gen_mwh[best_gen_ind[1]],
    net_gen_mw = net_gen_mw[best_gen_ind[1]],
    # nameplate data might exist when the gen doesnt, so select a source independently
    # best_np_ind = which(!is.na(nameplate_mw))[1],
    data_source_nameplate = data_source_names[best_np_ind[1]],
    nameplate_mw = nameplate_mw[best_np_ind[1]],
    nameplate_mwh = nameplate_mwh[best_np_ind[1]],
    eia_id1 = eia_id1[1],
    eia_id2 = eia_id2[1],
    eia_id3 = eia_id3[1],
    plant_id_pudl1 = plant_id_pudl1[1],
    plant_id_pudl2 = plant_id_pudl2[1],
    n_eia_ids_combined = n_eia_ids_combined[1],
    eia_id_was_combined = eia_id_was_combined[1],
    n_eia_ids_combined2 = n_eia_ids_combined2[1],
    n_hours = n_hours[1]
  ) |>
  rename(data_source = data_source_selected) |>
  mutate(target_mwh = if_else(net_gen_mwh < 0, 0, net_gen_mwh)) |>
  mutate(p_ave = target_mwh / n_hours) |>
  mutate(p_ave = if_else(p_ave > nameplate_mw, nameplate_mw, p_ave)) |>
  group_by(eia_id) |>
  # drop plants with all NA values or less than 1 year of data
  filter(!all(is.na(p_ave))) |>
  # or less than 1 year of data
  filter(length(which(!is.na(p_ave))) > 12) |>
  # or with all zeros
  filter(!all(na.omit(p_ave) == 0))


# ----------------------------------------------------------------------------
# missing data imputation ----------------------------------------------------
# ----------------------------------------------------------------------------
# %%
annual =
  hydro_gen_source_selected |>
  mutate(year = year(datetime)) |>
  group_by(eia_id, year) |>
  summarise(
    annual_nameplate_mw = max(nameplate_mw, na.rm = T),
    annual_p_ave = agg_na_rm_unless_all_na(p_ave, mean),
    sources = str_flatten_comma(unique(data_source)),
    n_hours_annual = sum(n_hours)
  ) |>
  mutate(annual_cf = annual_p_ave / annual_nameplate_mw) |>
  # drop plants with no data, if any
  # TODO look into these
  filter(!all(is.na(annual_p_ave))) |>
  mutate(plant_ave_cf = mean(annual_cf, na.rm = T))

# average capacity factor for
conus_ave_cf =
  annual |>
  group_by(eia_id) |>
  summarise(ave_cf = mean(annual_cf, na.rm = T)) |>
  pull(ave_cf) |>
  # hist()
  mean()

# fill in average plant capacities if no plant specefic values are available
annual = annual |>
  mutate(annual_ave_cf = ifelse(is.na(plant_ave_cf), conus_ave_cf, plant_ave_cf))


# impute the missing annual average gen
if (!file.exists(annual_p_ave_imputed_fn) | !cache) {
  annual_p_ave_imputed_wide =
    annual |>
    pivot_wider(id_cols = year, names_from = eia_id, values_from = annual_p_ave) |>
    select(-year) |>
    missRanger()
  annual_p_ave_imputed_wide |>
    write_csv(annual_p_ave_imputed_fn)
} else {
  annual_p_ave_imputed_wide = read_csv(annual_p_ave_imputed_fn)
}

annual_p_ave_imputed =
  annual |>
  left_join(
    annual_p_ave_imputed_wide |>
      mutate(year = start_year:end_year) |>
      # mutate(datetime = datetime_sequence_a) |>
      pivot_longer(-year, names_to = 'eia_id', values_to = 'annual_p_ave_i'),
    by = join_by(eia_id, year)
  ) |>
  mutate(datetime = ym(s('{year}-01'))) |>
  mutate(data_source = ifelse(is.na(annual_cf), 'imputed_annual', sources)) |>
  mutate(annual_p_ave = ifelse(is.na(annual_p_ave), annual_p_ave_i, annual_p_ave)) |>
  mutate(
    annual_p_ave = ifelse(
      annual_p_ave > annual_nameplate_mw,
      annual_ave_cf * annual_nameplate_mw,
      annual_p_ave
    )
  ) |>
  # TODO fix these unknown cases
  mutate(data_source = ifelse(is.na(data_source), 'unknown', data_source))

# TODO check cases where plant is at capacity the entire year, seems unlikely
annual_p_ave_imputed |>
  filter(annual_p_ave >= annual_nameplate_mw)

# %%
annual_p_ave_imputed %>%
  # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
  rename(nameplate_mw = annual_nameplate_mw) |>
  # drop_na(p_ave) |>
  arrange(eia_id, year) |>
  # reduce the number of data source labels to simplify the plotting
  mutate(data_source = ifelse(str_detect(data_source, ','), 'mixed', data_source)) |>
  multipage_pdf_by_group('eia_id', 'annual_p_ave', 'figures/complete_gen_annual_imputed.pdf') |>
  # ggplot warns about dropping NA values, ignore
  suppressWarnings()


# ----------------------------------------------------------------------------
# disaggregation -------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% Disag
flow_disag_join = flow_disag |>
  select(eia_id1 = eia_id, datetime, flow_proxy, p_disag1) |>
  na.omit() |>
  ungroup()

# add in flow proxies where available
hydro_gen_with_flow_proxy =
  hydro_gen_source_selected |>
  ungroup() |>
  mutate(year = year(datetime), month = month(datetime)) |>
  left_join(
    annual_p_ave_imputed |> select(-datetime) |> rename(data_source_annual = data_source),
    by = join_by(eia_id, year)
  ) |>
  left_join(flow_disag_join, by = join_by(eia_id1, datetime)) |>
  left_join(
    flow_disag_join |> rename(p_disag2 = p_disag1, eia_id2 = eia_id1, flow_proxy2 = flow_proxy),
    by = join_by(eia_id2, datetime)
  ) |>
  left_join(
    flow_disag_join |> rename(p_disag3 = p_disag1, eia_id3 = eia_id1, flow_proxy3 = flow_proxy),
    by = join_by(eia_id3, datetime)
  )

# any plants withut a proxy, use the average gen from other years to make a proxy
ave_gen_proxy =
  hydro_gen_with_flow_proxy |>
  filter(is.na(flow_proxy)) |>
  group_by(eia_id, month) |>
  summarise(
    p_ave_monthly_mean = mean(p_ave, na.rm = T),
    p_ave_monthly_max = max(p_ave, na.rm = T)
  ) |>
  mutate(
    # average monthly gen proxy
    p_disag_gen = p_ave_monthly_mean / sum(p_ave_monthly_mean),
    # the maximum observed monthly capacity factor to scale to
    p_disag_gen_max = p_ave_monthly_max / sum(p_ave_monthly_mean),
    gen_proxy = 'ave_gen'
  )

# simple smooting function based on a max proprtion value
smooth_monthly_proxy = function(p, p_max = 0.25, buffer = 1.25) {
  iter = 0
  while (any(p > p_max)) {
    iter = iter + 1
    p = zoo::rollmean(c(rep(0, 6), p, rep(0, 6)), 13)
    p = p / sum(p)
    if (iter > 50) {
      break
    }
  }
  p
}

hydro_gen_with_flow_gen_proxy =
  hydro_gen_with_flow_proxy |>
  left_join(ave_gen_proxy, by = join_by(eia_id, month)) |>
  mutate(
    proxy = ifelse(is.na(flow_proxy), gen_proxy, flow_proxy),
    p_disag1 = ifelse(is.na(p_disag1), p_disag_gen, p_disag1)
  ) |>
  group_by(eia_id, year) |>
  group_split() |>
  map_dfr(
    \(plant_year) {
      if (any(is.na(plant_year$gen_proxy))) {
        return(plant_year)
      }
      plant_year |> mutate(p_disag1 = smooth_monthly_proxy(p_disag1, max(p_disag_gen_max)))
    },
    .progress = T
  )

# should not return any rows
# hydro_gen_with_flow_gen_proxy |> filter(is.na(p_ave) & is.na(p_disag1))

hydro_gen_with_disag_needs_scaling =
  hydro_gen_with_flow_gen_proxy |>
  # ungroup() |>
  # group_by(eia_id, year) |>
  mutate(
    data_source = case_when(
      is.na(p_ave) ~ flow_proxy,
      .default = data_source
    ),
    # to disag from annual aMW to monthly aMW,
    # need to convert to energy first then convert back
    p_ave = case_when(
      is.na(p_ave) & !is.na(p_disag1) ~ p_disag1 * annual_p_ave_i * n_hours_annual / n_hours,
      is.na(p_ave) & !is.na(p_disag2) ~ p_disag2 * annual_p_ave_i * n_hours_annual / n_hours,
      is.na(p_ave) & !is.na(p_disag3) ~ p_disag3 * annual_p_ave_i * n_hours_annual / n_hours,
      .default = p_ave
    )
  ) |>
  distinct(datetime, eia_id, p_ave, .keep_all = TRUE) |>
  # anything missing at this point did not have data or a flow proxy
  # so use the (imputed) annual value
  mutate(
    data_source = case_when(
      is.na(p_ave) ~ data_source_annual,
      .default = data_source
    ),
    p_ave = case_when(
      is.na(p_ave) ~ annual_p_ave,
      .default = p_ave
    )
  ) |>
  # recompute total
  mutate(
    target_mwh = p_ave * n_hours,
    # net_gen_mw = p_ave_i,
    nameplate_mwh = nameplate_mw * n_hours
  )
#  Should be no missing p_ave or nameplate_mw data at this point
hydro_gen_with_disag_needs_scaling |> filter(is.na(p_ave))
hydro_gen_with_disag_needs_scaling |> filter(is.na(nameplate_mw))

# need to scale the proxy-based gen when it exceeds the nameplate cap
# using this approach from rectifhyd
#   PNNL_MW = fraction * netgen_annual / n_hours,
#   PNNL_MWh = PNNL_MW / max(PNNL_MW) * nameplate_MW * n_hours,
#   PNNL_MW = PNNL_MWh / n_hours
hydro_gen_with_disag =
  hydro_gen_with_disag_needs_scaling |>
  group_by(eia_id, year) |>
  group_split() |>
  map_dfr(
    \(plant_year) {
      p_max = max(plant_year$p_ave)

      if (all(plant_year$p_ave < p_max) | p_max == 0) {
        return(plant_year)
      }
      plant_year |>
        mutate(
          target_mwh = p_ave / p_max * nameplate_mw * n_hours,
          p_ave = target_mwh / n_hours
        )
    },
    .progress = T
  )

# %%
# now impute the remaining data that was not covered by
# any data source or flow based disag
# if (!file.exists(hydro_gen_with_disag_imputed_fn) | !cache) {
#   hydro_gen_with_disag_imputed_wide =
#     hydro_gen_with_disag |>
#     pivot_wider(id_cols = datetime, names_from = eia_id, values_from = p_ave) |>
#     select(-datetime) |>
#     missRanger()
#   hydro_gen_with_disag_imputed_wide |>
#     write_csv(hydro_gen_with_disag_imputed_fn)
# } else {
#   hydro_gen_with_disag_imputed_wide = read_csv(hydro_gen_with_disag_imputed_fn)
# }

# hydro_gen_with_disag_imputed =
#   hydro_gen_with_disag |>
#   inner_join(
#     hydro_gen_with_disag_imputed_wide |>
#       mutate(datetime = hydro_gen_with_disag$datetime |> unique() |> sort()) |>
#       pivot_longer(-datetime, names_to = 'eia_id', values_to = 'p_ave_i'),
#     by = join_by(datetime, eia_id)
#   ) |>
#   group_by(eia_id) |>
#   # any nameplate value missing at this point needs to be filled manually
#   fill(nameplate_mw, .direction = 'downup') |>
#   mutate(
#     # set the missing data labels
#     data_source_nameplate = case_when(
#       is.na(data_source_nameplate) ~ "filled",
#       .default = data_source_nameplate
#     ),
#     data_source = case_when(
#       is.na(data_source) ~ "imputed",
#       .default = data_source
#     ),
#     # set the period average power where missing
#     p_ave = case_when(
#       is.na(p_ave) ~ p_ave_i,
#       .default = p_ave
#     ),
#   ) |>
#   # recompute total
#   mutate(
#     target_mwh = p_ave * n_hours,
#     # net_gen_mw = p_ave_i,
#     nameplate_mwh = nameplate_mw * n_hours
#   )

# %%

# hydro_gen_with_disag %>%
#   # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
#   multipage_pdf_by_group('eia_id', 'p_ave', 'figures/complete_gen_monthly_imputed.pdf') |>
#   # ggplot warns about dropping NA values, ignore
#   suppressWarnings()

# ----------------------------------------------------------------------------
# data cleaning --------------------------------------------------------------
# ----------------------------------------------------------------------------
# %%
# Compute complete timeseries of capacity at each plant based on
# when the plant began and possibly ended service
eia_hydro_generators =
  read_parquet(pudl_generators_fn) |>
  filter(
    energy_source_code_1 == 'WAT',
    prime_mover_code == 'HY'
  ) |>
  mutate(eia_id = as.character(plant_id_eia))

find_generator_operating_date_range = function(generator) {
  operating_date = generator |>
    pull(generator_operating_date) |>
    agg_na_rm_unless_all_na(min) |>
    as.Date()
  retirement_date = generator |>
    pull(generator_retirement_date) |>
    agg_na_rm_unless_all_na(min) |>
    as.Date()
  # browser()
  tibble(
    eia_id = generator$eia_id,
    generator_id = generator$generator_id,
    operating_date,
    retirement_date
  )
}

pudl_operating_dates =
  eia_hydro_generators |>
  group_by(eia_id) |>
  group_split() %>%
  # .[[1]] -> x
  map_dfr(
    function(generator) {
      # browser()
      generator |> find_generator_operating_date_range() |> mutate(eia_id = as.character(eia_id))
    },
    .progress = TRUE
  ) |>
  distinct_all() |>
  # fix corner cases with the dates
  filter(is.na(retirement_date) | retirement_date > ym(s('{start_year}-01'))) |>
  filter(retirement_date > operating_date | is.na(retirement_date) | is.na(operating_date)) |>
  group_by(eia_id, generator_id) |>
  group_split() |>
  map_dfr(
    \(plant) {
      # message(plant$eia_id)
      start_date = ym(s('{start_year}-01'))
      end_date = ym(s('{end_year}-12'))

      eid = plant$eia_id
      gid = plant$generator_id

      # if (eid == 10903) {
      #   message(paste(
      #     eid,
      #     gid,
      #     plant$operating_date,
      #     plant$retirement_date,
      #     start_date,
      #     end_date,
      #     '\n'
      #   ))
      # }
      od = plant$operating_date
      na_start = is.na(od)
      od = if_else(!na_start, as.Date(od), start_date)

      rd = plant$retirement_date
      na_end = is.na(rd)
      rd = if_else(!na_end, as.Date(rd), end_date)

      op_code = if (na_start) {
        if (na_end) {
          # TODO find more sources of info on operation dates (ferc?)
          # both begin and end dates are missing so assume it is in service
          'existing_assumed'
        } else {
          # start date missing so assume it was in service
          'existing_assumed'
        }
      } else {
        if (na_end) {
          # retirement dates are empty if its still in service
          'existing'
        } else {
          # both start and end dates are there
          'existing'
        }
      }

      # if (eid == 10903) {
      #   message(paste(
      #     eid,
      #     gid,
      #     na_start,
      #     na_end,
      #     start_date,
      #     op_code,
      #     max(c(od, start_date), na.rm = T),
      #     min(c(rd, end_date), na.rm = T),
      #     '\n'
      #   ))
      # }

      # message(paste(eid, gid, od, rd, start_date, end_date, '\n'))
      # browser()
      expanded_dates = tibble(
        eia_id = eid,
        generator_id = gid,
        datetime = seq.Date(
          max(c(od, start_date), na.rm = T),
          min(c(rd, end_date), na.rm = T),
          by = 'month'
        ),
        operational_status = op_code
      )

      full_dates = tibble(
        eia_id = eid,
        generator_id = gid,
        datetime = seq.Date(start_date, end_date, by = 'month'),
        operating_date = od,
        retirement_date = rd
      )

      # if (eid == 100) {
      #   # browser()
      # }
      op_status = full_dates |>
        full_join(expanded_dates, by = join_by(eia_id, generator_id, datetime)) |>
        mutate(
          operational_status = case_when(
            is.na(operational_status) & datetime < operating_date ~ 'prior_to_service',
            is.na(operational_status) & datetime >= retirement_date ~ 'retired',
            .default = operational_status
          )
        )
    },
    .progress = TRUE
  )

# should return no rows
# pudl_operating_dates |> group_by(eia_id,operating_date) |> groups_gt1()
eia_hydro_gen_monthly_1980_2022_complete = read_csv('data/complete_gen_eia_raw_1980_to_2024.csv')
eia_plant_capacity =
  eia_hydro_gen_monthly_1980_2022_complete |>
  select(datetime, eia_id = eia_id1, nameplate_mw) |>
  mutate(eia_id = as.character(eia_id))

eia_generator_capacity =
  pudl_operating_dates |>
  left_join(
    eia_hydro_generators |> select(eia_id, generator_id, datetime = report_date, capacity_mw),
    by = join_by(eia_id, generator_id, datetime)
  )
eia_generator_capacity |> write_csv('data/eia_generator_capacity_1980_2024.csv')

eia_plant_capacity_complete =
  eia_generator_capacity |>
  group_by(datetime, eia_id) |>
  # aggregate generators
  summarise(
    operating_date = min(operating_date),
    retirement_date = min(retirement_date),
    # operational_status = if_else(
    #   any(str_detect(operational_status, 'existing')),
    #   str_subset(operational_status, 'existing')[1],
    #   operational_status[1]
    # ),
    operational_status = str_flatten_comma(unique(operational_status)),
    capacity_mw = agg_na_rm_unless_all_na(capacity_mw, sum)
  ) |>
  # now set missing capacity based on operational status
  mutate(
    # zero out any capacity values when the plant was not in service
    capacity_mw = case_when(
      operational_status == 'prior_to_service' ~ 0,
      operational_status == 'retired' ~ 0,
      .default = capacity_mw
    )
  ) |>
  left_join(eia_plant_capacity, by = join_by(datetime, eia_id)) |>
  mutate(
    # zero out any capacity values when the plant was not in service
    nameplate_mw = case_when(
      operational_status == 'prior_to_service' ~ 0,
      operational_status == 'retired' ~ 0,
      .default = nameplate_mw
    )
  ) |>
  # replace missing or inaccurate capacity values
  mutate(
    nameplate_mw = case_when(
      is.na(nameplate_mw) ~ capacity_mw,
      capacity_mw < 0.0001 & nameplate_mw > 0 ~ 0,
      # capacity_mw < nameplate_mw & nameplate_mw > 0 ~ 0,
      .default = nameplate_mw
    )
  ) |>
  select(-capacity_mw)


plants_with_missing_cap =
  eia_plant_capacity_complete |>
  filter(is.na(nameplate_mw)) |>
  pull(eia_id) |>
  unique()

# sanity check
# estimated_cap_by_gen =
#   eia_hydro_gen_monthly_1980_2022_complete |>
#   filter(eia_id1 %in% plants_with_missing_cap) |>
#   group_by(eia_id) |>
#   summarise(capacity_mw_est = agg_na_rm_unless_all_na(net_gen_mwh, max)) |>
#   drop_na(capacity_mw_est) |>
#   filter(capacity_mw_est > 0)
# eia_hydro_generators |>
#   filter(plant_id_eia %in% plants_with_missing_cap) |>
#   drop_na(net_generation_mwh) |>
#   filter(net_generation_mwh > 0) |>
#   distinct(plant_id_eia, generator_id)

# ----------------------------------------------------------------------------
# fix bad data ---------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% Merge in the capacity values that have been modfied according to the service dates
hydro_gen_with_disag_with_op_status =
  hydro_gen_with_disag |>
  left_join(
    eia_plant_capacity_complete |>
      mutate(eia_id1 = as.numeric(eia_id)) |>
      select(-eia_id) |>
      rename(np1 = nameplate_mw),
    by = join_by(eia_id1, datetime)
  ) |>
  left_join(
    eia_plant_capacity_complete |>
      mutate(eia_id2 = as.numeric(eia_id)) |>
      select(-eia_id) |>
      rename(
        np2 = nameplate_mw,
        operating_date2 = operating_date,
        retirement_date2 = retirement_date,
        operational_status2 = operational_status
      ),
    by = join_by(eia_id2, datetime)
  ) |>
  left_join(
    eia_plant_capacity_complete |>
      mutate(eia_id3 = as.numeric(eia_id)) |>
      select(-eia_id) |>
      rename(
        np3 = nameplate_mw,
        operating_date3 = operating_date,
        retirement_date3 = retirement_date,
        operational_status3 = operational_status
      ),
    by = join_by(eia_id3, datetime)
  )

#TODO fix these edge cases that slipped through
hydro_gen_with_disag_with_op_status |> filter(is.na(operational_status))

# Fix the gen and nameplate for periods where the plant was not operating
hydro_gen_corected =
  hydro_gen_with_disag_with_op_status |>
  ungroup() |>
  mutate(
    p_ave = case_when(
      !str_detect(operational_status, 'existing') ~ 0,
      # np1 == 0 & nameplate_mw > 0 ~ 0,
      .default = p_ave
    ),
    nameplate_mw = case_when(
      !str_detect(operational_status, 'existing') ~ 0,
      # np1 == 0 & nameplate_mw > 0 ~ 0,
      .default = nameplate_mw
    ),
    target_mwh = p_ave * n_hours,
    # nameplate_mwh = nameplate_mw * n_hours,
    capacity_factor = p_ave / nameplate_mw
  )


# ----------------------------------------------------------------------------
# finalize -------------------------------------------------------------------
# ----------------------------------------------------------------------------

hydro_gen_complete_limited_metadata =
  hydro_gen_corected |>
  mutate(
    # eia_id_was_combined = eia_id_was_combined | eia_id_was_combined2,
    n_eia_ids_combined = max(n_eia_ids_combined, n_eia_ids_combined2)
  ) |>
  select(
    datetime,
    eia_id,
    data_source,
    p_ave,
    target_mwh,
    data_source_nameplate,
    nameplate_mw,
    n_hours,
    eia_id_was_combined,
    operating_date,
    retirement_date,
    operational_status,
    eia_id_was_combined,
    eia_id1,
    eia_id2,
    eia_id3,
    plant_id_pudl1,
    plant_id_pudl2
  ) |>
  # TODO fix these unknown cases
  mutate(data_source = ifelse(is.na(data_source), 'unknown', data_source))

# %%
# TODO bring in more plant metdata
hydro_plant_metadata =
  eia_hydro_plants_pudl |>
  filter(year(report_date) <= end_year) |>
  mutate(eia_id = as.numeric(eia_id)) |>
  select(
    eia_id1 = eia_id,
    plant_name_eia,
    # plant_id_pudl,
    lat = latitude,
    lon = longitude,
    county,
    state,
    timezone,
    ba_eia = balancing_authority_code_eia,
    nerc_region,
    datetime = report_date,
    utility_id_eia,
    utility_name_eia
  )
# TODO Create metadata consistent with the combined eia plants
# eha |> group_by(eia_id) |> mutate(eha_id=str_flatten_comma(eha_ptid)) |> filter(eia_id=='54103')
# left_join(eha |> distinct(eia_id, .keep_all = T), by = join_by(eia_id))

hydro_gen_monthly_complete =
  hydro_gen_complete_limited_metadata |>
  left_join(hydro_plant_metadata, by = join_by(datetime, eia_id1)) |>
  group_by(eia_id) |>
  # assume metadata is constant for all missing values
  fill(
    lat,
    lon,
    county,
    state,
    timezone,
    ba_eia,
    nerc_region,
    utility_id_eia,
    utility_name_eia,
    .direction = 'downup'
  ) |>
  ungroup()


# %%
hydro_gen_monthly_complete %>%
  # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
  multipage_pdf_by_group('eia_id', 'p_ave', 'figures/complete_gen_monthly_final.pdf') |>
  # ggplot warns about dropping NA values, ignore
  suppressWarnings()

#%%

modes = eha |>
  # select(eia_id = EIA_PtID, mode = Mode) |>
  filter(!is.na(eia_id)) |>
  mutate(mode = if_else(grepl("Run-of-river", mode), "RoR", "Storage")) |>
  filter(!duplicated(eia_id)) |>
  unique() |>
  rename(eia_id1 = eia_id)

b1_params_general =
  read_csv(
    "data/b1_params_pnw_federal_dams_max_min_ador_monthly.csv",
    # col_types = cols(eia_id = col_character())
  ) |>
  rename(eia_id1 = eia_id) |>
  left_join(modes, by = join_by(eia_id1)) |>
  group_by(mode) |>
  summarise(
    max_param = mean(max_param),
    min_param = mean(min_param),
    ador_param = mean(ador_param)
  )

b1_params_pnw =
  read_csv(
    "data/b1_params_pnw_federal_dams_max_min_ador_monthly.csv",
    # col_types = cols(eia_id = col_character())
  ) |>
  select(-dam) |>
  rename(eia_id1 = eia_id)


hydro_gen_monthly_final =
  bind_rows(
    hydro_gen_monthly_complete |>
      left_join(modes, by = join_by(eia_id1)) |>
      mutate(mode = ifelse(is.na(mode), 'RoR', mode)) |>
      filter(!(eia_id %in% b1_params_pnw[["eia_id"]])) |>
      left_join(b1_params_general, by = join_by(mode)),
    hydro_gen_monthly_complete |>
      left_join(b1_params_pnw, by = join_by(eia_id1)) |>
      filter(eia_id %in% b1_params_pnw[["eia_id"]])
  ) |>
  mutate(
    p_max = p_ave + max_param * (nameplate_mw - p_ave),
    p_min = min_param * p_ave,
    ador = ador_param * (p_max - p_min)
  ) |>
  select(-c(max_param, min_param, ador_param)) |>
  mutate(
    western = if_else(
      state %in% c("WA", "ID", "CO", "UT", "NM", "WY", "MT", "CA", "OR", "NV", "AZ"),
      TRUE,
      FALSE
    ),
    year = year(datetime),
    month = month(datetime)
  )
#|>
# select(
#   eia_id,
#   plant,
#   state,
#   ba,
#   year,
#   month,
#   target_mwh,
#   nameplate = nameplate_mw,
#   p_ave,
#   p_min,
#   p_max,
#   ador
# ) |>
# left_join(eia_and_huc4, by = join_by(eia_id)) |>
# left_join(huc4_flows_monthly, by = join_by(year, month, HUC4)) |>
# rename(HUC4_flow_cfs = av_flow_cfs)

# perform basic checks
# pnw_dam_data = read_csv('data/usace_dam_data_daily_1980_2024.csv') |> mutate(eia_id1 = eia_id)

# these should all return zero rows
hydro_gen_monthly_final |> filter(p_min < 0)
hydro_gen_monthly_final |> filter(p_min > p_max)
hydro_gen_monthly_final |> filter(p_min > (p_max - ador))
hydro_gen_monthly_final |> filter(p_min > p_ave)
hydro_gen_monthly_final |> filter(p_max < p_ave - .001)
hydro_gen_monthly_final |> filter(p_max < p_min)
hydro_gen_monthly_final |> filter(ador < 0)
hydro_gen_monthly_final |> filter(ador < 0)


# mutate(year = year(datetime), month = month(datetime)) |>
# left_join(pnw_dam_data, by = join_by(year, month, eia_id1)) #|>
# mutate(month = factor(month, levels = month.abb)) |>
# mutate(
# eia_id = as.integer(eia_id),
# year = as.integer(year),
# datetime = sprintf('%s-%02d-01', year, `names=`(1:12, month.abb)[month])
# ) #|>
# rename(eia_id = eia_id) |>
# filter(eia_id == 3075) |>
# ggplot(aes(month, p_ave, group = year)) +
# geom_line() +
# facet_wrap(~year) +
# geom_line(aes(y = p_min), col = "red") +
# geom_line(aes(y = p_max), col = "blue") +
# geom_line(aes(y = ador), col = "pink")

hydro_gen_monthly_final |>
  mutate_if(is.double, function(x) round(x, 4)) |>
  write_csv(paste0(output_dir, "/B1_monthly.csv"), na = "")

# monthly = list.files(output_dir, '*monthly*', full.names = T) |>
#   map(function(x) read_csv(x, progress = F, show = F)) |>
#   bind_rows()

hydro_gen_monthly_final |>
  filter(western) |>
  mutate(year = year(datetime), month = month(datetime)) |>
  group_by(month, year) |>
  summarise(energy_mwh = sum(target_mwh, na.rm = T), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  ggplot(aes(month, energy_mwh / 1000, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("orange", "cornflowerblue")) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Energy [GWh]")

hydro_gen_monthly_final |>
  filter(western) |>
  group_by(year, month) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  pivot_wider(id_cols = month, names_from = year, values_from = energy_mwh) |>
  mutate(pct_diff = (`2001` - `2009`) / `2009` * 100)
