# read_eia_hydro_gen_pre_2001.R
#
# Created by Cameron Bracken, cameron.bracken@pnnl.gov, Dec 31, 2025
#

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')


# ---------------------------------------------------------------------------
# data ingest ---------------------------------------------------------------
# ---------------------------------------------------------------------------
# %% Read hilarri and pudl data

# Read HILLARI v3 data which has been pre-filtered for just CONUS hydro plants
# see downloading script
hilarri_hydro_plants =
  file.path(data_dir, config::get('hilarri_csv')) |>
  read_csv()

# Read PUDL (eia) data
# pudl data filtered for just hydro and cleaned up columns
eia_hydro_generators = read_eia_pudl_generators_parquet_hydro(pudl_generators_fn)
# raw pudl data
eia_hydro_plants = read_parquet(pudl_plants_fn)
eia_generators_raw = read_parquet(pudl_generators_fn)
eia_hydro_generators_raw =
  eia_generators_raw |>
  filter(
    energy_source_code_1 == 'WAT',
    prime_mover_code == 'HY'
  )

# %% Determine target hydro plants from hilarri and pudl data
target_eia_ids_hilarri = hilarri_hydro_plants$eia_ptid |>
  unique() |>
  as.integer() |>
  sort()
target_eia_ids_eia_pudl = eia_hydro_generators_raw$plant_id_eia |>
  unique() |>
  as.integer() |>
  sort()

# these are all either super small (<1MW) or
# TODO figure out a coherent plan for these plants
# these are generally small and/or non operational plants
# but add them anyway for completeness
hilarri_eia_ids_not_in_pudl = setdiff(target_eia_ids_hilarri, target_eia_ids_eia_pudl)
# HILLARI is missing a bunch...
pudl_eia_ids_not_in_hilarri = setdiff(target_eia_ids_eia_pudl, target_eia_ids_hilarri)

# check if any of the eha plant are actually in pudl after
# pre-screening has been done
target_eia_ids_hilarri_additional =
  eia_hydro_generators_raw |>
  # eia_hydro_generators |>
  filter(plant_id_eia %in% pudl_eia_ids_not_in_hilarri) |>
  distinct(plant_id_eia, generator_id, .keep_all = T)

# some eia ids do not have pudl data, but we still want to include them to reconstruct with proxies
target_eia_ids =
  c(
    target_eia_ids_eia_pudl,
    target_eia_ids_hilarri_additional$plant_id_eia
  ) |>
  unique() |>
  sort() |>
  as.character()


# %% setup for filling missing data and combining plants

# all dataes we want to represent
complete_date_seq = seq.Date(
  s('{start_year}-01-01'),
  s('{end_year}-12-01'),
  by = 'month'
)

# some pudl plants have mutiple eia ids
pudl_eia_map_uncombined =
  eia_hydro_generators |>
  filter(eia_id %in% target_eia_ids) |>
  distinct(plant_id_pudl, eia_id)

pudl_eia_map_combined =
  pudl_eia_map_uncombined |>
  arrange(eia_id) |>
  group_by(plant_id_pudl) |>
  summarise(eia_id = str_flatten(eia_id, '_')) |>
  mutate(
    eia_id1 = str_split_i(eia_id, '_', 1),
    eia_id2 = str_split_i(eia_id, '_', 2),
    eia_id3 = str_split_i(eia_id, '_', 3)
  )

message('\nAggregating the following eia ids that have the same pudl id:')
pudl_eia_map_combined |> filter(!is.na(eia_id2)) |> print(n = 100)

# %%  other pudl/eia ids should to be combined since they are only
# split for reporting purposes (see rectifhydplus paper for details)
#
# The combined plants are in identified by the RHPID field when it has
# a forward slashsett
# eg. "id1/it2_desc"
rfpids_combined = read_csv(rfp_data_fn) |>
  filter(str_detect(RHPID, '/')) |>
  distinct(RHPID, .keep_all = T) |>
  select(1)

rfp_eia_pudl_map_combined = tibble(
  eia_id = rfpids_combined |>
    pull(RHPID) |>
    str_split_i('_', 1) |>
    str_replace('/', '_'),
  eia_id1 = eia_id |> str_split_i('_', 1),
  eia_id2 = eia_id |> str_split_i('_', 2)
) |>
  left_join(
    eia_hydro_generators |>
      distinct(plant_id_pudl1 = plant_id_pudl, eia_id1 = eia_id),
    by = join_by(eia_id1)
  ) |>
  left_join(
    eia_hydro_generators |>
      distinct(plant_id_pudl2 = plant_id_pudl, eia_id2 = eia_id),
    by = join_by(eia_id2)
  ) |>
  mutate(plant_id_pudl = paste0(plant_id_pudl1, '_', plant_id_pudl2)) |>
  filter(plant_id_pudl1 != plant_id_pudl2)

rfp_eia_pudl_map_uncombined = rfp_eia_pudl_map_combined |>
  pivot_longer(c(plant_id_pudl1, plant_id_pudl2)) |>
  rename(plant_id_pudl_new = plant_id_pudl, plant_id_pudl = value) |>
  select(-c(name)) |>
  pivot_longer(c(eia_id1, eia_id2)) |>
  rename(eia_id_new = eia_id, eia_id = value) |>
  select(-c(name))

# for testing
# pudl_eia_mapping = pudl_eia_map_uncombined
# rfp_pudl_eia_mapping = rfp_eia_pudl_map_combined

message('\nAggregating the following eia ids that :')
rfp_eia_pudl_map_uncombined |>
  filter(str_detect(plant_id_pudl_new, '_')) |>
  distinct(eia_id_new, .keep_all = TRUE) |>
  print(n = 100)

# %% read gen from eia spreadsheets
# read and format hydro data from EIA using rfp function for reading EIA data
# only pulls 1980 to 2022
message('\nReading and formatting eia spreadsheets.')

# reading the spreadsheets uses functions from rectifhydplus, thanks Sean!
rfp_code_path =
  if (config::get('rfp_code_checkout_repo')) {
    config::get('rfp_code_dir') |>
      paste0('_main') %>%
      file.path(data_dir, .)
  } else {
    config::get('rfp_code_checkout_repo') |>
      paste0('_', rfp_version) %>%
      file.path(data_dir, .)
  }
source(file.path(rfp_code_path, '/R/EIA_xl_readers.R'))
source(file.path(rfp_code_path, '/R/data reading and cleaning.R'))

# set up input data paths
rfp_inputs_dir = config::get('rfp_inputs_dir') |> paste0('_', rfp_version)
eia_gen_dir = file.path(data_dir, rfp_inputs_dir, 'EIA/Generation/')
eia_plant_dir = file.path(data_dir, rfp_inputs_dir, 'EIA/Plant/')

eia_raw_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_eia_raw_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(eia_raw_complete_fn)) {
  eia_hydro_gen_monthly_1980_2022_complete =
    read_eia_spreadsheets_rfp(
      eia_gen_dir,
      eia_plant_dir,
      target_eia_ids
    ) |>
    # ignore warnings about converting character data in generation spreadheets
    suppressWarnings() |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined,
      data_source_name = 'eia_raw'
    )
  eia_hydro_gen_monthly_1980_2022_complete |>
    write_csv(eia_raw_complete_fn)
} else {
  eia_hydro_gen_monthly_1980_2022_complete = read_csv(eia_raw_complete_fn)
  message(s('Loaded cached file: {eia_raw_complete_fn}'))
}

# %%
# get PUDL data, has data back to 2001
message('\nReading and formatting pudl eia data.')
eia_pudl_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_eia_pudl_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(eia_pudl_complete_fn)) {
  eia_hydro_gen_monthly_pudl_complete =
    eia_hydro_generators |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined,
      data_source_name = 'eia_pudl '
    )
  eia_hydro_gen_monthly_pudl_complete |>
    write_csv(eia_pudl_complete_fn)
} else {
  eia_hydro_gen_monthly_pudl_complete = read_csv(eia_pudl_complete_fn)
  message(s('Loaded cached file: {eia_pudl_complete_fn}'))
}


#%% rectifhyd (not plus)
message(s('\nReading and formatting rectifhyd data version {rf_version}.'))

rf_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_{rf_version_tag}_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(rf_complete_fn)) {
  rf_hydro_gen_monthly_complete = read_rectifhyd(rf_fn) |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined,
      data_source_name = rf_version_tag
    )
  rf_hydro_gen_monthly_complete |>
    write_csv(rf_complete_fn)
} else {
  rf_hydro_gen_monthly_complete = read_csv(rf_complete_fn)
  message(s('Loaded cached file: {rf_complete_fn}'))
}


# %% rectifhyd plus
# rfp monthly data
message(s('\nReading and formatting rectifhydplus data version {rfp_version}.'))
rfp_complete_fn =
  file.path(
    data_dir,
    s('complete_gen_{rfp_version_tag}_{start_year}_to_{end_year}.csv')
  )
if (!file.exists(rfp_complete_fn)) {
  rfp_hydro_gen_monthly_complete = get_rfp_monthly_hydro_gen(rfp_data_fn) |>
    complete_and_combine_eia_ids(
      complete_date_seq,
      target_eia_ids,
      pudl_eia_map_uncombined,
      rfp_eia_pudl_map_uncombined,
      data_source_name = rfp_version_tag
    )
  rfp_hydro_gen_monthly_complete |>
    write_csv(rfp_complete_fn)
} else {
  rfp_hydro_gen_monthly_complete = read_csv(rfp_complete_fn)
  message(s('Loaded cached file: {rfp_complete_fn}'))
}

# ---------------------------------------------------------------------------
# merging datasets ----------------------------------------------------------
# ---------------------------------------------------------------------------

# %% Combine all the monthly data sources, rfp, rf, eia, eia_pudl
message('\nCombinining data sources.')

# joining all the data manually to check for errors
hydro_gen_data_complete =
  eia_hydro_gen_monthly_pudl_complete |>
  left_join(
    rfp_hydro_gen_monthly_complete,
    by = join_by(eia_id, datetime),
    suffix = c('-pudl_eia', s('-rfp_{rfp_version}'))
  ) |>
  inner_join(
    eia_hydro_gen_monthly_1980_2022_complete |>
      inner_join(
        rf_hydro_gen_monthly_complete,
        by = join_by(eia_id, datetime),
        suffix = c('-eia', s('-rf_{rf_version}'))
      ),
    by = join_by(datetime, eia_id)
  ) |>
  # data source columns are now redundant
  select(-starts_with('data_source'))

hydro_gen_data_complete |> write_csv(hydro_gen_data_wide_fn)
message(s('Wrote {hydro_gen_data_wide_fn}'))

hydro_gen_data_long = bind_rows(
  eia_hydro_gen_monthly_pudl_complete,
  rfp_hydro_gen_monthly_complete,
  eia_hydro_gen_monthly_1980_2022_complete,
  rf_hydro_gen_monthly_complete
) |>
  mutate(
    n_hours = days_in_month(datetime) * 24,
    net_gen_mw = net_gen_mwh / n_hours,
    nameplate_mwh = nameplate_mw * n_hours
  )

hydro_gen_data_long |> write_csv(hydro_gen_data_long_fn)
message(s('Wrote {hydro_gen_data_long_fn}'))

# ----------------------------------------------------------------------------
# diagnostics ----------------------------------------------------------------
# ----------------------------------------------------------------------------

#%% diagnostics
message()
message("The following plants have mutiple eia_id's for one plant:")
hydro_gen_data_long |>
  ungroup() |>
  filter(if_any(starts_with("n_eia_ids_combined"), ~ . > 1)) |>
  distinct(
    eia_id,
    across(starts_with("plant_id_pudl")),
    across(starts_with("eia_id"))
  ) |>
  # arrange(eia_id) |>
  print(n = 100)

# to double check, this should should be the same as above
# hydro_gen_data_complete |>
#   filter(if_any(starts_with("n_eia_ids_combined"), ~ . > 1)) |>
#   distinct(
#     eia_id,
#     across(starts_with("plant_id_pudl")),
#     across(starts_with("eia_id"))
#   ) |>
#   select(eia_id,ends_with('-eia')) %>%
#   set_names(names(.) %>% gsub('-eia', '', .)) |>
#   print(n = 100)

# %%
message()
message('These plants were combined in rfp:')

rfpids_combined = read_csv(rfp_data_fn) |>
  filter(str_detect(RHPID, '/')) |>
  distinct(RHPID, .keep_all = T)
print(rfpids_combined |> select(1))

#%%
message()
message('If the eia ids were combined properly then this should have no rows:')
hydro_gen_data_long |>
  group_by(data_source, eia_id) |>
  mutate(
    has_change = length(unique(n_eia_ids_combined)),
    has_change2 = length(unique(n_eia_ids_combined))
  ) |>
  filter(has_change > 1 | has_change2 > 1)


# %%
message()
eia_hydro_gen_monthly_rfp =
  read_eia_spreadsheets_rfp(
    eia_gen_dir,
    eia_plant_dir,
    target_eia_ids,
    complete_data = F
  ) |>
  mutate(
    sum_total = jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec,
    diff = sum_total - total,
    pdiff = diff / total
  ) |>
  filter(pdiff > 0.01)
message(
  'rectifhydplus - these annual values dont add up to the monthly totals within 1%:'
)
# TODO look into these and figure out a fix
eia_hydro_gen_monthly_rfp |>
  print(n = 100)

# %% check target plants
# TODO check why these were getting filtered from the pudl eia data
# target_eia_ids_hilarri_additional |>
#   select(
#     plant_id_eia,
#     generator_id,
#     plant_id_pudl,
#     planned_generator_retirement_date,
#     energy_source_code_1,
#     prime_mover_code,
#     generator_operating_date,
#     generator_retirement_date,
#     operational_status,
#     capacity_mw
#   ) |>
#   print(n = 1000)

message(
  'Checking target plants -- pudl should contain every target plant (should return zero rows):'
)
eia_hydro_generators_raw |> filter(!(plant_id_eia %in% target_eia_ids)) |> print()
# eia_hydro_generators |> filter(!(eia_id %in% target_eia_ids)) |> print()

message(
  'Checking target plants -- these eia ids are in rfp but not in pudl (should return zero rows):'
)
# rectifhyd target plants are based on EIA860, 2022
rfp_target_eia_ids = identify_target_plants(eia_gen_dir, eia_plant_dir)
rfp_target_eia_ids |> filter(!(EIA_ID %in% target_eia_ids)) |> print()

# TODO check annual vs monthly values all datasets
# TODO check nameplate vs observed gen
# TODO run more checks

# %% plots
if (create_figures) {
  hydro_gen_data_long %>%
    # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
    multipage_pdf_by_group('eia_id', 'figures/compare_gen_monthly.pdf') |>
    # ggplot warns aout dropping NA values, ignore
    suppressWarnings()
}


# %% read different sources of annual gen data
# TODO include annual data as well
## Annual
# use PUDL data, has data back to 2001 in some cases
# eia_hydro_gen_annual_pudl = get_eia_annual_hydro_gen_pudl(target_eia_ids) |>
#   mutate(data_source = 'eia_pudl')

# ferc_hydro_gen_annual = get_ferc_annual_hydro_gen() |>
#   mutate(data_source = 'ferc')

# rfp_gen_annual = rfp_hydro_gen_monthly |>
#   mutate(year = year(datetime)) |>
#   group_by(eia_id, year, data_source) |>
#   summarise(
#     datetime = first(datetime),
#     rfp_eia_id_was_split = first(rfp_eia_id_was_split),
#     net_gen_mwh = sum(net_gen_mwh, na.rm = T),
#     nameplate_mw = first(nameplate_mw)
#   )

# eia_hydro_gen_annual = eia_hydro_gen_monthly_1980_2022 |>
#   mutate(year = year(datetime)) |>
#   group_by(eia_id, year, data_source) |>
#   summarise(
#     datetime = first(datetime),
#     net_gen_mwh = sum(net_gen_mwh, na.rm = T),
#     nameplate_mw = first(nameplate_mw)
#   )

if (create_figures) {
  hydro_gen_data_long %>%
    mutate(year = year(datetime)) |>
    group_by(eia_id, year, data_source) |>
    summarise(
      net_gen_mwh = agg_na_rm_unless_all_na(net_gen_mwh, sum),
      net_gen_mw = agg_na_rm_unless_all_na(net_gen_mw, mean),
      nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw, max),
      datetime = ym(s('{year}-01'))
    ) |>
    # filter(eia_id %in% (.$eia_id |> unique() |> head())) |>
    multipage_pdf_by_group('eia_id', 'figures/compare_gen_annual.pdf') |>
    # ggplot warns aout dropping NA values, ignore
    suppressWarnings()
}
