# 3b-disaggregate-using-gage-flow.R
#
# Prepare data to disaggregate annual hydropower using downstream flows
#
# Original author: Sean Turner sean.turner@pnnl.gov
#
## 2023 1.2.1 Update - Cameron Bracken cameron.bracken@pnnl.gov
## 2024 1.4.0 Update - Cameron Bracken cameron.bracken@pnnl.gov

# setup ----------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
# also loads input and output file names
source('packages_and_options.R')
source('utilities.R')


# ---------------------------------------------------------------------------
# data ingest ---------------------------------------------------------------
# ---------------------------------------------------------------------------
# %% Read hilarri and pudl data

# Read HILLARI v3 data which has been pre-filtered for just CONUS hydro plants
# see downloading script
hilarri_hydro_plants = read_csv(hilarri_fn)
eia_huc4 = read_csv('data/eia_huc4.csv')

# %%
# year of the latest EIA data to use
# start_year = 2001
# end_year = 2024
eha_fn = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"
# hillari_gpkg = "data/HILARRI_v3/HILARRI_v3_preliminary.gpkg"

# %%
# read in HILARRI database
# hilarri = st_read(hillari_gpkg) |>
#   filter(!is.na(eha_ptid)) |>
#   st_drop_geometry()

# %%
# TODO check if this is needed
# get huc4 for plants that are not in hilarri, these are just backup
additional_HUC =
  read_xlsx(eha_fn, sheet = "Operational") |>
  janitor::clean_names(parsing_option = 3) |>
  # rename(eha_ptid = eha_pt_id) |>
  filter(!(eha_pt_id %in% hilarri_hydro_plants$eha_ptid), !is.na(huc)) |>
  # HUCs in the 2022 and 2023 data are formatted as numbers not text
  # so we have to pull some funny business to prevent scientific notation
  # getting converted to text and to preserve the leading zeros
  mutate(huc = format(huc, scientific = FALSE)) |>
  mutate(huc = sprintf('%012s', str_trim(huc))) |>
  mutate(huc4 = substr(huc, 1, 4)) |>
  select(eha_ptid = eha_pt_id, eia_id = eia_pt_id, huc4) |>
  filter(!is.na(eia_id)) |>
  distinct_all()


# %%
# get eha ids for plants that have release data
release_based_cases =
  read_csv(release_fractions_fn) |>
  group_by(eha_ptid) |>
  group_split() |> # .[[20]] -> plant
  map_dfr(function(plant) {
    plant |>
      mutate(is_na = is.na(fraction_release)) |>
      group_by(year) |>
      summarise(na_per_year = sum(is_na)) -> na_per_year

    if (any(na_per_year[["na_per_year"]])) {
      message(plant$eha_ptid[1])
      return(tibble())
    }

    return(plant)
  }) |>
  pull(eha_ptid) |>
  unique()

# %%
# get huc4 and remove cases where reservoir release have it covered!
plants_to_disag_with_huc4_flow =
  hilarri_hydro_plants |>
  mutate(huc4 = substr(huc_12, 1, 4)) |>
  select(eha_ptid, eia_id = eia_ptid, huc4) |>
  bind_rows(additional_HUC) |>
  filter(!(eha_ptid %in% release_based_cases)) |>
  as_tibble() |>
  distinct_all()

# desired data sequence
year_month_seq =
  expand.grid(
    year = start_year:end_year,
    month = 1:12 #month.abb #factor(month.abb, levels = month.abb, ordered = T)
  ) |>
  as_tibble()

# %%
# Pull USGS data for each huc4
huc4_average_flows_all =
  read_csv(huc4_flow_imputed_monthly_fn) |>
  # # na.omit() |>
  # group_by(huc4, day, month, year) |>
  # summarise(av_flow_cfs = mean(av_flow_cfs, na.rm = T)) |>
  group_by(huc4, month, year) |>
  summarise(av_flow_cfs = mean(av_flow_cfs, na.rm = T)) |>
  right_join(year_month_seq, by = c("month", "year"))


# %%
# huc4 flow based fractions
initial_huc4_fractions =
  plants_to_disag_with_huc4_flow |>
  left_join(
    huc4_average_flows_all |>
      na.omit() |>
      group_by(year, huc4) |>
      arrange(huc4, year, month) |>
      mutate(fraction = av_flow_cfs / sum(av_flow_cfs)) |>
      ungroup() |>
      select(year, month, huc4, fraction),
    relationship = "many-to-many",
    by = join_by(huc4)
  ) |>
  # select(-huc4) |>
  group_by(eha_ptid, year) |> # filter(eha_ptid == "hc0141_p01") |>
  mutate(
    fraction = if_else(fraction == 0, 0.005, fraction),
    fraction = fraction / sum(fraction)
  ) |>
  ungroup()

# %%
# if any of the fractions are missing, replace the missing value with an average
# of the values from that same month in other years
final_huc4_fractions =
  initial_huc4_fractions |>
  na.omit() |>
  group_by(eha_ptid) |>
  group_split() %>% #  .[[865]] -> site
  map_dfr(function(site) {
    if (!any(is.na(site[["fraction"]]))) {
      # message(site$eha_ptid[1], ' ', site$huc4[1])
      return(site)
    }

    replacement_fractions =
      site |>
      group_by(month) |>
      summarise(fraction = mean(fraction, na.rm = T)) |>
      mutate(fraction = fraction / sum(fraction))

    site |>
      group_by(year) |>
      group_split() |>
      # split(.$year) |> # .[[2]] -> yr
      map_dfr(function(yr) {
        if (!any(is.na(yr$fraction))) {
          return(yr)
        }

        replacement_fractions$fraction = yr$fraction
        return(yr)
      })
  })

final_huc4_fractions |> write_csv(huc4_fractions_fn)

#%%
problem_sites <- huc4_average_flows_all |>
  filter(is.na(av_flow_cfs)) |>
  pull(huc4) |>
  unique()

#
# read_csv("Data/USGS_Streamgage_huc4.csv") |>
#   mutate(huc4 = if_else(nchar(huc4) == 3, paste0("0", huc4), as.character(huc4))) |>
#   #filter(!is.na(StreamGage_FEA1)) |>
#   rename(usgs_id = StreamGage_FEA1) |>
#   mutate(usgs_id = if_else(usgs_id == "03085000", "03072655", usgs_id),
#          usgs_id = if_else(usgs_id == "08162500", "08159200", usgs_id),
#          usgs_id = if_else(usgs_id == "02420000", "02428400", usgs_id),
#          usgs_id = if_else(huc4 == "0415", "04266500", usgs_id),
#          usgs_id = if_else(huc4 == "0106", "01059000", usgs_id),
#          usgs_id = if_else(huc4 == "1602", "10141000", usgs_id),
#          usgs_id = if_else(huc4 == "1707", "14103000", usgs_id)) |>
#   mutate(gap_in_record = if_else(huc4 %in% problem_sites, "Yes", "No")) |>
#   write_csv("Data/USGS_Steamgage_huc4_STgaps.csv")
#
# read_csv("Data/USGS_Steamgage_huc4_STgaps.csv") |> View()
