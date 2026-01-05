# 3b-disaggregate-using-gage-flow.R
#
# Prepare data to disaggregate annual hydropower using downstream flows
#
# Original author: Sean Turner sean.turner@pnnl.gov
#
## 2023 1.2.1 Update - Cameron Bracken cameron.bracken@pnnl.gov
## 2024 1.4.0 Update - Cameron Bracken cameron.bracken@pnnl.gov

# %%
library(tidyverse)
library(conflicted)
library(dataRetrieval)
library(readxl)
library(sf)

conflicts_prefer(dplyr::filter)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)


# %%
# year of the latest EIA data to use
start_year = 2001
end_year = 2024
eha_fn = "data/ORNL_EHAHydroPlant_PublicFY2024.xlsx"
hillari_gpkg = "data/HILARRI_v3/HILARRI_v3_preliminary.gpkg"

# %%
# read in HILARRI database
hilarri = st_read(hillari_gpkg) |>
  filter(!is.na(eha_ptid)) |>
  st_drop_geometry()

# %%
# get huc4 for plants that are not in hilarri
additional_HUC =
  read_xlsx(eha_fn, sheet = "Operational") |>
  janitor::clean_names(parsing_option = 3) |>
  rename(eha_ptid = eha_pt_id) |>
  filter(!(eha_ptid %in% hilarri$eha_ptid), !is.na(huc)) |>
  # HUCs in the 2022 and 2023 data are formatted as numbers not text
  # so we have to pull some funny business to prevent scientific notation
  # getting converted to text and to preserve the leading zeros
  mutate(huc = format(huc, scientific = FALSE)) |>
  mutate(huc = gsub(" ", "0", huc)) |>
  # mutate(HUC = if_else(nchar(HUC) == 11, paste0("0", HUC), as.character(HUC))) |>
  # filter(nchar(HUC) == 12) |>
  mutate(huc4 = substr(huc, 1, 4)) |>
  select(eha_ptid = eha_ptid, huc4) |>
  unique()

# %%
# get eha ids for plants that have release data
release_based_cases =
  read_csv("data/release_based_fractions.csv") |>
  group_by(eha_ptid) |>
  group_split() |> # .[[20]] -> plant
  map_dfr(function(plant) {
    plant |>
      mutate(is_na = is.na(fraction)) |>
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
plants_to_disag_with_flow =
  hilarri |>
  mutate(huc4 = substr(huc_12, 1, 4)) |>
  select(eha_ptid, huc4) |>
  bind_rows(additional_HUC) |>
  filter(!eha_ptid %in% release_based_cases) |>
  as_tibble()

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
  read_csv("data/huc4_flow_usgs_long_imputed.csv") |>
  group_by(huc4, day, month, year) |>
  summarise(av_flow_cfs = mean(av_flow_cfs, na.rm = T)) |>
  group_by(huc4, month, year) |>
  summarise(av_flow_cfs = mean(av_flow_cfs, na.rm = T)) |>
  right_join(year_month_seq, by = c("month", "year"))


# %%
# huc4 flow based fractions
initial_huc4_fractions =
  plants_to_disag_with_flow |>
  left_join(
    huc4_average_flows_all |>
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
  group_by(eha_ptid) |>
  group_split() |> # .[[1]] -> site
  map_dfr(function(site) {
    if (!any(is.na(site[["fraction"]]))) {
      # message(site)
      return(site)
    }

    site |>
      group_by(month) |>
      summarise(fraction = mean(fraction, na.rm = T)) |>
      mutate(fraction = fraction / sum(fraction)) -> replacement_fractions

    site |>
      group_by(year) |>
      group_split() |>
      # split(.$year) |> # .[[2]] -> yr
      map_dfr(function(yr) {
        if (!any(is.na(yr$fraction))) {
          return(yr)
        }

        yr$fraction <- replacement_fractions$fraction
        return(yr)
      })
  })

final_huc4_fractions |>
  write_csv("data/huc4_based_fractions.csv")

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
