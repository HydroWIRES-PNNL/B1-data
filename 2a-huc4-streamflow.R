# 2a-huc4-streamflow.R
#
# Created by Sean Turner, 2022
#
# Updated by Cameron Bracken, cameron.braken@pnnl.gov,
# 2023 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2024 Update - Cameron Bracken cameron.bracken@pnnl.gov
# 2025 Update - Cameron Bracken cameron.bracken@pnnl.gov
#             - Added data imputation

library(conflicted)
library(tidyverse)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)


library(tidyverse)
library(dataRetrieval)
library(missRanger)

end_year = 2024

# associate EIA plant code with HUC4
# use Dan's EHA POD dataset EHA and HILLARI have a lot of issues
# run this script to regenerate the eia_huc4.csv file
# source('data/huc4_plants.R')
EIA_and_HUC4 <- read_csv('data/eia_huc4.csv')


## CB Oct 2023, had to change line 140 from Data/USGS_Streamgage_HUC4.csv
## 05587455 to 05587450, the gage was returning no streamflow data
##
## get USGS daily flow data averges
"data/USGS_Streamgage_HUC4.csv" |>
  read_csv() %>%
  # vroom::vroom("Data/USGS_00060_HUC4.csv") %>%
  filter(!is.na(StreamGage_FEA1)) %>%
  select(HUC4, USGS_ID = StreamGage_FEA1) %>%
  # select(HUC4, USGS_ID = USGS_ID) %>%
  pmap_dfr(function(HUC4, USGS_ID) {
    message(USGS_ID)

    gauge_fn <- "data/usgs/%s.csv" |> sprintf(USGS_ID)

    if (!file.exists(gauge_fn)) {
      dir.create("data/usgs", showWarnings = F)

      dataRetrieval::readNWISdata(
        sites = USGS_ID,
        service = "dv",
        parameterCd = "00060",
        asDateTime = FALSE,
        startDate = "2001-01-01",
        endDate = sprintf("%s-12-31", end_year)
      ) -> data_dl
      data_dl %>%
        as_tibble() %>%
        select(
          av_flow_cfs = X_00060_00003,
          date = dateTime
        ) %>%
        mutate(day = day(date), month = month(date), year = year(date)) %>%
        group_by(day, month, year) %>%
        summarise(
          av_flow_cfs = mean(av_flow_cfs, na.rm = T),
          .groups = "drop"
        ) %>%
        mutate(HUC4 = as.character(!!HUC4), USGS_ID = !!USGS_ID) -> gauge_flow
      write_csv(gauge_flow, gauge_fn, progress = F)
    } else {
      gauge_flow <- read_csv(
        gauge_fn,
        progress = F,
        show = F,
        col_types = "ddddcc"
      )
    }
    gauge_flow
  }) -> HUC4_average_flows

HUC4_average_flows %>%
  mutate(HUC4 = if_else(nchar(HUC4) == 3, paste0(0, HUC4), HUC4)) -> all_flows

# impute missing flow data to create complete set
all_flows |>
  distinct(year, month, day, HUC4, USGS_ID, .keep_all = T) |>
  pivot_wider(
    id_cols = c(year, month, day),
    names_from = c(HUC4, USGS_ID),
    values_from = av_flow_cfs
  ) -> all_flows_wide

all_flows_wide |>
  select(year, month, day) -> ymd_label

all_flows_wide |>
  select(-year, -month, -day) |>
  missRanger() -> all_flow_wide_imputed

ymd_label |>
  bind_cols(all_flow_wide_imputed) |>
  pivot_longer(
    -c(year, month, day),
    names_to = c("HUC4", "USGS_ID"),
    names_sep = "_",
    values_to = "av_flow_cfs"
  ) |>
  arrange(HUC4, year, month, day) -> all_flow_imputed

all_flow_imputed |> write_csv("data/HUC4_average_flows_imputed.csv")
