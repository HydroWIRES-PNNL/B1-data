# 0c-get-misc-data.R
#
# Created by <name>, Jan 02, 2026
#
#

# %%
library(conflicted)
library(tidyverse)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)


# %%
# Include some manual updates to hillari
# https://code.ornl.gov/turnersw/rectifhydplus/-/raw/main/data/misc/hillari_changes.csv
download.file(
  'https://code.ornl.gov/turnersw/rectifhydplus/-/raw/main/data/misc/HILARRI_changes.csv?ref_type=heads&inline=false',
  'data/hillarri_changes.csv'
)
hillari_changes = 'data/hillarri_changes.csv'

hillari = st_read(hillari_fn) |>
  select(-geom) |>
  left_join(
    read_csv(hillari_changes, col_types = "c"),
    by = "eia_ptid"
  ) |>
  mutate(
    usgs_gage = if_else(!is.na(usgs_gage_new), usgs_gage_new, usgs_gage)
  ) |>
  select(-usgs_gage_new) |>
  as_tibble() |>
  filter(!is.na(eia_ptid), prjct_type == 'Conventional hydropower')

write_csv(hillari, 'data/hillarri_v3.csv')
