# scripts/compare_versions.R
#
#
#
# Created by Cameron Bracken, Jan 12, 2026

# ----------------------------------------------------------------------------
# setup ----------------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% packages - load conflicted first to avoid warnings
library(conflicted)
conflicts_prefer(dplyr::filter)
library(tidyverse)
library(tools)
library(janitor)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

source('utilities.R')


# %% options

data_dir = 'data'

interim_version_dir = 'data/B1_data_1.4.0/'
interim_version_number = '1.4.0'
current_version_dir = 'B1_data_1.5.0/'
current_version_number = '1.5.0'

previous_versions = tribble(
  ~url                                                    , ~zip_fn             ,
  'https://zenodo.org/records/8408246/files/B1_data.zip'  , 'B1_data_1.1.2.zip' ,
  'https://zenodo.org/records/10574003/files/B1_data.zip' , 'B1_data_1.2.0.zip' ,
  'https://zenodo.org/records/13351949/files/B1_data.zip' , 'B1_data_1.3.0.zip'
) |>
  mutate(
    dir_rename_to = zip_fn |> basename() |> tools::file_path_sans_ext(),
    version = zip_fn |> file_path_sans_ext() |> str_split_i('_', 3),
    exdir = 'data',
    path = file.path(exdir, dir_rename_to)
  )

# %% download and read data

b1_versions = bind_rows(
  previous_versions,
  data.frame(path = interim_version_dir, version = interim_version_number),
  data.frame(path = current_version_dir, version = current_version_number)
)

# download previous version data and unzip it
previous_versions |> pmap(download_unzip_rename_orig) -> shh

b1m = b1_versions |> pmap_dfr(read_b1, timestep = 'monthly')
b1w = b1_versions |> pmap_dfr(read_b1, timestep = 'weekly')


# %% plot
plot_one_plant = function(id, data, version_colors = NULL) {
  plant_name = data |> filter(eia_id == id) |> pull(plant) |> unique()
  if (is.null(version_colors)) {
    versions = unique(data$version)
    version_colors = ggthemes::colorblind_pal()(8)[1:length(versions) + 1]
    names(version_colors) = versions
  }
  p = data |>
    filter(eia_id == id) |>
    ggplot() +
    geom_line(aes(datetime, target_mwh, color = factor(version))) +
    theme_minimal() +
    scale_color_manual(values = version_colors) +
    labs(title = paste(plant_name, id), color = 'B1 Data\nVersion')
  return(p)
}
plot_one_plant(b1m$eia_id |> sample(1), b1m)

pdf('figures/B1_version_compare_monthly.pdf', 6, 4, onefile = TRUE)
shhh = b1m$eia_id |>
  unique() |>
  sort() |>
  map(
    function(id) {
      plot_one_plant(id, b1m) |> print()
    },
    .progress = TRUE
  )
dev.off()

pdf('figures/B1_version_compare_weekly.pdf', 6, 4, onefile = TRUE)
shhh = b1w$eia_id |>
  unique() |>
  sort() |>
  map(
    function(id) {
      plot_one_plant(id, b1w) |> print()
    },
    .progress = TRUE
  )
dev.off()

problem_plants = c(31)
