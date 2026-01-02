library(conflicted)
conflicts_prefer(dplyr::filter)
library(tidyverse)
library(tools)
library(semver)
library(janitor)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

data_dir = 'data'
current_version_dir = 'B1_data_1.4.0/'
current_version_number = '1.4.0'

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

b1_versions = bind_rows(
  previous_versions,
  data.frame(path = current_version_dir, version = current_version_number)
)

download_unzip_rename = function(
  url,
  zip_fn,
  exdir = '.',
  dir_rename_to = NULL,
  delete_zip = FALSE,
  cache = TRUE,
  download_method = 'wget',
  overwrite = TRUE,
  ...
) {
  zip_path = file.path(exdir, zip_fn)
  if (cache & file.exists(zip_path)) {
    message(
      'download_unzip_rename: ',
      zip_path,
      ' exists, not downloading again, set cache=FALSE to re-download.'
    )
  } else {
    download.file(
      url,
      zip_path,
      method = download_method,
      overwrite = overwrite
    ) # might fail if not installed
  }
  output_dir_name = unzip(zip_path, list = T)$Name[1]
  unzip(zip_path, exdir = exdir)
  if (!is.null(dir_rename_to)) {
    unzipped_output_dir = file.path(exdir, output_dir_name)
    new_unzipped_output_dir = file.path(exdir, dir_rename_to)
    if (file.exists(new_unzipped_output_dir) & overwrite) {
      unlink(new_unzipped_output_dir, recursive = T)
    }
    file.rename(unzipped_output_dir, new_unzipped_output_dir)
  }
  if (delete_zip) {
    unlink(zip_path)
  }
}

# download previous version data and unzip it
previous_versions |> pmap(download_unzip_rename) -> shh

read_b1 = function(path, version, timestep = 'monthly', ...) {
  #
  message('Reading B1 data version ', version, ' ', timestep)

  sv = parse_version(version)
  if (sv < parse_version('1.4.0')) {
    # multiple files, one per year
    subdir = list.files(path, full.names = TRUE) |>
      str_subset(regex(timestep, ignore_case = TRUE))
    # browser()
    subdir |>
      list.files('*', full.names = T) |>
      map(read_csv, .progress = TRUE) |>
      bind_rows() %>%
      {
        # older data did not have a consistent datetime column name
        if (timestep == 'weekly') {
          mutate(., datetime = week_start)
        } else if (timestep == 'monthly') {
          mutate(., datetime = ymd(sprintf('%s-%s-01', year, month)))
        } else {
          stop("Timestep must be 'monthly' or 'weekly'.")
        }
      } |>
      janitor::clean_names(parsing_option = 3) |>
      mutate(version = version)
  } else {
    fn = list.files(path, full.names = TRUE) |>
      str_subset(regex(timestep, ignore_case = TRUE))
    # one file for enture period
    read_csv(fn) |>
      mutate(version = version)
  }
}


b1m = b1_versions |> pmap(read_b1, timestep = 'monthly') |> bind_rows()
b1w = b1_versions |> pmap(read_b1, timestep = 'weekly') |> bind_rows()


plot_one_plant = function(id, data) {
  plant_name = data |> filter(eia_id == id) |> pull(plant) |> unique()
  data |>
    filter(eia_id == id) |>
    ggplot() +
    geom_line(aes(datetime, target_mwh, color = factor(version))) +
    theme_minimal() +
    ggthemes::scale_color_colorblind() +
    labs(title = paste(plant_name, id), color = 'B1 Data\nVersion')
}
plot_one_plant(b1m$eia_id |> sample(1), b1m)

pdf('B1_version_compare_monthly.pdf', 6, 4, onefile = TRUE)
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

pdf('B1_version_compare_weekly.pdf', 6, 4, onefile = TRUE)
shhh = b1w$eia_id |>
  unique() |>
  sort() |>
  map(
    function(id) {
      plot_one_plant(id, b1m) |> print()
    },
    .progress = TRUE
  )
dev.off()

problem_plants = c(31)
