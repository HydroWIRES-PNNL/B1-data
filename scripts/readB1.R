library(tidyverse)

version <- "1.3"
path <- "~/Downloads/B1_data"
timestep <- "monthly"

subdir <- list.files(path, full.names = TRUE) |>
  str_subset(regex(timestep, ignore_case = TRUE))

subdir |>
  list.files("*", full.names = T) |>
  map(\(x) read_csv(x, show_col_types = F, progress = F), .progress = TRUE) |>
  bind_rows() |>
  mutate(version = version)
