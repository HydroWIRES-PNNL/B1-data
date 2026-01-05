## 2a. Prepare data to disaggregate annual hydropower using reservoir releases from ResOpsUS
## Author: Sean Turner sean.turner@pnnl.gov
## Hydrosource (HILLARI) data downloaded from https://doi.org/10.21951/hillari/1781642 (2022-03-17)
## 2023 1.2.1 Update - Cameron Bracken cameron.bracken@pnnl.gov
## 2024 1.4.0 Update - Cameron Bracken cameron.bracken@pnnl.gov

# %%
library(starfit) # read ResOpsUS data
library(tidyverse)
library(missRanger)
library(sf)
library(readxl)
# library(janitor)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6
)

# %% #settings
# resops has some data in 2021 but for the most part it ends in 2020
# anything later will be imputed
output_years <- 1980:2024

# get
eha = read_xlsx('data/ORNL_EHAHydroPlant_PublicFY2024.xlsx')
hillari = read_csv('data/hillarri_v3.csv')
grand_to_eha <- hillari |> select(eha_ptid, grand_id)
grand_in_ResOpsUS <-
  list.files("data/ResOpsUS/time_series_all/") %>%
  substr(., 10, nchar(.) - 4) %>%
  as.integer()

# %%
# pull in resops data
message('Reading ResOps data.')
resops_release_data <-
  grand_to_eha %>%
  pull(grand_id) %>%
  unique() %>%
  map(
    function(grand) {
      if (!grand %in% grand_in_ResOpsUS) {
        return(tibble())
      }

      read_reservoir_data(USRDATS_path = "Data/ResOpsUS", dam_id = grand) %>%
        mutate(
          year = year(date),
          month = month(date) # ,label = T)
        ) %>%
        filter(year %in% output_years) %>%
        mutate(r_cumecs_Q90 = quantile(r_cumecs, 0.9, na.rm = T)) %>%
        # TODO find a better way to do this
        mutate(
          r_cumecs_capped = if_else(
            r_cumecs > r_cumecs_Q90,
            r_cumecs_Q90,
            r_cumecs
          )
        ) %>%
        mutate(
          r_cumecs_capped = ifelse(r_cumecs_capped < 0, 0, r_cumecs_capped)
        ) %>%
        group_by(year, month) %>%
        summarise(
          av_release_cumecs = mean(r_cumecs_capped, na.rm = T),
          .groups = "drop"
        ) -> release_data

      expand.grid(
        month = 1:12, # ordered(month.abb, levels = month.abb),
        year = output_years
      ) %>%
        left_join(
          release_data,
          by = c("month", "year")
        ) -> release_data_full_period

      # filter for cases with significant gaps (< 5 years of data)
      if (release_data_full_period |> na.omit() |> nrow() < 12 * 5) {
        return(tibble())
      }

      return(release_data_full_period |> mutate(grand_id = !!grand))
    },
    .progress = TRUE
  ) |>
  bind_rows()

# %%
# impute missing resops data
message('Filling missing values.')
resops_imputed_fn = 'data/resops_imputed_%s_to_%s.csv' |>
  sprintf(first(output_years), last(output_years))

if (!file.exists(resops_imputed_fn)) {
  resops_release_fraction_filled = resops_release_data |>
    pivot_wider(
      id_cols = c(month, year),
      names_from = grand_id,
      values_from = av_release_cumecs
    ) |>
    missRanger() |>
    pivot_longer(
      -c(year, month),
      names_to = "grand_id",
      values_to = "fraction"
    ) |>
    group_by(grand_id, year) |>
    mutate(fraction = fraction / sum(fraction)) |>
    na.omit() |>
    left_join(
      resops_release_data |>
        filter(is.na(av_release_cumecs)) |>
        select(-av_release_cumecs) |>
        mutate(
          imputed = TRUE,
          grand_id = as.character(grand_id)
        ),
      by = join_by(month, year, grand_id)
    ) |>
    ungroup() |>
    left_join(
      hillari |>
        mutate(grand_id = as.character(grand_id)) |>
        distinct(grand_id, .keep_all = T),
      by = "grand_id"
    ) |>
    select(eha_ptid, year, month, fraction, imputed)

  resops_release_fraction_filled |> write_csv(resops_imputed_fn)
} else {
  resops_release_fraction_filled = read_csv(resops_imputed_fn)
}

# %%
# plot to show where data was imputed
message('Creating plots.')
p_resops <- resops_release_fraction_filled |>
  mutate(
    date = ISOdate(year, month, 1),
    fraction_not_imputed = ifelse(imputed, fraction, NA)
  ) |>
  ggplot() +
  geom_line(aes(date, fraction)) +
  geom_line(
    aes(date, fraction_not_imputed),
    color = "red"
  ) +
  facet_wrap(~eha_ptid) +
  geom_text(
    aes(label = eha_ptid),
    data = resops_release_fraction_filled |> distinct(eha_ptid),
    x = ISOdate(2000, 01, 1),
    y = 0.6,
    hjust = 'center',
    vjust = 'center',
    size = 2
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    strip.background = element_blank()
  ) +
  scale_x_datetime(date_labels = "%y") +
  labs(x = "Year", y = "Release Fraction")
p_resops
plot_fn = "figures/resops.png"
ggsave(plot_fn, p_resops, width = 16, height = 12)
message('Created: ', plot_fn)

# %%
# pull in gage based release data
message('Creating gage and release based fractions.')
gage_flow_fraction <- "data/flow/proc/flow_all_eia_hydro_long_%s-01-01_to_%s-12-31.csv" |>
  sprintf(first(output_years), last(output_years)) |>
  read_csv() |>
  janitor::clean_names() |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(eia_id, year, month) |>
  # average monthly flows
  summarise(value = mean(value, na.rm = T), .groups = "drop") |>
  group_by(eia_id, year) |>
  mutate(
    fraction = value / sum(value),
    # the fraction will be NaN in years where all the values are 0,
    # so replace those fractions with zero, its not many points
    fraction = ifelse(is.na(fraction), 0, fraction)
  ) |>
  ungroup() %>%
  left_join(
    # There are multiple eia_id's for some eha_ptid's because some
    # some eia plants are split, like hoover which is in two states,
    # There is also one eha_ptid that represents two eia_id's
    # but exclude the second one since represents the same plant
    hillari |> rename(eia_id = eia_ptid) |> distinct(eia_id, .keep_all = T),
    by = "eia_id"
  ) %>%
  select(eha_ptid, year, month, fraction)


# only use resops if gage based fraction is not available
release_based_fractions <- gage_flow_fraction |>
  full_join(
    resops_release_fraction_filled |> select(-imputed),
    by = join_by(eha_ptid, year, month),
    suffix = c('_gage', '_resops')
  ) |>
  mutate(
    fraction = case_when(
      !is.na(fraction_gage) ~ fraction_gage,
      !is.na(fraction_resops) ~ fraction_resops,
      .default = NA
    )
  ) |>
  select(-fraction_resops, -fraction_resops) |>
  group_by(eha_ptid, year) |>
  # fix for a year of all 0 fractions
  mutate(
    fraction = if_else(fraction == 0, 0.005, fraction),
    fraction = fraction / sum(fraction),
    month = month.abb[month]
  )


release_based_fractions %>%
  write_csv("data/release_based_fractions.csv")
