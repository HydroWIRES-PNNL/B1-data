library(tidyverse)
library(ggthemes)

options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1e6,
  dplyr.summarise.inform = FALSE
)

b1_monthly <- "~/Downloads/B1_data/B1_MONTHLY/" |>
  list.files("*", full.names = T) |>
  map(read_csv) |>
  bind_rows() |>
  mutate(
    datetime = fast_strptime(sprintf("%s-%s-01", year, month), "%Y-%b-%d") |> as.Date()
  ) |>
  rename_all(tolower) |>
  mutate(power_mwh = target_mwh) |>
  # all the values for this plant are zero
  group_by(plant) |>
  # don't include plants with all zero gen
  filter(!length(unique(power_mwh)) == 1) |>
  ungroup()

b1_weekly <- "~/Downloads/B1_data/B1_WEEKLY/" |>
  list.files("*", full.names = T) |>
  map(read_csv) |>
  bind_rows() |>
  mutate(datetime = week_start) |>
  rename_all(tolower) |>
  mutate(power_mwh = target_mwh) |>
  # all the values for this plant are zero
  group_by(plant) |>
  # don't include plants with all zero gen
  filter(!length(unique(power_mwh)) == 1) |>
  ungroup()

b1_weekly_west <- b1_weekly |>
  filter(western) |>
  group_by(datetime) |>
  summarise(
    p_avg = sum(p_avg),
    energy_mwh = sum(target_mwh)
  )
b1_monthly_west <- b1_monthly |>
  filter(western) |>
  group_by(datetime) |>
  summarise(p_avg = sum(p_avg))

b1_west <- bind_rows(
  b1_weekly_west |> mutate(type = "weekly"),
  b1_monthly_west |> mutate(type = "monthly"),
  b1_monthly_west |>
    mutate(type = "monthly") |>
    filter(month(datetime) == 12) |>
    mutate(datetime = datetime + months(1) - days(1))
)

y <- 2015
ggplot() +
  geom_step(
    aes(datetime, p_avg / 1000, color = type),
    data = b1_west |> filter(year(datetime) == y)
  ) +
  theme_bw() +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  theme(panel.grid.minor = element_blank()) +
  labs(x = y, y = "Average Generation [GW]") +
  scale_color_discrete("")

y <- 2015
ggplot() +
  geom_step(
    aes(datetime, p_avg / 1000, color = type),
    data = b1_west |> filter(year(datetime) == y)
  ) +
  theme_bw() +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  theme(panel.grid.minor = element_blank()) +
  labs(x = y, y = "Average Generation [GW]") +
  scale_color_discrete("")


b1_weekly_west |>
  mutate(year = year(datetime)) |>
  filter(year %in% c(2001, 2009)) |>
  mutate(datetime = ymd(sprintf("2000-%s-%s", month(datetime), day(datetime)))) |>
  ggplot() +
  geom_bar(
    aes(datetime, energy_mwh / 1000, fill = factor(year), group = year),
    stat = "identity",
    position = "dodge"
  ) +
  theme_bw() +
  theme(legend.position = 'top') +
  scale_fill_manual('', values = colorblind_pal()(8)[2:3]) +
  scale_x_date(date_labels = '%b', date_breaks = 'month') +
  labs(x = '', y = 'Energy [GWh]')
