# 6_validation.R
#
#
#
# Created by Cameron Bracken, Jan 10, 2026

# ----------------------------------------------------------------------------
# setup ----------------------------------------------------------------------
# ----------------------------------------------------------------------------
# %% packages and utility functions
# load common packages and options
source('packages_and_options.R')
source('utilities.R')

# %% read data
reader = ifelse(output_format == 'csv', read_csv, read_parquet)
b1_monthly = file.path(b1_dir, s('B1_monthly.{output_format}')) |> reader()
b1_weekly = file.path(b1_dir, s('B1_weekly.{output_format}')) |> reader()
b1_metadata = file.path(b1_dir, s('B1_metadata.{output_format}')) |> reader()
pnw_dam_data = read_csv('data/usace_dam_data_daily_1980_2024.csv') |> mutate(eia_id1 = eia_id)

conds = c(
  "p_min < 0",
  "p_min > p_max",
  "p_min > (p_max - ador)",
  "p_min > p_ave",
  "p_max < p_ave - .001",
  "p_max < p_min",
  "ador < 0",
  "ador < 0"
)

nrows_after_condition = function(x, cond) {
  x |> filter(!!rlang::parse_expr(cond)) |> nrow()
}
print_nrows_after_condition = function(x, cond) {
  nr = nrows_after_condition(x, cond)
  nr_print = ifelse(nr > 0, red(nr), green(nr))
  cond_print = blue(cond)
  message(s('Number of rows where {cond_print}: {nr_print}'))
}

# %% monthly
# perform basic checks

message('----------------------')
message('B1 Monthly diagnostics')
message('----------------------')


# these should all return zero rows
conds |>
  walk(\(cond) {
    print_nrows_after_condition(b1_monthly, cond)
  })


# monthly plots
wecc_monthly_2001_2009 =
  b1_monthly |>
  filter(western) |>
  mutate(year = year(datetime), month = month(datetime)) |>
  group_by(month, year) |>
  summarise(energy_mwh = sum(target_mwh, na.rm = T), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  ggplot(aes(month, energy_mwh / 1000, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("orange", "cornflowerblue")) +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Energy [GWh]")

# b1_monthly |>
#   filter(western) |>
#   group_by(year, month) |>
#   summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
#   filter(year %in% c(2001, 2009)) |>
#   pivot_wider(id_cols = month, names_from = year, values_from = energy_mwh) |>
#   mutate(pct_diff = (`2001` - `2009`) / `2009` * 100)

# b1_annual =
#   b1_monthly |>
#   # aggregate to annual if possible, but split the years if the op status changed
#   group_by(eia_id, year, operational_status) |>
#   reframe(
#     datetime = datetime[1],
#     net_gen_mwh = agg_na_rm_unless_all_na(target_mwh, sum),
#     nameplate_mw = agg_na_rm_unless_all_na(nameplate_mw, max),
#     n_hours = sum(n_hours),
#     operational_status = last(operational_status)
#   ) |>
#   mutate(annual_cf = net_gen_mwh / (nameplate_mw * n_hours)) |>
#   left_join(b1_metadata, by = join_by(eia_id, year, nameplate_mw, operational_status, datetime))

# TODO fix unknown source
p_sources_m =
  b1_monthly |>
  mutate(data_source = factor(data_source)) |>
  group_by(datetime, data_source) |>
  count(.drop = FALSE) |>
  ggplot() +
  geom_area(aes(datetime, n, fill = data_source)) +
  theme_minimal() +
  scale_fill_paletteer_d("pals::kelly", direction = -1, name = 'Data Source') +
  labs(y = 'Number of hydropower plants')
print(p_sources_m)
ggsave(s('{figures_dir}/data_source_count_monthly.pdf'), p_sources_m, width = 7, height = 3)
ggsave(s('{figures_dir}/data_source_count_monthly.png'), p_sources_m, width = 7, height = 3)

# %% weekly
message('---------------------')
message('B1 Weekly diagnostics')
message('---------------------')

# these should all return zero rows
conds |>
  walk(\(cond) {
    print_nrows_after_condition(b1_weekly, cond)
  })

wecc_weekly_2001_2009 =
  b1_weekly |>
  filter(western == TRUE) |>
  group_by(year, week_start) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  mutate(week_start = `year<-`(week_start, 2000)) |>
  filter(week_start < as.Date("2000-12-31")) |>
  ggplot(aes(week_start, energy_mwh / 1000, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("", values = c("orange", "cornflowerblue")) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Energy [GWh]")
print(wecc_weekly_2001_2009)
ggsave(s('{figures_dir}/wecc_total_2001_2009.pdf'), width = 7, height = 5)

# # TODO fix unknown source
# p_sources_w =
#   b1_weekly |>
#   filter(year == 200) |>
#   ggplot() +
#   geom_bar(aes(week_start, fill = data_source), ) +
#   scale_fill_manual(values = colourblind_pal()(8)[-1][c(1:5, 7, 6)]) +
#   theme_minimal() +
#   labs(title = 'Data sources by month', y = 'Number of plants', x = 'Date')

# p_sources_w =
#   b1_weekly |>
#   filter(year == 200) |>
#   group_by(datetime, data_source) |>
#   summarise(n = n()) |>
#   ggplot() +
#   geom_area(aes(datetime, n, fill = data_source)) +
#   theme_minimal() +
#   scale_fill_paletteer_d("pals::kelly", direction = -1, name = 'Data Source') +
#   labs(y = 'Number of gages')
# print(p_sources_w)
# ggsave(s('{figures_dir}/data_source_count_weekly.pdf'), width = 7, height = 5)

# difference between a wet and dry year
b1_weekly |>
  filter(western == TRUE) |>
  group_by(year, week_start) |>
  summarise(energy_mwh = sum(target_mwh), .groups = "drop") |>
  filter(year %in% c(2001, 2009)) |>
  mutate(week_start = `year<-`(week_start, 2000)) |>
  pivot_wider(id_cols = week_start, names_from = year, values_from = energy_mwh) |>
  mutate(pct_diff = (`2001` - `2009`) / `2009` * 100) |>
  print(n = 100)


# %% weekly vs monthly
message('-----------------------')
message('B1 combined diagnostics')
message('-----------------------')


b1 = bind_rows(
  b1_monthly |> mutate(timestep = 'monthly'),
  b1_weekly |> mutate(timestep = 'weekly')
)

# TODO find oput why there is a diffrence, they should be identical
p_val_monthly_vs_weekly =
  b1 |>
  filter(western) |>
  group_by(timestep, year) |>
  summarise(energy_twh = sum(target_mwh, na.rm = T) / 1000000, .groups = "drop") |>
  ggplot(aes(year, energy_twh, color = timestep)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  scale_color_colorblind()

# %% compare to eia observations where available

eia_monthly = read_parquet('data/out_eia__monthly_generators.parquet') |>
  select(datetime = report_date, eia_id1 = plant_id_eia, generator_id, net_generation_mwh) |>
  group_by(datetime, eia_id1) |>
  summarise(eia_mwh = sum(net_generation_mwh, na.rm = T))

# eia_annual = read_parquet('data/out_eia923__generation.parquet') |>
#   mutate(year = year(report_date)) |>
#   select(year, eia_id1 = plant_id_eia, generator_id, net_generation_mwh) |>
#   group_by(year, eia_id1) |>
#   summarise(eia_mwh = sum(net_generation_mwh, na.rm = T))

b1_annual_total_from_monthly_with_eia =
  b1_monthly |>
  left_join(eia_monthly, by = join_by(datetime, eia_id1)) |>
  drop_na(eia_mwh) |>
  group_by(year) |>
  summarise(
    b1_twh = sum(target_mwh, na.rm = T) / 1000000,
    eia_twh = sum(eia_mwh, na.rm = T) / 1000000
  )

# TODO find out why there is a bias
p_val_annual_b1_vs_eia =
  b1_annual_total_from_monthly_with_eia |>
  pivot_longer(
    -c(year),
    names_to = c('dataset'),
    values_to = 'energy_twh'
  ) |>
  ggplot(aes(year, energy_twh, linetype = dataset, color = dataset)) +
  geom_line() +
  geom_point() +
  scale_color_paletteer_d("pals::kelly", direction = -1, name = 'Data Source') +
  scale_linetype_discrete(name = 'Data Source') +
  theme_classic() +
  theme(legend.position = 'inside', legend.position.inside = c(.9, .85))
