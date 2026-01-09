library(sf)
library(tidyverse)

eha_fn <- '/Volumes/data/shapefiles/9505-dams/EHA2021_9505pod_v1.0.xlsx'
# eha_fn = 'data/ORNL_EHAHydroPlant_PublicFY2024.xlsx'

eha_pod_fn <- '/Volumes/data/shapefiles/9505-dams/Plant_operational_ExternalGISFY2021_9505pod_v1.0.shp'

eha_pod = eha_pod_fn |> st_read() |> filter(!is.na(EIA_PID))
eha_ll = cbind(eha_pod |> st_coordinates(), eha_pod |> pull(EIA_PID)) |>
  as_tibble() |>
  rename_all(~ c('lon', 'lat', 'EIA_ID'))

# HS = readxl::read_xlsx(eha_fn, sheet = "Operational") %>%
#   select(
#     EHA_PtID,
#     plant = PtName,
#     EIA_ID = EIA_PtID,
#     nameplate_MW = CH_MW,
#     BA = BACode,
#     lon=Lon, lat=Lat
#   )

huc4_sf = read_sf('/Volumes/data/shapefiles/HUC4/HUC4.shp')
huc4_sf_pl = st_transform(huc4_sf, 9311)
dsf = st_transform(st_as_sf(eha_ll, coords = c("lon", "lat"), crs = 4326), 9311)
int = st_intersects(dsf, huc4_sf_pl)
huc4_ind = sapply(int, function(x) {
  ifelse(length(x) > 0, x, NA)
})
eha_ll$HUC4 = huc4_sf$huc4[huc4_ind]

eha_ll |>
  select(EIA_ID, HUC4) |>
  janitor::clean_names() |>
  distinct_all() |>
  write_csv('data/eia_huc4.csv')
