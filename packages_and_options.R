# packages_and_options.R
#
# Global options and packages for the B1 data scripts
#
# Created by Cameron Bracken, cameron.bracken@pnnl.gov, Jan 06, 2026

library(conflicted)

# %% renv
## initial setup
# install.packages('renv')
# renv::init()
# vignette("renv")
# renv::snapshot()
library(renv)

# %% packages - load conflicted first to avoid warnings
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
library(janitor)
library(glue)
# shortcut for string interpolation like pythins f strings
s = glue::glue
library(arrow)
library(missRanger)
library(dataRetrieval)
library(cder)
library(sf)
library(starfit)
library(readxl)
library(tools)
# needed by rfp
library(foreign)
library(zoo)

# %% R options
options(
  readr.show_progress = FALSE,
  readr.show_col_types = FALSE,
  pillar.width = 1000,
  dplyr.summarise.inform = FALSE
)

# %% settings
# directory for all data
data_dir = config::get('data_dir')
# subdirectories
figures_dir = config::get('figures_dir')
eia_flow_dir = config::get('eia_flow_dir')
resops_flow_dir = config::get('resops_data_dir')
cache = config::get('cache')

# cutoff for excluding low capacity plant, set to zero to include everything
# needed for calling rfp target plant functions, not used by the B1 code
capacity_cutoff_MW <- 0

# output will be filtered to these years
# TODO add filtering everywhere
start_year = config::get('start_year') #min(year_range)
end_year = config::get('end_year') #max(year_range)
year_range = start_year:end_year
datetime_sequence_a = seq.Date(s('{start_year}-01-01'), s('{end_year}-12-01'), by = 'year')
datetime_sequence_m = seq.Date(s('{start_year}-01-01'), s('{end_year}-12-01'), by = 'month')


# %% set up paths and data source versions
rfp_version = config::get('rfp_version')
rf_version = config::get('rf_version')

rfp_code_dir =
  if (config::get('rfp_code_checkout_repo')) {
    config::get('rfp_code_dir') |> paste0('_main')
  } else {
    config::get('rfp_code_dir') |> paste0('_', rfp_version)
  }
rfp_supl_dir = config::get('rfp_supl_dir') |> paste0('_', rfp_version)
rfp_misc_dir = config::get('rfp_misc_dir') |> paste0('_', rfp_version)
rfp_data_dir = config::get('rfp_data_dir') |> paste0('_', rfp_version)
rfp_inputs_dir = config::get('rfp_inputs_dir') |> paste0('_', rfp_version)

# %%
# file names used across mutiple scripts
hilarri_fn = file.path(data_dir, config::get('hilarri_csv'))
release_fractions_fn = s("{data_dir}/release_based_fractions.csv")
huc4_flow_imputed_monthly_fn = s("{data_dir}/huc4_flow_usgs_long_imputed.csv")
huc4_flow_imputed_monthly_wide_fn = 'data/huc4_flow_usgs_monthly_wide_imputed.csv'
huc4_flow_imputed_daily_fn = s("{data_dir}/huc4_flow_usgs_daily_long_imputed.csv")
huc4_flow_imputed_daily_wide_fn = 'data/huc4_flow_usgs_daily_wide_imputed.csv'
huc4_fractions_fn = s("{data_dir}/huc4_based_fractions.csv")
release_fractions_fn = s("{data_dir}/release_based_fractions.csv")
resops_imputed_fn = s("{data_dir}/resops_imputed_{start_year}_{end_year}.csv")
gage_flow_eia_fn = s("{eia_flow_dir}/proc/flow_all_eia_hydro_long_{start_year}_to_{end_year}.csv")
pudl_generators_fn = s("{data_dir}/out_eia__monthly_generators.parquet")
pudl_generators_annual_fn = s("{data_dir}/out_eia__yearly_generators.parquet")
pudl_plants_fn = s("{data_dir}/out_eia__yearly_plants.parquet")
rfp_data_fn = file.path(data_dir, s('rectifhyd_{as.character(rfp_version)}.csv'))
hydro_gen_data_wide_fn =
  s('{data_dir}/hydro_gen_multisource_monthly_{start_year}_{end_year}_wide.csv')
hydro_gen_data_long_fn =
  s('{data_dir}/hydro_gen_multisource_monthly_{start_year}_{end_year}_long.csv')
hydro_gen_with_disag_imputed_fn = s('{data_dir}/hydro_gen_with_disag_imputed.csv')
annual_p_ave_imputed_fn = s('{data_dir}/annual_p_ave_imputed.csv')
flow_based_disag_fn = s('{data_dir}/flow_based_proxy_disag_proportions.csv')

# to shut up the syntax linter, . is from the magrittr package but not defined globally
. = NULL

# %% data tags with version numbers
rf_version_tag = rf_version |>
  as.character() %>%
  # str_replace(fixed('.'), '_') %>%
  s('rf_', .)
rfp_version_tag = rfp_version |>
  as.character() %>%
  # str_replace(fixed('.'), '_') %>%
  s('rfp_', .)

# source global functions
source('utilities.R')

# for downloading usgs data
check_env_set_usgs_api_key()
