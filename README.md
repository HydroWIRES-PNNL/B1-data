# HydroWIRES B1-data v1.4.0: Monthly and Weekly Hydropower Generation and Constraints for the United States

**Authors:** Cameron Bracken (PNNL), Daniel Broman (PNNL), Nathalie Voisin (PNNL)

**Corresponding author:** cameron.bracken@pnnl.gov

- [Overview](#overview)
- [New in version 1.4.0](#new-in-version-140)
- [Dataset columns](#dataset-columns)
- [Methodology](#methodology)
- [Steps to reproduce](#steps-to-reproduce)
- [Required input data](#required-input-data)
- [Processing pipeline](#processing-pipeline)
- [Output directory structure](#output-directory-structure)
- [R package dependencies](#r-package-dependencies)
- [Related datasets and resources](#related-datasets-and-resources)
- [License](#license)
- [Citation](#citation)

## Overview

The B1 dataset provides both monthly and weekly hydropower generation targets and operational constraints (maximum and minimum generation, average daily operating range) for over 1,500 hydropower plants across the United States. The data is intended for use in energy system models including Production Cost Models (PCMs), Capacity Expansion Models (CEMs), and Resource Adequacy (RA) models.

The dataset is built on the [RectifHyd v1.4.0](#rectifhyd) monthly generation estimates, which disaggregate annual EIA-923 data to monthly resolution using observed streamflow. The B1-data pipeline extends this by:

1. **Adding operational constraints** -- Deriving p_min, p_max, and ADOR parameters from 28 Pacific Northwest hydropower plants with high-quality hourly USACE generation data, then applying these parameters to all plants nationwide
2. **Weekly disaggregation** -- Breaking monthly generation targets into 53-week resolution using daily streamflow patterns
3. **Joining supplementary data** -- HUC4 watershed flows, PNW dam operational data (forebay, inflow, outflow), and plant metadata

**Coverage:** 2001-2024, ~1,500 CONUS hydropower plants (Alaska and Hawaii excluded)

## New in version 1.4.0

- **Updated through 2024**: Extended time series from 2001-2024 using final EIA-923 data
- **Removed hydrofixr dependency**: USACE dam data is now downloaded directly from NWD web services, eliminating the need for the `hydrofixr` R package and its associated data files
- **Code refactor**: Scripts cleaned up for better maintainability and readability
- **Improved validation**: New validation script (6-validation.R) with diagnostic plots and version comparisons
- **Centralized outputs**: All generated files now in `output/` directory
- **Single output files**: Monthly and weekly data combined into one file each (previously split by year)

See the [Changelog](#changelog) section for the full version history.


## Dataset columns

### Monthly dataset (`B1_monthly.csv`)

| Column | Description |
|--------|-------------|
| `eia_id` | EIA plant identifier |
| `plant` | Plant name |
| `state` | State (two-letter abbreviation) |
| `ba` | Balancing authority area |
| `year` | Year |
| `month` | Month name |
| `monthi` | Numeric month index (1-12) |
| `target_mwh` | Energy target for the month (MWh) |
| `nameplate` | Maximum plant capacity (MW) |
| `n_hours` | Number of hours in the month |
| `p_avg` | Average generation (MW) = target_mwh / n_hours |
| `p_min` | Minimum generation constraint (MW) |
| `p_max` | Maximum generation constraint (MW) |
| `ador` | Average Daily Operating Range (MW) |
| `mode` | Operating mode: "Run-of-River" or "Storage" |
| `western` | Boolean: plant is in the Western US |
| `huc4` | 4-digit Hydrologic Unit Code (zero-padded) |
| `usgs_id` | USGS gauge associated with the HUC4 |
| `huc4_flow_cfs` | Monthly mean HUC4 streamflow (cubic feet per second) |
| `forebay_ft` | Forebay elevation in feet (PNW plants only) |
| `inflow_cfs` | Reservoir inflow in cfs (PNW plants only) |
| `outflow_cfs` | Reservoir outflow in cfs (PNW plants only) |

### Weekly dataset (`B1_weekly.csv`)

Same columns as monthly, plus:

| Column | Description |
|--------|-------------|
| `jweek` | Week index (1-53) |
| `week_start` | Start date of the week (YYYY-MM-DD) |
| `n_hours` | Number of hours in the week (typically 168) |


## Methodology

### Constraint parameter derivation

The operational constraints are derived empirically from 28 Pacific Northwest hydropower plants operated by the US Army Corps of Engineers (USACE). These plants have publicly available hourly generation data (2001-2024) that allows direct computation of actual operating parameters.

For each PNW plant, the parameters are computed from the observed hourly power data:

```
max_param = (p_max_observed - p_avg) / (nameplate - p_avg)
min_param = p_min_observed / p_avg
ador_param = ador_observed / (p_max_observed - p_min_observed)
```

These parameters are then averaged by operating mode (Run-of-River vs Storage) and applied to all non-PNW plants:

```
p_max = p_avg + max_param * (nameplate - p_avg)
p_min = min_param * p_avg
ador  = ador_param * (p_max - p_min)
```

The 28 PNW plants retain their plant-specific parameters. All other plants use the mode-averaged parameter values.

### Weekly disaggregation

Monthly energy targets are disaggregated to weekly resolution using daily streamflow as a proxy for daily generation patterns:

1. For each plant, obtain daily flow data from the best available source:
   - **Plant-specific release/gauge data** (preferred) -- from `2-gauge-streamflow.R`
   - **HUC4 watershed flow** (fallback) -- from `1-huc4-streamflow.R`
   - **Flat allocation** (last resort) -- equal daily energy when no flow data is available
2. Compute daily energy allocation: `daily_energy = monthly_target * (daily_flow / monthly_total_flow)`
3. Aggregate daily values into 53 fixed 7-day weeks per year (starting January 1, not ISO 8601 weeks)
4. Apply weekly-based PNW parameters for weekly constraints

### PNW dam list (28 plants)

The following USACE dams in the Pacific Northwest provide hourly generation data used for parameter derivation:

| Dam Code | Plant Name | EIA ID |
|----------|-----------|--------|
| BON | Bonneville | 3075 |
| CHJ | Chief Joseph | 3921 |
| GCL | Grand Coulee | 6163 |
| IHR | Ice Harbor | 3925 |
| JDA | John Day | 3082 |
| LGS | Lower Granite | 3926 |
| LMN | Lower Monumental | 3927 |
| LWG | Little Goose | 6175 |
| MCN | McNary | 3084 |
| PRD | Priest Rapids | 3887 |
| TDA | The Dalles | 3895 |
| LIB | Libby | 6172 |
| ALF | Albeni Falls | 851 |
| BCL | Big Cliff | 3074 |
| CGR | Cougar | 3076 |
| DET | Detroit | 3077 |
| DEX | Dexter | 3078 |
| DWR | Dworshak | 840 |
| FOS | Foster | 6552 |
| GPR | Green Peter | 3080 |
| HCR | Hills Creek | 3081 |
| HGH | Howard Hanson | 2203 |
| LOP | Lookout Point | 3083 |
| LOS | Lost Creek | 6174 |
| RIS | Roza | 6200 |
| RRH | Rocky Reach | 3883 |
| WAN | Wanapum | 3888 |
| WEL | Wells | 3886 |

### Western US classification

Plants in the following states are classified as "western": WA, ID, CO, UT, NM, WY, MT, CA, OR, NV, AZ.


## Steps to reproduce

### 1. Clone this repository

```bash
git clone <repo-url>
cd B1-data-1.4
```

### 2. Download required input data

Place the following files in the `data/` directory (see [Required input data](#required-input-data) for full details):

- **RectifHyd v1.4.0**: `data/RectifHyd_v1.4.0.csv`
- **EHA FY2024 database**: `data/ORNL_EHAHydroPlant_PublicFY2024.xlsx`
- **HILARRI v1.1 database**: `data/HILARRI_v1_1/` (unzipped)
- **EIA-HUC4 crosswalk**: `data/eia_huc4.csv`
- **USGS-HUC4 gauge mapping**: `data/USGS_Streamgage_huc4.csv`
- **Flow-EIA crosswalk**: `data/gauge-inputs/flow_to_EIA_crosswalk.csv`
- **ResOpsUS time series**: `data/ResOpsUS/time_series_all/`

### 3. Install R dependencies

```r
install.packages("renv")
renv::restore()
```

### 4. Run the processing pipeline

Execute the following scripts in order. Each script depends on outputs from previous steps.

```bash
Rscript 1-huc4-streamflow.R       # Download HUC4-level streamflow data
Rscript 2-gauge-streamflow.R       # Download gauge-level streamflow data
Rscript 3-hydropower.R             # Download USACE PNW dam operational data
Rscript 4-pnw-params.R             # Derive PNW constraint parameters
Rscript 5a-B1-monthly.R            # Generate monthly B1 dataset
Rscript 5b-B1-weekly.R             # Generate weekly B1 dataset
Rscript 6-validation.R             # Validate outputs and generate plots
```

All scripts can also be run from within R using `source('script-name.R')`.

### 5. Review outputs

Final datasets will be in `output/B1_data_1.4.0/`. See [Output directory structure](#output-directory-structure).


## Required input data

### Static datasets (downloaded once)

| Dataset | Version | Description | Location in `data/` | Download |
|---------|---------|-------------|---------------------|----------|
| RectifHyd | v1.4.0 | Monthly hydropower generation estimates for ~1,500 plants (2001-2024). This is the primary generation input. | `data/RectifHyd_v1.4.0.csv` | [PNNL DataHub](link) / [Zenodo](https://zenodo.org/records/10011017) |
| EHA | FY2024 | Existing Hydropower Assets database from ORNL. Provides plant metadata including nameplate capacity, operating mode (Run-of-River vs Storage), balancing authority, state, and HUC codes. | `data/ORNL_EHAHydroPlant_PublicFY2024.xlsx` | [HydroSource](https://hydrosource.ornl.gov/data/datasets/eha-capacity-factor-plant-database-2005-2024/) |
| HILARRI | v1.1 | Hydropower Infrastructure - LAkes, Reservoirs, and Rivers database. Links plant IDs to dams, reservoirs, HUC codes, and GRanD IDs. | `data/HILARRI_v1_1/` | [HydroSource](https://hydrosource.ornl.gov/) |
| ResOpsUS | v1.0 | Observed reservoir operations (storage, inflow, outflow) for ~700 US reservoirs. Used for release-based flow proxies. | `data/ResOpsUS/time_series_all/` | [Zenodo](https://doi.org/10.5281/zenodo.5367383) |

### Crosswalk and mapping files (included in repository)

| File | Description |
|------|-------------|
| `data/eia_huc4.csv` | Crosswalk between EIA plant IDs and HUC4 watershed codes |
| `data/USGS_Streamgage_huc4.csv` | Representative USGS gauge for each HUC4 watershed |
| `data/gauge-inputs/flow_to_EIA_crosswalk.csv` | Manually curated mapping of EIA plant IDs to preferred streamflow gauge stations and data sources |

### Data downloaded at runtime

The following data is downloaded automatically by pipeline scripts and cached locally:

| Source | Script | Description | API / Package | Cache Location |
|--------|--------|-------------|---------------|----------------|
| USGS NWIS | 1, 2 | Daily mean discharge from ~250 stream gauges | `dataRetrieval` R package | `data/usgs/`, `data/flow/raw/` |
| California CDEC | 2 | Daily reservoir release/flow for California dams | `cder` R package | `data/flow/raw/` |
| Reclamation RISE | 2 | Daily flow from Bureau of Reclamation facilities | RISE REST API | `data/flow/raw/` |
| Colorado CDSS | 2 | Daily streamflow from Colorado's Decision Support Systems | CDSS REST API | `data/flow/raw/` |
| Reclamation PNH | 2 | Daily flow from Pacific Northwest Hydromet (Columbia Basin) | Hydromet web service | `data/flow/raw/` |
| Reclamation MBH | 2 | Daily flow from Missouri Basin Hydromet | Hydromet web service | `data/flow/raw/` |
| USACE NWD | 2, 3 | Daily/hourly flow and power data from Army Corps Northwestern Division dams | NWD Dataquery web service | `data/flow/raw/`, `data/usace/` |
| ResOpsUS (local) | 2 | Reservoir release time series (read from local files, not downloaded) | CSV files | `data/ResOpsUS/` |


## Processing pipeline

| Step | Script | Description | Key Inputs | Key Outputs |
|------|--------|-------------|------------|-------------|
| 1 | `1-huc4-streamflow.R` | Download USGS daily streamflow from HUC4 representative gauges. Impute missing values across gauges using `missRanger`. | `data/eia_huc4.csv`, `data/USGS_Streamgage_huc4.csv` | `output/huc4_average_flows_imputed.csv` |
| 2 | `2-gauge-streamflow.R` | Download plant-specific flow data from 8 sources (USGS, CDEC, RISE, CDSS, PNH, MBH, NWD, ResOpsUS). Impute gaps with `missRanger`. | `data/gauge-inputs/flow_to_EIA_crosswalk.csv` | `data/flow/proc/flow_all_td.csv`, `data/flow/proc/flow_all_sp.csv` |
| 3 | `3-hydropower.R` | Download USACE hourly dam data (power, forebay, inflow, outflow) for 28 PNW dams from NWD web services. Aggregate to daily. | NWD web services (28 dams) | `output/pnw_hourly_power.csv`, `output/pnw_daily_data.csv` |
| 4 | `4-pnw-params.R` | Analyze PNW hourly power data to derive max/min/ADOR constraint parameters by mode (monthly and weekly). | `output/pnw_hourly_power.csv`, EHA database | `output/PNW_28_max_min_ador_parameters.csv`, `output/PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv`, `output/USACE_weekly_parameters_28.csv` |
| 5a | `5a-B1-monthly.R` | Combine RectifHyd monthly generation with EHA metadata and PNW-derived constraints. Join HUC4 flows and PNW dam data. | `data/RectifHyd_v1.4.0.csv`, EHA, PNW params, HUC4 flows, PNW daily data | `output/B1_data_1.4.0/B1_monthly.csv` |
| 5b | `5b-B1-weekly.R` | Disaggregate monthly targets to weekly using daily flow patterns. Apply weekly constraint parameters. | Monthly B1, gauge flows, HUC4 flows, weekly PNW params | `output/B1_data_1.4.0/B1_weekly.csv` |
| 6 | `6-validation.R` | Validate outputs through integrity checks, comparisons with RectifHyd source and previous B1 versions. | B1 monthly/weekly, RectifHyd, previous versions | `output/validation_report.txt`, `output/validation_plots/` |


## Output directory structure

```
output/
├── huc4_average_flows_imputed.csv               # HUC4 daily streamflow (from step 1)
├── pnw_hourly_power.csv                         # PNW hourly power data (from step 3)
├── pnw_daily_data.csv                           # PNW daily dam data (from step 3)
├── PNW_28_max_min_ador_parameters.csv           # Monthly PNW parameters (from step 4)
├── PNW_28_max_min_ador_parameters_WEEKLY_BASED.csv  # Weekly PNW parameters (from step 4)
├── USACE_weekly_parameters_28.csv               # USACE weekly targets (from step 4)
├── B1_data_1.4.0/
│   ├── B1_monthly.csv                           # Final monthly dataset
│   └── B1_weekly.csv                            # Final weekly dataset
├── validation_report.txt                        # Validation summary
└── validation_plots/                            # Diagnostic plots
    ├── 01_generation_timeseries.png
    ├── 02_seasonal_pattern.png
    ├── 03_b1_vs_rectifhyd.png
    ├── 04_constraints_by_size.png
    ├── 05_weekly_vs_monthly.png
    ├── 06_capacity_factor_distribution.png
    ├── 07_version_comparison_aggregate.png
    └── version_comparisons/
        ├── B1_version_compare_monthly_all_plants.pdf
        └── B1_version_compare_weekly_all_plants.pdf
```


## R package dependencies

R dependencies are managed with `renv`. After cloning, run `renv::restore()` to install all packages at their locked versions. Key packages include:

- `tidyverse` -- Data manipulation and visualization
- `arrow` -- Reading/writing Parquet files
- `readxl` -- Reading EIA and EHA Excel spreadsheets
- `dataRetrieval` -- USGS NWIS streamflow data retrieval
- `cder` -- California CDEC data retrieval
- `missRanger` -- Random forest imputation for missing streamflow values
- `janitor` -- Column name cleaning
- `glue` -- String interpolation
- `paletteer` -- Color palettes for diagnostic plots
- `conflicted` -- Explicit function conflict resolution
- `renv` -- Reproducible R package management

### GridView conversion (Python)

The `gridview-inputs/` directory contains Python scripts to convert B1 datasets into GridView PCM input format:
- `monthly_GV_Format.py` -- Converts monthly B1 data
- `weekly_GV_Format.py` -- Converts weekly B1 data
- Uses crosswalk file: `gridview_to_EIA_ID_v2.csv`


## Related datasets and resources

### Input data sources

- [RectifHyd v1.4.0](link to datahub) -- Monthly hydropower generation estimates for ~1,500 plants (2001-2024). The primary generation input to B1-data. See the [RectifHyd README](https://github.com/HydroWIRES-PNNL/rectifhyd) for methodology.
- [EIA-923](https://www.eia.gov/electricity/data/eia923/) -- US Energy Information Administration monthly and annual power generation data from Form 923 (and earlier Form 906/920). RectifHyd disaggregates the annual values from this dataset.
- [ORNL Existing Hydropower Assets (EHA)](https://hydrosource.ornl.gov/data/datasets/eha-capacity-factor-plant-database-2005-2024/) -- Comprehensive database of ~2,300 US hydropower plants with nameplate capacity, operating mode (Run-of-River vs Storage), balancing authority, state, and HUC codes.
- [HILARRI](https://hydrosource.ornl.gov/data/datasets/hilarri-v3/) -- Hydropower Infrastructure - LAkes, Reservoirs, and Rivers database linking hydropower plants to dams, reservoirs, river reaches. Provides GRanD IDs, NHDPlus COMIDs, and HUC codes. v1.1 used for B1-data-1.4; v3 used for RectifHyd-1.4.
- [ResOpsUS](https://doi.org/10.5281/zenodo.5367383) -- Observed reservoir operations (storage, inflow, outflow) for ~700 major US reservoirs (Steyaert et al. 2022). Time series largely cover 1930-2020.

### Streamflow data sources accessed at runtime

- [USGS NWIS](https://waterdata.usgs.gov/nwis) -- Daily and instantaneous streamflow at thousands of gauges nationwide. Accessed via the `dataRetrieval` R package.
- [California CDEC](https://cdec.water.ca.gov/) -- California Data Exchange Center providing reservoir and streamflow data. Accessed via the `cder` R package.
- [Reclamation RISE](https://data.usbr.gov/rise/api) -- Bureau of Reclamation's Information Sharing Environment. Provides daily flow/release data at Reclamation facilities.
- [Colorado CDSS](https://dwr.state.co.us/tools/statetype) -- Colorado's Decision Support Systems providing streamflow and diversion data.
- [Reclamation Pacific Northwest Hydromet](https://www.usbr.gov/pn/hydromet/) -- Daily hydromet data for Columbia Basin facilities (Reclamation's Pacific Northwest Region).
- [Reclamation Missouri Basin Hydromet](https://www.usbr.gov/gp/hydromet/) -- Daily hydromet data for Missouri Basin facilities (Reclamation's Great Plains Region).
- [USACE NWD Dataquery](https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/) -- Army Corps of Engineers Northwestern Division near-real-time and historical data for Columbia Basin dams. Provides hourly power, forebay, inflow, and outflow data for 28 PNW dams.

### Alternative and complementary hydropower datasets

- [RectifHydPlus v1.1](https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/) -- Alternative monthly hydropower generation dataset from ORNL covering ~593 plants (>10 MW) from 1980-2019. Uses additional data sources including DayFlow simulated streamflow, ISTARF reservoir simulation, and direct EIA spreadsheet reading. See [Turner 2025](https://www.nature.com/articles/s41597-025-05323-y).
- [HESC v2](https://hydrosource.ornl.gov/) -- Hydropower Energy Storage Capacity dataset from ORNL, providing energy storage estimates for US hydropower reservoirs.
- [PUDL](https://catalyst.coop/pudl/) -- Public Utility Data Liberation project, providing cleaned and standardized EIA data as Parquet files. Used in B1-data v2.0.

### Other versions of B1-data

- [B1-data v2.0](https://github.com/HydroWIRES-PNNL/B1-data) -- Major update that extends back to 1980, integrates multiple generation sources (PUDL, RectifHydPlus, RectifHyd), includes plant-level metadata, and uses `config.yml` for centralized configuration.
- [B1-data v1.3](https://zenodo.org/records/13351949) / [v1.2](https://zenodo.org/records/10574003) / [v1.1](https://zenodo.org/records/8408246) -- Earlier versions of the B1 dataset on Zenodo.

### Tools and software

- [Grid Hydro](link) -- PNNL's home for hydrology and hydropower related resources.
- [hydrofixr](https://github.com/pnnl/hydrofixr) -- R package for retrieving and processing USACE hydropower dam data. Used in B1-data v1.3 and earlier; replaced by direct NWD API calls in v1.4.

### Key publications

- Bracken, C., Broman, D. & Voisin, N. (2025). B1 Data: monthly and weekly hydropower generation and operating constraints for 1500 plants in the United States. *Scientific Data*. https://doi.org/10.1038/s41597-025-05097-3 -- **B1-data constraints paper.**
- Turner, S.W.D., Voisin, N. & Nelson, K. (2022). Revised monthly energy generation estimates for 1,500 hydroelectric power plants in the United States. *Scientific Data* 9, 675. https://doi.org/10.1038/s41597-022-01748-x -- **Original RectifHyd paper.**
- Turner, S.W.D. (2025). Monthly generation for 593 hydropower plants in the United States: 1980-2022. *Scientific Data*. https://www.nature.com/articles/s41597-025-05323-y -- **RectifHydPlus paper.**
- Steyaert, J.C., Condon, L.E., Turner, S.W.D. & Voisin, N. (2022). ResOpsUS, a dataset of historical reservoir operations in the contiguous United States. *Scientific Data* 9, 34. https://doi.org/10.1038/s41597-022-01134-7


## Changelog

**Version 1.4.0** (Jan 2026)

- Updated data through 2024
- Removed hydrofixr dependency; USACE data downloaded directly from NWD web services
- Annual files combined into single monthly and weekly output files
- Code refactored for better maintainability
- New validation script with diagnostic plots
- Fixed a bug with HUC4 proxy flow affecting ~130 small plants

**Version 1.3.0** (Aug 2024)

- Uses RectifHyd 1.3 which includes several hundred new observed flow data locations from a variety of sources (USGS, CDEC, RISE, CDSS, PNH, MBH, NWD, ResOpsUS)

**Version 1.2.0** (Dec 2023)

- Adds hydro plant data (forebay, inflow, outflow) for Pacific Northwest plants and HUC4 flow data for most plants

**Version 1.1.2** (Nov 2023)

- Updates the data using final EIA 2022 data

**Version 1.1.0** (Aug 2023)

- Extends the data to 2022 using updated versions of the underlying data


## License

BSD 2-Clause License.

Copyright (c) 2024, Pacific Northwest National Laboratory. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Citation

If you use this dataset, please cite:

> Bracken, C., Broman, D. & Voisin, N. (2025). B1 Data: monthly and weekly hydropower generation and operating constraints for 1500 plants in the United States. *Scientific Data*. https://doi.org/10.1038/s41597-025-05097-3

and

> Bracken, C., Broman, D. & Voisin, N. (2026). HydroWIRES B1-data: Monthly and Weekly Hydropower Generation and Constraint Data (Version 1.4.0) [Data set]. PNNL DataHub.

## Support

For questions or issues, please contact cameron.bracken@pnnl.gov or open an issue on GitHub.
