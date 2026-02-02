# HydroWIRES B1-data: Monthly and Weekly Hydropower Constraints Based on Disaggregated EIA-923 Data

**Version 1.4.0**

**Authors:** Cameron Bracken (PNNL), Daniel Broman (PNNL), Nathalie Voisin (PNNL)

**Corresponding author:** cameron.bracken@pnnl.gov

This repository contains the code to reproduce the dataset: [HydroWIRES B1-data: Monthly and Weekly Hydropower Generation and Constraint data](ref)

## Overview

The B1 dataset provides both monthly and weekly hydropower constraints (maximum and minimum generation) and power targets for hundreds of hydropower plants across the United States. The data is intended for use in Production Cost Models (PCMs) and Capacity Expansion Models (CEMs). The hydropower data is based on disaggregated monthly and annual reported EIA 923 data and monthly power data from the RectifHyd dataset.

## Steps to Reproduce

### 1. Download this repository
```bash
git clone https://github.com/HydroWIRES-PNNL/B1-data-1.4.git
cd B1-data-1.4
```

### 2. Download required input data files

Place the following files in the `data/` directory:

- **RectifHyd v1.4.0** (monthly hydropower data): Download from [Zenodo](https://zenodo.org/records/10011017) and save as `data/RectifHyd_v1.4.0.csv`

- **EHA FY2024 database** (plant metadata): Download [ORNL_EHAHydroPlant_PublicFY2024.xlsx](https://hydrosource.ornl.gov/) and place in `data/`

- **HILARRI database** (reservoir data): Download and unzip [HILARRI_v1_1_0.zip](https://hydrosource.ornl.gov/sites/default/files/2021-08/HILARRI_v1_1_0.zip) into `data/`

### 4. Run the processing pipeline

Execute the following scripts in order. Each script depends on outputs from previous steps:

```bash
# Step 1: Download and prepare HUC4-level streamflow data
Rscript 1-huc4-streamflow.R

# Step 2: Download and prepare gauge-level streamflow data for specific plants
Rscript 2-gauge-streamflow.R

# Step 3: Download USACE dam operational data (downloads directly from NWD web services)
Rscript 3-hydropower.R

# Step 4: Calculate PNW parameters from hourly generation data
Rscript 4-pnw-params.R

# Step 5a: Generate monthly B1 constraints dataset
Rscript 5a-B1-monthly.R

# Step 5b: Generate weekly B1 constraints dataset
Rscript 5b-B1-weekly.R

# Step 6: Validate outputs and generate diagnostic plots
Rscript 6-validation.R
```

All scripts can also be run from within R using `source('script-name.R')`.

### 5. Review outputs

The final datasets will be in the `output/` directory:

- `output/B1_data_1.4.0/B1_monthly.csv` - Monthly constraints dataset
- `output/B1_data_1.4.0/B1_weekly.csv` - Weekly constraints dataset
- `output/validation_report.txt` - Data validation summary
- `output/validation_plots/` - Diagnostic plots

## Key Features

- **Monthly and weekly resolutions** for flexible model integration
- **Coverage**: 2001-2024 for hundreds of US hydropower plants (excluding Alaska and Hawaii)
- **Constraints**: p_min, p_max, and ADOR (Average Daily Operating Range)
- **Flow-based disaggregation**: Uses HUC4 or plant-specific streamflow to disaggregate monthly to weekly
- **PNW-derived parameters**: Empirically derived from 28 Pacific Northwest hydropower plants with high-quality hourly data
- **Regional support**: Western and non-Western US plant classifications

## Dataset Columns

Key columns in the final datasets:

- `eia_id`: EIA plant identifier
- `plant`, `state`, `ba`: Plant metadata
- `year`, `month` (monthly) or `year`, `jweek`, `week_start` (weekly)
- `target_mwh`: Energy target
- `nameplate`: Maximum plant capacity (MW)
- `p_avg`, `p_min`, `p_max`: Average, minimum, maximum generation constraints (MW)
- `ador`: Average Daily Operating Range (MW)
- `huc4`, `usgs_id`: Watershed identifiers
- `huc4_flow_cfs`: HUC4 streamflow (cubic feet per second)
- `forebay_ft`, `inflow_cfs`, `outflow_cfs`: Dam operational data (PNW plants only)

## Changes in Version 1.4

- **Updated to 2024 data**: Extended time series from 2001-2024
- **Removed hydrofixr dependency**: USACE data now downloaded directly from NWD web services
- **Code refactor**: Code and pipeline cleand up for better maintainability and readability
- **Improved validation**: New validation script with diagnostic plots
- **Centralized outputs**: All generated files now in `output/` directory

## Related Resources

The disaggregation methodology and source data:

- [RectifHyd Dataset (Zenodo)](https://zenodo.org/records/10011017) - Monthly only data
- [Turner et al. 2022 - Scientific Data](https://www.nature.com/articles/s41597-022-01748-x) 
- [Original disaggregation code](https://github.com/immm-sfa/turner_voisin_nelson_2022_scientific_data)

Final published dataset:

- [HydroWIRES B1 Dataset (Zenodo)](https://zenodo.org/records/13351949)

## Citation

If you use this dataset, please cite:

> Bracken, C., Broman, D., & Voisin, N. (2024). HydroWIRES B1-data: Monthly and Weekly Hydropower Generation and Constraint Data (Version 1.4.0) [Data set]. (ref)

## Support

For questions or issues, please contact cameron.bracken@pnnl.gov or open an issue on GitHub.


