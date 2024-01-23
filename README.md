# HydroWIRES B1: Monthly and Weekly Hydropower Constraints Based on Disaggregated EIA-923 Data

This repo contains the code to reproduce the dataset: [HydroWIRES B1: Monthly and Weekly Hydropower Constraints Based on Disaggregated EIA-923 Data](https://zenodo.org/records/8408246).

Steps to reproduce:

  1. Download this repo 
  
  2. Install the [hydrofixr package](https://github.com/pnnl/hydrofixr)
  
  3. Download [these data files](https://github.com/HydroWIRES-PNNL/B1-data/releases/tag/1.0) on GitHub and place them in `data/hydrofixr`
  
  4. Download all the data files from [this Zenodo record](https://zenodo.org/records/5773123) and place them in   `data/hydrofixr`
  
  5. Download the latest version of the [RectifHyd data](https://zenodo.org/records/10011017) and place it in `data`
  
  6. Download the [EHA 2023 database](https://hydrosource.ornl.gov/sites/default/files/2023-08/ORNL_EHAHydroPlant_FY2023_rev.xlsx) and place the Excel file it in `data`
  
  7. Download the [HILLARI database](https://hydrosource.ornl.gov/sites/default/files/2021-08/HILARRI_v1_1_0.zip) and unpack   the zip file into `data`
  
  8. Run `1-pnw-params.R` from within R using `source('1-pnw-params.R')` or on the command line `Rscript 1-pnw-params.R` to   produce the parameters for disaggregation
  
  9. Run `2a-B1-monthly.R` from within R using `source('2a-B1-monthly.R')` or on the command line `Rscript 2a-B1-monthly.R`   to produce monthly constraints 
  
  10. Run `2a-B1-weekly.R` from within R using `source('2a-B1-weekly.R')` or on the command line `Rscript 2a-B1-weekly.R` to produce weekly constraints

The output from these steps will be two directories `B1_monthly` and `B1_weekly` with one file per year. The dataset provides both monthly and weekly constraints (maximum and minimum generation) and power targets for hundreds of hydropower plants across the United States. The data is intended for use in Production Cost Models (PCMs) and Capacity Expansion Models (CEMs). The hydropower data is based on disaggregated annual power data which is part of the EIA-923 dataset. The original disaggregation procedure is detailed here:

- https://www.nature.com/articles/s41597-022-01748-x
- https://github.com/immm-sfa/turner_voisin_nelson_2022_scientific_data
- https://github.com/pnnl/hydrofixr
- https://zenodo.org/records/10011017

The final data is here:

- https://zenodo.org/records/8408246

Corresponding author: cameron.bracken@pnnl.gov 
