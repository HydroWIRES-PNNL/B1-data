# HydroWIRES B1: Monthly and Weekly Hydropower Constraints Based on Disaggregated EIA-923 Data

Authors: Cameron Bracken (PNNL), Daniel Broman (PNNL), Nathlie Voisin (PNNL)

Corresponding author: cameron.bracken@pnnl.gov 

This repo contains the code to reproduce the dataset: [HydroWIRES B1: Monthly and Weekly Hydropower Constraints Based on Disaggregated EIA-923 Data](https://zenodo.org/records/13351949).

NOTE: __This documentation referrs to version 1.4.0 and earlier, it needs updating for version 2.0.0.__

Steps to reproduce:

  1. Download this repo 
  
  2. Install the [hydrofixr package](https://github.com/pnnl/hydrofixr)
  
  3. Download [these data files](https://github.com/HydroWIRES-PNNL/B1-data/releases/tag/1.0) from GitHub and place them in `data/hydrofixr`
  
  4. Download all the data files from [this Zenodo record](https://zenodo.org/records/5773123) and place them in   `data/hydrofixr`
  
  5. Download the latest version of the [RectifHyd data](https://zenodo.org/records/10011017) and place it in `data`
  
  6. Download the [EHA 2023 database](https://hydrosource.ornl.gov/sites/default/files/2023-08/ORNL_EHAHydroPlant_FY2023_rev.xlsx) and place the Excel file in `data`
  
  7. Download the [HILLARI database](https://hydrosource.ornl.gov/sites/default/files/2021-08/HILARRI_v1_1_0.zip) and unpack the zip file into `data`
  
  8. Install the required R packages, from within R run `install.packages(c('tidyverse','dataRetrieval','missRanger','readxl'))`
  
  9. Run `1-pnw-params.R` from within R using `source('1-pnw-params.R')` or on the command line `Rscript 1-pnw-params.R` to   produce the parameters for disaggregation
  
  10. Run `2-streamflow.R` from within R using `source('2-streamflow.R')` or on the command line `Rscript 2-streamflow.R` to download and prepare the streamflow data
  
  11. Run `3-hydropower.R` from within R using `source('3-hydropower.R')` or on the command line `Rscript 3-hydropower.R` to download and prepare the hydropower data (power, inflow, outflow, forebay)
  
  12. Run `4a-B1-monthly.R` from within R using `source('4a-B1-monthly.R')` or on the command line `Rscript 4a-B1-monthly.R` to produce monthly constraints 
  
  13. Run `4a-B1-weekly.R` from within R using `source('4a-B1-weekly.R')` or on the command line `Rscript 4a-B1-weekly.R` to produce weekly constraints

The output from these steps will be two directories `B1_monthly` and `B1_weekly` with one file per year. The dataset provides both monthly and weekly constraints (maximum and minimum generation) and power targets for hundreds of hydropower plants across the United States. The data is intended for use in Production Cost Models (PCMs) and Capacity Expansion Models (CEMs). The hydropower data is based on disaggregated annual power data which is part of the EIA-923 dataset. The original disaggregation procedure is detailed here:

- https://www.nature.com/articles/s41597-022-01748-x
- https://github.com/immm-sfa/turner_voisin_nelson_2022_scientific_data
- https://github.com/pnnl/hydrofixr
- https://zenodo.org/records/10011017

The final data is here:

- https://zenodo.org/records/8408246




## RectifHydPlus License

Version 1.4 makes use of [RectifHydPlus code](https://code.ornl.gov/turnersw/rectifhydplus) and [data](https://hydrosource.ornl.gov/data/datasets/rectifhydplus/) for which the following license applies:

BSD 2-Clause License

Copyright (c) 2024, Oak Ridge National Laboratory

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
