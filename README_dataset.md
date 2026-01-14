# HydroWIRES B1 Data v2.0.0: Historical Monthly and Weekly Hydropower Data for the United States

Cameron Bracken, Nathalie Voisin, Daniel Broman -- PNNL

The B1 dataset provides observationally derived hydropower generation data as well as inferred maximum and minimum generation for about 1,500 hydropower plants across the United States. The data is intended for multiple uses:

-   The generation data may be useful for studies requiring a long record (45 years) of observationally derived hydropower generation.
-   The hydropower constraints (minimum power, maximum power, and daily operational range) can be used in energy system models such as Production Cost Models (PCMs), Capacity Expansion (CEP) models, and Resource Adequancy (RA) models.

## Dataset development

### Generation data and disaggregation

Version 2.0.0 is a significant update that extends the data back to 1980 (previously it went to 2001). The monthly generation data is derived from multiple sources:

-   [EIA 923](https://www.eia.gov/electricity/data/eia923/) and [EIA 906](https://www.eia.gov/electricity/data/eia923/eia906u.php) monthly and annual reported total generation and capacity.
-   [EIA 860](https://www.eia.gov/electricity/data/eia860m/) plant and generator information from the [Public Utility Data Liberation](https://catalyst.coop/pudl/) project.
-   [RectifHyd version 1.3](https://zenodo.org/records/11584567/) monthly estimated generation at about 1500 plants.\
-   [RectifHydPlus version 1.1](https://hydrosource.ornl.gov/data/datasets/rectifhydplus_v1-1/) monthly estimated generation at about 600 plants with greater then 10 MW of capacity.

The monthly data sources are selected in the following priority order: RectifHydPlus, EIA, RectifHyd. Any missing data after this is estimated by first imputing annual total generation then disaggregating to monthly and weekly using the best available proxy for a particular plant. Available proxies are (from best to worst) turbine release data, total plant outflow, flow at the HUC4 outlet, and average observed generation.

![](data_source_count_monthly.png)

The weekly generation data is disaggregated from annual using the same proxy priority as the monthly data.

### EIA ID aggregation

Several EIA plant IDs refer to the same plant but are split for reporting purposes (such as Hoover Dam). See the [RectifHydPlus paper](https://www.nature.com/articles/s41597-025-05323-y) for details. In version 2.0.0 of the B1 data, these plants are combined where possible. The `eia_id` column will have the ids combined like this: `xxxxx_yyyyy` where the underscode separates the two combined ids. Currently no more than 3 EIA IDs are combined and the 

### Additional details

The code to reproduce this dataset is available [here](https://github.com/HydroWIRES-PNNL/B1-data). The original disaggregation procedure is detailed in the [RectifHyd Paper](https://www.nature.com/articles/s41597-022-01748-x) and the [RectifHyd code repo](https://github.com/immm-sfa/turner_voisin_nelson_2022_scientific_data). The min and max power constraint development is detailed in [this paper](https://doi.org/10.1038/s41597-025-05097-3).

Corresponding authors: cameron.bracken\@pnnl.gov, nathalie.voisin\@pnnl.gov

## Changelog

__Version 2.0.0__

-   Extends the data back to 1980
-   Uses mutiple monthly data sources, selecting the best available source

-   Annual files are now combined into a single file (one for monthly and one for weekly)
-   Fixes a bug with the HUC4 proxy flow affecting about 130 small plants

__Version 1.4.0 (not released publicly)__

-   Updates the data through 2024
-   Annual files are now combined into a single file (one for monthly and one for weekly)
-   Fixes a bug with the HUC4 proxy flow affecting about 130 small plants

__Version 1.3.0__

-   Uses RectiHyd 1.3 which includes several hundred new observed data locations from a variety of sources

__Version 1.2.0__

-   Adds hydro plant data (forebay, inflow, outflow) for some Pacific Northwest plants and HUC4 flow data for most plants

__Version 1.1.2__

-   Updates the data using final EIA 2022 data

__Version 1.1.1__

-   Fixes the zip format

__Version 1.1.0__

-   Extends the data to 2022 using updated versions of the underlying data


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
