## README

#### Replication files for "Impact of extreme precipitation events on facility-based births: an analysis of 21 sub-Saharan African countries"

### Authors

Oumar Aly Ba, Fleur Hierink, Cameron Taylor, Peter M Macharia, Lenka Beňová, Jérémy Laurent-Lucchetti, and Nicolas Ray

### Overview

This repository contains the R scripts, data management processes, and an R Project file used in the research project investigating the relationship between extreme precipitation events and facility-based births. The analysis aims to understand how weather extremes affect health service delivery and accessibility in various regions.

### Repository Structure

-   `EPEs_and_FBs.Rproj`: R Project file for RStudio (sets working directory and environment).
-   `README.md`, `README.html`: Documentation files.
-   `data/`: Input datasets (not provided here; see **Notes on Data Availability**).
-   `output/`: Directory where results, figures, and tables are stored after running the scripts.
-   `scripts/`: All replication scripts, organized as follows:
    -   `00_Source.R`: **Master replication script**. Clears the R session, loads required packages and prints their versions, sources helper functions, defines the destination folder for tables and figures, and runs the full pipeline (`1_main_results` → `2_supplementary`). Memory is cleared between stages.
    -   `01_Function.R`: Helper functions used throughout the analysis (decimal truncation, weighted means, coefficient and confidence interval extraction).
    -   `1_main_results/`: Scripts generating the main results of the study.
    -   `2_supplementary/`: Scripts generating data and descriptive statistics, sensitivity analyses, extended analyses, heterogeneity analysis, supplementary attribution and additional descriptive statistics.

### Scripts and Files Description

##### 0. Paper Replication

-   `00_Source.R`: Master script for replication.
-   `01_Function.R`: Custom functions used throughout the project.

##### 1. Main Results

-   `01_table_1_baseline_results.R`: Generates the baseline results in Table 1.
-   `02_figure_1_and_S5_multiple_cutoff.R`: Produces Figure 1 and Supplementary Figure S5, showing results for multiple cutoff analyses.
-   `03_figure_2_heterogeneity.R`: Creates Figure 2, exploring heterogeneity in effects.
-   `04_figure_3_and_S11_attribution.R`: Generates attribution figures (Figure 3 and Supplementary Figure S11).

##### 2. Supplementary

###### 1. Data and Descriptive Statistics

-   `01_descriptive_statistics.R`: Descriptive statistics for the study.

###### 2. Sensitivity Analysis

-   `01_table_S6_missing_precipitation_records.R`: Table S6 - Analysis with rematched missing precipitation records.
-   `02_table_S7_probit.R`: Table S7 - Probit regression.
-   `03_figure_S6_DLM.R`: Figure S6 - Distributed lag linear model.
-   `04_figure_S7_randomization_based_placebo.R`: Figure S7 - Randomization-based placebo tests.
-   `05_table_S8_recent_birth.R`: Table S8 - Recent birth analysis.
-   `06_figure_S8_ETCCDI_indices.R`: Figure S8 - ETCCDI climate indices.
-   `07_table_S9_S10_EPE_exceedance.R`: Tables S9 & S10 - EPE exceedance measures.
-   `08_table_S11_alternative_fixed_effects.R`: Table S11 - Alternative fixed effects specifications.

###### 3. Extended Analysis

-   `01_figure_S9_precip_thresholds_acute_vs_sustained.R`: Figure S9 - Precipitation thresholds for acute VS sustained exposure.
-   `02_figure_S10_acute_vs_sustained_model.R`: Figure S10 - Acute VS sustained rainfall exposure model.
-   `03_table_S12_facility_level_additional.R`: Table S12 - Additional analysis on facility-level.
-   `04_table_S13_skilled_birth_additional.R`: Table S13 - Additional analysis on skilled births.

###### 4. Heterogeneity of EPE Impacts

-   `01_table_S14_wealth.R`: Table S14 - Heterogeneity by wealth.
-   `02_table_S15_travel_time.R`: Table S15 - Heterogeneity by travel time.
-   `03_table_S16_perceived_access_motorized.R`: Table S16 - Heterogeneity by perceived access and ownership of motorized vehicle.
-   `04_table_S17_road_length.R`: Table S17 - Heterogeneity by road length.
-   `05_table_S18_climate_zone.R`: Table S18 - Heterogeneity by climate zone.

###### 5. Attribution Supplementary

-   `01_figure_S12_country_coefficients.R`: Figure S12 - Country-specific coefficients.
-   `02_figure_S13_attribution.R`: Figure S13 - Attribution map with country-specific coefficients.

###### 6. Extra Information and Data

-   `01_extra_descriptive_statistics.R`: Additional information and descriptive tables and figures on travel time, road length, and climate zones.

### Installation and Setup

Before running the scripts:

**1.** Ensure **R** and **RStudio** are installed. We used **R** version 4.2.2 (2022-10-31).

**2.** Open the project in RStudio using `EPEs_and_FBs.Rproj` to ensure all paths and settings are automatically configured.

**3.** Install required R packages (versions used at replication time):

``` r
# List of packages and versions used
rdhs (0.8.2), sf (1.0.14), tidyverse (2.0.0), dplyr (1.1.4), chirps (0.1.4), lubridate (1.9.3),
furrr (0.3.1), labelled (2.10.0), flextable (0.9.1), stargazer (5.2.3), rnaturalearth (0.3.2),
rnaturalearthdata (0.1.0), table1 (1.4.3), fixest (0.11.0), lmtest (0.9.40), texreg (1.38.6),
raster (3.6.20), broom (1.0.5), terra (1.7.23), viridis (0.6.2), scales (1.3.0), exactextractr (0.9.1),
readxl (1.4.3), archive (1.1.7), naniar (1.0.0), car (3.1.1), gtsummary (1.7.2), gt (0.10.0),
ggpubr (0.5.0), kableExtra (1.3.4), ggridges (0.5.4), alpaca (0.3.4), mcreplicate (0.1.2),
marginaleffects (0.17.0), osmextract (0.4.1), grid (4.2.2), patchwork (1.2.0), ggrepel (0.9.2), 
RColorBrewer (1.1.3), scico (1.5.0)
```

<ins>Use this command to install these packages, replacing `packageName` and `version` as needed:</ins>

``` r
install.packages("packageName", version = "version")
```

### Notes on Data Availability

-   **DHS data:** Due to the DHS Program’s data use agreement (<https://dhsprogram.com/data/Terms-of-Use.cfm>), the raw individual-level DHS data with location identifiers cannot be shared in this repository. Users wishing to replicate the full analysis must register and request access to the DHS data at <https://dhsprogram.com/data/>. Once access is granted, the DHS sample can be reconstructed by combining multiple Kids Recode (KR) files across surveys and aligning births with precipitation exposures, as described in the paper.
-   **GADM data:** For a complete replication of the **maps** and **attribution results**, download the GADM administrative boundaries dataset in GeoPackage (`.gpkg`) format here <https://gadm.org/download_world.html>. Then place the files in: `data/raw/GADM/`\
-   **Other inputs:** Additional datasets (CHIRPS precipitation, travel time, OpenStreetMap roads, climate zones) are publicly available from their respective sources. The download and preprocessing scripts are not included in this repository. The provided analysis scripts reproduce the results using these prepared inputs.

### Usage

To run the scripts, ensure your RStudio session is set to the project directory. Before starting any analysis, open `scripts/00_Source.R` and execute the lines that **(a)** load packages, **(b)** load helper functions (`scripts/01_Function.R`), and **(c)** define the destination folder for tables and figures. Once these are loaded, you can run the scripts in `scripts/1_main_results` and `scripts/2_supplementary` directly—no need to reload packages or functions.

### Computational Details

-   **Hardware specifications:** MacBook Pro, Apple M1 Pro, 8 cores, 32 GB RAM.

-   **Runtime:** The following main heavy scripts can take from 30 minutes to 5 hours to run each:

    -   `scripts/1_main_results/02_figure_1_and_S5_multiple_cutoff.R`
    -   `scripts/1_main_results/04_figure_3_and_S11_attribution.R`
    -   `scripts/2_supplementary/2_sensitivity_analysis/02_table_S7_probit.R`
    -   `scripts/2_supplementary/2_sensitivity_analysis/04_figure_S7_randomization_based_placebo.R`
    -   `scripts/2_supplementary/2_sensitivity_analysis/06_figure_S8_ETCCDI_indices.R`
    -   `scripts/2_supplementary/3_extended_analysis/02_figure_S10_acute_vs_sustained_model.R`
    -   `scripts/2_supplementary/5_attribution_supplementary/01_figure_S12_country_coefficients.R`
    -   `scripts/2_supplementary/5_attribution_supplementary/02_figure_S13_attribution.R`

### Contact

For further information, contact Oumar Aly Ba at [oumar.ba\@unige.ch](mailto:oumar.ba@unige.ch) or [o.aly.ba\@gmail.com](mailto:o.aly.ba@gmail.com)

### Citation

If you use the data or methodology of this project in your own research, please cite the paper.

------------------------------------------------------------------------
