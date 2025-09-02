## README

#### Replication files for "Impact of extreme precipitation events on facility-based births: an analysis of 21 sub-Saharan African countries"

### Authors
Oumar Aly Ba, Fleur Hierink, Cameron Taylor, Peter M Macharia, Lenka Beňová, Jérémy Laurent-Lucchetti, and Nicolas Ray

### Overview
This repository contains the R scripts, data management processes, and an R Project file used in the research project investigating the relationship between extreme precipitation events and facility-based births. The analysis aims to understand how weather extremes affect health service delivery and accessibility in various regions.


### Repository Structure

- `EPEs_and_Institutional_Birth.Rproj`: R Project file for RStudio (sets working directory and environment).
- `README.md`, `README.html`: Documentation files.
- `data/`: Input datasets (not provided here; see **Notes on Data Availability**).
- `output/`: Directory where results, figures, and tables are stored after running the scripts.
- `scripts/`: All replication scripts, organized as follows:
  - `00_Source.R`: **Master replication script**. Clears the R session, loads required packages and prints their versions, sources helper functions, and runs the full pipeline (`1_main_results` → `2_attribution` → `3_supplementary`). Memory is cleared between stages.
  - `01_Function.R`: Helper functions used throughout the analysis (decimal truncation, weighted means, coefficient and confidence interval extraction).
  - `1_main_results/`: Scripts generating the main results of the study.
  - `2_attribution/`: Attribution analysis scripts.
  - `3_supplementary/`: Sensitivity analyses, extended analyses, supplementary attribution and supplementary information.


### Scripts and Files Description

##### 0. Paper Replication
- `scripts/00_Source.R`: Master script for replication.
- `scripts/01_Function.R`: Custom functions used throughout the project.

##### 1. Main Results
- `scripts/1_main_results/01_table_1_baseline_results.R`: Generates the baseline results in Table 1.
- `scripts/1_main_results/02_figure_1_multiple_cutoff.R`: Produces Figure 1, showing results for multiple cutoff analyses.
- `scripts/1_main_results/03_figure_2_heterogeneity.R`: Creates Figure 2, exploring heterogeneity in effects.

##### 2. Attribution
- `scripts/2_attribution/01_figure_3_and_S5_attribution.R`: Generates attribution figures (Figure 3 and Supplementary Figure S5).

##### 3. Supplementary

###### Sensitivity Analysis
- Scripts `01` to `08`: Sensitivity analyses, including (but not limited to) missing precipitation records, placebo tests, distributed lag models, ETCCDI climate indices, precipitation exceedance measures, and alternative fixed effects.

###### Extended Analysis
- Scripts `01` to `08`: Extended analyses exploring sustained rainfall exposure, alternative outcomes (e.g., skilled births), and heterogeneity across multiple factors, including (but not limited to) wealth, travel time, road networks, and climate zones.

###### Attribution Supplementary
- Scripts `01` and `02`: Additional attribution analyses (attribution with country coefficients).

###### Extra Information and Data
- `scripts/3_supplementary/4_extra_information_and_data/01_supplementary_descriptive_tables_figures.R`: Descriptive tables and figures in supplementary information.

### Installation and Setup
Before running the scripts:

**1.** Ensure **R** and **RStudio** are installed. We used **R** version 4.2.2 (2022-10-31).

**2.** Open the project in RStudio using `EPEs_and_Institutional_Birth.Rproj` to ensure all paths and settings are automatically configured.

**3.** Install required R packages (versions used at replication time):


```r
# List of packages and versions used
rdhs (0.8.2), sf (1.0.14), tidyverse (2.0.0), dplyr (1.1.4), chirps (0.1.4), lubridate (1.9.3),
furrr (0.3.1), labelled (2.10.0), flextable (0.9.1), stargazer (5.2.3), rnaturalearth (0.3.2),
rnaturalearthdata (0.1.0), table1 (1.4.3), fixest (0.11.0), lmtest (0.9.40), texreg (1.38.6),
raster (3.6.20), broom (1.0.5), terra (1.7.23), viridis (0.6.2), scales (1.3.0), exactextractr (0.9.1),
readxl (1.4.3), archive (1.1.7), naniar (1.0.0), car (3.1.1), gtsummary (1.7.2), gt (0.10.0),
ggpubr (0.5.0), kableExtra (1.3.4), ggridges (0.5.4), alpaca (0.3.4), mcreplicate (0.1.2),
marginaleffects (0.17.0), osmextract (0.4.1), patchwork (1.2.0), ggrepel (0.9.2), RColorBrewer (1.1.3)

```

<ins>Use this command to install these packages, replacing `packageName` and `version` as needed:</ins>
```r
install.packages("packageName", version = "version")
```

###  Notes on Data Availability
- **DHS data:** Due to the DHS Program’s data use agreement (https://dhsprogram.com/data/Terms-of-Use.cfm), the raw individual-level DHS data with location identifiers cannot be shared in this repository. Users wishing to replicate the full analysis must register and request access to the DHS data at https://dhsprogram.com/data/. Once access is granted, the DHS sample can be reconstructed by combining multiple Kids Recode (KR) files across surveys and aligning births with precipitation exposures, as described in the paper.
- **GADM data:** For a complete replication of the **maps** and **attribution results**, download the GADM administrative boundaries dataset in GeoPackage (`.gpkg`) format here https://gadm.org/download_world.html. Then place the files in: `data/raw/GADM/`  
- **Other inputs:** Additional datasets (CHIRPS precipitation, travel time, OpenStreetMap roads, climate zones) are publicly available from their respective sources. The download and preprocessing scripts are not included in this repository. The provided analysis scripts reproduce the results using these prepared inputs.


### Usage
To run the scripts, ensure your RStudio session is set to the project directory. Before starting any analysis, execute `scripts/00_Source.R` (to load packages) and `scripts/01_Function.R` (to load helper functions). Once these are loaded, you can run the scripts in `scripts/1_main_results`,`scripts/2_attribution`, and `scripts/3_supplementary` directly without reloading packages or functions. 


### Computational Details

- **Hardware specifications:** MacBook Pro, Apple M1 Pro, 8 cores, 32 GB RAM.

- **Runtime:** The following main heavy scripts can take from 30 minutes to 5 hours to run each:
  - `scripts/1_main_results/02_figure_1_multiple_cutoff.R`
  - `scripts/2_attribution/01_figure_3_and_S5_attribution.R`
  - `scripts/3_supplementary/1_sensitivity_analysis/02_table_S2_probit.R`
  - `scripts/3_supplementary/1_sensitivity_analysis/04_figure_S2_randomization_based_placebo.R`
  - `scripts/3_supplementary/1_sensitivity_analysis/06_figure_S3_ETCCDI_indices.R`
  - `scripts/3_supplementary/2_extended_analysis/01_figure_S4_acute_vs_sustained_model.R`
  - `scripts/3_supplementary/2_attribution_supplementary/01_figure_S6_country_coefficients.R`
  - `scripts/3_supplementary/2_attribution_supplementary/02_figure_S7_attribution.R`


### Contact
For further information, contact Oumar Aly Ba at [oumar.ba@unige.ch](mailto:oumar.ba@unige.ch).

### Citation
If you use the data or methodology of this project in your own research, please cite the paper.

---
