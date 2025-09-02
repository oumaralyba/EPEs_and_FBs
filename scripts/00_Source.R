##' Title: Source script
##' Author: Oumar Aly Ba
##' University of Geneva
##' Manuscript :Impact of extreme precipitation events on facility-based births: an analysis of 21 sub-Saharan African countries
##' 

################################################################
# Initialize and Load packages
################################################################

# Clear environment
rm(list = ls())
gc()  

# Packages
# devtools::install_github("ropensci/rdhs")
library(rdhs)  
library(sf)
library(tidyverse)
library(dplyr)
library("chirps")
library(lubridate)
library(furrr)
library(labelled)
library(flextable)
library(stargazer)
library("rnaturalearth")
library("rnaturalearthdata")
library(table1)
library(fixest)
library(lmtest)
library(texreg)
library("raster")
library(broom)
library(terra)
library(viridis)
library(scales)  # for the labels
library(exactextractr) # for exact_extract command 
library(readxl)
library(archive) # for unzip downlooaded folder
library(naniar) # for command replace_with_na
library(car) # for linear hypothesis
library("gtsummary") # for tables
library(gt) # for tables with black text
library(ggpubr) # For figures 
library(kableExtra) # for exporting latex table 
library(ggridges) # For density plot
library(alpaca) # for logistic regression
library(mcreplicate) # for randomization
library(marginaleffects) # for hypothesis testing of linear combination
library(osmextract) # downloading OSM data on roads and rivers
library(grid)   # for unit()
library(patchwork) # Combine the two maps side by side
library(ggrepel)
library(RColorBrewer)


# Chec version of all currently loaded packages and their versions
loaded_packages <- loadedNamespaces()
package_versions <- sapply(loaded_packages, function(x) {
  packageVersion(x)
})
package_versions


################################################################
###### Load custom functions
################################################################

# Load custom functions
source("scripts/01_Function.R")


################################################################
###### 1_main_results
################################################################

## Table 1
source("scripts/2_main_results/01_table_1_baseline_results.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

##  Figure 1
source("scripts/2_main_results/02_figure_1_multiple_cutoff.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

##  Figure 2
source("scripts/2_main_results/03_figure_2_heterogeneity.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions


################################################################
###### 2_attribution
################################################################

##  Figure 3 and S5
source("scripts/3_attribution/01_figure_3_and_S5_attribution.R") 
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions


################################################################
###### 3_supplementary 
################################################################


###### 1_sensitivity_analysis  ###### 

# Table S1: Missing Precipitation Records
source("scripts/4_supplementary/1_sensitivity_analysis/01_table_S1_missing_precipitation_records.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S2: Probit Analysis
source("scripts/4_supplementary/1_sensitivity_analysis/02_table_S2_probit.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Figure S1: Distributed Lag Models
source("scripts/4_supplementary/1_sensitivity_analysis/03_figure_S1_DLM.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Figure S2: Randomization-Based Placebo
source("scripts/4_supplementary/1_sensitivity_analysis/04_figure_S2_randomization_based_placebo.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S3: Recent Birth Analysis
source("scripts/4_supplementary/1_sensitivity_analysis/05_table_S3_recent_birth.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Figure S3: ETCCDI Climate Indices
source("scripts/4_supplementary/1_sensitivity_analysis/06_figure_S3_ETCCDI_indices.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Tables S4 & S5: EPE Exceedance
source("scripts/4_supplementary/1_sensitivity_analysis/07_table_S4_S5_EPE_exceedance.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S6: Alternative Fixed Effects
source("scripts/4_supplementary/1_sensitivity_analysis/08_table_S6_alternative_fixed_effects.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions



###### 2_extended_analysis  ###### 

# Figure S4: Acute and Sustained Rainfall Exposure Model 
source("scripts/4_supplementary/2_extended_analysis/01_figure_S4_acute_vs_sustained_model.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S7: Additional Facility-Level Analysis
source("scripts/4_supplementary/2_extended_analysis/02_table_S7_facility_level_additional.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S8: Additional Analysis on Skilled Births
source("scripts/4_supplementary/2_extended_analysis/03_table_S8_skilled_birth_additional.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S9: Wealth Analysis
source("scripts/4_supplementary/2_extended_analysis/04_table_S9_wealth.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S10: Travel Time Analysis
source("scripts/4_supplementary/2_extended_analysis/05_table_S10_travel_time.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S11: Perceived Access to Motorized Transport
source("scripts/4_supplementary/2_extended_analysis/06_table_S11_perceived_accesss_motorized.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S12: Road Length Analysis
source("scripts/4_supplementary/2_extended_analysis/07_table_S12_road_length.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Table S13: Climate Zone Analysis
source("scripts/4_supplementary/2_extended_analysis/08_table_S13_climate_zone.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions


###### 3_attribution_supplementary  ###### 

# Figure S6: Country Coefficients
source("scripts/4_supplementary/3_attribution_supplementary/01_figure_S6_country_coefficients.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions

# Figure S7: Additional Attribution Analysis
source("scripts/4_supplementary/3_attribution_supplementary/02_figure_S7_attribution.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions


###### 4_extra_information_and_data  ###### 

## Extra Information and Data
# Supplementary - Descriptive Tables & Figures
source("scripts/4_supplementary/4_extra_information_and_data/01_supplementary_descriptive_tables_figures.R")
rm(list = setdiff(ls(), lsf.str())) # Remove all objects except functions















