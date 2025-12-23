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
library(scico)

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
###### Define destination paths for outputs
################################################################

# Set destination paths
# output_path <- here::here("/Users/oumaralyba/Dropbox/Apps/Overleaf/Extreme Precipitation Events and Facility-based birth/") # for other path (e.g., Overleaf)
output_path <- here::here("output/") # Local project output path


################################################################
###### 1_main_results
################################################################

# Table 1
source("scripts/1_main_results/01_table_1_baseline_results.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

#  Figure 1
source("scripts/1_main_results/02_figure_1_and_S5_multiple_cutoff.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

#  Figure 2
source("scripts/1_main_results/03_figure_2_heterogeneity.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

#  Figure 3 and S11
source("scripts/1_main_results/04_figure_3_and_S11_attribution.R") 
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path


################################################################
###### 2_supplementary 
################################################################


###### 1_data_and_descriptive_statistics  ###### 

# Descriptive Statistics
source("scripts/2_supplementary/1_data_and_descriptive_statistics/01_descriptive_statistics.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path


###### 2_sensitivity_analysis  ###### 

# Table S6: Missing precipitation records
source("scripts/2_supplementary/2_sensitivity_analysis/01_table_S6_missing_precipitation_records.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S7: Probit analysis
source("scripts/2_supplementary/2_sensitivity_analysis/02_table_S7_probit.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Figure S6: Distributed lag models
source("scripts/2_supplementary/2_sensitivity_analysis/03_figure_S6_DLM.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Figure S7: Randomization-based placebo
source("scripts/2_supplementary/2_sensitivity_analysis/04_figure_S7_randomization_based_placebo.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S8: Recent birth analysis
source("scripts/2_supplementary/2_sensitivity_analysis/05_table_S8_recent_birth.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Figure S8: ETCCDI climate indices
source("scripts/2_supplementary/2_sensitivity_analysis/06_figure_S8_ETCCDI_indices.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Tables S9 & S10: EPE exceedance
source("scripts/2_supplementary/2_sensitivity_analysis/07_table_S9_S10_EPE_exceedance.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S11: Alternative fixed effects
source("scripts/2_supplementary/2_sensitivity_analysis/08_table_S11_alternative_fixed_effects.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path


###### 3_extended_analysis  ###### 

# Figure S9: Precipitation thresholds - Acute vs Sustained
source("scripts/2_supplementary/3_extended_analysis/01_figure_S9_precip_thresholds_acute_vs_sustained.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Figure S10: Acute and sustained rainfall exposure model 
source("scripts/2_supplementary/3_extended_analysis/02_figure_S10_acute_vs_sustained_model.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S12: Additional facility-level analysis
source("scripts/2_supplementary/3_extended_analysis/03_table_S12_facility_level_additional.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S13: Additional analysis on skilled births
source("scripts/2_supplementary/3_extended_analysis/04_table_S13_skilled_birth_additional.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path


###### 4_heterogeneity_of_EPE_impacts  ###### 

# Table S14: Wealth analysis
source("scripts/2_supplementary/4_heterogeneity_of_EPE_impacts/01_table_S14_wealth.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S15: Travel time analysis
source("scripts/2_supplementary/4_heterogeneity_of_EPE_impacts/02_table_S15_travel_time.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S16: Perceived access to motorized transport
source("scripts/2_supplementary/4_heterogeneity_of_EPE_impacts/03_table_S16_perceived_access_motorized.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S17: Road length analysis
source("scripts/2_supplementary/4_heterogeneity_of_EPE_impacts/04_table_S17_road_length.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Table S18: Climate zone analysis
source("scripts/2_supplementary/4_heterogeneity_of_EPE_impacts/05_table_S18_climate_zone.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path


###### 5_attribution_supplementary  ###### 

# Figure S12: Country coefficients
source("scripts/2_supplementary/5_attribution_supplementary/01_figure_S12_country_coefficients.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path

# Figure S13: Additional attribution analysis
source("scripts/2_supplementary/5_attribution_supplementary/02_figure_S13_attribution.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path


###### 6_extra_information_and_data  ###### 

# Extra descriptive statistics
source("scripts/2_supplementary/6_extra_information_and_data/01_extra_descriptive_statistics.R")
rm(list = setdiff(ls(), c(lsf.str(), "output_path"))) # Remove all objects except functions and output_path















