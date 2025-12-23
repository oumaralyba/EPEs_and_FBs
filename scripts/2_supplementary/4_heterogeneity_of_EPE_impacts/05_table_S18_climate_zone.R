################################################################
#############      Table S18 : Climate Zone       ##############
################################################################


#################################################################
# Load data
#################################################################

# Read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")


################################################################
# Regression
################################################################

# Regression with climate zone classes
ols_fe_dhs_cbd_climate <- feols(Facility_based_births*1000 ~ 
                                  prec_85_count_3d_0_lag:factor(Climate_Group) 
                                + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                                | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

ols_fe_dhs_cbd_epe_climate <- feols(Facility_based_births*1000 ~ 
                                      epe_5day_p85:factor(Climate_Group) 
                                    + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                                    | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Extract confidence intervals, sample means and p-values of interaction terms
################################################################

# Extract confidence intervals
confint(ols_fe_dhs_cbd_climate, level = 0.95, se = "cluster")
confint(ols_fe_dhs_cbd_epe_climate, level = 0.95, se = "cluster")

# Extract p-values for interaction terms (all three per model)
# For ols_fe_dhs_cbd_climate
pval_climate_cbd_arid_vs_tropical      <- linearHypothesis(ols_fe_dhs_cbd_climate, "prec_85_count_3d_0_lag:factor(Climate_Group)B: Arid = prec_85_count_3d_0_lag:factor(Climate_Group)A: Tropical")[2,3]
pval_climate_cbd_temperate_vs_tropical <- linearHypothesis(ols_fe_dhs_cbd_climate, "prec_85_count_3d_0_lag:factor(Climate_Group)C: Temperate = prec_85_count_3d_0_lag:factor(Climate_Group)A: Tropical")[2,3]
pval_climate_cbd_arid_vs_temperate <- linearHypothesis(ols_fe_dhs_cbd_climate, "prec_85_count_3d_0_lag:factor(Climate_Group)B: Arid = prec_85_count_3d_0_lag:factor(Climate_Group)C: Temperate")[2,3]

# For ols_fe_dhs_cbd_epe_climate
pval_climate_epe_arid_vs_tropical      <- linearHypothesis(ols_fe_dhs_cbd_epe_climate, "epe_5day_p85:factor(Climate_Group)B: Arid = epe_5day_p85:factor(Climate_Group)A: Tropical")[2,3]
pval_climate_epe_temperate_vs_tropical <- linearHypothesis(ols_fe_dhs_cbd_epe_climate, "epe_5day_p85:factor(Climate_Group)C: Temperate = epe_5day_p85:factor(Climate_Group)A: Tropical")[2,3]
pval_climate_epe_arid_vs_temperate <- linearHypothesis(ols_fe_dhs_cbd_epe_climate, "epe_5day_p85:factor(Climate_Group)B: Arid = epe_5day_p85:factor(Climate_Group)C: Temperate")[2,3]

# Convert p-values to character
pval_climate_cbd <- list(
  "Arid vs Tropical"      = substr(sprintf("%.8f", pval_climate_cbd_arid_vs_tropical), 1, 6),
  "Temperate vs Tropical" = substr(sprintf("%.8f", pval_climate_cbd_temperate_vs_tropical), 1, 6),
  "Arid vs Temperate"     = substr(sprintf("%.8f", pval_climate_cbd_arid_vs_temperate), 1, 6)
)
pval_climate_epe <- list(
  "Arid vs Tropical"      = substr(sprintf("%.8f", pval_climate_epe_arid_vs_tropical), 1, 6),
  "Temperate vs Tropical" = substr(sprintf("%.8f", pval_climate_epe_temperate_vs_tropical), 1, 6),
  "Arid vs Temperate"     = substr(sprintf("%.8f", pval_climate_epe_arid_vs_temperate), 1, 6)
)


model_list_climate <- mget(c("ols_fe_dhs_cbd_climate", "ols_fe_dhs_cbd_epe_climate"))
sample_means_climate <- sapply(model_list_climate,
                               calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)
sample_mean_rows_climate <- setNames(sprintf("%.3f", sample_means_climate), names(sample_means_climate))

################################################################
# Export regression
################################################################

texreg(list(ols_fe_dhs_cbd_climate, ols_fe_dhs_cbd_epe_climate),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.header = list("Facility-based birth (per 1,000 births)"= 1:2),
       custom.model.names = c("(1)", "(2)"),
       custom.gof.rows = list(
         "P(Test: Tropical = Arid)"      = c(pval_climate_cbd[["Arid vs Tropical"]],      pval_climate_epe[["Arid vs Tropical"]]),
         "P(Test: Tropical = Temperate)" = c(pval_climate_cbd[["Temperate vs Tropical"]], pval_climate_epe[["Temperate vs Tropical"]]),
         "P(Test: Arid = Temperate)"     = c(pval_climate_cbd[["Arid vs Temperate"]],     pval_climate_epe[["Arid vs Temperate"]]),
         "DHS cluster FE"                = c("Yes", "Yes"),
         "Country-day of birth FE"       = c("Yes", "Yes"),
         "Maternal and household controls" = c("Yes", "Yes"),
         "Sample mean"                   = sample_mean_rows_climate
       ),
       custom.coef.map = list(
         "prec_85_count_3d_0_lag:factor(Climate_Group)A: Tropical" = "N of days over 85 percentiles X Tropical",
         "prec_85_count_3d_0_lag:factor(Climate_Group)B: Arid" = "N of days over 85 percentiles X Arid",
         "prec_85_count_3d_0_lag:factor(Climate_Group)C: Temperate" = "N of days over 85 percentiles X Temperate",
         "epe_5day_p85:factor(Climate_Group)A: Tropical" = "EPE 5-days speel (85th percentiles) X Tropical",
         "epe_5day_p85:factor(Climate_Group)B: Arid" = "EPE 5-days speel (85th percentiles) X Arid",
         "epe_5day_p85:factor(Climate_Group)C: Temperate" = "EPE 5-days speel (85th percentiles) X Temperate"
       ),
       caption = "Cumulative days over 85 percentiles thresholds by Köppen-Geiger climate group",
       include.rsquared = TRUE,
       include.proj.stats = FALSE,
       include.adjrs = FALSE,
       include.fixef_sizes = FALSE,
       include.groups = FALSE,
       include.nobs = TRUE,
       caption.above = TRUE,
       use.packages=FALSE,
       table = FALSE,
       custom.note = "",
       file = paste(output_path , "Tables/ols_fe_dhs_cbd_5D_accumulation_3D_time_window_model_climate.tex", sep = "")
       )




