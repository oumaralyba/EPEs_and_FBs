################################################################
#####  Table S16 : Perceived access and motorized        #######
################################################################


# ################################################################
# # Load data
# ################################################################

final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Regression 
################################################################


# Table S16 column 1
final_data$Perceived_access <- as.factor(final_data$Perceived_access)
ols_fe_dhs_cbd_pa <- feols(Facility_based_births*1000 ~
                             prec_85_count_3d_0_lag:Perceived_access + Perceived_access + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                             DHS_sample_cluster + Country_birth_date ,
                           final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")



# Table S16 column 2
final_data$motorcycle_scooter_all <- as.factor(final_data$motorcycle_scooter_all)
ols_fe_dhs_cbd_motorcycle_scooter_all <- feols(Facility_based_births*1000 ~
                             prec_85_count_3d_0_lag:motorcycle_scooter_all + motorcycle_scooter_all + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                             DHS_sample_cluster + Country_birth_date ,
                           final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")



# Table S16 column 3
final_data$car_truck_all <- as.factor(final_data$car_truck_all)
ols_fe_dhs_cbd_car_truck_all <- feols(Facility_based_births*1000 ~
                                                 prec_85_count_3d_0_lag:car_truck_all + car_truck_all + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                                 DHS_sample_cluster + Country_birth_date ,
                                               final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

 

################################################################
# Extract confidence intervals, samples means, p-values of interaction terms, linear combination of coefficients
################################################################

# Extract the confidence interval of model with car-owner households to mention in the paper 
confint(ols_fe_dhs_cbd_car_truck_all, level = 0.95, se = "cluster") 

# Extract p-values for interaction terms of the model
pval_perc_acc <- linearHypothesis(ols_fe_dhs_cbd_pa, "prec_85_count_3d_0_lag:Perceived_access0 = prec_85_count_3d_0_lag:Perceived_access1")
pval_moto <- linearHypothesis(ols_fe_dhs_cbd_motorcycle_scooter_all, "prec_85_count_3d_0_lag:motorcycle_scooter_all0 = prec_85_count_3d_0_lag:motorcycle_scooter_all1")
pval_car <- linearHypothesis(ols_fe_dhs_cbd_car_truck_all, "prec_85_count_3d_0_lag:car_truck_all0 = prec_85_count_3d_0_lag:car_truck_all1")

# Convert the p-values in character
pval_perc_acc <- substr(sprintf("%.8f", pval_perc_acc[2,3]), 1, 6)
pval_moto <- substr(sprintf("%.8f", pval_moto[2,3]), 1, 6)
pval_car <- substr(sprintf("%.8f", pval_car[2,3]), 1, 6)

# Storing the models in a list for export
model_list <- mget(c("ols_fe_dhs_cbd_pa",
                     "ols_fe_dhs_cbd_motorcycle_scooter_all",
                     "ols_fe_dhs_cbd_car_truck_all"
                     ))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# Linear combination for interpretation 
################################################################

###### Test the significance of the cumulated effect of those who have car and those who do not have a car 
# The benefits of having a car is no longer present as when there is EPEs, the benefits goes from 18.03782 to -11.5 and  -11.5 is not significantly different from 0
hypotheses(ols_fe_dhs_cbd_car_truck_all,  hypothesis = "car_truck_all1 + `prec_85_count_3d_0_lag:car_truck_all1` = 0") 


# The cost of not having a car is not exacerbated when there is EPEs, as the cost goes from -18.03782 to -27.1 and  -27.1 is significantly different from 0
ols_fe_dhs_cbd_car_truck_all_inv <- feols(Facility_based_births*1000 ~
                                            prec_85_count_3d_0_lag*ref(car_truck_all,1) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                            DHS_sample_cluster + Country_birth_date ,
                                          final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
summary(ols_fe_dhs_cbd_car_truck_all_inv)
hypotheses(ols_fe_dhs_cbd_car_truck_all_inv, hypothesis = "prec_85_count_3d_0_lag+`ref(car_truck_all, 1)0`+`prec_85_count_3d_0_lag:ref(car_truck_all, 1)0` = 0")


################################################################
# Export regression 
################################################################

# Table - Export
texreg(model_list,
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.header = list("Facility-based birth (per 1,000 births)"= 1:3),
       custom.model.names = c("(1)", "(2)", "(3)"),   
       custom.gof.rows = list(
         "P(Test: Probem = Not a problem)" =c(paste(pval_perc_acc), "", ""),
         "P(Test: Not owner = Owner)" =c("", paste(pval_moto), ""),
         "P(Test: Not owner = Owner)" =c("", "", paste(pval_car)),
         "DHS cluster FE"  = c("Yes", "Yes", "Yes"),
         "Country-day of birth FE" = c("Yes", "Yes", "Yes"),
         "Maternal and household controls" = c("Yes", "Yes", "Yes"),
         "Sample mean" = sample_mean_rows
         ),
       custom.coef.map = list(
                              "prec_85_count_3d_0_lag:Perceived_access0" = " N of days over 85 percentiles X Distance is not a big problem",
                              "prec_85_count_3d_0_lag:Perceived_access1" = " N of days over 85 percentiles X Distance is a big problem",
                              "prec_85_count_3d_0_lag:motorcycle_scooter_all0" = " N of days over 85 percentiles X Household dont own motorcycle/scooter",
                              "prec_85_count_3d_0_lag:motorcycle_scooter_all1" = " N of days over 85 percentiles X Household own motorcycle/scooter",
                              "prec_85_count_3d_0_lag:car_truck_all0" = "N of days over 85 percentiles X Household dont own car/truck",
                              "prec_85_count_3d_0_lag:car_truck_all1" = "N of days over 85 percentiles X Household own car/truck",
                              "Perceived_access1" = "Distance is a big problem",
                              "motorcycle_scooter_all1" = "Household own motorcycle/scooter",
                              "car_truck_all1" = "Household own car/truck"),
       caption = "EPEs and Facility-based birth : Heterogeneity in perceived access and motorized vehicle",
       include.rsquared = TRUE,
       include.proj.stats = FALSE,
       include.adjrs = FALSE,
       include.fixef_sizes = FALSE,
       include.groups = FALSE,
       include.nobs = TRUE,
       caption.above = TRUE,
       use.packages=FALSE,
       table = FALSE,
       custom.note = "Clustered (DHS cluster) standard-errors in parentheses.  Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}",
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_perceived_access.tex", sep = "")
       )








