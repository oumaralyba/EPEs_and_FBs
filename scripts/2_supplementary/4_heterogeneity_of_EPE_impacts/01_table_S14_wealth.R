################################################################
##############    Table S14 :  Household wealth   ###############
################################################################

# ################################################################
# # Load data
# ################################################################

final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Regression
################################################################

# Table S14 - Regression 
ols_fe_dhs_cbd_wealth <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:Wealth + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date ,
      final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Extract confidence intervals, samples means and p-values of interaction terms
################################################################

# Extract the confidence interval to mention in the paper
confint(ols_fe_dhs_cbd_wealth, level = 0.95, se = "cluster") 

# Extract p-values for interaction terms
pval_poorest_poorer <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorest = prec_85_count_3d_0_lag:Wealthpoorer")
pval_poorest_middle <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorest = prec_85_count_3d_0_lag:Wealthmiddle")
pval_poorest_richer <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorest = prec_85_count_3d_0_lag:Wealthricher")
pval_poorest_richest <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorest = prec_85_count_3d_0_lag:Wealthrichest")
pval_poorer_middle <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorer = prec_85_count_3d_0_lag:Wealthmiddle")
pval_poorer_richer <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorer = prec_85_count_3d_0_lag:Wealthricher")
pval_poorer_richest <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthpoorer = prec_85_count_3d_0_lag:Wealthrichest")
pval_middle_richer <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthmiddle = prec_85_count_3d_0_lag:Wealthricher")
pval_middle_richest <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthmiddle = prec_85_count_3d_0_lag:Wealthrichest")
pval_richer_richest <- linearHypothesis(ols_fe_dhs_cbd_wealth, "prec_85_count_3d_0_lag:Wealthricher = prec_85_count_3d_0_lag:Wealthrichest")

# Convert p-values in character
pval_poorest_poorer <- substr(sprintf("%.8f", pval_poorest_poorer[2,3]), 1, 6)
pval_poorest_middle <- substr(sprintf("%.8f", pval_poorest_middle[2,3]), 1, 6)
pval_poorest_richer <- substr(sprintf("%.8f", pval_poorest_richer[2,3]), 1, 6)
pval_poorest_richest <- substr(sprintf("%.8f", pval_poorest_richest[2,3]), 1, 6)
pval_poorer_middle <- substr(sprintf("%.8f", pval_poorer_middle[2,3]), 1, 6)
pval_poorer_richer <- substr(sprintf("%.8f", pval_poorer_richer[2,3]), 1, 6)
pval_poorer_richest <- substr(sprintf("%.8f", pval_poorer_richest[2,3]), 1, 6)
pval_middle_richer <- substr(sprintf("%.8f", pval_middle_richer[2,3]), 1, 6)
pval_middle_richest <- substr(sprintf("%.8f", pval_middle_richest[2,3]), 1, 6)
pval_richer_richest <- substr(sprintf("%.8f", pval_richer_richest[2,3]), 1, 6)


# Storing the models in a list for export
model_list <- mget(c("ols_fe_dhs_cbd_wealth"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# Export regression 
################################################################

# Table - Export
summary(ols_fe_dhs_cbd_wealth)
texreg(ols_fe_dhs_cbd_wealth,
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       #custom.header = list("Boat hours"= 1:3, "Fishing hours" = 4:6, "Boat count" = 7:9),
       custom.header = list("Facility-based birth (per 1,000 births)"= 1) ,
       custom.model.names = c("(1)"),   
       custom.gof.rows = list(
         "P(Test: Poorest = Poorer)" =paste(pval_poorest_poorer),
         "P(Test: Poorest = Middle)" =paste(pval_poorest_middle),
         "P(Test: Poorest = Richer)" =paste(pval_poorest_richer),
         "P(Test: Poorest = Richest)" =paste(pval_poorest_richest),
         "P(Test: Poorer = Middle)" =paste(pval_poorer_middle),
         "P(Test: Poorer = Richer)" =paste(pval_poorer_richer),
         "P(Test: Poorer = Richest)" =paste(pval_poorer_richest),
         "P(Test: Middle = Richer)" =paste(pval_middle_richer),
         "P(Test: Middle = Richest)" =paste(pval_middle_richest),
         "P(Test: Richer = Richest)" =paste(pval_richer_richest),
         "DHS cluster FE"  = c("Yes"),
         "Country-day of birth FE" = c("Yes"),
         "Maternal and household controls" = c("Yes"),
         "Sample mean" = sample_mean_rows
         ),
       custom.coef.map = list("prec_85_count_3d_0_lag" = "N of days over 85 percentiles",
                              "prec_85_count_3d_0_lag:factor(Wealth)poorest" = "N of days over 85 percentiles X poorest",
                              "prec_85_count_3d_0_lag:Wealthpoorest" = "N of days over 85 percentiles X poorest",
                              "prec_85_count_3d_0_lag:Wealthpoorer" = "N of days over 85 percentiles X poorer",
                              "prec_85_count_3d_0_lag:Wealthmiddle" = "N of days over 85 percentiles X middle",
                              "prec_85_count_3d_0_lag:Wealthricher" = "N of days over 85 percentiles X richer",
                              "prec_85_count_3d_0_lag:Wealthrichest" = "N of days over 85 percentiles X richest",
                              "factor(Wealth)poorer" = "Poorer",
                              "factor(Wealth)middle" = "Middle",
                              "factor(Wealth)richer" = "Richer",
                              "factor(Wealth)richest" = "Richest"),
       caption = "Cumulative days over 85 percentiles thresholds",
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
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_wealth.tex", sep = "")
       # scalebox = 0.55
       )







