################################################################
##############    Table S17 :  Road length       ###############
################################################################

# ################################################################
# # Load data
# ################################################################
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Regression
################################################################

# Regression Table S17 column (1)
reg_prec_log_length_interaction <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag*log(0.01+length_m) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
          DHS_sample_cluster + Country_birth_date , 
        final_data,
        cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Regression Table S17 column (2)
final_data$median_length_m_dummy_var <- as.factor(final_data$median_length_m_dummy_var)
reg_prec_median_length_interaction <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:median_length_m_dummy_var + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
          DHS_sample_cluster + Country_birth_date , 
        final_data,
        cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")





# Regression Table S17 column (3)
final_data$length_m_quart_bin <- as.factor(final_data$length_m_quart_bin)
reg_prec_length_m_quart_bin_interaction <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:length_m_quart_bin+ factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                           DHS_sample_cluster + Country_birth_date , 
                                         final_data,
                                         cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Extract confidence intervals, samples means and p-values of interaction terms
################################################################

# Extract the confidence interval of model in Table S17 column (3) to mention in the paper 
confint(reg_prec_length_m_quart_bin_interaction, level = 0.95, se = "cluster") 


# Extract p-values for interaction terms of the models in Table S17 
pval_median_dummy <- linearHypothesis(reg_prec_median_length_interaction, "prec_85_count_3d_0_lag:median_length_m_dummy_var0 = prec_85_count_3d_0_lag:median_length_m_dummy_var1") # Table S17 column (2)
pval_q1_q2 <- linearHypothesis(reg_prec_length_m_quart_bin_interaction, "prec_85_count_3d_0_lag:length_m_quart_bin1 = prec_85_count_3d_0_lag:length_m_quart_bin2") # Table S17 column (3)
pval_q1_q3 <- linearHypothesis(reg_prec_length_m_quart_bin_interaction, "prec_85_count_3d_0_lag:length_m_quart_bin1 = prec_85_count_3d_0_lag:length_m_quart_bin3") # Table S17 column (3)
pval_q1_q4 <- linearHypothesis(reg_prec_length_m_quart_bin_interaction, "prec_85_count_3d_0_lag:length_m_quart_bin1 = prec_85_count_3d_0_lag:length_m_quart_bin4") # Table S17 column (3)
pval_q2_q3 <- linearHypothesis(reg_prec_length_m_quart_bin_interaction, "prec_85_count_3d_0_lag:length_m_quart_bin2 = prec_85_count_3d_0_lag:length_m_quart_bin3") # Table S17 column (3)
pval_q2_q4 <- linearHypothesis(reg_prec_length_m_quart_bin_interaction, "prec_85_count_3d_0_lag:length_m_quart_bin2 = prec_85_count_3d_0_lag:length_m_quart_bin4") # Table S17 column (3)
pval_q3_q4 <- linearHypothesis(reg_prec_length_m_quart_bin_interaction, "prec_85_count_3d_0_lag:length_m_quart_bin3 = prec_85_count_3d_0_lag:length_m_quart_bin4") # Table S17 column (3)


# Convert the p-values in character
pval_median_dummy <- substr(sprintf("%.8f", pval_median_dummy[2,3]), 1, 6) # Table S17 column (2)
pval_q1_q2 <- substr(sprintf("%.8f", pval_q1_q2[2,3]), 1, 6) # Table S17 column (3)
pval_q1_q3 <- substr(sprintf("%.8f", pval_q1_q3[2,3]), 1, 6) # Table S17 column (3)
pval_q1_q4 <- substr(sprintf("%.8f", pval_q1_q4[2,3]), 1, 6) # Table S17 column (3)
pval_q2_q3 <- substr(sprintf("%.8f", pval_q2_q3[2,3]), 1, 6) # Table S17 column (3)
pval_q2_q4 <- substr(sprintf("%.8f", pval_q2_q4[2,3]), 1, 6) # Table S17 column (3)
pval_q3_q4 <- substr(sprintf("%.8f", pval_q3_q4[2,3]), 1, 6) # Table S17 column (3)


# Storing the models in a list for export
model_list <- mget(
  c("reg_prec_log_length_interaction", 
    "reg_prec_median_length_interaction" , 
    "reg_prec_length_m_quart_bin_interaction"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# Export regression 
################################################################

# Table - Export
texreg(model_list,
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.gof.rows = list(
         "P(Test: Below median = Above median)"     =c("", paste(pval_median_dummy), ""),
         "P(Test: Road length Q1 = Road length Q2)" =c("", "", paste(pval_q1_q2)),
         "P(Test: Road length Q1 = Road length Q3)" =c("", "", paste(pval_q1_q3)),
         "P(Test: Road length Q1 = Road length Q4)" =c("", "", paste(pval_q1_q4)),
         "P(Test: Road length Q2 = Road length Q3)" =c("", "", paste(pval_q2_q3)),
         "P(Test: Road length Q2 = Road length Q4)" =c("", "", paste(pval_q2_q4)),
         "P(Test: Road length Q3 = Road length Q4)" =c("", "", paste(pval_q3_q4)),
         "DHS sample cluster FE"  = c("Yes", "Yes", "Yes"),
         "Country-day of birth FE" = c("Yes", "Yes", "Yes"),
         "Maternal and household controls" = c("Yes", "Yes", "Yes"),
         "Sample mean" = sample_mean_rows
         ),
       custom.coef.map = list("prec_85_count_3d_0_lag" = "N of days over 85 percentiles",
                              "prec_85_count_3d_0_lag:log(0.01 + length_m)" = "N of days over 85 percentiles X log(0.01+Road Length)",
                              "prec_85_count_3d_0_lag:median_length_m_dummy_var0" = " N of days over 85 percentiles X Road Length (below median)",
                              "prec_85_count_3d_0_lag:median_length_m_dummy_var1" = " N of days over 85 percentiles X Road Length (above median)",
                              "prec_85_count_3d_0_lag:length_m_quart_bin1" = " N of days over 85 percentiles X Road length Q1",
                              "prec_85_count_3d_0_lag:length_m_quart_bin2" = " N of days over 85 percentiles X Road length Q2",
                              "prec_85_count_3d_0_lag:length_m_quart_bin3" = " N of days over 85 percentiles X Road length Q3",
                              "prec_85_count_3d_0_lag:length_m_quart_bin4" = " N of days over 85 percentiles X Road length Q4"
                              ),
       custom.header = list("Facility-based birth (per 1,000 births)"= 1:3),
       custom.model.names = c("(1)", "(2)", "(3)"),   
       caption = "EPEs and Facility-based birth : Heterogeneity in Travel Time",
       include.rsquared = TRUE,
       include.proj.stats = FALSE,
       include.adjrs = FALSE,
       include.fixef_sizes = FALSE,
       include.groups = FALSE,
       include.nobs = TRUE,
       table = FALSE,
       caption.above = TRUE,
       use.packages = FALSE,
       custom.note = "Clustered (DHS cluster) standard-errors in parentheses.  Signif. Codes: ***: 0.01, **: 0.05, *: 0.1}",
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_road_length.tex", sep = "")
       )


