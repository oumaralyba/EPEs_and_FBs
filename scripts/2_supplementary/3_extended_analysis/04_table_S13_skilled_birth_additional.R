################################################################
############       Table S13 : Skilled births     ###############
################################################################


# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
# Set as dataframe otherwise when filtering with feols, regression are very slow
final_data <- final_data %>% as.data.frame() 


################################################################
# Regression 
################################################################

# Checks the distribution of Skilled birth, Non-Skilled/No one VS Facility-based births
table(final_data$rh_del_pvskill, final_data$Facility_based_births, useNA = "always")
table(to_factor(final_data$rh_del_pvskill),to_factor(final_data$Facility_based_births), useNA = "always")


# Skilled birth VS Non-Skilled/No one
unique(final_data$rh_del_pvskill)
table(final_data$rh_del_pvskill, useNA = "always")
final_data <- final_data %>%
  mutate(Skilled_birth = case_when(
    rh_del_pvskill == 1 ~ 1,                         
    rh_del_pvskill == 2 | rh_del_pvskill == 3 ~ 0,   
    rh_del_pvskill == 9 ~ NA_real_,                  
    TRUE ~ NA_real_ )) %>%
  set_value_labels(Skilled_birth = c("Non Skilled birth" = 0, "Skilled birth" = 1)) %>%
  set_variable_labels(Skilled_birth = "Skilled birth")
table(as_factor(final_data$rh_del_pvskill), as_factor(final_data$Skilled_birth))

reg_skilled_vs_non_skilled_noone <- feols(Skilled_birth*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
        DHS_sample_cluster + Country_birth_date , 
      final_data,
      cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")



# Skilled birth VS Non-Skilled
final_data <- final_data %>%
  mutate(Skilled_Unskilled = case_when(
    rh_del_pvskill == 1 ~ 1,  # Skilled provider
    rh_del_pvskill == 2 ~ 0,  # Unskilled provider
    TRUE ~ NA_real_           # NA for No one, Don't know/missing, and any other cases
  )) %>%
  set_value_labels(Skilled_Unskilled = c("Unskilled provider" = 0, "Skilled provider" = 1)) %>%
  set_variable_labels(Skilled_Unskilled = "Skilled provider VS Unskilled provider")
table(as_factor(final_data$rh_del_pvskill), as_factor(final_data$Skilled_Unskilled))

reg_skilled_vs_unskilled <- feols(Skilled_Unskilled*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
        DHS_sample_cluster + Country_birth_date , 
      final_data,
      cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Skilled birth VS No one
final_data <- final_data %>%
  mutate(Skilled_vs_NoOne = case_when(
    rh_del_pvskill == 1 ~ 1,  # Skilled provider
    rh_del_pvskill == 3 ~ 0,  # No one
    TRUE ~ NA_real_           # NA for Unskilled provider, Don't know/missing, and any other cases
  )) %>%
  set_value_labels(Skilled_vs_NoOne = c("No one" = 0, "Skilled provider" = 1)) %>%
  set_variable_labels(Skilled_vs_NoOne = "Skilled birth VS No one")
table(as_factor(final_data$rh_del_pvskill), as_factor(final_data$Skilled_vs_NoOne))


reg_skilled_vs_noone <- feols(Skilled_vs_NoOne*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
        DHS_sample_cluster + Country_birth_date , 
      final_data,
      cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Unskilled birth VS No one
final_data <- final_data %>%
  mutate(Unskilled_NoOne = case_when(
    rh_del_pvskill == 2 ~ 1,                         
    rh_del_pvskill == 3 ~ 0,   
    rh_del_pvskill == 1 | rh_del_pvskill == 9 ~ NA_real_,                  
    TRUE ~ NA_real_)) %>%
  set_value_labels(Unskilled_NoOne = c("No one" = 0, "Unskilled provider" = 1)) %>%
  set_variable_labels(Unskilled_NoOne = "Unskilled VS No one")
table(as_factor(final_data$rh_del_pvskill), as_factor(final_data$Unskilled_NoOne))

reg_unskilled_vs_noone <- feols(Unskilled_NoOne*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
        DHS_sample_cluster + Country_birth_date , 
      final_data,
      cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")



################################################################
# Extract samples means 
################################################################

# Storing the models in a list for export
model_list <- mget(
  c("reg_skilled_vs_non_skilled_noone", 
    "reg_skilled_vs_unskilled" , 
    "reg_skilled_vs_noone" , 
    "reg_unskilled_vs_noone"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))

################################################################
# Export regression 
################################################################

# Export regression
# Create a custom style without "Fit statistics" header and the line above it
custom_style <- style.tex(
  #main = "main",
  line.top = "\\midrule \\midrule",
  depvar.title = "",  # Title for the dependent variable
  depvar.style = "",
  model.title = "\\cmidrule(lr){2-5} \\
  &  Skilled VS Non-Skilled/No one &  Skilled VS Non-Skilled  & Skilled VS No one & Unskilled VS No one\\ 
  &  \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-5} \\",   # Remove "Model" header
  var.title = "\\midrule",     # Remove "Variables" header
  stats.title = "",
  #fixef.title = "Fixed-effects", 
  fixef.title = "\\midrule",
  fixef.suffix = " FE"
  #fixef.prefix = ""
)

# # Table - Export
etable(model_list,
       tex = TRUE,                      # To output LaTeX code
       digits = "r3",                       # Number of digits to display
       digits.stats = 3,
       keep = "N of days",
       dict = c("prec_85_count_3d_0_lag" = "N of days over 85 percentiles",
                "DHS_sample_cluster" = "DHS cluster",
                "Country_birth_date" = "Country-day of birth",
                "Skilled_birth*1000" = "Skilled birth attendance (per 1,000 births)",
                "Skilled_Unskilled*1000" = "Skilled birth attendance (per 1,000 births)",
                "Skilled_vs_NoOne*1000" = "Skilled birth attendance (per 1,000 births)",
                "Unskilled_NoOne*1000" = "Skilled birth attendance (per 1,000 births)"
       ),  # Labels for variables
       fitstat = c("n","r2"),
       extralines = list(
         "_^Sample mean" = c(sample_mean_rows),
         "_^Maternal and household controls" = c("Yes", "Yes", "Yes", "Yes")),
       depvar = T,
       float = FALSE,
       style.tex= custom_style,  
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_skilled_birth.tex", sep = ""),
       replace = TRUE
)








