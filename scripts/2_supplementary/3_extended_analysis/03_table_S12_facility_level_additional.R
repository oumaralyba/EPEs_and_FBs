################################################################
###   Table S12 : Extended analysis across facility level     ###
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


### Check the classication of facility level
# unique(final_data$rh_del_pllevel_3)
# table(final_data$rh_del_pllevel_3, useNA = "always")
# table(as_factor(final_data$rh_del_pllevel_3), useNA = "always")
# table(final_data$place_of_delivery_label, as_factor(final_data$rh_del_pllevel_3), useNA = "ifany")
# table(final_data$Facility_based_births, as_factor(final_data$rh_del_pllevel_3), useNA = "ifany")
# table(as_factor(final_data$rh_del_pllevel), as_factor(final_data$rh_del_pllevel_3), useNA = "ifany")


# Public hospital VS non-facility
reg_public_hospital_vs_non_facility <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                               DHS_sample_cluster + Country_birth_date , 
                                             final_data %>% filter(rh_del_pllevel_3== 1 | rh_del_pllevel_3== 4 | rh_del_pllevel_3== 5), # I added rh_del_pllevel_3== 5 because it is not "vs home" but "vs non-facility birth"
                                             cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Public lower-level VS non-facility
reg_public_lower_vs_non_facility <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                            DHS_sample_cluster + Country_birth_date , 
                                          final_data %>% filter(rh_del_pllevel_3== 2 | rh_del_pllevel_3== 4| rh_del_pllevel_3== 5), # I added rh_del_pllevel_3== 5 because it is not "vs home" but "vs non-facility birth"
                                          cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Non-public (all levels) VS non-facility
reg_nonpublic_vs_non_facility <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                         DHS_sample_cluster + Country_birth_date , 
                                       final_data %>% filter(rh_del_pllevel_3== 3 | rh_del_pllevel_3== 4| rh_del_pllevel_3== 5), # I added rh_del_pllevel_3== 5 because it is not "vs home" but "vs non-facility birth"
                                       cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")



# Public hospital VS Public lower-level
# Create the variable
final_data <- final_data %>%
  mutate(FB_PH_PLL = case_when(
    rh_del_pllevel_3 == 1 ~ 1,
    rh_del_pllevel_3 == 2 ~ 0,
    TRUE ~ NA_real_ ))  %>% 
  set_value_labels(FB_PH_PLL = c("Public lower-level" = 0, "Public hospital" = 1)) %>%
  set_variable_labels(FB_PH_PLL = "Live births Public hospital VS Public lower-level")
unique(final_data$FB_PH_PLL)
table(as.factor(final_data$FB_PH_PLL), useNA = "always")

reg_public_hospital_vs_lower <- feols(FB_PH_PLL*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                                        DHS_sample_cluster + Country_birth_date , 
                                      final_data,
                                      cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Extract samples means 
################################################################

# Storing the models in a list for export
model_list <- mget(
  c("reg_public_hospital_vs_non_facility", 
    "reg_public_lower_vs_non_facility" , 
    "reg_nonpublic_vs_non_facility" , 
    "reg_public_hospital_vs_lower"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# Export regression 
################################################################

# Create a custom style without "Fit statistics" header and the line above it
custom_style <- style.tex(
  #main = "main",
  line.top = "\\midrule \\midrule",
  depvar.title = "",  # Title for the dependent variable
  depvar.style = "",
  model.title = "\\cmidrule(lr){2-5} \\
  &  Public Hospital VS Non-facility &  Public Lower VS Non-facility  &  Nonpublic (all levels)  VS Non-facility &  Public Hospital VS Public Lower \\ 
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
                "Facility_based_births*1000" = "Facility-based birth (per 1,000 births)",
                "FB_PH_PLL*1000" = "Facility-based birth (per 1,000 births)"
       ),  # Labels for variables
       fitstat = c("n","r2"),
       extralines = list(
         "_^Sample mean" = c(sample_mean_rows),
         "_^Maternal and household controls" = c("Yes", "Yes", "Yes", "Yes")),
       depvar = T,
       float = FALSE,
       style.tex= custom_style,  
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_facility_level.tex", sep = ""),
       replace = TRUE
)





