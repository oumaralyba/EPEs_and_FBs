################################################################
###    Table S6 : Full sample with rematched precipitation   ###
################################################################

# ################################################################
# # Load data
# ################################################################

# read the full sample data with rematched precipitation 
final_data <- readRDS(file = "data/processed/final_data/final_data_rematched_prec_coord.Rds")
final_data <- as.data.frame(final_data)


################################################################
# Regression
################################################################

# If data properly loaded, then one should get 4583 observations with missing_chirps=="Yes"
final_data %>% 
  filter(missing_chirps=="Yes") %>%
  count()

# Table S1 - Regression
ols_fe <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
ols_fe_dhs <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
ols_fe_dhs_bd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster +  birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")




################################################################
# Extract confidence intervals, samples means and decrease in sample means
################################################################

# Extract the confidence interval
confint(ols_fe_dhs_cbd, parm="prec_85_count_3d_0_lag", level = 0.95, se = "cluster")

# Storing the models in a list for export
model_list <- mget(
  c("ols_fe", 
    "ols_fe_dhs" , 
    "ols_fe_dhs_bd" , 
    "ols_fe_dhs_cbd"))


# Applying the function to each model to get the sample mean of the outcome while keeping model names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set the sample mean number in character for table export
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# Export regression 
################################################################

# Create a custom style without "Fit statistics" header and the line above it
custom_style <- style.tex(
  #main = "main",
  line.top = "\\midrule \\midrule",
  depvar.title = "",  # Title for the dependent variable
  depvar.style = "", # police pf the dependent variable
  model.title = "\\cmidrule(lr){2-5}",   # Remove "Model" header
  var.title = "\\midrule",     # Remove "Variables" header
  stats.title = "",
  #fixef.title = "Fixed-effects", 
  fixef.title = "\\midrule",
  fixef.suffix = " FE"
  #fixef.prefix = ""
)


# Table - Export
etable(model_list,
       tex = T,                      # To output LaTeX code
       digits = "r3",                       # Number of digits to display
       digits.stats = 3,
       keep = "N of days",
       dict = c("prec_85_count_3d_0_lag" = "N of days over 85 percentiles",
                "DHS_sample_cluster" = "DHS cluster",
                "birth_date" = "Day of birth",
                "Country_birth_date" = "Country-day of birth",
                "Facility_based_births*1000" = "Facility-based birth (per 1,000 births)"
       ),  # Labels for variables
       fitstat = c("n","r2"),
       extralines = list(
                         "_^Sample mean" = c(sample_mean_rows),
                         "_^Maternal and household controls" = c("Yes", "Yes", "Yes", "Yes")),
       # extralines = list("_^Sample mean" = c(sample_mean_rows)),
       depvar = T,
       #notes = "DHS sample cluster FE and Country Day of birth FE are included in all models.",
       #style.tex = style.tex("aer"),
       float = FALSE,
       style.tex= custom_style,  
       file = paste(output_path , "Tables/Table_prec_85_count_3d_0_lag_rematched_prec_coord.tex", sep = ""),
       replace = TRUE
)






