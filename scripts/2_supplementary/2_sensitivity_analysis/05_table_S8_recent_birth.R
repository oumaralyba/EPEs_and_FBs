################################################################
##################     Table S8 : Recent birth   ###############
################################################################

# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Replicate baseline regression in Table 1 column 4
################################################################

# Replicate regression in Table 1 in column 4 to include it in Table S3
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ 
                          prec_85_count_3d_0_lag + 
                          factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                        | DHS_sample_cluster + Country_birth_date , 
                        final_data, cluster = "DHS_sample_cluster",
                        weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Regression with most recent birth only
################################################################

# Regression with most recent birth only
table(final_data$bidx)
ols_fe_dhs_cbd_lastbirth <- feols(Facility_based_births*1000 ~ 
                                    prec_85_count_3d_0_lag + 
                                    factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                                  | DHS_sample_cluster + Country_birth_date , 
                                  final_data %>% filter(bidx==1), cluster = "DHS_sample_cluster",
                                  weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Extract  coefficients, samples means and decrease in sample means
################################################################

# Storing the models in a list for export
model_list <- mget(
  c("ols_fe_dhs_cbd", 
    "ols_fe_dhs_cbd_lastbirth"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# Export regression 
################################################################

# Create a custom style similar to the previous examples
custom_style <- style.tex(
  line.top = "\\midrule \\midrule",
  depvar.title = "",  # Title for the dependent variable
  depvar.style = "",  # Style of the dependent variable
  #model.title = "\\cmidrule(lr){2-3}",   # Custom model title style
  model.title = "\\cmidrule(lr){2-3} \\
  &  All live births &  Last live birth  \\ 
  &  \\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\ ",   # Custom model title style
  var.title = "\\midrule",     # Remove "Variables" header
  stats.title = "",
  fixef.title = "\\midrule",
  fixef.suffix = " FE"
  )

# Export the regression results with etable
etable(model_list,
       tex = TRUE,                      # To output LaTeX code
       digits = "r3",                      # Number of digits to display
       digits.stats = 3,
       keep = "N of days", # Specify variables to keep
       dict = c("prec_85_count_3d_0_lag" = "N of days over 85 percentiles",
                "DHS_sample_cluster" = "DHS cluster",
                "Country_birth_date" = "Country-day of birth",
                "Facility_based_births*1000" = "Facility-based birth (per 1,000 births)"
       ),  # Custom labels for variables
       fitstat = c("n","r2"),            # Specify fit statistics to include
       extralines = list("_^Sample mean" = c(sample_mean_rows),
                         "_^Maternal and household controls" = c("Yes", "Yes")),
       depvar = TRUE,                    # Include dependent variable name
       float = FALSE,                    # Do not use LaTeX float environments
       style.tex = custom_style,         # Apply the custom style
       file = paste(output_path , "Tables/Table_prec_85_count_3d_0_lag_recall_bias.tex", sep = ""),
       replace = TRUE                    # Replace existing file
       )


