################################################################
######    Table S11 : Alternative set of fixed effects  ######### 
################################################################

# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Regression with alternative fixed effects
################################################################


# Replicate baseline regression in Table 1 in column 4 to include it in Table S6 (with DHS cluster and Country-day of birth fixed effects)
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ 
                          prec_85_count_3d_0_lag + 
                          factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                        | DHS_sample_cluster + Country_birth_date , 
                        final_data, cluster = "DHS_sample_cluster",
                        weights = ~ rescaled_weight, fixef.rm= "singleton")


# Regression with ADM2 and Country-day of birth fixed effects
ols_fe_R_cbd <- feols(Facility_based_births*1000 ~ 
                         prec_85_count_3d_0_lag + 
                         factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                       | GID_2 + Country_birth_date , 
                       final_data, cluster = "DHS_sample_cluster",
                       weights = ~ rescaled_weight, fixef.rm= "singleton")


# Regression with ADM2-month and Country-day of birth fixed effects
ols_fe_Rm_cbd <- feols(Facility_based_births*1000 ~ 
                             prec_85_count_3d_0_lag + 
                             factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                           | Region_month_gadm + Country_birth_date , 
                           final_data, cluster = "DHS_sample_cluster",
                           weights = ~ rescaled_weight, fixef.rm= "singleton")


# Regression with Cell and Country-day of birth fixed effects
ols_fe_cells_cbd <- feols(Facility_based_births*1000 ~ 
                          prec_85_count_3d_0_lag + 
                          factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                        | cells + Country_birth_date , 
                        final_data, cluster = "DHS_sample_cluster",
                        weights = ~ rescaled_weight, fixef.rm= "singleton")


# Regression with Cell-month and Country-day of birth fixed effects
ols_fe_cellsmonth_cbd <- feols(Facility_based_births*1000 ~ 
                            prec_85_count_3d_0_lag + 
                            factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                          | cell_month_5km + Country_birth_date , 
                          final_data, cluster = "DHS_sample_cluster",
                          weights = ~ rescaled_weight, fixef.rm= "singleton")



################################################################
# Extract  coefficients, samples means and decrease in sample means
################################################################

# Storing the models in a list for export
model_list <- mget(
  c("ols_fe_dhs_cbd", 
    "ols_fe_R_cbd" , 
    "ols_fe_Rm_cbd" , 
    "ols_fe_cells_cbd" , 
    "ols_fe_cellsmonth_cbd"))


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
  depvar.style = "", # police pf the dependent variable
  model.title = "\\cmidrule(lr){2-6}",   # Remove "Model" header
  var.title = "\\midrule",     # Remove "Variables" header
  stats.title = "",
  #fixef.title = "Fixed-effects", 
  fixef.title = "\\midrule",
  fixef.suffix = " FE",
  tablefoot= FALSE
)


# # Table - Export
etable(model_list,
       tex = T,                      # To output LaTeX code
       digits = "r3",                       # Number of digits to display
       digits.stats = 3,
       keep = "N of days",
       dict = c("prec_85_count_3d_0_lag" = "N of days over 85 percentiles",
                "Facility_based_births*1000" = "Facility-based birth (per 1,000 births)"
       ),  # Labels for variables
       fitstat = c("n","r2"),
       extralines = list(
         "_^Sample mean" = c(sample_mean_rows),
         "_^Maternal and household controls" = c("Yes", "Yes", "Yes", "Yes", "Yes")
       ),
       fixef.group =  list(
         "DHS cluster FE" =  "DHS_sample_cluster",
         "_^Country-day of birth FE" = "Country_birth_date",
         "_^Cell-birth month FE" = "cell_month_5km",
         "_^Cells FE" = "cells",
         "_^ADM2-birth month FE" = "Region_month_gadm",
         "_^ADM2 FE" = "GID_2"), 
       float = FALSE,
       style.tex= custom_style, 
       file = paste(output_path , "Tables/Table_robustness_alternative_FE_prec_85_count_3d_0_lag.tex", sep = ""),
       replace = TRUE
)



