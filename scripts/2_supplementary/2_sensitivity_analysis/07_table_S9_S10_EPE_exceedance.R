################################################################
########    Table S9 & S10 :  Precipitation Exceedance      #####
################################################################

# ################################################################
# # Load data
# ################################################################

final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


# ################################################################
# # Table S9 : Regression precipitation exceedance with percentiles cutoff
# ################################################################

# Run regression R80pTOT
exced_80_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_80_total, 1, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
exced_80_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_80_total, 2, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Run regression R85pTOT
exced_85_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_85_total, 1, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
exced_85_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_85_total, 2, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Run regression R90pTOT
exced_90_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_90_total, 1, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
exced_90_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_90_total, 2, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Run regression R95pTOT
exced_95_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_95_total, 1, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
exced_95_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_95_total, 2, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# # Table S9 : Extract confidence intervals and samples means
################################################################

# Storing the models in a list for export
model_list <- mget( c("exced_80_total_poly1_reg", "exced_80_total_poly2_reg", 
                      "exced_85_total_poly1_reg", "exced_85_total_poly2_reg", 
                      "exced_90_total_poly1_reg", "exced_90_total_poly2_reg", 
                      "exced_95_total_poly1_reg", "exced_95_total_poly2_reg"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))




################################################################
# # Table S9 :Export regression 
################################################################

# Create a custom style without "Fit statistics" header and the line above it
custom_style <- style.tex(
  #main = "main",
  line.top = "\\midrule \\midrule",
  depvar.title = "",  # Title for the dependent variable
  depvar.style = "",
  model.title = "\\cmidrule(lr){2-9} \\
  &  \\multicolumn{2}{c}{R80pTOT} &  \\multicolumn{2}{c}{R85pTOT}  &  \\multicolumn{2}{c}{R90pTOT}  &  \\multicolumn{2}{c}{R95pTOT} \\
  &  \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9} \\",   # Remove "Model" header
  var.title = "\\midrule",     # Remove "Variables" header
  stats.title = "",
  #fixef.title = "Fixed-effects", 
  fixef.title = "\\midrule",
  fixef.suffix = " FE",
  tablefoot= FALSE
  #fixef.prefix = ""
)

# # Table - Export
etable(list(exced_80_total_poly1_reg,exced_80_total_poly2_reg,  exced_85_total_poly1_reg, exced_85_total_poly2_reg, exced_90_total_poly1_reg, exced_90_total_poly2_reg, exced_95_total_poly1_reg, exced_95_total_poly2_reg),
       tex = T,                      # To output LaTeX code
       digits = "r3",                       # Number of digits to display
       digits.stats = 3,
       keep = "Precipitation",
       dict = c(         "poly(exced_80_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_80_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_80_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "poly(exced_85_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_85_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_85_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "poly(exced_90_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_90_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_90_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "poly(exced_95_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_95_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_95_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                "DHS_sample_cluster" = "DHS cluster",
                "Country_birth_date" = "Country-day of birth",
                "Facility_based_births*1000" = "Facility-based birth (per 1,000 births)"
       ),  # Labels for variables
       fitstat = c("n","r2"),
       extralines = list("_^Sample mean" = c(sample_mean_rows),
                         "_^Maternal and household controls" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
       depvar = T,
       float = FALSE,
       style.tex= custom_style,  
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_facility_level_perc_exceedance_poly.tex", sep = ""),
       replace = TRUE
)



# ################################################################
# # Table S10 : Regression precipitation exceedance with RXXmmTOT
# ################################################################


# Run regression R5mmTOT
exced_5mm_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_5mm_total, 1, raw = TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm = "singleton")
exced_5mm_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_5mm_total, 2, raw = TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Run regression R10mmTOT
exced_10mm_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_10mm_total, 1, raw = TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm = "singleton")
exced_10mm_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_10mm_total, 2, raw = TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Run regression R15mmTOT
exced_15mm_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_15mm_total, 1, raw = TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm = "singleton")
exced_15mm_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_15mm_total, 2, raw = TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date, final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Run regression R20mmTOT
exced_20mm_total_poly1_reg <- feols(Facility_based_births*1000 ~ poly(exced_20mm_total, 1, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
exced_20mm_total_poly2_reg <- feols(Facility_based_births*1000 ~ poly(exced_20mm_total, 2, raw =  TRUE) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# # Table S10 : Extract confidence intervals and samples means
################################################################

# Storing the models in a list for export
model_list <- mget(
  c("exced_5mm_total_poly1_reg" , "exced_5mm_total_poly2_reg" , 
    "exced_10mm_total_poly1_reg", "exced_10mm_total_poly2_reg",
    "exced_15mm_total_poly1_reg", "exced_15mm_total_poly2_reg", 
    "exced_20mm_total_poly1_reg", "exced_20mm_total_poly2_reg"))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)

# Set names for texreg using the model names
sample_mean_rows <- setNames(sprintf("%.3f", sample_means), names(sample_means))


################################################################
# # Table S10 :Export regression 
################################################################


# Create a custom style without "Fit statistics" header and the line above it
custom_style <- style.tex(
  #main = "main",
  line.top = "\\midrule \\midrule",
  depvar.title = "",  # Title for the dependent variable
  depvar.style = "",
  model.title = "\\cmidrule(lr){2-9} \\
  &  \\multicolumn{2}{c}{R5mmTOT} &  \\multicolumn{2}{c}{R10mmTOT}  &  \\multicolumn{2}{c}{R15mmTOT}  &  \\multicolumn{2}{c}{R20mmTOT} \\ 
  &  \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9} \\",   # Remove "Model" header
  var.title = "\\midrule",     # Remove "Variables" header
  stats.title = "",
  #fixef.title = "Fixed-effects", 
  fixef.title = "\\midrule",
  fixef.suffix = " FE",
  tablefoot= FALSE
  #fixef.prefix = ""
)

# # Table - Export
etable(list(exced_5mm_total_poly1_reg,exced_5mm_total_poly2_reg,  exced_10mm_total_poly1_reg, exced_10mm_total_poly2_reg, exced_15mm_total_poly1_reg, exced_15mm_total_poly2_reg, exced_20mm_total_poly1_reg, exced_20mm_total_poly2_reg),
       tex = T,                      # To output LaTeX code
       digits = "r3",                       # Number of digits to display
       digits.stats = 3,
       keep = "Precipitation",
       dict = c(         "poly(exced_5mm_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_5mm_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_5mm_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "poly(exced_10mm_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_10mm_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_10mm_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "poly(exced_15mm_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_15mm_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_15mm_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "poly(exced_20mm_total, 1, raw = TRUE)" = "Precipitation exceedance",
                         "poly(exced_20mm_total, 2, raw = TRUE)1"= "Precipitation exceedance",
                         "poly(exced_20mm_total, 2, raw = TRUE)2"= "Precipitation exceedance$^2$",
                         "DHS_sample_cluster" = "DHS cluster",
                         "Country_birth_date" = "Country-day of birth",
                         "Facility_based_births*1000" = "Facility-based birth (per 1,000 births)"
       ),  # Labels for variables
       fitstat = c("n","r2"),
       extralines = list("_^Sample mean" = c(sample_mean_rows),
                         "_^Maternal and household controls" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
       depvar = T,
       float = FALSE,
       style.tex= custom_style,  
       file = paste(output_path , "Tables/Table_ols_fe_dhs_cbd_facility_level_Rmm_exceedance_poly.tex", sep = ""),
       replace = TRUE
       )











