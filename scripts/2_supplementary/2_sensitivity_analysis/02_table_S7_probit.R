################################################################
##########  Table S7 : Probit regression      ##################
################################################################

# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- as.data.frame(final_data)

# Subset the data for faster computation
final_data_subset <- final_data %>%
  dplyr::select(CountryName, Facility_based_births, prec_85_count_3d_0_lag ,prec_85_count_3d_0_lag,prec_85_count_5d_0_lag,prec_90_count_3d_0_lag,prec_95_count_3d_0_lag, 
                Wealth, Education, parity_at_birth_twins_corr_grp, twins_dummy,  agetatbirth, DHS_sample_cluster, Country_birth_date, Country_month, 
                unique_mother_id, birth_date, rescaled_weight, travel_time_median)



################################################################
# Replicate regression in Table 1 column 4 
################################################################

# Replicate regression in Table 1 in column 4 to include it in Table S2 
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Lets get the sample mean of the outcome
ols_fe_dhs_cbd_y_mean <- final_data %>%
  filter(row_number() %in% obs(ols_fe_dhs_cbd)) %>%
  summarize(y_mean = weighted.mean(Facility_based_births*1000, w = rescaled_weight))

# Lets apply the truncation/capping function
ols_fe_dhs_cbd_y_mean <- truncate_to_three_decimals(ols_fe_dhs_cbd_y_mean)



################################################################
# Logistic regression with ALPACA package 
################################################################
 
# Regression
probit_fe_dhs_cbd_alpaca <- alpaca::feglm(Facility_based_births ~ prec_85_count_3d_0_lag + 
                                + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy)  + agetatbirth
                              | DHS_sample_cluster + Country_birth_date  # add FE
                              | DHS_sample_cluster  ,   # cluster std error at this level
                              family = binomial(link = "probit"),
                              data=final_data_subset, 
                              weights = "rescaled_weight" )
summary(probit_fe_dhs_cbd_alpaca)

# Compute average partial effects
probit_fe_dhs_cbd_alpaca.apes <- alpaca::getAPEs(probit_fe_dhs_cbd_alpaca,  panel.structure = "classic")
probit_fe_dhs_cbd_alpaca.apes_summary <- summary(probit_fe_dhs_cbd_alpaca.apes)
probit_fe_dhs_cbd_alpaca.apes_summary

# Apply analytical bias correction -  biasCorr is a post-estimation routine that can be used to substantially reduce the incidental parameter bias problem 
probit_fe_dhs_cbd_alpaca.bc <- biasCorr(probit_fe_dhs_cbd_alpaca, panel.structure = "classic")
summary(probit_fe_dhs_cbd_alpaca.bc)

# Compute bias-corrected average partial effects
# We follow Cruz-Gonzalez et al. (2017) and correct for the incidental parameter bias
# which arises in binary response models (Neyman and Scott, 1948; Fern ́andez-Val and Weidner, 2016; Cruz-Gonzalez et al., 2017)
probit_fe_dhs_cbd_alpaca.bc.apes <- alpaca::getAPEs(probit_fe_dhs_cbd_alpaca.bc,  panel.structure = "classic")
probit_fe_dhs_cbd_alpaca.bc.apes_summary <- summary(probit_fe_dhs_cbd_alpaca.bc.apes) 
probit_fe_dhs_cbd_alpaca.bc.apes_summary


# Lets computing sample mean of the logistic regression
probit_fe_dhs_cbd_alpaca_y_mean <- probit_fe_dhs_cbd_alpaca[["data"]] %>%
  summarize(y_mean = weighted.mean(Facility_based_births*1000, w = rescaled_weight))

# Lets apply the truncation/capping function on the sample mean of the logistic regression
probit_fe_dhs_cbd_alpaca_y_mean <- truncate_to_three_decimals(probit_fe_dhs_cbd_alpaca_y_mean)



################################################################
# Extract coefficients from the logistic regressions
################################################################

# Create coefficient extractor function for texreg
extract.APEs <- function(model) {
  s <- summary(model) # Assume 'model' is your APEs object
  
  # Create lists to store the components that texreg needs
  coef.names <- rownames(s$cm)
  coef <- s$cm[, "Estimate"]*1000 # convert coefficient in per thousand
  se <- s$cm[, "Std. error"]
  pvalues <- s$cm[, "Pr(> |z|)"]
  nobs <- probit_fe_dhs_cbd_alpaca[["nobs"]][["nobs"]]
  
  # Goodness-of-fit measures and names, including number of observations
  gof <- c(nobs) # Add more goodness-of-fit measures if needed
  
  # Goodness-of-fit measures and names, including number of observations
  gof <- c(nobs) # Add more goodness-of-fit measures if needed
  gof.names <- c("Num. obs.") # Add names for other goodness-of-fit measures if added
  gof.decimal <- c(FALSE) # Indicate whether the gof values should be rounded
  
  
  tr <- createTexreg(
    coef.names = coef.names,
    coef = coef,
    se = se,
    pvalues = pvalues,
    gof = gof,
    gof.names = gof.names,
    gof.decimal = gof.decimal
  )
  return(tr)
}

# Extract Average partial effect coefficient for texreg
probit_fe_dhs_cbd_alpaca.apes_summary <- extract.APEs(probit_fe_dhs_cbd_alpaca.apes)
probit_fe_dhs_cbd_alpaca.bc.apes_summary <- extract.APEs(probit_fe_dhs_cbd_alpaca.bc.apes)


################################################################
# Export regression 
################################################################

# Table - Export
ols_fe_dhs_cbd_extract <- texreg::extract(ols_fe_dhs_cbd,   
          include.rsquared = FALSE,
          include.adjrs = FALSE,
          include.fixef_sizes = FALSE,
          include.groups = FALSE,
          include.nobs = TRUE)

texreg(list(ols_fe_dhs_cbd_extract,
            probit_fe_dhs_cbd_alpaca.apes_summary,
            probit_fe_dhs_cbd_alpaca.bc.apes_summary),
       stars = c(0.01, 0.05, 0.1),
       digits = 3,
       custom.header = list("LPM"= 1, "Probit AME" = 2, "Probit AME (bias-corrected)" = 3),
       booktabs = TRUE,
       custom.model.names = c("(1)", "(2)" ,"(3)"),
       custom.gof.rows = list(
         "DHS cluster FE"  = c("Yes", "Yes","Yes"),
         "Country-day of birth FE" = c("Yes", "Yes","Yes"),
         "Maternal and household controls" = c("Yes", "Yes","Yes"),
         "Sample mean" = c(sprintf("%.3f", ols_fe_dhs_cbd_y_mean), sprintf("%.3f", probit_fe_dhs_cbd_alpaca_y_mean), 
                           sprintf("%.3f", probit_fe_dhs_cbd_alpaca_y_mean))),
       custom.coef.map = list("prec_85_count_3d_0_lag" = " N of days over 85 percentiles"),
       caption = "Cumulative days over 80 percentiles thresholds",
       caption.above = TRUE,
       use.packages=FALSE,
       table = FALSE,
       custom.note = "", 
       file = paste(output_path , "Tables/Table_prec_85_count_3d_0_lag_probit_APE.tex", sep = "")
       )



