################################################################
########     Figure S12 : Country-specific coefficients   ########
################################################################

# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Create special function truncate/Cap to three decimal places
################################################################

# Truncate/Cap to three decimal places
truncate_to_three_decimals_10000 <- function(x) {floor(x * 10000) / 10000} # If I dont put 10000, the rounding fails !!!!!


################################################################
# Regression : Baseline Regression (Table 1 column 4)
################################################################

# Baseline Regression (Table 1 column 4)
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
summary(ols_fe_dhs_cbd)

# Extract the main estimate for the variable of interest from the baseline regression
main_estimate <- ols_fe_dhs_cbd[["coefficients"]][["prec_85_count_3d_0_lag"]]

# Add the coefficients and main estimate of the main results
main_coef <- summary(ols_fe_dhs_cbd)
main_ci <- confint(ols_fe_dhs_cbd, level = .95)
y_mean <- final_data %>%
  filter(row_number() %in% obs(ols_fe_dhs_cbd)) %>%
  summarize(weighted_mean = weighted.mean(Facility_based_births*1000, w = rescaled_weight)) %>%
  pull(weighted_mean)

main_effect_df <- data.frame(Country_Sample =  "Main Results",
                             Estimate = main_coef[["coefficients"]][["prec_85_count_3d_0_lag"]],
                             Std.Error = main_coef[["se"]][["prec_85_count_3d_0_lag"]],
                             Conf.Low = main_ci["prec_85_count_3d_0_lag", 1],
                             Conf.High = main_ci["prec_85_count_3d_0_lag", 2],
                             Sample_Mean = y_mean)

# Computing the coefficient's percentage from sample means
main_effect_df <- main_effect_df %>%
  mutate(PercentageChange = (Estimate / Sample_Mean) * 100) %>%
  rowwise() %>%
  mutate(PercentageChange_trunc = truncate_to_three_decimals_10000(PercentageChange)) %>%
  ungroup() %>% # Don't forget to ungroup after using rowwise()
  as.data.frame()  %>%
  mutate(PercentageChange_trunc = abs(PercentageChange_trunc)) %>%
  mutate(Label = paste0(sprintf("%.2f", PercentageChange_trunc), "%")) 


################################################################
# Regression : Country-specific regression
################################################################


# We create a data frame to store the coefficients for each country excluded
by_country_effects_df <- data.frame(Country_Sample = character(), Estimate = numeric(), Std.Error = numeric(), Conf.Low = numeric(), Conf.High = numeric(), Sample_Mean = numeric(),  stringsAsFactors = FALSE)

# Loop through each country, run the regression with observations from that country only, and store the coefficient of interest
for (country in unique(final_data$CountryName)) {
  data_one_country <- final_data[final_data$CountryName == country, ]
  
  # Run the model with observation from the current country only
  model_one_country <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , data_one_country, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
  
  # Get the coefficients and confidence intervals of the model variables
  coefs <- summary(model_one_country)
  ci <- confint(model_one_country, level = .95)
  
  # Get the national sample mean of Facility based birth per 1'000 live births
  y_mean <- data_one_country %>%
    filter(row_number() %in% obs(model_one_country)) %>%
    summarize(weighted_mean = weighted.mean(Facility_based_births*1000, w = rescaled_weight)) %>%
    pull(weighted_mean)
  
  # Store the results
  by_country_effects_df <- rbind(by_country_effects_df,
                                 data.frame(Country_Sample = country, 
                                            Estimate = coefs[["coefficients"]][["prec_85_count_3d_0_lag"]],
                                            Std.Error = coefs[["se"]][["prec_85_count_3d_0_lag"]], 
                                            Conf.Low = ci["prec_85_count_3d_0_lag", 1], 
                                            Conf.High = ci["prec_85_count_3d_0_lag", 2],
                                            Sample_Mean = y_mean))
}

# Computing the coefficient's percentage of the national sample means for each country
head(by_country_effects_df)
str(by_country_effects_df)
by_country_effects_df <- by_country_effects_df %>%
  mutate(PercentageChange = (Estimate / Sample_Mean) * 100) %>%
  rowwise() %>%
  mutate(PercentageChange_trunc = truncate_to_three_decimals_10000(PercentageChange)) %>%
  ungroup() %>% # Don't forget to ungroup after using rowwise()
  as.data.frame()  %>%
  mutate(PercentageChange_trunc = abs(PercentageChange_trunc)) %>%
  mutate(Label = paste0(sprintf("%.2f", PercentageChange_trunc), "%"))  # Create a label column that formats the truncation with no additional rounding

head(by_country_effects_df)



################################################################
# Export figure 
################################################################

# Define a fixed x position for the percentage labels, which should be slightly greater than the maximum Conf.High
label_x_position <- max(by_country_effects_df$Conf.High, na.rm = TRUE) + 10  # Adjust the offset as necessary


# Plot the effects
by_country_plot_prec_85_count_3d_0_lag <- ggplot(by_country_effects_df, aes(x = Estimate, y = reorder(Country_Sample, Estimate))) +
  geom_point(main_effect_df,  mapping = aes(x = Estimate, y = reorder(Country_Sample, Estimate)), , color = "blue" ) +
  geom_errorbarh(main_effect_df,  mapping = aes(xmin = Conf.Low, xmax = Conf.High), height = 0.2, , color = "blue") +
  geom_point() +
  geom_errorbarh(aes(xmin = Conf.Low, xmax = Conf.High), height = 0.2) +
  geom_text(main_effect_df, mapping = aes(x = label_x_position, y = Country_Sample, label = sprintf("%.1f%%", PercentageChange_trunc)), 
            hjust = 0.5, color = "black", size = 5) +
  geom_text(aes(x = label_x_position, y = Country_Sample, label = sprintf("%.1f%%", PercentageChange_trunc)), 
            hjust = 0.5, color = "black", size = 5) +   # Add percentage over each coefficient
  geom_vline(xintercept = 0, linetype = "dotted") +
  labs(x = "Change in facility-based (per 1,000 births)", y = ""
  ) +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(face = "bold", size = 14),)
by_country_plot_prec_85_count_3d_0_lag


ggsave(file = paste(output_path, "Figures/ols_fe_dhs_cbd_fig_by_country.pdf", sep = ""), width=7, height=10)




