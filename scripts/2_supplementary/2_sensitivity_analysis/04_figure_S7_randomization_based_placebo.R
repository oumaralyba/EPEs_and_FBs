################################################################
###   Figure S7 : Placebo - randomization based placebo      ###
################################################################


# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- as.data.frame(final_data)



# ################################################################
# Subset the data to include only necessary columns far faster computation 
# ################################################################


# Create a Children id so that I reshape the data to long format before randomizing exposure to EPE days and non EPE days for each live birth considering a time window of +-14 days around his own birth date and DHS cluster location
final_data <- final_data %>%
  mutate(children_id = row_number())
n_distinct(final_data$children_id)

# Subset the data 
chirps_columns <- c(
  "chirps_14_lag", "chirps_13_lag", "chirps_12_lag", "chirps_11_lag", "chirps_10_lag",
  "chirps_9_lag", "chirps_8_lag", "chirps_7_lag", "chirps_6_lag", "chirps_5_lag",
  "chirps_4_lag", "chirps_3_lag", "chirps_2_lag", "chirps_1_lag", "chirps_0_lag",
  "chirps_1_lead", "chirps_2_lead", "chirps_3_lead", "chirps_4_lead", "chirps_5_lead",
  "chirps_6_lead", "chirps_7_lead", "chirps_8_lead", "chirps_9_lead", "chirps_10_lead",
  "chirps_11_lead", "chirps_12_lead", "chirps_13_lead", "chirps_14_lead"
)
required_column <- c( "children_id", "Facility_based_births", "Wealth", "Education", "parity_at_birth_twins_corr_grp", "twins_dummy", "agetatbirth", "DHS_sample_cluster", "Country_birth_date", "rescaled_weight")
cut_off <- c("precipitation_p85")
all_cols <- c(chirps_columns, required_column, cut_off)
final_data_subset <- final_data[, all_cols, drop = FALSE]



# ################################################################
# Create a function for randomization
# ################################################################

# Function to randomize precipitation within window
randomize_precip <- function(df) {
  
  data_long <- df %>%
    pivot_longer(
      cols = chirps_columns,  # only gather chirps columns
      names_to = "precipitation_variable",
      values_to = "value")
  
  data_shuffled <- data_long %>%
    group_by(children_id) %>%
    mutate(shuffled_value = sample(value, replace = FALSE)) %>%
    ungroup()
  
  data_wide_shuffled <- data_shuffled %>%
    dplyr::select(-value) %>%
    pivot_wider(
      names_from = precipitation_variable,
      values_from = shuffled_value)
  
  data_wide_shuffled <- data_wide_shuffled %>%
    mutate(prec_85_count_3d_0_lag := rowSums(dplyr::select(., paste0("chirps_", 0:2, "_lag")) > precipitation_p85))
  
  # Regression with permutation
  premutation_model <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag
                             + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | 
                               DHS_sample_cluster + Country_birth_date , 
                             data_wide_shuffled, 
                             cluster = "DHS_sample_cluster", 
                             weights = ~ rescaled_weight,
                             fixef.rm= "singleton")
  
  # Return the coefficient of the placebo exposed variable
  return(premutation_model[["coefficients"]][["prec_85_count_3d_0_lag"]]) 
}



# ################################################################
# Regression with randomization - 10'000 times 
# ################################################################

# Set seed for reproducibility
set.seed(1284)  

# Randomization sample,  run regression and store coefficients results 10'000 times 
randomized_dfs_10000 <- mc_replicate(10000, randomize_precip(final_data_subset), mc.cores = 7)
saveRDS(randomized_dfs_10000, file = "data/processed/placebo/randomized_dfs_10000.Rds") # randomized_dfs_10000 <- readRDS("data/processed/placebo/randomized_dfs_10000.Rds")



################################################################
# Export figure 
################################################################

# Extract main regression coefficient (Table 1, column 4) for plot
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Plot the results
histogram <- gghistogram(randomized_dfs_10000, fill = "lightgray",
                         rug = F, bins = 200) + 
  theme_minimal() +
  theme( axis.text = element_text(size = 14),
         axis.title = element_text(size = 14, face = "bold"))


# Add vertical lines for the main coefficient
histogram <- histogram + 
  geom_vline(xintercept =ols_fe_dhs_cbd[["coefficients"]][["prec_85_count_3d_0_lag"]] , linetype = "solid", color = "#001959", linewidth = 0.9) 


# Calculate specific quantiles and mean of the placebo coefficients
quantiles <- quantile(randomized_dfs_10000, probs = c(0.005, 0.025, 0.05, 0.95, 0.975, 0.995))

quantiles
mean(randomized_dfs_10000)

# Red dashed lines for percentiles 0.5%, 2.5%, 5%, 95%, 97.5%, 99.5% 
histogram <- histogram +
  geom_vline(xintercept = quantiles["0.5%"], linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_vline(xintercept = quantiles["2.5%"], linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_vline(xintercept = quantiles["5%"], linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_vline(xintercept = quantiles["95%"], linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_vline(xintercept = quantiles["97.5%"], linetype = "dashed", color = "red", linewidth = 0.6) +
  geom_vline(xintercept = quantiles["99.5%"], linetype = "dashed", color = "red", linewidth = 0.6)


# Add x-axis label
histogram <- histogram + labs(x = "Change in facility-based birth (per 1,000 births)")

# Print the histogram with quantile lines
print(histogram)

#Save the histogram with quantile lines
ggsave(file = paste(output_path, "Figures/EPEs_85_placebo_distribution_10_000_times.pdf", sep = ""), width=12, height=6, units = "in")


################################################################
# Interpet the figure 
################################################################

# Step 1: Store my baseline coefficients from Table 1 column 4 
observed_stat <- ols_fe_dhs_cbd[["coefficients"]][["prec_85_count_3d_0_lag"]] 

# Step 2: Calculate the p-value against the placebo coefficients
p_value <- mean(randomized_dfs_10000 <= observed_stat)
p_value # p-value= 0.0029
table(randomized_dfs_10000 > observed_stat)

