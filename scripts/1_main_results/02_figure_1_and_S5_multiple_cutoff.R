###################################################################
##### Figure 1 & S5 : Effects of EPEs on Facility-based birth #####
###################################################################

# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- as.data.frame(final_data)
format(object.size(final_data), units = "Mb")

################################################################
# Regression
################################################################

# Set my Facility_based_births from  haven_labelled to numeric again  otherwise the regression loop fail
str(final_data$Facility_based_births)
class(final_data$Facility_based_births)
final_data <- final_data %>%
  mutate(Facility_based_births = as.numeric(Facility_based_births))
str(final_data$Facility_based_births)
class(final_data$Facility_based_births)


# Define the regression function
regress_func <- function(var) {
  feols(as.formula(paste0("Facility_based_births*1000 ~ ", var,
                          " + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date  ")),
        data = final_data_subset,
        cluster = "DHS_sample_cluster",
        weights = ~ rescaled_weight,
        fixef.rm= "singleton")
  }


# Apply the regression function
for(i in 3:14) {
  # Subset the data for the current time window
  windows <- paste0(i, "d")
  required_cols <- c("Facility_based_births", "Wealth", "Education", "parity_at_birth_twins_corr_grp", "twins_dummy", "agetatbirth", "DHS_sample_cluster", "Country_birth_date", "rescaled_weight")
  required_cols <- c(required_cols, paste0("prec_", seq(50, 95, by = 1), "_count", "_", windows, "_0_lag"))
  final_data_subset <- final_data[ , required_cols]
  format(object.size(final_data_subset), units = "Mb")
  
  # To store all regressions from 50 to 95
  results <- list()
  
  # Set up parallel processing
  plan(strategy = "multisession", workers = availableCores() - 1)
  
  # Create the variable names for the current time window
  prec_vars <- paste0("prec_", seq(50, 95, by = 1), "_count", "_", windows,  "_0_lag")
  
  # Apply the regression function in parallel
  results[[windows]] <- future_map(prec_vars, regress_func, .options=furrr_options(seed=TRUE) , .progress = TRUE)
  
  # End the multisession
  plan(sequential)
  
  # Store results from plot
  saveRDS(results, file = paste0("data/processed/regression_results/cutoff_and_time_window/results_",windows,"_0_lag.Rds"))
  }



################################################################
# Extract coefficients, confidence intervals 
################################################################


# Initialize empty list to store data frames
all_data <- list()

for(i in 3:14) {
  # Load the model results
  windows <- paste0(i, "d")
  df <- map_df(readRDS(file = paste0("data/processed/regression_results/cutoff_and_time_window/results_", windows, "_0_lag.Rds"))[[windows]],
               ~ get_model_data(.x),
               .id = "model")

  # Add a list identifier
  df$time_window <- i
  
  # Remove "_xd" from the values of variable term so that they can be overlapped in plot
  df <- df %>%
    mutate(term = gsub(paste0("_", windows), "", term))
  
  # Store the data frame in the list
  all_data[[windows]] <- df
  }

# Combine all data frames
combined <- bind_rows(all_data)

# Filter to include only the coefficients of interest
prec_vars <- paste0("prec_", seq(50, 95, by = 1), "_count",  "_0_lag") 
combined <- combined %>% 
  filter(term %in% prec_vars)


# Convert time_window to a factor for nice color plot
combined$time_window <- as.factor(combined$time_window)

# Rename the cut-off variable name for nicer plot
combined <- combined %>%
  mutate(cut_off = str_replace(term, "prec_(\\d+)_count_0_lag", "\\1th"))


################################################################
# Check if sample mean in Facility based-births is identical in all models
################################################################

# Lets check if we have the same number of observation in all regression
# Base path where RDS files are stored
base_path <- "data/processed/regression_results/cutoff_and_time_window/"

# Initialize empty list to store number of observations
nobs <- list()

# Loop through each day from 3 to 14
for (day in 3:14) {
  # Format the file name and day key
  file_name <- sprintf("results_%dd_0_lag.Rds", day)
  day_key <- sprintf("%dd", day)
  
  # Full path to the RDS file
  file_path <- file.path(base_path, file_name)
  
  # Check if the RDS file exists
  if (file.exists(file_path)) {
    # Read the RDS file
    results <- readRDS(file_path)
    
    # Extract the number of observations and store in the list
    nobs[[sprintf("results_%s_lag_obs", day_key)]] <- sapply(results[[day_key]], function(x) x[["nobs"]])
    
    # Remove the loaded results from memory
    rm(results)
    }
  else {
    # If the file doesn't exist, store NA or some other placeholder
    nobs[[sprintf("results_%s_lag_obs", day_key)]] <- NA
    }
  }


# We have the same number of observation in all regression model so the sample mean in Facility based-births is identical in all models
unique(unlist(nobs)) 

# Run the main regression to identify the observation from which we compute the sample mean that si similar in all models
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Calculate the sample mean of the outcome variable that we will use in the figure
sample_mean <- final_data %>%
  filter(row_number() %in% obs(ols_fe_dhs_cbd)) %>%
  summarize(weighted_mean = weighted.mean(Facility_based_births * 1000, w = rescaled_weight)) %>%
  pull(weighted_mean)

# Check the percentage decrease from baseline of our main results is the same
ols_fe_dhs_cbd[["coefficients"]][["prec_85_count_3d_0_lag"]] / sample_mean * 100


################################################################
# Export Figure 1 and Supplementary Figure S5
################################################################

# Check the maximum effect across precentile for the 3d time window exposure.
combined %>%
  filter(time_window==3) %>%
  sort("estimate")

# Plot combined results for all cut-offs and for time windows 3 to 7 days
ggplot(
  combined %>% filter(as.numeric(as.character(time_window)) >= 3 & as.numeric(as.character(time_window)) <= 7) , aes( x = cut_off, y = estimate, color = as.factor(time_window) )) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", linetype = "dashed") +
  geom_point( position = position_dodge(width = 0.8), size = 1.25) +
  geom_errorbar( aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 1, position = position_dodge(0.8), alpha = 0.8) +
  scale_y_continuous( breaks = seq(-25, 20, 5), limits = c(-25, 5), labels = scales::comma, sec.axis = sec_axis( ~ . * 100 / sample_mean, name = "Change from baseline (%)")) +
  scale_color_scico_d(name   = "Time window", labels = function(x) paste0(x, " days"), palette = "batlow" ) +
  theme_light() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.title        = element_text(face = "bold", size = 14),
    legend.position   = "bottom",
    legend.text       = element_text(size = 14),
    legend.title      = element_text(size = 14, face = "bold"),
    plot.title        = element_text(face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(hjust = 0.5),
    plot.caption      = element_text(hjust = 1),
    panel.background  = element_rect(fill = "transparent"),
    plot.background   = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1, title.position = "left", title.hjust = 0.5)) +
  labs(
    x     = "Percentile threshold for EPE definition",
    y     = "Change in facility-based birth (per 1,000 births)",
    color = "Time window")

# Export Figure 1
ggsave(file = paste(output_path, "Figures/ols_fe_dhs_cbd_cutoff_3to7days_0_lag_combined.pdf", sep = ""), width=16,height=8, bg = "transparent")


# Plot combined results for all cut-offs and for time windows 3 to 14 days
ggplot(
  combined, aes( x = cut_off, y = estimate, color = as.factor(time_window) )) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", linetype = "dashed") +
  geom_point( position = position_dodge(width = 0.8), size = 1.25) +
  geom_errorbar( aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.5, position = position_dodge(0.8), alpha = 0.8) +
  scale_y_continuous( breaks = seq(-25, 20, 5), limits = c(-25, 5), labels = scales::comma, sec.axis = sec_axis( ~ . * 100 / sample_mean, name = "Change from baseline (%)")) +
  scale_color_scico_d(name   = "Time window", labels = function(x) paste0(x, " days"), palette = "batlow" ) +
  theme_light() +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.title        = element_text(face = "bold", size = 14),
    legend.position   = "bottom",
    legend.text       = element_text(size = 14),
    legend.title      = element_text(size = 14, face = "bold"),
    plot.title        = element_text(face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(hjust = 0.5),
    plot.caption      = element_text(hjust = 1),
    panel.background  = element_rect(fill = "transparent"),
    plot.background   = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1, title.position = "left", title.hjust = 0.5)) +
  labs(
    x     = "Percentile threshold for EPE definition",
    y     = "Change in facility-based birth (per 1,000 births)",
    color = "Time window")

# Export Supplementary Figure S5
ggsave(file = paste(output_path, "Figures/ols_fe_dhs_cbd_cutoff_3to14days_0_lag_combined.pdf", sep = ""), width=16,height=8, bg = "transparent")













