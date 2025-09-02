################################################################
####  Figure S3 : Multiple  ETCCDI indices and time window  ####
################################################################


# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()
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
  windows <- i
  required_cols <- c("Facility_based_births", "Wealth", "Education", "parity_at_birth_twins_corr_grp", "twins_dummy", "agetatbirth", "DHS_sample_cluster", "Country_birth_date", "rescaled_weight")
  required_cols <- c(required_cols, paste0("R", seq(1, 25, by = 1), "mm_tot_lag", windows))
  final_data_subset <- final_data[ , required_cols]
  format(object.size(final_data_subset), units = "Mb")
  
  # To store all regressions
  results <- list()
  
  # Set up parallel processing
  plan(strategy = "multisession", workers = availableCores() - 1)
  
  # Create the variable names for the current time window
  prec_vars <-  paste0("R", seq(1, 25, by = 1), "mm_tot_lag", windows)
  
  # Apply the regression function in parallel
  results[[windows]] <- future_map(prec_vars, regress_func, .options=furrr_options(seed=TRUE) , .progress = TRUE)
  
  # End the multisession
  plan(sequential)
  
  # Store results from plot
  saveRDS(results, file = paste0("data/processed/regression_results/cutoff_and_time_window_ETCCDI/results__ETCCDI",windows,".Rds"))
  }



################################################################
# Extract coefficients, confidence intervals, and rename coefficients names
################################################################

# Initialize empty list to store data frames
all_data <- list()

for(i in 3:14) {
  # Load the model results
  windows <- i
  df <- map_df(readRDS(file = paste0("data/processed/regression_results/cutoff_and_time_window_ETCCDI/results__ETCCDI", windows, ".Rds"))[[windows]], 
               ~ get_model_data(.x), 
               .id = "model")
  
  # Add a list identifier
  df$time_window <- i
  
  # Store the data frame in the list
  all_data[[windows]] <- df
  }

# Combine all data frames
combined <- bind_rows(all_data)

# Filter to keep only the coefficients of interest
prec_vars <- c()
# Loop through the lag values
for (lag in 3:14) {
  prec_vars <- c(prec_vars, paste0("R", 1:25, "mm_tot_lag", lag))
  }
combined <- combined %>% 
  filter(term %in% prec_vars)


# Convert time_window to a factor for nice color plot
combined$time_window <- as.factor(combined$time_window)

# Rename the cut-off variable name for nicer plot
combined <- combined %>%  mutate(cut_off = as_factor(str_extract(prec_vars, "\\d+"))) 


################################################################
# Export figure 
################################################################


# Plot with with Multiple Cutoff and time exposure ranging from 3 days to 14 days
ggplot(combined, aes(x = cut_off, y = estimate, color = as.factor(time_window))) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "black", linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.01, position = position_dodge(0.8), alpha = 0.8 ) +
  geom_point(position = position_dodge(width = 0.8), size = 1.25) +
  scale_y_continuous(breaks = seq(-25, 20, 5), labels = scales::comma) +
  scale_color_viridis_d(name = "Time window", labels = function(x) paste0(x, " days"), option = "turbo") +
  #theme_minimal(base_size = 14) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    # panel.grid.major = element_line(linewidth = 0.05, linetype = 'dashed', colour = "gray")
  ) +
  guides(color = guide_legend(nrow = 1, title.position = "left", title.hjust = 0.5)) +
  labs(
    x = "RXmm threshold for EPE definition",
    y = "Change in facility-based (per 1,000 births)",
    color = "Time window"
  )

ggsave("output/Figures/ols_fe_dhs_cbd_n_cutoff_nd_0_lag_combined_ETCCDI.pdf",width=16,height=8)



# Plot with with Multiple Cutoff and one time exposure windows of 3 days
ggplot(combined %>% filter(time_window==3), aes(x = cut_off, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = viridis(1, option = "turbo", begin = 0), position = position_dodge(0.8), alpha = 0.8 ) +
  geom_point(position = position_dodge(width = 0.8), color = viridis(1, option = "turbo", begin = 0),  size = 1.5) +
  scale_y_continuous(breaks = seq(-25, 20, 5), labels = scales::comma) +
  scale_color_viridis_d(name = "Time window", labels = function(x) paste0(x, " days"), option = "turbo") +
  #theme_minimal(base_size = 14) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    # panel.grid.major = element_line(linewidth = 0.05, linetype = 'dashed', colour = "gray")
  ) +
  guides(color = guide_legend(nrow = 1, title.position = "left", title.hjust = 0.5)) +
  labs(
    x = "RXmm threshold for EPE definition",
    y = "Change in facility-based (per 1,000 births)"
  )

ggsave("output/Figures/ols_fe_dhs_cbd_n_cutoff_ETCCDI.pdf",width=16,height=8)






















