################################################################
### Figure S10 : Acute and Sustained rainfall exposure model ###
################################################################


#################################################################
## Load data
#################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- as.data.frame(final_data)
format(object.size(final_data), units = "Mb")

#################################################################
# # Define accumulation periods and time -windows
#################################################################

# Define accumulation periods nd time -windows
accumulation_days <- 3:14


################################################################
# Regressions for Sustained rainfall exposure models
################################################################

# Set Facility_based_births from haven_labelled to numeric
str(final_data$Facility_based_births)
class(final_data$Facility_based_births)
final_data <- final_data %>%
  mutate(Facility_based_births = as.numeric(Facility_based_births))

# Define the regression function
regress_func <- function(var) {
  feols(as.formula(paste0("Facility_based_births*1000 ~ ", var,
                          " + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date")),
        data = final_data_subset,
        cluster = "DHS_sample_cluster",
        weights = ~ rescaled_weight,
        fixef.rm = "singleton")
}

# Apply the regression function for each time window
for(i in accumulation_days) {
  # Subset the data for the current time window
  required_cols <- c("Facility_based_births", "Wealth", "Education", "parity_at_birth_twins_corr_grp", 
                     "twins_dummy", "agetatbirth", "DHS_sample_cluster", "Country_birth_date", "rescaled_weight")
  required_cols <- c(required_cols, paste0("epe_", i, "day_p", seq(50, 99, by = 1)))
  final_data_subset <- final_data[, required_cols]
  format(object.size(final_data_subset), units = "Mb")
  
  # To store all regressions from 50 to 99
  results <- list()
  
  # Set up parallel processing
  plan(strategy = "multisession", workers = availableCores() - 1)
  
  # Create the variable names for the current time window
  epe_vars <- paste0("epe_", i, "day_p", seq(50, 99, by = 1))
  
  # Apply the regression function in parallel
  results[[paste0(i, "day")]] <- future_map(epe_vars, regress_func, .options = furrr_options(seed = TRUE), .progress = TRUE)
  
  # End the multisession
  plan(sequential)
  
  # Store results
  saveRDS(results, file = paste0("data/processed/regression_results/accumulation_threshold_wet_period/results_", i, "day_epe.Rds")) 
  # results <- readRDS(file = paste0("data/processed/regression_results/accumulation_threshold_wet_period/results_4day_epe.Rds"))
  
  # Print completion message for the current time window
  cat(paste("Completed regressions for all percentiles (50th-99th) for", i, "day accumulation period.\n"))
}


#################################################################
# Extract coefficients, confidence intervals of Sustained rainfall exposure models
#################################################################

# Initialize empty list to store data frames
all_data <- list()

for(i in accumulation_days) {
  # Load the model results
  df <- map_df(readRDS(file = paste0("data/processed/regression_results/accumulation_threshold_wet_period/results_", i, "day_epe.Rds"))[[paste0(i, "day")]],
               ~ get_model_data(.x),
               .id = "model")
  
  # Add a list identifier
  df$time_window <- i
  
  # Store the data frame in the list
  all_data[[paste0(i, "day")]] <- df
}

# Combine all data frames
combined <- bind_rows(all_data)

# Filter to include only the coefficients of interest
epe_vars <- paste0("epe_", rep(accumulation_days, each = length(seq(50, 95, by = 1))), "day_p", seq(50, 95, by = 1))
combined <- combined %>% 
  filter(term %in% epe_vars)

# Convert time_window to a factor for nice color plot
combined$time_window <- as.factor(combined$time_window)

# Extract percentile from the variable name for nicer plot
combined <- combined %>%
  mutate(cut_off = str_extract(term, "p(\\d+)") %>% str_replace("p", "") %>% paste0("th"))


################################################################
# Extract coefficients, confidence intervals of Figure 1 (Acute rainfall exposure model )
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
combined_figure_1 <- bind_rows(all_data)

# Filter to include only the coefficients of interest
prec_vars <- paste0("prec_", seq(50, 95, by = 1), "_count",  "_0_lag") 
combined_figure_1 <- combined_figure_1 %>% 
  filter(term %in% prec_vars)


# Convert time_window to a factor for nice color plot
combined_figure_1$time_window <- as.factor(combined_figure_1$time_window)

# Rename the cut-off variable name for nicer plot
combined_figure_1 <- combined_figure_1 %>%
  mutate(cut_off = str_replace(term, "prec_(\\d+)_count_0_lag", "\\1th"))



################################################################
# Merge output from Acute and Sustained rainfall exposure model for comparison plot
# (merge combined with combined_figure_1 )
################################################################

# Append combined_figure_1 to combined and create a variable that differentiates the two data frames
head(combined)
head(combined_figure_1)
combined <- combined %>% mutate(source = "accumulation")
combined_figure_1 <- combined_figure_1 %>% mutate(source = "time")

# Bind both datasets
combined_complete <- bind_rows(combined, combined_figure_1)

# Reshape to wide format by cut_off and time_window
combined_wide <- combined_complete %>%
  select(cut_off, time_window, estimate, conf.low, conf.high, source) %>%
  pivot_wider(
    names_from = source,
    values_from = c(estimate, conf.low, conf.high),
    names_sep = "_"
  )

# Filter selected cutoffs
selected_cutoffs <- paste0(seq(50, 95, by = 5), "th")
combined_filtered <- combined_wide %>% filter(cut_off %in% selected_cutoffs)


################################################################
# Figure S10 : Estimated changes in facility-based births under acute and sustained rainfall exposure models
################################################################

# Create a new label variable for cut_off
combined_filtered <- combined_filtered %>%
  mutate(cut_off_label = paste0(cut_off, " percentile"),
         cut_off_label = factor(cut_off_label, 
                                levels = paste0(sort(unique(cut_off)), " percentile")))


# Plot Comparison of % Change between Accumulation and Time Window Models with facet_wrap by percentile threshold cutoff
ggplot(combined_filtered, 
       aes(x = estimate_accumulation, y = estimate_time, color = factor(time_window), label = time_window)) +
  geom_errorbar(aes(ymin = conf.low_time, ymax = conf.high_time), width = 1.25, alpha = 0.9) +
  geom_errorbarh(aes(xmin = conf.low_accumulation, xmax = conf.high_accumulation), height = 1.25, alpha = 0.9) +
  geom_point(size = 2, alpha = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",linewidth = 0.5, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", linewidth = 0.5, color = "black", alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = 0.5, color = "black", alpha = 0.9) +
  scale_color_scico_d(name = "Sustained and acute \ntime window exposure", direction = -1, labels = function(x) paste0(x, " days"), palette = "roma") + # "roma" or "batlow"
  scale_x_continuous(breaks = seq(-40, 20, by = 10), limits = c(-40, 20)) +  labs(
    x = "Change in facility-based birth (per 1,000 births) \nunder sustained (accumulation-window) rainfall exposure model specification",
    y = "Change in facility-based birth (per 1,000 births) \nunder acute (time-window) rainfall exposure model specification",
    color = "Sustained and acute \ntime window exposure"
  ) +
  facet_wrap(~ cut_off_label, ncol = 2, scales = "free_x") +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(face = "bold", size = 20),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.25),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "transparent", colour = NA),
    strip.text = element_text(hjust = 0, size = 20, colour = "black")) +
  guides(color = guide_legend(nrow = 1, title.position = "left", title.hjust = 0.5, override.aes = list(size = 4)))



ggsave(file = paste(output_path, "Figures/ols_fe_dhs_cbd_accumulation_vs_time_window_models.pdf", sep = ""),width=20,height=8*3, units = "in", dpi = 300)





