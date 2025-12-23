################################################################
####     Figure 2: Heterogeneity in the effect of EPEs      ####
################################################################

# ################################################################
# # Load data
# ################################################################
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Create function for sample means
################################################################

# Truncate/Cap to three decimal places
truncate_to_three_decimals_10000 <- function(x) {floor(x * 10000) / 10000} # If I dont put 10000, the rounding fails.

# Function to calculate weighted means directly within the pipeline
calculate_weighted_mean_fig2 <- function(model, data, weight_var) {
  mean_value <- data %>%
    filter(row_number() %in% obs(model)) %>%
    summarize(weighted_mean = weighted.mean(.data[[all.vars(formula(model))[1]]]*1000, .data[[weight_var]])) %>% 
    pull(weighted_mean)
  # return(mean_value) # Using the truncation function defined previously
}

# ################################################################
# # Regression
# ################################################################

# Main estimate 
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + 
                          factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                        | DHS_sample_cluster + Country_birth_date , 
                        final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Heterogeneity : Household wealth
ols_fe_dhs_cbd_income <- feols(Facility_based_births*1000 ~ Wealth:prec_85_count_3d_0_lag +
                                 factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth
                               | DHS_sample_cluster + Country_birth_date ,
                               final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Heterogeneity : Travel time quartile
ols_fe_dhs_cbd_tt_quart_bin <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:factor(travel_time_weighted_median_qart_bin) +
                                       factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth 
                                     | DHS_sample_cluster + Country_birth_date ,
                                     final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Heterogeneity : Distance is a big problem
ols_fe_dhs_cbd_perc_acc <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:factor(Perceived_access) + 
                                   factor(Perceived_access) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth 
                                 | DHS_sample_cluster + Country_birth_date ,
                                 final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Heterogeneity : Car Ownership
ols_fe_dhs_cbd_car_truck_all <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:factor(car_truck_all) +
                                        factor(car_truck_all) + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth 
                                      | DHS_sample_cluster + Country_birth_date ,
                                      final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# Heterogeneity : Road length quartile
ols_fe_dhs_cbd_length_m_quart_bin <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag:length_m_quart_bin +
                                             factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth 
                                           | DHS_sample_cluster + Country_birth_date , 
                                           final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")


# ################################################################
# # Compute sample mean for plot 
# ################################################################

# Storing the models in a list for export
model_list <- mget(
  c("ols_fe_dhs_cbd", 
    "ols_fe_dhs_cbd_income" , 
    "ols_fe_dhs_cbd_tt_quart_bin" , 
    "ols_fe_dhs_cbd_perc_acc",
    "ols_fe_dhs_cbd_car_truck_all",
    "ols_fe_dhs_cbd_length_m_quart_bin"
    ))


# Applying the function to each model, keeping names
sample_means <- sapply(model_list,
                       calculate_weighted_mean_fig2, data = final_data, weight_var = "rescaled_weight", USE.NAMES = TRUE)



# ################################################################
# # Create a dataframe to store regression results and its sample mean
# ################################################################

# Converting the sample mean vector to dataframe with a regression name column
sample_means_df <- sample_means %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Regression_name") %>%
  rename(Sample_Mean = ".")  # Rename the column "." to 'Sample_Mean'

# Mapping regression names to a new names for the plot
model_descriptions <- c(
  ols_fe_dhs_cbd = "Main estimate",
  ols_fe_dhs_cbd_income = "Wealth",
  ols_fe_dhs_cbd_tt_quart_bin = "Travel time",
  ols_fe_dhs_cbd_perc_acc = "Perceived access",
  ols_fe_dhs_cbd_car_truck_all = "Car ownership",
  ols_fe_dhs_cbd_length_m_quart_bin = "Road length")

model_descriptions <- model_descriptions %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Regression_name") %>%
  rename(Model = ".")   # Rename the column "." to 'Sample_Mean'


# Adding the descriptive model names
sample_means_df <- sample_means_df %>%
  left_join(model_descriptions, by = "Regression_name")

# Print the updated dataframe
print(sample_means_df)


################################################################
# Extract coefficients, confidence intervals, samples means and decrease in sample means
################################################################

# Define the models and their respective names
models <- list(
  Main = ols_fe_dhs_cbd,
  Wealth = ols_fe_dhs_cbd_income,
  "Travel time" = ols_fe_dhs_cbd_tt_quart_bin,
  "Perceived access" = ols_fe_dhs_cbd_perc_acc,
  "Car ownership" = ols_fe_dhs_cbd_car_truck_all,
  "Road length" = ols_fe_dhs_cbd_length_m_quart_bin
  )

# Custom function to extract the necessary data for our plot
get_model_data_heterogeneity <- function(model, model_name) {
  # Extract the coefficients and their standard errors
  coef <- coef(model)
  se <- sqrt(diag(vcov(model)))
  conf_int <- confint(model)

  # Create a data frame
  df <- data.frame(
    Model = model_name,
    Term = names(coef),
    Estimate = coef,
    Std.Error = se,
    Conf.Low = conf_int[, "2.5 %"],
    Conf.High = conf_int[, "97.5 %"],
    stringsAsFactors = FALSE  # avoid factors to make manipulation easier
    )

  # Filter terms that contain "prec_85_count_3d_0_lag" our variable of interest
  df <- df[grep("prec_85_count_3d_0_lag", df$Term), ]
  # Clean up the term names to be more readable
  df$Term <- gsub("prec_85_count_3d_0_lag", "", df$Term)
  df$Term <- gsub("[:_]", " ", df$Term)
  df$Term <- gsub("^\\s+|\\s+$", "", df$Term) # Trim leading and trailing whitespace
  df
  }

# Apply the function to each model and combine the results
plot_data <- do.call(rbind, lapply(names(models), function(mn) get_model_data_heterogeneity(models[[mn]], mn)))



################################################################
# Prepare figure aesthetics 
################################################################

# Convert 'Model' and 'Term' to factors for ordering in the plot
plot_data$Model <- factor(plot_data$Model, levels = names(models))
plot_data$Term <- factor(plot_data$Term, levels = unique(plot_data$Term))


# Modify levels to ensure the main estimate is labelled correctly
plot_data$Model <- as.character(plot_data$Model)
plot_data$Model[plot_data$Model == "Main"] <- "Main estimate"
plot_data <- plot_data %>%
  mutate(Term = case_when( Model == "Main estimate" ~ "Main estimate",  TRUE ~ Term )) %>%
  mutate(Term_label = Term) %>%
  mutate(Term_label = case_when( Term_label == "Wealthpoorest" ~ "Poorest", 
                           Term_label == "Wealthpoorer" ~ "Poorer", 
                           Term_label == "Wealthmiddle" ~ "Middle", 
                           Term_label == "Wealthricher" ~ "Richer", 
                           Term_label == "Wealthrichest" ~ "Richest", 
                           Term_label == "factor(travel time weighted median qart bin)1" ~ "Travel time Q1", 
                           Term_label == "factor(travel time weighted median qart bin)2" ~ "Travel time Q2", 
                           Term_label == "factor(travel time weighted median qart bin)3" ~ "Travel time Q3", 
                           Term_label == "factor(travel time weighted median qart bin)4" ~ "Travel time Q4", 
                           Term_label == "factor(Perceived access)0" ~ "Distance barrier (No)", 
                           Term_label == "factor(Perceived access)1" ~ "Distance barrier (Yes)", 
                           Term_label == "factor(car truck all)0" ~ "Car/truck owner (No)", 
                           Term_label == "factor(car truck all)1" ~ "Car/truck owner (Yes)", 
                           Term_label == "length m quart bin1" ~ "Road length Q1", 
                           Term_label == "length m quart bin2" ~ "Road length Q2", 
                           Term_label == "length m quart bin3" ~ "Road length Q3", 
                           Term_label == "length m quart bin4" ~ "Road length Q4", 
                           TRUE ~ Term_label ))



# Add a column indicating the sample mean before computing the coefficient's percentage from sample means
plot_data <- plot_data %>%
  left_join(sample_means_df, by= "Model")

# Computing the coefficient's percentage from sample means
plot_data <- plot_data %>%
  mutate(PercentageChange = (Estimate / Sample_Mean) * 100) %>%
  rowwise() %>%
  mutate(PercentageChange_trunc = truncate_to_three_decimals_10000(PercentageChange)) %>%
  ungroup() %>% # Don't forget to ungroup after using rowwise()
  as.data.frame()  %>%
  mutate(Label = paste0(sprintf("%.2f", PercentageChange_trunc), "%"))  # Create a label column that formats the truncation with no additional rounding

head(plot_data)



# Reorder the factor levels for Term to match the order we want in the plot
plot_data$Term_label <- factor(plot_data$Term_label, levels = unique(plot_data$Term_label))

#Set the Model order to not be on the alphabetical order
plot_data$Model <- factor(plot_data$Model, levels = unique(plot_data$Model))

# Add vertical dashed lines at specific positions
vertical_line_positions <- c(1.5, 6.5, 10.5, 12.5, 14.5)

# Calculate additional spacing for labels based on the range of the confidence interval
plot_data <- plot_data %>%
  mutate(
    LabelPos = ifelse(
      Estimate >= 0,
      Conf.High + (0.15 * (Conf.High - Conf.Low)),  # Increase offset for positive estimates
      Conf.Low - (0.15 * (Conf.High - Conf.Low))    # Decrease offset for negative estimates
    )
  )


################################################################
# Export figure 
################################################################

# Load colors from scico package
batlow10 <- scico::scico(12, palette = "batlow")

ggplot(plot_data, aes(x = Term_label, y = Estimate, ymin = Conf.Low, ymax = Conf.High, color = Model)) +
  geom_pointrange(size = 0.5, linewidth = 1) +
  geom_text(aes(y = LabelPos, label = sprintf("%.1f%%", PercentageChange_trunc)), vjust = 0, hjust = 0.5, color = "black", size = 4) +
  geom_vline(xintercept = vertical_line_positions, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_color_manual(values = batlow10[c(1, 3, 5, 7, 9, 11)]) +
  theme_minimal() +
  guides(color = guide_legend(title = NULL, nrow = 1)) +  # This will remove the title from the legend
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate the x axis labels for better readability
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 16),  # Increase legend text size
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 10),  # Adjust text size as needed
    axis.line.x = element_line(color = "black", linewidth = 0.5),  # Add black line on the x-axis
    axis.line.y = element_line(color = "black", linewidth = 0.5)   # Add black line on the y-axis
  ) +
  labs(y = "Change in facility-based birth (per 1,000 births)",
       x="" # remove the x axis label that is fct_rev(Term_label)
  )


# Print the plot
ggsave(file = paste(output_path, "Figures/Heterogeneity_EPE.pdf", sep = ""), width=18, height=7)




