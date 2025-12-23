################################################################
########     Figure S6 : Distributed lag linear model   ########
########      (Anticipation and spillover effect)       ########
################################################################

# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()

################################################################
# Regression : Distributed lag regression 
################################################################

# Figure S1 - Regression
ols_fe_dhs_cbd_85 <- feols(Facility_based_births*1000 ~ 
                             prec_85_count_3d_4_lead + prec_85_count_3d_3_lead + 
                             prec_85_count_3d_2_lead + prec_85_count_3d_1_lead +
                             prec_85_count_3d_0_lag + prec_85_count_3d_1_lag + 
                             prec_85_count_3d_2_lag + prec_85_count_3d_3_lag + 
                             prec_85_count_3d_4_lag +
                             factor(Wealth) + factor(Education) + 
                             factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + 
                             agetatbirth |
                             DHS_sample_cluster + Country_birth_date, 
                           final_data, cluster = "DHS_sample_cluster", 
                           weights = ~ rescaled_weight, fixef.rm= "singleton")


################################################################
# Extract coefficients, confidence intervals and rename coefficients
################################################################

# Custom function to rename terms
rename_terms <- function(term) {
  if (grepl("lead$", term)) {
    lead_time <- as.numeric(gsub(".*_(\\d+)_lead$", "\\1", term))
    return(paste("Lead", lead_time))  # Negative for sorting in reverse order
  } else if (grepl("lag$", term)) {
    lag_time <- as.numeric(gsub(".*_(\\d+)_lag$", "\\1", term))
    return(paste("Lag", -lag_time))
  } else {
    return(term)
  }
}

# Extract the necessary data for our plot 
get_model_data_DLM <- function(model, terms) {
  coef <- coef(model)
  se <- sqrt(diag(vcov(model)))
  conf_int <- confint(model)
  
  data_frame <- data.frame(
    Term = names(coef),
    Estimate = coef,
    Std.Error = se,
    Conf.Low = conf_int[, "2.5 %"],
    Conf.High = conf_int[, "97.5 %"]
  )
  
  # Filter to include only the coefficients of interest
  data_frame <- data_frame %>%
    filter(Term %in% terms) %>%
    mutate(
      Term = sapply(Term, rename_terms), # Apply custom renaming function
      Order = as.numeric(sub(".* ", "", Term)) # Extract numeric part for ordering
    ) %>%
    arrange(Order) # Arrange by order
  
  # Factor the terms with levels in the desired order
  data_frame$Term <- factor(data_frame$Term, levels = data_frame$Term)
  
  return(data_frame)
  }

# Specify the terms you want to plot (original order)
terms_to_plot <- c(
  "prec_85_count_3d_4_lead", "prec_85_count_3d_3_lead", 
  "prec_85_count_3d_2_lead", "prec_85_count_3d_1_lead", 
  "prec_85_count_3d_0_lag",  "prec_85_count_3d_1_lag", 
  "prec_85_count_3d_2_lag",  "prec_85_count_3d_3_lag", 
  "prec_85_count_3d_4_lag"
                 )

# Extract and prepare the data for plotting
plot_data <- get_model_data_DLM(ols_fe_dhs_cbd_85, terms_to_plot)




################################################################
# Export figure 
################################################################

# Define new labels for plot
new_labels <- c(
  "Lag -4" = "[T-14 ; T-12]",
  "Lag -3" = "[T-11 ; T-9]",
  "Lag -2" = "[T-8 ; T-6]",
  "Lag -1" = "[T-5 ; T-3]",
  "Lag 0" = "[T-2 ; T]",
  "Lead 1" = "[T+1 ; T+3]",
  "Lead 2" = "[T+4 ; T+6]",
  "Lead 3" = "[T+7 ; T+9]",
  "Lead 4" = "[T+10 ; T+12]"
)

# Create the plot
ggplot(plot_data, aes(x = Term, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Conf.Low, ymax = Conf.High), width = 0.2) +
  theme_light() +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(angle = 0,  vjust = 1, size = 14),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.background = element_rect(fill = "transparent", colour = NA),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    # panel.grid.major = element_line(linewidth = 0.05, linetype = 'dashed', colour = "gray")
  ) +
  labs(x = "Three day intervals around day birth",
       y = "Change in facility-based (per 1,000 births)",
       # title = "Effect on Facility Based Births per 1000"
       ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_x_discrete(labels = new_labels)   # Apply custom labels

ggsave(file = paste(output_path, "Figures/Figure_prec_85_count_DLM.pdf", sep = ""), width=15, height=8)




