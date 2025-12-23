####################################################################################
# Supplementary Information: Data and descriptive statistics
####################################################################################

####################################################################################
# Context:
# This script generates descriptive statistics tables and figures for the
# "Data and Descriptive Statistics" section of the supplementary material.
# 
# The script produces the following outputs:
# - Table S1: DHS country-wave table
# - Table S2: Summary statistics of key variables
# - Table S3: Summary statistics by outcome
# - Table S4: Summary statistics by outcome across countries
# - Table S5: Summary statistics by EPE exposure across countries
# - Figure S1: Maps of facility-based births (mean and standard deviation)
# - Figure S2: Maps of EPE cut-off values and standard deviation of EPE exposure
# - Figure S3: Distribution of 85th percentile cutoff values
# - Figure S4: Maps of EPE cut-off values for multiple percentiles (60th, 70th, 80th, 90th)
####################################################################################


# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Table S1: DHS country-wave table Table
################################################################

# Export Table of DHS survey waves used 
dhs_table <- final_data %>%
  group_by(SurveyId) %>%
  mutate(birth_range = paste(min(birth_date), max(birth_date)),
         Number_of_births = n()) %>%
  ungroup() %>%
  distinct(SurveyId, CountryName, SurveyYearLabel, Number_of_births, birth_range) %>%
  dplyr::select(CountryName, SurveyYearLabel, Number_of_births, birth_range) %>% 
  arrange(CountryName) %>%
  rename(
    `Country Name` = CountryName,
    `Survey Year` = SurveyYearLabel,
    `Number of Births` = Number_of_births,
    `Birth Range` = birth_range
  )


# Create a flextable
dhs_table_ft <- flextable::flextable(dhs_table)
dhs_table_ft <- flextable::autofit(dhs_table_ft)
save_as_image(dhs_table_ft, 
              path = "output/Tables/dhs_country_table.png",
              res = 300
              )


################################################################
# Compute number of DHS cluster locations
################################################################

length(unique(final_data$DHS_sample_cluster)) # There is 12 948 DHS cluster locations as mentionned in the abstract


################################################################
# Table S2: : Summary statistics
################################################################


summary_table <-  final_data %>%
  as.data.frame() %>%
  dplyr::select(Facility_based_births, prec_85_count_3d_0_lag, R20mm_tot_lag3,
                Wealth, Education, parity_at_birth_twins_corr_grp, twins_dummy, agetatbirth, Perceived_access, car_truck_all) %>%
  mutate(Wealth = as.numeric(Wealth),
         Education = as.numeric(Education),
         parity_at_birth_twins_corr_grp = as.numeric(parity_at_birth_twins_corr_grp))

stargazer(summary_table, summary = TRUE,
          title="Descriptive statistics", digits=4, type="latex",
          out.header = FALSE, align=TRUE,
          order = c("Facility_based_births", "prec_85_count_3d_0_lag"),
          covariate.labels = c("Facility-based birth",
                               "N of days over 85 percentiles", "N of days over 20 mm/day",
                               "Wealth", "Education", "Parity at birth", "Twins", "Age at birth",
                                "Perceived distance to a health facility as barrier access", "Car/truck-owning households"),
          iqr= TRUE,
          float = FALSE,
          out =  paste(output_path , "Tables/summary_statistic.tex", sep = "")
          ) 




################################################################
# Table S3 : Summary statistics by Outcome 
################################################################

# Use tbl_summary to create a summary table
summary_by_outcome <- tbl_summary(
  final_data %>%
    mutate(Facility_based_births =  haven::as_factor(Facility_based_births),
           Perceived_access =  haven::as_factor(Perceived_access),
           car_truck_all =  haven::as_factor(car_truck_all)) %>%
    mutate(length_m_quart_bin = recode_factor(length_m_quart_bin,
                                              "1" = "Quartile 1",
                                              "2" = "Quartile 2",
                                              "3" = "Quartile 3", 
                                              "4" = "Quartile 4")) %>%
    set_variable_labels(length_m_quart_bin = "Road length (quartile bins)") %>%
    mutate(travel_time_weighted_median_qart_bin = recode_factor(travel_time_weighted_median_qart_bin,
                                                                "1" = "Quartile 1",
                                                                "2" = "Quartile 2",
                                                                "3" = "Quartile 3", 
                                                                "4" = "Quartile 4")) %>%
    set_variable_labels(travel_time_weighted_median_qart_bin = "Travel time to the nearest health facility (quartile bins)") %>%
    dplyr::select(
      Facility_based_births,
      prec_85_count_3d_0_lag,
      R20mm_tot_lag3,
      Wealth,
      Education,
      parity_at_birth_twins_corr_grp, 
      twins_dummy,
      travel_time_weighted_median_qart_bin,
      Perceived_access,
      car_truck_all,
      length_m_quart_bin
    ),
  by = "Facility_based_births",
  # specify the grouping variable
  type = list(
    all_continuous() ~ "continuous2",
    # to get mean (SD) and median [IQR] for any continuous variables
    all_categorical() ~ "categorical" # to get counts and percentages for categorical variables
  ),
  label = list(
    prec_85_count_3d_0_lag ~ "N of days over 85 percentiles",
    R20mm_tot_lag3 ~ "N of days over 20 mm/day",
    Wealth ~ "Wealth",
    Education ~ "Education",
    parity_at_birth_twins_corr_grp ~ "Parity at birth",
    twins_dummy ~ "Twins",
    travel_time_weighted_median_qart_bin ~ "Travel time to the nearest health facility (quartile bins)",
    Perceived_access ~ "Perceived distance to a health facility as barrier access",
    car_truck_all ~ "Car/truck-owning households", 
    length_m_quart_bin ~"Road length (quartile bins)"
  ),
  digits = everything() ~ c(0, 2), # Set digits for percentages to 2
  percent = "row",
  missing = "no" # to exclude missing data from the summary statistics
) 



# Add overall column to the table
summary_by_outcome <- add_overall(summary_by_outcome) %>%
  modify_header(label ~ "", 
                all_stat_cols() ~ "**{level}** {style_number(n)} ({style_number(p, scale =100,  digits = 2)}%)" #  because smaller percentages are rounded to digits + 1 places.
    ) %>%
  modify_footnote(everything() ~ NA_character_)

# Print the table
print(summary_by_outcome)

# Save the table as png and set explicit text color to black
summary_by_outcome_gt_table <- summary_by_outcome %>%
  as_gt() %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = list(cells_body(), cells_column_labels(), cells_title())
    )
gt::gtsave(summary_by_outcome_gt_table, paste(output_path , "Tables/summary_statistic_by_outcome.png", sep = ""), vwidth = 1500, vheight = 1000)



################################################################
# Table S4 : Summary statistics by Outcome across country only
################################################################

# Use tbl_summary to create a summary table
summary_outcome_by_country <- tbl_summary(
  final_data %>%
    mutate(Facility_based_births =  haven::as_factor(Facility_based_births)) %>%
    dplyr::select(
      Facility_based_births,
      CountryName
    ),
  by = "Facility_based_births",
  # specify the grouping variable
  type = list(
    all_continuous() ~ "continuous2",
    # to get mean (SD) and median [IQR] for any continuous variables
    all_categorical() ~ "categorical" # to get counts and percentages for categorical variables
  ),
  label = list(
    CountryName ~ "Country"
  ),
  digits = everything() ~ c(0, 2), # Set digits for percentages to 2
  percent = "row",
  missing = "no" # to exclude missing data from the summary statistics
) 


# Add overall column to the table
summary_outcome_by_country <- add_overall(summary_outcome_by_country) %>%
  modify_header(label ~ "", 
                all_stat_cols() ~ "**{level}** {style_number(n)} ({style_number(p, scale =100,  digits = 2)}%)" #  because smaller percentages are rounded to digits + 1 places.
  ) %>%
  modify_footnote(everything() ~ NA_character_)

# Print the table
print(summary_outcome_by_country)

# Save the table as png and set explicit text color to black
summary_outcome_by_country_gt_table <- summary_outcome_by_country %>%
  as_gt() %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = list(cells_body(), cells_column_labels(), cells_title()))

gt::gtsave(summary_outcome_by_country_gt_table, paste(output_path , "Tables/summary_statistic_by_outcome_across_country.png", sep = ""), vwidth = 1500, vheight = 1000)



################################################################
# Table S5 : Summary statistics by exposure-country 
################################################################

# Use tbl_summary to create a summary table
summary_exposure_by_country<- tbl_summary(
  final_data %>%
    dplyr::select( prec_85_count_3d_0_lag, CountryName) %>%
    set_value_labels(prec_85_count_3d_0_lag = c("0 day" = 0, "1 day" = 1, "2 days"=2, "3 days"=3 )) %>%
    mutate(prec_85_count_3d_0_lag =  haven::as_factor(prec_85_count_3d_0_lag)),
  by = "prec_85_count_3d_0_lag", # specify the grouping variable
  type = list(
    all_continuous() ~ "continuous2", # to get mean (SD) and median [IQR] for any continuous variables
    all_categorical() ~ "categorical" # to get counts and percentages for categorical variables
  ),
  label = list(
    CountryName ~"Country"),
  digits = everything() ~ c(0, 2), # Set digits for percentages to 2
  percent ="row",
  missing = "no" # to exclude missing data from the summary statistics
  
)

# Add overall column to the table  ((340)/256101) = 0.001327601
summary_exposure_by_country <- add_overall(summary_exposure_by_country) %>%
  modify_header(label ~ "", 
    all_stat_cols() ~ "**{level}** {style_number(n)} ({style_number(p, scale =100,  digits = 2)}%)" #  because smaller percentages are rounded to digits + 1 places.
    ) %>%
  modify_spanning_header(all_stat_cols() ~ "**Day wit EPE**")  %>%
  modify_footnote(everything() ~ NA_character_)

# Print the table
print(summary_exposure_by_country)

# Save the table as png and set explicit text color to black
summary_exposure_by_country_gt_table <- summary_exposure_by_country %>%
  as_gt() %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = list(cells_body(), cells_column_labels(), cells_title()))

gt::gtsave(summary_exposure_by_country_gt_table, paste(output_path , "Tables/summary_statistic_by_exposure_country.png", sep = ""), vwidth = 1500, vheight = 1000)




################################################################
# Prepare GADM data for map in Figure S1, S2, S3 and S4 : 
################################################################

# Load GADM data for Africa only (to improve performance)
gadm_data <- st_read("data/raw/GADM/gadm_410-levels.gpkg", 
                     layer= "ADM_0")

# Define North African countries
north_africa <- c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia", "Western Sahara")

# Assign region types (North Africa vs Study Region)
gadm_africa <- gadm_data %>% 
  filter(GID_0 %in% c("DZA", "EGY", "LBY", "MAR", "TUN", "ESH",  # North Africa
                      "AGO", "BEN", "BWA", "BFA", "BDI", "CMR", "CPV", "CAF", "TCD", 
                      "COM", "COG", "COD", "DJI", "GNQ", "ERI", "SWZ", "ETH", "GAB", 
                      "GMB", "GHA", "GIN", "GNB", "CIV", "KEN", "LSO", "LBR", "MDG", 
                      "MWI", "MLI", "MRT", "MUS", "MOZ", "NAM", "NER", "NGA", "RWA", 
                      "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", 
                      "TGO", "UGA", "ZMB", "ZWE")) %>%
  mutate(region = ifelse(COUNTRY %in% north_africa, "North Africa", "Sub-Saharan Africa"))


# Create a Study country variable using the list in "unique(final_data$CountryName)"
study_country <- unique(final_data$CountryName)
gadm_africa <- gadm_africa %>%
  mutate(study_country = ifelse(COUNTRY %in% study_country, "Study Country", "Other")) 


################################################################
# Figure S1(a) : Map of share of Facility-based births by cluster
################################################################

# Calculate share of institutional birth by cluster
institutional_birth_by_cluster <- final_data %>%
  group_by(LATNUM, LONGNUM) %>%
  summarize(percent_facility_based_births = mean(Facility_based_births)*100)

# Create the map - simplified and optimized
map_plot <- ggplot() +
  # Study region countries first (in darker gray)
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill = "gray80", colour = "gray50", linewidth = 0.1) +
  # North African countries on top (in medium gray)
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill = "white", colour = "gray60", linewidth = 0.1) +
  #Study countries on top
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.1) +
  # Add points for institutional births
  geom_point(data = institutional_birth_by_cluster, 
             aes(x = LONGNUM, y = LATNUM, colour = percent_facility_based_births), 
             size = 0.25, alpha = 0.9, shape = 15, stroke = 0) +
  # Diverging, colour-blind-safe palette (roma)
  scico::scale_color_scico(palette  = "roma",         # diverging scientific colour map. or "vik"
                           limits   = c(0, 100),     
                           breaks   = seq(0, 100, 25),
                           guide    = guide_colorbar(title = "Facility-based birth (%)\n", barwidth  = 9, barheight = 0.5)) +
  # Set map boundaries
  coord_sf(xlim = c(-20,52), ylim = c(-36,40), expand = FALSE) +
  # Use theme_void() for clean map with no grid or axes
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(file = paste(output_path, "Figures/percent_facility_based_births.pdf", sep = ""), 
       plot = map_plot,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)




###############################################################
# Figure S1(b) : Map of standard deviation of Facility-based births by cluster
################################################################

# Calculate share of institutional birth by cluster
institutional_birth_std_by_cluster <- final_data %>%
  group_by(LATNUM, LONGNUM) %>%
  summarize(std_facility_based_births = sd(Facility_based_births))
summary(institutional_birth_std_by_cluster$std_facility_based_births)


# Create custom breaks for better visualization of the distribution
# Define breaks that better represent the distribution
breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)

# Create the map - simplified and optimized
map_plot <- ggplot() +
  # Study region countries first (in darker gray)
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill = "gray80", colour = "gray50", linewidth = 0.1) +
  # North African countries on top (in medium gray)
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill = "white", colour = "gray60", linewidth = 0.1) +
  #Study countries on top
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.1) +
  # Add points for institutional births
  geom_point(data = institutional_birth_std_by_cluster, 
             aes(x = LONGNUM, y = LATNUM, colour = std_facility_based_births), 
             size = 0.25, alpha = 0.9, shape = 15, stroke = 0) +
  # Use viridis or magma color palette which works better for this range
  scale_color_viridis_c(option = "viridis", direction = -1,
                        breaks = breaks,
                        guide = guide_colorbar(title = "Facility-based birth (Std) \n",
                                               barwidth = 9, barheight = 0.5)) +
  # Set map boundaries
  coord_sf(xlim = c(-20,52), ylim = c(-36,40), expand = FALSE) +
  # Use theme_void() for clean map with no grid or axes
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA)
  )


ggsave(file = paste(output_path, "Figures/std_facility_based_births.pdf", sep = ""), 
       plot = map_plot,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)





##############################################################
# Figure S2(a) : Map of EPE cut-off for 85th Percentile Cut-off
################################################################

# Import percentile bands cut off
prc_bands <- stack("data/processed/CHIRPS_threshold/50_to_99/threshold/Perc_Cut_clipped_EPSG4326_5566m_20230614_buf5km-0000000000-0000000000.tif")

# Select the band of the raster that indicate the value of the 85th Percentile Cut-off
band1 <- prc_bands[[36]] # Select the  band

# Convert the raster to a dataframe for ggplot
df <- as.data.frame(rasterToPoints(band1))

# Calculate minimum and maximum values
min_val <- round(min(df$precipitation_p85, na.rm = TRUE), 1)
max_val <- round(max(df$precipitation_p85, na.rm = TRUE), 1)

# Define a sequence of breaks for the legend
legend_breaks <- round(seq(min_val, max_val, length.out = 5), 1)

EPEs_85_cuttoff <- ggplot() +
  # Study region countries first (in darker gray)
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill = "gray80", colour = "gray50", linewidth = 0.1) +
  # North African countries on top (in medium gray)
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill = "white", colour = "gray60", linewidth = 0.1) +
  # Study countries on top
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.1) +
  # Add precipitation data
  geom_tile(data = df, aes(x = x, y = y, fill = precipitation_p85), alpha = 1) +
  scico::scale_fill_scico(palette = "roma",
                          direction = -1,
                          limits  = c(min_val, max_val),
                          breaks  = legend_breaks,
                          labels  = legend_breaks,
                          oob = scales::squish,  # Squish values outside limits to min/max
                          guide   = guide_colorbar(title = "mm/day \n", barwidth  = 9, barheight = 0.5)) +
  # Set map boundaries
  coord_sf(xlim = c(-20, 52), ylim = c(-36, 40), expand = FALSE) +
  # Use theme_void() for clean map with no grid or axes
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Plot the map in Rstudio
EPEs_85_cuttoff

# Save the map to a file
ggsave(file = paste(output_path, "Figures/EPEs_85_cuttoff.pdf", sep = ""), 
       plot = EPEs_85_cuttoff,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)



###############################################################
# Figure S2(b) : Map of standard deviation of EPE by cluster
################################################################

# Calculate standard deviation of EPEs by cluster
prec_85_count_3d_0_lag_std_by_cluster <- final_data %>%
  group_by(LATNUM, LONGNUM) %>%
  summarize(std_prec_85_count_3d_0_lag = sd(prec_85_count_3d_0_lag))
summary(prec_85_count_3d_0_lag_std_by_cluster$std_prec_85_count_3d_0_lag)


# Create custom breaks for better visualization based on actual distribution
breaks <- c(0,  0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4)

# Create the map - simplified and optimized
map_plot <- ggplot() +
  # Study region countries first (in darker gray)
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill = "gray80", colour = "gray50", linewidth = 0.1) +
  # North African countries on top (in medium gray)
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill = "white", colour = "gray60", linewidth = 0.1) +
  #Study countries on top
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.1) +
  # Add points for EPEs
  geom_point(data = prec_85_count_3d_0_lag_std_by_cluster, 
             aes(x = LONGNUM, y = LATNUM, colour = std_prec_85_count_3d_0_lag), 
             size = 0.25, alpha = 0.9, shape = 15, stroke = 0) +
  # Use viridis color palette which works better for this range
  scale_color_viridis_c(option = "viridis", direction = -1,
                        breaks = breaks,
                        guide = guide_colorbar(title = "EPEs (Std) \n",
                                               barwidth = 9, barheight = 0.75)) +
  # Set map boundaries
  coord_sf(xlim = c(-20,52), ylim = c(-36,40), expand = FALSE) +
  # Use theme_void() for clean map with no grid or axes
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave(file = paste(output_path, "Figures/std_of_EPEs.pdf", sep = ""), 
       plot = map_plot,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)



# ################################################################
# Figure S3: Distribution of the 85 percentile cutoff values defining EPEs
# ################################################################


# Create the base histogram
EPEs_85_cuttoff_distribution <- gghistogram(final_data, x = "precipitation_p85", fill = "lightgray",
                         rug = TRUE, bins = 200) + 
  theme_minimal()  # Adding a minimal theme for cleaner appearance

# Calculate specific quantiles
quantiles <- quantile(final_data$precipitation_p85, probs = c(0.01, 0.25, 0.5, 0.75, 0.99))

# Add vertical lines for each quantile
# Red lines for 1st and 99th percentiles
EPEs_85_cuttoff_distribution <- EPEs_85_cuttoff_distribution + 
  geom_vline(xintercept = quantiles["1%"], linetype = "dashed", color = "red") +
  geom_vline(xintercept = quantiles["99%"], linetype = "dashed", color = "red")

# Blue lines for 25th, 50th (median), and 75th percentiles
EPEs_85_cuttoff_distribution <- EPEs_85_cuttoff_distribution + 
  geom_vline(xintercept = quantiles["25%"], linetype = "dashed", color = "blue") +
  geom_vline(xintercept = quantiles["50%"], linetype = "dashed", color = "blue") +
  geom_vline(xintercept = quantiles["75%"], linetype = "dashed", color = "blue")


# Add x-axis label
EPEs_85_cuttoff_distribution <- EPEs_85_cuttoff_distribution + labs(x = "85th Percentile Cut-off (mm/day)")

# Print the histogram with quantile lines
print(EPEs_85_cuttoff_distribution)

# Save the histogram with quantile lines
ggsave(file = paste(output_path, "Figures/EPEs_85_cuttoff_distribution.pdf", sep = ""), width = 12, height = 6, units = "in")


##############################################################
# Figure S4 : Maps of EPE cut-off for 60th, 70th, 80th and 90th Percentile
################################################################

# Import percentile bands cut off - only do this once
prc_bands <- stack("data/processed/CHIRPS_threshold/50_to_99/threshold/Perc_Cut_clipped_EPSG4326_5566m_20230614_buf5km-0000000000-0000000000.tif")

# Common output directory
output_dir <- paste0(output_path, "Figures/")

# Function to create EPE cutoff map
create_epe_map <- function(band_num, percentile) {
  # Extract the band and name
  band_name <- paste0("precipitation_p", percentile)
  band <- prc_bands[[band_num]]
  
  # Convert to dataframe
  df <- as.data.frame(rasterToPoints(band))
  
  # Calculate 1st and 99th percentiles for color scale limits
  min_val <- round(quantile(df[[band_name]], probs = 0.005, na.rm = TRUE), 1)
  max_val <- round(quantile(df[[band_name]], probs = 0.995, na.rm = TRUE), 1)
  legend_breaks <- round(seq(min_val, max_val, length.out = 5), 1)
  
  # Create the map
  map <- ggplot() +
    # Study region countries first (in darker gray)
    geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
            fill = "gray80", colour = "gray50", linewidth = 0.1) +
    # North African countries on top (in medium gray)
    geom_sf(data = filter(gadm_africa, region == "North Africa"),
            fill = "white", colour = "gray60", linewidth = 0.1) +
    # Study countries on top
    geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
            fill = NA, colour = "black", linewidth = 0.1) +
    # Add precipitation data
    geom_tile(data = df, aes(x = x, y = y, fill = !!sym(band_name)), alpha = 1) +
    scico::scale_fill_scico(palette = "roma",
                            direction = -1,
                            breaks = legend_breaks,
                            labels = legend_breaks,
                            limits = c(min_val, max_val),
                            oob = scales::squish,  # Squish values outside limits to min/max
                            guide = guide_colorbar(title = "mm/day \n", barwidth = 9, barheight = 0.5)) +
    # Set map boundaries
    coord_sf(xlim = c(-20, 52), ylim = c(-36, 40), expand = FALSE) +
    # Use theme_void() for clean map with no grid or axes
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Save the map to a file
  filename <- paste0(output_dir, "EPEs_", percentile, "_cuttoff.pdf")
  ggsave(filename, plot = map, width = 90, height = 100, units = "mm", dpi = 300)
  
  return(map)
}

# Create maps for each percentile
EPEs_60_cuttoff <- create_epe_map(11, 60)
EPEs_70_cuttoff <- create_epe_map(21, 70)
EPEs_80_cuttoff <- create_epe_map(31, 80)
EPEs_90_cuttoff <- create_epe_map(41, 90)

# Display maps
EPEs_60_cuttoff
EPEs_70_cuttoff
EPEs_80_cuttoff
EPEs_90_cuttoff


###############################################################
# Figure S4 : Merge Map of EPE cut-off value for the 60th, 70th, 80th and 90th Percentile Cut-off
################################################################

# Add labels to individual maps
EPEs_60_cuttoff <- EPEs_60_cuttoff + 
  annotate("text", x = -18, y = 37, label = "a", size = 3)

EPEs_70_cuttoff <- EPEs_70_cuttoff + 
  annotate("text", x = -18, y = 37, label = "b", size = 3)

EPEs_80_cuttoff <- EPEs_80_cuttoff + 
  annotate("text", x = -18, y = 37, label = "c", size = 3)

EPEs_90_cuttoff <- EPEs_90_cuttoff + 
  annotate("text", x = -18, y = 37, label = "d", size = 3)

# Combine all four maps into a single figure (2x2 grid)
EPEs_cuttoff_maps_all <- ggpubr::ggarrange(
  EPEs_60_cuttoff, EPEs_70_cuttoff,
  EPEs_80_cuttoff, EPEs_90_cuttoff,
  nrow = 2, ncol = 2
)

# Display the combined figure
EPEs_cuttoff_maps_all

# Save the combined map to a file
ggsave(paste0(output_dir, "EPEs_cuttoff_maps_all.pdf"), 
       EPEs_cuttoff_maps_all, width = 180, height = 180, units = "mm", dpi = 300)





