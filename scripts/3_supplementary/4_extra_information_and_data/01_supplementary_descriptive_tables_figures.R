# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


################################################################
# Table S14: DHS country-wave table Table
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
# Table S15: : Summary statistics
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
          out = "output/Tables/summary_statistic.tex"
          ) 




################################################################
# Table S16 : Summary statistics by Outcome 
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
gt::gtsave(summary_by_outcome_gt_table,"output/Tables/summary_statistic_by_outcome.png", vwidth = 1500, vheight = 1000)



################################################################
# Table S17 : Summary statistics by Outcome across country only
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


gt::gtsave(summary_outcome_by_country_gt_table,"output/Tables/summary_statistic_by_outcome_across_country.png", vwidth = 1500, vheight = 1000)



################################################################
# Table S18 : Summary statistics by exposure-country 
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
gt::gtsave(summary_exposure_by_country_gt_table,"output/Tables/summary_statistic_by_exposure_country.png", vwidth = 1500, vheight = 1000)




################################################################
# Prepare GADM data for map in Figure S8, S9, S11 and S15 : 
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
# Figure S8(a) : Map of share of Facility-based births by cluster
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
  # Use RdYlBu color palette for the data points
  scale_color_distiller(palette = "RdYlBu", direction = 1,
                        guide = guide_colorbar(title = "Facility-based birth (%) \n",
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

ggsave("output/Figures/percent_facility_based_births.pdf",
       plot = map_plot,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)




###############################################################
# Figure S8(b) : Map of standard deviation of Facility-based births by cluster
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


ggsave("output/Figures/std_facility_based_births.pdf",
       plot = map_plot,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)






##############################################################
# Figure S9(a) : Map of EPE cut-off for 85th Percentile Cut-off
################################################################

# Import percentile bands cut off
prc_bands <- stack("data/processed/CHIRPS_threshold/50_to_99/Perc_Cut_clipped_EPSG4326_5566m_20230614_buf5km-0000000000-0000000000.tif")

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
  scale_fill_gradientn(colors = turbo(256), 
                       breaks = legend_breaks,
                       labels = legend_breaks,
                       limits = c(min_val, max_val),
                       guide = guide_colorbar(title = "mm/day \n",
                                              barwidth = 9, barheight = 0.5)) +
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
ggsave("output/Figures/EPEs_85_cuttoff.pdf",
       plot = EPEs_85_cuttoff,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)



###############################################################
# Figure S9(b) : Map of standard deviation of EPE by cluster
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


ggsave("output/Figures/std_of_EPEs.pdf",
       plot = map_plot,
       width = 90, 
       height = 100, 
       units = "mm", 
       dpi = 300)



# ################################################################
# Figure S10: Distribution of the 85 percentile cutoff values defining EPEs
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
ggsave("output/Figures/EPEs_85_cuttoff_distribution.pdf", width = 12, height = 6, units = "in")


##############################################################
# Figure S11 : Maps of EPE cut-off for 60th, 70th, 80th and 90th Percentile
################################################################

# Import percentile bands cut off - only do this once
prc_bands <- stack("data/processed/CHIRPS_threshold/50_to_99/Perc_Cut_clipped_EPSG4326_5566m_20230614_buf5km-0000000000-0000000000.tif")

# Common output directory
output_dir <- "output/Figures/"

# Function to create EPE cutoff map
create_epe_map <- function(band_num, percentile) {
  # Extract the band and name
  band_name <- paste0("precipitation_p", percentile)
  band <- prc_bands[[band_num]]
  
  # Convert to dataframe
  df <- as.data.frame(rasterToPoints(band))
  
  # Calculate min/max and legend breaks
  min_val <- round(min(df[[band_name]], na.rm = TRUE), 1)
  max_val <- round(max(df[[band_name]], na.rm = TRUE), 1)
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
    scale_fill_gradientn(colors = turbo(256), 
                         breaks = legend_breaks,
                         labels = legend_breaks,
                         limits = c(min_val, max_val),
                         guide = guide_colorbar(title = "mm/day \n",
                                                barwidth = 9, barheight = 0.5)) +
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
# Figure S11 : Merge Map of EPE cut-off value for the 60th, 70th, 80th and 90th Percentile Cut-off
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



###############################################################
# Figure S12: Precipitation thresholds for acute (daily) and sustained  (3–14 day) rainfall exposure models across 21 SSA countries
### Notes:Plot of percentile values for randomly selected  location by country 
################################################################


# Transform the data in an extract table where each row is a unique coordinates in the format of spatvector 
extract_table_bf_rr_ub <-final_data %>%
  as.data.frame() %>%
  dplyr::select(LONGNUM, LATNUM, place_of_residence, CountryName) %>% 
  distinct(LONGNUM, LATNUM, place_of_residence, .keep_all = TRUE) %>% # I drop duplicate respondent within the same DHS cluster to speed computation time.
  mutate(place_of_residence = to_character(place_of_residence)) %>%  # Convert "place_of_residence"  value into its attached characters 
  vect(geom=c("LONGNUM", "LATNUM"),crs="+proj=longlat +datum=WGS84 +no_defs", keepgeom = TRUE)


set.seed(427)  # For reproducibility

countries <- sort(unique(extract_table_bf_rr_ub$CountryName))

# Prepare a data frame to store percentile threshold data for one random pixel per country
facet_data <- list()
facet_titles <- list()

for (country in countries) {
  # Get all locations for this country
  country_locations <- extract_table_bf_rr_ub[extract_table_bf_rr_ub$CountryName == country, ]
  
  # Randomly sample 1 location
  selected_index <- sample(1:nrow(country_locations), 1)
  selected_location <- country_locations[selected_index, ]
  
  # Find the corresponding row in final_data by matching LONGNUM, LATNUM, and CountryName
  match_row <- which(final_data$LONGNUM == selected_location$LONGNUM &
                       final_data$LATNUM == selected_location$LATNUM &
                       final_data$CountryName == selected_location$CountryName)[1]
  
  # Extract percentile threshold data for this location
  location_coords <- c(final_data$LONGNUM[match_row], final_data$LATNUM[match_row])
  location_country <- final_data$CountryName[match_row]
  
  # Accumulation-based percentile values
  percentile_data <- map_dfr(3:14, ~ {
    perc_cols <- paste0("precipitation_p", 50:95, "_", .x, "day_AC")
    data.frame(
      accumulation_days = .x,
      percentile = 50:95,
      threshold_value = as.numeric(final_data[match_row, perc_cols]),
      type = paste0(.x, "-days accumulation"),
      CountryName = location_country,
      coords = paste0("(", 
                      sub("^(-?\\d+\\.\\d{3}).*$", "\\1", as.character(location_coords[1])), 
                      ", ", 
                      sub("^(-?\\d+\\.\\d{3}).*$", "\\1", as.character(location_coords[2])), 
                      ")")  )
  })
  
  # Daily percentile values if available
  daily_perc_cols <- paste0("precipitation_p", 50:95)
  available_daily_cols <- daily_perc_cols[daily_perc_cols %in% names(final_data)]
  
  if(length(available_daily_cols) > 0) {
    daily_data <- data.frame(
      accumulation_days = 1,
      percentile = 50:(50 + length(available_daily_cols) - 1),
      threshold_value = as.numeric(final_data[match_row, available_daily_cols]),
      type = "1-day (daily)",
      CountryName = location_country,
      coords = paste0("(", 
                      sub("^(-?\\d+\\.\\d{3}).*$", "\\1", as.character(location_coords[1])), 
                      ", ", 
                      sub("^(-?\\d+\\.\\d{3}).*$", "\\1", as.character(location_coords[2])), 
                      ")") 
    )
    percentile_data <- rbind(daily_data, percentile_data)
  }
  
  facet_data[[country]] <- percentile_data
  facet_titles[[country]] <- paste0(location_country, "\n", "Coordinates: ", 
                                    paste0("(", 
                                           sub("^(-?\\d+\\.\\d{3}).*$", "\\1", as.character(location_coords[1])), 
                                           ", ", 
                                           sub("^(-?\\d+\\.\\d{3}).*$", "\\1", as.character(location_coords[2])), 
                                           ")") 
                                    )
  }

facet_data_df <- bind_rows(facet_data)

# Order factor levels
all_types <- c("1-day (daily)", paste0(3:14, "-days accumulation"))
facet_data_df$type <- factor(facet_data_df$type, levels = all_types)

# Create a new facet label variable with country and coordinates
facet_data_df$facet_label <- paste0(facet_data_df$CountryName, " - coordinates: ", facet_data_df$coords)
facet_data_df$facet_label <- factor(facet_data_df$facet_label, levels = unique(facet_data_df$facet_label))

# Faceted plot with legend in two lines
facet_plot <- ggplot(facet_data_df, aes(x = percentile, y = threshold_value, color = type)) +
  geom_line(linewidth = 3) +
  geom_point(size = 2) +
  scale_color_viridis_d(name = "Accumulation\nwindow length", option = "turbo") +
  scale_x_continuous(breaks = seq(50, 95, by = 5), limits = c(50, 95), expand = c(0, 0.9)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = scales::comma) +
  facet_wrap(~ facet_label, ncol = 3, scales = "free") +
  theme_light() +
  theme(
    axis.title = element_text(size = 40, face = "bold"),
    plot.title = element_text(hjust = 0, size = 35),
    axis.text = element_text(size = 33),
    legend.position = "bottom",
    legend.text = element_text(size = 35),
    legend.title = element_text(size = 40, face = "bold"),
    legend.key.size = unit(3, "lines"), # <-- Increase legend symbol size
    strip.background = element_rect(fill = "transparent", colour = NA),
    strip.text = element_text(hjust = 0, face = "plain", size = 33, colour = "black"),
    panel.background = element_rect(fill = 'transparent'),
    plot.background = element_rect(fill = 'transparent', color = NA),
    legend.background = element_rect(fill = 'transparent'),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Percentile threshold",
    y = "Precipitation threshold (mm)"
  ) +
  guides(color = guide_legend(ncol = 5, title.position = "left", title.hjust = 0)
  )


print(facet_plot)


# Save the combined plot as A4 size (portrait: 8.27 x 11.69 inches) multiplied by 4
ggsave("output/Figures/percentile_thresholds_across_accumulation.pdf",
       facet_plot, width = 8.27 * 4, height = 11.69 * 4, units = "in", dpi = 300)






# ################################################################
# Table S19:  Distribution of Travel Time  
# ################################################################

# Compute the value of the travel time quartile group by country
country_quartile_tt <- final_data %>%
  group_by(CountryName) %>%
  summarise(
    Q0 = quantile(travel_time_weighted_median, probs = 0, na.rm = TRUE),
    Q25 = quantile(travel_time_weighted_median, probs = 0.25, na.rm = TRUE),
    Q50 = quantile(travel_time_weighted_median, probs = 0.50, na.rm = TRUE),
    Q75 = quantile(travel_time_weighted_median, probs = 0.75, na.rm = TRUE),
    Q100 = quantile(travel_time_weighted_median, probs = 1, na.rm = TRUE)) %>%
  rename(Country = CountryName)
country_quartile_tt

# Export the table 
kbl(country_quartile_tt, 
    digits = 3,
    booktabs = T, 
    linesep =  "",
    "latex") %>% 
  save_kable("output/Tables/Travel_time_quartile_by_country.tex")




# ################################################################
# Figure S13: Distribution of travel time by country
# ################################################################

# Calculate the 99th percentile of the travel time for nicer plot
percentile99 <- quantile(final_data$travel_time_weighted_median, 0.99,  na.rm = TRUE)


# Plot distribution with quartile lines
ggplot(final_data, aes(x = travel_time_weighted_median, y = CountryName)) +
  geom_density_ridges_gradient(aes(fill = ..density..), scale = 1, size = 0.3, rel_min_height = 0.01) +
  geom_segment(data = country_quartile_tt, aes(x = Q25, xend = Q25, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "red", linetype = "solid", alpha = 0.9, size=0.9) +
  geom_segment(data = country_quartile_tt, aes(x = Q50, xend = Q50, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "blue", linetype = "solid", alpha = 0.9, size=0.9) +
  geom_segment(data = country_quartile_tt, aes(x = Q75, xend = Q75, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "red", linetype = "solid", alpha = 0.9, size=0.9) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0, percentile99, by = 30), limits = c(0, percentile99)) +
  labs(
    x = "Travel time (minutes)",
    y = ""
  ) +
  theme_ridges(grid = TRUE) +
  theme(
    axis.title = element_text(size = 18),
    legend.position = "none",
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 18)
    )
ggsave("output/Figures/Travel_time_hist_by_country.pdf", width = 16, height = 13, units = "in")




# ################################################################
# Table S20 : Distribution of road length  
# ################################################################

# Compute the value of the road length quartile group by country
country_quartile_road_length <- final_data %>%
  group_by(CountryName) %>%
  summarise(
    Q00_length_m = quantile(length_m, probs = 0, na.rm = TRUE),
    Q25_length_m = quantile(length_m, probs = 0.25, na.rm = TRUE),
    Q50_length_m = quantile(length_m, probs = 0.50, na.rm = TRUE),
    Q75_length_m = quantile(length_m, probs = 0.75, na.rm = TRUE),
    Q100_length_m = quantile(length_m, probs = 1, na.rm = TRUE))  %>%
  rename(Country = CountryName)
country_quartile_road_length

# Export the table 
country_quartile_road_length %>%
  rename(Q0 = Q00_length_m,
         Q25 = Q25_length_m,
         Q50 = Q50_length_m,
         Q75 = Q75_length_m,
         Q100 = Q100_length_m) %>%
  kbl(digits = 3,
      booktabs = T, 
      linesep =  "",
      "latex") %>% 
  save_kable("output/Tables/Road_length_quartile_by_country.tex")


# ################################################################
# Figure S14: Distribution of road length by country 
# ################################################################

# Calculate the 99th percentile of the road length for nicer plot
percentile99 <- quantile(final_data$length_m, 0.99,  na.rm = TRUE)

# Plot distribution with quartile lines
ggplot(final_data, aes(x = length_m, y = CountryName)) +
  geom_density_ridges_gradient(aes(fill = ..density..), scale = 1, size = 0.3, rel_min_height = 0.01) +
  geom_segment(data = country_quartile_road_length, aes(x = Q25_length_m, xend = Q25_length_m, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "red", linetype = "solid", alpha = 0.9, size=0.9) +
  geom_segment(data = country_quartile_road_length, aes(x = Q50_length_m, xend = Q50_length_m, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "blue", linetype = "solid", alpha = 0.9, size=0.9) +
  geom_segment(data = country_quartile_road_length, aes(x = Q75_length_m, xend = Q75_length_m, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "red", linetype = "solid", alpha = 0.9, size=0.9) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, percentile99) ,labels = label_number()) + # This formats the labels as regular numbers
  labs(
    x = "Road length (meter)",
    y = "") +
  theme_ridges(grid = TRUE) +
  theme(
    axis.title = element_text(size = 18),
    legend.position = "none",
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 18)
  )
ggsave("output/Figures/Road_length_hist_by_country.pdf", width = 16, height = 13, units = "in")




# ################################################################
# Figure S15: Climate zone classification of DHS clusters
# ################################################################


# Prepare cluster data (one row per unique location)
cluster_locations <- final_data %>%
  as.data.frame() %>%
  dplyr::select(LONGNUM, LATNUM, place_of_residence, CountryName, Climate_Group) %>%
  distinct(LONGNUM, LATNUM, place_of_residence, .keep_all = TRUE)

# Map: Overlay clusters on Africa map, colored by climate group
map_plot <- ggplot() +
  # Sub-Saharan Africa background
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill = "gray80", colour = "gray50", linewidth = 0.1) +
  # North Africa overlay
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill = "white", colour = "gray60", linewidth = 0.1) +
  # Study countries outline
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.1) +
  # DHS cluster points colored by climate group
  geom_point(data = cluster_locations,
             aes(x = LONGNUM, y = LATNUM, color = Climate_Group),
             size = 0.5, alpha = 0.8, shape = 16) +
  scale_color_manual(
    name = "Climate zone",
    values = c(
      "B: Arid" = "red",
      "A: Tropical" = "blue",
      "C: Temperate" = "green4",
      "D: Cold" = "purple",
      "E: Polar" = "gray"
    ),
    breaks = c("A: Tropical", "B: Arid", "C: Temperate", "D: Cold", "E: Polar"),
    labels = c("Tropical", "Arid", "Temperate", "Cold", "Polar")
  ) +
  # labs(title = "DHS Cluster Locations by Köppen-Geiger Climate Group", x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-20, 52), ylim = c(-36, 40), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA)) +
  guides( color = guide_legend(
    override.aes = list(size = 4) # Increase legend point size only
  ))

# # Save the map
ggsave("output/Figures/dhs_cluster_climate_group_map.pdf",
       plot = map_plot,
       width = 8, height = 8, dpi = 300)

# Display in RStudio
map_plot









