###############################################################
# Figure S9: Precipitation thresholds for acute (daily) and sustained  (3–14 day) rainfall exposure models across 21 SSA countries
### Notes:Plot of percentile values for randomly selected  location by country 
################################################################

####################################################################################
# Context:
# This script generates Figure S9, which visualizes precipitation thresholds across
# different percentiles (50th-95th) for both acute (1-day, used in Table 1 and Figures 1,2 and 3 
# in the main manuscript) and sustained (3-14 day) rainfall accumulation windows. 
# The analysis samples one random location per country from 21 Sub-Saharan African (SSA) 
# countries and creates a faceted plot showing how threshold values vary by accumulation 
# window length at each sampled location.
####################################################################################

#################################################################
## Load data
#################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- as.data.frame(final_data)

###############################################################
## Prepare data for plotting
###############################################################


# Transform the data in an extract table where each row is a unique coordinates in the format of spatvector 
extract_table_bf_rr_ub <-final_data %>%
  as.data.frame() %>%
  dplyr::select(LONGNUM, LATNUM, place_of_residence, CountryName) %>% 
  distinct(LONGNUM, LATNUM, place_of_residence, .keep_all = TRUE) %>% # I drop duplicate respondent within the same DHS cluster to speed computation time.
  mutate(place_of_residence = to_character(place_of_residence)) %>%  # Convert "place_of_residence"  value into its attached characters 
  vect(geom=c("LONGNUM", "LATNUM"),crs="+proj=longlat +datum=WGS84 +no_defs", keepgeom = TRUE)


###############################################################
## Create the plot 
###############################################################

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

# Combine all into a single data frame
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
  scale_color_scico_d(name = "Accumulation\nwindow length", direction = -1, palette = "roma") + # "roma" or "batlow"
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
    panel.grid.minor = element_blank()) +
  labs(
    x = "Percentile threshold",
    y = "Precipitation threshold (mm)") +
  guides(color = guide_legend(ncol = 5, title.position = "left", title.hjust = 0))


print(facet_plot)


###############################################################
## Export the plot 
###############################################################

# Save the combined plot as A4 size (portrait: 8.27 x 11.69 inches) multiplied by 4
ggsave(file = paste(output_path, "Figures/percentile_thresholds_across_accumulation.pdf", sep = ""), 
       facet_plot, width = 8.27 * 4, height = 11.69 * 4, units = "in", dpi = 300)