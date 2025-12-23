####################################################################################
# Supplementary Information: Extra Information and Data
####################################################################################

####################################################################################
# Context:
# This script generates additional descriptive statistics tables and figures for the
# "Extra Information and Data" section of the supplementary material.
# 
# The script produces the following outputs:
# - Figure S14: Distribution of travel time by country
# - Table S19: Travel time quartiles by country
# - Figure S15: Distribution of road length by country
# - Table S20: Road length quartiles by country
# - Figure S16: Climate zone classification of DHS clusters
####################################################################################


# ################################################################
# # Load data
# ################################################################

# read the data
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
final_data <- final_data %>% as.data.frame()


# ################################################################
# Figure S14: Distribution of travel time by country
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

# Calculate the 99th percentile of the travel time for nicer plot
percentile99 <- quantile(final_data$travel_time_weighted_median, 0.99,  na.rm = TRUE)


# Plot distribution with quartile lines
ggplot(final_data, aes(x = travel_time_weighted_median, y = CountryName)) +
  geom_density_ridges_gradient(aes(fill = after_stat(density)), scale = 1, size = 0.3, rel_min_height = 0.01) +
  geom_segment(data = country_quartile_tt, aes(x = Q25, xend = Q25, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "#D55E00", linetype = "solid", alpha = 0.9, linewidth=0.9) +
  geom_segment(data = country_quartile_tt, aes(x = Q50, xend = Q50, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "blue", linetype = "solid", alpha = 0.9, linewidth=0.9) +
  geom_segment(data = country_quartile_tt, aes(x = Q75, xend = Q75, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "#D55E00", linetype = "solid", alpha = 0.9, linewidth=0.9) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0, percentile99, by = 30), limits = c(0, percentile99)) +
  labs(
    x = "Travel time (minutes)",
    y = "") +
  theme_ridges(grid = TRUE) +
  theme(
    axis.title = element_text(size = 18),
    legend.position = "none",
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 18))

ggsave(file = paste(output_path, "Figures/Travel_time_hist_by_country.pdf", sep = ""), width = 16, height = 13, units = "in")


# ################################################################
# Table S19:  Distribution of Travel Time  
# ################################################################

# Export the table 
country_quartile_tt
kbl(country_quartile_tt, 
    digits = 3,
    booktabs = T, 
    linesep =  "",
    format.args = list(big.mark = ",", decimal.mark = "."),
    "latex") %>% 
  save_kable(file = paste(output_path , "Tables/Travel_time_quartile_by_country.tex", sep = ""))


# ################################################################
# Figure S15: Distribution of road length by country 
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

# Calculate the 99th percentile of the road length for nicer plot
percentile99 <- quantile(final_data$length_m, 0.99,  na.rm = TRUE)

# Plot distribution with quartile lines
ggplot(final_data, aes(x = length_m, y = CountryName)) +
  geom_density_ridges_gradient(aes(fill = after_stat(density)), scale = 1, size = 0.3, rel_min_height = 0.01) +
  geom_segment(data = country_quartile_road_length, aes(x = Q25_length_m, xend = Q25_length_m, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "#D55E00", linetype = "solid", alpha = 0.9, linewidth=0.9) +
  geom_segment(data = country_quartile_road_length, aes(x = Q50_length_m, xend = Q50_length_m, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "blue", linetype = "solid", alpha = 0.9, linewidth=0.9) +
  geom_segment(data = country_quartile_road_length, aes(x = Q75_length_m, xend = Q75_length_m, y = as.numeric(as.factor(Country)), yend = as.numeric(as.factor(Country)) + 0.9), color = "#D55E00", linetype = "solid", alpha = 0.9, linewidth=0.9) +
  scale_fill_viridis_c() +
  scale_x_continuous(limits = c(0, percentile99), labels = scales::comma) +
  labs(
    x = "Road length (meter)",
    y = "") +
  theme_ridges(grid = TRUE) +
  theme(
    axis.title = element_text(size = 18),
    legend.position = "none",
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 18))

ggsave(file = paste(output_path, "Figures/Road_length_hist_by_country.pdf", sep = ""), width = 16, height = 13, units = "in")


# ################################################################
# Table S20 : Distribution of road length  
# ################################################################

# Export the table 
country_quartile_road_length
country_quartile_road_length %>%
  rename(Q0 = Q00_length_m,
         Q25 = Q25_length_m,
         Q50 = Q50_length_m,
         Q75 = Q75_length_m,
         Q100 = Q100_length_m) %>%
  kbl(digits = 3,
      booktabs = T, 
      linesep =  "",
      format.args = list(big.mark = ",", decimal.mark = "."),
      "latex") %>% 
  save_kable(file = paste(output_path , "Tables/Road_length_quartile_by_country.tex", sep = ""))



################################################################
# Prepare GADM data for map in Figure S16 : 
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

# ################################################################
# Figure S16: Climate zone classification of DHS clusters
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
      "A: Tropical"  = "#009E73",  # green
      "B: Arid"      = "#D55E00",  # orange-red
      "C: Temperate" = "#0072B2",  # blue
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

# Display in RStudio
map_plot

# # Save the map
ggsave(file = paste(output_path, "Figures/dhs_cluster_climate_group_map.pdf", sep = ""), 
       plot = map_plot,
       width = 8, height = 8, dpi = 300)






