####################################################################################
# Figure S13: Non-facility birth and EPEs in 2015 - country-specific coefficients  #
####################################################################################

####################################################################################
# Context:
# This script estimates the number and rate of non-facility births attributable to
# extreme precipitation events (EPEs) in 2015 across 21 Sub-Saharan African countries.
# It integrates geospatial birth data, EPE exposure rasters, and country-specific
# regression coefficients to generate high-resolution attribution maps.
#
# Notes: The script "04_figure_3_and_S11_attribution.R" must be run prior as it download the "worldpop_birth_raster.tif" raster
#
# Main Steps:
# 1. Load all necessary data: regression data, EPE exposure rasters, and WorldPop birth raster.
# 2. Estimate country-specific regression coefficients for the effect of EPEs on non-facility births.
# 3. Prepare GADM country boundaries to store coefficients and mask results to study countries.
# 4. Create a raster storing country-specific coefficients.
# 5. Multiply country-specific coefficients by EPE exposure for each pixel-day in 2015.
# 6. Align and resample rasters to the common (1km) resolution of the WorldPop birth raster.
# 7. Compute the number of non-facility births attributable to EPEs per pixel-day.
# 8. Aggregate results to annual totals per pixel.
# 9. Prepare data for mapping: absolute numbers (Figure S13a) - mask and resample to 20x20 km.
# 10. Prepare data for mapping: per 1,000 live births (Figure S13b) - mask and resample WorldPop birth raster to 20x20 km to rescale attribution in per 1,000 live births.
# 11. Prepare GADM data for mapping and visualization.
# 12. Generate and save maps for Figure S13(a) and S13(b), and create combined figures.
####################################################################################


####################################################################################
####  Step 1: Load Data for Attribution  (Coefficient, Exposure, Birth raster) #####
####################################################################################

## Load  all the necessary data
# (1) Data for regression to get baseline coefficients in the papers and country-specific coefficients
# (2) Raster showing total exposure to EPEs between t and t-2 for each day in 2015 (computed in Google Earth Engine using script: EPE_Institutional_birth_attribution)
# (3) Worldpop birth raster of year 2015


# (1) Data for regression to get baseline coefficients in the papers and country-specific coefficients
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")

# (2) Raster showing total exposure to EPEs between t and t-2 for each day in 2015 (computed in Google Earth Engine using script: EPE_Institutional_birth_attribution)
epe_days <- rast("data/raw/EPE_events/Precipitation_Above_85th_Percentile_1DayWindow_2015_SSA.tif") # 5 km resolution # 1 days windows

# (3) Worldpop birth raster of year 2015
# Notes: The script "01_figure_3_and_S5_attribution.R" must be run prior as it download the "worldpop_birth_raster.tif" raster
worldpop_birth_raster <- rast("data/raw/worldpop_birth_extracted/worldpop_birth_raster.tif") # 1 km resolution


####################################################################################
#### Step 2: Estimate Country-Specific Coefficients                     ############
####################################################################################

# Baseline regression in Table 2 column (4)
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Extract the main estimate of the variable of interest from the baseline regression
main_estimate <- ols_fe_dhs_cbd[["coefficients"]][["prec_85_count_3d_0_lag"]]

# We create a data frame to store the country-specific coefficients
by_country_effects_df <- data.frame(Country_Sample = character(), Estimate = numeric(), Std.Error = numeric(), Conf.Low = numeric(), Conf.High = numeric(), stringsAsFactors = FALSE)

# Loop through each country, run the regression with only the observation from this country, and store the coefficient of interest
for (country in unique(final_data$CountryName)) {
  data_one_country <- final_data[final_data$CountryName == country, ]
  
  # Run the model without the current country
  model_one_country <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , data_one_country, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")
  
  # Get the coefficient and confidence interval for the variable of interest
  coefs <- summary(model_one_country)
  ci <- confint(model_one_country, level = .95)
  
  # Store the results
  by_country_effects_df <- rbind(by_country_effects_df,
                                 data.frame(Country_Sample = country, 
                                            Estimate = coefs[["coefficients"]][["prec_85_count_3d_0_lag"]],
                                            Std.Error = coefs[["se"]][["prec_85_count_3d_0_lag"]], 
                                            Conf.Low = ci["prec_85_count_3d_0_lag", 1], 
                                            Conf.High = ci["prec_85_count_3d_0_lag", 2]))
}

by_country_effects_df




#####################################################################################################################################
#### Step 3: Prepare GADM Boundaries to Store Country-Specific Coefficients in Raster and Mask Results to the 21 SSA Countries  #####
#####################################################################################################################################


# Load GADM data at the ADM0 level to filter for my countries of interests and to store the country-specific coefficients
st_layers("data/raw/GADM/gadm_410-levels.gpkg")
gadm_data <- st_read("data/raw/GADM/gadm_410-levels.gpkg",
                     layer= "ADM_0")

gadm_data <- gadm_data %>% filter(COUNTRY %in% unique(final_data$CountryName))


####################################################################################
#### Step 4: Create a Raster that Store Country-Specific Coefficients   ############
####################################################################################

coef_raster <- terra::rasterize(gadm_data, epe_days, field = "COUNTRY")
plot(coef_raster)
coef_raster

# Check if the country names are available in the raster
unique(coef_raster)
unique(by_country_effects_df$Country_Sample)

# Create a named vector of your estimates with country names as names of the vector
coef_vector <- setNames(by_country_effects_df$Estimate, by_country_effects_df$Country_Sample)
sort(coef_vector)

# Update the raster values based on the country names
for(country in names(coef_vector)) {
  # Assuming the raster categories are named after countries
  coef_raster[coef_raster == country] <- coef_vector[country]
}

# Plot the updated raster
test_rast <- as(coef_raster, "Raster")
plot(test_rast)
plot(coef_raster)
unique(values(coef_raster))

# Save the coefficient raster
terra::writeRaster(coef_raster,   "data/temp/coef_raster.tif", overwrite = TRUE) # coef_raster <- rast("data/temp/coef_raster.tif")



#############################################################################################
####  Step 5: Multiply the Country-Specific Coefficients with Exposure to EPEs in 2015  #####
####  Notes: This is equation (3) in the supplementary appendix                         #####
####  Notes: We use country-specific coefficients here                                  #####
#############################################################################################


# Multiply the country-coefficient by the total exposure to EPEs between t and t-2 for each day in 2015  
format(object.size(epe_days), units = "Mb")
crs(epe_days)
epe_days_exposure_country_risk <- epe_days*(coef_raster/1000)*(-1)  #0.005 OR divied per thousand if Facility_based_births*1000

# Check the results
plot(epe_days$Day_1)
plot(epe_days_exposure_country_risk$Day_1)

terra::writeRaster(epe_days_exposure_country_risk, "data/temp/epe_days_exposure_country_risk.tiff", overwrite=TRUE)



####################################################################################
####  Step 6: Align and Resample Rasters to Common Resolutions                 #####
####################################################################################

# Get the number of birth per day assuming an uniform distribution of birth across a year
days_in_2015 <- ymd("2016-01-01") - ymd("2015-01-01")
days_in_2015 <- as.numeric(days_in_2015, "days")
worldpop_birth_raster_daily <- worldpop_birth_raster/days_in_2015


# Resample and align resolution of EPE exposure (5km) to the resolution of the daily birth raster (1km)
epe_days_exposure_country_risk_align <- terra::resample(epe_days_exposure_country_risk, worldpop_birth_raster_daily, method = "near", threads=TRUE) # the bilinear applies weights based on the distance of the four nearest cell centers smoothing the output raster grid.

# Save the resampled raster
writeRaster(epe_days_exposure_country_risk_align, "data/temp/epe_days_exposure_country_risk_align_2015.tiff", overwrite=TRUE)



####################################################################################
####  Step 7: Compute Non-Facility Births Attributable to EPE per Pixel-Day    #####
####  Notes: This is equation (4) in the supplementary appendix                #####
####  Notes: We use country-specific coefficients here                         #####
####################################################################################


# Multiply the daily birth raster with the estimated changes in the probability of non-facility birth of each pixel-day of the year 2015 
# Notes: This is equation (4) in the supplementary appendix but with country-specific coefficients here
epe_days_exposure_country_risk_align_birth_2015 <- epe_days_exposure_country_risk_align*worldpop_birth_raster_daily
plot(epe_days_exposure_country_risk_align_birth_2015$Day_1)
writeRaster(epe_days_exposure_country_risk_align_birth_2015, "data/temp/epe_days_exposure_country_risk_align_birth_2015.tiff", overwrite=TRUE) # epe_days_exposure_country_risk_align_birth_2015 <- rast("data/temp/epe_days_exposure_country_risk_align_birth_2015.tiff") 




####################################################################################
####  Step 8: Aggregate Results to Annual                                      #####
####  Notes: This is equation (5) in the supplementary appendix                #####
####################################################################################


# Compute the total excess of non-facility birth due to EPE in 2015 for each pixel and save it
# Notes: This is equation (5) in the supplementary appendix but with country-specific coefficients here
epe_days_exposure_country_risk_align_birth_2015_sum <- app(epe_days_exposure_country_risk_align_birth_2015, 
                                                           fun = sum,
                                                           na.rm = TRUE,
                                                           cores = 5)
writeRaster(epe_days_exposure_country_risk_align_birth_2015_sum, "data/temp/epe_days_exposure_country_risk_align_birth_2015_sum.tiff", overwrite=TRUE)  # epe_days_exposure_country_risk_align_birth_2015_sum <- rast("data/temp/epe_days_exposure_country_risk_align_birth_2015_sum.tiff")




####################################################################################
####  Step 9: Prepare Data for Mapping (Figure S13(a): Absolute Numbers)       #####
####  Notes: Mask results to the 21 SSA countries and resample to 20km X 20km  #####
####################################################################################


# Use the country raster to keep only attribution results from the study region
epe_days_exposure_country_risk_align_birth_2015_sum_filtered <- terra::mask(epe_days_exposure_country_risk_align_birth_2015_sum, gadm_data)

# Current resolution in degrees
current_resolution <- res(epe_days_exposure_country_risk_align_birth_2015_sum_filtered)[1]

# Desired pixel size in kilometers
desired_pixel_size_km <- 20

# Conversion factor (approximate average for both latitude and longitude)
km_per_degree <- 111

# Calculate degrees per desired pixel size
degrees_per_desired_pixel <- desired_pixel_size_km / km_per_degree

# Calculate the multiplication factor
multiplication_factor <- degrees_per_desired_pixel / current_resolution

# Output the multiplication factor
multiplication_factor

# Save in 20km X 20 km resolution for QGIS mapping
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km <- terra::aggregate(epe_days_exposure_country_risk_align_birth_2015_sum_filtered, fact=multiplication_factor, fun=sum, na.rm=TRUE)
writeRaster(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km, "data/processed/Attribution/epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km.tiff", overwrite=TRUE)  # epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km <- rast("data/processed/Attribution/epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km.tiff)


# # Explore the data quickly
# plot(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km)
# summary(as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km))
# hist(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km, breaks=200)
# nrow(as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km) )
# as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km) %>% filter(sum<=0) %>% nrow()
# as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km) %>% filter(sum<=0) %>% View()
# as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km) %>% filter(sum<=-5) %>% nrow()


########################################################################################
####  Step 10: Prepare Data for Mapping (Figure S13(b): Per 1'000 Live Births)     #####
####  Notes: Mask and resample birth raster to 20km X 20km to rescale attribution  #####
####         in per 1,000 live births.                                             #####
########################################################################################

# We use the attribution raster of 20 km X 20 km
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km


# Lets compute the number live births in each 20 km X 20 km
# Use the country raster to keep only birth from the study region before aggregating at a higher resolution otherwise I will pick up population in country outside my sample and have underestimated rate
worldpop_birth_raster_filtered <- terra::mask(worldpop_birth_raster, gadm_data)
worldpop_birth_raster_filtered_sum_20km <- terra::aggregate(worldpop_birth_raster_filtered, fact=multiplication_factor, fun=sum, na.rm=TRUE)
writeRaster(worldpop_birth_raster_filtered_sum_20km, "data/temp/worldpop_birth_raster_filtered_sum_20km.tiff", overwrite=TRUE)  # worldpop_birth_raster_filtered_sum_20km <- rast("data/temp/worldpop_birth_raster_filtered_sum_20km.tiff")


# Lets check the resolution of both
res(worldpop_birth_raster_filtered_sum_20km)
res(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km)
crs(worldpop_birth_raster_filtered_sum_20km)
crs(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km)

# Lets compute the number of additional HB per 1'000 live births
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered <- (epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km/worldpop_birth_raster_filtered_sum_20km)*1000

#Plot
plot(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered)
summary(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered)
as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered) %>% filter(sum==0)
hist(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered)

# Save the raster of attribution per 1'000 live birth
writeRaster(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered, "data/processed/Attribution/epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered.tiff", overwrite=TRUE)  # epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered <- rast("data/processed/Attribution/epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered.tiff")




####################################################################################
####  Step 11: Prepare GADM Data for Mapping                                   #####
####################################################################################

# Load GADM data 
gadm_data_map <- st_read("data/raw/GADM/gadm_410-levels.gpkg", 
                         layer= "ADM_0")

# Define North African countries
north_africa <- c("Algeria", "Egypt", "Libya", "Morocco", "Tunisia", "Western Sahara")

# Assign region types (North Africa vs Study Region)
gadm_africa <- gadm_data_map %>% 
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


# Simply the shape of the polygons gadm_africa because Benin has stranger random line inside his country border
gadm_africa <- gadm_africa %>%
  st_make_valid() %>% 
  st_simplify(dTolerance = 0.01)




####################################################################################
####  Step 12: Generate and Save Maps for Figure S13(a) and S13(b)             #####
####################################################################################

####  Load the attribution data for mapping of Figure S13(a) and S13(b) 
# Load the attribution raster for Figure S13(a) 
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km <- rast("data/processed/Attribution/epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km.tiff")

# Load the attribution raster for Figure S13(b) 
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered <- rast("data/processed/Attribution/epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered.tiff")

# Set values to NA in pixels of the attribution rasters when there is 0 births in 2015 for better visualization
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km[worldpop_birth_raster_filtered_sum_20km == 0] <- NA
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered[worldpop_birth_raster_filtered_sum_20km == 0] <- NA


#### Figure S13(a)
# Transform into raster dataframe
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km <- as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km, xy = TRUE, na.rm = TRUE)

# Define breaks and colors for Figure 6(a) based on QGIS styling
max(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km$sum) # maximum is 570.0812 so last break set to 571
min(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km$sum) # minimum is -160.4012 so last break set to -161
breaks <- c(-161, -5, -1, 0, 1, 5, 10, 15, 20, 30, 50, 571)

# Colors extracted from QGIS styling document
colours <- c(
  "#4e32ff",  # < -5
  "#9b9bff",  # [-5, -1) 
  "#b0e0e6",  # [-1, 0) 
  "#faebdd",  # [0, 1) 
  "#f58860",  # [1, 5)  
  "#ec4c3e",  # [5, 10) 
  "#cb1b4f",  # [10, 15)
  "#9b1d5d",  # [15, 20)   
  "#742563",  # [20, 30)
  "#492358",  # [30, 50) 
  "#091052"   # >= 50
)

# Cut data into the defined categories
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km$bin <- cut(
  epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km$sum,   
  breaks = breaks,
  include.lowest = TRUE,
  right = FALSE
)



# Create Map S13(a) - Number of non-facility births
map_a <- ggplot() +
  # 1. Study region (Sub-Saharan) in light gray
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill     = "gray90",
          colour   = "gray50",
          linewidth = 0.1) +
  # 2. North Africa on top in white
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill     = "white",
          colour   = "gray50",
          linewidth = 0.1) +
  # 3. The raster
  geom_tile(data = epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km, aes(x = x, y = y, fill = bin)) +
  # 4. Define the discrete bins legend for the raster
  scale_fill_discrete(name   = "Number of non-facility births", 
                      type = colours) +                        
  # 5. Customize the legend appearance
  guides(fill = guide_bins(show.limits = T)) +  
  # 6. Highlight study countries with black borders
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.2) +
  #  7. Set map boundaries to focus on the region of interest
  coord_sf(xlim = c(-20,52), ylim = c(-36,40), expand = FALSE) +
  # 8. Apply theme for the map
  theme_void() +
  theme(
    legend.position        = "bottom",                            # Place legend at the bottom
    legend.title.align     = 0.5,                                 # Center-align legend title
    legend.title.position  = "top",                               # Position legend title on top
    legend.title           = element_text(face = "bold"),         # Bold legend title
    legend.text            = element_text(size = 10, margin = margin(t = 5, unit = "pt")),  # Legend text styling
    legend.key.spacing.x   = unit(0, "cm"),                       # Remove horizontal gaps in legend
    legend.key             = element_rect(colour = NA, fill = NA),# Transparent legend keys
    legend.key.width       = unit(1.5, "cm"),                       # Set legend key width
    legend.key.height      = unit(0.5, "cm"),                     # Set legend key height
    legend.box.spacing     = unit(0.5, "cm")                      # Add spacing around the legend box
  )


#### Figure S13(b)
# Transform into raster dataframe
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered <- as.data.frame(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered, xy = TRUE, na.rm = TRUE)


# Define breaks based on the QGIS symbology (adapting for our data)
max(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered$sum) # maximum is 7.401924 so last break set to 8
min(epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered$sum) # minimum is -8.631651 so last break set to -9
breaks_b <- c(-9, -6.5, -5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 8)

# Define colors based on the vik colors from scico with "#F2D8C2"  in the central bins
# scico::scico(n = 15, palette = "vik", direction = 1)
# colors_b[8] <- "#F2D8C2" # my central bin (-0.5–0.5)
# colors_b[8] <- "#C8C8C8"  # slightly stronger separatio
colors_b <- c(
  "#001260",  # < -6.5
  "#022E73",  # [-6.5, -5.5) 
  "#034A85",  # [-5.5, -4.5) 
  "#186A99",  # [-4.5, -3.5) 
  "#4A8FB2",  # [-3.5, -2.5) 
  "#84B4CC",  # [-2.5, -1.5) 
  "#BDD6E3",  # [-1.5, -0.5) 
  "#C8C8C8",  # [-0.5, 0.5)  
  "#E7C6B2",  # [0.5, 1.5)   
  "#D8A283",  # [1.5, 2.5)   
  "#C98157",  # [2.5, 3.5)   
  "#BB612E",  # [3.5, 4.5)   
  "#9D3709",  # [4.5, 5.5)   
  "#781806",  # [5.5, 6.5)   
  "#590007"   # >= 6.5  
)

# Cut data into the defined categories
epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered$bin <- cut(
  epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered$sum,
  breaks = breaks_b,
  include.lowest = TRUE,
  right = FALSE
)



# Create Map S13(b) - Per thousand rate
map_b <- ggplot() +
  # 1. Plot the study region (Sub-Saharan Africa) in light gray
  geom_sf(data = filter(gadm_africa, region == "Sub-Saharan Africa"),
          fill     = "gray90",
          colour   = "gray50", 
          linewidth = 0.1) +    
  # 2. Overlay North Africa on top in white
  geom_sf(data = filter(gadm_africa, region == "North Africa"),
          fill     = "white", 
          colour   = "gray50",
          linewidth = 0.1) +  
  # 3. Add the raster data for non-facility births per 1,000
  geom_tile(data = epe_days_exposure_country_risk_align_birth_2015_sum_filtered_20km_per_thsd_filtered, 
            aes(x = x, y = y, fill = bin)) +
  # 4. Define the discrete bins legend for the raster
  scale_fill_discrete(name   = "Non-facility births (per 1'000)", 
                      type = colors_b) +                        
  # 5. Customize the legend appearance
  guides(fill = guide_bins(show.limits = T)) +                
  # 6. Highlight study countries with black borders
  geom_sf(data = filter(gadm_africa, study_country == "Study Country"),
          fill = NA, colour = "black", linewidth = 0.2) +         
  # 7. Set map boundaries to focus on the region of interest
  coord_sf(xlim = c(-20,52), ylim = c(-36,40), expand = FALSE) +   
  # 8. Apply theme for the map
  theme_void() +
  theme(
    legend.position        = "bottom",                            # Place legend at the bottom
    legend.title.align     = 0.5,                                 # Center-align legend title
    legend.title.position  = "top",                               # Position legend title on top
    legend.title           = element_text(face = "bold"),         # Bold legend title
    legend.text            = element_text(size = 10, margin = margin(t = 5, unit = "pt")),  # Legend text styling
    legend.key.spacing.x   = unit(0, "cm"),                       # Remove horizontal gaps in legend
    legend.key             = element_rect(colour = NA, fill = NA),# Transparent legend keys
    legend.key.width       = unit(1, "cm"),                       # Set legend key width
    legend.key.height      = unit(0.5, "cm"),                     # Set legend key height
    legend.box.spacing     = unit(0.5, "cm")                      # Add spacing around the legend box
  )



####   Save individual maps if needed
ggsave(file = paste(output_path, "Figures/attribution_plot_heterogeneity_absolute.pdf", sep = ""), 
       map_a, 
       width = 8, height = 8, dpi = 300)

ggsave(file = paste(output_path, "Figures/attribution_plot_heterogeneity_per1000.pdf", sep = ""), 
       map_b, 
       width = 8, height = 8, dpi = 300)


# Create combined figure with (a) and (b) labels
combined_map <- map_a + map_b + 
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.tag = element_text(size = 14, face = "bold"))
  ) +
  plot_layout(ncol = 2)


# Display the combined map
print(combined_map)

####   Save the combined map
ggsave(file = paste(output_path, "Figures/attribution_plot_combined_heterogeneity.pdf", sep = ""), 
       combined_map,
       width = 16, height = 8, dpi = 300)
















