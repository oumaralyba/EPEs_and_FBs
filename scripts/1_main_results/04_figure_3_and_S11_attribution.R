####################################################################################
####     Figure 3 & S11 : Non-facility birth and EPEs in 2015 (by country)      #####
####################################################################################


####################################################################################
# Context:
# The code estimates the number and rate of non-facility births attributable to 
# extreme precipitation events (EPEs) in 2015 across 21 Sub-Saharan African countries.
# It combines geospatial birth data, EPE exposure rasters, and baseline regression result to 
# generate high-resolution attribution maps and country-level statistics.
#
# Main Steps:
# 1. Download and merge WorldPop 2015 birth rasters for all countries of interest.
# 2. Load and process EPE exposure rasters (computed in Google Earth Engine).
# 3. Multiply main regression coefficient by EPE exposure.
# 4. Align and resample rasters to a common resolution.
# 5. Compute the number of non-facility births attributable to EPEs per pixel-day.
# 6. Aggregate results to annual totals per pixel.
# 7. Prepare GADM country boundaries to mask results to study countries.
# 8. Prepare data for mapping (Figure 3a: Absolute Numbers) - mask and resample to 20x20 km.
# 9. Prepare data for mapping (Figure 3a: Per 1'000 live births) - mask and resample WorldPop birth raster to 20x20 km to rescale attribution in per 1,000 live births.
# 10. Prepare GADM data for mapping.  
# 11. Generate and save maps for Figure 3a and 3b.
# 12. Calculate non-facility birth due to EPE by country (Figure S5).
####################################################################################


####################################################################################
####  Step 1: Download and Merge WorldPop 2015 Birth Rasters for All Countries  #####
####################################################################################

# Mapping of ISO country codes to country names for easier reference and URL generation
country_names <- c(AGO = "Angola", BDI = "Burundi", BEN = "Benin", BFA = "Burkina_Faso", 
                   BWA = "Botswana", CAF = "Central_African_Republic", CIV = "Cote_D_ivoire", 
                   CMR = "Cameroon", COD = "Democratic_Republic_of_the_Congo", COG = "Congo", 
                   COM = "Comoros", CPV = "Cabo_Verde", DJI = "Djibouti", ERI = "Eritrea", 
                   ETH = "Ethiopia", GAB = "Gabon", GHA = "Ghana", GIN = "Guinea", 
                   GMB = "Gambia", GNB = "GuineaBissau", GNQ = "Equatorial_Guinea", 
                   KEN = "Kenya", LSO = "Lesotho", LBR = "Liberia", MDG = "Madagascar", 
                   MWI = "Malawi", MLI = "Mali", MOZ = "Mozambique", MRT = "Mauritania", 
                   MUS = "Mauritius", MYT = "Mayotte", NAM = "Namibia", NER = "Niger", 
                   NGA = "Nigeria", REU = "Reunion", RWA = "Rwanda", SEN = "Senegal", 
                   SYC = "Seychelles", SLE = "Sierra_Leone", SOM = "Somalia", SSD = "South_Sudan",
                   SWZ = "Swaziland", TCD = "Chad", TGO = "Togo", 
                   TZA = "United_Republic_Of_Tanzania", UGA = "Uganda", ZAF = "South_Africa", ZMB = "Zambia", 
                   ZWE = "Zimbabwe")

# Convert the mapping above into a data frame to facilitate operations like URL generation
country_names_df <- data.frame(
  country_codes = names(country_names),
  country_names = country_names,
  stringsAsFactors = FALSE)


# Function to generate download URL for birth data given a country code
generate_url <- function(country_code, country_name) {
  paste0("https://data.worldpop.org/GIS/Births/Individual_countries/", 
         country_code, "/", 
         country_name, "_1km_births.7z")
}

# Generate URLs for all countries using the mapping dataframe
urls <- mapply(generate_url, country_names_df$country_codes, country_names_df$country_names)

# Download and save the files to a specified directory
for (url in urls) {
  file_name <- paste0("data/raw/worldpop_birth/", basename(url))
  download.file(url, destfile = file_name, mode = "wb")
}

# Function to extract .7z (zipped) file and read the raster file using the archive package
read_raster_from_7z <- function(file_path, country_name) {
  
  # Define the directory where the extracted files will be stored, organized by country
  output_dir <- paste0("data/raw/worldpop_birth_extracted/", country_name, "_births/")
  
  # Create the directory if it does not exist, suppress warnings for existing directories
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Extract the .7z (zipped) file into the specified directory
  archive::archive_extract(file_path, dir = output_dir)
  
  # Construct a pattern to find the correct raster file based on country code and expected file naming conventions
  raster_file_pattern <- paste0(country_names_df[country_names_df$country_names==paste(country_name),]$country_codes, "_births_pp_v2_2015.tif") # country_names_df[country_names_df$country_names==paste(country_name),]$country_codes
  
  # List all files matching the pattern in the directory
  extracted_files <- list.files(output_dir, pattern = raster_file_pattern, full.names = TRUE)
  
  # Check if any files were found and load the raster data from the first matching file
  if (length(extracted_files) > 0) {
    raster_data <- raster(extracted_files[1])
    return(raster_data)
  } else {
    return(NULL)
  }
}

# Read and process raster files for each country listed in the dataframe and store in a list
raster_list <- list()
for (i in 1:nrow(country_names_df)) {
  country_name <- country_names_df$country_names[i]
  file_name <- paste0(country_name, "_1km_births.7z")
  file_path <- file.path("data/raw/worldpop_birth", file_name)
  
  # Use the previously defined function to read raster data from the .7z file
  raster_data <- read_raster_from_7z(file_path, country_name)
  
  # If raster data was successfully loaded, add it to the list using the country name as the key
  if (!is.null(raster_data)) {
    raster_list[[country_name]] <- raster_data
  }
}


# Merge all raster layers into a single RasterLayer
# Note: Convert the named list to an unnamed list to avoid merge failures and work only with double bracket
raster_list <- unname(raster_list)
worldpop_birth_raster <- do.call(merge, raster_list)

# Save the combined raster to a file
writeRaster(worldpop_birth_raster, filename = "data/raw/worldpop_birth_extracted/worldpop_birth_raster.tif", format = "GTiff", overwrite = TRUE)



####################################################################################
####  Step 2: Load Data for Attribution  (Coefficient, Exposure, Birth raster) #####
####################################################################################

## Load  all the necessary data
# (1) Data for regression to get baseline coefficients in the papers
# (2) Raster showing total exposure to EPEs between t and t-2 for each day in 2015 (computed in Google Earth Engine using script: EPE_Institutional_birth_attribution)
# (3) Worldpop birth raster of year 2015


# (1) Data for regression to get baseline coefficients in the papers
final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")

# (2) Raster showing total exposure to EPEs between t and t-2 for each day in 2015 (computed in Google Earth Engine using script: EPE_Institutional_birth_attribution)
epe_days <- rast("data/raw/EPE_events/Precipitation_Above_85th_Percentile_1DayWindow_2015_SSA.tif") # 5 km resolution # 1 days windows

# (3) Worldpop birth raster of year 2015
worldpop_birth_raster <- rast("data/raw/worldpop_birth_extracted/worldpop_birth_raster.tif") # 1 km resolution





####################################################################################
####  Step 3: Multiply the Baseline Coefficient with Exposure to EPEs in 2015  #####
####  Notes: This is equation (3) in the supplementary appendix                #####
####################################################################################


# Replicate baseline regression in Table 1 in column 4 for attribution
ols_fe_dhs_cbd <- feols(Facility_based_births*1000 ~ prec_85_count_3d_0_lag + factor(Wealth) + factor(Education) + factor(parity_at_birth_twins_corr_grp) + factor(twins_dummy) + agetatbirth | DHS_sample_cluster + Country_birth_date , final_data, cluster = "DHS_sample_cluster", weights = ~ rescaled_weight, fixef.rm= "singleton")

# Multiply the coefficient by the total exposure to EPEs between t and t-2 for each day in 2015  
format(object.size(epe_days), units = "Mb")
epe_days_exposure <- epe_days*(ols_fe_dhs_cbd$coeftable[1]/1000)*(-1) # 0.01075337 OR divied per thousand if Facility_based_births*1000

# Check if computation correct and save temporarily
plot(epe_days$Day_1)
plot(epe_days_exposure$Day_1)
terra::writeRaster(epe_days_exposure, "data/temp/epe_days_exposure.tiff", overwrite=TRUE)

# Multiply the confidence interval with exposure
coef_low <- confint(ols_fe_dhs_cbd, parm="prec_85_count_3d_0_lag", level = 0.95, se = "cluster")$`2.5 %`
coef_high <- confint(ols_fe_dhs_cbd, parm="prec_85_count_3d_0_lag", level = 0.95, se = "cluster")$`97.5 %`
epe_days_exposure_conf_low <- epe_days*(coef_low/1000)*(-1) 
epe_days_exposure_conf_high <- epe_days*(coef_high/1000)*(-1) 

# Save temporarily
terra::writeRaster(epe_days_exposure_conf_low, "data/temp/epe_days_exposure_conf_low.tiff", overwrite=TRUE)
terra::writeRaster(epe_days_exposure_conf_high, "data/temp/epe_days_exposure_conf_high.tiff", overwrite=TRUE)



####################################################################################
####  Step 4: Align and Resample Rasters to Common Resolutions                 #####
####################################################################################

# Get the number of birth per day assuming an uniform distribution of birth across the year 2015
# Notes: This is equation (2) in the supplementary appendix
days_in_2015 <- ymd("2016-01-01") - ymd("2015-01-01")
days_in_2015 <- as.numeric(days_in_2015, "days")
worldpop_birth_raster_daily <- worldpop_birth_raster/days_in_2015


# Resample and align resolution of EPE exposure (5km) to the resolution of the daily birth raster (1km)
epe_days_exposure_align <- terra::resample(epe_days_exposure, worldpop_birth_raster_daily, method = "near", threads=TRUE) # the bilinear applies weights based on the distance of the four nearest cell centers smoothing the output raster grid.
writeRaster(epe_days_exposure_align, "data/temp/epe_days_exposure_align_2015.tiff", overwrite=TRUE)

# Do the same with the confidence intervals
epe_days_exposure_conf_low_align <- terra::resample(epe_days_exposure_conf_low, worldpop_birth_raster_daily, method = "near", threads=TRUE) 
writeRaster(epe_days_exposure_conf_low_align, "data/temp/epe_days_exposure_conf_low_align_2015.tiff", overwrite=TRUE)
epe_days_exposure_conf_high_align <- terra::resample(epe_days_exposure_conf_high, worldpop_birth_raster_daily, method = "near", threads=TRUE) 
writeRaster(epe_days_exposure_conf_high_align, "data/temp/epe_days_exposure_conf_high_align_2015.tiff", overwrite=TRUE)


####################################################################################
####  Step 5: Compute Non-Facility Births Attributable to EPE per Pixel-Day    #####
####  Notes: This is equation (4) in the supplementary appendix                #####
####################################################################################

# Multiply the daily birth raster with the estimated changes in the probability of non-facility birth of each pixel-day of the year 2015 
# Notes: This is equation (4) in the supplementary appendix
epe_days_exposure_align_birth_2015 <- epe_days_exposure_align*worldpop_birth_raster_daily

# Check the computation
plot(epe_days_exposure_align_birth_2015$Day_1)
writeRaster(epe_days_exposure_align_birth_2015, "data/temp/epe_days_exposure_align_birth_2015.tiff", overwrite=TRUE) # epe_days_exposure_align_birth_2015 <- rast("data/temp/epe_days_exposure_align_birth_2015.tiff") 


# Multiply the daily birth raster with the estimated changes in the probability of non-facility birth of each pixel-day of the year 2015 for low/high conf interval
# Notes: This is equation (4) in the supplementary appendix
epe_days_exposure_conf_low_align_birth_2015 <- epe_days_exposure_conf_low_align*worldpop_birth_raster_daily
epe_days_exposure_conf_high_align_birth_2015 <- epe_days_exposure_conf_high_align*worldpop_birth_raster_daily



####################################################################################
####  Step 6: Aggregate Results to Annual                                      #####
####  Notes: This is equation (5) in the supplementary appendix                #####
####################################################################################


# Compute the total excess of non-facility birth due to EPE in 2015 for each pixel and save it
# Notes: This is equation (5) in the supplementary appendix
epe_days_exposure_align_birth_2015_sum <- app(epe_days_exposure_align_birth_2015, 
                                              fun = sum,
                                              na.rm = TRUE,
                                              cores = 5)
writeRaster(epe_days_exposure_align_birth_2015_sum, "data/temp/epe_days_exposure_align_birth_2015_sum.tiff", overwrite=TRUE)  # epe_days_exposure_align_birth_2015_sum <- rast("data/temp/epe_days_exposure_align_birth_2015_sum.tiff")


# Compute the total excess of non-facility birth due to EPE in 2015 for each pixelfor low/high conf interval  and save it
# Notes: This is equation (5) in the supplementary appendix
epe_days_exposure_conf_low_align_birth_2015_sum <- app(epe_days_exposure_conf_low_align_birth_2015, 
                                                       fun = sum,
                                                       na.rm = TRUE,
                                                       cores = 5)
writeRaster(epe_days_exposure_conf_low_align_birth_2015_sum, "data/temp/epe_days_exposure_conf_low_align_birth_2015_sum.tiff", overwrite=TRUE)  # epe_days_exposure_conf_low_align_birth_2015_sum <- rast("data/temp/epe_days_exposure_conf_low_align_birth_2015_sum.tiff")

epe_days_exposure_conf_high_align_birth_2015_sum <- app(epe_days_exposure_conf_high_align_birth_2015, 
                                                        fun = sum,
                                                        na.rm = TRUE,
                                                        cores = 5)
writeRaster(epe_days_exposure_conf_high_align_birth_2015_sum, "data/temp/epe_days_exposure_conf_high_align_birth_2015_sum.tiff", overwrite=TRUE)  # epe_days_exposure_conf_high_align_birth_2015_sum <- rast("data/temp/epe_days_exposure_conf_high_align_birth_2015_sum.tiff")


# Delete useless object
rm(epe_days, 
   epe_days_exposure,epe_days_exposure_conf_low, epe_days_exposure_conf_high,
   epe_days_exposure_align, epe_days_exposure_conf_low_align, epe_days_exposure_conf_high_align,
   ols_fe_dhs_cbd)



####################################################################################
####  Step 7: Prepare GADM Boundaries to Mask Results to the 21 SSA Countries  #####
####################################################################################

# Load GADM data at the ADM0 level and filter for my country of interest
st_layers("data/raw/GADM/gadm_410-levels.gpkg")
gadm_data <- st_read("data/raw/GADM/gadm_410-levels.gpkg",
                     layer= "ADM_0")


# final_data <- readRDS(file = "data/processed/final_data/final_data.Rds")
gadm_data <- gadm_data %>% filter(COUNTRY %in% unique(final_data$CountryName))



####################################################################################
####  Step 8: Prepare Data for Mapping (Figure 3a: Absolute Numbers)           #####
####  Notes: Mask results to the 21 SSA countries and resample to 20km X 20km  #####
####################################################################################

# Use the country raster to keep only attribution results from the study region
epe_days_exposure_align_birth_2015_sum_filtered <- terra::mask(epe_days_exposure_align_birth_2015_sum, gadm_data)

# Current resolution in degrees
current_resolution <- res(epe_days_exposure_align_birth_2015_sum_filtered)[1]

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

# Save in 20km X 20 km resolution for Visualisation in QGIS
epe_days_exposure_align_birth_2015_sum_filtered_20km <- terra::aggregate(epe_days_exposure_align_birth_2015_sum_filtered, fact=multiplication_factor, fun=sum, na.rm=TRUE)
writeRaster(epe_days_exposure_align_birth_2015_sum_filtered_20km, "data/processed/Attribution/epe_days_exposure_align_birth_2015_sum_filtered_20km.tiff", overwrite=TRUE)  # epe_days_exposure_align_birth_2015_sum_filtered_20km <- rast("data/processed/Attribution/epe_days_exposure_align_birth_2015_sum_filtered_20km.tiff")


########################################################################################
####  Step 9: Prepare Data for Mapping (Figure 3b: Per 1'000 Live Births)          #####
####  Notes: Mask and resample birth raster to 20km X 20km to rescale attribution  #####
####         in per 1,000 live births.                                             #####
########################################################################################

# We use the attribution raster of 20 km X 20 km
epe_days_exposure_align_birth_2015_sum_filtered_20km

# Lets compute the number live births in each 20 km X 20 km
worldpop_birth_raster <- rast("data/raw/worldpop_birth_extracted/worldpop_birth_raster.tif") # 1 km resolution

# Use the country raster to keep only birth from the study region before aggregating at a higher resolution otherwise I will pick up population in country outside my sample and have underestimated rate
worldpop_birth_raster_filtered <- terra::mask(worldpop_birth_raster, gadm_data)
worldpop_birth_raster_filtered_sum_20km <- terra::aggregate(worldpop_birth_raster_filtered, fact=multiplication_factor, fun=sum, na.rm=TRUE)
writeRaster(worldpop_birth_raster_filtered_sum_20km, "data/temp/worldpop_birth_raster_filtered_sum_20km.tiff", overwrite=TRUE)  # worldpop_birth_raster_filtered_sum_20km <- rast("data/temp/worldpop_birth_raster_filtered_sum_20km.tiff")

# Lets check the resolution of both
res(worldpop_birth_raster_filtered_sum_20km)
res(epe_days_exposure_align_birth_2015_sum_filtered_20km)
crs(worldpop_birth_raster_filtered_sum_20km)
crs(epe_days_exposure_align_birth_2015_sum_filtered_20km)

# Lets compute the number of additional HB per 1'000 live births
epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered <- (epe_days_exposure_align_birth_2015_sum_filtered_20km/worldpop_birth_raster_filtered_sum_20km)*1000

#Plot
plot(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered)
summary(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered)
as.data.frame(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered) %>% filter(sum==0)
hist(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered, breaks=100)

# Save the raster of attribution per 1'000 live birth for Visualisation in QGIS
writeRaster(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered, "data/processed/Attribution/epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered.tiff", overwrite=TRUE)  # epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered <- rast("data/processed/Attribution/epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered.tiff")



####################################################################################
####  Step 10: Prepare GADM Data for Mapping                                   #####
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
####  Step 11: Generate and Save Maps for Figure 3a and 3b                     #####
####################################################################################


#### Load the attribution data for mapping of Figure 3(a) and 3(b) 
# Load the attribution raster for Figure 3(a) 
epe_days_exposure_align_birth_2015_sum_filtered_20km <- rast("data/processed/Attribution/epe_days_exposure_align_birth_2015_sum_filtered_20km.tiff")

# Load the attribution raster for Figure 3(b) 
epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered <- rast("data/processed/Attribution/epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered.tiff")

# Set values to NA in pixels of the attribution rasters where there is 0 births in 2015 for better visualization
epe_days_exposure_align_birth_2015_sum_filtered_20km[worldpop_birth_raster_filtered_sum_20km == 0] <- NA
epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered[worldpop_birth_raster_filtered_sum_20km == 0] <- NA



#### Figure 3(a)
# Transform into raster dataframe
epe_days_exposure_align_birth_2015_sum_filtered_20km <- as.data.frame(epe_days_exposure_align_birth_2015_sum_filtered_20km, xy = TRUE, na.rm = TRUE)

# Define breaks and colors for Figure 3(a) based on QGIS styling
max(epe_days_exposure_align_birth_2015_sum_filtered_20km$sum) # maximum is 253.1848 so last break set to 254
breaks <- c(0, 1, 5, 10, 15, 20, 30, 50, 254)

# Colors extracted from QGIS styling document
colours <- c(
  "#faebdd",  # [0,1)
  "#f58860",  # [1,5)
  "#ec4c3e",  # [5,10)
  "#cb1b4f",  # [10,15)
  "#971c5b",  # [15,20)
  "#611f53",  # [20,30)
  "#30173a",  # [30,50)
  "#03051a"   # [50,Inf] or [50,254]
)

# Cut data into the defined categories
epe_days_exposure_align_birth_2015_sum_filtered_20km$bin <- cut(
  epe_days_exposure_align_birth_2015_sum_filtered_20km$sum,   
  breaks = breaks,
  include.lowest = TRUE, # include 0 in first bin
  right = FALSE           # intervals like [0,1), [1,5), …
)


# Create Map 3(a) - Number of non-facility births
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
  geom_tile(data = epe_days_exposure_align_birth_2015_sum_filtered_20km, aes(x = x, y = y, fill = bin)) +
  # 4. Define the discrete bins legend for the raster
  scale_fill_discrete(name   = "Number of non-facility births", 
                      type = colours) +                        
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
    legend.title.position  = "top",                               # Position legend title on top
    legend.title           = element_text(hjust=0.5, face = "bold"),         # Bold legend title and Center-align legend title
    legend.text            = element_text(size = 10, margin = margin(t = 5, unit = "pt")),  # Legend text styling
    legend.key.spacing.x   = unit(0, "cm"),                       # Remove horizontal gaps in legend
    legend.key             = element_rect(colour = NA, fill = NA),# Transparent legend keys
    legend.key.width       = unit(1.5, "cm"),                       # Set legend key width
    legend.key.height      = unit(0.5, "cm"),                     # Set legend key height
    legend.box.spacing     = unit(0.5, "cm")                      # Add spacing around the legend box
  )



#### Figure 3(b)
# Transform into raster dataframe
epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered <- as.data.frame(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered, xy = TRUE, na.rm = TRUE)


# Define breaks and colors for Figure 3(b) based on QGIS styling
max(epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered$sum) # maximum is 3.985593 so last break set to 4
breaks_b <- c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 4)


# Colors extracted from QGIS styling document
colors_b <- c(
  "#f79e75",  # [0,0.5)
  "#e94340",  # [0.5,1.0)
  "#a71b59",  # [1.0,1.5)
  "#541c4c",  # [1.5,2.0)
  "#2a1142",  # [2.0,2.5)
  "#03051a"   # [2.5,Inf] or [2.5,4]
)

# Cut into factor bins
epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered$bin <- cut(
  epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered$sum,
  breaks = breaks_b,
  include.lowest = TRUE,
  right = FALSE
)


# Create Map 3(b) - Per thousand rate
map_b <- ggplot() +
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
  geom_tile(data = epe_days_exposure_align_birth_2015_sum_20km_per_thsd_filtered, 
            aes(x = x, y = y, fill = bin)) +
  # 4. Define the discrete bins legend for the raster
  scale_fill_discrete(name   = "Non-facility births (per 1,000)", 
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
    legend.title.position  = "top",                               # Position legend title on top
    legend.title           = element_text(hjust=0.5, face = "bold"),         # Bold legend title and Center-align legend title
    legend.text            = element_text(size = 10, margin = margin(t = 5, unit = "pt")),  # Legend text styling
    legend.key.spacing.x   = unit(0, "cm"),                       # Remove horizontal gaps in legend
    legend.key             = element_rect(colour = NA, fill = NA),# Transparent legend keys
    legend.key.width       = unit(1.5, "cm"),                       # Set legend key width
    legend.key.height      = unit(0.5, "cm"),                     # Set legend key height
    legend.box.spacing     = unit(0.5, "cm")                      # Add spacing around the legend box
  )


####  Save individual maps if needed
ggsave(file = paste(output_path, "Figures/attribution_plot_absolute.pdf", sep = ""), 
       map_a, 
       width = 8, height = 8, dpi = 300)

ggsave(file = paste(output_path, "Figures/attribution_plot_per1000.pdf", sep = ""), 
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

####  Save the combined map
ggsave(file = paste(output_path, "Figures/attribution_plot_combined.pdf", sep = ""), 
       combined_map,
       width = 16, height = 8, dpi = 300)




####################################################################################
####  Step 12: Calculate Non-facility birth due to EPE by Country (Figure S11)  #####
####################################################################################

# We take the 1km raster for the zonal statisitcs instead of the 10km in order to minimize error
epe_days_exposure_align_birth_2015_sum

# Rasterize my country shapefile
country_raster <- terra::rasterize(gadm_data, epe_days_exposure_align_birth_2015_sum, field = "COUNTRY")
plot(country_raster)

# Use the country raster to keep only pixel from the study region
epe_days_exposure_align_birth_2015_sum_filtered <- terra::mask(epe_days_exposure_align_birth_2015_sum, country_raster)
plot(epe_days_exposure_align_birth_2015_sum_filtered)


# Create a figure of number of Non-facility birth attributable to 2015 EPEs by country
HB_by_country <- exact_extract(epe_days_exposure_align_birth_2015_sum_filtered,
                               gadm_data,
                               fun = c("sum"),
                               append_cols = c("COUNTRY", "GID_0"),
                               progress = TRUE)
HB_by_country


# Data manipulation with dplyr to arrange the countries based on the number of deaths
HB_by_country <- HB_by_country %>%
  arrange(desc(sum)) %>%
  rename(NFB_EPE_sum_2015 = sum)

# Calculate the 2015 country total birth for percentage calculation
total_NFB_EPE_sum_2015 <- sum(HB_by_country$NFB_EPE_sum_2015)

# Add a new column for the percentage of the 2015 country total birth
HB_by_country <- HB_by_country %>%
  mutate(percentage = NFB_EPE_sum_2015 / total_NFB_EPE_sum_2015 * 100)

# Create the figure of the number of Non-facility birth attributable to EPEs in 2015
HB_by_country_figure <- ggplot(HB_by_country, aes(x = reorder(COUNTRY, NFB_EPE_sum_2015), y = NFB_EPE_sum_2015)) +
  geom_bar(stat = "identity", fill = "#006600", color = "black") +  # Adding color for the outline
  coord_flip() +  # Flip coordinates to make horizontal bar plot
  labs(y = "Number of non-facility birth related to extreme precipitation events", 
       x = "") +
  geom_text(aes(label = sprintf("%.1f%%", percentage), y = NFB_EPE_sum_2015 + 0.05 * max(NFB_EPE_sum_2015)), hjust = 0.5, size = 5) +  # Add percentages as text above the bars
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

HB_by_country_figure

# Save the plot
ggsave(file = paste(output_path, "Figures/HB_by_country_figure.pdf", sep = ""), 
       width = 15, height = 10)

###### Compute Sum of Non-facility birth due to EPE ot mention the paper ######
# Sum of Non-facility birth due to EPE in the countries of interest
global(epe_days_exposure_align_birth_2015_sum_filtered, fun = 'sum', na.rm = TRUE) #  29084.63

# Use the country raster to keep only pixel from the study region for confidence intervals
epe_days_exposure_conf_low_align_birth_2015_sum_filtered <- terra::mask(epe_days_exposure_conf_low_align_birth_2015_sum, country_raster)
epe_days_exposure_conf_high_align_birth_2015_sum_filtered <- terra::mask(epe_days_exposure_conf_high_align_birth_2015_sum, country_raster)

# Sum of Non-facility birth due to EPE (conf low) in the countries of interest
global(epe_days_exposure_conf_low_align_birth_2015_sum_filtered, fun = 'sum', na.rm = TRUE) # 49508.58

# Sum of Non-facility birth due to EPE (conf high) in the countries of interest
global(epe_days_exposure_conf_high_align_birth_2015_sum_filtered, fun = 'sum', na.rm = TRUE) # 8660.682



