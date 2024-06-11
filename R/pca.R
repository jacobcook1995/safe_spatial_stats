library(safedata)
library(raster)
library(sf)
library(dplyr)

# Set path to data folder on my local machine
data_folder <- "~/Documents/VirtualRainforest/datasets/"

# Set safedata directory
set_safe_dir(file.path(data_folder, "safedata_dir/"))

# Use gazetteer to find all vegetation plots
gazetteer <- load_gazetteer()
order_two_plots <- subset(gazetteer, fractal_order == 2)

# Reproject these plots into the tif files that the tif files that that get loaded
# in for later use
vegetation_plots <- st_transform(
  order_two_plots,
  crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs"
)

# Make a list with the codes identifying the plots that have already been sampled
sampled_plot_codes <- c(
  "B_", "C_", "E_", "LFE_", "VJR_", "RP_LFE_", "OG1_", "OG2_", "OG3_"
)
# Create a single regex looking for any of those patterns at the start of the string
sampled_plot_regex <- paste("^", sampled_plot_codes, collapse = "|", sep = "")
sampled_plots <- subset(vegetation_plots, grepl(sampled_plot_regex, location))

# Make data frames to store all data for both all vegetation plots and just the
# already sampled plots
data_all_plots <- data.frame(location = vegetation_plots$location)
data_sampled_plots <- data.frame(location = sampled_plots$location)

# Add the geometries into two data frames
data_all_plots <- merge(
  data_all_plots, vegetation_plots[, c("location", "geometry")],
  by = "location", all.x = TRUE
)
data_sampled_plots <- merge(
  data_sampled_plots, sampled_plots[, c("location", "geometry")],
  by = "location", all.x = TRUE
)

# Load in AGB data, changing the relevant column name to match
agb_data <- read.csv(
  file.path(
    data_folder,
    "random_bits/aboveground_biomass/LiDAR_Swinfield.csv"
  )
)
colnames(agb_data)[colnames(agb_data) == "ID"] <- "location"

# Select only the plot_id and agb columns from biomass_df
agb_data_selected <- agb_data %>% select(location, agb)

# Add the relevant AGB data to the data frames
data_all_plots <- left_join(data_all_plots, agb_data_selected, by = "location")
data_sampled_plots <- left_join(
  data_sampled_plots, agb_data_selected,
  by = "location"
)

# Load in microclimate tif data (this wasn't downloaded using safedata so can't be
# loaded using it)
t_max_data <- raster(file.path(
  data_folder, "random_bits/microclimate_data/T_max.tif"
))
t_mean_data <- raster(file.path(
  data_folder, "random_bits/microclimate_data/T_mean.tif"
))
vpd_max_data <- raster(file.path(
  data_folder, "random_bits/microclimate_data/VPD_max.tif"
))
vpd_mean_data <- raster(file.path(
  data_folder, "random_bits/microclimate_data/VPD_mean.tif"
))

# Now add all of the microclimate data into the two dataframes
data_all_plots$T_max <- extract(
  t_max_data, as(data_all_plots$geometry, "Spatial")
)
data_all_plots$T_mean <- extract(
  t_mean_data, as(data_all_plots$geometry, "Spatial")
)
data_all_plots$VPD_max <- extract(
  vpd_max_data, as(data_all_plots$geometry, "Spatial")
)
data_all_plots$VPD_mean <- extract(
  vpd_mean_data, as(data_all_plots$geometry, "Spatial")
)
data_sampled_plots$T_max <- extract(
  t_max_data, as(data_sampled_plots$geometry, "Spatial")
)
data_sampled_plots$T_mean <- extract(
  t_mean_data, as(data_sampled_plots$geometry, "Spatial")
)
data_sampled_plots$VPD_max <- extract(
  vpd_max_data, as(data_sampled_plots$geometry, "Spatial")
)
data_sampled_plots$VPD_mean <- extract(
  vpd_mean_data, as(data_sampled_plots$geometry, "Spatial")
)

# Load in the roughness dataset that we are interested in, this is the Wilson 2007
# method as it is the newer one
roughness_data <- raster(file.path(
  data_folder, "safedata_dir/630063/3697796/SRTM_UTM50N_TRI_Wilson2007.tif"
))
data_all_plots$roughness <- extract(
  roughness_data, as(data_all_plots$geometry, "Spatial")
)
data_sampled_plots$roughness <- extract(
  roughness_data, as(data_sampled_plots$geometry, "Spatial")
)

# Then load in the elevation, slope and aspect data
elevation_data <- raster(file.path(
  data_folder, "safedata_dir/630004/3490488/SRTM_UTM50N_processed.tif"
))
slope_data <- raster(file.path(
  data_folder, "safedata_dir/630004/3490488/SRTM_UTM50N_slope.tif"
))
aspect_data <- raster(file.path(
  data_folder, "safedata_dir/630004/3490488/SRTM_UTM50N_aspect.tif"
))
data_all_plots$elevation <- extract(
  elevation_data, as(data_all_plots$geometry, "Spatial")
)
data_all_plots$slope <- extract(
  slope_data, as(data_all_plots$geometry, "Spatial")
)
data_all_plots$aspect <- extract(
  aspect_data, as(data_all_plots$geometry, "Spatial")
)
data_sampled_plots$elevation <- extract(
  elevation_data, as(data_sampled_plots$geometry, "Spatial")
)
data_sampled_plots$slope <- extract(
  slope_data, as(data_sampled_plots$geometry, "Spatial")
)
data_sampled_plots$aspect <- extract(
  aspect_data, as(data_sampled_plots$geometry, "Spatial")
)
