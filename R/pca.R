library(safedata)
library(raster)
library(sf)
library(dplyr)

source("./R/plotting_functions.R")
source("./R/pca_space_division.R")

# Set path to data folder on my local machine
data_folder <- "~/Documents/VirtualRainforest/datasets/"
output_folder <- "./output"

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

# Make data frames to store all data for both all vegetation plots and just the
# already sampled plots
data_all_plots <- data.frame(location = vegetation_plots$location)

# Add information to the dataframe about whether the plot has been sampled
sampled_plot_codes <- c(
  "B_", "C_", "E_", "LFE_", "VJR_", "RP_LFE_", "OG1_", "OG2_", "OG3_", "DW_"
)
sampled_plot_regex <- paste("^", sampled_plot_codes, collapse = "|", sep = "")
# Add a new column to indicate if a plot is sampled
data_all_plots$sampled <- ifelse(
  grepl(sampled_plot_regex, data_all_plots$location), TRUE, FALSE
)

# Add the geometries into two data frames
data_all_plots <- merge(
  data_all_plots, vegetation_plots[, c("location", "geometry")],
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

# Define a function to replace NA values with the average of the nearest neighbors
fill_na_with_nearest <- function(x) {
  if (is.na(x[5])) { # The center cell in the 3x3 window
    return(mean(x, na.rm = TRUE))
  } else {
    return(x[5]) # Return the original value if it's not NA
  }
}

# Apply the focal function to fill NA values for the 4 microclimate datasets
t_max_data <- focal(t_max_data,
  w = matrix(1, 3, 3), fun = fill_na_with_nearest, na.rm = FALSE, pad = TRUE
)
t_mean_data <- focal(t_mean_data,
  w = matrix(1, 3, 3), fun = fill_na_with_nearest, na.rm = FALSE, pad = TRUE
)
vpd_max_data <- focal(vpd_max_data,
  w = matrix(1, 3, 3), fun = fill_na_with_nearest, na.rm = FALSE, pad = TRUE
)
vpd_mean_data <- focal(vpd_mean_data,
  w = matrix(1, 3, 3), fun = fill_na_with_nearest, na.rm = FALSE, pad = TRUE
)

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

# Load in the roughness dataset that we are interested in, this is the Wilson 2007
# method as it is the newer one
roughness_data <- raster(file.path(
  data_folder, "safedata_dir/630063/3697796/SRTM_UTM50N_TRI_Wilson2007.tif"
))
data_all_plots$roughness <- extract(
  roughness_data, as(data_all_plots$geometry, "Spatial")
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

# Now that all data has been loaded in make the location column a row name
rownames(data_all_plots) <- data_all_plots$location
data_all_plots <- data_all_plots[, -1]

# Okay first want to do the PCA using all plots and data but with microclimate data
# excluded, if one microclimate variable is missing they all are, so can just filter out
# max temperature
plots_with_all_data <- data_all_plots[complete.cases(data_all_plots$T_max), ]

pca_with_microclimate <- prcomp(
  plots_with_all_data[
    , !(names(plots_with_all_data) %in% c("geometry", "sampled"))
  ],
  scale = TRUE
)
# Add information about sampled plots to the PCA result
pca_with_microclimate$sampled <- plots_with_all_data$sampled

# Plot first three PCA axes
png(file.path(output_folder, "microclimate_pca_1.png"),
  width = 800, height = 600
)
nice_pca_plot(pca_with_microclimate, axis_1 = 1, axis_2 = 2)
dev.off()
png(file.path(output_folder, "microclimate_pca_2.png"),
  width = 800, height = 600
)
nice_pca_plot(pca_with_microclimate, axis_1 = 1, axis_2 = 3)
dev.off()
png(file.path(output_folder, "microclimate_pca_3.png"),
  width = 800, height = 600
)
nice_pca_plot(pca_with_microclimate, axis_1 = 2, axis_2 = 3)
dev.off()

# Separate out Maliau plots for separate PCA analysis
maliau_plot_codes <- c("OG1_", "OG2_", "OG3_")
maliau_plot_regex <- paste("^", maliau_plot_codes, collapse = "|", sep = "")

data_maliau_plots <- data_all_plots[
  grepl(maliau_plot_regex, rownames(data_all_plots)),
]
# Remove all the microclimate data as there isn't any for Maliau
data_maliau_plots <- subset(
  data_maliau_plots,
  select = -c(T_max, T_mean, VPD_max, VPD_mean)
)

# Now run the PCA for the Maliau plots
pca_maliau <- prcomp(
  data_maliau_plots[
    , !(names(data_maliau_plots) %in% c("geometry", "sampled"))
  ],
  scale = TRUE
)
# Add information about sampled plots to the PCA result
pca_maliau$sampled <- data_maliau_plots$sampled
png(file.path(output_folder, "maliau_pca_1.png"),
  width = 800, height = 600
)
nice_pca_plot(pca_maliau, axis_1 = 1, axis_2 = 2)
dev.off()
png(file.path(output_folder, "maliau_pca_2.png"),
  width = 800, height = 600
)
nice_pca_plot(pca_maliau, axis_1 = 1, axis_2 = 3)
dev.off()
png(file.path(output_folder, "maliau_pca_3.png"),
  width = 800, height = 600
)
nice_pca_plot(pca_maliau, axis_1 = 2, axis_2 = 3)
dev.off()

# Have decided on 8 plots for Maliau and 32 for the wider SAFE project area

# Basically just hard coding the weights in for Maliau
maliau_axis_n_boxes <- list("PC1" = 2, "PC2" = 2, "PC3" = 2)

maliau_samples <- pca_space_division(pca_maliau, maliau_axis_n_boxes)

# Do the PCA analysis for just the sampled points in the SAFE project area
sampled_safe_area <- plots_with_all_data %>% filter(sampled == TRUE)
pca_sampled_safe <- prcomp(
  sampled_safe_area[
    , !(names(sampled_safe_area) %in% c("geometry", "sampled"))
  ],
  scale = TRUE
)

safe_axis_n_boxes <- list("PC1" = 8, "PC2" = 4, "PC3" = 2)

safe_samples <- pca_space_division(pca_sampled_safe, safe_axis_n_boxes)

# THEN FINAL THING IS TO SHOW PLOTS WITH THE POINTS ON
