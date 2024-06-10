library(safedata)
library(raster)
library(sf)

# Set safedata directory
set_safe_dir("~/Documents/VirtualRainforest/datasets/safedata_dir")

# Use gazetteer to find all vegetation plots
gazetteer <- load_gazetteer()
order_two_plots <- subset(gazetteer, fractal_order == 2)

# Reproject these plots into the tif files that the tif files that that get loaded in later use
vegetation_plots <- st_transform(order_two_plots, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs")

# Make a list with the codes identifying the plots that have already been sampled
sampled_plot_codes <- c(
    "B_", "C_", "E_", "LFE_", "VJR_", "RP_LFE_", "OG1_", "OG2_", "OG3_"
)
# Create a single regex looking for any of those patterns at the start of the string
sampled_plot_regex <- paste("^", sampled_plot_codes, collapse = "|", sep = "")
sampled_plots <- subset(vegetation_plots, grepl(sampled_plot_regex, location))

# Load in AGB data
agb_data <- read.csv(
    "~/Documents/VirtualRainforest/datasets/random_bits/aboveground_biomass/LiDAR_Swinfield.csv"
)

# Load in microclimate tif data (this wasn't downloaded using safedata so can't be
# loaded using it)
T_max_data <- raster("~/Documents/VirtualRainforest/datasets/random_bits/microclimate_data/T_max.tif")
T_mean_data <- raster("~/Documents/VirtualRainforest/datasets/random_bits/microclimate_data/T_mean.tif")
VPD_max_data <- raster("~/Documents/VirtualRainforest/datasets/random_bits/microclimate_data/VPD_max.tif")
VPD_mean_data <- raster("~/Documents/VirtualRainforest/datasets/random_bits/microclimate_data/VPD_mean.tif")

# Load in the roughness dataset that we are interested in, this is the Wilson 2007
# method as it is the newer one
roughness_data <- raster("~/Documents/VirtualRainforest/datasets/safedata_dir/630063/3697796/SRTM_UTM50N_TRI_Wilson2007.tif")

# Then load in the elevation data
elevation_data <- raster("~/Documents/VirtualRainforest/datasets/safedata_dir/630004/3490488/SRTM_UTM50N_processed.tif")
slope_data <- raster("~/Documents/VirtualRainforest/datasets/safedata_dir/630004/3490488/SRTM_UTM50N_slope.tif")
aspect_data <- raster("~/Documents/VirtualRainforest/datasets/safedata_dir/630004/3490488/SRTM_UTM50N_aspect.tif")

# Need to then put everything into a PCA. Input is a dataframe, so need to build a frame
# as I go
