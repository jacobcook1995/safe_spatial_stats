library(terra)

# This script is related to gather_spatial_covariates.R, and gather the spatial
# data on soil type. I'm keeping this as a separate script because the main
# script already has a step that takes a long time to execute.

# This script reprojects the Harmonized World Soil Database v2.0, (see
# https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/
#   harmonized-world-soil-database-v20/en/) to fit our study area. Nothing
# further is done with this because when I plotted the points I sampled over
# this it turned out all sampled points lay within a single soil area, so this
# offers no predictive power. Worth retaining this script though as it allows me
# to find the specific soil classification of this area

# Load in the downloaded raster for the soil database
soil_raster_path <- file.path("./primary/HWSD2_RASTER/HWSD2.bil")
soil_raster_full <- rast(soil_raster_path)

# Now need to reproject this raster to UTM50N and match the extent with the EVI
# raster extent, using nearest neighbour sampling to avoid averaging integer categories
soil_raster_utm50N <- project(soil_raster_full, "EPSG:32650", method = "near")

evi_raster <- rast("Sabah_EVI_2024_Q1.tif")

soil_raster_reduced <- crop(soil_raster_utm50N, ext(evi_raster))

writeRaster(soil_raster_reduced, "soil_map.tif", overwrite = TRUE)
