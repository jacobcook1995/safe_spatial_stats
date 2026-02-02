library(terra)

# ---- First step is to download the spatial data of interest -------

# We want to include the SRTM elevation (+ slope and aspect) data, there is also
# ASTER elevation data but we only want to include one elevation dataset and
# believe SRTM to be more reliable.
# As the safedata package doesn't currently work we instead manually download
# this data from https://zenodo.org/records/3490488

# Want to include roughness estimates. We want to use pre-calculated values as
# that saves effort. Two different calculation methods are used in the dataset.
# For our analysis we shall use Wilson et al 2007 as it's the newer approach.
# As the safedata package doesn't currently work we instead manually download
# this data from https://zenodo.org/records/3697796

# Water flow rates are probably something that impact soil properties quite a
# bit. This was previously calculated from the satellite elevation (etc) data,
# and will be far from perfect. However, it's worth including this as it's
# easily accessible data
# As the safedata package doesn't currently work we instead manually download
# this data from https://zenodo.org/records/3490687

# There's LIDAR data from the SAFE project and Maliau areas. There's canopy
# height models etc but for ease we are just going to take their estimates of
# above ground biomass.
# As the safedata package doesn't currently work we instead manually download
# this data from https://zenodo.org/records/4020697
# The two relevant files are "Maliau_acd.tif" and "SAFE_acd.tif", which contain
# the above ground carbon estimates for the Maliau Basin plots and the SAFE
# plots, respectively. I am not going to load them in this script as they have
# different extents and resolutions so can't be combined into a single plot

# All of the above datasets can be downloaded from Zenodo, so the download etc
# should happen in the `ve_data_science` repo

# Sentinel-2 quarterly mosaics can be downloaded from the Copernicus browser
# (https://browser.dataspace.copernicus.eu/). The area we are interested in
# (basically the central area of Sabah) is covered by 9 mosaics. We have
# downloaded these 9 mosaics for Q1 of 2024, which is the time period that most
# of the field work occurred within. We believe this is a good assumption as
# vegetation cover should not (generally) vary much month by month. However, we
# may have to revisit this if there are significant gaps in the coverage for the
# Q1 mosaic. This script now calculates the Enhanced Vegetation Index (EVI)
# based on these mosaics and returns a single file to be used for downstream
# analysis.

mosaic_dates <- c("2024_Q1")

mosaic_coords <-
  c("50NMK", "50NML", "50NMM", "50NNK", "50NNL", "50NNM", "50NPK", "50NPL", "50NPM")

# TODO - USE THESE DETAILS TO GENERATE THE FILE PATHS AND LOAD IN THE DATA

# TODO - ONCE WE HAVE SOIL TYPE DATA THIS SHOULD ALSO BE ADDED