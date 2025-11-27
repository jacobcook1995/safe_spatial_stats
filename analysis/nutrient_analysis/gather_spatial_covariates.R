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

# TODO - ONCE WE HAVE SOIL TYPE DATA THIS SHOULD ALSO BE ADDED
# TODO - WORK OUT HOW TO DOWNLOAD THE RELEVANT SATELLITE PRODUCTS