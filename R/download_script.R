library(safedata)

# Set safedata directory to download data to
set_safe_dir("~/Documents/VirtualRainforest/datasets/safedata_dir")

# Want to include the microclimate data, so include its record ID. The record ID
# provided doesn't currently work, so I downloaded the files manually. I've left the
# code in as this is something likely to be fixed in future.
microclimate_id <- 7893600 # https://zenodo.org/records/7893600

# Want to include roughness estimates
roughness_id <- 3697796 # https://zenodo.org/records/3697796

# And the derived hydrological data
hydro_id <- 3490687 # https://zenodo.org/records/3490687

# Finally we want to include the SRTM elevation data, there is also ASTER elevation data
# but we only want to include one elevation dataset and believe SRTM to be more
# reliable.
elevation_id <- 3490488 # https://zenodo.org/records/3490488

tif_data_ids <- c(microclimate_id, roughness_id, hydro_id, elevation_id)

# We have already identified which datasets we want to download already, so we load them
# based on record ids. The option to download all files is needed here as we explictly
# want the tif files
download_safe_files(tif_data_ids, xlsx_only = FALSE)
