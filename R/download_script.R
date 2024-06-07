library(safedata)

# Find safedata directory to download data to and set
my_safe_directory <- getOption("safedata.dir")
set_safe_dir(my_safe_directory)

# Want to include the microclimate data, so include it's project ID
microclimate_id <- ???? #  https://zenodo.org/records/7893600

# Want to include roughness estimates
roughness_id <- ???? # https://zenodo.org/records/3697796

# And the derived hydrological data
hydro_id <- ?????? # https://zenodo.org/records/3490687


# Finally we want to include the SRTM elevation data, there is also ASTER elevation data
# but we only want to include one elevation dataset and believe SRTM to be more
# reliable.
elevation_id <- ???? # https://zenodo.org/records/3490488

tif_data_ids <- list(microclimate_id, roughness_id, hydro_id, elevation_id)

# We have already identified which datasets we want to download already, so we load them
# based on record ids. The option to download all files is needed here as we explictly
# want the tif files
download_safe_files(tif_data_ids, xlsx_only = FALSE)
