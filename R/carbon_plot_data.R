library(safedata)
library(dplyr)

source("./R/narrowing_functions.R")

# Set safedata directory to download data to
set_safe_dir("~/Documents/ReferenceMaterials/ve_datasets/safedata_dir")

# First looking for all datasets that include each relevant carbon plot
car_tow_data <- search_spatial(location = "CarTow")
car_b_n_data <- search_spatial(location = "Car_BN")
car_b_s_data <- search_spatial(location = "Car_BS")
car_e_data <- search_spatial(location = "Car_E")
car_d_1_data <- search_spatial(location = "Car_D1")
car_d_2_data <- search_spatial(location = "Car_D2")
car_bel_data <- search_spatial(location = "Car_Bel")
car_ser_data <- search_spatial(location = "Car_Ser")

# Put all the dataset lists into a single list
data_list <- list(
  car_tow_data, car_b_n_data, car_b_s_data, car_e_data,
  car_d_1_data, car_d_2_data, car_bel_data, car_ser_data
)

# Filter out all outdated and restricted datasets
for (i in seq_along(data_list)) {
  data_list[[i]] <- ignore_old_datasets(data_list[[i]])
}

# Loop over everything in the list to combine them
for (i in seq_along(data_list)) {
  if (i == 1) {
    data_for_all_plots <- data_list[[i]]
    data_for_any_plots <- data_list[[i]]
  } else {
    data_for_all_plots <- inner_join(
      data_for_all_plots, data_list[[i]]
    )
    data_for_any_plots <- full_join(
      data_for_any_plots, data_list[[i]]
    )
  }
}
