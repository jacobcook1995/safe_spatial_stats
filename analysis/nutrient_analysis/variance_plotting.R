# Basic script to look the variance of my soil nutrient data
library(terra)
library(sf)
library(dplyr)

plot_variances <-
  function(plot_mean, plot_sd, x_axis, plot_name, plot_title, x_unit, y_unit) {
    #' Plot the variances of a given set of data
    #'
    #' This is written as a function because I basically repeat the same steps for
    #' four different variables.
    #'
    #' @param plot_mean Mean values of variable across the plots
    #' @param plot_sd Standard deviations of the variable across the plots
    #' @param x_axis Values to plot against on the x-axis
    #' @param plot_name File name to save plot under
    #' @param plot_title Title for the plot
    #' @param x_unit Unit of the plots x-axis
    #' @param y_unit Unit of the plots y-axis
    #' @return
    #' @export

    png(plot_name, width = 1200, height = 900)

    # Increase margin sizes so that labels don't get clipped off
    par(mar = c(6, 7, 4, 2))

    # Colour code values without standard deviations
    cols <- ifelse(is.na(plot_sd), "red", "black")

    plot(x_axis, plot_mean,
      ylim = c(
        min(plot_mean - plot_sd, na.rm = TRUE),
        max(plot_mean + plot_sd, na.rm = TRUE)
      ),
      pch = 16, xlab = x_unit, ylab = y_unit, col = cols,
      main = plot_title, cex.lab = 2, cex.main = 2
    )

    # Only rows where sd is not NA
    idx <- !is.na(plot_sd)

    legend("topright",
      legend = c("Bulked sample", "Subsampled"),
      col = c("red", "black"), pch = 16
    )

    arrows(
      x0 = x_axis[idx],
      y0 = plot_mean[idx] - plot_sd[idx],
      x1 = x_axis[idx],
      y1 = plot_mean[idx] + plot_sd[idx],
      angle = 90,
      code = 3,
      length = 0.05
    )

    # Save plot by closing
    dev.off()
  }

plot_mean_vs_variance <-
  function(plot_mean, plot_sd, plot_name, plot_title, x_unit, y_unit) {
    #' Plot the mean vs variance for a given variable
    #'
    #' This is written as a function because I basically repeat the same steps for
    #' four different variables.
    #'
    #' @param plot_mean Mean values of variable across the plots
    #' @param plot_sd Standard deviations of the variable across the plots
    #' @param plot_name File name to save plot under
    #' @param plot_title Title for the plot
    #' @param x_unit Unit of the plots x-axis
    #' @param y_unit Unit of the plots y-axis
    #' @return
    #' @export

    png(plot_name, width = 1200, height = 900)

    # Increase margin sizes so that labels don't get clipped off
    par(mar = c(6, 7, 4, 2))

    # First plot/analyse rows where sd is not NA
    idx <- !is.na(plot_sd)

    plot(plot_mean[idx], plot_sd[idx],
      pch = 16, xlab = x_unit, ylab = y_unit, main = plot_title, cex.lab = 2,
      cex.main = 2, col = "black"
    )
    # Making a logarithmic model to test the relationship
    model_log <- lm(log(plot_sd[idx]) ~ log(plot_mean[idx]))

    # Plot line showing best fit prediction
    log_intercept <- coef(model_log)[1]
    curvature <- coef(model_log)[2]
    curve(exp(log_intercept) * x^curvature, add = TRUE, col = "blue", lwd = 2) # nolint: object_usage_linter

    # Save plot by closing
    dev.off()
  }

make_plain_box_plot <-
  function(core_data, plot_data, var_to_plot, plot_name, plot_title, y_unit) {
    #' Make box plots of individual sample data for a variable of interest
    #'
    #' This plots the box plots by plot number. It is written as a function
    #' because I basically repeat the same steps for four different variables.
    #'
    #' @param core_data Full set of data for the cores
    #' @param var_to_plot Name of variable to be plotted
    #' @param plot_name File name to save plot under
    #' @param plot_title Title for the plot
    #' @param y_unit Unit of the plots y-axis
    #' @return
    #' @export

    png(plot_name, width = 1200, height = 900)

    # Increase margin sizes so that labels don't get clipped off
    par(mar = c(6, 7, 4, 2))

    plotting_formula <- reformulate("plot_code", response = var_to_plot)

    boxes <- boxplot(plotting_formula, data = core_data, plot = FALSE)
    bxp(boxes,
      outline = FALSE, main = plot_title, cex.main = 2, ylab = y_unit, cex.lab = 2
    )
    stripchart(plotting_formula,
      data = core_data, method = "jitter", pch = 16,
      vertical = TRUE, col = "blue", add = TRUE
    )

    legend("topright",
      legend = c("Bulked sample", "Subsampled"),
      col = c("red", "blue"), pch = 16
    )

    # Save plot by closing
    dev.off()
  }

make_biomass_box_plot <-
  function(core_data, plot_data, var_to_plot, plot_name, plot_title, y_unit) {
    #' Make box plots of individual sample data for a variable of interest
    #'
    #' These box plots are arranged relative to above ground biomass. It is
    #' written as a function because I basically repeat the same steps for four
    #' different variables.
    #'
    #' @param core_data Full set of data for the cores
    #' @param plot_data Full set of data for each plot
    #' @param var_to_plot Name of variable to be plotted
    #' @param plot_name File name to save plot under
    #' @param plot_title Title for the plot
    #' @param y_unit Unit of the plots y-axis
    #' @return
    #' @export

    png(plot_name, width = 1200, height = 900)

    # Increase margin sizes so that labels don't get clipped off
    par(mar = c(6, 7, 4, 2))

    # Define plotting formula and use to calculate box plots
    plotting_formula <- reformulate("plot_code", response = var_to_plot)
    boxes <- boxplot(plotting_formula, data = core_data, plot = FALSE)

    # Find position of box plots along the biomass axis
    biomasses <- data.frame(
      plot_code = boxes$names,
      agb = plot_data$agb[match(boxes$names, plot_data$plot_code)]
    )

    # Then actually plot everything
    bxp(boxes,
      at = biomasses$agb, outline = FALSE, main = plot_title, cex.main = 2,
      ylab = y_unit, cex.lab = 2, xaxt = "n",
      xlab = "Above ground biomass (tonnes per hectare)"
    )
    axis(1, at = pretty(biomasses$agb))
    stripchart(plotting_formula,
      at = biomasses$agb, data = core_data, method = "jitter", pch = 16,
      vertical = TRUE, col = "blue", add = TRUE
    )

    legend("topright",
      legend = c("Bulked sample", "Subsampled"),
      col = c("red", "blue"), pch = 16
    )

    # Save plot by closing
    dev.off()
  }

# First load in the data (only care about the plot level summaries). Need to
# make sure to skip header information.
plot_data <- readxl::read_xlsx(
  "./output/SAFE_soil_nutrient_data.xlsx",
  sheet = "PlotData", skip = 4
)

# Convert standard deviation data to be numeric
plot_data$sd_total_carbon <- as.numeric(plot_data$sd_total_carbon)
plot_data$sd_total_nitrogen <- as.numeric(plot_data$sd_total_nitrogen)
plot_data$sd_total_phosphorus <- as.numeric(plot_data$sd_total_phosphorus)
plot_data$sd_available_phosphorus <- as.numeric(plot_data$sd_available_phosphorus)

# Read in the safe project gazetteer
gazetteer <- st_read(file.path("./output/gazetteer.geojson"))

# Reproject to UTM zone 50N
reprojected_gazetteer <- st_transform(gazetteer, crs = "EPSG:32650")

# Find new values for the centroids and replace the values in the gazetteer
centroids <- st_centroid(reprojected_gazetteer)

reprojected_gazetteer <- reprojected_gazetteer %>%
  mutate(
    centroid_x = st_coordinates(centroids)[, 1],
    centroid_y = st_coordinates(centroids)[, 2]
  )


# Join the gazetteer data to the soil data based on plot codes
plot_data <- plot_data %>%
  left_join(
    st_drop_geometry(reprojected_gazetteer),
    by = c("plot_code" = "location")
  )

# Load in elevation data
elevation_raster <- rast(file.path("./primary/SRTM_UTM50N_processed.tif"))

# Convert centroids from plot_data into points, and then extract relevant
# elevation values based on them
plot_centers <-
  vect(plot_data, geom = c("centroid_x", "centroid_y"), crs = crs(elevation_raster))

plot_data$elevation <- extract(elevation_raster, plot_centers)[, 2]

# Load in EVI data and add as a column
evi_raster <- rast(file.path("./output/Sabah_EVI_2024_Q1.tif"))
plot_data$evi <- extract(evi_raster, plot_centers)[, 2]

# Load in LIDAR data
lidar_data <- read.csv(file.path("./primary/LiDAR_Swinfield.csv"))

# Add the AGB data to the existing plot dataframe
plot_data <- plot_data %>%
  left_join(lidar_data %>% select(ID, agb), by = c("plot_code" = "ID"))

# Look at variation vs elevation first

# For total carbon
plot_variances(
  plot_mean = plot_data$total_carbon, plot_sd = plot_data$sd_total_carbon,
  x_axis = plot_data$elevation, plot_name = "figures/total_carbon_vs_elevation.png",
  plot_title = "Variation in total carbon with elevation",
  x_unit = "Elevation (m)", y_unit = "Total Carbon (%)"
)

# Total nitrogen
plot_variances(
  plot_mean = plot_data$total_nitrogen, plot_sd = plot_data$sd_total_nitrogen,
  x_axis = plot_data$elevation, plot_name = "figures/total_nitrogen_vs_elevation.png",
  plot_title = "Variation in total nitrogen with elevation",
  x_unit = "Elevation (m)", y_unit = "Total Nitrogen (%)"
)

# Total phosphorus
plot_variances(
  plot_mean = plot_data$total_phosphorus, plot_sd = plot_data$sd_total_phosphorus,
  x_axis = plot_data$elevation, plot_name = "figures/total_phosphorus_vs_elevation.png",
  plot_title = "Variation in total phosphorus with elevation",
  x_unit = "Elevation (m)", y_unit = "Total Phosphorus (mg/kg)"
)

# Available phosphorus
plot_variances(
  plot_mean = plot_data$available_phosphorus,
  plot_sd = plot_data$sd_available_phosphorus, x_axis = plot_data$elevation,
  plot_name = "figures/available_phosphorus_vs_elevation.png",
  x_unit = "Elevation (m)", y_unit = "Available Phosphorus (mg/kg)",
  plot_title = "Variation in available phosphorus with elevation"
)

# Then look a variation vs EVI

# For total carbon
plot_variances(
  plot_mean = plot_data$total_carbon, plot_sd = plot_data$sd_total_carbon,
  x_axis = plot_data$evi, plot_name = "figures/total_carbon_vs_EVI.png",
  plot_title = "Variation in total carbon with remotely sensed biomass",
  x_unit = "Enhanced vegetation index", y_unit = "Total Carbon (%)"
)

# Total nitrogen
plot_variances(
  plot_mean = plot_data$total_nitrogen, plot_sd = plot_data$sd_total_nitrogen,
  x_axis = plot_data$evi, plot_name = "figures/total_nitrogen_vs_EVI.png",
  plot_title = "Variation in total nitrogen with remotely sensed biomass",
  x_unit = "Enhanced vegetation index", y_unit = "Total Nitrogen (%)"
)

# Total phosphorus
plot_variances(
  plot_mean = plot_data$total_phosphorus, plot_sd = plot_data$sd_total_phosphorus,
  x_axis = plot_data$evi, plot_name = "figures/total_phosphorus_vs_EVI.png",
  plot_title = "Variation in total phosphorus with remotely sensed biomass",
  x_unit = "Enhanced vegetation index", y_unit = "Total Phosphorus (mg/kg)"
)

# Available phosphorus
plot_variances(
  plot_mean = plot_data$available_phosphorus,
  plot_sd = plot_data$sd_available_phosphorus, x_axis = plot_data$evi,
  plot_name = "figures/available_phosphorus_vs_EVI.png",
  x_unit = "Enhanced vegetation index", y_unit = "Available Phosphorus (mg/kg)",
  plot_title = "Variation in available phosphorus with remotely sensed biomass"
)

# Then look a variation vs LIDAR derived AGB

# For total carbon
plot_variances(
  plot_mean = plot_data$total_carbon, plot_sd = plot_data$sd_total_carbon,
  x_axis = plot_data$agb, plot_name = "figures/total_carbon_vs_agb.png",
  plot_title = "Variation in total carbon with LIDAR estimated biomass",
  x_unit = "Above ground biomass (tonnes per hectare)", y_unit = "Total Carbon (%)"
)

# Total nitrogen
plot_variances(
  plot_mean = plot_data$total_nitrogen, plot_sd = plot_data$sd_total_nitrogen,
  x_axis = plot_data$agb, plot_name = "figures/total_nitrogen_vs_agb.png",
  plot_title = "Variation in total nitrogen with LIDAR estimated biomass",
  x_unit = "Above ground biomass (tonnes per hectare)", y_unit = "Total Nitrogen (%)"
)

# Total phosphorus
plot_variances(
  plot_mean = plot_data$total_phosphorus, plot_sd = plot_data$sd_total_phosphorus,
  x_axis = plot_data$agb, plot_name = "figures/total_phosphorus_vs_agb.png",
  plot_title = "Variation in total phosphorus with LIDAR estimated biomass",
  x_unit = "Above ground biomass (tonnes per hectare)",
  y_unit = "Total Phosphorus (mg/kg)"
)

# Available phosphorus
plot_variances(
  plot_mean = plot_data$available_phosphorus,
  plot_sd = plot_data$sd_available_phosphorus, x_axis = plot_data$agb,
  plot_name = "figures/available_phosphorus_vs_agb.png",
  x_unit = "Above ground biomass (tonnes per hectare)",
  y_unit = "Available Phosphorus (mg/kg)",
  plot_title = "Variation in available phosphorus with LIDAR estimated biomass"
)

# To make boxplots I need the full core data. Need to make sure to skip header
# information.
core_data <- readxl::read_xlsx(
  "./output/SAFE_soil_nutrient_data.xlsx",
  sheet = "CoreData", skip = 4, na = "NA"
)

# Plot the simple box plots first
# Plot total carbon
make_plain_box_plot(
  core_data = core_data, var_to_plot = "total_carbon",
  plot_name = "figures/plain_box_plot_total_carbon.png",
  plot_title = "Variation in total carbon with LIDAR estimated biomass",
  y_unit = "Total Carbon (%)"
)

# total nitrogen
make_plain_box_plot(
  core_data = core_data, var_to_plot = "total_nitrogen",
  plot_name = "figures/plain_box_plot_total_nitrogen.png",
  plot_title = "Variation in total nitrogen with LIDAR estimated biomass",
  y_unit = "Total Nitrogen (%)"
)

# total phosphorus
make_plain_box_plot(
  core_data = core_data, var_to_plot = "total_phosphorus",
  plot_name = "figures/plain_box_plot_total_phosphorus.png",
  plot_title = "Variation in total phosphorus with LIDAR estimated biomass",
  y_unit = "Total Phosphorus (mg/kg)"
)

# available phosphorus
make_plain_box_plot(
  core_data = core_data, var_to_plot = "available_phosphorus",
  plot_name = "figures/plain_box_plot_available_phosphorus.png",
  plot_title = paste0(
    "Variation in available phosphorus with LIDAR ",
    "estimated biomass"
  ),
  y_unit = "Available Phosphorus (mg/kg)"
)

# Then plot the box plots that are shown in the context of the other data
# Plot total carbon
make_biomass_box_plot(
  core_data = core_data, plot_data = plot_data, var_to_plot = "total_carbon",
  plot_name = "figures/box_plot_total_carbon_vs_agb.png",
  plot_title = "Variation in total carbon with LIDAR estimated biomass",
  y_unit = "Total Carbon (%)"
)

# total nitrogen
make_biomass_box_plot(
  core_data = core_data, plot_data = plot_data, var_to_plot = "total_nitrogen",
  plot_name = "figures/box_plot_total_nitrogen_vs_agb.png",
  plot_title = "Variation in total nitrogen with LIDAR estimated biomass",
  y_unit = "Total Nitrogen (%)"
)

# total phosphorus
make_biomass_box_plot(
  core_data = core_data, plot_data = plot_data, var_to_plot = "total_phosphorus",
  plot_name = "figures/box_plot_total_phosphorus_vs_agb.png",
  plot_title = "Variation in total phosphorus with LIDAR estimated biomass",
  y_unit = "Total Phosphorus (mg/kg)"
)

# available phosphorus
make_biomass_box_plot(
  core_data = core_data, plot_data = plot_data, var_to_plot = "available_phosphorus",
  plot_name = "figures/box_plot_available_phosphorus_vs_agb.png",
  plot_title = paste0(
    "Variation in available phosphorus with LIDAR ",
    "estimated biomass"
  ),
  y_unit = "Available Phosphorus (mg/kg)"
)

# TODO - STILL NEED TO ADD PROPER DENSITY KERNELS (COMPARING TOTAL DISTRIBUTION
# OF MEANS VS SUBSAMPLED)
# Now plot mean vs standard deviations
# for total carbon
plot_mean_vs_variance(
  plot_mean = plot_data$total_carbon,
  plot_sd = plot_data$sd_total_carbon,
  plot_name = "figures/mean_vs_sd_total_carbon.png",
  x_unit = "Mean total carbon (%)",
  y_unit = "Standard deviation total carbon (%)",
  plot_title = "Standard deviation vs mean for total carbon"
)

# total nitrogen
plot_mean_vs_variance(
  plot_mean = plot_data$total_nitrogen,
  plot_sd = plot_data$sd_total_nitrogen,
  plot_name = "figures/mean_vs_sd_total_nitrogen.png",
  x_unit = "Mean total nitrogen (%)",
  y_unit = "Standard deviation total nitrogen (%)",
  plot_title = "Standard deviation vs mean for total nitrogen"
)

# total phosphorus
plot_mean_vs_variance(
  plot_mean = plot_data$total_phosphorus,
  plot_sd = plot_data$sd_total_phosphorus,
  plot_name = "figures/mean_vs_sd_total_phosphorus.png",
  x_unit = "Mean total phosphorus (mg/kg)",
  y_unit = "Standard deviation total phosphorus (mg/kg)",
  plot_title = "Standard deviation vs mean for total phosphorus"
)

# available phosphorus
plot_mean_vs_variance(
  plot_mean = plot_data$available_phosphorus,
  plot_sd = plot_data$sd_available_phosphorus,
  plot_name = "figures/mean_vs_sd_available_phosphorus.png",
  x_unit = "Mean available phosphorus (mg/kg)",
  y_unit = "Standard deviation available phosphorus (mg/kg)",
  plot_title = "Standard deviation vs mean for available phosphorus"
)
