# This script reads in the existing SAFE project gazetteer and manually adds the
# three deadwood plots previously defined by Terhi Riutta
library(sf)

# Read in the existing safe project gazetteer
gazetteer <- st_read(file.path("./primary/gazetteer.geojson"))


############################
# Adding Terhi Riutta's deadwood plots (essentially vegetation plots outside
# the existing structure)
############################

# These point names and centroids are defined in the deadwood dataset that Terhi
# previously published (https://zenodo.org/records/4899608)
points_from_Terhi <- data.frame(
  name = c("OG3_DW1", "OG3_DW2", "OG3_DW3"),
  lon = c(116.970434, 116.967075, 116.965199),
  lat = c(4.733986, 4.7341235, 4.734606)
)

# Convert to sf object
deadwood_points <- st_as_sf(
  points_from_Terhi,
  coords = c("lat", "lon"),
  crs = 4326 # WGS84 (standard GeoJSON CRS)
)

# Rename columns
names(deadwood_points) <- c("location", "geometry")

# populate properties
deadwood_points$type <- "SAFE Sampling point"
deadwood_points$display_order <- 7
deadwood_points$fractal_order <- NA
deadwood_points$transect_order <- NA
deadwood_points$parent <- NA
deadwood_points$region <- "SAFE"
deadwood_points$plot_size <- "25m x 25m"
centroids <- st_coordinates(deadwood_points)
deadwood_points$centroid_x <- centroids[, 1]
deadwood_points$centroid_y <- centroids[, 2]
deadwood_points$source <- "Manual addition based on details provided by Terhi Riutta"
# Point location, so bounding box is just the point
deadwood_points$bbox_xmin <- centroids[, 1]
deadwood_points$bbox_xmax <- centroids[, 1]
deadwood_points$bbox_ymin <- centroids[, 2]
deadwood_points$bbox_ymax <- centroids[, 2]

# Merge the new points into the existing gazetteer
new_gazetteer <- rbind(gazetteer, deadwood_points)

# Save the updated gazetteer
st_write(new_gazetteer, file.path("./output/gazetteer.geojson"))
