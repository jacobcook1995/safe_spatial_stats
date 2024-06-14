pca_space_division <- function(pca, axis_n_boxes) {
  #' Find plots spaced as evenly as possible in PCA space.
  #'
  #' This function divides the PCA space into boxes and finds the plot that
  #' is closest to the centre of each box and then returns it. This is
  #' weighted by the proportion of explained variance for each axis. This
  #' function only works for the 3 axes, and would need to be reworked if we
  #' wanted to consider more axes.
  #'
  #' @param pca The output of the prcomp function.
  #' @param axis_n_boxes List linking each PCA axis to the number of boxes there
  #'   should be along it.
  #' @return A dataframe containing information on every point that is nearest
  #'   to a centre.
  #' @export

  # Extract the scores from the PCA result
  scores <- as.data.frame(pca$x[, seq_along(axis_n_boxes)])
  scores$plot_id <- row.names(scores)
  row.names(scores) <- NULL

  # Get axis divisions - currently regular, could use quantile
  axis_splits <- list()

  for (ax in names(axis_n_boxes)) {
    axis_data <- scores[, ax]
    axis_splits[[ax]] <- seq(
      min(axis_data), max(axis_data),
      length = axis_n_boxes[[ax]] + 1
    )
  }

  # work out which box each point is in
  for (ax in names(axis_splits)) {
    scores[, paste(ax, "_box", sep = "")] <- findInterval(
      scores[, ax], axis_splits[[ax]],
      rightmost.closed = TRUE
    )
  }

  scores$box_id <- with(scores, sprintf("%i-%i-%i", PC1_box, PC2_box, PC3_box))

  # Find box centre coordinates as midpoint
  axis_centres <- list()
  for (ax in names(axis_splits)) {
    splits <- axis_splits[[ax]]
    axis_centres[[ax]] <- rev(rev(splits)[-1]) + diff(splits) / 2
  }

  # Get a dataframe
  axis_centres_coords <- expand.grid(axis_centres)
  names(axis_centres_coords) <- sprintf(
    "box_centre_%s",
    names(axis_centres_coords)
  )

  # Add a box index along each axis
  axis_centres_ids <- expand.grid(lapply(axis_centres, seq_along))
  axis_centres_coords$box_id <- with(
    axis_centres_ids, sprintf("%i-%i-%i", PC1, PC2, PC3)
  )

  # Identify distance to box centre for each observation
  scores <- merge(scores, axis_centres_coords, by = "box_id")

  # Find PCA summary extract proportion of explained variance for each axis
  pca_summary <- summary(pca)
  pca_weights <- c(
    pca_summary$importance[2],
    pca_summary$importance[5],
    pca_summary$importance[8]
  )

  # Calculated weighted (by proportion of explained variance) distance to box centre
  scores$dist_to_box_centre <- with(
    scores,
    sqrt(
      (pca_weights[1] * (PC1 - box_centre_PC1))^2 +
        (pca_weights[2] * (PC2 - box_centre_PC2))^2 +
        (pca_weights[3] * (PC3 - box_centre_PC3))^2
    )
  )

  grouped_by_box <- split(scores, f = as.factor(scores$box_id))
  closest_to_centre <- do.call(rbind, lapply(
    grouped_by_box,
    function(x) x[which.min(x$dist_to_box_centre), ]
  ))

  return(closest_to_centre)
}
