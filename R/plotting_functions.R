nice_pca_plot <- function(pca, axis_1 = 1, axis_2 = 2) {
  #' Make a nice biplot of two pca axes
  #'
  #' This function makes a clean plot of a PCA plot, this is for two axes that
  #' can be chosen by the user if no components are chosen then it defaults to
  #' axis 1 and axis 2.

  #' @param pca The output of the prcomp function.
  #' @param axis_1 The first axis to plot
  #' @param axis_2 The second axis to plots
  #' @export

  # First find the summary for the PCA
  pca_summary <- summary(pca)

  # Define arrow and label positions
  arrows_x_pos <- pca$rotation[, axis_1] * 7.5
  arrows_y_pos <- pca$rotation[, axis_2] * 7.5
  labels_y_pos <- arrows_y_pos # Create a vector of y axis coordinates
  # Find low and high variables
  low <- which(arrows_y_pos < 0)
  high <- which(arrows_y_pos > 0)
  labels_y_pos <- replace(labels_y_pos, low, "1")
  labels_y_pos <- replace(labels_y_pos, high, "3")

  # Identify oil palm plots
  oil_palm_plot_codes <- c("OP1_", "OP2_", "OP3_")
  oil_palm_plot_regex <- paste("^", oil_palm_plot_codes,
    collapse = "|", sep = ""
  )
  # Identify riparian plots
  riparian_plot_codes <- "RP_"

  # Assign point shapes based on the pattern match, 19 for circles, 17 for
  # triangles, 15 for square
  shapes <- ifelse(grepl(oil_palm_plot_regex, rownames(pca$x)), 17,
    ifelse(grepl(riparian_plot_codes, rownames(pca$x)), 15, 19)
  )

  # Define plot limits to make sure arrows aren't cut off
  new_xrange <- range(c(pca$x[, axis_1], arrows_x_pos))
  new_yrange <- range(c(pca$x[, axis_2], arrows_y_pos))
  # Plot points
  plot(pca$x[, axis_1], pca$x[, axis_2],
    xlab = paste(
      "PCA ", axis_1, " (",
      round(pca_summary$importance[3 * axis_1 - 1] * 100, 1), "%)",
      sep = ""
    ),
    ylab = paste(
      "PCA ", axis_2, " (",
      round(pca_summary$importance[3 * axis_2 - 1] * 100, 1), "%)"
    ),
    xlim = new_xrange, ylim = new_yrange,
    col = ifelse(pca$sampled, "#05ba05", "black"),
    pch = shapes
  )
  legend("topright",
    legend = c("Sampled", "Not Sampled", "OP", "RP"),
    pch = c(19, 19, 17, 19),
    col = c("#05ba05", "black", "black", "black")
  )

  # Add arrows and labels to the plot
  arrows(
    x0 = 0, x1 = arrows_x_pos,
    y0 = 0, y1 = arrows_y_pos,
    col = "red", length = 0.15, lwd = 1.5
  )
  text(arrows_x_pos, arrows_y_pos,
    labels = row.names(pca_summary$rotation), col = "red", pos = labels_y_pos
  )
}
