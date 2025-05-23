ignore_old_datasets <- function(safe_datasets) {
  #' Take a dataframe containing a safedata datasets and exclude outdated ones.
  #'
  #' This function excludes datasets that have a newer version.
  #'
  #' @param safe_datasets Dataframe containing the full set of relevant
  #'   datasets.
  #' @export Dataframe with the outdated datasets excluded

  # First exclude any datasets without a mra code (which implies they have been
  # withdrawn)
  safe_datasets <- safe_datasets[!is.na(safe_datasets$mra), ]

  # Exclude any datasets for which the most recent record mra does not
  # match the dataset record ID
  safe_datasets <- safe_datasets[safe_datasets$mra == safe_datasets$record, ]
}
