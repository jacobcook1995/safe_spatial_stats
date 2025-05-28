raw_plot_data <- read.csv(
  "./primary/safe-soil-nutrients-csv/form-1__nutrient-sampling-form.csv"
)

# Make a new dataframe with the just the epicollect ID's in
clean_plot_data <- data.frame(row.names = raw_plot_data$ec5_uuid)

# Then add in the date and time information
clean_plot_data$date <- raw_plot_data$X2_Date
clean_plot_data$time <- raw_plot_data$X3_Time

# Plot codes are given in a mix of formats but we only want to keep the number, which
# for normal plots is always at the end of the format. Either on it's own or preceded by
# a space, an underscore, a dash, or a comma
clean_plot_data$plot_code <- paste(
  raw_plot_data$X4_Which_block,
  sub(".*[ _,-](\\d+)", "\\1", raw_plot_data$X7_Whats_the_plot_ID),
  sep = "_"
)

# The above approach doesn't work for carbon or riparian plots so they have separate
# handling.

# For the carbon plots the names are all clean, so copy these names across
clean_plot_data$plot_code[raw_plot_data$X5_Is_this_a_carbon_p == "Yes"] <-
  raw_plot_data$X7_Whats_the_plot_ID[raw_plot_data$X5_Is_this_a_carbon_p == "Yes"]


# The 4 riparian plot codes are also fine, so copy these names across
clean_plot_data$plot_code[startsWith(raw_plot_data$X7_Whats_the_plot_ID, "RP")] <-
  raw_plot_data$X7_Whats_the_plot_ID[startsWith(
    raw_plot_data$X7_Whats_the_plot_ID,
    "RP"
  )]

# Now need to clean the names of the people recording the data. To do this first need to
# regularise how names are separated. Replace separators used (commas, double commas,
# full stop, spaces, multiple spaces) with comma + space
names_cleaned <- gsub("\\s*[,\\.]+\\s*|\\s+", ", ", raw_plot_data$X12_Names_of_data_rec)

# Then define a mapping of nicknames + abbreviations + misspellings onto standard names
name_map <- c(
  "jac" = "Jacob", "rio" = "Sabri", "sab" = "Sabri", "savri" = "Sabri", "pol" = "Poldi",
  "sel" = "Selvenson", "kil" = "Kiel"
)

replace_names <- function(name_str, mapping) {
  parts <- strsplit(name_str, ",\\s*")[[1]]
  lower_parts <- tolower(parts)
  parts[lower_parts %in% names(mapping)] <-
    mapping[lower_parts[lower_parts %in% names(mapping)]]
  paste(parts, collapse = ", ")
}

# Use function with mapping to standardise the names and then add them to the cleaned
# data
clean_plot_data$data_recorders <-
  sapply(names_cleaned, replace_names, mapping = name_map)
