library(tibble)

raw_plot_data <- read.csv(
  "./primary/safe-soil-nutrients-csv/form-1__nutrient-sampling-form.csv"
)

# Make a new dataframe with the just the epicollect ID's in
clean_plot_data <- data.frame(row.names = raw_plot_data$ec5_uuid)

# Then add in the date and time information as well as the comments
clean_plot_data$date <- raw_plot_data$X2_Date
clean_plot_data$time <- raw_plot_data$X3_Time
clean_plot_data$notes <- raw_plot_data$X17_Notes

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

# The plot VJR_767 was reached on 23/04/2024 at the initial stages of data recording
# were started. However, the rain got too heavy so sampling was abandoned. The plot was
# visited again on 30/04/2024, and a full set of samples were obtained. This step
# removes the entry for the aborted sampling.
clean_plot_data <- clean_plot_data[
  !(
    clean_plot_data$plot_code == "VJR_767" & clean_plot_data$date == "23/04/2024" &
      clean_plot_data$notes == "Rain"
  ),
]

# Now have extracted all the necessary plot level data and need to work on the core
# specific data
raw_core_data <- read.csv("./primary/safe-soil-nutrients-csv/form-2__soil-core.csv")

# Make a new dataframe with the just the epicollect ID's in
clean_core_data <- data.frame()
clean_core_data <- data.frame(
  id = raw_core_data$ec5_uuid, parent_id = raw_core_data$ec5_parent_uuid
)

# Make a simplified version of the plot data with the information I want to associate
# with individual cores
relevant_plot_data <- clean_plot_data[,
  c("plot_code", "date", "time", "data_recorders"),
  drop = FALSE
]
relevant_plot_data$ID <- rownames(relevant_plot_data)

# Merge in the relevant plot data based on the parent ID
clean_core_data <- merge(
  clean_core_data, relevant_plot_data,
  by.x = "parent_id", by.y = "ID", all.x = TRUE
)

# As the plot codes are unique the parent ID column can now be deleted
clean_core_data$parent_id <- NULL

# The core id should be made into a row name
clean_core_data <- clean_core_data %>% tibble::column_to_rownames(var = "id")


# TODO - EXTRACT LOCATION NAMES, LOCATION NAME COLUMN DEPENDS ON WHETHER IT IS A CARBON
# PLOT OR NOT
# TODO - NEED TO CHECK THAT THE RECORDED LOCATION NAMES MATCH THE BAG LOCATION NAMES
# (MASSIVELY ANNOYING IF THEY DON'T)

# TODO - EXTRACT O-HORIZON DEPTHS. NEED TO FIND OUT WHEN I SWITCHED FROM MEASURING IN mm
# to cm
