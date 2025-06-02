library(tibble)
library(readxl)

raw_plot_data <- read.csv(
  "./primary/safe-soil-nutrients-csv/form-1__nutrient-sampling-form.csv"
)

# Make a new dataframe with the just the epicollect ID's in
clean_plot_data <- data.frame(row.names = raw_plot_data$ec5_uuid)

# Then add in the date and time information as well as the comments
clean_plot_data$date <- as.Date(raw_plot_data$X2_Date, format = "%d/%m/%Y")
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
    clean_plot_data$plot_code == "VJR_767" &
      clean_plot_data$date == as.Date("2024/04/23") & clean_plot_data$notes == "Rain"
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
relevant_plot_data$id <- rownames(relevant_plot_data)

# Merge in the relevant plot data based on the parent ID
clean_core_data <- merge(
  clean_core_data, relevant_plot_data,
  by.x = "parent_id", by.y = "id", all.x = TRUE
)

# As the plot codes are unique the parent ID column can now be deleted
clean_core_data$parent_id <- NULL


# Find location in plot. This requires an extra step as carbon plots used different
# option on epicollect, which placed them in a different column
location_in_plot <- ifelse(
  raw_core_data$X18_Are_you_sampling_ == "Yes",
  raw_core_data$X20_Where_has_the_cor, raw_core_data$X19_Which_point_has_t
)

# Create dataframe to store cleaned plot location data (and carbon plot status as a
# bool)
core_locations <- data.frame(
  id = raw_core_data$ec5_uuid,
  carbon_plot = ifelse(raw_core_data$X18_Are_you_sampling_ == "Yes", TRUE, FALSE),
  location_in_plot = location_in_plot
)

# Then merge the cleaned locations data into the broader cleaned core data
clean_core_data <- merge(clean_core_data, core_locations, by = "id", all.x = TRUE)

# Extract the O-horizon heights to the core_data
o_depths <- data.frame(
  id = raw_core_data$ec5_uuid,
  o_horizon_depth = raw_core_data$X22_How_deep_is_the_O
)
o_depths <- merge(
  o_depths, clean_core_data[, c("date", "id"), drop = FALSE],
  by = "id", all.x = TRUE
)
# These heights need to be cleaned because for the first sampling excursion (LFE +
# RP_LFE + CarTow, 16/02/2024-19/02/2024) the O horizons were measured in mm rather than
# cm.
early_dates <- as.Date(c("2024-02-16", "2024-02-17", "2024-02-18", "2024-02-19"))
o_depths$o_horizon_depth <-
  ifelse(
    o_depths$date %in% early_dates,
    o_depths$o_horizon_depth / 10,
    o_depths$o_horizon_depth
  )


# Once the cleaning has happened delete the date information and merge in
o_depths$date <- NULL
clean_core_data <- merge(clean_core_data, o_depths, by = "id", all.x = TRUE)

# Finally the epicollect if for the core should be made into a row name
clean_core_data <- clean_core_data %>% tibble::column_to_rownames(var = "id")

# The names I provided the lab don't always match with what I used in epicollect, so I
# need to provide a mapping
plot_code_map <- c(
  "DW1" = "DW_1", "DW2" = "DW_2", "DW3" = "DW_3", "Car-Bel" = "Car_Bel",
  "Car-Ser" = "Car_Ser"
)
# Read in the lab data for the physical properties (measured once per plot).
lab_data_physical <- read_excel(
  "./primary/S12_2024 & S1_2025 - Results.xlsx",
  sheet = "S12_2024 (N = 104)"
)
# Then replace the plot codes that differ with the versions used in epicollect
lab_data_physical$`Sample ID` <-
  ifelse(lab_data_physical$`Sample ID` %in% names(plot_code_map),
    plot_code_map[lab_data_physical$`Sample ID`],
    lab_data_physical$`Sample ID`
  )

# The relevant parts of the physical data can then be added to the cleaned plot data
clean_plot_data <-
  merge(clean_plot_data,
    lab_data_physical[,
      c(
        "Sample ID", "Bulk density (g/cm3)",
        "pH (in water)", "Clay (%)", "Silt (%)", "Sand (%)"
      ),
      drop = FALSE
    ],
    by.x = "plot_code", by.y = "Sample ID", all.x = TRUE
  )

# TODO - BIG QUESTION IS HOW TO COMBINE THIS DATA WITH THE DATA FROM EPICOLLECT
# HAVE A PLOT AVERAGED BULK DENSITY, PH, AND SOIL FRACTION TO READ IN
# THEN HAVE TOTAL C, N AND P. AND AVAILABLE P FOR A VARIETY OF SAMPLES. SOME OF THESE
# ARE POOLED PLOT AVERAGES, AND SOME OF THESE ARE SPECIFIC CORES. 5 CORES PER PLOT IF
# THEY ARE NOT SUBSAMPLED. IF A PLOT IS SUBSAMPLED A BULK VALUE IS NOT OBTAINED
