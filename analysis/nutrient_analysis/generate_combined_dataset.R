library(tibble)
library(dplyr)
library(openxlsx2)

# ------------- Data cleaning --------------

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

# Finally the epicollect id for the core should be made into a row name
clean_core_data <- clean_core_data %>% tibble::column_to_rownames(var = "id")

# The names I provided the lab don't always match with what I used in epicollect, so I
# need to provide a mapping
plot_code_map <- c(
  "DW1" = "DW_1", "DW2" = "DW_2", "DW3" = "DW_3", "Car-Bel" = "Car_Bel",
  "Car-Ser" = "Car_Ser"
)
# Read in the lab data for the physical properties (measured once per plot).
lab_data_physical <- read_xlsx(
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

# Now read in the lab data for the nutrient properties (measured either once per plot or
# for 5 cores in the plot).
lab_data_nutrient <- read_xlsx(
  "./primary/S12_2024 & S1_2025 - Results.xlsx",
  sheet = "S1_2025 (N = 276)"
)

# Separate out the data that is for specific cores rather than pooled samples for entire
# plots
nutrient_data_cores <-
  lab_data_nutrient[lab_data_nutrient$Remarks != "Pooled subsamples", ]

# Define a mapping to convert the remarks into the location in plot names used in
# epicollect
location_mapping <- c(
  "West 5 meters" = "West 5m", "West 10 meters" = "West 10m",
  "East 5 meters" = "East 5m", "East 10 meters" = "East 10m",
  "North 5 meters" = "North 5m", "North 10 meters" = "North 10m",
  "South 5 meters" = "South 5m", "South 10 meters" = "South 10m",
  "Centre" = "Centre"
)

nutrient_data_cores$location_in_plot <-
  location_mapping[as.character(nutrient_data_cores$Remarks)]

# Remarks is now no longer needed (along with Lab. No.) so they can be deleted
nutrient_data_cores$Remarks <- NULL
nutrient_data_cores$`Lab. No.` <- NULL

# Add the information on the cores with chemical data to the cleaned core data
clean_core_data <- clean_core_data %>%
  left_join(nutrient_data_cores,
    by = c("plot_code" = "Sample ID", "location_in_plot"),
  )

# Separate out the data that is for entire plots rather than pooled samples for entire
# plots
nutrient_data_plots <-
  lab_data_nutrient[lab_data_nutrient$Remarks == "Pooled subsamples", ]

# Remarks and Lab. No. are not needed so they can be deleted
nutrient_data_plots$Remarks <- NULL
nutrient_data_plots$`Lab. No.` <- NULL

# Need to correct plot codes that don't match with epicollect
nutrient_data_plots$`Sample ID` <-
  ifelse(nutrient_data_plots$`Sample ID` %in% names(plot_code_map),
    plot_code_map[nutrient_data_plots$`Sample ID`],
    nutrient_data_plots$`Sample ID`
  )

# Then the chemical data obtained for whole plots can be added to the cleaned plot data
clean_plot_data <- clean_plot_data %>%
  left_join(nutrient_data_plots,
    by = c("plot_code" = "Sample ID"),
  )

# Add a column to indicate whether a plot has been subsampled.
clean_plot_data$Subsampled <-
  ifelse((clean_plot_data$plot_code %in% nutrient_data_plots$`Sample ID`), FALSE, TRUE)

# Average the O-horizon depths over each plot (relevant for bulk density, pH and soil
# texture)
plot_o_horizons <- clean_core_data %>%
  group_by(plot_code) %>%
  summarise(
    plot_o_horizon_mean = mean(o_horizon_depth),
    plot_o_horizon_sd = sd(o_horizon_depth),
    .groups = "drop"
  )
clean_plot_data <- clean_plot_data %>%
  left_join(plot_o_horizons, by = "plot_code")


# Filter nutrient core data by whether they were used for nutrient subsampling (10m for
# normal plot, 40m for carbon plot)
#  TODO - I'm assuming that 40m was used for carbon plots, but need to double check this
#  with Rolando
nutrient_sample_locations <-
  c(
    "Centre", "East 10m", "West 10m", "North 10m", "South 10m", "East 40m", "West 40m",
    "North 40m", "South 40m"
  )

nutrient_cores <- clean_core_data %>%
  filter(location_in_plot %in% nutrient_sample_locations)

# Then find stats for the o-horizons of the cores used for nutrient analysis
nutrient_sample_o_horizons <- nutrient_cores %>%
  group_by(plot_code) %>%
  summarise(
    nutrient_cores_o_horizon_mean = mean(o_horizon_depth),
    nutrient_cores_o_horizon_sd = sd(o_horizon_depth),
    .groups = "drop"
  )

clean_plot_data <- clean_plot_data %>%
  left_join(nutrient_sample_o_horizons, by = "plot_code")

# Now extract only the cores that are analysed separately rather than combined
subsampled_cores <- nutrient_cores %>%
  filter(!(plot_code %in% nutrient_data_plots$`Sample ID`))

# For the cores that are used for nutrient subsampling find means and standard
# deviations
subsampled_nutrient_stats <- subsampled_cores %>%
  group_by(plot_code) %>%
  summarise(
    mean_total_C = mean(`Total C (%)`),
    sd_total_C = sd(`Total C (%)`),
    mean_total_N = mean(`Total N (%)`),
    sd_total_N = sd(`Total N (%)`),
    mean_total_P = mean(`Total P (mg/kg)`),
    sd_total_P = sd(`Total P (mg/kg)`),
    mean_available_P = mean(`Available P (mg/kg)`),
    sd_available_P = sd(`Available P (mg/kg)`),
    .groups = "drop"
  )

# The standard deviations are renamed before they get added to the ploy data
subsampled_nutrient_stats <-
  subsampled_nutrient_stats %>% rename(
    `Total C (%)` = mean_total_C,
    `Standard deviation total C` = sd_total_C,
    `Total N (%)` = mean_total_N,
    `Standard deviation total N` = sd_total_N,
    `Total P (mg/kg)` = mean_total_P,
    `Standard deviation total P` = sd_total_P,
    `Available P (mg/kg)` = mean_available_P,
    `Standard deviation available P` = sd_available_P
  )

# These columns already exist in the plot data and should only be replaced where there
# isn't already a plot composite sample (i.e. where there's a NA value)
cols_to_update <-
  c("Total C (%)", "Total N (%)", "Total P (mg/kg)", "Available P (mg/kg)")

# Add the nutrients averages and standard deviations to the cleaned plot data
clean_plot_data <- clean_plot_data %>%
  left_join(subsampled_nutrient_stats, by = "plot_code", suffix = c("", ".new")) %>%
  mutate(across(
    all_of(cols_to_update),
    ~ coalesce(.x, get(paste0(cur_column(), ".new")))
  )) %>%
  select(-ends_with(".new"))

# ------------- Converting data to `safedata`` formatted Excel workbook --------------

# Write out all of the summary metadata that I need to include
summary_metadata <- list(
  "Project ID" = c(234),
  "Access status" = c("Embargo"),
  "Title" = c(
    "Soil nutrient concentrations across the SAFE project disturbance gradient."
  ),
  "Description" = c(paste(
    "A study examining how soil physical and nutrient properties change along a",
    "logging gradient. Multiple soil cores were taken from the SAFE project vegetation",
    "and carbon plots, with the O-horizon depth being estimated for each core. One",
    "core per plot was weighed weighed to estimate the bulk density of the plot. A",
    "composite sample was made by combining the remaining cores for each plot, and the",
    "soil pH and texture was found. Total carbon, nitrogen and phosphorus and",
    "available phosphorus were also found by lab analysis. This was either done for a",
    "composite sample of five cores, or for five cores individually. All lab analysis",
    "were performed at the chemistry lab of the Forest Research Centre in Sepilok,",
    "Sandakan, Sabah."
  )),
  "Embargo date" = c("2027-07-24"),
  "Author name" = c("Cook, Jacob", "Tsen, Sandy", "Nilus, Reuben", "Ewers, Robert M."),
  "Author email" = c("jc2017@ic.ac.uk", "", "", "r.ewers@ic.ac.uk"),
  "Author affiliation" = c(
    "Imperial College London", "Sabah Forestry Department",
    "Sabah Forestry Department", "Imperial College London"
  ),
  "Author ORCID" = c("0000-0002-7320-1706", "", "", "0000-0002-9001-0610"),
  "Worksheet name" = c("PlotData", "CoreData"),
  "Worksheet title" = c("Soil data at plot level", "Soil data for specific cores"),
  "Worksheet description" = c(
    "The soil physical and nutrient data for each SAFE vegetation plot.",
    "The nutrient data for individual cores taken from SAFE vegetation plots."
  ),
  "Keywords" = c("Carbon", "Soil", "Nutrients", "SAFE core data", "SAFE project"),
  "Publication DOI" = c(""),
  "Permit type" = c("Research"),
  "Permit authority" = c("Sabah Biodiversity Council"),
  "Permit number" = c("JKM/MBS.1000-2/2 JLD. 16 (177)"),
  # TODO - HAVE ASKED ROB IF ANYONE ELSE SHOULD BE CREDITED HERE
  "Funding body" = c("NOMIS Foundation"),
  "Funding type" = c("Grant"),
  "Funding reference" = c("Distinguished Scientist Award to Robert M. Ewers"),
  "Funding link" = c(
    paste0(
      "https://nomisfoundation.ch/projects/a-virtual-rainforest-for-understanding-the",
      "-stability-resilience-and-sustainability-of-complex-ecosystems/"
    )
  ),
  "Start date" = c("2024-02-16"),
  "End date" = c("2024-04-30"),
  "West" = c(116.75),
  "East" = c(117.82),
  "South" = c(4.50),
  "North" = c(5.07)
)

# Make a dataframe where the first column is given by the label, and the other entries
# are given by the values. Empty strings are inserted if specific vectors are below the
# maximum width
max_width <- max(sapply(summary_metadata, length))

formatted_metadata <- do.call(rbind, lapply(names(summary_metadata), function(label) {
  values <- summary_metadata[[label]]
  c(label, values, rep("", max_width - length(values)))
}))
summary_metadata_frame <- as.data.frame(formatted_metadata)

wb <- wb_workbook()
wb <- wb_add_worksheet(wb, "Summary")

# Function to ensure that empty values don't get written, that numeric values are
# written out as numbers, and that dates are written as dates
output_value <- function(value, i, j) {
  # Don't print out empty values
  if (value != "") {
    if (!is.na(suppressWarnings(as.numeric(value)))) {
      wb$add_data("Summary", x = as.numeric(value), start_row = i, start_col = j)
    } else if (grepl("^\\d{4}-\\d{2}-\\d{2}$", value)) {
      wb$add_data("Summary", x = as.Date(value), start_row = i, start_col = j)
    } else {
      wb$add_data("Summary", x = value, start_row = i, start_col = j)
    }
  }
}

# Have to loop over so that numeric values get written out as numeric values
for (i in seq_len(nrow(summary_metadata_frame))) {
  # Write row name (as character)
  wb$add_data("Summary", x = summary_metadata_frame[i, 1], start_row = i, start_col = 1)

  # Any output in possible numeric form is identified and converted
  for (j in seq(2, ncol(summary_metadata_frame))) {
    output_value(value = summary_metadata_frame[i, j], i = i, j = j)
  }
}

# I used DWx as a shorthand for OG3_DWx, these should be converted to the full plot code
dw_map <- c("DW_1" = "OG3_DW1", "DW_2" = "OG3_DW2", "DW_3" = "OG3_DW3")
clean_plot_data$plot_code <-
  ifelse(
    clean_plot_data$plot_code %in% names(dw_map), dw_map[clean_plot_data$plot_code],
    clean_plot_data$plot_code
  )
clean_core_data$plot_code <-
  ifelse(
    clean_core_data$plot_code %in% names(dw_map), dw_map[clean_core_data$plot_code],
    clean_core_data$plot_code
  )

# Add the locations used as a Locations sheet
wb <- wb_add_worksheet(wb, "Locations")
wb$add_data("Locations", x = "Location name", start_row = 1, start_col = 1)
wb$add_data("Locations",
  x = clean_plot_data$plot_code[order(clean_plot_data$plot_code)], start_row = 2
)

# Write out all the metadata here so that it can be inserted into the data worksheets.
# This involves a simplified name ("new_name"), the field type, the values a categorical
# field can take (empty for non-categorical fields) a description of what the field is,
# the units of the field (where relevant), and the method used to generate the data
# TODO - ADD method
all_column_metadata <- list(
  plot_code = list(
    new_name = "plot_code", description = "SAFE Project plot name",
    field_type = "Location"
  ),
  date = list(
    new_name = "date", description = "Date that plot was sampled on",
    field_type = "Date"
  ),
  time = list(
    new_name = "time", description = "Time at which sampling of the plot started",
    field_type = "Time"
  ),
  carbon_plot = list(
    new_name = "carbon_plot", field_type = "Categorical",
    description = "Whether the plot is a SAFE project carbon plot",
    levels = "TRUE;FALSE"
  ),
  location_in_plot = list(
    new_name = "location_in_plot",
    description = "Point in plot core was taken from, measured relative to the centre",
    field_type = "ID"
  ),
  o_horizon_depth = list(
    new_name = "core_o_horizon", description = "Depth at which soil O-horizon occurs",
    field_type = "Numeric", units = "cm"
  ),
  `pH (in water)` = list(
    new_name = "pH", field_type = "Numeric",
    description = "pH (in water) of the soil sample", units = "standard units"
  ),
  `Bulk density (g/cm3)` = list(
    new_name = "bulk_density", field_type = "Numeric",
    description = "Bulk density of soil in the plot", units = "g/(cm^3)"
  ),
  # TODO - Need to ask Rolando the details of this, i.e. whether it is percentage of the
  # total or of the mineral component
  `Clay (%)` = list(
    new_name = "clay", field_type = "Numeric",
    description = "Fraction of the soil sample that is clay", units = "%"
  ),
  `Silt (%)` = list(
    new_name = "silt", field_type = "Numeric",
    description = "Fraction of the soil sample that is silt", units = "%"
  ),
  `Sand (%)` = list(
    new_name = "sand", field_type = "Numeric",
    description = "Fraction of the soil sample that is sand", units = "%"
  ),
  plot_o_horizon_mean = list(
    new_name = "plot_mean_o_horizon", field_type = "Numeric",
    description = "Mean O-horizon depth across all cores taken from the plot",
    units = "cm"
  ),
  plot_o_horizon_sd = list(
    new_name = "sd_plot_o_horizon", field_type = "Numeric",
    description = (
      "Standard deviation of O-horizon depth across all cores taken from the plot"
    ),
    units = "cm"
  ),
  `Total C (%)` = list(
    new_name = "total_carbon", field_type = "Numeric",
    description = "Fraction of the soil sample that is carbon (in any form)",
    units = "%"
  ),
  `Total N (%)` = list(
    new_name = "total_nitrogen", field_type = "Numeric",
    description = "Fraction of the soil sample that is nitrogen (in any form)",
    units = "%"
  ),
  `Total P (mg/kg)` = list(
    new_name = "total_phosphorus", field_type = "Numeric",
    description = "Fraction of the soil sample that is phosphorus (in any form)",
    units = "mg/kg"
  ),
  `Available P (mg/kg)` = list(
    new_name = "available_phosphorus", field_type = "Numeric",
    description = "Fraction of the soil sample that is plant available phosphorus",
    units = "mg/kg"
  ),
  nutrient_cores_o_horizon_mean = list(
    new_name = "nutrient_cores_mean_o_horizon", field_type = "Numeric",
    description = (
      "Mean O-horizon depth across the cores used for nutrient analysis"
    ),
    units = "cm"
  ),
  nutrient_cores_o_horizon_sd = list(
    new_name = "sd_nutrient_cores_o_horizon", field_type = "Numeric",
    description = paste(
      "Standard deviation of O-horizon depth across the cores used for nutrient",
      "analysis"
    ),
    units = "cm"
  ),
  Subsampled = list(
    new_name = "Subsampled", field_type = "Categorical",
    description = paste(
      "Whether the five cores used for nutrient analysis were combined into a single",
      "composite sample, or analysed separately (i.e. subsampled)"
    ),
    levels = "TRUE;FALSE"
  ),
  `Standard deviation total C` = list(
    new_name = "sd_total_carbon", field_type = "Numeric",
    description = paste(
      "Standard deviation of fraction of the soil sample that is carbon (in any form)",
      "between cores. This is only relevant for subsampled plots."
    ),
    units = "%"
  ),
  `Standard deviation total N` = list(
    new_name = "sd_total_nitrogen", field_type = "Numeric",
    description = paste(
      "Standard deviation of fraction of the soil sample that is nitrogen (in any",
      "form) between cores. This is only relevant for subsampled plots."
    ),
    units = "%"
  ),
  `Standard deviation total P` = list(
    new_name = "sd_total_phosphorus", field_type = "Numeric",
    description = paste(
      "Standard deviation of fraction of the soil sample that is phosphorus (in any",
      "form) between cores. This is only relevant for subsampled plots."
    ),
    units = "mg/kg"
  ),
  `Standard deviation available P` = list(
    new_name = "sd_available_phosphorus", field_type = "Numeric",
    description = paste(
      "Standard deviation of fraction of the soil sample that is plant available",
      "phosphorus between cores. This is only relevant for subsampled plots."
    ),
    units = "mg/kg"
  ),
  data_recorders = list(
    new_name = "data_recorders", description = "Who gathered the data in the field",
    field_type = "Comments"
  ),
  notes = list(
    new_name = "notes", description = "General notes about the plot",
    field_type = "Comments"
  )
)

# Find columns that are present in each datasheet
plot_data_columns <- intersect(names(all_column_metadata), names(clean_plot_data))
core_data_columns <- intersect(names(all_column_metadata), names(clean_core_data))

# Reorder the data frames so that they are ordered by when samples were taken and into
# my preferred column order
clean_plot_data <-
  clean_plot_data[order(clean_plot_data$date, clean_plot_data$time), plot_data_columns]
# For cores all get same time stamp within the plot (so further order by location)
clean_core_data <- clean_core_data[
  order(clean_core_data$date, clean_core_data$time, clean_core_data$location_in_plot),
  core_data_columns
]

# Make maps to rename columns by and then rename the columns to simpler names
plot_rename_map <- sapply(
  plot_data_columns, function(name) all_column_metadata[[name]]$new_name,
  USE.NAMES = TRUE
)
core_rename_map <- sapply(
  core_data_columns, function(name) all_column_metadata[[name]]$new_name,
  USE.NAMES = TRUE
)

names(clean_plot_data)[names(clean_plot_data) %in% names(plot_rename_map)] <-
  plot_rename_map[
    names(clean_plot_data)[names(clean_plot_data) %in% names(plot_rename_map)]
  ]
names(clean_core_data)[names(clean_core_data) %in% names(core_rename_map)] <-
  core_rename_map[
    names(clean_core_data)[names(clean_core_data) %in% names(core_rename_map)]
  ]

# Define metadata categories and gather worksheet metadata to write out along with the
# data itself
metadata_categories <-
  c("field_type", "levels", "description", "units", "method", "field_name")

get_or_na <- function(x, name) {
  if (!is.null(x[[name]])) x[[name]] else NA
}

plot_metadata <- sapply(all_column_metadata[plot_data_columns], function(x) {
  sapply(metadata_categories, function(category) get_or_na(x, category))
})
core_metadata <- sapply(all_column_metadata[core_data_columns], function(x) {
  sapply(metadata_categories, function(category) get_or_na(x, category))
})

# Add the data frames to the workbooks with NA values properly outputted as strings
wb$add_worksheet("PlotData")
wb$add_data("PlotData", x = metadata_categories, start_row = 1)
wb$add_data(
  "PlotData",
  x = plot_metadata, start_col = 2, col_names = FALSE, na.strings = ""
)
wb$add_data(
  "PlotData", clean_plot_data,
  na.strings = "NA", start_col = 2, start_row = length(metadata_categories)
)
wb$add_worksheet("CoreData")
wb$add_data("CoreData", x = metadata_categories, start_row = 1)
wb$add_data(
  "CoreData",
  x = core_metadata, start_col = 2, col_names = FALSE, na.strings = ""
)
wb$add_data(
  "CoreData", clean_core_data,
  na.strings = "NA", start_col = 2, start_row = length(metadata_categories)
)

wb_save(wb, "SAFE_soil_nutrient_data.xlsx")

# ------------- Plotting --------------

# Sum all the clay, silt and sand contents and then plot them as a histogram
total_css <-
  rowSums(lab_data_physical[, c("Clay (%)", "Silt (%)", "Sand (%)"), drop = FALSE])

hist(total_css,
  breaks = 50, col = "orange", main = "Clay + sand + silt",
  xlab = "Combined clay, silt and sand (%)"
)
