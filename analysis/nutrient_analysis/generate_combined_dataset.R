library(tibble)
library(readxl)
library(dplyr)

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

# Now read in the lab data for the nutrient properties (measured either once per plot or
# for 5 cores in the plot).
lab_data_nutrient <- read_excel(
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

# TODO - NEED TO WORK OUT HOW TO OUTPUT THIS DATA AS AN EXCEL FILE

# USEFUL WAY OF CHECKING DATA colSums(is.na(clean_plot_data))


# ------------- Plotting --------------

# Sum all the clay, silt and sand contents and then plot them as a histogram
total_css <-
  rowSums(lab_data_physical[, c("Clay (%)", "Silt (%)", "Sand (%)"), drop = FALSE])

hist(total_css,
  breaks = 50, col = "orange", main = "Clay + sand + silt",
  xlab = "Combined clay, silt and sand (%)"
)
