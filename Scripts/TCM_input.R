
#  TCM CURRENT DATA INGESTION PIPELINE
#
#  Purpose:
#   - Locate and import all TCM current CSVs
#   - Restrict to folders containing "Current" data
#   - Attach deployment metadata from filenames
#   - Add spatial (lat/lon) and regional context
#   - Export a single combined CSV for QC and analysis
#
#  Notes:
#   - Assumes one deployment per CSV file
#   - Site codes are inferred from filenames
#   - Coordinates match EXO / PAR sensor locations



# ---- Load required packages --------------------------------

library(readr)      # Fast and consistent CSV reading
library(dplyr)      # Data wrangling
library(stringr)    # Filename parsing via regex
library(lubridate)  # Date-time parsing


# ---- Define working directories ----------------------------

# Project root (assumes script is run from the project directory)
wd <- getwd()

# Root directory containing raw TCM data
dir.data <- file.path(wd, "Raw data from sensors", "TCM1")

# Output directories
dir.outputs <- file.path(wd, "Outputs")
dir.csv     <- file.path(wd, "CSVs")


# ---- Identify folders containing current data --------------

# Recursively search for subfolders that end with "/Current" as opposed to temperature
# data from TCM-1s
current_dirs <- list.dirs(
  path        = dir.data,
  recursive   = TRUE,
  full.names = TRUE
) |>
  grep("/Current$", x = _, value = TRUE)



# ---- List all current CSV files ----------------------------

# Find all CSV files contained within "Current" folders
current_files <- list.files(
  path        = current_dirs,
  pattern     = "\\.csv$",
  full.names  = TRUE,
  recursive   = TRUE
)


# ---- Read and process each CSV -----------------------------

# Initialize list to hold per-file data frames
current_list <- list()

for (file in current_files) {
  
  # Read raw CSV
  df <- read_csv(file, show_col_types = FALSE)
  
  # Attach file-level metadata
  df <- df %>%
    mutate(
      source_file = basename(file),
      row_in_file = row_number(),
      
      # Extract 4-character site code from filename
      # Example: "_BCF1_" → "BCF1"
      site = str_extract(basename(file), "_([A-Za-z0-9]{4})_") |>
        str_remove_all("_")
    )
  
  current_list[[file]] <- df
}


# ---- Combine all files into one data frame -----------------

current_data <- bind_rows(current_list)

# ---- Identify folders containing temperature data --------------

# Recursively search for subfolders that end with "/Current" as opposed to temperature
# data from TCM-1s
temp_dirs <- list.dirs(
  path        = dir.data,
  recursive   = TRUE,
  full.names = TRUE
) |>
  grep("/Temperature$", x = _, value = TRUE)



# ---- List all current CSV files ----------------------------

# Find all CSV files contained within "Current" folders
temp_files <- list.files(
  path        = temp_dirs,
  pattern     = "\\.csv$",
  full.names  = TRUE,
  recursive   = TRUE
)


# ---- Read and process each CSV -----------------------------

# Initialize list to hold per-file data frames
temp_list <- list()

for (file in temp_files) {
  
  # Read raw CSV
  df <- read_csv(file, show_col_types = FALSE)
  
  # Attach file-level metadata
  df <- df %>%
    mutate(
      source_file = basename(file),
      
      # Extract 4-character site code from filename
      # Example: "_BCF1_" → "BCF1"
      site = str_extract(basename(file), "_([A-Za-z0-9]{4})_") |>
        str_remove_all("_")
    )
  
  temp_list[[file]] <- df
}


# ---- Combine all files into one data frame -----------------

temp_data <- bind_rows(temp_list)


# ---- Merge current and temp data -----------------

tcm_data <- left_join(current_data, temp_data, by = c("site", "ISO 8601 Time"))


# ---- Remove/rename columns for consistency ------------------------

tcm_data <- tcm_data[,-c(9:14, 16)]

colnames(tcm_data) <- c(
  "Time_UTC",
  "Speed_cm_s",
  "Heading_deg",
  "Velocity_N_cm_s",
  "Velocity_E_cm_s",
  "source_file",
  "row_in_file",
  "site",
  "Temp_C"
)


# ---- Attach latitude and longitude -------------------------

# Coordinates correspond to fixed monitoring sites
latitude_values <- c(
  AOF1 = 57.65784, KOB1 = 57.53318, KIS1 = 57.76711,
  SSF1 = 59.46033, MIO1 = 59.57137, BCF1 = 59.46783,
  ROK1 = 60.56290, SBO1 = 60.65705, SBR1 = 60.63698
)

longitude_values <- c(
  AOF1 = -152.42018, KOB1 = -154.02696, KIS1 = -152.41043,
  SSF1 = -151.51878, MIO1 = -151.27263, BCF1 = -151.51840,
  ROK1 = -145.96046, SBO1 = -145.89151, SBR1 = -146.00447
)

tcm_data <- tcm_data %>%
  mutate(
    Latitude  = latitude_values[site],
    Longitude = longitude_values[site]
  )


# ---- Attach region identifier ------------------------------

# Region codes used consistently across the project
region_values <- c(
  AOF1 = "kod",  KOB1 = "kod",  KIS1 = "kod",
  SSF1 = "kbay", MIO1 = "kbay", BCF1 = "kbay",
  ROK1 = "pws",  SBO1 = "pws",  SBR1 = "pws"
)

tcm_data <- tcm_data %>%
  mutate(region = region_values[site])


# ---- Convert timestamps to POSIXct -------------------------

# Ensure all timestamps are parsed consistently in UTC
tcm_data <- tcm_data %>%
  mutate(Time_UTC = ymd_hms(Time_UTC, tz = "UTC"))


# ---- Export combined dataset -------------------------------

# Write a single CSV for QC and analysis pipelines
write_csv(
  tcm_data,
  file.path(dir.csv, "TCM_data.csv")
)

# Create CSV for 2023
write.csv(
  tcm_data %>% filter(year(as.Date(Time_UTC)) == 2023),
  file.path(dir.csv, "TCM_2023_RAW.csv"),
  row.names = FALSE
)

# Create CSV for 2024
write.csv(
  tcm_data %>% filter(year(as.Date(Time_UTC)) == 2024),
  file.path(dir.csv, "TCM_2024_RAW.csv"),
  row.names = FALSE
)

