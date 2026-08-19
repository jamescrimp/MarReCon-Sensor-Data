# ============================================================
#  PAR SENSOR DATA INGESTION & CLEANING PIPELINE
#
#  Purpose:
#   - Import raw PME Mini-PAR .TXT files
#   - Combine into a single dataset
#   - Extract site metadata from filenames
#   - Attach latitude, longitude, and region
#   - Export a clean CSV for downstream analysis
#
#  Notes:
#   - Raw PAR files contain 9 lines of metadata headers
#   - Site codes are inferred from filenames
#   - Coordinates match EXO2 sensor locations
# ============================================================


# ---- Load required packages --------------------------------

library(dplyr)    # Data wrangling
library(stringr)  # Regex-based site extraction
library(purrr)    # Functional mapping over files


# ---- Define working directories ----------------------------

# Project root (assumes script is run from project directory)
wd <- getwd()

# Directory containing raw PAR .TXT files
dir.data <- file.path(wd, "Raw data from sensors", "PAR")

# Output directory for cleaned CSV
dir.csv  <- file.path(wd, "CSVs")


# ---- Locate raw PAR files ----------------------------------

# Recursively find all .TXT files in the PAR directory
par_files <- list.files(
  path       = dir.data,
  pattern    = "\\.TXT$",
  full.names = TRUE,
  recursive  = TRUE
)


# ---- Import PAR files --------------------------------------

# Read each PAR file into a data frame
# - Skip first 9 lines (instrument metadata)
# - Store filename for later site extraction
par_list <- map(par_files, ~ {
  
  df <- read.csv(.x, skip = 9, header = FALSE)
  
  df$path <- tools::file_path_sans_ext(basename(.x))
  
  df
})


# ---- Combine all files into one data frame -----------------

par_data <- bind_rows(par_list)


# ---- Extract site identifier from filename -----------------

# Example filename:
#   "BCF1_03APR24.TXT" → extracts "BCF1"
#
# Regex explanation:
#   - [A-Z]+\\d+  → site code (letters + numbers)
#   - (?=_)       → followed by underscore
par_data <- par_data %>%
  mutate(
    Site = str_extract(path, "[A-Z]+\\d+(?=_)")
  )


# ---- Assign column names -----------------------------------

# Raw files include two leading unused columns → remove them
par_data <- par_data[, -c(1, 2)]

# Apply human-readable column names
colnames(par_data) <- c(
  "Time_UTC",        # Sensor timestamp (UTC)
  "Battery_V",       # Battery voltage (V)
  "Temp_C",          # Internal sensor temperature (°C)
  "PAR",             # Photosynthetically Active Radiation
  "Accel_X",         # Acceleration X-axis
  "Accel_Y",         # Acceleration Y-axis
  "Accel_Z",         # Acceleration Z-axis
  "Filepath",        # Original filename (sans extension)
  "Site"             # Site code (parsed from filename)
)


# ---- Attach latitude & longitude ---------------------------

# Coordinates correspond to EXO2 sensor deployment locations
lat_vals <- c(
  AOF1 = 57.65784, KOB1 = 57.53318, KIS1 = 57.76711,
  SSF1 = 59.46033, MIO1 = 59.57137, BCF1 = 59.46783,
  ROK1 = 60.56290, SBO1 = 60.65705, SBR1 = 60.63698
)

lon_vals <- c(
  AOF1 = -152.42018, KOB1 = -154.02696, KIS1 = -152.41043,
  SSF1 = -151.51878, MIO1 = -151.27263, BCF1 = -151.51840,
  ROK1 = -145.96046, SBO1 = -145.89151, SBR1 = -146.00447
)

par_data <- par_data %>%
  mutate(
    Latitude  = lat_vals[Site],
    Longitude = lon_vals[Site]
  )


# ---- Attach region identifier ------------------------------

# Region codes used consistently across the project
region_vals <- c(
  AOF1 = "kod", KOB1 = "kod", KIS1 = "kod",
  SSF1 = "kby", MIO1 = "kby", BCF1 = "kby",
  ROK1 = "pws", SBO1 = "pws", SBR1 = "pws"
)

par_data <- par_data %>%
  mutate(region = region_vals[Site])


# ---- Export cleaned PAR dataset ----------------------------

# Write a UTF-8 encoded CSV for downstream analysis
write.csv(
  par_data,
  file         = file.path(dir.csv, "PAR_data.csv"),
  row.names    = FALSE,
  fileEncoding = "UTF-8"
)


# Create CSV for 2023
write.csv(
  par_data %>% filter(year(as.Date(Time_UTC)) == 2023),
  file.path(dir.csv, "PAR_2023_RAW.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Create CSV for 2024
write.csv(
  par_data %>% filter(year(as.Date(Time_UTC)) == 2024),
  file.path(dir.csv, "PAR_2024_RAW.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
