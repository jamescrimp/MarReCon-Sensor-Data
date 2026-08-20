
#  EXO IN SITU DATA INPUT
#
#  Purpose:
#   - Locate and import all EXO IN SITU CSVs from 3 to 5 month deployments at farm sites
#   - Attach deployment metadata from filenames
#   - Add spatial (lat/lon) and regional context
#   - Export a single combined CSV for QC and analysis
#
#  Notes:
#   - Assumes one deployment per CSV file
#   - Site codes are inferred from filenames
#   - Coordinates match EXO sensor locations


#Load required packages
library(dplyr) # Data wrangling
library(lubridate) # Date-time parsing
library(stringr) # Filename parsing via regex

#Save working directory path as an object
wd <- getwd()

#Create path for data
#uncomment which one you will be using

# James drive path
dir <- file.path("~/Library/CloudStorage/GoogleDrive-jcrimp@alaska.edu/Shared drives/Mariculture ReCon/Data/Sensor Data Management")
dir.data <- file.path(dir, "Raw data from sensors/EXO_INSITU")

# Sierra drive path 
# dir.data <- file.path("H:/Shared drives/Mariculture ReCon/Data Management/Raw data from sensors/EXO_INSITU")

#Create paths for outputs
dir.outputs <-file.path(dir, "Outputs")
dir.csv <- file.path(dir, "CSVs")

#Get list of all EXO CSV files
csv_files <- list.files(path = dir.data, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

#Create an empty list to store individual data frames
data_list <- list()

#Loop through each CSV file and read it into a data frame
for (file in csv_files) {
  
  df <- read.csv(csv_files[8], skip = 9, header = FALSE, stringsAsFactors = FALSE)
  
  df <- read.csv(file, skip = 9, header = FALSE)
  # 
  #Create a site identifier for each dataframe
  df$path <- tools::file_path_sans_ext(basename(file))
  
  data_list[[file]] <- df
}

# Combine all data frames into one
exo_dataI <- bind_rows(data_list)

print(exo_dataI)

# Add a unique site label and get rid of the filepath
exo_dataI <- exo_dataI %>% mutate(site = str_extract(path, "[A-Z]+\\d+(?=_\\d{2}[A-Z]{3}\\d{2}$)"))

# Get rid of column 3 and 23
exo_dataI <- exo_dataI[,c(-3, -13, -14, -21:-22)]

# Add Column labels
colnames(exo_dataI) <- c("Date", "Time_UTC", "Temp_C", "Cond_uS/cm", "SpCond_uS/cm", "TDS_mg/L","Sal_PSU","NLF_conductivity_uS/cm", "Depth_M","Vertical_position_M", "Pressure_PSIA", "Chlorophyll_RFU", "Chlorophyll_ug/L", "ODO_%sat", "ODO_mg/L", "Turbidity_FNU","Cable_Pwr_V", "Site")

#Add columns for lat and long
exo_dataI$Latitude <- NA
exo_dataI$Longitude <- NA

#Add values to lat/long corresponding to coordinates of in situ EXO 2s at each site
latitude_values <- c(
  AOF1 = 57.65784,
  KOB1 = 57.53318,
  KIS1 = 57.76711,
  SSF1 = 59.46033,
  MIO1 = 59.57137,
  BCF1 = 59.46783,
  ROK1 = 60.56290,
  SBO1 = 60.65705,
  SBR1 = 60.63698
)

longitude_values <- c(
  AOF1 = -152.42018,
  KOB1 = -154.02696,
  KIS1 = -152.41043,
  SSF1 = -151.51878,
  MIO1 = -151.27263,
  BCF1 = -151.51840,
  ROK1 = -145.96046,
  SBO1 = -145.89151,
  SBR1 = -146.00447
)

exo_dataI$Latitude <- latitude_values[exo_dataI$Site]
exo_dataI$Longitude <- longitude_values[exo_dataI$Site]

#Create a single datetime column
exo_dataI$Date <- mdy(exo_dataI$Date)
exo_dataI$Time_UTC <- hms(exo_dataI$Time_UTC)
exo_dataI$Time_UTC <- exo_dataI$Date + exo_dataI$Time_UTC

# Rearrange columns to match similar variables

exo_dataI <- exo_dataI[, c(
  "Date", 
  "Time_UTC",
  "Site",
  "Depth_M",
  "Temp_C", 
  "Sal_PSU",
  "Turbidity_FNU",
  "Chlorophyll_RFU", 
  "Chlorophyll_ug/L", 
  "Cond_uS/cm", 
  "SpCond_uS/cm", 
  "NLF_conductivity_uS/cm",
  "TDS_mg/L",
  "Vertical_position_M", 
  "Pressure_PSIA", 
  "ODO_%sat", 
  "ODO_mg/L",
  "Cable_Pwr_V",
  "Latitude",
  "Longitude"
)]

str(exo_dataI)

#Create a column identifying region

region_values <- c(
  AOF1 = "kod",
  KOB1 = "kod",
  KIS1 = "kod",
  SSF1 = "kbay",
  MIO1 = "kbay",
  BCF1 = "kbay",
  ROK1 = "pws",
  SBO1 = "pws",
  SBR1 = "pws"
)

exo_dataI$region <- region_values[exo_dataI$Site]


#Create a csv file for further review
write.csv(exo_dataI, file.path(dir.csv, "EXO_I_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")

#Create individual CSV files by year for upload to research workspace

exo_data_23 <- exo_dataI %>% filter(format(Date, "%Y") == "2023")
write.csv(exo_data_23, file.path(dir.csv, "EXO_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")

exo_data_24 <- exo_dataI %>% filter(format(Date, "%Y") == "2024")
write.csv(exo_data_24, file.path(dir.csv, "EXO_2024.csv"), row.names = FALSE, fileEncoding = "UTF-8")
