#  EXO PROFILES DATA INPUT
#
#  Purpose:
#   - Locate and import all EXO depth profiles taken at farm sites
#   - Attach deployment metadata from filenames
#   - Add spatial (lat/lon) and regional context
#   - Export a single combined CSV for QC and analysis
#
#  Notes:
#   - Assumes one profile per CSV file
#   - Site codes are inferred from filenames

# Load required packages

library(dplyr) # Data wrangling
library(lubridate) # Date-time parsing
library(stringr) # Filename parsing via regex

#Save working directory path as an object
wd <- getwd()

#Create path for data

######### Uncomment the path you are using ##########

# James drive path
dir <- file.path("~/Library/CloudStorage/GoogleDrive-jcrimp@alaska.edu/Shared drives/Mariculture ReCon/Data/Sensor Data Management")
dir.data <- file.path(dir, "Raw data from sensors/EXO_profiles")

# Sierra drive path 
# dir.data <- file.path("H:/Shared drives/Mariculture ReCon/Data Management/Raw data from sensors/EXO_INSITU")

#Create paths for outputs
dir.outputs <-file.path(dir, "Outputs")
dir.csv <- file.path(dir, "CSVs")

#Find all CSV files

csv_files <- list.files(
  path = dir.data,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)


#Get list of all EXO CSV files
csv_files <- list.files(path = dir.data, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Exclude folders labeled "Data Dump" or "Raw Data sorted"
csv_files <- csv_files[
  !grepl(
    "Data Dump|Raw Data sorted",
    dirname(csv_files),
    ignore.case = TRUE
  )
]

#Create an empty list to store individual data frames
data_list <- list()

#Loop through each CSV file and read it into a data frame
for (file in csv_files) {
  
  df <- read.csv(file, skip = 9, 
                 header = FALSE, 
                 colClasses = "character")
  
  data_list[[file]] <- df
}

# Combine all data frames into one
exo_dataD <- bind_rows(data_list)

#Remove any rows where the value in the third column is not "0" (all data bearing rows should have a 0 value)
exo_dataD <- exo_dataD[exo_dataD[[3]] == "0", ]

# Get rid of variables we don't want
exo_dataD <- exo_dataD[,c(-3, -8, -12, -16, -17, -19, -22, -23 -26, -27, -28, -29, -30, -34, -35)]

# Add Column labels
colnames(exo_dataD) <- c("Date", 
                         "Time_UTC", 
                         "Site", 
                         "ODO_%sat",
                         "ODO_mg/L", 
                         "Temp_C", 
                         "Cond_uS/cm", 
                         "Sal_PSU", 
                         "Turbidity_FNU", 
                         "SpCond_uS/cm", 
                         "Chlorophyll_ug/L", 
                         "Chlorophyll_RFU", 
                         "Cable_Pwr_V", 
                         "Latitude", 
                         "Longitude", 
                         "NLF_conductivity_uS/cm", 
                         "TDS_mg/L", 
                         "Vertical_position_M", 
                         "Depth_M", 
                         "Pressure_PSIA")

exo_dataD <- exo_dataD[, c(
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


#Create a single datetime column and change timezone to UTC (raw discrete EXO 2 data was in Alaska time)
exo_dataD$Date <- mdy(exo_dataD$Date)
exo_dataD$Time <- hms(exo_dataD$Time_UTC)
exo_dataD$Time <- exo_dataD$Date + exo_dataD$Time
exo_dataD$Time <- as.POSIXct(exo_dataD$Time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
exo_dataD$Time_UTC <- with_tz(exo_dataD$Time, tzone = "GMT")
exo_dataD$Date <- date(exo_dataD$Time_UTC)
exo_dataD <- exo_dataD[,-21]

#Add region identifiers

exo_dataD$region <- region_values[exo_dataD$Site]


#Create a csv file of just the discrete data for further review
write.csv(exo_dataD, file.path(dir.csv, "EXO_D_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")

#Combine In Situ and Discrete Exo Data

exo_data <- rbind(exo_dataI, exo_dataD)

#Create a csv file of the combined data for further review
write.csv(exo_data, file.path(dir.csv, "EXO_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")

#Create a csv file of just 2023 data for upload to research workspace

exo_data_23 <- exo_data %>% filter(format(Date, "%Y") == "2023")

write.csv(exo_data_23, file.path(dir.csv, "EXO_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")