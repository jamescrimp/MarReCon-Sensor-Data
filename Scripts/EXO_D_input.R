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
dir.outputs <- file.path(dir, "Outputs")
dir.csv <- file.path(dir, "CSVs")

#Get list of all EXO CSV files
csv_files <- list.files(
  path = dir.data,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

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
  
  df <- read.csv(
    file,
    skip = 9,
    header = FALSE,
    colClasses = "character",
    fileEncoding = "latin1"
  )
  
  data_list[[file]] <- df
}

#Check to make sure the first line of each dataframe is its column name
for (i in seq_along(data_list)) {
  
  cat("\n\nFILE:", names(data_list)[i], "\n")
  
  print(
    data_list[[i]][
      1:min(3, nrow(data_list[[i]])),
      1:min(10, ncol(data_list[[i]]))
    ]
  )
}

#Function to standardize EXO column names
clean_exo_names <- function(x) {
  
  #Remove common encoding artifact
  x <- gsub("Â", "", x, fixed = TRUE)
  
  #Convert to lowercase for matching
  x <- tolower(trimws(x))
  
  #Standardize date and time columns
  x[x %in% c("date")] <- "Date"
  
  x[x %in% c(
    "time",
    "time (hh:mm:ss)",
    "time (hh:mm:ss tt)"
  )] <- "Time"
  
  x[x %in% c("time (fract. sec)")] <- "Time Fractional Seconds"
  
  x[x %in% c("site name")] <- "Site Name"
  
  #Standardize dissolved oxygen columns
  x[x %in% c(
    "do % saturation",
    "do ( % sat )",
    "odo % sat"
  )] <- "DO % Saturation"
  
  x[x %in% c(
    "do mg/l",
    "do ( mg/l )",
    "odo mg/l"
  )] <- "DO mg/L"
  
  x[x %in% c(
    "do ( % localb )"
  )] <- "DO % LocalB"
  
  x[x %in% c(
    "do ( % cb )"
  )] <- "DO % CB"
  
  #Standardize temperature columns
  x[x %in% c(
    "temperature",
    "temp ( °c )",
    "temp °c"
  )] <- "Temperature"
  
  #Standardize conductivity columns
  x[x %in% c(
    "conductivity",
    "cond ( µs/cm )",
    "cond µs/cm"
  )] <- "Conductivity"
  
  x[x %in% c(
    "specific conductivity",
    "sp cond ( µs/cm )",
    "spcond µs/cm"
  )] <- "Specific Conductivity"
  
  x[x %in% c(
    "nlfcond ( µs/cm )",
    "nlf cond µs/cm"
  )] <- "nLF Conductivity"
  
  #Standardize salinity columns
  x[x %in% c(
    "salinity",
    "sal ( psu )",
    "sal psu"
  )] <- "Salinity"
  
  #Standardize turbidity and TSS columns
  x[x %in% c(
    "turbidity ( fnu )",
    "turbidity fnu"
  )] <- "Turbidity"
  
  x[x %in% c(
    "tss ( mg/l )"
  )] <- "TSS"
  
  #Standardize chlorophyll columns while preserving measurement type
  x[x %in% c(
    "chlorophyll ( µg/l )",
    "chlorophyll µg/l"
  )] <- "Chlorophyll µg/L"
  
  x[x %in% c(
    "chlorophyll ( rfu )",
    "chlorophyll rfu"
  )] <- "Chlorophyll RFU"
  
  x[x %in% c(
    "chlorophyll ( cells/ml )"
  )] <- "Chlorophyll cells/mL"
  
  #Standardize phycoerythrin columns while preserving measurement type
  x[x %in% c(
    "phycoerythrin ( rfu )",
    "tal pe rfu"
  )] <- "Phycoerythrin RFU"
  
  x[x %in% c(
    "phycoerythrin ( µg/l )",
    "tal pe µg/l"
  )] <- "Phycoerythrin µg/L"
  
  x[x %in% c(
    "phycoerythrin ( cells/ml )"
  )] <- "Phycoerythrin cells/mL"
  
  #Standardize pressure and barometer columns
  x[x %in% c(
    "absolute pressure ( psi a )",
    "pressure psi a"
  )] <- "Pressure"
  
  x[x %in% c(
    "barometer ( mmhg )"
  )] <- "Barometer"
  
  #Standardize TDS, altitude, and resistivity columns
  x[x %in% c(
    "tds ( mg/l )",
    "tds mg/l"
  )] <- "TDS"
  
  x[x %in% c(
    "altitude ( m )"
  )] <- "Altitude"
  
  x[x %in% c(
    "resistivity ( ohms-cm )"
  )] <- "Resistivity"
  
  #Standardize water density columns while preserving measurement type
  x[x %in% c(
    "water density ( sigma-t )"
  )] <- "Water Density Sigma-T"
  
  x[x %in% c(
    "water density ( sigma )"
  )] <- "Water Density Sigma"
  
  #Standardize position and depth columns
  x[x %in% c(
    "vertical position ( m )",
    "vertical position m"
  )] <- "Vertical Position"
  
  x[x %in% c(
    "depth ( m )",
    "depth m"
  )] <- "Depth"
  
  #Standardize GPS columns
  x[x %in% c(
    "gps latitude ( ° )"
  )] <- "GPS Latitude"
  
  x[x %in% c(
    "gps longitude ( ° )"
  )] <- "GPS Longitude"
  
  #Standardize power columns
  x[x %in% c(
    "battery power ( voltage )"
  )] <- "Battery Power"
  
  x[x %in% c(
    "cable power ( voltage )",
    "cable pwr v"
  )] <- "Cable Power"
  
  #Standardize other columns
  x[x %in% c(
    "fault code"
  )] <- "Fault Code"
  
  x[x %in% c(
    "wiper position volt"
  )] <- "Wiper Position"
  
  return(x)
}

#Make first row of each dataframe the column names and clean them up
data_list <- lapply(data_list, function(df) {
  
  names(df) <- as.character(df[1, ])
  df <- df[-1, ]
  
  names(df) <- clean_exo_names(names(df))
  
  df
})

#Get all unique column names across the data frames
all_names <- sort(
  unique(
    unlist(
      lapply(data_list, names)
    )
  )
)

#Display all standardized column names
all_names

#Reorder every data frame to the same column order
data_list_aligned <- lapply(data_list, function(df) {
  
  #Identify columns missing from this dataframe
  missing <- setdiff(all_names, names(df))
  
  #Add missing columns as NA
  if (length(missing) > 0) {
    df[missing] <- NA
  }
  
  #Put columns in the same order
  df <- df[all_names]
  
  df
})

#Combine all data frames
exo_dataD <- do.call(rbind, data_list_aligned)

exo_dataD <- exo_dataD %>%
  rename(Date = `date (mm/dd/yyyy)`)

#Keep only desired columns
keep_names <- c(
  "Date",
  "Time",
  "Site Name",
  "Depth",
  "Temperature",
  "Salinity",
  "Conductivity",
  "DO % Saturation",
  "DO mg/L",
  "TDS"
)

exo_dataD <- exo_dataD[
  ,
  names(exo_dataD) %in% keep_names,
  drop = FALSE
]

#Check final column names
column_names <- names(exo_dataD)
column_names


#Create a single datetime column and change timezone to UTC (raw discrete EXO 2 data was in Alaska time)
exo_dataD$Date <- mdy(exo_dataD$Date)
exo_dataD$Time <- hms(exo_dataD$Time)
exo_dataD$Time <- exo_dataD$Date + exo_dataD$Time
exo_dataD$Time <- as.POSIXct(exo_dataD$Time, format = "%Y-%m-%d %H:%M:%S", tz = "America/Anchorage")
exo_dataD$Time_UTC <- with_tz(exo_dataD$Time, tzone = "GMT")
exo_dataD$Date <- date(exo_dataD$Time_UTC)

#Add region identifiers

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

exo_dataD$region <- region_values[exo_dataD$Site]


#Create a csv file of just the discrete data for further review
write.csv(exo_dataD, file.path(dir.csv, "EXO_D_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")

#Create a csv file of just 2023 data for upload to research workspace

exo_dataD_23 <- exo_dataD %>% filter(format(Date, "%Y") == "2023")

write.csv(exo_dataD_23, file.path(dir.csv, "EXO_D_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")
