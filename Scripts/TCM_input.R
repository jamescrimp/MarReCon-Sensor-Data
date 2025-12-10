
#Required Packages
library(ggplot2)
library(DBI)
library(purrr)
library(readr)
library(gsw)
library(sf)
library(dplyr)
library(lubridate)
library(readxl)
library(stringr)

#Save working directory path as an object
wd <- getwd()

#Create paths for data and outputs
dir.data <- file.path(wd, "Raw data from sensors/TCM1")  
dir.outputs <-file.path(wd, "Outputs")
dir.csv <- file.path(wd, "CSVs")

current.data <- list.dirs(dir.data, full.names = TRUE, recursive = TRUE) %>%
  grep("/Current$", ., value = TRUE)


#Get list of all EXO CSV files
csv_files <- list.files(path = current.data, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

#Create an empty list to store individual data frames
data_list <- list()



data_list <- list()

#Loop through each CSV file and read it into a data fram
for (file in csv_files) {
  
  df <- read.csv(file, header = TRUE)
  
  # Add metadata for trimming later
  df <- df %>%
    mutate(
      source_file = basename(file),
      row_in_file = row_number(),
      code = str_extract(basename(file), "_([A-Za-z0-9]{4})_") |> 
        str_replace_all("_", "")
    )
  
  data_list[[file]] <- df
}

# Combine
tcm_data <- bind_rows(data_list)


# Add Column labels
colnames(tcm_data) <- c("Time_UTC", "Speed_cm/s", "Heading", "Velocity_N_cm/s", "Velocity_E_cm/s","source_file", "row_in_file", "site")

#Add columns for lat and long
tcm_data$Latitude <- NA
tcm_data$Longitude <- NA

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

tcm_data$Latitude <- latitude_values[tcm_data$site]
tcm_data$Longitude <- longitude_values[tcm_data$site]

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

tcm_data$region <- region_values[tcm_data$site]

#Convert time to ymd_hms

tcm_data$Time_UTC <- ymd_hms(tcm_data$Time_UTC)



#Create a csv file for further review
write.csv(tcm_data, file.path(dir.csv, "TCM_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")

