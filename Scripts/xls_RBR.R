library(oce)
library(RSQLite)
library(ggplot2)
library(DBI)
library(purrr)
library(readr)
library(gsw)
library(sf)
library(dplyr)
library(lubridate)
library(tibble)
library(stringr)
library(readxl)

#This is code to upload csvs from Ruskin that were created for .rsk files would not read into R 
#Updated 11/20/2025 
#Sierra Greene

wd<- setwd("G:/My Drive/RBR Data")

# Set File Paths
dir.data <- ("G:/My Drive/RBR Data")
# dir.data <- file.path("~/Desktop/RBR data that doesn't work")
dir.outputs <-file.path(wd, "outputs")


#______________________________________________________________


# List all files with *.xlsx extension
xrsk_files <- list.files(path = dir.data, recursive = TRUE, pattern = "*.xlsx$", full.names = TRUE)

#load in data, change coluns to labels we need, turn to ctd object, 
#trim data to keep downcasts
# Initialize an empty list to store CTD objects
data_list <- list()

for (file in xrsk_files) {
  # Read just the sheet lablled 'Data', skipping the first 2 rows so we can rename what we want
  sheet_data <- read_excel(file, sheet = "Data", skip = 1)
  
  # Add a column to indicate the file path- helps w/ naming
  sheet_data <- sheet_data %>% mutate(FilePath = file)
  
   #Rename columns to match CTD expectations (new = old)
  sheet_data <- sheet_data %>% rename(
    press = "Pressure",    
    temperature = "Temperature",  
    salinity = "Salinity" ,
    pressure = "Sea pressure" 
  )
  
  # Convert to CTD object
  ctd_obj <- as.ctd(sheet_data)
  
  # Use ctdTrim to keep only downcasts
  ctd_trimmed <- ctdTrim(ctd_obj, method = "downcast")
  
  # Append the trimmed CTD object to the list
  data_list[[length(data_list) + 1]] <- ctd_trimmed
}
#This will give an error code about reading the 'Pressure'column, but data will have read ok into data_list

#Combine parts in the data df of each part of the data_list
xrbr_data <- dplyr::bind_rows(lapply(data_list, function(ctd_obj) {
  as.data.frame(ctd_obj@data)
}))
#remoce rows out of the water
xrbr_data <- subset(xrbr_data, pressure >= 0.2)

#Add site names
xrbr_data <- xrbr_data %>%
  mutate(Site = str_extract(FilePath, "(?<=\\()[^\\)]+"))

#only keep data we need: time, pressure, temperature, conductivity, site
xrbr_data <- xrbr_data %>%
  select(Time, pressure, temperature, Conductivity, salinity, Site)
#Add Date
xrbr_data <- xrbr_data %>%
  mutate(date = as.Date(Time))

#xrbr_data <- xrbr_data %>%
  #rename(pressure =  "Sea pressure")

xrbr_data <- xrbr_data %>%
  rename_with(tolower)

#Remove negative pressure rows (not in water)
xrbr_data <- subset(xrbr_data, pressure >= 0.2)

str(xrbr_data)

#Fix sites that were recorded in AK time zone in 2024, all data in 2025 is OK
#Spring:3/10/2024 
#Fall: 11/3/202
#   1/1/24 - 3/10/24 = +9 hrs
#   3/10/24 - 11/3/24 = +8 hrs
#   11/3/24 - 12/31/24 = +9 hrs
#Correct: all of AOF1, MIO1 and BCF1 until June 
#Make a temporary df in case we mess up
rbr_timefix <- xrbr_data

rbr_timefix <- rbr_timefix %>%
  mutate(
    time = case_when(
      # AOF1 
      site == "AOF1" & time >= ymd("2024-01-01") & time < ymd("2024-03-10") ~ 
        time + hours(9),
      site == "AOF1" & time >= ymd("2024-03-10") & time < ymd("2024-11-03") ~ 
        time + hours(8),
      site == "AOF1" & time >= ymd("2024-11-03") & time <= ymd("2024-12-31") ~ 
        time + hours(9),
      
      # BCF1 
      site == "BCF1" & time >= ymd("2024-01-01") & time < ymd("2024-03-10") ~ 
        time + hours(9),
      site == "BCF1" & time >= ymd("2024-03-10") & time <= ymd("2024-05-15") ~ 
        time + hours(8),
      
      # MIO1 
      site == "MIO1" & time >= ymd("2024-01-01") & time < ymd("2024-03-10") ~ 
        time + hours(9),
      site == "MIO1" & time >= ymd("2024-03-10") & time <= ymd("2024-06-01") ~ 
        time + hours(8),
      
      # Keep original time for all other cases
      TRUE ~ time
    )
  )

xrbr_data <- rbr_timefix  

write.csv(xrbr_data, file.path("C:/MarRecon_code/thesis_work/RBR_code/xRBR_data.csv"), row.names = FALSE)

#_____________________EXO PROFILES____________________
#It will take some work to wrangle the csvs into shape before running this code
#Upload data from EXO profiles and extract salinity and temp data from it 
#Create a forloop to pull data from the data.dir 

data.dir <- "C:/MarRecon_code/clean_data/Raw data from sensors_17APR2025/EXO_profiles"

#skip certain folders in this directory
#
# Get all CSV files in directory 
csv_files <- list.files(path = data.dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Create an empty list
data_list <- list()

# Read and store each CSV
for (file in csv_files) {
  df <- read_csv(file, skip = 9, col_names = FALSE)
  df$path <- tools::file_path_sans_ext(basename(file)) 
  data_list[[file]] <- df
}

# Combine into one data frame
combined_df <- bind_rows(data_list, .id = "file_path")

exo_dataI <- combined_df


