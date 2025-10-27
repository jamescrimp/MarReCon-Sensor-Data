
#Required Packages
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

setwd("~/Library/CloudStorage/GoogleDrive-jcrimp@alaska.edu/Shared drives/Mariculture ReCon/Data Management")

#Save working directory path as an object
wd <- getwd()

#folder path 
dir.data <- file.path(wd, "Raw data from sensors/HOBO")  
dir.outputs <-file.path(wd, "James MarRecon Code/outputs")
dir.csv <-file.path(wd, "James MarRecon Code/outputs/CSVs")

#Import hobo data

hobo_files <- list.files(path = dir.data, recursive = TRUE, pattern = "*.csv$", full.names = TRUE)

#Exclude files that are using the factory calibration for salinity, rather than the field calibration
hobo_files <- hobo_files[!grepl("/FactoryCal[^/]*\\.csv$", hobo_files)]

# Initialize an empty list to store the CTD objects
hobo_list <- list()

# Loop through each .csv file, read and convert to data frame
for (file in hobo_files) {
  # Read the .csv file
  hobo_data <- read.csv(file, header = FALSE)

  # Get rid of all columns except for the first five
  hobo_data <- hobo_data[, 1:6]
  
  #Create a site identifier for each dataframe
  hobo_data$path <- tools::file_path_sans_ext(file)
  
  # Add a unique site label and get rid of the filepath
  hobo_data <- hobo_data %>% mutate(site = str_extract(path, "[A-Z]+\\d+(?=_\\d{2}[A-Z]{3}\\d{2}$)"))
  
  # #Get rid of the first column and row
  hobo_data <- hobo_data[-1, -1]
  # 
  # #convert the first row to column names
  colnames(hobo_data) <- hobo_data[1,]
  hobo_data <- hobo_data[-1,]
  # 
  # # Change the column names
  colnames(hobo_data) <- c("time", "LowRange", "HighRange", "temp", "salinity", "site")
  # 
  # # Append the object to the list
  hobo_list[[file]] <- hobo_data
}

# Combine all HOBO data into a single data frame
hobo <- do.call(rbind, hobo_list)
rownames(hobo) <- NULL
hobo <- hobo[,-6]
colnames(hobo)[6] <- "site"

#Convert to correct units
# hobo$time <- parse_date_time(hobo$time, orders = c("m/d/y I:M p", "m/d/y H:M"))
# hobo$time <- mdy_hms(hobo$time)
# hobo$date <- date(hobo$time)
hobo$LowRange <- as.numeric(hobo$LowRange)
hobo$HighRange <- as.numeric(hobo$HighRange)
hobo$Temp <- as.numeric(hobo$temp)

#Convert units for conductivity
hobo$LowRange <- hobo$LowRange/1000
hobo$HighRange <- hobo$HighRange/1000

#Add region identifier
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

hobo$region <- region_values[hobo$site]


#Create a csv file 
write.csv(hobo, file.path(dir.csv, "hobo_data.csv"), row.names = FALSE)



