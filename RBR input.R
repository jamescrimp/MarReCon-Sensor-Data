
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


setwd("~/Library/CloudStorage/GoogleDrive-jcrimp@alaska.edu/Shared drives/Mariculture ReCon/Data Management/James MarRecon Code")

#Save working directory path as an object
wd <- getwd()

# Set File Paths
dir.data <- "~/Library/CloudStorage/GoogleDrive-evosfarmdata@gmail.com/My Drive/RBR Data"
# dir.data <- file.path("~/Desktop/RBR data that doesn't work")
dir.outputs <-file.path(wd, "outputs")


#______________________________________________________________


# List all files with *.rsk extension, recursively
rsk_files <- list.files(path = dir.data, recursive = TRUE, pattern = "*.rsk$", full.names = TRUE)

# Exclude files that are in the folder "exclude_folder"
rsk_files <- rsk_files[!grepl("duplicate", rsk_files)]
rsk_files <- rsk_files[!grepl("wont open in ruskin", rsk_files)]
rsk_files <- rsk_files[!grepl("wont open in r", rsk_files)]
rsk_files <- rsk_files[!grepl("data null", rsk_files)]
rsk_files <- rsk_files[!grepl("Tests", rsk_files)]
rsk_files <- rsk_files[!grepl("Meta's farm 557", rsk_files)]


# Initialize an empty list to store the CTD objects
rbr_list <- list()

#Test file upload
# test <- read.rsk(rsk_files[17])

# Loop through each .rsk file, read and convert to CTD
for (file in rsk_files) {
  # Read the .rsk file
  rbr_data <- read.rsk(file)
  
  # Convert to a CTD object
  rbr_data <- as.ctd(rbr_data)
  
  # Append the CTD object to the list
  rbr_list[[file]] <- rbr_data
}

# Combine all CTD data into a single data frame
# Extract the relevant data from each CTD object (e.g., pressure, temperature, salinity)
rbr_data_list <- lapply(rbr_list, function(rbr) {
  data.frame(
    time = rbr@data$time,
    pressure = rbr@data$pressure,
    temperature = rbr@data$temperature,
    conductivity = rbr@data$conductivity,
    salinity = rbr@data$salinity,
    latitude = rbr@metadata[["latitude"]],
    longitude = rbr@metadata[["longitude"]]
  )
})

# Combine all data frames into one
rbr_data <- do.call(rbind, rbr_data_list)
#Remove negative pressure rows (not in water)
rbr_data <- subset(rbr_data, pressure >= 0.2)

#Come up with site label
rbr_data <- rownames_to_column(rbr_data, var = "FilePaths")
rbr_data <- rbr_data %>% mutate(site = str_extract(FilePaths, "\\(([^)]+)\\)"))
rbr_data$site <- str_replace_all(rbr_data$site, "[()]", "")
rbr_data <- rbr_data[,-1]

# Reverse sign pressure for plotting
rbr_data$pressure <- -rbr_data$pressure 

#Add date
rbr_data$date <- as.Date(rbr_data$time)

#Get rid of data that we don't want
rbr_data <- rbr_data %>% filter(site %in% c("AOF1", "BCF1", "KIS1", "KOB1", "MIO1","SBR1","ROK1","SBO1","SSF1", "PBO1"))

#Sort data from Moss Island Oysters into Bootlegger's Cove (BCF1) and Peterson Bay (PBO1) sites by longitude



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

rbr_data$region <- region_values[rbr_data$site]



#Look at structure
str(rbr_data)

#Create a csv file 
write.csv(rbr_data, file.path(dir.outputs, "RBR_data.csv"), row.names = FALSE)

