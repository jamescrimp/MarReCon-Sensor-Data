#This is the most recent script for RBR data 
#SG 8/14/26
#
#Function of this script:
# Pulls .rsk files from the EVOS Farmer google drive ("H:/My Drive/RBR Data") and combines them based on farm folder
# Tells you which files were read in correctly and which had errors (helpful to use Ruskin to check error files)
# Creates one CSV file “rbr_data.csv” that has all profile data from readable .rsk files combined

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


#setwd("G:/My Drive/RBR Data")

#Save working directory path as an object
wd <- getwd()

# Set File Paths: Sierra 
# (change this to your local desktop path if youre not sierra)
dir.data <- ("H:/My Drive/RBR Data")

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
rsk_files <- rsk_files[!grepl("MarRecon backups", rsk_files)]
rsk_files <- rsk_files[!grepl("screenshots", rsk_files)]
rsk_files <- rsk_files[!grepl("Sean", rsk_files)]
rsk_files <- rsk_files[!grepl("offloaded March 3", rsk_files)]
rsk_files <- rsk_files[!grepl("exclude", rsk_files)]
rsk_files <- rsk_files[!grepl("duplicates", rsk_files)]
rsk_files <- rsk_files[!grepl("questions", rsk_files)]
rsk_files <- rsk_files[!grepl("Ruskin xls", rsk_files)]


####
# Read in CTD .rsk files from Google Drive and keep only down cast data
# 
# Empty list to store the CTD objects
rbr_list <- list()
# Store names of skipped files- corrupt for some reason 
skipped_files <- character()  


# Loop through each .rsk file, read and convert to CTD
for (file in rsk_files) {
  tryCatch({
    # Read the .rsk file
    rbr_data <- read.rsk(file)
    
    # Convert to a CTD object
    rbr_data <- as.ctd(rbr_data)
    
    # Trim to keep only downcast data
    rbr_data <- ctdTrim(rbr_data)
    
    # Append the CTD object to the list
    rbr_list[[file]] <- rbr_data
    
    # Optional: print progress for successful files
    cat("Successfully processed and trimmed:", basename(file), "\n")
    
  }, error = function(e) {
    # Add failed file to skipped list
    skipped_files <<- c(skipped_files, file)
    
    # Print error message
    cat("SKIPPED:", basename(file), "- Error:", e$message, "\n")
  })
}

# Report summary at the end
cat("\n=== PROCESSING SUMMARY ===\n")
cat("Successfully processed:", length(rbr_list), "files\n")
cat("Skipped:", length(skipped_files), "files\n")

if (length(skipped_files) > 0) {
  cat("\nSkipped files:\n")
  for (file in skipped_files) {
    cat(" -", basename(file), "\n")
  }
}


# Combine all CTD data into a single data frame
# Extract the relevant data from each CTD object (e.g., pressure, temperature, salinity)
rbr_data_list <- lapply(rbr_list, function(rbr) {
  data.frame(
    time = rbr@data$time,
    pressure = rbr@data$pressure,
    temperature = rbr@data$temperature,
    conductivity = rbr@data$conductivity,
    salinity = rbr@data$salinity
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
#rbr_data$pressure <- -rbr_data$pressure 

#Add date
rbr_data$date <- as.Date(rbr_data$time)

#Get rid of data that we don't want
rbr_data <- rbr_data %>% 
  dplyr::filter(site %in% c("AOF1", "BCF1", "KIS1", "KOB1", "MIO1","SBR1","ROK1","SBO1","SBO1","SSF1"))




#Look at structure
str(rbr_data)

#Fix sites that were recorded in AK time zone in 2024, all data in 2025 is OK
#Spring:3/10/2024 
#Fall: 11/3/202
#   1/1/24 - 3/10/24 = +9 hrs
#   3/10/24 - 11/3/24 = +8 hrs
#   11/3/24 - 12/31/24 = +9 hrs
#Correct: all of AOF1, MIO1 and BCF1 until June 
#Make a temporary df in case we mess up
rbr_timefix <- rbr_data

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

rbr_data <- rbr_timefix  

#Pull of data from 2026 for AOF1 
AOF126 <- rbr_data

AOF126 <- rbr_data %>%
  filter(year(date) == 2026, site == "AOF1")

write.csv(AOF126, file.path("C:/Users/sierr/MarReCon-Sensor-Data/csv_exports/
AOF1_2026.csv"), row.names = FALSE)

#write.csv(AOF126, file.path(wd, "AOF1_RBR_2026.csv"), row.names = FALSE)


#Create a csv file 
write.csv(rbr_data, file.path("C:/MarRecon_code/thesis_work/RBR_code/ RBR_data.csv"), row.names = FALSE)
