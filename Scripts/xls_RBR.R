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
#Updated 8/18/2026
#Sierra Greene

#NOTE: For some reason, the only version of Ruskin that will open certain files is 
#V2.17.202203042007. Files from certain farms (especially ROK1) wont open in new verions. The Ruskin support tech thinks this is a bug with newer versions that wont open mobile .rsk files

wd <- getwd()

# Set File Paths
dir.data <- ("H:/My Drive/RBR Data")
# dir.data <- file.path("~/Desktop/RBR data that doesn't work")
dir.outputs <-file.path(wd, "outputs")



# List all files with *.xlsx extension
xrsk_files <- list.files(path = dir.data, recursive = TRUE, pattern = "*.xlsx$", full.names = TRUE)


#Troubleshoot for missing data
#
#Make sure all headers are the same (or at least a sample)
lapply(xrsk_files[c(1, 50, 100, 150)], function(f) {
  names(read_excel(f, sheet = "Data", skip = 1))
})
#Looks good

#Test pressure vs sea pressure errors
#make sure we are only reading in sea pressure
lapply(xrsk_files[c(1, 50, 100, 150)], function(f) {
  names(read_excel(f, sheet = "Data", skip = 1))[c(1,2,3,5,7,9,10)]
})

#Read in csvs and convert to ctd objects
#make sure loop continues if there are errors 
data_list <- list()
skipped <- list()

for (file in xrsk_files) {
  sheet_data <- tryCatch(
    read_excel(file, sheet = "Data", skip = 1),
    error = function(e) { message("Can't read: ", basename(file)); NULL }
  )
  if (is.null(sheet_data)) next
  
  # Guard: skip files that don't have enough columns
  if (ncol(sheet_data) < 10) {
    skipped[[file]] <- names(sheet_data)
    message("SKIPPING (only ", ncol(sheet_data), " cols): ", basename(file))
    next
  }
  
  # Keep only columns 1,2,3,5,7,9,10 by position (5 = Sea pressure)
  sheet_data <- sheet_data %>%
    select(1, 2, 3, 5, 7, 9, 10) %>%
    mutate(FilePath = file)
  
  # Rename by name (unambiguous now that duplicate/extra pressure cols are gone)
  needed <- c("Temperature", "Salinity", "Sea pressure")
  missing_cols <- setdiff(needed, names(sheet_data))
  if (length(missing_cols) > 0) {
    skipped[[file]] <- names(sheet_data)
    message("SKIPPING (missing: ", paste(missing_cols, collapse = ", "), "): ", basename(file))
    next
  }
  
  sheet_data <- sheet_data %>%
    rename(
      temperature = "Temperature",
      salinity = "Salinity",
      pressure = "Sea pressure"
    )
  
  ctd_obj <- as.ctd(sheet_data)
  ctd_trimmed <- ctdTrim(ctd_obj, method = "downcast")
  
  cat(basename(file), ": raw =", nrow(sheet_data),
      " trimmed =", length(ctd_trimmed[["pressure"]]), "\n")
  
  data_list[[file]] <- ctd_trimmed
}

#Check to make sure that all files read in- if so, the xrsk_files will match the data_list number
length(xrsk_files); length(data_list); length(skipped)

#Which files got skipped?
names(skipped)          # file paths that were skipped
skipped[[1]]             # column names of the first skipped file, to see what's actually in it
#These files sus and are ok to be skipped

#Combine into one df
xrbr_data <- dplyr::bind_rows(lapply(data_list, function(ctd_obj) {
  as.data.frame(ctd_obj@data)
}))

#remove rows collected out of the water
xrbr_data <- subset(xrbr_data, pressure >= 0.2)

#Add site names based on folder files are located in 
xrbr_data <- xrbr_data %>%
  mutate(Site = str_extract(FilePath, "(?<=\\()[^\\)]+"))

#only keep data we need: time, pressure, temperature, conductivity, site
xrbr_data <- xrbr_data %>%
  select(Time, pressure, temperature, Conductivity, salinity, Site)

#Add Date
xrbr_data <- xrbr_data %>%
  mutate(date = as.Date(Time))

xrbr_data <- xrbr_data %>%
  rename_with(tolower)


#----------------
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

xrbr_test <- xrbr_data
library(janitor)
#Check for duplicate rows
xrbr_test %>% get_dupes()

#Check for duplicates based on site
xrbr_test %>% get_dupes(site)

#Remove duplicate rows (keep first one)
xrbr_test1 <- xrbr_test %>% distinct()

xrbr_data <- xrbr_test1

xrbr_test1 %>%
  distinct(site, date) %>%
  count(site, name = "n_sampling_events")

write.csv(xrbr_data, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\xRBR_data_18AUG26.csv"), row.names = FALSE)

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


