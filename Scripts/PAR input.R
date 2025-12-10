
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


#Save working directory path as an object
wd <- getwd()

#Save data and output as objects
dir.data <- file.path(wd, "Raw data from sensors/PAR")  
dir.outputs <-file.path(wd, "Outputs")
dir.csv <-file.path(wd, "CSVs")


par_files <- list.files(path = dir.data, pattern = "\\.TXT$", full.names = TRUE, recursive = TRUE)

#Empty list to store individual data frames
data_list <- list()

#Loop through each CSV file and read it into a data frame
for (file in par_files) {
  df <- read.csv(file, skip = 9, header = FALSE)
  
  #Create a site identifier for each dataframe
  df$path <- tools::file_path_sans_ext(basename(file))
  
  data_list[[file]] <- df
}

# Combine all data frames into one
par_data <- bind_rows(data_list)

# Add a unique site label
par_data <- par_data %>% mutate(site = str_extract(path, "[A-Z]+\\d+(?=_\\d{2}[A-Z]{3}\\d{2}$)"))

# Get rid of columns we don't need
par_data <- par_data[,c(-1, -2)]

# Add Column labels
colnames(par_data) <- c("Time_UTC", "Battery_V", "Temp_C", "PAR","Acceleration_X","Acceleration_Y", "Acceleration_Z", "Filepath", "Site")

#Add columns for lat and long
par_data$Latitude <- NA
par_data$Longitude <- NA

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

par_data$Latitude <- latitude_values[par_data$Site]
par_data$Longitude <- longitude_values[par_data$Site]

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

par_data$region <- region_values[par_data$Site]

#Create a csv file for further review
write.csv(par_data, file.path(dir.csv, "PAR_data.csv"), row.names = FALSE, fileEncoding = "UTF-8")
