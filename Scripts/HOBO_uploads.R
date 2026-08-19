#Upload and QAQC HOBO data files
#Updated: Dec 10 2025
#Sierra Greene 
#
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
library(tidyr)
library(cowplot)
library(stringr)
library(oce)
library(RSQLite)
library(ggplot2)
library(tibble)
library(tidyverse)

#This script uploads calibrated HOBO csvs from the Google drive 
#We need to modify the csvs before we combine them
#I only want to keep rows that have salinity data bc those have been calibrated
#there will be overlaps in times/dates- we need to delete the first row after each deleted section 

# Define data directory
dir.data <- file.path("H:/Shared drives/Mariculture ReCon/Data Management/Raw data from sensors/HOBO")

# Verify directory exists
if (!dir.exists(dir.data)) {
  stop("Data directory not found: ", dir.data)
}

# Get all CSV files recursively
csv_files <- list.files(
  path = dir.data, 
  pattern = "*.csv", 
  full.names = TRUE, 
  recursive = TRUE
)


# Read in temperature only csv  -------------------------------------------

# Get all CSV files specifically from "Temperature CSVs" subfolders
csv_files_temp <- list.files(
  path = dir.data,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
) 

# Keep only files inside "Temperature CSVs" folders
csv_files_temp <- csv_files_temp[grepl("Temperature CSVs", csv_files_temp)]

cat("Found", length(csv_files_temp), "CSV files in Temperature CSVs folders\n")

data_list_temp <- list()

for (file in csv_files_temp) {
  tryCatch({
    path_parts <- strsplit(file, "/|\\\\")[[1]]
    hobo_index <- which(path_parts == "HOBO")
    site_name <- path_parts[hobo_index + 1]
    
    df <- read_csv(file, skip = 2, col_names = FALSE, show_col_types = FALSE)
    
    df$site <- site_name
    df$source_file <- tools::file_path_sans_ext(basename(file))
    
    data_list_temp[[file]] <- df
    cat("✓ Loaded:", site_name, "-", basename(file), "\n")
    
  }, error = function(e) {
    warning("Failed to read ", basename(file), ": ", e$message)
  })
}

combined_df_temp <- bind_rows(data_list_temp)

#Remove col 4
combined_df_temp <- combined_df_temp %>% 
  select(-4)

#Rename columns 
#1-datetime, 2-Conductivity 3-Temp 4-SpCond 5-Salinity 
colnames(combined_df_temp) <- c("Time_UTC",
                                "Temp_C", 
                                "Site")

#rename df
hobo_temp <- combined_df_temp

#Add region
#Create a column identifying region
region_values <- c(
  AOF1 = "KOD",
  KOB1 = "KOD",
  KIS1 = "KOD",
  SSF1 = "KBY",
  MIO1 = "KBY",
  BCF1 = "KBY",
  ROK1 = "PWS",
  SBO1 = "PWS",
  SBR1 = "PWS"
)

hobo_temp$region <- region_values[hobo_temp$Site]

#Create a new df to make sure times parse 
df_test <- hobo_temp

df_test <- df_test %>%
  mutate(
    Time_UTC = if_else(
      str_detect(Time_UTC, "/\\d{4}\\s"),  # Has 4-digit year
      mdy_hm(Time_UTC),
      mdy_hm(Time_UTC)  # lubridate handles 2-digit years automatically
    ),
    date = as.Date(Time_UTC),
    year = year(Time_UTC)
  )
#Looks good
hobo_temp <- df_test
##STOPE HERE TO MAKE SURE ALL TIMES PARSE
#Check to make sure all dates have parsed correctly. If some sites have not, but you thik they should (format is correct), open their original CSV in the drive click "save as" CSV, then rerun all code- for some reason this helps


# Read in all hobo data ---------------------------------------------------

# Exclude specific folders- only look in calibrated folders
exclude_patterns <- c("NOT calibrated CSVs", "RAW HOBO files", "Read_me", "Misc", "SBO1_3m", "Error_files", "Hidden", "CSV not broken up", "SBO1_3m", "AN IMPORTANT Read me!!", "Temperature CSVs")
for (pattern in exclude_patterns) {
  csv_files <- csv_files[!grepl(pattern, csv_files)]
}

# Check if any files remain
if (length(csv_files) == 0) {
  stop("No CSV files found after applying filters")
}

cat("Found", length(csv_files), "CSV files to process\n")

# Function to clean each dataframe
clean_dataframe <- function(df) {
  # Find rows where column 5 is NA or empty
  rows_to_delete <- which(is.na(df[[5]]) | df[[5]] == "")
  
  if (length(rows_to_delete) == 0) {
    return(df)  # No rows to delete
  }
  
  # For each row to delete, find duplicate date/times and mark the closest one
  additional_rows_to_delete <- c()
  
  for (row_idx in rows_to_delete) {
    # Get the date/time value from column 2 (adjust if your date/time is in a different column)
    datetime_val <- df[[2]][row_idx]
    
    # Find all rows with the same date/time
    duplicate_rows <- which(df[[2]] == datetime_val)
    
    # If there are duplicates, find the one closest to the deleted row
    if (length(duplicate_rows) > 1) {
      # Remove the current row from duplicates list
      duplicate_rows <- duplicate_rows[duplicate_rows != row_idx]
      
      # Find closest row by distance
      distances <- abs(duplicate_rows - row_idx)
      closest_row <- duplicate_rows[which.min(distances)]
      
      additional_rows_to_delete <- c(additional_rows_to_delete, closest_row)
    }
  }
  
  # Combine all rows to delete
  all_rows_to_delete <- unique(c(rows_to_delete, additional_rows_to_delete))
  
  # Remove rows
  if (length(all_rows_to_delete) > 0) {
    df <- df[-all_rows_to_delete, ]
  }
  
  return(df)
}

# Read, clean, and combine CSV files
data_list <- list()
for (file in csv_files) {
  tryCatch({
    df <- read_csv(file, skip = 2, col_names = FALSE, show_col_types = FALSE)
    df$source_file <- tools::file_path_sans_ext(basename(file))
    
    # Clean the dataframe
    df_cleaned <- clean_dataframe(df)
    
    data_list[[file]] <- df_cleaned
    cat("✓ Loaded and cleaned:", basename(file), "\n")
  }, error = function(e) {
    warning("Failed to read ", basename(file), ": ", e$message)
  })
}


# Combine into one data frame
combined_df <- bind_rows(data_list, .id = "file_path")

##STOP HERE TO SEE IF THERE ARE ISSUES COMBINING##

# Create site label from file paths
hobo_all <- combined_df %>%
  mutate(
    # Extract site code from file path (e.g., "AOF1", "BCF1")
    site = str_extract(file_path, "/(\\w+)/Endpoint", group = 1),
    
    # Alternative: Extract from filename if site is in the filename
    # site = str_extract(source_file, "^[A-Z]+_([A-Z0-9]+)_", group = 1),
    
    # Clean up if needed
    site = str_trim(site)
  ) %>%
  
#Remove columns we dont need
  select(-file_path,-source_file)

#Remove col 6 and7 and temp data
hobo_all <- hobo_all %>% 
  select(-6, -7, -3)

#Rename columns 
#1-datetime, 2-Conductivity 3-Temp 4-SpCond 5-Salinity 
colnames(hobo_all) <- c("Time_UTC",
                         "Cond_uS.cm",
                         "SpCond_uS.cm",
                         "Sal_PSU",
                         "Site")

#Corrtect date/time
df_test <- hobo_all

df_test <- df_test %>%
  mutate(
    Time_UTC = if_else(
      str_detect(Time_UTC, "/\\d{4}\\s"),  # Has 4-digit year
      mdy_hm(Time_UTC),
      mdy_hm(Time_UTC)  # lubridate handles 2-digit years automatically
    ),
    date = as.Date(Time_UTC),
    year = year(Time_UTC)
  )
hobo_all <- df_test

str(hobo_all)

#check for duplicated that will mess up joining
# Check for duplicates in hobo_temp
temp_dup <- hobo_temp %>%
  group_by(Time_UTC, Site) %>%
  filter(n() > 1) %>%
  arrange(Time_UTC, Site)

# Check for duplicates in hobo_sal
hobo_dup <- hobo_all %>%
  group_by(Time_UTC, Site) %>%
  filter(n() > 1) %>%
  arrange(Time_UTC, Site)

#duplicates are from an overlap in new sensors out and old ones being pulled. Not sure how to access which is which- im going to delete all duplicates
#
hobo_temp <- hobo_temp %>% distinct(Time_UTC, Site, .keep_all = TRUE)
hobo_all  <- hobo_all  %>% distinct(Time_UTC, Site, .keep_all = TRUE)

# Combine hobo sal/cond with temp data ------------------------------------

# Select relevant columns from hobo_data
hobo_sal <- hobo_all %>%
  select(Time_UTC, Site, Sal_PSU, Cond_uS.cm, SpCond_uS.cm)

# Join to hobo_temp where datetime and site match
hobo_data <- hobo_temp %>%
  left_join(hobo_sal, by = c("Time_UTC", "Site"))

 
# Start QAQC --------------------------------------------------------------
#remove points where sensors def weren't in the water
hobo_data <- hobo_data %>%filter(Cond_uS.cm > 1000)
#this is low, but accounts for errors where cond channel didn't work but temp did 


#Remove points outside of biological expectations
# Salinity over 35
hobo_data$Sal_PSU[hobo_data$Sal_PSU > 35] <- NA
#Conductivity over 50000
hobo_data$Cond_uS.cm[hobo_data$Cond_uS.cm > 50000] <- NA
#Specific Conductivity over 55000
hobo_data$SpCond_uS.cm[hobo_data$SpCond_uS.cm > 55000] <- NA

#hobo_data is the working df of initial QAQC data

#Lets remove spcond, we dont really need that
hobo_data <- hobo_data %>% 
  select(-9)

# Manualy remove drfit chunks ---------------------------------------------
#look for sections that are clear drifs in salinity that cannot be saved
#AOF1- drift in 06/2025

AOF1_spring <- hobo_data %>%
  filter(Site == "AOF1",
         year(Time_UTC) == 2025,
         month(Time_UTC) %in% c(5, 6, 7)) %>%
  ggplot(aes(x = Time_UTC, y = Sal_PSU)) +
  geom_point(color = "steelblue") +
  labs(
    title = "Salinity at AOF1 — June 2025",
    x = "Date",
    y = "Salinity (PSU)"
  ) +
  theme_minimal()
#Looks like 6/15 is where downdrift starts. Make sal, cond, spcond NA
hobo_data <- hobo_data %>%
  mutate(
    Sal_PSU = ifelse(Site == "AOF1" & 
                  Time_UTC >= as.POSIXct("2025-06-15", tz = "UTC") & 
                  Time_UTC <= as.POSIXct("2025-07-01", tz = "UTC"), 
                     NA, Sal_PSU),
    Cond_uS.cm = ifelse(Site == "AOF1" & 
                  Time_UTC >= as.POSIXct("2025-06-15", tz = "UTC") & 
                  Time_UTC <= as.POSIXct("2025-07-01", tz = "UTC"), 
                        NA, Cond_uS.cm))
  


#SSF1- drift in spring 2024
SSF1_spring <- hobo_data %>%
  filter(Site == "SSF1",
         year(Time_UTC) == 2024,
         month(Time_UTC) %in% c(1,2,3,4,5)) %>%
  ggplot(aes(x = Time_UTC, y = Sal_PSU)) +
  geom_point(color = "steelblue") +
  labs(
    title = "Salinity at SSF1",
    x = "Date",
    y = "Salinity (PSU)"
  ) +
  theme_minimal()
#remove March data
hobo_data <- hobo_data %>%
  mutate(
    Sal_PSU = ifelse(Site == "SSF1" & 
                       Time_UTC >= as.POSIXct("2024-03-01", tz = "UTC") & 
                       Time_UTC <= as.POSIXct("2024-04-01", tz = "UTC"), 
                     NA, Sal_PSU),
    Cond_uS.cm = ifelse(Site == "SSF1" & 
                          Time_UTC >= as.POSIXct("2024-03-01", tz = "UTC") & 
                          Time_UTC <= as.POSIXct("2024-04-01", tz = "UTC"), 
                        NA, Cond_uS.cm))

#Look at SD 
#Define variables to use in calculations before altering headers
vars <- c("Cond_uS.cm", "Temp_C", "Sal_PSU")

#Calculate SD for each variable
sd_hobo <- hobo_data %>%
  dplyr::group_by(Site, year) %>%
  dplyr::summarise(across(all_of(vars), ~ sd(., na.rm = TRUE), .names = "SD_{.col}"),
                   .groups = "drop")

#Join SD data back to RAW data in new df
hobo_data_yr <- left_join(hobo_data, sd_hobo, by = c("Site", "year"))

#hobo_data_yr is our working df for raw data and stats

#Flag outliers if they are > 3 * SD from previous point 
# Initialize outlier columns
for (var in vars) {
  hobo_data_yr[[paste0("outlier_", var)]] <- 0
}

# Loop through rows
for (i in 2:nrow(hobo_data_yr)) {
  
  site_curr  <- hobo_data_yr$Site[i]
  site_prev  <- hobo_data_yr$Site[i - 1]
  year_curr  <- hobo_data_yr$year[i]
  year_prev  <- hobo_data_yr$year[i - 1]
  
  # First: Check that nothing is NA
  if (!is.na(site_curr) && !is.na(site_prev) &&
      !is.na(year_curr) && !is.na(year_prev)) {
    
    # Second: Check if Site and Year match between current and previous row
    if (site_curr == site_prev && year_curr == year_prev) {
      
      for (var in vars) {
        val_curr <- hobo_data_yr[[var]][i]
        val_prev <- hobo_data_yr[[var]][i - 1]
        sd_val   <- hobo_data_yr[[paste0("SD_", var)]][i]
        
        if (!is.na(val_curr) && !is.na(val_prev) && !is.na(sd_val)) {
          if (abs(val_curr - val_prev) > (3 * sd_val)) {
            hobo_data_yr[[paste0("outlier_", var)]][i] <- 1
          }
        }
      }
    }
  }
}

#Look at plots to visualize outliers/ bad data that should be removed 

Sal_SD <- ggplot(hobo_data_yr, aes(x = date, y = Sal_PSU)) +
  geom_point(aes(color = as.factor(outlier_Sal_PSU)), alpha = 0.6) +
  labs(
    title = "Sal outlier check (x3 SD)",
    x = "Date",
    y = "Sal"
  ) +
  scale_color_manual(
    values = c("0" = "green", "1" = "red"),
    name = "outlier"
  ) +
  facet_wrap(~Site) +
  theme_minimal()

Temp_SD <- ggplot(hobo_data_yr, aes(x = date, y = Temp_C)) +
  geom_point(aes(color = as.factor(outlier_Temp_C)), alpha = 0.6) +
  labs(
    title = "temp outlier check (x3 SD)",
    x = "Date",
    y = "Temp"
  ) +
  scale_color_manual(
    values = c("0" = "green", "1" = "red"),
    name = "outlier"
  ) +
  facet_wrap(~Site) +
  theme_minimal()

Cond_SD <- ggplot(hobo_data_yr, aes(x = date, y = Cond_uS.cm)) +
  geom_point(aes(color = as.factor(outlier_Cond_uS.cm)), alpha = 0.6) +
  labs(
    title = "Cond outlier check (x3 SD)",
    x = "Date",
    y = "Cond"
  ) +
  scale_color_manual(
    values = c("0" = "green", "1" = "red"),
    name = "outlier"
  ) +
  facet_wrap(~Site) +
  theme_minimal()

library(patchwork)
Sal_SD + Cond_SD

#remove SD outlier points to match EXO QAQC
#Delete points that are outliers 
hobo_data_final <- hobo_data_yr %>%
  mutate(
    Cond_uS.cm = ifelse(outlier_Cond_uS.cm == 1, NA, Cond_uS.cm),
    Temp_C = ifelse(outlier_Temp_C == 1, NA, Temp_C),
    Sal_PSU = ifelse(outlier_Sal_PSU == 1, NA, Sal_PSU)
  )

#Clean up df
hobo_data_final <- hobo_data_final %>%
  select(1:8)

#Make all caps 
colnames(hobo_data_final) <- toupper(colnames(hobo_data_final))

#Export hobo data 
#all hobo data
write.csv(hobo_data_final, file = "hobo_data_final.csv", row.names = FALSE)

#2023 data
hobo_data_final23 <- hobo_data_final %>%
  filter(YEAR == "2023")

write.csv(hobo_data_final23, file = "hobo_data_final23.csv", row.names = FALSE)

#2024 data
hobo_data_final24 <- hobo_data_final %>%
  filter(YEAR == "2024")

write.csv(hobo_data_final24, file = "hobo_data_final24.csv", row.names = FALSE)


# Old code ----------------------------------------------------------------
#Export RAW 2024 and 2025 data for RWS
#2024
# hobo_data24 <- hobo_data %>%
#   filter(year =="2024")
# write.csv(hobo_data24, file.path(wd, "hobo_data24.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# #2025
# hobo_data25 <- hobo_data %>%
#   filter(year =="2025")
# write.csv(hobo_data25, file.path(wd, "hobo_data25.csv"), row.names = FALSE, fileEncoding = "UTF-8")


# #remove points where HOBO was in warm saltwater bucket: 
# #Upload CSV of deployment starts/stops to remove data near then
# cond_log <- read_csv("H:/Shared drives/Mariculture ReCon/Data Management/Raw data from sensors/HOBO/Misc/HOBO_Cond_log_18DEC2025.csv")
# #rename column 9 "solution"
# # Rename column 9
# names(cond_log)[9] <- "Sol_type"
# 
# #Fix date 
# cond_log$Date_UTC <- as_date(cond_log$Date_UTC, format = "%m/%d/%Y") 
# #Create datetime column
# cond_log$Datetime <- ymd_hms(paste(cond_log$Date_UTC, cond_log$Time_UTC), tz = "UTC")
# 
# #Delete points around when sensors were calibrating in saltwater bucket
# # Filter cond_log for saltwater bucket entries
# bucket_entries <- cond_log %>%
#   filter(Sol_type == "saltwater bucket")
# 
# # Check if there are any bucket entries
# if (nrow(bucket_entries) > 0) {
#   
#   # Loop through each bucket entry
#   for (i in 1:nrow(bucket_entries)) {
#     bucket_site <- bucket_entries$site[i]
#     bucket_datetime <- bucket_entries$datetime[i]
#     
#     # Find matching row in hobo_data
#     match_idx <- which(hobo_data$site == bucket_site & 
#                          hobo_data$datetime == bucket_datetime)
#     
#     if (length(match_idx) > 0) {
#       # Calculate indices of 5 rows before the match
#       rows_to_delete <- (match_idx - 5):(match_idx - 1)
#       
#       # Keep only valid row indices (in case there are fewer than 5 rows before)
#       rows_to_delete <- rows_to_delete[rows_to_delete > 0]
#       
#       # Delete those rows
#       if (length(rows_to_delete) > 0) {
#         hobo_data <- hobo_data[-rows_to_delete, ]
#         cat("Deleted", length(rows_to_delete), "rows before", bucket_datetime, 
#             "at site", bucket_site, "\n")
#       }
#     } else {
#       warning("No matching datetime found in hobo_data for ", bucket_datetime, 
#               " at site ", bucket_site)
#     }
#   }
#   
#   cat("\nTotal rows remaining in hobo_data:", nrow(hobo_data), "\n")
#   
# } else {
#   cat("No 'saltwaterbucket' entries found in cond_log\n")
# }
#   

#Median QAQC analysis
#Use median rather than mode so these outliers dont impact QAQC
library(pracma)

#Look for MAD outliers in any of the vars while accounting for NA
safe_hampel <- function(x, k = 24, t0 = 3) {
  flag <- rep(0, length(x))
  non_na_idx <- which(!is.na(x))
  
  if (length(non_na_idx) > k) {
    result <- hampel(x[non_na_idx], k = k, t0 = t0)
    flag[non_na_idx[result$ind]] <- 1
  }
  return(flag)
}

# Apply Hampel filter to all three variables
hobo_data_med <- hobo_data %>%
  group_by(Site) %>%
  arrange(Time_UTC, .by_group = TRUE) %>%
  mutate(
    outlier_hampel_temp = safe_hampel(Temp_C,     k = 24, t0 = 3),
    outlier_hampel_sal  = safe_hampel(Sal_PSU,    k = 24, t0 = 3),
    outlier_hampel_cond = safe_hampel(Cond_uS.cm, k = 24, t0 = 3),
    outlier_hampel_any  = as.integer(
      outlier_hampel_temp == 1 | outlier_hampel_sal == 1 | outlier_hampel_cond == 1
    )
  ) %>%
  ungroup()


# #Look at temp data w/ outliers identified
# #
# hobo_data_2024 <- hobo_data_med %>%
#   filter(year == "2024")
# 
# hobo_data_2025 <- hobo_data_med %>%
#   filter(year == "2025")


Temp <- ggplot(hobo_data_med, aes(x = date, y = Temp_C)) +
  geom_point(aes(color = as.factor(outlier_hampel_temp)), alpha = 0.6) +
  labs(
    title = "Temp outlier check",
    x = "Date",
    y = "Temp"
  ) +
  scale_color_manual(
    values = c("0" = "green", "1" = "red"),
    name = "outlier"
  ) +
  facet_wrap(~Site) +
  theme_minimal()

Sal <- ggplot(hobo_data_med, aes(x = date, y = Sal_PSU)) +
  geom_point(aes(color = as.factor(outlier_hampel_sal)), alpha = 0.6) +
  labs(
    title = "Sal outlier check",
    x = "Date",
    y = "Sal"
  ) +
  scale_color_manual(
    values = c("0" = "green", "1" = "red"),
    name = "outlier"
  ) +
  facet_wrap(~Site) +
  theme_minimal()

Cond <- ggplot(hobo_data_med, aes(x = date, y = Cond_uS.cm)) +
  geom_point(aes(color = as.factor(outlier_hampel_cond)), alpha = 0.6) +
  labs(
    title = "Cond outlier check",
    x = "Date",
    y = "Cond"
  ) +
  scale_color_manual(
    values = c("0" = "green", "1" = "red"),
    name = "outlier"
  ) +
  facet_wrap(~Site) +
  theme_minimal()

Cond
Temp
Sal


#
# #Looks like temp  gets most of the bucket points
# #Remove rows where temp has outlier 
# hobo_data1 <- hobo_data_med %>%
#   filter(outlier_hampel_temp == 0)
# 
# #Look at data now
# Temp <- ggplot(hobo_data1, aes(x = date, y = Temp_C)) +
#   geom_point(alpha = 0.6) +
#   labs(
#     title = "Temp check, outliers removed",
#     x = "Date",
#     y = "Temp"
#   ) +
#   facet_wrap(~Site) +
#   theme_minimal()
# 
# Cond <- ggplot(hobo_data1, aes(x = date, y = Cond_uS.cm)) +
#   geom_point(alpha = 0.6) +
#   labs(
#     title = "Cond check, outliers removed",
#     x = "Date",
#     y = "Cond"
#   ) +
#   facet_wrap(~Site) +
#   theme_minimal()
# 
# Sal <- ggplot(hobo_data1, aes(x = date, y = Sal_PSU)) +
#   geom_point(alpha = 0.6) +
#   labs(
#     title = "Sal check, outliers removed",
#     x = "Date",
#     y = "Sal"
#   ) +
#   facet_wrap(~Site) +
#   theme_minimal()





# #Still some cal points that didnt get removed
# #Lets use the hamel filter again 
# hobo_data_med1 <- hobo_data1 %>%
#   group_by(Site) %>%
#   arrange(Datetime, .by_group = TRUE) %>%
#   mutate(
#     outlier_hampel_temp = safe_hampel(Temp_C,     k = 24, t0 = 3),
#     outlier_hampel_sal  = safe_hampel(Sal_PSU,    k = 24, t0 = 3),
#     outlier_hampel_cond = safe_hampel(Cond_uS.cm, k = 24, t0 = 3),
#     outlier_hampel_any  = as.integer(
#       outlier_hampel_temp == 1 | outlier_hampel_sal == 1 | outlier_hampel_cond == 1
#     )
#   ) %>%
#   ungroup()
# 
# #Plot temp
# Temp <- ggplot(hobo_data_med1, aes(x = date, y = Temp_C)) +
#   geom_point(aes(color = as.factor(outlier_hampel_temp)), alpha = 0.6) +
#   labs(
#     title = "Temp outlier check",
#     x = "Date",
#     y = "Temp"
#   ) +
#   scale_color_manual(
#     values = c("0" = "green", "1" = "red"),
#     name = "outlier"
#   ) +
#   facet_wrap(~Site) +
#   theme_minimal()
# 
# #Plot temp
# Sal <- ggplot(hobo_data_med1, aes(x = date, y = Sal_PSU)) +
#   geom_point(aes(color = as.factor(outlier_hampel_sal)), alpha = 0.6) +
#   labs(
#     title = "Sal outlier check",
#     x = "Date",
#     y = "Temp"
#   ) +
#    scale_color_manual(
#      values = c("0" = "green", "1" = "red"),
#      name = "outlier"
#    ) +
#    facet_wrap(~Site) +
#   theme_minimal()
# 
# 
# 
# 
# 
# # Function to create plots with flexible y variable and facet
# create_outlier_plot <- function(data, y_var, outlier_var, 
#                                 facet_var = "Site", 
#                                 title_prefix = "Regional") {
#   
#   # Create the plot
#   p <- ggplot(data, aes(x = date, y = .data[[y_var]])) +
#     geom_point(aes(color = factor(.data[[outlier_var]])), alpha = 0.6) +
#     labs(
#       title = paste(title_prefix, toupper(gsub("_.*", "", y_var))),
#       x = "Date",
#       y = y_var
#     ) +
#     scale_color_manual(
#       values = c("0" = "green", "1" = "red"),
#       name = "Outlier"
#     ) +
#     facet_wrap(as.formula(paste("~", facet_var)), ncol = 1, scales = "free_y") +
#     theme_minimal()
#   
#   return(p)
# }
# 
# # Loop through multiple y-variables
# variables <- c("Sal_PSU", "Temp_C", "Cond_uS.cm")  # Add your variable names
# 
# #Look at data in 2023
# hobo_data_2025 <- hobo_data_yr %>%
#   filter(year == "2025")
# 
# hobo_data_2024 <- hobo_data_yr %>%
#   filter(year == "2024")
# 
# for (var in variables) {
#   outlier_col <- paste0("outlier_", var)
#   
#   p <- create_outlier_plot(
#     data = hobo_data_2025,
#     y_var = var,
#     outlier_var = outlier_col,
#     facet_var = "Site"
#   )
#   
#   print(p)}
# 
# 
# #salinity and cond data from SSF1 and BCF1 do not look reliable in 2023
# #
# #looks pretty good!




