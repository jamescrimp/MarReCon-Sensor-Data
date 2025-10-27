
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


#Set Working Directory 
setwd("~/Library/CloudStorage/GoogleDrive-jcrimp@alaska.edu/Shared drives/Mariculture ReCon/Data Management")

#Save working directory path as an object
wd <- getwd()

#Create paths for data and outputs
dir.csv <- file.path(wd, "James MarRecon Code/outputs/CSVs")  
dir.outputs <-file.path(wd, "James MarRecon Code/outputs")

# Import EXO data

exo_data <- read.csv(file.path(dir.csv, "EXO_I_data.csv"), header = TRUE)
exo_data$Time_UTC <- as.POSIXct(exo_data$Time_UTC, format = "%Y-%m-%d %H:%M:%S")
exo_data$Date <- as.Date(exo_data$Date)

#Initial Conductivity QAQC
#Exclude values where conductivity < 10,000 (??). This indicates that the exo was out out the water,
#likely while getting services/data uploaded.

exo_data <- exo_data %>%filter(Cond_uS.cm > 10000)


#Now lets look at the time series of the variables we are interested in to see if there are still
#signs of data errors

# Denote variables of interest
variables <- names(exo_data)[sapply(exo_data, is.numeric)]  

#Create a time series of the raw data from variables of interest for each farm

site_names <- unique(exo_data$Site)

#Create a new folder within outputs to store these plots
exo_raw_plots <- file.path(dir.outputs, "EXO_raw_plots")
if (!dir.exists(exo_raw_plots)) {
  dir.create(exo_raw_plots)
}

for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(exo_raw_plots, paste0("Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for the current variable
  for (Site in unique(exo_data$Site)) {
    # Filter data for the current site
    site_data <- exo_data[exo_data$Site == Site, ]
    
    # Create the time series plot
    p <- ggplot(site_data, aes(x = Date, y = .data[[variable]])) +
      geom_line(color = "blue") +
      labs(
        title = paste("Time Series for", variable, "at", Site),
        x = "Date",
        y = variable
      ) +
      scale_x_date(
        limits = c(as.Date("2023-10-01"), as.Date("2024-12-31")), # Set range from October 2023 to December 2024
        date_breaks = "2 months",                                 # Optional: Breaks every 2 months
        date_labels = "%b %Y"                                     # Format: "Oct 2023", "Dec 2023", etc.
      )+
      theme_minimal()
    
    # Print the plot to the PDF
    print(p)
  }
  
  # Close the PDF file
  dev.off()
}
  

#Graphs of the raw data still show numerous distinct outliers and maybe some instruments that need calibration.

#For Chlorophyll, we notice that BCF1 had a long below zero stretch from October 2023 to April 2024 (when the sensor was swapped).
#This looks like a calibration error, so we will take out the data for this time, and for all other stretches where chlorophyll RFU values
#are lower than -1.  Values between -1 and zero we will change to zero.

exo_data$Chlorophyll_RFU <- ifelse(
  exo_data$Chlorophyll_RFU < -1, NA,                          # Below -1 to NA
  ifelse(exo_data$Chlorophyll_RFU >= -1 & exo_data$Chlorophyll_RFU < 0, 
         0,                                                   # Between -1 and 0 to 0
         exo_data$Chlorophyll_RFU)                           # Else, keep same
)



#Another way to get rid of data that might be errors is to rule out data that are outside a certain
#number of IQRs from the first and third quartile of the spread of data by site and by month (i.e. outliers)


# Calculate seasonal statistics for each farm to get as tight of data groupings as possible
exo_data$month <- month(exo_data$Date)  # Extract month

seasonal_stats <- exo_data %>%
  group_by(Site, month) %>%
  summarize(across(where(is.numeric), list(
    mean = mean,
    sd = sd,
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop")  # Ensure grouped data is dropped after summarizing

# Join seasonal stats back to the main data
exo_data <- exo_data %>%
  left_join(seasonal_stats, by = c("Site", "month"))


# Loop over numeric columns to calculate IQR and flag outliers
for (variable in variables) {
  Q1_col <- paste0(variable, "_Q1")  # Corresponding Q1 column
  Q3_col <- paste0(variable, "_Q3")  # Corresponding Q3 column
  
  # Check if Q1 and Q3 exist for the column
  if (Q1_col %in% names(exo_data) & Q3_col %in% names(exo_data)) {
    IQR_col <- paste0("IQR_", variable)
    exo_data[[IQR_col]] <- exo_data[[Q3_col]] - exo_data[[Q1_col]]  # Compute IQR
    
    # Replace outlier values in the variable column with NA
    exo_data[[variable]] <- ifelse(
      exo_data[[variable]] < (exo_data[[Q1_col]] - 4 * exo_data[[IQR_col]]) |
        exo_data[[variable]] > (exo_data[[Q3_col]] + 4 * exo_data[[IQR_col]]),
      NA, exo_data[[variable]]
    )
  }
}

#Now that we have excluded serious outliers, we can graphically examine the data again

#Create a new folder within outputs to store these plots
exo_clean_plots <- file.path(dir.outputs, "EXO_clean_plots")
if (!dir.exists(exo_clean_plots)) {
  dir.create(exo_clean_plots)
}


for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(exo_clean_plots, paste0("Clean_Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for the current variable
  for (Site in unique(exo_data$Site)) {
    # Filter data for the current site
    site_data <- exo_data[exo_data$Site == Site, ]
    
    # Create the time series plot
    p <- ggplot(site_data, aes(x = Date, y = .data[[variable]])) +
      geom_line(color = "blue") +
      labs(
        title = paste("Time Series for", variable, "at", Site),
        x = "Date",
        y = variable
      ) +
      scale_x_date(
        limits = c(as.Date("2023-10-01"), as.Date("2024-12-31")), # Set range from October 2023 to December 2024
        date_breaks = "2 months",                                 # Optional: Breaks every 2 months
        date_labels = "%b %Y"                                     # Format: "Oct 2023", "Dec 2023", etc.
      )+
      theme_minimal()
    
    # Print the plot to the PDF
    print(p)
  }
  
  # Close the PDF file
  dev.off()
}

#Get rid of QC columns
exo_data <- exo_data[,c(1:21)]

#Create a csv file of the cleaned data for further review
write.csv(exo_data, file.path(dir.csv, "EXO_I_clean.csv"), row.names = FALSE, fileEncoding = "UTF-8")
