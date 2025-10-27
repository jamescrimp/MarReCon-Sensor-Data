
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
dir.outputs <-file.path(wd, "James MarRecon Code/outputs")
dir.csv <- file.path(wd, "James MarRecon Code/outputs/CSVs")

# Import hobo data

hobo_data <- read.csv(file.path(dir.csv, "hobo_data.csv"), header = TRUE)
hobo_data$time <-mdy_hm(hobo_data$time, tz = "UTC")
hobo_data$Date <- as.Date(hobo_data$time)


#Initial Conductivity QAQC
#Exclude values where conductivity < 10,000 (??). This indicates that the hobo was out out the water,
#likely while getting services/data uploaded.

# hobo_data <- hobo_data %>%filter(Cond_uS.cm > 10000)


#Now lets look at the time series of the variables we are interested in to see if there are still
#signs of data errors

# Denote variables of interest
variables <- names(hobo_data)[sapply(hobo_data, is.numeric)]  

#Create a time series of the raw data from variables of interest for each farm

site_names <- unique(hobo_data$site)

#Create a new folder within outputs to store these plots
hobo_raw_plots <- file.path(dir.outputs, "hobo_raw_plots")
if (!dir.exists(hobo_raw_plots)) {
  dir.create(hobo_raw_plots)
}

for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(hobo_raw_plots, paste0("Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for the current variable
  for (site in unique(hobo_data$site)) {
    # Filter data for the current site
    site_data <- hobo_data[hobo_data$site == site, ]
    
    # Create the time series plot
    p <- ggplot(site_data, aes(x = Date, y = .data[[variable]])) +
      geom_line(color = "blue") +
      labs(
        title = paste("Time Series for", variable, "at", site),
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



#One way to get rid of data that might be errors is to rule out data that are outside a certain
#number of IQRs from the first and third quartile of the spread of data by site and by month (i.e. outliers)


# Calculate seasonal statistics for each farm to get as tight of data groupings as possible
hobo_data$month <- month(hobo_data$Date)  # Extract month

seasonal_stats <- hobo_data %>%
  group_by(site, month) %>%
  summarize(across(where(is.numeric), list(
    mean = mean,
    sd = sd,
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop")  # Ensure grouped data is dropped after summarizing

# Join seasonal stats back to the main data
hobo_data <- hobo_data %>%
  left_join(seasonal_stats, by = c("site", "month"))


# Loop over numeric columns to calculate IQR and flag outliers
for (variable in variables) {
  Q1_col <- paste0(variable, "_Q1")  # Corresponding Q1 column
  Q3_col <- paste0(variable, "_Q3")  # Corresponding Q3 column
  
  # Check if Q1 and Q3 exist for the column
  if (Q1_col %in% names(hobo_data) & Q3_col %in% names(hobo_data)) {
    IQR_col <- paste0("IQR_", variable)
    hobo_data[[IQR_col]] <- hobo_data[[Q3_col]] - hobo_data[[Q1_col]]  # Compute IQR
    
    # Replace outlier values in the variable column with NA
    hobo_data[[variable]] <- ifelse(
      hobo_data[[variable]] < (hobo_data[[Q1_col]] - 2 * hobo_data[[IQR_col]]) |
        hobo_data[[variable]] > (hobo_data[[Q3_col]] + 2 * hobo_data[[IQR_col]]),
      NA, hobo_data[[variable]]
    )
  }
}

#Now that we have excluded serious outliers, we can graphically examine the data again

#Create a new folder within outputs to store these plots
hobo_clean_plots <- file.path(dir.outputs, "hobo_clean_plots")
if (!dir.exists(hobo_clean_plots)) {
  dir.create(hobo_clean_plots)
}


for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(hobo_clean_plots, paste0("Clean_Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for the current variable
  for (site in unique(hobo_data$site)) {
    # Filter data for the current site
    site_data <- hobo_data[hobo_data$site == site, ]
    
    # Create the time series plot
    p <- ggplot(site_data, aes(x = Date, y = .data[[variable]])) +
      geom_line(color = "blue") +
      labs(
        title = paste("Time Series for", variable, "at", site),
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



#It seems like AOF1 was left on after being taken out of the water for an extended time. We can filter
#data when then temp is above 18 as this obviously is above the trend

hobo_data <- hobo_data %>% filter(!(site == "AOF1" & Temp > 18))

#Rerun the plots

hobo_clean_plots <- file.path(dir.outputs, "hobo_clean_plots")
if (!dir.exists(hobo_clean_plots)) {
  dir.create(hobo_clean_plots)
}


for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(hobo_clean_plots, paste0("Clean_Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for the current variable
  for (site in unique(hobo_data$site)) {
    # Filter data for the current site
    site_data <- hobo_data[hobo_data$site == site, ]
    
    # Create the time series plot
    p <- ggplot(site_data, aes(x = Date, y = .data[[variable]])) +
      geom_line(color = "blue") +
      labs(
        title = paste("Time Series for", variable, "at", site),
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
hobo_data <- hobo_data[,c(1:8)]

#Create a csv file of the cleaned data for further review
write.csv(hobo_data, file.path(dir.csv, "hobo_clean.csv"), row.names = FALSE, fileEncoding = "UTF-8")
