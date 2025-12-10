
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
library(purrr)
library(stringr)
library(zoo)
library(purrr)


#Save working directory path as an object
wd <- getwd()

#Create paths for data and outputs
dir.csv <- file.path(wd, "CSVs")  
dir.outputs <-file.path(wd, "Outputs")

# Import TCM data

tcm_data <- read.csv(file.path(dir.csv, "TCM_data.csv"), header = TRUE)
tcm_data$Time_UTC <- as.POSIXct(tcm_data$Time_UTC, format = "%Y-%m-%d %H:%M:%S")
tcm_data$Date <- as.Date(tcm_data$Time_UTC)


#Now lets look at the time series of the variables we are interested in to see if there are still
#signs of data errors

# Denote variables of interest
variables <- "Speed_cm.s"

#Create a time series of the raw data from variables of interest for each farm

site_names <- unique(tcm_data$site)

#Create a new folder within outputs to store these plots
tcm_raw_plots <- file.path(dir.outputs, "tcm_raw_plots")
if (!dir.exists(tcm_raw_plots)) {
  dir.create(tcm_raw_plots)
}

for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(tcm_raw_plots, paste0("Time_Series_", variable, ".pdf")), width = 8, height = 6)

  # Create a plot for each site for the current variable
  for (site in unique(tcm_data$site)) {
    # Filter data for the current site
    site_data <- tcm_data[tcm_data$site == site, ]

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
# 
# 

#We can see that there are large chunks of time at the start and ends of each deployment where the TCM sensors are
#out of the water. We need to detect these time periods and trim them. We do this by creating rules that:
#-Keep only the part of the record where Speed_cm_s < threshold for N consecutive samples.
#-Everything before that is “pre-deployment”.
#-Everything after the last such block is “post-recovery”.

#Create a Trim function
trim_tcm <- function(df,
                     speed_col = "Speed_cm.s",
                     threshold = 60,
                     window = 50) {
  
  # Identify where speed is below threshold
  good <- df[[speed_col]] < threshold
  
  r <- rle(good)
  
  # Find runs long enough
  long_runs <- which(r$values & r$lengths >= window)
  if (length(long_runs) == 0) return(df)   # nothing to trim, return original
  
  run_starts <- cumsum(c(1, head(r$lengths, -1)))
  
  start_idx <- run_starts[ long_runs[1] ]
  
  last_run <- long_runs[length(long_runs)]
  end_idx <- run_starts[last_run] + r$lengths[last_run] - 1
  
  df[start_idx:end_idx, ]
}


tcm_trimmed <- tcm_data %>%
  group_by(source_file) %>%
  group_modify(~ trim_tcm(.x)) %>%
  ungroup()



nrow(tcm_data)
nrow(tcm_trimmed)

#Now lets take a look at the graphs again

# #Create a new folder within outputs to store these plots
tcm_trimmed_plots <- file.path(dir.outputs, "tcm_trimmed_plots")
if (!dir.exists(tcm_trimmed_plots)) {
  dir.create(tcm_trimmed_plots)
}
#
#
for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(tcm_trimmed_plots, paste0("Clean_Time_Series_", variable, ".pdf")), width = 8, height = 6)

  # Create a plot for each site for the current variable
  for (site in unique(tcm_trimmed$site)) {
    # Filter data for the current site
    site_data <- tcm_trimmed[tcm_trimmed$site == site, ]

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


#The data at the start and ends of deployments is trimmed, but there are still some outliers in the middle of the
#deployments. Could be that the sensors got alterred by some structural issue at the farm, who knows. Lets try getting
#rid of these by using IQR to define outliers. 

remove_iqr <- function(df, speed_col = "Speed_cm.s") {
  
  Q1 <- quantile(df[[speed_col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[speed_col]], 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  
  df %>% filter(df[[speed_col]] > Q1 - 40*IQRv,
                df[[speed_col]] < Q3 + 40*IQRv)
}


tcm_clean <- remove_iqr(tcm_trimmed)


tcm_clean_plots <- file.path(dir.outputs, "tcm_clean_plots")
if (!dir.exists(tcm_clean_plots)) {
  dir.create(tcm_clean_plots)
}
#
#
for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(tcm_clean_plots, paste0("Clean_Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for the current variable
  for (site in unique(tcm_clean$site)) {
    # Filter data for the current site
    site_data <- tcm_clean[tcm_clean$site == site, ]
    
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

#Looks like we've cleared major outliers while also retaining data that looks plausible. We'll call it a wrap for QAQC.


#Get rid of QC columns
# tcm_data <- tcm_data[,c(1:5,8,9,10,11,12)]

#Create a csv file of the cleaned data for further review
write.csv(tcm_clean, file.path(dir.csv, "TCM_clean.csv"), row.names = FALSE, fileEncoding = "UTF-8")

