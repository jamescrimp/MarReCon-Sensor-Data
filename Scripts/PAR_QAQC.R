
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


#Save working directory path as an object
wd <- getwd()

#Create paths for data and outputs
dir.outputs <-file.path(wd, "Outputs")
dir.csv <- file.path(wd, "CSVs")

# Import par data

par_data <- read.csv(file.path(dir.csv, "par_data.csv"), header = TRUE)
par_data$Time_UTC <-as.POSIXct(par_data$Time_UTC, format = "%Y-%m-%d %H:%M:%S")
par_data$Date <- as.Date(par_data$Time_UTC)
par_data$Year <- year(par_data$Date)


#Check to make sure there are no NA values- some in wiper pos but thats ok 
na_counts<- par_data %>%
  summarise_all(~ sum(is.na(.)))

print(na_counts)

#There are 13 NAs without a time or date, so we will exclude these

par_data <- na.omit(par_data)



# Sensor Specs ------------------------------------------------------------

#Remove points that are outside of PAR sensor specs (O and 3,000 μmol s-1 m-2)

par_data <- par_data[par_data$PAR <= 3000 & par_data$PAR >= 0 ,]





# Inspect data ------------------------------------------------------------


# Denote variables of interest
variables <- c("Temp_C", "PAR") 

#Create a time series of the raw data from variables of interest for each farm

Site_names <- unique(par_data$Site)

#Create a new folder within outputs to store these plots
par_raw_plots <- file.path(dir.outputs, "par_raw_plots")
if (!dir.exists(par_raw_plots)) {
  dir.create(par_raw_plots)
}

#Create lines between deployments
deployment_bounds <- par_data %>%
  arrange(Site, Date) %>%
  group_by(Site, Filepath) %>%
  summarise(
    start_date = min(Date, na.rm = TRUE),
    end_date   = max(Date, na.rm = TRUE),
    .groups = "drop"
  )

for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(par_raw_plots, paste0("Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each Site for the current variable
  for (Site in unique(par_data$Site)) {
    # Filter data for the current Site
    Site_data <- par_data[par_data$Site == Site, ]
    
    # Create the time series plot
    p <- ggplot(Site_data, aes(x = Date, y = .data[[variable]])) +
      geom_point(color = "blue") +
      
      # Deployment START (green)
      geom_vline(
        data = deployment_bounds[deployment_bounds$Site == Site, ],
        aes(xintercept = start_date),
        color = "darkgreen",
        linetype = "dashed",
        linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      
      # Deployment END (red)
      geom_vline(
        data = deployment_bounds[deployment_bounds$Site == Site, ],
        aes(xintercept = end_date),
        color = "red",
        linetype = "dashed",
        linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      
      labs(
        title = paste("Time Series for", variable, "at", Site),
        x = "Date",
        y = variable
      ) +
      scale_x_date(
        limits = c(as.Date("2023-10-01"), as.Date("2024-12-31")),
        date_breaks = "2 months",
        date_labels = "%b %Y"
      ) +
      theme_minimal()
    
    # Print the plot to the PDF
    print(p)
  }
  
  # Close the PDF file
  dev.off()
}



# Trimming off deployment time --------------------------------------------


#It makes sense to trim the start and end of each deployment a bit to account for time when sensors are out of the water
#We'll do this by two days, since sensors generally weren't turned on or not turned off outside of this window

trim_par <- function(df, time_col = "Time_UTC", days_trim = 4) {
  
  df[[time_col]] <- as.POSIXct(df[[time_col]])
  
  start_time <- min(df[[time_col]], na.rm = TRUE) + days(days_trim)
  end_time   <- max(df[[time_col]], na.rm = TRUE) - days(days_trim)
  
  df[df[[time_col]] >= start_time & df[[time_col]] <= end_time, ]
}

par_trimmed <- par_data %>%
  group_by(Filepath) %>%
  group_modify(~ trim_par(.x)) %>%
  ungroup()


nrow(par_data)
nrow(par_trimmed)



# Trimming outliers by SD -------------------------------------------------

#Calculate SD for each variable
sd_par <- par_trimmed %>%
  dplyr::group_by(Site, Year) %>%
  dplyr::summarise(across(all_of(variables), ~ sd(., na.rm = TRUE), .names = "SD_{.col}"),
                   .groups = "drop")

#Join SD data back to RAW data in new df
par_trimmed_yr <- left_join(par_trimmed, sd_par, by = c("Site", "Year"))



#Flag outliers if they are > 3 * SD from previous point 
# Initialize outlier columns
for (variable in variables) {
  par_trimmed_yr[[paste0("outlier_", variable)]] <- 0
}

# Loop through rows
for (i in 2:nrow(par_trimmed_yr)) {
  
  site_curr  <- par_trimmed_yr$Site[i]
  site_prev  <- par_trimmed_yr$Site[i - 1]
  Year_curr  <- par_trimmed_yr$Year[i]
  Year_prev  <- par_trimmed_yr$Year[i - 1]
  
  # First: Check that nothing is NA
  if (!is.na(site_curr) && !is.na(site_prev) &&
      !is.na(Year_curr) && !is.na(Year_prev)) {
    
    # Second: Check if Site and Year match between current and previous row
    if (site_curr == site_prev && Year_curr == Year_prev) {
      
      for (variable in variables) {
        val_curr <- par_trimmed_yr[[variable]][i]
        val_prev <- par_trimmed_yr[[variable]][i - 1]
        sd_val   <- par_trimmed_yr[[paste0("SD_", variable)]][i]
        
        if (!is.na(val_curr) && !is.na(val_prev) && !is.na(sd_val)) {
          if (abs(val_curr - val_prev) > (3 * sd_val)) {
            par_trimmed_yr[[paste0("outlier_", variable)]][i] <- 1
          }
        }
      }
    }
  }
}


#___________________________________________________________________________-
#Look and see if there are any visible outliers based on time series graphs 
#Make a plot for each region, and each parameter w/outliers


for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(par_raw_plots, paste0("Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each Site for the current variable
  for (Site in unique(par_trimmed_yr$Site)) {
    # Filter data for the current Site
    Site_data <- par_trimmed_yr[par_trimmed_yr$Site == Site, ]
    
    # Create the time series plot
    p <- ggplot(Site_data,
                aes(x = Date, y = .data[[variable]], color = factor(outlier_PAR)), alpha = 0.6) +
      geom_point() +
      
      
      # Deployment START (green)
      geom_vline(
        data = deployment_bounds[deployment_bounds$Site == Site, ],
        aes(xintercept = start_date),
        color = "darkgreen",
        linetype = "dashed",
        linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      
      # Deployment END (red)
      geom_vline(
        data = deployment_bounds[deployment_bounds$Site == Site, ],
        aes(xintercept = end_date),
        color = "red",
        linetype = "dashed",
        linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      
      labs(
        title = paste("Time Series for", variable, "at", Site),
        x = "Date",
        y = variable
      ) +
      scale_x_date(
        limits = c(as.Date("2023-10-01"), as.Date("2024-12-31")),
        date_breaks = "2 months",
        date_labels = "%b %Y"
      ) +
      theme_minimal()
    
    # Print the plot to the PDF
    print(p)
  }
  
  # Close the PDF file
  dev.off()
}


###REMOVE OUTLIERS##
###
#Make values more than the 3 * SD from previous point NA

#One way to get rid of data that might be errors is to rule out data that are outside a certain
#number of IQRs from the first and third quartile of the spread of data by Site and by month (i.e. outliers)


# Calculate seasonal statistics for each farm to get as tight of data groupings as possible
par_data$month <- month(par_data$Date)  # Extract month

seasonal_stats <- par_data %>%
  group_by(Site, month) %>%
  summarize(across(where(is.numeric), list(
    mean = mean,
    sd = sd,
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
  ), .names = "{.col}_{.fn}"), .groups = "drop")  # Ensure grouped data is dropped after summarizing

# Join seasonal stats back to the main data
par_data <- par_data %>%
  left_join(seasonal_stats, by = c("Site", "month"))


# Loop over numeric columns to calculate IQR and flag outliers
for (variable in variables) {
  Q1_col <- paste0(variable, "_Q1")  # Corresponding Q1 column
  Q3_col <- paste0(variable, "_Q3")  # Corresponding Q3 column
  
  # Check if Q1 and Q3 exist for the column
  if (Q1_col %in% names(par_data) & Q3_col %in% names(par_data)) {
    IQR_col <- paste0("IQR_", variable)
    par_data[[IQR_col]] <- par_data[[Q3_col]] - par_data[[Q1_col]]  # Compute IQR
    
    # Replace outlier values in the variable column with NA
    par_data[[variable]] <- ifelse(
      par_data[[variable]] < (par_data[[Q1_col]] - 2 * par_data[[IQR_col]]) |
        par_data[[variable]] > (par_data[[Q3_col]] + 2 * par_data[[IQR_col]]),
      NA, par_data[[variable]]
    )
  }
}



# Reexamine data ----------------------------------------------------------




#Now lets look at the graphs again

#Now that we have excluded serious outliers, we can graphically examine the data again

#Create a new folder within outputs to store these plots
par_trimmed_plots <- file.path(dir.outputs, "par_trimmed_plots")
if (!dir.exists(par_trimmed_plots)) {
  dir.create(par_trimmed_plots)
}


for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(par_trimmed_plots, paste0("Trimmed_Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each Site for the current variable
  for (Site in unique(par_data$Site)) {
    # Filter data for the current Site
    Site_data <- par_data[par_data$Site == Site, ]
    
    # Create the time series plot
    p <- ggplot(Site_data, aes(x = Date, y = .data[[variable]])) +
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


#Graphs of the raw data show numerous distinct outliers and maybe some instruments that need calibration.




#Now that we have excluded serious outliers, we can graphically examine the data again

#Create a new folder within outputs to store these plots
par_clean_plots <- file.path(dir.outputs, "par_clean_plots")
if (!dir.exists(par_clean_plots)) {
  dir.create(par_clean_plots)
}


for (variable in variables) {
  # Specify the PDF file path for the current variable
  pdf(file = file.path(par_clean_plots, paste0("Clean_Time_Series_", variable, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each Site for the current variable
  for (Site in unique(par_data$Site)) {
    # Filter data for the current Site
    Site_data <- par_data[par_data$Site == Site, ]
    
    # Create the time series plot
    p <- ggplot(Site_data, aes(x = Date, y = .data[[variable]])) +
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


#Rerun the plots

par_clean_plots <- file.path(dir.outputs, "par_clean_plots")
if (!dir.exists(par_clean_plots)) {
  dir.create(par_clean_plots)
}



#Get rid of QC columns
par_data <- par_data[,c(1:12)]

#Create a csv file of the cleaned data for further review
write.csv(par_data, file.path(dir.csv, "par_clean.csv"), row.names = FALSE, fileEncoding = "UTF-8")
