##Use this script to upload raw insitu CSVs from the shared drive and perform QAQC! 
#Updated: 5/6/2026 
#Sierra Greene

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

#Set Working Directory 
setwd("C:/MarRecon_code/thesis_work")

#Save working directory path as an object
wd <- getwd()

#Pull drectly from MarRecon shared drive 
dir.data <- file.path("H:/Shared drives/Mariculture ReCon/Data Management/Raw data from sensors/EXO_INSITU")


# Get all CSV files 
# If a CSV file doesn't load correctly, open the file and "save.as" a CSV again
csv_files <- list.files(path = dir.data, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Create an empty list
data_list <- list()

# Read and store each CSV
for (file in csv_files) {
  df <- read_csv(file, skip = 9, col_names = FALSE)
  df$path <- tools::file_path_sans_ext(basename(file))  # Add source info
  data_list[[file]] <- df
}

# Combine into one data frame
combined_df <- bind_rows(data_list, .id = "file_path")

exo_dataI <- combined_df
#exo_dataI is all insitu exo data combined 
str(exo_dataI)

# Get rid of column 3 and 22 (site name from file and filepath)
#exo_dataI <- exo_dataI[,c(-3, -13, -14, -21)]
exo_dataI <- exo_dataI[,-c(1,23)]

# Add Column labels
colnames(exo_dataI) <- c("Date", 
                         "Time_UTC", 
                         "Site",
                         "Temp_C", 
                         "Cond_uS.cm", 
                         "SpCond_uS.cm", 
                         "TDS_mg.L",
                         "Sal_PSU", 
                         "NLF_conductivity_uS.cm", 
                         "Depth_M",
                         "Vertical_position_M", 
                         "Pressure_PSIA", 
                         "TAL_PE_RFU", 
                         "TAL_PE_ug.L", 
                         "Chlorophyll_RFU", 
                         "Chlorophyll_ug.L", 
                         "ODO_sat", 
                         "ODO_mg.L", 
                         "Turbidity_FNU",
                         "Cable_Pwr_V", 
                         "Wiper_POS")


#remove rows where all data is NA
exo_dataI <- exo_dataI[rowSums(is.na(exo_dataI)) != ncol(exo_dataI), ]

#Make sure all sites are accounted for- if this doesnt look right check CSVs
unique(exo_dataI$Site)

#Add columns for lat and long
exo_dataI$Latitude <- NA
exo_dataI$Longitude <- NA

#Add values to lat/long corresponding to coordinates of in situ EXO 2s at each site
latitude_values <- c(
  AOF1 = 57.65773, 
  KOB1 = 57.53318,
  KIS1 = 57.76711,
  SSF1 = 59.46044,
  MIO1 = 59.57137,
  BCF1 = 59.46797,
  ROK1 = 60.56271,
  SBO1 = 60.65693,
  SBR1 = 60.63697
)

longitude_values <- c(
  AOF1 = -152.41992,
  KOB1 = -154.02696,
  KIS1 = -152.41043,
  SSF1 = -151.5188,
  MIO1 = -151.27263,
  BCF1 = -151.51875,
  ROK1 = -145.96066,
  SBO1 = -145.89151,
  SBR1 = -146.00452
)

exo_dataI$Latitude <- latitude_values[exo_dataI$Site]
exo_dataI$Longitude <- longitude_values[exo_dataI$Site]

str(exo_dataI)


#Create a single datetime column
exo_dataI$Date <- mdy(exo_dataI$Date)
exo_dataI$Time_UTC <- hms(exo_dataI$Time_UTC)
exo_dataI$Time_UTC <- exo_dataI$Date + exo_dataI$Time_UTC

str(exo_dataI)


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

exo_dataI$Region <- region_values[exo_dataI$Site]

#Check to make sure there are no NA values- some in wiper pos but thats ok 
na_counts<- exo_dataI %>%
  summarise_all(~ sum(is.na(.)))

print(na_counts)

#Create a csv file for further review
#write.csv(exo_dataI, file.path(wd, "EXO_I_data_18NOV25.csv"), row.names = FALSE, fileEncoding = "UTF-8")


#rename our working df
exo_data <- exo_dataI

str(exo_data)

#Exclude values where conductivity < 10,000- This indicates that the exo was out out the water
exo_data <- exo_data %>%filter(Cond_uS.cm > 10000)
exo_data <- exo_data %>%filter(Depth_M > 1)

#Add month and year column
exo_data$Month <- month(exo_data$Date)  # Extract month
exo_data$Year <- year(exo_data$Date)  # Extract year

#Export Raw data for RWS
#2024
# EXO_data24 <- exo_data %>%
#   filter(Year =="2024")
# write.csv(EXO_data24, file.path(wd, "EXO_data24.csv"), row.names = FALSE, fileEncoding = "UTF-8")

#2025
# EXO_data25 <- exo_data %>%
#   filter(Year =="2025")
# write.csv(EXO_data25, file.path(wd, "EXO_data25.csv"), row.names = FALSE, fileEncoding = "UTF-8")


# Sensor specs ------------------------------------------------------------
#Remove points that are outside of optical sensor specs. 
# chl: 0 - 100 RFU, 0 - 400 ug/L, resolution: 0.01
# turb: 0 - 999 FNU, resolution: 0.3
# TAL PE: 0 - 100 RFU, 0-280 μg/L
# Oxygen: 500%, 50 mg/L

#Chl 
exo_data$Chlorophyll_RFU[exo_data$Chlorophyll_RFU <= 0 & 
                           exo_data$Chlorophyll_RFU >= -0.1] <- 0
exo_data$Chlorophyll_RFU[exo_data$Chlorophyll_RFU <= -0.11] <- NA
exo_data$Chlorophyll_RFU[exo_data$Chlorophyll_RFU >= 100] <- NA

exo_data$Chlorophyll_ug.L[exo_data$Chlorophyll_ug.L <= 0 & 
                           exo_data$Chlorophyll_ug.L >= -0.1] <- 0
exo_data$Chlorophyll_ug.L[exo_data$Chlorophyll_ug.L <= -0.11] <- NA
exo_data$Chlorophyll_ug.L[exo_data$Chlorophyll_ug.L >= 100] <- NA

#TAL PE
exo_data$TAL_PE_RFU[exo_data$TAL_PE_RFU <= 0 & 
                           exo_data$TAL_PE_RFU >= -0.1] <- 0
exo_data$TAL_PE_RFU[exo_data$TAL_PE_RFU <= -0.11] <- NA
exo_data$TAL_PE_RFU[exo_data$TAL_PE_RFU >= 100] <- NA

exo_data$TAL_PE_ug.L[exo_data$TAL_PE_ug.L <= 0 & 
                            exo_data$TAL_PE_ug.L >= -0.1] <- 0
exo_data$TAL_PE_ug.L[exo_data$TAL_PE_ug.L <= -0.11] <- NA
exo_data$TAL_PE_ug.L[exo_data$TAL_PE_ug.L >= 100] <- NA

#Turbidity- 124 is max we calibrate to 
exo_data$Turbidity_FNU[exo_data$Turbidity_FNU > 124] <- NA
exo_data$Turbidity_FNU[exo_data$Turbidity_FNU < -0.3] <- NA
exo_data$Turbidity_FNU[exo_data$Turbidity_FNU <= 0 & 
                         exo_data$Turbidity_FNU >= -0.3] <- 0

#Oxygen
exo_data$ODO_mg.L[exo_data$ODO_mg.L > 50] <- NA
exo_data$ODO_sat[exo_data$ODO_sat > 500] <- NA



#Remove points outside of biological expectations
# Salinity over 35 
exo_data$Sal_PSU[exo_data$Sal_PSU > 35] <- NA
#Conductivity over 50000- what we calibrate to
exo_data$Cond_uS.cm[exo_data$Cond_uS.cm > 50000] <- NA
#Specific Conductivity over 55000
exo_data$SpCond_uS.cm[exo_data$SpCond_uS.cm > 55000] <- NA


#save all exo data that has basic QAQC
exo_data_all <- exo_data

#Export ROK1 spring 2026 data for AK 
ROK1_sp26 <- exo_data %>%
    filter(Year =="2026", 
           Site == "ROK1")

ROK1_sp26 <- ROK1_sp26[,-c(7,9,11,12,13,14,20,21,22,23,24,25,26)]
#Export 

write.csv(ROK1_sp26, file.path(wd, "ROK1_sp26.csv"), row.names = FALSE, fileEncoding = "UTF-8")

#_____End of basic QAQC based on sensor specs___________________ 
#
#
#Calculate SD based on annual RAW values for the year- this is what VIMS does with these sensors. They use +/- 1 SD from previous point, but that seems to limiting. We will use +/- 3 SD  


# Start of advanced QAQC --------------------------------------------------
library(dplyr)

#Calculate site based monthly SD for each variable

vars <- names(exo_data)[sapply(exo_data, is.numeric) & !names(exo_data) %in% c("Year", "Site")]

sd_exo <- exo_data %>%
  dplyr::group_by(Year, Site) %>%
  dplyr::summarise(across(all_of(vars), 
                          ~ sd(., na.rm = TRUE), .names = "SD_{.col}"),
                           .groups = "drop")

#Join SD data back to RAW data in new df
exo_data_yr <- left_join(exo_data, sd_exo, 
                         by = c("Site", "Year"))

#exo_data_yr is our working df for raw data and stats

#Flag outliers if they are > 3 * SD from previous point 
# Initialize outlier columns
for (var in vars) {
  exo_data_yr[[paste0("outlier_", var)]] <- 0
}

# Loop through rows
for (i in 2:nrow(exo_data_yr)) {
  
  site_curr  <- exo_data_yr$Site[i]
  site_prev  <- exo_data_yr$Site[i - 1]
  year_curr  <- exo_data_yr$Year[i]
  year_prev  <- exo_data_yr$Year[i - 1]
  
  # First: Check that nothing is NA
  if (!is.na(site_curr) && !is.na(site_prev) &&
      !is.na(year_curr) && !is.na(year_prev)) {
    
    # Second: Check if Site and Year match between current and previous row
    if (site_curr == site_prev && year_curr == year_prev) {
      
      for (var in vars) {
        val_curr <- exo_data_yr[[var]][i]
        val_prev <- exo_data_yr[[var]][i - 1]
        sd_val   <- exo_data_yr[[paste0("SD_", var)]][i]
        
        if (!is.na(val_curr) && !is.na(val_prev) && !is.na(sd_val)) {
          if (abs(val_curr - val_prev) > (3 * sd_val)) {
            exo_data_yr[[paste0("outlier_", var)]][i] <- 1
          }
        }
      }
    }
  }
}

#look at df we created 
exo_data_yr

#___________________________________________________________________________-
#Look and see if there are any visible outliers based on time series graphs 
#Make a plot for each region, and each parameter w/outliers

# Check outliers visually -------------------------------------------------
#PWS 
#
pws_data <- exo_data_yr %>%
  filter(Region == "PWS")

#salinity- 
PWSsal <- ggplot(pws_data, aes(y = Sal_PSU, x = Date)) +
  geom_point(aes(color = factor(outlier_Sal_PSU)), alpha = 0.6) +
  labs(
    title = "PWS SAL",
    x = "Salinity",
    y = "Date"
  ) +
  aes(color=outlier_Sal_PSU) +
  scale_color_manual(values = c("0" = "green", "1" = "red"))+
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#Need to assess salinity at ROK1 at end of summer 2025- whacky sensor

#temp
PWStmp <- ggplot(pws_data, aes(y = Temp_C, x = Date)) +
  geom_point(aes(color = factor(outlier_Temp_C)), alpha = 0.6) +
  labs(
    title = "PWS tmp",
    x = "Temp",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red"))+
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#chl RFU
PWSchl <- ggplot(pws_data, aes(y = Chlorophyll_RFU, x = Date)) +
  geom_point(aes(color = factor(outlier_Chlorophyll_RFU)), alpha = 0.6) +
  labs(
    title = "PWS Chlorophyll RFU",
    x = "Date",
    y = "Chlorophyll RFU",
    color = "Outlier"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

  
#turbidity 
PWSturb <- ggplot(pws_data, aes(y = Turbidity_FNU, x = Date)) +
geom_point(aes(color = factor (outlier_Turbidity_FNU)), alpha = 0.6) +
  labs(
    title = "PWS turb",
    x = "Turbidity",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#Turbidity at SBO1 too high at end of April 2026

#DO %
#
PWSDOper <- ggplot(pws_data, aes(y = ODO_sat, x = Date)) +
  geom_point(aes(color = factor (outlier_ODO_sat)), alpha = 0.6) +
  labs(
    title = "PWS DO%",
    x = "DO %",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_cowplot()

#DO mg/L
PWSDOmgl <- ggplot(pws_data, aes(y = ODO_mg.L, x = Date)) +
  geom_point(aes(color = factor (outlier_ODO_mg.L)), alpha = 0.6) +
  labs(
    title = "PWS DO mg/L",
    x = "DO mg/L",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#
#Look further into:
#ROK1 summer 2025 salinty manually remove
#ROK1 summer 2025 DO mg/L- looks like there is a drop in DO mg/L but not %
#SBO1 april 2026 turbidity
#___________________KBY________________
#
kby_data <- exo_data_yr %>%
  filter(Region == "KBY")

#salinity
KBYsal <- ggplot(kby_data, aes(y = Sal_PSU, x = Date)) +
  geom_point(aes(color = factor (outlier_Sal_PSU)), alpha = 0.6) +
  labs(
    title = "KBY SAL",
    x = "Salinity",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#wonky BCF1 salinity in winter 2026 from bad sensor
#low sal drift at MIO1?

#temp
KBYtmp <- ggplot(kby_data, aes(y = Temp_C, x = Date)) +
  geom_point(aes(color = factor (outlier_Temp_C)), alpha = 0.6) +
  labs(
    title = "KBY tmp",
    x = "Temp",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red"))+
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#chl RFU
KBYchl <- ggplot(kby_data, aes(y = Chlorophyll_RFU, x = Date)) +
  geom_point(aes(color = factor(outlier_Chlorophyll_RFU)), alpha = 0.6) +
  labs(
    title = "KBY Chlorophyll RFU",
    x = "Date",
    y = "Chlorophyll RFU",
    color = "Outlier"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#Looks like there are outliers in SSF1

#turbidity 
KBYturb <- ggplot(kby_data, aes(y = Turbidity_FNU, x = Date)) +
  geom_point(aes(color = factor (outlier_Turbidity_FNU)), alpha = 0.6) +
  labs(
    title = "KBY turb",
    x = "Turbidity",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#Looks like there are outliers in SSF1, maybe MIO1
#DO %
#
KBYDOper <- ggplot(kby_data, aes(y = ODO_sat, x = Date)) +
  geom_point(aes(color = factor (outlier_ODO_sat)), alpha = 0.6) +
  labs(
    title = "kby DO%",
    x = "DO %",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#DO mg/L
KBYDOmgl <- ggplot(kby_data, aes(y = ODO_mg.L, x = Date)) +
  geom_point(aes(color = factor (outlier_ODO_mg.L)), alpha = 0.6) +
  labs(
    title = "KBY DO mg/L",
    x = "DO mg/L",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#BCF1 mg/L oxygen likley impacted by winky CT sensor- looks lower than it should
#
#____________________KOD_________________
kod_data <- exo_data_yr %>%
  filter(Region == "KOD")

#salinity
KODsal <- ggplot(kod_data, aes(y = Sal_PSU, x = Date)) +
  geom_point(aes(color = factor (outlier_Sal_PSU)), alpha = 0.6)+
  labs(
    title = "KOD SAL",
    x = "Salinity",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#KOB1 looks like its drifting low after Aug 2024- cut off after 6 mo?
#temp
KODtmp <- ggplot(kod_data, aes(y = Temp_C, x = Date)) +
  geom_point(aes(color = factor (outlier_Temp_C)), alpha = 0.6) +
  labs(
    title = "KOD tmp",
    x = "Temp",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#chl RFU
KODchl <- ggplot(kod_data, aes(y = Chlorophyll_RFU, x = Date)) +
  geom_point(aes(color = factor (outlier_Chlorophyll_RFU)), alpha = 0.6) +
  labs(
    title = "KOD chl rfu",
    x = "Chl",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#Possible outliers at AOF1

#turbidity 
KODturb <- ggplot(kod_data, aes(y = Turbidity_FNU, x = Date)) +
  geom_point(aes(color = factor (outlier_Turbidity_FNU)), alpha = 0.6) +
  labs(
    title = "KOD turb",
    x = "Turbidity",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()
#Spike at KIS1 likely due to sensor drift- remove month before calibration?
#Lets look a little closer into this
KIS1smturb <- exo_data_yr %>%
  filter(
    Site == "KIS1",
    month(Date) %in% c(7, 8)  # 7 = July, 8 = August
  ) %>%
  select(Date, Site, Turbidity_FNU)
#Add in a line when the sensor was swapped- looks like it drops off right after its swapped
KIS1turbsmr <- ggplot(KIS1smturb, aes(y = Turbidity_FNU, x = Date)) +
  geom_point(alpha = 0.6, color = "brown") +
  geom_vline(xintercept = as.Date("2024-08-09"), color = "red", linetype = "dashed", linewidth = 1) 

#DO %
#
KODDOper <- ggplot(kod_data, aes(y = ODO_sat, x = Date)) +
  geom_point(aes(color = factor (outlier_ODO_sat)), alpha = 0.6) +
  labs(
    title = "KOD DO%",
    x = "DO %",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#DO mg/L
KODDOmgl <- ggplot(kod_data, aes(y = ODO_mg.L, x = Date)) +
  geom_point(aes(color = factor (outlier_ODO_mg.L)), alpha = 0.6) +
  labs(
    title = "KOD DO mg/L",
    x = "DO mg/L",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#KBO1 mg/L doesnt necessarily look bad, but should be removed during salinity drift period

###REMOVE OUTLIERS##
###
#Make values more than the 3 * SD from previous point NA

# Outlier removal ---------------------------------------------------------
#Create new df to mess around with 
exo_data2 <- exo_data_yr

#Loop through and make NA if there is a 1 in each outlier_param column, make the 
#param point NA. 
for (var in vars) {
  outlier_var <- paste0("outlier_", var)
  exo_data2[[var]][exo_data2[[outlier_var]] == 1] <- NA
}


#take a look- original data w/ flaggedoutliers 
testchl <- ggplot(exo_data_yr, aes(y = Chlorophyll_RFU, x = Date)) +
  geom_point(aes(color = factor (outlier_Chlorophyll_RFU)), alpha = 0.6) +
  labs(
    title = "test chl rfu",
    x = "Chl",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

#Chl Data w/ outliers removed 
testchl2 <- ggplot(exo_data2, aes(y = Chlorophyll_RFU, x = Date)) +
  geom_point(aes(color = factor (outlier_Chlorophyll_RFU)), alpha = 0.6) +
  labs(
    title = "test chl rfu",
    x = "Chl",
    y = "Date"
  ) +
  scale_color_manual(values = c("0" = "green", "1" = "red")) +
  facet_wrap(~ Site, ncol = 1, scales = "free_y") +
  theme_minimal()

na_counts<- exo_data2 %>%
  summarise_all(~ sum(is.na(.)))

print(na_counts)

#At SSF1- there are only 4 points in very high RFU range with no build up to them
#These are likely still errors-lets remove them
exo_data2 <- exo_data2[
  !(exo_data2$Site == "SSF1" &      
      exo_data2$Chlorophyll_RFU > 30), ]

#Export CSV with outliers removed
#write.csv(exo_data2, file = "EXO_data_outrmv.csv", row.names = FALSE)

#
df <- exo_data2 
str(df)

#Remove columns we dont need 
##name them
drop <- c("TDS_mg.L", "NLF_conductivity_uS.cm", "Vertical_position_M",
          "Cable_Pwr_V", "Wiper_POS", "SD_NLF_conductivity_uS.cm", 
          "SD_Vertical_position_M", "SD_Pressure_PSIA", "SD_Cable_Pwr_V", "SD_Wiper_POS",
          "SD_Latitude", "SD_Longitude", "SD_TDS_mg.L", "TAL_PE_RFU", "TAL_PE_ug.L" )


#remove 'drop' headers from df
df3 <- df[,!(names(df)%in%drop)]
#Remove outlier stats and SD columns
df3 <- df3[, -c(20:53)]

str(df3)
df1 <- df3

#Add season column
df1 <- df1 %>%
  mutate(
    season = case_when(
      month(Date) %in% c(12, 1, 2)  ~ "Winter",
      month(Date) %in% c(3, 4, 5)   ~ "Spring",
      month(Date) %in% c(6, 7, 8)   ~ "Summer",
      month(Date) %in% c(9, 10, 11) ~ "Fall"
    )
  )

#Make headers cap
colnames(df1) <- toupper(colnames(df1))
str(df1) 
#df1 will be our working dataframe

#Create value of params we want to work with 
vars <- c("TEMP_C", "COND_US.CM", "SAL_PSU", "DEPTH_M",
          "CHLOROPHYLL_RFU", "CHLOROPHYLL_UG.L", "ODO_SAT",  
          "ODO_MG.L", "TURBIDITY_FNU")

# ------Manual adjustments/trim------------------
#make adjustments for visual QAQC errors noticed 
#
#make turbidity at KIS1 NA for dates when it looks like sensor drift affected data
#look visually
#Look closer at KIS1 in July and Aug
KIS1smturb <- df1 %>%
  dplyr::filter(
    SITE == "KIS1",
    month(DATE) %in% c(7, 8)  
  ) %>%
  select(DATE, SITE, TURBIDITY_FNU)
#Add in a line when the sensor was swapped- looks like it drops off right after its swapped
KIS1turbsmr <- ggplot(KIS1smturb, aes(y = TURBIDITY_FNU, x = DATE)) +
  geom_point(alpha = 0.6, color = "brown") +
  scale_x_date(
    date_breaks = "1 day",           
    date_labels = "%m %d"               
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2024-08-09"), color = "red", linetype = "dashed", linewidth = 1)
#looks like it started to drift up on 7/21 
#Make points 7/21 - 8/9 = NA
df1 <- df1 %>%
  mutate(TURBIDITY_FNU = ifelse(SITE == "KIS1" & DATE >= as.Date("2024-07-23") & DATE <= as.Date("2024-08-09"), NA, TURBIDITY_FNU))


#Salinity error at ROK1 in summer 2025
#view salinty at ROK1 in summer
ROK1smsal <- df1 %>%
  dplyr::filter(
    SITE == "ROK1",
    YEAR == "2025",
    month(DATE) %in% c(7)  
  ) %>%
  select(DATE, SITE, SAL_PSU, COND_US.CM, SPCOND_US.CM )

ROK1sum_sal <- ggplot(ROK1smsal, aes(y = COND_US.CM, x = DATE))+
  geom_point(alpha = 0.6, color = "lightblue") +
  scale_x_date(
    date_breaks = "1 day",           
    date_labels = "%m %d") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Salinity, cond, spcon data went wonky on 7/24 (7/23 last good day)
#Delete cond, sal, ODO mgL
df1 <- df1 %>%
  mutate(
    across(
      c(SPCOND_US.CM, COND_US.CM, SAL_PSU, ODO_MG.L),
      ~ ifelse(SITE == "ROK1" & 
                 DATE > as.Date("2025-07-23") & 
                 DATE < as.Date("2026-08-14"), NA, .x)
    )
  )



#Salinity drift at KOB1
KOB1 <- df1 %>%
  dplyr::filter(
    SITE == "KOB1")

#plot KOB1 salinity 
KOB1_sal <- ggplot(KOB1, aes(y = SAL_PSU, x = DATE))+
    geom_point(alpha = 0.6, color = "lightblue") +
    scale_x_date(
      date_breaks = "1 month",           
      date_labels = "%m %d %y") +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
#salinity drops after 8/2024- this is after 6 mo deployed
#Sensors went out on 2/10/2024, came back 6/4/2025
#Remove data after the 6 month mark because it is not reliable
#only salinity looks most impacted 

df1 <- df1 %>%
  mutate(
    across(
      c(SPCOND_US.CM, COND_US.CM, SAL_PSU, ODO_MG.L),
      ~ ifelse(SITE == "KOB1" & 
                 DATE > as.Date("2024-08-10") & 
                 DATE < as.Date("2025-06-04"), NA, .x)
    )
  )

#turbidty at SBO1 in the spring 2026
SBO1spturb <- df1 %>%
  dplyr::filter(
    SITE == "SBO1",
    YEAR == "2026",
    month(DATE) %in% c(3, 4)  
  ) %>%
  select(DATE, SITE, TURBIDITY_FNU )

SBO1spturb <- ggplot(SBO1spturb, aes(y = TURBIDITY_FNU, x = DATE))+
  geom_point(alpha = 0.6, color = "lightblue") +
  scale_x_date(
    date_breaks = "1 day",           
    date_labels = "%m %d") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#This step is messing up turbidity values for some reason!!
#turb spikes on 4/16- remove 4/15 - 4/17
df1 <- df1 %>%
  mutate(TURBIDITY_FNU = ifelse(SITE == "SBO1" & 
                                  DATE > as.Date("2026-04-15") & 
                                  DATE < as.Date("2026-04-17"), NA, TURBIDITY_FNU))
                                                      


#BCF1-salinity
BCF1wint <- df1 %>%
  dplyr::filter(
    SITE == "BCF1",
    DATE >= as.Date("2025-11-01") & DATE <= as.Date("2026-04-30")
  ) %>%
  select(DATE, SITE, SAL_PSU, COND_US.CM, SPCOND_US.CM, ODO_MG.L)

BCF1sal <- ggplot(BCF1wint, aes(y = SAL_PSU, x = DATE))+
  geom_point(alpha = 0.6, color = "lightblue") +
  scale_x_date(
    limits = as.Date(c("2025-11-01", "2025-11-15")),
    date_breaks = "1 day",           
    date_labels = "%m %d") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Looks like data is wonky after 11/06-remove all data after this
df1 <- df1 %>%
  mutate(
    across(
      c(SPCOND_US.CM, COND_US.CM, SAL_PSU, ODO_MG.L),
      ~ ifelse(SITE == "BCF1" & 
                 DATE > as.Date("2025-11-06") & 
                 DATE < as.Date("2026-04-06"), NA, .x)
    )
  )

#remove rows where all values are NA
df1 <- df1 |> filter(!if_all(everything(), is.na))

#save df before interpolation
df_QAQC <- df1

#export df before interpolation
#write.csv(df1, file = "df1_26MAY2026SG.csv", row.names = FALSE)

#Break up by year 
#2023\
#2024
#2025
#site, Time_UTC, lat, long, salinity, temp, chl (ug/L), chl (RFU), turbidity (FNU), depth

df_QAQC <-  df_QAQC %>%
  select(DATE, TIME_UTC, SITE, DEPTH_M, TEMP_C, SAL_PSU, COND_US.CM, CHLOROPHYLL_RFU, CHLOROPHYLL_UG.L, TURBIDITY_FNU, ODO_SAT, ODO_MG.L, LATITUDE, LONGITUDE, REGION, YEAR)

df1_2023<- df_QAQC %>%
  dplyr::filter(YEAR=="2023")

df1_2024<- df_QAQC %>%
  dplyr::filter(YEAR=="2024")

df1_2025<- df_QAQC %>%
  dplyr::filter(YEAR=="2025")



# #Export
write.csv(df1_2023, file.path(wd, "df1_2023.csv"), row.names = FALSE, fileEncoding = "UTF-8")

write.csv(df1_2024, file.path(wd, "df1_2024.csv"), row.names = FALSE, fileEncoding = "UTF-8")

write.csv(df1_2025, file.path(wd, "df1_2025.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Imputation for NA values ------------------------------------------------
#weighted average imputation
library(imputeTS)

# Ensure the data is sorted correctly
df1 <- df1 %>%
  arrange(SITE, TIME_UTC)

#remove rows where all data is NA
df1 <- df1[rowSums(is.na(df1)) != ncol(df1), ]

# Loop through variables to create imputed versions
for (v in vars) {
  df1[[v]] <- df1 %>%
    arrange(SITE, TIME_UTC) %>%
    group_by(SITE) %>%
    mutate(temp_impute = na_ma(.data[[v]], 
                               k = 24, #use 24 hours before and after NA pt
                               weighting = "exponential", 
                               maxgap = 336), #dont impute if data gap > 2 wks
           !!v := if_else(is.na(.data[[v]]), temp_impute, .data[[v]])) %>%
    pull(!!sym(v))
}

df_interpolated <- df1

#check to see how many NA are left 
na_counts <- df1 %>%
  summarise_all(~ sum(is.na(.)))

print(na_counts)
#still lots of NA for chl RFU, all others OK 

#export df as csv- this is df that has large outlier removed, and resulting NAs replacedn with imputed values Move to discussion and site thesis

write.csv(df_interpolated, file = "df_interpolated_11MAY26_SG.csv", row.names = FALSE)


# RWS export csvs ---------------------------------------------------------
#2023 data QAQC for RWS
#df_QAQC = dataframe w/ QAQC but no interpolation
df_2023 <- df_QAQC %>%
  filter(YEAR =="2023")ow.names = FALSE)



