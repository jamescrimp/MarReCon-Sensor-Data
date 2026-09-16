#Use this script to look at EXO data ASAP after pulling from the field
#DEFINITIELY  do this before deploying any of the sensors again!
#this script is to check that all sensors are working correctly NOT to perform QAQC 

#updated 3/20/26 SG

#libraries
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
library(zoo)
library(oce)

#Set working directory 
#setwd("I:\\Shared drives\\Mariculture ReCon\\Data Management\\Raw data from sensors\\EXO_INSITU")

#read csv files that you just offloaded. change:
# -farm folder  
# -farm name csv 
# so that this is the farm name you need 



# Add data file -----------------------------------------------------------
#Sierra path
#Note: if csv doesnt read in, open it and 'save as' then rerun
test <- read_csv("I:/Shared drives/Mariculture ReCon/Data/Sensor Data Management/Raw data from sensors/EXO_INSITU/KIS1/RAW_EXO_I_KOD_KIS1_9APR26.csv", skip = 9, col_names = FALSE)

#Combine multiple files if needed here
#Name the combined df "test" still and the rest of the code will still run 

# Add Column labels
colnames(test) <- c("Date", 
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

#Create a single datetime column
test$Date <- mdy(test$Date)
test$Time_UTC <- hms(test$Time_UTC)
test$Time_UTC <- test$Date + test$Time_UTC



# Step 1: Dates ------------------------------------------------------------------
#Look at the first and last dates that data was recorded
#Do they match what is supposed to be there?
range(test$Date, na.rm = TRUE)



# Step 2: In water only ------------------------------------------------------------------
#remove values recorded out of water- values where conductivity < 10,000
test <- test %>%filter(Cond_uS.cm > 10000)
test <- test %>%filter(Depth_M > 1)


# Step 3: Ranges ----------------------------------------------------------
#look at ranges for each paramter we care about to see if any fall our of what we are expecting 
#salinity (15 - 35)
range(test$Sal_PSU, na.rm = TRUE)
#Conductivity (2000-5000)
range(test$Cond_uS.cm, na.rm = TRUE)
#temperaure (0 - 20)
range(test$Temp_C, na.rm = TRUE)
#oxygen mg/L (5-15)
range(test$ODO_mg.L, na.rm = TRUE)
#oxygen % (80-150)
range(test$ODO_sat, na.rm = TRUE)
#turbidity (0-20)
range(test$Turbidity_FNU, na.rm = TRUE)
#chlorophyl (0-30)
range(test$Chlorophyll_RFU, na.rm = TRUE)
#chlorophyl ug/L (0-30)
range(test$Chlorophyll_ug.L, na.rm = TRUE)
#Battery pack V (11-13)
range(test$Cable_Pwr_V, na.rm = TRUE)
#Depth(2-5)
range(test$Depth_M, na.rm = TRUE)

##Notes so far
# Chl RFU too hi


# Step 4: Plot ------------------------------------------------------------
#Plot data to look for weird stuff
#use 'facet_wrap(~ Site, ncol = 1, scales = "free_y")'if there are multiple sites 
salinity <- ggplot(test, aes(y = Sal_PSU, x = Date)) +
  geom_point() +
  labs(
    title = "Salnity",
    x = "Salinity",
    y = "Date") +
  theme_minimal()

conductivity <- ggplot(test, aes(y = Cond_uS.cm, x = Date)) +
  geom_point() +
  labs(
    title = "Conductivity",
    x = "Cond",
    y = "Date") +
  theme_minimal()

temperature <- ggplot(test, aes(y = Temp_C, x = Date)) +
  geom_point() +
  labs(
    title = "Temperature",
    x = "Temperature",
    y = "Date") +
  theme_minimal()

ox_mgL <- ggplot(test, aes(y = ODO_mg.L, x = Date)) +
  geom_point() +
  labs(
    title = "Oxygen",
    x = "Oxygen(mg/L)",
    y = "Date") +
  theme_minimal()

ox_per <- ggplot(test, aes(y = ODO_sat, x = Date)) +
  geom_point() +
  labs(
    title = "Oxygen",
    x = "Oxygen(%)",
    y = "Date") +
  theme_minimal()

turbidity <- ggplot(test, aes(y = Turbidity_FNU, x = Date)) +
  geom_point() +
  labs(
    title = "Turbidity",
    x = "Turbidity(FNU)",
    y = "Date") +
  theme_minimal()

chlorophyll_RFU <- ggplot(test, aes(y = Chlorophyll_RFU, x = Date)) +
  geom_point() +
  labs(
    title = "Chlorophyll",
    x = "Chlorophyll(RFU)",
    y = "Date") +
  theme_minimal()

chlorophyll_ugL <- ggplot(test, aes(y = Chlorophyll_ug.L, x = Date)) +
  geom_point() +
  labs(
    title = "Chlorophyll",
    x = "Chlorophyll(ugL)",
    y = "Date") +
  theme_minimal()

salinity
temperature
conductivity
ox_per
ox_mgL
chlorophyll_RFU
chlorophyll_ugL

# Step 5: Deep dive -------------------------------------------------------
#Look into things that seem weird- this will change each time 

#chl troubleshooting
test$Chlorophyll_ug.L[test$Chlorophyll_ug.L > 100] <- NA
test$Turbidity_FNU[test$Turbidity_FNU < -0.3] <- NA

#remove points outside of bio expectations
test$Turbidity_FNU[test$Turbidity_FNU > 150] <- NA


#Look at plot again
#Looks much better- still some outliers but likely not due to sensor issues




