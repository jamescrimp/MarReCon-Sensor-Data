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
library(fields) 
library(tidyverse)
library(patchwork)
library(cowplot)
library(readr)
library(janitor)

#read these data files in from the shared drive
dir.data <- file.path(
  "I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs"
)

#Data from RSK files read into R (latest version)
RBRlist <- read_csv(file.path(dir.data, "RBR_data_02NOV26.csv"))

#Data from Excel Ruskin files
xRBRlist <- read_csv(file.path(dir.data, "xRBR_data_02NOV26.csv"))

# #add data from some EXO profiles we need
# exoRBRlist <- read_csv(
#   "C:/MarRecon_code/thesis_work/RBR_code/EXO_prof/EXO_profiles.csv")
# #correct date form
# exoRBRlist$date <- mdy(exoRBRlist$date)

#Combine date time column 
#exoRBRlist$time <- ymd_hms(paste(exoRBRlist$date, exoRBRlist$time))
#str(exoRBRlist)


#make pressure neg (better for plotting)
xRBRlist$pressure <- -xRBRlist$pressure # reverse sign for plotting
RBRlist$pressure <- -RBRlist$pressure # reverse sign for plotting
#exoRBRlist$pressure <- -exoRBRlist$pressure

#Combine xRBR(csv data) and RBR (.rsk data)
RBRdat <- rbind(RBRlist,xRBRlist)
#RBRdat <- rbind(RBRdat,exoRBRlist)

RBRdat %>%
  distinct(site, date) %>%
  count(site, name = "n_sampling_events")

#look for duplicates
RBRdat %>% get_dupes()

#Remove duplicate rows (keep first one)
RBRdat <- RBRdat %>% distinct()

#remove conductivity less than 10- likely not in water yet
RBRdat <- RBRdat %>%
  dplyr::filter(conductivity >= 10)
#RBRdat is working data fram with all RBR data in it


#Convert conductivity from mS/cm to us/cm
RBRdat <- RBRdat %>%
  mutate(conductivity = conductivity * 1000)

str(RBRdat)

#Add lat and long for each site- not necessarily where the profile was taken
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

RBRdat$Latitude <- latitude_values[RBRdat$site]
RBRdat$Longitude <- longitude_values[RBRdat$site]

str(RBRdat)

#Add density
RBRdat <- RBRdat %>%
  rowwise() %>%
  mutate(
    SA = gsw_SA_from_SP(salinity, pressure, as.numeric(Longitude), as.numeric(Latitude)),
    CT = gsw_CT_from_t(SA, temperature, pressure),
    RHO = gsw_rho(SA, CT, pressure)
  ) %>%
  ungroup()

#Convert density to sigma-t (rho - 1000)
RBRdat <- RBRdat %>%
  rowwise() %>%
  mutate(
    sigt = (RHO - 1000)) %>%
  ungroup()

#it looks like some upcasts didnt get removed, and some downcasts got duplicated. 
#Lets delete rows where all values are the same 
#
RBRdat1 <- RBRdat %>% 
  group_by(site, date) %>% 
  distinct() %>% 
  ungroup()

#Looks like it removed a good number of rows
#
#Remove data from 2023- no one really knew what they were doing 
RBRdat1 <- RBRdat1 %>%
  dplyr::filter(date >= "2024-01-01")

#Add year date
RBRdat1 <- RBRdat1 %>%
  mutate(year = year(date))

#add doy column 
RBRdat1$doy <- yday(RBRdat1$date)

#Make headers the same as EXO and Hobo data
RBRdat <- RBRdat %>% rename(
  TIME_UTC = time, 
  DEPTH_M = pressure,
  TEMP_C = temperature,
  COND_US.CM = conductivity,
  SAL_PSU = salinity,
  SITE = site,
  DATE = date
)
#make all names upper
names(RBRdat1) <- toupper(names(RBRdat1))

#Remove calculated values for export
RBRdat <- RBRdat1 %>% select(-c(CT, SA, RHO, SIGT, DOY))

#RBRdat1 has calculated density values
#RBRdat has values from RBR

# #Export this CSV that has all RBR data recorded by farmers up to date 
write.csv(RBRdat, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\RBR_data_all_02NOV26.csv"), row.names = FALSE)

#Break into years for export 
#2024 and 2025 data 
RBRdata2024 <- RBRdat %>%
  dplyr::filter(YEAR == "2024")

RBRdata2025 <- RBRdat %>%
  dplyr::filter(YEAR == "2025")

RBRdata2026 <- RBRdat %>%
  dplyr::filter(YEAR == "2026")

#Export 2025 data
write.csv(RBRdata2024, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\RBR_data_2024.csv"), row.names = FALSE)

#Export 2025 data
write.csv(RBRdata2025, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\RBR_data_2025.csv"), row.names = FALSE)

#Export 2026 data
write.csv(RBRdata2026, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\RBR_data_2026.csv"), row.names = FALSE)