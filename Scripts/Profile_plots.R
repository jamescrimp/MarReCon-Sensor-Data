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

#read these data files in from the shared drive
dir.data <- file.path(
  "I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs"
)

#Data from RSK files read into R (latest version)
RBRlist <- read_csv(file.path(dir.data, "RBR_data_18AUG26.csv"))

#Data from Excel Ruskin files
xRBRlist <- read_csv(file.path(dir.data, "xRBR_data_18AUG26.csv"))

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

#Export this CSV that has all RBR data recorded by farmers up to date (18AUG2026)
write.csv(RBRdat, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\RBR_data_all_18AUG26.csv"), row.names = FALSE)

# Calculate decimal year
date_to_decimal <- function(date) {
  year <- year(date)
  start_of_year <- ymd(paste0(year, "-01-01"))
  end_of_year <- ymd(paste0(year + 1, "-01-01"))
  days_in_year <- as.numeric(difftime(end_of_year, start_of_year, units = "days"))
  days_since_start <- as.numeric(difftime(date, start_of_year, units = "days"))
  
  
  decimal_year <- year + (days_since_start / days_in_year)
  return(decimal_year)
}

RBRdat$decimal_year <- date_to_decimal(RBRdat$time)

#Add DOY
#RBRdat$doy <- yday(RBRdat$date)

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
#make all names lowercase
names(RBRdat1) <- tolower(names(RBRdat1))
#add doy column 
RBRdat1$doy <- yday(RBRdat1$date)
  
#Break into years 
#2024 and 2025 data 
RBRdata2024 <- RBRdat1 %>%
  dplyr::filter(year == "2024")

RBRdata2025 <- RBRdat1 %>%
  dplyr::filter(year == "2025")

RBRdata2026 <- RBRdat1 %>%
  dplyr::filter(year == "2026")

#Export CSV of RBRs from 2024:
#write.csv(RBRdata2024, file.path("C:/MarRecon_code/thesis_work/RBR_code/ RBRdata2024.csv"), row.names = FALSE)

#Export 2025 data
write.csv(RBRdata2025, file.path("C:/MarRecon_code/thesis_work/RBR_code/ RBRdata2025.csv"), row.names = FALSE)

#Export 2026 data
write.csv(RBRdata2026, file.path("C:/MarRecon_code/thesis_work/RBR_code/ RBRdata2026.csv"), row.names = FALSE)

#Export all data 
#write.csv(RBRdat1, file.path("C:/MarRecon_code/thesis_work/RBR_code/ RBRdata_complete.csv"), row.names = FALSE)

#Thesis data mod for DOY plots 
#make this data I will use for thesis: march 2024 - Feb 2025 
RBR_thesis <- RBRdat1 %>%
  dplyr::filter(time >= as.POSIXct("2024-03-01") & 
                  time < as.POSIXct("2025-02-28"))
#add DOY for thesis data 
RBR_thesis$date <- as.Date(RBR_thesis$date)  # adjust column name as needed

# Thesis work -------------------------------------------------------------
# Create continuous DOY starting from March 1, 2024
march_1_2024 <- as.Date("2024-03-01")
RBR_thesis$doy <- as.numeric(RBR_thesis$date - march_1_2024) + 1


#________________flexible function to plot sal/temp/density ________________
####PWS####
ctd_plotPWS <- function(site_data, site_name, variable = "salinity") {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(21, 33), name = "Salinity"),
    temperature = list(limits = c(0, 18), name = "Temperature (°C)"),
    sigt = list(limits = c(16, 28), name = "Sigma t (g/cm³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    dplyr::filter(site == site_name) %>%  # Filter for specific site
    dplyr::filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
                    is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.8,
#span is rigidity of the model- lower is very fine scale, closer to 1 is very broad (less likely to overfit)
    na.action = na.omit)
  
  # Create prediction grid based on actual data ranges
  x <- seq(1:365)
  y <- seq(min(0), 
           max(-40), 
           length = 100)
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions
  z <- predict(fit.lo, grd)
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "black", size = 0.1, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246, 276, 307, 338, 365),
      labels = c("Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1", "Aug 1", 
                 "Sep 1", "Oct 1", "Nov 1", "Dec 1", "Jan 1", "Feb 1", "Mar 1"),
      limits = c(1, 365)
    )  +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot() +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title = element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}
# use ctd_plot function to create GAM plots of salinity, temperature, and RHO (density)
TEST <- ctd_plotPWS(site_data=RBRdata2025, "ROK1", "salinity")


#2024 salinity data from PWS 
ROK124s <- ctd_plotPWS(site_data=RBR_thesis, "ROK1", "salinity")+
  theme(legend.position = "none") +
  labs(title ="A) PWS1")+ 
  theme(axis.text.x = element_blank())+
  ylab("Depth (m)")

SBO124s <- ctd_plotPWS(site_data=RBR_thesis, "SBO1", "salinity")+
  theme(legend.position = "none") +
  labs(y = NULL)+
  labs(title ="B) PWS2")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())
  
SBR124s <- ctd_plotPWS(site_data=RBR_thesis, "SBR1", "salinity")+
  labs(y = NULL)+
  labs(title ="C) PWS3")+ 
  theme(axis.text.y = element_blank())+
theme(axis.text.x = element_blank())
#+theme(axis.text.x = element_blank())

PWSsal24 <-  ROK124s + SBO124s + SBR124s 

PWSsal24_noROK <-  SBO124s + SBR124s + ROK1s
PWSsal24_noROK

#Export
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/PWSsal24_noROK.png", PWSsal24_noROK, width = 10, height =7)

#2024 temp data from PWS 
ROK124t <- ctd_plotPWS(site_data=RBR_thesis, "ROK1", "temperature")+
  theme(legend.position = "none") +
  labs(title ="D) PWS1") +
  ylab("Depth (m)")

SBO124t <- ctd_plotPWS(site_data=RBR_thesis, "SBO1", "temperature")+
  theme(legend.position = "none") +
  theme(legend.position = "none") +
  labs(y = NULL)+
  labs(title ="E) PWS2")+
  theme(axis.text.y = element_blank())

SBR124t <- ctd_plotPWS(site_data=RBR_thesis, "SBR1", "temperature")+
  labs(y = NULL)+
  labs(title = "F) PWS3")+ 
 theme(axis.text.y = element_blank())

PWStmp24_noROK1 <-  SBO124t + SBR124t
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/PWStmp24_noROK1.png", PWStmp24_noROK1, width = 10, height =7)

#Export 

#2024 density data from PWS 
ROK124d <- ctd_plotPWS(site_data=RBR_thesis, "ROK1", "sigt")+
  theme(legend.position = "none") 

SBO124d <- ctd_plotPWS(site_data=RBR_thesis, "SBO1", "sigt")+
  theme(legend.position = "none") +
  labs(y = NULL)

SBR124d <- ctd_plotPWS(site_data=RBR_thesis, "SBR1", "sigt")+
  labs(y = NULL)

PWSden24 <- ROK124d + SBO124d + SBR124d

PWSall <- #(ROK124d + SBO124d + SBR124d) /
  (ROK124s + SBO124s + SBR124s)/
  (ROK124t + SBO124t + SBR124t)

#combine sal and temp in one plot
# Column labels as separate plots
# col1 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "ROK1") + 
#   theme_void()
# 
# col2 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "SBO1") + 
#   theme_void()
# 
# col3 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "SBR1") + 
#   theme_void()

# Define the layout design
layoutplot <- "
    ffffffggggggbbbbbb
    ffffffggggggbbbbbb
    ffffffggggggbbbbbb
    hhhhhhiiiiiicccccc
    hhhhhhiiiiiicccccc
    hhhhhhiiiiiicccccc"

# Compose plots into a named list
plotlist <- list(
  #d = col1, e = col2, a = col3,
  f = ROK124s, g = SBO124s, b = SBR124s,
  h = ROK124t, i = SBO124t, c = SBR124t
)

# Create the composite plot
CTD_PLOT_PWS <- wrap_plots(plotlist, 
                      design = layoutplot)

ggsave("C:/MarRecon_code/thesis_work/Plots/Thesis_final/CTD_PLOT_PWS.png", 
       plot = CTD_PLOT_PWS,
       width = 10, height = 10, units = "in",
       dpi = 300, device = "png", 
       bg = "white")


#KBY function- uses dif temp, sal, depth ranges
# Function to create plot for one site with flexible variable selection
ctd_plotKBY <- function(site_data, site_name, variable = "salinity") {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(22, 33), name = "Salinity"),
    temperature = list(limits = c(0, 18), name = "Temperature (°C)"),
    sigt = list(limits = c(16, 28), name = "Sigma t (g/cm³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    dplyr::filter(site == site_name) %>%  # Filter for specific site
    dplyr::filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
                    is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.8,
                  #span is rigidity of the model- lower is very fine scale, closer to 1 is very broad
                  na.action = na.omit)
  
  # Create prediction grid based on actual data ranges
  x <- seq(1:365)
  y <- seq(min(0), 
           max(-15), 
           length = 100)
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions
  z <- predict(fit.lo, grd)
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "black", size = 0.1, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246, 276, 307, 338, 365),
      labels = c("Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1", "Aug 1", 
                 "Sep 1", "Oct 1", "Nov 1", "Dec 1", "Jan 1", "Feb 1", "Mar 1"),
      limits = c(1, 365)
    )  +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot()+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title = element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#2024 salinity data from KBY 
SSF124s <- ctd_plotKBY(site_data=RBR_thesis, "SSF1", "salinity")+
  theme(legend.position = "none") +
  labs(title = "A) KBay3")+ 
  ylab("Depth (m)")+
  theme(axis.text.x = element_blank())

MIO124s <- ctd_plotKBY(site_data=RBR_thesis, "MIO1", "salinity")+
  labs(y = NULL,
       title ="B) KBay2")+  
  theme(axis.text.y = element_blank())+
theme(axis.text.x = element_blank())

#not enough data to use
#BCF124s <- ctd_plot(site_data=RBR_thesis, "BCF1", "salinity")+
 # labs(y = NULL)

KBYsal24 <- SSF124s + MIO124s #+ BCF124s
#Export
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KBYsal24.png", KBYsal24, width = 10, height =7)

#2024 temp data from KBY 
SSF124t <- ctd_plotKBY(site_data=RBR_thesis, 
                       "SSF1", "temperature") +
  ylab("Depth (m)")+
  theme(legend.position = "none")+
        labs(title ="C) KBay3") 
          

MIO124t <- ctd_plotKBY(site_data=RBR_thesis, 
                       "MIO1", "temperature") +
  labs(y = NULL,
       title ="D) KBay2")+ 
  theme(axis.text.y = element_blank())

KBYtmp24 <-SSF124t + MIO124t 

KBYall <- (SSF124s + MIO124s) /
  (SSF124t + MIO124t)

#2024 density data from KBY 
#
SSF124d <- ctd_plotKBY(site_data=RBR_thesis, "SSF1", "sigt")+
theme(legend.position = "none") +
  labs(y = NULL)

MIO124d <- ctd_plotKBY(site_data=RBR_thesis, "MIO1", "sigt")+
  labs(y = NULL)

KBYden24 <- SSF124d + MIO124d 

#combine sal and temp in one plot
# Column labels as separate plots
# col1 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "SSF1") + 
#   theme_void()
# 
# col2 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "MIO1") + 
#   theme_void()


# Define the layout design
layoutplot <- "
    ffffffgggggg
    ffffffgggggg
    ffffffgggggg
    hhhhhhiiiiii
    hhhhhhiiiiii
    hhhhhhiiiiii"

# Compose plots into a named list
plotlist <- list(
 # d = col1, e = col2, 
  f = SSF124s, g = MIO124s, 
  h = SSF124t, i = MIO124t
)

# Create the composite plot
CTD_PLOT_KBY <- wrap_plots(plotlist, 
                           design = layoutplot)

CTD_PLOT_KBY

ggsave("C:/MarRecon_code/thesis_work/Plots/Thesis_final/CTD_PLOT_KBY.png", 
       plot = CTD_PLOT_KBY,
       width = 10, height = 10, units = "in",
       dpi = 300, device = "png", 
       bg = "white")

#Kodiak function -------------------------------------------
# Function to create plot for one site with flexible variable selection
ctd_plotKOD <- function(site_data, site_name, variable = "salinity") {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(27, 33), name = "Salinity"),
    temperature = list(limits = c(2, 12), name = "Temperature (°C)"),
    sigt = list(limits = c(16, 28), name = "Sigma t (g/cm³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    dplyr::filter(site == site_name) %>%  # Filter for specific site
    dplyr::filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
                    is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.8,
#span is rigidity of the model- lower is very fine scale, but can enhance noise. closer to 1 is very broad and could miss trends
                  na.action = na.omit)
  
  # Create prediction grid based on actual data ranges
  x <- seq(1:365)
  y <- seq(min(0), 
           max(-20), 
           length = 100)
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions
  z <- predict(fit.lo, grd)
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "black", size = 0.1, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 62, 93, 123, 154, 185, 215, 246, 276, 307, 338, 365),
      labels = c("Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1", "Aug 1", 
                 "Sep 1", "Oct 1", "Nov 1", "Dec 1", "Jan 1", "Feb 1", "Mar 1"),
      limits = c(1, 365)
    )  +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot()+
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title = element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

#KOD salinity 2024
AOF124s <- ctd_plotKOD(site_data=RBR_thesis, 
                       "AOF1", "salinity")+
  theme(legend.position = "none") +
  labs(title = "A) Kodiak1") +
  ylab("Depth (m)")+
  theme(axis.text.x = element_blank())

KIS124s <- ctd_plotKOD(site_data=RBR_thesis, 
                       "KIS1", "salinity")+
  labs(y = NULL,
       title = "B) Kodiak2")  + 
  theme(axis.text.y = element_blank())+
 theme(axis.text.x = element_blank())

KODsal24 <- AOF124s + KIS124s 
#Export
#ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KODsal24.png", KODsal24, width = 10, height =7)

#2024 temp data from KBY 
AOF124t <- ctd_plotKOD(site_data=RBR_thesis, "AOF1", "temperature")+
  theme(legend.position = "none") +
  labs(title = "C) Kodiak1")+
  ylab("Depth (m)")

KIS124t <- ctd_plotKOD(site_data=RBR_thesis, "KIS1", "temperature")+
  labs(y = NULL,
       title ="D) Kodiak2") + 
  theme(axis.text.y = element_blank())

KODtmp24 <- AOF124t + KIS124t

KODall <- (AOF124s + KIS124s)/
        (AOF124t + KIS124t)



#2024 density data from Kodiak
AOF124d <- ctd_plotKOD(site_data=RBR_thesis, "AOF1", "sigt")+
  theme(legend.position = "none") 

KIS124d <- ctd_plotKOD(site_data=RBR_thesis, "KIS1", "sigt")+
  labs(y = NULL)

KODden24 <- AOF124d + KIS124d

Allt <- (ROK124t + SBO124t + SBR124t)/
  (SSF124t + MIO124t)/
  (AOF124t + KIS124t)

Alls <- (ROK124s + SBO124s  + SBR124s)/
  (SSF124s + MIO124s)/
  (AOF124s + KIS124s)
#combine plots
# col1 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "AOF1") + 
#   theme_void()
# 
# col2 <- ggplot() + 
#   annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "KIS1") + 
#   theme_void()


# Define the layout design
layoutplot <- "
    ffffffgggggg
    ffffffgggggg
    ffffffgggggg
    hhhhhhiiiiii
    hhhhhhiiiiii
    hhhhhhiiiiii"

# Compose plots into a named list
plotlist <- list(
 # d = col1, e = col2, 
  f = AOF124s, g = KIS124s, 
  h = AOF124t, i = KIS124t
)

# Create the composite plot
CTD_PLOT_KOD <- wrap_plots(plotlist, 
                           design = layoutplot)

ggsave("C:/MarRecon_code/thesis_work/Plots/Thesis_final/CTD_PLOT_KOD.png", 
       plot = CTD_PLOT_KOD,
       width = 10, height = 10, units = "in",
       dpi = 300, device = "png", 
       bg = "white")

#Export!
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/CTD_PLOT_PWS.png", CTD_PLOT_PWS, width = 15, height =12)

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/CTD_PLOT_KBY.png", CTD_PLOT_KBY, width = 15, height =12)

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/CTD_PLOT_KOD.png", CTD_PLOT_KOD, width = 15, height =12)

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/Allt.png", Allt, width = 15, height =15 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/Alls.png", Alls, width = 15, height =15 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/PWSsal24.png", PWSsal24, width = 15, height =8 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/PWStmp24.png", PWStmp24, width = 15, height =8 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KBYsal24.png", KBYsal24, width = 15, height =8 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KBYtmp24.png", KBYtmp24, width = 15, height =8 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KODsal24.png", KODsal24, width = 15, height =8 )

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KODtmp24.png", KODtmp24, width = 15, height =8 )
#End of plots for thesis!


###################calendar year plots######################

# Calendar year plots -----------------------------------------------------


ctd_plot_annualPWS <- function(site_data, site_name, variable = "salinity") {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(21, 33), name = "Salinity"),
    temperature = list(limits = c(0, 18), name = "Temperature (°C)"),
    sigt = list(limits = c(16, 28), name = "Sigma t (g/cm³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    dplyr::filter(site == site_name) %>%
    dplyr::filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
                    is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.8,
                  na.action = na.omit)#span adjusts model fit 
  
  # Create prediction grid based on actual data ranges
  x <- seq(1, 365)
  y <- seq(min(0), 
           max(-40), 
           length = 100)#adjust y to change depth 
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions
  z <- predict(fit.lo, grd)
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis (Jan-Dec)
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "darkgrey", size = 0.05, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1", 
                 "Jul 1", "Aug 1", "Sep 1", "Oct 1", "Nov 1", "Dec 1"),
      limits = c(1, 365)
    ) +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

Test <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBO1", "sigt")+
  theme(legend.position = "none") +
  labs(title = NULL)

#2024 salinity data from PWS 
ROK124s <- ctd_plot_annualPWS(site_data=RBRdata2024, "ROK1", "salinity")+
  theme(legend.position = "none") +
  labs(title =NULL)+ theme(axis.text.x = element_blank())

SBO124s <- ctd_plot_annualPWS(site_data=RBRdata2024, "SBO1", "salinity")+
  theme(legend.position = "none") +
  labs(y = NULL)+
  labs(title =NULL)+ theme(axis.text.x = element_blank())+ 
  theme(axis.text.y = element_blank())

SBR124s <- ctd_plot_annualPWS(site_data=RBRdata2024, "SBR1", "salinity")+
  labs(y = NULL)+
  labs(title =NULL)+ theme(axis.text.x = element_blank())+ 
  theme(axis.text.y = element_blank())

PWSsal24 <- ROK124s + SBO124s + SBR124s


#2024 temp data from PWS 
ROK124t <- ctd_plot_annualPWS(site_data=RBRdata2024, "ROK1", "temperature")+
  theme(legend.position = "none") +
  labs(title =NULL)

SBO124t <- ctd_plot_annualPWS(site_data=RBRdata2024, "SBO1", "temperature")+
  theme(legend.position = "none") +
  labs(y = NULL)+
  labs(title =NULL)+ 
  theme(axis.text.y = element_blank())

SBR124t <- ctd_plot_annualPWS(site_data=RBRdata2024, "SBR1", "temperature")+
  labs(y = NULL)+
  labs(title =NULL)+ 
  theme(axis.text.y = element_blank())

PWStmp24 <- ROK124t + SBO124t + SBR124t

#2024 density data from PWS 
ROK124d <- ctd_plot_annualPWS(site_data=RBRdata2024, "ROK1", "sigt")+
  theme(legend.position = "none") 

SBO124d <- ctd_plot_annualPWS(site_data=RBRdata2024, "SBO1", "sigt")+
  theme(legend.position = "none") +
  labs(y = NULL)

SBR124d <- ctd_plot_annualPWS(site_data=RBRdata2024, "SBR1", "sigt")+
  labs(y = NULL)

PWSden24 <- ROK124d + SBO124d + SBR124d

PWSall <- #(ROK124d + SBO124d + SBR124d) /
  (ROK124s + SBO124s + SBR124s)/
  (ROK124t + SBO124t + SBR124t)

#combine sal and temp in one plot
# Column labels as separate plots
col1 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Royal Ocean") + 
  theme_void()
col2 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Simpson Bay") + 
  theme_void()
col3 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Sheep Bay") + 
  theme_void()
title24 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Prince William Sound Temperature and Salinity 2024") + 
  theme_void()

# Define the layout design
layoutplot <- "
    ###jjjjjjjjjjjj###
    ddddddeeeeeeaaaaaa
    ffffffggggggbbbbbb
    ffffffggggggbbbbbb
    ffffffggggggbbbbbb
    hhhhhhiiiiiicccccc
    hhhhhhiiiiiicccccc
    hhhhhhiiiiiicccccc"

# Compose plots into a named list
plotlist <- list(
  d = col1, e = col2, a = col3,
  f = ROK124s, g = SBO124s, b = SBR124s,
  h = ROK124t, i = SBO124t, c = SBR124t,
  j = title24
)

# Create the composite plot - use + instead of &
CTD_PLOT_PWS2024 <- wrap_plots(plotlist, 
                           design = layoutplot)

#Export 2024 CTD plots 
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/PWS2024.png", CTD_PLOT_PWS2024, width = 15, height =8 )

#2025
# salinity data from PWS 
ROK125s <- ctd_plot_annualPWS(site_data=RBRdata2025, "ROK1", "salinity")+
  theme(legend.position = "none") +
  labs(title =NULL)+ theme(axis.text.x = element_blank())

SBO125s <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBO1", "salinity")+
  theme(legend.position = "none") +
  labs(y = NULL)+
  labs(title =NULL)+ theme(axis.text.x = element_blank())+ 
  theme(axis.text.y = element_blank())

SBR125s <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBR1", "salinity")+
  labs(y = NULL)+
  labs(title =NULL)+ theme(axis.text.x = element_blank())+ 
  theme(axis.text.y = element_blank())

PWSsal25 <- ROK125s + SBO125s + SBR125s


#2025 temp data from PWS 
ROK125t <- ctd_plot_annualPWS(site_data=RBRdata2025, "ROK1", "temperature")+
  theme(legend.position = "none") +
  labs(title =NULL)

SBO125t <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBO1", "temperature") +
  labs(y = "Depth (m)", title = NULL) +
  scale_x_continuous(
    name = NULL,
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"),
    limits = c(121, 305)
  ) +
  scale_fill_gradientn(
    colors = tim.colors(100),
    name = "Temp\n(°C)",
    limits = c(0, 18),
    guide = guide_colorbar(barwidth = 0.5, barheight = 4)
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )
SBO125t

SBO125t <- ctd_plot_annualPWS(site_data=RBRdata2025, 
                              "SBO1", "temperature") +
  labs(y = "Depth (m)", title = NULL) +
  scale_x_continuous(
    name = NULL,
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"),
    limits = c(121, 305)
  ) +
  scale_y_continuous(
    name = "Depth (m)",
    breaks = c(0, -1, -2, -3, -4, -5),
    labels = c("0", "1", "2", "3", "4", "5"),
    limits = c(-5, 0)
  ) +
  scale_fill_gradientn(
    colors = tim.colors(100),
    name = "Temp\n(°C)",
    limits = c(5, 16),
    guide = guide_colorbar(barwidth = 0.5, barheight = 4)
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )
SBO125t
  
  #theme(axis.text.y = element_blank())

SBR125t <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBR1", "temperature")+
  labs(y = NULL)+
  labs(title =NULL)+ 
  theme(axis.text.y = element_blank())

PWStmp25 <- ROK125t + SBO125t + SBR125t

#2024 density data from PWS 
ROK125d <- ctd_plot_annualPWS(site_data=RBRdata2025, "ROK1", "sigt")+
  theme(legend.position = "none") 

SBO125d <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBO1", "sigt")+
  theme(legend.position = "none") +
  labs(y = NULL)

SBR125d <- ctd_plot_annualPWS(site_data=RBRdata2025, "SBR1", "sigt")+
  labs(y = NULL)

PWSden25 <- ROK125d + SBO125d + SBR125d

title25 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Prince William Sound Temperature and Salinity 2025") + 
  theme_void()


# Compose plots into a named list
plotlist25 <- list(
  d = col1, e = col2, a = col3,
  f = ROK125s, g = SBO125s, b = SBR125s,
  h = ROK125t, i = SBO125t, c = SBR125t,
  j = title25
)

# Create the composite plot - use + instead of &
CTD_PLOT_PWS2025 <- wrap_plots(plotlist25, 
                               design = layoutplot)


CTD_PLOT_PWS2024

#Export 2025 CTD plots 
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/PWS2025.png", CTD_PLOT_PWS2025, width = 15, height =8 )

#########################KBay#######################
ctd_plot_annualKBY <- function(site_data, site_name, variable = "salinity") {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(26, 33), name = "Salinity"),
    temperature = list(limits = c(0, 14), name = "Temperature (°C)"),
    sigt = list(limits = c(16, 28), name = "Sigma t (g/cm³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    dplyr::filter(site == site_name) %>%
    dplyr::filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
                    is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.8,
                  na.action = na.omit)#span adjusts model fit 
  
  # Create prediction grid based on actual data ranges
  x <- seq(1, 365)
  y <- seq(min(0), 
           max(-15), 
           length = 100)#adjust y to change depth 
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions
  z <- predict(fit.lo, grd)
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis (Jan-Dec)
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "black", size = 0.05, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1", 
                 "Jul 1", "Aug 1", "Sep 1", "Oct 1", "Nov 1", "Dec 1"),
      limits = c(1, 365)
    ) +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}
#
#2024 salinity data from KBY 
SSF124s <- ctd_plot_annualKBY(site_data=RBRdata2024, "SSF1", "salinity")+
  theme(legend.position = "none") +
  labs(title = NULL)+ theme(axis.text.x = element_blank())

MIO124s <- ctd_plot_annualKBY(site_data=RBRdata2024, "MIO1", "salinity")+
  labs(y = NULL,
       title =NULL)+ theme(axis.text.x = element_blank())+ 
  theme(axis.text.y = element_blank())

#not enough data to use
#BCF124s <- ctd_plot(site_data=RBR_thesis, "BCF1", "salinity")+
# labs(y = NULL)

#2024 temp data from KBY 
SSF124t <- ctd_plot_annualKBY(site_data=RBRdata2024, "SSF1", "temperature")+
  theme(legend.position = "none") +
  labs(title =NULL)

MIO124t <- ctd_plot_annualKBY(site_data=RBRdata2024, "MIO1", "temperature")+
  labs(y = NULL,
       title =NULL)+ 
  theme(axis.text.y = element_blank())

KBYtmp24 <-SSF124t + MIO124t 

KBYall <- (SSF124s + MIO124s) /
  (SSF124t + MIO124t)

#2024 density data from KBY 
#
SSF124d <- ctd_plot_annualKBY(site_data=RBRdata2024, "SSF1", "sigt")+
  theme(legend.position = "none") +
  labs(y = NULL)

MIO124d <- ctd_plot_annualKBY(site_data=RBRdata2024, "MIO1", "sigt")+
  labs(y = NULL)

KBYden24 <- SSF124d + MIO124d 

#combine sal and temp in one plot
# Column labels as separate plots
col1 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Spinnaker Sea Farms") + 
  theme_void()

col2 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Peterson Bay") + 
  theme_void()

title24 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Kachemak Bay Temperature and Salinity 2024") + 
  theme_void()


# Define the layout design
layoutplot <- "
    ###jjjjjj###
    ddddddeeeeee
    ffffffgggggg
    ffffffgggggg
    ffffffgggggg
    hhhhhhiiiiii
    hhhhhhiiiiii
    hhhhhhiiiiii"

# Compose plots into a named list
plotlist24 <- list(
  d = col1, e = col2, 
  f = SSF124s, g = MIO124s, 
  h = SSF124t, i = MIO124t,
  j =title24
)


CTD_PLOT_KBY2024 <- wrap_plots(plotlist24, 
                               design = layoutplot)
#save 
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KBY2024.png", CTD_PLOT_KBY2024, width = 15, height =8)

#2025 data 
SSF125s <- ctd_plot_annualKBY(site_data=RBRdata2025, "SSF1", "salinity")+
  theme(legend.position = "none") +
  labs(title = NULL)+ theme(axis.text.x = element_blank())

MIO125s <- ctd_plot_annualKBY(site_data=RBRdata2025, "MIO1", "salinity")+
  theme(legend.position = "none") +
  labs(title = NULL)+ theme(axis.text.x = element_blank())

BCF125s <- ctd_plot_annualKBY(site_data=RBRdata2025, "BCF1", "salinity")+
  labs(y = NULL,
       title =NULL)+ theme(axis.text.x = element_blank())


#2024 temp data from KBY 
SSF125t <- ctd_plot_annualKBY(site_data=RBRdata2025, "SSF1", "temperature")+
  theme(legend.position = "none") +
  labs(title =NULL)

MIO125t <- ctd_plot_annualKBY(site_data=RBRdata2025, "MIO1", "temperature")+
  theme(legend.position = "none") +
  labs(title =NULL)

MIO125t <- ctd_plot_annualKBY(site_data=RBRdata2025, 
                              "MIO1", "temperature") +
  labs(y = "Depth (m)", title = NULL) +
  scale_x_continuous(
    name = NULL,
    breaks = c(121, 152, 182, 213, 244, 274, 305),
    labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov"),
    limits = c(121, 305)
  ) +
  scale_y_continuous(
    name = "Depth (m)",
    breaks = c(0, -1, -2, -3, -4, -5),
    labels = c("0", "1", "2", "3", "4", "5"),
    limits = c(-5, 0)
  ) +
  scale_fill_gradientn(
    colors = tim.colors(100),
    name = "Temp\n(°C)",
    limits = c(7, 13),
    guide = guide_colorbar(barwidth = 0.5, barheight = 4)
  ) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8)
  )
MIO125t

#Export
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/MIO1_temp_sum2025.png", MIO125t, width = 5, height =8, bg = "white")

BCF125t <- ctd_plot_annualKBY(site_data=RBRdata2025, "BCF1", "temperature")+
  theme(legend.position = "right") +
  labs(title =NULL)


#combine sal and temp in one plot
# Column labels as separate plots
col1 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Spinnaker Sea Farms") + 
  theme_void()

col2 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Peterson Bay") + 
  theme_void()

col3 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Bootleggers Cove") + 
  theme_void()

title25 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Kachemak Bay Temperature and Salinity 2025") + 
  theme_void()


# Define the layout design
layoutplot <- "
    ###jjjjjjjjjjjj###
    ddddddeeeeeeaaaaaa
    ffffffggggggbbbbbb
    ffffffggggggbbbbbb
    ffffffggggggbbbbbb
    hhhhhhiiiiiicccccc
    hhhhhhiiiiiicccccc
    hhhhhhiiiiiicccccc"

# Compose plots into a named list
plotlist25 <- list(
  d = col1, e = col2, a = col3,
  f = SSF125s, g = MIO125s, b = BCF125s,
  h = SSF125t, i = MIO125t, c = BCF125t,
  j = title25
)


CTD_PLOT_KBY2025 <- wrap_plots(plotlist25, 
                               design = layoutplot)
#save 
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KBY2025.png", CTD_PLOT_KBY2025, width = 15, height =8)

############################Kodiak######################################
ctd_plot_annualKOD <- function(site_data, site_name, variable = "salinity") {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(26, 33), name = "Salinity"),
    temperature = list(limits = c(0, 14), name = "Temperature (°C)"),
    sigt = list(limits = c(16, 28), name = "Sigma t (g/cm³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    dplyr::filter(site == site_name) %>%
    dplyr::filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
                    is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.8,
                  na.action = na.omit)#span adjusts model fit 
  
  # Create prediction grid based on actual data ranges
  x <- seq(1, 365)
  y <- seq(min(0), 
           max(-20), 
           length = 100)#adjust y to change depth 
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions
  z <- predict(fit.lo, grd)
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis (Jan-Dec)
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "darkgrey", size = 0.05, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = c("Jan 1", "Feb 1", "Mar 1", "Apr 1", "May 1", "Jun 1", 
                 "Jul 1", "Aug 1", "Sep 1", "Oct 1", "Nov 1", "Dec 1"),
      limits = c(1, 365)
    ) +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}
#2024
#KOD salinity 2024
AOF124s <- ctd_plot_annualKOD(site_data=RBRdata2024, "AOF1", "salinity")+
  theme(legend.position = "none") +
  labs(title = NULL) + theme(axis.text.x = element_blank())

KIS124s <- ctd_plot_annualKOD(site_data=RBRdata2024, "KIS1", "salinity")+
  labs(y = NULL,
       title = NULL) + theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_blank())

KODsal24 <- AOF124s + KIS124s 


#2024 temp data from KBY 
AOF124t <- ctd_plot_annualKOD(site_data=RBRdata2024, "AOF1", "temperature")+
  theme(legend.position = "none") +
  labs(title = NULL)

KIS124t <- ctd_plot_annualKOD(site_data=RBRdata2024, "KIS1", "temperature")+
  labs(y = NULL,
       title =NULL) + 
  theme(axis.text.y = element_blank())

KODtmp24 <- AOF124t + KIS124t

KODall <- (AOF124s + KIS124s)/
  (AOF124t + KIS124t)


#combine plots
col1 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Kalsin Bay") + 
  theme_void()

col2 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "KISS") + 
  theme_void()

title24 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Kodiak Temperature and Salinity 2024") + 
  theme_void()


# Define the layout design
layoutplot <- "
    ###jjjjjj###
    ddddddeeeeee
    ffffffgggggg
    ffffffgggggg
    ffffffgggggg
    hhhhhhiiiiii
    hhhhhhiiiiii
    hhhhhhiiiiii"

# Compose plots into a named list
plotlist24 <- list(
  d = col1, e = col2, 
  f = AOF124s, g = KIS124s, 
  h = AOF124t, i = KIS124t,
  j = title24
)

# Create the composite plot
CTD_PLOT_KOD2024 <- wrap_plots(plotlist24, 
                           design = layoutplot)

#2025
AOF125s <- ctd_plot_annualKOD(site_data=RBRdata2025, "AOF1", "salinity")+
  theme(legend.position = "none") +
  labs(title = NULL) + theme(axis.text.x = element_blank())

KIS125s <- ctd_plot_annualKOD(site_data=RBRdata2025, "KIS1", "salinity")+
  labs(y = NULL,
       title = NULL) + theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_blank())

#KOB125s <- ctd_plot_annualKOD(site_data=RBRdata2025, "KOB1", "salinity")+
#  labs(y = NULL,
 #      title = NULL) + theme(axis.text.x = element_blank()) + 
 # theme(axis.text.y = element_blank())

KODsal24 <- AOF124s + KIS124s #+ KOB125s


#2025 temp data  
AOF125t <- ctd_plot_annualKOD(site_data=RBRdata2025, "AOF1", "temperature")+
  theme(legend.position = "none") +
  labs(title = NULL)

KIS125t <- ctd_plot_annualKOD(site_data=RBRdata2025, "KIS1", "temperature")+
  labs(y = NULL,
       title =NULL) + 
  theme(axis.text.y = element_blank())





#combine plots
col1 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = 1, label = "Kalsin Bay") + 
  theme_void()

col2 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "KISS") + 
  theme_void()

title25 <- ggplot() + 
  annotate(geom = 'text', size = 4, fontface = 1, x = 1, y = -1, label = "Kodiak Temperature and Salinity 2025") + 
  theme_void()


# Define the layout design
layoutplot <- "
    ###jjjjjj###
    ddddddeeeeee
    ffffffgggggg
    ffffffgggggg
    ffffffgggggg
    hhhhhhiiiiii
    hhhhhhiiiiii
    hhhhhhiiiiii"

# Compose plots into a named list
plotlist25 <- list(
  d = col1, e = col2, 
  f = AOF125s, g = KIS125s, 
  h = AOF125t, i = KIS125t,
  j = title25
)

# Create the composite plot
CTD_PLOT_KOD2025 <- wrap_plots(plotlist25, 
                           design = layoutplot)

ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KOD2025.png", CTD_PLOT_KOD2025, width = 15, height =8)

CTD_PLOT_KOD2024
ggsave("C:/MarRecon_code/thesis_work/RBR_code/plots/KOD2024.png", CTD_PLOT_KOD2024, width = 15, height =8)

#Fit with MLD model______________________________________________________
#
# Function to create plot for one site with flexible variable selection and MLD calculation
ctd_plot <- function(site_data, site_name, variable = "salinity", calculate_mld = FALSE) {
  # Define variable-specific settings
  var_settings <- list(
    salinity = list(limits = c(28, 33), name = "Salinity"),
    temperature = list(limits = c(0, 13), name = "Temperature (°C)"),
    rho = list(limits = c(1018, 1025), name = "Density (kg/m³)")
  )
  
  # Check if variable exists in data
  if (!variable %in% names(site_data)) {
    stop(paste("Variable", variable, "not found in data"))
  }
  
  # Check if density exists for MLD calculation
  if (calculate_mld && !"rho" %in% names(site_data)) {
    stop("Density (rho) variable not found in data - required for MLD calculation")
  }
  
  # Filter for the specific site first, then clean data
  site_data_clean <- site_data %>%
    filter(site == site_name) %>%  # Filter for specific site
    filter(!is.na(!!sym(variable)) & !is.na(doy) & !is.na(pressure) &
             is.finite(!!sym(variable)) & is.finite(doy) & is.finite(pressure))
  
  # Additional filtering for density if calculating MLD
  if (calculate_mld) {
    site_data_clean <- site_data_clean %>%
      filter(!is.na(rho) & is.finite(rho))
  }
  
  # Fit LOESS model using the selected variable
  formula_str <- paste(variable, "~ doy + pressure")
  fit.lo <- loess(as.formula(formula_str), data = site_data_clean, span = 0.4,
                  #span is rigidity of the model- lower is very fine scale, closer to 1 is very broad
                  na.action = na.omit)
  
  # Fit LOESS model for density if calculating MLD
  if (calculate_mld) {
    fit.rho <- loess(rho ~ doy + pressure, data = site_data_clean, span = 0.4,
                     na.action = na.omit)
  }
  
  # Create prediction grid based on actual data ranges
  x <- seq(1:365)
  y <- seq(min(site_data_clean$pressure, na.rm = TRUE), 
           max(site_data_clean$pressure, na.rm = TRUE), 
           length = 100)
  
  grd <- expand.grid(doy = x, pressure = y)
  
  # Get predictions for the selected variable
  z <- predict(fit.lo, grd)
  
  # Get density predictions if calculating MLD
  if (calculate_mld) {
    z_rho <- predict(fit.rho, grd)
    grd$rho_pred <- as.vector(z_rho)
  }
  
  # Convert to data frame for ggplot
  plot_data <- grd %>%
    mutate(var_pred = as.vector(z))
  
  # Calculate Mixed Layer Depth if requested
  mld_data <- NULL
  if (calculate_mld) {
    mld_data <- plot_data %>%
      group_by(doy) %>%
      arrange(doy, pressure) %>%  # Ensure proper depth ordering
      mutate(
        surface_rho = first(rho_pred, na_rm = TRUE),  # Get surface density (shallowest pressure)
        rho_diff = abs(rho_pred - surface_rho)  # Use absolute difference
      ) %>%
      # Remove any rows with NA predictions
      filter(!is.na(rho_pred) & !is.na(surface_rho)) %>%
      # Find first depth where density difference exceeds 0.125 kg/m³
      filter(rho_diff >= 0.125) %>%
      slice_head(n = 1) %>%  # Take the shallowest depth meeting criteria
      ungroup() %>%
      select(doy, mld = pressure, surface_rho, mld_rho = rho_pred, rho_diff)
    
    # Handle cases where MLD criterion is never met
    missing_days <- setdiff(1:365, mld_data$doy)
    if (length(missing_days) > 0) {
      missing_mld <- plot_data %>%
        filter(doy %in% missing_days) %>%
        group_by(doy) %>%
        arrange(doy, pressure) %>%
        # Use the deepest available depth as MLD when criterion isn't met
        slice_tail(n = 1) %>%
        ungroup() %>%
        mutate(
          surface_rho = NA,  # Mark as missing criterion
          mld = pressure,
          mld_rho = rho_pred,
          rho_diff = NA
        ) %>%
        select(doy, mld, surface_rho, mld_rho, rho_diff)
      
      mld_data <- bind_rows(mld_data, missing_mld) %>%
        arrange(doy)
    }
    
    # Add a flag for days where criterion wasn't met
    mld_data <- mld_data %>%
      mutate(criterion_met = !is.na(rho_diff) & rho_diff >= 0.125)
  }
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot with variable-specific scale and month x-axis
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "black", size = 0.1, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add MLD line if calculated
  if (calculate_mld && nrow(mld_data) > 0) {
    p <- p + 
      geom_line(data = mld_data, aes(x = doy, y = mld), 
                color = "white", size = 1.2, inherit.aes = FALSE) +
      geom_line(data = mld_data, aes(x = doy, y = mld), 
                color = "red", size = 0.8, inherit.aes = FALSE)
  }
  
  # Return plot and optionally MLD data
  if (calculate_mld) {
    return(list(
      plot = p,
      mld_data = mld_data,
      plot_data = plot_data
    ))
  } else {
    return(p)
  }
}

# Helper function to extract just MLD data without plotting
calculate_mld_only <- function(site_data, site_name) {
  # Filter and clean data
  site_data_clean <- site_data %>%
    filter(site == site_name) %>%
    filter(!is.na(rho) & !is.na(doy) & !is.na(pressure) &
             is.finite(rho) & is.finite(doy) & is.finite(pressure))
  
  if (nrow(site_data_clean) == 0) {
    stop(paste("No valid data found for site:", site_name))
  }
  
  # Fit LOESS model for density
  fit.rho <- loess(rho ~ doy + pressure, data = site_data_clean, span = 0.4,
                   na.action = na.omit)
  
  # Create prediction grid
  x <- seq(1:365)
  y <- seq(min(site_data_clean$pressure, na.rm = TRUE), 
           max(site_data_clean$pressure, na.rm = TRUE), 
           length = 100)
  
  grd <- expand.grid(doy = x, pressure = y)
  z_rho <- predict(fit.rho, grd)
  
  # Calculate MLD with improved logic
  # *** KEY FIX: Use the SAME prediction data for MLD calculation ***
  mld_data <- NULL
  if (calculate_mld) {
    # If we're plotting density, use the same predictions
    if (variable == "rho") {
      plot_data$rho_pred <- plot_data$var_pred
    } else {
      # If plotting something else, we need separate density predictions
      fit.rho <- loess(rho ~ doy + pressure, data = site_data_clean, span = 0.4,
                       na.action = na.omit)
      z_rho <- predict(fit.rho, grd)
      plot_data$rho_pred <- as.vector(z_rho)
    }
    
    # Calculate MLD using the prediction data
    mld_data <- plot_data %>%
      group_by(doy) %>%
      arrange(doy, pressure) %>%
      mutate(
        surface_rho = first(rho_pred, na_rm = TRUE),
        rho_diff = abs(rho_pred - surface_rho)
      ) %>%
      filter(!is.na(rho_pred) & !is.na(surface_rho)) %>%
      filter(rho_diff >= 0.125) %>%
      slice_head(n = 1) %>%
      ungroup() %>%
      select(doy, mld = pressure, surface_rho, mld_rho = rho_pred, rho_diff)
    
    # Handle missing days
    missing_days <- setdiff(1:365, mld_data$doy)
    if (length(missing_days) > 0) {
      missing_mld <- plot_data %>%
        filter(doy %in% missing_days) %>%
        group_by(doy) %>%
        arrange(doy, pressure) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        mutate(
          surface_rho = NA,
          mld = pressure,
          mld_rho = rho_pred,
          rho_diff = NA
        ) %>%
        select(doy, mld, surface_rho, mld_rho, rho_diff)
      
      mld_data <- bind_rows(mld_data, missing_mld) %>%
        arrange(doy)
    }
    
    mld_data <- mld_data %>%
      mutate(criterion_met = !is.na(rho_diff) & rho_diff >= 0.125)
  }
  
  # Get settings for the selected variable
  current_settings <- var_settings[[variable]]
  
  # Create ggplot
  p <- ggplot(plot_data, aes(x = doy, y = pressure, fill = var_pred)) +
    geom_raster() +
    scale_fill_gradientn(colors = tim.colors(100), 
                         name = current_settings$name,
                         limits = current_settings$limits) +
    geom_point(data = site_data_clean, aes_string(x = "doy", y = "pressure"), 
               fill = NA, color = "black", size = 0.1, shape = 3) +
    scale_x_continuous(
      name = NULL,
      breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ) +
    labs(title = paste("Site:", site_name, "-", current_settings$name),
         y = "Pressure (dBar)") +
    theme_cowplot() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Add MLD line if calculated
  if (calculate_mld && nrow(mld_data) > 0) {
    p <- p + 
      geom_line(data = mld_data, aes(x = doy, y = mld), 
                color = "white", size = 1.2, inherit.aes = FALSE) +
      geom_line(data = mld_data, aes(x = doy, y = mld), 
                color = "red", size = 0.8, inherit.aes = FALSE)
  }
  
  # Return plot and optionally MLD data
  if (calculate_mld) {
    return(list(
      plot = p,
      mld_data = mld_data,
      plot_data = plot_data
    ))
  } else {
    return(p)
  }
}



result <- ctd_plot(RBR_thesis, "SBO1", variable = "rho", calculate_mld = TRUE)
plot_with_mld <- result$plot
mld_data <- result$mld_data



#------------ Fit LOESS model: original script from franz class----------------
KIS1sfit.lo <- loess(RHO ~ doy + pressure, data = KIS125, span=0.2)

layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1)) 
# Two columns: 4 parts for the image, 1 part for the legend


#Salinity at KIS1
# Set up a grid of values for computing predicted values (For the 'image' function, values in x and y need to be in increasing order)
x <- seq(45, 350,length=100)
y <- seq(-16, 0, length=100) 

# This creates a grid of all combinations of sampling dates and depths in the x and y vectors:
(grd <- expand.grid(doy=x, pressure=y))

# Predicted values from model over entire grid:
z <- predict(KIS1sfit.lo, grd)
library(fields)  # required for the oceanographic color scheme:

image(x, y, z, col = tim.colors(100), 
      xlab="", ylab="",
      cex.axis=1, cex.lab=1.2,
      xaxt = "n")
axis(1, at=pretty(x), labels=TRUE, tick=TRUE, line=0)
title("Kodiak Island Sustainable Seaweeds Salinity")
# show when/where measurements were taken:
points(KIS1$doy, KIS1$pressure, pch=10, cex=0.1) 

win.graph(3,1)
par(mar=c(2,.5,0,.5))
xx <- seq(min(z, na.rm=T), max(z,na.rm=T), length=80)
i <- xx[2]-xx[1]
plot(c(xx[1]-i,xx[80]+i),c(0,1),type="n",axes=0,xlab="",ylab="")
rect(xx-i, 0, xx+i, 1, col=tim.colors(80), border=NA)
axis(1, line=0, cex.axis=1)
dev.prev() 


#-------------------------------------
dev.off()
#SSF1
SSF1sfit.lo <- loess(RHO ~ doy + pressure, data = SSF1, span=0.2)

# Set up a grid of values for computing predicted values (For the 'image' function, values in x and y need to be in increasing order)
x <- seq(45, 350,length=100)
y <- seq(-16, 0, length=100) 

# This creates a grid of all combinations of sampling dates and depths in the x and y vectors:
(grd <- expand.grid(doy=x, pressure=y))

# Predicted values from model over entire grid:
z <- predict(SSF1sfit.lo, grd)

image(x, y, z, col = tim.colors(100), 
      xlab="", ylab="",
      cex.axis=1, cex.lab=1.2,
      xaxt="n")
axis(1, at=pretty(x), labels=TRUE, tick=TRUE, line=0)
title("Spinnaker Sea Farms salinity")
# show when/where measurements were taken:
points(SSF1$doy, SSF1$pressure, pch=10, cex=0.1) 

par(mar=c(2,.5,0,.5))
xx <- seq(min(z, na.rm=T), max(z,na.rm=T), length=80)
i <- xx[2]-xx[1]
plot(c(xx[1]-i,xx[80]+i),c(0,1),type="n",axes=0,xlab="",ylab="")
rect(xx-i, 0, xx+i, 1, col=tim.colors(100), border=NA)
axis(1, line=0, cex.axis=1)
#---------------------------------------
#SBO1
dev.off()
SBO1sfit.lo <- loess(salinity ~ doy + pressure, data = SBO1, span=0.2)
summary(SBO1sfit.lo)

# Set up a grid of values for computing predicted values (For the 'image' function, values in x and y need to be in increasing order)
x <- seq(01, 364,length=100)
y <- seq(-12, 0, length=100) 

# This creates a grid of all combinations of sampling dates and depths in the x and y vectors:
(grd <- expand.grid(doy=x, pressure=y))

# Predicted values from model over entire grid:
z <- predict(SBO1sfit.lo, grd)
#library(fields)  # required for the oceanographic color scheme:

image(x, y, z, col = tim.colors(100), 
      xlab="Day of year", ylab="Depth (m)",
      cex.axis=1, cex.lab=1.2)
#title("Simpson Bay Oyster Farm Salinity")
# show when/where measurements were taken:
points(SBO1$doy, SBO1$pressure, pch=10, cex=0.1) 

par(mar=c(2,.5,0,.5))
xx <- seq(min(z, na.rm=T), max(z,na.rm=T), length=80)
i <- xx[2]-xx[1]
plot(c(xx[1]-i,xx[80]+i),c(0,1),type="n",axes=0,xlab="",ylab="")
rect(xx-i, 0, xx+i, 1, col=tim.colors(100), border=NA)
axis(1, line=0, cex.axis=1)

#Try w dates ______________________
# Salinity analysis with dates
SBO1sfit.lo <- loess(salinity ~ as.numeric(date) + pressure, data = SBO1, span=0.2)

# Create date sequence from Jan 2024 to June 2025
start_date <- as.Date("2024-02-01")
end_date <- as.Date("2025-06-30")
x_dates <- seq(start_date, end_date, length.out=100)
x_numeric <- as.numeric(x_dates)  # Convert to numeric for prediction

y <- seq(-12, 0, length=100) 
grd <- expand.grid(date=x_numeric, pressure=y)
z <- predict(SBO1sfit.lo, grd)

# Create the plot
image(x_numeric, y, z, col = tim.colors(100), 
      xlab="Date", ylab="Depth (m)", axes=FALSE,
      cex.axis=1, cex.lab=1.2)

# Add custom date axis
axis(2)  # y-axis (depth)
axis.Date(1, at=seq(start_date, end_date, by="3 months"), 
          x=seq(start_date, end_date, by="3 months"),
          format="%b %Y")  # Shows "Jan 2024", "Apr 2024", etc.

# Add measurement points
points(as.numeric(SBO1$date), SBO1$pressure, pch=10, cex=0.1)

# Add title
title("Simpson Bay Oyster Farm Salinity")

#Temp SBO1
SBO1tfit.lo <- loess(temperature ~ doy + pressure, data = SBO1, span=0.2)

summary(SBO1tfit.lo)

# Set up a grid of values for computing predicted values (For the 'image' function, values in x and y need to be in increasing order)
x <- seq(45, 350,length=100)
y <- seq(-12, 0, length=100) 

# This creates a grid of all combinations of sampling dates and depths in the x and y vectors:
(grd <- expand.grid(doy=x, pressure=y))

# Predicted values from model over entire grid:
z <- predict(SBO1tfit.lo, grd)
#library(fields)  # required for the oceanographic color scheme:

image(x, y, z, col = tim.colors(100), 
      xlab="Day of year", ylab="Depth (m)",
      cex.axis=1, cex.lab=1.2)
title("Simpson Bay Oyster Farm Temperature")
# show when/where measurements were taken:
points(SBO1$doy, SBO1$pressure, pch=10, cex=0.1) 

par(mar=c(2,.5,0,.5))
xx <- seq(min(z, na.rm=T), max(z,na.rm=T), length=80)
i <- xx[2]-xx[1]
plot(c(xx[1]-i,xx[80]+i),c(0,1),type="n",axes=0,xlab="",ylab="")
rect(xx-i, 0, xx+i, 1, col=tim.colors(100), border=NA)
axis(1, line=0, cex.axis=1)

#-----------------------------------------------------------------------------
#ROK1- data from the RBR is off. Fall is reporting as July 
ROK1sfit.lo <- loess(salinity ~ decimal_year + pressure, data = ROK1, span=0.2)

# Set up a grid of values for computing predicted values (For the 'image' function, values in x and y need to be in increasing order)
x <- seq(2024.2,2024.9,length=100)
y <- seq(-50, 0, length=100) 

# This creates a grid of all combinations of sampling dates and depths in the x and y vectors:
(grd <- expand.grid(decimal_year=x, pressure=y))

# Predicted values from model over entire grid:
z <- predict(ROK1sfit.lo, grd)
library(fields)  # required for the oceanographic color scheme:

image(x, y, z, col = tim.colors(100), 
      xlab="Time", ylab="Depth (m)",
      cex.axis=1, cex.lab=1.2)
title("KIS1 salinity")
# show when/where measurements were taken:
points(KIS1$decimal_year, KIS1$pressure, pch=10, cex=0.1) 


win.graph(3,1)
par(mar=c(2,.5,0,.5))
xx <- seq(min(z, na.rm=T), max(z,na.rm=T), length=80)
i <- xx[2]-xx[1]
plot(c(xx[1]-i,xx[80]+i),c(0,1),type="n",axes=0,xlab="",ylab="")
rect(xx-i, 0, xx+i, 1, col=tim.colors(100), border=NA)
axis(1, line=0, cex.axis=1)
dev.prev() 

#Model fits
summary(SBO1sfit.lo)
summary(SSF1sfit.lo)
summary(KIS1sfit.lo)


#____________________________________
#
library(fields)  # Required for tim.colors
dev.off()

png("stacked_plots_with_legend1.png", width = 800, height = 1200)
# Set up a layout with four rows: three plots and one legend
layout(matrix(c(1, 2, 3, 4), nrow = 4, byrow = TRUE), heights = c(1, 1, 1, 0.3))  # Adjust heights for better fit

# Define a consistent color scale for all plots
x <- seq(45, 350, length = 100)
y <- seq(-16, 0, length = 100)
grd <- expand.grid(doy = x, pressure = y)
z_range <- range(c(
  predict(KIS1sfit.lo, grd),
  predict(SSF1sfit.lo, grd),
  predict(SBO1sfit.lo, grd)
), na.rm = TRUE)  # Common range for all plots

# Adjust plot margins
par(mar = c(3, 4, 2, 2))

# First plot: KIS1
z <- predict(KIS1sfit.lo, grd)
image(x, y, z, col = tim.colors(100), zlim = z_range, 
      xlab = "", ylab = "", cex.axis = 2, cex.lab = 2)
title("", cex.main = 0.9)
points(KIS1$doy, KIS1$pressure, pch = 10, cex = 0.5)

# Second plot: SSF1
z <- predict(SSF1sfit.lo, grd)
image(x, y, z, col = tim.colors(100), zlim = z_range, 
      xlab = "", ylab = "", cex.axis = 2, cex.lab = 2)
title("", cex.main = 0.9)
points(SSF1$doy, SSF1$pressure, pch = 10, cex = 0.5)

# Third plot: SBO1
z <- predict(SBO1sfit.lo, grd)
image(x, y, z, col = tim.colors(100), zlim = z_range, 
      xlab = "", ylab = "", cex.axis = 2, cex.lab = 2)
title("", cex.main = 0.9)
points(SBO1$doy, SBO1$pressure, pch = 10, cex = 0.5)

# Fourth panel: Shared legend
par(mar = c(3, 1, 2, 1))  # Adjust margins for the legend
xx <- seq(z_range[1], z_range[2], length = 80)
i <- xx[2] - xx[1]
plot(c(xx[1] - i, xx[80] + i), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
rect(xx - i, 0, xx + i, 1, col = tim.colors(80), border = NA)
axis(1, at = pretty(z_range), line = 0, cex.axis = 2)

dev.off()


# Temp_____________________________________________________________
# 

#models
KIS1tfit.lo <- loess(temperature ~ doy + pressure, data = KIS1, span=0.2)
SSF1tfit.lo <- loess(temperature ~ doy + pressure, data = SSF1, span=0.2)
SBO1tfit.lo <- loess(temperature ~ doy + pressure, data = SBO1, span=0.2)



dev.off()

png("stacked_plots_with_legend1.temp.png", width = 800, height = 1200)
# Set up a layout with four rows: three plots and one legend
layout(matrix(c(1, 2, 3, 4), nrow = 4, byrow = TRUE), heights = c(1, 1, 1, 0.3))  # Adjust heights for better fit

# Define a consistent color scale for all plots
x <- seq(45, 350, length = 100)
y <- seq(-16, 0, length = 100)
grd <- expand.grid(doy = x, pressure = y)
z_range <- range(c(
  predict(KIS1tfit.lo, grd),
  predict(SSF1tfit.lo, grd),
  predict(SBO1tfit.lo, grd)
), na.rm = TRUE)  # Common range for all plots

# Adjust plot margins
par(mar = c(3, 4, 2, 2))

# First plot: KIS1
z <- predict(KIS1tfit.lo, grd)
image(x, y, z, col = tim.colors(100), zlim = z_range, 
      xlab = "", ylab = "", cex.axis = 2, cex.lab = 2)
title("", cex.main = 0.9)
points(KIS1$doy, KIS1$pressure, pch = 10, cex = 0.5)

# Second plot: SSF1
z <- predict(SSF1tfit.lo, grd)
image(x, y, z, col = tim.colors(100), zlim = z_range, 
      xlab = "", ylab = "", cex.axis = 2, cex.lab = 2)
title("", cex.main = 0.9)
points(SSF1$doy, SSF1$pressure, pch = 10, cex = 0.5)

# Third plot: SBO1
z <- predict(SBO1tfit.lo, grd)
image(x, y, z, col = tim.colors(100), zlim = z_range, 
      xlab = "", ylab = "", cex.axis = 2, cex.lab = 2)
title("", cex.main = 0.9)
points(SBO1$doy, SBO1$pressure, pch = 10, cex = 0.5)

# Fourth panel: Shared legend
par(mar = c(3, 1, 2, 1))  # Adjust margins for the legend
xx <- seq(z_range[1], z_range[2], length = 80)
i <- xx[2] - xx[1]
plot(c(xx[1] - i, xx[80] + i), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
rect(xx - i, 0, xx + i, 1, col = tim.colors(80), border = NA)
axis(1, at = pretty(z_range), line = 0, cex.axis = 2)

dev.off()

