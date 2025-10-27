

# Data input --------------------------------------------------------------

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
library(cowplot)
library(patchwork)

setwd("~/Library/CloudStorage/GoogleDrive-jcrimp@alaska.edu/Shared drives/Mariculture ReCon/Data Management")

#Save working directory path as an object
wd <- getwd()

#file path 
dir.dat <- file.path(wd, "Raw data from sensors")  
dir.csv <- file.path(wd, "James MarRecon Code/outputs/CSVs")  
dir.outputs <- file.path(wd, "James MarRecon Code/outputs")

#Create folders for Temp, Sal, Chlor, and Turb

temp_plots <- file.path(dir.outputs, "Temperatute_plots")
if (!dir.exists(temp_plots)) {
  dir.create(temp_plots)
}

sal_plots <- file.path(dir.outputs, "Salinity_plots")
if (!dir.exists(sal_plots)) {
  dir.create(sal_plots)
}

chlor_plots <- file.path(dir.outputs, "Chlorophyll_plots")
if (!dir.exists(chlor_plots)) {
  dir.create(chlor_plots)
}

turb_plots <- file.path(dir.outputs, "Turbidity_plots")
if (!dir.exists(turb_plots)) {
  dir.create(turb_plots)
}

cond_plots <- file.path(dir.outputs, "Conductivity_plots")
if (!dir.exists(turb_plots)) {
  dir.create(turb_plots)
}

oyxgen_plots <- file.path(dir.outputs, "Oxygen_plots")
if (!dir.exists(turb_plots)) {
  dir.create(turb_plots)
}

par_plots <- file.path(dir.outputs, "PAR_plots")
if (!dir.exists(turb_plots)) {
  dir.create(turb_plots)
}

oyster_plots <- file.path(dir.outputs, "oyster_plots")
if (!dir.exists(oyster_plots)) {
  dir.create(oyster_plots)
}

# Named list mapping each variable to its corresponding output folder
variable_dirs <- list(
  MeanTemp = temp_plots,
  MeanSal = sal_plots,
  MeanChlor = chlor_plots,
  MeanTurb = turb_plots,
  MeanCond = cond_plots
)


# Import data

# EXO

exo_dataI <- read.csv(file.path(dir.csv, "EXO_I_clean.csv"))
exo_dataI$Date <- as.Date(exo_dataI$Date)
exo_dataI$Time_UTC <- as.POSIXct(exo_dataI$Time_UTC)

exo_dataI$Site <- factor(exo_dataI$Site, levels=c("AOF1","KIS1","KOB1",
                                                      "BCF1","MIO1","SSF1",
                                                      "ROK1","SBO1","SBR1"))

#RBR

rbr_data <- read.csv(file.path(dir.csv, "RBR_data.csv"))
rbr_data$Date <- as.Date(rbr_data$Date)
rbr_data$Time <- as.POSIXct(rbr_data$Time)

rbr_data$site <- factor(rbr_data$site, levels=c("AOF1","KIS1","KOB1",
                                                "BCF1","MIO1","SSF1",
                                                "ROK1","SBO1","SBR1"))

#HOBO

hobo_data <- read.csv(file.path(dir.csv, "hobo_clean.csv"))
hobo_data$time <- as.POSIXct(hobo_data$time)
hobo_data$site <- factor(hobo_data$site, levels=c("AOF1","KIS1","KOB1",
                                                  "BCF1","MIO1","SSF1",
                                                  "ROK1","SBO1","SBR1"))
names(hobo_data)[8] <- "date"
hobo_data$date <- as.Date(hobo_data$date)

hobo_raw <- read.csv(file.path(dir.csv, "hobo_data.csv"))
hobo_raw$time <- as.POSIXct(hobo_raw$time, format = "%m/%d/%y %I:%M:%S %p")
hobo_raw$site <- factor(hobo_raw$site, levels=c("AOF1","KIS1","KOB1",
                                                  "BCF1","MIO1","SSF1",
                                                  "ROK1","SBO1","SBR1"))
hobo_raw$date <- as.Date(hobo_raw$time) 
hobo_raw$salinity <- as.numeric(hobo_raw$salinity)

#PAR

par_data <- read.csv(file.path(dir.csv, "par_clean.csv"))
par_data$date <- as.Date(par_data$Date)
par_data$time <- as.POSIXct(par_data$Time_UTC)

par_data$Site <- factor(par_data$Site, levels=c("AOF1","KIS1","KOB1",
                                                  "BCF1","MIO1","SSF1",
                                                  "ROK1","SBO1","SBR1"))

#Oyster growth

growth_data <- read.csv(file.path(dir.dat, "oyster_samples/oyster_morph_dat.csv"))
growth_data$Arrival_date <- as.Date(growth_data$Arrival_date)
growth_data <- growth_data[c(1:1430),c(1:18)]
growth_data$site <- factor(growth_data$Farm, levels=c("AOF1","KIS1","KOB1",
                                                      "BCF1","MIO1","SSF1",
                                                      "ROK1","SBO1","SBR1"))

growth_data <- growth_data%>% 
  filter(!is.na(as.numeric(Shell_length_mm))) %>% 
  filter(!is.na(as.numeric(Shell_height_mm))) %>% 
  filter(!is.na(as.numeric(Shell_depth_mm))) %>% 
  filter(!is.na(as.numeric(Whole_weight_g))) %>% 
  filter(!is.na(as.numeric(Meat_weight_g))) %>% 
  mutate(
    Shell_length_mm = as.numeric(Shell_length_mm),
    Shell_height_mm = as.numeric(Shell_height_mm),
    Shell_depth_mm = as.numeric(Shell_depth_mm),
    Whole_weight_g = as.numeric(Whole_weight_g),
    Meat_weight_g = as.numeric(Meat_weight_g)
  )
juv_data <- growth_data %>% filter(Oyster_age == "Juvenile")

sub_data <- growth_data %>% filter(Oyster_age == "Subadult")




#Summarize EXO data by date and site

exo_by_date <- exo_dataI %>% group_by(Date, Site, region) %>% summarize("MeanTemp" = mean(Temp_C, na.rm = TRUE),
                                                               "MeanSal" = mean(Sal_PSU, na.rm = TRUE),
                                                               "MeanTurb" = mean(Turbidity_FNU, na.rm = TRUE),
                                                               "MeanChlor" = mean(Chlorophyll_RFU, na.rm = TRUE),
                                                               "MeanCond" = mean(Cond_uS.cm, na.rm = TRUE)
)

#Summarize EXO data by date and region

exo_by_region <- exo_dataI %>% group_by(Date, region) %>% summarize("MeanTemp" = mean(Temp_C, na.rm = TRUE),
                                                                        "MeanSal" = mean(Sal_PSU, na.rm = TRUE),
                                                                        "MeanTurb" = mean(Turbidity_FNU, na.rm = TRUE),
                                                                        "MeanChlor" = mean(Chlorophyll_RFU, na.rm = TRUE),
                                                                        "MeanCond" = mean(Cond_uS.cm, na.rm = TRUE)
)

#Summarize EXO data by site for summer 2024
exo_summer <- exo_dataI %>% filter(Date > "2024-05-31" & Date < "2024-09-01") %>% 
  group_by(Site, region) %>% summarize("MeanTemp" = mean(Temp_C, na.rm = TRUE),
                                             "MeanSal" = mean(Sal_PSU, na.rm = TRUE),
                                             "MeanTurb" = mean(Turbidity_FNU, na.rm = TRUE),
                                             "MeanChlor" = mean(Chlorophyll_RFU, na.rm = TRUE),
                                             "MeanCond" = mean(Cond_uS.cm, na.rm = TRUE))



#Summarize RBR data by date for first meter of depth
rbr_1M_by_date <- rbr_data %>% filter(Pressure >= -1 & Pressure < 0) %>% group_by(Date, site, region) %>% summarize("MeanPressure" = mean(Pressure),
                                                                  "MeanTemp" = mean(Temperature),
                                                                  "MeanCond" = mean(Conductivity),
                                                                  "MeanSal" = mean(Salinity))

#Summarize RBR data by date for third meter of depth

rbr_3M_by_date <- rbr_data %>% filter(Pressure >= -4 & Pressure < -2) %>% group_by(Date, site, region) %>% summarize("MeanPressure" = mean(Pressure),
                                                                                                            "MeanTemp" = mean(Temperature),
                                                                                                            "MeanCond" = mean(Conductivity),
                                                                                                            "MeanSal" = mean(Salinity))

#Summarize hobo data by date

hobo_by_date <- hobo_data %>% group_by(date, site) %>% summarize(MeanLowRange = mean(LowRange, na.rm = TRUE),
                                                                 MeanHighRange = mean(HighRange, na.rm = TRUE),
                                                                 MeanTemp = mean(Temp, na.rm = TRUE)
)

raw_hobo_by_date <- hobo_raw %>% group_by(date, site) %>% summarize(MeanLowRange = mean(LowRange, na.rm = TRUE),
                                                                 MeanHighRange = mean(HighRange, na.rm = TRUE),
                                                                 MeanTemp = mean(Temp, na.rm = TRUE),
                                                                 MeanSal = mean(salinity),
)



#Summarize PAR data by date

par_by_date <- par_data %>% group_by(date, Site, region) %>% summarize(MeanPAR = mean(PAR, na.rm = TRUE),
                                                                 MeanTemp = mean(Temp_C, na.rm = TRUE)
)

# Descriptive Stats ----------------------------------------------------------------

#EXO temperature by region

temperature.by.region <- exo_dataI %>% group_by(region) %>% 
  summarise(mean.temp = mean(Temp_C, na.rm = TRUE), sd.temp = sd(Temp_C, na.rm = TRUE),
            mean.sal = mean(Sal_PSU, na.rm = TRUE), sd.sal = sd(Sal_PSU, na.rm = TRUE),
            mean.turb = mean(Turbidity_FNU, na.rm = TRUE), sd.turb = sd(Turbidity_FNU, na.rm = TRUE))

#Add columns with mean and sd temp in F

temperature.by.region$mean.temp.f <- temperature.by.region$mean.temp * 9/5 + 32
temperature.by.region$sd.temp.f <- temperature.by.region$sd.temp * 9/5 + 32



# Plots by site ---------------------------------------------------

#Loop version

# Denote variables of interest
variables <- c("MeanTemp", "MeanSal", "MeanChlor", "MeanTurb", "MeanCond")

#Create a time series of the raw data from variables of interest for each farm

site_names <- unique(exo_dataI$Site)


for (variable in variables) {

  # Get the correct output directory for the current variable
  output_dir <- variable_dirs[[variable]]
  
   # Specify the PDF file path for the current variable
  pdf(file = file.path(output_dir, paste0("Time_Series_", variable, ".pdf")), width = 8, height = 6)

  #Create a time series for the current variable
  p <- ggplot() +
  geom_line(data = exo_by_date, 
            aes(x = Date, 
                y = !!sym(variable), 
                group = interaction(region, Site), 
                col = interaction(region, Site))) +
    scale_color_manual(
      name = "Region",
      values = c(
        # Kodiak shades of blue
        "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
        # PWS shades of green
        "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
        # KAC shades of red
        "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
      ),
      breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
      labels = c(
        "kod.AOF1" = "Kodiak",
        "pws.ROK1" = "Prince William Sound",
        "kbay.SSF1" = "Kachemak Bay"
      )
    ) +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = "1 month",
                 breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"),
                 limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
                 expand = c(0.01,0)) +
    scale_y_continuous() +
      # name = paste("(", variable, "°C)") +
    theme_cowplot() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "none",
      # axis.text.x = element_blank(),
      legend.title = element_blank()
    )
  # Print the plot to the PDF
  print(p)

# Close the PDF file
dev.off()
}



#Temperature

g.temp.by.site <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, Site), col = interaction(region, Site))) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    limits = c(0,16), 
    expand = c (0, 0),
    name = "Temperature (°C)") +
  #   sec.axis = sec_axis(~ . * 9/5 + 32, name = "Temperature (°F)") # Conversion from °C to °F
  # ) +
  theme_cowplot() +
  theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = c(0.01, 0.9),
        # axis.text.x = element_blank(),
        legend.title = element_blank()
  )

g.temp.by.site

ggsave("Temperature_by_site.png", plot = last_plot(), device = NULL, path = temp_plots,
       scale = 1, width = 10, height = 3.5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Salinity

g.sal.by.site <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanSal, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
    scale_y_continuous(
      name = "Salinity") +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_blank(),
          legend.position = c(0.01, 0.2),
          legend.title = element_blank()
    )

g.sal.by.site

ggsave("Salinity_by_site.png", plot = last_plot(), device = NULL, path = sal_plots,
       scale = 1, width = 10, height = 3.5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Chlorophyll

g.chlor.by.site <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanChlor, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Chlorophyll (RFU)") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.9),
        legend.title = element_blank()
  )

g.chlor.by.site

ggsave("Chlorophyll_by_site.png", plot = last_plot(), device = NULL, path = chlor_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Turbidity
g.turb.by.site <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanTurb, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Turbidity (FNU)") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.85),
        legend.title = element_blank()
  )


g.turb.by.site

ggsave("Turbidity_by_site.png", plot = last_plot(), device = NULL, path = turb_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#PAR
g.par.by.site <- ggplot() +
  geom_line(data = par_by_date, aes(x = date, y = MeanPAR, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    # breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    # labels = c(
    #   "kod.AOF1" = "Kodiak",
    #   "pws.ROK1" = "Prince William Sound",
    #   "kbay.SSF1" = "Kachemak Bay"
    # )
    labels = c(
      "kod.AOF1" = "AOF1",
      "kod.KIS1" = "KIS1",
      "kod.KOB1" = "KOB1",
      "pws.ROK1" = "ROK1",
      "pws.SBO1" = "SBO1",
      "pws.SBR1" = "SBR1",
      "kbay.SSF1" = "SSF1",
      "kbay.MIO1" = "MIO1",
      "kbay.BCF1" = "BCF1"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "PAR") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.75),
        legend.title = element_blank()
  )


g.par.by.site

ggsave("PAR_by_site.png", plot = last_plot(), device = NULL, path = par_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()





# Plots by site, faceted by region ----------------------------------------

#Temperature

g.temp.by.site.faceted <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanTemp, group= Site, col = Site)) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Temperature (°C)") +
  #   sec.axis = sec_axis(~ . * 9/5 + 32, name = "Temperature (°F)") # Conversion from °C to °F
  # ) +
  theme_cowplot() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # legend.position = c(0.01, 0.2),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.temp.by.site.faceted

ggsave("Temperature_by_site_faceted.png", plot = last_plot(), device = NULL, path = temp_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#Salinity

g.sal.by.site.faceted <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanSal, group= Site, col = Site)) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Salinity") +
  theme_cowplot() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # legend.position = c(0.01, 0.2),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.sal.by.site.faceted

ggsave("Salinity_by_site_faceted.png", plot = last_plot(), device = NULL, path = sal_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#Chlorophyll

g.chlor.by.site.faceted <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanChlor, group= interaction(region, Site), col = interaction(region, Site))) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    )
    # breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    # labels = c(
    #   "kod.AOF1" = "Kodiak",
    #   "pws.ROK1" = "Prince William Sound",
    #   "kbay.SSF1" = "Kachemak Bay"
    # )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Chlorophyll (RFU)") +
  theme_cowplot() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # legend.position = c(0.01, 0.2),
        # legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.chlor.by.site.faceted

ggsave("Chlorophyll_by_site_faceted.png", plot = last_plot(), device = NULL, path = chlor_plots,
       scale = 1, width = 12, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Turbidity
g.turb.by.site.faceted <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanTurb, group= Site, col = Site)) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Turbidity (FNU)") +
  theme_cowplot() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # legend.position = c(0.01, 0.2),
        legend.title = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.turb.by.site.faceted

ggsave("Turbidity_by_site_faceted.png", plot = last_plot(), device = NULL, path = turb_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#PAR

g.par.by.site.faceted <- ggplot() +
  geom_line(data = par_by_date, aes(x = date, y = MeanPAR, group= Site, col = Site)) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "PAR") +
  theme_cowplot() +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        # legend.position = c(0.01, 0.2),
        legend.title = element_blank(),
        # legend.position = "none",
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.par.by.site.faceted

ggsave("PAR_by_site_faceted.png", plot = last_plot(), device = NULL, path = par_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

# Plots by region ---------------------------------------------------------

g.temp.by.region <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanTemp, group = region, col = region)) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(0,16), 
    expand = c (0, 0)) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
  labels = c("kod" = "Kodiak",
             "pws" = "Prince William Sound",
             "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.9),
        legend.title = element_blank()
  )

g.temp.by.region

ggsave("Temp_by_region.png", plot = last_plot(), device = NULL, path = temp_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#Salinity

g.sal.by.region <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanSal, group = region, col = region)) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Salinity",
    limits = c(24,34),
    expand = c (0, 0)) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.1),
        legend.title = element_blank()
  )

g.sal.by.region

ggsave("Salinity_by_region.png", plot = last_plot(), device = NULL, path = sal_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Chlorophyll

g.chlor.by.region <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanChlor, group = region, col = region)) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Chlorophyll (RFU)",
    limits = c(0,6),
    expand = c (0, 0)) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.9),
        legend.title = element_blank()
  )

g.chlor.by.region

ggsave("Chlorophyll_by_region.png", plot = last_plot(), device = NULL, path = chlor_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Turbidity

g.turb.by.region <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanTurb, group = region, col = region)) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Turbidity (FNU)",
    limits = c(0,16), 
    expand = c (0, 0)) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.9),
        legend.title = element_blank()
  )

g.turb.by.region

ggsave("Turbidity_by_region.png", plot = last_plot(), device = NULL, path = turb_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

# Plots by region, faceted by region --------------------------------------

#Temperature

g.temp.by.region.facet <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanTemp, group = region, col = region)) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Temperature (°C)",
    limits = c(0,16),
    breaks = seq(0, 16, by = 4),
    expand = c (0, 0),) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        # legend.position = c(0.65, 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.temp.by.region.facet

ggsave("Temp_by_region_faceted.png", plot = last_plot(), device = NULL, path = temp_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#Salinity

g.sal.by.region.facet <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanSal, group = region, col = region)) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Salinity",
    limits = c(24,34),
    breaks = seq(24, 34, by = 2),
    expand = c (0, 0),) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        # legend.position = c(0.65, 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.sal.by.region.facet

ggsave("Salinity_by_region_faceted.png", plot = last_plot(), device = NULL, path = sal_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Chlorophyll

g.chlor.by.region.facet <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanChlor, group = region, col = region)) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Chlorophyll (RFU)",
    limits = c(0,12),
    breaks = seq(0, 12, by = 2),
    expand = c (0, 0),) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        # legend.position = c(0.65, 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.chlor.by.region.facet

ggsave("Chlorophyll_by_region_faceted.png", plot = last_plot(), device = NULL, path = chlor_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Turbidity

g.turb.by.region.facet <- ggplot() +
  geom_line(data = exo_by_region, aes(x = Date, y = MeanTurb, group = region, col = region)) +
  facet_wrap(~region, ncol = 1,
             labeller = as_labeller(c(
               "kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay"
             ))) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date(max(exo_by_date$Date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(exo_by_date$Date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Turbidity (FNU)",
    limits = c(0,16),
    breaks = seq(0, 16, by = 4),
    expand = c (0, 0),) +
  scale_color_manual(
    values = c(
      # Kodiak shades of blue
      "kod" = "#377EB8", "pws" = "#4DAF4A", "kbay" = "#E41A1C"),
    labels = c("kod" = "Kodiak",
               "pws" = "Prince William Sound",
               "kbay" = "Kachemak Bay")
  ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        # legend.position = c(0.65, 0.5),
        legend.position = "none",
        legend.title = element_blank(),
        strip.background = element_rect(fill = "white", color = "black", size = 1),  # Thicker border
        strip.text = element_text(color = "black")
  )

g.turb.by.region.facet

ggsave("Turb_by_region_faceted.png", plot = last_plot(), device = NULL, path = turb_plots,
       scale = 1, width = 10, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

# Graphs for presentations ------------------------------------------------


g.temp.combined <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, Site), col = interaction(region, Site))) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date("2024-11-01"), by = "1 month"), 
               limits = as.Date(c("2024-01-01", "2024-11-01")),
               expand = c(0.01,0)) +
  scale_y_continuous(
    limits = c(0,16), 
    expand = c (0, 0),
    name = "Temperature (°C)") +
  #   sec.axis = sec_axis(~ . * 9/5 + 32, name = "Temperature (°F)") # Conversion from °C to °F
  # ) +
  theme_cowplot() +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    # legend.position = c(0.01, 0.9),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g.temp.combined


g.sal.combined <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanSal, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date("2024-11-01"), by = "1 month"), 
               limits = as.Date(c("2024-01-01", "2024-11-01")),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Salinity",
    limits = c(24,34),
    expand = c (0, 0)) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.2),
        legend.title = element_blank()
  )

g.sal.combined

g.region.temp.sal <- (g.temp.combined/g.sal.combined +
  plot_layout(widths = c(4, 4)))

g.region.temp.sal

ggsave("Temperature_salinity_by_region.png", plot = last_plot(), device = NULL, path = dir.outputs,
       scale = 1, width = 5, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

#Chlorophyll

g.chlor.combined <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanChlor, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date("2024-11-01"), by = "1 month"), 
               limits = as.Date(c("2024-01-01", "2024-11-01")),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Chlorophyll (RFU)") +
  theme_cowplot() +
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    # legend.position = c(0.01, 0.9),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g.chlor.combined


#Turbidity
g.turb.combined <- ggplot() +
  geom_line(data = exo_by_date, aes(x = Date, y = MeanTurb, group= interaction(region, Site), col = interaction(region, Site))) +
  scale_color_manual(
    name = "Region",
    values = c(
      # Kodiak shades of blue
      "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
      # PWS shades of green
      "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
      # KAC shades of red
      "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
    ),
    breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
    labels = c(
      "kod.AOF1" = "Kodiak",
      "pws.ROK1" = "Prince William Sound",
      "kbay.SSF1" = "Kachemak Bay"
    )
  ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = seq(as.Date("2024-01-01"), as.Date("2024-11-01"), by = "1 month"), 
               limits = as.Date(c("2024-01-01", "2024-11-01")),
               expand = c(0.01,0)) +
  scale_y_continuous(
    name = "Turbidity (FNU)") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.position = c(0.01, 0.85),
        legend.title = element_blank()
  )


g.turb.combined



g.region.chlor.turb <- (g.chlor.combined/g.turb.combined +
                        plot_layout(widths = c(4, 4)))

g.region.chlor.turb

ggsave("Chlorophyll_turbidity_by_region.png", plot = last_plot(), device = NULL, path = dir.outputs,
       scale = 1, width = 5, height = 5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()

# Hobo plots --------------------------------------------------------------

  
g.hobo.by.date <- ggplot() +
  geom_line(data = hobo_by_date, aes(x = date, y = MeanTemp, group= site, col = site)) +
  # geom_point(data = rbr_3M_by_date, aes(x = Date, y = MeanTemp, group= interaction(region, site), col = interaction(region, site))) +
  # scale_color_manual(
  #   name = "Region",
  #   values = c(
  #     # Kodiak shades of blue
  #     "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
  #     # PWS shades of green
  #     "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
  #     # KAC shades of red
  #     "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
  #   ),
  #   breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
  #   labels = c(
  #     "kod.AOF1" = "Kodiak",
  #     "pws.ROK1" = "Prince William Sound",
  #     "kbay.SSF1" = "Kachemak Bay"
  #   )
  # ) +
  scale_x_date(date_labels = "%b %Y", 
               date_breaks = "1 month",
               breaks = as.Date(c("2024-01-01", max(hobo_by_date$date)), by = "1 month"), 
               limits = as.Date(c("2024-01-01", max(hobo_by_date$date))),
               expand = c(0.01,0)) +
  scale_y_continuous(
    limits = c(0,16), 
    expand = c (0, 0),
    name = "Temperature (°C)") +
    # sec.axis = sec_axis(~ . * 9/5 + 32, name = "Temperature (°F)") # Conversion from °C to °F
  # ) +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = c(0.9, 0.4),
        # axis.text.x = element_blank(),
        legend.title = element_blank()
  )

g.hobo.by.date

ggsave("Hobo_temp_by_site.png", plot = last_plot(), device = NULL, path = temp_plots,
       scale = 1, width = 10, height = 3.5, units = "in",
       dpi = 600, limitsize = TRUE);dev.off()


#Hobo Descriptive statistics

hobo.temp.by.region <- hobo_data %>% group_by(region) %>% 
  summarise(mean.temp = mean(Temp, na.rm = TRUE), sd.temp = sd(temp, na.rm = TRUE))



#add columns with mean and sd temp in F

temperature.by.region$mean.temp.f <- temperature.by.region$mean.temp * 9/5 + 32
temperature.by.region$sd.temp.f <- temperature.by.region$sd.temp * 9/5 + 32

#Descriptive stats for just time when oysters were in water

temperature.by.region.summer <- exo_dataI %>% group_by(region) %>% filter(Date > "2024/05/31" & Date < "2024/09/01") %>% 
  summarise(mean.temp = mean(Temp_C, na.rm = TRUE), sd.temp = sd(Temp_C, na.rm = TRUE),
            mean.sal = mean(Sal_PSU, na.rm = TRUE), sd.sal = sd(Sal_PSU, na.rm = TRUE),
            mean.turb = mean(Turbidity_FNU, na.rm = TRUE), sd.turb = sd(Turbidity_FNU, na.rm = TRUE))

#add columns with mean and sd temp in F

temperature.by.region.summer$mean.temp.f <- temperature.by.region.summer$mean.temp * 9/5 + 32
temperature.by.region.summer$sd.temp.f <- temperature.by.region.summer$sd.temp * 9/5 + 32


#Combined EXO, Hobo, and RBR plots for temperature

for (Site in unique(exo_by_date$Site)) {
  
  
  pdf(file = file.path(temp_plots, paste0("Time_Series_", Site, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for temperature
    exo_site_data <- exo_by_date[exo_by_date$Site == Site, ]
    threeM_rbr_site_data <- rbr_3M_by_date[rbr_3M_by_date$site == Site, ]
    oneM_rbr_site_data <- rbr_1M_by_date[rbr_1M_by_date$site == Site, ]
    hobo_site_data <- hobo_by_date[hobo_by_date$site == Site, ]
    
    # Create the time series plot
    p <- ggplot() +
      geom_line(data = exo_site_data, aes(x = Date, y = MeanTemp, col = "EXO")) +
      geom_line(data = hobo_site_data, aes(x = date, y = MeanTemp, col = "HOBO")) +
      geom_point(data = oneM_rbr_site_data, aes(x = Date, y = MeanTemp, col = "3M RBR")) +
      geom_point(data = threeM_rbr_site_data, aes(x = Date, y = MeanTemp, col = "1M RBR")) +
      labs(
        title = paste("Temperature Time Series at", Site),
        x = "Date",
        y = "Temperature (°C)"
      ) +
      scale_color_manual(values = c("EXO" = "blue", "3M RBR" = "blue", "1M RBR" = "red", "HOBO" = "red")) +
      scale_x_date(date_labels = "%b %Y",
                   date_breaks = "1 month",
                   breaks = seq(as.Date("2023-11-01"), as.Date("2025-04-28"), by = "1 month"),
                   limits = as.Date(c("2023-11-01", "2025-04-28")),
                   expand = c(0.01,0)) +
      theme_cowplot() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            axis.title.x = element_blank(),
            legend.position = c(0.01, 0.9),
            legend.title = element_blank()
      )
    
    # Print the plot to the PDF
    print(p)
    
  # Close the PDF file
  dev.off()
}


#Combined EXO and RBR plots for salinity

for (Site in unique(exo_by_date$Site)) {
  
  
  pdf(file = file.path(sal_plots, paste0("Time_Series_", Site, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for salinity
  exo_site_data <- exo_by_date[exo_by_date$Site == Site, ]
  threeM_rbr_site_data <- rbr_3M_by_date[rbr_3M_by_date$site == Site, ]
  oneM_rbr_site_data <- rbr_1M_by_date[rbr_1M_by_date$site == Site, ]
  
  # Create the time series plot
  p <- ggplot() +
    geom_line(data = exo_site_data, aes(x = Date, y = MeanSal, col = "EXO")) +
    geom_point(data = oneM_rbr_site_data, aes(x = Date, y = MeanSal, col = "3M RBR")) +
    geom_point(data = threeM_rbr_site_data, aes(x = Date, y = MeanSal, col = "1M RBR")) +
    labs(
      title = paste("Salinity Time Series at", Site),
      x = "Date",
      y = "Salinity"
    ) +
    scale_color_manual(values = c("EXO" = "blue", "3M RBR" = "blue", "1M RBR" = "red")) +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = "1 month",
                 breaks = seq(as.Date("2023-11-01"), as.Date("2025-04-28"), by = "1 month"),
                 limits = as.Date(c("2023-11-01", "2025-04-28")),
                 expand = c(0.01,0)) +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_blank(),
          legend.position = c(0.01, 0.9),
          legend.title = element_blank()
    )
  
  # Print the plot to the PDF
  print(p)
  
  # Close the PDF file
  dev.off()
}


#Combined EXO, Hobo, and RBR plots for salinity

for (Site in unique(exo_by_date$Site)) {
  
  
  pdf(file = file.path(sal_plots, paste0("Raw_Time_Series_", Site, ".pdf")), width = 8, height = 6)
  
  # Create a plot for each site for salinity
  exo_site_data <- exo_by_date[exo_by_date$Site == Site, ]
  threeM_rbr_site_data <- rbr_3M_by_date[rbr_3M_by_date$site == Site, ]
  oneM_rbr_site_data <- rbr_1M_by_date[rbr_1M_by_date$site == Site, ]
  hobo_site_data <- raw_hobo_by_date[raw_hobo_by_date$site == Site, ]
  
  # Create the time series plot
  p <- ggplot() +
    geom_line(data = exo_site_data, aes(x = Date, y = MeanSal, col = "EXO")) +
    geom_line(data = hobo_site_data, aes(x = date, y = MeanSal, col = "HOBO")) +
    geom_point(data = oneM_rbr_site_data, aes(x = Date, y = MeanSal, col = "1M RBR")) +
    geom_point(data = threeM_rbr_site_data, aes(x = Date, y = MeanSal, col = "3M RBR")) +
    labs(
      title = paste("Salinity Time Series at", Site),
      x = "Date",
      y = "Salinity"
    ) +
    scale_color_manual(values = c("EXO" = "blue", "3M RBR" = "blue", "1M RBR" = "red", "HOBO" = "red")) +
    scale_x_date(date_labels = "%b %Y",
                 date_breaks = "1 month",
                 breaks = seq(as.Date("2023-11-01"), as.Date("2025-04-28"), by = "1 month"),
                 limits = as.Date(c("2023-11-01", "2025-04-28")),
                 expand = c(0.01,0)) +
    theme_cowplot() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.title.x = element_blank(),
          legend.position = c(0.01, 0.9),
          legend.title = element_blank()
    )
  
  # Print the plot to the PDF
  print(p)
  
  # Close the PDF file
  dev.off()
}


# Oyster Growth plots ------------------------------------------------------------


#Subadults

g.sub.shell.by.site <- ggplot(data = sub_data, aes(site, Shell_length_mm)) +
  geom_boxplot() +  # avoids duplicating outliers with points
  geom_jitter(width = 0.2, alpha = 0.5) +  # spread the points a bit
  theme_minimal()

g.sub.shell.by.site  

g.sub.weight.by.site <- ggplot(data = sub_data, aes(site, Whole_weight_g)) +
  geom_boxplot() +  # avoids duplicating outliers with points
  geom_jitter(width = 0.2, alpha = 0.5) +  # spread the points a bit
  theme_minimal()

g.sub.weight.by.site  

g.sub.meat.by.site <- ggplot(data = sub_data, aes(site, Meat_weight_g)) +
  geom_boxplot() +  # avoids duplicating outliers with points
  geom_jitter(width = 0.2, alpha = 0.5) +  # spread the points a bit
  theme_minimal()

g.sub.meat.by.site  


#Juveniles

g.juv.shell.by.site <- ggplot(data = juv_data, aes(site, Shell_length_mm)) +
  geom_boxplot() +  # avoids duplicating outliers with points
  geom_jitter(width = 0.2, alpha = 0.5) +  # spread the points a bit
  theme_minimal()

g.juv.shell.by.site  

g.juv.weight.by.site <- ggplot(data = juv_data, aes(site, Whole_weight_g)) +
  geom_boxplot() +  # avoids duplicating outliers with points
  geom_jitter(width = 0.2, alpha = 0.5) +  # spread the points a bit
  theme_minimal()

g.juv.weight.by.site  

g.juv.meat.by.site <- ggplot(data = juv_data, aes(site, Meat_weight_g)) +
  geom_boxplot() +  # avoids duplicating outliers with points
  geom_jitter(width = 0.2, alpha = 0.5) +  # spread the points a bit
  theme_minimal()

g.juv.meat.by.site  

  
                               
#   scale_color_manual(
#     name = "Region",
#     values = c(
#       # Kodiak shades of blue
#       "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
#       # PWS shades of green
#       "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
#       # KAC shades of red
#       "kbay.SSF1" = "#E41A1C", "kbay.BCF1" = "#D95F02", "kbay.MIO1" = "#FC9272"
#     ),
#     breaks = c("kod.AOF1", "pws.ROK1", "kbay.SSF1"), # Choose one representative site per region for legend
#     labels = c(
#       "kod.AOF1" = "Kodiak",
#       "pws.ROK1" = "Prince William Sound",
#       "kbay.SSF1" = "Kachemak Bay"
#     )
#   ) +
#   scale_x_date(date_labels = "%b %Y", 
#                date_breaks = "1 month",
#                breaks = seq(as.Date("2024-01-01"), as.Date("2025-05-01"), by = "1 month"), 
#                limits = as.Date(c("2024-01-01", "2025-05-01")),
#                expand = c(0.01,0)) +
#   scale_y_continuous(
#     limits = c(0,16), 
#     expand = c (0, 0),
#     name = "Temperature (°C)") +
#   #   sec.axis = sec_axis(~ . * 9/5 + 32, name = "Temperature (°F)") # Conversion from °C to °F
#   # ) +
#   theme_cowplot() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
#     legend.position = c(0.01, 0.9),
#     # axis.text.x = element_blank(),
#     legend.title = element_blank()
#   )
# 
# g.temp.by.site
# 
# ggsave("Temperature_by_site.png", plot = last_plot(), device = NULL, path = temp_plots,
#        scale = 1, width = 10, height = 3.5, units = "in",
#        dpi = 600, limitsize = TRUE);dev.off()
# 
