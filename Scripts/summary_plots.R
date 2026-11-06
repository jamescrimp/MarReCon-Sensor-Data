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


#Save working directory path as an object
wd <- getwd()

dir.data <- file.path(
  "I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management"
)

#file path 
dir.dat <- file.path(dir.data, "QAQC sensor data")  
dir.csv <- file.path(dir.data, "CSVs")  
dir.outputs <- file.path(dir.data, "Outputs")

#Add in EXO
exo_dataI <- read.csv(file.path(dir.dat, "df_interpolated_05NOV26_SG.csv"))

df1 <- exo_dataI

# Helper: parse mixed "date only" / "date + time" strings as UTC
parse_utc <- function(x) {
  parse_date_time(as.character(x),
                  orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                  tz = "UTC", quiet = TRUE)
}

# ---- EXO ----
exo <- read.csv(file.path(dir.dat, "df_interpolated_05NOV26_SG.csv"))
exo$TIME_UTC <- parse_utc(exo$TIME_UTC)
exo$DATE     <- as_date(exo$DATE)

# ---- HOBO ----
hobo <- read.csv(file.path(dir.dat, "HOBO_06NOV26_SG.csv"))
colnames(hobo) <- toupper(colnames(hobo))
hobo <- hobo %>% rename(TEMP_C = TEMP)
hobo$TIME_UTC <- parse_utc(hobo$TIME_UTC)
hobo$DATE     <- as_date(hobo$DATE)

# ---- Check for anything that still failed to parse ----
sum(is.na(exo$TIME_UTC))
sum(is.na(hobo$TIME_UTC))

# If any remain, inspect the raw strings:
raw_hobo <- read.csv(file.path(dir.dat, "HOBO_06NOV26_SG.csv"),
                     colClasses = "character")
names(raw_hobo) <- toupper(names(raw_hobo))
head(raw_hobo$TIME_UTC[is.na(hobo$TIME_UTC)], 20)

# ---- Daily means ----

# # Make sure EXO and HOBO use the same temperature column name
# if ("TEMP" %in% names(exo)) exo <- exo %>% rename(TEMP_C = TEMP)

# Recompute daily means (TEMP_C and SAL_PSU)
daily_mean <- function(df) {
  df %>%
    mutate(DATE = as_date(TIME_UTC)) %>%
    group_by(SITE, DATE) %>%
    summarise(across(any_of(c("TEMP_C", "SAL_PSU")),
                     ~ mean(.x, na.rm = TRUE)),
              .groups = "drop")
}

exo_daily  <- daily_mean(exo)
hobo_daily <- daily_mean(hobo)

# Generic plotting function
plot_var_by_site <- function(var, ylab) {
  dat <- bind_rows(
    exo_daily  %>% mutate(SOURCE = "EXO"),
    hobo_daily %>% mutate(SOURCE = "HOBO")
  ) %>%
    filter(!is.na(.data[[var]]), !is.nan(.data[[var]]), !is.na(DATE)) %>%
    mutate(YEAR = year(DATE))
  
  sites <- unique(dat$SITE)
  
  plots <- lapply(sites, function(site) {
    dat %>%
      filter(SITE == site) %>%
      ggplot(aes(x = DATE, y = .data[[var]], color = SOURCE)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 0.8, alpha = 0.6) +
      facet_wrap(~ YEAR, scales = "free_x", ncol = 1) +
      scale_x_date(date_labels = "%b") +
      scale_color_manual(values = c(EXO = "#1b9e77", HOBO = "#d95f02")) +
      labs(title = paste("Daily mean", ylab, ":", site),
           x = NULL, y = ylab, color = "Sensor") +
      theme_bw() +
      theme(legend.position = "bottom")
  })
  names(plots) <- sites
  
  # Save
  for (s in sites) {
    n_years <- n_distinct(dat$YEAR[dat$SITE == s])
    ggsave(file.path(dir.outputs, paste0("daily_", var, "_", s, ".png")),
           plots[[s]], width = 9, height = 3 * n_years, dpi = 300)
  }
  plots
}

plots_temp <- plot_var_by_site("TEMP_C", "Temperature (°C)")
plots_sal  <- plot_var_by_site("SAL_PSU", "Salinity (PSU)")

# View
names(plots_temp)
plots_temp[["ROK1"]]


#Export as PDFs

pdf(file.path(dir.outputs, "daily_TEMP_C_all_sites.pdf"), width = 9, height = 10)
for (p in plots_temp) print(p)
dev.off()

pdf(file.path(dir.outputs, "daily_SAL_PSU_all_sites.pdf"), width = 9, height = 10)
for (p in plots_sal) print(p)
dev.off()










#################################################
daily_mean <- function(df) {
  df %>%
    mutate(DATE = as_date(TIME_UTC)) %>%
    group_by(SITE, DATE) %>%
    summarise(across(any_of(c("TEMP_C", "SAL_PSU")),
                     ~ mean(.x, na.rm = TRUE)),
              .groups = "drop")
}

exo_daily  <- daily_mean(exo)
hobo_daily <- daily_mean(hobo)


# Combine daily means (SAL_PSU only), tagged by instrument
sal_daily <- bind_rows(
  exo_daily  %>% mutate(SOURCE = "EXO"),
  hobo_daily %>% mutate(SOURCE = "HOBO")
) %>%
  filter(!is.na(SAL_PSU), !is.nan(SAL_PSU)) %>%
  mutate(YEAR = year(DATE))

sal_daily <- bind_rows(
  exo_daily  %>% mutate(SOURCE = "EXO"),
  hobo_daily %>% mutate(SOURCE = "HOBO")
) %>%
  filter(!is.na(SAL_PSU), !is.nan(SAL_PSU), !is.na(DATE)) %>%
  mutate(YEAR = year(DATE))

# One plot per site, faceted by year
plot_site <- function(site) {
  sal_daily %>%
    filter(SITE == site) %>%
    ggplot(aes(x = DATE, y = SAL_PSU, color = SOURCE)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.8, alpha = 0.6) +
    facet_wrap(~ YEAR, scales = "free_x", ncol = 1) +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    scale_color_manual(values = c(EXO = "#1b9e77", HOBO = "#d95f02")) +
    labs(title = paste("Daily mean salinity:", site),
         x = NULL, y = "Salinity (PSU)", color = "Sensor") +
    theme_bw() +
    theme(legend.position = "bottom")
}

sites <- unique(sal_daily$SITE)
plots <- setNames(lapply(sites, plot_site), sites)

# View one
plots[["ROK1"]]

# Save all
for (s in sites) {
  ggsave(file.path(dir.outputs, paste0("daily_SAL_PSU_", s, ".png")),
         plots[[s]], width = 9, height = 3 * n_distinct(sal_daily$YEAR[sal_daily$SITE == s]),
         dpi = 300)
}












#change time to POSIXct
df1$TIME_UTC <- as.POSIXct(df1$TIME_UTC, format="%Y-%m-%d %H:%M")
df1$DATE <- as_date(df1$DATE)

#Correct TIME_UTC values that didnt parse correctly
df_try <- df1 %>%
  arrange(SITE, TIME_UTC) %>%
  group_by(SITE) %>%
  mutate(
    # Convert TIME_UTC to numeric (seconds since epoch)
    time_num = as.numeric(TIME_UTC),
    
    # Interpolate only missing values
    time_num = ifelse(is.na(time_num), NA, time_num),
    time_num_interp = imputeTS::na_interpolation(time_num, option = "linear"),
    
    # Convert back to POSIXct
    TIME_UTC = as.POSIXct(time_num_interp, origin = "1970-01-01", tz = "UTC")
  ) %>%
  select(-time_num, -time_num_interp) %>%
  ungroup()

#Convert back to df1
df1 <- df_try

#Add in HOBO
hobo_dataI <- read.csv(file.path(dir.dat, "HOBO_06NOV26_SG.csv"))

colnames(hobo_dataI) <- toupper(colnames(hobo_dataI))

hobo_dataI$TIME_UTC <- as.POSIXct(hobo_dataI$TIME_UTC, format="%Y-%m-%d %H:%M")
hobo_dataI$DATE <- as_date(hobo_dataI$DATE)

hobo_dataI$TIME_UTC <- parse_date_time(
  hobo_dataI$TIME_UTC,
  orders = c("Ymd HMS", "Ymd HM", "Ymd"),
  tz = "UTC"
)

bad <- hobo_dataI %>%
  mutate(parsed = as.POSIXct(TIME_UTC, format = "%Y-%m-%d %H:%M", tz = "UTC")) %>%
  filter(is.na(parsed) & !is.na(TIME_UTC))

head(bad$TIME_UTC, 20)
unique(nchar(as.character(hobo_dataI$TIME_UTC))) 

df_try <- hobo_dataI %>%
  arrange(SITE, TIME_UTC) %>%
  group_by(SITE) %>%
  mutate(
    # Convert TIME_UTC to numeric (seconds since epoch)
    time_num = as.numeric(TIME_UTC),
    
    # Interpolate only missing values
    time_num = ifelse(is.na(time_num), NA, time_num),
    time_num_interp = imputeTS::na_interpolation(time_num, option = "linear"),
    
    # Convert back to POSIXct
    TIME_UTC = as.POSIXct(time_num_interp, origin = "1970-01-01", tz = "UTC")
  ) %>%
  select(-time_num, -time_num_interp) %>%
  ungroup()

df1 <- df_try

hobo <- hobo_dataI
  
exo <- exo_dataI


