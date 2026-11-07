<<<<<<< HEAD
#Upload and QAQC HOBO data files
#Updated: Nov 5 2026
#Sierra Greene 
#

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

#My google drive link
dir.data <- "I:/Shared drives/Mariculture ReCon/Data/Sensor Data Management/Raw data from sensors/HOBO"
stopifnot(dir.exists(dir.data))

# ---- File lists --------------------------------------------------------
all_csvs <- list.files(dir.data, pattern = "\\.csv$", recursive = TRUE,
                       full.names = TRUE, ignore.case = TRUE)

exclude <- c("NOT calibrated CSVs", "RAW HOBO files", "Read_me", "Misc", "Error_files",
             "Hidden", "CSV not broken up", "AN IMPORTANT Read me!!", "3m")  # "3m" also covers SBO1_3m, Temperature CSVs_3m
all_csvs <- all_csvs[!Reduce(`|`, lapply(exclude, grepl, x = all_csvs, fixed = TRUE))]

cond_files <- all_csvs[!grepl("Temperature CSVs", all_csvs, fixed = TRUE)]
temp_files <- all_csvs[basename(dirname(all_csvs)) == "Temperature CSVs" &
                         !grepl("Endpoint calibrated CSVs", all_csvs, fixed = TRUE)]

stopifnot(length(cond_files) > 0, length(temp_files) > 0)
cat(length(cond_files), "conductivity files;", length(temp_files), "temperature files\n")
table(basename(dirname(dirname(temp_files))))   # files per site folder

# ---- Reader (header on row 2; first prefix must be "Date Time") ---------
read_hobo <- function(f, prefixes, skip = 1) {
  hdr <- trimws(names(read.csv(f, skip = skip, nrows = 0, check.names = FALSE)))
  idx <- vapply(prefixes, function(p) which(startsWith(hdr, p))[1], integer(1))  # NA if none
  
  if (anyNA(idx)) warning(basename(f), ": no column starting with: ",
                          paste(prefixes[is.na(idx)], collapse = ", "),
                          call. = FALSE, immediate. = TRUE)
  
  cc <- rep("NULL", length(hdr)); cc[na.omit(idx)] <- "character"   # skip all other columns
  df <- read.csv(f, skip = skip, colClasses = cc, check.names = FALSE)
  names(df) <- names(idx)[match(which(cc != "NULL"), idx)]
  
  df[setdiff(prefixes, names(df))] <- NA_character_   # missing prefixes -> NA
  df <- df[prefixes]
  df[-1] <- lapply(df[-1], as.numeric)
  
  mutate(df, source_file = basename(f),
         site_folder = basename(dirname(dirname(f))), .before = 1)
}

# ---- Time parsing (exact formats, year-range check) ---------------------
formats <- c("%m/%d/%Y %H:%M",      # 4-digit year, no seconds
             "%m/%d/%y %H:%M:%S")   # 2-digit year, seconds

parse_times <- function(x, formats, yr_range = c(2023, 2026)) {
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  for (fmt in formats) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    p  <- as.POSIXct(strptime(x[todo], fmt, tz = "UTC"))
    ok <- !is.na(p) & year(p) >= yr_range[1] & year(p) <= yr_range[2]
    out[which(todo)[ok]] <- p[ok]
  }
  out
}

# Run once on the raw text column (it can't re-parse a POSIXct)
add_times <- function(df) {
  df %>% mutate(Time_raw = Time_UTC,
                Time_UTC = parse_times(Time_raw, formats),
                date = as.Date(Time_UTC), year = year(Time_UTC))
}

# ---- Conductivity data --------------------------------------------------
hobo_all <- bind_rows(lapply(cond_files, read_hobo,
                             prefixes = c("Date Time", "High Range", "Temp", "Specific Conductance", "Salinity"))) %>%
  rename(Time_UTC = `Date Time`, Cond_uS.cm = `High Range`,
         SpCond_uS.cm = `Specific Conductance`, Sal_PSU = Salinity) %>%
  mutate(Site = str_split_i(source_file, "_", 2)) %>%
  drop_na(Sal_PSU) %>%
  add_times()

# ---- Temperature data ---------------------------------------------------
hobo_temp <- bind_rows(lapply(temp_files, read_hobo, prefixes = c("Date Time", "Temp"))) %>%
  rename(Time_UTC = `Date Time`) %>%
  mutate(Site = str_split_i(source_file, "_", 2)) %>%
  add_times()

# ---- Checks (all should be clean) ----------------------------------------
for (d in list(hobo_all, hobo_temp)) {
  print(d %>% filter(is.na(Site)) %>% distinct(source_file))       # 0 rows = good
  print(sort(unique(d$Site)))                                      # expected sites only
  print(d %>% filter(Site != site_folder) %>% count(Site, site_folder))  # 0 rows = names match folders
  cat("Unparsed times:", sum(is.na(d$Time_UTC)), "\n")             # want 0
  print(count(d, year))                                            # only 2023-2026
}


#check for duplicated that will mess up joining
# Check for duplicates in hobo_temp
temp_dup <- hobo_temp %>%
  group_by(Time_UTC, Site) %>%
  filter(n() > 1) %>%
  arrange(Time_UTC, Site)
#This seems like an acceptable amount of overlap

# Check for duplicates in hobo_sal
hobo_dup <- hobo_all %>%
  group_by(Time_UTC, Site) %>%
  filter(n() > 1) %>%
  arrange(Time_UTC, Site)
#Quite a few here, lets do some QAQC to reduce

#Remove rows where salinity is outside of expectations
sal_min <- 10
sal_max <- 35

hobo_all <- hobo_all %>%
  filter(is.na(Sal_PSU) | (Sal_PSU >= sal_min & Sal_PSU <= sal_max))

#duplicates are from an overlap in new sensors out and old ones being pulled. Not sure how to access which is which- im going to delete all duplicates
#
hobo_temp <- hobo_temp %>% distinct(Time_UTC, Site, .keep_all = TRUE)

# Select relevant columns from hobo_data
hobo_sal <- hobo_all %>%
  select(Time_UTC, Site, Sal_PSU, Cond_uS.cm, SpCond_uS.cm)

# Join to hobo_temp where datetime and site match
hobo_data <- hobo_temp %>%
  left_join(hobo_sal, by = c("Time_UTC", "Site"))

# Timezone fix ------------------------------------------------------------

#Fix dates/sites that were recorded in AKT instead of UTC
#MIO1: 11/3 - 4/6
#BCF1: 11/3/25 - 4/4/26
#SSF1: 11/4 - 4/7 
#

timezone <- hobo_data

akt_windows <- tibble::tribble(
  ~Site,  ~start,        ~end,
  "MIO1", "2025-11-03",  "2026-04-06",
  "BCF1", "2025-11-03",  "2026-04-04",
  "SSF1", "2025-11-04",  "2026-04-07"
) %>%
  mutate(across(c(start, end), as.Date))

timezone <- timezone %>%
  mutate(date_raw = as.Date(Time_UTC)) %>%      # clock date as recorded
  left_join(akt_windows, by = "Site") %>%
  mutate(
    is_akt   = !is.na(start) & date_raw >= start & date_raw <= end,
    Time_UTC = if_else(
      is_akt,
      with_tz(force_tz(Time_UTC, "America/Anchorage"), "UTC"),
      Time_UTC
    ),
    date = as.Date(Time_UTC),
    year = year(Time_UTC)
  ) %>%
  select(-start, -end, -date_raw)

# Sanity check: how many rows were shifted, per site
timezone %>% count(Site, is_akt) %>% filter(Site %in% akt_windows$Site)
#Looks good

hobo_data <- timezone

#Remove columns we dont need
hobo_data <- hobo_data %>% 
  select(-1, -2, -6, -12)

######################################################

# Start QAQC --------------------------------------------------------------
#remove points where sensors def weren't in the water
hobo_data <- hobo_data %>%filter(Cond_uS.cm > 1000)
#this is low, but accounts for errors where cond channel didn't work but temp did 


#Remove points outside of biological expectations
# Salinity over 35
hobo_data$Sal_PSU[hobo_data$Sal_PSU > 35] <- NA
#Conductivity over 50000
hobo_data$Cond_uS.cm[hobo_data$Cond_uS.cm > 50000] <- NA
#Specific Conductivity over 55000
hobo_data$SpCond_uS.cm[hobo_data$SpCond_uS.cm > 55000] <- NA

#hobo_data is the working df of initial QAQC data

#Export this df
write.csv(hobo_data, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\HOBO_data_06NOV26_SG.csv"), row.names = FALSE)

#View by year and site 

# Daily mean/min/max per site and year
daily_summary <- function(d, var) {
  d %>%
    filter(!is.na({{ var }})) %>%
    mutate(date = as.Date(Time_UTC), year = year(Time_UTC)) %>%
    group_by(Site, year, date) %>%
    summarise(mean = mean({{ var }}), min = min({{ var }}), max = max({{ var }}),
              .groups = "drop")
}

plot_daily <- function(daily, ylab, color) {
  ggplot(daily, aes(date, mean)) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = color, alpha = 0.25) +
    geom_line(color = color, linewidth = 0.4) +
    facet_grid(Site ~ year, scales = "free_x") +   # each year gets its own x range
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 10) +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

temp_daily <- daily_summary(hobo_temp, Temp)       # temp loggers (likely the longest record)
sal_daily  <- daily_summary(hobo_all, Sal_PSU)

p_temp <- plot_daily(temp_daily, "Temperature (°C)", "firebrick")
p_sal  <- plot_daily(sal_daily,  "Salinity (PSU)",   "steelblue")

ggsave("temp_by_site_year.png", p_temp, width = 12, height = 2 * n_distinct(temp_daily$Site), dpi = 200, limitsize = FALSE)
ggsave("sal_by_site_year.png",  p_sal,  width = 12, height = 2 * n_distinct(sal_daily$Site),  dpi = 200, limitsize = FALSE)































=======
#Upload and QAQC HOBO data files
#Updated: Nov 5 2026
#Sierra Greene 
#

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

#My google drive link
dir.data <- "I:/Shared drives/Mariculture ReCon/Data/Sensor Data Management/Raw data from sensors/HOBO"
stopifnot(dir.exists(dir.data))

# ---- File lists --------------------------------------------------------
all_csvs <- list.files(dir.data, pattern = "\\.csv$", recursive = TRUE,
                       full.names = TRUE, ignore.case = TRUE)

exclude <- c("NOT calibrated CSVs", "RAW HOBO files", "Read_me", "Misc", "Error_files",
             "Hidden", "CSV not broken up", "AN IMPORTANT Read me!!", "3m")  # "3m" also covers SBO1_3m, Temperature CSVs_3m
all_csvs <- all_csvs[!Reduce(`|`, lapply(exclude, grepl, x = all_csvs, fixed = TRUE))]

cond_files <- all_csvs[!grepl("Temperature CSVs", all_csvs, fixed = TRUE)]
temp_files <- all_csvs[basename(dirname(all_csvs)) == "Temperature CSVs" &
                         !grepl("Endpoint calibrated CSVs", all_csvs, fixed = TRUE)]

stopifnot(length(cond_files) > 0, length(temp_files) > 0)
cat(length(cond_files), "conductivity files;", length(temp_files), "temperature files\n")
table(basename(dirname(dirname(temp_files))))   # files per site folder

# ---- Reader (header on row 2; first prefix must be "Date Time") ---------
read_hobo <- function(f, prefixes, skip = 1) {
  hdr <- trimws(names(read.csv(f, skip = skip, nrows = 0, check.names = FALSE)))
  idx <- vapply(prefixes, function(p) which(startsWith(hdr, p))[1], integer(1))  # NA if none
  
  if (anyNA(idx)) warning(basename(f), ": no column starting with: ",
                          paste(prefixes[is.na(idx)], collapse = ", "),
                          call. = FALSE, immediate. = TRUE)
  
  cc <- rep("NULL", length(hdr)); cc[na.omit(idx)] <- "character"   # skip all other columns
  df <- read.csv(f, skip = skip, colClasses = cc, check.names = FALSE)
  names(df) <- names(idx)[match(which(cc != "NULL"), idx)]
  
  df[setdiff(prefixes, names(df))] <- NA_character_   # missing prefixes -> NA
  df <- df[prefixes]
  df[-1] <- lapply(df[-1], as.numeric)
  
  mutate(df, source_file = basename(f),
         site_folder = basename(dirname(dirname(f))), .before = 1)
}

# ---- Time parsing (exact formats, year-range check) ---------------------
formats <- c("%m/%d/%Y %H:%M",      # 4-digit year, no seconds
             "%m/%d/%y %H:%M:%S")   # 2-digit year, seconds

parse_times <- function(x, formats, yr_range = c(2023, 2026)) {
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  for (fmt in formats) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    p  <- as.POSIXct(strptime(x[todo], fmt, tz = "UTC"))
    ok <- !is.na(p) & year(p) >= yr_range[1] & year(p) <= yr_range[2]
    out[which(todo)[ok]] <- p[ok]
  }
  out
}

# Run once on the raw text column (it can't re-parse a POSIXct)
add_times <- function(df) {
  df %>% mutate(Time_raw = Time_UTC,
                Time_UTC = parse_times(Time_raw, formats),
                date = as.Date(Time_UTC), year = year(Time_UTC))
}

# ---- Conductivity data --------------------------------------------------
hobo_all <- bind_rows(lapply(cond_files, read_hobo,
                             prefixes = c("Date Time", "High Range", "Temp", "Specific Conductance", "Salinity"))) %>%
  rename(Time_UTC = `Date Time`, Cond_uS.cm = `High Range`,
         SpCond_uS.cm = `Specific Conductance`, Sal_PSU = Salinity) %>%
  mutate(Site = str_split_i(source_file, "_", 2)) %>%
  drop_na(Sal_PSU) %>%
  add_times()

# ---- Temperature data ---------------------------------------------------
hobo_temp <- bind_rows(lapply(temp_files, read_hobo, prefixes = c("Date Time", "Temp"))) %>%
  rename(Time_UTC = `Date Time`) %>%
  mutate(Site = str_split_i(source_file, "_", 2)) %>%
  add_times()

# ---- Checks (all should be clean) ----------------------------------------
for (d in list(hobo_all, hobo_temp)) {
  print(d %>% filter(is.na(Site)) %>% distinct(source_file))       # 0 rows = good
  print(sort(unique(d$Site)))                                      # expected sites only
  print(d %>% filter(Site != site_folder) %>% count(Site, site_folder))  # 0 rows = names match folders
  cat("Unparsed times:", sum(is.na(d$Time_UTC)), "\n")             # want 0
  print(count(d, year))                                            # only 2023-2026
}


#check for duplicated that will mess up joining
# Check for duplicates in hobo_temp
temp_dup <- hobo_temp %>%
  group_by(Time_UTC, Site) %>%
  filter(n() > 1) %>%
  arrange(Time_UTC, Site)
#This seems like an acceptable amount of overlap

# Check for duplicates in hobo_sal
hobo_dup <- hobo_all %>%
  group_by(Time_UTC, Site) %>%
  filter(n() > 1) %>%
  arrange(Time_UTC, Site)
#Quite a few here, lets do some QAQC to reduce

#Remove rows where salinity is outside of expectations
sal_min <- 10
sal_max <- 35

hobo_all <- hobo_all %>%
  filter(is.na(Sal_PSU) | (Sal_PSU >= sal_min & Sal_PSU <= sal_max))

#duplicates are from an overlap in new sensors out and old ones being pulled. Not sure how to tell which is which- im going to delete all duplicates
#
hobo_temp <- hobo_temp %>% distinct(Time_UTC, Site, .keep_all = TRUE)

# Select relevant columns from hobo_data
hobo_sal <- hobo_all %>%
  select(Time_UTC, Site, Sal_PSU, Cond_uS.cm, SpCond_uS.cm)

# Join to hobo_temp where datetime and site match
hobo_data <- hobo_temp %>%
  left_join(hobo_sal, by = c("Time_UTC", "Site"))

# Timezone fix ------------------------------------------------------------

#Fix dates/sites that were recorded in AKT instead of UTC
#MIO1: 11/3 - 4/6
#BCF1: 11/3/25 - 4/4/26
#SSF1: 11/4 - 4/7 
#

timezone <- hobo_data

akt_windows <- tibble::tribble(
  ~Site,  ~start,        ~end,
  "MIO1", "2025-11-03",  "2026-04-06",
  "BCF1", "2025-11-03",  "2026-04-04",
  "SSF1", "2025-11-04",  "2026-04-07"
) %>%
  mutate(across(c(start, end), as.Date))

timezone <- timezone %>%
  mutate(date_raw = as.Date(Time_UTC)) %>%      # clock date as recorded
  left_join(akt_windows, by = "Site") %>%
  mutate(
    is_akt   = !is.na(start) & date_raw >= start & date_raw <= end,
    Time_UTC = if_else(
      is_akt,
      with_tz(force_tz(Time_UTC, "America/Anchorage"), "UTC"),
      Time_UTC
    ),
    date = as.Date(Time_UTC),
    year = year(Time_UTC)
  ) %>%
  select(-start, -end, -date_raw)

# Sanity check: how many rows were shifted, per site
timezone %>% count(Site, is_akt) %>% filter(Site %in% akt_windows$Site)
#Looks good

hobo_data <- timezone

#Remove columns we dont need
hobo_data <- hobo_data %>% 
  select(-1, -2, -6, -12)

######################################################

# Start QAQC --------------------------------------------------------------
#remove points where sensors def weren't in the water
hobo_data <- hobo_data %>%filter(Cond_uS.cm > 1000)
#this is low, but accounts for errors where cond channel didn't work but temp did 


#Remove points outside of biological expectations
# Salinity over 35
hobo_data$Sal_PSU[hobo_data$Sal_PSU > 35] <- NA
#Conductivity over 50000
hobo_data$Cond_uS.cm[hobo_data$Cond_uS.cm > 50000] <- NA
#Specific Conductivity over 55000
hobo_data$SpCond_uS.cm[hobo_data$SpCond_uS.cm > 55000] <- NA

#hobo_data is the working df of initial QAQC data

#Export this df
write.csv(hobo_data, file.path("I:\\Shared drives\\Mariculture ReCon\\Data\\Sensor Data Management\\CSVs\\HOBO_data_06NOV26_SG.csv"), row.names = FALSE)

#View by year and site 

# Daily mean/min/max per site and year
daily_summary <- function(d, var) {
  d %>%
    filter(!is.na({{ var }})) %>%
    mutate(date = as.Date(Time_UTC), year = year(Time_UTC)) %>%
    group_by(Site, year, date) %>%
    summarise(mean = mean({{ var }}), min = min({{ var }}), max = max({{ var }}),
              .groups = "drop")
}

plot_daily <- function(daily, ylab, color) {
  ggplot(daily, aes(date, mean)) +
    geom_ribbon(aes(ymin = min, ymax = max), fill = color, alpha = 0.25) +
    geom_line(color = color, linewidth = 0.4) +
    facet_grid(Site ~ year, scales = "free_x") +   # each year gets its own x range
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 10) +
    theme(strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

temp_daily <- daily_summary(hobo_temp, Temp)       # temp loggers (likely the longest record)
sal_daily  <- daily_summary(hobo_all, Sal_PSU)

p_temp <- plot_daily(temp_daily, "Temperature (°C)", "firebrick")
p_sal  <- plot_daily(sal_daily,  "Salinity (PSU)",   "steelblue")

ggsave("temp_by_site_year.png", p_temp, width = 12, height = 2 * n_distinct(temp_daily$Site), dpi = 200, limitsize = FALSE)
ggsave("sal_by_site_year.png",  p_sal,  width = 12, height = 2 * n_distinct(sal_daily$Site),  dpi = 200, limitsize = FALSE)































>>>>>>> Sierra
