#Nutrient upload
#Updated 9/24/2026
#SG
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
library(oce)
library(RSQLite)
library(ggplot2)
library(tibble)
library(tidyverse)

#Start at row 12, only read col 1 - 14
#Read ein csvs in 
#I:\Shared drives\Mariculture ReCon\Data\Kelp, oyster, nutrient, chl\Nutrients\Read into R

#Make sure all files are formatted correctly asnd do not have weird stuff (random words) at the bottom!! 

#Set file path- Sierra
dir.data <- file.path("I:/Shared drives/Mariculture ReCon/Data/Kelp, oyster, nutrient, chl/Nutrients/Read into R")

files <- list.files(dir.data, pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("R_Uploads_Template", basename(files))]

data_upload <- files |>
  set_names(basename(files)) |>
  map(~ read_csv(
    .x,
    skip       = 11,
    col_names  = FALSE,
    col_select = 1:14,
    col_types  = cols(.default = "c")
  ) |>
    filter(if_any(everything(), ~ !is.na(.x)))  
  ) |>
  bind_rows(.id = "source_file")

#there are wonky dates in here, lets see if we can find where they are coming from

data_upload$DATE <- mdy(data_upload$X6)   # month-day-year
#what dates are there?
range(data_upload$DATE, na.rm = TRUE)

#Look for sampling events that have more than 3 replicates
many <- data_upload |>
  filter(!is.na(DATE)) |>
  group_by(X4, DATE) |>
  filter(n() > 3) |>
  ungroup() |>
  arrange(X4, DATE)

many

#Remove columns we dont need
data_all <- data_upload[, c(5, 10:16)]

#Name columns 
#1: SITE
#3: NITRATE
#4: NITRITE
#5: PHOSPHATE
# SILICATE
#6: AMMONIUM
#7: NOX

data_all <- data_all |>
  rename(
    SITE      = X4,
    NITRATE   = X9,
    NITRITE   = X10,
    PHOSPHATE = X11,
    SILICATE  = X12,
    AMMONIUM  = X13,
    NOX  = X14
  )

#Correct SITE names that were entered wrong
data_all <- data_all |>
  mutate(SITE = recode(SITE,
                       "AOF"  = "AOF1",
                       "BCF1F1"  = "BCF1",
                       "ROK11" = "ROK1"
  ))




#Export combined nutrient data w/o QAQC 
write.csv(data_all, file = "I:/Shared drives/Mariculture ReCon/Data/Kelp, oyster, nutrient, chl/Nutrients/Nutrient compiled/nutrient_compiled_noQAQC_24SEPT2026_SG.csv", row.names = FALSE)

#QAQC based on triplicates
#lets make columns for mean, SD, and N
#MAke a new df to save stats to 
df1 <- data_all

vars <-c("NITRATE", "NITRITE", "PHOSPHATE", "SILICATE", "AMMONIUM", "NOX")  

df1 <- df1 |>
  mutate(across(all_of(vars), as.numeric)) |>
  group_by(SITE, DATE) |>
  summarise(
    across(all_of(vars),
           list(mean = ~ mean(.x, na.rm = TRUE),
                sd   = ~ sd(.x, na.rm = TRUE),
                n    = ~ sum(!is.na(.x))),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  )

#Export to drive 
write.csv(df1, file = "I:/Shared drives/Mariculture ReCon/Data/Kelp, oyster, nutrient, chl/Nutrients/Nutrient compiled/nutrient_stats_noQAQC_24SEPT2026_SG.csv", row.names = FALSE)

#Add regional column 

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

df1$REGION <- region_values[df1$SITE]
#check that all looks right in the df

#look at structure of variate
str(df1)
# #make DATE Datetime
# df1$DATE <- mdy(df1$DATE)   # month-day-year

#Plot what these look like

plot_nutrient <- function(data, nutrient, date_col = "DATE",
                          site_col = "SITE", region_col = "REGION",
                          free_y = FALSE) {
  
  # Build column names from the nutrient prefix (e.g., "NITRATE" -> NITRATE_mean, NITRATE_sd)
  mean_col <- paste0(nutrient, "_mean")
  sd_col   <- paste0(nutrient, "_sd")
  
  # Check that everything exists
  needed <- c(date_col, site_col, region_col, mean_col, sd_col)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  
  ggplot(data, aes(x = .data[[date_col]], y = .data[[mean_col]])) +
    geom_errorbar(aes(ymin = .data[[mean_col]] - .data[[sd_col]],
                      ymax = .data[[mean_col]] + .data[[sd_col]]),
                  width = 0, colour = "grey40") +
    geom_point(size = 2) +
    geom_line(aes(group = 1), alpha = 0.4) +
    scale_x_date(date_breaks = "3 months",
                 date_labels = "%m/%d/%Y")+
    facet_wrap(vars(.data[[region_col]], .data[[site_col]]), scales = if (free_y) "free_y" else "fixed")+
    labs(x = "Date",
         y = paste0(tools::toTitleCase(tolower(nutrient)), " (mean \u00b1 SD)"),
         title = paste(tools::toTitleCase(tolower(nutrient)), "by site and region")) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey90"),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_nutrient(df1, "NITRATE", free_y = TRUE)
plot_nutrient(df1, "PHOSPHATE", free_y = TRUE)
plot_nutrient(df1, "AMMONIUM", free_y = TRUE)
plot_nutrient(df1, "SILICATE", free_y = TRUE)

