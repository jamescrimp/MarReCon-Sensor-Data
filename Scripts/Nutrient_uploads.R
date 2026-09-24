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

#Set file path- Sierra
dir.data <- file.path("I:/Shared drives/Mariculture ReCon/Data/Kelp, oyster, nutrient, chl/Nutrients/Read into R")

files <- list.files(dir.data, pattern = "\\.csv$", full.names = TRUE)
files <- files[!grepl("R_Uploads_Template", basename(files))]

data_all <- files |>
  set_names(basename(files)) |>
  map(~ read_csv(
    .x,
    skip       = 11,
    col_names  = FALSE,
    col_select = 1:14,
    col_types  = cols(.default = "c")
  ) |>
    filter(if_any(everything(), ~ !is.na(.x)))   # drop fully-blank rows
  ) |>
  bind_rows(.id = "source_file")

#Remove columns we dont need
data_all <- data_all[, c(5, 7, 11:15)]

#Name columns 
#1: SITE
#2: DATE 
#3: NITRATE
#4: NITRITE
#5: PHOSPHATE
#6: AMMONIUM
#7: NOX

data_all <- data_all |>
  rename(
    SITE      = X4,
    DATE      = X6,
    NITRATE   = X10,
    NITRITE   = X11,
    PHOSPHATE = X12,
    AMMONIUM  = X13,
    NOX       = X14
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

vars <-c("NITRATE", "NITRITE", "PHOSPHATE", "AMMONIUM", "NOX")  

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

#Plot what these look like


