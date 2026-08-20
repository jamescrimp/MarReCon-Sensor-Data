
#  TCM CURRENT DATA QC PIPELINE
#  - Import raw TCM data
#  - Deployment-edge trimming
#  - SD- and IQR-based QC
#  - Raw + cleaned plotting



# ---- Load required packages --------------------------------

library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)


# ---- Define working directories ----------------------------

# Project root (assumes this script is run from the project directory)
wd <- getwd()

# Input CSVs
dir.csv     <- file.path(wd, "CSVs")

# Outputs (plots + cleaned data)
dir.outputs <- file.path(wd, "Outputs")


# ---- Function: Import raw TCM data -------------------------

# Reads a single compiled TCM CSV and standardizes time columns
# Returns: data frame with POSIXct time and Date column
import_tcm <- function(csv_dir, file = "TCM_data.csv") {
  
  read_csv(file.path(csv_dir, file), show_col_types = FALSE) %>%
    mutate(
      Time_UTC = ymd_hms(Time_UTC),
      Date     = as.Date(Time_UTC)
    )
}


# ---- Function: Flag deployment trim blocks -----------------

# Identifies the contiguous block of "in-water" data for a deployment
# based on current speed being below a reasonable threshold.
#
# qc_flag values:
#   - "pre_trim"  : before valid deployment
#   - "kept"      : valid in-water data
#   - "post_trim" : after recovery
#
# Assumes data are already grouped by deployment (source_file)
flag_trim_blocks <- function(df,
                             speed_col = "Speed_cm_s",
                             threshold = 60,
                             window = 144) {
  
  # Identify observations below threshold
  good <- df[[speed_col]] < threshold
  r    <- rle(good)
  
  # Initialize flag
  df$qc_flag <- "pre_trim"
  
  # Find long enough runs of valid data
  long_runs <- which(r$values & r$lengths >= window)
  
  # If no valid block is found, keep everything
  if (length(long_runs) == 0) {
    df$qc_flag <- "kept"
    return(df)
  }
  
  # Determine start/end indices of the valid block
  run_starts <- cumsum(c(1, head(r$lengths, -1)))
  start_idx  <- run_starts[long_runs[1]]
  end_idx    <- run_starts[max(long_runs)] + r$lengths[max(long_runs)] - 1
  
  # Apply flags
  df$qc_flag[start_idx:end_idx] <- "kept"
  df$qc_flag[end_idx:nrow(df)]  <- "post_trim"
  
  df
}


# ---- Function: Flag IQR outliers (speed only) ---------------

# Flags extreme values using a global IQR threshold
# Intended for obvious spikes, not fine-scale variability
#
# qc_iqr_flag values:
#   - "kept"
#   - "post_trim"
flag_iqr_outliers <- function(df,
                              speed_col = "Speed_cm_s",
                              iqr_mult = 40) {
  
  Q1   <- quantile(df[[speed_col]], 0.25, na.rm = TRUE)
  Q3   <- quantile(df[[speed_col]], 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  
  df %>%
    mutate(
      qc_iqr_flag = ifelse(
        .data[[speed_col]] < Q1 - iqr_mult * IQRv |
          .data[[speed_col]] > Q3 + iqr_mult * IQRv,
        "post_trim",
        "kept"
      )
    )
}


# ---- Function: Flag SD outliers (speed only) ----------------

# Flags points that change too abruptly relative to the site-level SD
#
# qc_sd_flag values:
#   - "kept"
#   - "post_trim"
flag_sd_outliers <- function(df,
                             speed_col = "Speed_cm_s",
                             sd_multiplier = 3) {
  
  df %>%
    arrange(site, Time_UTC) %>%
    group_by(site) %>%
    mutate(
      sd_speed   = sd(.data[[speed_col]], na.rm = TRUE),
      diff_prev  = abs(.data[[speed_col]] - lag(.data[[speed_col]])),
      qc_sd_flag = ifelse(
        !is.na(diff_prev) & diff_prev > sd_multiplier * sd_speed,
        "post_trim",
        "kept"
      )
    ) %>%
    ungroup() %>%
    select(-sd_speed, -diff_prev)
}


# ---- Function: Filter rows by QC flag -----------------------

# Generic helper for removing rows flagged as not kept
remove_outliers <- function(df, flag_col) {
  df %>% filter(.data[[flag_col]] == "kept")
}


# ---- Function: Raw plots (looped by design) -----------------

# Creates multi-page PDFs:
#   - one PDF per variable
#   - one page per site
# Points are colored by deployment (source_file)
plot_raw <- function(df, vars, output_dir, prefix) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (v in vars) {
    
    pdf(file.path(output_dir, paste0(prefix, v, ".pdf")), 12, 6)
    
    for (s in unique(df$site)) {
      
      p <- ggplot(
        df %>% filter(site == s),
        aes(Date, .data[[v]], color = source_file)
      ) +
        geom_point(size = 0.6) +
        labs(
          title = paste(v, "—", s),
          color = "Deployment"
        ) +
        theme_minimal()
      
      print(p)
    }
    
    dev.off()
  }
}


# ---- Function: Cleaned speed plot ---------------------------

# Faceted plot showing final QC results across all sites
# plot_clean <- function(df, var, output_dir, filename) {
#   
#   dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
#   
#   p <- ggplot(df, aes(Date, .data[[var]], color = qc_flag)) +
#     geom_point(size = 0.5) +
#     labs(
#       title = var,
#       color = "QC Flag"
#     ) +
#     theme_minimal()
#   
#   ggsave(file.path(output_dir, filename), p, width = 12, height = 8)
# }


plot_clean <- function(df, vars, output_dir, prefix) {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  for (v in vars) {
    
    pdf(file.path(output_dir, paste0(prefix, v, ".pdf")), 12, 6)
    
    for (s in unique(df$site)) {
      
      p <- ggplot(
        df %>% filter(site == s),
        aes(Date, .data[[v]], color = source_file)
      ) +
        geom_point(size = 0.6) +
        labs(
          title = paste(v, "—", s),
          color = "Deployment"
        ) +
        theme_minimal()
      
      print(p)
    }
    
    dev.off()
  }
}

# ---- Pipeline execution ------------------------------------

# Import raw data
tcm_raw <- import_tcm(dir.csv)

# Raw diagnostic plots
plot_raw(
  df         = tcm_raw,
  vars       = c("Speed_cm_s", "Heading_deg"),
  output_dir = file.path(dir.outputs, "TCM_Raw_Plots"),
  prefix     = "Raw_"
)

# Trim deployment edges
ends_flagged <- tcm_raw %>%
  group_by(source_file) %>%
  group_modify(~ flag_trim_blocks(.x)) %>%
  ungroup()

ends_trimmed <- remove_outliers(ends_flagged, "qc_flag")

# SD-based QC
sd_flagged <- flag_sd_outliers(ends_trimmed)
sd_trimmed <- remove_outliers(sd_flagged, "qc_sd_flag")

# Cleaned diagnostic plot
plot_clean(
  df         = sd_flagged,
  var        = c("Speed_cm_s", "Heading_deg"),
  output_dir = file.path(dir.outputs, "TCM_QC_Plots"),
  prefix = "Clean_"
)

# Remove helper QC columns before export
tcm_clean <- sd_trimmed[, c(-1, -7, -14, -15)]

# Create CSV for 2023
write.csv(
  tcm_clean %>% filter(year(Date) == 2023),
  file.path(dir.csv, "TCM_2023_QAQC.csv"),
  row.names = FALSE
)

# Export cleaned data
write_csv(tcm_clean, file.path(dir.csv, "TCM_clean.csv"))
