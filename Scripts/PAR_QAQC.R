
#  PAR DATA QC AND CLEANING PIPELINE
#
#  Purpose:
#   - Import raw PAR + temperature data
#   - Apply reproducible QC steps
#   - Visualize deployments and trimming
#   - Flag potential outliers (exploratory only)
#   - Export cleaned datasets and QC summary
#
#  Notes:
#   - Outlier flags are for visual inspection only
#   - Final exported data retain all non-QC fields



# ---- Load required packages --------------------------------

library(tidyverse)   # dplyr, ggplot2, tibble, readr, etc.
library(lubridate)   # Date and time handling


# ---- User-defined paths and settings -----------------------

# Project directories
wd      <- getwd()
dir.csv <- file.path(wd, "CSVs")

# Output directory for raw / QC plots
dir.outputs <- file.path(wd, "Outputs", "PAR_QC")

# Variables of interest (must exist in data)
vars <- c("Temp_C", "PAR")

# Sensor specification limits
PAR_MAX <- 3000
PAR_MIN <- 0


# ---- Function: Load and format PAR data --------------------

# Reads cleaned PAR CSV and standardizes time columns
# Returns a data frame with Time_UTC, Date, and Year columns
load_par_data <- function(csv_path) {
  
  read.csv(csv_path, header = TRUE) %>%
    mutate(
      Time_UTC = as.POSIXct(Time_UTC, format = "%Y-%m-%d %H:%M:%S"),
      Date     = as.Date(Time_UTC),
      Year     = year(Date)
    )
}


# ---- Function: Initial QC (NAs + sensor bounds) ------------

# Removes rows with missing timestamps and values outside sensor specs
# Returns:
#   - data: cleaned data frame
#   - qc  : tibble logging row counts at each QC step
qc_initial <- function(df, par_max, par_min) {
  
  qc_log <- tibble(
    step = c("raw", "remove_NA", "sensor_bounds"),
    rows_remaining = NA_integer_
  )
  
  qc_log$rows_remaining[1] <- nrow(df)
  
  df1 <- df %>% drop_na(Time_UTC, Date)
  qc_log$rows_remaining[2] <- nrow(df1)
  
  df2 <- df1 %>%
    filter(PAR >= par_min, PAR <= par_max)
  qc_log$rows_remaining[3] <- nrow(df2)
  
  list(data = df2, qc = qc_log)
}


# ---- Function: Calculate deployment boundaries -------------

# Determines deployment start/end dates per site and file
# Used only for plotting vertical reference lines
calc_deployment_bounds <- function(df) {
  
  df %>%
    arrange(Site, Date) %>%
    group_by(Site, Filepath) %>%
    summarise(
      deploy_start = min(Date, na.rm = TRUE),
      deploy_end   = max(Date, na.rm = TRUE),
      .groups = "drop"
    )
}


# ---- Function: Plot time series with deployment markers ----

# Creates multi-page PDFs:
#   - one PDF per variable
#   - one page per site
#
# If outlier flags exist (outlier_<var>), points are colored accordingly
plot_timeseries <- function(df, bounds, variables, dir.outputs) {
  
  if (!dir.exists(dir.outputs)) dir.create(dir.outputs, recursive = TRUE)
  
  for (var in variables) {
    
    outlier_col  <- paste0("outlier_", var)
    has_outliers <- outlier_col %in% names(df)
    
    pdf(file.path(dir.outputs, paste0("timeseries_", var, ".pdf")),
        width = 8, height = 6)
    
    for (site in unique(df$Site)) {
      
      site_df <- df %>% filter(Site == site)
      
      # Base plot (with or without outlier coloring)
      if (has_outliers) {
        p <- ggplot(site_df, aes(
          x     = Date,
          y     = .data[[var]],
          color = factor(.data[[outlier_col]])
        )) +
          geom_point(alpha = 0.7) +
          scale_color_manual(
            values = c("0" = "blue", "1" = "red"),
            labels = c("0" = "Normal", "1" = "Outlier"),
            name   = "Outlier"
          )
      } else {
        p <- ggplot(site_df, aes(Date, .data[[var]])) +
          geom_point(color = "blue", alpha = 0.6)
      }
      
      # Add deployment boundaries
      p <- p +
        geom_vline(
          data = bounds %>% filter(Site == site),
          aes(xintercept = deploy_start),
          color = "darkgreen", linetype = "dashed"
        ) +
        geom_vline(
          data = bounds %>% filter(Site == site),
          aes(xintercept = deploy_end),
          color = "red", linetype = "dashed"
        ) +
        scale_x_date(
          limits      = c(as.Date("2023-10-01"), as.Date("2024-12-31")),
          date_breaks = "2 months",
          date_labels = "%b %Y"
        ) +
        labs(
          title = paste(site, "-", var),
          x     = "Date",
          y     = var
        ) +
        theme_minimal()
      
      print(p)
    }
    
    dev.off()
  }
}


# ---- Function: Trim deployment start/end -------------------

# Removes first and last N days of each deployment
# Intended to eliminate handling and recovery artifacts
trim_deployments <- function(df, days_trim) {
  
  df %>%
    group_by(Filepath) %>%
    filter(
      Time_UTC >= min(Time_UTC) + days(days_trim),
      Time_UTC <= max(Time_UTC) - days(days_trim)
    ) %>%
    ungroup()
}


# ---- Function: Flag SD-based outliers (exploratory) ---------

# Flags abrupt changes using site- and year-specific SDs
# NOTE: These flags are for visualization only
flag_sd_outliers <- function(df, variables, sd_multiplier = 3) {
  
  # Compute SDs by site and year
  sd_tbl <- df %>%
    group_by(Site, Year) %>%
    summarise(
      across(all_of(variables), sd, na.rm = TRUE, .names = "sd_{.col}"),
      .groups = "drop"
    )
  
  df2 <- left_join(df, sd_tbl, by = c("Site", "Year"))
  
  for (v in variables) {
    
    flag_col <- paste0("outlier_", v)
    sd_col   <- paste0("sd_", v)
    
    df2[[flag_col]] <- 0
    
    for (i in 2:nrow(df2)) {
      
      if (df2$Site[i] == df2$Site[i - 1] &&
          df2$Year[i] == df2$Year[i - 1]) {
        
        if (!is.na(df2[[v]][i]) &&
            !is.na(df2[[v]][i - 1]) &&
            !is.na(df2[[sd_col]][i])) {
          
          if (abs(df2[[v]][i] - df2[[v]][i - 1]) >
              sd_multiplier * df2[[sd_col]][i]) {
            df2[[flag_col]][i] <- 1
          }
        }
      }
    }
  }
  
  df2
}

# ---- Function: Remove SD-flagged outliers ------------------
#
# Removes rows flagged as SD outliers.
# Can be applied:
#   - to ALL variables passed (default)
#   - OR to a single specified variable only (e.g., "PAR")
#
# Args:
#   df             : data frame with outlier_<var> columns
#   variables     : character vector of variables with SD flags
#   specified_var : optional single variable name to apply filtering to
#
# Returns:
#   filtered data frame
remove_sd_outliers <- function(df, variables, specified_var = NULL) {
  
  # Decide which variables to use for filtering
  vars_to_apply <- if (is.null(specified_var)) {
    variables
  } else {
    specified_var
  }
  
  # Build outlier flag column names
  flag_cols <- paste0("outlier_", vars_to_apply)
  
  # Keep rows where all selected outlier flags are 0
  df %>%
    filter(if_all(all_of(flag_cols), ~ .x == 0))
}

# ---- Function: Build QC summary table ----------------------

# Combines row counts from each major QC step
build_qc_summary <- function(qc_initial_log,
                             df_trimmed,
                             df_flagged) {
  
  tibble(
    step = c(
      qc_initial_log$step,
      "deployment_trim",
      "outlier_flagging"
    ),
    rows_remaining = c(
      qc_initial_log$rows_remaining,
      nrow(df_trimmed),
      nrow(df_flagged)
    )
  )
}


# ---- Run QC pipeline ---------------------------------------

# Load raw data
par_raw <- load_par_data(file.path(dir.csv, "par_data.csv"))

# Initial QC (NAs + sensor bounds)
qc1    <- qc_initial(par_raw, PAR_MAX, PAR_MIN)
par_qc <- qc1$data

# Calculate deployment boundaries (for plotting only)
deploy_bounds <- calc_deployment_bounds(par_qc)

# Raw plots
plot_timeseries(par_qc, deploy_bounds, vars, file.path(dir.outputs, "raw_plots"))

# Deployment trimming window (days from start and end)
DAYS_TRIM <- 4

# Trim deployment edges
par_trimmed <- trim_deployments(par_qc, DAYS_TRIM)

# Trimmed plots
plot_timeseries(par_trimmed, deploy_bounds, vars, file.path(dir.outputs, "trimmed_plots"))



# Flag SD-based outliers (exploratory)
par_flagged <- flag_sd_outliers(par_trimmed, vars, sd_multiplier = 3)

# Outlier plots
plot_timeseries(par_flagged, deploy_bounds, vars, file.path(dir.outputs, "sd_plots"))

# Remove SD-based ouliers, but only for temperature (exploratory)

par_sd_clean <- remove_sd_outliers(
  df             = par_flagged,
  variables      = vars,
  specified_var  = "Temp_C"
)

# Plots with SD outliers removed (exploratory)
plot_timeseries(par_sd_clean, deploy_bounds, vars, file.path(dir.outputs, "sd_outliers_removed"))

# QC summary table
qc_summary <- build_qc_summary(qc1$qc, par_trimmed, par_flagged)
print(qc_summary)


# ---- Export cleaned data -----------------------------------

# Remove helper QC columns before export
par_clean <- par_flagged %>%
  select(-starts_with("sd_"), -starts_with("outlier_"))

# Full cleaned dataset
write.csv(
  par_clean,
  file.path(dir.csv, "par_clean.csv"),
  row.names = FALSE
)

# Create CSV for 2024
write.csv(
  filter(par_clean, Year == 2023),
  file.path(dir.csv, "PAR_2023_QAQC.csv"),
  row.names = FALSE
)


# QC summary table
write.csv(
  qc_summary,
  file.path(dir.csv, "par_QC_summary.csv"),
  row.names = FALSE
)
