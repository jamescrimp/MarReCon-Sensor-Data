
#  ENVIRONMENTAL SENSOR SUMMARY + PLOTTING PIPELINE
#
#  Components:
#   - EXO daily means (Temp, Sal, Chlor, Turb, Cond)
#   - RBR daily means at depth bins (1 m, 3 m)
#   - PAR daily means
#   - TCM hourly means
#   - HOBO daily means (Temp, Sal, Cond)
#   - Weekly salinity-by-site PDFs using EXO + HOBO (weekly averages)
#   - Hourly salinity-by-site PDFs using EXO + HOBO (raw / non-summarized)
#   - NEW: Weekly temperature-by-site PDFs using EXO + HOBO (weekly averages)
#   - NEW: Hourly temperature-by-site PDFs using EXO + HOBO (raw / non-summarized)
#
#  Intended use:
#   - Run top-to-bottom inside an R project
#   - Produces publication-ready plots in Outputs/


# ---- Load required packages --------------------------------
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(cowplot)
library(patchwork)

# ---- Global constants --------------------------------------
SITE_LEVELS <- c(
  "AOF1","KIS1","KOB1","BCF1","MIO1",
  "SSF1","ROK1","SBO1","SBR1"
)

REGION_LABELS <- c(
  kod = "Kodiak",
  pws = "Prince William Sound",
  kby = "Kachemak Bay"
)

SITE_REGION_COLORS <- c(
  "kod.AOF1" = "#377EB8", "kod.KIS1" = "#6BAED6", "kod.KOB1" = "#9ECAE1",
  "pws.ROK1" = "#4DAF4A", "pws.SBO1" = "#74C476", "pws.SBR1" = "#A1D99B",
  "kby.SSF1" = "#E41A1C", "kby.BCF1" = "#D95F02", "kby.MIO1" = "#FC9272"
)

REGION_COLORS <- c(
  kod = "#377EB8",
  pws = "#4DAF4A",
  kby = "#E41A1C"
)

DATE_MIN <- as.Date("2024-01-01")

EXO_VARS <- c(
  "MeanTemp",
  "MeanSal",
  "MeanChlor",
  "MeanTurb",
  "MeanCond",
  "MeanDOSat",
  "MeanDOMgL"
)

# ---- Directory helpers -------------------------------------
make_dirs <- function(wd = getwd()) {
  
  dirs <- list(
    wd          = wd,
    dir.dat     = file.path(wd, "Raw data from sensors"),
    dir.csv     = file.path(wd, "CSVs"),
    dir.outputs = file.path(wd, "Outputs")
  )
  
  for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }
  
  plot_dirs <- list(
    MeanTemp  = file.path(dirs$dir.outputs, "Temperature_plots"),
    MeanSal   = file.path(dirs$dir.outputs, "Salinity_plots"),
    MeanChlor = file.path(dirs$dir.outputs, "Chlorophyll_plots"),
    MeanTurb  = file.path(dirs$dir.outputs, "Turbidity_plots"),
    MeanCond  = file.path(dirs$dir.outputs, "Conductivity_plots"),
    PAR       = file.path(dirs$dir.outputs, "PAR_plots"),
    Oxygen    = file.path(dirs$dir.outputs, "Oxygen_plots"),
    DO    = file.path(dirs$dir.outputs, "DO_plots"),
    Hobo      = file.path(dirs$dir.outputs, "Hobo_plots")
  )
  
  for (p in plot_dirs) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE)
  }
  
  list(dirs = dirs, plot_dirs = plot_dirs)
}

# ---- Data readers ------------------------------------------
read_exo <- function(dir.csv, file = "EXO_I_clean.csv") {
  read_csv(file.path(dir.csv, file), show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(
      date   = as.Date(date),
      site   = factor(toupper(site), levels = SITE_LEVELS),
      region = tolower(region)
    )
}

read_rbr <- function(
    dir.csv,
    file_csv  = "RBR_data_clean.csv",
    file_xlsx = "xlsx.RBR_data_clean.csv"
) {
  
  read_one <- function(path) {
    read_csv(path, show_col_types = FALSE) %>%
      rename_with(tolower) %>%
      mutate(
        date = as.Date(date),
        time = as.POSIXct(time),
        site = factor(site, levels = SITE_LEVELS)
      )
  }
  
  rbr_csv  <- read_one(file.path(dir.csv, file_csv))
  rbr_xlsx <- read_one(file.path(dir.csv, file_xlsx))
  
  bind_rows(rbr_csv, rbr_xlsx)
}

read_par <- function(dir.csv, file = "par_clean.csv") {
  read_csv(file.path(dir.csv, file), show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(
      date = as.Date(date),
      time = as.POSIXct(time_utc),
      site = factor(site, levels = SITE_LEVELS)
    )
}

read_tcm <- function(dir.csv, file = "tcm_clean.csv") {
  read_csv(file.path(dir.csv, file), show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(
      time = as.POSIXct(time_utc),
      date = as.Date(date),
      hour = floor_date(time, "hour")
    )
}

read_hobo <- function(dir.csv, file = "hobo_data_final.csv") {
  read_csv(file.path(dir.csv, file), show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    na.omit() %>%
    mutate(
      time = as.POSIXct(datetime),
      date = as.Date(date)
    )
}

# ---- Small helpers for robust timestamps -------------------
ensure_time_col <- function(df, time_candidates = c("time_utc","datetime","time","timestamp","date_time")) {
  nms <- names(df)
  cand <- intersect(time_candidates, nms)
  
  if (length(cand) == 0) {
    # If there's no obvious time column, we cannot do hourly plots properly
    stop(
      "No timestamp column found for hourly plots. Expected one of: ",
      paste(time_candidates, collapse = ", "),
      ". Found columns: ", paste(nms, collapse = ", ")
    )
  }
  
  # Use the first matching candidate
  time_col <- cand[1]
  
  df %>%
    mutate(
      time = as.POSIXct(.data[[time_col]]),
      date = as.Date(date)
    )
}

# ---- Data summarizers --------------------------------------
summarise_exo_daily <- function(exo) {
  exo %>%
    group_by(date, site, region) %>%
    summarise(
      MeanTemp  = mean(temp_c, na.rm = TRUE),
      MeanSal   = mean(sal_psu, na.rm = TRUE),
      MeanTurb  = mean(turbidity_fnu, na.rm = TRUE),
      MeanChlor = mean(chlorophyll_rfu, na.rm = TRUE),
      MeanCond  = mean(`cond_us.cm`, na.rm = TRUE),
      MeanDOSat    = mean(`odo_sat`, na.rm = TRUE),
      MeanDOMgL    = mean(`odo_mg.l`, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_exo_region_daily <- function(exo) {
  exo %>%
    group_by(date, region) %>%
    summarise(
      MeanTemp  = mean(temp_c, na.rm = TRUE),
      MeanSal   = mean(sal_psu, na.rm = TRUE),
      MeanTurb  = mean(turbidity_fnu, na.rm = TRUE),
      MeanChlor = mean(chlorophyll_rfu, na.rm = TRUE),
      MeanCond  = mean(`cond_us.cm`, na.rm = TRUE),
      MeanDOSat    = mean(`odo_sat`, na.rm = TRUE),
      MeanDOMgL    = mean(`odo_mg.l`, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_rbr_depth_daily <- function(rbr,
                                      pressure_min,
                                      pressure_max,
                                      depth_label) {
  rbr %>%
    filter(pressure >= pressure_min, pressure < pressure_max) %>%
    group_by(date, site, region) %>%
    summarise(
      MeanPressure = mean(pressure, na.rm = TRUE),
      MeanTemp     = mean(temperature, na.rm = TRUE),
      MeanCond     = mean(conductivity, na.rm = TRUE),
      MeanSal      = mean(salinity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(depth = depth_label)
}

summarise_par_daily <- function(par) {
  par %>%
    group_by(date, site, region) %>%
    summarise(
      MeanPAR  = mean(par, na.rm = TRUE),
      MeanTemp = mean(temp_c, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_tcm_hourly <- function(tcm) {
  tcm %>%
    group_by(date, site, region, hour) %>%
    summarise(
      HourlySpeed = mean(speed_cm_s, na.rm = TRUE),
      MeanHead    = mean(heading_deg, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_hobo_daily <- function(hobo) {
  hobo %>%
    group_by(date, site, region) %>%
    summarise(
      MeanTemp = mean(temp_c, na.rm = TRUE),
      MeanSal  = mean(sal_psu, na.rm = TRUE),
      MeanCond = mean(`cond_us.cm`, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Weekly salinity summaries (EXO + HOBO) -----------------
summarise_exo_sal_weekly <- function(exo, week_start = 1) {
  exo %>%
    mutate(week = floor_date(date, unit = "week", week_start = week_start)) %>%
    group_by(week, site, region) %>%
    summarise(
      MeanSal = mean(sal_psu, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_hobo_sal_weekly <- function(hobo, week_start = 1) {
  hobo %>%
    mutate(week = floor_date(date, unit = "week", week_start = week_start)) %>%
    group_by(week, site, region) %>%
    summarise(
      MeanSal = mean(sal_psu, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Weekly temperature summaries (EXO + HOBO) ---------
summarise_exo_temp_weekly <- function(exo, week_start = 1) {
  exo %>%
    mutate(week = floor_date(date, unit = "week", week_start = week_start)) %>%
    group_by(week, site, region) %>%
    summarise(
      MeanTemp = mean(temp_c, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_hobo_temp_weekly <- function(hobo, week_start = 1) {
  hobo %>%
    mutate(week = floor_date(date, unit = "week", week_start = week_start)) %>%
    group_by(week, site, region) %>%
    summarise(
      MeanTemp = mean(temp_c, na.rm = TRUE),
      .groups = "drop"
    )
}

summarise_par_weekly <- function(par, week_start = 1) {
  par %>%
    mutate(week = floor_date(date, unit = "week", week_start = week_start)) %>%
    group_by(week, site, region) %>%
    summarise(
      MeanPAR  = mean(par, na.rm = TRUE),
      MeanTemp = mean(temp_c, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- QC / descriptive statistics ---------------------------
region_descriptive_stats <- function(exo) {
  exo %>%
    group_by(region) %>%
    summarise(
      mean.temp = mean(temp_c, na.rm = TRUE),
      sd.temp   = sd(temp_c, na.rm = TRUE),
      mean.sal  = mean(sal_psu, na.rm = TRUE),
      sd.sal    = sd(sal_psu, na.rm = TRUE),
      mean.turb = mean(turbidity_fnu, na.rm = TRUE),
      sd.turb   = sd(turbidity_fnu, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean.temp.f = mean.temp * 9/5 + 32,
      sd.temp.f   = sd.temp * 9/5 + 32
    )
}

# ---- Plotting helpers --------------------------------------
x_scale_monthly <- function(date_min = DATE_MIN) {
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "1 month",
    expand      = c(0.01, 0)
  )
}

x_scale_monthly_datetime <- function() {
  scale_x_datetime(
    date_labels = "%b %Y",
    date_breaks = "1 month",
    expand      = c(0.01, 0)
  )
}

base_theme <- function() {
  theme_cowplot() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    )
}

save_plot <- function(p, filename, out_dir,
                      width = 10, height = 4, dpi = 600) {
  ggsave(
    filename  = filename,
    plot      = p,
    path      = out_dir,
    width     = width,
    height    = height,
    units     = "in",
    dpi       = dpi,
    limitsize = TRUE
  )
}

# ---- Plot functions: EXO -----------------------------------
plot_exo_variable_by_site <- function(exo_by_date, var,
                                      date_min = DATE_MIN) {
  exo_by_date %>%
    mutate(site_region = paste(region, site, sep = ".")) %>%
    ggplot() +
    geom_line(aes(
      x     = date,
      y     = .data[[var]],
      group = site_region,
      color = site_region
    )) +
    scale_color_manual(values = SITE_REGION_COLORS) +
    x_scale_monthly(date_min) +
    labs(y = var) +
    base_theme() +
    theme(legend.position = "none")
}

plot_exo_variable_faceted_region <- function(exo_by_date, var,
                                             date_min = DATE_MIN) {
  ggplot(exo_by_date) +
    geom_line(aes(x = date, y = .data[[var]], group = site, col = site)) +
    facet_wrap(~ region, ncol = 1,
               labeller = as_labeller(REGION_LABELS)) +
    x_scale_monthly(date_min) +
    labs(y = var) +
    base_theme() +
    theme(
      legend.position  = "none",
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text       = element_text(color = "black")
    )
}

plot_exo_region_mean <- function(exo_by_region, var,
                                 date_min = DATE_MIN) {
  ggplot(exo_by_region) +
    geom_line(aes(x = date, y = .data[[var]],
                  group = region, col = region)) +
    scale_color_manual(values = REGION_COLORS,
                       labels = REGION_LABELS) +
    x_scale_monthly(date_min) +
    labs(y = var) +
    base_theme() +
    theme(legend.title = element_blank())
}

# ---- Plot functions: Temperature by site (DAILY: EXO + RBR + HOBO +PAR) ----
plot_temperature_by_site <- function(exo_by_date,
                                     rbr_surface,
                                     rbr_3m,
                                     hobo_by_date,
                                     par_by_date,
                                     out_dir,
                                     date_min = DATE_MIN,
                                     date_max = NULL,
                                     gap_days = 7) {
  
  sites <- sort(unique(exo_by_date$site))
  
  for (site_id in sites) {
    
    pdf(
      file   = file.path(out_dir, paste0("Temperature_Time_Series_", site_id, ".pdf")),
      width  = 8,
      height = 6
    )
    
    # Determine max date for this site if not provided
    if (is.null(date_max)) {
      date_max_site <- max(
        c(
          exo_by_date$date[exo_by_date$site == site_id],
          rbr_surface$date[rbr_surface$site == site_id],
          rbr_3m$date[rbr_3m$site == site_id],
          hobo_by_date$date[hobo_by_date$site == site_id],
          par_by_date$date[par_by_date$site == site_id]
        ),
        na.rm = TRUE
      )
    } else {
      date_max_site <- date_max
    }
    
    exo_plot <- exo_by_date %>%
      filter(site == site_id, date >= date_min, date <= date_max_site) %>%
      arrange(date) %>%
      mutate(gap = c(0, as.numeric(diff(date))),
             line_id = cumsum(gap > gap_days))
    
    hobo_plot <- hobo_by_date %>%
      filter(site == site_id,
             date >= as.Date("2024-05-01"),
             date <= date_max_site) %>%
      arrange(date) %>%
      mutate(gap = c(0, as.numeric(diff(date))),
             line_id = cumsum(gap > gap_days))
    
    par_plot <- par_by_date %>%
      filter(site == site_id, date >= date_min, date <= date_max_site) %>%
      arrange(date) %>%
      mutate(gap = c(0, as.numeric(diff(date))),
             line_id = cumsum(gap > gap_days))
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = date, y = MeanTemp, group = line_id, color = "EXO (3 m)"),
        linewidth = 0.8
      ) +
      geom_point(
        data = rbr_surface %>% filter(site == site_id, date >= date_min, date <= date_max_site),
        aes(x = date, y = MeanTemp, color = "RBR (surface)"),
        size = 1.5
      ) +
      geom_point(
        data = rbr_3m %>% filter(site == site_id, date >= date_min, date <= date_max_site),
        aes(x = date, y = MeanTemp, color = "RBR (3 m)"),
        size = 1.5
      ) +
      geom_line(
        data = hobo_plot,
        aes(x = date, y = MeanTemp, group = line_id, color = "HOBO (0.75 m)"),
        linewidth = 0.8
      ) +
      geom_line(
        data = par_plot,
        aes(x = date, y = MeanTemp, group = line_id, color = "miniPAR (3 m)"),
        linewidth = 0.8
      ) +
      scale_color_manual(values = c(
        "EXO (3 m)"      = "#377EB8",
        "RBR (3 m)"      = "#377EB8",
        "HOBO (0.75 m)"  = "#E41A1C",
        "RBR (surface)"  = "#E41A1C",
        "miniPAR (3 m)"  = "#984EA3"   # ← new purple PAR line
      )) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
      x_scale_monthly(DATE_MIN) +
      coord_cartesian(xlim = c(date_min, date_max_site)) +
      labs(
        title = paste("Daily Temperature Time Series –", site_id),
        y     = "Temperature (°C)"
      ) +
      base_theme() +
      theme(legend.title = element_blank())
    
    print(p)
    dev.off()
  }
}

# ---- Plot functions: Daily Salinity by site (EXO + RBR + HOBO) ----
plot_salinity_by_site <- function(exo_by_date,
                                  rbr_surface,
                                  rbr_3m,
                                  hobo_by_date,
                                  out_dir,
                                  date_min = DATE_MIN,
                                  date_max = NULL,
                                  gap_days = 7) {
  
  sites <- sort(unique(exo_by_date$site))
  
  for (site_id in sites) {
    
    pdf(
      file   = file.path(out_dir,
                         paste0("Salinity_Time_Series_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    exo_plot <- exo_by_date %>%
      filter(site == site_id,
             date >= date_min,
             if (is.null(date_max)) TRUE else date <= date_max) %>%
      arrange(date) %>%
      mutate(
        gap     = c(0, as.numeric(diff(date))),
        line_id = cumsum(gap > gap_days)
      )
    
    hobo_plot <- hobo_by_date %>%
      filter(site == site_id,
             date >= as.Date("2024-05-01"),
             if (is.null(date_max)) TRUE else date <= date_max) %>%
      arrange(date) %>%
      mutate(
        gap     = c(0, as.numeric(diff(date))),
        line_id = cumsum(gap > gap_days)
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = date, y = MeanSal,
            group = line_id, color = "EXO (3 m)"),
        linewidth = 0.8
      ) +
      geom_point(
        data = rbr_surface %>%
          filter(site == site_id,
                 date >= date_min,
                 if (is.null(date_max)) TRUE else date <= date_max),
        aes(x = date, y = MeanSal, color = "RBR (surface)"),
        size = 1.5
      ) +
      geom_point(
        data = rbr_3m %>%
          filter(site == site_id,
                 date >= date_min,
                 if (is.null(date_max)) TRUE else date <= date_max),
        aes(x = date, y = MeanSal, color = "RBR (3 m)"),
        size = 1.5
      ) +
      geom_line(
        data = hobo_plot,
        aes(x = date, y = MeanSal,
            group = line_id, color = "HOBO (0.75 m)"),
        linewidth = 0.8
      ) +
      scale_color_manual(values = c(
        "EXO (3 m)"      = "#377EB8",
        "RBR (3 m)"      = "#377EB8",
        "HOBO (0.75 m)"  = "#E41A1C",
        "RBR (surface)"  = "#E41A1C"
      )) +
      scale_y_continuous(limits = c(10, 35), expand = c(0, 0)) +
      x_scale_monthly() +
      coord_cartesian(xlim = c(date_min, date_max)) +
      labs(
        title = paste("Daily Salinity Time Series –", site_id),
        y     = "Salinity"
      ) +
      base_theme() +
      theme(legend.title = element_blank())
    
    print(p)
    dev.off()
  }
}

# ---- Plot function: Weekly Salinity by site (EXO + HOBO) ----
plot_salinity_by_site_weekly <- function(exo_weekly,
                                         hobo_weekly,
                                         out_dir,
                                         date_min = DATE_MIN,
                                         date_max = NULL,
                                         gap_weeks = 2) {
  
  sites <- sort(unique(exo_weekly$site))
  
  for (site_id in sites) {
    
    pdf(
      file   = file.path(out_dir,
                         paste0("Salinity_Time_Series_WEEKLY_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    exo_plot <- exo_weekly %>%
      filter(site == site_id,
             week >= date_min,
             if (is.null(date_max)) TRUE else week <= date_max) %>%
      arrange(week) %>%
      mutate(
        gap     = c(0, as.numeric(diff(week))),
        line_id = cumsum(gap > (7 * gap_weeks))
      )
    
    hobo_plot <- hobo_weekly %>%
      filter(site == site_id,
             week >= as.Date("2024-05-01"),
             if (is.null(date_max)) TRUE else week <= date_max) %>%
      arrange(week) %>%
      mutate(
        gap     = c(0, as.numeric(diff(week))),
        line_id = cumsum(gap > (7 * gap_weeks))
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = week, y = MeanSal,
            group = line_id, color = "EXO"),
        linewidth = 0.9
      ) +
      geom_line(
        data = hobo_plot,
        aes(x = week, y = MeanSal,
            group = line_id, color = "HOBO"),
        linewidth = 0.9
      ) +
      scale_color_manual(values = c(
        "EXO"  = "#377EB8",
        "HOBO" = "#E41A1C"
      )) +
      scale_y_continuous(limits = c(10, 35), expand = c(0, 0)) +
      x_scale_monthly() +
      coord_cartesian(xlim = c(date_min, date_max)) +
      labs(
        title = paste("Weekly Salinity Time Series –", site_id),
        y     = "Salinity"
      ) +
      base_theme() +
      theme(legend.title = element_blank())
    
    print(p)
    dev.off()
  }
}

# ---- Plot functions: Hourly Salinity by site (EXO + HOBO) ----
plot_salinity_by_site_hourly <- function(exo_data,
                                         hobo_data,
                                         out_dir,
                                         date_min = DATE_MIN,
                                         date_max = NULL,
                                         gap_hours = 24 * 7) {
  
  exo_hourly  <- ensure_time_col(exo_data)  %>% filter(!is.na(time))
  hobo_hourly <- ensure_time_col(hobo_data) %>% filter(!is.na(time))
  
  # Use EXO sites as the driver (falls back to HOBO if needed)
  sites <- sort(unique(c(as.character(exo_hourly$site), as.character(hobo_hourly$site))))
  sites <- sites[!is.na(sites) & sites != ""]
  
  for (site_id in sites) {
    
    pdf(
      file   = file.path(out_dir,
                         paste0("Salinity_Time_Series_HOURLY_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    exo_plot <- exo_hourly %>%
      mutate(site = factor(toupper(site), levels = SITE_LEVELS)) %>%
      filter(site == site_id,
             as.Date(time) >= date_min,
             if (is.null(date_max)) TRUE else as.Date(time) <= date_max) %>%
      arrange(time) %>%
      mutate(
        gap_h   = c(0, as.numeric(difftime(time[-1], time[-n()], units = "hours"))),
        line_id = cumsum(gap_h > gap_hours)
      )
    
    hobo_plot <- hobo_hourly %>%
      mutate(site = factor(toupper(site), levels = SITE_LEVELS)) %>%
      filter(site == site_id,
             as.Date(time) >= as.Date("2024-05-01"),
             if (is.null(date_max)) TRUE else as.Date(time) <= date_max) %>%
      arrange(time) %>%
      mutate(
        gap_h   = c(0, as.numeric(difftime(time[-1], time[-n()], units = "hours"))),
        line_id = cumsum(gap_h > gap_hours)
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = time, y = sal_psu,
            group = line_id, color = "EXO"),
        linewidth = 0.6
      ) +
      geom_line(
        data = hobo_plot,
        aes(x = time, y = sal_psu,
            group = line_id, color = "HOBO"),
        linewidth = 0.6
      ) +
      scale_color_manual(values = c(
        "EXO"  = "#377EB8",
        "HOBO" = "#E41A1C"
      )) +
      scale_y_continuous(limits = c(10, 35), expand = c(0, 0)) +
      x_scale_monthly_datetime() +
      coord_cartesian(xlim = c(as.POSIXct(date_min), if (is.null(date_max)) NA else as.POSIXct(date_max))) +
      labs(
        title = paste("Hourly Salinity Time Series –", site_id),
        y     = "Salinity"
      ) +
      base_theme() +
      theme(legend.title = element_blank())
    
    print(p)
    dev.off()
  }
}

# 
#  NEW: Temperature weekly + hourly plots (EXO + HOBO)
#

# ---- Plot function: Weekly Temperature by site (EXO + HOBO) ----
plot_temperature_by_site_weekly <- function(exo_weekly,
                                            hobo_weekly,
                                            par_by_week,
                                            out_dir,
                                            date_min = DATE_MIN,
                                            date_max = NULL,
                                            gap_weeks = 2) {
  
  sites <- sort(unique(exo_weekly$site))
  
  for (site_id in sites) {
    
    pdf(
      file   = file.path(out_dir, paste0("Temperature_Time_Series_WEEKLY_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    if (is.null(date_max)) {
      date_max_site <- max(
        c(
          exo_weekly$week[exo_weekly$site == site_id],
          hobo_weekly$week[hobo_weekly$site == site_id],
          par_by_week$week[par_by_week$site == site_id]
        ),
        na.rm = TRUE
      )
    } else {
      date_max_site <- date_max
    }
    
    exo_plot <- exo_weekly %>%
      filter(site == site_id, week >= date_min, week <= date_max_site) %>%
      arrange(week) %>%
      mutate(
        gap     = c(0, as.numeric(diff(week))),
        line_id = cumsum(gap > (7 * gap_weeks))
      )
    
    hobo_plot <- hobo_weekly %>%
      filter(site == site_id,
             week >= as.Date("2024-05-01"),
             week <= date_max_site) %>%
      arrange(week) %>%
      mutate(
        gap     = c(0, as.numeric(diff(week))),
        line_id = cumsum(gap > (7 * gap_weeks))
      )
    
    par_plot <- par_by_week %>%
      filter(site == site_id, week >= date_min, week <= date_max_site) %>%
      arrange(week) %>%
      mutate(
        gap     = c(0, as.numeric(diff(week))),
        line_id = cumsum(gap > (7 * gap_weeks))
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = week, y = MeanTemp, group = line_id, color = "EXO"),
        linewidth = 0.9
      ) +
      geom_line(
        data = hobo_plot,
        aes(x = week, y = MeanTemp, group = line_id, color = "HOBO"),
        linewidth = 0.9
      ) +
      # PAR weekly temperature (fixed black line)
      geom_line(
        data = par_plot,
        aes(x = week, y = MeanTemp, group = line_id),
        color = "black",
        linewidth = 0.8
      ) +
      scale_color_manual(values = c(
        "EXO"  = "#377EB8",
        "HOBO" = "#E41A1C"
      )) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
      x_scale_monthly(DATE_MIN) +
      coord_cartesian(xlim = c(date_min, date_max_site)) +
      labs(
        title = paste("Weekly Temperature Time Series –", site_id),
        y     = "Temperature (°C)"
      ) +
      base_theme() +
      theme(legend.title = element_blank())
    
    print(p)
    dev.off()
  }
}

# ---- Plot function: Hourly Temperature by site (EXO + HOBO) ----
plot_temperature_by_site_hourly <- function(exo_data,
                                            hobo_data,
                                            out_dir,
                                            date_min = DATE_MIN,
                                            date_max = NULL,
                                            gap_hours = 24 * 7) {
  
  exo_hourly  <- ensure_time_col(exo_data)  %>% filter(!is.na(time))
  hobo_hourly <- ensure_time_col(hobo_data) %>% filter(!is.na(time))
  
  sites <- sort(unique(c(as.character(exo_hourly$site), as.character(hobo_hourly$site))))
  sites <- sites[!is.na(sites) & sites != ""]
  
  for (site_id in sites) {
    
    pdf(
      file   = file.path(out_dir,
                         paste0("Temperature_Time_Series_HOURLY_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    exo_plot <- exo_hourly %>%
      mutate(site = factor(toupper(site), levels = SITE_LEVELS)) %>%
      filter(site == site_id,
             as.Date(time) >= date_min,
             if (is.null(date_max)) TRUE else as.Date(time) <= date_max) %>%
      arrange(time) %>%
      mutate(
        gap_h   = c(0, as.numeric(difftime(time[-1], time[-n()], units = "hours"))),
        line_id = cumsum(gap_h > gap_hours)
      )
    
    hobo_plot <- hobo_hourly %>%
      mutate(site = factor(toupper(site), levels = SITE_LEVELS)) %>%
      filter(site == site_id,
             as.Date(time) >= as.Date("2024-05-01"),
             if (is.null(date_max)) TRUE else as.Date(time) <= date_max) %>%
      arrange(time) %>%
      mutate(
        gap_h   = c(0, as.numeric(difftime(time[-1], time[-n()], units = "hours"))),
        line_id = cumsum(gap_h > gap_hours)
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = time, y = temp_c,
            group = line_id, color = "EXO"),
        linewidth = 0.6
      ) +
      geom_line(
        data = hobo_plot,
        aes(x = time, y = temp_c,
            group = line_id, color = "HOBO"),
        linewidth = 0.6
      ) +
      scale_color_manual(values = c(
        "EXO"  = "#377EB8",
        "HOBO" = "#E41A1C"
      )) +
      scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
      x_scale_monthly_datetime() +
      coord_cartesian(xlim = c(as.POSIXct(date_min), if (is.null(date_max)) NA else as.POSIXct(date_max))) +
      labs(
        title = paste("Hourly Temperature Time Series –", site_id),
        y     = "Temperature (°C)"
      ) +
      base_theme() +
      theme(legend.title = element_blank())
    
    print(p)
    dev.off()
  }
}

# ---- Plot function: Daily Chlorophyll by Site ----
plot_chlorophyll_by_site_daily <- function(exo_by_date,
                                           out_dir,
                                           date_min = DATE_MIN,
                                           date_max = NULL,
                                           gap_days = 7) {
  
  sites <- sort(unique(exo_by_date$site))
  
  for (site_id in sites) {
    
    # If date_max not provided, use the max date available for this site
    date_max_site <- if (is.null(date_max)) {
      max(exo_by_date$date[exo_by_date$site == site_id], na.rm = TRUE)
    } else {
      date_max
    }
    
    pdf(
      file   = file.path(out_dir, paste0("Chlorophyll_Time_Series_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    # Break EXO lines across gaps
    exo_plot <- exo_by_date %>%
      filter(site == site_id,
             date >= date_min,
             date <= date_max_site) %>%
      arrange(date) %>%
      mutate(
        gap     = c(0, as.numeric(diff(date))),
        line_id = cumsum(gap > gap_days)
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = date, y = MeanChlor, group = line_id),
        color = "#377EB8",
        linewidth = 0.8
      ) +
      x_scale_monthly(DATE_MIN) +
      coord_cartesian(xlim = c(date_min, date_max_site)) +
      labs(
        title = paste("Daily Chlorophyll Time Series –", site_id),
        y     = "Chlorophyll (RFU)"
      ) +
      base_theme()
    
    print(p)
    dev.off()
  }
}


# ---- Plot function: Daily Turbidity by Site ----
plot_turbidity_by_site_daily <- function(exo_by_date,
                                         out_dir,
                                         date_min = DATE_MIN,
                                         date_max = NULL,
                                         gap_days = 7) {
  
  sites <- sort(unique(exo_by_date$site))
  
  for (site_id in sites) {
    
    # If date_max not provided, use the max date available for this site
    date_max_site <- if (is.null(date_max)) {
      max(exo_by_date$date[exo_by_date$site == site_id], na.rm = TRUE)
    } else {
      date_max
    }
    
    pdf(
      file   = file.path(out_dir, paste0("Turbidity_Time_Series_", site_id, ".pdf")),
      width  = 10,
      height = 6
    )
    
    # Break EXO lines across gaps
    exo_plot <- exo_by_date %>%
      filter(site == site_id,
             date >= date_min,
             date <= date_max_site) %>%
      arrange(date) %>%
      mutate(
        gap     = c(0, as.numeric(diff(date))),
        line_id = cumsum(gap > gap_days)
      )
    
    p <- ggplot() +
      geom_line(
        data = exo_plot,
        aes(x = date, y = MeanTurb, group = line_id),
        color = "#377EB8",
        linewidth = 0.8
      ) +
      x_scale_monthly(DATE_MIN) +
      coord_cartesian(xlim = c(date_min, date_max_site)) +
      labs(
        title = paste("Daily Turbidity Time Series –", site_id),
        y     = "Turbidity (FNU)"
      ) +
      base_theme()
    
    print(p)
    dev.off()
  }
}



# Average temperature profile from summertime farms -----------------------



# ---- Main pipeline -----------------------------------------
paths <- make_dirs()

# ---- Import data ----
exo_data  <- read_exo(paths$dirs$dir.csv)
rbr_data  <- read_rbr(paths$dirs$dir.csv)
par_data  <- read_par(paths$dirs$dir.csv)
tcm_data  <- read_tcm(paths$dirs$dir.csv)
hobo_data <- read_hobo(paths$dirs$dir.csv)

# ---- Summarise data (daily) ----
exo_by_date   <- summarise_exo_daily(exo_data)
exo_by_region <- summarise_exo_region_daily(exo_data)

rbr_surface <- summarise_rbr_depth_daily(rbr_data, 0,   0.5, "1 m")
rbr_3m      <- summarise_rbr_depth_daily(rbr_data, 2.5, 3.5, "3 m")

par_by_date  <- summarise_par_daily(par_data)
tcm_by_hour  <- summarise_tcm_hourly(tcm_data)
hobo_by_date <- summarise_hobo_daily(hobo_data)

# ---- Weekly summaries (EXO + HOBO) ----
exo_sal_weekly   <- summarise_exo_sal_weekly(exo_data)
hobo_sal_weekly  <- summarise_hobo_sal_weekly(hobo_data)

exo_temp_weekly  <- summarise_exo_temp_weekly(exo_data)
hobo_temp_weekly <- summarise_hobo_temp_weekly(hobo_data)
par_by_week <- summarise_par_weekly(par_data)

# ---- Descriptive statistics ----
temperature_by_region <- region_descriptive_stats(exo_data)
print(temperature_by_region)

# ---- Plots: EXO by site (PDFs) -----------------------------
for (var in EXO_VARS) {
  out_dir <- paths$plot_dirs[[var]]
  pdf(file.path(out_dir, paste0("Time_Series_", var, ".pdf")), 8, 6)
  print(plot_exo_variable_by_site(exo_by_date, var))
  dev.off()
}

# ---- Plots: EXO by site, faceted by region -----------------
facet_vars <- c("MeanTemp", "MeanSal", "MeanChlor", "MeanTurb")
for (var in facet_vars) {
  p <- plot_exo_variable_faceted_region(exo_by_date, var)
  save_plot(p, paste0(var, "_by_site_faceted.png"),
            paths$plot_dirs[[var]], width = 10, height = 5)
}

# ---- Plots: Region means -----------------------------------
region_vars <- c("MeanTemp", "MeanSal", "MeanChlor", "MeanTurb")
for (var in region_vars) {
  
  p <- plot_exo_region_mean(exo_by_region, var)
  
  if (var == "MeanTemp")  p <- p + scale_y_continuous(limits = c(0, 16))
  if (var == "MeanSal")   p <- p + scale_y_continuous(limits = c(24, 34))
  if (var == "MeanChlor") p <- p + scale_y_continuous(limits = c(0, 6))
  if (var == "MeanTurb")  p <- p + scale_y_continuous(limits = c(0, 16))
  
  save_plot(p, paste0(var, "_by_region.png"),
            paths$plot_dirs[[var]], width = 10, height = 5)
}

# ---- Temperature by site (DAILY) --------------------
plot_temperature_by_site(
  exo_by_date  = exo_by_date,
  rbr_surface  = rbr_surface,
  rbr_3m       = rbr_3m,
  hobo_by_date = hobo_by_date,
  par_by_date = par_by_date,
  out_dir      = paths$plot_dirs$MeanTemp,
  date_min     = DATE_MIN,
  date_max     = as.Date("2026-07-31")
)

# ---- Plots: Temperature by site (HOURLY) --------------
plot_temperature_by_site_hourly(
  exo_data  = exo_data,
  hobo_data = hobo_data,
  out_dir   = paths$plot_dirs$MeanTemp,
  date_min  = DATE_MIN,
  date_max  = as.Date("2026-07-31"),
  gap_hours = 24 * 7
)

# ---- Plots: Temperature by site (WEEKLY) --------------
plot_temperature_by_site_weekly(
  exo_weekly  = exo_temp_weekly,
  hobo_weekly = hobo_temp_weekly,
  par_by_week = par_by_week,
  out_dir     = paths$plot_dirs$MeanTemp,
  date_min    = DATE_MIN,
  date_max    = as.Date("2026-07-31"),
  gap_weeks   = 2
)

# ---- Plots: Salinity by site (DAILY) -----------------------
plot_salinity_by_site(
  exo_by_date  = exo_by_date,
  rbr_surface  = rbr_surface,
  rbr_3m       = rbr_3m,
  hobo_by_date = hobo_by_date,
  out_dir      = paths$plot_dirs$MeanSal,
  date_min     = DATE_MIN,
  date_max     = as.Date("2026-07-31")
)

# ---- Plots: Salinity by site (HOURLY) ----------------------
plot_salinity_by_site_hourly(
  exo_data  = exo_data,
  hobo_data = hobo_data,
  out_dir   = paths$plot_dirs$MeanSal,
  date_min  = DATE_MIN,
  date_max  = as.Date("2026-07-31"),
  gap_hours = 24 * 7
)

# ---- Plots: Salinity by site (WEEKLY) ----------------------
plot_salinity_by_site_weekly(
  exo_weekly  = exo_sal_weekly,
  hobo_weekly = hobo_sal_weekly,
  out_dir     = paths$plot_dirs$MeanSal,
  date_min    = DATE_MIN,
  date_max    = as.Date("2026-07-31"),
  gap_weeks   = 2
)

# ---- Plots: Chlorophyll by site (DAILY) -----------------------
plot_chlorophyll_by_site_daily(
  exo_by_date  = exo_by_date,
  out_dir      = paths$plot_dirs$MeanChlor,
  date_min     = DATE_MIN,
  date_max     = as.Date("2026-07-31")
)

# ---- Plots: Turbidity by site (DAILY) -----------------------
plot_turbidity_by_site_daily(
  exo_by_date  = exo_by_date,
  out_dir      = paths$plot_dirs$MeanTurb,
  date_min     = DATE_MIN,
  date_max     = as.Date("2026-07-31")
)


# ---- Plot: PAR by site -------------------------------------
if ("MeanPAR" %in% names(par_by_date)) {
  
  p_par <- ggplot(par_by_date) +
    geom_line(aes(
      x     = date,
      y     = MeanPAR,
      group = interaction(region, site),
      col   = interaction(region, site)
    )) +
    scale_color_manual(values = SITE_REGION_COLORS) +
    x_scale_monthly(DATE_MIN) +
    labs(y = "PAR") +
    base_theme() +
    theme(legend.title = element_blank())
  
  save_plot(p_par, "PAR_by_site.png",
            paths$plot_dirs$PAR, width = 10, height = 5)
}

# # ---- Plot: TCM current example -----------------------------
# tcm_by_hour_filtered <- tcm_by_hour %>% filter(HourlySpeed < 40)
# p_current_sbo1 <- tcm_by_hour_filtered %>%
#   filter(site == "SBO1") %>%
#   ggplot(aes(hour, HourlySpeed, color = MeanHead)) +
#   geom_point(alpha = 0.3, size = 0.5) +
#   scale_color_viridis_c(option = "plasma") +
#   x_scale_monthly(DATE_MIN) +
#   labs(
#     x = "Time",
#     y = "Current Speed (cm/s)",
#     color = "Direction (°)"
#   ) +
#   theme_minimal()
# print(p_current_sbo1)



# Function: RBR Temperature Profiles by Year (Separate PDFs)


plot_rbr_temperature_profile_by_year <- function(
    rbr_data,
    out_dir,
    date_min = DATE_MIN,
    date_max = NULL,
    depth_bin_size = 0.5
) {
  
  rbr <- rbr_data %>%
    mutate(
      depth_m = pressure,  # ~1 dbar ≈ 1 m
      site  = factor(toupper(site), levels = SITE_LEVELS),
      month = lubridate::month(date),
      year  = lubridate::year(date)
    ) %>%
    
    # Filter: date range + April–October + top 10 m
    filter(
      date >= date_min,
      if (is.null(date_max)) TRUE else date <= date_max,
      month >= 6 & month <= 8,
      depth_m <= 10 & depth_m >= .2
    ) %>%
    
    filter(!is.na(depth_m), !is.na(temperature)) %>%
    
    # Depth binning
    mutate(
      depth_bin = round(depth_m / depth_bin_size) * depth_bin_size
    )
  
  sites <- sort(unique(rbr$site))
  
  for (site_id in sites) {
    
    site_data <- rbr %>% filter(site == site_id)
    
    if (nrow(site_data) == 0) next
    
    years <- sort(unique(site_data$year))
    
    for (yr in years) {
      
      year_data <- site_data %>% filter(year == yr)
      
      profile <- year_data %>%
        group_by(depth_bin) %>%
        summarise(
          MeanTemp = mean(temperature, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(depth_bin)
      
      pdf(
        file   = file.path(out_dir,
                           paste0("RBR_Temperature_Profile_", site_id, "_", yr, ".pdf")),
        width  = 4,
        height = 7
      )
      
      p <- ggplot(profile, aes(x = MeanTemp, y = depth_bin)) +
        # geom_point(color = "steelblue", size = 2) +
        geom_line(method = "loess", span = 0.3, color = "steelblue", se = FALSE, linewidth = 1.2) +
        scale_y_reverse(limits = c(10, 0), breaks = 0:10) +
        labs(
          x = "Temperature (°C)",
          y = "Depth (m)") +
        base_theme() +   
        theme(axis.title.x = element_text(size = 14))  # ← override
      
      print(p)
      dev.off()
    }
  }
}

plot_rbr_temperature_profile_by_year(
  rbr_data = rbr_data,
  out_dir  = paths$plot_dirs$MeanTemp,
  date_min = DATE_MIN,
  date_max = as.Date("2026-07-31"),
  depth_bin_size = .5
)
