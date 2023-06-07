library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)

source("R/helper_functions.R")
out_dir <- "GOTM/Output/Experiment_output"
#

# ========= 5. change_Q_AT_ST ========
scenario <- 'change_Q_AT_ST'
# will have one of these for each timestep summary (daily, monthly, seasonal)
# run for each metric
metrics <- gsub("Hourly_", "", 
                gsub(".txt", "", 
                     list.files(file.path(out_dir, scenario, "Summaries"), pattern = "Hourly")))

for (metric in metrics) {
  metric_hourly <- readr::read_delim(file.path(out_dir, scenario, "Summaries", paste0("Hourly_",
                                                                                      metric, 
                                                                                      ".txt")), show_col_types = F)
  
  metric_daily <- metric_hourly %>%
    mutate(date = ymd(format(datetime, "%Y-%m-%d"))) %>%
    group_by(date) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - date, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # daily summary
  if (!exists("daily_summary_e")) {
    daily_summary_e <- metric_daily 
  } else {
    daily_summary_e <- full_join(daily_summary_e, metric_daily, by = c("T_change","Q_change", "date"))
  }
  
  metric_yearmon <- metric_hourly %>%
    mutate(month = as.yearmon(format(datetime, "%Y-%m"))) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # yearmon summary
  if (!exists("yearmon_summary_e")) {
    yearmon_summary_e <- metric_yearmon 
  } else {
    yearmon_summary_e <- full_join(yearmon_summary_e, metric_yearmon, by = c("T_change","Q_change", "month"))
  }
  
  metric_monthly <- metric_hourly %>%
    mutate(month = month(datetime)) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # monthly summary
  if (!exists("monthly_summary_e")) {
    monthly_summary_e <- metric_monthly 
  } else {
    monthly_summary_e <- full_join(monthly_summary_e, metric_monthly, by = c("T_change","Q_change", "month"))
  }
  
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # seasonal summary
  if (!exists("seasonal_summary_e")) {
    seasonal_summary_e <- metric_seasonal 
  } else {
    seasonal_summary_e <- full_join(seasonal_summary_e, metric_seasonal, by = c("T_change","Q_change", "season"))
  }
  
  metric_seasonyear <- metric_hourly %>%
    mutate(season = as.seasonyear(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # seasonyear summary
  if (!exists("seasonyear_summary_e")) {
    seasonyear_summary_e <- metric_seasonyear 
  } else {
    seasonyear_summary_e <- full_join(seasonyear_summary_e, metric_seasonyear, by = c("T_change","Q_change", "season"))
  }
  
  metric_jday <- metric_hourly %>%
    mutate(jday = yday(datetime)) %>%
    group_by(jday) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - jday, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # jday summary
  if (!exists("jday_summary_e")) {
    jday_summary_e <- metric_jday 
  } else {
    jday_summary_e <- full_join(jday_summary_e, metric_jday, by = c("T_change","Q_change", "jday"))
  }
  
  
  print(metric)
}

# write the output to delim files
select(daily_summary_e, c(date, Q_change, T_change)) %>% # extract ID not to be rounded
  bind_cols(., round(daily_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_daily.txt"),
              delim = "\t")

select(jday_summary_e, c(jday, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(jday_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_jday.txt"),
              delim = "\t")

select(monthly_summary_e, c(month, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(monthly_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_monthly.txt"),
              delim = "\t")

select(yearmon_summary_e, c(month, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(yearmon_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_yearmon.txt"),
              delim = "\t")

select(seasonal_summary_e,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
              delim = "\t")


select(seasonyear_summary_e,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonyear_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonyear.txt"),
                     delim = "\t")
#==========================================================#


# ========= 4. change_Q_AT ========
scenario <- 'change_Q_AT'
# will have one of these for each timstep summary (daily, monthly, seasonal)
# run for each metric
metrics <- gsub("Hourly_", "", 
                gsub(".txt", "", 
                     list.files(file.path(out_dir, scenario, "Summaries"), pattern = "Hourly")))

for (metric in metrics) {
  metric_hourly <- readr::read_delim(file.path(out_dir, scenario, "Summaries", paste0("Hourly_",
                                                                                      metric, 
                                                                                      ".txt")), show_col_types = F)
  
  metric_daily <- metric_hourly %>%
    mutate(date = ymd(format(datetime, "%Y-%m-%d"))) %>%
    group_by(date) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - date, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # daily summary
  if (!exists("daily_summary_d")) {
    daily_summary_d <- metric_daily 
  } else {
    daily_summary_d <- full_join(daily_summary_d, metric_daily, by = c("T_change","Q_change", "date"))
  }
  
  metric_yearmon <- metric_hourly %>%
    mutate(month = as.yearmon(format(datetime, "%Y-%m"))) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean)%>%
    pivot_longer(cols = - month, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  # yearmon summary
  if (!exists("yearmon_summary_d")) {
    yearmon_summary_d <- metric_yearmon 
  } else {
    yearmon_summary_d <- full_join(yearmon_summary_d, metric_yearmon, by = c("T_change","Q_change", "month"))
  }
  
  metric_monthly <- metric_hourly %>%
    mutate(month = month(datetime)) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # monthly summary
  if (!exists("monthly_summary_d")) {
    monthly_summary_d <- metric_monthly 
  } else {
    monthly_summary_d <- full_join(monthly_summary_d, metric_monthly, by = c("T_change","Q_change", "month"))
  }
  
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # seasonal summary
  if (!exists("seasonal_summary_d")) {
    seasonal_summary_d <- metric_seasonal 
  } else {
    seasonal_summary_d <- full_join(seasonal_summary_d, metric_seasonal, by = c("T_change","Q_change", "season"))
  }
  
  metric_seasonyear <- metric_hourly %>%
    mutate(season = as.seasonyear(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # seasonyear summary
  if (!exists("seasonyear_summary_d")) {
    seasonyear_summary_d <- metric_seasonyear 
  } else {
    seasonyear_summary_d <- full_join(seasonyear_summary_d, metric_seasonyear, by = c("T_change","Q_change", "season"))
  }
  
  metric_jday <- metric_hourly %>%
    mutate(jday = yday(datetime)) %>%
    group_by(jday) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - jday, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # jday summary
  if (!exists("jday_summary_d")) {
    jday_summary_d <- metric_jday 
  } else {
    jday_summary_d <- full_join(jday_summary_d, metric_jday, by = c("T_change","Q_change", "jday"))
  }
  
  
  print(metric)
}

# write the output to delim files
select(daily_summary_d, c(date, Q_change, T_change)) %>% # extract ID not to be rounded
  bind_cols(., round(daily_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_daily.txt"),
                     delim = "\t")

select(jday_summary_d, c(jday, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(jday_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_jday.txt"),
                     delim = "\t")

select(monthly_summary_d, c(month, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(monthly_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_monthly.txt"),
                     delim = "\t")

select(yearmon_summary_d, c(month, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(yearmon_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_yearmon.txt"),
                     delim = "\t")

select(seasonyear_summary_d,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonyear_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonyear.txt"),
                     delim = "\t")

select(seasonal_summary_d,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
                     delim = "\t")
#==========================================================#
