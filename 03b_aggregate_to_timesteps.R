library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)

source("R/helper_functions.R")
out_dir <- "GOTM/Output/Experiment_output"
#

# ========= change_Q_AT_ST ========
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
  # seasonal summary
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  
  if (!exists("seasonal_summary_e")) {
    seasonal_summary_e <- metric_seasonal 
  } else {
    seasonal_summary_e <- full_join(seasonal_summary_e, metric_seasonal, by = c("T_change","Q_change", "season"))
  }
  
  # seasonyear summary
  metric_seasonyear <- metric_hourly %>%
    mutate(season = as.seasonyear(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  
  if (!exists("seasonyear_summary_e")) {
    seasonyear_summary_e <- metric_seasonyear 
  } else {
    seasonyear_summary_e <- full_join(seasonyear_summary_e, metric_seasonyear, by = c("T_change","Q_change", "season"))
  }
  
  print(metric)
}

# write the output to delim files
select(seasonal_summary_e,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
              delim = "\t")


select(seasonyear_summary_e,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonyear_summary_e[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonyear.txt"),
                     delim = "\t")
#==========================================================#


# ========= change_Q_AT ========
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
  # seasonal summary
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = c("Q_change","T_change"),
                 names_prefix = "Q_",
                 names_sep = "_T_",
                 values_to = metric)
  
  # seasonyear summary
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
  
 
  if (!exists("seasonyear_summary_d")) {
    seasonyear_summary_d <- metric_seasonyear 
  } else {
    seasonyear_summary_d <- full_join(seasonyear_summary_d, metric_seasonyear, by = c("T_change","Q_change", "season"))
  }
  
  
  print(metric)
}

# write the output to delim files
select(seasonyear_summary_d,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonyear_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonyear.txt"),
                     delim = "\t")

select(seasonal_summary_d,  c(season, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
                     delim = "\t")
#==========================================================#
