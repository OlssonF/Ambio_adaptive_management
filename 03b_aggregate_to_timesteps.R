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


# ========= 1. change_AT ========
scenario <- 'change_AT'
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
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # daily summary
  if (!exists("daily_summary_a")) {
    daily_summary_a <- metric_daily 
  } else {
    daily_summary_a <- full_join(daily_summary_a, metric_daily, by = c("T_change", "date"))
  }
  
  metric_yearmon <- metric_hourly %>%
    mutate(month = as.yearmon(format(datetime, "%Y-%m"))) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  # yearmon summary
  if (!exists("yearmon_summary_a")) {
    yearmon_summary_a <- metric_yearmon 
  } else {
    yearmon_summary_a <- full_join(yearmon_summary_a, metric_yearmon, by = c("T_change", "month"))
  }
  
  metric_monthly <- metric_hourly %>%
    mutate(month = month(datetime)) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # monthly summary
  if (!exists("monthly_summary_a")) {
    monthly_summary_a <- metric_monthly 
  } else {
    monthly_summary_a <- full_join(monthly_summary_a, metric_monthly, by = c("T_change", "month"))
  }
  
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # seasonal summary
  if (!exists("seasonal_summary_a")) {
    seasonal_summary_a <- metric_seasonal 
  } else {
    seasonal_summary_a <- full_join(seasonal_summary_a, metric_seasonal, by = c("T_change", "season"))
  }
  
  
  
  metric_season_year <- metric_hourly %>%
    mutate(season = as.seasonyear(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # seasonal summary
  if (!exists("seasonal_summary_a")) {
    seasonal_summary_a <- metric_seasonal 
  } else {
    seasonal_summary_a <- full_join(seasonal_summary_a, metric_seasonal, by = c("T_change", "season"))
  }
  
  metric_jday <- metric_hourly %>%
    mutate(jday = yday(datetime)) %>%
    group_by(jday) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - jday, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # jday summary
  if (!exists("jday_summary_a")) {
    jday_summary_a <- metric_jday 
  } else {
    jday_summary_a <- full_join(jday_summary_a, metric_jday, by = c("T_change", "jday"))
  }
  
  
  print(metric)
}

# write the output to delim files
select(daily_summary_a, c(date, T_change)) %>% # extract ID not to be rounded
  bind_cols(., round(daily_summary_a[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_daily.txt"),
              sep = "\t", row.names = F, quote= F)

select(jday_summary_a, c(jday, T_change)) %>%# extract datetime
  bind_cols(., round(jday_summary_a[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_jday.txt"),
              sep = "\t", row.names = F, quote= F)

select(monthly_summary_a, c(month, T_change)) %>%# extract datetime
  bind_cols(., round(monthly_summary_a[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_monthly.txt"),
              sep = "\t", row.names = F, quote= F)

select(yearmon_summary_a, c(month, T_change)) %>%# extract datetime
  bind_cols(., round(yearmon_summary_a[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_yearmon.txt"),
              sep = "\t", row.names = F, quote= F)

select(seasonal_summary_a, c(season, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_a[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
              sep = "\t", row.names = F, quote= F)
#==========================================================#


# ========= 2. change_AT_ST ========
scenario <- 'change_AT_ST'
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
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # daily summary
  if (!exists("daily_summary_b")) {
    daily_summary_b <- metric_daily 
  } else {
    daily_summary_b <- full_join(daily_summary_b, metric_daily, by = c("T_change", "date"))
  }
  
  metric_yearmon <- metric_hourly %>%
    mutate(month = as.yearmon(format(datetime, "%Y-%m"))) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  # yearmon summary
  if (!exists("yearmon_summary_b")) {
    yearmon_summary_b <- metric_yearmon 
  } else {
    yearmon_summary_b <- full_join(yearmon_summary_b, metric_yearmon, by = c("T_change", "month"))
  }
  
  metric_monthly <- metric_hourly %>%
    mutate(month = month(datetime)) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # monthly summary
  if (!exists("monthly_summary_b")) {
    monthly_summary_b <- metric_monthly 
  } else {
    monthly_summary_b <- full_join(monthly_summary_b, metric_monthly, by = c("T_change", "month"))
  }
  
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # seasonal summary
  if (!exists("seasonal_summary_b")) {
    seasonal_summary_b <- metric_seasonal 
  } else {
    seasonal_summary_b <- full_join(seasonal_summary_b, metric_seasonal, by = c("T_change", "season"))
  }
  
  metric_jday <- metric_hourly %>%
    mutate(jday = yday(datetime)) %>%
    group_by(jday) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - jday, 
                 names_to = "T_change",
                 names_prefix = "T_",
                 values_to = metric)
  
  # jday summary
  if (!exists("jday_summary_b")) {
    jday_summary_b <- metric_jday 
  } else {
    jday_summary_b <- full_join(jday_summary_b, metric_jday, by = c("T_change", "jday"))
  }
  
  
  print(metric)
}

# write the output to delim files
select(daily_summary_b, c(date, T_change)) %>% # extract ID not to be rounded
  bind_cols(., round(daily_summary_b[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_daily.txt"),
              sep = "\t", row.names = F, quote= F)

select(jday_summary_b, c(jday, T_change)) %>%# extract datetime
  bind_cols(., round(jday_summary_b[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_jday.txt"),
              sep = "\t", row.names = F, quote= F)

select(monthly_summary_b, c(month, T_change)) %>%# extract datetime
  bind_cols(., round(monthly_summary_b[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_monthly.txt"),
              sep = "\t", row.names = F, quote= F)

select(yearmon_summary_b, c(month, T_change)) %>%# extract datetime
  bind_cols(., round(yearmon_summary_b[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_yearmon.txt"),
              sep = "\t", row.names = F, quote= F)

select(seasonal_summary_b, c(season, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_b[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
              sep = "\t", row.names = F, quote= F)
#==========================================================#



# ========= 3. change_Q ========
scenario <- 'change_Q'
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
                 names_to = "Q_change",
                 names_prefix = "Q_",
                 values_to = metric)
  
  # daily summary
  if (!exists("daily_summary_c")) {
    daily_summary_c <- metric_daily 
  } else {
    daily_summary_c <- full_join(daily_summary_c, metric_daily, by = c("Q_change", "date"))
  }
  
  metric_yearmon <- metric_hourly %>%
    mutate(month = as.yearmon(format(datetime, "%Y-%m"))) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = "Q_change",
                 names_prefix = "Q_",
                 values_to = metric)
  # yearmon summary
  if (!exists("yearmon_summary_c")) {
    yearmon_summary_c <- metric_yearmon 
  } else {
    yearmon_summary_c <- full_join(yearmon_summary_c, metric_yearmon, by = c("Q_change", "month"))
  }
  
  metric_monthly <- metric_hourly %>%
    mutate(month = month(datetime)) %>%
    group_by(month) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - month, 
                 names_to = "Q_change",
                 names_prefix = "Q_",
                 values_to = metric)
  
  # monthly summary
  if (!exists("monthly_summary_c")) {
    monthly_summary_c <- metric_monthly 
  } else {
    monthly_summary_c <- full_join(monthly_summary_c, metric_monthly, by = c("Q_change", "month"))
  }
  
  metric_seasonal <- metric_hourly %>%
    mutate(season = as.season(datetime)) %>%
    group_by(season) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - season, 
                 names_to = "Q_change",
                 names_prefix = "Q_",
                 values_to = metric)
  
  # seasonal summary
  if (!exists("seasonal_summary_c")) {
    seasonal_summary_c <- metric_seasonal 
  } else {
    seasonal_summary_c <- full_join(seasonal_summary_c, metric_seasonal, by = c("Q_change", "season"))
  }
  
  metric_jday <- metric_hourly %>%
    mutate(jday = yday(datetime)) %>%
    group_by(jday) %>%
    summarise_if(is.numeric, mean) %>%
    pivot_longer(cols = - jday, 
                 names_to = "Q_change",
                 names_prefix = "Q_",
                 values_to = metric)
  
  # jday summary
  if (!exists("jday_summary_c")) {
    jday_summary_c <- metric_jday 
  } else {
    jday_summary_c <- full_join(jday_summary_c, metric_jday, by = c("Q_change", "jday"))
  }
  
  
  print(metric)
}

# write the output to delim files
select(daily_summary_c, c(date, Q_change)) %>% # extract ID not to be rounded
  bind_cols(., round(daily_summary_c[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_daily.txt"),
              sep = "\t", row.names = F, quote= F)

select(jday_summary_c, c(jday, Q_change)) %>%# extract datetime
  bind_cols(., round(jday_summary_c[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_jday.txt"),
              sep = "\t", row.names = F, quote= F)

select(monthly_summary_c, c(month, Q_change)) %>%# extract datetime
  bind_cols(., round(monthly_summary_c[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_monthly.txt"),
              sep = "\t", row.names = F, quote= F)

select(yearmon_summary_c, c(month, Q_change)) %>%# extract datetime
  bind_cols(., round(yearmon_summary_c[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_yearmon.txt"),
              sep = "\t", row.names = F, quote= F)

select(seasonal_summary_c, c(season, Q_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_c[,metrics], 4)) %>% # rounded numeric columns
  write.table(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
              sep = "\t", row.names = F, quote= F)
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

select(jday_summary_d, c(date, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(jday_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_jday.txt"),
                     delim = "\t")

select(monthly_summary_d, c(date, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(monthly_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_monthly.txt"),
                     delim = "\t")

select(yearmon_summary_d, c(date, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(yearmon_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_yearmon.txt"),
                     delim = "\t")

select(seasonal_summary_d,  c(date, Q_change, T_change)) %>%# extract datetime
  bind_cols(., round(seasonal_summary_d[,metrics], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out_dir, scenario, "Summaries", "metric_summary_seasonal.txt"),
                     delim = "\t")
#==========================================================#
