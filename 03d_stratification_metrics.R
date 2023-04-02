# script to calculate the stratification summaries
  # 1. length of summer stratification
  # 2. dates of onset and overturn

library(tidyverse)
library(lubridate) 
library(ggplot2)
library(rLakeAnalyzer)

out_loc <- 'GOTM/Output/Experiment_output'
source("R/helper_functions.R")
source("r/modified_rLA.R")

# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column

# experimental scenarios
all_combinations <- read.table("GOTM/Output/Experiment_output/scenarios.txt", header = T)

# === 5. change_Q_AT_ST =====

experiment <- 'change_Q_AT_ST'

for (i in 1:nrow(all_combinations)) {
  # read in the hourly water temperatures
  name_use <- paste0("T_",all_combinations$t[i], "_Q_", all_combinations$inflowQ[i])
  
  df.tmp <- readr::read_delim(paste0("GOTM/Output/Experiment_output/",
                                     experiment, "/Mod_temp_", name_use, ".txt"), 
                              delim = "\t", skip = 9, show_col_types = F,
                              col_names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  
  #===water density===
  #using the lake analyzer
  density.tmp <- water.density(df.tmp[,2:51])
  density.tmp <- cbind(density.tmp, df.tmp[,1])
  colnames(density.tmp)[51] <- "Datetime"
  
  density.tmp <- density.tmp %>%
    mutate(Datetime = ymd_hms(Datetime), # format datetime
           month = format(as.Date(Datetime), "%Y-%m"), # format month
           day = ymd(format(as.Date(Datetime), "%Y-%m-%d")), # format day
           season = as.season(Datetime)) # format season, user defined function
  
  
  #calculate the density difference between the top and bottom
  density.tmp$diff <- density.tmp$wtr_5.94 - density.tmp$wtr_0.06
  #create df to decide if strat yes/no
  strat_yn <- density.tmp[,c("Datetime", "season", "month")] %>%
    mutate(dens_diff = density.tmp$diff, # what is the density diff
           temp_diff = df.tmp$wtr_0.06 - df.tmp$wtr_5.94)
  # add the column of top-bottom temp diff from met_temp df (top-bottom)
  
  
  #### start by year
  strat_by_year <- strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(year) %>%
    # longest period of strat
    summarise(longest_strat = max.strat(temp_diff = temp_diff, 
                                        dens_diff = dens_diff), 
              # total strat
              strat_n = total.strat(temp_diff = temp_diff, 
                                    dens_diff = dens_diff),
              # mixis metric
              mixis = longest_strat/strat_n,
              # total inverse strat
              inverse_n = total.inverse(temp_diff = temp_diff, 
                                        dens_diff = dens_diff),
              # strat dates
              start = strat.dates(temp_diff = temp_diff,
                                  dens_diff = dens_diff,
                                  dates = Datetime)[,"start"],
              end = strat.dates(temp_diff = temp_diff,
                                dens_diff = dens_diff,
                                dates = Datetime)[,"end"],
              T_change = all_combinations$t[i],
              Q_change = all_combinations$inflowQ[i])
  
  if (!exists("strat_summary_year_e")) {
    strat_summary_year_e <- strat_by_year
  } else {
    strat_summary_year_e <- bind_rows(strat_summary_year_e, strat_by_year)
  }
  
  #### monthly ###
  #deriving the number of stratified days per month (normal only)
  strat_by_month <- 
    strat_yn %>%
    mutate(month = factor(month(Datetime),
                          levels = 1:12, 
                          labels = month.abb),
           year = year(Datetime)) %>%
    group_by(month, year) %>%
    # normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(T_change = all_combinations$t[i],
           Q_change = all_combinations$inflowQ[i])
  
  if (!exists("strat_summary_month_e")) {
    strat_summary_month_e <- strat_by_month
  } else {
    strat_summary_month_e <- bind_rows(strat_summary_month_e, strat_by_month)
  }
  
  ### seasonal ###
  #deriving the number of stratified days per season (normal only)
  strat_by_season <- 
    strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(season, year) %>%
    # uses the defintion for normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(T_change = all_combinations$t[i],
           Q_change = all_combinations$inflowQ[i])
  
  if (!exists("strat_summary_season_e")) {
    strat_summary_season_e <- strat_by_season
  } else {
    strat_summary_season_e <- bind_rows(strat_summary_season_e, strat_by_season)
  }
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}


readr::write_delim(strat_summary_month_e, file.path(out_loc, experiment, "Summaries/Strat_summary_monthly.txt"), 
                   delim ="\t")
readr::write_delim(strat_summary_season_e, file.path(out_loc, experiment,"Summaries/Strat_summary_seasonal.txt"),
                   delim = '\t')
readr::write_delim(strat_summary_year_e, file.path(out_loc, experiment, "Summaries/Strat_summary_annual.txt"),
                   delim ="\t")

#=======================================================#


# ====== 4. change_Q_AT =======
for (i in 1:nrow(all_combinations)) {
  # read in the hourl/ water temperatures
  name_use <- paste0("T_",all_combinations$t[i], "_Q_", all_combinations$inflowQ[i])
  
  df.tmp <- readr::read_delim(paste0("GOTM/Output/Experiment_output/",
                                     experiment, "/Mod_temp_", name_use, ".txt"), 
                              delim = "\t", skip = 9, show_col_types = F,
                              col_names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #===water density===
  #using the lake analyzer
  density.tmp <- water.density(df.tmp[,2:51])
  density.tmp <- cbind(density.tmp, df.tmp[,1])
  colnames(density.tmp)[51] <- "Datetime"
  
  density.tmp <- density.tmp %>%
    mutate(Datetime = ymd_hms(Datetime), # format datetime
           month = format(as.Date(Datetime), "%Y-%m"), # format month
           day = ymd(format(as.Date(Datetime), "%Y-%m-%d")), # format day
           season = as.season(Datetime)) # format season, user defined function
  
  
  #calculate the density difference between the top and bottom
  density.tmp$diff <- density.tmp$wtr_5.94 - density.tmp$wtr_0.06
  #create df to decide if strat yes/no
  strat_yn <- density.tmp[,c("Datetime", "season", "month")] %>%
    mutate(dens_diff = density.tmp$diff, # what is the density diff
           temp_diff = df.tmp$wtr_0.06 - df.tmp$wtr_5.94)
  # add the column of top-bottom temp diff from met_temp df (top-bottom)
  
  
  #### start by year
  strat_by_year <- strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(year) %>%
    # longest period of strat
    summarise(longest_strat = max.strat(temp_diff = temp_diff, 
                                        dens_diff = dens_diff), 
              # total strat
              strat_n = total.strat(temp_diff = temp_diff, 
                                    dens_diff = dens_diff),
              # mixis metric
              mixis = longest_strat/strat_n,
              # total inverse strat
              inverse_n = total.inverse(temp_diff = temp_diff, 
                                        dens_diff = dens_diff),
              # strat dates
              start = strat.dates(temp_diff = temp_diff,
                                  dens_diff = dens_diff,
                                  dates = Datetime)[,"start"],
              end = strat.dates(temp_diff = temp_diff,
                                dens_diff = dens_diff,
                                dates = Datetime)[,"end"],
              Q_change = all_combinations$inflowQ[i],
              T_change = all_combinations$t[i])
  
  if (!exists("strat_summary_year_d")) {
    strat_summary_year_d <- strat_by_year
  } else {
    strat_summary_year_d <- bind_rows(strat_summary_year_d, strat_by_year)
  }
  
  #### monthly ###
  #deriving the number of stratified days per month (normal only)
  strat_by_month <- 
    strat_yn %>%
    mutate(month = factor(month(Datetime),
                          levels = 1:12, 
                          labels = month.abb),
           year = year(Datetime)) %>%
    group_by(month, year) %>%
    # normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(Q_change = all_combinations$inflowQ[i],
           T_change = all_combinations$t[i])
  
  if (!exists("strat_summary_month")) {
    strat_summary_month_d <- strat_by_month
  } else {
    strat_summary_month_d <- bind_rows(strat_summary_month_d, strat_by_month)
  }
  
  ### seasonal ###
  #deriving the number of stratified days per season (normal only)
  strat_by_season <- 
    strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(season, year) %>%
    # uses the defintion for normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(Q_change = all_combinations$inflowQ[i],
           T_change = all_combinations$t[i])
  
  if (!exists("strat_summary_season_d")) {
    strat_summary_season_d <- strat_by_season
  } else {
    strat_summary_season_d <- bind_rows(strat_summary_season_d, strat_by_season)
  }
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}


readr::write_delim(strat_summary_month_d, file.path(out_loc, experiment, "Summaries/Strat_summary_monthly.txt"), 
                   delim ="\t")
readr::write_delim(strat_summary_season_d, file.path(out_loc, experiment,"Summaries/Strat_summary_seasonal.txt"),
                   delim = '\t')
readr::write_delim(strat_summary_year_d, file.path(out_loc, experiment, "Summaries/Strat_summary_annual.txt"),
                   delim ="\t")

#=======================================================#



# === a - airT =====
experiment <- 'change_AT'

for (i in unique(all_combinations$t)) {
  # read in the hourl/ water temperatures
  name_use <- paste0("T_",i)
  
  df.tmp <- readr::read_delim(paste0("GOTM/Output/Experiment_output/",
                              experiment, "/Mod_temp_", name_use, ".txt"), 
                       delim = "\t", skip = 9,
                       col_names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #===water density===
  #using the lake analyzer
  density.tmp <- water.density(df.tmp[,2:51])
  density.tmp <- cbind(density.tmp, df.tmp[,1])
  colnames(density.tmp)[51] <- "Datetime"
  
  density.tmp <- density.tmp %>%
    mutate(Datetime = ymd_hms(Datetime), # format datetime
           month = format(as.Date(Datetime), "%Y-%m"), # format month
           day = ymd(format(as.Date(Datetime), "%Y-%m-%d")), # format day
           season = as.season(Datetime)) # format season, user defined function
  
  
  #calculate the density difference between the top and bottom
  density.tmp$diff <- density.tmp$wtr_5.94 - density.tmp$wtr_0.06
  #create df to decide if strat yes/no
  strat_yn <- density.tmp[,c("Datetime", "season", "month")] %>%
    mutate(dens_diff = density.tmp$diff, # what is the density diff
           temp_diff = df.tmp$wtr_0.06 - df.tmp$wtr_5.94)
  # add the column of top-bottom temp diff from met_temp df (top-bottom)
  
  
  #### start by year
  strat_by_year <- strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(year) %>%
    # longest period of strat
    summarise(longest_strat = max.strat(temp_diff = temp_diff, 
                                        dens_diff = dens_diff), 
              # total strat
              strat_n = total.strat(temp_diff = temp_diff, 
                                    dens_diff = dens_diff),
              # mixis metric
              mixis = longest_strat/strat_n,
              # total inverse strat
              inverse_n = total.inverse(temp_diff = temp_diff, 
                                        dens_diff = dens_diff),
              # strat dates
              start = strat.dates(temp_diff = temp_diff,
                                  dens_diff = dens_diff,
                                  dates = Datetime)[,"start"],
              end = strat.dates(temp_diff = temp_diff,
                                dens_diff = dens_diff,
                                dates = Datetime)[,"end"],
              T_change = i)
  
  if (!exists("strat_summary_year_a")) {
    strat_summary_year_a <- strat_by_year
  } else {
    strat_summary_year_a <- bind_rows(strat_summary_year_a, strat_by_year)
  }
  
  #### monthly ###
  #deriving the number of stratified days per month (normal only)
  strat_by_month <- 
    strat_yn %>%
    mutate(month = factor(month(Datetime),
                          levels = 1:12, 
                          labels = month.abb),
           year = year(Datetime)) %>%
    group_by(month, year) %>%
    # normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(T_change = i)
  
  if (!exists("strat_summary_month_a")) {
    strat_summary_month_a <- strat_by_month
  } else {
    strat_summary_month_a <- bind_rows(strat_summary_month_a, strat_by_month)
  }
  
  ### seasonal ###
  #deriving the number of stratified days per season (normal only)
  strat_by_season <- 
    strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(season, year) %>%
    # uses the defintion for normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(T_change = i)
  
  if (!exists("strat_summary_season_a")) {
    strat_summary_season_a <- strat_by_season
  } else {
    strat_summary_season_a <- bind_rows(strat_summary_season_a, strat_by_season)
  }
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}


readr::write_delim(strat_summary_month_a, file.path(out_loc, experiment, "Summaries/Strat_summary (monthly).txt"), 
                   delim ="\t")
readr::write_delim(strat_summary_season_a, file.path(out_loc, experiment,"Summaries/Strat_summary (seasonal).txt"),
            delim = '\t')
readr::write_delim(strat_summary_year_a, file.path(out_loc, experiment, "Summaries/Strat_summary (annual).txt"),
            delim ="\t")

#=======================================================#


# === b - airT + inflowT =====
for (i in unique(all_combinations$t)) {
  # read in the hourl/ water temperatures
  name_use <- paste0("T_",i)
  
  df.tmp <- read.table(paste0("b - airT + inflowT/Mod_temp_", name_use, ".txt"),
                       sep = "\t", skip = 9, header = F, 
                       col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #===water density===
  #using the lake analyzer
  density.tmp <- water.density(df.tmp[,2:51])
  density.tmp <- cbind(density.tmp, df.tmp[,1])
  colnames(density.tmp)[51] <- "Datetime"
  
  density.tmp <- density.tmp %>%
    mutate(Datetime = ymd_hms(Datetime), # format datetime
           month = format(as.Date(Datetime), "%Y-%m"), # format month
           day = ymd(format(as.Date(Datetime), "%Y-%m-%d")), # format day
           season = as.season(Datetime)) # format season, user defined function
  
  
  #calculate the density difference between the top and bottom
  density.tmp$diff <- density.tmp$wtr_5.94 - density.tmp$wtr_0.06
  #create df to decide if strat yes/no
  strat_yn <- density.tmp[,c("Datetime", "season", "month")] %>%
    mutate(dens_diff = density.tmp$diff, # what is the density diff
           temp_diff = df.tmp$wtr_0.06 - df.tmp$wtr_5.94)
  # add the column of top-bottom temp diff from met_temp df (top-bottom)
  
  
  #### start by year
  strat_by_year <- strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(year) %>%
    # longest period of strat
    summarise(longest_strat = max.strat(temp_diff = temp_diff, 
                                        dens_diff = dens_diff), 
              # total strat
              strat_n = total.strat(temp_diff = temp_diff, 
                                    dens_diff = dens_diff),
              # mixis metric
              mixis = longest_strat/strat_n,
              # total inverse strat
              inverse_n = total.inverse(temp_diff = temp_diff, 
                                        dens_diff = dens_diff),
              # strat dates
              start = strat.dates(temp_diff = temp_diff,
                                  dens_diff = dens_diff,
                                  dates = Datetime)[,"start"],
              end = strat.dates(temp_diff = temp_diff,
                                dens_diff = dens_diff,
                                dates = Datetime)[,"end"],
              T_change = i)
  
  if (!exists("strat_summary_year_b")) {
    strat_summary_year_b <- strat_by_year
  } else {
    strat_summary_year_b <- bind_rows(strat_summary_year_b, strat_by_year)
  }
  
  #### monthly ###
  #deriving the number of stratified days per month (normal only)
  strat_by_month <- 
    strat_yn %>%
    mutate(month = factor(month(Datetime),
                          levels = 1:12, 
                          labels = month.abb),
           year = year(Datetime)) %>%
    group_by(month, year) %>%
    # normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(T_change = i)
  
  if (!exists("strat_summary_month_b")) {
    strat_summary_month_b <- strat_by_month
  } else {
    strat_summary_month_b <- bind_rows(strat_summary_month_b, strat_by_month)
  }
  
  ### seasonal ###
  #deriving the number of stratified days per season (normal only)
  strat_by_season <- 
    strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(season, year) %>%
    # uses the defintion for normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(T_change = i)
  
  if (!exists("strat_summary_season_b")) {
    strat_summary_season_b <- strat_by_season
  } else {
    strat_summary_season_b <- bind_rows(strat_summary_season_b, strat_by_season)
  }
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}


write.table(strat_summary_month_b, "./b - airT + inflowT/Summaries/Strat_summary (monthly).txt",
            sep ="\t", row.names = F, quote = F)
write.table(strat_summary_season_b, "./b - airT + inflowT/Summaries/Strat_summary (seasonal).txt",
            sep ="\t", row.names = F, quote = F)
write.table(strat_summary_year_b, "./b - airT + inflowT/Summaries/Strat_summary (annual).txt",
            sep ="\t", row.names = F, quote = F)

#=======================================================#


# === c - inflowQ =====
for (i in unique(all_combinations$inflowQ)) {
  # read in the hourl/ water temperatures
  name_use <- paste0("Q_", i)
  
  df.tmp <- read.table(paste0("c - inflowQ/Mod_temp_", name_use, ".txt"),
                       sep = "\t", skip = 9, header = F, 
                       col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #===water density===
  #using the lake analyzer
  density.tmp <- water.density(df.tmp[,2:51])
  density.tmp <- cbind(density.tmp, df.tmp[,1])
  colnames(density.tmp)[51] <- "Datetime"
  
  density.tmp <- density.tmp %>%
    mutate(Datetime = ymd_hms(Datetime), # format datetime
           month = format(as.Date(Datetime), "%Y-%m"), # format month
           day = ymd(format(as.Date(Datetime), "%Y-%m-%d")), # format day
           season = as.season(Datetime)) # format season, user defined function
  
  
  #calculate the density difference between the top and bottom
  density.tmp$diff <- density.tmp$wtr_5.94 - density.tmp$wtr_0.06
  #create df to decide if strat yes/no
  strat_yn <- density.tmp[,c("Datetime", "season", "month")] %>%
    mutate(dens_diff = density.tmp$diff, # what is the density diff
           temp_diff = df.tmp$wtr_0.06 - df.tmp$wtr_5.94)
  # add the column of top-bottom temp diff from met_temp df (top-bottom)
  
  
  #### start by year
  strat_by_year <- strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(year) %>%
    # longest period of strat
    summarise(longest_strat = max.strat(temp_diff = temp_diff, 
                                        dens_diff = dens_diff), 
              # total strat
              strat_n = total.strat(temp_diff = temp_diff, 
                                    dens_diff = dens_diff),
              # mixis metric
              mixis = longest_strat/strat_n,
              # total inverse strat
              inverse_n = total.inverse(temp_diff = temp_diff, 
                                        dens_diff = dens_diff),
              # strat dates
              start = strat.dates(temp_diff = temp_diff,
                                  dens_diff = dens_diff,
                                  dates = Datetime)[,"start"],
              end = strat.dates(temp_diff = temp_diff,
                                dens_diff = dens_diff,
                                dates = Datetime)[,"end"],
              Q_change = i)
  
  if (!exists("strat_summary_year_c")) {
    strat_summary_year_c <- strat_by_year
  } else {
    strat_summary_year_c <- bind_rows(strat_summary_year_c, strat_by_year)
  }
  
  #### monthly ###
  #deriving the number of stratified days per month (normal only)
  strat_by_month <- 
    strat_yn %>%
    mutate(month = factor(month(Datetime),
                          levels = 1:12, 
                          labels = month.abb),
           year = year(Datetime)) %>%
    group_by(month, year) %>%
    # normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(Q_change = i)
  
  if (!exists("strat_summary_month_c")) {
    strat_summary_month_c <- strat_by_month
  } else {
    strat_summary_month_c <- bind_rows(strat_summary_month_c, strat_by_month)
  }
  
  ### seasonal ###
  #deriving the number of stratified days per season (normal only)
  strat_by_season <- 
    strat_yn %>%
    mutate(year = year(Datetime)) %>%
    group_by(season, year) %>%
    # uses the defintion for normal stratification
    summarise(str_norm = total.strat(temp_diff = temp_diff,
                                     dens_diff = dens_diff)) %>%
    mutate(Q_change = i)
  
  if (!exists("strat_summary_season_c")) {
    strat_summary_season_c <- strat_by_season
  } else {
    strat_summary_season_c <- bind_rows(strat_summary_season_c, strat_by_season)
  }
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}


write.table(strat_summary_month_c, "./c - inflowQ/Summaries/Strat_summary (monthly).txt",
            sep ="\t", row.names = F, quote = F)
write.table(strat_summary_season_c, "./c - inflowQ/Summaries/Strat_summary (seasonal).txt",
            sep ="\t", row.names = F, quote = F)
write.table(strat_summary_year_c, "./c - inflowQ/Summaries/Strat_summary (annual).txt",
            sep ="\t", row.names = F, quote = F)

#=======================================================#






