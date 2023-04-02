library(tidyverse)
library(ggplot2)
library(lubridate)
library(zoo)

source("R/helper_functions.R")

# creates a dataframe with the timesteps and the values 
timesteps <- data.frame(time = c("daily", "jday", "monthly", "yearmon", "seasonal", "seasonyear"),
                        val = c("date", "jday", "month", "month", "season", "season"))

metrics <- c("bottomT",
             "density_diff",
             # "md_1",
             # "md_2",
             "schmidt", 
             "surfaceT", 
             "vol_av_temp")

out_dir <- "GOTM/Output/Experiment_output"
#==============================================#



# ============ 5. change_Q_AT_ST ======================
# files to use for looking at changes
# these are aggreagated to different timesteps
scenario <- 'change_Q_AT_ST'
summaries <- list.files(path = file.path(out_dir, scenario, 'Summaries'),
                        pattern = "metric_summary")

#read in all the df
for (file in summaries) {
  temp <- readr::read_delim(file.path(out_dir, scenario, 'Summaries', file), show_col_types = F)
  assign(gsub(".txt", "_summary", gsub("metric_summary_", "", file)),temp)
}

# extract the baseline summary for each timestep
for (i in timesteps$time) {
  t <- get(paste0(i, "_summary")) %>%
    filter(Q_change == 1 & T_change == 0)
  assign(paste0(i, "_base"), t)
}

# for each timestep it extracts the baseline and summary tables

times_use <- timesteps |> 
  filter(time %in% c("seasonal", "seasonyear"))


# then calculates the percentage and absolute changes for each row
for (j in 1:nrow(times_use)) {
  summary <- get(paste0(times_use$time[j], "_summary"))
  # extract the summary table for the timestep
  base <- get(paste0(times_use$time[j], "_base"))
  # extract the baseline table for the timestep
  
  #create empty df to put the values in
  assign(x = "temp_perc_change", summary)
  assign(x = "temp_abs_change", summary)
  
  for (i in 1:nrow(summary)) {
    # calculate the pecentage chage based on the functions above
    # assings values to the relvent columns, based on names
    temp_perc_change[i, metrics] <- perc.change.v(scenario_row = summary[i,], 
                                                  t = as.character(times_use$val[j]),
                                                  base_df = base)
    
    # calculate the absolute chage based on the functions above
    temp_abs_change[i,metrics] <- abs.change.v(summary[i,], 
                                               t = as.character(times_use$val[j]),
                                               base_df = base)
    
    
    # write the temporary df
    select(temp_perc_change[i,], c(times_use$val[j], Q_change, T_change)) %>% # extract id columns
      # only want metrics to 3dp
      bind_cols(., round(temp_perc_change[i,metrics], 3)) %>% 
      write.table(., paste0(out_dir, '/', scenario, '/', "Summaries/percent_change_",times_use$time[j], ".txt"),
                  sep = "\t", quote =F, row.names = F,
                  append = T, # append to existing file
                  col.names = ifelse(file.exists(paste0(out_dir, '/', scenario, '/', 
                                                        "Summaries/percent_change_",
                                                        times_use$time[j], ".txt")) ==
                                       T, # if thhe file already exists
                                     F, # omit the column names
                                     T))
    
    select(temp_abs_change[i,], c(times_use$val[j], Q_change, T_change)) %>% # extract id columns
      # only want metrics to 3dp
      bind_cols(., round(temp_abs_change[i,metrics], 3)) %>% 
      write.table(., paste0(out_dir, '/', scenario, '/', "Summaries/abs_change_",times_use$time[j], ".txt"),
                  sep = "\t", quote =F, row.names = F,
                  append = T, # append to existing file
                  col.names =ifelse(file.exists(paste0(out_dir, '/', scenario, '/',
                                                       "Summaries/abs_change_",times_use$time[j], ".txt")) ==
                                      T, # if thhe file already exists
                                    F, # omit the column names
                                    T))
    
    print(i)
    Sys.sleep(0.01)
    flush.console()
  }
  
  
  print(times_use$time[j])
  Sys.sleep(0.01)
  flush.console()
}
#======================================================#


# ============ 1. change_AT ======================
# files to use for looking at changes
# these are aggreagated to different timesteps
summaries <- list.files(path = "./a - airT/Summaries/",
                        pattern = "metric_summary")

#read in all the df
for (file in summaries) {
  temp <- read.delim(paste0("./a - airT/Summaries/", file))
  assign(gsub(".txt", "_summary", gsub("metric_summary_", "", file)),temp)
}

# extract the baseline summary for each timestep
for (i in timesteps$time) {
  t <- get(paste0(i, "_summary")) %>%
    filter(T_change == 0)
  assign(paste0(i, "_base"),t)
}

# for each timestep it extracts the baseline and summary tables
# then calculates the percentage and absolute changes for each row
for (j in 1:nrow(timesteps)) {
  summary <- get(paste0(timesteps$time[j], "_summary")) # extract the summary table for the timestep
  base <- get(paste0(timesteps$time[j], "_base")) # extract the baseline table for the timestep
  
  #create empty df to put the values in
  assign(x = "temp_perc_change", summary)
  assign(x = "temp_abs_change", summary)
  
  for (i in 1:nrow(summary)) {
    # calculate the pecentage chage based on the functions above
    # assings values to the relvent columns, based on names
    temp_perc_change[i, metrics] <- perc.change.v(summary[i,], 
                                                           t = as.character(timesteps$val[j]),
                                                           base_df = base)
    
    # calculate the absolute chage based on the functions above
    temp_abs_change[i,metrics] <- abs.change.v(summary[i,], 
                                                        t = as.character(timesteps$val[j]),
                                                        base_df = base)
    
    print(i)
    Sys.sleep(0.01)
    flush.console()
  }
  
  # write the temporary df
  select(temp_perc_change, c(timesteps$val[j], T_change)) %>% # extract id columns
    bind_cols(., round(temp_perc_change[,metrics], 3)) %>% 
    write.table(., paste0("./a - airT/Summaries/perc_change_",timesteps$time[j], ".txt"),
                sep = "\t", quote =F, row.names = F)
  
  select(temp_abs_change, c(timesteps$val[j], T_change)) %>% # extract id columns
    bind_cols(., round(temp_abs_change[,metrics], 3)) %>% 
    write.table(., paste0("./a - airT/Summaries/abs_change_",timesteps$time[j], ".txt"),
                sep = "\t", quote =F, row.names = F)
  
  print(timesteps$time[j])
  Sys.sleep(0.01)
  flush.console()
}
#===============================================================#


# ============ 2. change_AT_ST ======================
# files to use for looking at changes
# these are aggreagated to different timesteps
summaries <- list.files(path = "./b - airT + inflowT/Summaries/",
                        pattern = "metric_summary")

#read in all the df
for (file in summaries) {
  temp <- read.delim(paste0("./b - airT + inflowT/Summaries/", file))
  assign(gsub(".txt", "_summary", gsub("metric_summary_", "", file)),temp)
}

# extract the baseline summary for each timestep
for (i in timesteps$time) {
  t <- get(paste0(i, "_summary")) %>%
    filter(T_change == 0)
  assign(paste0(i, "_base"),t)
}

# for each timestep it extracts the baseline and summary tables
# then calculates the percentage and absolute changes for each row
for (j in 1:nrow(timesteps)) {
  summary <- get(paste0(timesteps$time[j], "_summary")) # extract the summary table for the timestep
  base <- get(paste0(timesteps$time[j], "_base")) # extract the baseline table for the timestep
  
  #create empty df to put the values in
  assign(x = "temp_perc_change", summary)
  assign(x = "temp_abs_change", summary)
  
  for (i in 1:nrow(summary)) {
    # calculate the pecentage chage based on the functions above
    # assings values to the relvent columns, based on names
    temp_perc_change[i, metrics] <- perc.change.v(summary[i,], 
                                                           t = as.character(timesteps$val[j]),
                                                           base_df = base)
    
    # calculate the absolute chage based on the functions above
    temp_abs_change[i,metrics] <- abs.change.v(summary[i,], 
                                                        t = as.character(timesteps$val[j]),
                                                        base_df = base)
    
    print(i)
    Sys.sleep(0.01)
    flush.console()
  }
  
  # write the temporary df
  select(temp_perc_change, c(timesteps$val[j],  T_change)) %>% # extract id columns
    bind_cols(., round(temp_perc_change[,metrics], 3)) %>% 
    write.table(., paste0("./b - airT + inflowT/Summaries/perc_change_",timesteps$time[j], ".txt"),
                sep = "\t", quote =F, row.names = F)
  
  select(temp_abs_change, c(timesteps$val[j],  T_change)) %>% # extract id columns
    bind_cols(., round(temp_abs_change[,metrics], 3)) %>% 
    write.table(., paste0("./b - airT + inflowT/Summaries/abs_change_",timesteps$time[j], ".txt"),
                sep = "\t", quote =F, row.names = F)
  
  print(timesteps$time[j])
  Sys.sleep(0.01)
  flush.console()
  
}
#===============================================================#


# ============ 3. change_Q ======================
# files to use for looking at changes
# these are aggreagated to different timesteps
summaries <- list.files(path = "./c - inflowQ/Summaries/",
                        pattern = "metric_summary")

#read in all the df
for (file in summaries) {
  temp <- read.delim(paste0("./c - inflowQ/Summaries/", file))
  assign(gsub(".txt", "_summary", gsub("metric_summary_", "", file)),temp)
}

# extract the baseline summary for each timestep
for (i in timesteps$time) {
  t <- get(paste0(i, "_summary")) %>%
    filter(Q_change == 1)
  assign(paste0(i, "_base"),t)
}

# for each timestep it extracts the baseline and summary tables
# then calculates the percentage and absolute changes for each row
for (j in 1:nrow(timesteps)) {
  summary <- get(paste0(timesteps$time[j], "_summary")) # extract the summary table for the timestep
  base <- get(paste0(timesteps$time[j], "_base")) # extract the baseline table for the timestep
  
  #create empty df to put the values in
  assign(x = "temp_perc_change", summary)
  assign(x = "temp_abs_change", summary)
  
  for (i in 1:nrow(summary)) {
    # calculate the pecentage chage based on the functions above
    # assings values to the relvent columns, based on names
    temp_perc_change[i, metrics] <- perc.change.v(summary[i,], 
                                                           t = as.character(timesteps$val[j]),
                                                           base_df = base)
    
    # calculate the absolute chage based on the functions above
    temp_abs_change[i,metrics] <- abs.change.v(summary[i,], 
                                                        t = as.character(timesteps$val[j]),
                                                        base_df = base)
    
    print(i)
    Sys.sleep(0.01)
    flush.console()
  }
  
  # write the temporary df
  select(temp_perc_change, c(timesteps$val[j], Q_change)) %>% # extract id columns
    bind_cols(., round(temp_perc_change[,metrics], 3)) %>% 
    write.table(., paste0("./c - inflowQ/Summaries/perc_change_",timesteps$time[j], ".txt"),
                sep = "\t", quote =F, row.names = F)
  
  select(temp_abs_change, c(timesteps$val[j], Q_change)) %>% # extract id columns
    bind_cols(., round(temp_abs_change[,metrics], 3)) %>% 
    write.table(., paste0("./c - inflowQ/Summaries/abs_change_",timesteps$time[j], ".txt"),
                sep = "\t", quote =F, row.names = F)
  
  print(timesteps$time[j])
  Sys.sleep(0.01)
  flush.console()
}
#===============================================================#


# ============ 4. change_Q_AT ======================
# files to use for looking at changes
# these are aggreagated to different timesteps
summaries <- list.files(path = "./d - airT + inflowQ/Summaries/",
                        pattern = "metric_summary")

#read in all the df
for (file in summaries) {
  temp <- read.delim(paste0("./d - airT + inflowQ/Summaries/", file))
  assign(gsub(".txt", "_summary", gsub("metric_summary_", "", file)),temp)
}

# extract the baseline summary for each timestep
for (i in timesteps$time) {
  t <- get(paste0(i, "_summary")) %>%
    filter(Q_change == 1 & T_change == 0)
  assign(paste0(i, "_base"),t)
}

# for each timestep it extracts the baseline and summary tables
# then calculates the percentage and absolute changes for each row
for (j in 4:nrow(timesteps)) {
  summary <- get(paste0(timesteps$time[j], "_summary")) # extract the summary table for the timestep
  base <- get(paste0(timesteps$time[j], "_base")) # extract the baseline table for the timestep
  
  #create empty df to put the values in
  assign(x = "temp_perc_change", summary)
  assign(x = "temp_abs_change", summary)
  
  for (i in 1:nrow(summary)) {
    # calculate the pecentage chage based on the functions above
    # assings values to the relvent columns, based on names
    temp_perc_change[i, metrics] <- perc.change.v(summary[i,], 
                                                           t = as.character(timesteps$val[j]),
                                                           base_df = base)
    
    # calculate the absolute chage based on the functions above
    temp_abs_change[i,metrics] <- abs.change.v(summary[i,], 
                                                        t = as.character(timesteps$val[j]),
                                                        base_df = base)
    
    
    # write the temporary df
    select(temp_perc_change[i,], c(timesteps$val[j], Q_change, T_change)) %>% # extract id columns
      bind_cols(., round(temp_perc_change[i,metrics], 3)) %>% 
      write.table(., paste0("./d - airT + inflowQ/Summaries/percent_change_",timesteps$time[j], ".txt"),
                  sep = "\t", quote =F, row.names = F,
                  append = T, # append to existing file
                  col.names = ifelse(file.exists(paste0("./d - airT + inflowQ/Summaries/percent_change_",
                                                       timesteps$time[j]
                                                       ,".txt")) ==
                                      T, # if thhe file already exists
                                    F, # omit the column names
                                    T))
    
    select(temp_abs_change[i,], c(timesteps$val[j], Q_change, T_change)) %>% # extract id columns
      bind_cols(., round(temp_abs_change[i,metrics], 3)) %>% 
      write.table(., paste0("./d - airT + inflowQ/Summaries/abs_change_",timesteps$time[j], ".txt"),
                  sep = "\t", quote =F, row.names = F,
                  append = T, # append to existing file
                  col.names =ifelse(file.exists(paste0("./d - airT + inflowQ/Summaries/abs_change_",
                                                timesteps$time[j],
                                                ".txt")) ==
                                      T, # if thhe file already exists
                                    F, # omit the column names
                                    T))
    
    print(i)
    Sys.sleep(0.01)
    flush.console()
  }
  
  
  print(timesteps$time[j])
  Sys.sleep(0.01)
  flush.console()
}
#===============================================================#

