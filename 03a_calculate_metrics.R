library(tidyverse)
library(ggplot2)
library(lubridate)
library(rLakeAnalyzer)
dir <- here::here()
setwd(dir)
out.dir <- 'GOTM/Output/Experiment_output'
source("R/modified_rLA.R")
source("R/helper_functions.R")


# ======= change_Q_AT_ST =======
# experimental scenarios
all_combinations <- read.table("./GOTM/Output/Experiment_output/scenarios.txt", header = T)

elter_bathy <- load.bathy("GOTM/bathymetry_analysis.dat")
# create a table to put the results in
start_date <- ymd_hms("2012-01-01 00:00:00")
end_date <- ymd_hms("2019-12-31 23:00:00")

scenario <- 'change_Q_AT_ST'

# make an empty df to put results in
hourly_schmidt <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_schmidt[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Obs_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column


# loop through reading each file and calculate metrics
for (i in 1:nrow(all_combinations)) {
  
  name_use <- paste0("T_",all_combinations$t[i], "_Q_", all_combinations$inflowQ[i])
  
  temp <- list.files(path = file.path(out.dir, scenario),
                           pattern = paste0("Mod_temp_", name_use, ".txt"),
                           recursive = T, full.names = T)[1:8] |>  
    lapply(read.table, sep = "\t", skip = 9, header = F, 
           col.names = c("datetime", paste0("wtr_",z))) |> 
    bind_rows() %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  
  hourly_surfaceT[,i+1] <- temp$wtr_0.06
 
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  readr::write_delim(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
                     delim = "\t")


select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
                     delim  = "\t")
#=====================================================================#


# ======= change_Q_AT =======

# experimental scenarios
all_combinations <- read.table("./GOTM/Output/Experiment_output/scenarios.txt", header = T)

elter_bathy <- load.bathy("GOTM/bathymetry_analysis.dat")
# create a table to put the results in
start_date <- ymd_hms("2012-01-01 00:00:00")
end_date <- ymd_hms("2019-12-31 23:00:00")

scenario <- 'change_Q_AT'


# make an empty df to put results in
hourly_schmidt <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_schmidt[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column


# loop through reading each file and calculate metrics
for (i in 1:nrow(all_combinations)) {
  name_use <- paste0("T_",all_combinations$t[i], "_Q_", all_combinations$inflowQ[i])
  
  temp <- list.files(path = file.path(out.dir, scenario),
                     pattern = paste0("Mod_temp_", name_use, ".txt"),
                     recursive = T, full.names = T)[1:8] |>  
    lapply(read.table, sep = "\t", skip = 9, header = F, 
           col.names = c("datetime", paste0("wtr_",z))) |> 
    bind_rows()%>% #assign column names
    mutate(datetime = ymd_hms(datetime))

  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  hourly_surfaceT[,i+1] <- temp$wtr_0.06

  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  write.table(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
              sep = "\t", row.names = F, quote = F)

#=====================================================================#


