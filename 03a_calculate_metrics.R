library(tidyverse)
library(ggplot2)
library(lubridate)
library(rLakeAnalyzer)
dir <- here::here()
setwd(dir)
out.dir <- 'GOTM/Output/Experiment_output'
source("R/modified_rLA.R")
source("R/helper_functions.R")


# ======= 5. change_Q_AT_ST =======
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

hourly_vol_av <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_vol_av[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_density_diff <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_density_diff[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_md1 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md1[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_md2 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md2[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_bottomT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_bottomT[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA


# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column


# loop through reading each file and calculate metrics
for (i in 1:nrow(all_combinations)) {
  
  name_use <- paste0("T_",all_combinations$t[i], "_Q_", all_combinations$inflowQ[i])
  
  temp <- read.table(file.path(out.dir, scenario, paste0("Mod_temp_", name_use, ".txt")),
                     sep = "\t", skip = 9, header = F, 
                     col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  hourly_vol_av[,i+1] <-  ts.layer.temperature(temp, elter_bathy, top = 0, bottom = 6)[,2]
  hourly_density_diff[,i+1] <- density.diff(temp[, -1])
  # hourly_md1[,i+1] <- ts.metaD.density(temp) %>%
  #   mutate(top = ifelse(is.nan(top), 6, top)) %>%
  #   select(top)
  # hourly_md2[,i+1] <- md.dens.diff(temp, max.depth = 6, diff = 0.1) #second md method (0.1 dens diff)
  hourly_surfaceT[,i+1] <- temp$wtr_0.06
  hourly_bottomT[,i+1] <- temp$wtr_5.94
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_density_diff, datetime) %>%# extract datetime
  bind_cols(., round(hourly_density_diff[,2:ncol(hourly_density_diff)], 4)) %>% # rounded numeric columns
  readr::write_delim(.,file.path(out.dir, scenario, "Summaries", "Hourly_density_diff.txt"),
                     delim = "\t")

select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  readr::write_delim(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
                     delim = "\t")

select(hourly_vol_av, datetime) %>%# extract datetime
  bind_cols(., round(hourly_vol_av[,2:ncol(hourly_vol_av)], 4)) %>% # rounded numeric columns
  readr::write_delim(.,file.path(out.dir, scenario, "Summaries","Hourly_vol_av_temp.txt"),
                     delim = "\t")


select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  readr::write_delim(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
                     delim  = "\t")

select(hourly_bottomT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_bottomT[,2:ncol(hourly_bottomT)], 4)) %>% # rounded numeric columns
  readr::write_delim(.,file.path(out.dir, scenario, "Summaries","Hourly_bottomT.txt"),
                     delim = "\t")


# select(hourly_md1, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md1[,2:ncol(hourly_md1)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_1.txt",
#               sep = "\t", row.names = F, quote = F)
# 
# select(hourly_md2, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md2[,2:ncol(hourly_md2)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_2.txt",
#               sep = "\t", row.names = F, quote = F)
#=====================================================================#


# ======= 1. change_AT =======

# experimental scenarios
all_combinations <- read.table("./GOTM/Output/Experiment_output/scenarios.txt", header = T)
T_changes <- unique(all_combinations$t)

elter_bathy <- load.bathy("GOTM/bathymetry_analysis.dat")
# create a table to put the results in
start_date <- ymd_hms("2012-01-01 00:00:00")
end_date <- ymd_hms("2019-12-31 23:00:00")

scenario <- 'change_AT'
# make an empty df to put results in
hourly_schmidt <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_schmidt[ ,paste0("T_",T_changes)] <- NA

hourly_vol_av <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_vol_av[ ,paste0("T_",T_changes)] <- NA

hourly_density_diff <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_density_diff[ ,paste0("T_",T_changes)] <- NA

hourly_md1 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md1[ ,paste0("T_",T_changes)] <- NA

hourly_md2 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md2[ ,paste0("T_",T_changes)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("T_",T_changes)] <- NA

hourly_bottomT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_bottomT[ ,paste0("T_",T_changes)] <- NA


# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column

# loop through reading each file and calculate metrics
for (i in 1:length(T_changes)) {
  name_use <- paste0("T_", T_changes[i])
  
  temp <- read.table(file.path(out.dir, scenario, paste0("Mod_temp_", name_use, ".txt")),
                     sep = "\t", skip = 9, header = F, 
                     col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  hourly_vol_av[,i+1] <-  ts.layer.temperature(temp, elter_bathy, top = 0, bottom = 6)[,2]
  hourly_density_diff[,i+1] <- density.diff(temp[, -1])
  # hourly_md1[,i+1] <- ts.metaD.density(temp) %>%
  #   mutate(top = ifelse(is.nan(top), 6, top)) %>% # original md method
  #   select(top)
  # hourly_md2[,i+1] <- md.dens.diff(temp, max.depth = 6, diff = 0.1) #second md method (0.1 dens diff)
  hourly_surfaceT[,i+1] <- temp$wtr_0.06
  hourly_bottomT[,i+1] <- temp$wtr_5.94
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_density_diff, datetime) %>%# extract datetime
  bind_cols(., round(hourly_density_diff[,2:ncol(hourly_density_diff)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_density_diff.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_vol_av, datetime) %>%# extract datetime
  bind_cols(., round(hourly_vol_av[,2:ncol(hourly_vol_av)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_vol_av_temp.txt"),
              sep = "\t", row.names = F, quote = F)

# select(hourly_md1, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md1[,2:ncol(hourly_md1)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_1.txt",
#               sep = "\t", row.names = F, quote = F)
# 
# select(hourly_md2, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md2[,2:ncol(hourly_md2)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_2.txt",
#               sep = "\t", row.names = F, quote = F)

select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  write.table(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_bottomT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_bottomT[,2:ncol(hourly_bottomT)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_bottomT.txt"),
              sep = "\t", row.names = F, quote = F)
#=====================================================================#


# ======= 2. change_AT_ST =======

# experimental scenarios
all_combinations <- read.table("./GOTM/Output/Experiment_output/scenarios.txt", header = T)
T_changes <- unique(all_combinations$t)

elter_bathy <- load.bathy("GOTM/bathymetry_analysis.dat")
# create a table to put the results in
start_date <- ymd_hms("2012-01-01 00:00:00")
end_date <- ymd_hms("2019-12-31 23:00:00")

scenario <- 'change_AT_ST'

# make an empty df to put results in
hourly_schmidt <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_schmidt[ ,paste0("T_",T_changes)] <- NA

hourly_vol_av <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_vol_av[ ,paste0("T_",T_changes)] <- NA

hourly_density_diff <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_density_diff[ ,paste0("T_",T_changes)] <- NA

hourly_md1 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md1[ ,paste0("T_",T_changes)] <- NA

hourly_md2 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md2[ ,paste0("T_",T_changes)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("T_",T_changes)] <- NA

hourly_bottomT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_bottomT[ ,paste0("T_",T_changes)] <- NA


# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column


# loop through reading each file and calculate metrics
for (i in 1:length(T_changes)) {
  
  name_use <- paste0("T_", T_changes[i])
  
  temp <- read.table(file.path(out.dir, scenario, paste0("Mod_temp_", name_use, ".txt")),
                     sep = "\t", skip = 9, header = F, 
                     col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  hourly_vol_av[,i+1] <-  ts.layer.temperature(temp, elter_bathy, top = 0, bottom = 6)[,2]
  hourly_density_diff[,i+1] <- density.diff(temp[, -1])
  hourly_md1[,i+1] <- ts.metaD.density(temp) %>%
    mutate(top = ifelse(is.nan(top), 6, top)) %>%
    select(top)
  hourly_md2[,i+1] <- md.dens.diff(temp, max.depth = 6, diff = 0.1) 
  hourly_surfaceT[,i+1] <- temp$wtr_0.06
  hourly_bottomT[,i+1] <- temp$wtr_5.94
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_density_diff, datetime) %>%# extract datetime
  bind_cols(., round(hourly_density_diff[,2:ncol(hourly_density_diff)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_density_diff.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_vol_av, datetime) %>%# extract datetime
  bind_cols(., round(hourly_vol_av[,2:ncol(hourly_vol_av)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_vol_av_temp.txt"),
              sep = "\t", row.names = F, quote = F)

# select(hourly_md1, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md1[,2:ncol(hourly_md1)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_1.txt",
#               sep = "\t", row.names = F, quote = F)
# 
# select(hourly_md2, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md2[,2:ncol(hourly_md2)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_2.txt",
#               sep = "\t", row.names = F, quote = F)

select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  write.table(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_bottomT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_bottomT[,2:ncol(hourly_bottomT)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_bottomT.txt"),
              sep = "\t", row.names = F, quote = F)
#=====================================================================#


# ======= 3. change_Q =======
# experimental scenarios
all_combinations <- read.table("./GOTM/Output/Experiment_output/scenarios.txt", header = T)
Q_changes <- unique(all_combinations$inflowQ)

elter_bathy <- load.bathy("GOTM/bathymetry_analysis.dat")
# create a table to put the results in
start_date <- ymd_hms("2012-01-01 00:00:00")
end_date <- ymd_hms("2019-12-31 23:00:00")

scenario <- 'change_Q'

# make an empty df to put results in
hourly_schmidt <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_schmidt[ ,paste0("Q_",Q_changes)] <- NA

hourly_vol_av <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_vol_av[ ,paste0("Q_",Q_changes)] <- NA

hourly_density_diff <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_density_diff[ ,paste0("Q_",Q_changes)] <- NA

hourly_md1 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md1[ ,paste0("Q_",Q_changes)] <- NA

hourly_md2 <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_md2[ ,paste0("Q_",Q_changes)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("Q_",Q_changes)] <- NA

hourly_bottomT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_bottomT[ ,paste0("Q_",Q_changes)] <- NA


# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column


# loop through reading each file and calculate metrics
for (i in 1:length(Q_changes)) {
  name_use <- paste0("Q_", Q_changes[i])
  
  temp <- read.table(file.path(out.dir, scenario, paste0("Mod_temp_", name_use, ".txt")),
                     sep = "\t", skip = 9, header = F, 
                     col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  hourly_vol_av[,i+1] <-  ts.layer.temperature(temp, elter_bathy, top = 0, bottom = 6)[,2]
  hourly_density_diff[,i+1] <- density.diff(temp[, -1])
  # hourly_md1[,i+1] <- ts.metaD.density(temp) %>%
  #   mutate(top = ifelse(is.nan(top), 6, top)) %>%
  #   select(top)
  # hourly_md2[,i+1] <-   hourly_md2 <- md.dens.diff(temp, max.depth = 6, diff = 0.1) 
  hourly_surfaceT[,i+1] <- temp$wtr_0.06
  hourly_bottomT[,i+1] <- temp$wtr_5.94
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_density_diff, datetime) %>%# extract datetime
  bind_cols(., round(hourly_density_diff[,2:ncol(hourly_density_diff)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_density_diff.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_vol_av, datetime) %>%# extract datetime
  bind_cols(., round(hourly_vol_av[,2:ncol(hourly_vol_av)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_vol_av_temp.txt"),
              sep = "\t", row.names = F, quote = F)

# select(hourly_md1, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md1[,2:ncol(hourly_md1)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_1.txt",
#               sep = "\t", row.names = F, quote = F)
# 
# select(hourly_md2, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md2[,2:ncol(hourly_md2)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_2.txt",
#               sep = "\t", row.names = F, quote = F)

select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  write.table(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_bottomT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_bottomT[,2:ncol(hourly_bottomT)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_bottomT.txt"),
              sep = "\t", row.names = F, quote = F)
#=====================================================================#


# ======= 4. change_Q_AT =======

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

hourly_vol_av <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_vol_av[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_density_diff <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_density_diff[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

# hourly_md1 <- data.frame(datetime = seq(start_date, end_date, "hour"))
# hourly_md1[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA
# 
# hourly_md2 <- data.frame(datetime = seq(start_date, end_date, "hour"))
# hourly_md2[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_surfaceT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_surfaceT[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA

hourly_bottomT <- data.frame(datetime = seq(start_date, end_date, "hour"))
hourly_bottomT[ ,paste0("Q_",all_combinations$inflowQ, "_T_", all_combinations$t)] <- NA


# column heading, date and depths
z <- as.numeric(read.table("GOTM/Output/Mod_z.txt", 
                           nrows = 1, # the rows are all repeats
                           skip =9, 
                           header = F, 
                           sep ="\t")[1,-1]) * -1 # drop the date column


# loop through reading each file and calculate metrics
for (i in 1:nrow(all_combinations)) {
  name_use <- paste0("T_",all_combinations$t[i], "_Q_", all_combinations$inflowQ[i])
  
  temp <- read.table(file.path(out.dir, scenario, paste0("Mod_temp_", name_use, ".txt")),
                     sep = "\t", skip = 9, header = F, 
                     col.names = c("datetime", paste0("wtr_",z))) %>% #assign column names
    mutate(datetime = ymd_hms(datetime))
  
  #calculate the metrics
  hourly_schmidt[,i+1] <- ts.schmidt.stability(temp, elter_bathy)[,2]
  hourly_vol_av[,i+1] <-  ts.layer.temperature(temp, elter_bathy, top = 0, bottom = 6)[,2]
  hourly_density_diff[,i+1] <- density.diff(temp[, -1])
  # hourly_md1[,i+1] <- ts.metaD.density(temp) %>%
  #   mutate(top = ifelse(is.nan(top), 6, top)) %>%
  #   select(top)
  # hourly_md2[,i+1] <- md.dens.diff(temp, max.depth = 6, diff = 0.1) #second md method (0.1 dens diff)
  hourly_surfaceT[,i+1] <- temp$wtr_0.06
  hourly_bottomT[,i+1] <- temp$wtr_5.94
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dir.create(file.path(out.dir, scenario, 'Summaries'), recursive = T, showWarnings = F)

#write the output to summaries
select(hourly_density_diff, datetime) %>%# extract datetime
  bind_cols(., round(hourly_density_diff[,2:ncol(hourly_density_diff)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_density_diff.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_schmidt, datetime) %>%# extract datetime
  bind_cols(., round(hourly_schmidt[,2:ncol(hourly_schmidt)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries", "Hourly_schmidt.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_vol_av, datetime) %>%# extract datetime
  bind_cols(., round(hourly_vol_av[,2:ncol(hourly_vol_av)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_vol_av_temp.txt"),
              sep = "\t", row.names = F, quote = F)

# select(hourly_md1, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md1[,2:ncol(hourly_md1)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_1.txt",
#               sep = "\t", row.names = F, quote = F)
# 
# select(hourly_md2, datetime) %>%# extract datetime
#   bind_cols(., round(hourly_md2[,2:ncol(hourly_md2)], 4)) %>% # rounded numeric columns
#   write.table(., "./a - airT/Summaries/Hourly_md_2.txt",
#               sep = "\t", row.names = F, quote = F)

select(hourly_surfaceT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_surfaceT[,2:ncol(hourly_surfaceT)], 4)) %>% # rounded numeric columns
  write.table(., file.path(out.dir, scenario, "Summaries","Hourly_surfaceT.txt"),
              sep = "\t", row.names = F, quote = F)

select(hourly_bottomT, datetime) %>%# extract datetime
  bind_cols(., round(hourly_bottomT[,2:ncol(hourly_bottomT)], 4)) %>% # rounded numeric columns
  write.table(.,file.path(out.dir, scenario, "Summaries","Hourly_bottomT.txt"),
              sep = "\t", row.names = F, quote = F)
#=====================================================================#



