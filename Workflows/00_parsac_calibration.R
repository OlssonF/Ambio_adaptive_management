library(RSQLite)
library(tidyverse)


# Script to run the parsac calibration via a batch file
setwd(here::here())
cal_location <- 'GOTM/parsac'
setwd(cal_location)

# write the batch files
config_file <- 'config_elter_parsac_v2.xml'
runs <- 2000
bat_outfile <- 'parsac_config.bat'

write(c(paste('parsac', 'calibration', 'run',
              config_file, '--maxfun', runs), '',
        'pause'),
        file = bat_outfile)
  
# Run parsac batch file      
system(bat_outfile, minimized = F, invisible = F)


# extract best values from db
filename <- 'elter_20230326.db'
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)


param_vals <- dbReadTable(db,"results") |> 
  # maximum ln likelihood
  filter(lnlikelihood == max(lnlikelihood)) |> 
  select(parameters) |> 
  pull() |> 
  # parameter values held in one string - to be split
  str_split(';', simplify = T) 

# make into a usable format
param_vals <- round(as.numeric(as.vector(format(as.numeric(param_vals), scientific = F))), 3)

params <- data.frame(param = c('shf', 'swr', 'wsf', 'k_min', 'g1', 'g2', 'dummy'), 
                     vals = param_vals)

write_csv(params, 'calibration_values.csv')