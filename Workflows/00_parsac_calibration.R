library(RSQLite)
library(tidyverse)


# Script to run the parsac calibration via a batch file
setwd(here::here())
cal_location <- 'GOTM/parsac'
setwd(cal_location)

# write the batch files
config_file <- 'config_elter_parsac_final.xml'
runs <- 2000
bat_outfile <- 'parsac_config.bat'

write(c(paste('parsac', 'calibration', 'run',
              config_file, '--maxfun', runs), '',
        'pause'),
        file = bat_outfile)
  
# Run parsac batch file      
system(bat_outfile, minimized = F, invisible = F)


# extract best values from db
filename <- 'elter_final.db'
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
param_vals <- c(round(as.numeric(as.vector(format(as.numeric(param_vals[c(1:3,5,6)]), scientific = F))), 3),
                param_vals[4])

params <- data.frame(param = c('shf', 'swr', 'wsf', 'g2', 'dummy','k_min'), 
                     vals = param_vals)

write_csv(params, 'calibration_values.csv')
