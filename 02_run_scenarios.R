library(tidyverse)

gotm_config <- 'gotm_runs.yaml'

#set the working  directory to the run folder
dir <- here::here()
setwd(file.path(dir, "GOTM"))

# New set up to have the inflow going to the depth of equal density,

# check start and stop are set
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'start', 
                                  value = '2012-01-01 00:00:00')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'stop', 
                                  value = '2019-12-31 23:00:00')

# no initial conditions so need to change the temperature method
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'temperature', key2 = 'method', 
                                  value = 1)


# assign the calibrated parameters
# light attenuation
g1 <- "0.5"
g2 <- "1.0"

LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g1', 
                                  key3 = 'constant_value', 
                                  value = g1)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g2', 
                                  key3 = 'constant_value',
                                  value = g2)

# Turbulence
k_min <- "3.65e-7"
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'turbulence', key2 = 'turb_param', 
                                  key3 = 'k_min',
                                  value = k_min)

# Scaling factors
swr <- '1.03'
shf <- '0.8'
wind <- '1.20'

LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'surface', key2 = 'meteo', 
                                  key3 = 'swr', key4 = 'scale_factor', 
                                  value = swr)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'surface', key2 = 'fluxes', 
                                  key3 = 'heat', key4 = 'scale_factor', 
                                  value = shf)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'surface', key2 = 'meteo', 
                                  key3 = 'u10', key4 = 'scale_factor', 
                                  value = wind)


#======== Experiment 1; airT + inflowT + inflowQ changes ========
# get all the combinations of airT (and inflowT) and inflowQ changes
Q_vals <- gsub(".dat", "", 
               gsub("Inflow_Q_", "", 
                    list.files(path = "./Input/Q_iterations", pattern = ".dat")))

T_vals <- gsub(".dat", "", 
               gsub("met_data_elter_", "", 
                    list.files(path = "./Input/AT_iterations", pattern = "met_data_elter")))

all_combinations <- expand.grid(inflowQ = Q_vals, 
                                t = T_vals)



inflow_col <- 1
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'temp', key4 = 'column', 
                                  value = inflow_col)

LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'flow', key4 = 'column', 
                                  value = inflow_col)

# vector of years to loop through
years <- seq(2012, 2019, 1)

for (i in 1:nrow(all_combinations)) {
  
  
  # met file
  AT_iteration <- paste0("./Input/AT_iterations/met_data_elter_",all_combinations$t[i], ".dat")
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'meteo', 
                                    key3 = 'airt', key4 = 'file', 
                                    value = AT_iteration)
  
  # inflow temperature file
  inflowT_iteration <- paste0("./Input/AT_iterations/Inflow_T_",all_combinations$t[i], ".dat")
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'streams', key2 = 'inflow', 
                                    key3 = 'temp', key4 = 'file', 
                                    value = inflowT_iteration)
  
  # inflow discharge file
  Q_iteration <- paste0("./Input/Q_iterations/Inflow_Q_",all_combinations$inflowQ[i], ".dat")
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'streams', key2 = 'inflow', 
                                    key3 = 'flow', key4 = 'file', 
                                    value = Q_iteration)
  # want to loop through each year
  for (j in 1:length(years)) {
    year <- years[j]
    start <- paste0(year, '-01-01 00:00:00')
    stop <- paste0(year, '-12-31 23:00:00')
    # change in configuration yaml
    LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                      key1 = 'time', key2 = 'start', 
                                      value = start)
    
    LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                      key1 = 'time', key2 = 'stop', 
                                      value = stop)
    
    GOTMr::run_gotm(yaml_file = gotm_config)
    
    #copy the modelled output file proceed by this model and rename
    file.copy(from = "./Output/Mod_temp.txt",
              to = file.path("./Output/Experiment_output/change_Q_AT_ST", year, paste0("Mod_temp_",
                                                                                        "T_", all_combinations$t[i],
                                                                                        "_Q_", all_combinations$inflowQ[i], 
                                                                                        ".txt")),
              overwrite = T)
  }
  
  
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}
#==================================#

#======== Experiment 2; airT + inflowQ changes ========

# get all the combinations of airT (and inflowT) and inflowQ changes
Q_vals <- gsub(".dat", "", 
               gsub("Inflow_Q_", "", 
                    list.files(path = "./Input/Q_iterations", pattern = ".dat")))

T_vals <- gsub(".dat", "", 
               gsub("met_data_elter_", "", 
                    list.files(path = "./Input/AT_iterations", pattern = "met_data_elter")))

all_combinations <- expand.grid(inflowQ = Q_vals, 
                                t = T_vals)


#make sure the the inflowT is set at the baseline values
inflowT <- "Inflow_BACI.dat"
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'temp', key4 = 'file', 
                                  value = inflowT)

T_col <- 2
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'temp', key4 = 'column', 
                                  value = T_col)
Q_col <- 1
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'flow', key4 = 'column', 
                                  value = Q_col)
# vector of years to loop through
years <- seq(2012, 2019, 1)

for (i in 1:nrow(all_combinations)) {
  
  # met file
  AT_iteration <- paste0("Input/AT_iterations/met_data_elter_",all_combinations$t[i], ".dat")
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'meteo', 
                                    key3 = 'airt', key4 = 'file', 
                                    value = AT_iteration)
  
  # inflow discharge file
  Q_iteration <- paste0("Input/Q_iterations/Inflow_Q_",all_combinations$inflowQ[i], ".dat")
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'streams', key2 = 'inflow', 
                                    key3 = 'flow', key4 = 'file', 
                                    value = Q_iteration)
  
  # want to loop through each year
  for (j in 1:length(years)) {
    year <- years[j]
    start <- paste0(year, '-01-01 00:00:00')
    stop <- paste0(year, '-12-31 23:00:00')
    # change in configuration yaml
    LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                      key1 = 'time', key2 = 'start', 
                                      value = start)
    
    LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                      key1 = 'time', key2 = 'stop', 
                                      value = stop)
    
    GOTMr::run_gotm(yaml_file = gotm_config)
    
    #copy the modelled output file proceed by this model and rename
    file.copy(from = "./Output/Mod_temp.txt",
              to = file.path("./Output/Experiment_output/change_Q_AT", year, paste0("Mod_temp_",
                                                                                       "T_", all_combinations$t[i],
                                                                                       "_Q_", all_combinations$inflowQ[i], 
                                                                                       ".txt")),
              overwrite = T) 
    }
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}
#==================================#