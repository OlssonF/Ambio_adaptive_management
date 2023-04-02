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


#======== Experiment 5; airT + inflowT + inflowQ changes ========
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
  
  GOTMr::run_gotm(yaml_file = gotm_config)
  
  #copy the modelled output file proced by this model and rename
  file.copy(from = "./Output/Mod_temp.txt",
            to = paste0("./Output/Experiment_output/change_Q_AT_ST/Mod_temp_",
                        "T_", all_combinations$t[i],
                        "_Q_", all_combinations$inflowQ[i], 
                        ".txt"),
            overwrite = T)
  
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}
#==================================#

#======== Experiment 4; airT + inflowQ changes ========

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
  
  GOTMr::run_gotm(yaml_file = gotm_config)
  
  #copy the modelled output file proced by this model and rename
  file.copy(from = "./Output/Mod_temp.txt",
            to = paste0("./Output/Experiment_output/change_Q_AT/Mod_temp_",
                        "T_", all_combinations$t[i],
                        "_Q_", all_combinations$inflowQ[i], 
                        ".txt"),
            overwrite = T)
  
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}
#==================================#

#======= Experiment 1: air temperature changes===========
# only want to output the modelled data
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'output', key2 = 'output/Mod', 
                                  key3 = 'format', 
                                  value = 'text')

#make sure the the inflow is set at the baseline values
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'flow', key4 = 'file', 
                                  value = 'Inflow_BACI.dat')

# also change the temperature of inflow to baseline
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'temp', key4 = 'file', 
                                  value = 'Inflow_BACI.dat')

# ensure the column for inflow Q and T are set correctly
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'flow', key4 = 'column', 
                                  value = '1')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'temp', key4 = 'column', 
                                  value = '2', )


# make a list of the inflow iteration files
AT_files <- list.files(path = "Input/AT_iterations", pattern = "met_data_elter") 
#make a list of simplified names which can be used when naming files
AT_names <- gsub(".dat", "", gsub("met_data_elter_", "", AT_files))



for (i in 1:length(AT_files)) {
  
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'meteo', 
                                    key3 = 'airt', key4 = 'file', 
                                    value = file.path('Input/AT_iterations', AT_files[i]))
  
  GOTMr::run_gotm(yaml_file = gotm_config)
  
  #copy the modelled output file proced by this model and rename
  file.copy(from = "./Output/Mod_temp.txt",
            to = paste0("./Output/Experiment_output/change_AT/Mod_temp_T_",
                        AT_names[i],
                        ".txt"),
            overwrite = T)
  
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

#=====================================#

#======= Experiment 2: airT + inflowT changes===========
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'output', key2 = 'output/Mod', 
                                  key3 = 'format', 
                                  value = 'text')

#make sure the the inflow is set at the baseline values
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'flow', key4 = 'file', 
                                  value = 'inflow_BACI.dat')


#make sure the the inflow  columns will change for this run
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'flow', key4 = 'column', 
                                  value = '1')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow', 
                                  key3 = 'temp', key4 = 'column', 
                                  value = '1')

# make a list of the temperature iteration files
# for inflow and air
T_files <- list.files(path = "Input/AT_iterations", pattern = "met_data_elter") 
#make a list of simplified names which can be used when naming files
T_names <- gsub(".dat", "", gsub("met_data_elter_", "", T_files))

for (i in 1:length(T_names)) {
  
  # meteo file
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'meteo', 
                                    key3 = 'airt', key4 = 'file', 
                                    value = file.path('Input/AT_iterations', T_files[i]))#change the file specified in the .xml file
  
  # inflowT file
  inflowT_iteration <- paste0("./Input/AT_iterations/Inflow_T_",T_names[i], ".dat")
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'streams', key2 = 'inflow', 
                                    key3 = 'temp', key4 = 'file', 
                                    value = inflowT_iteration)
  
  GOTMr::run_gotm(yaml_file = gotm_config)
  
  #copy the modelled output file proced by this model and rename
  file.copy(from = "./Output/Mod_temp.txt",
            to = paste0("./Output/Experiment_output/change_AT_ST/Mod_temp_T_",
                        T_names[i],
                        ".txt"),
            overwrite = T)
  
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

#=====================================#

#======== Experiment 3; inflowQ changes ========
#make sure the the met data is set at the baseline values
meteo_file <- "met_data_elter_2012-2019.dat"
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'surface', key2 = 'meteo', 
                                  key3 = 'airt', key4 = 'file', 
                                  value = meteo_file) 



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


# make a list of the inflow iteration files
Q_files <- list.files(path = "./Input/Q_iterations", pattern = ".dat") 
#make a list of simplified names which can be used when naming files
Q_names <- gsub(".dat", "", gsub("Inflow_", "", Q_files))

for (i in 1:length(Q_files)) {
  # loop through each iteration
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'streams', key2 = 'inflow', 
                                    key3 = 'flow', key4 = 'file', 
                                    value = file.path('Input/Q_iterations',Q_files[i]))
  
  GOTMr::run_gotm(yaml_file = gotm_config)
  
  #copy the modelled output file proced by this model and rename
  file.copy(from = "./Output/Mod_temp.txt",
            to = paste0("./Output/Experiment_output/change_Q/Mod_temp_",
                        Q_names[i],
                        ".txt"),
            overwrite = T)
  
  
  print(i)
  Sys.sleep(0.01)
  flush.console()
}
#========================================#

