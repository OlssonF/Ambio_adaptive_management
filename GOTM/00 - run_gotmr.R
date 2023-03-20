library(tidyverse)
gotm_config <- "gotm.yaml"

setwd('./GOTM')


#analysis of the output

depths <- read.delim("./Output/Obs_z.txt",
                     skip = 9, header = F) #skip first nine lines
z <- -depths[1, 2:51] #extract depths

# Observed data======================================
#read in observed data
obs_temp <-
  read.delim("./Output/Obs_temp.txt",
             skip = 8,
             header = T)
colnames(obs_temp) <-
  c('datetime', paste0("wtr_", z))#wtr_1 is top of water, wtr_50 is bottom
obs_temp <- obs_temp[, c(1, 51:2)] # reverse the order of the columns

obs_temp <- obs_temp |> 
  mutate(datetime = lubridate::ymd_hms(datetime)) |> 
  pivot_longer(cols = -datetime,
               names_to = 'depth', values_to = 'obs_temp',
               names_prefix = 'wtr_' )
#===================================

# Run GOTM scenario
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'start', 
                                  value = '2017-01-01 00:00:00')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'stop', 
                                  value = '2017-12-31 23:00:00')


LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'surface', key2 = 'meteo',
                                  key3 = 'airt', key4 = 'file',
                                  value = 'met_data_elter_2012-2019.dat')


LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow',
                                  key3 = 'flow', key4 = 'file', 
                                  value = 'inflow_BACI.dat')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow',
                                  key3 = 'temp', key4 = 'file', 
                                  value = 'inflow_BACI.dat')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow',
                                  key3 = 'flow', key4 = 'column', 
                                  value = '1')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'streams', key2 = 'inflow',
                                  key3 = 'temp', key4 = 'column', 
                                  value = '2')

# light attenuation
g1 <- "0.48"
g2 <- "0.61"

LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g1', 
                                  key3 = 'constant_value', 
                                  value = g1)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g2', 
                                  key3 = 'constant_value',
                                  value = g2)

# Turbulence
k_min <- "1.4e-7"
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'turbulence', key2 = 'turb_param', 
                                  key3 = 'k_min',
                                  value = k_min)

# Scaling factors
swr <- '0.8'
shf <- '0.8'
wind <- '1'

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

GOTMr::run_gotm(yaml_file = gotm_config)


#read in modelled data
mod_temp <-
  read.delim("./Output/Mod_temp.txt", skip = 8, header = T)
colnames(mod_temp) <-
  c('datetime', paste0("wtr_", z))#wtr_1 is top of water, wtr_50 is bottom
mod_temp <- mod_temp[, c(1, 51:2)] # reverse the order of the columns
mod_temp <- mod_temp |> 
  mutate(datetime = lubridate::ymd_hms(datetime)) |> 
  pivot_longer(cols = -datetime,
               names_to = 'depth', values_to = 'mod_temp',
               names_prefix = 'wtr_')
  #===================================

# compare with observations
full_join(mod_temp, obs_temp, 
          by = c('datetime', 'depth')) |> 
  mutate(depth = round(as.numeric(depth), 1)) |> 
  filter(depth %in% c(0.5, 1,2,3,4,5,5.9)) |> 
  ggplot(aes(x=datetime)) + 
  geom_line(aes(y= mod_temp, colour = 'mod')) +
  geom_line(aes(y= obs_temp, colour = 'obs')) +
  facet_wrap(~depth)

hydroGOF::rmse(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)
hydroGOF::NSE(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)
hydroGOF::mae(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)

