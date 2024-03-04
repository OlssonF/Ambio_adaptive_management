library(tidyverse)
gotm_config <- "gotm_runs.yaml"

setwd('./GOTM')


# Run GOTM scenario
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'start', 
                                  value = '2018-01-01 00:00:00')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'stop', 
                                  value = '2018-12-31 23:00:00')


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

# read in calibration/validation values
calibration_results <- read_csv('./parsac/calibration_values.csv')

# light attenuation
g1 <- "0.5"
g2 <- calibration_results |> filter(param == 'g2') |> pull(vals)

LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g1', 
                                  key3 = 'constant_value', 
                                  value = g1)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g2', 
                                  key3 = 'constant_value',
                                  value = g2)

# Turbulence
k_min <- calibration_results |> filter(param == 'k_min') |> pull(vals)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'turbulence', key2 = 'turb_param', 
                                  key3 = 'k_min',
                                  value = k_min)

# Scaling factors
swr <- calibration_results |> filter(param == 'swr') |> pull(vals)
shf <- calibration_results |> filter(param == 'shf') |> pull(vals)
wind <- calibration_results |> filter(param == 'wsf') |> pull(vals)

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



#analysis of the output

depths <- read.delim("./Output/Obs_z.txt",
                     skip = 9, header = F) #skip first nine lines
z <- -depths[1, 2:51] #extract depths

# Observed data======================================
#read in observed data
obs_temp <-
  read.delim("./Output/Obs_temp_obs.txt",
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

cal_mod <- mod_temp |> 
  mutate(depth = as.numeric(depth)) |> 
  ggplot(aes(x=datetime, y=depth, z = mod_temp, fill = mod_temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0), name = 'Depth (m)') +
  scale_x_datetime(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'turbo', limits = c(0,30), name = 'Temperature  (°C)')+
  theme_bw() +
  labs(title = 'Calibration - Modelled')
cal_obs <- 
  obs_temp |> 
  mutate(depth = as.numeric(depth)) |> 
  ggplot(aes(x=datetime, y=depth, z = obs_temp, fill = obs_temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0), name = 'Depth (m)') +
  scale_x_datetime(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'turbo', limits = c(0,30), name = 'Temperature  (°C)')+
  theme_bw() +
  labs(title = 'Calibration - Observed')


hydroGOF::rmse(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)
hydroGOF::NSE(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)
hydroGOF::mae(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)

# plot only surface temperature
SWT_cal <- mod_temp |> 
  filter(depth == min(depth)) |> 
  ggplot(aes(x=datetime, y= mod_temp)) +
  geom_point(data = filter(obs_temp, depth == min(depth)),
             aes(y = obs_temp),
             size = 0.9, 
             colour = 'blue', alpha = 0.7) +
  geom_line() +
  theme_bw() +
  labs(x='Date', y = 'Surface water temperature (°C)') +
  coord_cartesian(ylim = c(0, 25))

# plot only surface temperature
BWT_cal <- mod_temp |> 
  filter(depth == max(depth)) |> 
  ggplot(aes(x=datetime, y= mod_temp)) +
  geom_point(data = filter(obs_temp, depth == max(depth)),
             aes(y = obs_temp),
             size = 0.9, 
             colour = 'blue', alpha = 0.7) +
  geom_line() +
  theme_bw() +
  labs(x='Date', y = 'Bottom water temperature (°C)') +
  coord_cartesian(ylim = c(0, 25))


hydroGOF::rmse(sim = filter(mod_temp, depth == 5.94)$mod_temp, 
               obs = filter(obs_temp, depth == 5.94)$obs_temp)
hydroGOF::NSE(sim = filter(mod_temp, depth == 5.94)$mod_temp, 
              obs = filter(obs_temp, depth == 5.94)$obs_temp)
hydroGOF::mae(sim = filter(mod_temp, depth == 5.94)$mod_temp, 
              obs = filter(obs_temp, depth == 5.94)$obs_temp)

# calculate the schmidt stability and plot those
elter_bathy <- load.bathy("bathymetry_analysis.dat")

mod_temp_wide <- mod_temp |> 
  pivot_wider(names_from = depth,
              values_from = mod_temp, names_prefix = 'wtr_') 

mod_schmidt <- ts.schmidt.stability(mod_temp_wide, elter_bathy)

obs_temp_wide <- obs_temp |> 
  pivot_wider(names_from = depth,
              values_from = obs_temp, names_prefix = 'wtr_') 

obs_schmidt <- ts.schmidt.stability(obs_temp_wide, elter_bathy)

schmidt_cal <- ggplot(mod_schmidt, aes(x=datetime, y= schmidt.stability)) +
  geom_point(data = obs_schmidt, colour = 'blue', size = 0.9, alpha = 0.7) +
  geom_line() +
  labs(x='Date', y = expression(paste("Schmidt stability ", "(", J~m^-2, ")"))) +
  theme_bw()

hydroGOF::rmse(sim = mod_schmidt$schmidt.stability, obs = obs_schmidt$schmidt.stability)
hydroGOF::NSE(sim = mod_schmidt$schmidt.stability, obs = obs_schmidt$schmidt.stability)
hydroGOF::mae(sim = mod_schmidt$schmidt.stability, obs = obs_schmidt$schmidt.stability)

#======================================#

#### Validation period ####
# Also run during the validation period
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'start', 
                                  value = '2019-01-01 00:00:00')
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'time', key2 = 'stop', 
                                  value = '2019-12-15 23:00:00')

GOTMr::run_gotm(yaml_file = gotm_config)

# Observed data======================================
#read in observed data
obs_temp <-
  read.delim("./Output/Obs_temp_obs.txt",
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

val_mod <-mod_temp |> 
  mutate(depth = as.numeric(depth)) |> 
  ggplot(aes(x=datetime, y=depth, z = mod_temp, fill = mod_temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0), name = 'Depth (m)') +
  scale_x_datetime(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'turbo', limits = c(0,30), name = 'Temperature  (°C)')+
  theme_bw() +
  labs(title = 'Validation - Modelled')

val_obs <- obs_temp |> 
  mutate(depth = as.numeric(depth)) |> 
  ggplot(aes(x=datetime, y=depth, z = obs_temp, fill = obs_temp)) +
  geom_tile() +
  scale_y_reverse(expand = c(0,0), name = 'Depth (m)') +
  scale_x_datetime(expand = c(0,0)) +
  scale_fill_viridis_c(option = 'turbo', limits = c(0,30), name = 'Temperature (°C)')+
  theme_bw() +
  labs(title = 'Validation - Observed')


hydroGOF::rmse(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)
hydroGOF::NSE(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)
hydroGOF::mae(sim = mod_temp$mod_temp, obs = obs_temp$obs_temp)

# plot only surface temperature
SWT_val <- mod_temp |> 
  filter(depth == min(depth)) |> 
  ggplot(aes(x=datetime, y= mod_temp)) +
  geom_point(data = filter(obs_temp, depth == min(depth)),
             aes(y = obs_temp),
             size = 0.9, 
             colour = 'blue', alpha = 0.7) +
  geom_line() +
  theme_bw() +
  labs(x='Date', y = 'Surface water temperature (°C)') +
  coord_cartesian(ylim = c(0, 25))

# plot only surface temperature
BWT_val <- mod_temp |> 
  filter(depth == max(depth)) |> 
  ggplot(aes(x=datetime, y= mod_temp)) +
  geom_point(data = filter(obs_temp, depth == max(depth)),
             aes(y = obs_temp),
             size = 0.9, 
             colour = 'blue', alpha = 0.7) +
  geom_line() +
  theme_bw() +
  labs(x='Date', y = 'Bottom water temperature (°C)') +
  coord_cartesian(ylim = c(0, 25))
# calculate the schmidt stability and plot those
elter_bathy <- load.bathy("bathymetry_analysis.dat")

mod_temp_wide <- mod_temp |> 
  pivot_wider(names_from = depth,
              values_from = mod_temp, names_prefix = 'wtr_') 

mod_schmidt <- ts.schmidt.stability(mod_temp_wide, elter_bathy)

obs_temp_wide <- obs_temp |> 
  pivot_wider(names_from = depth,
              values_from = obs_temp, names_prefix = 'wtr_') 

obs_schmidt <- ts.schmidt.stability(obs_temp_wide, elter_bathy)

schmidt_val <- ggplot(mod_schmidt, aes(x=datetime, y= schmidt.stability)) +
  geom_point(data = obs_schmidt, colour = 'blue', size = 0.9, alpha = 0.7) +
  geom_line() +
  labs(x='Date', y = expression(paste("Schmidt stability ", "(", J~m^-2, ")"))) +
  theme_bw()

hydroGOF::rmse(sim = mod_schmidt$schmidt.stability, obs = obs_schmidt$schmidt.stability)
hydroGOF::NSE(sim = mod_schmidt$schmidt.stability, obs = obs_schmidt$schmidt.stability)
hydroGOF::mae(sim = mod_schmidt$schmidt.stability, obs = obs_schmidt$schmidt.stability)

#======================================#

cowplot::plot_grid(cal_obs, cal_mod, val_obs, val_mod, nrow = 2) |> 
  ggsave(path = 'Output/Cal_plots/', filename ='Figure_S2.png', 
         width = 18, height = 14.5, units = 'cm')


cowplot::plot_grid((SWT_cal + labs(title = 'Calibration')), (SWT_val  + labs(title = 'Validation')), 
                   BWT_cal, BWT_val,
                   schmidt_cal, schmidt_val, labels = 'AUTO',
                   nrow = 3, align = 'hv')  |> 
  ggsave(path = 'Output/Cal_plots/', filename ='Figure_S3.png', 
         width = 18, height = 21, units = 'cm')
