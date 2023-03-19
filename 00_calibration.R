library("rLakeAnalyzer")
library("lubridate")
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")
library("hydroGOF")

gotm_config <- "gotm_elter.yaml"

#create vectors of the possible values of each parameter to be tested
shf <- seq(from= 0.8, to =1.2, by = 0.1)
swr <- seq(from= 0.8, to =1.2, by = 0.1)
wind <- seq(from= 0.8, to =1.2, by = 0.1)

l <- list(shf, swr, wind)
#function creates grid of all the possible combinations of the supplied vector
iterations <- expand.grid(l)
colnames(iterations) <- c("shf", "swr", "wind")


#============================================================
#analysis of the output

depths <- read.delim("./output/Obs_z.txt",
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
obs_temp$datetime <- ymd_hms(obs_temp$datetime)
#===================================

#===================================
#do calculations of metrics

bathy <- load.bathy("./bathymetry_analysis.dat")


#mixed depth
obs_temp$meta_top <- ts.meta.depths(obs_temp)[, 2]

#av temp
obs_temp$av.temp <-
  ts.layer.temperature(obs_temp[, 1:51], top = 0, bottom = 6, bathy)[, 2]

#schmidt stability
obs_temp$schmidt <- ts.schmidt.stability(obs_temp[, 1:51], bathy)[, 2]
#==================================


#==================================
#get the month and year only
obs_temp$month <- format(as.Date(obs_temp$datetime), "%Y-%m")

obs_temp[1:5, 50:55]

#set the month as a factor
obs_temp$month <- as.factor(obs_temp$month)

#group the data frame by month
grouped_obs_month <- group_by(obs_temp, month)

#specify the columns that are to be averaged (ie not date or season)
names <-
  colnames(grouped_obs_month[2:54]) #gets the names of the depth columns

#find column averages
monthly_av_obs <- grouped_obs_month %>%
  summarise_at(names, funs(name = mean(., na.rm = T))) #gets a mean of each of the specified column
#as it is a grouped table it returns a mean for each season

#adjust col names
colnames(monthly_av_obs) <- c("month", names)


#melt data
monthly_av_obs <-
  melt(
    monthly_av_obs,
    na.rm = F,
    value.name = "temp",
    id.vars = "month",
    variable.name = "depth"
  )
monthly_av_obs$month <- as.factor(monthly_av_obs$month)

#remove wtr from numbers
monthly_av_obs$depth <- gsub("wtr_", "", monthly_av_obs$depth)


#take off the metrics into seperate df
monthly_metrics_obs <-
  subset(monthly_av_obs,
         depth == "meta_top" | depth == "schmidt" | depth == "av.temp")


#set to the right type
colnames(monthly_metrics_obs) <- c("month", "metric", "value")
monthly_metrics_obs$metric <- as.factor(monthly_metrics_obs$metric)

#remove the metrics from obs df
monthly_av_obs$depth <-
  as.numeric(as.character(monthly_av_obs$depth))
monthly_av_obs <- na.exclude(monthly_av_obs)


obs_temp$date <- ymd(trunc(obs_temp$datetime, units = "days"))

col_av <-
  colnames(obs_temp)[2:54] #gets the names of the depth columns

obs_temp <- group_by(obs_temp, date) #group the dataframe by date
daily_av_obs <- obs_temp %>%
  summarise_at(col_av, funs(name = mean)) #gets a mean of each of the specified column

colnames(daily_av_obs) <- c("date", col_av)




#======== model fit table ======
#preallocate a matrix to add the nash sutcliffe results to
names <-  colnames(obs_temp[2:54])

nse <- data.frame(matrix(nrow=125, ncol = 53))
rmse <- data.frame(matrix(nrow=125, ncol = 53))
colnames(nse) <- c(names)
colnames(rmse) <- c(names)

#==================================
# make sure everything is set to baseline conditions
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
k_min <- "1.4e-7"

LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g1', 
                                  key3 = 'constant_value', 
                                  value = g1)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'light_extinction', key2 = 'g2', 
                                  key3 = 'constant_value',
                                  value = g2)
LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                  key1 = 'turbulence', key2 = 'turb_param', 
                                  key3 = 'k_min',
                                  value = k_min)
#========================#
for (i in 1:nrow(iterations)) {
  parameters_use <- iterations[i,]
  
  
  # scaling factors
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'meteo', 
                                    key3 = 'swr', key4 = 'scale_factor', 
                                    value = parameters_use$swr)
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'fluxes', 
                                    key3 = 'heat', key4 = 'scale_factor', 
                                    value = parameters_use$shf)
  LakeEnsemblR::input_yaml_multiple(file = gotm_config,
                                    key1 = 'surface', key2 = 'meteo', 
                                    key3 = 'u10', key4 = 'scale_factor', 
                                    value = parameters_use$wind)
  
  #############################################################
  
  GOTMr::run_gotm(yaml_file = gotm_config)
  
  message('GOTM run ', i)
  
  # analysing the modelled output======================================
  #read in modelled data
  mod_temp <-
    read.delim("./Output/Mod_temp.txt", skip = 8, header = T)
  colnames(mod_temp) <-
    c('datetime', paste0("wtr_", z))#wtr_1 is top of water, wtr_50 is bottom
  mod_temp <- mod_temp[, c(1, 51:2)] # reverse the order of the columns
  mod_temp$datetime <- ymd_hms(mod_temp$datetime)
  #===================================
  
  #metrics modelled===================================
  #do calculations of metrics
  
  #mixed depth
  mod_temp$meta_top <- ts.meta.depths(mod_temp)[, 2]
  
  #av temp
  mod_temp$av.temp <-
    ts.layer.temperature(mod_temp[, 1:51], top = 0, bottom = 6, bathy)[, 2]
  
  #schmidt stability
  mod_temp$schmidt <- ts.schmidt.stability(mod_temp[, 1:51], bathy)[, 2]
  #==================================
  
  #==================================
  #get the month and year only
  mod_temp$month <- format(as.Date(mod_temp$datetime), "%Y-%m")
  
  # mod_temp[1:5, 50:55]
  
  #set the month as a factor
  mod_temp$month <- as.factor(mod_temp$month)
  
  #group the data frame by month
  grouped_mod_month <- group_by(mod_temp, month)
  
  #specify the columns that are to be averaged (ie not date or season)
  names <-
    colnames(grouped_mod_month[2:54]) #gets the names of the depth columns
  
  #find column averages
  monthly_av_mod <- grouped_mod_month %>%
    summarise_at(names, funs(name = mean(., na.rm = T))) #gets a mean of each of the specified column
  #as it is a grouped table it returns a mean for each season
  
  #adjust col names
  colnames(monthly_av_mod) <- c("month", names)
  
  
  #melt data
  monthly_av_mod <-
    melt(
      monthly_av_mod,
      na.rm = F,
      value.name = "temp",
      id.vars = "month",
      variable.name = "depth"
    )
  monthly_av_mod$month <- as.factor(monthly_av_mod$month)
  
  #remove wtr from numbers
  monthly_av_mod$depth <- gsub("wtr_", "", monthly_av_mod$depth)
  
  #take off the metrics into seperate df
  monthly_metrics_mod <-
    subset(monthly_av_mod,
           depth == "meta_top" | depth == "schmidt" | depth == "av.temp")
  
  
  #set to the right type
  colnames(monthly_metrics_mod) <- c("month", "metric", "value")
  monthly_metrics_mod$metric <- as.factor(monthly_metrics_mod$metric)
  
  #remove the metrics from mod df
  monthly_av_mod$depth <-
    as.numeric(as.character(monthly_av_mod$depth))
  monthly_av_mod <- na.exclude(monthly_av_mod)
  
  
  mod_temp$date <- ymd(trunc(mod_temp$datetime, units = "days"))
  
  col_av <-
    colnames(mod_temp)[2:54] #gets the names of the depth columns
  
  mod_temp <- group_by(mod_temp, date) #group the dataframe by date
  daily_av_mod <- mod_temp %>%
    summarise_at(col_av, funs(name = mean)) #gets a mean of each of the specified column
  
  colnames(daily_av_mod) <- c("date", col_av)
  
  #===Plotting=================================================
  
  #plotting and saving output
  
  profiles <-
    ggplot() +
    geom_path(data = monthly_av_mod, aes(x = temp, y = depth, colour = "red")) +
    geom_path(data = monthly_av_obs, aes(x = temp, y = depth, colour = "blue")) +
    scale_y_continuous(
      trans = "reverse",
      expand = c(0, 0),
      breaks = c(0, 1, 2, 3, 4, 5, 6)
    ) +
    #scale_x_continuous(limits = c(0,20)) +
    theme_classic() + facet_wrap( ~ month, strip.position = "bottom") +
    labs(x = "Temperature (?C)", y = "Depth (m)", 
         title = paste0("shf=", parameters_use[1], 
                        ", swr=", parameters_use[2], 
                        ", wind=", parameters_use[3])) +
    scale_colour_manual(
      name = "Source",
      values = c("blue", "red"),
      labels = c("Observation", "Model")
    )
  
  
  # schmidt.fig <-
  #   ggplot() +
  #   geom_path(data = daily_av_mod, aes(x = date, y = schmidt, colour = "Model")) +
  #   geom_path(data = daily_av_obs, aes(x = date, y = schmidt, colour = "Obs")) +
  #   theme_classic() +
  #   labs(x = "Date", y = "Schmidt stability", 
  #        title = paste0("shf=", parameters_use[1], 
  #                       ", swr=", parameters_use[2], 
  #                       ", wind=", parameters_use[3])) +
  #   geom_hline(yintercept = 0) + theme(legend.title = element_blank())
  # 
  
  # av.temp.fig <-
  #   ggplot() +
  #   geom_path(data = daily_av_mod, aes(x = date, y = av.temp, colour = "Model")) +
  #   geom_path(data = daily_av_obs, aes(x = date, y = av.temp, colour = "Obs")) +
  #   theme_classic() + theme(legend.title = element_blank()) +
  #   labs(x = "Date", y = "Volume average temperature", 
  #        title = paste0("shf=", parameters_use[1], 
  #                       ", swr=", parameters_use[2], 
  #                       ", wind=", parameters_use[3]))
  # 
  ggsave(profiles, filename = paste0("./Plots/Inflow_method4/", "monthly_prof_shf=", parameters_use[1], 
                                     ",swr=", parameters_use[2], 
                                     ",wind=", parameters_use[3], ".png"), height = 4, width = 6)
  
  # ggsave(schmidt.fig, filename = paste0("./Plots/Inflow_method4/", "schmidt_shf=", parameters_use[1], 
  #                                       ",swr=", parameters_use[2], 
  #                                       ",wind=", parameters_use[3], ".png"), height = 4, width = 6)
  # 
  # ggsave(av.temp.fig, filename = paste0("./Plots/Inflow_method4/ ", "avtemp_shf=", parameters_use[1], 
  #                                       ",swr=", parameters_use[2], 
  #                                       ",wind=", parameters_use[3], ".png"), height = 4, width = 6)
  
  
  nse[i,] <- hydroGOF::NSE(sim = mod_temp[,2:54], obs = obs_temp[,2:54])
  rmse[i,] <- hydroGOF::rmse(sim = mod_temp[,2:54], obs = obs_temp[,2:54])
  
  #============================================================
}


#what has the best fit
iterations[which(nse$schmidt == max(nse$schmidt)),]
iterations[which(nse$av.temp == max(nse$av.temp)),]
iterations[which(nse$wtr_5.94 == max(nse$wtr_5.94)),]


NSE_output <- bind_cols(iterations, nse)
rmse_output <- bind_cols(iterations, rmse)

NSE_output$av_nse <- apply(NSE_output[,4:53],1,mean)
rmse_output$av_rmse <- apply(rmse_output[,4:53],1,mean)

iterations[which(NSE_output$av_nse == max(NSE_output$av_nse)),]
iterations[which(rmse_output$av_rmse == min(rmse_output$av_rmse)),]

max(NSE_output$av_nse)
min(rmse_output$av_rmse)
