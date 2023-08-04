library(tidyverse)
library(lubridate)
# setwd("E:/GOTM/elterwater/R_scripts/GOTM-2")
# source("functions needed.R")

out_dir <- 'GOTM/Output/Experiment_output'
experiment <- 'change_Q_AT_ST'

abs_change_seasonal <- read_delim(file.path(out_dir, experiment, "Summaries", "abs_change_seasonal.txt") ,
                                  show_col_types = F)%>%
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")))

# ===== Summer SWT & BWT======
# what air temperature change is equivelent to the effect of a x% reduction in summer

# just get the surfaceT and bottomT
abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, bottomT) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

equiv_effect_summer_temps <- expand.grid(T_change_val = unique(abs_temps$T_change),
                            # only want the Q reductions (< 1), more Q will cause cooling in summer
                            Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) < 1)]) %>%
  mutate(equiv_BWT = NA,
         equiv_SWT = NA)

# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x1 <- abs_temps %>%
  filter(Q_change == 1, season == "summer") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)


for (i in 1:length(unique(abs_temps$T_change))) {
  T_change_val <- unique(abs_temps$T_change)[i]
  
  T_only_change <- abs_temps |> 
    filter(season == 'summer',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 1:5) { # only the ones less than 1
    Q_change_val <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on summer temperatures
    change_val1 <- abs_temps$surfaceT[which(abs_temps$Q_change == Q_change_val & 
                                              abs_temps$T_change == T_change_val & 
                                              abs_temps$season == "summer")] - T_only_change$surfaceT
    
    
    change_val2 <- abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                              abs_temps$T_change == T_change_val & 
                                              abs_temps$season == "summer")] - T_only_change$bottomT
    
    
    
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_summer_temps$equiv_SWT[which(equiv_effect_summer_temps$T_change_val == T_change_val &
                                                   equiv_effect_summer_temps$Q_change_val == Q_change_val)] <- approx(x = x1$surfaceT,
                                                                                                                      y=x1$T_change, 
                                                                                                                      xout = change_val1)$y 
    equiv_effect_summer_temps$equiv_BWT[which(equiv_effect_summer_temps$T_change_val == T_change_val &
                                                   equiv_effect_summer_temps$Q_change_val == Q_change_val)] <- approx(x = x1$bottomT,
                                                                                                                      y=x1$T_change, 
                                                                                                                      xout = change_val2)$y 
  }
  
  
  print(i)
  }



write_delim(equiv_effect_summer_temps, 
            file.path(out_dir, experiment, "Summaries", "air_temp_equiv_summer_new_method.txt"),
            delim = "\t")

#==================================#

# === winter SWT & BWT =====
# what air temperature change is equivelent to the effect of a x% reduction in winter

# just get the surfaceT and bottomT
abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, bottomT) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

equiv_effect_winter_temps <- expand.grid(T_change_val = unique(abs_temps$T_change),
                                         # only want the Q increases (> 1), less Q will cause cooling in winter
                                         Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) > 1)]) %>%
  mutate(equiv_BWT = NA,
         equiv_SWT = NA)

# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x1 <- abs_temps %>%
  filter(Q_change == 1, season == "winter") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)


for (i in 1:length(unique(abs_temps$T_change))) {
  T_change_val <- unique(abs_temps$T_change)[i]
  
  T_only_change <- abs_temps |> 
    filter(season == 'winter',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 5:9) { # only the ones less than 1
    Q_change_val <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on winter temperatures
    change_val1 <- abs_temps$surfaceT[which(abs_temps$Q_change == Q_change_val & 
                                              abs_temps$T_change == T_change_val & 
                                              abs_temps$season == "winter")] - T_only_change$surfaceT
    
    
    change_val2 <- abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                             abs_temps$T_change == T_change_val & 
                                             abs_temps$season == "winter")] - T_only_change$bottomT
    
    
    
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_winter_temps$equiv_SWT[which(equiv_effect_winter_temps$T_change_val == T_change_val &
                                                equiv_effect_winter_temps$Q_change_val == Q_change_val)] <- approx(x = x1$surfaceT,
                                                                                                                   y=x1$T_change, 
                                                                                                                   xout = change_val1)$y 
    equiv_effect_winter_temps$equiv_BWT[which(equiv_effect_winter_temps$T_change_val == T_change_val &
                                                equiv_effect_winter_temps$Q_change_val == Q_change_val)] <- approx(x = x1$bottomT,
                                                                                                                   y=x1$T_change, 
                                                                                                                   xout = change_val2)$y 
  }
  
  
  print(i)
}



write_delim(equiv_effect_winter_temps, 
            file.path(out_dir, experiment, "Summaries", "air_temp_equiv_winter_new_method.txt"),
            delim = "\t")

#==================================#

# === summer schmidt stability ====
perc_change_seasonal <- read_delim(file.path(out_dir, experiment, "Summaries", "percent_change_seasonal.txt") ,
                                   show_col_types = F) |> 
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")))
# just get the schmidt
stability <- perc_change_seasonal %>%
  select(season, T_change, Q_change, schmidt) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

equiv_effect_stability <- expand.grid(T_change = unique(stability$T_change),
                            # only want the Q reductions (< 1), more Q will cause cooling
                            Q_change = unique(stability$Q_change)[which(unique(stability$Q_change) < 1)]) %>%
  mutate(equiv_effect = NA)

# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x2 <- stability %>%
  filter(Q_change == 1, season == "summer") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)


for (i in 1:length(unique(stability$T_change))) {
  T_change_val <- unique(stability$T_change)[i]
  
  T_only_change <- stability |> 
    filter(season == 'summer',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 1:4) {
    Q_change_val <- unique(stability$Q_change)[j]

    change_val2 <- stability$schmidt[which(stability$Q_change == Q_change_val & # 
                                        stability$T_change == T_change_val & 
                                        stability$season == "summer")] - T_only_change$schmidt
    
   
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_stability$equiv_effect[which(equiv_effect_stability$T_change == T_change_val &
                                                equiv_effect_stability$Q_change == Q_change_val)] <- approx(x = x2$schmidt, 
                                                                                                            y=x2$T_change, 
                                                                                                            xout = change_val2)$y
  }
  
  
  print(i)
}

write_delim(equiv_effect_stability, file.path(out_dir, experiment, "Summaries", "air_temp_equiv_summer_stability_new_method.txt"),
            delim = "\t")

#================================================#

# cooling mitigation

# Mitigation potential ----------------------------------------------------
abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, bottomT, vol_av_temp, schmidt) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

# Summer Mitigation potential -------------------
# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x1 <- abs_temps %>%
  filter(Q_change == 1, season == "summer") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)

mit_summer_temps <- expand.grid(T_change_val = unique(abs_temps$T_change)[-1],
                                         # only want the Q increases (< 1), more Q will cause cooling in summer
                                         Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) > 1)]) %>%
  mutate(mitigate_SWT = NA,
         mitigate_BWT = NA,
         mitigate_VAWT = NA,
         mitigate_schmidt = NA)

for (i in 2:length(unique(abs_temps$T_change))) {
  T_change_val <- unique(abs_temps$T_change)[i]
  
  T_only_change <- abs_temps |> 
    filter(season == 'summer',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 5:9) { # only the morethan than 1
    Q_change_val <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on summer temperatures
    # summer surface T
    change_val1 <- T_only_change$surfaceT - 
      abs_temps$surfaceT[which(abs_temps$Q_change == Q_change_val & 
                                 abs_temps$T_change == T_change_val & 
                                 abs_temps$season == "summer")]
    
    mitigate <- approx(x = x1$surfaceT,
                       y = x1$T_change, 
                       xout = change_val1)$y
    
    # summer bottomT
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                 abs_temps$T_change == T_change_val & 
                                 abs_temps$season == "summer")]
    
    mitigate2 <- approx(x = x1$bottomT,
                       y = x1$T_change, 
                       xout = change_val2)$y
    
    # vol_av_temp
    change_val3 <- T_only_change$vol_av_temp - 
      abs_temps$vol_av_temp[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "summer")]
    
    mitigate3 <- approx(x = x1$vol_av_temp,
                        y = x1$T_change, 
                        xout = change_val3)$y
    
    # schmidt
    change_val4 <- T_only_change$schmidt - 
      abs_temps$schmidt[which(abs_temps$Q_change == Q_change_val & 
                                    abs_temps$T_change == T_change_val & 
                                    abs_temps$season == "summer")]
    
    mitigate4 <- approx(x = x1$schmidt,
                        y = x1$T_change, 
                        xout = change_val4)$y
    
    
    mit_summer_temps$mitigate_SWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate
    
    
    mit_summer_temps$mitigate_BWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_summer_temps$mitigate_VAWT[which(mit_summer_temps$T_change_val == T_change_val &
                                           mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate3
    
    mit_summer_temps$mitigate_schmidt[which(mit_summer_temps$T_change_val == T_change_val &
                                           mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate4
  
  }
  
  
  print(i)
}

write_delim(mit_summer_temps, file.path(out_dir, experiment, "Summaries", "mitigation_potential_summer.txt"),
            delim = "\t")

# Winter Mitigation potential -------------------
# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x2 <- abs_temps %>%
  filter(Q_change == 1, season == "winter") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)

mit_winter_temps <- expand.grid(T_change_val = unique(abs_temps$T_change)[-1],
                                # only want the Q increases (< 1), more Q will cause cooling in summer
                                Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) < 1)]) %>%
  mutate(mitigate_SWT = NA,
         mitigate_BWT = NA,
         mitigate_VAWT = NA)

for (i in 2:length(unique(abs_temps$T_change))) {
  T_change_val <- unique(abs_temps$T_change)[i]
  
  T_only_change <- abs_temps |> 
    filter(season == 'winter',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 1:5) { # only the less  than 1
    Q_change_val <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on winter temperatures
    # winter surface T
    change_val1 <- T_only_change$surfaceT - 
      abs_temps$surfaceT[which(abs_temps$Q_change == Q_change_val & 
                                 abs_temps$T_change == T_change_val & 
                                 abs_temps$season == "winter")]
    
    mitigate <- approx(x = x2$surfaceT,
                       y = x2$T_change, 
                       xout = change_val1)$y
    
    # summer bottomT
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "winter")]
    
    mitigate2 <- approx(x = x2$bottomT,
                        y = x2$T_change, 
                        xout = change_val2)$y
    
    # vol_av_temp
    change_val3 <- T_only_change$vol_av_temp - 
      abs_temps$vol_av_temp[which(abs_temps$Q_change == Q_change_val & 
                                    abs_temps$T_change == T_change_val & 
                                    abs_temps$season == "winter")]
    
    mitigate3 <- approx(x = x2$vol_av_temp,
                        y = x2$T_change, 
                        xout = change_val3)$y
    
    
    mit_winter_temps$mitigate_SWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate
    
    
    mit_winter_temps$mitigate_BWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_winter_temps$mitigate_VAWT[which(mit_winter_temps$T_change_val == T_change_val &
                                           mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate3
    
  }
  
  
  print(i)
}

write_delim(mit_winter_temps, file.path(out_dir, experiment, "Summaries", "mitigation_potential_winter.txt"),
            delim = "\t")


# mitigation potential without ST effect ----------------------------------
out_dir <- 'GOTM/Output/Experiment_output'
experiment <- 'change_Q_AT'

abs_change_seasonal_noST <- read_delim(file.path(out_dir, experiment, "Summaries", "abs_change_seasonal.txt") ,
                                  show_col_types = F) %>%
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")))

# cooling mitigation
abs_temps <- abs_change_seasonal_noST %>%
  select(season, T_change, Q_change, surfaceT, bottomT, vol_av_temp) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

# Summer Mitigation potential -------------------
# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x1 <- abs_temps %>%
  filter(Q_change == 1, season == "summer") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)

mit_summer_temps <- expand.grid(T_change_val = unique(abs_temps$T_change)[-1],
                                # only want the Q increases (< 1), more Q will cause cooling in summer
                                Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) > 1)]) %>%
  mutate(mitigate_SWT = NA,
         mitigate_BWT = NA,
         mitigate_VAWT = NA)

for (i in 2:length(unique(abs_temps$T_change))) {
  T_change_val <- unique(abs_temps$T_change)[i]
  
  T_only_change <- abs_temps |> 
    filter(season == 'summer',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 5:9) { # only the morethan than 1
    Q_change_val <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on summer temperatures
    # summer surface T
    change_val1 <- T_only_change$surfaceT - 
      abs_temps$surfaceT[which(abs_temps$Q_change == Q_change_val & 
                                 abs_temps$T_change == T_change_val & 
                                 abs_temps$season == "summer")]
    
    mitigate <- approx(x = x1$surfaceT,
                       y = x1$T_change, 
                       xout = change_val1)$y
    
    # summer bottomT
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "summer")]
    
    mitigate2 <- approx(x = x1$bottomT,
                        y = x1$T_change, 
                        xout = change_val2)$y
    
    # vol_av_temp
    change_val3 <- T_only_change$vol_av_temp - 
      abs_temps$vol_av_temp[which(abs_temps$Q_change == Q_change_val & 
                                    abs_temps$T_change == T_change_val & 
                                    abs_temps$season == "summer")]
    
    mitigate3 <- approx(x = x1$vol_av_temp,
                        y = x1$T_change, 
                        xout = change_val3)$y
    
    
    mit_summer_temps$mitigate_SWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate
    
    
    mit_summer_temps$mitigate_BWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_summer_temps$mitigate_VAWT[which(mit_summer_temps$T_change_val == T_change_val &
                                           mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate3
    
  }
  
  
  print(i)
}

write_delim(mit_summer_temps, file.path(out_dir, experiment, "Summaries", "mitigation_potential_summer.txt"),
            delim = "\t")

# Winter Mitigation potential -------------------
# extract the values where there is no Q_change, isolate a temperature effect
# create a vector that will be interpolated
x2 <- abs_temps %>%
  filter(Q_change == 1, season == "winter") %>%
  # needs to be a numberic vector to be interpolated
  mutate(T_change = as.numeric(as.character(T_change))) %>%
  arrange(T_change)

mit_winter_temps <- expand.grid(T_change_val = unique(abs_temps$T_change)[-1],
                                # only want the Q increases (< 1), more Q will cause cooling in summer
                                Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) < 1)]) %>%
  mutate(mitigate_SWT = NA,
         mitigate_BWT = NA,
         mitigate_VAWT = NA)

for (i in 2:length(unique(abs_temps$T_change))) {
  T_change_val <- unique(abs_temps$T_change)[i]
  
  T_only_change <- abs_temps |> 
    filter(season == 'winter',
           T_change == T_change_val,
           Q_change == 1)
  
  for (j in 1:5) { # only the less  than 1
    Q_change_val <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on winter temperatures
    # winter surface T
    change_val1 <- T_only_change$surfaceT - 
      abs_temps$surfaceT[which(abs_temps$Q_change == Q_change_val & 
                                 abs_temps$T_change == T_change_val & 
                                 abs_temps$season == "winter")]
    
    mitigate <- approx(x = x2$surfaceT,
                       y = x2$T_change, 
                       xout = change_val1)$y
    
    # summer bottomT
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "winter")]
    
    mitigate2 <- approx(x = x2$bottomT,
                        y = x2$T_change, 
                        xout = change_val2)$y
    
    # vol_av_temp
    change_val3 <- T_only_change$vol_av_temp - 
      abs_temps$vol_av_temp[which(abs_temps$Q_change == Q_change_val & 
                                    abs_temps$T_change == T_change_val & 
                                    abs_temps$season == "winter")]
    
    mitigate3 <- approx(x = x2$vol_av_temp,
                        y = x2$T_change, 
                        xout = change_val3)$y
    
    
    mit_winter_temps$mitigate_SWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate
    
    
    mit_winter_temps$mitigate_BWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_winter_temps$mitigate_VAWT[which(mit_winter_temps$T_change_val == T_change_val &
                                           mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate3
    
  }
  
  
  print(i)
}

write_delim(mit_winter_temps, file.path(out_dir, experiment, "Summaries", "mitigation_potential_winter.txt"),
            delim = "\t")


