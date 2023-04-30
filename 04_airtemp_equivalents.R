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

# just get the surfaceT
abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, bottomT) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

equiv_effect_summer_temps <- expand.grid(T_change = unique(abs_temps$T_change),
                            # only want the Q reductions (< 1), more Q will cause cooling
                            Q_change = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) < 1)]) %>%
  mutate(equiv_BWT = NA,
         equiv_SWT = NA)

for (i in 1:length(unique(abs_temps$T_change))) {
  T_change <- unique(abs_temps$T_change)[i]
  
  for (j in 1:5) { # only the ones less than 1
    Q_change <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on smmer temperatures
    change_val1 <- abs_temps$surfaceT[which(abs_temps$Q_change == Q_change & 
                                              abs_temps$T_change == T_change & 
                                              abs_temps$season == "summer")]
    
    change_val2 <- abs_temps$bottomT[which(abs_temps$Q_change == Q_change & 
                                              abs_temps$T_change == T_change & 
                                              abs_temps$season == "summer")]
    
    # extract the values where there is no Q_change, isolate a temperature effect
    # create a vector that will be interpolated
    x1 <- abs_temps %>%
      filter(Q_change == 1, season == "summer") %>%
      # needs to be a numberic vector to be interpolated
      mutate(T_change = as.numeric(as.character(T_change))) %>%
      arrange(T_change)
    
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_summer_temps$equiv_SWT[which(equiv_effect_summer_temps$T_change == T_change &
                                                   equiv_effect_summer_temps$Q_change == Q_change)] <- approx(x = x1$surfaceT,
                                                                                                y=x1$T_change, 
                                                                                                xout = change_val1)$y - T_change
    equiv_effect_summer_temps$equiv_BWT[which(equiv_effect_summer_temps$T_change == T_change &
                                                   equiv_effect_summer_temps$Q_change == Q_change)] <- approx(x = x1$bottomT,
                                                                                            y=x1$T_change, 
                                                                                            xout = change_val2)$y - T_change
  }
  
  
  print(i)
  }



write_delim(equiv_effect_summer_temps, 
            file.path(out_dir, experiment, "Summaries", "air_temp_equiv_summer.txt"),
            delim = "\t")

#==================================#

# === winter SWT & BWT =====
# what air temperature change is equivelent to the effect of a x% reduction in summer

abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, bottomT) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

equiv_effect_winter_temps <- expand.grid(T_change = unique(abs_temps$T_change),
                                         # only want the Q increase (> 1), less Q will cause cooling
                                         Q_change = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) > 1)]) %>%
  mutate(equiv_BWT = NA,
         equiv_SWT = NA)

for (i in 1:length(unique(abs_temps$T_change))) {
  T_change <- unique(abs_temps$T_change)[i]
  
  for (j in 5:9) { # only the ones more than 1
    Q_change <- unique(abs_temps$Q_change)[j]
    # extract the effect of a x% Q reduction on smmer temperatures
    change_val1 <- abs_temps$surfaceT[which(abs_temps$Q_change == Q_change & 
                                              abs_temps$T_change == T_change & 
                                              abs_temps$season == "winter")]
    
    change_val2 <- abs_temps$bottomT[which(abs_temps$Q_change == Q_change & 
                                             abs_temps$T_change == T_change & 
                                             abs_temps$season == "winter")]
    
    # extract the values where there is no Q_change, isolate a temperature effect
    # create a vector that will be interpolated
    x1 <- abs_temps %>%
      filter(Q_change == 1, season == "winter") %>%
      # needs to be a numberic vector to be interpolated
      mutate(T_change = as.numeric(as.character(T_change))) %>%
      arrange(T_change)
    
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_winter_temps$equiv_SWT[which(equiv_effect_winter_temps$T_change == T_change &
                                                equiv_effect_winter_temps$Q_change == Q_change)] <- approx(x = x1$surfaceT,
                                                                                                           y=x1$T_change, 
                                                                                                           xout = change_val1)$y - T_change
    equiv_effect_winter_temps$equiv_BWT[which(equiv_effect_winter_temps$T_change == T_change &
                                                equiv_effect_winter_temps$Q_change == Q_change)] <- approx(x = x1$bottomT,
                                                                                                           y=x1$T_change, 
                                                                                                           xout = change_val2)$y - T_change
  }
  
  
  print(i)
}


write_delim(equiv_effect_winter_temps, file.path(out_dir, experiment, "Summaries", "air_temp_equiv_winter.txt"),
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

for (i in 1:length(unique(stability$T_change))) {
  T_change <- unique(stability$T_change)[i]
  
  for (j in 1:4) {
    Q_change <- unique(stability$Q_change)[j]
    # extract the ffect of a 50% reduction on smmer temperature
    change_val2 <- stability$schmidt[which(stability$Q_change == Q_change & # 
                                        stability$T_change == T_change & 
                                        stability$season == "summer")]
    
    # extract the values where there is no Q_change, isolate a temperature effect
    # create a vector that will be interpolated
    x2 <- stability %>%
      filter(Q_change == 1, season == "summer") %>%
      # needs to be a numberic vector to be interpolated
      mutate(T_change = as.numeric(as.character(T_change))) %>%
      arrange(T_change)
    
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_stability$equiv_effect[which(equiv_effect_stability$T_change == T_change &
                                                equiv_effect_stability$Q_change == Q_change)] <- approx(x = x2$schmidt, y=x2$T_change, 
                                                                                    xout = change_val2)$y - T_change 
                                                                                                          # what is the "additonal" warming?
  }
  
  
  print(i)
}

write_delim(equiv_effect_stability, file.path(out_dir, experiment, "Summaries", "air_temp_equiv_summer_stability.txt"),
            delim = "\t")

