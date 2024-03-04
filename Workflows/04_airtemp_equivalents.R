library(tidyverse)
library(lubridate)


out_dir <- 'GOTM/Output/Experiment_output'
experiment <- 'change_Q_AT_ST'
experiment_without <- 'change_Q_AT'

abs_change_seasonal <- read_delim(file.path(out_dir, experiment, "Summaries", "abs_change_seasonal.txt") ,
                                  show_col_types = F)%>%
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")))

abs_change_seasonal_without <- read_delim(file.path(out_dir, experiment_without, "Summaries", "abs_change_seasonal.txt") ,
                                          show_col_types = F)%>%
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")))
# ===== Summer SWT + BWT======
# what air temperature change is equivelent to the effect of a x% reduction in summer

# just get the surfaceT + bottomT
abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, bottomT) %>%
  mutate(T_change = as.numeric(as.character(T_change)),
         Q_change = as.numeric(as.character(Q_change))) %>%
  arrange(T_change, Q_change)

equiv_effect_summer_temps <- expand.grid(T_change_val = unique(abs_temps$T_change),
                                         # only want the Q reductions (< 1), more Q will cause cooling in summer
                                         Q_change_val = unique(abs_temps$Q_change)[which(unique(abs_temps$Q_change) < 1)]) %>%
  mutate(equiv_SWT = NA,
         equiv_BWT = NA)

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
            file.path(out_dir, experiment, "Summaries", "air_temp_equiv_summerWT.txt"),
            delim = "\t")

#==================================#

# === winter SWT =====
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
  mutate(equiv_SWT = NA,
         equiv_BWT = NA)

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
                                                                                                                   xout = change_val2)
    
  }
  
  
  print(i)
}



write_delim(equiv_effect_winter_temps, 
            file.path(out_dir, experiment, "Summaries", "air_temp_equiv_winterWT.txt"),
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
                                      Q_change = unique(stability$Q_change)) %>%
  mutate(effect = ifelse(Q_change > 1, 'mitigate', 
                         ifelse(Q_change < 1, 'compound', NA)),
         value = NA)

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
    
    effect <- approx(x = x2$schmidt, 
                     y=x2$T_change, 
                     xout = change_val2)$y
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_stability$value[which(equiv_effect_stability$T_change == T_change_val &
                                         equiv_effect_stability$Q_change == Q_change_val)] <- effect
  }
  
  for (k in 5:9) {
    Q_change_val <- unique(stability$Q_change)[k]
    
    # schmidt
    change_val <- T_only_change$schmidt - 
      stability$schmidt[which(stability$Q_change == Q_change_val & 
                                stability$T_change == T_change_val & 
                                stability$season == "summer")]
    
    mitigate <- approx(x = x2$schmidt,
                       y = x2$T_change, 
                       xout = change_val)$y
    
    equiv_effect_stability$value[which(equiv_effect_stability$T_change == T_change_val &
                                         equiv_effect_stability$Q_change == Q_change_val)] <- mitigate
    
  }
  print(i)
}

equiv_effect_stability |> 
  na.omit(effect) |> 
  write_delim(file.path(out_dir, experiment, "Summaries", "air_temp_equiv_summer_stability.txt"),
              delim = "\t")

#================================================#

# Mitigation potential ----------------------------------------------------
abs_temps <- abs_change_seasonal %>%
  select(season, T_change, Q_change, surfaceT, schmidt, bottomT) %>%
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
    
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "summer")]
    
    change_val3 <- T_only_change$schmidt - 
      abs_temps$schmidt[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "summer")]
    
    mitigate1 <- approx(x = x1$surfaceT,
                        y = x1$T_change, 
                        xout = change_val1)$y
    
    mitigate2 <- approx(x = x1$bottomT,
                        y = x1$T_change, 
                        xout = change_val2)$y
    
    mitigate3 <- approx(x = x1$schmidt,
                        y = x1$T_change, 
                        xout = change_val3)$y
    
    
    mit_summer_temps$mitigate_SWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate1
    
    mit_summer_temps$mitigate_BWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_summer_temps$mitigate_schmidt[which(mit_summer_temps$T_change_val == T_change_val &
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
         mitigate_schmidt = NA)

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
    
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "winter")]
    
    change_val3 <- T_only_change$schmidt - 
      abs_temps$schmidt[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "winter")]
    
    mitigate1 <- approx(x = x2$surfaceT,
                        y = x2$T_change, 
                        xout = change_val1)$y
    
    mitigate2 <- approx(x = x2$bottomT,
                        y = x2$T_change, 
                        xout = change_val2)$y
    
    mitigate3 <- approx(x = x2$schmidt,
                        y = x2$T_change, 
                        xout = change_val3)$y
    
    
    mit_winter_temps$mitigate_SWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate1
    
    mit_winter_temps$mitigate_BWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_winter_temps$mitigate_schmidt[which(mit_winter_temps$T_change_val == T_change_val &
                                              mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate3
    
  }
  
  
  print(i)
}

write_delim(mit_winter_temps, file.path(out_dir, experiment, "Summaries", "mitigation_potential_winter.txt"),
            delim = "\t")


# mitigation potential without ST effect ----------------------------------

abs_change_seasonal_noST <- read_delim(file.path(out_dir, experiment_without, "Summaries", "abs_change_seasonal.txt") ,
                                       show_col_types = F) %>%
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change),
         season = factor(season, levels = c("winter", "spring", "summer", "autumn")))

# cooling mitigation
abs_temps <- abs_change_seasonal_noST %>%
  select(season, T_change, Q_change, surfaceT, bottomT, schmidt) %>%
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
    
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "summer")]
    
    change_val3 <- T_only_change$schmidt - 
      abs_temps$schmidt[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "summer")]
    
    mitigate1 <- approx(x = x1$surfaceT,
                        y = x1$T_change, 
                        xout = change_val1)$y
    
    mitigate2 <- approx(x = x1$bottomT,
                        y = x1$T_change, 
                        xout = change_val2)$y
    
    mitigate3 <- approx(x = x1$schmidt,
                        y = x1$T_change, 
                        xout = change_val3)$y
    
    
    mit_summer_temps$mitigate_SWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate1
    
    mit_summer_temps$mitigate_BWT[which(mit_summer_temps$T_change_val == T_change_val &
                                          mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_summer_temps$mitigate_schmidt[which(mit_summer_temps$T_change_val == T_change_val &
                                              mit_summer_temps$Q_change_val == Q_change_val)] <- mitigate3
    
  }
  
  
  print(i)
}

write_delim(mit_summer_temps, file.path(out_dir, experiment_without, "Summaries", "mitigation_potential_summer.txt"),
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
         mitigate_schmidt = NA)

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
    
    change_val2 <- T_only_change$bottomT - 
      abs_temps$bottomT[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "winter")]
    
    change_val3 <- T_only_change$schmidt - 
      abs_temps$schmidt[which(abs_temps$Q_change == Q_change_val & 
                                abs_temps$T_change == T_change_val & 
                                abs_temps$season == "winter")]
    
    mitigate1 <- approx(x = x2$surfaceT,
                        y = x2$T_change, 
                        xout = change_val1)$y
    
    mitigate2 <- approx(x = x2$bottomT,
                        y = x2$T_change, 
                        xout = change_val2)$y
    
    mitigate3 <- approx(x = x2$schmidt,
                        y = x2$T_change, 
                        xout = change_val3)$y
    
    
    mit_winter_temps$mitigate_SWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate1
    
    mit_winter_temps$mitigate_BWT[which(mit_winter_temps$T_change_val == T_change_val &
                                          mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate2
    
    mit_winter_temps$mitigate_schmidt[which(mit_winter_temps$T_change_val == T_change_val &
                                              mit_winter_temps$Q_change_val == Q_change_val)] <- mitigate3
    
  }
  
  
  print(i)
}

write_delim(mit_winter_temps, file.path(out_dir, experiment_without, "Summaries", "mitigation_potential_winter.txt"),
            delim = "\t")



perc_change_seasonal <- read_delim(file.path(out_dir, experiment_without, "Summaries", "percent_change_seasonal.txt") ,
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
                                      Q_change = unique(stability$Q_change)) %>%
  mutate(effect = ifelse(Q_change > 1, 'mitigate', 
                         ifelse(Q_change < 1, 'compound', NA)),
         value = NA)

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
    
    effect <- approx(x = x2$schmidt, 
                     y=x2$T_change, 
                     xout = change_val2)$y
    # approx function linearly interpolates
    # estimate the y value for which the x value is change_val (the change caused by 50% reduction)
    equiv_effect_stability$value[which(equiv_effect_stability$T_change == T_change_val &
                                         equiv_effect_stability$Q_change == Q_change_val)] <- effect
  }
  
  for (k in 5:9) {
    Q_change_val <- unique(stability$Q_change)[k]
    
    # schmidt
    change_val <- T_only_change$schmidt - 
      stability$schmidt[which(stability$Q_change == Q_change_val & 
                                stability$T_change == T_change_val & 
                                stability$season == "summer")]
    
    mitigate <- approx(x = x2$schmidt,
                       y = x2$T_change, 
                       xout = change_val)$y
    
    equiv_effect_stability$value[which(equiv_effect_stability$T_change == T_change_val &
                                         equiv_effect_stability$Q_change == Q_change_val)] <- mitigate
    
  }
  print(i)
}

equiv_effect_stability |> 
  na.omit(effect) |> 
  write_delim(file.path(out_dir, experiment_without, "Summaries", "air_temp_equiv_summer_stability.txt"),
              delim = "\t")
