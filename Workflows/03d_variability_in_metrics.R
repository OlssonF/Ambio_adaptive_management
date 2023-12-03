
library(tidyverse)
library(lubridate) 
library(ggplot2)
library(rLakeAnalyzer)


source("R/helper_functions.R")
source("r/modified_rLA.R")


# ====== change Q_AT_ST=====
out_dir <- 'GOTM/Output/Experiment_output/change_Q_AT_ST'
# read in the year_season summaries
# this is the data used to derive the sd of changes
abs_change_year_season <- read.delim(file.path(out_dir, "Summaries/abs_change_seasonyear.txt")) %>%
  separate(season,into = c("season", "year"), sep = " ") %>%
  mutate(season = factor(season,
                         levels = c("spring", "summer", "autumn", "winter")),
         year = as.numeric(year))

# percentage change better for some metrics (stability)
perc_change_year_season <- read.delim(file.path(out_dir, "Summaries/percent_change_seasonyear.txt")) %>%
  separate(season,into = c("season", "year"), sep = " ") %>%
  mutate(season = factor(season,
                         levels = c("spring", "summer", "autumn", "winter")),
         year = as.numeric(year))

# for the year_season summary, calculate the sd and mean change
abs_change_seasonal <- abs_change_year_season %>%
  group_by(season, Q_change, T_change) %>%
  summarise_at(vars("schmidt":"surfaceT"),  # for all metrics
               list(mean = mean, # mean
                    sd = sd)) # want to know the variability in the change in metrics


perc_change_seasonal <- perc_change_year_season %>%  
  group_by(season, Q_change, T_change) %>%
  summarise_at(vars("schmidt":"surfaceT"), 
               list(mean = mean, # mean change in seasonal metrics
                    sd = sd))# want to know the variability in the change in metrics
  
# extract the names to identity, these columns do not get rounded
id_cols <- c("season", "Q_change", "T_change")
# write the data frame
select(abs_change_seasonal, all_of(id_cols)) %>% # extract id columns
  # only want metrics to 3dp
  # not included in id_cols
  bind_cols(., round(abs_change_seasonal[,!names(abs_change_seasonal) %in% id_cols], 3)) %>% 
  write.table(., file.path(out_dir, "/Summaries/abs_change_seasonal_variability.txt"),
              sep = "\t", quote =F, row.names = F)

select(perc_change_seasonal, all_of(id_cols)) %>% # extract id columns
  # only want metrics to 3dp
  # not included in id_cols
  bind_cols(., round(perc_change_seasonal[,!names(perc_change_seasonal) %in% id_cols], 3)) %>% 
  write.table(., file.path(out_dir, "/Summaries/perc_change_seasonal_variability.txt"),
              sep = "\t", quote =F, row.names = F)

# ====== change_Q_AT =====
out_dir <- 'GOTM/Output/Experiment_output/change_Q_AT'
# read in the year_season summaries
# this is the data used to derive the sd of changes
abs_change_year_season <- read.delim(file.path(out_dir, "Summaries/abs_change_seasonyear.txt")) %>%
  separate(season,into = c("season", "year"), sep = " ") %>%
  mutate(season = factor(season,
                         levels = c("spring", "summer", "autumn", "winter")),
         year = as.numeric(year))

# percentage change better for some metrics (stability)
perc_change_year_season <- read.delim(file.path(out_dir, "Summaries/percent_change_seasonyear.txt")) %>%
  separate(season,into = c("season", "year"), sep = " ") %>%
  mutate(season = factor(season,
                         levels = c("spring", "summer", "autumn", "winter")),
         year = as.numeric(year))

# for the year_season summary, calculate the sd and mean change
abs_change_seasonal <- abs_change_year_season %>%
  group_by(season, Q_change, T_change) %>%
  summarise_at(vars("schmidt":"surfaceT"),  # for all metrics
               list(mean = mean, # mean
                    sd = sd)) # want to know the variability in the change in metrics


perc_change_seasonal <- perc_change_year_season %>%  
  group_by(season, Q_change, T_change) %>%
  summarise_at(vars("schmidt":"surfaceT"), 
               list(mean = mean, # mean change in seasonal metrics
                    sd = sd))# want to know the variability in the change in metrics

# extract the names to identity, these columns do not get rounded
id_cols <- c("season", "Q_change", "T_change")
# write the data frame
select(abs_change_seasonal, all_of(id_cols)) %>% # extract id columns
  # only want metrics to 3dp
  # not included in id_cols
  bind_cols(., round(abs_change_seasonal[,!names(abs_change_seasonal) %in% id_cols], 3)) %>% 
  write.table(., file.path(out_dir, "/Summaries/abs_change_seasonal_variability.txt"),
              sep = "\t", quote =F, row.names = F)

select(perc_change_seasonal, all_of(id_cols)) %>% # extract id columns
  # only want metrics to 3dp
  # not included in id_cols
  bind_cols(., round(perc_change_seasonal[,!names(perc_change_seasonal) %in% id_cols], 3)) %>% 
  write.table(., file.path(out_dir, "/Summaries/perc_change_seasonal_variability.txt"),
              sep = "\t", quote =F, row.names = F)
