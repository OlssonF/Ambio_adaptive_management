
library(tidyverse)
library(lubridate) 
library(ggplot2)
library(rLakeAnalyzer)


source("R/helper_functions.R")
source("r/modified_rLA.R")

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
  summarise_at(vars("bottomT":"vol_av_temp"),  # for all metrics
               list(mean = mean, # mean
                    sd = sd)) # want to know the variability in the change in metrics


perc_change_seasonal <- perc_change_year_season %>%  
  group_by(season, Q_change, T_change) %>%
  summarise_at(vars("bottomT":"vol_av_temp"), 
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


# example plot
abs_change_seasonal %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=surfaceT_mean, colour = season, fill = season)) +
  geom_ribbon(aes(ymax = surfaceT_mean + 2*surfaceT_sd,
                  ymin = surfaceT_mean - 2*surfaceT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  #facet_wrap(~season) +
  theme_bw() +
  labs(title = "mean change +/- 2 * sd",
       y= "change in surface water temperature (oC)")

# example plot
abs_change_seasonal %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=surfaceT_mean, colour = season, fill = season)) +
  geom_ribbon(aes(ymax = surfaceT_mean + 2*surfaceT_sd,
                  ymin = surfaceT_mean - 2*surfaceT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = "mean change +/- 2 * sd",
       y= "change in surface water temperature (oC)")


# example plot
perc_change_seasonal %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=schmidt_mean, colour = season, fill = season)) +
  geom_ribbon(aes(ymax = schmidt_mean + 2*schmidt_sd,
                  ymin = schmidt_mean - 2*schmidt_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  #facet_wrap(~season) +
  theme_bw() +
  labs(title = "mean change +/- 2 * sd",
       y= "change in schmidt stability %")


abs_change_seasonal %>%
  filter(T_change == 0, season == "summer") %>%
  print(., width = Inf)
#==================================#
# variability in stratification
strat_annual <- read.delim(file.path(out_dir, "Summaries/Strat_summary_annual.txt")) %>%
  mutate(start = yday(start),
         end = yday(end))

strat_base <- strat_annual %>%
  filter(T_change == 0, Q_change == 1)

strat_changes <- strat_annual %>%
  group_by(T_change, Q_change) %>%
  mutate(longest_strat = (longest_strat - strat_base[,"longest_strat"])/24,
            strat_n = strat_n - strat_base[,"strat_n"],
            mixis = mixis - strat_base[,"mixis"],
            inverse_n = inverse_n - strat_base[,"inverse_n"],
            start = start - strat_base[,"start"], 
            end = end - strat_base[,"end"])

# for the year_season summary, calculate the sd and mean change
strat_changes %>%
  group_by(Q_change, T_change) %>%
  summarise_at(vars("longest_strat":"end"),  # for all metrics
               list(mean = mean, # mean
                    sd = sd)) %>% # want to know the variability in the change in metrics
  write.table(., "./GOTM/Output/Experiment_output/change_Q_AT_ST/Summaries/Strat_summary_variability.txt",
              sep = "\t", quote = F, row.names = F)


