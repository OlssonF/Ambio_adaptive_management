library(tidyverse)
library(lubridate) 
library(ggplot2)
library(cowplot)
library(ggpubr)

out_dir <- "GOTM/Output/Experiment_output"

# Read in output
experiment <- 'change_Q_AT_ST'

abs_change <- read_delim(file.path(out_dir, experiment, "Summaries", "abs_change_seasonal_variability.txt"),
                         show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) 

perc_change <- read_delim(file.path(out_dir, experiment, "Summaries", "perc_change_seasonal_variability.txt"),
                          show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter")))



strat_change <- read_delim(file.path(out_dir,experiment, "Summaries","Strat_summary_variability.txt"),
                           show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100)


airT_equiv_summer <- read_delim(file.path(out_dir,experiment, "Summaries", 
                                              "air_temp_equiv_summer_new_method.txt"),
                                    show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100)
airT_equiv_summer_stability <- read_delim(file.path(out_dir,experiment, "Summaries",
                                                    "air_temp_equiv_summer_stability_new_method.txt"),
                                          show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change - 1)) * 100)
airT_equiv_winter <- read_delim(file.path(out_dir,experiment, "Summaries", "air_temp_equiv_winter_new_method.txt"),
                                    show_col_types = F) %>%
  mutate(Q_increase = abs((Q_change_val - 1)) * 100)


strat_indices <- read_delim(file.path(out_dir,experiment, "Summaries", "Strat_summary_annual.txt"),
                            show_col_types = F)%>%
  mutate(Q_change = as.factor(Q_change),
         T_change = as.factor(T_change))




# surface water temperature =====
# SWT w/ AT
SWT_AT <- abs_change %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=surfaceT_mean)) +
  geom_ribbon(aes(ymax = surfaceT_mean + surfaceT_sd,
                  ymin = surfaceT_mean - surfaceT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in SWT (°C)")       

# SWT w/ Q
SWT_Q <- abs_change %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=surfaceT_mean)) +
  geom_ribbon(aes(ymax = surfaceT_mean + surfaceT_sd,
                  ymin = surfaceT_mean - surfaceT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Flow change (%)",
       y= "Change in SWT (°C)")        


facet_labs <- c(spring = 'A) spring',
                summer = 'B) summer',
                autumn = 'C) autumn',
                winter = 'D) winter')
SWT_AT_Q <- abs_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = surfaceT_mean), colour ="black") +
  geom_text(aes(label = round(surfaceT_mean, 1))) +
  scale_fill_gradient2(limits = c(-1.5,4.5), low = "blue", high ="indianred",
                       name = "Change in SWT (°C)") +
  theme_minimal() +
  facet_wrap(~season, labeller = labeller(season = facet_labs)) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm"),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (+ °C)")

# top row of plot (individual driver)
SWT_ind <- plot_grid(SWT_AT, SWT_Q, nrow = 1, align = "vh", labels = c("A","B")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "SWT_change_individual.png",
         width= 20, height = 9, units = "cm")


# Q and AT combined as seperate plo
ggsave(SWT_AT_Q, path = file.path(out_dir, experiment, "Plots"),
       filename = "SWT_change_combined.png",
       width= 20, height = 14, units = "cm")

# air temperature equivalent changes
airT_equiv_summer_SWT <- ggplot(airT_equiv_summer, aes(x=T_change_val, y= equiv_SWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,2.0), 
                     breaks = seq(0,2.0,0.2)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 

airT_equiv_winter_SWT <- ggplot(airT_equiv_winter, 
                            aes(x=T_change_val, y= equiv_SWT, colour = as.factor(Q_increase))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,2.0), 
                     breaks = seq(0,2.0,0.2)) +
  scale_colour_viridis_d(name = "Flow increase (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0),
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 


ggarrange(airT_equiv_summer_SWT, airT_equiv_winter_SWT, labels = c("A) summer", "B) winter"),
          hjust = -0.1) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "SWT_airT_equiv.png",
         width= 20, height = 10.5, units = "cm")
  #==========================# 

# bottom water temperature =====
# BWT w/ AT
BWT_AT <- abs_change %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=bottomT_mean)) +
  geom_ribbon(aes(ymax = bottomT_mean + bottomT_sd,
                  ymin = bottomT_mean - bottomT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "T change (+ °C)",
       y= "Change in BWT (°C)")       

# BWT w/ Q
BWT_Q <- abs_change %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=bottomT_mean)) +
  geom_ribbon(aes(ymax = bottomT_mean + bottomT_sd,
                  ymin = bottomT_mean - bottomT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5,1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Q change (%)",
       y= "Change in BWT (°C)")        


BWT_AT_Q <- abs_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = bottomT_mean), colour ="black") +
  geom_text(aes(label = round(bottomT_mean, 1))) +
  scale_fill_gradient2(limits = c(-1,4), low = "blue", high ="indianred",
                       name = "Change in BWT (°C)") +
  theme_minimal() +
  facet_wrap(~season) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm")) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (°C)")

# top row of plot (individual driver)
BWT_ind <- plot_grid(BWT_AT, BWT_Q, nrow = 1, align = "vh", labels = c("A","B"))
# Q and AT combined added underneath
plot_grid(BWT_ind, BWT_AT_Q, nrow =2, rel_heights = c(1,1.5), align = "v", axis ="rb", 
          labels = c(" ", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "BWT_change.png",
         width= 20, height = 20, units = "cm")


# air temperature equivalent changes
airT_equiv_summer_BWT <- ggplot(airT_equiv_summer, aes(x=T_change_val, y= equiv_BWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,1.4,0.2)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 

airT_equiv_winter_BWT <- ggplot(airT_equiv_winter, 
                                aes(x=T_change_val, y= equiv_BWT, colour = as.factor(Q_increase))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(-0.5,0.5), 
                     breaks = seq(-0.4, 0.4,0.2)) +
  scale_colour_viridis_d(name = "Flow increase (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0),
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 


ggarrange(airT_equiv_summer_BWT, airT_equiv_winter_BWT, labels = c("A) summer", "B) winter"),
          hjust = -0.1) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "BWT_airT_equiv.png",
         width= 20, height = 10.5, units = "cm")
#==========================# +


# VAT =====
# Volume-average temperature w/ AT
VAT_AT <- abs_change %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=vol_av_temp_mean)) +
  geom_ribbon(aes(ymax = vol_av_temp_mean + vol_av_temp_sd,
                  ymin = vol_av_temp_mean - vol_av_temp_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in volume average temperature (°C)")       

# VAT w/ Q
VAT_Q <- abs_change %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=vol_av_temp_mean)) +
  geom_ribbon(aes(ymax = vol_av_temp_mean + vol_av_temp_sd,
                  ymin = vol_av_temp_mean - vol_av_temp_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Flow change (%)",
       y= "Change in volume average temperature (°C)")        


facet_labs <- c(spring = 'A) spring',
                summer = 'B) summer',
                autumn = 'C) autumn',
                winter = 'D) winter')

VAT_AT_Q <- abs_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = vol_av_temp_mean), colour ="black") +
  geom_text(aes(label = round(vol_av_temp_mean, 1))) +
  scale_fill_gradient2(limits = c(-0.8,4.5), low = "blue", high ="indianred",
                       name = "Change in volume average temperature (°C)") +
  theme_minimal() +
  facet_wrap(~season, labeller = labeller(season = facet_labs)) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm"),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (+ °C)")

# top row of plot (individual driver)
VAT_ind <- plot_grid(VAT_AT, VAT_Q, nrow = 1, align = "vh", labels = c("A","B")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "VAT_change_individual.png",
         width= 20, height = 9, units = "cm")


# Q and AT combined as seperate plo
ggsave(VAT_AT_Q, path = file.path(out_dir, experiment, "Plots"),
       filename = "VAT_change_combined.png",
       width= 20, height = 14, units = "cm")

# =========================================#

# stability =====

# stability w/ AT
stability_AT <- perc_change %>%
  filter(season == "summer") %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=schmidt_mean)) +
  geom_ribbon(aes(ymax = schmidt_mean + schmidt_sd,
                  ymin = schmidt_mean - schmidt_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature\nchange (+ °C)",
       y= "Change in stability (%)")       

# stability w/ Q
stability_Q <- perc_change %>%
  filter(season == "summer") %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=schmidt_mean)) +
  geom_ribbon(aes(ymax = schmidt_mean + schmidt_sd,
                  ymin = schmidt_mean - schmidt_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Flow change (%)",
       y= "Change in stability (°C)")        


stability_AT_Q <- perc_change %>%
  filter(season == "summer") %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = schmidt_mean), colour ="black") +
  geom_text(aes(label = round(schmidt_mean, 1)), size = 3) +
  scale_fill_gradient2(limits = c(-10,75), low = "orchid4", high ="springgreen4",
                       name = "Change in stability (%)") +
  theme_minimal() +
  facet_wrap(~season) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm")) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (°C)") 
 
# top row of plot (individual driver)
stability_ind <- plot_grid(stability_AT, stability_Q, nrow = 1, align = "vh", labels = c("A","B"), hjust =-2)
# Q and AT combined added underneath
plot_grid(stability_ind, stability_AT_Q, nrow =2,
          rel_heights = c(0.7,1), 
          align = "v", 
          axis ="rb", 
          labels = c(" ", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "stability change.png",
         width= 10, height = 12.5, units = "cm")


# air temperature equivalent changes
airT_equiv_stability <- 
  airT_equiv_summer_stability |> 
  filter(effect == 'compound') |> 
  ggplot(aes(x=T_change, 
             y= value,
             colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,2.0), 
                     breaks = seq(0,2.0,0.2)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0),
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 


ggarrange(airT_equiv_summer_SWT, airT_equiv_stability, airT_equiv_winter_SWT, 
          labels = c("A) summer SWT", "B) summer stability", "C) winter SWT"),
          nrow = 1,
          hjust = -0.1) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "SWT_stability_airT_equiv.png",
         width= 23, height = 10, units = "cm")


ggarrange(plot_grid(stability_ind, stability_AT_Q, nrow =2,
          rel_heights = c(0.7,1), 
          align = "v", 
          axis ="rb", 
          labels = c(" ", "C"), hjust = -2), 
          plot_grid(NULL, airT_equiv_stability, NULL, rel_heights = c(0.3,1,0.3), ncol = 1,
                    labels = c("", "D", ""), hjust = -2), 
          widths = c(1.2, 1))  %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "Stability_plots.png",
         width= 20, height = 14, units = "cm")


# mitigation potential
summer_schmidt_MP <- 
  airT_equiv_summer_stability |> 
  filter(effect == 'mitigate') |> 
  ggplot(aes(x=T_change, y= value, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,2.0,0.2)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)")

#==============================================#

# densdiff =====

# densdiff w/ AT
densdiff_AT <- abs_change %>%
  filter(season == "summer") %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=density_diff_mean)) +
  geom_ribbon(aes(ymax = density_diff_mean + density_diff_sd,
                  ymin = density_diff_mean - density_diff_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature\nchange (+ °C)",
       y= "Change in density difference")       

# densdiff w/ Q
densdiff_Q <- abs_change %>%
  filter(season == "summer") %>%
  # only want to look at Q_change effects
  filter(T_change == 0)  %>%
  ggplot(., aes(x=Q_change, y=density_diff_mean)) +
  geom_ribbon(aes(ymax = density_diff_mean + density_diff_sd,
                  ymin = density_diff_mean - density_diff_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Flow change (%)",
       y= "Change in density difference")        


densdiff_AT_Q <- abs_change %>%
  # filter(season == "summer") %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = density_diff_mean), colour ="black") +
  geom_text(aes(label = round(density_diff_mean, 1)), size = 3) +
  scale_fill_gradient2(limits = c(-0.2,0.6), low = "orchid4", high ="springgreen4",
                       name = "Change in density difference") +
  theme_minimal() +
  facet_wrap(~season) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm")) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (°C)") 

# top row of plot (individual driver)
densdiff_ind <- plot_grid(densdiff_AT, densdiff_Q, nrow = 1, align = "vh", labels = c("A","B"), hjust =-2)
# Q and AT combined added underneath
plot_grid(densdiff_ind, densdiff_AT_Q, nrow =2,
          rel_heights = c(0.7,1), 
          align = "v", 
          axis ="rb", 
          labels = c(" ", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "densdiff change.png",
         width= 20, height = 20, units = "cm")

#=============================#
# summer stratified period ======
strat_AT <- strat_change %>%
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=longest_strat_mean)) +
  geom_ribbon(aes(ymax = longest_strat_mean + longest_strat_sd,
                  ymin = longest_strat_mean - longest_strat_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in summer\nstratified period (days)")  

strat_Q <- strat_change %>%
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=longest_strat_mean)) +
  geom_ribbon(aes(ymax = longest_strat_mean + longest_strat_sd,
                  ymin = longest_strat_mean - longest_strat_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Flow change (%)",
       y= "Change in summer\nstratified period (days)")  

strat_AT_Q <- strat_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = longest_strat_mean), colour ="black") +
  geom_text(aes(label = round(longest_strat_mean, 1))) +
  scale_fill_gradient2(limits = c(min(strat_change$longest_strat_mean),
                                  max(strat_change$longest_strat_mean)), 
                       low = "orchid4", high ="springgreen4",
                       name = "Change in summer\nstratified period (days)") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm")) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (°C)")

# top row of plot (individual driver)
strat_ind <- plot_grid(strat_AT, strat_Q, nrow = 1, align = "vh", labels = c("A","B"))
# Q and AT combined added underneath
plot_grid(strat_ind, strat_AT_Q, nrow =2,
          rel_heights = c(0.7,1), 
          align = "v", 
          axis ="rb", 
          labels = c(" ", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "strat_change.png",
         width= 11, height = 13.5, units = "cm")

#==============================================#

# inverse stratification ====
inverse_AT <- strat_change %>%
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=inverse_n_mean)) +
  geom_ribbon(aes(ymax = inverse_n_mean + inverse_n_sd,
                  ymin = inverse_n_mean - inverse_n_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in inverse\nstratifcation (hours)")  

inverse_Q <- strat_change %>%
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=inverse_n_mean)) +
  geom_ribbon(aes(ymax = inverse_n_mean + inverse_n_sd,
                  ymin = inverse_n_mean - inverse_n_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  theme_bw() +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Flow change (%)",
       y= "Change in inverse\nstratifcation (hours)")  


inverse_AT_Q <- strat_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = inverse_n_mean), colour ="black") +
  geom_text(aes(label = round(inverse_n_mean, 1))) +
  scale_fill_gradient2(limits = c(min(strat_change$inverse_n_mean),
                                  max(strat_change$inverse_n_mean)), low = "orchid4", high ="springgreen4",
                       name = "Change in inverse\nstratification (hours)") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm")) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "\nAir temperature change (+ °C)")


# top row of plot (individual driver)
inverse_ind <- plot_grid(inverse_AT, inverse_Q, nrow = 1, align = "vh", labels = c("A","B"))
# Q and AT combined added underneath
plot_grid(inverse_ind, inverse_AT_Q, nrow =2,
          rel_heights = c(0.7,1), 
          align = "v", 
          axis ="rb", 
          labels = c(" ", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "inverse_strat_change.png",
         width= 14, height = 14, units = "cm")

#===============================================#

# strat dates ======

change_dates_AT <- 
  strat_change %>%
  filter(Q_change == 1) %>%
  ggplot(.) +
  
  geom_line(aes(x=T_change, y = start_mean, colour = "onset")) +
  geom_point(aes(x=T_change, y = start_mean)) +
  
  geom_ribbon(aes(x=T_change, ymax = start_mean + start_sd,
                  ymin = start_mean - start_sd, colour = "onset"), alpha =0.3, show.legend = F) +
  
  geom_line(aes(x=T_change, y = end_mean, colour = "overturn")) +
  geom_point(aes(x=T_change, y = end_mean)) +
  
  geom_ribbon(aes(x=T_change, 
                  ymax = end_mean + end_sd,
                  ymin = end_mean - end_sd, colour = "overturn"), alpha =0.3, show.legend = F) +
  labs(x = "Air temperature change (+ °C)",
       y = "Change in date (days)") +
  scale_colour_viridis_d(begin = 0.3, end = 0.8, name = "") +
  scale_y_continuous(limits = c(-60,35)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.margin = margin(-0.4,0,-0.2,0, unit = "cm"))

change_dates_Q <- strat_change %>%
  filter(T_change == 0) %>%
  ggplot(.) +
  
  geom_line(aes(x=Q_change, y = start_mean, colour = "onset")) +
  geom_point(aes(x=Q_change, y = start_mean)) +
  
  geom_ribbon(aes(x=Q_change, ymax = start_mean + start_sd,
                  ymin = start_mean - start_sd, colour = "onset"), alpha =0.3, show.legend = F) +
  
  geom_line(aes(x=Q_change, y = end_mean, colour = "overturn")) +
  geom_point(aes(x=Q_change, y = end_mean)) +
  
  geom_ribbon(aes(x=Q_change, 
                  ymax = end_mean + end_sd,
                  ymin = end_mean - end_sd, colour = "overturn"), alpha =0.3, show.legend = F) +
  labs(x = "Flow change (%)",
       y = "Change in date (days)") +
  scale_colour_viridis_d(begin = 0.3, end = 0.8, name = "") +
  scale_y_continuous(limits = c(-60,35)) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  theme_bw()


change_dates_ind <- ggarrange(ggarrange(change_dates_AT, change_dates_Q, 
                                        nrow = 1, align = "hv",  
                                        labels = c("A","B"), 
                                        common.legend = T, legend = "none"),
                              as_ggplot(get_legend(change_dates_AT)),
                              nrow = 2, 
                              heights = c(1,0.1))

change_end_AT_Q <- strat_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = end_mean), colour ="black") +
  geom_text(aes(label = round(end_mean, 1))) +
  scale_fill_gradient2(limits = c(-35,25), low = "orchid4", high ="springgreen4",
                       name = "Change in date (days)") +
  theme_minimal() +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (°C)") +
  theme(legend.position = "bottom",
        legend.margin = margin(-0.5,0,-0.2,0, unit = "cm"))


change_start_AT_Q <- 
  strat_change %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = start_mean), colour ="black") +
  geom_text(aes(label = round(start_mean, 1))) +
  scale_fill_gradient2(limits = c(-35,25), low = "orchid4", high ="springgreen4",
                       name = "Change in date (days)") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.2,0, unit = "cm")) +
  scale_x_discrete(breaks = as.factor(unique(abs_change$Q_change)),
                   labels = as.factor(unique(abs_change$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (°C)")

change_dates_AT_Q <- ggarrange(ggarrange(change_start_AT_Q, change_end_AT_Q, 
                                         common.legend = T, legend = "none", labels = c("C", "D")), 
                               as_ggplot(get_legend(change_end_AT_Q)), nrow =  2, 
                               heights = c(1, 0.2))


ggarrange(change_dates_ind, change_dates_AT_Q, nrow = 2) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "strat_dates_change.png",
         width= 22, height = 20, units = "cm")



# strat_indices %>%
#   mutate(start = yday(ymd_hms(start)),
#          end = yday(ymd_hms(end))) %>%
#   group_by(Q_change, T_change) %>%
#   summarise(max_start = max(start),
#             median_start = median(start),
#             mean_start = mean(start),
#             min_start = min(start),
#             sd_start = sd(start),
#             mean_end = mean(end),
#             sd_end = sd(end)) %>%
#   ggplot(., aes(x=Q_change, colour=T_change)) +
#   geom_linerange(aes(x = Q_change, 
#                      ymax = mean_end , 
#                      ymin = mean_start, group = T_change), 
#                  position = position_dodge(0.9), colour = "grey", size = 1.5) +
#   geom_point(aes(y = mean_start),
#              position = position_dodge(0.9)) +
#   geom_point(aes(y = mean_end),
#              position = position_dodge(0.9)) +
#   geom_linerange(aes(x = Q_change, 
#                      ymax = mean_start + sd_start, 
#                      ymin = mean_start - sd_start), 
#                  position = position_dodge(0.9)) +
#   geom_linerange(aes(x = Q_change, 
#                      ymax = mean_end + sd_end, 
#                      ymin = mean_end - sd_end), 
#                  position = position_dodge(0.9)) +
#   scale_color_viridis_d(begin = 0.9, end = 0.1) +
#   scale_y_continuous(limits = c(0,365), expand = c(0,0),
#                      breaks = seq(0,360, 30)) +
#   labs(y= "Day of year") +
#   theme_bw(base_size = 11) +
#   theme(axis.title = element_text(size = 15))

# strat_indices %>%
#   filter(T_change == 0) %>%
#   mutate(start = yday(ymd_hms(start)),
#          end = yday(ymd_hms(end))) %>%
#   group_by(Q_change) %>%
#   summarise(max_start = max(start),
#             median_start = median(start),
#             mean_start = mean(start),
#             min_start = min(start),
#             sd_start = sd(start),
#             mean_end = mean(end),
#             sd_end = sd(end)) %>%
#   ggplot(.) +
#   geom_ribbon(aes(x=as.numeric(as.character(Q_change)), 
#                   ymax = mean_end + sd_end,
#                   ymin = mean_end - sd_end), colour = NA, alpha =0.3) +
#   geom_line(aes(x=as.numeric(as.character(Q_change)), y = mean_end)) +
#   geom_point(size =0.9, aes(x=as.numeric(as.character(Q_change)), y = mean_end)) +
#   
#   geom_ribbon(aes(x=as.numeric(as.character(Q_change)), 
#                   ymax = mean_start + sd_start,
#                   ymin = mean_start - sd_start), colour = NA, alpha =0.3) +
#   geom_line(aes(x=as.numeric(as.character(Q_change)), y = mean_start)) +
#   geom_point(size =0.9, aes(x=as.numeric(as.character(Q_change)), y = mean_start)) +
#   
#   theme_bw() +
#   scale_x_continuous(breaks = c(0.5, 0.7, 0.9, 1.1,1.3, 1.5),
#                      labels = c(-50, -30, -10, 10, 30, 50)) +
#   scale_y_continuous(limits = c(0,366)) +
#   theme(panel.grid.minor = element_blank()) +
#   labs(x = "Q change (%)",
#        y= "End date (DOY)")  
# 
# strat_indices %>%
#   filter(Q_change == 1) %>%
#   mutate(start = yday(ymd_hms(start)),
#          end = yday(ymd_hms(end))) %>%
#   group_by(T_change) %>%
#   summarise(max_start = max(start),
#             median_start = median(start),
#             mean_start = mean(start),
#             min_start = min(start),
#             sd_start = sd(start),
#             mean_end = mean(end),
#             sd_end = sd(end)) %>%
#   ggplot(.) +
#   geom_ribbon(aes(x=as.numeric(as.character(T_change)), 
#                   ymax = mean_end + sd_end,
#                   ymin = mean_end - sd_end), colour = NA, alpha =0.3) +
#   geom_line(aes(x=as.numeric(as.character(T_change)), y = mean_end)) +
#   geom_point(size =0.9, aes(x=as.numeric(as.character(T_change)), y = mean_end)) +
#   
#   geom_ribbon(aes(x=as.numeric(as.character(T_change)), 
#                   ymax = mean_start + sd_start,
#                   ymin = mean_start - sd_start), colour = NA, alpha =0.3) +
#   geom_line(aes(x=as.numeric(as.character(T_change)), y = mean_start)) +
#   geom_point(size =0.9, aes(x=as.numeric(as.character(T_change)), y = mean_start)) +
#   
#   theme_bw() +
#   scale_y_continuous(limits = c(50,300),
#                      breaks = seq(50,300,50)) +
#   theme(panel.grid.minor = element_blank()) +
#   labs(x = "T change (+ °C)",
#        y= "End date (DOY)")  

# lake inflow temperature difference
# get estimate inflow temp and surface temps - find the average difference
inflows <- read.delim("GOTM/Inflow_BACI.dat") %>%
  mutate(X.DateTime = ymd_hms(X.DateTime),
         jday = yday(format(X.DateTime, "%Y-%m-%d"))) %>%
  group_by(jday) %>%
  summarise(inflow_T = mean(inflow_T))


surfaceT <- read.delim("GOTM/Output/Experiment_output/change_Q_AT/Mod_temp_T_0_Q_1.txt", header = F,
                       skip = 9)[,c(1,51)] %>%
  rename(X.DateTime = V1,
         surface_T = V51) %>%
  mutate(X.DateTime = ymd_hms(X.DateTime),
         jday = yday(format(X.DateTime, "%Y-%m-%d"))) %>%
  group_by(jday) %>%
  summarise(surface_T = mean(surface_T))

full_join(inflows, surfaceT, by = "jday") %>%
  mutate(temp_diff = inflow_T - surface_T) %>%
  ggplot(., aes(x=jday, y= temp_diff)) +
  geom_line() +
  geom_smooth(se = F, method = "gam") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(name = "Day of year",
                     breaks = seq(0,350,50)) +
  scale_y_continuous(name = "Inflow - lake temperature\ndifference (°C)",
                     breaks = seq(-5,2.5,2.5)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())


# Mitigation potential ---------
MP_summer <- read_delim(file.path(out_dir,experiment, "Summaries", 
                                    "mitigation_potential_summer.txt"),
                          show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100)

summer_SWT_MP <- 
  ggplot(MP_summer, aes(x=T_change_val, y= mitigate_SWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,2.0,0.5)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

summer_VAWT_MP <- 
  ggplot(MP_summer, aes(x=T_change_val, y= mitigate_VAWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,2.0,0.5)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

summer_BWT_MP <- 
  ggplot(MP_summer, aes(x=T_change_val, y= mitigate_BWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.2), 
                     breaks = seq(0,2.0,0.2)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)")


MP_winter <- read_delim(file.path(out_dir,experiment, "Summaries", 
                                  "mitigation_potential_winter.txt"),
                        show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100)

winter_SWT_MP <- 
  ggplot(MP_winter, aes(x=T_change_val, y= mitigate_SWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,2.0,0.5)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

winter_VAWT_MP <- 
  ggplot(MP_winter, aes(x=T_change_val, y= mitigate_VAWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,2.0,0.5)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

winter_BWT_MP <- ggplot(MP_winter, aes(x=T_change_val, y= mitigate_BWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.5), 
                     breaks = seq(0,2.0,0.2)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

ggarrange(summer_SWT_MP, winter_SWT_MP, labels = c('a) summer',
                                                   'b) winter')) |> 
  ggsave(filename = file.path(out_dir, experiment, 'Plots', 'mitigation_potential_SWT.png'),
         height = 10, width = 20, units = 'cm')

ggarrange(summer_VAWT_MP, winter_VAWT_MP, labels = c('a) summer',
                                                   'b) winter')) |> 
  ggsave(filename = file.path(out_dir, experiment, 'Plots', 'mitigation_potential_VAWT.png'),
         height = 10, width = 20, units = 'cm')

# plots for temperatures without ST effect ----------------
out_dir <- "GOTM/Output/Experiment_output"

# Read in output
experiment_without <- 'change_Q_AT'

abs_change_without <- read_delim(file.path(out_dir, experiment_without, "Summaries", "abs_change_seasonal_variability.txt"),
                                 show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) 

perc_change_without <- read_delim(file.path(out_dir, experiment_without, "Summaries", "perc_change_seasonal_variability.txt"),
                                  show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter")))

experiment_with <- 'change_Q_AT_ST'

abs_change_with <- read_delim(file.path(out_dir, experiment_with, "Summaries", "abs_change_seasonal_variability.txt"),
                              show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) 

perc_change_with <- read_delim(file.path(out_dir, experiment_with, "Summaries", "perc_change_seasonal_variability.txt"),
                               show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter")))
#===============================#

# VAWT difference with and without ST effect --------------------

# Difference with and without ST effect

VAWT_with <- abs_change_with |> 
  select(season, Q_change,
         T_change, vol_av_temp_mean, vol_av_temp_sd, Q_change_percentage) |> 
  rename(VAWT_with = vol_av_temp_mean)

VAWT_without <- abs_change_without |> 
  select(season, Q_change,
         T_change, vol_av_temp_mean, vol_av_temp_sd, Q_change_percentage) |> 
  rename(VAWT_without = vol_av_temp_mean)

summer_VAWT_stream_diff <- 
  full_join(VAWT_with, VAWT_without, by = c('season', 'Q_change', "T_change", 'Q_change_percentage')) |> 
  filter(season == 'summer') |> 
  # Q_change <= 1) |> 
  mutate(diff = VAWT_without - VAWT_with,
         perc = ((VAWT_with - VAWT_without)/VAWT_with)*100) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = diff), colour ="black") +
  geom_text(aes(label = round(diff, 1)), size = 3) +
  scale_fill_viridis_c(limits = c(-2.5, 0), name = 'Effect difference (°C)', option = 'mako', begin = 0.3, end = 1) +
  labs(x='Flow change (%)', y = 'Air temperature change (+ °C)', tag = 'a) summer') +
  scale_x_discrete(breaks = as.factor(unique(VAWT_with$Q_change)),
                   labels = as.factor(unique(VAWT_with$Q_change_percentage))) +
  theme_minimal() +
  theme(plot.tag = element_text(hjust = -1, face = 'bold'))

winter_VAWT_stream_diff <- 
  full_join(VAWT_with, VAWT_without, by = c('season', 'Q_change', "T_change", 'Q_change_percentage')) |> 
  filter(season == 'winter') |> 
  # Q_change >= 1) |> 
  mutate(diff = VAWT_without - VAWT_with) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = diff), colour ="black") +
  geom_text(aes(label = round(diff, 1)), size = 3) +
  scale_fill_viridis_c(limits = c(-2.5, 0), option = 'mako', begin = 0.3, end = 1)+
  labs(x='Flow change (%)', y = 'Air temperature change (+ °C)', tag = 'b) winter') +
  scale_x_discrete(breaks = as.factor(unique(VAWT_with$Q_change)),
                   labels = as.factor(unique(VAWT_with$Q_change_percentage))) +
  theme_minimal() +
  theme(plot.tag = element_text(hjust = -1, face = 'bold')) 


ggpubr::ggarrange(summer_VAWT_stream_diff, 
                  winter_VAWT_stream_diff, common.legend = T) |> 
  ggsave(path = file.path(out_dir, experiment_without, "Plots"),
         filename = "VAT_change_difference.png",
         width= 20, height = 9, units = "cm")


# Absolute changes in VAWT with no ST effect -------------
VA_without_AT <-
  VAWT_without %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=VAWT_without)) +
  geom_ribbon(aes(ymax = VAWT_without + vol_av_temp_sd,
                  ymin = VAWT_without - vol_av_temp_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in volume average temperature (°C)")       


VA_without_Q <- 
  VAWT_without %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=VAWT_without)) +
  geom_ribbon(aes(ymax = VAWT_without + vol_av_temp_sd,
                  ymin = VAWT_without - vol_av_temp_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Flow change (%)",
       y= "Change in volume average temperature (°C)")     

facet_labs <- c(spring = 'A) spring',
                summer = 'B) summer',
                autumn = 'C) autumn',
                winter = 'D) winter')

VAT_AT_Q <- VAWT_without %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = VAWT_without), colour ="black") +
  geom_text(aes(label = round(VAWT_without, 1))) +
  scale_fill_gradient2(limits = c(-0.8,4.5), low = "blue", high ="indianred",
                       name = "Change in volume average temperature (°C)") +
  theme_minimal() +
  facet_wrap(~season, labeller = labeller(season = facet_labs)) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm"),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(breaks = as.factor(unique(VAWT_without$Q_change)),
                   labels = as.factor(unique(VAWT_without$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (+ °C)")

# top row of plot (individual driver)
VAT_ind <- plot_grid(VA_without_AT, VA_without_Q, nrow = 1, align = "vh", labels = c("A","B")) %>%
  ggsave(path = file.path(out_dir, experiment_without, "Plots"),
         filename = "VAT_change_individual.png",
         width= 20, height = 9, units = "cm")


# Q and AT combined as seperate plo
ggsave(VAT_AT_Q, path = file.path(out_dir, experiment_without, "Plots"),
       filename = "VAT_change_combined.png",
       width= 20, height = 14, units = "cm")



# SWT difference with and without ST effect --------------------
SWT_with <- abs_change_with |> 
  select(season, Q_change,
         T_change, surfaceT_mean, surfaceT_sd) |> 
  rename(SWT_with = surfaceT_mean)

SWT_without <- abs_change_without |> 
  select(season, Q_change,
         T_change, surfaceT_mean, surfaceT_sd) |> 
  rename(SWT_without = surfaceT_mean)

summer_SWT_stream_diff <- 
  full_join(SWT_with, SWT_without) |> 
  filter(season == 'summer') |> 
  mutate(diff = SWT_without - SWT_with) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = diff), colour ="black") +
  geom_text(aes(label = round(diff, 1)), size = 3) +
  scale_fill_viridis_c(limits = c(-2.5, 0), name = 'Effect difference (°C)',
                       option = 'mako', begin = 0.3, end = 1) +
  labs(x='Flow change (%)', y = 'Air temperature change (+ °C)', tag = 'b) winter') +
  scale_x_discrete(breaks = as.factor(unique(VAWT_with$Q_change)),
                   labels = as.factor(unique(VAWT_with$Q_change_percentage))) +
  theme_minimal() +
  theme(plot.tag = element_text(hjust = -1, face = 'bold')) 

winter_SWT_stream_diff <- 
  full_join(SWT_with, SWT_without) |> 
  filter(season == 'winter') |> 
  mutate(diff = SWT_without - SWT_with) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = diff), colour ="black") +
  geom_text(aes(label = round(diff, 1)), size = 3) +
  scale_fill_viridis_c(limits = c(-2.5, 0), name = 'Effect difference (°C)',
                       option = 'mako', begin = 0.3, end = 1) +
  labs(x='Flow change (%)', y = 'Air temperature change (+ °C)', tag = 'b) winter') +
  scale_x_discrete(breaks = as.factor(unique(VAWT_with$Q_change)),
                   labels = as.factor(unique(VAWT_with$Q_change_percentage))) +
  theme_minimal() +
  theme(plot.tag = element_text(hjust = -1, face = 'bold')) 

ggpubr::ggarrange(summer_SWT_stream_diff, 
                  winter_SWT_stream_diff, common.legend = T) |> 
  ggsave(path = file.path(out_dir, experiment_without, "Plots"),
         filename = "SWT_change_difference.png",
         width= 20, height = 9, units = "cm")


# Absolute changes in SWT with no ST effect -------------
SWT_without_AT <-
  SWT_without %>%
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=SWT_without)) +
  geom_ribbon(aes(ymax = SWT_without + surfaceT_sd,
                  ymin = SWT_without - surfaceT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in surface water temperature (°C)")       


SWT_without_Q <- 
  SWT_without %>%
  # only want to look at Q_change effects
  filter(T_change == 0) %>%
  ggplot(., aes(x=Q_change, y=SWT_without)) +
  geom_ribbon(aes(ymax = SWT_without + surfaceT_sd,
                  ymin = SWT_without - surfaceT_sd),colour = NA, alpha =0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0.3, 0.5, 0.7, 0.9, 1.1,1.3, 1.5, 1.7),
                     labels = c(-70, -50, -30, -10, 10, 30, 50, 70)) +
  labs(x = "Flow change (%)",
       y= "Change in surface water temperature (°C)")     

facet_labs <- c(spring = 'A) spring',
                summer = 'B) summer',
                autumn = 'C) autumn',
                winter = 'D) winter')

SWT_AT_Q <- 
  SWT_without %>%
  ggplot(., aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = SWT_without), colour ="black") +
  geom_text(aes(label = round(SWT_without, 1))) +
  scale_fill_gradient2(limits = c(-1.2,4.5), low = "blue", high ="indianred",
                       name = "Change in surface water temperature (°C)") +
  theme_minimal() +
  facet_wrap(~season, labeller = labeller(season = facet_labs)) +
  theme(legend.position = "top",
        legend.margin = margin(-0.1,0,-0.4,0, unit = "cm"),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(breaks = as.factor(unique(VAWT_without$Q_change)),
                   labels = as.factor(unique(VAWT_without$Q_change_percentage))) +
  labs(x= "Flow change (%)",
       y= "Air temperature change (+ °C)")

# top row of plot (individual driver)
SWT_ind <- plot_grid(SWT_without_AT, SWT_without_Q, nrow = 1, align = "vh", labels = c("A","B")) %>%
  ggsave(path = file.path(out_dir, experiment_without, "Plots"),
         filename = "SWT_change_individual.png",
         width= 20, height = 9, units = "cm")


# Q and AT combined as seperate plo
ggsave(SWT_AT_Q, path = file.path(out_dir, experiment_without, "Plots"),
       filename = "SWT_change_combined.png",
       width= 20, height = 14, units = "cm")
#===========================#

# mitigation potential without ST effect
MP_summer_noST <- read_delim(file.path(out_dir,experiment_without, "Summaries", 
                                  "mitigation_potential_summer.txt"),
                        show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100)

summer_SWT_MP_noST <-
  ggplot(MP_summer_noST, aes(x=T_change_val, y= mitigate_SWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,1.6), 
                     breaks = seq(0,2.0,0.4)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

summer_VAWT_MP_noST <-
  MP_summer_noST |> 
  mutate(above_limit = ifelse(is.na(mitigate_VAWT), T, F),
         mitigate_VAWT = ifelse(is.na(mitigate_VAWT), 4, mitigate_VAWT)) |> 
  ggplot(aes(x=T_change_val, y= mitigate_VAWT, colour = as.factor(Q_reduction))) +
  geom_point(aes(shape = above_limit)) +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,4), 
                     breaks = seq(0,4.0,1)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") +
  scale_shape_manual(values = c(19, 4)) +
  guides(shape = 'none')

MP_winter_noST <- read_delim(file.path(out_dir,experiment_without, "Summaries", 
                                       "mitigation_potential_winter.txt"),
                             show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100)

winter_SWT_MP_noST <-
  ggplot(MP_winter_noST, aes(x=T_change_val, y= mitigate_SWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,4), 
                     breaks = seq(0,4.0,1)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 

winter_VAWT_MP_noST <-
  ggplot(MP_winter_noST, aes(x=T_change_val, y= mitigate_VAWT, colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,4), 
                     breaks = seq(0,4.0,1)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential (°C)") 


ggarrange(summer_SWT_MP_noST, winter_SWT_MP_noST, labels = c('a) summer',
                                                   'b) winter')) |> 
  ggsave(filename = file.path(out_dir, experiment_without, 'Plots', 'mitigation_potential_SWT.png'),
         height = 10, width = 20, units = 'cm')

ggarrange(summer_VAWT_MP_noST, winter_VAWT_MP_noST, labels = c('a) summer',
                                                     'b) winter')) |> 
  ggsave(filename = file.path(out_dir, experiment_without, 'Plots', 'mitigation_potential_VAWT.png'),
         height = 10, width = 20, units = 'cm')
