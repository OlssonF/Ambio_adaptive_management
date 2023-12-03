library(tidyverse)
library(lubridate) 
library(ggplot2)
library(cowplot)
library(ggpubr)

out_dir <- "GOTM/Output/Experiment_output"

# Read in relevant output from the analyses
experiment <- 'change_Q_AT_ST'
experiment_without <- 'change_Q_AT'


abs_change <- read_delim(file.path(out_dir, experiment, "Summaries", "abs_change_seasonal_variability.txt"),
                         show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) 

abs_change_without <- read_delim(file.path(out_dir, experiment_without, "Summaries", "abs_change_seasonal_variability.txt"),
                                 show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter"))) 


perc_change <- read_delim(file.path(out_dir, experiment, "Summaries", "perc_change_seasonal_variability.txt"),
                          show_col_types = F) %>%
  mutate(Q_change_percentage = (Q_change - 1) * 100,
         season = factor(season, levels = c("spring", "summer", "autumn", "winter")))


airT_equiv_summer <- read_delim(file.path(out_dir,experiment, "Summaries", 
                                          "air_temp_equiv_summerSWT.txt"),
                                show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100)


airT_equiv_winter <- read_delim(file.path(out_dir,experiment, "Summaries", "air_temp_equiv_winterSWT.txt"),
                                show_col_types = F) %>%
  mutate(Q_increase = abs((Q_change_val - 1)) * 100)


airT_equiv_summer_stability <- read_delim(file.path(out_dir,experiment, "Summaries",
                                                    "air_temp_equiv_summer_stability.txt"),
                                          show_col_types = F) |> 
  mutate(Q_reduction = abs((Q_change - 1)) * 100,
         mitigate_inflow = 'No')



airT_equiv_summer_stability_without <- read_delim(file.path(out_dir,experiment_without, "Summaries",
                                                    "air_temp_equiv_summer_stability.txt"),
                                          show_col_types = F) |>  
  mutate(Q_reduction = abs((Q_change - 1)) * 100,
         mitigate_inflow = 'Yes')



# Mitigiation potential 
MP_summer <- read_delim(file.path(out_dir,experiment, "Summaries", 
                                  "mitigation_potential_summerSWT.txt"),
                        show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100,
         mitigate_inflow = 'No') 

MP_summer_noST <- read_delim(file.path(out_dir,experiment_without, "Summaries", 
                                       "mitigation_potential_summerSWT.txt"),
                             show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100,
         mitigate_inflow = 'Yes')
MP_winter <- read_delim(file.path(out_dir,experiment, "Summaries", 
                                  "mitigation_potential_winterSWT.txt"),
                        show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100,
         mitigate_inflow = 'No') 

MP_winter_noST <- read_delim(file.path(out_dir,experiment_without, "Summaries", 
                                       "mitigation_potential_winter.txt"),
                             show_col_types = F) %>%
  mutate(Q_reduction = abs((Q_change_val - 1)) * 100,
         mitigate_inflow = 'Yes')


# Figure 1 - SWT changes ---------------------------------

# SWT w/ AT
SWT_AT <-
  abs_change %>%
  filter(season %in% c('summer', 'winter')) |> 
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=surfaceT_mean)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_ribbon(aes(ymax = surfaceT_mean + surfaceT_sd,
                  ymin = surfaceT_mean - surfaceT_sd),colour = NA, alpha = 0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in SWT (°C)") +
  scale_y_continuous(limits = c(0,4))

# SWT w/ Q
SWT_Q <- abs_change %>%
  # only want to look at Q_change effects
  filter(T_change == 0,
         season %in% c('summer', 'winter')) %>%
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


facet_labs <- c(summer = 'summer',
                winter = 'winter')

SWT_AT_Q <- abs_change %>%
  filter(season %in% c('summer', 'winter')) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
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

SWT_ind <- plot_grid(SWT_AT, SWT_Q, nrow = 2, align = "vh", labels = c("A","B")) 

plot_grid(SWT_ind, SWT_AT_Q, nrow = 2, rel_heights = c(1.5,1), labels = c("", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "SWT_change.png",
         width= 15, height = 22, units = "cm")

#------------------------------------------------------#

# Figure 2 air temperature equivalent effects ----------------
airT_equiv_summer_SWT <- 
  ggplot(airT_equiv_summer, aes(x=T_change_val, 
                                y= equiv_SWT, 
                                colour = as.factor(Q_reduction))) +
  geom_point() +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,2.0), 
                     breaks = seq(0,2.0,0.5)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", 
                         option = 'viridis',
                         begin = 0.5, end = 0,
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
                     breaks = seq(0,2.0,0.5)) +
  scale_colour_viridis_d(name = "Flow increase (%)", option = 'viridis', 
                         begin = 0.5, end = 1,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0),
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 


ggarrange(airT_equiv_summer_SWT, airT_equiv_winter_SWT,
          labels = c("A) summer SWT", "B) winter SWT"),
          hjust = -0.5) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "airT_equiv_SWT.png",
         width= 15, height = 8, units = "cm")

#-----------------------------------------------------#

# Figure 3 - changes to water column stability ------
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
  scale_colour_viridis_d(name = "Flow reduction (%)", 
                         begin = 0.5, end = 0,
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", legend.margin = margin(0,0,-4,0),
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Air temperature equivalent (°C)") 

# combine plots
stability_ind <- plot_grid(stability_AT, stability_Q, nrow = 1, align = "vh", labels = c("A","B"), hjust =-2)

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

#-------------------------------------------------------#

# Figure 4 - difference in SWT without inflowT ------------------

SWT_without <- abs_change_without |> 
  select(season, Q_change,
         T_change, surfaceT_mean) |> 
  rename(SWT_without = surfaceT_mean)

SWT_with <- abs_change |> 
  select(season, Q_change,
         T_change, surfaceT_mean) |> 
  rename(SWT_with = surfaceT_mean)

SWT_stream_diff <- 
  full_join(SWT_with, SWT_without) |> 
  filter(season %in% c('summer', 'winter')) |> 
  mutate(diff = SWT_without - SWT_with,
         perc = (SWT_without/SWT_with)*100) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
  geom_tile(aes(fill = diff), colour ="black") +
  geom_text(aes(label = round(diff, 1)), size = 3) +
  scale_fill_viridis_c(limits = c(-2, -0.001), name = 'SWT difference (°C)',na.value = 'white',
                       option = 'mako', begin = 0.3, end = 1) +
  labs(x='Flow change (%)', y = 'Air temperature change (+ °C)') +
  scale_x_discrete(breaks = as.factor(unique(SWT_with$Q_change)),
                   labels = as.factor((unique(SWT_with$Q_change) -1) * 100)) +
  theme_minimal() +
  facet_wrap(~season) +
  theme(plot.tag = element_text(hjust = -1, face = 'bold')) 

SWT_stream_diff %>%
  ggsave(path = file.path(out_dir, experiment_without, "Plots"),
         filename = "SWT_stream_difference.png",
         width= 18, height = 8, units = "cm")
  #------------------------------------------------------#

# Figure 5 - mitigation potential -------------------------

summer_swt_mp <-
  bind_rows(MP_summer, MP_summer_noST) |> 
  ggplot(aes(x=T_change_val, y= 100*(mitigate_SWT/T_change_val),
             colour = as.factor(Q_reduction), shape = mitigate_inflow)) +
  geom_jitter(width = 0.1, size = 1.8) +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,210), 
                     breaks = seq(0,200,50)) +
  scale_x_continuous(breaks = seq(0.5,4,0.5)) +
  scale_colour_viridis_d(name = "Flow increase (%)", begin = 0.1, end = 0.9,
                         option = 'mako',
                         guide = guide_legend(direction = "vertical",
                                              title.position = "top", title.hjust = 0.5))  +
  scale_shape_manual(name = "Mitigate inflow warming?", 
                     values = c(16,1),
                     guide = guide_legend(direction = 'vertical',
                                          title.position = 'top',
                                          title.hjust = 0.5)) +
  theme(legend.position = "left", legend.margin = margin(0,0,-4,0), legend.spacing = unit(0.9, units = 'cm'),
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential\n(% of air temperature response)") + 
  guides(colour = guide_legend(order = 2), 
         shape = guide_legend(order = 1))


summer_schmidt_mp <-
  bind_rows(airT_equiv_summer_stability, airT_equiv_summer_stability_without) |>
  filter(effect == 'mitigate',
         T_change > 0) |> 
  ggplot(aes(x=T_change, y= 100*(value/T_change), colour = as.factor(Q_reduction), 
             shape = mitigate_inflow)) +
  geom_jitter(width = 0.1, size = 1.8) +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  # scale_y_continuous(limit = c(0,220), 
  #                    breaks = seq(0,200,20)) +
  scale_x_continuous(breaks = seq(0.5,4,0.5)) +
  scale_colour_viridis_d(name = "Flow increase (%)", begin = 0.1, end = 0.9,
                         option = 'mako',
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "top"))  +
  theme(legend.position = "bottom", 
        legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential\n(% of air temperature response)") +
  scale_shape_manual(name = "Mitigate inflow warming?", 
                     values = c(16,1),
                     guide = guide_legend(direction = 'horizontal',
                                          title.position = 'top',
                                          title.hjust = 0.5)) + 
  guides(colour = guide_legend(order = 2), 
         shape = guide_legend(order = 1))


winter_swt_mp <-
  bind_rows(MP_winter, MP_winter_noST) |> 
  ggplot(aes(x=T_change_val, y= 100*(mitigate_SWT/T_change_val),
             colour = as.factor(Q_reduction), shape = mitigate_inflow)) +
  geom_jitter(width = 0.1, size = 1.8) +
  theme_bw() +
  theme(plot.margin = margin(1,0.5,0.5,0.5, unit = "cm")) +
  scale_y_continuous(limit = c(0,600),
                     breaks = seq(0,600,100)) +
  scale_x_continuous(breaks = seq(0.5,4,0.5)) +
  scale_colour_viridis_d(name = "Flow reduction (%)", begin = 0.1, end = 0.9,
                         option = 'rocket',
                         guide = guide_legend(direction = "horizontal",
                                              title.position = "left", title.hjust = 0.5, title.theme = element_text(colour = 'white')))  +
  scale_shape_manual(name = "Mitigate inflow warming?", 
                     values = c(16,1),
                     guide = 'none') +
  theme(legend.position = "right", 
        legend.margin = margin(0,0,-4,0), 
        panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Mitigation potential\n(% of air temperature response)")+ 
  guides(colour = guide_legend(order = 1))


legends <- ggarrange(NULL, get_legend(summer_swt_mp), get_legend(winter_swt_mp), NULL, 
                     nrow = 4, align = 'v', heights = c(0.1,1,1,0.5))

mp_plots <- ggarrange(ggarrange(summer_swt_mp,
                    summer_schmidt_mp, 
                    nrow = 1, labels = c('A) Summer SWT',
                                         'B) Summer stability'),
                    legend = 'none'),
          ggarrange(winter_swt_mp, NULL, 
                    nrow = 1, labels = c('C) Winter SWT',
                                         ''), 
                    legend = 'none'), 
          nrow = 2, align = 'hv')  

ggarrange(legends, mp_plots, widths = c(1.2,4))|>  
  ggsave(filename = file.path(out_dir, experiment, 'Plots', 'mitigation_potential_combined.png'),
         height = 12, width = 21, units = 'cm')

#--------------------------------------------------#

# Figure S2 cal/val

# Figure S4 autumn/spring SWT ---------------------
# SWT w/ AT
SWT_AT <-
  abs_change %>%
  filter(season %in% c('spring', 'autumn')) |> 
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=surfaceT_mean)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
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
  filter(T_change == 0,
         season %in% c('spring', 'autumn')) %>%
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


facet_labs <- c(spring = 'spring',
                autumn = 'autumn')

SWT_AT_Q <- abs_change %>%
  filter(season %in% c('spring', 'autumn')) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
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

SWT_ind <- plot_grid(SWT_AT, SWT_Q, nrow = 2, align = "vh", labels = c("A","B")) 

plot_grid(SWT_ind, SWT_AT_Q, nrow = 2, rel_heights = c(1.5,1), labels = c("", "C")) %>%
  ggsave(path = file.path(out_dir, experiment, "Plots"),
         filename = "SWT_change_S1.png",
         width= 15, height = 22, units = "cm")


# Figure s5 - responses with stream warmsing mitigation
# SWT w/ AT
SWT_AT_without <-
  abs_change_without %>%
  filter(season %in% c('summer', 'winter')) |> 
  # only want to look at Q_change effects
  filter(Q_change == 1) %>%
  ggplot(., aes(x=T_change, y=surfaceT_mean)) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
  geom_ribbon(aes(ymax = surfaceT_mean + surfaceT_sd,
                  ymin = surfaceT_mean - surfaceT_sd),colour = NA, alpha = 0.3) +
  geom_line() +
  geom_point(size =0.9) +
  facet_wrap(~season) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Air temperature change (+ °C)",
       y= "Change in SWT (°C)") +
  scale_y_continuous(limits = c(0,4))

# SWT w/ Q
SWT_Q_without <- abs_change_without %>%
  # only want to look at Q_change effects
  filter(T_change == 0,
         season %in% c('summer', 'winter')) %>%
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


facet_labs <- c(summer = 'summer',
                winter = 'winter')

SWT_AT_Q_without <- abs_change_without %>%
  filter(season %in% c('summer', 'winter')) |> 
  ggplot(aes(x=as.factor(Q_change), as.factor(T_change))) +
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

SWT_ind_without <- plot_grid(SWT_AT_without, SWT_Q_without, nrow = 2, align = "vh", labels = c("A","B")) 

plot_grid(SWT_ind_without, SWT_AT_Q_without, nrow = 2, rel_heights = c(1.5,1), labels = c("", "C")) %>%
  ggsave(path = file.path(out_dir, experiment_without, "Plots"),
         filename = "SWT_change_without.png",
         width= 15, height = 22, units = "cm")
  