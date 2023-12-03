library(tidyverse)
library(lubridate)

setwd("E:\\GOTM\\elterwater\\R_scripts\\GOTM-2")
# want to look at the difference in temperature between the lake surface and inflow temperature
inflowT <- read.delim("../../elterwater run/Inflow_BACI.dat") %>%
  mutate(datetime = ymd_hms(X.DateTime)) %>%
  select(datetime, inflow_T)

# only the surface temperature under current conditions
surfaceT <- read.delim("P:/NEC06449_Stu_Freya_Olsson_Envision/Data/GOTM output/Experiments output/Paper 2/e - airT + inflowT + inflowQ/Summaries/Hourly_surfaceT.txt") %>%
  select(datetime, Q_1_T_0) %>%
  mutate(datetime = ymd_hms(datetime))

inflowT %>%
  group_by(yday(datetime)) %>%
  summarise(inflow_T = mean(inflow_T)) %>%
  ggplot(., aes(x=`yday(datetime)`, y= inflow_T))+
  geom_line() +
  geom_smooth(method ="gam")

surfaceT %>%
  group_by(yday(datetime)) %>%
  summarise(Q_1_T_0 = mean(Q_1_T_0)) %>%
  ggplot(., aes(x=`yday(datetime)`, y= Q_1_T_0))+
  geom_line() +
  geom_smooth(method = "gam")

# find the difference in temperature
temp_diff <- full_join(inflowT, surfaceT) %>%
  mutate(temp_diff = inflow_T - Q_1_T_0)

temp_diff %>%
  mutate(jday = yday(datetime)) %>%
  group_by(jday) %>%
  summarise(mean_diff = mean(temp_diff),
            sd_diff = sd(temp_diff)) %>%
  ggplot(., aes(x=jday, y= mean_diff)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Lake-inflow difference (oC)") +
  scale_x_continuous(name = "Day of year",
                     breaks = c(0,50,100,150,200,250,300, 350)) +
  geom_line() +
  geom_smooth(method = "gam", formula = y~s(x, bs = "cc"),
              se = F) 
