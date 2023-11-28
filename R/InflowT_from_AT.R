library("lubridate")
library("plyr")
library("dplyr")
library("ggplot2")
library("zoo")


#read in temperature and discharge data from the pipe
transfer_temp <- read.csv(file = "./Data/Transfer_data_July2017_Dec2019.csv", header=T) %>%
  select(Date_Time, Temp) |> 
  mutate(DateTime = dmy_hm(as.character(Date_Time)), #change datetime format
         Date = format(as.Date(DateTime,format="%Y-%m-%d")), #add date only
         Hour = format(trunc(DateTime, units= "hour"))) #add hour only

#======read in the air temperatures
AT <- read.csv("./GOTM/met_data_elter_2012-2019.dat", sep = "\t") |> 
  select(X.DateTime, AT) |> 
  dplyr::rename(DateTime = X.DateTime) %>%
  mutate(DateTime = ymd_hms(DateTime),
         Date = as.Date(DateTime))


#get the length of data that is the same as the transfers
start_date <- ymd_hms(transfer_temp$DateTime[1])
end_date <- ymd_hms(transfer_temp$DateTime[nrow(transfer_temp)])


#================================================
#find the hourly average for the pipe temps

transfer_temp_hourly <- transfer_temp %>%
  group_by(Hour) %>%
  summarise(pipe_T = mean(Temp)) %>%
  dplyr::rename(DateTime = Hour) %>%
  mutate(DateTime = ymd_hms(DateTime))


merge <- right_join(AT, transfer_temp_hourly, by = "DateTime")


merge_ma <- merge %>%
  mutate(AT_ma = rollapply(AT, 12, mean, align = "right", fill = NA),
         pipe_T_ma =rollapply(pipe_T, 12, mean, align = "right", fill = NA))
         

#==================================================

#======finding the relationship 


# hourly
linear_ma <- lm(merge_ma$pipe_T ~ merge_ma$AT_ma)
summary(linear_ma)

intercept2 <- linear_ma$coefficients[1]
gradient2 <- linear_ma$coefficients[2]

IB_inflow_temp_roll <- AT[,1:2] %>%
  mutate(AT_ma = rollapply(AT, 12, mean, align = "right", fill = NA),
         Water_T = (AT_ma * gradient2) + intercept2)


# what is the error in the model fit, compared to the observations

merge_ma <- merge_ma %>%
  mutate(pred_wtr_ra = predict.lm(linear_ma, 
                                  newdata = data.frame(AT = AT_ma)),
         residual_ra = pipe_T - pred_wtr_ra) # find the residual


# FILL IN 12 MISSING VALUES with next 24
IB_inflow_temp_roll  <- IB_inflow_temp_roll %>%
  mutate(Water_T = ifelse(is.na(Water_T),
                            lead(Water_T, n = 24),
                            Water_T))


# #============================================#
