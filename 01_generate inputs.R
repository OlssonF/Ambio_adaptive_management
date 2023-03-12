
# create input files to use in the experiment looking at the interaction of changing
  # air temperature and inflow volume

#  baseline observations to be modified, 8 years
met_dat <- read.table("./GOTM/met_data_elter_2012-2019.dat", sep = "\t", 
                      header = T)
colnames(met_dat) <-  c("!DateTime", 
                        "u10",
                        "v10",
                        "MSLP",
                        "AT",
                        "RH",
                        "cc")

inflow_dat <- read.table("./GOTM/Inflow_BACI.dat", sep = "\t", header = T)
colnames(inflow_dat) <-  c("!DateTime", 
                           "inflow_Q",
                           "inflow_T")

# ==== air temperature =====
# 9 iterations ranging from 0.5 degree change to 4 degrees, plus current
temp_iterations <- sort(c(0, seq(0.5, 4, by = 0.5)))
# 9 iterations of flow ranging from decrease by 70% to increase by 70%, plus baseline
flow_iterations <- sort(c(1, seq(0.3, 1.7, by = 0.2)))


#using each of these factors change the air temperature in the driving data
for (i in 1:length(temp_iterations)) {
  met_dat_tmp <- met_dat
  met_dat_tmp$AT <- met_dat_tmp$AT + temp_iterations[i]
  write.table(met_dat_tmp, 
              file = paste0("./GOTM/Input/AT_iterations/met_data_elter_",
                            temp_iterations[i],
                            ".dat"),
              row.names = F, 
              quote = F, 
              sep = "\t")
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

# ===== inflow temperature =====
# based on changes in airT, inflow T will change
source('R/InflowT_from_AT.R')

#using the linear model with moving average
intercept_ma <- linear_ma$coefficients[[1]] # intercept
gradient_ma <- linear_ma$coefficients[[2]] # intercept

# based on the air temperature scenarios estimate the stream temperature
for (i in 1:length(temp_iterations)) {
  inflowT_tmp <- met_dat[,c("!DateTime", "AT")] %>%
    mutate(AT_val = AT + temp_iterations[i], # change the air temp
           AT_ma = rollapply(AT_val, 12, mean, align = "right", fill = NA),# moving av
           inflow_T = (AT_ma * gradient_ma) + intercept_ma) # calculate the inflowT
  
  #fill in gaps with 
  inflowT_tmp <- inflowT_tmp %>%
    mutate(inflow_T = ifelse(is.na(inflow_T), 
                                lead(inflow_T, n = 24), 
                                inflow_T))
  

  write.table(inflowT_tmp[,c("!DateTime", "inflow_T")], 
              file = paste0("GOTM/Input/AT_iterations/Inflow_T_",
                            temp_iterations[i],
                            ".dat"),
              row.names = F, 
              quote = F, 
              sep = "\t")
    print(i)
  Sys.sleep(0.01)
  flush.console()
}

# ========= inflow Q ============

#using each of these factors change the flow in the driving data
for (i in 1:length(flow_iterations)) {
  inflow_dat_tmp <- inflow_dat[,c("!DateTime", "inflow_Q")]
  inflow_dat_tmp$inflow_Q <- inflow_dat_tmp$inflow_Q * flow_iterations[i]
  write.table(inflow_dat_tmp, 
              file = paste0("GOTM/Input/Q_iterations/Inflow_Q_",
                            flow_iterations[i],
                            ".dat"),
              row.names = F, 
              quote = F, 
              sep = "\t")
  print(i)
  Sys.sleep(0.01)
  flush.console()
}


#=========================================#
