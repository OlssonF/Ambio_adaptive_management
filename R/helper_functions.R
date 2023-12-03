
# functions
#====functions needed====
#write a function to calculate the percentage change
percent.change <-  function(old, new) { # this says what is needed in the function
  percent_change <- ((new - old)/old)*100 # this defines the function
  return(percent_change) #this says what is returned in the console
}

#write a function to calculate the absolute change
abs.change <-  function(old, new) {
  abs_change <- (new - old)
  return(abs_change)
}


# vectorized version of percentage change function
perc.change.v <- function(scenario_row, base_df, t, metrics) {
  old <- base_df[which(base_df[,t] == pull(scenario_row[,t])), # matches the timestep
                 metrics] 
  new <- scenario_row[,metrics]
  perc_change <- percent.change(old, new)
  
  return(perc_change)
}

abs.change.v <- function(scenario_row, base_df, t, metrics) {
  old <- base_df[which(base_df[,t] == pull(scenario_row[,t])),
                 metrics]
  
  new <- scenario_row[metrics]
  abs_change <-  abs.change(old, new)
  
  return(abs_change)
}
#=========================================#
# approximate the mixed depth based on a 0.1 kg/m3 density difference from the top 
md.dens.diff <- function(wtr, max.depth, diff) {
  test <- water.density(wtr[,2:51]) %>%
    bind_cols(select(wtr, datetime),.) %>%
    pivot_longer(wtr_0.06:wtr_5.94,
                 names_to = "depth",
                 names_prefix = "wtr_",
                 values_to = "wtr_density", 
                 names_ptypes = list(depth = numeric())) 
  
  md <- test[-c(1:51),] %>%
    group_by(datetime) %>%
    summarise(md = approx(xout = wtr_density[depth == min(depth)] + diff, 
                          # where is the density 0.1 more than surface 
                          y= depth, 
                          x = wtr_density, 
                          method = "linear")$y) %>%
    mutate()
  
  return(c(6,ifelse(!is.na(md$md),
                md$md, max.depth)))
}



# extract the season from date
as.season <- function(date) {
  yq <- zoo::as.yearqtr(zoo::as.yearmon(date, "%m/%d/%Y") + 1/12)
  season <- factor(format(yq, "%q"),
                   levels = 1:4,
                   labels = c("winter", "spring", "summer", "autumn"))
  return(season)
}

# extract the season and from date
as.seasonyear <- function(date) {
  yq <- zoo::as.yearqtr(zoo::as.yearmon(date, "%m/%d/%Y") + 1/12)
  season <- factor(format(yq, "%q"),
                   levels = 1:4,
                   labels = c("winter", "spring", "summer", "autumn"))
  season_year <- paste(season, year(yq))
  return(season_year)
}

# extract the season from date, but uses the meteorological year
# so the january/February of the year = winter previous year
  # e.g Jan 2012 == winter 2011
as.met.season <- function(date) {
  yq <- zoo::as.yearqtr(zoo::as.yearmon(date, "%m/%d/%Y") - 2/12)
  year_season <- paste(factor(format(yq, "%q"),
                         levels = 1:4,
                         labels = c("spring", "summer", "autumn", "winter")),
                  format(yq, "%Y"))
  
  return(year_season)
}



