
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
perc.change.v <- function(scenario_row, base_df, t) {
  old <- base_df[which(base_df[,t] == scenario_row[,t]), # matches the timestep
                 c("bottomT",
                   "density_diff",
                   #"md_1",
                   #"md_2",
                   "schmidt", 
                   "surfaceT", 
                   "vol_av_temp")] 
  new <- scenario_row[,c("bottomT",
                         "density_diff",
                         #"md_1",
                         #"md_2",
                         "schmidt", 
                         "surfaceT", 
                         "vol_av_temp")]
  perc_change <- percent.change(old, new)
  
  return(perc_change)
}

abs.change.v <- function(scenario_row, base_df, t) {
  old <- base_df[which(base_df[,t] == scenario_row[,t]),
                 c("bottomT",
                   "density_diff",
                   #"md_1",
                   #"md_2",
                   "schmidt", 
                   "surfaceT", 
                   "vol_av_temp")]
  
  new <- scenario_row[,c("bottomT",
                         "density_diff",
                         #"md_1",
                         #"md_2",
                         "schmidt", 
                         "surfaceT", 
                         "vol_av_temp")]
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

#calculate the density difference between top and bottom
density.diff <- function(wtr) {
  surface_col <- colnames(wtr)[which.min(as.numeric(gsub("wtr_", "", colnames(wtr))))]
  deepest_col <- colnames(wtr)[which.max(as.numeric(gsub("wtr_", "", colnames(wtr))))]
  diff <- rLakeAnalyzer::water.density(wtr[,deepest_col])  -  
    rLakeAnalyzer::water.density(wtr[,surface_col])
  return(diff)
}

# extract the season from date
as.season <- function(date) {
  yq <- zoo::as.yearqtr(zoo::as.yearmon(date, "%m/%d/%Y") + 1/12)
  season <- factor(format(yq, "%q"),
                   levels = 1:4,
                   labels = c("winter", "spring", "summer", "autumn"))
  return(season)
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

##### stratification metriccs #####
# write function to find longest period per year
max.strat <- function(temp_diff, dens_diff) {
  r <- rle(temp_diff > 0.1 &
             # top warmer than bottom
             dens_diff > 0.1)
  max_strat <- max(r$lengths[r$values==TRUE])
  return(max_strat)
}

# total stratification
total.strat <- function(temp_diff, dens_diff) {
  r <- rle(temp_diff > 0.1 &
             # top warmer than bottom
             dens_diff > 0.1)
  tot_strat <- sum(r$lengths[r$values==TRUE])
  return(tot_strat)
}

# total inverse strat
total.inverse <- function(temp_diff, dens_diff) {
  r <- rle(temp_diff < -0.1 &
             # top colder than bottom
             dens_diff > 0.1)
  tot_inv <- sum(r$lengths[r$values==TRUE])
  return(tot_inv)
}

###=== strat dates ====###
#getting the dates for the start and end of the longest (normally) stratified period
strat.dates <- function(temp_diff, dens_diff, dates) {
  r <- rle(temp_diff > 0.1 &
             # top warmer than bottom
             dens_diff > 0.1)
  
  # make into a df
  strat_length <- data.frame(strat = r$values,
                             lengths = r$lengths) %>%
    # Get the end of each run
    mutate(end = cumsum(lengths),
           # Get the start of each run
           start = end - lengths + 1)
  
  #define the start row
  start.row <- strat_length |> 
    filter(strat == T) |>  
    slice_max(lengths) |> 
    select(start) |> 
    pull()
  #gets the row with the start date
  
  #of the run which has the max length and is TRuE
  end.row <- strat_length |> 
    filter(strat == T) |>  
    slice_max(lengths) |> 
    select(end) |> 
    pull()#gets the row with the end date
  
  #of the run which has the max length and is TRuE
  strat_dates <- data.frame(start = dates[start.row],
                            end = dates[end.row])
  
  return(strat_dates)
  
  
}


