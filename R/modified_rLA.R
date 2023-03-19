#modified lake analyzer functions
# these modifications allow a density cut-off rather than a temperature
# cut-off to be be used when calculating mixed depth

findPeaks <- function(dataIn, thresh=0){
  
  varL = length(dataIn);
  locs = rep(FALSE, varL);
  peaks= rep(NaN, varL);
  
  for(i in 2:varL-1){
    pkI = which.max(dataIn[(i-1):(i+1)])
    posPeak = max(dataIn[(i-1):(i+1)]);
    
    if(pkI == 2) {
      peaks[i] = posPeak;
      locs[i]  = TRUE;
    }
  }
  
  inds = 1:varL;
  locs = inds[locs];
  peaks= peaks[locs];
  
  # remove all below threshold value
  
  useI = peaks > thresh;
  locs = locs[useI];
  
  return(locs)
}


drop.datetime <- function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"
  
  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
  
  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column. Datetime column was supplied.')
    }else{
      warning('Unable to find a datetime column. Assuming no datetime column was supplied.')
      return(data)
    }
    
  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }
  
  return(data[,-dt_indx, drop=FALSE])
}

get.datetime = function(data, error=FALSE){
  datetime.pattern = "(datetime|timestamp|time|date)"
  
  header = names(data)
  dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
  
  if(length(dt_indx) < 1){
    if(error){
      stop('Unable to find a datetime column.')
    }else{
      warning('Unable to find a datetime column, attempting to ignore.')
      return(NULL)
    }
  }else if(length(dt_indx) > 1){
    stop('datetime column ambiguity. You can only have one column of datetime.')
  }
  
  return(data[,dt_indx])
}


thermoD.density <- function(wtr, depths, Smin = 0.1, seasonal = TRUE, index = FALSE, 
                            mixed.cutoff = 0.5) { #density difference cutoff
  
  if (any(is.na(wtr))) {
    return(NaN)
  }
  if (diff(range(water.density(wtr), na.rm = TRUE)) < mixed.cutoff) { #cutoff changed to a density difference cutoff
    return(NaN)
  }
  if (length(wtr) < 3) {
    return(NaN)
  }
  if (length(depths) != length(unique(depths))) {
    stop("Depths all must be unique")
  }
  rhoVar = water.density(wtr)
  dRhoPerc = 0.15
  numDepths = length(depths)
  drho_dz = rep(NaN, numDepths - 1)
  for (i in 1:(numDepths - 1)) {
    drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i + 
                                                       1] - depths[i])
  }
  thermoInd = which.max(drho_dz)
  mDrhoZ = drho_dz[thermoInd]
  thermoD = mean(depths[thermoInd:(thermoInd + 1)])
  if (thermoInd > 1 && thermoInd < (numDepths - 1)) {
    Sdn = -(depths[thermoInd + 1] - depths[thermoInd])/(drho_dz[thermoInd + 
                                                                  1] - drho_dz[thermoInd])
    Sup = (depths[thermoInd] - depths[thermoInd - 1])/(drho_dz[thermoInd] - 
                                                         drho_dz[thermoInd - 1])
    upD = depths[thermoInd]
    dnD = depths[thermoInd + 1]
    if (!is.infinite(Sup) & !is.infinite(Sdn)) {
      thermoD = dnD * (Sdn/(Sdn + Sup)) + upD * (Sup/(Sdn + 
                                                        Sup))
    }
  }
  dRhoCut = max(c(dRhoPerc * mDrhoZ, Smin))
  locs = findPeaks(drho_dz, dRhoCut)
  pks = drho_dz[locs]
  if (length(pks) == 0) {
    SthermoD = thermoD
    SthermoInd = thermoInd
  }
  else {
    mDrhoZ = pks[length(pks)]
    SthermoInd = locs[length(pks)]
    if (SthermoInd > (thermoInd + 1)) {
      SthermoD = mean(depths[SthermoInd:(SthermoInd + 1)])
      if (SthermoInd > 1 && SthermoInd < (numDepths - 1)) {
        Sdn = -(depths[SthermoInd + 1] - depths[SthermoInd])/(drho_dz[SthermoInd + 
                                                                        1] - drho_dz[SthermoInd])
        Sup = (depths[SthermoInd] - depths[SthermoInd - 
                                             1])/(drho_dz[SthermoInd] - drho_dz[SthermoInd - 
                                                                                  1])
        upD = depths[SthermoInd]
        dnD = depths[SthermoInd + 1]
        if (!is.infinite(Sup) & !is.infinite(Sdn)) {
          SthermoD = dnD * (Sdn/(Sdn + Sup)) + upD * 
            (Sup/(Sdn + Sup))
        }
      }
    }
    else {
      SthermoD = thermoD
      SthermoInd = thermoInd
    }
  }
  if (SthermoD < thermoD) {
    SthermoD = thermoD
    SthermoInd = thermoInd
  }
  if (index) {
    if (seasonal) {
      return(SthermoInd)
    }
    else {
      return(thermoInd)
    }
  }
  else {
    if (seasonal) {
      return(SthermoD)
    }
    else {
      return(thermoD)
    }
  }
}


thermoD.depth <- function (wtr, depths, Smin = 0.1, seasonal = TRUE, index = FALSE, 
                           mixed.cutoff = 1) 
{
  if (any(is.na(wtr))) {
    return(NaN)
  }
  if (diff(range(water.density(wtr), na.rm = TRUE)) < mixed.cutoff) { #density cutoff
    return(NaN)
  }
  if (length(wtr) < 3) {
    return(NaN)
  }
  if (length(depths) != length(unique(depths))) {
    stop("Depths all must be unique")
  }
  rhoVar = water.density(wtr)
  dRhoPerc = 0.15
  numDepths = length(depths)
  drho_dz = rep(NaN, numDepths - 1)
  for (i in 1:(numDepths - 1)) {
    drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i + 
                                                       1] - depths[i])
  }
  thermoInd = which.max(drho_dz)
  mDrhoZ = drho_dz[thermoInd]
  thermoD = mean(depths[thermoInd:(thermoInd + 1)])
  if (thermoInd > 1 && thermoInd < (numDepths - 1)) {
    Sdn = -(depths[thermoInd + 1] - depths[thermoInd])/(drho_dz[thermoInd + 
                                                                  1] - drho_dz[thermoInd])
    Sup = (depths[thermoInd] - depths[thermoInd - 1])/(drho_dz[thermoInd] - 
                                                         drho_dz[thermoInd - 1])
    upD = depths[thermoInd]
    dnD = depths[thermoInd + 1]
    if (!is.infinite(Sup) & !is.infinite(Sdn)) {
      thermoD = dnD * (Sdn/(Sdn + Sup)) + upD * (Sup/(Sdn + 
                                                        Sup))
    }
  }
  dRhoCut = max(c(dRhoPerc * mDrhoZ, Smin))
  locs = findPeaks(drho_dz, dRhoCut)
  pks = drho_dz[locs]
  if (length(pks) == 0) {
    SthermoD = thermoD
    SthermoInd = thermoInd
  }
  else {
    mDrhoZ = pks[length(pks)]
    SthermoInd = locs[length(pks)]
    if (SthermoInd > (thermoInd + 1)) {
      SthermoD = mean(depths[SthermoInd:(SthermoInd + 1)])
      if (SthermoInd > 1 && SthermoInd < (numDepths - 1)) {
        Sdn = -(depths[SthermoInd + 1] - depths[SthermoInd])/(drho_dz[SthermoInd + 
                                                                        1] - drho_dz[SthermoInd])
        Sup = (depths[SthermoInd] - depths[SthermoInd - 
                                             1])/(drho_dz[SthermoInd] - drho_dz[SthermoInd - 
                                                                                  1])
        upD = depths[SthermoInd]
        dnD = depths[SthermoInd + 1]
        if (!is.infinite(Sup) & !is.infinite(Sdn)) {
          SthermoD = dnD * (Sdn/(Sdn + Sup)) + upD * 
            (Sup/(Sdn + Sup))
        }
      }
    }
    else {
      SthermoD = thermoD
      SthermoInd = thermoInd
    }
  }
  if (SthermoD < thermoD) {
    SthermoD = thermoD
    SthermoInd = thermoInd
  }
  if (index) {
    if (seasonal) {
      return(SthermoInd)
    }
    else {
      return(thermoInd)
    }
  }
  else {
    if (seasonal) {
      return(SthermoD)
    }
    else {
      return(thermoD)
    }
  }
}

ts.thermoD.depth <- function (wtr, Smin = 0.1, na.rm = FALSE, ...) 
{
  depths = get.offsets(wtr)
  n = nrow(wtr)
  t.d = rep(NA, n)
  wtr.mat = as.matrix(drop.datetime(wtr))
  dimnames(wtr.mat) <- NULL
  for (i in 1:n) {
    if (na.rm) {
      temps = wtr.mat[i, ]
      notNA = !is.na(temps)
      t.d[i] = thermoD.depth(temps[notNA], depths[notNA], 
                             ...)
    }
    else {
      if (any(is.na(wtr.mat[i, ]))) {
        t.d[i] = NA
        next
      }
      t.d[i] = thermoD.depth(wtr.mat[i, ], depths, ...)
    }
  }
  output = data.frame(datetime = get.datetime(wtr), thermo.depth = t.d)
  return(output)
}


metaD.density <- function (wtr, depths, slope = 0.1, seasonal = TRUE, mixed.cutoff = 0.5) 
{
  if (any(is.na(wtr))) {
    return(rep(NaN, 2))
  }
  if (length(wtr) < 3) {
    return(c(max(depths), max(depths)))
  }
  depths = sort.int(depths, index.return = TRUE)
  wtr = wtr[depths$ix]
  depths = depths$x
  thermoD = thermoD.density(wtr, depths, seasonal = seasonal, 
                            mixed.cutoff = mixed.cutoff)
  if (is.na(thermoD)) {
    return(c(NaN, NaN))
  }
  rhoVar = water.density(wtr)
  dRhoPerc = 0.15
  numDepths = length(depths)
  drho_dz = vector(mode = "double", length = numDepths - 1)
  for (i in 1:(numDepths - 1)) {
    drho_dz[i] = (rhoVar[i + 1] - rhoVar[i])/(depths[i + 
                                                       1] - depths[i])
  }
  metaBot_depth = depths[numDepths]
  metaTop_depth = depths[1]
  Tdepth = rep(NaN, numDepths - 1)
  for (i in 1:(numDepths - 1)) {
    Tdepth[i] = mean(depths[i:(i + 1)])
  }
  tmp = sort.int(unique(c(Tdepth, thermoD)), index.return = TRUE)
  sortDepth = tmp$x
  sortInd = tmp$ix
  numDepths = length(sortDepth)
  drho_dz = stats::approx(Tdepth, drho_dz, sortDepth)
  drho_dz = drho_dz$y
  thermo_index = which(sortDepth == thermoD)
  for (i in thermo_index:numDepths) {
    if (!is.na(drho_dz[i]) && drho_dz[i] < slope) {
      metaBot_depth = sortDepth[i]
      break
    }
  }
  if (i - thermo_index >= 1 && (!is.na(drho_dz[thermo_index]) && 
                                drho_dz[thermo_index] > slope)) {
    metaBot_depth = stats::approx(drho_dz[thermo_index:i], 
                                  sortDepth[thermo_index:i], slope)
    metaBot_depth = metaBot_depth$y
  }
  if (is.na(metaBot_depth)) {
    metaBot_depth = depths[numDepths]
  }
  for (i in seq(thermo_index, 1)) {
    if (!is.na(drho_dz[i]) && drho_dz[i] < slope) {
      metaTop_depth = sortDepth[i]
      break
    }
  }
  if (thermo_index - i >= 1 && (!is.na(drho_dz[thermo_index]) && 
                                drho_dz[thermo_index] > slope)) {
    metaTop_depth = stats::approx(drho_dz[i:thermo_index], 
                                  sortDepth[i:thermo_index], slope)
    metaTop_depth = metaTop_depth$y
  }
  if (is.na(metaTop_depth)) {
    metaTop_depth = depths[i]
  }
  return(c(metaTop_depth, metaBot_depth))
}


ts.metaD.density <- function (wtr, slope = 0.1, na.rm = FALSE, ...) 
{
  depths = get.offsets(wtr)
  n = nrow(wtr)
  wtr.mat = as.matrix(dplyr::select(wtr, -c(datetime)))
  m.d = matrix(NA, nrow = n, ncol = 2)
  for (i in 1:n) {
    if (na.rm) {
      temps = wtr.mat[i, ]
      notNA = !is.na(temps)
      m.d[i, ] = metaD.density(temps[notNA], depths[notNA], 
                               slope, ...)
    }
    else {
      m.d[i, ] = metaD.density(wtr.mat[i, ], depths, slope, 
                               ...)
    }
  }
  return(data.frame(datetime = wtr$datetime, top = m.d[, 
                                                       1], bottom = m.d[, 2]))
}

#==========================#
