#### Planning of logging in natural forest
#################################

# This function select all the plots for logging and give their ACS0, 
# the ecological characteristic of the plot,
# their area effectivelly logged(AreaLogged) and the time of logging t0
# it does this for each iteration

# VolP is matrix of Vol produced by plantation (m3) for each year (in colum) and each iteration (in row), obtained from plantation model
# VolTim is the  volume of timber expected to be produced for each year (m3), set by scenario
# LogInt is the logging intensity (m3/ha)
# Tsimu is the time over which is run the whole model
# alpha_LoggingPlan and beta_LoggingPlan Parameter of beta distrib giving the ratio between total area and logged area in natural forest
# dataLogUnit are the information on logging unit from actual values ONF
# nrun is the number of simulation

LoggingPlan <- function (nrun, VolP, VolTimb, LogInt, Tsimu, 
                         dataLogUnit, alpha_LoggingPlan, beta_LoggingPlan, PlantDuration,
                         scenario) {
  if(dim(VolP)[1]/nrun!=Tsimu) {
    stop("The length of  VolP should be equal to Tsimu * nrun")}
  vRun <- vector("list", nrun) # list of data.tables of results (one per run) 
  for (r in 1:nrun) { # for one simulation
    # create an empty table for all year of a run
    dataRun <- data.table(iter = integer(),
                          ID_LogUnit = factor(),
                          AGB0 = numeric(),
                          MAP = numeric(),
                          RainSeas = numeric(),
                          BulkDens = numeric(),
                          AreaLogUnit = numeric(),
                          t0 = integer(),
                          AreaLogged = numeric())
    
    for (i in 1:Tsimu) {# for one year in Tsimu and one run 
      ## sample logging units from dataLogUnit (only among plots not already taken in this run)
      # and keep as many as needed
      # first get a subset of dataLogUnit without plots already taken this run
      LogUnitAvail <- dataLogUnit[!(ID_LogUnit %in% dataRun$ID_LogUnit),]
      # sample within it
      if (dim(LogUnitAvail)[1]>0) # if there are still plots available for logging
      {
        # draw them all in a random order
        dataYrRun <- data.table(iter = r,
                                LogUnitAvail[sample(.N)],
                                t0 = i) 
        # add area actually logged
        dataYrRun$AreaLogged <- dataYrRun$AreaLogUnit*rbeta(nrow(dataYrRun), 
                                                            alpha_LoggingPlan,beta_LoggingPlan)
      } else { break } # end the loop on year as there won't be enought for the following years either
      

      ## remove final rows once the target production is reached for this year
      # check if the target is reach
      if (tail(cumsum(dataYrRun$AreaLogged*LogInt),1) >= 
          VolTimb-VolP[iter==r & t0==i, VolPlant]) {
        # if it is => remove row exceeding the production
        ## lastUnitHarv: last logging unit needed to reach our goal; the rest will be discarded
        lastUnitHarv <- 
          min(which(cumsum(dataYrRun$AreaLogged*LogInt) >= VolTimb-VolP[iter==r & t0==i, VolPlant]))
        dataYrRun <- dataYrRun[1:lastUnitHarv,]        
      } else {
        # give an empty table for this year 
        #(to avoid have year with some logging but not enought to reach the target)
        dataYrRun <- dataYrRun[0,]
        break # break the loop to not have to run following years
      }
      
      # add data for this year to data for all year of a run
      dataRun <- rbindlist(c(list(dataRun), list(dataYrRun)), use.names = FALSE)
    } # enf of the loop for a given year
    
    vRun[[r]] <- dataRun  
  } # end of the loop for a given run
  dataNatFor <- rbindlist(vRun, use.names = TRUE)
  
  # remove all years after the first year that doesn't reach the target, accross all runs
  minLast <- min(dataNatFor[,max(t0), by=iter]$V1) # get the smallest last year accross runs
  dataNatFor <- dataNatFor[t0<=minLast]
  
  #  warning when there is not enought to reach the target for 
  #the whole lenght of simulation for all runs
  if (minLast != Tsimu) {
    warning('It is not possible to reach the targeted volume of logged wood without logging a plot that as already been logged. The function stops at the last year for which it is possible to reach the targeted volume.') 
  }
  
  # for scenario substitution, stop logging as soon as the plantation start producing
  if (scenario == "PlantSubst") {
    dataNatFor <- dataNatFor[t0<=PlantDuration]
  }
  
  dataNatFor$LogInt <- LogInt
  
  return(dataNatFor)
}  


