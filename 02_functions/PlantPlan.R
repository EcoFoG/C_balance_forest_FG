# Function PlantPlan - planning of plantation
################################################################"

# This function selects the plots in which to plant every year and for each simulation.
# The objective is that the cumulated area planted by year i is > AreaPlant * i
# So that is there is too much planted in one year, the next year is less planted to compensate

PlantPlan <- function(nrun, PlantDuration, Ccontent, dataPlotforPlant, Tsimu, RCub,
                      VolTimbPlant, ChangeProd) {
  
  # Expected volume of timber at the final harvest of a plantation (in m3/ha)
  VolPlantExp <- get_VolPlantExp(AGBendPlant=AGBendPlant, Ccontent=Ccontent,
                                 PlantDuration=PlantDuration, RCub=RCub, 
                                 Data_wood_plantation=Data_wood_plantation,
                                 ChangeProd=ChangeProd)
  # get the expected volume of timber at har, Ccontent, PlantDuration, RCub, Data_wood_plantation)
  # get the expected volume of timber at harvest of plantation, by simulating a very high number of plantation and taking the mean
  # Area to plant every year to get the expected annual timber harvest (VolTimbPlant) (in ha)
  Area2Plant <- VolTimbPlant / VolPlantExp
  
  vRun <- vector("list", nrun) # list of data.tables of results (one per simulation) 
  for (r in 1:nrun) { # for one run
    # create an empty table for all year of a run
    dataRun <- data.table(iter = integer(),
                          ID_Plant = factor(),
                          AreaPlant = numeric(), # in ha
                          AGB0 = numeric(), # per ha, before logging
                          YearLogged = numeric(), 
                          AreaTot = numeric(),
                          AreaLogged = numeric(),
                          code_parcOLD = factor(),
                          t0 = integer(), # year of the event (here the plantation),
                          PlantCycle = integer()) # number of planting cycle

    # for each year in the first cycle (before the first harvest)
    # plant on deforested natural forest starting with the first decade, then the next, then...
    for (i in 1:PlantDuration) {
      # get the objective for this year as
      # the cumulated expected up to year i - the cumulated actually planted by year i-1
      Area2PlantYr <- Area2Plant * i - dataRun[,sum(AreaPlant)] 
      
      # if the objective is negative (planted too much so far) => don't plant this year
      if(Area2PlantYr <= 0) {next}
      
      # get plots that have been logged in the 10 earlier years (1974-1983) and not yet planted
      dataAvail1 <- dataPlotforPlant[!(ID_Plant %in% dataRun$ID_Plant) & YearLogged>=1974 & YearLogged<1984]
      if (dim(dataAvail1)[1]>0) {
        dataYrRun <- data.table(iter = r,
                                dataAvail1[sample(.N)],
                                t0 = i,
                                PlantCycle = 1)
      } else { # if no more available => get an empty table
        dataYrRun <- data.table(iter = integer(),
                                dataAvail1[sample(.N)],
                                t0 = integer(),
                                PlantCycle = integer()) }
      
      # check if there is enough to reach the targeted Area2Plant
      # if not, get in the second decade
      if (dataYrRun[,sum(AreaPlant)] < Area2PlantYr) {
        dataAvail2 <- dataPlotforPlant[!(ID_Plant %in% dataRun$ID_Plant) & YearLogged>=1984 & YearLogged<1994]
        if (dim(dataAvail2)[1]>0) {
          dataYrRun <- rbindlist(c(list(dataYrRun), 
                                   list(data.table(iter = r,
                                                   dataAvail2[sample(.N)],
                                                   t0 = i,
                                                   PlantCycle = 1))), use.names = TRUE)
        } else {
          stop("There is not enought plots available for conversion to plantation. Can be changed in the planting function")
        } 
        
      }
      
      # remove the extra rows for this year
      lastPlantNeeded <- min(which(cumsum(dataYrRun$AreaPlant) >= Area2PlantYr)) # last plantation needed to reach the area needed
      dataYrRun <- dataYrRun[1:lastPlantNeeded,]

      dataRun <- rbindlist(c(list(dataRun), list(dataYrRun)), use.names = TRUE)
    }
    
   
    # for following planting cycles
    # count how many planting cycles in total
    Ncycle <- ceiling(Tsimu/PlantDuration)
    # add full cutting cycle
    for (R in 2:Ncycle) {
      dataRun <- rbindlist(c(list(dataRun), 
                             list(data.table(dataRun[PlantCycle==1,!"PlantCycle",with=FALSE],
                                             PlantCycle = R))), use.names = TRUE)
      dataRun[PlantCycle==R, t0:=(as.integer(t0+PlantDuration*(R-1)))]
    }
    # remove the year after the end of Tsimu
    dataRun <- dataRun[t0 <=Tsimu,]
    
    vRun[[r]] <- dataRun
  } 
  
  dataPlantation <- rbindlist(vRun, use.names = TRUE)
  dataPlantation$tPlant <- dataPlantation$t0
  dataPlantation$trelat <- 0
  dataPlantation$YearType <- as.factor("P")
  dataPlantation <- cbind(dataPlantation, Data_wood_plantation[sample(.N, dim(dataPlantation)[1], replace=TRUE), c("WDPlant", "PCIhPlant")])
  return(dataPlantation)  
}


