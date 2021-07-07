# Function Sawmill - cumulative C balance of C from the sawmill
###############################################################################

# This function give the cumulative C balances from the sawmill plant accross all logging unit 
# as list a data.table with row = iteration and colum = year of simulation
# CcumSawnAlt is the culumative flux in sawnwood used instead of another material
# CcumSawnwood is culumative flux in sawnwood used as wood (not instead of something else)
# CcumDustWaste is culumative emission of C from dust waste
# CSawmillFuel is a vector of the C in byproduct of sawmill that is sent to the biomass plant to produce energy

# type is NatFor for natural forest and Plant for plantation

Sawmill <- function(data, type, Esawmill, DFtable, t05, Conso,
                    RsawnAlt, Rwaste, nrun, Tm0, Tm) {
  
  if(dim(data)[1] != dim(Tm0)[1]) {
    stop("data and Tm0 must have the same number of rows")}
  if(dim(data)[1] != dim(Tm)[1]) {
    stop("data and Tm must have the same number of rows")} 
  
  # For Plantation
  # get amount of C in each "fate" of the wood arriving at the sawmill for each plot, each year and each iteration
  # in tC (for the whole plot NOT per ha)  
  if (type=="Plant") {
    dataSawmill <- data.table(iter = data$iter,
                              CsawnAlt = data[,Csawmill * AreaPlant * Esawmill * RsawnAlt], # in wood sawn and used instead of another material
                              CsawnWood = data[,Csawmill * AreaPlant * Esawmill * (1-RsawnAlt)], # in wood sawn and use not as a replacement of another material
                              CDustWaste = data[,Csawmill * AreaPlant  * (1-Esawmill) * Rwaste]) # in sawdust true waste
    CSawmillFuelperHA = data[,Csawmill * (1-Esawmill) * (1-Rwaste)] # in wood that goes to biomass plant
  }
  
  # FOR NATURAL FOREST
  # get amount of C in each "fate" of the wood arriving at the sawmill for each plot and each iteration
  # in tC (for the whole plot NOT per ha)
  if (type=="NatFor") {
    dataSawmill <- data.table(iter = data$iter,
                              CsawnAlt = data[,LogInt * WDtimber * Ccontent * 
                                                  AreaLogged * Esawmill * RsawnAlt], # in wood sawn and use instead of another material
                              CsawnWood = data[,LogInt * WDtimber * Ccontent * 
                                                 AreaLogged * Esawmill * (1-RsawnAlt)], # in wood sawn and use not as a replacement of another material
                              CDustWaste = data[,LogInt * WDtimber * Ccontent * 
                                                  AreaLogged * Rwaste* (1- Esawmill)]) # in sawdust true waste
    CSawmillFuelperHA = data[,LogInt * WDtimber * Ccontent * 
                          (1- Esawmill) *(1 - Rwaste)] # in wood that goes to biomass plant
  }
  
  
  # FOR PAST LOGGING
  # get amount of C in each "fate" of the wood arriving at the sawmill for each plot and each iteration
  # in tC (for the whole plot NOT per ha)
  if (type=="PastLog") {
    # Rwaste is not constant : 0 before the construction of first biomass plant (tfirstbioPlant), Rwaste after
    data$RwastePast <- NA
    data$RwastePast <- as.numeric(data$RwastePast)
    data[t0<tfirstbioPlant, RwastePast:=1]
    data[t0>=tfirstbioPlant, RwastePast:=Rwaste]
    
    dataSawmill <- data.table(iter = data$iter,
                              CsawnAlt = data[,LogInt * WDtimber * Ccontent * 
                                                  AreaLogged * Esawmill * RsawnAlt], # in wood sawn and use instead of another material
                              CsawnWood = data[,LogInt * WDtimber * Ccontent * 
                                                 AreaLogged * Esawmill * (1-RsawnAlt)], # in wood sawn and use not as a replacement of another material
                              CDustWaste = data[,LogInt * WDtimber * Ccontent * 
                                                  AreaLogged * RwastePast* (1- Esawmill)]) # in sawdust true waste
    CSawmillFuelperHA = data[,LogInt * WDtimber * Ccontent * 
                               (1- Esawmill) *(1 - RwastePast)] # in wood that goes to biomass plant
  }

  
  #### calculate CcumSawnAlt the culumative flux in sawnwood used instead of another material
  dataSawmill <- merge(dataSawmill, DFtable, by="iter") # add DF to dataSawmill
  balanceCSawnAlt <- dataSawmill[, - CsawnAlt * DF * Tm0] # per plot*iteration 
  balanceCSawnAlt$iter <- data$iter
  CcumSawnAlt <- balanceCSawnAlt[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumSawnAlt[, iter:=NULL]
  
  
  #### calculate CcumSawnwood the culumative flux in sawnwood used as wood (not instead of something else)
  # decay of Sawnwood
    dataSawmill <- merge(dataSawmill, t05, by="iter")
    dataSawmill$lambdaSW <- dataSawmill[,log(2)/t05]
    CdecaySawn <- dataSawmill[, CsawnWood * (1-exp(-lambdaSW*Tm))]
  # emission due to functionning
    dataSawmill <- merge(dataSawmill, Conso, by="iter")
    
    CemisSawn <- dataSawmill[,CsawnWood * Conso * Cfossil * Tm0/ 
                               (WDtimber * Ccontent*1000)]
    CemisSawn[is.na(CemisSawn)] <- 0
  # sum of the two by plot*iteration
  balanceCSawnwood <- CdecaySawn + CemisSawn
  balanceCSawnwood$iter <- data$iter
  CcumSawnwood <- balanceCSawnwood[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumSawnwood[, iter:=NULL]
  
  
  
  #### calculate CcumDustWaste the culumative emission of C from dust waste
  balanceCDustWaste <- dataSawmill[,CDustWaste * Tm0]
  balanceCDustWaste$iter <- data$iter
  CcumDustWaste <- balanceCDustWaste[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumDustWaste[, iter:=NULL]  
  
  ### calculate CcumSawmillNoAvoid, the cumulative balance of the sawmill 
  # if they were no avoided emission from the use of wood instead of another material
  # to do this, we calculate the balance of the sawmill if all the wood (ie CsawnWood + CsawnAlt) was used for a product usually made of wood
    # decay of Sawnwood
  CdecaySawnIFnoAvoid <- dataSawmill[, (CsawnWood+CsawnAlt) * (1-exp(-lambdaSW*Tm))]
  # emission due to functionning
  CemisSawnIFnoAvoid <- dataSawmill[,(CsawnWood+CsawnAlt) * Conso * Cfossil * Tm0/ 
                             (WDtimber * Ccontent*1000)]
  CemisSawnIFnoAvoid[is.na(CemisSawnIFnoAvoid)] <- 0
  # sum of the two by plot*iteration
  balanceCSawnwoodIFnoAvoid <- CdecaySawnIFnoAvoid + CemisSawnIFnoAvoid
  balanceCSawnwoodIFnoAvoid$iter <- data$iter
  CcumSawnwoodIFnoAvoid <- balanceCSawnwoodIFnoAvoid[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumSawnwoodIFnoAvoid[, iter:=NULL]
  
  
  
  return(list(CSawmillFuel = CSawmillFuelperHA,
              CcumSawnAlt = CcumSawnAlt,
              CcumSawnwood = CcumSawnwood,
              CcumDustWaste = CcumDustWaste,
              CcumSawmillNoAvoid = CcumSawnwoodIFnoAvoid + CcumDustWaste)) # total emission of the sawmill if no use of wood instead of another material
}



