### Function ActCum- cumulative balance of C logistic activities and transport 
################################################################

# This function give the cumulative C balance accross all logging unit 
# as a list of two data.table with row = iteration and colum = year of simulation
# * CcumOtherAct : all other emissions 
# * CcumTrans : emissions from transports (forest to sawmill, forest to biomass plant and sawmill to biomass plant), includes loading

ActCum <- function(data, type, DistMillBioPlant, Ccontent, Tm0) {
  
  if(dim(data)[1] != dim(Tm0)[1]) {
    stop("data and Tm must have the same number of rows")} 
  
  # for plantation (whole plot NOT per ha)
  if (type=="Plant") {
    dataAct <- data.table(iter=  data$iter,
                          COtherAct = data[, (CplantAct) * AreaPlant],
                          CTrans = data[,AreaPlant * (CplantTrans +
                                                         (CSawmillFuel/Ccontent) * (EmisTransport/1000)* DistMillBioPlant)])  
    # we consider that the dust transported is fully dry
    # NB the loading of the SawmillFuel is not accounted for (but as DistMillBioPlant = 0, not important)
    }
  
  
  
  # for natural forest data for each plot (whole plot NOT per ha)
  if (type=="NatFor") {
  dataAct <- data.table(iter=  data$iter,
                        COtherAct = data[, (Cplanning + CroadBuild + CLogging) * AreaLogged],
                        CTrans = data[,AreaLogged * (CTrans +
                                                      (CSawmillFuel/Ccontent) * (EmisTransport/1000)* DistMillBioPlant)])
  # we consider that the dust transported is fully dry
  # NB the loading of the SawmillFuel is not accounted for (but as DistMillBioPlant = 0, not important)
  }
  
  # calculate CcumOtherAct  
  balanceCOtherAct <- dataAct[,COtherAct * Tm0]
  balanceCOtherAct$iter <- data$iter
  CcumOtherAct <- balanceCOtherAct[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumOtherAct[, iter:=NULL]
  
  # calculate CcumTrans 
  balanceCTrans <- dataAct[,CTrans * Tm0]
  balanceCTrans$iter <- data$iter
  CcumTrans <- balanceCTrans[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumTrans[, iter:=NULL]
  
  return(list(CcumOtherAct = CcumOtherAct,
              CcumTrans = CcumTrans))
  
}


