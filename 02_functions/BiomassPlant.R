# Function BiomassPlant - cumulative C balance of C from the biomass plant
###############################################################################

# This function give the cumulative C balance from the biomass plant accross all logging unit 
# as a list of data.table with row = iteration and colum = year of simulation
# CcumBurnt : emission through biomass combustion
# CcumAvoidFuel : avoided emission for energy
# CcumOpBioPlant : functioning of biomass plant

BiomassPlant <- function(data, type, PCIhNatFor, H , EbiomassPlant, Cfossil, Tm0) {

  if(dim(data)[1] != dim(Tm0)[1]) {
    stop("data and Tm0 must have the same number of rows")}

  # FOR Plantation
  # data for each plot and year and iteration (whole plot NOT per ha)
  if (type=="Plant") {
    dataBiomassP <- data.table(iter = data$iter,
                               Cburnt = data[, (Cfuel + CSawmillFuel) * AreaPlant], # C (t) emitted by combustion
                               Fuelwood = data[, (Cfuel + CSawmillFuel) * AreaPlant /
                                 (Ccontent * (1-H))], # quantity of green fuelwood for the whole plot (in t)
                               PCIhPlant=data$PCIhPlant)
    dataBiomassP$CavoidFuel <- dataBiomassP[, PCIhPlant * Fuelwood * EbiomassPlant * Cfossil/1000] 
  }
  
  # FOR NATURAL FOREST
  # data for each plot and iteration (whole plot NOT per ha)
  if (type=="NatFor") {
    dataBiomassP <- data.table(iter = data$iter,
                               Cburnt = data[,(Cdamfuel + CDeforFuel + CSawmillFuel) * AreaLogged], # C (t) emitted by combustion
                               Fuelwood = data[,(Cdamfuel + CDeforFuel + CSawmillFuel) * AreaLogged 
                                            / (Ccontent * (1-H))]) # quantity of green fuelwood for the whole plot (in t)
    dataBiomassP$CavoidFuel <- dataBiomassP[, PCIhNatFor * Fuelwood * EbiomassPlant * Cfossil/1000] # in t
  }
 
  dataBiomassP$CopBiomass <- dataBiomassP[, Fuelwood * EmisBiomassPlant] # in t
  
  # calculate CcumBurnt
  balanceCBurnt <- dataBiomassP[,Cburnt * Tm0]
  balanceCBurnt$iter <- data$iter
  CcumBurnt <- balanceCBurnt[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumBurnt[, iter:=NULL]
  
  # calculate CcumAvoidFuel
  balanceCAvoid <- dataBiomassP[,-CavoidFuel * Tm0]
  balanceCAvoid$iter <- data$iter
  CcumAvoidFuel <- balanceCAvoid[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumAvoidFuel[, iter:=NULL]
  
  # calculate CcumOpBioPlant 
  balanceCop<- dataBiomassP[,CopBiomass * Tm0]
  balanceCop$iter <- data$iter
  CcumOpBioPlant <- balanceCop[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  CcumOpBioPlant[, iter:=NULL]
  
  return(list(CcumBurnt = CcumBurnt,
              CcumAvoidFuel = CcumAvoidFuel,
              CcumOpBioPlant = CcumOpBioPlant))
  
}




