### Function PlantGrowthCum - cumulative fluxes of C due to growth of plantation
######################################################################################

# This function give the cumulative C balance accross all plot
# as a data.table with row = iteration and colum = year of simulation

PlantGrowthCum <- function(dataPlantation, Tm0Plant) {
  # data for each plot and year (whole plot NOT per ha)
  dataPlantGrowth <- data.table(iter = dataPlantation$iter,
                                CgrowthP = - dataPlantation[,Cgrowth * AreaPlant])
  # calculate CcumPgrowth
  balanceCgrowth <- dataPlantGrowth[, CgrowthP * Tm0Plant]
  balanceCgrowth$iter <- dataPlantGrowth$iter
  CcumPgrowth <- balanceCgrowth[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by=.(iter)]
  CcumPgrowth[, iter:=NULL]
  
  return(CcumPgrowth)
}


