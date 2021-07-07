##### Trajectory for all plantations
###################################################

# Run PlantTraj for all plantation * iteration

PlantTrajAll <- function(dataPlantation, PlantDuration,
                         ChangeProd, # change Productivity if wanted
                         determ = FALSE){ # for sensitivity test of PlantTraj(if TRUE)  
  
  if(!is.null(ChangeProd)) {
    if (ChangeProd<0) {stop("function PlantTraj: ChangeProd must be positive")}
  }

  # Run PlantTraj as many times as there are rows (iter * plot) in dataPlantation
  if (determ==FALSE) {
    TrajAll <- replicate(dim(dataPlantation)[1],
                         PlantTraj(PlantDuration, NThin, AGBendPlant, gammaPlantTraj, Rprod, RLWN, Tfuel, T10,
                                   ChangeProd=ChangeProd), 
                         simplify=FALSE) 
  }
  if (determ == TRUE) {
    TrajAll <- replicate(dim(dataPlantation)[1],
                         PlantTraj(PlantDuration, NThin, AGBendPlant, gammaPlantTraj, Rprod, RLWN, Tfuel, T10,
                                   ChangeProd=ChangeProd, determ = TRUE), 
                         simplify=FALSE)
  }
  # rbindlist all the trajectories
  TrajAll <- rbindlist(TrajAll, use.names = TRUE)
  # add colums with info on plots
  dataPlot <- dataPlantation[,.(iter, ID_Plant, AreaPlant, AGB0, YearLogged, AreaTot, AreaLogged, code_parcOLD, t0, 
                                PlantCycle, tPlant, WDPlant,PCIhPlant)]
  TrajAll <- cbind(dataPlot[rep(seq_len(nrow(dataPlot)), each=PlantDuration),], TrajAll)
  # remove uneeded colums
  TrajAll[ ,c("Cmortality", "Cthin", "Calive") := NULL]
  # change t0
  TrajAll[, t0:=tPlant+trelat] # year of the line = year of planting + year relative to year of planting
  # remove year after end of simulation
  TrajAll <- TrajAll[t0<=Tsimu]
  # rbind with dataPlantation (containing only row for years of plantation)
  dataPlantation$Cgrowth <- 0
  dataPlantation <- rbind(dataPlantation, TrajAll)
  setorder(dataPlantation, iter, tPlant, ID_Plant)
  
  # for year of plantation, all the wood harvested for timber or fulewood comes from natural forest (the year of first plantation, the other year, no wood)
  # so we use the PCI and WD of natural forest
  dataPlantation[YearType=="P", WDPlant:=WDfuel]
  dataPlantation[YearType=="P", PCIhPlant:=PCIhNatFor]
  
  return(dataPlantation)
}    
    
    
    

    
  
  
  



# library(microbenchmark)
# 
# microbenchmark(replicate(500,PlantTraj(), simplify=FALSE), times=1) # environ 55s for 500 times
# 
# microbenchmark(for (i in 1:500) {
#   vRow[[i]] <- list(PlantTraj())
# }, times=1)    # environ 55s for 500 times

