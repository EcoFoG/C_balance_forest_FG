#### Function GetResultsFlux.R
##########################################

# This function calculate the total (for all plots) fluxes (cumulative or annual)
# and gives the median and CI
# and save them

# NB: it very similar to the function graphFlux

#### arguments:
# outputScenario: output data from function runScenario
# type: either "cumulative" or "annual"
# StartYr: year when to start (in calendar year)
# NewConnectPast (for when we want to overwrite the ConnectPast of the scenario)
# CI: vector of low and high CI (eg: c(0.025, 0.975))

GetResultFlux <- function(outputScenario, type, scenario=NULL, nrun, 
                          StartYr=NULL, NewConnectPast=NULL, CI) {
  
  # get connect past for the scenario 
  if (!is.null(NewConnectPast)) {
    ConnectPast <- NewConnectPast
  } else {
    if (!is.null(scenario)) {
      ConnectPast <- dataScenario[Scenario == scenario, ConnectPast]
    } else {
      ConnectPast <- FALSE
    }
  }
  
  # we cannot have a StartYr when ConnectPast=TRUE
  if (!is.null(StartYr) & ConnectPast==TRUE) {
    stop("You cannot give a StartYear if ConnectPast is TRUE")}
  
  ### Calculate agregated fluxes 
  # (still in data.table with all iteration, 
  # with a first colum being the flux source (Net, NetPlant or NetNF))
  # and a second being the Flux type
  ###########################################################################
  list2env(outputScenario,.GlobalEnv) # copy all the elements of the list outputScenario into the global environement
  
  
  # get a data.table with only 0 to store result for total fluxes
  if (ConnectPast == TRUE) {
    CcumNET <- as.data.table(matrix( rep( 0, len=nrun * dim(CcumPastrege)[2]), nrow = nrun))
    colnames(CcumNET) <-colnames(CcumPastrege)
    CcumPrevious <- CcumNET[, 1:(dim(CcumPastrege)[2]-Tsimu), with=TRUE] # past years, only 0
  } else {
    CcumNET <- as.data.table(matrix( rep( 0, len=nrun * Tsimu), nrow = nrun))
    colnames(CcumNET) <- as.character(1:Tsimu)
  }
  # make a list to store all the fluxes we need
  AllFlux <- list(CcumNET = CcumNET)
  
  # run only if there is some logging in natural forest
  if ("dataNatFor" %in% names(outputScenario)) {
    CcumNFallSawmill <- CcumNFsawmill$CcumNFSawnAlt + CcumNFsawmill$CcumNFsawnwood + CcumNFsawmill$CcumNFdustWaste # net fluxes in Sawmill from NF
    CcumNFSawmillAvoid <-  CcumNFallSawmill - CcumNFsawmill$CcumNFSawmillNoAvoid
    CcumNFallBiomass <- CcumNFbiomassP$CcumNFburnt + CcumNFbiomassP$CcumNFavoidFuel + CcumNFbiomassP$CcumNFopBioPlant # net flux from biomassplant in NF
    CcumNFallAct <- CcumNFAct$CcumNFotherAct + CcumNFAct$CcumNFtrans # net fluxes for all act in NatFor 
    CcumNFall <- CcumNFrecov + CcumNFrege + CcumNFallSawmill + CcumNFallBiomass + CcumNFallAct + CcumNFdecay
    if (ConnectPast == TRUE) {
      CcumNFallLong <- cbind(CcumPrevious, CcumNFall)
      AllFlux$CcumNET <- AllFlux$CcumNET + CcumNFallLong
    } else {
      AllFlux$CcumNET <- AllFlux$CcumNET + CcumNFall
    }
    AllFlux <-c(AllFlux,  CcumNFall= list(CcumNFall), CcumNFrecov = list(CcumNFrecov), CcumNFrege= list(CcumNFrege),
                CcumNFallSawmill=list(CcumNFallSawmill), CcumNFSawnAlt=list(CcumNFsawmill$CcumNFSawnAlt),
                  CcumNFsawnwood=list(CcumNFsawmill$CcumNFsawnwood), CcumNFdustWaste=list(CcumNFsawmill$CcumNFdustWaste),
                  CcumNFSawmillAvoid=list(CcumNFSawmillAvoid), CcumNFSawmillNoAvoid = list(CcumNFsawmill$CcumNFSawmillNoAvoid),
                CcumNFallBiomass=list(CcumNFallBiomass), CcumNFburnt=list(CcumNFbiomassP$CcumNFburnt),
                  CcumNFavoidFuel=list(CcumNFbiomassP$CcumNFavoidFuel), CcumNFopBioPlant=list(CcumNFbiomassP$CcumNFopBioPlant),
                  CcumNFBiomassNoAvoid = list(CcumNFbiomassP$CcumNFburnt + CcumNFbiomassP$CcumNFopBioPlant),
                CcumNFallAct=list(CcumNFallAct),
                CcumNFdecay= list(CcumNFdecay))
  }
  
  # run only if plantations are implemented in the scenario
  if ("dataPlantation" %in% names(outputScenario)) {
    CcumPallSawmill <- CcumPsawmill$CcumPSawnAlt + CcumPsawmill$CcumPsawnwood + CcumPsawmill$CcumPdustWaste # net fluxes in Sawmill from Plantation
    CcumPSawmillAvoid <-  CcumPallSawmill - CcumPsawmill$CcumPSawmillNoAvoid
    CcumPallBiomass <- CcumPbiomassP$CcumPburnt + CcumPbiomassP$CcumPavoidFuel + CcumPbiomassP$CcumPopBioPlant# net flux from biomassplant in plantation
    CcumPallAct <- CcumPAct$CcumPotherAct + CcumPAct$CcumPtrans # net fluxes for all act in NatFor
    CcumPall <- CcumPgrowth + CcumPallSawmill + CcumPallBiomass + CcumPallAct + CcumPdecay
    if (ConnectPast == TRUE) {
      CcumPallLong <- cbind(CcumPrevious, CcumPall)
      AllFlux$CcumNET <- AllFlux$CcumNET + CcumPallLong
    } else {
      AllFlux$CcumNET <- AllFlux$CcumNET + CcumPall
    }
    AllFlux <-c(AllFlux, CcumPall=list(CcumPall), CcumPgrowth=list(CcumPgrowth), 
                CcumPallSawmill=list(CcumPallSawmill), CcumPSawnAlt=list(CcumPsawmill$CcumPSawnAlt),
                  CcumPsawnwood=list(CcumPsawmill$CcumPsawnwood), CcumPdustWaste=list(CcumPsawmill$CcumPdustWaste),
                  CcumPSawmillAvoid=list(CcumPSawmillAvoid), CcumPSawmillNoAvoid = list(CcumPsawmill$CcumPSawmillNoAvoid),
                CcumPallBiomass=list(CcumPallBiomass), CcumPburnt =list(CcumPbiomassP$CcumPburnt),
                  CcumPavoidFuel=list(CcumPbiomassP$CcumPavoidFuel), CcumPopBioPlant=list(CcumPbiomassP$CcumPopBioPlant),
                  CcumPBiomassNoAvoid = list(CcumPbiomassP$CcumPburnt + CcumPbiomassP$CcumPopBioPlant),
                CcumPallAct=list(CcumPallAct), CcumPdecay= list(CcumPdecay))
  }   
  
  # add past fluxes
  if (ConnectPast == TRUE) {
    CcumPastallSawmill <- CcumPastsawmill$CcumPastSawnAlt + CcumPastsawmill$CcumPastsawnwood +
      CcumPastsawmill$CcumPastdustWaste # net fluxes in Sawmill from Past logging
    CcumPastSawmillAvoid <-  CcumPastallSawmill - CcumPastsawmill$CcumPastSawmillNoAvoid
    CcumPastallBiomass <- CcumPastbiomassP$CcumPastburnt + CcumPastbiomassP$CcumPastavoidFuel + 
      CcumPastbiomassP$CcumPastopBioPlant # net flux from biomassplant in past logging
    CcumPastallAct <- CcumPastAct$CcumPastotherAct + CcumPastAct$CcumPasttrans # net fluxes for all act in Past
    CcumPastall <- CcumPastrecov + CcumPastrege + CcumPastallSawmill + 
      CcumPastallBiomass + CcumPastallAct + CcumPastdecay
    AllFlux$CcumNET <- AllFlux$CcumNET + CcumPastall
    AllFlux <-c(AllFlux,  CcumPastall= list(CcumPastall), CcumPastrecov = list(CcumPastrecov), 
                CcumPastrege= list(CcumPastrege),
                CcumPastallSawmill=list(CcumPastallSawmill), CcumPastSawnAlt=list(CcumPastsawmill$CcumPastSawnAlt),
                  CcumPastsawnwood=list(CcumPastsawmill$CcumPastsawnwood), CcumPastdustWaste=list(CcumPastsawmill$CcumPastdustWaste),
                  CcumPastSawmillAvoid=list(CcumPastSawmillAvoid), CcumPastSawmillNoAvoid = list(CcumPastsawmill$CcumPastSawmillNoAvoid),
                CcumPastallBiomass=list(CcumPastallBiomass), CcumPastburnt=list(CcumPastbiomassP$CcumPastburnt),
                  CcumPastavoidFuel=list(CcumPastbiomassP$CcumPastavoidFuel), CcumPastopBioPlant=list(CcumPastbiomassP$CcumPastopBioPlant),
                  CcumPastBiomassNoAvoid = list(CcumPastbiomassP$CcumPastburnt + CcumPastbiomassP$CcumPastopBioPlant),
                CcumPastallAct=list(CcumPastallAct),
                CcumPastdecay= list(CcumPastdecay))
  }
  
  
  ### Add Net flux without avoided
  #########################################################################
  CcumNetNoAvoid  <- AllFlux$CcumNET
  # substract all avoided fluxes
  if ("dataNatFor" %in% names(outputScenario)) {
    if (ConnectPast == TRUE){
      CcumNetNoAvoid <- CcumNetNoAvoid  - cbind(CcumPrevious, AllFlux$CcumNFSawmillAvoid) - 
        cbind(CcumPrevious, AllFlux$CcumNFavoidFuel)
    } else {
      CcumNetNoAvoid <- CcumNetNoAvoid  - AllFlux$CcumNFSawmillAvoid - AllFlux$CcumNFavoidFuel
    }
  }
  
  if ("dataPlantation" %in% names(outputScenario)) {
    if (ConnectPast == TRUE){
      CcumNetNoAvoid <- CcumNetNoAvoid  - cbind(CcumPrevious,AllFlux$CcumPSawmillAvoid) - 
        cbind(CcumPrevious,AllFlux$CcumPavoidFuel)
    } else {
      CcumNetNoAvoid <- CcumNetNoAvoid  - AllFlux$CcumPSawmillAvoid - AllFlux$CcumPavoidFuel
    }
  }
  
  if (ConnectPast == TRUE) {
    CcumNetNoAvoid <- CcumNetNoAvoid  - AllFlux$CcumPastSawmillAvoid - AllFlux$CcumPastavoidFuel
  }
  
  AllFlux <-c(AllFlux,  CcumNetNoAvoid= list(CcumNetNoAvoid))
  
  
  
  
  ### Bind them in one single long table AND CONVERT 
  #########################################################################
  if (type=="cumulative") {scaling <- 1000000} # to convert tons into Tg
  if (type=="annual") {scaling <- 1000} # to convert tons into Gg
  
  # put them all in long tables
  for (i in 1:length(AllFlux)) {
    AllFlux[[i]]$iter <- row.names(AllFlux[[i]])
    AllFlux[[i]] <- melt(AllFlux[[i]], id.vars= "iter", 
                         variable.name = "Yr", measure.vars = head(colnames(AllFlux[[i]]),-1),
                         value.name = "CumFlux")
    AllFlux[[i]]$FluxType <- names(AllFlux)[i]
    AllFlux[[i]]$CumFlux <- AllFlux[[i]]$CumFlux/scaling # convert
  }
  # bind them all together
  AllFlux <- rbindlist(AllFlux, use.names = TRUE)
  AllFlux$iter <- as.factor(AllFlux$iter)
  AllFlux$FluxType <- as.factor(AllFlux$FluxType)
  
  
  ### If we want annual fluxes, get the difference between years
  if (type=="annual") {
    AllFlux[,AnnuFlux := CumFlux - shift(CumFlux), by=.(iter, FluxType)]
    AllFlux[is.na(AnnuFlux), AnnuFlux:=CumFlux] # for 1st years
  }
  
  
  ### get median and CI
  # for cumulative
  if (type=="cumulative") {
    AllFluxSummary <- AllFlux[,.(Median=median(CumFlux), 
                                CIlow = quantile(CumFlux, probs = CI[1]),
                                CIhigh = quantile(CumFlux, probs = CI[2])),
                             by=.(Yr, FluxType)]
  }
  # for annual
  if (type=="annual") {
    AllFluxSummary <- AllFlux[,.(Median=median(AnnuFlux), 
                                CIlow = quantile(AnnuFlux, probs = CI[1]),
                                CIhigh = quantile(AnnuFlux, probs = CI[2])),
                             by=.(Yr, FluxType)]
  }
  
  # Add the absolute year
  if (!is.null(StartYr)) {
    AllFluxSummary$Yr <- AllFluxSummary$Yr + StartYr - 1
  }
  
  return(AllFluxSummary)
  
}

  