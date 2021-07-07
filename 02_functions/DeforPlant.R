#### Deforestation for 1st plantation

DeforPlant <- function(dataPlantation, CoeffBOdefor, CoeffBEdefor, Ccontent,
                       defor = TRUE, # do we need to deforest to plant ? default yes
                       determ = FALSE, # for sensitivity test of DeforPlant(if TRUE)
                       determDecay=FALSE) {# for sensitivity test of DecayResid(if TRUE)
  
  if (determ==TRUE) {
    CoeffBOdefor <- CoeffBOdefor[lp__==max(lp__),]
  }
  
  ### For the 1st plantation cyle (ie year when there is deforestation)
    dataPlantationFirst <- dataPlantation[PlantCycle==1,]
  
    # if there is deforestation to plant: camculate Csawmill, CFuel, CLWN and CFWN
    if (defor == TRUE) {
    
      # add RTimb, the prop of AGB0 that goes to sawmill when deforestation
      if (determ==FALSE) {
        dataPlantationFirst$RTimb <- rbeta(dim(dataPlantationFirst)[1],  
                                           CoeffBOdefor$alpha, CoeffBOdefor$beta)
      }
      if (determ==TRUE) { # for test sensitivity, take the mode
        if(unique(CoeffBOdefor$alpha) <=1) {stop("function DeforPlant: for sensitivity analysis, alpha should be >1")}
        if(unique(CoeffBOdefor$beta) <=1) {stop("function DeforPlant: for sensitivity analysis, beta should be >1")}
        dataPlantationFirst$RTimb <- CoeffBOdefor[, (alpha-1)/(alpha+beta-2)]
      }
      # calculate Csawmill
      dataPlantationFirst$Csawmill <- dataPlantationFirst[, AGB0 * RTimb * Ccontent]
      
      # add RFuel, the prop of AGB0 that goes to biomass plant when deforestation
      if (determ==FALSE) {
        dataPlantationFirst$RFuel <- rbeta(dim(dataPlantationFirst)[1], 
                                           CoeffBEdefor$alpha , CoeffBEdefor$beta)
      }
      if (determ==TRUE) { # for test sensitivity, take the mode
        dataPlantationFirst$RFuel <- CoeffBEdefor[, (alpha-1)/(alpha+beta-2)]
      }
      # calculate Cfuel 
      dataPlantationFirst$Cfuel <- dataPlantationFirst[, AGB0 * RFuel * Ccontent]
      
      # add fLWN, the prop of LWN in the wood left to decay
      if (determDecay==FALSE) {
        dataPlantationFirst$fLWN <- rbeta(dim(dataPlantationFirst)[1], 1115,169)
      }
      if (determDecay==TRUE) {
        dataPlantationFirst$fLWN <- (1115-1)/(1115+169-2)
      }
      
      # calculate CFWN
      dataPlantationFirst$CFWN <- dataPlantationFirst[, AGB0 * (1-RTimb-RFuel) *
                                                        (1-fLWN) * Ccontent]  
      # calculate CLWN
      dataPlantationFirst$CLWN <- dataPlantationFirst[, AGB0 * (1-RTimb-RFuel) *
                                                        fLWN * Ccontent]
      
      # remove colums with R
      dataPlantationFirst[, c("RTimb", "RFuel", "fLWN") := NULL]
    }
  
  # if there is No deforestation to plant: Csawmill, CFuel, CLWN and CFWN are all equal to 0
    if (defor == FALSE) {
      dataPlantationFirst <- cbind(dataPlantationFirst,
                                   data.table(Csawmill = rep(0, dim(dataPlantationFirst)[1]),
                                              Cfuel =0 ,
                                              CFWN =0, CLWN=0))
    }

  
  ### For all the other years, nothing happens (the harvest will be added in another function)
  if (length(dataPlantation[,unique(PlantCycle)])>1) {
    dataPlantationLater <- dataPlantation[PlantCycle!=1,]
    dataPlantationLater <- cbind(dataPlantationLater,
                                 data.table(Csawmill = rep(0, dim(dataPlantationLater)[1]),
                                            Cfuel =0 ,
                                            CFWN =0, CLWN=0))
  }

  # bind 
  if (length(dataPlantation[,unique(PlantCycle)])==1) {
    dataPlantation <- dataPlantationFirst
  }
  if (length(dataPlantation[,unique(PlantCycle)])>1) {
    dataPlantation <- rbind(dataPlantationFirst, dataPlantationLater)
  }
                               
  # return
  return(dataPlantation)
 
}
