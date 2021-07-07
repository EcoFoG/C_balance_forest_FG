### Function DecayResid - cumulative balance of C from decay of deadwood leaft on site
################################################################

# FOR NATURAL FOREST
# This function give the cumulative C balance due to deadwood decay accross all logging unit 
# as a data.table with row = iteration and colum = year of simulation
# Cdecay:  decaying wood comes from damage, (Cdamresid) and  logging road, trail, decks (CDeforResid) 
# (given per ha dans dataNatFor)


DecayResid <- function (data, type, CoeffDecayLWB, Tm,
                        determ = FALSE) { # for sensitivity test (if TRUE)
  
  if(dim(data)[1] != dim(Tm)[1]) {
    stop("data and Tm must have the same number of rows")} 
  
  # get dataDecay for plantation
  if (type=="Plant") {
    dataDecay <- data.table(iter = data$iter)
    dataDecay$CLWN <- data[,CLWN*AreaPlant]
    dataDecay$CFWN <- data[,CFWN*AreaPlant]
  }
  
  # geta data decay for natural forest : get CLWN and CFWN per plot (for the whole area, not per ha) at the time of logging
  if (type=="NatFor") {
    dataDecay <- data.table(iter = data$iter,
                            Area = data$AreaLogged,
                            Cdecayha = data[,Cdamresid + CDeforResid])
    # get fLWN
    if (determ==FALSE) {
      dataDecay$fLWN <- rbeta(length(data$iter), 1115,169)
    }
    if (determ==TRUE){
      dataDecay$fLWN <- (1115-1)/(1115+169-2)
    }
    dataDecay$CLWN <- dataDecay[, fLWN * Cdecayha * Area] # per plot (not per ha anymore)
    dataDecay$CFWN <- dataDecay[, (1-fLWN) * Cdecayha * Area] # per plot (not per ha anymore)
  }
  
  # for test sensitivity, not possible to get all post at max likelihood because no likelihood
  # so I get the mode for each posterior distribution
  if (determ==TRUE) {
    CoeffDecayLWB$lambda1 <- modeest::mlv(CoeffDecayLWB$lambda1, method = "Parzen") 
    CoeffDecayLWB$lambda2 <- modeest::mlv(CoeffDecayLWB$lambda2, method = "Parzen") 
    CoeffDecayLWB$sd <- 0
    CoeffDecayLWB$p1 <- modeest::mlv(CoeffDecayLWB$p1, method = "Parzen") 
    CoeffDecayLWB <- unique(CoeffDecayLWB)
  }
  
  # get a set of parameter for each plot and each iterations
  paramDecay <- CoeffDecayLWB[sample(.N, length(data$iter), replace=TRUE)] # (p1 is Pi1)
  #paramDecay$row <- 1:length(data$iter) # to be able to draw epsilon per plot
  if (determ==FALSE) {
    paramDecay$lambdaFWN <- rnorm(length(data$iter), mean=0.19, sd=0.026)
  }
  if (determ==TRUE) {
    paramDecay$lambdaFWN <- 0.19
  }
  
  
  # merge 
  dataDecay <- data.table(dataDecay, paramDecay)
  
  # calculate the cumulative fluxes per each plot (in t) (for the whole area, not per ha) 
  # emission so positive
  balanceDecay <-  dataDecay[, CFWN * (1-exp(-lambdaFWN * Tm)) +    # for FWN
                               CLWN * (1-(p1 * exp(-lambda1 * Tm) +  # for LWN (part 1)
                                      (1-p1) * exp(-lambda2 * Tm)))] # + # for LWN (part 2)
                                      #epsilon))] # on LWN
  ## no epsilon because huge number of plots and additive, independent error: 
  # sum(mean+error)=sum(mean)+0, it doesn't change the results overall
  
  balanceDecay$iter <- data$iter
  
  # calculate the total across plots (in t) for each iteration
  Cdecay <- balanceDecay[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  Cdecay[, iter:=NULL]
  
  return(Cdecay)
  
}  
  

