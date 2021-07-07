### Sub-model of logging damage in natural forest
#################################

# This function gives 
# DisturbInt : the disturbance intensity = AGBloss/AGB0 = ACSloss/ACS0 (avec AGBloss = logged + damage)
# Cdamfuel : the C due to damage that is used as fuelwood (t/ha)
# Cdamresid : the C due to damage that is unused and left on site (t/ha)
# from
# AGBext : the biomass extracted by logging (= LogInt * WDtimber)
# and AGB0: the initial biomass before logging (= ACS0 / Ccontent)


# ACS0 is a vector for all plot
Damage <- function (LogInt, WDtimber, Ccontent, ACS0, CoeffDam, CoeffDam2Fuel, 
                    RIL, CoeffRIL, FuelNatFor, 
                    type, # NatFor or PastLog
                    Efuelcollect, # possibility to increase/decrease 
                    # the efficiency of fuelwood collection (eg Efuelcollect= 2 => double the efficiency of fuelwood collection)
                    determ = FALSE) { # for sensitivity test (if TRUE)
  
  if (!(type %in% c("NatFor", "PastLog"))) {
    stop("function Damage: The type should be either NatFor or PastLog")}
  
  if(!is.null(Efuelcollect)) {
    if (Efuelcollect<0) {stop("function Damage: Efuelcollect must be positive")}
  }
  
  Ratioext <-  LogInt * WDtimber * Ccontent/ACS0 # AGBext/AGB0 = ACSext/ACS
  if(min(Ratioext) <=0 | min(Ratioext) > 1) {stop("function Damage: ACSext/ACS0 should be strickly positive and less than 1")} # TEMPORARY to check pb sampling in beta
  
  # for test sensitivity, get all post at max likelihood
  if (determ==TRUE) {
    CoeffDam <- CoeffDam[lp__==max(lp__),]
  }

  # Get coefficients alphaDam et betDam
  CoeffGP <- CoeffDam[sample(.N, length(ACS0), replace=TRUE), c("gamma", "phi")] # draw a set of coef for each plot and iteration
  alphaDam <- Ratioext^CoeffGP[,gamma] * CoeffGP[,phi] # because phi =(1/C)-1
  if(min(alphaDam)<=0) {stop("function Damage: alphaDam shouldn't be negative")}
  betaDam <- (1 - Ratioext^CoeffGP[,gamma]) * CoeffGP[,phi] # because phi =(1/C)-1
  if(min(betaDam)<=0) {stop("function Damage: betaDam shouldn't be negative")} 
  # create empty vector to store result of DisturbInt
  RatioDam <-vector(length=length(ACS0)) # ACsdam/(ACS0-ACSext)
  # for each plot, get RatioDam
  if (determ==FALSE) {
    for (i in 1:length(ACS0)) {
      RatioDam[i] <- rbeta(1, shape1=alphaDam[i], shape2=betaDam[i]) # sample in beta distrib submodel
    }
  }
  if (determ==TRUE) { # for test sensitivity, take the mode
    if(unique(alphaDam) <=1) {stop("function Damage: for sensitivity analysis, alphaDam should be >1")}
    if(unique(betaDam) <=1) {stop("function Damage: for sensitivity analysis, betaDam should be >1")}
    for (i in 1:length(ACS0)) {
    RatioDam[i] <- (alphaDam[i]-1) / (alphaDam[i] + betaDam[i] -2)
    }
  }

  
  # Calculate ACS of all damage (fuelwood and residual)
  ACSdamAll <- RatioDam*(ACS0 - (LogInt * WDtimber * Ccontent)) # LogInt * WDtimber * Ccontent is ACSext
  
  # if RIL, reduced the damage
  if(RIL==TRUE) {
    # get a ratio of reduction of damage Rreduc for each plot and run
    Rreduc <- rbeta(length(ACSdamAll), shape1=CoeffRIL$alpha, shape2=CoeffRIL$beta)
    ACSdamAll <- ACSdamAll * (1-Rreduc)
  }
  
  # Calculate the amount of C in fuelwood from damage (if any is taken)
  if(FuelNatFor==TRUE & type=="NatFor") {
    RDam2Fuel <- rbeta(length(ACSdamAll), 
                       CoeffDam2Fuel$alpha, 
                       CoeffDam2Fuel$beta)
    if (!is.null(Efuelcollect)) { # increase/decrease the efficiency of fuelwood collection
      RDam2Fuel <- RDam2Fuel * Efuelcollect
      RDam2Fuel[which(RDam2Fuel>1)] <- 1 # to avoid trying to take more than the available
    }
    Cdamfuel <- RDam2Fuel * ACSdamAll
  } else {
    Cdamfuel <- 0
  }

  return(data.table(DisturbInt = (ACSdamAll +  (LogInt * WDtimber * Ccontent)) / ACS0,  # (ACSdamall + ACSext) / ACS0
                    Cdamfuel = Cdamfuel,
                    Cdamresid = ACSdamAll - Cdamfuel)) # amount of C in damage that is residual (ie left on site)
} 














