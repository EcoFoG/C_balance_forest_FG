### Function Run1PlotNatFor
##############################################

# based on RunScenario but oly for 1 plot logged in the first year
# for natural forest

Run1PlotNatFor <- function (scenario, nrun, Tsimu, 
                            methodRecov, NewLogIntScenar=NULL, Efuelcollect = NULL,
                            Esawmill = 0.3, # efficiency of sawmill
                            RsawnAlt = 0.4, # proportion of sawnwood used instead of another material (
                            EbiomassPlant = 0.22, # efficiency of biomass plant
                            determ = NULL) {  # for sensitivity analysis (can be a vector)
                        
  if(!methodRecov%in%c(1,2)) {
    stop("The method for recovery should be 1 or 2")}
  
  ### Get data for scenario #########################################################
  if (!is.null(NewLogIntScenar)) {
    LogIntScenar <- NewLogIntScenar
  } else {
    LogIntScenar <- dataScenario[Scenario == scenario, LogInt]}
  FuelNatFor <- dataScenario[Scenario == scenario, FuelNatFor]
  RIL <- dataScenario[Scenario == scenario, RIL]
  
  ### Preliminary calculations ####################################################
  # Change FuelRoadUnit if there is no use of logging byproduct as fuelwood
  if(FuelNatFor!=TRUE) {FuelRoadUnit <- 0} 
  # get one value for diplacement factor DF for each run (same value for NatFor and Plant for all plots and all year of a simulation)
  DFtable <- data.table(iter = 1:nrun, 
                        DF = rnorm(n= nrun, mean = CoeffDF[,mu], 
                                   sd = CoeffDF[,sigma])) 
  # half-life of sawnwood (same value for NatFor and Plant for all plots and all year of a simulation)
  t05 <- data.table(iter = 1:nrun, 
                    t05 = rtruncnorm(n= nrun, a=0, mean = 35, sd=15))
  # conso of sawmill (same value for NatFor and Plant for all plots and all year of a simulation)
  Conso <- data.table(iter = 1:nrun, 
                      Conso = runif(n= nrun, min=35.2, max=102.4))
  # get the coeffDam2Fuel for this scenario
  if(RIL==FALSE) {CoeffDam2Fuel <- CoeffDam2FuelCL} # for conventional logging
  if(RIL==TRUE) {CoeffDam2Fuel <- CoeffDam2FuelRIL} # for RIL
  # get the coeff for recovery model, depending on the method
  if(methodRecov==1) {CoeffRecov <- CoeffRecov_m1}
  if(methodRecov==2) {CoeffRecov <- CoeffRecov_m2}
  ###################################################################################
  
  
  

  ### NATURAL FOREST ####################################################################

    # create a data set with just one test plot 
    dataNatFor <- data.table(iter=1:nrun,
                             ID_LogUnit="Test",
                             AGB0=median(dataLogUnit$AGB),
                             MAP=median(dataLogUnit$MAP),
                             RainSeas=median(dataLogUnit$RainSeas),
                             BulkDens=median(dataLogUnit$BulkDens),
                             AreaLogUnit=1,
                             t0=1,
                             AreaLogged=1,
                             LogInt=LogIntScenar)
    
    # run Damage => calculate the logging damage
    if (!("Damage" %in% determ)){
      dataNatFor <- cbind(dataNatFor, 
                          Damage(LogInt=dataNatFor$LogInt, WDtimber=WDtimber, Ccontent=Ccontent, 
                                 ACS0=dataNatFor$AGB0 * Ccontent, CoeffDam=CoeffDam,
                                 CoeffDam2Fuel=CoeffDam2Fuel, RIL=RIL, CoeffRIL = CoeffRIL,
                                 FuelNatFor=FuelNatFor, type="NatFor", Efuelcollect=Efuelcollect)) 
    }
    if ("Damage" %in% determ){ # for sensitivity analysis
      dataNatFor <- cbind(dataNatFor, 
                          Damage(LogInt=dataNatFor$LogInt, WDtimber=WDtimber, Ccontent=Ccontent, 
                                 ACS0=dataNatFor$AGB0 * Ccontent, CoeffDam=CoeffDam,
                                 CoeffDam2Fuel=CoeffDam2Fuel, RIL=RIL, CoeffRIL = CoeffRIL,
                                 FuelNatFor=FuelNatFor, type="NatFor", Efuelcollect=Efuelcollect,
                                 determ = TRUE))    
    }

    
    # Run DeforNatFor => calculate data on logging road, main trail and deck
    if (!("DeforNatFor" %in% determ)){
      dataNatFor <- cbind(dataNatFor, 
                          DeforNatFor(LogInt=dataNatFor$LogInt, CdamFuel=dataNatFor$Cdamfuel,
                                      ACS0=dataNatFor$AGB0*Ccontent, CoeffRroad=CoeffRroad,
                                      CoeffWroad=CoeffWroad, WDfuel=WDfuel,
                                      FuelRoadUnit=FuelRoadUnit, H=H, type="NatFor")) 
    }
    if ("DeforNatFor" %in% determ){ # for sensitivity analysis
      dataNatFor <- cbind(dataNatFor, 
                          DeforNatFor(LogInt=dataNatFor$LogInt, CdamFuel=dataNatFor$Cdamfuel,
                                      ACS0=dataNatFor$AGB0*Ccontent, CoeffRroad=CoeffRroad,
                                      CoeffWroad=CoeffWroad, WDfuel=WDfuel,
                                      FuelRoadUnit=FuelRoadUnit, H=H, type="NatFor",
                                      determ=TRUE)) 
    }
  

    
    # Run LogAct => C of all emissions from logging activities 
    dataNatFor <- cbind(dataNatFor, 
                        LogAct(EmisPlanning=EmisPlanning, EmisRoadBuild=EmisRoadBuild, 
                               EmisLogging=EmisLogging, EmisTransport=EmisTransport, 
                               EmisLoading=EmisLoading, Lroad= dataNatFor$Lroad, 
                               Cdamfuel=dataNatFor$Cdamfuel, DistTimbNatFor=DistTimbNatFor,
                               DistFuelNatFor=DistFuelNatFor, H=H, Ccontent=Ccontent, 
                               FuelRoadUnit=FuelRoadUnit, WDtimber=WDtimber , LogInt=dataNatFor$LogInt,
                               type="NatFor")) 
  ###################################################################################
  
  

  ### Get cumulative fluxes for natural forest ##########################################
    # get a time matrix since logging for every plot and every iteration
    # plots * interations = row , year = colums
    time <- 1:Tsimu
    TmNatFor <- data.table(t(sapply(dataNatFor$t0, function(j) {time-j})))
    colnames(TmNatFor) <- as.character(time)
    TmNatFor <- TmNatFor[, replace(.SD, .SD < 0, NA)]
    
    # get a time matrix with 1 for the year of logging and all years after and NA otherwise
    Tm0NatFor <- as.data.table(matrix(dataNatFor$t0 , length(dataNatFor$t0) , length(time))) # repeat the same columns
    colnames(Tm0NatFor) <- as.character(time)
    # Na if the t0 for the row is less than the columns name
    for(col in 1:length(time)) {set(Tm0NatFor, i=which(Tm0NatFor[[col]]>col), j=col, value=NA)}
    # 1 if it's more
    for(col in 1:length(time)) {set(Tm0NatFor, i=which(Tm0NatFor[[col]]<=col) , j=col, value=1)}
    
    # Run the function giving the C fluxes cumulated for all plots
    # each function return a data.table of C fluxes per year (or a list when several type of fluxes)
    # each function is run per iteration so we get for each flux
    # a data.table with each row = an iterations and each colum = a year
    
    # Run Recovery : returns a dataframe
    if (!("Recovery" %in% determ)){
      CcumNFrecov <- Recovery(method=methodRecov,
                              data=dataNatFor,
                              CoeffRecov=CoeffRecov,
                              standardiseRecov=standardiseRecov, 
                              Ccontent=Ccontent,
                              Tm=TmNatFor)
    }
    if ("Recovery" %in% determ){
      CcumNFrecov <- Recovery(method=methodRecov,
                              data=dataNatFor,
                              CoeffRecov=CoeffRecov,
                              standardiseRecov=standardiseRecov, 
                              Ccontent=Ccontent,
                              Tm=TmNatFor, determ=TRUE)
    }
    
    # Run Regeneration : returns a dataframe
    if (!("Regeneration" %in% determ)){
      CcumNFrege <- Regeneration(data=dataNatFor,
                                 CoeffRege=CoeffRege, 
                                 Ccontent=Ccontent,
                                 Tm=TmNatFor)
    }
    if ("Regeneration" %in% determ){
      CcumNFrege <- Regeneration(data=dataNatFor,
                                 CoeffRege=CoeffRege, 
                                 Ccontent=Ccontent,
                                 Tm=TmNatFor, determ=TRUE)
    }
    
    
    # Run Sawmill : returns a list
    CcumNFsawmill <- Sawmill(data = dataNatFor, type = "NatFor",
                             Esawmill = Esawmill, DFtable=DFtable, t05=t05, Conso=Conso,
                             RsawnAlt = RsawnAlt, Rwaste = Rwaste,
                             nrun=nrun, Tm0=Tm0NatFor, Tm=TmNatFor)
    dataNatFor$CSawmillFuel <- CcumNFsawmill$CSawmillFuel # add the quantity of C from sawdust going to the biomass plant
    CcumNFsawmill$CSawmillFuel <- NULL # remove it from the list as not needed
    names(CcumNFsawmill) <- c("CcumNFSawnAlt", "CcumNFsawnwood", 
                              "CcumNFdustWaste", "CcumNFSawmillNoAvoid") # renames to avoid same names than in Plantation
    
    # Run BiomassPlant : returns a list
    CcumNFbiomassP <- BiomassPlant(data=dataNatFor, type="NatFor", PCIhNatFor=PCIhNatFor, 
                                   H=H , EbiomassPlant=EbiomassPlant, 
                                   Cfossil=Cfossil, Tm0=Tm0NatFor)
    names(CcumNFbiomassP) <- c("CcumNFburnt" , "CcumNFavoidFuel" , "CcumNFopBioPlant") # renames to avoid same names than in Plantation
    
    # Run ActCum : returns a list
    CcumNFAct <- ActCum(data=dataNatFor, type="NatFor", 
                        DistMillBioPlant=DistMillBioPlant, Ccontent=Ccontent, Tm0=Tm0NatFor)
    names(CcumNFAct) <- c("CcumNFotherAct","CcumNFtrans") # renames to avoid same names than in Plantation
    
    # Run Deadwood decay : returns a dataframe
    if (!("DecayResid" %in% determ)){
    CcumNFdecay <- DecayResid(data=dataNatFor, type="NatFor",
                              CoeffDecayLWB=CoeffDecayLWB, Tm=TmNatFor)
    }
    if ("DecayResid" %in% determ){
      CcumNFdecay <- DecayResid(data=dataNatFor, type="NatFor",
                                CoeffDecayLWB=CoeffDecayLWB, Tm=TmNatFor, determ=TRUE)
    }
    
  ###################################################################################
  
  
  ### bind all data in a list and return ############################################
  # if only natural forest in the scenario
    outputScenario <- list(dataNatFor = dataNatFor,
                           CcumNFrecov = CcumNFrecov, 
                           CcumNFrege = CcumNFrege, 
                           CcumNFsawmill = CcumNFsawmill, 
                           CcumNFbiomassP = CcumNFbiomassP, 
                           CcumNFAct = CcumNFAct,
                           CcumNFdecay = CcumNFdecay)

  
  return(outputScenario)
}
