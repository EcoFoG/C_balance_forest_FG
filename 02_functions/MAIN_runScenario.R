## Function RunScenario ###########################################
########################################################################

# This function runs the scenario

###### arguments:
# scenario : name of the scenario to run
# nrun : number of run
# Tsimu : time overwhich to run the scenario
# outputFile : name of the output Rdata file

# This function start by building 2 data.table
# dataPlantation : one row per plot and run and year of plantation
# dataNatFor : one row per plot and run 
# then it runs the functions to get the cumulative fluxes for plantation and natural forest

RunScenario <- function (scenario, nrun, Tsimu, 
                         defor = TRUE, # do we need to deforest to plant ? default yes
                         methodRecov, Efuelcollect = NULL, 
                         PlantDurationExp = 30, PlantDurationObs=PlantDurationExp, # expected and real duration of plantation
                         ChangeProdExp = NULL, ChangeProdObs = ChangeProdExp, # expected and observed change of productivity
                         Esawmill = 0.3, # efficiency of sawmill
                         RsawnAlt = 0.4, # proportion of sawnwood used instead of another material (
                         EbiomassPlant = 0.22) { # efficiency of biomass plant
 
  if(!methodRecov%in%c(1,2)) {
    stop("The method for recovery should be 1 or 2")}
  
  ### Get data for scenario #########################################################
  VolTimb <- dataScenario[Scenario == scenario, VolTimb]
  LogIntScenar <- dataScenario[Scenario == scenario, LogInt]
  VolTimbPlant <- dataScenario[Scenario == scenario, VolTimbPlant]
  FuelNatFor <- dataScenario[Scenario == scenario, FuelNatFor]
  ConnectPast <- dataScenario[Scenario == scenario, ConnectPast]
  RIL <- dataScenario[Scenario == scenario, RIL]
  ###################################################################################
  
  
  
  ### Preliminary calculations ####################################################
  # Change FuelRoadUnit if there is no use of logging byproduct as fuelwood
    if(FuelNatFor!=TRUE) {FuelRoadUnit <- 0} 
  # get one value for diplacement factor DF for each run (same value for NatFor and Plant for all plots and all year of a simulation)
    DFtable <- data.table(iter = 1:nrun, 
                          DF = rnorm(n= nrun, mean = CoeffDF[,mu], 
                                     sd = CoeffDF[,sigma])) 
  # half-life of sawnwood (same value for NatFor and Plant for all plots and all year of a simulation)
    t05 <- data.table(iter = 1:nrun, 
                      t05 = rtruncnorm(n= nrun, mean = 35, sd=15, a=0))
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
  
  
  
  ### get dataPlantation ####################################################################
  # run only if VolTimbPlant is not zero (meaning that plantations are implemented in the scenario)
  if (VolTimbPlant!=0) {
    # run PlantPlan => plan the plantation
    dataPlantation <- PlantPlan(nrun = nrun, 
                                PlantDuration= PlantDurationExp, Ccontent = Ccontent,
                                dataPlotforPlant= dataPlotforPlant,
                                Tsimu =Tsimu, VolTimbPlant=VolTimbPlant, 
                                ChangeProd=ChangeProdExp, RCub=RCub)
    # NB at this stage, only one row per plantation and per iteration
    # all row are for the plantation year
    
    # there are 3 types of time in dataPlantation
    # t0 is the time of the event (as described in YearType, can be planting, or other) in absolute time
    # tPlant : year of plantation (in absolute time)
    # trelat : time of the event in the relative time of the plantation (so trelat = 0 in the year of plantation)
    
    # run DeForPlant => use the wood from deforestation for plantation
    dataPlantation <- DeforPlant(dataPlantation = dataPlantation, 
                                 defor = defor, 
                                 CoeffBOdefor = CoeffBOdefor, 
                                 CoeffBEdefor = CoeffBEdefor, 
                                 Ccontent = Ccontent) 
    
    # run PlantTrajAll (uses PlantTraj) to get trajectories of all plantation * iterations
    dataPlantation <- PlantTrajAll(dataPlantation, PlantDuration=PlantDurationObs,
                                   ChangeProd=ChangeProdObs)
    
    # run PlantAct to get the emission (per ha) due to plantation activities and wood transport
    dataPlantation <- PlantAct(dataPlantation=dataPlantation, 
                               defor = defor, 
                               EmisPlanPlant=EmisPlanPlant,
                               EmisDefor=EmisDefor, EmisTransport=EmisTransport, 
                               EmisLoading=EmisLoading, EmisStack=EmisStack, 
                               EmisPrepPlant=EmisPrepPlant, EmisPlantClean=EmisPlantClean, 
                               DistTimbPlant=DistTimbPlant, DistFuelPlant=DistFuelPlant)
    
    # Calculate VolP: the volume actually produced in plantation per year (column) and run (row)
    VolP <- dataPlantation[, .(VolPlant=sum(Csawmill * AreaPlant /(Ccontent * WDPlant))), 
                           by=.(iter, t0)]
    
  } else {
    VolP <- data.table(iter = rep(1:nrun, each=Tsimu),
                       t0 = rep(1:Tsimu, nrun),
                       VolPlant = 0) # VolP = 0 if no plantation
  }
  ###################################################################################
  
  
  ### NATURAL FOREST ####################################################################
  # run only if LogIntScenar is not zero (meaning that there is some logging in natural forest)
  if (LogIntScenar!=0) {
    # run LoggingPlan =>  create a logging plan
    dataNatFor <- LoggingPlan(nrun = nrun, VolP=VolP, VolTimb=VolTimb, LogInt=LogIntScenar, 
                              Tsimu=Tsimu, dataLogUnit=dataLogUnit, 
                              alpha_LoggingPlan=alpha_LoggingPlan, 
                              beta_LoggingPlan = beta_LoggingPlan,
                              PlantDuration= PlantDurationObs,
                              scenario=scenario) 
    
    # run Damage => calculate the logging damage
    dataNatFor <- cbind(dataNatFor, 
                        Damage(LogInt=dataNatFor$LogInt, WDtimber=WDtimber, Ccontent=Ccontent, 
                               ACS0=dataNatFor$AGB0 * Ccontent, CoeffDam=CoeffDam,
                               CoeffDam2Fuel=CoeffDam2Fuel, RIL=RIL, CoeffRIL = CoeffRIL,
                               FuelNatFor=FuelNatFor, type="NatFor", Efuelcollect=Efuelcollect)) 
    
    
    # Run DeforNatFor => calculate data on logging road, main trail and deck
    dataNatFor <- cbind(dataNatFor, 
                        DeforNatFor(LogInt=dataNatFor$LogInt, CdamFuel=dataNatFor$Cdamfuel,
                                    ACS0=dataNatFor$AGB0*Ccontent, CoeffRroad=CoeffRroad,
                                    CoeffWroad=CoeffWroad, WDfuel=WDfuel,
                                    FuelRoadUnit=FuelRoadUnit, H=H, type="NatFor")) 
    
    # Run LogAct => C of all emissions from logging activities 
    dataNatFor <- cbind(dataNatFor, 
                        LogAct(EmisPlanning=EmisPlanning, EmisRoadBuild=EmisRoadBuild, 
                               EmisLogging=EmisLogging, EmisTransport=EmisTransport, 
                               EmisLoading=EmisLoading, Lroad= dataNatFor$Lroad, 
                               Cdamfuel=dataNatFor$Cdamfuel, DistTimbNatFor=DistTimbNatFor,
                               DistFuelNatFor=DistFuelNatFor, H=H, Ccontent=Ccontent, 
                               FuelRoadUnit=FuelRoadUnit, WDtimber=WDtimber , LogInt=dataNatFor$LogInt,
                               type="NatFor")) 
  }
  ###################################################################################
    
    
  ### Get cumulative fluxes for plantation ##########################################
  # run only if VolTimbPlant is not zero (meaning that plantations are implemented in the scenario)
  if (VolTimbPlant!=0) {
    # get a time matrix since logging for every plantation and every year of the plantation and every iteration
    # plots * interations = row , year = colums
    # time <- 1:dataPlantation[,max(t0)] # to stop at the last year when logging is possible, even if it's not Tsimu
    time <- 1:Tsimu
    TmPlant <- data.table(t(sapply(dataPlantation$t0, function(j) {time-j})))
    colnames(TmPlant) <- as.character(time)
    TmPlant <- TmPlant[, replace(.SD, .SD < 0, NA)]
    
    # get a time matrix with 1 for the year of event and all years after and NA otherwise
    Tm0Plant <- as.data.table(matrix(dataPlantation$t0 , length(dataPlantation$t0) , length(time))) # repeat the same columns
    colnames(Tm0Plant) <- as.character(time)
    # Na if the t0 for the row less than the columns name
    for(col in 1:length(time)) {set(Tm0Plant, i=which(Tm0Plant[[col]]>col), j=col, value=NA)}
    # 1 if it's more
    for(col in 1:length(time)) {set(Tm0Plant, i=which(Tm0Plant[[col]]<=col) , j=col, value=1)}
    
    # Run the function giving the C fluxes cumulated for all plots
    # each function return a data.table of C fluxes per year (or a list when several type of fluxes)
    # each function is run per iteration so we get for each flux
    # a data.table with each row = an iterations and each colum = a year
    
    # Run PlantGrowthCum : returns a data.table
    CcumPgrowth <- PlantGrowthCum(dataPlantation = dataPlantation, Tm0Plant = Tm0Plant)
    
    # Run Sawmill : returns a list
    CcumPsawmill <- Sawmill(data = dataPlantation, type = "Plant",
                            Esawmill = Esawmill, DFtable=DFtable, t05=t05, Conso=Conso,
                            RsawnAlt = RsawnAlt, Rwaste = Rwaste,
                            nrun=nrun, Tm0=Tm0Plant, Tm=TmPlant)
    dataPlantation$CSawmillFuel <- CcumPsawmill$CSawmillFuel # add the quantity of C from sawdust going to the biomass plant per ha
    CcumPsawmill$CSawmillFuel <- NULL # remove it from the list as not needed
    names(CcumPsawmill) <- c("CcumPSawnAlt", "CcumPsawnwood", 
                             "CcumPdustWaste", "CcumPSawmillNoAvoid") # renames to avoid same names than in NatFor
    
    # Run BiomassPlant : returns a list
    CcumPbiomassP <- BiomassPlant(data=dataPlantation, type="Plant", PCIhNatFor=PCIhNatFor, 
                                  H=H , EbiomassPlant=EbiomassPlant, 
                                  Cfossil=Cfossil, Tm0=Tm0Plant)

    names(CcumPbiomassP) <- c("CcumPburnt", "CcumPavoidFuel", "CcumPopBioPlant") # renames to avoid same names than in NatFor
    
    # Run ActCum: returns a list
    CcumPAct <- ActCum(data=dataPlantation, type="Plant", 
                       DistMillBioPlant=DistMillBioPlant, Ccontent=Ccontent, Tm0=Tm0Plant)
    names(CcumPAct) <- c("CcumPotherAct", "CcumPtrans") # renames to avoid same names than in NatFor
    
    
    # Run DecayResid : returns a data.table
    CcumPdecay <- DecayResid(data=dataPlantation, type="Plant", 
                             CoeffDecayLWB=CoeffDecayLWB, Tm=TmPlant)
  }
  ###################################################################################
    
    
    
  ### Get cumulative fluxes for natural forest ##########################################
  # run only if LogIntScenar is not zero (meaning that there is some logging in natural forest)
  if (LogIntScenar!=0) {
    # get a time matrix since logging for every plot and every iteration
    # plots * interations = row , year = colums
    # time <- 1:dataNatFor[,max(t0)] # to stop at the last year when logging is possible, even if it's not Tsimu
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
    CcumNFrecov <- Recovery(method=methodRecov, 
                            data=dataNatFor,
                            CoeffRecov=CoeffRecov,
                            standardiseRecov=standardiseRecov, 
                            Ccontent=Ccontent,
                            Tm=TmNatFor)
    
    # Run Regeneration : returns a dataframe
    CcumNFrege <- Regeneration(data=dataNatFor,
                               CoeffRege=CoeffRege, 
                               Ccontent=Ccontent,
                               Tm=TmNatFor)
    
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
    CcumNFdecay <- DecayResid(data=dataNatFor, type="NatFor",
                              CoeffDecayLWB=CoeffDecayLWB, Tm=TmNatFor)
  }
  ###################################################################################
  
  
  ### PAST LOGGING  ####################################################################
    # run only if ConnectPast = TRUE
    if (ConnectPast == TRUE) {
      
      # Get Data past logging (repeat as many times as there are iterations)
      dataPastLog <- data.table(iter=rep(1:nrun, each=dim(dataPastLogging)[1]),
                                dataPastLogging)
      
      # check that all plots deforested for plantation have been logged in one go (not over several years) in the past
      if (VolTimbPlant!=0) {
      NbLogged <- dataPastLog[, .(NbLogged=.N/nrun), by=ID_LogUnit] 
      if (dim(NbLogged[NbLogged>1 & ID_LogUnit %in% unique(dataPlantation$code_parcOLD),])[1]>0) {
        stop("Provide a dataset of plots available for plantation with only plots logged over one single year")}  
      }
      
      # run Damage => calculate the logging damage (never RIL for past logging and never fuelwood)
      dataPastLog <- cbind(dataPastLog, 
                          Damage(LogInt=dataPastLog$LogInt, WDtimber=WDtimber, Ccontent=Ccontent, 
                                 ACS0=dataPastLog$AGB0 * Ccontent, CoeffDam=CoeffDam,
                                 CoeffDam2Fuel=CoeffDam2Fuel, RIL=FALSE, CoeffRIL = CoeffRIL,
                                 FuelNatFor=FuelNatFor, type="PastLog", Efuelcollect=Efuelcollect)) 
      
      # Run DeforNatFor => calculate data on logging road, main trail and deck
      dataPastLog <- cbind(dataPastLog, 
                          DeforNatFor(LogInt=dataPastLog$LogInt, CdamFuel=dataPastLog$Cdamfuel,
                                      ACS0=dataPastLog$AGB0*Ccontent, CoeffRroad=CoeffRroad,
                                      CoeffWroad=CoeffWroad, WDfuel=WDfuel,
                                      FuelRoadUnit=FuelRoadUnit, H=H, type="PastLog")) 
      
      # Run LogAct => C of all emissions from logging activities 
      dataPastLog <- cbind(dataPastLog, 
                          LogAct(EmisPlanning=EmisPlanning, EmisRoadBuild=EmisRoadBuild, 
                                 EmisLogging=EmisLogging, EmisTransport=EmisTransport, 
                                 EmisLoading=EmisLoading, Lroad= dataPastLog$Lroad, 
                                 Cdamfuel=dataPastLog$Cdamfuel, DistTimbNatFor=DistTimbNatFor,
                                 DistFuelNatFor=DistFuelNatFor, H=H, Ccontent=Ccontent, 
                                 FuelRoadUnit=FuelRoadUnit, WDtimber=WDtimber , LogInt=dataPastLog$LogInt, 
                                 type="PastLog")) 
      
      
      ### Get cumulative fluxes for Past logging 
      
        # sort by t0
        setorder(dataPastLog, t0)
      
        # add t0Past for which 1 is the first year of past logging
        dataPastLog$t0Past <- dataPastLog[, t0-dataPastLog[,min(t0)]+1]
      
        # get a time matrix since logging for every plot and every iteration
        # plots * interations = row , year = colums
        # the first year is the 1 for the year of first logging
        # there are as many year as year since the first logging to the last + Tsimu (to get fluxes up to the end of simulation time)
        time <- 1:(Tsimu+dataPastLog[,max(t0Past)])
        TmPast <- data.table(t(sapply(dataPastLog$t0Past, function(j) {time-j})))
        colnames(TmPast) <- as.character(time)
        TmPast <- TmPast[, replace(.SD, .SD < 0, NA)]
        
        # get a time matrix with 1 for the year of logging and all years after and NA otherwise
        Tm0Past <- as.data.table(matrix(dataPastLog$t0Past , length(dataPastLog$t0Past) , length(time))) # repeat the same columns
        colnames(Tm0Past) <- as.character(time)
        # Na if the t0 for the row is less than the columns name
        for(col in 1:length(time)) {set(Tm0Past, i=which(Tm0Past[[col]]>col), j=col, value=NA)}
        # 1 if it's more
        for(col in 1:length(time)) {set(Tm0Past, i=which(Tm0Past[[col]]<=col) , j=col, value=1)}
        
        # If plantation
        # get a time matrix of the same size than TmPast and Tm0Past 
        # giving the proportion of AreaLogged that hasn't been deforested for plantation (PropLeft)
        # ie is still recovering and regenerating
        #####
        if (VolTimbPlant!=0) {
        # create a dataframe with as many row as plot * iteration and columns are years in Past (for the retrospective part)
        TmPastLeft <- as.data.table(matrix(data=1, 
                                               nrow=dim(dataPastLog[,.(iter, ID_LogUnit)])[1], 
                                               ncol=dataPastLog[,max(t0Past)]))
        colnames(TmPastLeft) <- as.character(dataPastLog[,min(t0)]:dataPastLog[,max(t0)]) # only ones because nothing converted in plantation before prospective part
        
        TmPastLeft <- data.table(dataPastLog[,.(iter, ID_LogUnit)], TmPastLeft)
        
        # add PropLeft for the prospective part
        for (y in (dataPastLog[,max(t0)]+1):(dataPastLog[,max(t0)]+Tsimu)) { # for all year in the prospective part
          dataConvertY <- data.table(dataPastLog[,.(iter, ID_LogUnit, AreaLogged)], t=y) # temp data
          # sum of area converted for each plot and iteration up to year y
          TEMP <- dataPlantation[tPlant+dataPastLog[,max(t0)]<=y & PlantCycle==1 & YearType=="P", # all row of conversion (PlantCycle==1 and t0==1) up to y 
                                 .(AreaConv=sum(AreaPlant)), by=.(iter, code_parcOLD)]
          colnames(TEMP) <- c("iter", "ID_LogUnit", "AreaConv")
          dataConvertY <- merge(dataConvertY, TEMP, by=c("iter", "ID_LogUnit"), all.x = TRUE, sort=FALSE)
          dataConvertY[is.na(AreaConv), AreaConv:=0]
          dataConvertY$PropLeft <- dataConvertY[, 1-(AreaConv/AreaLogged)]
          TmPastLeft$new <- dataConvertY$PropLeft
          names(TmPastLeft)[names(TmPastLeft) == "new"] <- as.character(y) 
        }
        TmPastLeft <- TmPastLeft[, c("iter", "ID_LogUnit") := NULL] # remove two first columns
        }
        
        # Run the function giving the C fluxes cumulated for all plots
        # each function return a data.table of C fluxes per year (or a list when several type of fluxes)
        # each function is run per iteration so we get for each flux
        # a data.table with each row = an iterations and each colum = a year
        
        if (VolTimbPlant!=0) { # if some logged plots in the past have been converted to plantation
          # Run Recovery : returns a dataframe
          CcumPastrecov <- Recovery(method=methodRecov,
                                    data=dataPastLog,
                                    CoeffRecov=CoeffRecov,
                                    standardiseRecov=standardiseRecov, 
                                    Ccontent=Ccontent,
                                    Tm=TmPast, TmPastLeft=TmPastLeft)
          
          # Run Regeneration : returns a dataframe
          CcumPastrege <- Regeneration(data=dataPastLog,
                                       CoeffRege=CoeffRege, 
                                       Ccontent=Ccontent,
                                       Tm=TmPast, TmPastLeft=TmPastLeft)
        } else {
          # Run Recovery : returns a dataframe
          CcumPastrecov <- Recovery(method=methodRecov,
                                    data=dataPastLog,
                                    CoeffRecov=CoeffRecov,
                                    standardiseRecov=standardiseRecov, 
                                    Ccontent=Ccontent,
                                    Tm=TmPast)
          
          # Run Regeneration : returns a dataframe
          CcumPastrege <- Regeneration(data=dataPastLog,
                                     CoeffRege=CoeffRege, 
                                     Ccontent=Ccontent,
                                     Tm=TmPast)
        }
        
        # Run Sawmill : returns a list
        CcumPastsawmill <- Sawmill(data = dataPastLog, type = "PastLog",
                                   Esawmill = Esawmill, DFtable=DFtable, t05=t05, Conso=Conso,
                                   RsawnAlt = RsawnAlt, Rwaste = Rwaste,
                                   nrun=nrun, Tm0=Tm0Past, Tm=TmPast)
        dataPastLog$CSawmillFuel <- CcumPastsawmill$CSawmillFuel # add the quantity of C from sawdust going to the biomass plant
        CcumPastsawmill$CSawmillFuel <- NULL # remove it from the list as not needed
        names(CcumPastsawmill) <- c("CcumPastSawnAlt", "CcumPastsawnwood", 
                                    "CcumPastdustWaste", "CcumPastSawmillNoAvoid") # renames to avoid same names than in Plantation
        
        # Run BiomassPlant : returns a list
        CcumPastbiomassP <- BiomassPlant(data=dataPastLog, type="NatFor", PCIhNatFor=PCIhNatFor, # ok to use NatFor because qty of fuel calculated before 
                                         H=H , EbiomassPlant=EbiomassPlant, 
                                         Cfossil=Cfossil, Tm0=Tm0Past)
        names(CcumPastbiomassP) <- c("CcumPastburnt" , "CcumPastavoidFuel" , "CcumPastopBioPlant") # renames to avoid same names than in Plantation
        
        # Run ActCum : returns a list
        CcumPastAct <- ActCum(data=dataPastLog, type="NatFor", # ok to use NatFor because same structure of data
                              DistMillBioPlant=DistMillBioPlant, Ccontent=Ccontent, Tm0=Tm0Past)
        names(CcumPastAct) <- c("CcumPastotherAct","CcumPasttrans") # renames to avoid same names than in Plantation
        
        # Run Deadwood decay : returns a dataframe
        CcumPastdecay <- DecayResid(data=dataPastLog, type="NatFor",
                                    CoeffDecayLWB=CoeffDecayLWB, Tm=TmPast)
        
  }
  ###################################################################################
  
  ### put all matrix in the same time scale #########################################
  # if ConnectPast == FALSE => no need to change, everything is in relative year (1:Tsimu)   
  if (ConnectPast == TRUE) {   
    # change years for matrices of PastLog
    colnames(CcumPastrecov) <- as.character(as.numeric(colnames(CcumPastrecov)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastrege) <- as.character(as.numeric(colnames(CcumPastrege)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastsawmill$CcumPastSawnAlt) <- as.character(as.numeric(colnames(CcumPastsawmill$CcumPastSawnAlt)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastsawmill$CcumPastsawnwood) <- as.character(as.numeric(colnames(CcumPastsawmill$CcumPastsawnwood)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastsawmill$CcumPastdustWaste) <- as.character(as.numeric(colnames(CcumPastsawmill$CcumPastdustWaste)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastsawmill$CcumPastSawmillNoAvoid) <- as.character(as.numeric(colnames(CcumPastsawmill$CcumPastSawmillNoAvoid)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastbiomassP$CcumPastburnt) <- as.character(as.numeric(colnames(CcumPastbiomassP$CcumPastburnt)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastbiomassP$CcumPastavoidFuel) <- as.character(as.numeric(colnames(CcumPastbiomassP$CcumPastavoidFuel)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastbiomassP$CcumPastopBioPlant) <- as.character(as.numeric(colnames(CcumPastbiomassP$CcumPastopBioPlant)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastAct$CcumPastotherAct) <- as.character(as.numeric(colnames(CcumPastAct$CcumPastotherAct)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastAct$CcumPasttrans) <- as.character(as.numeric(colnames(CcumPastAct$CcumPasttrans)) + dataPastLog[,min(t0)] -1)
    colnames(CcumPastdecay) <- as.character(as.numeric(colnames(CcumPastdecay)) + dataPastLog[,min(t0)] -1)
    
    # change years for matrices Plantation
    if (VolTimbPlant!=0) {
      dataPlantation$t0 <- dataPlantation$t0 + dataPastLog[,max(t0)]
      colnames(CcumPgrowth) <- as.character(as.numeric(colnames(CcumPgrowth)) + dataPastLog[,max(t0)])
      colnames(CcumPsawmill$CcumPSawnAlt) <- as.character(as.numeric(colnames(CcumPsawmill$CcumPSawnAlt)) + dataPastLog[,max(t0)])
      colnames(CcumPsawmill$CcumPsawnwood) <- as.character(as.numeric(colnames(CcumPsawmill$CcumPsawnwood)) +  dataPastLog[,max(t0)])
      colnames(CcumPsawmill$CcumPSawmillNoAvoid) <- as.character(as.numeric(colnames(CcumPsawmill$CcumPSawmillNoAvoid)) +  dataPastLog[,max(t0)])
      colnames(CcumPsawmill$CcumPdustWaste) <- as.character(as.numeric(colnames(CcumPsawmill$CcumPdustWaste)) +  dataPastLog[,max(t0)])
      colnames(CcumPbiomassP$CcumPburnt) <- as.character(as.numeric(colnames(CcumPbiomassP$CcumPburnt)) +  dataPastLog[,max(t0)])
      colnames(CcumPbiomassP$CcumPavoidFuel) <- as.character(as.numeric(colnames(CcumPbiomassP$CcumPavoidFuel)) +  dataPastLog[,max(t0)])
      colnames(CcumPbiomassP$CcumPopBioPlant) <- as.character(as.numeric(colnames(CcumPbiomassP$CcumPopBioPlant)) +  dataPastLog[,max(t0)])
      colnames(CcumPAct$CcumPotherAct) <- as.character(as.numeric(colnames(CcumPAct$CcumPotherAct)) +  dataPastLog[,max(t0)])
      colnames(CcumPAct$CcumPtrans) <- as.character(as.numeric(colnames(CcumPAct$CcumPtrans)) +  dataPastLog[,max(t0)])
      colnames(CcumPdecay) <- as.character(as.numeric(colnames(CcumPdecay)) +  dataPastLog[,max(t0)])
    }
    
    # change years for NatFor
    if (LogIntScenar!=0) {
      dataNatFor$t0 <- dataNatFor$t0 + dataPastLog[,max(t0)]
      colnames(CcumNFrecov) <- as.character(as.numeric(colnames(CcumNFrecov)) + dataPastLog[,max(t0)])
      colnames(CcumNFrege) <- as.character(as.numeric(colnames(CcumNFrege)) + dataPastLog[,max(t0)])
      colnames(CcumNFsawmill$CcumNFSawnAlt) <- as.character(as.numeric(colnames(CcumNFsawmill$CcumNFSawnAlt)) + dataPastLog[,max(t0)])
      colnames(CcumNFsawmill$CcumNFsawnwood) <- as.character(as.numeric(colnames(CcumNFsawmill$CcumNFsawnwood)) + dataPastLog[,max(t0)])
      colnames(CcumNFsawmill$CcumNFdustWaste) <- as.character(as.numeric(colnames(CcumNFsawmill$CcumNFdustWaste)) + dataPastLog[,max(t0)])
      colnames(CcumNFsawmill$CcumNFSawmillNoAvoid) <- as.character(as.numeric(colnames(CcumNFsawmill$CcumNFSawmillNoAvoid)) +  dataPastLog[,max(t0)])
      colnames(CcumNFbiomassP$CcumNFburnt) <- as.character(as.numeric(colnames(CcumNFbiomassP$CcumNFburnt)) + dataPastLog[,max(t0)])
      colnames(CcumNFbiomassP$CcumNFavoidFuel) <- as.character(as.numeric(colnames(CcumNFbiomassP$CcumNFavoidFuel)) + dataPastLog[,max(t0)])
      colnames(CcumNFbiomassP$CcumNFopBioPlant) <- as.character(as.numeric(colnames(CcumNFbiomassP$CcumNFopBioPlant)) + dataPastLog[,max(t0)])
      colnames(CcumNFAct$CcumNFotherAct) <- as.character(as.numeric(colnames(CcumNFAct$CcumNFotherAct)) + dataPastLog[,max(t0)])
      colnames(CcumNFAct$CcumNFtrans) <- as.character(as.numeric(colnames(CcumNFAct$CcumNFtrans)) + dataPastLog[,max(t0)])
      colnames(CcumNFdecay) <- as.character(as.numeric(colnames(CcumNFdecay)) + dataPastLog[,max(t0)])
    } 
  }
  ###################################################################################  
  
    
  ### bind all data in a list and return ############################################
  # if only natural forest in the scenario
  if (VolTimbPlant==0 & LogIntScenar!=0) {
    outputScenario <- list(dataNatFor = dataNatFor,
                           CcumNFrecov = CcumNFrecov, 
                           CcumNFrege = CcumNFrege, 
                           CcumNFsawmill = CcumNFsawmill, 
                           CcumNFbiomassP = CcumNFbiomassP, 
                           CcumNFAct = CcumNFAct,
                           CcumNFdecay = CcumNFdecay)
  }
    
  # if only plantation  
    if (LogIntScenar==0 & VolTimbPlant!=0) {  
      outputScenario <- list(dataPlantation = dataPlantation,
                                             CcumPgrowth = CcumPgrowth,
                                             CcumPsawmill = CcumPsawmill,
                                             CcumPbiomassP = CcumPbiomassP,
                                             CcumPAct = CcumPAct, 
                                             CcumPdecay = CcumPdecay)
    }
  
  # if both logging in natural forest and plantation
    if (VolTimbPlant!=0 & LogIntScenar!=0) {
      outputScenario <- list(dataPlantation = dataPlantation,
                             dataNatFor = dataNatFor,
                             CcumPgrowth = CcumPgrowth,
                             CcumPsawmill = CcumPsawmill,
                             CcumPbiomassP = CcumPbiomassP,
                             CcumPAct = CcumPAct, 
                             CcumPdecay = CcumPdecay, 
                             CcumNFrecov = CcumNFrecov, 
                             CcumNFrege = CcumNFrege, 
                             CcumNFsawmill = CcumNFsawmill, 
                             CcumNFbiomassP = CcumNFbiomassP, 
                             CcumNFAct = CcumNFAct,
                             CcumNFdecay = CcumNFdecay)   
    }
    
    # add PastLog if ConnectPast is  TRUE
    if (ConnectPast == TRUE) { 
      if (VolTimbPlant==0 & LogIntScenar==0){ # if only Past
        outputScenario  <- list(dataPastLog = dataPastLog,
                                CcumPastrecov = CcumPastrecov,
                                CcumPastrege = CcumPastrege,
                                CcumPastsawmill = CcumPastsawmill,
                                CcumPastbiomassP = CcumPastbiomassP,
                                CcumPastAct = CcumPastAct,
                                CcumPastdecay =CcumPastdecay)
      } else {
        outputScenario <- append(outputScenario, list(dataPastLog = dataPastLog,
                                                      CcumPastrecov = CcumPastrecov,
                                                      CcumPastrege = CcumPastrege,
                                                      CcumPastsawmill = CcumPastsawmill,
                                                      CcumPastbiomassP = CcumPastbiomassP,
                                                      CcumPastAct = CcumPastAct,
                                                      CcumPastdecay =CcumPastdecay))
      }
    }
    
    
    return(outputScenario)
}
