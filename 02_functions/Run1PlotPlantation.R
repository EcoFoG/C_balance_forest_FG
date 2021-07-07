### Function Run1PlotPlantation
##############################################

# based on RunScenario but only for 1 plantation established in the first year


Run1PlotPlantation <- function (nrun, Tsimu, Replant, 
                                PlantDurationObs=30, # real duration of plantation
                                ChangeProdObs = NULL, # real change of productivity
                                Esawmill = 0.3, # efficiency of sawmill
                                RsawnAlt = 0.4, # proportion of sawnwood used instead of another material (
                                EbiomassPlant = 0.22 ,# efficiency of biomass plant
                                determ = NULL) {  # for sensitivity analysis

  
  ### Preliminary calculations ####################################################
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
  ###################################################################################
  
  
  # create a data set with just one test plot 
  if (Replant==TRUE) {
    nbCycle <- ceiling(Tsimu/PlantDurationObs) # get number of cycle
    YearPlant <- rep(as.numeric(NA), nbCycle) # to store year of plantation
    YearPlant[1] <- 1
    for (i in 2:nbCycle) {
      YearPlant[i] <- YearPlant[i-1] + PlantDurationObs
    }
    PlantCycle <- 1:nbCycle
  }
  if (Replant==FALSE) {
    nbCycle <- 1
    YearPlant <- 1
    PlantCycle <- 1
  }
  
  
  dataPlantation <- data.table(iter=rep(1:nrun,each=nbCycle), # as many line per iter as there are planting cyle
                               ID_Plant="Test",
                               AreaPlant=1,
                               AGB0=median(dataPlotforPlant$AGB0),
                               t0=YearPlant,
                               PlantCycle = PlantCycle,
                               trelat =0,
                               YearType = as.factor("P"),
                               YearLogged= as.numeric(NA),
                               AreaTot= as.numeric(NA),
                               AreaLogged= as.numeric(NA),
                               code_parcOLD = as.numeric(NA))
  dataPlantation$tPlant <- dataPlantation$t0
  dataPlantation <- cbind(dataPlantation, Data_wood_plantation[sample(.N, dim(dataPlantation)[1], replace=TRUE), c("WDPlant", "PCIhPlant")])
    
    
  # run DeForPlant => use the wood from deforestation for plantation
  if (!("DeforPlant" %in% determ) & !("DecayResid" %in% determ)) {
    dataPlantation <- DeforPlant(dataPlantation = dataPlantation, 
                                 CoeffBOdefor = CoeffBOdefor, 
                                 CoeffBEdefor = CoeffBEdefor, 
                                 Ccontent = Ccontent) 
  }
  if ("DeforPlant" %in% determ & !("DecayResid" %in% determ)) {
    dataPlantation <- DeforPlant(dataPlantation = dataPlantation, 
                                 CoeffBOdefor = CoeffBOdefor, 
                                 CoeffBEdefor = CoeffBEdefor, 
                                 Ccontent = Ccontent, determ = TRUE) 
  }
  if (!("DeforPlant" %in% determ) & "DecayResid" %in% determ) {
    dataPlantation <- DeforPlant(dataPlantation = dataPlantation, 
                                 CoeffBOdefor = CoeffBOdefor, 
                                 CoeffBEdefor = CoeffBEdefor, 
                                 Ccontent = Ccontent, determDecay=TRUE) 
  }
  if ("DeforPlant" %in% determ & "DecayResid" %in% determ) {
    dataPlantation <- DeforPlant(dataPlantation = dataPlantation, 
                                 CoeffBOdefor = CoeffBOdefor, 
                                 CoeffBEdefor = CoeffBEdefor, 
                                 Ccontent = Ccontent, determ = TRUE, determDecay=TRUE) 
  }

  
  # run PlantTrajAll (uses PlantTraj) to get trajectories of all plantation * iterations
  if (!("PlantTraj" %in% determ)) {
    dataPlantation <- PlantTrajAll(dataPlantation, PlantDuration=PlantDurationObs,
                                   ChangeProd=ChangeProdObs)
  }
  if ("PlantTraj" %in% determ) {
    dataPlantation <- PlantTrajAll(dataPlantation, PlantDuration=PlantDurationObs,
                                   ChangeProd=ChangeProdObs, determ = TRUE)
  }  
  

  
  # run PlantAct to get the emission (per ha) due to plantation activities and wood transport
  dataPlantation <- PlantAct(dataPlantation=dataPlantation, EmisPlanPlant=EmisPlanPlant,
                             EmisDefor=EmisDefor, EmisTransport=EmisTransport, 
                             EmisLoading=EmisLoading, EmisStack=EmisStack, 
                             EmisPrepPlant=EmisPrepPlant, EmisPlantClean=EmisPlantClean, 
                             DistTimbPlant=DistTimbPlant, DistFuelPlant=DistFuelPlant)
  
  
  
  
  ### Get cumulative fluxes for plantation ##########################################
    # get a time matrix since logging for every plantation and every year of the plantation and every iteration
    # plots * interations = row , year = colums
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
    if (!("DecayResid" %in% determ)){
      CcumPdecay <- DecayResid(data=dataPlantation, type="Plant", 
                               CoeffDecayLWB=CoeffDecayLWB, Tm=TmPlant)
    }
    if ("DecayResid" %in% determ){
      CcumPdecay <- DecayResid(data=dataPlantation, type="Plant", 
                               CoeffDecayLWB=CoeffDecayLWB, Tm=TmPlant, determ=TRUE)
    }
    
    
    
    ### bind all data in a list and return ############################################
    outputScenario <- list(dataPlantation = dataPlantation,
                           CcumPgrowth = CcumPgrowth,
                           CcumPsawmill = CcumPsawmill,
                           CcumPbiomassP = CcumPbiomassP,
                           CcumPAct = CcumPAct, 
                           CcumPdecay = CcumPdecay)
    
    return(outputScenario)

}
