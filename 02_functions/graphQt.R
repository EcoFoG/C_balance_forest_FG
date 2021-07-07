#### Plot annual volume of timber OR quantity of energy actually produced
##############################################################

#### arguments:
# outputScenario: output data from function runScenario
# typeGraph: either "Timber" or "Energy"
# scenario: the scenario run
# StartYr: year when to start (in calendar year)
# CI: vector of low and high CI (eg: c(0.025, 0.975))


graphQt <- function(outputScenario, typeGraph, scenario, StartYr=NULL, CI, ylim=NULL,
                    EbiomassPlant=NULL) {
  
  # get connect past for the scenario 
  ConnectPast <- dataScenario[Scenario == scenario, ConnectPast]
  
  # need to provide EbiomassPlant if type is Energy
  if (typeGraph=="Energy" & is.null(EbiomassPlant)) {
    stop("You need to provide EbiomassPlant if type is Energy")
  }
    
  # we cannot have a StartYr when ConnectPast=TRUE
  if (!is.null(StartYr) & ConnectPast==TRUE) {
    stop("You cannot give a StartYear if ConnectPast is TRUE")}
  
  ### get theme and color
  source(file="02_functions/ThemeGraph.R")
  
  list2env(outputScenario,.GlobalEnv) # copy all the elements of the list outputScenario into the global environement
  
  ### Get data for scenario 
  VolTimb <- dataScenario[Scenario == scenario, VolTimb]
  # LogInt <- dataScenario[Scenario == scenario, LogInt] NOT NEEDED ANYMORE BECAUSE IN DataNatFor
  
  
  ### Calculate the needed quantity and melt into long tables #####################################
  
  # create a dataframe to store the results
  if (ConnectPast == TRUE) {
    Qt <- data.table(iter = rep(1:nrun, each=dim(CcumPastrege)[2]),
                                t0=as.integer(colnames(CcumPastrege)))
  } else {
    Qt <- data.table(iter = rep(1:nrun, each=Tsimu),
                     t0 = rep(1:Tsimu, nrun))
  }
  
  ### If we want timber volume
  if (typeGraph=="Timber") {
    # run only if there is some logging in natural forest
    if ("dataNatFor" %in% names(outputScenario)) {
      Qt <- merge(Qt, dataNatFor[, .(QtNatFor= sum(LogInt*AreaLogged)),
                                 by=.(iter, t0)],
                  by = c("iter", "t0"),  all.x = TRUE)
    }
    # run only if plantations are implemented in the scenario
    if ("dataPlantation" %in% names(outputScenario)) {
      Qt <- merge(Qt, dataPlantation[, .(QtPlant=sum(Csawmill * AreaPlant /(Ccontent * WDPlant))),
                                     by=.(iter, t0)],
                  by = c("iter", "t0"),   all.x = TRUE)
    }
    # run only if ConnectPast=TRUE
    if (ConnectPast == TRUE) {
      Qt <- merge(Qt, dataPastLog[, .(QtPast=sum(LogInt*AreaLogged)),
                                  by=.(iter, t0)],
               by = c("iter", "t0"),   all.x = TRUE)
    }
  }
  
  
  ### If we want the quantity of energy => in GWh
  if (typeGraph=="Energy") {
    # run only if there is some logging in natural forest
    if ("dataNatFor" %in% names(outputScenario)) {
      Qt <- merge(Qt, 
                  dataNatFor[, 
                             .(QtNatFor= sum(AreaLogged*
                                               ((Cdamfuel+CDeforFuel+CSawmillFuel)/(Ccontent*(1-H)))*
                                               PCIhNatFor * EbiomassPlant)/1000000),
                             by=.(iter, t0)],
                  by = c("iter", "t0"),  all.x = TRUE)
    }
    # run only if plantations are implemented in the scenario
    if ("dataPlantation" %in% names(outputScenario)) {
      Qt <- merge(Qt, 
                  dataPlantation[, 
                             .(QtPlant= sum(AreaPlant*
                                               ((Cfuel+CSawmillFuel)/(Ccontent*(1-H)))*
                                              PCIhPlant * EbiomassPlant)/1000000),
                             by=.(iter, t0)],
                  by = c("iter", "t0"),  all.x = TRUE)
    }
    # run only if ConnectPast=TRUE
    if (ConnectPast == TRUE) {
      Qt <- merge(Qt, dataPastLog[, .(QtPast=sum(AreaLogged*
                                                   ((Cdamfuel+CDeforFuel+CSawmillFuel)/(Ccontent*(1-H)))*
                                                   PCIhNatFor * EbiomassPlant)/1000000),
                                  by=.(iter, t0)],
                  by = c("iter", "t0"),   all.x = TRUE)
    }
  }
  
  # replace NA per 0 
  if ("dataNatFor" %in% names(outputScenario)) {
    Qt[is.na(QtNatFor), QtNatFor:=0] }
  if ("dataPlantation" %in% names(outputScenario)) {
    Qt[is.na(QtPlant), QtPlant:=0] }  
  if (ConnectPast == TRUE) {
    Qt[is.na(QtPast), QtPast:=0] }  
  
  # if all 3 types of fluxes
  if ("dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) { 
    Qt$QtTot <- Qt[, QtNatFor + QtPlant + QtPast]
    Qt <- melt(Qt, id.vars= c("iter", "t0"), variable.name = "TypeQt", value.name = "Qt")
  }
    
  # if NatFor and Plantation
  if ("dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == FALSE) {
    Qt$QtTot <- Qt[, QtNatFor + QtPlant]
    Qt <- melt(Qt, id.vars= c("iter", "t0"), variable.name = "TypeQt", value.name = "Qt")
  }
  
  # if NatFor and Past
  if ("dataNatFor" %in% names(outputScenario) & 
      !"dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) { 
    Qt$QtTot <- Qt[, QtNatFor + QtPast]
    Qt <- melt(Qt, id.vars= c("iter", "t0"), variable.name = "TypeQt", value.name = "Qt")
  }
  
  # if Plant and Past
  if (!"dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) { 
    Qt$QtTot <- Qt[, QtPlant + QtPast]
    Qt <- melt(Qt, id.vars= c("iter", "t0"), variable.name = "TypeQt", value.name = "Qt")
  }
  
  # if only one of natural forest and plantation
  if (dim(Qt)[2] == 3) {
    Qt$TypeQt <- colnames(Qt)[3]
    colnames(Qt)[3] <- "Qt"
  }
  
  # Get median and CI
  Qt4graph <- Qt[, .(Median=median(Qt),
                             CIlow = quantile(Qt, probs = CI[1], na.rm =TRUE),
                             CIhigh = quantile(Qt, probs = CI[2], na.rm =TRUE)),
                         by=.(t0, TypeQt)]
  
  # Rename for legend
  # if all 3 types of fluxes
  if ("dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) { 
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtPast", "QtNatFor", "QtPlant"), 
                              labels=c("Total", "Historical", "Natural forest", "Plantation"))
  }
  # if NatFor and Plantation
  if ("dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == FALSE) {
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtNatFor", "QtPlant"), 
                              labels=c("Total", "Natural forest", "Plantation"))
  }
  # if NatFor and Past
  if ("dataNatFor" %in% names(outputScenario) & 
      !"dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) {
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtPast", "QtNatFor"), 
                              labels=c("Total", "Historical", "Natural forest"))
  }
  # if Plant and Past
  if (!"dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) { 
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtPast", "QtPlant"), 
                              labels=c("Total", "Historical", "Plantation"))
  }
  # if NatFor only
  if ("dataNatFor" %in% names(outputScenario) & 
      !"dataPlantation" %in% names(outputScenario) & 
      ConnectPast == FALSE) { 
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtNatFor"), 
                              labels=c("Total", "Natural forest"))
  }
  # if Plantation only
  if (!"dataNatFor" %in% names(outputScenario) & 
      "dataPlantation" %in% names(outputScenario) & 
      ConnectPast == FALSE) { 
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtPlant"), 
                              labels=c("Total", "Plantation"))
  }
  # if Past only 
  if (!"dataNatFor" %in% names(outputScenario) & 
      !"dataPlantation" %in% names(outputScenario) & 
      ConnectPast == TRUE) { 
    Qt4graph$TypeQt <- factor(Qt4graph$TypeQt, 
                              levels=c("QtTot", "QtPast"), 
                              labels=c("Total", "Historical"))
  }
  
  
  # Add the absolute year
  if (!is.null(StartYr)) {
    Qt4graph$t0 <- as.numeric(Qt4graph$t0) + StartYr - 1
  }
  
  
  ### Make the graph ###########################################################
  if (typeGraph=="Timber") {ylab="Annual timber volume (m3/yr)"}
  if (typeGraph=="Energy") {ylab="Annual production of energy (GWh/yr)"}
  
  graph <- ggplot(data=Qt4graph, aes(x=t0, group=TypeQt, col=TypeQt)) + 
    geom_line(aes(y=Median), size=1) +
    geom_ribbon(aes(ymin=CIlow, ymax=CIhigh, fill=TypeQt), alpha = 0.2, colour = NA) +
    xlab("Year") + ylab(ylab) +
    scale_colour_manual(values = myColQt) +
    scale_fill_manual(values = myColQt) + 
    scale_y_continuous(limits = c(0, NA)) +
    MyTheme +
    theme(legend.title = element_blank()) + 
    theme(legend.position="bottom")
  
  if (!is.null(ylim)) {
    graph <- graph + ylim(ylim[1], ylim[2])  
  }
  
  if(ConnectPast == FALSE) {
    graph <- graph + scale_x_continuous(limits = c(StartYr, NA))
  }
  
  if (typeGraph=="Timber") {
    graph <- graph + geom_hline(yintercept = VolTimb, linetype = "dashed")}  # Voltotal expected
  
  if(ConnectPast == TRUE) {
    StSim <- dataPastLog[,max(t0)] + 1
    graph <- graph + geom_vline(xintercept = StSim, linetype = "dashed") 
  }
  
  if ("dataPlantation" %in% names(outputScenario)) {
    StPlant <- dataPlantation[YearType=="H", min(t0)]
    graph <- graph + geom_vline(xintercept = StPlant, linetype = "dotted") 
  }  # to add a dashed line when first plantation are harvestable
  
  return(graph) 
}
  
