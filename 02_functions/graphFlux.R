 #### Funtion graphFlux ##############
 ######################################################
 
 # Make a graph of fluxes, per type of fluxes
 # either cumulated or annual fluxes
 
 #### arguments:
 # AllFlux4graph: results summarised (median and CI) as obtained by function GetResultFlux
 # typeGraph: either "cumulative" or "annual"
 # ylim: data:table of limits of y axes (to do the same for all) eg: ylim=c(-25,10)
 
 
 graphFlux <- function(AllFlux4graph, typeGraph, ylim, StartYr=NULL,
                       PlantDurationObs=30, sizeLine=1) {
   
 
   ### get theme and color
   source(file="02_functions/ThemeGraph.R")
   
   # remove the fluxes that we don't want to put on the graph 
   AllFlux4graph <- AllFlux4graph[!(FluxType %in% c("CcumNetNoAvoid",
                                                    "CcumNFSawnAlt", "CcumNFBiomassNoAvoid", "CcumNFdustWaste",
                                                    "CcumNFSawmillAvoid", "CcumNFSawmillNoAvoid","CcumNFsawnwood",
                                                    "CcumNFavoidFuel", "CcumNFBiomassNoAvoid", "CcumNFburnt", "CcumNFopBioPlant",
                                                    "CcumPSawnAlt", "CcumPBiomassNoAvoid", "CcumPdustWaste",
                                                    "CcumPSawmillAvoid", "CcumPSawmillNoAvoid","CcumPsawnwood",
                                                    "CcumPavoidFuel", "CcumPBiomassNoAvoid", "CcumPburnt", "CcumPopBioPlant",
                                                    "CcumPastSawnAlt", "CcumPastBiomassNoAvoid", "CcumPastdustWaste",
                                                    "CcumPastSawmillAvoid", "CcumPastSawmillNoAvoid","CcumPastsawnwood",
                                                    "CcumPastavoidFuel","CcumPastBiomassNoAvoid", "CcumPastburnt", "CcumPastopBioPlant"))]
   AllFlux4graph[, FluxType := droplevels(FluxType)]
   
   
   ### Add FluxSource (for facet_grid)
     AllFlux4graph$Subgraph <- as.factor(NA)
     AllFlux4graph[FluxType %in% c("CcumNET", "CcumNFall", "CcumPall", "CcumPastall"), Subgraph:= "Net fluxes"]
     AllFlux4graph[FluxType %in% c("CcumNFrecov", "CcumNFrege", "CcumNFallSawmill", 
                                   "CcumNFallBiomass", "CcumNFallAct", "CcumNFdecay"), Subgraph:= "Natural forest (prospective)"]
     AllFlux4graph[FluxType %in% c("CcumPgrowth", "CcumPallSawmill", "CcumPallBiomass",
                                   "CcumPallAct", "CcumPdecay"), Subgraph:= "Plantation (prospective)"]
     AllFlux4graph[FluxType %in% c("CcumPastrecov", "CcumPastrege", "CcumPastallSawmill", 
                                   "CcumPastallBiomass", "CcumPastallAct", "CcumPastdecay"), Subgraph:= "Historical"]
     AllFlux4graph$Subgraph <- as.factor(as.character(AllFlux4graph$Subgraph)) # to remove levels NA
     # change the order
     AllFlux4graph$Subgraph <- ordered(AllFlux4graph$Subgraph, 
                                       levels=c("Net fluxes", "Historical",
                                                "Natural forest (prospective)", "Plantation (prospective)"))  
   AllFlux4graph[,Subgraph:=droplevels(Subgraph)]
   
   
   # Combine and Rename for legend
   ###################################################################  
   
   # Levels we want (grouping some) of FluxType when NatFor, Plant and past are all present
   AllLevels <- c("CcumNET", "CcumNFall", "CcumallAct", "CcumallBiomass",
                  "CcumallSawmill", "Ccumdecay", "Ccumrecov", "Ccumrege", 
                  "CcumPall", "CcumallAct", "CcumallBiomass", "CcumallSawmill", 
                  "CcumPastall", "CcumallAct", "CcumallBiomass", "CcumallSawmill",
                  "Ccumdecay", "Ccumrecov", "Ccumrege", "Ccumdecay", "CcumPgrowth") 
   AllLevelsort <- c("CcumNET", "CcumPastall", "CcumNFall", "CcumPall", 
                      "CcumallSawmill", "CcumallBiomass", "CcumallAct", "Ccumdecay",
                      "Ccumrecov" , "Ccumrege", "CcumPgrowth") # levels left after grouping in the order we want them for legend
   
   AllLabels <- c("Net overall", "Net historical", 
                    "Net natural forest (sim.)", "Net plantation (sim.)",
                    "Sawmill", "Biomass plant", "Logistic activities", "Decay on site",
                    "Recovery in natural forest", "Regeneration in natural forest",
                    "Growth in plantation")
   
   
   # if all 3 types of fluxes
   if ("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       "Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       "Historical" %in% levels(AllFlux4graph$Subgraph)) { 
     levels(AllFlux4graph$FluxType) <- AllLevels
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort, labels = AllLabels)
   }
   
  
   # if NatFor and Plantation
   if ("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       "Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       !("Historical" %in% levels(AllFlux4graph$Subgraph))) { 
     levels(AllFlux4graph$FluxType) <- AllLevels[-(13:19)]
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort[-2], labels = AllLabels[-2])
   }
   
   # if NatFor and Past
   if ("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       !("Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph)) & 
       "Historical" %in% levels(AllFlux4graph$Subgraph)) { 
     levels(AllFlux4graph$FluxType) <- AllLevels[-c(9:12,20:21)]
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort[-c(4,11)], labels = AllLabels[-c(4,11)])
   }
   
   # if Plant and Past
   if (!("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph)) & 
       "Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       "Historical" %in% levels(AllFlux4graph$Subgraph)) { 
     levels(AllFlux4graph$FluxType) <- AllLevels[-c(2:8)]
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort[-3], labels = AllLabels[-3])
   }
   
   # if NatFor only
   if ("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph) & 
       !("Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph)) & 
       !("Historical" %in% levels(AllFlux4graph$Subgraph))) { 
     levels(AllFlux4graph$FluxType) <- AllLevels[c(1:8)]
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort[c(1,3,5:10)], labels = AllLabels[c(1,3,5:10)])
   }
   
   # if Plantation only
   if (!("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph))  & 
       "Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph)  & 
       !("Historical" %in% levels(AllFlux4graph$Subgraph))) { 
     levels(AllFlux4graph$FluxType) <- AllLevels[c(1,9:12,20:21)]
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort[c(1,4:8,11)], labels = AllLabels[c(1,4:8,11)])
   }
   
   # if Past only 
   if (!("Natural forest (prospective)" %in% levels(AllFlux4graph$Subgraph)) & 
       !("Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph)) & 
       "Historical" %in% levels(AllFlux4graph$Subgraph)) { 
     levels(AllFlux4graph$FluxType) <- AllLevels[c(1, 13:19)]
     AllFlux4graph$FluxType <- factor(AllFlux4graph$FluxType, 
                                      levels = AllLevelsort[c(1,2,5:10)], labels = AllLabels[c(1,2,5:10)])
   }
   
   
   # Remove CcumNET if only one flux (otherwise overlap exactly the other and change the color)
    if (length(unique(AllFlux4graph$Subgraph))<3) {
     AllFlux4graph <- AllFlux4graph[FluxType!="Net overall"]
   } 
   
   AllFlux4graph$Yr <- as.numeric(as.character(AllFlux4graph$Yr))
   
   ### Make the graph
   if (typeGraph=="cumulative") {ylab = "Cumulative carbon balance (TgC)"}
   if (typeGraph=="annual") {ylab = "Annual carbon fluxes (GgC/yr)"}
   

     graph <- ggplot(data=AllFlux4graph, aes(x=Yr, group=FluxType, col=FluxType)) +
                     geom_line(aes(y=Median), size=sizeLine) +
                     geom_ribbon(aes(ymin=CIlow, ymax=CIhigh, fill=FluxType), alpha = 0.2, colour = NA) +
                     facet_grid(.~Subgraph) +              
                     geom_hline(yintercept=0) +
                     xlab("Year") + ylab(ylab) +
                     ylim(ylim[1], ylim[2]) + 
                     labs(fill = "Type of flux", col="Type of flux") + 
                     scale_colour_manual(values = myColFlux) +
                     scale_fill_manual(values = myColFlux) + 
                     MyTheme +
                     theme(legend.position="bottom")
   
   
   if(!("Historical" %in% levels(AllFlux4graph$Subgraph))) {
     graph <- graph + scale_x_continuous(limits = c(StartYr, NA))
   }
   
   if("Historical" %in% levels(AllFlux4graph$Subgraph)) { # start prospective
     # ie when no more logging in the past (no more logistic activities)
     if (typeGraph=="cumulative") {
        temp <- AllFlux4graph[Subgraph=="Historical" & FluxType=="Logistic activities"]
        temp[ , diff := Median - shift(Median)]    
        StSim <- temp[diff==0 & !(Yr%in% c(1975,1976)), min(Yr)] # because no logging in 1975 and 1976
     }
     if (typeGraph=="annual") {
       StSim <- AllFlux4graph[Subgraph=="Historical" & FluxType=="Logistic activities" &
                                !(Yr%in% c(1975,1976)) & Median==0, # because no logging in 1975 and 1976
                               min(Yr)] 
     }
     graph <- graph + geom_vline(xintercept = StSim, linetype = "dashed") 

   } 
   
   if ("Plantation (prospective)" %in% levels(AllFlux4graph$Subgraph)) {# to add a dashed line when first plantation are harvestable
     if("Historical" %in% levels(AllFlux4graph$Subgraph)) {
      StPlant <- StSim + PlantDurationObs
      } else {
      StPlant <- AllFlux4graph[, unique(min(Yr))] + PlantDurationObs
      }
     graph <- graph + geom_vline(xintercept = StPlant, linetype = "dotted") 
    } 
                   
   return(graph)
 }
 