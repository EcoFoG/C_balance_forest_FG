##### Code to input all data and functions
##########################################################

# This not a function but a code to source.
# It loads all the data, all the functions and do some primary calculation.
# The input data relative to scenarios are not loaded here

rm(list=ls()) # clear environment


### Load libraries ###############################################################
  library(data.table)
  library(truncnorm)
  library(ggplot2)
  library(cowplot)
  library(grid)
  library(gridExtra)
###################################################################################


### Load all the function #########################################################
  source(file="02_functions/PlantPlan.R")
  source(file="02_functions/DeforPlant.R")
  source(file="02_functions/PlantTraj.R")
  source(file="02_functions/PlantTrajAll.R")
  source(file="02_functions/PlantAct.R")
  source(file="02_functions/PlantGrowthCum.R")
  source(file="02_functions/LoggingPlan.R")
  source(file="02_functions/Damage.R")
  source(file="02_functions/DeforNatFor.R")
  source(file="02_functions/LogAct.R")
  source(file="02_functions/Recovery.R")
  source(file="02_functions/Regeneration.R")
  source(file="02_functions/Sawmill.R")
  source(file="02_functions/BiomassPlant.R")
  source(file="02_functions/DecayResid.R")
  source(file="02_functions/ActCum.R")
  source(file="02_functions/MAIN_runScenario.R")
  source(file="02_functions/GetResultFlux.R")
  source(file="02_functions/graphFlux.R")
  source(file="02_functions/graphQt.R")
  source(file="02_functions/get_VolPlantExp.R")
  source(file="02_functions/Run1PlotNatFor.R")
  source(file="02_functions/Run1PlotPlantation.R")
###################################################################################


### Data on scenarios #############################################################
dataScenario <- data.table(read.csv("01_data/Scenarios.csv", header=TRUE)) 
###################################################################################

### Data on wood ###################################################################
  Ccontent <- 0.47 # C content of AGB 
  H <- 0.45 # water content of green wood (% of humidity)
  WDtimber <- 0.736 * 0.828 # WD of timber in natural forest (from three more logged species, weighted  Piponiot et al 2016 
  # * 0.828 to convert from 12% to 0% humidity (cf papier Vielledent)
  WDfuel <- 0.73 * 0.828 # WD for fuelwood (from all species, agreed at GFClim WP2 comitee)
  # Lower heating value for Natural Forest (average of Guyaneese species)
  PCIdryNatFor <- 18.8 # PCI of dry wood (in MJ/kg of wood) (PCI = pouvoir calorifique inférieur)
  # get PCI of green wood and change unit (cf rapport Pinta Cirad, 2011 p7)
  PCIhNatFor <- (PCIdryNatFor*1000 * (1-H) - 2443 *H) *0.277778 # in kWh/t with PCIdry in MJ/kg
  ### Wood density and Lower heating value for the 7 values taken for the production model in plantation
  Data_wood_plantation <- data.table(read.csv("01_data/data_wood_plantation.csv"))
  # get PCI of green wood and change unit (cf rapport Pinta Cirad, 2011 p7)
  Data_wood_plantation$PCIhPlant <- (Data_wood_plantation$PCIdryPlant*1000 * (1-H) - 2443 *H) *0.277778 # in kWh/t with PCIdry in MJ/k (0.27777 to convert form kJ/kg to kWh/t)
###################################################################################


### For Natural Forest ############################################################
  # Harvesting of fuelwood as byproduct of deforestation for Road 
  FuelRoadUnit <- 5 # tons of green fuelwood from byproduct from Road only, per ha in natural forest (t/ha) (at H45%)
  # NB will change to 0 if no use of byproduct as fuelwood
  
  # C emissions related to sylvicultural activities (data from ONF Balata-Saut Léodate)
  EmisPlanning <- 0.03 # emission due to planning in kg C /ha 
  EmisRoadBuild <- 0.53 # emissions due to road builind in kg C / m road
  EmisLogging <- 2.55 # emissions due to loggong in kg C / tonnes of green wood (BE and BE )
  
  # data on logging_units obtained for all ONF logging units that have not been logged 
  dataLogUnit <- data.table(read.csv("01_data/dataLogUnit.csv", header=TRUE))
  
  # Coeff model proportion of area logged / total area
  CoeffPropLogged <- data.table(read.csv("01_data/CoeffPropLogged.csv", header=TRUE)) 
  alpha_LoggingPlan <- CoeffPropLogged$alpha
  beta_LoggingPlan <- CoeffPropLogged$beta
  
  # Coeff models of damage
  CoeffDam <- data.table(read.csv("01_data/Coeff_model_damage.csv", header=TRUE)) # main model of damage
  CoeffDam2FuelCL <- data.table(read.csv("01_data/resModDam2FuelCL.csv", header=TRUE)) # table of coeff (only the mean) for model giving the % of damage that can be harvested as fuelwood for CL
  CoeffDam2FuelRIL <- data.table(read.csv("01_data/resModDam2FuelRIL.csv", header=TRUE)) # table of coeff (only the mean) for model giving the % of damage that can be harvested as fuelwood for RIL
  # Coeff beta distribution for RIL
  CoeffRIL <- data.table(read.csv("01_data/Coeff_RIL.csv", header=TRUE))
  
  # Coeff for the models of deforestation during logging in natural forests
  CoeffRroad <- data.table(read.csv("01_data/Coeff_model_Rroad.csv", header=TRUE)) # table of coefficiant of Rroad model
  CoeffWroad <- data.table(read.csv("01_data/Coeff_model_Wroad.csv", header=TRUE)) # table of coefficiant of Wroad model
  
  # Coeff model of C recovery (load for 2 methods)
  CoeffRecov_m1 <- data.table(read.csv("01_data/Coeff_recov_Method1.csv", header=TRUE, row.names = 1)) # table of coeff of recovery model 
  CoeffRecov_m2 <- data.table(read.csv("01_data/Coeff_recov_Method2.csv", header=TRUE, row.names = 1)) # table of coeff of recovery model 
  # value for standardisation covariates model recovery
  standardiseRecov <- read.csv("01_data/standardiseRecov.csv", header=TRUE)
  
  # Coeff of model of regeneration on deforested area
  CoeffRege <- data.table(read.csv("01_data/Coeff_model_Rege.csv", header=TRUE)) # table of coefficient for the regeneration model built on Arbocel
  
###################################################################################  
  
  
### For plantation ################################################################

  # Technical itinery
  # PlantDuration <- 30 #  the time before final harvest in plantation (yr) 
  NThin <- 3 # number of thinning in plantation 
  
  # Parameters for plantation trajectories
  Tfuel <- 10 # year from which the fuelwood harvest starts (when worth it regarding DBH) => ok, expert knowledge, given by Laurent
  T10 <- 10 # year at which the planted tree exceed 10cm DBH (and therefore have LWN) => expert knowledge
  RCub <- 0.730 # fraction of biomass of harvested tree in plantation that is timber 
  RfuelRemn <- 0.220 # fraction of biomass of an harvest tree that goes to fuelwood (RCub + RfuelRemn = RfuelThin) 
  RfuelThin <- RCub + RfuelRemn # fraction of biomass of thinned tree that is used as fuelwood 
  RLWN <- RCub + RfuelRemn # part of a tree with diamter >=10 cm
  # RCub, RfuelRemn, RfuelThin, RLWN are taken from original value of ForesTreeCulture 1 => 30-year old tree measured in Paracou plantation (pers.com Nicolini)
  Rprod <- 0.5372 # ratio AGBthin/AGBend = Volthin/Volend = ACSthin/ACSend (taken as mean from observed values in production table)
  gammaPlantTraj <- 0.1 # exponant of power relationship to get Rthin
  if (gammaPlantTraj >1) {stop("gammaPlantTraj cannot be more than 1")} #gammaPlantTraj needs to be <1 to get more than what would have died in a thinning year
  # AGBendPlant of plantation 
  AGBendPlant <- data.table(read.csv("01_data/Coeff_AGBendPlant.csv", header=TRUE))
  colnames(AGBendPlant) <- c("theta", "sigma", "lp__")


    # C emissions related to plantation activities 
  EmisPlanPlant <- 0.03 # planning plantation : kg C /ha same than natural forest when deforesting to plant
  EmisDefor <- 2.55 # deforestation : kg C / tonnes of green wood (BE and BE ) same than natural forest
  EmisPrepPlant <- 35.04 # kg C/ha preparation plantation (staff transport + gyrobroyage + sous-solage) cf fichier excel sur base tps donnés par Laurent et conso solicaz
  EmisPlantation <- 39.33 # kg C/ha, which includes only the transport of staff to plant cf fichier excel sur base tps donnés par Eric
  EmisPlantClean <- 9.41 # kg C/hacleaning plantation in early years (staff transport + gyrobroyage) cf fichier excel sur base tps donnés par Laurent et conso solicaz
  
  #### data on plots for plantation (plots that have already been logged
  dataPlotforPlant <- data.table(read.csv("01_data/dataPlot4Plant.csv", header=TRUE))

  
  ### Quantities of wood that can be valorised when deforestation for plantation:load coeff
  CoeffBOdefor <- data.table(read.csv("01_data/resModBOdefor.csv", header=TRUE)) # Timber
  CoeffBEdefor <- data.table(alpha=50, beta=50) # Fuelwood
  
###################################################################################  
  
  
  
### For Past logging #############################################################
  # get data Past logging
  dataPastLogging  <- data.table(read.csv("01_data/dataPastLogging.csv", header=TRUE))
  # get tfirstbioPlant the year the first biomass plant has started (to start sendind sawndust to the biomass plant)
  tfirstbioPlant <- 2009 # (cf https://www.afd.fr/fr/energies-renouvelables-la-guyane-valorise-sa-biomasse)
###################################################################################  
  
  
  
### Transport #####################################################################
  # Transport distance 
  DistTimbNatFor <- 150 # km (distance between forest and sawmill)
  DistFuelNatFor <- 150 # km (distance between forest and biomass plant)
  DistTimbPlant <- 100 # km (distance between plantation and sawmill)
  DistFuelPlant <- 100 # km (distance between plantation and biomass plant)
  DistMillBioPlant <- 0  # km (distance between sawmill and biomass plant)
  
  # Emissions due to transport 
  EmisTransport <- 0.02 # emission due to transport onlyt in kg C /tonnes of green wood and km of transport (data from ONF Balata-Saut Léodate)
  EmisLoading <- 0.72 # emission due to loading onto trucks in kg C / tonnes of green wood (BE and BE) (data from ONF Balata-Saut Léodate)
###################################################################################

  
### Sawmill ########################################################################
  Rwaste <- 0.1 # proportion of waste in the sawmill
  # Esawmill <- 0.3 # efficiency of sawmill (default value of argument in )
  # RsawnAlt <- 0.4 # proportion of sawnwood used instead of another material (leads to avoided emission) (default value of argument in )
  # 0.4 chosen using the table 3 in the study of Vernay 2009 => 1/2 of sawnwood used as structure)
  CoeffDF <- data.table(read.csv("01_data/Coeff_avoidedSawmill.csv", header=TRUE)) # Displacement factor of use for use of sawnwood instead of another material
###################################################################################

  
### Biomass plant ###################################################################
  # EbiomassPlant <- 0.22 # efficiency of biomass plant (default value of argument in )
  Cfossil <- 883*12/(44*1000) # C emission from energie production from fossil fuel (in kgC/ kWh) source  PRERURE (p72) converted
  EmisBiomassPlant <- 2.09/1000 # t C / tonnes of green wood : emission from wood chipping and storage of wood at the biomass plant (data from ONF Balata-Saut Léodate)
###################################################################################  
  

### Decay of residual necromass left on site #######################################
  CoeffDecayLWB <- data.table(read.csv("01_data/Coeff_LWB.csv", header=TRUE, row.names = 1)) # decay model
 
  