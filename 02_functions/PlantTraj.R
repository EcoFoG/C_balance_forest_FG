##### Trajectory of a single plantation
###################################################


PlantTraj <- function(PlantDuration, NThin, AGBendPlant, gammaPlantTraj, Rprod, RLWN,
                      Tfuel, T10,
                      ChangeProd, # change Productivity if wanted
                      determ = FALSE){ # for sensitivity test of PlantTraj(if TRUE)  
  
  # no more than a thining every 2 year
  if (NThin > PlantDuration/2) {stop("No more than one thining every two year")}
  
  # for test sensitivity, get all post at max likelihood
  if (determ==TRUE) {
    AGBendPlant <- AGBendPlant[lp__==max(lp__),]
  }
  
  # get year of thinning
  ListYrThin <- round(PlantDuration/(NThin+1)*1:(NThin+1)) # give year of thinning (including the last one, so do +1 to be able to change this year latter)
 
  # calculate ACSend and slopeACSend
  nrow <-sample(1:dim(AGBendPlant)[1],1) # draw a row number in the vector of posterior of slope
  if (determ==FALSE) {
    ACSend <- Ccontent * rlnorm(1, log(AGBendPlant[nrow,theta]*PlantDuration), AGBendPlant[nrow,sigma]) # ! result in C, no longer in AGB
  }
  if (determ==TRUE) { # for test sensitivity, take the mode
    ACSend <- Ccontent * exp(log(AGBendPlant[nrow,theta]*PlantDuration) - AGBendPlant[nrow,sigma]^2)
  }
  
  if(!is.null(ChangeProd)) {
    ACSend <- ACSend * ChangeProd
  }
  slopeACSend <- ACSend/PlantDuration
  
  # calculate Rthin, the prop of (ACSdiff - dead in years before plantation) corresponding to tree thinned (as opposed to dead on the plot)
  Rthin <- (NThin/PlantDuration)^sample(gammaPlantTraj, 1)
  
  # calculate ACSdiff
  ACSdiff <- (ACSend * Rprod) / ((1-(ListYrThin[1] / PlantDuration))* Rthin) 

  # create a data frame to store results
  Plantation <- data.table(trelat=1:PlantDuration, # trelat is NOT the time of plantation it is the time since the plantation
                           # so it the time when the corresponding C fluxes in columns are started
                           # for instance, for decay, trelat is the starting point of decay
                           YearType = as.factor("N"), # N mortality only, T thinning , H harvest
                           Csawmill = 0, # C going to sawmill
                           Cfuel = 0, # C going to biomass plant
                           CFWN = 0, # C in fine woody necromass left to decay
                           CLWN = 0, #  C in large woody necromass left to decay
                           Cmortality =0, # C in trees that die this year (not cumulative)
                           Cthin = 0, # C in  trees thinned this year (not cumulative)
                           Calive= 0) # C stock (cumulated) in tree that will be harvested (ie not die before harvest or be thinned)
  # fill in the type of year
  Plantation[ListYrThin, ]$YearType <- "T" # year of thinning
  Plantation[PlantDuration,]$YearType <- "H" # year of harvest (final year)
  
  # Fill in Plantation
  Plantation$Calive <- slopeACSend*Plantation$trelat # cumulative NPP in C
  Plantation$Cmortality <-  ACSdiff / PlantDuration # annual amount of C that would die naturally if no thinning
  Plantation[YearType=="T", Cthin:=(Rthin/NThin) * (ACSdiff - (ACSdiff * ListYrThin[1] / PlantDuration))] # amount of C thinned in a thinning year
  
  # change mortality for post thining years untill reached the amount thinned
  j <- floor(Plantation[ListYrThin[1],Cthin/Cmortality]) # number of year having a null mortality after a thining (I have taken than out of the loop because otherwise problem when the moratlity is reduce in the year before next thining)
  CthinUsual <- Plantation[ListYrThin[1],Cthin] # C in a thinning (idem Cthin but out of the table so no problem whatever happens in the table)
  Cmort <- Plantation[ListYrThin[1],Cmortality]
  for (i in ListYrThin[-length(ListYrThin)]+1) { # all year directlly following thining
    Plantation[i+j]$Cmortality <- Plantation[i+j]$Cmortality * (1 - ((CthinUsual/Cmort)-j)) 
    Plantation[i:(i+j-1),]$Cmortality <- 0
  }
  
  # fill in CFWN, CLWN and Cfuel
  Plantation[trelat < T10, CFWN:=(Cthin+Cmortality)] # before trees reach 10 cm DBH all wood is FWN
  Plantation[trelat >= T10 & trelat <Tfuel, CLWN:= RLWN*(Cmortality+Cthin)] # when there is LWB but not yet fuelwood
  Plantation[trelat >= T10 & trelat <Tfuel, CFWN:= (Cmortality+Cthin) - CLWN]
  Plantation[trelat >= Tfuel, Cfuel:=RfuelThin*(Cthin)] # extracted for fuelwood from thinning
  Plantation[trelat >= Tfuel, CLWN := RLWN * (Cmortality + Cthin * (1-RfuelThin))] # LWN from what is left on site (except in year of harvest)
  Plantation[trelat >= Tfuel, CFWN := (1-RLWN) * (Cmortality + Cthin * (1-RfuelThin))] # # FWN from what is left on site (except in year of harvest)
  # year of harvest (in these year all the wood left on site is FWN because shredded to prepare next plantation)
  Plantation[trelat==PlantDuration, Csawmill:= RCub*Calive]
  Plantation[trelat==PlantDuration, Cfuel:= RfuelRemn * Calive]
  Plantation[trelat==PlantDuration, CLWN := 0] # no LWN because all is shreded
  Plantation[trelat==PlantDuration, CFWN:= Calive + Cmortality - Csawmill -Cfuel]

  # Cgrowth : Cstored in this year (before taking away what is emitted by mortality or thining)
  # ie it's the ACSall
  Plantation$Cgrowth <- (ACSdiff + ACSend) / PlantDuration
  
  return(Plantation) 
}   

  
  
###### OLD before changing Rprod


# # NPPplant is the slope of NPP (2 columns value and errors) from model Bruno in AGB
# # Rprod is the ratio ACSall/ACSend
# # gammaPlantTraj (vector) exponant of power relationship to get Rthin 
# # Rthin = (NThin/PlantDuration)^gammaPlantTraj: proportion of the ACSdiff that is obatined from thinning
# # gammaPlantTraj needs to be <1 to get more than what would have died in a thinning year
# 
# 
# PlantTrajOLD <- function(PlantDuration, NThin, AGBendPlant, Rprod, gammaPlantTraj) {
#   

#   # (doesn't take DBH into account). This is also the amount of wood that can go to fuelwood.
#   # no more than a thining every 2 year
#   if (NThin > PlantDuration/2) {stop("No more than one thining every two year")}
#   # difference between AGBall and AGBend at the harvest time
#   nrow <-sample(1:dim(AGBendPlant)[1],1) # draw a row in the vector of posterio of slope
#   slopeACSend <- Ccontent * rlnorm(1, log(AGBendPlant[nrow,theta]), AGBendPlant[nrow,sigma]) # ! result in C, no longer in AGB
#   ACSdiff <- PlantDuration*slopeACSend* (Rprod-1) # ACSall (being PlantDuration*slopeNPP*Rprod) minus ACSend (PlantDuration*slopeACSend)
#   # prop of (ACSdiff - dead in year before plantation) corresponding to tree thinned (as opposed to dead on the plot)
#   Rthin <- (NThin/PlantDuration)^sample(gammaPlantTraj, 1)
#   # create a data frame to store results
#   Plantation <- data.table(trelat=1:PlantDuration, # trelat is NOT the time of plantation it is the time since the plantation
#                            # so it the time when the corresponding C fluxes in columns are started
#                            # for instance, for decay, trelat is the starting point of decay
#                           YearType = as.factor("N"), # N mortality only, T thinning , H harvest
#                            Csawmill = 0, # C going to sawmill
#                            Cfuel = 0, # C going to biomass plant
#                            CFWN = 0, # C in fine woody necromass left to decay
#                            CLWN = 0, #  C in large woody necromass left to decay
#                            Cmortality =0, # C in trees that die this year (not cumulative)
#                            Cthin = 0, # C in  trees thinned this year (not cumulative)
#                            Calive= 0) # C stock (cumulated) in tree that will be harvested (ie not die before harvest or be thinned)
#    ListYrThin <- round(PlantDuration/(NThin+1)*1:(NThin+1)) # give year of thinning (including the last one, so do +1 to be able to change this year latter)
#    Plantation[ListYrThin, ]$YearType <- "T" # year of thinning
#    Plantation[PlantDuration,]$YearType <- "H" # year of harvest (final year)
#   
#   # Fill in Plantation
#   Plantation$Calive <- slopeACSend*Plantation$trelat # cumulative NPP in C
#   Plantation$Cmortality <- slopeACSend*(Rprod-1) # annual amount of C that would die naturally (same as ACSdiff/PlantDuration)
#   Plantation[YearType=="T", Cthin:=(1/NThin) * Rthin * (ACSdiff - (ACSdiff * ListYrThin[1] / PlantDuration))] # amount of C thinned in a thinning year
#   Plantation[YearType=="T" & trelat >= Tfuel, Cfuel:=RLWN*Cthin]   # 0.774 for eq 15 paper Camila 2016
# 
#   # change mortality for post thining years untill reached the amount thinned
#   j <- floor(Plantation[ListYrThin[1],Cthin/Cmortality]) # number of year having a null mortality after a thining (I have taken than out of the loop because otherwise problem when the moratlity is reduce in the year before next thining)
#   CthinUsual <- Plantation[ListYrThin[1],Cthin] # C in a thinning (idem Cthin but out of the table so no problem whatever happens in the table)
#   Cmort <- Plantation[ListYrThin[1],Cmortality]
#   for (i in ListYrThin[-length(ListYrThin)]+1) { # all year directlly following thining
#        Plantation[i+j]$Cmortality <- Plantation[i+j]$Cmortality * (1 - ((CthinUsual/Cmort)-j)) # I have changed this line because the way it was previously written didn't work when the year of thinning add a reduced mortality
#        Plantation[i:(i+j-1),]$Cmortality <- 0
#   }
# 
#   Plantation[trelat < T10, CFWN:=(Cthin+Cmortality)] # before trees reach 10 cm DBH all wood is FWN
#   Plantation[trelat >= T10 & trelat <Tfuel, CLWN:= RLWN*(Cmortality+Cthin)] # when there is LWB but not yet fuelwood
#   Plantation[trelat >= T10 & trelat <Tfuel, CFWN:= (Cmortality+Cthin) - CLWN]
#   Plantation[trelat >= Tfuel, Cfuel:=RLWN*(Cthin)] # extracted for fuelwood
#   Plantation[trelat >= Tfuel, CLWN := Cmortality*RLWN] # LWN only from dead trees
#   Plantation[trelat >= Tfuel, CFWN:= (Cmortality+Cthin) - Cfuel - CLWN] # FWN from dead trees and part of trees for fuelwood
#   # year of harvest (in these year all the wood left on site is FWN because shredded to prepare next plantation)
#   Plantation[trelat==PlantDuration, Csawmill:= RCub*Calive]
#   Plantation[trelat==PlantDuration, Cfuel:= RLWN * (Calive-Csawmill)]
#   Plantation[trelat==PlantDuration, CLWN := 0] # no LWN because all is shreded
#   Plantation[trelat==PlantDuration, CFWN:= Cmortality*RLWN + # would be LWN but become FWN by shreding to prepare next plantation
#                                           (Cmortality + Calive-Csawmill) * (1-RLWN) ] # FWN from dead trees + remnant of harvested ones
#   
#   # Cgrowth : Cstored in this year (before taking away what is emitted by mortality or thining)
#   # ie it's the ACSall
#   Plantation$Cgrowth <- slopeACSend * Rprod
#     
# return(Plantation)  
# }



### Old version that was problematic for small value of gammaPlantTraj 
# because the amount taken in one thinning was more than the mortality between two thining
# I also changed the name of the GPP and NPP var because it's not actually a GPP and NPP 
# new name are ACSall and ACSend

# PlantTraj <- function(PlantDuration, NThin, NPPplant, Rprod, gammaPlantTraj) {
# 

#   # (doesn't take DBH into account). This is also the amount of wood that can go to fuelwood.
#   # no more than a thining every 2 year
#   if (NThin > PlantDuration/2) {stop("No more than one thining every two year")}
#   # difference between GPP and NPP at the harvest time
#   nrow <-sample(1:dim(NPPplant)[1],1) # draw a row in the vector of posterio of slope
#   slopeNPP <- Ccontent * rlnorm(1, log(NPPplant[nrow,theta]), NPPplant[nrow,sigma]) # ! result in C, no longer in AGB
#   ACSdiff <- PlantDuration*slopeNPP* (Rprod-1) # GPP (being PlantDuration*slopeNPP*Rprod) minus NPP (PlantDuration*slopeNPP)
#   # prop of ACSdiff corresponding to tree thinned (as opposed to dead on the plot)
#   Rthin <- (NThin/PlantDuration)^sample(gammaPlantTraj, 1)
#   # create a data frame to store results
#   Plantation <- data.table(trelat=1:PlantDuration, # trelat is NOT the time of plantation it is the time since the plantation
#                            # so it the time when the corresponding C fluxes in columns are started
#                            # for instance, for decay, trelat is the starting point of decay
#                            YearType = as.factor("N"), # N mortality only, T thinning , H harvest
#                            Csawmill = 0, # C going to sawmill
#                            Cfuel = 0, # C going to biomass plant
#                            CFWN = 0, # C in fine woody necromass left to decay
#                            CLWN = 0, #  C in large woody necromass left to decay
#                            Cmortality =0, # C in trees that die this year (not cumulative)
#                            Cthin = 0, # C in  trees thinned this year (not cumulative)
#                            Calive= 0) # C stock (cumulated) in tree that will be harvested (ie not die before harvest or be thinned)
#   ListYrThin <- round(PlantDuration/(NThin+1)*1:(NThin+1)) # give year of thinning (including the last one, so do +1 to be able to change this year latter)
#   Plantation[ListYrThin, ]$YearType <- "T" # year of thinning
#   Plantation[PlantDuration,]$YearType <- "H" # year of harvest (final year)
# 
#   # Fill in Plantation
#   Plantation$Calive <- slopeNPP*Plantation$trelat # cumulative NPP in C
#   Plantation$Cmortality <- slopeNPP*(Rprod-1) # annual amount of C that would die naturally (same as ACSdiff/PlantDuration)
#   Plantation[YearType=="T", Cthin:=ACSdiff*Rthin/NThin] # amount of C thinned in a thinning year
#   Plantation[YearType=="T" & trelat >= Tfuel, Cfuel:=RLWN*Cthin]   # 0.774 for eq 15 paper Camila 2016
#   # change mortality for post thining years untill reached the amount thinned
#   for (i in ListYrThin[-length(ListYrThin)]+1) { # all year directlly following thining
#     j <- floor(Plantation[i-1,Cthin/Cmortality]) # number of year having a null mortality after a thining
#     Plantation[i+j]$Cmortality <- Plantation[i+j]$Cmortality * (1 - (Plantation[i-1,Cthin/Cmortality]-j)) 
#     Plantation[i:(i+j-1),]$Cmortality <- 0
#   }
# 
#   Plantation[trelat < T10, CFWN:=(Cthin+Cmortality)] # before trees reach 10 cm DBH all wood is FWN
#   Plantation[trelat >= T10 & trelat <Tfuel, CLWN:= RLWN*(Cmortality+Cthin)] # when there is LWB but not yet fuelwood
#   Plantation[trelat >= T10 & trelat <Tfuel, CFWN:= (Cmortality+Cthin) - CLWN]
#   Plantation[trelat >= Tfuel, Cfuel:=RLWN*(Cthin)] # extracted for fuelwood
#   Plantation[trelat >= Tfuel, CLWN := Cmortality*RLWN] # LWN only from dead trees
#   Plantation[trelat >= Tfuel, CFWN:= (Cmortality+Cthin) - Cfuel - CLWN] # FWN from dead trees and part of trees for fuelwood
#   # year of harvest (in these year all the wood left on site is FWN because shredded to prepare next plantation)
#   Plantation[trelat==PlantDuration, Csawmill:= RCub*Calive]
#   Plantation[trelat==PlantDuration, Cfuel:= RLWN * (Calive-Csawmill)]
#   Plantation[trelat==PlantDuration, CLWN := 0] # no LWN because all is shreded
#   Plantation[trelat==PlantDuration, CFWN:= Cmortality*RLWN + # would be LWN but become FWN by shreding to prepare next plantation
#                (Cmortality + Calive-Csawmill) * (1-RLWN) ] # FWN from dead trees + remnant of harvested ones
# 
#   # Cgrowth : Cstored in this year (before taking away what is emitted by mortality or thining)
#   # ie it's the GPP
#   Plantation$Cgrowth <- slopeNPP * Rprod
# 
#   return(Plantation)
# }