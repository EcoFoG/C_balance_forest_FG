### Calculation of all emissions from plantation activities 
#################################

# gives the C emited in tonnes C / ha for all activities, distinguising
# CplantAct : all activities but wood transport (planning, felling, staff transport, preparation, plantation)
# CplantTrans : only loading and transport of wood (BO et BE)

PlantAct <- function(dataPlantation, EmisPlanPlant, EmisDefor, EmisTransport, 
                     EmisLoading, EmisStack, EmisPrepPlant, EmisPlantClean,
                     DistTimbPlant, DistFuelPlant,
                     defor = TRUE) { # do we need to deforest to plant ? default yes 
  
      # calculate t of green wood (timber and fuelwood) to transport for each year per ha
      dataPlantation$Fuel <- dataPlantation[, Cfuel / (Ccontent * (1-H))]
      dataPlantation$Timb <- dataPlantation[, Csawmill / (Ccontent * (1-H))]
      
      dataPlantation$CplantAct <- numeric()
      dataPlantation$CplantTrans <- numeric()
      
      # CplantAct in tonnes C
        # For year of 1st plantation : deforestation + preparation + plantation  
          # if there is deforestation before planting
          if (defor==TRUE) {
            dataPlantation[YearType=="P" & PlantCycle==1, CplantAct:= (EmisPlanPlant + # planning and supervion of deforestation
                                                                         EmisDefor * (Fuel+Timb) + # tree felling
                                                                         EmisPrepPlant +  # preparation plantation
                                                                         EmisPlantation) /1000] # plantation itsefl
          }
          # if there is no deforestation before planting
          if (defor==FALSE) {
            dataPlantation[YearType=="P" & PlantCycle==1, CplantAct:= (EmisPrepPlant + EmisPlantation) /1000]  
          }
        
        # For other year of plantation (except the first one) preparation + plantation  
        dataPlantation[YearType=="P" & PlantCycle!=1, CplantAct:= (EmisPrepPlant + EmisPlantation) /1000]       
        # For cleaning (thinning with wood export)
        dataPlantation[YearType=="T" & Cfuel==0, CplantAct:= EmisPlantClean/1000]
        # For thinning (when wood is exported)
        dataPlantation[YearType=="T" & Cfuel!=0, CplantAct:= EmisDefor * (Fuel+Timb) /1000] # tree felling
        # For harvest 
        dataPlantation[YearType=="H", CplantAct:= EmisDefor * (Fuel+Timb) /1000] # tree felling
        # fill NA with 0
        dataPlantation[is.na(CplantAct), CplantAct:=0]
      
      # CplantTrans for transport of wood
        dataPlantation[, CplantTrans := (EmisTransport * (Timb  * DistTimbPlant + Fuel * DistFuelPlant) +
            EmisLoading * (Timb + Fuel))/1000]
        
        # remove uneeded colums
        dataPlantation[, c("Fuel","Timb") := NULL]
  
  return(dataPlantation)
  
}
