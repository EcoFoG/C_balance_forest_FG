### Calculation of all emissions from logging activities 
# (planning, road building, logging, transport)
#################################

# gives the C emited by 4 types of emission related to logging (planning, road building, logging and transport)
# in tonnes C / ha
# as a function of Lroad (and qt of Timber and Fuelwood produced)
# !! excludes all C in AGB


LogAct <- function (EmisPlanning, EmisRoadBuild, EmisLogging, EmisTransport, EmisLoading, Lroad, 
                    Cdamfuel, DistTimbNatFor, DistFuelNatFor, H, Ccontent, FuelRoadUnit,
                    WDtimber , LogInt, type) {
  if (type=="PastLog") {FuelRoadUnit <- 0} # for historical no fuelwood from roads
  Fuel <- (Cdamfuel / (Ccontent * (1-H))) + FuelRoadUnit
  Timb <- WDtimber * LogInt / (1-H)
  CLogAct <- data.table(Cplanning = rep(EmisPlanning /1000, length(Lroad)), # in tonnes/ha
                        CroadBuild = EmisRoadBuild * Lroad /1000, # in tonnes/ha
                        CLogging = EmisLogging * (Timb + Fuel) /1000, # in tonnes/ha
                        CTrans = (EmisTransport * (Timb  * DistTimbNatFor + Fuel * DistFuelNatFor) +
                          EmisLoading * (Timb + Fuel))/1000 ) # in tonnes/ha
return(CLogAct)
}