# Deforestation for logging road, main skid trails and logging decks 
###################################################################

# This function gives Lroad, Ltrail, Wroad, Wtrail, Arearoad, Areatrail, Croad, 
# Ctrail, Cdeck, CDeforFuel, CDeforResid

DeforNatFor <- function(LogInt, CdamFuel, ACS0, CoeffRroad, CoeffWroad, WDfuel,
                        FuelRoadUnit, H, type,
                        determ = FALSE) { # for sensitivity test (if TRUE)
  
  if (type=="PastLog") {FuelRoadUnit <- 0} # for historical no fuelwood from roads
  VolFuel <- (1/WDfuel) * ((CdamFuel / Ccontent) + (1-H) * FuelRoadUnit) # Fuelwood from damage + from Road
  
  # for test sensitivity, get all post at max likelihood
  if (determ==TRUE) {
    CoeffRroad <- CoeffRroad[lp__==max(lp__),]
    CoeffWroad <- CoeffWroad[lp__==max(lp__),]
  }
  
  # draw a set of coeff for Rroad and a set for Wroad (draw an row)
  iRroad <- sample(1:dim(CoeffRroad)[1],length(ACS0),replace = TRUE)
  iWroad <- sample(1:dim(CoeffWroad)[1],length(ACS0),replace = TRUE)
  
  # get params model Rroad
  phi <- CoeffRroad[iRroad,"phi", with=FALSE]
  alpha <- CoeffRroad[iRroad,"alpha", with=FALSE]
  beta <- CoeffRroad[iRroad,"beta", with=FALSE]
  ARroad <- as.vector(as.matrix((1/(1 + exp(-(alpha + beta * log(LogInt + VolFuel))))) * phi)) 
  BRroad <- as.vector(as.matrix((1- (1/(1 + exp(-(alpha + beta * log(LogInt + VolFuel)))))) * phi))
  ParamRroad <- data.table(A=ARroad, B=BRroad, row= 1: length(ARroad))
  
  # get params model Wroad
  ParamWroad <- data.table(mean=as.vector(as.matrix(CoeffWroad[iWroad,"alpha", with=FALSE] + 
                             CoeffWroad[iWroad,"beta", with=FALSE] * (LogInt + VolFuel))),
                           sd = as.vector(as.matrix(CoeffWroad[iWroad,"sigma", with=FALSE])),
                           row= 1: length(ARroad))
  
  # fill in the result table
  if (determ==FALSE) {
    dataDefor <- data.table(Laccess = runif(length(ACS0), min=28,max=33),
                            Rroad = ParamRroad[, rbeta(1, shape1= A, shape2= B), by=.(as.factor(row))]$V1,
                            Wroad = ParamWroad[, rnorm(1, mean=mean, sd=sd), by=.(as.factor(row))]$V1, # in m
                            Wtrail = rnorm(length(ACS0), mean = 5.0000383, sd = 0.0019279)) # in m
    f50 <- rbeta(dim(dataDefor)[1], 15.10, 15.32) # f50 of Piponiot 2016 paper : the proportion of ACS in tree with DBH < 50cm
    dataDefor$Adeck <- rlnorm(dim(dataDefor)[1], meanlog = -6.12, sdlog=0.58) # in ha/logged ha, cf Piponiot 2016 paper
  }
  if (determ==TRUE) { # for test sensitivity, take the mode
    if(unique(ParamRroad$A) <=1) {stop("function DeforNatFor: for sensitivity analysis, A should be >1")}
    if(unique(ParamRroad$B) <=1) {stop("function DeforNatFor: for sensitivity analysis, B should be >1")}
    dataDefor <- data.table(Laccess = 0.5*(28+33),
                            Rroad = ParamRroad[, (A-1)/(A+B-2), by=.(as.factor(row))]$V1,
                            Wroad = ParamWroad[, unique(mean), by=.(as.factor(row))]$V1, # in m
                            Wtrail = 5.0000383) # in m
    f50 <- (15.10-1)/ (15.10+15.32-2) # f50 of Piponiot 2016 paper : the proportion of ACS in tree with DBH < 50cm
    dataDefor$Adeck <- exp(-6.12-0.58) # in ha/logged ha, cf Piponiot 2016 paper
  }
  
  dataDefor[, `:=` (Lroad = Laccess * Rroad, # in m
                    Ltrail = Laccess * (1-Rroad))] # in m
  dataDefor[, `:=` (Aroad = Lroad * Wroad/10000, # in ha/logged ha
                    Atrail = Ltrail * Wtrail/10000)]  # in ha/logged ha
                    
  dataDefor[, `:=` (Croad = ACS0 * Aroad, # in t/ha
                    Ctrail = ACS0 * Atrail * f50, # one value of f50 per plot
                    Cdeck =  ACS0 * Adeck, # in t/ha
                    CDeforFuel = (1-H) * FuelRoadUnit * Ccontent)] # in t/ha
  dataDefor[, `:=` (CDeforResid = Croad + Ctrail + Cdeck - CDeforFuel)] # in t/ha
  dataDefor[,c("Laccess", "Rroad", "Wroad", "Wtrail"):=NULL]# remove the column that we don't need
  return(dataDefor)
}


