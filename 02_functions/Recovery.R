### Function Recovery - cumulative balance of C recovering from logging in Natural forest
################################################################

# This function give the cumulative C balance due to recovery accross all logging unit 
# as a data.table with row = iteration and colum = year of simulation
# There are two possibilities for the model used by this function.


# with a single flux Call and a accumulation model with an inflexion point (covariates on the inflexion point)
Recovery <- function (method, data, CoeffRecov ,standardiseRecov, Ccontent, 
                      Tm, TmPastLeft=NULL,
                      determ = FALSE) { # for sensitivity test (if TRUE)
  
  if(!method%in%c(1,2)) {
    stop("The method for recovery should be 1 or 2")}
  
  # standardise the covariates
  dataPlotStandard <- 
    data.table(data[,.(iter, AreaLogged, AGB0, DisturbInt)],
               DisturbIntSt = (data$DisturbInt*100 - standardiseRecov$loss[1])/standardiseRecov$loss[2],
               ACSinit = data$AGB0 * Ccontent,
               ACS0St = (data$AGB0 * Ccontent - standardiseRecov$acs0[1])/standardiseRecov$acs0[2],
               MAPSt = (data$MAP -  standardiseRecov$prec[1])/standardiseRecov$prec[2],
               RainSeasSt = (data$RainSeas -  standardiseRecov$seas[1])/standardiseRecov$seas[2],
               BulkDensSt = (data$BulkDens - standardiseRecov$bd[1])/standardiseRecov$bd[2]) 
  
  
  if (method==2) {
    # get the sd of alphamax
    PostalphamaxP <- CoeffRecov[, 12:144, with=FALSE] 
    sdAlphamaxRecov <- PostalphamaxP[ , lapply(.SD, sd)]
    sdAlphamaxRecov <- data.table(plot=colnames(sdAlphamaxRecov), sd= t(sdAlphamaxRecov))
    sdAphamaxRecov <- median(sdAlphamaxRecov$sd.V1)
    rm(PostalphamaxP)
  }
  
  # for test sensitivity, get all post at max likelihood
  if (determ==TRUE) {
    CoeffRecov <- CoeffRecov[lp__==max(lp__),]
  }
  
  # get a set of parameter for each plot and each iterations
  paramRecov <- CoeffRecov[sample(.N, dim(data)[1], replace=TRUE)]
  paramRecov$row <- 1: dim(data)[1]
  dataPlotStand <- cbind(dataPlotStandard, paramRecov)
  
  # Get coefficient theta 
  if (determ==FALSE) {
    dataPlotStand$theta <- dataPlotStand[, rtruncnorm(1, a=0,
                                                      mean= theta_0 + lambda_loss * DisturbIntSt + 
                                                        lambda_acs0 * ACS0St  +
                                                        lambda_prec * MAPSt + lambda_seas * RainSeasSt +
                                                        lambda_bd * BulkDensSt,
                                                      sd=sd_theta), by=.(as.factor(row))]$V1 
  }
  if (determ==TRUE) { # for test sensitivity, take the mode
    dataPlotStand$theta <- dataPlotStand[, theta_0 + lambda_loss * DisturbIntSt + 
                                           lambda_acs0 * ACS0St  +
                                           lambda_prec * MAPSt + lambda_seas * RainSeasSt +
                                           lambda_bd * BulkDensSt]
    if(unique(dataPlotStand$theta) <0) {stop("function Recov: for sensitivity analysis, theta should be >0")}
  }
  

  # calcutate ACSmin
  dataPlotStand$ACSmin <- dataPlotStand[, ACSinit*(1-DisturbInt)]
  
  
  
  #### Method 1 #############################################################################
  if (method==1) {
      # drop columns not needed
      dataPlot <- dataPlotStand[,.(iter, AreaLogged, AGB0, DisturbInt, ACSinit, ACSmin,
                                   theta, beta, gamma, sigma, row)]
      # calculate the cumulative flux per ha (per each plot) (t/ha) (take the opposite because storage => negative)
      balanceRecov <- data.table(iter=dataPlot$iter,
                                 AreaLogged=dataPlot$AreaLogged,
                                 dataPlot[, -(ACSinit-ACSmin) * (1-exp(-beta * ((Tm/theta)^gamma)))])
  }
  ###########################################################################################
  
  
  #### Method 2 #############################################################################   
  if (method==2) {
    
    # drop columns not needed
    dataPlot <- dataPlotStand[,.(iter, AreaLogged, AGB0, DisturbInt, ACSinit, ACSmin,
                                 theta, beta, gamma, sigma, row)]
    # get estimates alphamax
    if (determ==FALSE) {
      dataPlot$alphamaxEst <- dataPlot[, rtruncnorm(n = 1, a=0, 
                                                    mean = ACSinit - ACSmin,
                                                    sd = sdAphamaxRecov),
                                       by=.(as.factor(row))]$V1
    }
    if (determ==TRUE) { # for test sensitivity, take the mode
      dataPlot$alphamaxEst <- dataPlot[, ACSinit - ACSmin]
      if(unique(dataPlot$alphamaxEst) <0) {stop("function Recov: for sensitivity analysis, alphamaxEst should be >0")}
    }

    # get a single epsilon per plot*iteration
    if (determ==FALSE) {
      dataPlot$epsilon <- dataPlot[, rnorm(1, mean= 0 ,sd=sigma), by=.(as.factor(row))]$V1
    }
    if (determ==TRUE) { # for test sensitivity, take the mode
      dataPlot$epsilon <- 0
    }
    
    # calculate the cumulative flux per ha (per each plot) (t/ha) (take the opposite because storage => negative)
    balanceRecov <- data.table(iter=dataPlot$iter,
                               AreaLogged=dataPlot$AreaLogged,
                               dataPlot[, -exp(log(alphamaxEst * (1-exp(-beta * ((Tm/theta)^gamma)))) + epsilon)])
  }
  ###########################################################################################    
  
  
  # get balanceRecov for the full area
  ColTime <- colnames(balanceRecov)[-c(1,2)] # names of the colum related to time
      
  if(missing(TmPastLeft)) { # case of dataNatFor and for past when there is not plantation
    balanceRecov[, (ColTime) := lapply(.SD, function(x) x * balanceRecov[['AreaLogged']]), 
                 .SDcols = ColTime] # multiply all column of time by the AreaLogged of the row
  } else { # case of dataPastLog with plantation
    balanceRecov[, (ColTime) := lapply(.SD, function(x) x * balanceRecov[['AreaLogged']]), 
                  .SDcols = ColTime] # multiply all column of time by the AreaLogged of the row 
    balanceRecov <- data.table(balanceRecov[,c(1,2), with=FALSE],
                                balanceRecov[,-c(1,2), with=FALSE] * TmPastLeft) # multiply by the area left (ie not converted to plantation TmPastLeft)
  }
      
  # calculate the total across plots (in t) for each iteration
  Crecov <- balanceRecov[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter), .SDcols = ColTime]
  Crecov[, iter:=NULL]
    
  return(Crecov)
} 



