### Function Regeneration - cumulative C balance of C from regeneration 
# on main skid trails and logging decks (not on logging roads)
###########################################################################"

# This function give the cumulative C balance due to regeneration accross all logging unit 
# as a data.table with row = iteration and colum = year of simulation

Regeneration <- function (data, CoeffRege, Ccontent, Tm, TmPastLeft=NULL,
                          determ = FALSE) { # for sensitivity test (if TRUE)
  
  # get the sigma of AGCmax (from posteriors of model)
  sdAGCmax <- sd(CoeffRege$AGCmax)
  
  # for test sensitivity, get all post at max likelihood
  if (determ==TRUE) {
    CoeffRege <- CoeffRege[lp__==max(lp__),]
  }
  
  # keep only data needed and get a set of parameter for each plot and each iterations
  dataRege <- cbind(data[,.(iter, AGB0)], 
                    CoeffRege[sample(.N, dim(data)[1], replace=TRUE)])
  dataRege$row <- 1: dim(dataRege)[1] # add a row ID for calculations
  dataRege$ACS0 <- dataRege$AGB0 * Ccontent
  dataRege$AreaRege <- data[, AreaLogged * (Atrail+Adeck)]
  # NB aboveground C is called AGC in model rege (in the posterior) and ACS here but it's the same
  
  # calculate ACSmaxPI (for each Plot and Iteration)
  # as the pre-logging ACS (ACS0) + error that is taken from 
  # the distribution of AGCmax posterior (to keep the uncertainty of AGCmax from the model)
  if (determ==FALSE) {
    dataRege$ACSmaxPI <- dataRege[, rnorm(1, mean=ACS0, sd=sdAGCmax),
                                  by=.(as.factor(row))]$V1
    # get epsilon for each plot*iteration
    dataRege$epsilon <- dataRege[, rnorm(1, mean=0, sd=sigma),
                                 by=.(as.factor(row))]$V1
  }
  if (determ==TRUE) { # for test sensitivity, take the mode
    dataRege$ACSmaxPI <- dataRege[, ACS0]
    # get epsilon for each plot*iteration
    dataRege$epsilon <- 0
  }

  
  # calculate the cumulative flux of C per each plot (in t) (for the whole area, not per ha) 
  # storage so negative
  balanceRege <- dataRege[, - AreaRege * exp(log(ACSmaxPI*(1-exp(-theta*Tm)))+epsilon)]
  
  if(!missing(TmPastLeft)) { # case of dataPastLog
    balanceRege <- balanceRege * TmPastLeft # multiply by the proportion of area left (ie not converted to plantation TmPastLeft)
  }
  
  balanceRege$iter <- dataRege$iter  

  # calculate the total across plots (in t) for each iteration
  Crege <- balanceRege[, lapply(.SD, function(x) sum(x, na.rm=TRUE)), by=.(iter)]
  Crege[, iter:=NULL]
  
  return(Crege)
}

