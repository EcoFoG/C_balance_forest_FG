# get the expected volume of timber at harvest of plantation, by simulating a very high number of plantation and taking the mean

get_VolPlantExp <- function(AGBendPlant, Ccontent, PlantDuration, RCub, 
                            Data_wood_plantation, ChangeProd) {
  
  nbsim <- 1000000
  
  dataVolExp <- AGBendPlant[sample(.N, nbsim, replace=TRUE)]
  dataVolExp$row <- 1: nbsim
  dataVolExp$ACSend <-dataVolExp[, Ccontent * rlnorm(1, log(theta*PlantDuration), sigma), by=.(as.factor(row))]$V1
  if(!is.null(ChangeProd)) {
    dataVolExp$ACSend <- dataVolExp$ACSend * ChangeProd
  }
  dataVolExp$CsawmillExp <- RCub*dataVolExp$ACSend
  dataVolExp <-cbind(dataVolExp, Data_wood_plantation[sample(.N, nbsim, replace=TRUE), "WDPlant"]) 
  dataVolExp$VolExp <- dataVolExp[, CsawmillExp / (Ccontent * WDPlant)]
  
  VolPlantExp <- mean(dataVolExp$VolExp)
  
  return(VolPlantExp)
}