#Option Processing
args <- commandArgs(trailingOnly = T)
if (length(args) > 0) {
  generateNewData <- as.logical(args[1])
} else {
  generateNewData <- F
}

#Which interventions to run:
source('interventionConfig.R')

#Hill Model:
source('hillFunctions.R')

#Base Data: 
C <- c(newCases=0,totPop=0,LTBIEn=0) 
if (!generateNewData) {
  P               <- read.csv(baseFile)
  generateNewData <- (length(P$X) != length(years))
  P$X             <- NULL
} 
if (generateNewData) {
  P <- hill(C,sigmaLBase,fBase,transBase,incLTBIBase,1,totT)
  write.csv(P,baseFile)
}
baseInc <- generateIncidence(P)

#Intervention Analyzing:
for (intervention in curInterventions) {
  intConfig <- interventionConfig(intervention)
  costs     <- intConfig$costs
  params    <- intConfig$params
  interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]])
  write.csv(interData, paste(c(intFilePrefix,intervention,intFileSuffix),
                             collapse=""))
}
