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
  P <- hill(C,1,totT)
  write.csv(P,baseFile)
}
baseInc <- generateIncidence(P)

#Intervention Analyzing:
for (intervention in curInterventions) {
  interData <- hill(interventionConfig(intervention))
  write.csv(interData, paste(c(intFilePrefix,intervention,intFileSuffix),
                             collapse=""))
}
