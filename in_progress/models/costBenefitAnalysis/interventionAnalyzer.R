#Option Processing
args <- commandArgs(trailingOnly = T)
if (length(args) > 0) {
  #TODO: Remove later
  #generateNewData <- F#as.logical(args[1])
  analysisType    <- as.numeric(args[1])
  index           <- as.numeric(args[2])
  #LTBIEnCosts     <- as.numeric(args[2])
  #activeTxC       <- as.numeric(args[3])
  #LTBITxC         <- as.numeric(args[4])
  #perIncLTBI      <- as.numeric(args[5])
} else {
  generateNewData <- F
  index <- 1
  analysisType <- 1
}
generateNewData <- F

#Which interventions to run:
source('interventionConfig.R')

#Hill Model:
source('hillFunctions.R')

LTBIEnCosts     <- 0
activeTxC       <- parms[['CtBase']]
LTBITxC         <- parms[['ClBase']]

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

#TODO: Give this kind of power to the base user!
#Intervention Analyzing:

# if (analysisType == 1) {
  # activeTxCRange <- seq(0,40000,by=200)
  # if (index > 1) {
    # run <- 201*21*21 + (index-2)*201*21*20 + 1
    # LTBITxCRange <- seq((index-1)*200+10,index*200,by=10)
  # } else if (index == 1) {
    # run <- 1
    # LTBITxCRange <- seq(0,200,by=10)
  # }
  # #LTBITx and ActiveTx
  # for (LTBITxC in LTBITxCRange) {
    # for (activeTxC in activeTxCRange) {
      # for (intervention in curInterventions) {
        # intConfig <- interventionConfig(intervention)
        # costs     <- intConfig$costs
        # params    <- intConfig$params
        # interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]],
                          # params[["incLTBI"]],activeTxC,LTBITxC)
        # write.csv(interData, paste(c(intFilePrefix,intervention,'Anl',analysisType,'Run',run,intFileSuffix),
                                   # collapse=""))
        # run <- run + 1
      # }
    # }
  # }
# } else if (analysisType==2) {
  # fRange <- seq(0,50,by=0.5)
  # if (index > 1) {
    # run <- 101*21*41 + (index-2)*101*21*40 + 1
    # LTBIEnCostsRange <- seq(400*(index-1)+10,400*index,by=10)
  # } else if (index == 1) {
    # run <- 1
    # LTBIEnCostsRange <- seq(0,400,by=10)
  # }
  # for (LTBIEnCosts in LTBIEnCostsRange) {
    # for (f in fRange) {
      # for (intervention in curInterventions) {
        # intConfig <- interventionConfig(intervention)
        # costs     <- intConfig$costs
        # costs[['LTBIEn']] <- LTBIEnCosts
        # params    <- intConfig$params
        # interData <- hill(costs,params[["sigmaL"]],f,params[["trans"]],
                          # params[["incLTBI"]],activeTxC,LTBITxC)
        # write.csv(interData, paste(c(intFilePrefix,intervention,'Anl',analysisType,'Run',run,intFileSuffix),
                                   # collapse=""))
        # run <- run + 1
      # }
    # }
  # }
# }
