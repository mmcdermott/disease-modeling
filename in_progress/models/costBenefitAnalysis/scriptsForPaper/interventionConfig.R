source('hillConstants.R')

directory     <- 'data/'
baseFile      <- paste(c(directory,'baseData.csv'),collapse="")
intFilePrefix <- directory
intFileSuffix <- '.csv'
#Intervention string details:
# inc/red                   --> increase/reduce 
# EnLTBI/Imm/LTBItrmt/Trans --> Entering LTBI, Immigration, LTBI Treatment, 
#                               Transmission.
# Magnitude (the numbers)   --> Interpert these by placing a 'by' prior to the 
#                               number and adding a percent at the end. 
# &                         --> 'and'. This chains interventions.
#Ex: 
# redImm25&incLTBItrmt100   --> reduce immigration by 25% and increase LTBI 
#                               treatment by 100%
allInterventions <- c("redEnLTBI100","redEnLTBI75","redEnLTBI50","redTrans100",
                      "redImm75","redImm50","incLTBItrmt100","incLTBItrmt300",
                      "redImm75&incLTBItrmt100","redImm50&incLTBItrmt100",
                      "redImm75&incLTBItrmt100","redImm50&incLTBItrmt300",
                      "incLTBItrmt100&redEnLTBI100","incLTBItrmt100&redEnLTBI75",
                      "incLTBItrmt100&redEnLTBI50","incLTBItrmt300&redEnLTBI100",
                      "incLTBItrmt300&redEnLTBI75","incLTBItrmt300&redEnLTBI50")
paperRedEnLTBIInts <- c("redEnLTBI10","redEnLTBI25","redEnLTBI50","redEnLTBI100")
paperRedTransInts <- c("redTrans100")
curInterventions <- c("redEnLTBI10","redEnLTBI25","redEnLTBI50",
                      "redEnLTBI100","redTrans100")

interventionConfig <- function(interventionStr) {
  error    <- F
  sigmaL   <- sigmaLBase #define me
  f        <- fBase
  trans    <- transBase
  incLTBI  <- incLTBIBase
  newCases <- 0
  totPop   <- 0
  LTBIEn   <- 0
  interVector <- strsplit(interventionStr,'&')[[1]]
  for (intervention in interVector) {
    interventionType <- sub("\\d+","",intervention)#sub empty str for digits
    interventionMag  <- as.numeric(sub("\\D+","",intervention))#sub empty str for non-digits
    if (interventionType == "redEnLTBI") {
      incLTBI <- incLTBI*(100-interventionMag)/100
      LTBIEn  <- 800 #LTBIEn + 400 + 600*(interventionMag/100) + x*100
    } else if (interventionType == "redImm") {
      #No Costs!
      if (interventionMag == 75) {
        f <- f * 0.25
      } else if (interventionMag == 50) {
        f <- f * 0.5
      } else {
        error = T
      }
    } else if (interventionType == "incLTBItrmt") {
      if (interventionMag == 100) {
        totPop <- totPop + 0.05 #THESE ARE SO TOTALLY MADE UP. 
        sigmaL <- sigmaL * 2
      } else if (interventionMag == 300) {
        totPop <- totPop + 0.1 #THESE ARE SO TOTALLY MADE UP. 
        sigmaL <- sigmaL * 4
      } else {
        error = T
      }
    } else if (interventionType == "redTrans") {
      if (interventionMag == 100) {
        newCases <- newCases + 1000
        trans    <- 0
      } else {
        error = T
      }
    } else {
      error = T
    }
    if (error) {
      print("I didn't recognize that intervention strategy. I'm sorry.")
      print("Here's the intervention you gave me: ")
      print(intervention)
      stop()
    }
  }
  costs <- c(newCases=newCases,totPop=totPop,LTBIEn=LTBIEn)
  params <- c(sigmaL=sigmaL,f=f,trans=trans,incLTBI=incLTBI)
  return(list(costs=costs,params=params))
}
