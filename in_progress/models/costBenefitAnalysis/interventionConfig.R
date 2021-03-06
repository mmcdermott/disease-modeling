source('hillConstants.R')

directory     <- 'data/'
baseFile      <- paste(c(directory,'baseData.csv'),collapse="")
intFilePrefix <- directory
intFileSuffix <- '.csv'
#Intervention string details:
# inc/red                   --> increase/reduce 
# EnLTBI/Imm/LTBItrmt/Trans --> Entering LTBI, Immigration, LTBI Treatment, 
#                               Transmission.
# setLTBICost               --> set cost of LTBI Treatment (usually used
#                               in conjunction with EnLTBI)
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
                      

redImm_Interventions <- allInterventions[5:6]
incLTBItrmt_Interventions <- allInterventions[7:8]
split20RedEnLTBIInter <- c('redEnLTBI0','redEnLTBI5','redEnLTBI10',
                           'redEnLTBI15','redEnLTBI20','redEnLTBI25',
                           'redEnLTBI30','redEnLTBI35','redEnLTBI40',
                           'redEnLTBI45','redEnLTBI50','redEnLTBI55',
                           'redEnLTBI60','redEnLTBI65','redEnLTBI70',
                           'redEnLTBI75','redEnLTBI80','redEnLTBI85',
                           'redEnLTBI90','redEnLTBI95','redEnLTBI100')
redEnLTBI_Interventions <- allInterventions[1:3]                 
redEnLTBI_Interventions_specialMag <- c('redEnLTBI10','redEnLTBI25','redEnLTBI50','redEnLTBI100')                 
curInterventions <- redEnLTBI_Interventions_specialMag
incLTBICost <- 0

interventionConfig <- function(interventionStr, x=0) { #x is an integer refering to cost option, ranges 0 to 5
  error    <- F
  sigmaL   <- sigmaLBase #define me
  f        <- fBase
  trans    <- transBase
  incLTBI  <- incLTBIBase
  newCases <- 0 #contact investigations
  totPop   <- 0 #total population cost per time step
  LTBIEn   <- 0 #LTBI entering cost
  interVector <- strsplit(interventionStr,'&')[[1]]
  for (intervention in interVector) {
    interventionType <- sub("\\d+","",intervention)#sub empty str for digits
    interventionMag  <- as.numeric(sub("\\D+","",intervention))#sub empty str for non-digits
    if (interventionType == "redEnLTBI") {
      #TODO: Make this a function depending on magnitude for greater flexibility
      # (minor)
      incLTBI <- incLTBI*(1-interventionMag/100)
      LTBIEn  <- 800 #LTBIEn + 400 + 600*(interventionMag/100) + x*100
    } else if (interventionType == "setLTBICost") {
      LTBIEn <- interventionMag
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
        totPop <- totPop + 0.025 + x*0.025
        sigmaL <- sigmaL * 2
      } else if (interventionMag == 300) {
        totPop <- totPop + 0.025 + x*0.025
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
