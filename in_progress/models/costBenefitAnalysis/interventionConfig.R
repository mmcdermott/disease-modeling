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

redImm_Interventions <- allInterventions[5:6]
incLTBItrmt_Interventions <- allInterventions[7:8]
redEnLTBI_Interventions <- allInterventions[1:3]

curInterventions <- redEnLTBI_Interventions

printError <- function(intervention,error) {
  print("I didn't recognize that intervention strategy. I'm sorry.")
  print("Here's the intervention you gave me: ")
  print(intervention)
  print("And here's the error")
  print(error)
}

redEnLTBICFun   <- function(mag,baseCost){return(baseCost+600*(mag/100))}
incLTBItrmtCFun <- function(mag,baseCost){return(baseCost+0.025)}
redTransCFun    <- function(mag,baseCost){return(baseCost+1000*(mag/100))}
interventionProps <- list(
  redEnLTBI   =list(prop='f',      cost=list(c('LTBIEn',   redEnLTBICFun))),
  redImm      =list(prop='f',      cost=list()),
  incLTBItrmt =list(prop='sigmaL', cost=list(c('totPop',   incLTBItrmtCFun))),
  redTrans    =list(prop='trans',  cost=list(c('newCases', redTransCFun))))

interventionConfig <- function(interventionStr, x=0) { #x is an integer refering to cost option, ranges 0 to 5
  costs  <- c(newCases=0,totPop=0,LTBIEn=Cl)
  params <- c(sigmaL=sigmaLBase,f=fBase,trans=transBase)
  intrs  <- strsplit(interventionStr,'&')[[1]]
  for (intervention in intrs) {
    type <- sub("\\d+","",intervention)#sub empty str for digits
    mag  <- as.numeric(sub("\\D+","",intervention))#sub empty str for non-digits
    if (pmatch("red",type,nomatch=-1) == 1) {
      sign <- -1
    } else if (pmatch("inc",type,nomatch=-1) == 1) {
      sign <- 1
    } else {
      printError(intervention, 'Unrecognized Prefix')
      next()
    }
    details <- interventionProps[[type]]
    if (length(details) == 0) {
      printError(intervention, 'No Stored Intervention Properties')
      next()
    }
    for (intProp in details$prop) {
      params[[intProp]] <- (1+sign*mag/100)*params[[intProp]]
    }
    for (costList in details$cost) {
      costs[[costList[1][[1]]]]<-costList[2][[1]](mag,costs[[costList[1][[1]]]])
    }
  }
  return(list(costs=costs,params=params))
}
