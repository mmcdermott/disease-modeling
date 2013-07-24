baseFile         <- 'baseData.csv'
intFilePrefix    <- 'intervention'
intFileSuffix    <- '.csv'
allInterventions <- c("redEnLTBI100","redEnLTBI75","redEnLTBI50","redTrans100",
                      "redImm25","redImm50","incLTBItrmt2","incLTBItrmt4",
                      "redImm25&incLTBItrmt2","redImm50&incLTBItrmt2",
                      "redImm25&incLTBItrmt2","redImm50&incLTBItrmt4",
                      "incLTBItrmt2&redEnLTBI100","incLTBItrmt2&redEnLTBI75",
                      "incLTBItrmt2&redEnLTBI50","incLTBItrmt4&redEnLTBI100",
                      "incLTBItrmt4&redEnLTBI75","incLTBItrmt4&redEnLTBI50")
curInterventions <- allInterventions

interventionCosts <- function(interventionStr) {
  error    <- F
  newCases <- 0
  totPop   <- 0
  LTBIEn   <- 0
  interVector <- strsplit(interventionStr,'&')[[1]]
  for (intervention in interVector) {
    interventionType <- sub("\\d+","",intervention)#sub empty str for digits
    interventionMag  <- sub("\\D+","",intervention)#sub empty str for non-digits
    if (interventionType == "redEnLTBI") {
      #TODO: Make this a function depending on magnitude for greater flexibility
      # (minor)
      if (interventionMag == 50) {
        LTBIEn <- LTBIEn + 500
      } else if (interventionMag == 75) {
        LTBIEn <- LTBIEn + 700
      } else if (interventionMag == 100) {
        LTBIEn <- LTBIEn + 1000
      } else {
        error = T
      }
    } else if (interventionType == "redImm") {
      #No Costs!
    } else if (interventionType == "incLTBItrmt") {
      if (interventionMag == 2) {
        totPop <- totPop + 1 #THESE ARE SO TOTALLY MADE UP. 
      } else if (interventionMag == 4) {
        totPop <- totPop + 2 #THESE ARE SO TOTALLY MADE UP. 
      } else {
        error = T
      }
    } else if (interventionType == "redTrans") {
      if (interventionMag == 100) {
        newCases <- newCases + 1000
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
  return(c(newCases=newCases,totPop=totPop,LTBIEn=LTBIEn))
}
