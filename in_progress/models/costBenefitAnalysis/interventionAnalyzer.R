generateNewData <- F
baseFile        <- 'baseData.csv'
intFilePrefix   <- 'intervention'
intFileSuffix   <- '.csv'
interventions   <- c("redEnLTBI100","redEnLTBI75","redEnLTBI50","redTrans100",
                     "redImm25","redImm50","incLTBItrmt2","incLTBItrmt4",
                     "redImm25&incLTBItrmt2","redImm50&incLTBItrmt2",
                     "redImm25&incLTBItrmt2","redImm50&incLTBItrmt4",
                     "incLTBItrmt2&redEnLTBI100","incLTBItrmt2&redEnLTBI75",
                     "incLTBItrmt2&redEnLTBI50","incLTBItrmt4&redEnLTBI100",
                     "incLTBItrmt4&redEnLTBI75","incLTBItrmt4&redEnLTBI50")
C <- c(newCases=0,totPop=0,LTBIEn=0) 
interventionCosts <- function(interventionStr) {
  newCases <- 0
  totPop   <- 0
  LTBIEn   <- 0
  interVector <- strsplit(interventionStr,'&')[[1]]
  for (intervention in interVector) {
    interventionType <- 0
    interventionMag  <- 0
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
      } else (interventionMag == 4) {
        totPop <- totPop + 2 #THESE ARE SO TOTALLY MADE UP. 
      } else {
        error = T
      }
    } else {
      error = T
    }
    if (error) {
      print("I didn't recognize that intervention strategy. I'm sorry.")
      stop()
    }
  }
  return(c(newCases=newCases,totPop=totPop,LTBIEn=LTBIEn))
}

#Linux Config: 
if (Sys.info()['sysname'] == "Linux") {
  #Making it plot on linux
  X11.options(type='nbcairo')
}

#Hill Model:
source('hillFunctions.R')

#Base Data: 
if (!generateNewData) {
  P               <- read.csv(baseFile)
  generateNewData <- (length(P$X) != length(years))
  P$X             <- NULL
} 
if (generateNewData) {
  P <- hill(C,sigmaLBase,fBase,1,1,1,totT)
  write.csv(P,baseFile)
}
baseInc <- generateIncidence(P)

#Intervention Data:
# Curing Incoming LTBI:
#  Costs:
C100 <- C
C75  <- C
C50  <- C

C100["LTBIEn"] <- 1000
C75["LTBIEn"]  <- 800
C50["LTBIEn"]  <- 700

#Plot A: Where we eliminate incoming LTBI 100%, 75%, or 50%
noImmLTBI      <- hill(C100,sigmaLBase,fBase,1,0)
noImmLTBIInc   <- generateIncidence(noImmLTBI)
someImmLTBI    <- hill(C75,sigmaLBase,fBase,1,0.25)
someImmLTBIInc <- generateIncidence(someImmLTBI)
halfImmLTBI    <- hill(C50,sigmaLBase,fBase,1,0.5)
halfImmLTBIInc <- generateIncidence(halfImmLTBI)

#When Calculating range, we presume that FB always has higher incidence rate
yrange <- range(c(0.5,baseInc$IN1,noImmLTBIInc$IN1,someImmLTBIInc$IN1,halfImmLTBIInc$IN1))
dev.new()
plot(years, baseInc$IN0, main="Plot A: Reduce incoming LTBI in 2008", log='y', xlab='year', ylab='incidence/million', ylim=yrange, type="l", col="blue")
lines(years, baseInc$INall, type="l", col="red")
lines(years, baseInc$IN1, type="l", col="green")
lines(years, noImmLTBIInc$IN0, type="l", col="blue", lty=2)
lines(years, noImmLTBIInc$INall, type="l", col="red", lty=2)
lines(years, noImmLTBIInc$IN1, type="l", col="green", lty=2)
lines(years, someImmLTBIInc$IN0, type="l", col="blue", lty=3)
lines(years, someImmLTBIInc$INall, type="l", col="red", lty=3)
lines(years, someImmLTBIInc$IN1, type="l", col="green", lty=3)
lines(years, halfImmLTBIInc$IN0, type="l", col="blue", lty=4)
lines(years, halfImmLTBIInc$INall, type="l", col="red", lty=4)
lines(years, halfImmLTBIInc$IN1, type="l", col="green", lty=4)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'No Incoming LTBI post 2008', '75% reduction of Inc. LTBI post 2008', '50% reduction of Inc. LTBI post 2008'), col=c("blue", "green", "red", "black","black","black"), lty=c(1,1,1,2,3,4))

#Cost Plot A:
yrange <- range(c(P$cN1+P$cN0,noImmLTBI$cN1+noImmLTBI$cN0,someImmLTBI$cN0+someImmLTBI$cN1,halfImmLTBI$cN0+halfImmLTBI$cN1))
dev.new()
plot(years, P$cN1+P$cN0, main="Plot A: cost of various interventions", xlab='year', ylab='USD', ylim=yrange, type="l", col="blue")
lines(years, noImmLTBI$cN0 + noImmLTBI$cN1, type="l", col="green", lty=1)
lines(years, someImmLTBI$cN0 + someImmLTBI$cN1, type="l", col="red", lty=1)
lines(years, halfImmLTBI$cN0 + halfImmLTBI$cN1, type="l", col="purple", lty=1)
legend('topright', legend=c('Base Cost - No Interventions', 'Cost with elimination of Inc. LTBI', 'Cost with 75% reduction of Inc. LTBI', 'Cost with 50% reduction of Inc. LTBI'), col=c("blue", "green", "red", "purple"), lty=c(1,1,1,1))

maxDifference   <- ((P$cN0+P$cN1)-(noImmLTBI$cN0+noImmLTBI$cN1))[cutoffT:totT]
savingsPerCase  <- maxDifference/(1e6*P$LTBIEn[cutoffT:totT])
dev.new()
plot(yearsPC, savingsPerCase, main="Savings Per Cured Case of Entering LTBI", xlab='year', ylab='USD', type="l", col="blue")

savingsPerCaseD <- maxDifference/(1e6*noImmLTBI$curedLTBIEnD[cutoffT:totT])
dev.new()
plot(yearsPC, savingsPerCaseD, main="Savings Per Discounted Cured Case of Entering LTBI", xlab='year', ylab='USD', type="l", col="blue")

##Plot A: Where Transmission is cut after 2008
#noTrans    <- hill(sigmaLBase,fBase,0)
#noTransInc <- generateIncidence(noTrans)
#
##When Calculating range, we presume that FB always has higher incidence rate
#yrange <- range(c(0.5,baseInc$IN1,noTransInc$IN1))
#dev.new()
#plot(years, baseInc$IN0, main="Plot A: Cut Trans. in 2008", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, baseInc$INall, type="l", col="red")
#lines(years, baseInc$IN1, type="l", col="green")
#lines(years, noTransInc$IN0, type="l", col="blue", lty=2)
#lines(years, noTransInc$INall, type="l", col="red", lty=2)
#lines(years, noTransInc$IN1, type="l", col="green", lty=2)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'With Transmission Cutoff in 2008'), col=c("blue", "green", "red", "black"), lty=c(1,1,1,2))
#
##Plot B: Where LTBI treatment x2 or x4
#doubled    <-hill(sigmaLBase*2,fBase)
#doubledInc <- generateIncidence(doubled)
#quad       <- hill(sigmaLBase*4,fBase)
#quadInc    <- generateIncidence(quad)
#
##When Calculating range, we presume that FB always has higher incidence rate after 2008
#yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
#dev.new()
#plot(years, baseInc$IN0, main="Plot B: Increase Chronic LTBI Treatment in 2008", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, baseInc$INall, type="l", col="red")
#lines(years, baseInc$IN1, type="l", col="green")
#lines(years, doubledInc$IN0, type="l", col="blue", lty=2)
#lines(years, doubledInc$INall, type="l", col="red", lty=2)
#lines(years, doubledInc$IN1, type="l", col="green", lty=2)
#lines(years, quadInc$IN0, type="l", col="blue", lty=3)
#lines(years, quadInc$INall, type="l", col="red", lty=3)
#lines(years, quadInc$IN1, type="l", col="green", lty=3)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c("blue", "green", "red", "black", "black"), lty=c(1,1,1,2,3))
#
##Plot C: Where FB arrivals cut in half, also LTBI treatment x2 or x4 after 2008
#halfFB           <- hill(sigmaLBase,fBase/2)
#halfFBInc        <- generateIncidence(halfFB)
#halfFBdoubled    <- hill(sigmaLBase*2,fBase/2)
#halfFBdoubledInc <- generateIncidence(halfFBdoubled)
#halfFBquad       <- hill(sigmaLBase*4,fBase/2)
#halfFBquadInc    <- generateIncidence(halfFBquad)
#
##When Calculating range, we presume that FB always has higher incidence rate
#yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
#dev.new()
#plot(years, halfFBInc $IN0, main="Plot C: Half FB Arrival Rate, increase LTBI Treatment", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, halfFBInc $INall, type="l", col="red")
#lines(years, halfFBInc $IN1, type="l", col="green")
#lines(years, halfFBdoubledInc $IN0, type="l", col="blue", lty=2)
#lines(years, halfFBdoubledInc $INall, type="l", col="red", lty=2)
#lines(years, halfFBdoubledInc $IN1, type="l", col="green", lty=2)
#lines(years, halfFBquadInc $IN0, type="l", col="blue", lty=3)
#lines(years, halfFBquadInc $INall, type="l", col="red", lty=3)
#lines(years, halfFBquadInc $IN1, type="l", col="green", lty=3)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c("blue", "green", "red", "black", "black"), lty=c(1,1,1,2,3))
#
##Plot D: Where FB arrivals divided by 4, also LTBI treatment x2 or x4 after 2008
#quarterFB           <- hill(sigmaLBase,fBase/4)
#quarterFBInc        <- generateIncidence(quarterFB)
#quarterFBdoubled    <- hill(sigmaLBase*2,fBase/4)
#quarterFBdoubledInc <- generateIncidence(quarterFBdoubled)
#quarterFBquad       <- hill(sigmaLBase*4,fBase/4)
#quarterFBquadInc    <- generateIncidence(quarterFBquad)
#
##When Calculating range, we presume that FB always has higher incidence rate
#yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
#dev.new()
#plot(years, quarterFBInc $IN0, main="Plot D: 1/4 FB Arrival Rate, increase LTBI Treatment", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, quarterFBInc $INall, type="l", col="red")
#lines(years, quarterFBInc $IN1, type="l", col="green")
#lines(years, quarterFBdoubledInc $IN0, type="l", col="blue", lty=2)
#lines(years, quarterFBdoubledInc $INall, type="l", col="red", lty=2)
#lines(years, quarterFBdoubledInc $IN1, type="l", col="green", lty=2)
#lines(years, quarterFBquadInc $IN0, type="l", col="blue", lty=3)
#lines(years, quarterFBquadInc $INall, type="l", col="red", lty=3)
#lines(years, quarterFBquadInc $IN1, type="l", col="green", lty=3)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c("blue", "green", "red", "black", "black"), lty=c(1,1,1,2,3))
