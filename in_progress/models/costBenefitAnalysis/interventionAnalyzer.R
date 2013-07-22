if (Sys.info()['sysname'] == "Linux") {
  #Making it plot on linux
  X11.options(type='nbcairo')
}
vF <- 1.5         #Progression rate of acute infection per year
vL0 <- 0.0014     #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1 <- 0.0010     #Progression rate for reactivation (chronic LTBI) in the FB population per year
generateIncidence <- function(dataSet) {
  IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
  IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
  INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
  frame <- data.frame(IN0,IN1,INall)
  write.table(frame, file="incData.csv", sep=",")
  return(frame)
}

baseData         <- read.csv('baseData.csv')
baseInc          <- generateIncidence(baseData)
interventionData <- read.csv('interventionData.csv')
interventionInc  <- generateIncidence(interventionData)

#Plot A: Comparing Baseline Incidence against Intervention Incidence
yrange <- range(c(0.5,baseInc$IN1,noImmLTBIInc$IN1,someImmLTBIInc$IN1,halfImmLTBIInc$IN1))
dev.new()
#TODO: make this grab intervention name from somewhere.
plot( years, baseInc$IN0,             type="l", col="blue", main="Intervention Effectiveness", log='y', xlab='year', 
                                                            ylab='incidence/million', ylim=yrange)
lines(years, baseInc$INall,           type="l", col="red")
lines(years, baseInc$IN1,             type="l", col="green")
lines(years, interventionInc$IN0,     type="l", col="blue",  lty=2)
lines(years, interventionInc$INall,   type="l", col="red",   lty=2)
lines(years, interventionInc$IN1,     type="l", col="green", lty=2)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Intervention 1'), col=c("blue", "green", "red", "black","black","black"), lty=c(1,1,1,2,3,4))

#Plot B: Comparing costs of various interventions
yrange    <- range(c(baseData$cN0 + baseData$cN1, interventionData$cN0+interventionData$cN1))
baseCost  <- baseData$cN0 + baseData$cN1
interCost <- interventionData$cN0 + interventionData$cN1
diffCost  <- baseCost - interCost
dev.new()
plot( years, baseCost,  type="l", col="brown")
lines(years, interCost, type="l", col="black")
lines(years, diffCost,  type="l", col="gold")
