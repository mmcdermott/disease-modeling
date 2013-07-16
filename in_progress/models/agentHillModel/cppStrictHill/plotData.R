vF <- 1.5         #Progression rate of acute infection per year
vL0 <- 0.0014     #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1 <- 0.0010     #Progression rate for reactivation (chronic LTBI) in the FB population per year
popConst <- 1000

generateIncidence <- function(dataSet) {
  IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
  IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
  INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
  frame <- data.frame(IN0,IN1,INall)
  write.table(frame, file="incData.csv", sep=",")
  return(frame)
}

args <- commandArgs(trailingOnly = T)
print(args)
if (length(args) > 0) {
  dataSet <- read.csv('modelData.csv')
  deltaT  <- as.numeric(args[1])
  print(deltaT)
  if (length(args) > 2) {
    initialYr <- as.integer(args[2])
    finalYr   <- as.integer(args[3])
  } else {
    # Defaults:
    initialYr <- 2000
    finalYr   <- 2100
  }
  print(initialYr)
  print(finalYr)
} else if (!exists('noImport')) {
  dataSet   <- read.csv('modelData.csv')
  deltaT    <- .05
  initialYr <- 2000
  finalYr   <- 2100
} else if (!exists('dataSet')) {
  print('No Data to Plot! Either list the import file as a cmd argument, or define it in the calling script')
  stop()
} 
if (Sys.info()['sysname'] == "Linux") {
  #Making it plot on linux
  X11.options(type='nbcairo')
}

yrs <- seq(initialYr, finalYr - deltaT, deltaT)
print(length(yrs))
inc <- generateIncidence(dataSet)
print(length(inc$IN0))
#plot incidence data
#  xlab, ylab  --> labels for x-, y-axes
#  log='y'     --> use logarithmic scale
#  ylim=yrange --> ensure we show all data
#  type='l'    --> draw line connecting data points
#col='blue'  --> color of graph
#lines() plots data in the same window as the first plot() command

#Plot:
yrange <- range(c(0.5,inc$IN1,inc$INall))
dev.new()
plot( yrs, inc$IN0,   main='Incidence over Time', xlab='year', ylab='incidence/million', ylim=yrange, type='l', col='blue',log = 'y')
lines(yrs, inc$INall, type='l', col='red')
lines(yrs, inc$IN1,   type='l', col='green')
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence','FB incidence','Total incidence'), col=c('blue', 'green', 'red'), lty=c(1,1,1))

detData <- read.csv('detHillData.csv')
detData <- detData[1:2000,]
lines(yrs, detData$IN0)
lines(yrs, detData$IN1)
lines(yrs, detData$INall)