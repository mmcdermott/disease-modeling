vF <- 1.5         #Progression rate of acute infection per year
vL0 <- 0.0014     #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1 <- 0.0010     #Progression rate for reactivation (chronic LTBI) in the FB population per year
popConst <- 1000

generateIncidence <- function(dataSet) {
    IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
    IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
    INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
	return(data.frame(IN0,IN1,INall))
}

args <- commandArgs(trailingOnly = T)
if (length(args) > 1) {
  importData <- as.logical(args[1])
  deltaT     <- as.numeric(args[2])
  print(deltaT)
  if (importData) {
    dataSet <- read.csv('modelData.csv')
  }
  if (length(args) > 2) {
    initialYr <- as.integer(args[3])
    finalYr   <- as.integer(args[4])
  } else {
    # Defaults:
    initialYr <- 2000
    finalYr   <- 2100
  }
  print(initialYr)
  print(finalYr)
} 
if (!exists('dataSet')) {
  print('No Data to Plot! Either list the import file as a cmd argument, or define it in the calling script')
  stop()
} else if (Sys.info()['sysname'] == "Linux") {
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
yrange <- range(c(0.5,inc$IN1,inc$IN0,inc$INall))
dev.new()
plot( yrs, inc$IN0,   type='l', col='blue', main='Incidence over Time', xlab='year', ylab='incidence/million', ylim=yrange)#,log='y')
lines(yrs, inc$INall, type='l', col='red')
lines(yrs, inc$IN1,   type='l', col='green')
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence','FB incidence','Total incidence'), col=c('blue', 'green', 'red'), lty=c(1,1,1))
