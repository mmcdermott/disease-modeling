library(ggplot2)
#TODO: Make this read from the files somehow
DELTA_T = 0.8
years = seq(2000,2100 - DELTA_T, DELTA_T)
totT = length(years)
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
  return(data.frame(IN0,IN1,INall))
}

USB   <- rep("USB",             totT)
FB    <- rep("FB",              totT)
all   <- rep("All",             totT)
noInt <- rep("No Intervention", totT)
int   <- rep("Intervention",    totT)

baseData         <- read.csv('baseData.csv')
interventionData <- read.csv('interventionData.csv')

baseInc          <- generateIncidence(baseData)
interventionInc  <- generateIncidence(interventionData)

#Incidence Reports: Comparing Baseline Incidence against Intervention Incidence
incData <- data.frame(year = years, 
                      baseUSB = baseInc$IN0,   intUSB = interventionInc$IN0, 
                      baseFB  = baseInc$IN1,    intFB = interventionInc$IN1, 
                      baseAll = baseInc$INall, intAll = interventionInc$INall)
incPlot <- ggplot(incData, aes(x=year)) + 
           scale_y_log10(breaks=c(1,2,5,10,25,50,100,200),
                         labels=c("Elimination (1)",2,5,10,25,50,100,200),
                         limits=c(0.5,250)) + 
           labs(x="Years", y="Incidence/million", color="Population", 
                linetype="Intervention Status") + 
           ggtitle("Incidence Levels for Intervention A") + 
           geom_line(aes(y=baseUSB, color=USB, linetype=noInt))  + 
           geom_line(aes(y=intUSB,  color=USB, linetype=int))    + 
           geom_line(aes(y=baseFB,  color=FB,  linetype=noInt))  + 
           geom_line(aes(y=intFB,   color=FB,  linetype=int))    + 
           geom_line(aes(y=baseAll, color=all, linetype=noInt))  + 
           geom_line(aes(y=intAll,  color=all, linetype=int))

#Total Costs Excluding Sticker Price: Comparing costs of various interventions
baseCost  <- baseData$cN0 + baseData$cN1                 #US Health Care System
                                                         # (HCS) TB costs due to
                                                         # base system
interCost <- interventionData$cN0 + interventionData$cN1 #US HCS TB costs due to
                                                         # intervenvtion
totSaved  <- baseCost - interCost                        #US HCS TB savings due
                                                         # to intervention
savingsData <- data.frame(year=years, baseCost=baseCost, interCost=interCost,
                          totSaved=totSaved)
savingsPlot <- ggplot(savingsData,aes(x=year)) + 
               labs(x="Years", y="USD", color="Intervention Status") +
               ggtitle("Total Saved by US Health Care System given
                        Intervention A, ignoring intervention cost") +
               geom_line(aes(y=baseCost, color=noInt)) + 
               geom_line(aes(y=interCost, color=int)) + 
               geom_ribbon(aes(ymin=interCost,ymax=baseCost,alpha=0.8)) + 
               geom_line(aes(y=totSaved, color=rep("Savings",totT)))
#Total Costs Including Sticker Price: Comparing costs of various interventions
#costInter <- interventionData$implementationCost         #Implementation cost of
#                                                         # intervention
#interTot  <- interCost + costInter                       #Total US cost due to
#                                                         # intervention
#totSpent  <- interTot - baseCost                         #Additional cost due to
                                                         # intervention

#dev.new()
#plot( years, baseCost,  type="l", col="brown")
#lines(years, interCost, type="l", col="black")
#lines(years, totSaved,  type="l", col="gold")
