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

#Data Labels
USB     <- rep("USB",             totT)
FB      <- rep("FB",              totT)
all     <- rep("All",             totT)
noInt   <- rep("No Intervention", totT)
int     <- rep("Intervention",    totT)
savings <- rep("Savings",         totT)
cost    <- rep("Cost",            totT)

#Aesthetics
USBC     <- 'blue'
FBC      <- 'green'
allC     <- 'red'
noIntC   <- 'blue'
intC     <- 'brown'
savingsC <- '#24913C'
costC    <- 'red'

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
#US Health Care System (HCS) TB costs due to base system
baseCost  <- (baseData$cN0 + baseData$cN1)/1e9
#US HCS TB costs due to intervenvtion
interCost <- (interventionData$cN0 + interventionData$cN1)/1e9
#US HCS TB savings due to intervention
totSaved  <- baseCost - interCost                        

savingsData <- data.frame(year=years, baseCost=baseCost, interCost=interCost,
                          totSaved=totSaved)
yrange      <- round(seq(min(savingsData$baseCost),max(savingsData$baseCost),by=0.5),1)
savingsPlot <- ggplot(savingsData,aes(x=year)) + 
               labs(x="Years", y="Billions of USD", color="Intervention Status") +
               scale_y_continuous(breaks=yrange) + 
               ggtitle("Total Saved by US Health Care System given
                        Intervention A, ignoring intervention cost") +
               geom_ribbon(aes(ymin=interCost,ymax=baseCost,fill=savings, alpha=0.2)) + 
               geom_line(aes(y=baseCost, color=noInt)) +
               geom_line(aes(y=interCost, color=int)) + 
               geom_line(aes(y=totSaved, color=savings)) +
               scale_fill_manual(values=c(savingsC)) + 
               scale_color_manual(values=c(noIntC,intC,savingsC)) + 
               guides(fill=F, alpha=F)

##Total Costs Including Sticker Price: Comparing costs of various interventions
##Implementation cost of intervention
#costInter <- interventionData$implementationCost
##Total US HCS cost due to intervention
#interTot  <- interCost + costInter
##Total additional spent by US HCS due to intervention
#totSpent  <- interTot - baseCost

#Total Cases Averted
totalCasesBase    <- baseData$progAcute0 + baseData$progAcute1 + 
                     baseData$progChron0 + baseData$progChron1
totalCasesBaseD   <- baseData$progTotalD0 + baseData$progTotalD1
totalCasesInt     <- IntData$progAcute0 + IntData$progAcute1 + 
                     IntData$progChron0 + IntData$progChron1
totalCasesIntD    <- IntData$progTotalD0 + IntData$progTotalD1
casesAvertedPlot  <-
