library(ggplot2)
source('deSolConstants.R')
source('hillFunctions.R') #for generateIncidence
source('interventionConfig.R')

#Title Generation:
plotTitle <- function(base,interventionName,final="") {
  if (final != "") {
    return(ggtitle(paste(c(base,interventionName,final),collapse=" ")))
  } else {
    return(ggtitle(paste(c(base,interventionName),collapse=" ")))
  }
}

#Data Labels
USB             <- rep("USB",             totT)
FB              <- rep("FB",              totT)
all             <- rep("All",             totT)
noInt           <- rep("No Intervention", totT)
int             <- rep("Intervention",    totT)
savings         <- rep("Savings",         totT)
costs           <- rep("Cost",            totT)
averted         <- rep("Cases Averted",   totT)
TBdeathsAverted <- rep("TB Deaths Averted",   totT)

#Aesthetics
USBC             <- 'blue'
FBC              <- 'green'
allC             <- 'red'
noIntC           <- 'black'
intC             <- 'blue'
savingsC         <- '#24913C'
costsC           <- '#9F0013'
avertedC         <- '#24913C'
TBdeathsAvertedC <- '#24913C'

baseData <- read.csv(baseFile)
#Base Incidence
baseInc <- generateIncidence(baseData)
#US Health Care System (HCS) TB costs due to base system
baseHCSCost <- (baseData$cN0 + baseData$cN1)/1e9
#Base Cases
baseCases  <- 1e6*(baseData$progAcute0 + baseData$progAcute1 + 
                   baseData$progChron0 + baseData$progChron1)
baseCasesD <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)
#Base Deaths
baseDeaths  <- 1e6*(baseData$tbdeath0 + baseData$tbdeath1)
baseDeathsD <- 1e6*(baseData$tbdeathD0 + baseData$tbdeathD1)

#TODO: Remove Duplicated Code

incidencePlotG <- function(interventionData,interventionName) {
  #Incidence Reports: Comparing Baseline Incidence against Intervention Incidence
  interventionInc  <- generateIncidence(interventionData)
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
             plotTitle("Incidence Levels for Intervention",interventionName) + 
             geom_line(aes(y=baseUSB, color=USB, linetype=noInt))  + 
             geom_line(aes(y=intUSB,  color=USB, linetype=int))    + 
             geom_line(aes(y=baseFB,  color=FB,  linetype=noInt))  + 
             geom_line(aes(y=intFB,   color=FB,  linetype=int))    + 
             geom_line(aes(y=baseAll, color=all, linetype=noInt))  + 
             geom_line(aes(y=intAll,  color=all, linetype=int))
  return(incPlot)
}

savingsPlotG <- function(interventionData,interventionName) {
  #Total Costs Excluding Sticker Price: Comparing costs of various interventions
  #US HCS TB costs due to intervenvtion
  interHCSCost <- (interventionData$cN0 + interventionData$cN1)/1e9
  #US HCS TB savings due to intervention
  totSaved  <- baseHCSCost - interHCSCost                        
  
  savingsData <- data.frame(year=years, baseHCSCost=baseHCSCost, interCost=interHCSCost,
                            totSaved=totSaved)
  yrange      <- round(seq(min(savingsData$baseHCSCost),max(savingsData$baseHCSCost),by=0.5),1)
  savingsPlot <- ggplot(savingsData,aes(x=year)) + 
                 labs(x="Years", y="Billions of USD", color="Intervention Status") +
                 scale_y_continuous(breaks=yrange) + 
                 plotTitle("Total Saved by US Health Care System given 
                           Intervention",interventionName,
                           "ignoring intervention cost") +
                 geom_ribbon(aes(ymin=interCost,ymax=baseHCSCost,fill=savings, alpha=0.2)) + 
                 geom_line(aes(y=baseHCSCost, color=noInt)) +
                 geom_line(aes(y=interCost, color=int)) + 
                 geom_line(aes(y=totSaved, color=savings)) +
                 scale_fill_manual(values=c(savingsC)) + 
                 scale_color_manual(values=c(intC,noIntC,savingsC)) + 
                 guides(fill=F, alpha=F)
  return(savingsPlot)
}

totCostsPlotG <- function(interventionData,interventionName) {
  #Total Costs Including Sticker Price: Comparing costs of various interventions
  interHCSCost <- (interventionData$cN0 + interventionData$cN1)/1e9
  #Implementation cost of intervention
  costOfInter <- (interventionData$interventionCost)/1e9
  #Total US HCS cost due to intervention
  interTot  <- interHCSCost + costOfInter
  #Total additional spent by US HCS due to intervention
  totSpent  <- interTot - baseHCSCost
  
  costData <- data.frame(year=years, baseHCSCost=baseHCSCost, interCost=interTot,
                         totSpent=totSpent)
  yrange      <- round(seq(min(costData$interCost),max(costData$interCost)+0.5,by=0.5),1)
  costsPlot <- ggplot(costData,aes(x=year)) + 
                 labs(x="Years", y="Billions of USD", color="Intervention Status") +
                 scale_y_continuous(breaks=yrange) + 
                 plotTitle("Total Spent by US Health Care System given
                            Intervention",interventionName,
                            "given presumed intervention cost") +
                 geom_ribbon(aes(ymin=baseHCSCost,ymax=interCost,fill=costs, alpha=1)) + 
                 geom_line(aes(y=baseHCSCost, color=noInt)) +
                 geom_line(aes(y=interCost, color=int)) + 
                 geom_line(aes(y=totSpent, color=costs)) +
                 scale_fill_manual(values=c(costsC)) + 
                 scale_color_manual(values=c(costsC,intC,noIntC)) + 
                 guides(fill=F, alpha=F)
  return(costsPlot)
}

casesAvertedPlotG <- function(interventionData,interventionName) {
  #Total Cases Averted
  intCases          <- 1e6*(interventionData$progAcute0 + interventionData$progAcute1 + 
                            interventionData$progChron0 + interventionData$progChron1)
  casesAverted      <- baseCases - intCases
  
  casesAvertedData  <- data.frame(year=years,baseCases=baseCases,
                                  intCases=intCases,
                                  casesAverted=casesAverted)
  
  yrange            <- round(seq(min(casesAvertedData$baseCases),
                                 max(casesAvertedData$baseCases),by=1e5),1)
  casesAvertedPlot  <- 
    ggplot(casesAvertedData,aes(x=year)) + 
    labs(x="Years", y="Cases of TB", color="Intervention Status") +
    scale_y_continuous(breaks=yrange) + 
    plotTitle("Total Cases of TB Averted given Intervention",interventionName) +
    geom_ribbon(aes(ymin=intCases,ymax=baseCases,fill=averted, alpha=0.2)) + 
    geom_line(aes(y=baseCases,    color=noInt)) +
    geom_line(aes(y=intCases,     color=int)) + 
    geom_line(aes(y=casesAverted, color=averted)) +
    scale_fill_manual(values=c(avertedC)) + 
    scale_color_manual(values=c(avertedC,intC,noIntC)) + 
    guides(fill=F, alpha=F)
  return(casesAvertedPlot)
}

discountedCasesAvertedPlotG <- function(interventionData,interventionName) {
  #Total Cases Averted, Discounted
  intCasesD         <- 1e6*(interventionData$progTotalD0 + interventionData$progTotalD1)
  casesAvertedD     <- baseCasesD - intCasesD
  casesAvertedDataD <- data.frame(year=years,baseCases=baseCasesD,
                                  intCases=intCasesD,
                                  casesAverted=casesAvertedD)
  
  yrange            <- round(seq(min(casesAvertedDataD$baseCases),
                                 max(casesAvertedDataD$baseCases),by=1e5),1)
  casesAvertedPlotD <- 
    ggplot(casesAvertedDataD,aes(x=year)) + 
    labs(x="Years", y="Discounted Cases of TB", 
         color="Intervention Status") +
    scale_y_continuous(breaks=yrange) + 
    plotTitle("Discounted Cases of TB Averted given Intervention",
              interventionName) +
    geom_ribbon(aes(ymin=intCases,ymax=baseCases,fill=averted, alpha=0.2)) + 
    geom_line(aes(y=baseCases, color=noInt)) +
    geom_line(aes(y=intCases, color=int)) + 
    geom_line(aes(y=casesAverted, color=averted)) +
    scale_fill_manual(values=c(avertedC)) + 
    scale_color_manual(values=c(avertedC,intC,noIntC)) + 
    guides(fill=F, alpha=F)
}

cpcaPlotG <- function(interventionData,interventionName) {
  #HCS cost borne by intervention
  interHCSCost <- (interventionData$cN0 + interventionData$cN1)/1e9
  #Implementation cost of intervention
  costOfInter <- (interventionData$interventionCost)/1e9
  #Total US HCS cost due to intervention
  interTot  <- interHCSCost + costOfInter
  #Total additional spent by US HCS due to intervention
  totSpent  <- interTot - baseHCSCost

  intCases          <- 1e6*(interventionData$progAcute0 + interventionData$progAcute1 + 
                            interventionData$progChron0 + interventionData$progChron1)
  casesAverted      <- baseCases - intCases

  cpcaData  <- data.frame(year=yearsPC,cpca=1e9*totSpent[cutoffT:totT]/casesAverted[cutoffT:totT])
  cpcaPlot  <- 
    ggplot(cpcaData,aes(x=year)) + 
    labs(x="Years", y="USD") +
    scale_x_continuous(breaks=c(initialYr,cutoffYr,seq(initialYr,finalYr,25))) +
    #scale_y_log10() + 
    plotTitle("Cost per Raw TB Case Averted due to Intervention",
              interventionName) +
    geom_line(aes(y=cpca))
  
  return(cpcaPlot)
}

discountedCpcaPlotG <- function(interventionData,interventionName) {
  #HCS cost borne by intervention
  interHCSCost <- (interventionData$cN0 + interventionData$cN1)/1e9
  #Implementation cost of intervention
  costOfInter <- (interventionData$interventionCost)/1e9
  #Total US HCS cost due to intervention
  interTot  <- interHCSCost + costOfInter
  #Total additional spent by US HCS due to intervention
  totSpent  <- interTot - baseHCSCost
  
  intCasesD         <- 1e6*(interventionData$progTotalD0 + interventionData$progTotalD1)
  casesAvertedD     <- baseCasesD - intCasesD
  #Cost per cases averted graph:
  cpcaDataD <- data.frame(year=yearsPC,cpca=1e9*totSpent[cutoffT:totT]/casesAvertedD[cutoffT:totT])
  
  cpcaPlotD <- 
    ggplot(cpcaDataD,aes(x=year)) + 
    labs(x="Years", y="USD") +
    scale_x_continuous(breaks=c(initialYr,cutoffYr,seq(initialYr,finalYr,25))) +
    #scale_y_log10() + 
    plotTitle("Cost per Discounted TB Case Averted due to Intervention",
              interventionName) +
    geom_line(aes(y=cpca))

  return(cpcaPlotD)
}

deathsAvertedPlotG <- function(interventionData,interventionName) {
  #TB Deaths:
  intDeaths          <- 1e6*(interventionData$tbdeath0 + interventionData$tbdeath1)
  deathsAverted      <- baseDeaths - intDeaths
  deathsAvertedData  <- data.frame(year=years,baseDeaths=baseDeaths,
                                  intDeaths=intDeaths,
                                  deathsAverted =deathsAverted)
  
  yrange             <- round(seq(min(deathsAvertedData$baseDeaths),
                                 max(deathsAvertedData$baseDeaths),by=5e3),1)
  deathsAvertedPlot  <- 
    ggplot(deathsAvertedData,aes(x=year)) + 
    labs(x="Years", y="TB Deaths", color="Intervention Status") +
    scale_y_continuous(breaks=yrange) + 
    plotTitle("Total TB Lives Saved Given Intervention",interventionName) +
    geom_ribbon(aes(ymin=intDeaths,ymax=baseDeaths,fill=averted, alpha=0.2)) + 
    geom_line(aes(y=baseDeaths,    color=noInt)) +
    geom_line(aes(y=intDeaths,     color=int)) + 
    geom_line(aes(y=deathsAverted, color=TBdeathsAverted)) +
    scale_fill_manual(values=c(TBdeathsAvertedC)) + 
    scale_color_manual(values=c(intC,noIntC,TBdeathsAvertedC)) + 
    guides(fill=F, alpha=F)

  return(deathsAvertedPlot)
}

discountedDeathsAvertedPlotG <- function(interventionData,interventionName) {
  intDeathsD         <- 1e6*(interventionData$tbdeathD0 + interventionData$tbdeathD1)
  deathsAvertedD     <- baseDeathsD - intDeathsD
  deathsAvertedDataD <- data.frame(year=years,baseDeaths=baseDeathsD,
                                  intDeaths=intDeathsD,
                                  deathsAverted=deathsAvertedD)
  
  yrange             <- round(seq(min(deathsAvertedDataD$baseDeaths),
                                 max(deathsAvertedDataD$baseDeaths),by=5e3),1)
  deathsAvertedPlotD <- 
    ggplot(deathsAvertedDataD,aes(x=year)) + 
    labs(x="Years", y="Discounted TB Deaths", color="Intervention Status") +
    scale_y_continuous(breaks=yrange) + 
    plotTitle("Discounted TB Lives Saved Given Intervention",interventionName) +
    geom_ribbon(aes(ymin=intDeaths,ymax=baseDeaths,fill=averted, alpha=0.2)) + 
    geom_line(aes(y=baseDeaths, color=noInt)) +
    geom_line(aes(y=intDeaths, color=int)) + 
    geom_line(aes(y=deathsAverted, color=TBdeathsAverted)) +
    scale_fill_manual(values=c(TBdeathsAvertedC)) + 
    scale_color_manual(values=c(intC,noIntC,TBdeathsAvertedC)) + 
    guides(fill=F, alpha=F)
}

#plotting Functions:
presentIntervention <- function(interventionName) {
  interventionData <- read.csv(paste(c(intFilePrefix,interventionName,
                                       intFileSuffix),collapse=""))

  incPlot            <- incidencePlotG(interventionData, interventionName) 
  #Savings Plot is ignoring the implementation cost of the intervention
  savingsPlot        <- savingsPlotG(interventionData, interventionName)
  costsPlot          <- totCostsPlotG(interventionData, interventionName) 
  casesAvertedPlot   <- casesAvertedPlotG(interventionData, interventionName)
  casesAvertedPlotD  <- discountedCasesAvertedPlotG(interventionData, 
                                                    interventionName)
  cpcaPlot           <- cpcaPlotG(interventionData, interventionName)
  cpcaPlotD          <- discountedCpcaPlotG(interventionData, interventionName)
  deathsAvertedPlot  <- deathsAvertedPlotG(interventionData, interventionName)
  deathsAvertedPlotD <- discountedDeathsAvertedPlotG(interventionData, interventionName)

  interventionType <- sub("\\d+","",intervention)#sub empty str for digits
  plotsToPresent <- list(incPlot,savingsPlot,costsPlot,casesAvertedPlot,
                      casesAvertedPlotD,cpcaPlot,cpcaPlotD,deathsAvertedPlot,
                      deathsAvertedPlotD)

  #Data Needed By Multiple Plots: 
  fileName <- paste(c('interventions/',intervention,"Analysis.pdf"),collapse="")
  pdf(fileName,onefile=T)
    for (plot in plotsToPresent) {
      print(plot)
    }
  dev.off()
}

for (intervention in curInterventions) {
  print(paste(c('Preparing Intervention',intervention),collapse=" "))
  presentIntervention(intervention)
}
