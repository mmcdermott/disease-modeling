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

baseData         <- read.csv(baseFile)
baseInc          <- generateIncidence(baseData)

presentIntervention <- function(interventionName) {
  interventionData <- read.csv(paste(c(intFilePrefix,interventionName,
                                       intFileSuffix),collapse=""))
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
             labs(x="Years", y="Incidence/Million", color="Population", 
                  linetype="Intervention Status") + 
             plotTitle("Incidence Levels for Intervention",interventionName) + 
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
                 plotTitle("Total Saved by US Health Care System given 
                           Intervention",interventionName,
                           "ignoring intervention cost") +
                 geom_ribbon(aes(ymin=interCost,ymax=baseCost,fill=savings, alpha=0.2)) + 
                 geom_line(aes(y=baseCost, color=noInt)) +
                 geom_line(aes(y=interCost, color=int)) + 
                 geom_line(aes(y=totSaved, color=savings)) +
                 scale_fill_manual(values=c(savingsC)) + 
                 scale_color_manual(values=c(intC,noIntC,savingsC)) + 
                 guides(fill=F, alpha=F)
  
  #Total Costs Including Sticker Price: Comparing costs of various interventions
  #Implementation cost of intervention
  costInter <- (interventionData$interventionCost)/1e9
  #Total US HCS cost due to intervention
  interTot  <- interCost + costInter
  #Total additional spent by US HCS due to intervention
  totSpent  <- interTot - baseCost
  
  costData <- data.frame(year=years, baseCost=baseCost, interCost=interTot,
                         totSpent=totSpent)
  yrange      <- round(seq(min(costData$interCost),max(costData$interCost)+0.5,by=0.5),1)
  costsPlot <- ggplot(costData,aes(x=year)) + 
                 labs(x="Years", y="Billions of USD", color="Intervention Status") +
                 scale_y_continuous(breaks=yrange) + 
                 plotTitle("Total Spent by US Health Care System given
                            Intervention",interventionName,
                            "given presumed intervention cost") +
                 geom_ribbon(aes(ymin=baseCost,ymax=interCost,fill=costs, alpha=1)) + 
                 geom_line(aes(y=baseCost, color=noInt)) +
                 geom_line(aes(y=interCost, color=int)) + 
                 geom_line(aes(y=totSpent, color=costs)) +
                 scale_fill_manual(values=c(costsC)) + 
                 scale_color_manual(values=c(costsC,intC,noIntC)) + 
                 guides(fill=F, alpha=F)
  
  #Total Cases Averted
  baseCases         <- 1e6*(baseData$progAcute0 + baseData$progAcute1 + 
                            baseData$progChron0 + baseData$progChron1)
  baseCasesD        <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)
  intCases          <- 1e6*(interventionData$progAcute0 + interventionData$progAcute1 + 
                            interventionData$progChron0 + interventionData$progChron1)
  intCasesD         <- 1e6*(interventionData$progTotalD0 + interventionData$progTotalD1)
  casesAverted      <- baseCases - intCases
  casesAvertedD     <- baseCasesD - intCasesD
  
  casesAvertedData  <- data.frame(year=years,baseCases=baseCases,
                                  intCases=intCases,
                                  casesAverted=casesAverted)
  casesAvertedDataD <- data.frame(year=years,baseCases=baseCasesD,
                                  intCases=intCasesD,
                                  casesAverted=casesAvertedD)
  
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
  
  #Cost per cases averted graph:
  cpcaData  <- data.frame(year=yearsPC,cpca=1e9*totSpent[cutoffT:totT]/casesAverted[cutoffT:totT])
  cpcaDataD <- data.frame(year=yearsPC,cpca=1e9*totSpent[cutoffT:totT]/casesAvertedD[cutoffT:totT])
  
  cpcaPlot  <- 
    ggplot(cpcaData,aes(x=year)) + 
    labs(x="Years", y="USD") +
    scale_x_continuous(breaks=c(initialYr,cutoffYr,seq(initialYr,finalYr,25))) +
    #scale_y_log10() + 
    plotTitle("Cost per Raw TB Case Averted due to Intervention",
              interventionName) +
    geom_line(aes(y=cpca))
  
  cpcaPlotD <- 
    ggplot(cpcaDataD,aes(x=year)) + 
    labs(x="Years", y="USD") +
    scale_x_continuous(breaks=c(initialYr,cutoffYr,seq(initialYr,finalYr,25))) +
    #scale_y_log10() + 
    plotTitle("Cost per Discounted TB Case Averted due to Intervention",
              interventionName) +
    geom_line(aes(y=cpca))
  
  #TB Deaths:
  baseDeaths         <- 1e6*(baseData$tbdeath0 + baseData$tbdeath1)
  baseDeathsD        <- 1e6*(baseData$tbdeathD0 + baseData$tbdeathD1)
  intDeaths          <- 1e6*(interventionData$tbdeath0 + interventionData$tbdeath1)
  intDeathsD         <- 1e6*(interventionData$tbdeathD0 + interventionData$tbdeathD1)
  deathsAverted      <- baseDeaths - intDeaths
  deathsAvertedD     <- baseDeathsD - intDeathsD
  
  deathsAvertedData  <- data.frame(year=years,baseDeaths=baseDeaths,
                                  intDeaths=intDeaths,
                                  deathsAverted =deathsAverted)
  deathsAvertedDataD <- data.frame(year=years,baseDeaths=baseDeathsD,
                                  intDeaths=intDeathsD,
                                  deathsAverted=deathsAvertedD)
  
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
  
  fileName <- paste(c('interventions/',intervention,"Analysis.pdf"),collapse="")
  pdf(fileName,onefile=T)
  print(incPlot)
  print(savingsPlot)
  print(costsPlot)
  print(casesAvertedPlot)
  print(casesAvertedPlotD)
  print(cpcaPlot)
  print(cpcaPlotD)
  print(deathsAvertedPlot)
  print(deathsAvertedPlotD)
  dev.off()
}

for (intervention in curInterventions) {
  print(paste(c('Preparing Intervention',intervention),collapse=" "))
  presentIntervention(intervention)
}
