library(ggplot2)
source('interventionGroups.R')
source('deSolConstants.R')

#Title Generation:
plotTitle <- function(base,interventionName,final="") {
  if (final != "") {
    return(ggtitle(paste(c(base,interventionName,final),collapse=" ")))
  } else {
    return(ggtitle(paste(c(base,interventionName),collapse=" ")))
  }
}

baseData <- read.csv(baseFile)
#Base Incidence
baseInc <- generateIncidence(baseData)
#US Health Care System (HCS) TB costs due to base system
baseHCSCost <- (baseData$cN0 + baseData$cN1)/1e9
baseCasesD <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)

#Data Labels
USB             <- rep("USB",                 totT)
FB              <- rep("FB",                  totT)
all             <- rep("All",                 totT)
noInt           <- rep("No Intervention",     totT)
int             <- rep("Intervention",        totT)
int10           <- rep("10% Cured",           totT)
int25           <- rep("25% Cured",           totT)
int50           <- rep("50% Cured",           totT)
int100          <- rep("100% Cured",          totT)
savings         <- rep("Savings",             totT)
costs           <- rep("Implementation Cost", totT)
totalCosts      <- rep("US HCS Cost",         totT)
averted         <- rep("Cases Averted",       totT)
TBdeathsAverted <- rep("TB Deaths Averted",   totT)
redEnLTBI100L   <- rep("100% reduction",      totT)
redEnLTBI75L    <- rep("75% reduction",       totT)
redEnLTBI50L    <- rep("50% reduction",       totT)
redEnLTBI25L    <- rep("25% reduction",       totT)
redEnLTBI10L    <- rep("10% reduction",       totT)

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

# Creating our data containers.
rawData      <- as.list(paperRedEnLTBIInts)
incidence    <- as.list(paperRedEnLTBIInts)
HCSCost      <- as.list(paperRedEnLTBIInts)
costOfInter  <- as.list(paperRedEnLTBIInts)  
saveOfInter  <- as.list(paperRedEnLTBIInts)  
interTot     <- as.list(paperRedEnLTBIInts)
totSpent     <- as.list(paperRedEnLTBIInts) 
cpca         <- as.list(paperRedEnLTBIInts)
casesAverted <- as.list(paperRedEnLTBIInts)
# Naming them for convenience.
names(rawData) <- paperRedEnLTBIInts
names(incidence) <- paperRedEnLTBIInts
names(HCSCost) <- paperRedEnLTBIInts
names(costOfInter) <- paperRedEnLTBIInts
names(saveOfInter) <- paperRedEnLTBIInts
names(interTot) <- paperRedEnLTBIInts
names(totSpent) <- paperRedEnLTBIInts
names(cpca) <- paperRedEnLTBIInts
names(casesAverted) <- paperRedEnLTBIInts

for (intervention in paperRedEnLTBIInts) {
  # First, grab the base data and incidence
  rawData[[intervention]] <-
    read.csv(paste(c(intFilePrefix,intervention, intFileSuffix),collapse=""))
  incidence[[intervention]] <-
    generateIncidence(rawData[[intervention]])
  #HCS cost borne by intervention
  HCSCost[[intervention]] <- (rawData[[intervention]]$cN0 +
                                   rawData[[intervention]]$cN1)/1e9
  #Implementation cost of intervention
  costOfInter[[intervention]] <-
    (rawData[[intervention]]$interventionCost)/1e9
  #Savings from intervention
  saveOfInter[[intervention]] <- baseHCSCost - HCSCost[[intervention]]
  #Total US HCS cost due to intervention
  interTot[[intervention]] <- HCSCost[[intervention]] +
                              costOfInter[[intervention]]
  #Total additional spent by US HCS due to intervention
  totSpent[[intervention]]  <- interTot[[intervention]] - baseHCSCost 
  
  #Cost per cases averted
  intCasesD <- 1e6*(rawData[[intervention]]$progTotalD0 + rawData[[intervention]]$progTotalD1)
  casesAverted[[intervention]] <- baseCasesD - intCasesD
  cpca[[intervention]] <-
    1e9*totSpent[[intervention]]/casesAverted[[intervention]]
}

#Incidence Reports: Comparing Baseline Incidence against Intervention Incidence
incData <- data.frame(year = years,
                      baseUSB=baseInc$IN0, 
                      baseFB =baseInc$IN1,
                      baseAll=baseInc$INall,
                      redEnLTBI10USB=incidence[["redEnLTBI10"]]$IN0,
                      redEnLTBI10FB =incidence[["redEnLTBI10"]]$IN1,
                      redEnLTBI10All=incidence[["redEnLTBI10"]]$INall,
                      redEnLTBI25USB=incidence[["redEnLTBI25"]]$IN0,
                      redEnLTBI25FB =incidence[["redEnLTBI25"]]$IN1,
                      redEnLTBI25All=incidence[["redEnLTBI25"]]$INall,
                      redEnLTBI50USB=incidence[["redEnLTBI50"]]$IN0,
                      redEnLTBI50FB =incidence[["redEnLTBI50"]]$IN1,
                      redEnLTBI50All=incidence[["redEnLTBI50"]]$INall,
                      redEnLTBI100USB=incidence[["redEnLTBI100"]]$IN0,
                      redEnLTBI100FB =incidence[["redEnLTBI100"]]$IN1,
                      redEnLTBI100All=incidence[["redEnLTBI100"]]$INall)
incPlot <- ggplot(incData, aes(x=year)) + 
           scale_y_log10(breaks=c(1,2,5,10,25,50,100,200),
                         labels=c("Elimination (1)",2,5,10,25,50,100,200),
                         limits=c(0.5,250)) + 
           labs(x="Years", y="Incidence/Million", color="Population", 
                linetype="Intervention Status") + 
           ggtitle("Incidence/Million with Various Immigrating LTBI Cure Rates") +
           geom_line(aes(y=baseUSB,         color=USB, linetype=noInt))  + 
           geom_line(aes(y=redEnLTBI10USB,  color=USB, linetype=int10))  + 
           geom_line(aes(y=redEnLTBI25USB,  color=USB, linetype=int25))  + 
           geom_line(aes(y=redEnLTBI50USB,  color=USB, linetype=int50))  + 
           geom_line(aes(y=redEnLTBI100USB, color=USB, linetype=int100)) + 
           geom_line(aes(y=baseFB,          color=FB,  linetype=noInt))  + 
           geom_line(aes(y=redEnLTBI10FB,   color=FB,  linetype=int10))  + 
           geom_line(aes(y=redEnLTBI25FB,   color=FB,  linetype=int25))  + 
           geom_line(aes(y=redEnLTBI50FB,   color=FB,  linetype=int50))  + 
           geom_line(aes(y=redEnLTBI100FB,  color=FB,  linetype=int100)) + 
           geom_line(aes(y=baseAll,         color=all, linetype=noInt))  + 
           geom_line(aes(y=redEnLTBI10All,  color=all, linetype=int10))  +
           geom_line(aes(y=redEnLTBI25All,  color=all, linetype=int25))  +
           geom_line(aes(y=redEnLTBI50All,  color=all, linetype=int50))  +
           geom_line(aes(y=redEnLTBI100All, color=all, linetype=int100)) +
           theme(axis.title=element_text(size=16),axis.text=element_text(size=15),
                 plot.title=element_text(size=18))#,legend.key.height =
                 #unit(1.8,'line'))

#Total Costs Excluding Sticker Price: Comparing costs of various interventions
#US Health Care System (HCS) TB costs due to base system
# baseCost  <- (baseData$cN0 + baseData$cN1)/1e9
# #US HCS TB costs due to intervenvtion
# interCost <- (rawData$cN0 + rawData$cN1)/1e9
# #US HCS TB savings due to intervention
# totSaved  <- baseCost - interCost                        
# 
# savingsData <- data.frame(year=years, baseCost=baseCost, interCost=interCost,
#                           totSaved=totSaved)
# yrange      <- round(seq(min(savingsData$baseCost),max(savingsData$baseCost),by=0.5),1)
# savingsPlot <- ggplot(savingsData,aes(x=year)) + 
#                labs(x="Years", y="Billions of USD", color="Intervention Status") +
#                scale_y_continuous(breaks=yrange) + 
#                plotTitle("Total Saved by US Health Care System given 
#                          Intervention",interventionName,
#                          "ignoring intervention cost") +
#                geom_ribbon(aes(ymin=interCost,ymax=baseCost,fill=savings, alpha=0.2)) + 
#                geom_line(aes(y=baseCost, color=noInt)) +
#                geom_line(aes(y=interCost, color=int)) + 
#                geom_line(aes(y=totSaved, color=savings)) +
#                scale_fill_manual(values=c(savingsC)) + 
#                scale_color_manual(values=c(intC,noIntC,savingsC)) + 
#                guides(fill=F, alpha=F)
# 
# #Total Costs Including Sticker Price: Comparing costs of various interventions
# #Implementation cost of intervention
# costInter <- (rawData$interventionCost)/1e9
# #Total US HCS cost due to intervention
# interTot  <- interCost + costInter
# #Total additional spent by US HCS due to intervention
# totSpent  <- interTot - baseCost
# 
# costData <- data.frame(year=years, baseCost=baseCost, interCost=interTot,
#                        totSpent=totSpent)
# yrange      <- round(seq(min(costData$interCost),max(costData$interCost)+0.5,by=0.5),1)
# costsPlot <- ggplot(costData,aes(x=year)) + 
#                labs(x="Years", y="Billions of USD", color="Intervention Status") +
#                scale_y_continuous(breaks=yrange) + 
#                plotTitle("Total Spent by US Health Care System given
#                           Intervention",interventionName,
#                           "given presumed intervention cost") +
#                geom_ribbon(aes(ymin=baseCost,ymax=interCost,fill=costs, alpha=1)) + 
#                geom_line(aes(y=baseCost, color=noInt)) +
#                geom_line(aes(y=interCost, color=int)) + 
#                geom_line(aes(y=totSpent, color=costs)) +
#                scale_fill_manual(values=c(costsC)) + 
#                scale_color_manual(values=c(costsC,intC,noIntC)) + 
#                guides(fill=F, alpha=F)
# 
# #Total Cases Averted
# baseCases         <- 1e6*(baseData$progAcute0 + baseData$progAcute1 + 
#                           baseData$progChron0 + baseData$progChron1)
# baseCasesD        <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)
# intCases          <- 1e6*(rawData$progAcute0 + rawData$progAcute1 + 
#                           rawData$progChron0 + rawData$progChron1)
# intCasesD         <- 1e6*(rawData$progTotalD0 + rawData$progTotalD1)
# casesAverted      <- baseCases - intCases
# casesAvertedD     <- baseCasesD - intCasesD
# 
# casesAvertedData  <- data.frame(year=years,baseCases=baseCases,
#                                 intCases=intCases,
#                                 casesAverted=casesAverted)
# casesAvertedDataD <- data.frame(year=years,baseCases=baseCasesD,
#                                 intCases=intCasesD,
#                                 casesAverted=casesAvertedD)
# 
# yrange            <- round(seq(min(casesAvertedData$baseCases),
#                                max(casesAvertedData$baseCases),by=1e5),1)
# casesAvertedPlot  <- 
#   ggplot(casesAvertedData,aes(x=year)) + 
#   labs(x="Years", y="Cases of TB", color="Intervention Status") +
#   scale_y_continuous(breaks=yrange) + 
#   plotTitle("Total Cases of TB Averted given Intervention",interventionName) +
#   geom_ribbon(aes(ymin=intCases,ymax=baseCases,fill=averted, alpha=0.2)) + 
#   geom_line(aes(y=baseCases,    color=noInt)) +
#   geom_line(aes(y=intCases,     color=int)) + 
#   geom_line(aes(y=casesAverted, color=averted)) +
#   scale_fill_manual(values=c(avertedC)) + 
#   scale_color_manual(values=c(avertedC,intC,noIntC)) + 
#   guides(fill=F, alpha=F)
# 
# yrange            <- round(seq(min(casesAvertedDataD$baseCases),
#                                max(casesAvertedDataD$baseCases),by=1e5),1)
# casesAvertedPlotD <- 
#   ggplot(casesAvertedDataD,aes(x=year)) + 
#   labs(x="Years", y="Discounted Cases of TB", 
#        color="Intervention Status") +
#   scale_y_continuous(breaks=yrange) + 
#   plotTitle("Discounted Cases of TB Averted given Intervention",
#             interventionName) +
#   geom_ribbon(aes(ymin=intCases,ymax=baseCases,fill=averted, alpha=0.2)) + 
#   geom_line(aes(y=baseCases, color=noInt)) +
#   geom_line(aes(y=intCases, color=int)) + 
#   geom_line(aes(y=casesAverted, color=averted)) +
#   scale_fill_manual(values=c(avertedC)) + 
#   scale_color_manual(values=c(avertedC,intC,noIntC)) + 
#   guides(fill=F, alpha=F)
# 
# #Cost per cases averted graph:
# cpcaData  <- data.frame(year=yearsPC,cpca=1e9*totSpent[cutoffT:totT]/casesAverted[cutoffT:totT])
# cpcaDataD <- data.frame(year=yearsPC,cpca=1e9*totSpent[cutoffT:totT]/casesAvertedD[cutoffT:totT])
# 
# cpcaPlot  <- 
#   ggplot(cpcaData,aes(x=year)) + 
#   labs(x="Years", y="USD") +
#   scale_x_continuous(breaks=c(initialYr,cutoffYr,seq(initialYr,finalYr,25))) +
#   #scale_y_log10() + 
#   plotTitle("Cost per Raw TB Case Averted due to Intervention",
#             interventionName) +
#   geom_line(aes(y=cpca))
# 
# cpcaPlotD <- 
#   ggplot(cpcaDataD,aes(x=year)) + 
#   labs(x="Years", y="USD") +
#   scale_x_continuous(breaks=c(initialYr,cutoffYr,seq(initialYr,finalYr,25))) +
#   #scale_y_log10() + 
#   plotTitle("Cost per Discounted TB Case Averted due to Intervention",
#             interventionName) +
#   geom_line(aes(y=cpca))
# 
# #TB Deaths:
# baseDeaths         <- 1e6*(baseData$tbdeath0 + baseData$tbdeath1)
# baseDeathsD        <- 1e6*(baseData$tbdeathD0 + baseData$tbdeathD1)
# intDeaths          <- 1e6*(rawData$tbdeath0 + rawData$tbdeath1)
# intDeathsD         <- 1e6*(rawData$tbdeathD0 + rawData$tbdeathD1)
# deathsAverted      <- baseDeaths - intDeaths
# deathsAvertedD     <- baseDeathsD - intDeathsD
# 
# deathsAvertedData  <- data.frame(year=years,baseDeaths=baseDeaths,
#                                 intDeaths=intDeaths,
#                                 deathsAverted =deathsAverted)
# deathsAvertedDataD <- data.frame(year=years,baseDeaths=baseDeathsD,
#                                 intDeaths=intDeathsD,
#                                 deathsAverted=deathsAvertedD)
# 
# yrange             <- round(seq(min(deathsAvertedData$baseDeaths),
#                                max(deathsAvertedData$baseDeaths),by=5e3),1)
# deathsAvertedPlot  <- 
#   ggplot(deathsAvertedData,aes(x=year)) + 
#   labs(x="Years", y="TB Deaths", color="Intervention Status") +
#   scale_y_continuous(breaks=yrange) + 
#   plotTitle("Total TB Lives Saved Given Intervention",interventionName) +
#   geom_ribbon(aes(ymin=intDeaths,ymax=baseDeaths,fill=averted, alpha=0.2)) + 
#   geom_line(aes(y=baseDeaths,    color=noInt)) +
#   geom_line(aes(y=intDeaths,     color=int)) + 
#   geom_line(aes(y=deathsAverted, color=TBdeathsAverted)) +
#   scale_fill_manual(values=c(TBdeathsAvertedC)) + 
#   scale_color_manual(values=c(intC,noIntC,TBdeathsAvertedC)) + 
#   guides(fill=F, alpha=F)
# 
# yrange             <- round(seq(min(deathsAvertedDataD$baseDeaths),
#                                max(deathsAvertedDataD$baseDeaths),by=5e3),1)
# deathsAvertedPlotD <- 
#   ggplot(deathsAvertedDataD,aes(x=year)) + 
#   labs(x="Years", y="Discounted TB Deaths", color="Intervention Status") +
#   scale_y_continuous(breaks=yrange) + 
#   plotTitle("Discounted TB Lives Saved Given Intervention",interventionName) +
#   geom_ribbon(aes(ymin=intDeaths,ymax=baseDeaths,fill=averted, alpha=0.2)) + 
#   geom_line(aes(y=baseDeaths, color=noInt)) +
#   geom_line(aes(y=intDeaths, color=int)) + 
#   geom_line(aes(y=deathsAverted, color=TBdeathsAverted)) +
#   scale_fill_manual(values=c(TBdeathsAvertedC)) + 
#   scale_color_manual(values=c(intC,noIntC,TBdeathsAvertedC)) + 
#   guides(fill=F, alpha=F)
# 
# 
# 
# ggsave('paperRedEnLTBI.pdf',x,width=15,height=12)

fileName <- "redEnLTBI.pdf" 
pdf(fileName,onefile=T)
print(incPlot)
#print(savingsPlot)
#print(costsPlot)
#print(casesAvertedPlot)
#print(casesAvertedPlotD)
#print(cpcaPlot)
#print(cpcaPlotD)
#print(deathsAvertedPlot)
#print(deathsAvertedPlotD)
dev.off()
