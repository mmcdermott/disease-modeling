library(ggplot2)
source('deSolConstants.R')
source('hillFunctions.R') #for generateIncidence
source('interventionConfig.R')

#plotting Functions:
source('plotGenerators.R')

#TODO: Remove Duplicated Code
#TODO: Add active disease cost plot
#TODO: Add active disease savings plot
#TODO: Make grouping possible
#TODO: Make grouping configurable
#TODO: Fix titles
#TODO: switch line type for intervention status
#TODO: determine how to handle plot defaults for multiple interventions

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
  #Defaults: TODO: make this dependent on config set in interventionConfig.R
  allPlots       <- list(incPlot,savingsPlot,costsPlot,casesAvertedPlot,
                         casesAvertedPlotD,cpcaPlot,cpcaPlotD,deathsAvertedPlot,
                         deathsAvertedPlotD)
  plotsToPresent <- allPlots
  if (interventionType == 'redImm') {
    plotsToPresent <- list(incPlot,savingsPlot,casesAvertedPlot,
                           casesAvertedPlotD,deathsAvertedPlot,
                           deathsAvertedPlotD)
  } else if (interventionType == 'redEnLTBI') {
    plotsToPresent <- allPlots
  } else if (interventionType == 'incLTBItrmt') {
    plotsToPresent <- list(incPlot,costsPlot,casesAvertedPlot,
                           casesAvertedPlotD,cpcaPlot,cpcaPlotD,
                           deathsAvertedPlot,deathsAvertedPlotD)
  } else if (interventionType == 'redTrans') {
    plotsToPresent <- list(incPlot,savingsPlot,casesAvertedPlot,
                           casesAvertedPlotD,deathsAvertedPlot,
                           deathsAvertedPlotD)
  } else {
    print("I didn't recognize that intervention, so I'm plotting everything")
    print(paste(c("Intervention:",intervention),collapse=" "))
  }

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
