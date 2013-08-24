#
# Generates table showing savings for redEnLTBI interventions
# (assuming default cost of curing LTBI, which is ...............)
#

library(xtable) # library for turning R objects into LaTeX tables
source('interventionAnalyzer.R')



### PART 1 - GENERATE DATA FROM MODEL ###

#Make sure you reset all intervention costs to default values in interventionConfig.R before running!!!

# Set of interventions
redEnLTBI_Interventions_paper <- c('redEnLTBI20', 'redEnLTBI35', 'redEnLTBI50', 'redEnLTBI65')

baseData <- read.csv(baseFile)
#Base Incidence
baseInc <- generateIncidence(baseData)
#US Health Care System (HCS) TB costs due to base system
baseHCSCost <- (baseData$cN0 + baseData$cN1)/1e9
baseCasesD <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)

#Costs related to Interventions
for(intervention in redEnLTBI_Interventions_paper) {
  intConfig <- interventionConfig(intervention)
  costs     <- intConfig$costs
  params    <- intConfig$params
  interData <- hill(costs,params[["sigmaL"]],params[["f"]],params[["trans"]],
                    params[["incLTBI"]])
  write.csv(interData, paste(c(intFilePrefix,intervention,intFileSuffix),
                             collapse=""))
}



### PART 2 - ORGANIZE DATA INTO MEANINGFUL CHUNKS ###

#Graphs (actually, not really)
interventionData <- as.list(1:4)
interHCSCost <- as.list(1:4)
costOfInter <- as.list(1:4)  #additional cost of intervention
saveOfInter <- as.list(1:4)  #additional savings of intervention
interTot <- as.list(1:4)
totSpent <- as.list(1:4)  #net cost

y <- 1
for (interventionName in redEnLTBI_Interventions_paper) {
  
  interventionData[[y]] <- read.csv(paste(c(intFilePrefix,interventionName,
                                       intFileSuffix),collapse=""))
  #HCS cost borne by intervention
  interHCSCost[[y]] <- (interventionData[[y]]$cN0 + interventionData[[y]]$cN1)/1e9
  #Implementation cost of intervention
  costOfInter[[y]] <- (interventionData[[y]]$interventionCost)/1e9
  #Savings from intervention
  saveOfInter[[y]] <- baseHCSCost - interHCSCost[[y]]
  #print(interventionName)
  #print(saveOfInter[[y]])
  #Total US HCS cost due to intervention
  interTot[[y]] <- interHCSCost[[y]] + costOfInter[[y]]
  #Total additional spent by US HCS due to intervention
  totSpent[[y]]  <- interTot[[y]] - baseHCSCost 
  
  #Cost per cases averted
  intCasesD         <- 1e6*(interventionData[[y]]$progTotalD0 + interventionData[[y]]$progTotalD1)
  casesAvertedD     <- baseCasesD - intCasesD
  cpcaDataD <- 1e9*totSpent[[y]]/casesAvertedD
  
  y <- y + 1

}



### PART 3 - PUT IT ALL IN A TABLE ###

# Sample 5 points in time
results <- data.frame(red20=rep(0,5), red35=rep(0,5), red50=rep(0,5), red65=rep(0,5), row.names = c('2000', '2025', '2050', '2075', '2100'))
for (interventionNumber in 1:4) {
    results[,interventionNumber] <- saveOfInter[[interventionNumber]][seq(1, totT, floor(totT/4))]
}
print(results)
res.table <- xtable(results, digits=6, align="|r|cccc|")

# output to file
sink("intrvTable.tex", append=FALSE, split=FALSE)
print(res.table)
sink()
