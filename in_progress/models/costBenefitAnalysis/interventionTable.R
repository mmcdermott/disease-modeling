#
# Generates table showing savings for redEnLTBI interventions
# (assuming default cost of curing LTBI, which is ...............)
#

library(xtable) # library for turning R objects into LaTeX tables
source('interventionAnalyzer.R')



### PART 1 - GENERATE DATA FROM MODEL ###

#Make sure you reset all intervention costs to default values in interventionConfig.R before running!!!

# Set of interventions
redEnLTBI_Interventions_paper <- c('redEnLTBI5', 'redEnLTBI10', 'redEnLTBI25', 'redEnLTBI50')

baseData <- read.csv(baseFile)
#Base Incidence
baseInc <- generateIncidence(baseData)
#US Health Care System (HCS) TB costs due to base system
baseHCSCost <- (baseData$cN0 + baseData$cN1)/1e9
baseCasesD <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)
baseDeathsD <- 1e6*(baseData$tbdeathD0 + baseData$tbdeathD1)

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

#Containers
interventionData <- as.list(1:4)
interHCSCost <- as.list(1:4)
costOfInter <- as.list(1:4)  #additional cost of intervention
saveOfInter <- as.list(1:4)  #additional savings of intervention
interTot <- as.list(1:4)
totSpent <- as.list(1:4)  #net cost
intCasesD <- as.list(1:4)
casesAvertedD <- as.list(1:4)
cpcaDataD <- as.list(1:4) #cost per case averted
intDeathsD <- as.list(1:4)
deathsAvertedD <- as.list(1:4) #TB deaths averted

y <- 1
for (interventionName in redEnLTBI_Interventions_paper) {
  
  interventionData[[y]] <- read.csv(paste(c(intFilePrefix,interventionName,
                                       intFileSuffix),collapse=""))
  #HCS cost borne by intervention [in billions $]
  interHCSCost[[y]] <- (interventionData[[y]]$cN0 + interventionData[[y]]$cN1)/1e9
  #Implementation cost of intervention [in billions $]
  costOfInter[[y]] <- (interventionData[[y]]$interventionCost)/1e9
  #Savings from intervention [in billions $]
  saveOfInter[[y]] <- baseHCSCost - interHCSCost[[y]]
  #print(interventionName)
  #print(saveOfInter[[y]])
  #Total US HCS cost due to intervention [in billions $]
  interTot[[y]] <- interHCSCost[[y]] + costOfInter[[y]]
  #Total additional spent by US HCS due to intervention [in billions $]
  totSpent[[y]]  <- interTot[[y]] - baseHCSCost 
  
  intCasesD[[y]]         <- 1e6*(interventionData[[y]]$progTotalD0 + interventionData[[y]]$progTotalD1)
  #Number of TB cases averted [in people]
  casesAvertedD[[y]]     <- baseCasesD - intCasesD[[y]]
  #Cost per cases averted [in $/case]
  cpcaDataD[[y]] <- 1e9*totSpent[[y]]/casesAvertedD[[y]]

  intDeathsD[[y]]     <- 1e6*(interventionData[[y]]$tbdeathD0 + interventionData[[y]]$tbdeathD1)
  #Number of TB deaths averted [in people]
  deathsAvertedD[[y]] <- baseDeathsD - intDeathsD[[y]]
  
  y <- y + 1

}



### PART 3 - PUT IT ALL IN A TABLE ###

# Sample 5 points in time
rownames <- c('2000', '2025', '2050', '2075', '2100')

# Savings from Intervention
svngResults <- data.frame(red5=rep(0,5), red10=rep(0,5), red25=rep(0,5), red50=rep(0,5), row.names = rownames)
for (interventionNumber in 1:4) {
    svngResults[,interventionNumber] <- saveOfInter[[interventionNumber]][seq(1, totT, floor(totT/4))]
}
print(svngResults)
svngRes.table <- xtable(svngResults, digits=6, align="|r|cccc|", caption = "Savings from Reducing Incoming LTBI by X percent (in billions of dollars)")


# Additional Cost of Intervention
aciResults <- data.frame(red5=rep(0,5), red10=rep(0,5), red25=rep(0,5), red50=rep(0,5), row.names = rownames)
for (interventionNumber in 1:4) {
    aciResults[,interventionNumber] <- totSpent[[interventionNumber]][seq(1, totT, floor(totT/4))]
}
print(aciResults)
aciRes.table <- xtable(aciResults, digits=6, align="|r|cccc|", caption = "Additional Cost of Reducing Incoming LTBI by X percent (in billions of dollars)")

# Number of Case Averted
ncavResults <- data.frame(red5=rep(0,5), red10=rep(0,5), red25=rep(0,5), red50=rep(0,5), row.names = rownames)
for (interventionNumber in 1:4) {
    ncavResults[,interventionNumber] <- casesAvertedD[[interventionNumber]][seq(1, totT, floor(totT/4))]
}
print(ncavResults)
ncavRes.table <- xtable(ncavResults, align="|r|cccc|", caption = "Cases of TB Averted by Reducing Incoming LTBI by X percent")

# Cost per Case Averted
cpcaResults <- data.frame(red5=rep(0,5), red10=rep(0,5), red25=rep(0,5), red50=rep(0,5), row.names = rownames)
for (interventionNumber in 1:4) {
    cpcaResults[,interventionNumber] <- cpcaDataD[[interventionNumber]][seq(1, totT, floor(totT/4))]
}
print(cpcaResults)
cpcaRes.table <- xtable(cpcaResults, align="|r|cccc|", caption = "Cost Per Case Averted by Reducing Incoming LTBI by X percent (in dollar per case)")

# Number of Deaths Averted
ndavResults <- data.frame(red5=rep(0,5), red10=rep(0,5), red25=rep(0,5), red50=rep(0,5), row.names = rownames)
for (interventionNumber in 1:4) {
    ndavResults[,interventionNumber] <- deathsAvertedD[[interventionNumber]][seq(1, totT, floor(totT/4))]
}
print(ndavResults)
ndavRes.table <- xtable(ndavResults, align="|r|cccc|", caption = "TB Deaths Averted by Reducing Incoming LTBI by X percent")

# output to file
sink("intrvTable.tex", append=FALSE, split=FALSE)
print(svngRes.table)
print(aciRes.table)
print(ncavRes.table)
print(cpcaRes.table)
print(ndavRes.table)
sink()
