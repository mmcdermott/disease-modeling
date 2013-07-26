source('interventionGroups.R')

baseData <- read.csv(baseFile)
#Base Incidence
baseInc <- generateIncidence(baseData)
#US Health Care System (HCS) TB costs due to base system
baseHCSCost <- (baseData$cN0 + baseData$cN1)/1e9
baseCasesD <- 1e6*(baseData$progTotalD0 + baseData$progTotalD1)

# #Tables
# redEnLTBIcostTable <- matrix(nrow=5,ncol=30)
# redEnLTBIcpcaDTable <- matrix(nrow=5,ncol=30)
# colNum <- 1

# for (x in 0:9) {
  # for (interventionName in redEnLTBI_Interventions) {
    # interventionData <- read.csv(paste(c(intFilePrefix,interventionName,x,
                                         # intFileSuffix),collapse=""))
    # #HCS cost borne by intervention
    # interHCSCost <- (interventionData$cN0 + interventionData$cN1)/1e9
    # #Implementation cost of intervention
    # costOfInter <- (interventionData$interventionCost)/1e9
    # #Total US HCS cost due to intervention
    # interTot  <- interHCSCost + costOfInter
    # #Total additional spent by US HCS due to intervention
    # totSpent  <- interTot - baseHCSCost
    
    # #Cost per cases averted
    # intCasesD         <- 1e6*(interventionData$progTotalD0 + interventionData$progTotalD1)
    # casesAvertedD     <- baseCasesD - intCasesD
    # cpcaDataD <- 1e9*totSpent/casesAvertedD
    
    # #Add data to matrix column
    # for( rowNum in 1:5 ) {
      # redEnLTBIcostTable[rowNum,colNum] <- totSpent[rowNum*25]  #depends on time step, 25 = TotT / 5
      # redEnLTBIcpcaDTable[rowNum,colNum] <- cpcaDataD[rowNum*25]
    # }
    # colNum <- colNum + 1
  # }
# }
