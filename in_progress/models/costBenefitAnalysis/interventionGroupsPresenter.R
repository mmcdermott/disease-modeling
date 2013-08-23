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

#Graphs
interHCSCost <- as.list(1:3)
costOfInter <- as.list(1:3)  #additional cost of intervention
saveOfInter <- as.list(1:3)  #additional savings of intervention
interTot <- as.list(1:3)
totSpent <- as.list(1:3)  #net cost

y <- 1
for (interventionName in redEnLTBI_Interventions) {
  interventionData <- read.csv(paste(c(intFilePrefix,interventionName,
                                       intFileSuffix),collapse=""))
  #HCS cost borne by intervention
  interHCSCost[[y]] <- (interventionData$cN0 + interventionData$cN1)/1e9
  #Implementation cost of intervention
  costOfInter[[y]] <- (interventionData$interventionCost)/1e9
  #Savings from intervention
  saveOfInter[[y]] <- baseHCSCost - interHCSCost[[y]]
  #Total US HCS cost due to intervention
  interTot[[y]] <- interHCSCost[[y]] + costOfInter[[y]]
  #Total additional spent by US HCS due to intervention
  totSpent[[y]]  <- interTot[[y]] - baseHCSCost 
  
  #Cost per cases averted
  intCasesD         <- 1e6*(interventionData$progTotalD0 + interventionData$progTotalD1)
  casesAvertedD     <- baseCasesD - intCasesD
  cpcaDataD <- 1e9*totSpent[[y]]/casesAvertedD
  
  y <- y + 1
}

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

yrange <- round(seq(min(-1*saveOfInter[[1]])-0.5,max(costOfInter[[1]])+0.5,by=0.5),1)

#redEnLTBI <- data.frame(year = years, 
#                        redEnLTBI100_costs   = costOfInter[[1]], 
#                        redEnLTBI75_costs    = costOfInter[[2]], 
#                        redEnLTBI50_costs    = costOfInter[[3]],
#                        redEnLTBI100_savings = -1*saveOfInter[[1]], 
#                        redEnLTBI75_savings  = -1*saveOfInter[[2]], 
#                        redEnLTBI50_savings  = -1*saveOfInter[[3]],
#                        redEnLTBI100_totCost = totSpent[[1]], 
#                        redEnLTBI75_totCost  = totSpent[[2]], 
#                        redEnLTBI50_totCost  = totSpent[[3]])
#
#x <- ggplot(redEnLTBI,aes(x=year)) + 
#       labs(x="Years", y="Billions of USD", color="Economic Distinction", 
#            linetype="% Reduction") +
#       scale_y_continuous(breaks=yrange) + 
#       plotTitle("Implementation Costs, Savings, and 
#US Health Care System (US HCS) 
#Costs for 100%, 75%, and 50% LTBI reduction","") + 
#       geom_hline(aes(y=0),size=2) +
#       geom_line(aes(y=redEnLTBI100_costs,   color=costs,      linetype=redEnLTBI100L), size=2) +
#       geom_line(aes(y=redEnLTBI75_costs,    color=costs,      linetype=redEnLTBI75L), size=2) +
#       geom_line(aes(y=redEnLTBI50_costs,    color=costs,      linetype=redEnLTBI50L), size=2) +
#       geom_line(aes(y=redEnLTBI100_savings, color=savings,    linetype=redEnLTBI100L), size=2) +
#       geom_line(aes(y=redEnLTBI75_savings,  color=savings,    linetype=redEnLTBI75L), size=2) +
#       geom_line(aes(y=redEnLTBI50_savings,  color=savings,    linetype=redEnLTBI50L), size=2) +
#       geom_line(aes(y=redEnLTBI100_totCost, color=totalCosts, linetype=redEnLTBI100L), size=2) +
#       geom_line(aes(y=redEnLTBI75_totCost,  color=totalCosts, linetype=redEnLTBI75L), size=2) +
#       geom_line(aes(y=redEnLTBI50_totCost,  color=totalCosts, linetype=redEnLTBI50L), size=2) +
#       guides(fill=F, alpha=F) + 
#       theme_gray(base_size=25) +
#       theme(legend.position=c(0.22,0.82), axis.title=element_text(size=40), 
#             axis.text=element_text(size=27), plot.title=element_text(size=40))
#             #legend.key.height=unit(1.8,'line')) + 
#
#ggsave('costRedEnLTBI.pdf',x,width=15,height=12)

# Plot of cost averted (savings)
# Only includes costs for Active TB treatment
# No treatment implementation costs accounted for

redEnLTBI <- data.frame(year = years, 
                        redEnLTBI10_savings = saveOfInter[[1]], 
                        redEnLTBI25_savings  = saveOfInter[[2]], 
                        redEnLTBI50_savings  = saveOfInter[[3]])

x <- ggplot(redEnLTBI,aes(x=year)) + 
       labs(x="Years", y="Billions of USD", color="Economic Distinction", 
            linetype="% Reduction") +
       #scale_y_continuous(breaks=yrange) + 
       plotTitle("Averted TB Costs for US HCS given 
10%, 25%, and 50% LTBI reduction","") + 
       geom_hline(aes(y=0),size=2) +
       geom_line(aes(y=redEnLTBI10_savings,  color=savings,    linetype=redEnLTBI10L), size=2) +
       geom_line(aes(y=redEnLTBI25_savings,  color=savings,    linetype=redEnLTBI25L), size=2) +
       geom_line(aes(y=redEnLTBI50_savings,  color=savings,    linetype=redEnLTBI50L), size=2) +
       guides(fill=F, alpha=F) + 
       theme_gray(base_size=25) +
       theme(legend.position=c(0.22,0.82), axis.title=element_text(size=40), 
             axis.text=element_text(size=27), plot.title=element_text(size=40)) +
       scale_color_manual(values=c(savingsC))
			 

ggsave('costAvertedRedEnLTBI.pdf',x,width=15,height=12)
