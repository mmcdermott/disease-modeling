
y <- 1
for (interventionName in paperRedEnLTBIInts) {
  interventionData[[y]] <- read.csv(paste(c(intFilePrefix,interventionName,
                                       intFileSuffix),collapse=""))
  #HCS cost borne by intervention
  interHCSCost[[y]] <- (interventionData[[y]]$cN0 + interventionData[[y]]$cN1)/1e9
  #Implementation cost of intervention
  costOfInter[[y]] <- (interventionData[[y]]$interventionCost)/1e9
  #Savings from intervention
  saveOfInter[[y]] <- baseHCSCost - interHCSCost[[y]]
  print(interventionName)
  print(saveOfInter[[y]])
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


yrange <- round(seq(min(-1*saveOfInter[[1]])-0.5,max(costOfInter[[1]])+0.5,by=0.5),1)

# Plot of cost averted (savings)
# Only includes costs for Active TB treatment
# No treatment implementation costs accounted for

redEnLTBISavings <- data.frame(year = years, 
                        redEnLTBI10_savings  = saveOfInter[[1]], 
                        redEnLTBI25_savings  = saveOfInter[[2]], 
                        redEnLTBI50_savings  = saveOfInter[[3]],
                        redEnLTBI100_savings = saveOfInter[[4]])

x <- ggplot(redEnLTBISavings,aes(x=year)) + 
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

