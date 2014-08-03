
# Creating our data containers.
rawData      <- data.frame(years=years,base=baseData)# as.list(paperRedEnLTBIInts)
incidence    <- data.frame(years=years,baseUSB=baseInc$IN0,
                                       baseFB =baseInc$IN1,
                                       baseAll=baseInc$INall)# as.list(paperRedEnLTBIInts)
HCSCost      <- data.frame(years=years)# as.list(paperRedEnLTBIInts)
costOfInter  <- data.frame(years=years)# as.list(paperRedEnLTBIInts)  
saveOfInter  <- data.frame(years=years)# as.list(paperRedEnLTBIInts)  
interTot     <- data.frame(years=years)# as.list(paperRedEnLTBIInts)
totSpent     <- data.frame(years=years)# as.list(paperRedEnLTBIInts) 
cpca         <- data.frame(years=years)# as.list(paperRedEnLTBIInts)
casesAverted <- data.frame(years=years)# as.list(paperRedEnLTBIInts)
# Naming them for convenience.
# names(interventionData) <- paperRedEnLTBIInts
# names(interventionInc)  <- paperRedEnLTBIInts
# names(interHCSCost)     <- paperRedEnLTBIInts
# names(costOfInter)      <- paperRedEnLTBIInts
# names(saveOfInter)      <- paperRedEnLTBIInts
# names(interTot)         <- paperRedEnLTBIInts
# names(totSpent)         <- paperRedEnLTBIInts
# names(cpca)             <- paperRedEnLTBIInts
# names(casesAverted)     <- paperRedEnLTBIInts
