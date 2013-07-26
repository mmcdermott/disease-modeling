acuteLatentFBL    <- rep("Acute Latent (FB)",totT)
chronicLatentFBL  <- rep("Chronic Latent (FB)", totT)
acuteLatentUSBL   <- rep("Acute Latent (USB)",totT)
chronicLatentUSBL <- rep("Chronic Latent (USB)", totT)
activeInfecFBL    <- rep("Active Infectious TB (FB)",totT)
activeNoInfecFBL  <- rep("Active Non-infectious TB (FB)", totT)
activeInfecUSBL   <- rep("Active Infectious TB (USB)",totT)
activeNoInfecUSBL <- rep("Active Non-infectious TB (USB)", totT)

generateSourcedIncidence <- function(dataSet) {
  with(as.list(parms), {
    IdF0  <- 1e6 * vF  * dataSet$F0
    IdF1  <- 1e6 * vF  * dataSet$F1
    IdL0  <- 1e6 * vL0 * dataSet$L0
    IdL1  <- 1e6 * vL1 * dataSet$L1
	  return(data.frame(IdF0,IdF1,IdL0,IdL1))
  })
}

baseSourcedInc <- generateSourcedIncidence(baseData)
IdL1c <- baseSourcedInc$IdL1
IdF1c <- IdL1c + baseSourcedInc$IdF1
IdL0c <- IdF1c + baseSourcedInc$IdL0
IdF0c <- IdL0c + baseSourcedInc$IdF0
baseCumulativeSourcedInc <- data.frame(year=years,IdL1c,IdF1c,IdL0c,IdF0c)
incPlotSourced <- ggplot(baseCumulativeSourcedInc, aes(x=year)) + 
           labs(x="Years", y="Incidence", fill="Population and Health State") + 
           plotTitle("Incidence Levels due to Various 
Populations With No Intervention","") +
           geom_ribbon(aes(ymin=0,    ymax=IdL1c,fill=chronicLatentFBL))  + 
           geom_ribbon(aes(ymin=IdL1c,ymax=IdF1c,fill=acuteLatentFBL))    + 
           geom_ribbon(aes(ymin=IdF1c,ymax=IdL0c,fill=chronicLatentUSBL)) + 
           geom_ribbon(aes(ymin=IdL0c,ymax=IdF0c,fill=acuteLatentUSBL))


#Order: IF1 = IF1c, IF1 + IL1 = IL1c 
IF1c <- baseTotalInc$IF1
IL1c <- IF1c + baseTotalInc$IL1
II1c <- IL1c + baseTotalInc$II1
IJ1c <- II1c + baseTotalInc$IJ1
IF0c <- IJ1c + baseTotalInc$IF0
IL0c <- IF0c + baseTotalInc$IL0
II0c <- IL0c + baseTotalInc$II0
IJ0c <- II0c + baseTotalInc$IJ0
baseCumulativeInc <- data.frame(year=years,IF1c,IL1c,II1c,IJ1c,IF0c,IL0c,II0c,IJ0c)
incPlotTotalAll <- ggplot(baseCumulativeInc, aes(x=year)) + 
           labs(x="Years", y="Incidence", fill="Population and Health State") + 
           plotTitle("Incidence Levels for Various Populations With No Intervention","") +
           geom_ribbon(aes(ymin=0,   ymax=IF1c,fill=acuteLatentFBL))    + 
           geom_ribbon(aes(ymin=IF1c,ymax=IL1c,fill=chronicLatentFBL))  + 
           geom_ribbon(aes(ymin=IL1c,ymax=II1c,fill=activeInfecFBL))    + 
           geom_ribbon(aes(ymin=II1c,ymax=IJ1c,fill=activeNoInfecFBL))  + 
           geom_ribbon(aes(ymin=IJ1c,ymax=IF0c,fill=acuteLatentUSBL))   + 
           geom_ribbon(aes(ymin=IF0c,ymax=IL0c,fill=chronicLatentUSBL)) + 
           geom_ribbon(aes(ymin=IL0c,ymax=II0c,fill=activeInfecUSBL))   + 
           geom_ribbon(aes(ymin=II0c,ymax=IJ0c,fill=activeNoInfecUSBL))


intLabels <- function(type,magnitude) {
  redEnLTBI100L   <- rep("100% Reduction in Entering LTBI", totT)
  redEnLTBI75L    <- rep("75% Reduction in Entering LTBI",  totT)
  redEnLTBI50L    <- rep("50% Reduction in Entering LTBI",  totT)
  redImm50L       <- rep("50% Reduction in Immigration",    totT)
  redImm75L       <- rep("75% Reduction in Immigration",    totT)
  incLTBItrmt100L <- rep("100% Increase in Immigration",    totT)
  incLTBItrmt300L <- rep("300% Increase in Immigration",    totT)
  redTrans100L    <- rep("100% Reduction in Transmission",  totT)
  #TODO: This is not the best way to do this
  return(eval(as.name(paste(c(type,magnitude,'L'),collapse=""))))
}

interventionTypes <- list('redEnLTBI','redImm','incLTBItrmt','redTrans')
interventionMags <- list(redEnLTBI=list(50,75,100),redImm=list(50,75),
                         incLTBItrmt=list(100,300),redTrans=list(100))
incDataTypeGrouped <- list()
for (interventionType in interventionTypes) {
  dataL = list()
  mags  = interventionMags[[interventionType]]
  for (mag in mags) {
    magnitude = as.character(mag)
    dataName  = paste(c(interventionType,magnitude),collapse="")
    dataL[[magnitude]] = incDataList[[dataName]]
  }
  incDataTypeGrouped[[interventionType]] = dataL
}

incPlotTypeGroupedG <- function(type, incDataTypeGrouped=incDataTypeGrouped) {
  data <- incDataTypeGrouped[[type]]
  plot <- ggplot() + 
    scale_y_log10(breaks=c(1,2,5,10,25,50,100,200),
                 labels=c("Elimination (1)",2,5,10,25,50,100,200),
                 limits=c(0.5,250)) + 
    labs(x="Years", y="Incidence/million", color="Population", 
          linetype="Intervention Status") + 
    plotTitle("Incidence Levels for Various Intervention Magnitudes","") +
    geom_line(data = data.frame(year=years, baseInc), aes(x=year, y=IN0,   
              color=USB, linetype=noInt))  + 
    geom_line(data = data.frame(year=years, baseInc), aes(x=year, y=IN1,   
              color=FB,  linetype=noInt))  + 
    geom_line(data = data.frame(year=years, baseInc), aes(x=year, y=INall, 
              color=all, linetype=noInt))
  for (magnitude in names(data)) {
    incData   <- data.frame(year=years,data[[magnitude]],
                            label=intLabels(type,magnitude))
    plot <- plot + 
    geom_line(data=incData, aes(x=year, y=IN0,   color=USB, linetype=label))+ 
    geom_line(data=incData, aes(x=year, y=IN1,   color=FB,  linetype=label))+ 
    geom_line(data=incData, aes(x=year, y=INall, color=all, linetype=label))
  }
  return(plot) 
}

incPlot <- ggplot(incData, aes(x=year)) + 
           scale_y_log10(breaks=c(1,2,5,10,25,50,100,200),
                         labels=c("Elimination (1)",2,5,10,25,50,100,200),
                         limits=c(0.5,250)) + 
           labs(x="Years", y="Incidence/million", color="Population", 
                  linetype="Intervention Status") + 
           plotTitle("Incidence Levels for Various Reductions of Entering LTBI","") +
           geom_line(aes(y=baseUSB, color=USB, linetype=noInt))  + 
           geom_line(aes(y=baseFB,  color=FB,  linetype=noInt))  + 
           geom_line(aes(y=baseAll, color=all, linetype=noInt))  + 
           geom_line(aes(y=redEnLTBI100USB, color=USB, linetype=redEnLTBI100L))  + 
           geom_line(aes(y=redEnLTBI100FB, color=FB, linetype=redEnLTBI100L))  + 
           geom_line(aes(y=redEnLTBI100All, color=all, linetype=redEnLTBI100L))  + 
           geom_line(aes(y=redEnLTBI75USB, color=USB, linetype=redEnLTBI75L))  + 
           geom_line(aes(y=redEnLTBI75FB, color=FB, linetype=redEnLTBI75L))  + 
           geom_line(aes(y=redEnLTBI75All, color=all, linetype=redEnLTBI75L))  + 
           geom_line(aes(y=redEnLTBI50USB, color=USB, linetype=redEnLTBI50L))  + 
           geom_line(aes(y=redEnLTBI50FB, color=FB, linetype=redEnLTBI50L))  + 
           geom_line(aes(y=redEnLTBI50All, color=all, linetype=redEnLTBI50L))
