#TODO: Make all values named vectors
noIntLs              <- "No Intervention"
noIntL               <- rep(noInts,totT)

acuteLatentFBLs      <- "FB Acute LTBI"
acuteLatentFBL       <- rep(acuteLatentFBLs,totT)
chronicLatentFBLs    <- "FB Chronic LTBI"
chronicLatentFBL     <- rep(chronicLatentFBLs, totT)
acuteLatentUSBLs     <- "USB Acute LTBI"
acuteLatentUSBL      <- rep(acuteLatentUSBLs,totT)
chronicLatentUSBLs   <- "USB Chronic LTBI"
chronicLatentUSBL    <- rep(chronicLatentUSBLs, totT)

#light color scheme
totalPopC            <- "#C2EE6B"
totalPopChronicLTBIC <- "#DEF7AC"
totalPopAcuteLTBIC   <- "#D5F791"
totalPopInfecTBC     <- "#739B23"
totalPopNonInfecTBC  <- "#9AB269"

FBC                  <- "#459999"
FBChronicLTBIC       <- "#8ECCCC"
FBAcuteLTBIC         <- "#78CCCC"
FBInfecTBC           <- "#166363"
FBNonInfecTBC        <- "#437373"

USBC                 <- "#FF7373"
USBChronicLTBIC      <- "#FFB2B2"
USBAcuteLTBIC        <- "#FF9696"
USBInfecTBC          <- "#A62525"
USBNonInfecTBC       <- "#BF7070"
                   
#dark color scheme
totalPopC            <- "#9FEE00"
totalPopChronicLTBIC <- "#C9F76F"
totalPopAcuteLTBIC   <- "#86B22D"
totalPopInfecTBC     <- "#B9F73E"
totalPopNonInfecTBC  <- "#679B00"

FBC                  <- "#009999"
FBChronicLTBIC       <- "#5CCCCC"
FBAcuteLTBIC         <- "#1D7373"
FBInfecTBC           <- "#33CCCC"
FBNonInfecTBC        <- "#006363"

USBC                 <- "#FF0000"
USBChronicLTBIC      <- "#FF7373"
USBAcuteLTBIC        <- "#BF3030"
USBInfecTBC          <- "#FF4040"
USBNonInfecTBC       <- "#A60000"

#old colors
acuteLatentFBc       <- "#FFA573"
chronicLatentFBc     <- "#FFC973"
acuteLatentUSBc      <- "#6999D3"
chronicLatentUSBc    <- "#5FD3B3"
latentFBc            <- "#FFB773"
latentUSBc           <- "#64B6C3"
activeTBFBdL1c       <- "#AA4DA6"
activeTBFBdF1c       <- "#D56097"
activeTBUSBdL0c      <- "#BFF27A"
activeTBUSBdF0c      <- "#FFFE80"

activeInfecFBL       <- rep("FB Active Infectious TB",totT)
activeNoInfecFBL     <- rep("FB Active Non-infectious TB", totT)
activeInfecUSBL      <- rep("USB Active Infectious TB",totT)
activeNoInfecUSBL    <- rep("USB Active Non-infectious TB", totT)

latentFBLs           <- "FB LTBI"
latentFBL            <- rep(latentFBLs,totT)
latentUSBLs          <- "USB LTBI"
latentUSBL           <- rep(latentUSBLs,totT)
activeTBFBdL1Ls      <- "FB Active TB due to Activation of LTBI"
activeTBFBdL1L       <- rep(activeTBFBdL1Ls,totT)
activeTBFBdF1Ls      <- "FB Active TB due to Novel Infection"
activeTBFBdF1L       <- rep(activeTBFBdF1Ls,totT)
activeTBUSBdL0Ls     <- "USB Active TB due to Activation of LTBI"
activeTBUSBdL0L      <- rep(activeTBUSBdL0Ls,totT)
activeTBUSBdF0Ls     <- "USB Active TB due to Novel Infection"
activeTBUSBdF0L      <- rep(activeTBUSBdF0Ls,totT)

#generateSourcedIncidence <- function(dataSet) {
#  with(as.list(parms), {
#    IdF0  <- 1e6 * vF  * dataSet$F0
#    IdF1  <- 1e6 * vF  * dataSet$F1
#    IdL0  <- 1e6 * vL0 * dataSet$L0
#    IdL1  <- 1e6 * vL1 * dataSet$L1
#	  return(data.frame(IdF0,IdF1,IdL0,IdL1))
#  })
#}
#
#baseSourcedInc <- generateSourcedIncidence(baseData)
#IdL1c <- baseSourcedInc$IdL1
#IdF1c <- IdL1c + baseSourcedInc$IdF1
#IdL0c <- IdF1c + baseSourcedInc$IdL0
#IdF0c <- IdL0c + baseSourcedInc$IdF0
#baseCumulativeSourcedInc <- data.frame(year=years,IdL1c,IdF1c,IdL0c,IdF0c)
#scale_fill_values <- c()
#scale_fill_values[[acuteLatentUSBLs]]   <- USBAcuteLTBIC
#scale_fill_values[[chronicLatentUSBLs]] <- USBChronicLTBIC
#scale_fill_values[[acuteLatentFBLs]]    <- FBAcuteLTBIC
#scale_fill_values[[chronicLatentFBLs]]  <- FBChronicLTBIC
#incPlotSourced <- ggplot(baseCumulativeSourcedInc, aes(x=year)) + 
#           labs(x="Years", y="Incidence", fill="Incidence Source") + 
#           plotTitle("Sourced US TB Incidence","") +
#           geom_ribbon(aes(ymin=0,    ymax=IdL1c,fill=chronicLatentFBL))  + 
#           geom_ribbon(aes(ymin=IdL1c,ymax=IdF1c,fill=acuteLatentFBL))    + 
#           geom_ribbon(aes(ymin=IdF1c,ymax=IdL0c,fill=chronicLatentUSBL)) + 
#           geom_ribbon(aes(ymin=IdL0c,ymax=IdF0c,fill=acuteLatentUSBL))   + 
#           theme_gray(base_size=25) + 
#           theme(legend.position=c(0.6,0.8), axis.title=element_text(size=30), 
#           axis.text=element_text(size=17), plot.title=element_text(size=30),
#           legend.key.height=unit(1.8,'line')) + 
#           coord_fixed(ratio=1/200) + 
#           scale_fill_manual(breaks=c(acuteLatentUSBLs, 
#                                      chronicLatentUSBLs,
#                                      acuteLatentFBLs,
#                                      chronicLatentFBLs), 
#                             values=scale_fill_values)
#ggsave('forPoster/incPlotSourced2.pdf',incPlotSourced,width=10,height=8)
>>>>>>> 922876bf9af44fa2f7e61e41ccd64bea4d0e5709

#Order: IF1 = IF1c, IF1 + IL1 = IL1c 
#IF1c <- baseTotalInc$IF1
#IL1c <- IF1c + baseTotalInc$IL1
#II1c <- IL1c + baseTotalInc$II1
#IJ1c <- II1c + baseTotalInc$IJ1
#IF0c <- IJ1c + baseTotalInc$IF0
#IL0c <- IF0c + baseTotalInc$IL0
#II0c <- IL0c + baseTotalInc$II0
#IJ0c <- II0c + baseTotalInc$IJ0
#baseCumulativeInc <- data.frame(year=years,IF1c,IL1c,II1c,IJ1c,IF0c,IL0c,II0c,IJ0c)
#incPlotTotalAll <- ggplot(baseCumulativeInc, aes(x=year)) + 
#           labs(x="Years", y="Incidence", fill="Population and Health State") + 
#           plotTitle("Incidence Levels for Various Populations With No Intervention","") +
#           geom_ribbon(aes(ymin=0,   ymax=IF1c,fill=acuteLatentFBL))    + 
#           geom_ribbon(aes(ymin=IF1c,ymax=IL1c,fill=chronicLatentFBL))  + 
#           geom_ribbon(aes(ymin=IL1c,ymax=II1c,fill=activeInfecFBL))    + 
#           geom_ribbon(aes(ymin=II1c,ymax=IJ1c,fill=activeNoInfecFBL))  + 
#           geom_ribbon(aes(ymin=IJ1c,ymax=IF0c,fill=acuteLatentUSBL))   + 
#           geom_ribbon(aes(ymin=IF0c,ymax=IL0c,fill=chronicLatentUSBL)) + 
#           geom_ribbon(aes(ymin=IL0c,ymax=II0c,fill=activeInfecUSBL))   + 
#           geom_ribbon(aes(ymin=II0c,ymax=IJ0c,fill=activeNoInfecUSBL))
#
#  
intLabels <- function(type,magnitude,vec=T) {
  redEnLTBI100Ls   <- "100% Entering LTBI Cured"
  redEnLTBI75Ls    <- "75% Entering LTBI Cured"
  redEnLTBI50Ls    <- "50% Entering LTBI Cured"
  redImm50Ls       <- "50% Immigration Reduction"
  redImm75Ls       <- "75% Immigration Reduction"
  incLTBItrmt100Ls <- "100% LTBI Treatment Increase"
  incLTBItrmt300Ls <- "300% LTBI Treatment Increase"
  redTrans100Ls    <- "100% Reduction in Transmission"
  redEnLTBI100L    <- rep("100% Entering LTBI Cured",        totT)
  redEnLTBI75L     <- rep("75% Entering LTBI Cured",         totT)
  redEnLTBI50L     <- rep("50% Entering LTBI Cured",         totT)
  redImm50L        <- rep("50% Immigration Reduction",       totT)
  redImm75L        <- rep("75% Immigration Reduction",       totT)
  incLTBItrmt100L  <- rep("100% LTBI Treatment Increase",    totT)
  incLTBItrmt300L  <- rep("300% LTBI Treatment Increase",    totT)
  redTrans100L     <- rep("100% Reduction in Transmission",  totT)
  #TODO: This is not the best way to do this
  if (vec) {
    return(eval(as.name(paste(c(type,magnitude,'L'),collapse=""))))
  } else {
    return(eval(as.name(paste(c(type,magnitude,'Ls'),collapse=""))))
  }
}

intLinetypes <- function(type,magnitude) {
  redEnLTBI100lt   <- 2
  redEnLTBI75lt    <- 3
  redEnLTBI50lt    <- 4
  redImm50lt       <- 2
  redImm75lt       <- 3
  incLTBItrmt100lt <- 2
  incLTBItrmt300lt <- 3
  redTrans100lt    <- 2
  #TODO: This is not the best way to do this
  return(eval(as.name(paste(c(type,magnitude,'lt'),collapse=""))))
}

interventionTypes <- list('redEnLTBI','redImm','incLTBItrmt','redTrans')
interventionMags <- list(redEnLTBI=list(50,75,100),redImm=list(50,75),
                         incLTBItrmt=list(100,300),redTrans=list(100))
incDataTypeGrouped <- list()
for (interventionType in interventionTypes) {
  dataL <- list()
  mags  <- interventionMags[[interventionType]]
  for (mag in mags) {
    magnitude <- as.character(mag)
    dataName  <- paste(c(interventionType,magnitude),collapse="")
    fileName  <- paste(c('data/',dataName,'.csv'),collapse="")
    intData   <- read.csv(fileName)
    dataL[[magnitude]] = generateIncidence(intData)
  }
  incDataTypeGrouped[[interventionType]] = dataL
}

incPlotTypeGroupedG <- function(type, incDataTypeGrouped) {
  data <- incDataTypeGrouped[[type]]
  #Line Type Specifications
  ltvalues <- c()
  ltvalues[[noIntLs]] <- 1
  #ltbreaks = c(noIntLs)
  for (i in (2:(length(data)+1))) {
    label <- intLabels(type,names(data)[i-1],F)
    ltvalues[[label]] <- intLinetypes(type,names(data)[i-1])
  }
  plot <- ggplot() + 
    scale_y_log10(breaks=c(1,2,5,10,25,50,100,200),
                 labels=c(1,2,5,10,25,50,100,200),
                 limits=c(0.5,250)) + 
    labs(x="Years", y="Incidence/million", color="Population", 
          linetype="Intervention Status") + 
    plotTitle("Incidence/Million vs. Intervention Magnitude","") +
    scale_linetype_manual(values=ltvalues) +  
    scale_color_manual(values=c(USB=USBC,FB=FBC,All=totalPopC)) + 
    geom_line(data = data.frame(year=years, baseInc), aes(x=year, y=IN0,   
              color=USB, linetype=noIntL), size=2)  + 
    geom_line(data = data.frame(year=years, baseInc), aes(x=year, y=IN1,   
              color=FB,  linetype=noIntL), size=2)  + 
    geom_line(data = data.frame(year=years, baseInc), aes(x=year, y=INall, 
              color=all, linetype=noIntL), size=2)  +
    theme_gray(base_size=20) + 
    theme(legend.position=c(0.18,0.22), axis.title=element_text(size=34), 
          axis.text=element_text(size=25), plot.title=element_text(size=34),
          legend.key.height=unit(1.8,'line')) + 
    geom_hline(data=data.frame(year=years), aes(x=year,y=1), size=2)
  for (magnitude in names(data)) {
    incData   <- data.frame(year=years,data[[magnitude]],
                            label=intLabels(type,magnitude))
    plot <- plot + 
    geom_line(data=incData, aes(x=year, y=IN0,   color=USB, linetype=label),
      size=2)+ 
    geom_line(data=incData, aes(x=year, y=IN1,   color=FB,  linetype=label),
      size=2)+ 
    geom_line(data=incData, aes(x=year, y=INall, color=all, linetype=label),
      size=2)
  }
  return(plot) 
}
J <- incPlotTypeGroupedG('redEnLTBI',incDataTypeGrouped)
ggsave('forPoster/redEnLTBIIncGrouped.pdf',J,width=10,height=10)
J <- incPlotTypeGroupedG('incLTBItrmt',incDataTypeGrouped) + 
     theme(legend.position=c(0.78,0.22))
ggsave('forPoster/incLTBItrmtIncGrouped.pdf',J,width=10,height=10)
#
#incPlot <- ggplot(incData, aes(x=year)) + 
#           scale_y_log10(breaks=c(1,2,5,10,25,50,100,200),
#                         labels=c("Elimination (1)",2,5,10,25,50,100,200),
#                         limits=c(0.5,250)) + 
#           labs(x="Years", y="Incidence/million", color="Population", 
#                  linetype="Intervention Status") + 
#           plotTitle("Incidence Levels for Various Reductions of Entering LTBI","") +
#           geom_line(aes(y=baseUSB, color=USB, linetype=noInt))  + 
#           geom_line(aes(y=baseFB,  color=FB,  linetype=noInt))  + 
#           geom_line(aes(y=baseAll, color=all, linetype=noInt))  + 
#           geom_line(aes(y=redEnLTBI100USB, color=USB, linetype=redEnLTBI100L))  + 
#           geom_line(aes(y=redEnLTBI100FB, color=FB, linetype=redEnLTBI100L))  + 
#           geom_line(aes(y=redEnLTBI100All, color=all, linetype=redEnLTBI100L))  + 
#           geom_line(aes(y=redEnLTBI75USB, color=USB, linetype=redEnLTBI75L))  + 
#           geom_line(aes(y=redEnLTBI75FB, color=FB, linetype=redEnLTBI75L))  + 
#           geom_line(aes(y=redEnLTBI75All, color=all, linetype=redEnLTBI75L))  + 
#           geom_line(aes(y=redEnLTBI50USB, color=USB, linetype=redEnLTBI50L))  + 
#           geom_line(aes(y=redEnLTBI50FB, color=FB, linetype=redEnLTBI50L))  + 
#           geom_line(aes(y=redEnLTBI50All, color=all, linetype=redEnLTBI50L))
#           
#baseData    <- read.csv('data/baseData.csv') 
#cLTBIFBc    <- (baseData$cL1 + baseData$cF1)/1e9
#cActFBdL1c  <- cLTBIFBc + (baseData$cI1dL1 + baseData$cJ1dL1)/1e9
#cActFBdF1c  <- cActFBdL1c + (baseData$cI1dF1 + baseData$cJ1dF1)/1e9
#cLTBIUSBc   <- cActFBdF1c + (baseData$cL0 + baseData$cF0)/1e9
#cActUSBdL0c <- cLTBIUSBc + (baseData$cI0dL0 + baseData$cJ0dL0)/1e9
#cActUSBdF0c <- cActUSBdL0c + (baseData$cI0dF0 + baseData$cJ0dF0)/1e9
#costDataSourced <- data.frame(year=years,cLTBIFBc,cActFBdL1c,cActFBdF1c,
#                              cLTBIUSBc,cActUSBdL0c,cActUSBdF0c)
#yrange          <- round(seq(0,14.5,by=0.5),1)
#costPlotSourced <- ggplot(baseCumulativeSourcedInc, aes(x=year)) + 
#  labs(x="Years", y="Billions of USD", fill="Cost Source") + 
#  scale_y_continuous(breaks=yrange) + 
#  plotTitle("Sourced US TB HCS Cost","") +
#  geom_ribbon(aes(ymin=0,           ymax=cLTBIFBc,    fill=latentFBL))       + 
#  geom_ribbon(aes(ymin=cLTBIFBc,    ymax=cActFBdL1c,  fill=activeTBFBdL1L))  + 
#  geom_ribbon(aes(ymin=cActFBdL1c,  ymax=cActFBdF1c,  fill=activeTBFBdF1L))  + 
#  geom_ribbon(aes(ymin=cActFBdF1c,  ymax=cLTBIUSBc,   fill=latentUSBL))      + 
#  geom_ribbon(aes(ymin=cLTBIUSBc,   ymax=cActUSBdL0c, fill=activeTBUSBdL0L)) + 
#  geom_ribbon(aes(ymin=cActUSBdL0c, ymax=cActUSBdF0c, fill=activeTBUSBdF0L)) + 
#  theme_gray(base_size=26) + 
#  theme(legend.position=c(0.22,0.85), axis.title=element_text(size=40), 
#        axis.text=element_text(size=27), plot.title=element_text(size=40),
#        legend.key.height=unit(1.8,'line')) + 
#  scale_fill_manual(breaks=c(activeTBUSBdF0Ls,activeTBUSBdL0Ls,latentUSBLs,
#                             activeTBFBdF1Ls,activeTBFBdL1Ls,latentFBLs),
#                    values=c(activeTBUSBdF0c,activeTBUSBdL0c,latentUSBc,
#                             activeTBFBdF1c,activeTBFBdL1c,latentFBc))
#
#ggsave('forPoster/costPlotSourced.pdf',costPlotSourced,width=15,height=12)
#
