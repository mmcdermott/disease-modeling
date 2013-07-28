library(MASS)
vF <- 1.5         #Progression rate of acute infection per year
vL0 <- 0.0014     #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1 <- 0.0010     #Progression rate for reactivation (chronic LTBI) in the FB population per year
popConst <- 1000

getIncidence <- function(slice) {
  IN0   <- 1e6 * (vF*slice$F0 + vL0*slice$L0)/slice$N0
  IN1   <- 1e6 * (vF*slice$F1 + vL1*slice$L1)/slice$N1
  INall <- 1e6 * (vF*(slice$F0 + slice$F1) + vL0*slice$L0 + vL1*slice$L1)/(slice$N0 + slice$N1)
  return(c(IN0,IN1,INall))
}

args <- commandArgs(trailingOnly = T)
if (length(args) > 0) {
  firstDataSet <- read.csv('modelDataRun1.csv')
  deltaT  <- as.numeric(args[1])
  print(deltaT)
  if (length(args) > 2) {
    initialYr <- as.integer(args[2])
    finalYr   <- as.integer(args[3])
  } else {
    # Defaults:
    initialYr <- 2000
    finalYr   <- 2100
  }
} else if (!exists('noImport')) {
  firstDataSet   <- read.csv('modelDataRun1.csv')
  deltaT    <- .05
  initialYr <- 2000
  finalYr   <- 2100
} else if (!exists('dataSet')) {
  print('No Data to Plot! Either list the import file as a cmd argument, or define it in the calling script')
  stop()
} 
if (Sys.info()['sysname'] == "Linux") {
  #Making it plot on linux
  X11.options(type='nbcairo')
}

finalValues <- matrix(nrow=2103, ncol=16)

filename <- "modelDataRun"
runNumber <- 1
extension <- ".csv"
rowNum <- 1
fileNameFull <- "modelDataRun1.csv"
while (file.exists(fileNameFull)) {
	dataSet <- read.csv(fileNameFull)
	finalValues[rowNum,] <- c(as.numeric(dataSet[2000,]), getIncidence(dataSet[2000,]))
	rowNum <- rowNum + 1
		
	runNumber <- runNumber + 1
        if (runNumber == 734) {
		runNumber <- 757
	} else if (runNumber == 1047) {
		runNumber <- 1081
	}
	fileNameFull <- paste(filename, as.character(runNumber), extension,sep="")
    print(fileNameFull)
}

# Columns are in the order:
# N0, S0, L0, F0, I0, J0, N1, S1, L1, F1, I1, J1,
# cost, IN0, IN1, INall

# L0 histogram
# dev.new()
# USBChronicLatents <- finalValues[,3]
# hist(USBChronicLatents, prob=TRUE, breaks=50)
# fitLine <- fitdistr(USBChronicLatents, "Normal")
# L0mean <- fitLine$estimate['mean']
# L0sd <- fitLine$estimate['sd']
# curve(dnorm(x,L0mean, L0sd), add=TRUE,col='red')
# legend('topright', legend=c(
    # paste('sample mean: ', signif(mean(USBChronicLatents))),
    # paste('sample stdev: ', signif(sd(USBChronicLatents))),
    # paste('fitted mean: ', signif(L0mean)),
    # paste('fitted stdev: ', signif(L0sd))
# ))

# L1 histogram
# dev.new()
# FBChronicLatents <- finalValues[,9]
# hist(FBChronicLatents, prob=TRUE, breaks=50)
# fitLine <- fitdistr(FBChronicLatents, "Normal")
# L1mean <- fitLine$estimate['mean']
# L1sd <- fitLine$estimate['sd']
# curve(dnorm(x,L1mean, L1sd), add=TRUE,col='red')
# legend('topright', legend=c(
    # paste('sample mean: ', signif(mean(FBChronicLatents))),
    # paste('sample stdev: ', signif(sd(FBChronicLatents))),
    # paste('fitted mean: ', signif(L1mean)),
    # paste('fitted stdev: ', signif(L1sd))
# ))

# IN0 histogram
dev.new()
USBIncidence <- finalValues[,14]
hist(USBIncidence, prob=TRUE, breaks=50)
fitLine <- fitdistr(USBIncidence, "Normal")
IN0mean <- fitLine$estimate['mean']
IN0sd <- fitLine$estimate['sd']
curve(dnorm(x,IN0mean, IN0sd), add=TRUE,col='red')
legend('topright', legend=c(
    paste('sample mean: ', signif(mean(USBIncidence))),
    paste('sample stdev: ', signif(sd(USBIncidence))),
    paste('fitted mean: ', signif(IN0mean)),
    paste('fitted stdev: ', signif(IN0sd))
))

# IN1 histogram
dev.new()
FBIncidence <- finalValues[,15]
hist(FBIncidence, prob=TRUE, breaks=50)
fitLine <- fitdistr(FBIncidence, "Normal")
IN1mean <- fitLine$estimate['mean']
IN1sd <- fitLine$estimate['sd']
curve(dnorm(x,IN1mean, IN1sd), add=TRUE,col='red')
legend('topright', legend=c(
    paste('sample mean: ', signif(mean(FBIncidence))),
    paste('sample stdev: ', signif(sd(FBIncidence))),
    paste('fitted mean: ', signif(IN1mean)),
    paste('fitted stdev: ', signif(IN1sd))
))
