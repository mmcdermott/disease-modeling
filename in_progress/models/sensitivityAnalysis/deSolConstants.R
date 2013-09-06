#Initial Values
deltaT    <- 0.8    #The length of each time step (years).
initialYr <- 2000
finalYr   <- 2100
cutoffYr  <- 2000
totT      <- floor((finalYr-initialYr)/deltaT)  #Time steps
cutoffT   <- floor((cutoffYr-initialYr)/deltaT) + 1
years     <- seq(initialYr+deltaT,finalYr,deltaT)
yearsPC   <- seq(cutoffYr,finalYr - deltaT,deltaT) #years post cutoff
