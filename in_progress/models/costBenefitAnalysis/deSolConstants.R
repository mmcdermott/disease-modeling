rkm <- function(Ydot,currY,deltaT,iter,startT=0){
  # This function performs a fourth-order Runge-Kutta Method on the system 
  # dY/dt = Ydot, and returns the next step after currY presuming timestep
  # length deltaT, and iteration iter
  tn   = deltaT*iter + startT
  k1   = Ydot(tn, currY)
  k2   = Ydot(tn + deltaT/2,currY + deltaT*k1/2)
  k3   = Ydot(tn + deltaT/2,currY + deltaT*k2/2)
  k4   = Ydot(tn + deltaT,currY + deltaT*k3)
  newY <- currY + (deltaT/6)*(k1+2*k2+2*k3+k4)
  return(newY)
}

#Initial Values
deltaT    <- 0.8    #The length of each time step (years).
initialYr <- 2000
finalYr   <- 2100
cutoffYr  <- 2008
totT      <- floor((finalYr-initialYr)/deltaT)  #Time steps
cutoffT   <- floor((cutoffYr-initialYr)/deltaT) + 1
years     <- seq(initialYr+deltaT,finalYr,deltaT)
yearsPC   <- seq(cutoffYr,finalYr - deltaT,deltaT) #years post cutoff
