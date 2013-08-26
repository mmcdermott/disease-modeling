library(deSolve) # for lsoda routine

#Hill Constants
source('hillConstants.R')

#DE Solving Parameters + runge-kutta solver: 
source('deSolConstants.R')

#Initializations:
# Compartments:
S0<-S1<-F0<-F1<-L0<-L1<-I0<-I1<-J0<-J1<-N0<-N1<-rep(0,totT)
# Compartment Costs:
cL0<-cF0<-cI0<-cJ0<-cL1<-cF1<-cI1<-cJ1<-cN0<-cN1<-rep(0,totT)
# Compartment Cost Sources:
cI0dL0<-cI0dF0<-cJ0dL0<-cJ0dF0<-cI1dL1<-cI1dF1<-cJ1dL1<-cJ1dF1<-rep(0,totT)
# Tracking Data:
LTBIEn       <- rep(0,totT)                #LTBI arrivals
LTBIEnD      <- rep(0,totT)                #Discounted LTBI arrivals
curedLTBIEn  <- rep(0,totT)                #Cured LTBI arrivals
curedLTBIEnD <- rep(0,totT)                #Discounted Cured LTBI arrivals
natdeath0    <- natdeath1   <- rep(0,totT) #Natural Deaths
tbdeath0     <- tbdeath1    <- rep(0,totT) #TB Deaths
tbdeathD0    <- tbdeathD1   <- rep(0,totT) #Discounted TB Deaths
progAcute0   <- progAcute1  <- rep(0,totT) #Acute LTBI Progressions
progChron0   <- progChron1  <- rep(0,totT) #Chronic LTBI Progressions
progTotalD0  <- progTotalD1 <- rep(0,totT) #Discounted Progressions
exogenous0   <- exogenous1  <- rep(0,totT) #Exogenous re-Infections
interventionCost <- rep(0,totT)            #Cumulative intervention cost

#Intervention per time step cost array 
# cost of new cases, total population, and LTBI cases entering
P <- data.frame(S0,F0,L0,I0,J0,S1,F1,L1,I1,J1,N0,N1,cL0,cF0,cI0,cI0dL0,cI0dF0,
                cJ0,cJ0dL0,cJ0dF0,cL1,cF1,cI1,cI1dL1,cI1dF1,cJ1,cJ1dL1,cJ1dF1,
                cN0,cN1,LTBIEn,LTBIEnD,curedLTBIEn,curedLTBIEnD,natdeath0,
                natdeath1,tbdeath0,tbdeath1,tbdeathD0,tbdeathD1,progAcute0,
                progChron0,progAcute1,progChron1,progTotalD0,progTotalD1, 
                exogenous0,exogenous1,interventionCost)

# parms is now a vector which must be referenced
P <- with(as.list(parms), {
  #Total Population
  P$N0[1] <- 250
  P$N1[1] <- 31.4
  #Acute (Fast) LTBI, new cases
  P$F0[1] <- (1-r0)*(newCases0)/vF
  P$F1[1] <- (1-r1)*(newCases1)/vF
  #Chronic (Long) LTBI
  P$L0[1] <- r0*(newCases0)/vL0
  P$L1[1] <- r1*(newCases1)/vL1
  #Infectious TB
  P$I0[1] <- q*newCases0/(mu0 + mud + phi0)
  P$I1[1] <- q*newCases1/(mu1 + mud + phi1)
  #Non-Infectious TB
  P$J0[1] <- (1-q)*newCases0/(mu0 + mud + phi0)
  P$J1[1] <- (1-q)*newCases1/(mu1 + mud + phi1)
  #Susceptible
  P$S0[1] <- P$N0[1] - P$F0[1] - P$L0[1] - P$I0[1] - P$J0[1]
  P$S1[1] <- P$N1[1] - P$F1[1] - P$L1[1] - P$I1[1] - P$J1[1]
  return(P)
})

#Treatment Effectiveness Data:
probHosp      <- .49 #Probability of hospitalization for active TB treatment
efficacyLTBI  <- .9  #LTBI treatement efficacy
adherenceLTBI <- .64 #LTBI treatment adherence
probLTBItreatsuccess <- efficacyLTBI*adherenceLTBI 

#Rate of time and health state discounting
discRt   <- 0.03  

#Compartment value dataset
P$LTBIEn[1]      <- 0.263109
P$natdeath0[1]   <- 0
P$natdeath1[1]   <- 0
P$tbdeath0[1]    <- 0
P$tbdeath1[1]    <- 0
P$progAcute0[1]  <- 0
P$progChron0[1]  <- 0
P$progAcute1[1]  <- 0
P$progChron1[1]  <- 0
P$exogenous0[1]  <- 0
P$exogenous1[1]  <- 0

  #Differential Equation Functions
Ddt <- function(t,v,parms) {
  with(as.list(c(t,v,parms)), {
    discV   <- 1/(1.03^t)  #amount costs, health states discount constant

    #parameter values initialized for each time step
    c01          <- (1-e0)*((1-e1)*N1)/((1-e0)*N0 + (1-e1)*N1)       #proportion of contacts made with FB individuals  (USB)
    c00          <- 1 - c01                                          #proportion of contacts made with USB individuals (USB)
    c10          <- (1-e1)*((1-e0)*N0)/((1-e0)*N0 + (1-e1)*N1)       #proportion of contacts made with USB individuals (FB)
    c11          <- 1 - c10                                          #proportion of contacts made with FB individuals  (FB)
    dLTBIEn      <- f*alpha*(N0+N1)                                  #FB arrivals with LTBI entering
    dnatdeath0   <- mu0 * N0                                         #Natural deaths (USB)
    dnatdeath1   <- mu1 * N1                                         #Natural deaths (FB)
    dtbdeath0    <- mud * (I0 + J0)                                  #TB deaths (USB)
    dtbdeath1    <- mud * (I1 + J1)                                  #TB deaths (FB)
    dtbdeathD0   <- discV * dtbdeath0                                #TB deaths with discounting (USB)
    dtbdeathD1   <- discV * dtbdeath1                                #TB deaths with discounting (FB)
    dprogAcute0  <- vF*F0                                            #Acute LTBI progressions to Active TB disease (USB)
    dprogAcute1  <- vF*F1                                            #Acute LTBI progressions to Active TB disease (FB)
    dprogChron0  <- vL0*L0                                           #Chronic LTBI progressions to Active TB disease (USB)
    dprogChron1  <- vL1*L1                                           #Chronic LTBI progressions to Active TB disease (FB)
    dprogTotal0  <- dprogAcute0 + dprogChron0                        #Progression to Active TB (USB)
    dprogTotal1  <- dprogAcute1 + dprogChron1                        #Progression to Active TB (FB)
    dprogTotalD0 <- discV * dprogTotal0                              #Progression to Active TB with discounting (USB)
    dprogTotalD1 <- discV * dprogTotal1                              #Progression to Active TB with discounting (FB)
    lambda0      <- transmission*(beta*(c00*(I0/N0) + c01*(I1/N1)))  #Forces of Infection (USB)
    lambda1      <- transmission*(beta*(c10*(I0/N0) + c11*(I1/N1)))  #Forces of Infection (FB)
    dexogenous0	 <- x*p*lambda0*L0                                   #Exogenous re-infections of Chronic LTBI to Acute LTBI (USB)
    dexogenous1  <- x*p*lambda1*L1                                   #Exogenous re-infections of Chronic LTBI to Acute LTBI (FB)
    dInterventionCost <- discV * (iCnewCases*1e6*(dprogTotal0+dprogTotal1) + iCtotPop*1e6*(N0+N1) + iCLTBIEn*1e6*dLTBIEn*(1-incLTBI))
    
    #Difference Equations (USB)
    dS0 <- ro*(N0+N1) + sigmaF0*F0 + sigmaL*L0 + phi0*(I0+J0) - lambda0*S0 - mu0*S0
    dF0 <- p*lambda0*S0 + dexogenous0 - (mu0 + vF + sigmaF0)*F0
    dL0 <- (1-p)*lambda0*S0 - dexogenous0 - (mu0 + vL0 + sigmaL)*L0
    dI0 <- q*(dprogAcute0 + dprogChron0) - (mu0 + mud + phi0)*I0
    dJ0 <- (1-q)*(dprogAcute0 + dprogChron0) - (mu0 + mud + phi0)*J0
    
    #Difference Equations (FB)
    dS1 <- (1-incLTBI)*dLTBIEn+(1-f)*alpha*(N0+N1) + sigmaF1*F1 + sigmaL*L1 + phi1*(I1 + J1) - lambda1*S1 - mu1*S1
    dF1 <- g*p*dLTBIEn*incLTBI + p*lambda1*S1 + dexogenous1 - (mu1 + vF + sigmaF1)*F1
    dL1 <- (1-g*p)*dLTBIEn*incLTBI + (1-p)*lambda1*S1 - dexogenous1 - (mu1 + vL1 +sigmaL)*L1
    dI1 <- q*(dprogAcute1 + dprogChron1) - (mu1 + mud + phi1)*I1
    dJ1 <- (1-q)*(dprogAcute1 + dprogChron1) - (mu1 + mud + phi1)*J1
    
    dN0 <- dS0 + dF0 + dL0 + dI0 + dJ0
    dN1 <- dS1 + dF1 + dL1 + dI1 + dJ1
    
    #Cost calculations
    dcL0 <- discV * Cl * sigmaL  * 1e6 * L0                    #cost for Chronic LTBI cures      (USB)
    dcF0 <- discV * Cl * sigmaF0 * 1e6 * F0                    #cost for Acute LTBI cures        (USB)
    dcI0 <- discV * Ct * q*(dprogAcute0 + dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    dcJ0 <- discV * Ct * (1-q)*(dprogAcute0 + dprogChron0) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcI0dL0 <- discV * Ct * q*(dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    dcI0dF0 <- discV * Ct * q*(dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    dcJ0dL0 <- discV * Ct * (1-q)*(dprogAcute0) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcJ0dF0 <- discV * Ct * (1-q)*(dprogAcute0) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcL1 <- discV * Cl * sigmaL  * 1e6 * L1                    #cost for Chronic LTBI cures      (FB)
    dcF1 <- discV * Cl * sigmaF1 * 1e6 * F1                    #cost for Acute LTBI cures        (FB)
    dcI1 <- discV * Ct * q*(dprogAcute1 + dprogChron1) * 1e6     #cost for Infectious TB cures     (FB)
    dcJ1 <- discV * Ct * (1-q)*(dprogAcute1 + dprogChron1) * 1e6 #cost for Non-Infectious TB cures (FB)
    dcI1dL1 <- discV * Ct * q*(dprogChron1) * 1e6     #cost for Infectious TB cures     (USB)
    dcI1dF1 <- discV * Ct * q*(dprogChron1) * 1e6     #cost for Infectious TB cures     (USB)
    dcJ1dL1 <- discV * Ct * (1-q)*(dprogAcute1) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcJ1dF1 <- discV * Ct * (1-q)*(dprogAcute1) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcN0 <- dcF0 + dcL0 + dcI0 + dcJ0                            #Total cost for all treatments (USB)
    dcN1 <- dcF1 + dcL1 + dcI1 + dcJ1                            #Total cost for all treatments (FB)
 
    return(list(c(dS0,dF0,dL0,dI0,dJ0,dS1,dF1,dL1,dI1,dJ1,dN0,dN1,dcL0,dcF0,dcI0,
             dcI0dL0,dcI0dF0,dcJ0,dcJ0dL0,dcJ0dF0,dcL1,dcF1,dcI1,dcI1dL1,dcI1dF1,
             dcJ1,dcJ1dL1,dcJ1dF1,dcN0,dcN1,(dLTBIEn*incLTBI),
             (dLTBIEn*incLTBI*discV),dLTBIEn*(1-incLTBI),
             (dLTBIEn*(1-incLTBI)*discV),dnatdeath0,dnatdeath1,dtbdeath0,
             dtbdeath1,dtbdeathD0, dtbdeathD1, dprogAcute0,dprogChron0,
             dprogAcute1,dprogChron1,dprogTotalD0, dprogTotalD1, dexogenous0,
             dexogenous1, dInterventionCost)))
  })
}

hill <- function(intervenCost,sigmaL,f,transmission=1,incLTBI=1,initial=cutoffT,final=totT,dataSet=P){
  # set values in parameters
  newparms <- c(iCnewCases=as.vector(intervenCost['newCases']), 
                iCtotPop=as.vector(intervenCost['totPop']), 
                iCLTBIEn=as.vector(intervenCost['LTBIEn']),
                sigmaL=sigmaL, f=f, transmission=transmission, incLTBI=incLTBI)
                #Ct=activeTxC,Cl=LTBITxC)
  parameters <- c(parms, newparms)
  # recursive=TRUE collapses dataframe to labeled vector
  initv <- c(dataSet[initial,], recursive=TRUE)
  # times = data points to be calculuated
  times <- (initial:final)*deltaT
  
  # compute master results
  mres <- lsoda(initv, times, Ddt, parameters)
  # mres[,-1] = mres without 1st column
  dataSet[initial:final,] <- c(mres[,-1])
  return(dataSet)
}

generateIncidence <- function(dataSet) {
  with(as.list(parms), {
    IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
    IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
    INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
	  return(data.frame(IN0,IN1,INall))
  })
}

generateTotalIncidence <- function(dataSet,incLTBI=1,transmission=1,f=fBase) {
  with(as.list(parms), {
    #proportion of contacts made with FB individuals  (USB)
    c01          <- (1-e0)*((1-e1)*dataSet$N1)/((1-e0)*dataSet$N0 + 
                    (1-e1)*dataSet$N1)
    #proportion of contacts made with USB individuals (USB)
    c00          <- 1 - c01
    c10          <- (1-e1)*((1-e0)*dataSet$N0)/((1-e0)*dataSet$N0 + 
                    (1-e1)*dataSet$N1)
    #proportion of contacts made with USB individuals (FB)
    c11          <- 1 - c10
    #proportion of contacts made with FB individuals  (FB)
    #Forces of Infection (USB)
    lambda0      <- transmission*(beta*(c00*(dataSet$I0/dataSet$N0) + 
                                        c01*(dataSet$I1/dataSet$N1)))
    #Forces of Infection (FB)
    lambda1      <- transmission*(beta*(c10*(dataSet$I0/dataSet$N0) + 
                                        c11*(dataSet$I1/dataSet$N1)))


    IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
    IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
    INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + 
                    vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
    IL0   <- 1e6 * ((1-p) * lambda0 * dataSet$S0)
    IL1   <- 1e6 * ((1-p) * lambda1 * dataSet$S1 + (1-g*p) * f * alpha * incLTBI
                    * (dataSet$N0 + dataSet$N1))
    IF0   <- 1e6 * (p     * lambda0 * dataSet$S0 + x * p * lambda0 * L0)
    IF1   <- 1e6 * (p     * lambda1 * dataSet$S1 + g * p * f * alpha * incLTBI
                    * (dataSet$N0 + dataSet$N1) + x * p * lambda1 * L1)
    II0   <- 1e6 * q     * (vF*dataSet$F0 + vL0*dataSet$L0)
    II1   <- 1e6 * q     * (vF*dataSet$F1 + vL1*dataSet$L1)
    IJ0   <- 1e6 * (1-q) * (vF*dataSet$F0 + vL0*dataSet$L0)
    IJ1   <- 1e6 * (1-q) * (vF*dataSet$F1 + vL1*dataSet$L1)
    return(data.frame(IN0,IN1,INall,IL0,IL1,IF0,IF1,II0,II1,IJ0,IJ1))
  })
}
