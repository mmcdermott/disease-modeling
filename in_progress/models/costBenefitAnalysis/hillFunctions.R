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
P <- data.frame(S0,F0,L0,I0,J0,S1,F1,L1,I1,J1,N0,N1,cL0,cF0,cI0,cJ0,cL1,cF1,cI1,
                cJ1,cN0,cN1,LTBIEn,LTBIEnD,curedLTBIEn,curedLTBIEnD,natdeath0,
                natdeath1,tbdeath0,tbdeath1,tbdeathD0,tbdeathD1,progAcute0,
                progChron0,progAcute1,progChron1,progTotalD0,progTotalD1, 
                exogenous0,exogenous1,interventionCost)

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

#Treatment Effectiveness Data:
probHosp      <- .49 #Probability of hospitalization for active TB treatment
efficacyLTBI  <- .9  #LTBI treatement efficacy
adherenceLTBI <- .64 #LTBI treatment adherence
probLTBItreatsuccess <- efficacyLTBI*adherenceLTBI 

#Cost Parameter Values: 
costtb   <- 2985   #TB treatment cost w/o hopsitalization <- Dylan supp. p.11-12
costhosp <- 25495  #TB treatment cost w/ hospitalization  <- Dylan supp. p.10
costLTBI <- 403.45 #LTBI treatment cost
Ct       <- costtb*(1-probHosp) + costhosp*probHosp #Cost of active TB treatment
Cl       <- costLTBI/probLTBItreatsuccess           #Cost of LTBI treatment

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

hill <- function(intervenCost,sigmaL,f,transmission=1,incLTBI=1,initial=cutoffT,final=totT,dataSet=P){
  #Differential Equation Functions
  Ddt <- function(t,v) {
    discV   <- 1/(1.03^t)  #amount costs, health states are discounted each time step
    #parameter values initialized for each time step
    c01     <- (1-e0)*((1-e1)*v$N1)/((1-e0)*v$N0 + (1-e1)*v$N1)  #proportion of contacts made with FB individuals  (USB)
    c00     <- 1 - c01                                           #proportion of contacts made with USB individuals (USB)
    c10     <- (1-e1)*((1-e0)*v$N0)/((1-e0)*v$N0 + (1-e1)*v$N1)  #proportion of contacts made with USB individuals (FB)
    c11     <- 1 - c10                                           #proportion of contacts made with FB individuals  (FB)
    dLTBIEn      <- f*alpha*(v$N0+v$N1)       #FB arrivals with LTBI entering
    dnatdeath0   <- mu0 * v$N0                #Natural deaths (USB)
    dnatdeath1   <- mu1 * v$N1                #Natural deaths (FB)
    dtbdeath0    <- mud * (v$I0 + v$J0)       #TB deaths (USB)
    dtbdeath1    <- mud * (v$I1 + v$J1)       #TB deaths (FB)
    dtbdeathD0   <- discV * dtbdeath0         #TB deaths with discounting (USB)
    dtbdeathD1   <- discV * dtbdeath1         #TB deaths with discounting (FB)
    dprogAcute0  <- vF*v$F0                   #Acute LTBI progressions to Active TB disease (USB)
    dprogAcute1  <- vF*v$F1                   #Acute LTBI progressions to Active TB disease (FB)
    dprogChron0  <- vL0*v$L0                  #Chronic LTBI progressions to Active TB disease (USB)
    dprogChron1  <- vL1*v$L1                  #Chronic LTBI progressions to Active TB disease (FB)
    dprogTotal0  <- dprogAcute0 + dprogChron0 #Progression to Active TB (USB)
    dprogTotal1  <- dprogAcute1 + dprogChron1 #Progression to Active TB (FB)
    dprogTotalD0 <- discV * dprogTotal0       #Progression to Active TB with discounting (USB)
    dprogTotalD1 <- discV * dprogTotal1       #Progression to Active TB with discounting (FB)
    lambda0      <- transmission*(beta*(c00*(v$I0/v$N0) + c01*(v$I1/v$N1)))  #Forces of Infection (USB)
    lambda1      <- transmission*(beta*(c10*(v$I0/v$N0) + c11*(v$I1/v$N1)))  #Forces of Infection (FB)
    dexogenous0	 <- x*p*lambda0*v$L0    #Exogenous re-infections of Chronic LTBI to Acute LTBI (USB)
    dexogenous1  <- x*p*lambda1*v$L1    #Exogenous re-infections of Chronic LTBI to Acute LTBI (FB)
    dInterventionCost <- discV * (intervenCost["newCases"]*1e6*(dprogTotal0+dprogTotal1) + intervenCost["totPop"]*1e6*(v$N0+v$N1) + intervenCost["LTBIEn"]*1e6*dLTBIEn*(1-incLTBI))
    
    #Difference Equations (USB)
    dS0     <- ro*(v$N0+v$N1) + sigmaF0*v$F0 + sigmaL*v$L0 + phi0*(v$I0+v$J0) - lambda0*v$S0 - mu0*v$S0
    dF0     <- p*lambda0*v$S0 + dexogenous0 - (mu0 + vF + sigmaF0)*v$F0
    dL0     <- (1-p)*lambda0*v$S0 - dexogenous0 - (mu0 + vL0 + sigmaL)*v$L0
    dI0     <- q*(dprogAcute0 + dprogChron0) - (mu0 + mud + phi0)*v$I0
    dJ0     <- (1-q)*(dprogAcute0 + dprogChron0) - (mu0 + mud + phi0)*v$J0
    
    #Difference Equations (FB)
    dS1     <- (1-incLTBI)*dLTBIEn+(1-f)*alpha*(v$N0+v$N1) + sigmaF1*v$F1 + sigmaL*v$L1 + phi1*(v$I1 + v$J1) - lambda1*v$S1 - mu1*v$S1
    dF1     <- g*p*dLTBIEn*incLTBI + p*lambda1*v$S1 + dexogenous1 - (mu1 + vF + sigmaF1)*v$F1
    dL1     <- (1-g*p)*dLTBIEn*incLTBI + (1-p)*lambda1*v$S1 - dexogenous1 - (mu1 + vL1 +sigmaL)*v$L1
    dI1     <- q*(dprogAcute1 + dprogChron1) - (mu1 + mud + phi1)*v$I1
    dJ1     <- (1-q)*(dprogAcute1 + dprogChron1) - (mu1 + mud + phi1)*v$J1
    
    dN0     <- dS0 + dF0 + dL0 + dI0 + dJ0
    dN1     <- dS1 + dF1 + dL1 + dI1 + dJ1
    
    #Cost calculations
    dcL0    <- discV * Cl * sigmaL  * 1e6 * v$L0                    #cost for Chronic LTBI cures      (USB)
    dcF0    <- discV * Cl * sigmaF0 * 1e6 * v$F0                    #cost for Acute LTBI cures        (USB)
    dcI0    <- discV * Ct * q*(dprogAcute0 + dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    dcJ0    <- discV * Ct * (1-q)*(dprogAcute0 + dprogChron0) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcL1    <- discV * Cl * sigmaL  * 1e6 * v$L1                    #cost for Chronic LTBI cures      (FB)
    dcF1    <- discV * Cl * sigmaF1 * 1e6 * v$F1                    #cost for Acute LTBI cures        (FB)
    dcI1    <- discV * Ct * q*(dprogAcute1 + dprogChron1) * 1e6     #cost for Infectious TB cures     (FB)
    dcJ1    <- discV * Ct * (1-q)*(dprogAcute1 + dprogChron1) * 1e6 #cost for Non-Infectious TB cures (FB)
    dcN0    <- dcF0 + dcL0 + dcI0 + dcJ0                            #Total cost for all treatments (USB)
    dcN1    <- dcF1 + dcL1 + dcI1 + dcJ1                            #Total cost for all treatments (FB)
    
    return(c(dS0,dF0,dL0,dI0,dJ0,dS1,dF1,dL1,dI1,dJ1,dN0,dN1,dcL0,dcF0,dcI0,
             dcJ0,dcL1,dcF1,dcI1,dcJ1,dcN0,dcN1,(dLTBIEn*incLTBI),
             (dLTBIEn*incLTBI*discV),dLTBIEn*(1-incLTBI),
             (dLTBIEn*(1-incLTBI)*discV),dnatdeath0,dnatdeath1,dtbdeath0,
             dtbdeath1,dtbdeathD0, dtbdeathD1, dprogAcute0,dprogChron0,
             dprogAcute1,dprogChron1,dprogTotalD0, dprogTotalD1, dexogenous0,
             dexogenous1, dInterventionCost) )
  }
  
  #TODO: Convert to lsoda
  for (i in initial:(final-1)) {
  	dataSet[i+1,] <- rkm(Ddt,dataSet[i,],deltaT,i)
  }
  return(dataSet)
}

generateIncidence <- function(dataSet) {
    IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
    IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
    INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
	return(data.frame(IN0,IN1,INall))
}
