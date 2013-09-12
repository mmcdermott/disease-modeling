library(deSolve) # for lsoda routine
library(sensitivity)  # for PRCC calculations

#Hill Constants
source('hillConstantsLHS.R')
#load('randLHS.RData')

#DE Solving Parameters + runge-kutta solver: 
source('deSolConstants.R')

#Initializations:
# Compartments:
S0<-S1<-F0<-F1<-L0<-L1<-I0<-I1<-J0<-J1<-N0<-N1<-rep(0,totT)
# Compartment Costs:
#cL0<-cF0<-cI0<-cJ0<-cL1<-cF1<-cI1<-cJ1<-cN0<-cN1<-rep(0,totT)
# Compartment Cost Sources:
#cI0dL0<-cI0dF0<-cJ0dL0<-cJ0dF0<-cI1dL1<-cI1dF1<-cJ1dL1<-cJ1dF1<-rep(0,totT)
# Tracking Data:
#LTBIEn       <- rep(0,totT)                #LTBI arrivals
#LTBIEnD      <- rep(0,totT)                #Discounted LTBI arrivals
#curedLTBIEn  <- rep(0,totT)                #Cured LTBI arrivals
#curedLTBIEnD <- rep(0,totT)                #Discounted Cured LTBI arrivals
#natdeath0    <- natdeath1   <- rep(0,totT) #Natural Deaths
#tbdeath0     <- tbdeath1    <- rep(0,totT) #TB Deaths
#tbdeathD0    <- tbdeathD1   <- rep(0,totT) #Discounted TB Deaths
#progAcute0   <- progAcute1  <- rep(0,totT) #Acute LTBI Progressions
#progChron0   <- progChron1  <- rep(0,totT) #Chronic LTBI Progressions
#progTotalD0  <- progTotalD1 <- rep(0,totT) #Discounted Progressions
#exogenous0   <- exogenous1  <- rep(0,totT) #Exogenous re-Infections
#interventionCost <- rep(0,totT)            #Cumulative intervention cost
cLatent <- rep(0,totT)
cActive <- rep(0,totT)
cTotal <- rep(0,totT)

#Intervention per time step cost array 
# cost of new cases, total population, and LTBI cases entering
P <- data.frame(S0,F0,L0,I0,J0,S1,F1,L1,I1,J1,N0,N1,cLatent,cActive,cTotal)

#Treatment Effectiveness Data:
probHosp      <- .49 #Probability of hospitalization for active TB treatment
efficacyLTBI  <- .9  #LTBI treatement efficacy
adherenceLTBI <- .64 #LTBI treatment adherence
probLTBItreatsuccess <- efficacyLTBI*adherenceLTBI 

#Rate of time and health state discounting
discRt   <- 0.03  

#Compartment value dataset
P$cLatent[1] <- 0
P$cActive[1] <- 0
P$cTotal[1] <- 0

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
    #dnatdeath0   <- mu0 * N0                                         #Natural deaths (USB)
    #dnatdeath1   <- mu1 * N1                                         #Natural deaths (FB)
    #dtbdeath0    <- mud * (I0 + J0)                                  #TB deaths (USB)
    #dtbdeath1    <- mud * (I1 + J1)                                  #TB deaths (FB)
    #dtbdeathD0   <- discV * dtbdeath0                                #TB deaths with discounting (USB)
    #dtbdeathD1   <- discV * dtbdeath1                                #TB deaths with discounting (FB)
    dprogAcute0  <- vF*F0                                            #Acute LTBI progressions to Active TB disease (USB)
    dprogAcute1  <- vF*F1                                            #Acute LTBI progressions to Active TB disease (FB)
    dprogChron0  <- vL0*L0                                           #Chronic LTBI progressions to Active TB disease (USB)
    dprogChron1  <- vL1*L1                                           #Chronic LTBI progressions to Active TB disease (FB)
    #dprogTotal0  <- dprogAcute0 + dprogChron0                       #Progression to Active TB (USB)
    #dprogTotal1  <- dprogAcute1 + dprogChron1                       #Progression to Active TB (FB)
    #dprogTotalD0 <- discV * dprogTotal0                             #Progression to Active TB with discounting (USB)
    #dprogTotalD1 <- discV * dprogTotal1                             #Progression to Active TB with discounting (FB)
    lambda0      <- beta*(c00*(I0/N0) + c01*(I1/N1))                 #Forces of Infection (USB)
    lambda1      <- beta*(c10*(I0/N0) + c11*(I1/N1))                 #Forces of Infection (FB)
    dexogenous0	 <- x*p*lambda0*L0                                   #Exogenous re-infections of Chronic LTBI to Acute LTBI (USB)
    dexogenous1  <- x*p*lambda1*L1                                   #Exogenous re-infections of Chronic LTBI to Acute LTBI (FB)
    #dInterventionCost <- discV * (iCnewCases*1e6*(dprogTotal0+dprogTotal1) + iCtotPop*1e6*(N0+N1) + iCLTBIEn*1e6*dLTBIEn*(1-incLTBI))
    
    #Difference Equations (USB)
    dS0 <- ro*(N0+N1) + sigmaF0*F0 + sigmaL*L0 + phi0*(I0+J0) - lambda0*S0 - mu0*S0
    dF0 <- p*lambda0*S0 + dexogenous0 - (mu0 + vF + sigmaF0)*F0
    dL0 <- (1-p)*lambda0*S0 - dexogenous0 - (mu0 + vL0 + sigmaL)*L0
    dI0 <- q*(dprogAcute0 + dprogChron0) - (mu0 + mud + phi0)*I0
    dJ0 <- (1-q)*(dprogAcute0 + dprogChron0) - (mu0 + mud + phi0)*J0
    
    #Difference Equations (FB)
    dS1 <- (1-f)*alpha*(N0+N1) + sigmaF1*F1 + sigmaL*L1 + phi1*(I1 + J1) - lambda1*S1 - mu1*S1
    dF1 <- g*p*dLTBIEn + p*lambda1*S1 + dexogenous1 - (mu1 + vF + sigmaF1)*F1
    dL1 <- (1-g*p)*dLTBIEn + (1-p)*lambda1*S1 - dexogenous1 - (mu1 + vL1 +sigmaL)*L1
    dI1 <- q*(dprogAcute1 + dprogChron1) - (mu1 + mud + phi1)*I1
    dJ1 <- (1-q)*(dprogAcute1 + dprogChron1) - (mu1 + mud + phi1)*J1
    
    dN0 <- dS0 + dF0 + dL0 + dI0 + dJ0
    dN1 <- dS1 + dF1 + dL1 + dI1 + dJ1
    
    #Cost calculations
    dcL0 <- discV * Cl * sigmaL  * 1e6 * L0                    #cost for Chronic LTBI cures      (USB)
    dcF0 <- discV * Cl * sigmaF0 * 1e6 * F0                    #cost for Acute LTBI cures        (USB)
    dcI0 <- discV * Ct * q*(dprogAcute0 + dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    dcJ0 <- discV * Ct * (1-q)*(dprogAcute0 + dprogChron0) * 1e6 #cost for Non-Infectious TB cures (USB)
    #dcI0dL0 <- discV * Ct * q*(dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    #dcI0dF0 <- discV * Ct * q*(dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    #dcJ0dL0 <- discV * Ct * (1-q)*(dprogAcute0) * 1e6 #cost for Non-Infectious TB cures (USB)
    #dcJ0dF0 <- discV * Ct * (1-q)*(dprogAcute0) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcL1 <- discV * Cl * sigmaL  * 1e6 * L1                    #cost for Chronic LTBI cures      (FB)
    dcF1 <- discV * Cl * sigmaF1 * 1e6 * F1                    #cost for Acute LTBI cures        (FB)
    dcI1 <- discV * Ct * q*(dprogAcute1 + dprogChron1) * 1e6     #cost for Infectious TB cures     (FB)
    dcJ1 <- discV * Ct * (1-q)*(dprogAcute1 + dprogChron1) * 1e6 #cost for Non-Infectious TB cures (FB)
    #dcI1dL1 <- discV * Ct * q*(dprogChron1) * 1e6     #cost for Infectious TB cures     (USB)
    #dcI1dF1 <- discV * Ct * q*(dprogChron1) * 1e6     #cost for Infectious TB cures     (USB)
    #dcJ1dL1 <- discV * Ct * (1-q)*(dprogAcute1) * 1e6 #cost for Non-Infectious TB cures (USB)
    #dcJ1dF1 <- discV * Ct * (1-q)*(dprogAcute1) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcN0 <- dcF0 + dcL0 + dcI0 + dcJ0                            #Total cost for all treatments (USB)
    dcN1 <- dcF1 + dcL1 + dcI1 + dcJ1                            #Total cost for all treatments (FB)
    dcLatent <- dcL0 + dcF0 + dcL1 + dcF1
    dcActive <- dcI0 + dcJ0 + dcI1 + dcJ1
    dcTotal <- dcLatent + dcActive
 
   return(list(c(dS0,dF0,dL0,dI0,dJ0,dS1,dF1,dL1,dI1,dJ1,dN0,dN1,dcLatent,dcActive,dcTotal)))
  })
}

#Index = i
hill <- function(i,transmission=1,incLTBI=1,initial=cutoffT,final=totT){
  # Imputed constants
  mu0   = 1/78      #Natural mortality rate USB per year
  mu1   = 1/53      #Natural mortality rate FB per year
  ro    = 0.018     #USB birth rate per year
  alpha = 0.005     #FB arrival rate per year
  vF    = 1.5       #Progression rate of acute infection per year
  l0    = 0.015     #Prevalence of LTBI in the USB population in 2000
  l1    = 0.211     #Prevalence of LTBI in the FB population in 2000
  #2010 New Cases in Population i (millions)
  #source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
  newCases0 = .008714  #US-born
  newCases1 = .007554  #Foreign-born

  # set values in parameters
  parameters <- c(randLHS[i,],phi0=0,phi1=0,sigmaF0=0,sigmaF1=0,beta=0,
                  mu0=mu0,mu1=mu1,ro=ro,alpha=alpha,vF=vF,transmission=1,recursive=TRUE)
  parameters <- with(as.list(randLHS[i,]), {
    parameters['phi0'] <- phi*(mu0 + mud)/(1-phi)
    parameters['phi1'] <- phi*(mu1 + mud)/(1-phi)
    parameters['sigmaF0'] <- sigmaF*(mu0 + vF)/(1-sigmaF)
    parameters['sigmaF1'] <- sigmaF*(mu1 + vF)/(1-sigmaF)
    return(parameters)
  })
  
  # set initial values
  P <- with(as.list(parameters), {	
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
  
  # Set beta
<<<<<<< HEAD
  parameters <- with(as.list(parameters), {
    c01  <- (1-e0)*((1-e1)*P$N1[1])/((1-e0)*P$N0[1] + (1-e1)*P$N1[1])       #proportion of contacts made with FB individuals  (USB)
    c00  <- 1 - c01                                                         #proportion of contacts made with USB individuals (USB)
	parameters['beta'] <- ARI0/(c00*P$I0[1]/P$N0[1] + c01*P$I1[1]/P$N1[1])
	return(parameters)
  })
  # This next line is a temporary fix to get sane values... the real problem is
  # probably in the formula for beta above
  # print(parameters)
  # parameters['beta'] <- 10.39
  
  # recursive=TRUE collapses dataframe to labeled vector
  initv <- c(P[initial,], recursive=TRUE)
  # times = data points to be calculuated
  #times <- (initial:final)*deltaT
  times <- c(initial,final)*deltaT
  
  # compute master results
  mres <- lsoda(initv, times, Ddt, parameters)
  # mres[,-1] = mres without 1st column
  #return(mres[final,-1])
  return(mres[2,-1])
}

generateIncidence <- function(dataSet) {
  with(as.list(parms), {
    #IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
    #IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
    INall <- 1e6 * (vF*(dataSet['F0'] + dataSet['F1']) + vL0*dataSet['L0'] + vL1*dataSet['L1'])/(dataSet['N0'] + dataSet['N1'])
    return(INall)
    #return(data.frame(IN0,IN1,INall))
  })
}

# Basic PRCC
incResult <- rep(0,n)
costLatentResult <- rep(0,n)
costActiveResult <- rep(0,n)
costTotalResult <- rep(0,n)
for(i in 1:n) {
    temp <- hill(i)
    incResult[i] <- generateIncidence(temp)
    costLatentResult[i] <- temp['cLatent']
    costActiveResult[i] <- temp['cActive']
    costTotalResult[i]  <- temp['cTotal']
}

incPRCC         <- pcc(randLHS, incResult,        rank=TRUE)
costLatentPRCC  <- pcc(randLHS, costLatentResult, rank=TRUE)
costActivePRCC  <- pcc(randLHS, costActiveResult, rank=TRUE)
costTotalPRCC   <- pcc(randLHS, costTotalResult,  rank=TRUE)
print(incPRCC)
print(costLatentPRCC)
print(costActivePRCC)
print(costTotalPRCC)
