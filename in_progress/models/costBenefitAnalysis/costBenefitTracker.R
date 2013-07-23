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

#Variables
mu0   <- 1/78      #Natural mortality rate USB per year
mu1   <- 1/53      #Natural mortality rate FB per year
ro    <- 0.018     #USB birth rate per year
alpha <- 0.005     #FB arrival rate per year
p     <- 0.103     #Fraction of new infections which are acute (fast progressors)
vF    <- 1.5       #Progression rate of acute infection per year
l0    <- 0.015     #Prevalence of LTBI in the USB population in 2000
l1    <- 0.211     #Prevalence of LTBI in the FB population in 2000: 
r0    <- 0.667     #Fraction of cases due to reactivation in the USB population
r1    <- 0.780     #Fraction of cases due to reactivation in the FB population
vL0   <- 0.0014    #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1   <- 0.0010    #Progression rate for reactivation (chronic LTBI) in the FB population per year
q     <- 0.708     #Fraction of infections progressing to infectious disease
mud   <- 0.115     #Mortality rate due to TB per year
x     <- 0.111     #Fraction of re-infected chronic LTBI moving to acute infection
ARI0  <- 0.030/100 #Annual risk of infection for USB in 2000
beta  <- 10.39     #Effective contact rate per year
e0    <- 0.965     #Fraction of preferred contacts with own population for USB
e1    <-0.985      #Fraction of preferred contacts with own population for FB
g     <- 0.0047    #Fraction of FB arrivals with LTBI who are fast progressors
phi0  <- 1.114     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (USB)
phi1  <- 1.167     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (FB)
sigmaF0 <- 1.296   #Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
sigmaF1 <- 1.301   #Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
sigmaLBase <- 0.057 #Treatment rate for chronic LTBI per year
fBase <- 0.187      #Fraction of FB arrivals with LTBI


#2010 New Cases in Population i (millions)
#source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
newCases0 <- .008714  #US-born
newCases1 <- .007554  #Foreign-born

#Initial Values
deltaT   <- .8    #The length of each time step (years).
finalYr  <- 100   #In years
totT     <- floor(finalYr/deltaT)  #Time steps
cutoffT <- floor(8/deltaT)

#Matrix of compartment values
S0 <- S1 <- F0 <- F1 <- L0 <- L1 <- I0 <- I1 <- J0 <- J1 <- N0 <- N1 <- rep(0,totT)  #compartment values
cL0 <- cF0 <- cI0 <- cJ0 <- cL1 <- cF1 <- cI1 <- cJ1 <- cN0 <- cN1 <- rep(0,totT)    #costs associated with each compartment
#Ct <- Cl <- rep(0,totT)  #total costs for active and latent treatment
LTBIEn <- rep(0,totT)    #FB arrivals with LTBI entering the USA
natdeath0 <- natdeath1 <- rep(0,totT)                               #natural deaths
tbdeath0 <- tbdeath1 <- tbdeathD0 <- tbdeathD1 <- rep(0,totT)       #TB deaths, TB deaths with discounting
progAcute0 <- progChron0 <- progAcute1 <- progChron1 <- rep(0,totT) #Progression from Acute, Chronic LTBI to Active TB
progTotalD0 <- progTotalD1 <- rep(0,totT)                           #Progression to Active TB with discounting
exogenous0 <- exogenous1 <- rep(0,totT)                             #Exogenous re-Infections
interventionCost <- rep(0,totT)                                     #Cumulative intervention cost

# ALSO TOOK OUT Ct,Cl FROM VECTOR BELOW
P <- data.frame(S0,F0,L0,I0,J0,S1,F1,L1,I1,J1,N0,N1,cL0,cF0,cI0,cJ0,cL1,cF1,cI1,cJ1,cN0,cN1,LTBIEn,natdeath0,natdeath1,tbdeath0,tbdeath1,tbdeathD0,tbdeathD1,progAcute0,progChron0,progAcute1,progChron1,progTotalD0, progTotalD1, exogenous0,exogenous1, interventionCost)
C <- c(newCases=0,totPop=0,LTBIEn=0) #Intervention cost array (cost of new cases, total population, and LTBI cases entering, each time step)
IN0 <- IN1 <- INall <- rep(0,totT)

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

#Cost Initial Values are all zero. 
probHosp <- .49  #Probability of hospitalization for active TB treatment
efficacyLTBI  <- .9
adherenceLTBI <- .64
probLTBItreatsuccess <- efficacyLTBI*adherenceLTBI #Probability of LTBI treatment success
costtb   <- 2985   #Cost of TB treatment without hopsitalization
costhosp <- 25495  #Cost of TB treatment with hospitalization
costLTBI <- 403.45 #Cost of LTBI treatment

#Cost Parameter Values: 
Ct <- costtb*(1-probHosp) + costhosp*probHosp #in USD (source: Dylan online supplement page 11-12)
Cl <- costLTBI/probLTBItreatsuccess           #in USD (source: Dylan online supplement page 10)
discRt  <- 0.03  #Rate of time discounting (used for health states as well)

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
    discV = 1/(1.03^t)  #amount costs, health states are discounted each time step
    #parameter values initialized for each time step
    c01     <- (1-e0)*((1-e1)*v$N1)/((1-e0)*v$N0 + (1-e1)*v$N1)  #proportion of contacts made with FB individuals  (USB)
    c00     <- 1 - c01                                           #proportion of contacts made with USB individuals (USB)
    c10     <- (1-e1)*((1-e0)*v$N0)/((1-e0)*v$N0 + (1-e1)*v$N1)  #proportion of contacts made with USB individuals (FB)
    c11     <- 1 - c10                                           #proportion of contacts made with FB individuals  (FB)
    dLTBIEn      <- f*alpha*(v$N0+v$N1)       #FB arrivals with LTBI entering
    #dLTBIEnD     <- discV * dLTBIEn           #FB arrivals with LTBI entering with discounting
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
    dInterventionCost <- discV * (intervenCost["newCases"]*(dprogTotal0+dprogTotal1) + intervenCost["totPop"]*(v$N0+v$N1) + intervenCost["LTBIEn"]*fBase*alpha*(v$N0+v$N1) )
    
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
    
    dN0     <- 0
    dN1     <- 0
    
    #Cost calculations
    dcL0    <- discV * Cl * sigmaL  * 1e6 * v$L0                    #cost for Chronic LTBI cures      (USB)
    dcF0    <- discV * Cl * sigmaF0 * 1e6 * v$F0                    #cost for Acute LTBI cures        (USB)
    dcI0    <- discV * Ct * q*(dprogAcute0 + dprogChron0) * 1e6     #cost for Infectious TB cures     (USB)
    dcJ0    <- discV * Ct * (1-q)*(dprogAcute0 + dprogChron0) * 1e6 #cost for Non-Infectious TB cures (USB)
    dcL1    <- discV * Cl * sigmaL  * 1e6 * v$L1                    #cost for Chronic LTBI cures      (FB)
    dcF1    <- discV * Cl * sigmaF1 * 1e6 * v$F1                    #cost for Acute LTBI cures        (FB)
    dcI1    <- discV * Ct * q*(dprogAcute1 + dprogChron1) * 1e6     #cost for Infectious TB cures     (FB)
    dcJ1    <- discV * Ct * (1-q)*(dprogAcute1 + dprogChron1) * 1e6 #cost for Non-Infectious TB cures (FB)
    dcN0    <- 0  #Total cost for all treatments (USB)
    dcN1    <- 0  #Total cost for all treatments (FB)
    
    return( c(dS0,dF0,dL0,dI0,dJ0,dS1,dF1,dL1,dI1,dJ1,dN0,dN1,dcL0,dcF0,dcI0,dcJ0,dcL1,dcF1,dcI1,dcJ1,dcN0,dcN1,dLTBIEn*incLTBI,dnatdeath0,dnatdeath1,dtbdeath0,dtbdeath1,dtbdeathD0, dtbdeathD1, dprogAcute0,dprogChron0,dprogAcute1,dprogChron1,dprogTotalD0, dprogTotalD1, dexogenous0,dexogenous1,dInterventionCost) )
  }
  
  for (i in initial:(final-1)) {
  	dataSet[i+1,]    <- rkm(Ddt,dataSet[i,],deltaT,i)
  	dataSet$N0[i+1]  <- sum(dataSet[i+1,1:5])
  	dataSet$N1[i+1]  <- sum(dataSet[i+1,6:10])
  	dataSet$cN0[i+1] <- sum(dataSet[i+1,13:16])
  	dataSet$cN1[i+1] <- sum(dataSet[i+1,17:20])
  }
  return(dataSet)
}

generateIncidence <- function(dataSet) {
    IN0   <- 1e6 * (vF*dataSet$F0 + vL0*dataSet$L0)/dataSet$N0
    IN1   <- 1e6 * (vF*dataSet$F1 + vL1*dataSet$L1)/dataSet$N1
    INall <- 1e6 * (vF*(dataSet$F0 + dataSet$F1) + vL0*dataSet$L0 + vL1*dataSet$L1)/(dataSet$N0 + dataSet$N1)
	return(data.frame(IN0,IN1,INall))
}

P       <- hill(C,sigmaLBase,fBase,1,1,1,totT)
baseInc <- generateIncidence(P)
years   <- seq(2000+deltaT,2000+finalYr,deltaT)

#plot incidence data
#  xlab, ylab  --> labels for x-, y-axes
#  log='y'     --> use logarithmic scale
#  ylim=yrange --> ensure we show all data
#  type="l"    --> draw line connecting data points
#  col="blue"  --> color of graph
#lines() plots data in the same window as the first plot() command

#Intervention costs for eliminating LTBI 100%, 75%, or 50%
C100 <- C
C75  <- C
C50  <- C

C100$LTBIEn <- 1000
C75$LTBIEn  <- 800
C50$LTBIEn  <- 700

#Plot A: Where we eliminate incoming LTBI 100%, 75%, or 50%
noImmLTBI      <- hill(C100,sigmaLBase,fBase,1,0)
noImmLTBIInc   <- generateIncidence(noImmLTBI)
someImmLTBI    <- hill(C75,sigmaLBase,fBase,1,0.25)
someImmLTBIInc <- generateIncidence(someImmLTBI)
halfImmLTBI    <- hill(C50,sigmaLBase,fBase,1,0.5)
halfImmLTBIInc <- generateIncidence(halfImmLTBI)

#When Calculating range, we presume that FB always has higher incidence rate
yrange <- range(c(0.5,baseInc$IN1,noImmLTBIInc$IN1,someImmLTBIInc$IN1,halfImmLTBIInc$IN1))
dev.new()
plot(years, baseInc$IN0, main="Plot A: Reduce incoming LTBI in 2008", log='y', xlab='year', ylab='incidence/million', ylim=yrange, type="l", col="blue")
lines(years, baseInc$INall, type="l", col="red")
lines(years, baseInc$IN1, type="l", col="green")
lines(years, noImmLTBIInc$IN0, type="l", col="blue", lty=2)
lines(years, noImmLTBIInc$INall, type="l", col="red", lty=2)
lines(years, noImmLTBIInc$IN1, type="l", col="green", lty=2)
lines(years, someImmLTBIInc$IN0, type="l", col="blue", lty=3)
lines(years, someImmLTBIInc$INall, type="l", col="red", lty=3)
lines(years, someImmLTBIInc$IN1, type="l", col="green", lty=3)
lines(years, halfImmLTBIInc$IN0, type="l", col="blue", lty=4)
lines(years, halfImmLTBIInc$INall, type="l", col="red", lty=4)
lines(years, halfImmLTBIInc$IN1, type="l", col="green", lty=4)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'No Incoming LTBI post 2008', '75% reduction of Inc. LTBI post 2008', '50% reduction of Inc. LTBI post 2008'), col=c("blue", "green", "red", "black","black","black"), lty=c(1,1,1,2,3,4))

# #Cost Plot A:
# yrange <- range(c(P$cN1+P$cN0,noImmLTBI$cN1+noImmLTBI$cN0,someImmLTBI$cN0+someImmLTBI$cN1,halfImmLTBI$cN0+halfImmLTBI$cN1))
# dev.new()
# plot(years, P$cN1+P$cN0, main="Plot A: cost of various interventions", xlab='year', ylab='USD', ylim=yrange, type="l", col="blue")
# #lines(years, P$cN0, type="l", col="blue", lty=2)
# #lines(years, P$cN1, type="l", col="blue", lty=3)
# lines(years, noImmLTBI$cN0 + noImmLTBI$cN1, type="l", col="green", lty=1)
# #lines(years, noImmLTBI$cN0, type="l", col="green", lty=2)
# #lines(years, noImmLTBI$cN1, type="l", col="green", lty=3)
# lines(years, someImmLTBI$cN0 + someImmLTBI$cN1, type="l", col="red", lty=1)
# #lines(years, someImmLTBI$cN0, type="l", col="red", lty=2)
# #lines(years, someImmLTBI$cN1, type="l", col="red", lty=3)
# lines(years, halfImmLTBI$cN0 + halfImmLTBI$cN1, type="l", col="purple", lty=1)
# #lines(years, halfImmLTBI$cN0, type="l", col="purple", lty=2)
# #lines(years, halfImmLTBI$cN1, type="l", col="purple", lty=3)
# abline(h = 1, lty = 'dotted')
# legend('topright', legend=c('Base Cost - No Interventions', 'Cost with elimination of Inc. LTBI', 'Cost with 75% reduction of Inc. LTBI', 'Cost with 50% reduction of Inc. LTBI'), col=c("blue", "green", "red", "purple"), lty=c(1,1,1,1))

# yearsPostCutoff <- years[(cutoffT+1):totT]
# maxDifference   <- ((P$cN0+P$cN1)-(noImmLTBI$cN0+noImmLTBI$cN1))[(cutoffT+1):totT]
# savingsPerCase  <- maxDifference/(1e6*P$LTBIEn[(cutoffT+1):totT])
# dev.new()
# plot(yearsPostCutoff, savingsPerCase, main="Savings Per Cured Case of Entering LTBI", xlab='year', ylab='USD', type="l", col="blue")



##Plot A: Where Transmission is cut after 2008
#noTrans    <- hill(sigmaLBase,fBase,0)
#noTransInc <- generateIncidence(noTrans)
#
##When Calculating range, we presume that FB always has higher incidence rate
#yrange <- range(c(0.5,baseInc$IN1,noTransInc$IN1))
#dev.new()
#plot(years, baseInc$IN0, main="Plot A: Cut Trans. in 2008", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, baseInc$INall, type="l", col="red")
#lines(years, baseInc$IN1, type="l", col="green")
#lines(years, noTransInc$IN0, type="l", col="blue", lty=2)
#lines(years, noTransInc$INall, type="l", col="red", lty=2)
#lines(years, noTransInc$IN1, type="l", col="green", lty=2)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'With Transmission Cutoff in 2008'), col=c("blue", "green", "red", "black"), lty=c(1,1,1,2))
#
##Plot B: Where LTBI treatment x2 or x4
#doubled    <-hill(sigmaLBase*2,fBase)
#doubledInc <- generateIncidence(doubled)
#quad       <- hill(sigmaLBase*4,fBase)
#quadInc    <- generateIncidence(quad)
#
##When Calculating range, we presume that FB always has higher incidence rate after 2008
#yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
#dev.new()
#plot(years, baseInc$IN0, main="Plot B: Increase Chronic LTBI Treatment in 2008", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, baseInc$INall, type="l", col="red")
#lines(years, baseInc$IN1, type="l", col="green")
#lines(years, doubledInc$IN0, type="l", col="blue", lty=2)
#lines(years, doubledInc$INall, type="l", col="red", lty=2)
#lines(years, doubledInc$IN1, type="l", col="green", lty=2)
#lines(years, quadInc$IN0, type="l", col="blue", lty=3)
#lines(years, quadInc$INall, type="l", col="red", lty=3)
#lines(years, quadInc$IN1, type="l", col="green", lty=3)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c("blue", "green", "red", "black", "black"), lty=c(1,1,1,2,3))
#
##Plot C: Where FB arrivals cut in half, also LTBI treatment x2 or x4 after 2008
#halfFB           <- hill(sigmaLBase,fBase/2)
#halfFBInc        <- generateIncidence(halfFB)
#halfFBdoubled    <- hill(sigmaLBase*2,fBase/2)
#halfFBdoubledInc <- generateIncidence(halfFBdoubled)
#halfFBquad       <- hill(sigmaLBase*4,fBase/2)
#halfFBquadInc    <- generateIncidence(halfFBquad)
#
##When Calculating range, we presume that FB always has higher incidence rate
#yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
#dev.new()
#plot(years, halfFBInc $IN0, main="Plot C: Half FB Arrival Rate, increase LTBI Treatment", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, halfFBInc $INall, type="l", col="red")
#lines(years, halfFBInc $IN1, type="l", col="green")
#lines(years, halfFBdoubledInc $IN0, type="l", col="blue", lty=2)
#lines(years, halfFBdoubledInc $INall, type="l", col="red", lty=2)
#lines(years, halfFBdoubledInc $IN1, type="l", col="green", lty=2)
#lines(years, halfFBquadInc $IN0, type="l", col="blue", lty=3)
#lines(years, halfFBquadInc $INall, type="l", col="red", lty=3)
#lines(years, halfFBquadInc $IN1, type="l", col="green", lty=3)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c("blue", "green", "red", "black", "black"), lty=c(1,1,1,2,3))
#
##Plot D: Where FB arrivals divided by 4, also LTBI treatment x2 or x4 after 2008
#quarterFB           <- hill(sigmaLBase,fBase/4)
#quarterFBInc        <- generateIncidence(quarterFB)
#quarterFBdoubled    <- hill(sigmaLBase*2,fBase/4)
#quarterFBdoubledInc <- generateIncidence(quarterFBdoubled)
#quarterFBquad       <- hill(sigmaLBase*4,fBase/4)
#quarterFBquadInc    <- generateIncidence(quarterFBquad)
#
##When Calculating range, we presume that FB always has higher incidence rate
#yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
#dev.new()
#plot(years, quarterFBInc $IN0, main="Plot D: 1/4 FB Arrival Rate, increase LTBI Treatment", xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type="l", col="blue")
#lines(years, quarterFBInc $INall, type="l", col="red")
#lines(years, quarterFBInc $IN1, type="l", col="green")
#lines(years, quarterFBdoubledInc $IN0, type="l", col="blue", lty=2)
#lines(years, quarterFBdoubledInc $INall, type="l", col="red", lty=2)
#lines(years, quarterFBdoubledInc $IN1, type="l", col="green", lty=2)
#lines(years, quarterFBquadInc $IN0, type="l", col="blue", lty=3)
#lines(years, quarterFBquadInc $INall, type="l", col="red", lty=3)
#lines(years, quarterFBquadInc $IN1, type="l", col="green", lty=3)
#abline(h = 1, lty = 'dotted')
#legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c("blue", "green", "red", "black", "black"), lty=c(1,1,1,2,3))
