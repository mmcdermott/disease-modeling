library(deSolve)        # for lsoda routine

# Global parameters
deltaT <- .8      #The length of each time step (years).
finalYr <- 100          #In years
totT <- finalYr/deltaT  #Time steps
cutoffYr <- 8/deltaT

#Model parameters
parms <- c(
mu0 = 1/78,       #Natural mortality rate USB per year
mu1 = 1/53,       #Natural mortality rate FB per year
ro = 0.018,       #USB birth rate per year
alpha = 0.005,    #FB arrival rate per year
p = 0.103,        #Fraction of new infections which are acute (fast progressors)
vF = 1.5,         #Progression rate of acute infection per year
l0 = 0.015,       #Prevalence of LTBI in the USB population in 2000
l1 = 0.211,       #Prevalence of LTBI in the FB population in 2000: 
r0 = 0.667,       #Fraction of cases due to reactivation in the USB population
r1 = 0.780,       #Fraction of cases due to reactivation in the FB population
vL0 = 0.0014,     #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1 = 0.0010,     #Progression rate for reactivation (chronic LTBI) in the FB population per year
q = 0.708,        #Fraction of infections progressing to infectious disease
mud = 0.115,      #Mortality rate due to TB per year
x = 0.111,        #Fraction of re-infected chronic LTBI moving to acute infection
f = 0.187,        #Fraction of FB arrivals with LTBI
ARI0 = 0.030/100, #Annual risk of infection for USB in 2000
beta = 10.39,     #Effective contact rate per year
e0 = 0.965,       #Fraction of preferred contacts with own population for USB
e1 = 0.985,       #Fraction of preferred contacts with own population for FB
g = 0.0047,       #Fraction of FB arrivals with LTBI who are fast progressors
                  #Cumulative fraction self-cure and treatment of active disease for both populations: 0.897
phi0 = 1.114,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (USB)
phi1 = 1.167,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (FB)
                  #Cumulative fraction of treatment for acute infection for both populations: 0.461
sigmaF0 = 1.296,  #Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
sigmaF1 = 1.301,  #Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
sigmaL = 0.057,   #Treatment rate for chronic LTBI per year

transmission = TRUE             #does transmission occur?
)

#Initial Values
#Matrix of compartment values
S0 <- S1 <- F0 <- F1 <- L0 <- L1 <- I0 <- I1 <- J0 <- J1 <- N0 <- N1 <- rep(0,totT)
P <- data.frame(S0,F0,L0,I0,J0,S1,F1,L1,I1,J1,N0,N1)
  
#2010 New Cases in Population i (millions)
#source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
newCases0 <- .008714  #US-born
newCases1 <- .007554  #Foreign-born

# parameter variable names made accessible via "with" function
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

Pdot <- function(t, v, parms){
with(as.list(c(parms, v)), {
    # helper parameters
    c01 <- (1-e0)*((1-e1)*N1)/((1-e0)*N0 + (1-e1)*N1)
    c00 <- 1 - c01
    c10 <- (1-e1)*((1-e0)*N0)/((1-e0)*N0 + (1-e1)*N1)
    c11 <- 1 - c10
    if (transmission) {
        lambda0 <- beta*(c00*(I0/N0) + c01*(I1/N1))
        lambda1 <- beta*(c10*(I0/N0) + c11*(I1/N1))
    } else {
        lambda0 <- lambda1 <- 0
    }
    #Differential Equations
    dS0 <- ro*(N0+N1) + sigmaF0*F0 + sigmaL*L0 + phi0*(I0+J0) - lambda0*S0 - mu0*S0
    dF0 <- p*lambda0*S0 + x*p*lambda0*L0 - (mu0 + vF + sigmaF0)*F0
    dL0 <- (1-p)*lambda0*S0 - x*p*lambda0*L0 - (mu0 + vL0 + sigmaL)*L0
    dI0 <- q*(vF*F0 + vL0*L0) - (mu0 + mud + phi0)*I0
    dJ0 <- (1-q)*(vF*F0+vL0*L0) - (mu0 + mud + phi0)*J0
    dS1 <- (1-f)*alpha*(N0+N1) + sigmaF1*F1 + sigmaL*L1 + phi1*(I1 + J1) - lambda1*S1 - mu1*S1
    dF1 <- g*p*f*alpha*(N0+N1) + p*lambda1*S1 + x*p*lambda1*L1 - (mu1 + vF + sigmaF1)*F1
    dL1 <- (1-g*p)*f*alpha*(N0+N1) + (1-p)*lambda1*S1 - x*p*lambda1*L1 - (mu1 + vL1 +sigmaL)*L1
    dI1 <- q*(vF*F1 + vL1*L1) - (mu1 + mud + phi1)*I1
    dJ1 <- (1-q)*(vF*F1 + vL1*L1) - (mu1 + mud + phi1)*J1
    dN0 <- (ro - mu0)*N0 + ro*N1 - mud*(I0 + J0)
    dN1 <- alpha*N0 + (alpha - mu1)*N1 - mud*(I1 + J1)
  
    return ( list(c(dS0, dF0, dL0, dI0, dJ0, dS1, dF1, dL1, dI1, dJ1, dN0, dN1)) )
    })
}

hill <- function(sigmaL, f, transmission=TRUE, initial=cutoffYr+1, final=totT+1, dataSet=P) {
    # set values in parms according to function parameters
    parms[c('sigmaL', 'f', 'transmission')] <- c(sigmaL, f, transmission)
    # recursive=TRUE collapses dataframe to labeled vector
    initv <- c(dataSet[initial,], recursive=TRUE)
    # times = data points to be calculated
    times <- initial:final*deltaT

    # compute master results
    mres <- lsoda(initv, times, Pdot, parms)
    # mres[,-1] = mres without 1st column
    dataSet[initial:final,] <- c(mres[,-1])
    return(dataSet)
}

generateIncidence <- function(mres) {
    with(as.list(parms), {
        IN0 <- 1e6 * (vF*mres$F0 + vL0*mres$L0)/mres$N0
        IN1 <- 1e6 * (vF*mres$F1 + vL1*mres$L1)/mres$N1
        INall <- 1e6 * (vF*(mres$F0 + mres$F1) + vL0*mres$L0 + vL1*mres$L1)/(mres$N0 + mres$N1)
        return(data.frame(IN0, IN1, INall))
    })
}

sigmaLBase <- 0.057
fBase <- 0.187
yrs <- seq(2000, 2000+finalYr, deltaT)

P<-hill(sigmaLBase,fBase,T,1,totT+1)
baseInc <- generateIncidence(P)
#plot incidence data
#  xlab, ylab  --> labels for x-, y-axes
#  log='y'     --> use logarithmic scale
#  ylim=yrange --> ensure we show all data
#  type='l'    --> draw line connecting data points
#col='blue'  --> color of graph
#lines() plots data in the same window as the first plot() command

#Plot A: Where Transmission is cut after 2008
noTrans <-hill(sigmaLBase,fBase,F)
noTransInc <- generateIncidence(noTrans)

#When Calculating range, we presume that FB always has higher incidence rate
yrange <- range(c(0.5,baseInc$IN1,noTransInc$IN1))
dev.new()
plot(yrs, baseInc$IN0, main='Plot A: Cut Trans. in 2008', xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type='l', col='blue')
lines(yrs, baseInc$INall, type='l', col='red')
lines(yrs, baseInc$IN1, type='l', col='green')
lines(yrs, noTransInc$IN0, type='l', col='blue', lty=2)
lines(yrs, noTransInc$INall, type='l', col='red', lty=2)
lines(yrs, noTransInc$IN1, type='l', col='green', lty=2)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'With Transmission Cutoff in 2008'), col=c('blue', 'green', 'red', 'black'), lty=c(1,1,1,2))

#Plot B: Where LTBI treatment x2 or x4
doubled <-hill(sigmaLBase*2,fBase,T)
doubledInc <- generateIncidence(doubled)
quad <- hill(sigmaLBase*4,fBase,T)
quadInc <- generateIncidence(quad)

#When Calculating range, we presume that FB always has higher incidence rate after 2008
yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
dev.new()
plot(yrs, baseInc$IN0, main='Plot B: Increase Chronic LTBI Treatment in 2008', xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type='l', col='blue')
lines(yrs, baseInc$INall, type='l', col='red')
lines(yrs, baseInc$IN1, type='l', col='green')
lines(yrs, doubledInc$IN0, type='l', col='blue', lty=2)
lines(yrs, doubledInc$INall, type='l', col='red', lty=2)
lines(yrs, doubledInc$IN1, type='l', col='green', lty=2)
lines(yrs, quadInc$IN0, type='l', col='blue', lty=3)
lines(yrs, quadInc$INall, type='l', col='red', lty=3)
lines(yrs, quadInc$IN1, type='l', col='green', lty=3)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c('blue', 'green', 'red', 'black', 'black'), lty=c(1,1,1,2,3))

#Plot C: Where FB arrivals cut in half, also LTBI treatment x2 or x4 after 2008
halfFB <- hill(sigmaLBase,fBase/2,T)
halfFBInc <- generateIncidence(halfFB)
halfFBdoubled <- hill(sigmaLBase*2,fBase/2,T)
halfFBdoubledInc <- generateIncidence(halfFBdoubled)
halfFBquad <- hill(sigmaLBase*4,fBase/2,T)
halfFBquadInc <- generateIncidence(halfFBquad)

#When Calculating range, we presume that FB always has higher incidence rate
yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
dev.new()
plot(yrs, halfFBInc $IN0, main='Plot C: Half FB Arrival Rate, increase LTBI Treatment', xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type='l', col='blue')
lines(yrs, halfFBInc $INall, type='l', col='red')
lines(yrs, halfFBInc $IN1, type='l', col='green')
lines(yrs, halfFBdoubledInc $IN0, type='l', col='blue', lty=2)
lines(yrs, halfFBdoubledInc $INall, type='l', col='red', lty=2)
lines(yrs, halfFBdoubledInc $IN1, type='l', col='green', lty=2)
lines(yrs, halfFBquadInc $IN0, type='l', col='blue', lty=3)
lines(yrs, halfFBquadInc $INall, type='l', col='red', lty=3)
lines(yrs, halfFBquadInc $IN1, type='l', col='green', lty=3)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c('blue', 'green', 'red', 'black', 'black'), lty=c(1,1,1,2,3))

#Plot D: Where FB arrivals divided by 4, also LTBI treatment x2 or x4 after 2008
quarterFB <- hill(sigmaLBase,fBase/4,T)
quarterFBInc <- generateIncidence(quarterFB)
quarterFBdoubled <- hill(sigmaLBase*2,fBase/4,T)
quarterFBdoubledInc <- generateIncidence(quarterFBdoubled)
quarterFBquad <- hill(sigmaLBase*4,fBase/4,T)
quarterFBquadInc <- generateIncidence(quarterFBquad)

#When Calculating range, we presume that FB always has higher incidence rate
yrange <- range(c(0.5,baseInc$IN1,quadInc$IN1,doubledInc$IN1))
dev.new()
plot(yrs, quarterFBInc $IN0, main='Plot D: 1/4 FB Arrival Rate, increase LTBI Treatment', xlab='year', ylab='incidence/million', log = 'y', ylim=yrange, type='l', col='blue')
lines(yrs, quarterFBInc $INall, type='l', col='red')
lines(yrs, quarterFBInc $IN1, type='l', col='green')
lines(yrs, quarterFBdoubledInc $IN0, type='l', col='blue', lty=2)
lines(yrs, quarterFBdoubledInc $INall, type='l', col='red', lty=2)
lines(yrs, quarterFBdoubledInc $IN1, type='l', col='green', lty=2)
lines(yrs, quarterFBquadInc $IN0, type='l', col='blue', lty=3)
lines(yrs, quarterFBquadInc $INall, type='l', col='red', lty=3)
lines(yrs, quarterFBquadInc $IN1, type='l', col='green', lty=3)
abline(h = 1, lty = 'dotted')
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence', 'Chronic LTBI Treatment x2 in 2008', 'Chronic LTBI Treatment x4 in 2008'), col=c('blue', 'green', 'red', 'black', 'black'), lty=c(1,1,1,2,3))
