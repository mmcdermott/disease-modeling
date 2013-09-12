library(lhs)
library(triangle)

#Uniform Latin Hypercube with 18 parameters, resolution: n
n <- 1000  #number of divisions in probability distributions
uniformRandLHS <- randomLHS(n,18)

sigmaLBase  <- 0.057
fBase       <- 0.187
transBase   <- 1
incLTBIBase <- 1


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

parms <- c(
mu0   = 1/78,      #Natural mortality rate USB per year
mu1   = 1/53,      #Natural mortality rate FB per year
ro    = 0.018,     #USB birth rate per year
alpha = 0.005,     #FB arrival rate per year
p     = 0.103,     #Fraction of new infections which are acute (fast progressors)
vF    = 1.5,       #Progression rate of acute infection per year
l0    = 0.015,     #Prevalence of LTBI in the USB population in 2000
l1    = 0.211,     #Prevalence of LTBI in the FB population in 2000: 
r0    = 0.667,     #Fraction of cases due to reactivation in the USB population
r1    = 0.780,     #Fraction of cases due to reactivation in the FB population
vL0   = 0.0014,    #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1   = 0.0010,    #Progression rate for reactivation (chronic LTBI) in the FB population per year
q     = 0.708,     #Fraction of infections progressing to infectious disease
mud   = 0.115,     #Mortality rate due to TB per year
x     = 0.111,     #Fraction of re-infected chronic LTBI moving to acute infection
ARI0  = 0.030/100, #Annual risk of infection for USB in 2000
beta  = 10.39,     #Effective contact rate per year
e0    = 0.965,     #Fraction of preferred contacts with own population for USB
e1    = 0.985,     #Fraction of preferred contacts with own population for FB
g     = 0.0047,    #Fraction of FB arrivals with LTBI who are fast progressors
phi0  = 1.114,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (USB)
phi1  = 1.167,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (FB)
sigmaF0 = 1.296,   #Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
sigmaF1 = 1.301,   #Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
sigmaLBase = sigmaLBase, #Treatment rate for chronic LTBI per year
fBase = fBase,      #Fraction of FB arrivals with LTBI

#2010 New Cases in Population i (millions)
#source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
newCases0 = .008714,  #US-born
newCases1 = .007554,  #Foreign-born

CtBase = Ct,
ClBase = Cl
)

#Ordered by PRCC absolute value from Hill's sensitivity analysis
randLHS <- data.frame(sigmaL=1:n,vL1=1:n,f=1:n,p=1:n,#beta=1:n,
                      ARI0=1:n,
                      q=1:n,g=1:n,sigmaF=1:n,r1=1:n,r0=1:n,
                      mud=1:n,x=1:n,vL0=1:n,phi=1:n,e0=1:n,
                      e1=1:n,Ct=1:n,Cl=1:n)

#Generate Latin Hypercube of parameter values
# For Hill constants, probability distributions
# used are triangular, determined from the best-fit
# values in the Hill model.  Endpoints are given by
# the 2.5 and 97.5 percentiles, and the mode is
# given by the best fit.  (See Table 1 in Hill paper)
# Probability distributions used in the original Hill
# model are given below for each parameter, commented out.
for(i in 1:n){
  # Track progress 
  if(i%%(n/100)==0) {print(i/(n/100))}
  #Original Hill parameters (best-fit distributions)
  randLHS$sigmaL[i] <- qtriangle(uniformRandLHS[i,1],0.015,0.086,0.057)       #Triangle(0.015,0.057,0.086)
  randLHS$vL1[i]    <- qtriangle(uniformRandLHS[i,2],0.0009,0.0014,0.0010)    #Triangle(0.0009,0.0010,0.0014)
  randLHS$f[i]      <- qtriangle(uniformRandLHS[i,3],0.157,0.232,0.187)       #Triangle(0.157,0.187,0.232)
  randLHS$p[i]      <- qtriangle(uniformRandLHS[i,4],0.053,0.137,0.103)       #Triangle(0.053,0.103,0.137)
  randLHS$ARI0[i]   <- qtriangle(uniformRandLHS[i,5],0.00021,0.00030,0.00030) #Triangle(0.00021,0.00030,0.00030)
  #randLHS$beta[i]   <- qtriangle(uniformRandLHS[i,5],5.06,21.44,10.39)        #Triangle(5.06,10.39,21.44)
  randLHS$q[i]      <- qtriangle(uniformRandLHS[i,6],0.569,0.825,0.708)       #Triangle(0.569,0.708,0.825)
  randLHS$g[i]      <- qtriangle(uniformRandLHS[i,7],0.0008,0.0815,0.0047)    #Triangle(0.0008,0.0047,0.0815)
  randLHS$sigmaF[i] <- qtriangle(uniformRandLHS[i,8],0.419,0.574,0.461)       #Triangle(0.419,0.461,0.574)
  randLHS$r1[i]     <- qtriangle(uniformRandLHS[i,9],0.759,0.831,0.780)       #Triangle(0.759,0.780,0.831)
  randLHS$r0[i]     <- qtriangle(uniformRandLHS[i,10],0.623,0.694,0.667)      #Triangle(0.623,0.667,0.694)
  randLHS$mud[i]    <- qtriangle(uniformRandLHS[i,11],0.071,0.231,0.115)      #Triangle(0.071,0.115,0.231)
  randLHS$x[i]      <- qtriangle(uniformRandLHS[i,12],0.088,0.860,0.111)      #Triangle(0.088,0.111,0.860)
  randLHS$vL0[i]    <- qtriangle(uniformRandLHS[i,13],0.0011,0.0015,0.0014)   #Triangle(0.0011,0.0014,0.0015)
  randLHS$phi[i]    <- qtriangle(uniformRandLHS[i,14],0.861,0.938,0.897)      #Triangle(0.861,0.897,0.938)
  randLHS$e0[i]     <- qtriangle(uniformRandLHS[i,15],0.853,0.995,0.965)      #Triangle(0.853,0.965,0.995)
  randLHS$e1[i]     <- qtriangle(uniformRandLHS[i,16],0.877,0.999,0.985)      #Triangle(0.877,0.985,0.999)
  
  #Cost Parameters
  randLHS$Ct[i]     <- uniformRandLHS[i,17]*(2803) + 12613   #Uniform(12613,15416)
  randLHS$Cl[i]     <- uniformRandLHS[i,18]*(140)  + 630     #Uniform(630,770)
  
  #Original Hill parameters (original distributions)
  # randLHS$sigmaL[i] <- uniformRandLHS[i,1]*(0.09) + 0.01                 #Uniform(0.01,0.10)
  ## randLHS$vL1[i]   <- imputed
  # randLHS$f[i]      <- uniformRandLHS[i,3]*(0.1) + 0.15                  #Uniform(0.15,0.25)
  # randLHS$p[i]      <- qtriangle(uniformRandLHS[i,4],0.010,0.150,0.056)  #Triangle(0.010,0.056,0.150)
  # randLHS$ARI0[i]   <- uniformRandLHS[i,5]*(0.0001) + 0.0002             #Uniform(0.0002,0.0003)
  # randLHS$q[i]      <- qtriangle(uniformRandLHS[i,6],0.50,0.85,0.75)     #Triangle(0.50,0.75,0.85)
  # randLHS$g[i]      <- uniformRandLHS[i,7]                               #Uniform(0,1)
  # randLHS$sigmaF[i] <- qtriangle(uniformRandLHS[i,8],0.40,0.60,0.50)     #Triangle(0.40,0.50,0.60)
  # randLHS$r1[i]     <- qtriangle(uniformRandLHS[i,9],0.75,0.85,0.80)     #Triangle(0.75,0.80,0.85)
  # randLHS$r0[i]     <- qtriangle(uniformRandLHS[i,10],0.60,0.70,0.65)    #Triangle(0.60,0.65,0.70)
  # randLHS$mud[i]    <- qtriangle(uniformRandLHS[i,11],0.06,0.28,0.14)    #Triangle(0.06,0.14,0.28)
  # randLHS$x[i]      <- uniformRandLHS[i,12]                              #Uniform(0,1)
  ## randLHS$vL0[i]   <- imputed
  # randLHS$phi[i]    <- qtriangle(uniformRandLHS[i,14],0.85,0.95,0.90)    #Triangle(0.85,0.90,0.95)
  # randLHS$e0[i]     <- uniformRandLHS[i,15]*(0.15)  + 0.85               #Uniform(0.85,1)
  # randLHS$e1[i]     <- uniformRandLHS[i,16]*(0.15)  + 0.85               #Uniform(0.85,1)
}

save(randLHS, file = 'randLHS.RData')
