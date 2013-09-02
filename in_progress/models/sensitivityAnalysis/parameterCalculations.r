  # randLHS$sigmaL[i] <- qtriangle(uniformRandLHS[i,1],0.015,0.086,0.057)       #Triangle(0.015,0.057,0.086)
  # randLHS$vL1[i]    <- qtriangle(uniformRandLHS[i,2],0.0009,0.0014,0.0010)    #Triangle(0.0009,0.0010,0.0014)
  # randLHS$f[i]      <- qtriangle(uniformRandLHS[i,3],0.157,0.232,0.187)       #Triangle(0.157,0.187,0.232)
  # randLHS$p[i]      <- qtriangle(uniformRandLHS[i,4],0.053,0.137,0.103)       #Triangle(0.053,0.103,0.137)
  randLHS$ARI0[i]   <- qtriangle(uniformRandLHS[i,5],0.00021,0.00030,0.00030) #Triangle(0.00021,0.00030,0.00030)
  # randLHS$q[i]      <- qtriangle(uniformRandLHS[i,6],0.569,0.825,0.708)       #Triangle(0.569,0.708,0.825)
  # randLHS$g[i]      <- qtriangle(uniformRandLHS[i,7],0.0008,0.0815,0.0047)    #Triangle(0.0008,0.0047,0.0815)
  randLHS$sigmaF[i] <- qtriangle(uniformRandLHS[i,8],0.419,0.574,0.461)       #Triangle(0.419,0.461,0.574)
  # randLHS$r1[i]     <- qtriangle(uniformRandLHS[i,9],0.759,0.831,0.780)       #Triangle(0.759,0.780,0.831)
  # randLHS$r0[i]     <- qtriangle(uniformRandLHS[i,10],0.623,0.694,0.667)      #Triangle(0.623,0.667,0.694)
  # randLHS$mud[i]    <- qtriangle(uniformRandLHS[i,11],0.071,0.231,0.115)      #Triangle(0.071,0.115,0.231)
  # randLHS$x[i]      <- qtriangle(uniformRandLHS[i,12],0.088,0.860,0.111)      #Triangle(0.088,0.111,0.860)
  # randLHS$vL0[i]    <- qtriangle(uniformRandLHS[i,13],0.0011,0.0015,0.0014)   #Triangle(0.0011,0.0014,0.0015)
  randLHS$phi[i]    <- qtriangle(uniformRandLHS[i,14],0.861,0.938,0.897)      #Triangle(0.861,0.897,0.938)
  # randLHS$e0[i]     <- qtriangle(uniformRandLHS[i,15],0.853,0.995,0.965)      #Triangle(0.853,0.965,0.995)
  # randLHS$e1[i]     <- qtriangle(uniformRandLHS[i,16],0.877,0.999,0.985)      #Triangle(0.877,0.985,0.999)
  
  # mu0   = 1/78,      #Natural mortality rate USB per year
# mu1   = 1/53,      #Natural mortality rate FB per year
# ro    = 0.018,     #USB birth rate per year
# alpha = 0.005,     #FB arrival rate per year
# p     = 0.103,     #Fraction of new infections which are acute (fast progressors)
# vF    = 1.5,       #Progression rate of acute infection per year
l0    = 0.015,     #Prevalence of LTBI in the USB population in 2000
l1    = 0.211,     #Prevalence of LTBI in the FB population in 2000: 
# r0    = 0.667,     #Fraction of cases due to reactivation in the USB population
# r1    = 0.780,     #Fraction of cases due to reactivation in the FB population
# vL0   = 0.0014,    #Progression rate for reactivation (chronic LTBI) in the USB population per year
# vL1   = 0.0010,    #Progression rate for reactivation (chronic LTBI) in the FB population per year
# q     = 0.708,     #Fraction of infections progressing to infectious disease
# mud   = 0.115,     #Mortality rate due to TB per year
# x     = 0.111,     #Fraction of re-infected chronic LTBI moving to acute infection
ARI0  = 0.030/100, #Annual risk of infection for USB in 2000
beta  = 10.39,     #Effective contact rate per year
# e0    = 0.965,     #Fraction of preferred contacts with own population for USB
# e1    = 0.985,     #Fraction of preferred contacts with own population for FB
# g     = 0.0047,    #Fraction of FB arrivals with LTBI who are fast progressors
phi0  = 1.114,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (USB)
phi1  = 1.167,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (FB)
sigmaF0 = 1.296,   #Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
sigmaF1 = 1.301,   #Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
# sigmaLBase = sigmaLBase, #Treatment rate for chronic LTBI per year
# fBase = fBase,      #Fraction of FB arrivals with LTBI

phi0 <- phi[i]*(mu0 + mud)/(1-phi[i])
phi1 <- phi[i]*(mu1 + mud)/(1-phi[i])
sigmaF0 <- sigmaF[i]*(mu0 + vF)/(1-sigmaF[i])
sigmaF1 <- sigmaF[i]*(mu1 + vF)/(1-sigmaF[i])
beta <- ARI0[i]*((mu0 + mud[i] + phi0)/q[i])/(c00*I0[1]/N0[1] + c01*I1[1]/N1[1])