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
e1    = 0.985,      #Fraction of preferred contacts with own population for FB
g     = 0.0047,    #Fraction of FB arrivals with LTBI who are fast progressors
phi0  = 1.114,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (USB)
phi1  = 1.167,     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (FB)
sigmaF0 = 1.296,   #Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
sigmaF1 = 1.301,   #Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
sigmaLBase = 0.057, #Treatment rate for chronic LTBI per year
fBase = 0.187,      #Fraction of FB arrivals with LTBI

#2010 New Cases in Population i (millions)
#source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
newCases0 = .008714,  #US-born
newCases1 = .007554  #Foreign-born
)
