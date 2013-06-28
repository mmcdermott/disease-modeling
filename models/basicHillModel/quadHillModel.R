#Variables
mu0 <- 1/78       #Natural mortality rate USB per year
mu1 <-1/53        #Natural mortality rate FB per year
ro <- 0.018       #USB birth rate per year
alpha <- 0.005    #FB birth rate per year
p <- 0.103        #Fraction of new infections which are acute (fast progressors)
vF <- 1.5         #Progression rate of acute infection per year
l0 <- 0.015       #Prevalence of LTBI in the USB population in 2000
l1 <- 0.211       #Prevalence of LTBI in the FB population in 2000: 
r0 <- 0.667       #Fraction of cases due to reactivation in the USB population
r1 <- 0.780       #Fraction of cases due to reactivation in the FB population
vL0 <- 0.0014     #Progression rate for reactivation (chronic LTBI) in the USB population per year
vL1 <- 0.0010     #Progression rate for reactivation (chronic LTBI) in the FB population per year
q <- 0.708        #Fraction of infections progressing to infectious disease
mud <- 0.115      #Mortality rate due to TB per year
x <- 0.111        #Fraction of re-infected chronic LTBI moving to acute infection
f <- 0.187        #Fraction of FB arrivals with LTBI
ARI0 <- 0.030/100 #Annual risk of infection for USB in 2000
beta <- 10.39     #Effective contact rate per year
e0 <- 0.965       #Fraction of preferred contacts with own population for USB
e1 <-0.985        #Fraction of preferred contacts with own population for FB
g <- 0.0047       #Fraction of FB arrivals with LTBI who are fast progressors
                  #Cumulative fraction self-cure and treatment of active disease for both populations: 0.897
phi0 <- 1.114     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (USB)
phi1 <- 1.167     #Cumulative fraction self-cure and treatment of active disease for both populations pre year RATES (FB)
                  #Cumulative fraction of treatment for acute infection for both populations: 0.461
sigmaF0 <- 1.296  #Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
sigmaF1 <- 1.301  #Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
sigmaL <- 0.057   #Treatment rate for chronic LTBI per year
deltat <- 0.719     #The length of each time step (years)
totT <- 100       #In years
T <- totT/deltat  #Time steps

#arrays of compartment values
S0 <- S1 <- F0 <- F1 <- L0 <- L1 <- I0 <- I1 <- J0 <- J1 <- N0 <- N1 <- rep(0,T)

#Initial Values
#Total population
N0[1] <- 250	# in millions USB
N1[1] <- 31.4	# in millions FB

#Acute (Fast) LTBI
#F0[1] <- p*l0*N0[1]
#F1[1] <- p*l1*N1[1]
	F0[1] <- (1-r0)*(.008714)/vF
	F1[1] <- (1-r1)*(.007554)/vF
#Chronic (Long) LTBI
#L0[1] <- (1-p)*l0*N0[1]
#L1[1] <- (1-p)*l1*N1[1]
	L0[1] <- r0*(.008714)/vL0
	L1[1] <- r1*(.007554)/vL1
#New Cases in Population i
newCases0 <- L0[1]*vL0/r0
newCases1 <- L1[1]*vL1/r1
#Infectious TB
I0[1] <- q*newCases0/(mu0 + mud + phi0)
I1[1] <- q*newCases1/(mu1 + mud + phi1)
#Non-Infectious TB
J0[1] <- (1-q)*newCases0/(mu0 + mud + phi0)
J1[1] <- (1-q)*newCases1/(mu1 + mud + phi1)
#Susceptible
S0[1] <- N0[1] - F0[1] - L0[1] - I0[1] - J0[1]
S1[1] <- N1[1] - F1[1] - L1[1] - I1[1] - J1[1]

for (i in 1:T){
        #N0[i+1] = N0[i] + (ro - mu0)*N0[i] + ro*N1[i] - mud*(I0[i]+J0[i])
        #N1[i+1] = N1[i] + alpha*N0[i] + (alpha - mu1)*N1[i] - mud*(I1[i]+J1[i])

        c01 <- (1-e0)*((1-e1)*N1[i])/((1-e0)*N0[i] + (1-e1)*N1[i])
        c00 <- 1 - c01
        c10 <- (1-e1)*((1-e0)*N0[i])/((1-e0)*N0[i] + (1-e1)*N1[i])
        c11 <- 1 - c10

	lambda0 <- beta*(c00*(I0[i]/N0[i]) + c01*(I1[i]/N1[i]))
	lambda1 <- beta*(c10*(I0[i]/N0[i]) + c11*(I1[i]/N1[i]))
	
	dS0dti <- (ro*(N0[i]+N1[i]) + sigmaF0*F0[i] + sigmaL*L0[i] + phi0*(I0[i]+J0[i]) - (lambda0+mu0)*S0[i])
	dF0dti <- (p*lambda0*S0[i] + x*p*lambda0*L0[i] - (mu0 + vF + sigmaF0)*F0[i] )
	dL0dti <- ((1-p)*lambda0*S0[i] - (x*p*lambda0+mu0+vL0+sigmaL)*L0[i])
	dI0dti <- (q*(vF*F0[i] + vL0*L0[i]) - (mu0 + mud + phi0)*I0[i])
	dJ0dti <- ((1-q)*(vF*F0[i]+vL0*L0[i]) - (mu0 + mud + phi0)*J0[i])
	dS1dti <- ((1-f)*alpha*(N0[i]+N1[i]) + sigmaF1*F1[i] + sigmaL*L1[i] + phi1*(I1[i] + J1[i]) - lambda1*S1[i] - mu1*S1[i])
	dF1dti <- (g*p*f*alpha*(N0[i]+N1[i]) + p*lambda1*S1[i] + x*p*lambda1*L1[i] - (mu1 + vF + sigmaF1)*F1[i])
	dL1dti <- ((1-g*p)*f*alpha*(N0[i]+N1[i]) + (1-p)*lambda1*S1[i] - x*p*lambda1*L1[i] - (mu1 + vL1 +sigmaL)*L1[i])
	dI1dti <- (q*(vF*F1[i] + vL1*L1[i]) - (mu1 + mud + phi1)*I1[i])
	dJ1dti <- ((1-q)*(vF*F1[i] + vL1*L1[i]) - (mu1 + mud + phi1)*J1[i])

	S0quadPart <- (deltat^2/2)*((sigmaF0*dF0dti + sigmaL*dL0dti + phi0*(dI0dti+dJ0dti)) - (dS0dti*(lambda0+mu0)))
	F0quadPart <- (deltat^2/2)*((p*lambda0*dS0dti + x*p*lambda0*dL0dti) - ((mu0 + vF + sigmaF0)*dF0dti) )
	L0quadPart <- (deltat^2/2)*(((1-p)*lambda0*dS0dti) - ((x*p*lambda0+mu0+vL0+sigmaL)*dL0dti))
	I0quadPart <- (deltat^2/2)*((q*(vF*dF0dti + vL0*dL0dti)) - ((mu0 + mud + phi0)*dI0dti))
	J0quadPart <- (deltat^2/2)*(((1-q)*(vF*dF0dti+vL0*dL0dti)) - ((mu0 + mud + phi0)*dJ0dti))
	S1quadPart <- (deltat^2/2)*((sigmaF1*dF1dti + sigmaL*dL1dti + phi1*(dI1dti + dJ1dti)) - ((lambda1+mu1)*dS1dti))
	F1quadPart <- (deltat^2/2)*((p*lambda1*dS1dti + x*p*lambda1*dL1dti) - ((mu1 + vF + sigmaF1)*dF1dti))
	L1quadPart <- (deltat^2/2)*(((1-p)*lambda1*dS1dti) - ((x*p*lambda1 + mu1 + vL1 +sigmaL)*dL1dti))
	I1quadPart <- (deltat^2/2)*((q*(vF*dF1dti + vL1*dL1dti)) - ((mu1 + mud + phi1)*dI1dti))
	J1quadPart <- (deltat^2/2)*(((1-q)*(vF*dF1dti + vL1*dL1dti)) - ((mu1 + mud + phi1)*dJ1dti))

	S0[i+1] <- S0[i] + deltat*dS0dti + S0quadPart
	F0[i+1] <- F0[i] + deltat*dF0dti + F0quadPart
	L0[i+1] <- L0[i] + deltat*dL0dti + L0quadPart
	I0[i+1] <- I0[i] + deltat*dI0dti + I0quadPart
	J0[i+1] <- J0[i] + deltat*dJ0dti + J0quadPart
	S1[i+1] <- S1[i] + deltat*dS1dti + S1quadPart
	F1[i+1] <- F1[i] + deltat*dF1dti + F1quadPart
	L1[i+1] <- L1[i] + deltat*dL1dti + L1quadPart
	I1[i+1] <- I1[i] + deltat*dI1dti + I1quadPart
	J1[i+1] <- J1[i] + deltat*dJ1dti + J1quadPart
	
	N0[i+1] <- S0[i+1]+F0[i+1]+L0[i+1]+I0[i+1]+J0[i+1]
	N1[i+1] <- S1[i+1]+F1[i+1]+L1[i+1]+I1[i+1]+J1[i+1]
}

# calculate incidence
IN0 <- 1e6 * (vF*F0 + vL0*L0)/N0
IN1 <- 1e6 * (vF*F1 + vL1*L1)/N1
INall <- 1e6 * (vF*(F0 + F1) + vL0*L0 + vL1*L1)/(N0 + N1)

#plot incidence data
#xlab, ylab  --> labels for x-, y-axes
#log='y'     --> use logarithmic scale
#ylim=yrange --> ensure we show all data
#type="l"    --> draw line connecting data points
#col="blue"  --> color of graph
#lines() plots data in the same window as the first plot() command
yrange <- range(c(0.5,IN0,IN1,INall))
plot(IN0, xlab='time unit', ylab='incidence/million', log='y', ylim=yrange, type="l", col="blue")
lines(IN1, type="l", col="green")
lines(INall, type="l", col="red")

#WHO target incidence line
abline(h = 1, lty = 'dotted')

#I'm sure there's a better way to do legend()...
legend('topright', legend=c('USB incidence', 'FB incidence', 'Total incidence'), col=c("blue", "green", "red"), lty=c(1,1,1))
