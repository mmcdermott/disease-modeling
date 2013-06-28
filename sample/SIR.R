# Simple SIR model simulation by Dylan

N <- 1000	# population size
T <- 100	# simulation end time
a <- 0.5	# infection parameter
b <- 0.3	# recovery parameter

S <- I <- R <- rep(0,T+1)
I[1] <- 1
S[1] <- N - I[1]
R[1] <- 0
for (i in 1:T) {
	S[i+1] <- S[i] - a/N * S[i] * I[i]
	R[i+1] <- R[i] + b * I[i] 
	I[i+1] <- N - R[i+1] - S[i+1]
}
  