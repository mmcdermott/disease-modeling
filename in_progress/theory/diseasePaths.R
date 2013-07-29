# Let numPaths(s,i) be the number of paths to some absorbing state
# of the discrete SIR stochastic model when there are s susceptibles
# and i infecteds. Then numPaths satisfies the recurrence relation
#
# numPaths(s,i) = 1 + sum_{j=0}^{i-1} { numPaths(s-1, i-j+1) }
#
# In addition, the two base cases are numPaths(s,0) = numPaths(0,i) = 1
#

N <- 10
# matrix n, rows are susceptibles, columns are infected
numPaths <- matrix(nrow=N+1, ncol=N+1)
# Invalid values of (s,i)
for(s in 1:N) { numPaths[s+1,N+1]  <- 0 }
for(i in 1:N) { numPaths[N+1,i+1]  <- 0 }
# Base cases
for(s in 0:N) { numPaths[s+1,1] <- 1 }
for(i in 0:N) { numPaths[1,i+1] <- 1 }
# Recursive calculation via dynamic programming
# NOTE: the numbers are different because indices in R
# start with row 1, not row 0
for(s in 1:(N-1)) {
    for(i in 1:(N-1)) {
        if(s + i > N) {
            numPaths[s+1,i+1] <- 0
        } else {
            #cat(s,i,' : ', numPaths[s,3:(i+2)],'\n')
            numPaths[s+1,i+1] <- 1 + sum(numPaths[s,3:(i+2)])
        }
    }
}

#Polynomial expansions of numPaths(s,i) for small values of s
f1 <- function(i) { return (1 + i) }
f2 <- function(i) { return (1 + 5/2*i + 1/2*i^2) }
f3 <- function(i) { return (1 + 35/6*i + 2*i^2 + 1/6*i^3) }
f4 <- function(i) { return (1 + 175/12*i + 155/24*i^2 + 11/12*i^3 + 1/24*i^4) }
f5 <- function(i) { return (1 + 2387/60*i + 485/24*i^2 + 89/24*i^3 + 7/24*i^4 + 1/120*i^5) }
f6 <- function(i) { return (1 + 7007/60*i + 2884/45*i^2 + 655/48*i^3 + 203/144*i^4 + 17/240*i^5 + 1/720*i^6) }
f7 <- function(i) { return (1 + 152009/420*i + 3745/18*i^2 + 35047/720*i^3 + 427/72*i^4 + 143/360*i^5 + 1/72*i^6 + 1/5040*i^7) }

catalan <- function(n) { return(choose(2*n,n)/(n+1)) }
s1 <- function(i) { cumsum(catalan(i)) }

# Some of these have nice factorizations ... but there's no real pattern
#f2 <- function(i) { return (1/2*i*(i+5)+1) }
#f3 <- function(i) { return (1/6*i*(i+5)*(i+7)+1) }
#f4 <- function(i) { return (1/24*i*(i+5)*(i+7)*(i+10)+1) }
#f6 <- function(i) { return (1/720*k*(k+6)*(k+7)*(k+11)*(k+13)*(k+14)+1) }

#Output
cat('\n\nRows-1 are # of susceptibles\nColumns-1 are # of infecteds\n\n')
#write.csv(numPaths,'dPaths.csv')
#print(numPaths)
print(numPaths[,2])
print(s1(0:(N-1)))


# Verifying that the polynomial expansions are correct
#f1(0:(N-1))
#f2(0:(N-2))
#f3(0:(N-3))
#f4(0:(N-4))
#f5(0:(N-5))
#f6(0:(N-6))
#f7(0:(N-7))
