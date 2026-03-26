# fishry 
#2bi
R <- 0.4
t <- 0:50
N <- numeric(length(t))
N[1] <- 2

for(i in 2:length(t)){
  N[i] <- N[i-1] + R*N[i-1]
}

plot(t, N, type="l", lwd=2, col="blue",
     xlab="Time (years)",
     ylab="Population Size (N)",
     main="Population Growth Over 50 Years")
grid()
