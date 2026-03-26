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

#2bii
N <- seq(0,50,1)
G <- 0.4*N

plot(N, G, type="l", lwd=2, col="red",
     xlab="Population Size (N)",
     ylab="G(N)",
     main="Production Function G(N) = 0.4N")
grid()