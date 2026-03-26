# fishry 
#2bi
R <- 0.4
t <- 0:50
N <- numeric(length(t))
N[1] <- 2

for(i in 2:length(t)){
  N[i] <- N[i-1] + R*N[i-1]
}
