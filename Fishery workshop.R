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

#2cii

N <- seq(0,200,1)
G <- -0.1*N

plot(N, G,
     type = "l",
     col = "purple",        # line color
     lwd = 3,            # bold/thick line
     xlab = "Population Size (N)",
     ylab = "G(N)",
     main = "Production Function G(N)")
grid()

#3a

library(readxl)

data <- read_excel("Paramecium_aurelia.xlsx")

N <- data$Paramecium

lambda <- N[-1] / N[-length(N)]

R <- lambda - 1

results <- data.frame(
  N = N[-length(N)],
  lambda = lambda,
  R = R
)

print(results)

#3b
plot(results$N, results$R,
     xlab = "Population size (N)",
     ylab = "Growth rate (R)",
     main = "Relationship between R and N",
     pch = 19,
     col = "green")
model <- lm(R ~ N, data = results)
summary(model)

plot(results$N, results$R,
     xlab = "Population size (N)",
     ylab = "Growth rate (R)",
     main = "Relationship between R and N",
     pch = 19,
     col = "green")

abline(model, col="red", lwd=2)

R0 = 1.361
b = -0.00243
k= -(R0/b)
k

N = 1
a = 1.361
b = -0.00243
r = a + b*N
r