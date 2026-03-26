# exercise 3
library(readr)
data <- read_excel("burdekin%20streamflow.xlsx")
colnames(data)
str(data)

# remove scientific notation
options(scipen = 999)

# Produce frequency histogram for January streamflow
hist(data$Jan,
     main = "Frequency Histogram of January Streamflow (1951–2003)",
     xlab = "Streamflow (ML)",
     ylab = "Frequency",
     col = "purple",
     border = "black",
     breaks = 10)

mean(data$Jan, na.rm = TRUE)
median(data$Jan, na.rm = TRUE)
sd(data$Jan, na.rm = TRUE)
quantile(data$Jan, na.rm = TRUE)
IQR(data$Jan, na.rm = TRUE)
