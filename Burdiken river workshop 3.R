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

plot(data$Jan, data$Feb,
     main = "February vs January Streamflow",
     xlab = "January Streamflow (ML)",
     ylab = "February Streamflow (ML)",
     pch = 16,
     col = "blue"
)

model <- lm(Feb ~ Jan, data = data)
abline(model, col = "red", lwd = 2)


cor(data$Jan, data$Feb, use = "complete.obs")

#Create flow categories for January and February 

data$JanGroup <- cut(data$Jan,
                     breaks = c(0, 1000000, 5000000, Inf),
                     labels = c("Low", "Moderate", "High"))

data$FebGroup <- cut(data$Feb,
                     breaks = c(0, 1000000, 5000000, Inf),
                     labels = c("Low", "Moderate", "High"))



# Convert to long format 

river_cat <- data.frame(
  Month = rep(c("January", "February"), each = nrow(data)),
  FlowGroup = c(data$JanGroup, data$FebGroup)
)

#Create Pivot Table
pivot_table <- table(river_cat$Month, river_cat$FlowGroup)

pivot_table

#Convert to Percentages

prop_table <- prop.table(pivot_table, margin = 1)