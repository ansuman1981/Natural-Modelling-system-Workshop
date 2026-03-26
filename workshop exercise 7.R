# chapter 7 question 6
#6i
# Load packages
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(readxl)

soi <- read_excel("soidata%20for%20q6%20Chap7.xlsx", col_names = FALSE)
# Set column names
colnames(soi) <- c("Year", "Month", "YearMonth", "SOI")

# Add plotting index
soi$Time <- 1:nrow(soi)
head(soi)

ggplot(soi, aes(x = Time, y = SOI)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  
  # Running mean line (question iii)
  geom_line(aes(y = run5), color = "black", linewidth = 1.5) +
  
  # Neutral line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  
  # ENSO thresholds
  geom_hline(yintercept = c(-7, 7), linetype = "dotted", color = "darkgreen") +
  
  labs(
    title = "Southern Oscillation Index (2007–2010)",
    x = "Time (Months 2007–2010)",
    y = "SOI"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12)
  )

#ii
soi$run5 <- stats::filter(soi$SOI, rep(1/5, 5), sides = 2)


# question 7  
library(readxl)
library(ggplot2)
library(dplyr)

df <- read_excel("sst34_1997to2002.xlsx", sheet = "Sheet1")
names(df)[1:4] <- c("MedianMonth", "Anomaly", "YEAR", "MONTHS")

df <- df %>%
  mutate(Date = as.Date(MedianMonth, origin = "1899-12-30"),
         Phase = case_when(Anomaly > 0.5 ~ "El Niño",
                           Anomaly < -0.5 ~ "La Niña",
                           TRUE ~ "Neutral"))

ggplot(df, aes(Date, Anomaly, fill = Phase)) +
  geom_col(width = 27) +
  scale_fill_manual(values = c("El Niño" = "red3", "La Niña" = "blue", "Neutral" = "gray80")) +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") +
  labs(title = "ONI & El Niño phases between 1997 and 2002",
       y = "SOI Anomaly") +
  ylim(-2, 3) +
  theme_minimal() +
  theme(legend.position = "bottom")


# exercise 9

# Exercise 9 (b) & (c) – Lagged SON ONI impact on Tully sugarcane yields


# Load required packages
library(readxl)     # reading Excel
library(dplyr)      # data manipulation
library(ggplot2)    # nice stacked bar plot
# install.packages("vcd")   # uncomment if not installed – for classic mosaic
library(vcd)


# 1. Read the data

ONI <- read_excel("NOAA_ONI_1950_to_2010.xlsx", sheet = 1)
Tully <- read_excel("TullySugar.xlsx", sheet = 1)

# Standardize column names (adjust if your Excel headers are slightly different)
colnames(Tully) <- c("Year", "Cane_yield", "SOI_phase")   # we ignore SOI_phase
colnames(ONI)[1] <- "Year"


# 2. Calculate yield anomaly


mean_yield <- mean(Tully$Cane_yield, na.rm = TRUE)
Tully <- Tully %>%
  mutate(yield_anomaly = Cane_yield - mean_yield)


# 3. Create lagged SON ONI 

ONI_lagged <- ONI %>%
  select(Year, SON) %>%
  mutate(Year_for_yield = Year + 1) %>%          # shift: SON 1950 to yield 1951
  rename(SON_lagged = SON) %>%
  select(Year_for_yield, SON_lagged)

Tully <- Tully %>%
  left_join(ONI_lagged, by = c("Year" = "Year_for_yield"))

# Keep only rows with valid lagged SON
Tully_valid <- Tully %>% filter(!is.na(SON_lagged))


# 4. Classify phases and outcome


Tully_valid <- Tully_valid %>%
  mutate(
    ONI_phase = case_when(
      SON_lagged <= -0.5 ~ "La Niña",
      SON_lagged >=  0.5 ~ "El Niño",
      TRUE               ~ "Neutral"
    ),
    ONI_phase = factor(ONI_phase, levels = c("La Niña", "Neutral", "El Niño")),
    
    YieldOutcome = if_else(yield_anomaly > 0, "better", "worse"),
    YieldOutcome = factor(YieldOutcome, levels = c("better", "worse"))
  )


# 5. Part (b) – Tables


# Yield anomaly statistics (min, mean, max)
anomaly_stats <- Tully_valid %>%
  group_by(ONI_phase) %>%
  summarise(
    Minimum = min(yield_anomaly, na.rm = TRUE),
    Mean    = mean(yield_anomaly, na.rm = TRUE),
    Maximum = max(yield_anomaly, na.rm = TRUE)
  ) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

cat("\n=== Yield anomaly statistics by lagged ONI phase ===\n")
print(anomaly_stats)

# Counts
counts <- table(Tully_valid$YieldOutcome, Tully_valid$ONI_phase)
cat("\n=== Yield outcome counts ===\n")
print(counts)

# Proportions (column percentages)
proportions <- prop.table(counts, margin = 2)
proportions <- round(proportions, 3)
cat("\n=== Yield outcome proportions ===\n")
print(proportions)


# 6. Part (c) – Figures



cat("\nGenerating classic mosaic plot...\n")
mosaicplot(table(Tully_valid$ONI_phase, Tully_valid$YieldOutcome),
           main  = "Effect of lagged ONI on yield outcome",
           xlab  = "ONI phase",
           ylab  = "Proportion of years",
           col   = c("blue", "red"),          # better = blue, worse = red
           cex.axis = 1.1,
           direction = "v")


prop_df <- as.data.frame(proportions) %>%
  rename(
    YieldOutcome = Var1,
    ONI_phase    = Var2,
    Proportion   = Freq
  ) %>%
  mutate(
    ONI_phase    = factor(ONI_phase, levels = c("La Niña", "Neutral", "El Niño")),
    YieldOutcome = factor(YieldOutcome, levels = c("better", "worse"))
  )

cat("\nGenerating 100% stacked bar plot...\n")
ggplot(prop_df, aes(x = ONI_phase, y = Proportion, fill = YieldOutcome)) +
  geom_bar(stat = "identity", position = position_fill()) +
  scale_fill_manual(values = c("better" = "blue", "worse" = "red")) +
  labs(
    title    = "Effect of lagged ONI phase on sugarcane yield outcome",
    subtitle = "Proportions of better (blue) and worse (red) than average yields",
    x        = "ONI phase (lagged SON)",
    y        = "Proportion of years"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    axis.text.x     = element_text(angle = 0, vjust = 0.5)
  )


#10) burdikin river 
#a)
library(readxl)
Burdekin <- read_excel("burdekin%20streamflow.xlsx")
head(Burdekin)
str(Burdekin)
summary(Burdekin)
#b)
n <- nrow(Burdekin)

Burdekin$Summer <- c(
  Burdekin$Dec[-n] + Burdekin$Jan[-1] + Burdekin$Feb[-1],
  NA
)

summer_data <- data.frame(
  Year = Burdekin$Year[1:(n-1)],
  Summer_Streamflow_ML = Burdekin$Summer[1:(n-1)]
)

head(Burdekin[, c("Year", "Dec", "Jan", "Feb", "summer")],10)
head(summer_data,15)
summary(summer_data$Summer_Streamflow_ML)
#c
#Create the plot
plot(summer_data$Year, 
     summer_data$Summer_Streamflow_ML / 1e6,     
     type = "b",                                 
     pch = 19,                                   
     col = "blue",
     lwd = 2,
     main = "Burdekin Summer (DJF) Streamflow 1951/52 to 2002/03",
     xlab = "Year",
     ylab = "Summer Streamflow (million Megalitres)",
     ylim = c(0, 60))
# Add mean line
abline(h = mean(summer_data$Summer_Streamflow_ML / 1e6, na.rm = TRUE), 
       col = "red", lty = 2, lwd = 2)

# Add linear trend line
abline(lm(Summer_Streamflow_ML / 1e6 ~ Year, data = summer_data), 
       col = "darkred", lwd = 3)

trend_model <- lm(Summer_Streamflow_ML ~ Year, data = summer_data)

summary(trend_model)
#d
# Mean summer streamflow
mean_djf <- mean(summer_data$Summer_Streamflow_ML, na.rm = TRUE)

# Median summer streamflow
median_djf <- median(summer_data$Summer_Streamflow_ML, na.rm = TRUE)

# Print the results nicely
cat("Mean DJF (Summer) Streamflow:", format(mean_djf, big.mark = ","), "ML\n")
cat("Median DJF (Summer) Streamflow:", format(median_djf, big.mark = ","), "ML\n")


# (e) Create a new factor variable: Above or Below Median
# =============================================

# First, make sure we have the median (from part d)
median_djf <- median(summer_data$Summer_Streamflow_ML, na.rm = TRUE)

# Create the new factor column in summer_data
summer_data$Flow_Level <- ifelse(summer_data$Summer_Streamflow_ML > median_djf, 
                                 "above", 
                                 "below")

# Convert it to a proper factor
summer_data$Flow_Level <- factor(summer_data$Flow_Level, levels = c("below", "above"))
table(summer_data$Flow_Level)

head(summer_data, 15)

#f
# Example placeholde:
summer_data$SON_ONI_Phase <- factor(rep(c("Neutral", "La Nina", "El Nino"), length.out = nrow(summer_data)),
                                    levels = c("El Nino", "Neutral", "La Nina"))
# 2. Calculate proportions

library(dplyr)

proportion_data <- summer_data %>%
  group_by(SON_ONI_Phase, Flow_Level) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(SON_ONI_Phase) %>%
  mutate(proportion = count / sum(count))


# 3. Produce the plot 

library(ggplot2)

ggplot(proportion_data, aes(x = SON_ONI_Phase, y = proportion, fill = Flow_Level)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  scale_fill_manual(values = c("below" = "orange", "above" = "steelblue")) +
  labs(title = "Proportion of Above and Below Median Summer Streamflow by SON ONI Phase",
       x = "SON ONI Phase",
       y = "Proportion",
       fill = "Flow Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

