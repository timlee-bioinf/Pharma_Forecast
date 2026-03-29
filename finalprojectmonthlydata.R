library(forecast)
library(dplyr)
library(ggplot2)

# Load data
data <- read.csv("Desktop/STATS443/salesmonthly.csv", header=T)

# --- 1. PREPARE MONTHLY SEASONAL DUMMIES ---
# Since we are monthly, we only need Month dummies and Trend
data$Month <- as.factor(format(as.Date(data$datum), "%m")) # Extracts month from date
season_dummies <- model.matrix(~ Month, data = data)[, -1]
trend_term <- 1:nrow(data)

# Combine for xreg
xreg_matrix <- cbind(Trend = trend_term, season_dummies)

# --- 2. DEFINE TARGET AND SPLIT ---
# Monthly frequency = 12
y_N02BE <- ts(data$N02BE, frequency=12)
x_R03 <- data$R03

# For monthly data (~6 years), a 12-month holdout (h=12) is standard
h <- 12 
ntotal <- nrow(data)
ntrain <- ntotal - h

y_train <- head(y_N02BE, ntrain)
y_test <- tail(y_N02BE, h)

# Prepare xreg with R03 included
xreg_combined <- cbind(R03_Sales = x_R03, xreg_matrix)
xreg_train <- xreg_combined[1:ntrain, ]
xreg_test <- xreg_combined[(ntrain + 1):ntotal, ]

# --- 3. FIT THE MODELS ---
# Use auto.arima to find the best (p,d,q)
# We set seasonal=FALSE because our Month dummies handle seasonality externally
arimax_model <- auto.arima(y_train, xreg = xreg_train, seasonal=FALSE)
summary(arimax_model)

# --- 4. CALCULATE RMSE USING PROFESSOR'S FUNCTION ---
# (Ensure your armax_fc function is loaded in the environment)

# Get the order from auto.arima (e.g., if it chose 1,0,1)
best_order <- arimaorder(arimax_model)[1:3] 

armax_results <- armax_fc(tsdata = y_N02BE, 
                          ntrain = ntrain, 
                          order = best_order, 
                          method = "CSS",
                          traincoef = arimax_model$coef, 
                          include.mean = TRUE, 
                          xreg = xreg_combined)

cat("Monthly ARMAX Model RMSE:", armax_results$rmse, "\n")

# --- 5. PERTURBATIONS ---
# Perturbed (2,0,1)
train_p1 <- arima(y_train, order=c(2,0,1), xreg=xreg_train)
res_p1 <- armax_fc(y_N02BE, ntrain, c(2,0,1), "CSS", train_p1$coef, TRUE, xreg_combined)

# Perturbed (1,0,2)
train_p2 <- arima(y_train, order=c(1,0,2), xreg=xreg_train)
res_p2 <- armax_fc(y_N02BE, ntrain, c(1,0,2), "CSS", train_p2$coef, TRUE, xreg_combined)

# --- 6. CROSS-CORRELATION ---
# Monthly CCF shows if this month's flu affects this month's painkillers
ccf(data$R03, data$N02BE, main="Monthly Cross-Correlation: R03 vs N02BE")


# Ensure the datum is in Date format
data$datum <- as.Date(data$datum)


# Convert the character dates into actual Date objects
data$datum <- as.Date(data$datum, format="%Y-%m-%d")

# Now run your ggplot code exactly as you have it...
# Plotting both series together
ggplot(data, aes(x = datum)) +
  # Line for Painkillers
  geom_line(aes(y = N02BE, color = "N02BE (Painkillers)"), size = 1) +
  # Line for Flu Medication
  geom_line(aes(y = R03, color = "R03 (Flu/Airway)"), size = 1) +
  # Adding points to see the specific monthly observations
  geom_point(aes(y = N02BE, color = "N02BE (Painkillers)"), size = 1.5) +
  geom_point(aes(y = R03, color = "R03 (Flu/Airway)"), size = 1.5) +
  # Formatting the X-axis for better readability
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "Monthly Sales: Painkillers vs. Flu Medication (2014-2019)",
       subtitle = "Aggregated monthly totals showing seasonal correlation",
       x = "Date",
       y = "Quantity Sold",
       color = "Drug Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Tilt labels for clarity

