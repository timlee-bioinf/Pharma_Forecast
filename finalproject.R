library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("Desktop/STATS443/salesdaily.csv", header=T)
head(data)


#data$Month <- as.factor(data$Month)
#data$Weekday.Name <- as.factor(data$Weekday.Name)
#season_dummies <- model.matrix(~ Month + Weekday.Name, data = data)[, -1]
#trend_term <- 1:nrow(data)
#xreg_matrix <- cbind(Trend = trend_term, season_dummies)


data_subset <- data %>% filter(Year == 2018)


ggplot(data_subset, aes(x = as.Date(datum, format="%m/%d/%Y"))) +
  geom_line(aes(y = N02BE, color = "N02BE (Painkillers)")) +
  geom_line(aes(y = R03, color = "R03 (Flu/Airway)")) +
  labs(title = "Daily Sales: Painkillers vs. Flu Medication (2018)",
       x = "Date",
       y = "Quantity Sold",
       color = "Drug Category") +
  theme_minimal()




y_N02BE <- ts(data$N02BE, frequency=7)


xreg_combined <- cbind(R03_Sales = data$R03, 
                       Trend = trend_term, 
                       season_dummies)


h <- 30
n_total <- nrow(data)

y_train <- head(y_N02BE, n_total - h)
y_test <- tail(y_N02BE, h)

xreg_train <- xreg_combined[1:(n_total - h), ]
xreg_test <- xreg_combined[(n_total - h + 1):n_total, ]

# 4. Fit the ARIMAX Model
arimax_R03_N02BE <- auto.arima(y_train, xreg = xreg_train)
summary(arimax_R03_N02BE)


# Create a 1-day lag of R03 using dplyr
# The first value will be NA, which we can fill with 0 or the mean
data <- data %>%
  mutate(R03_Lag1 = lag(R03, n = 1, default = 0))

# Now, create a NEW xreg matrix that includes both current day AND previous day sales
xreg_lagged <- cbind(R03_Sales = data$R03,
                     R03_Lag1 = data$R03_Lag1, 
                     Trend = trend_term, 
                     season_dummies)

# Split and model exactly as above...
xreg_train_lagged <- xreg_lagged[1:(n_total - h), ]
arimax_lagged_model <- auto.arima(y_train, xreg = xreg_train_lagged)

summary(arimax_lagged_model)


armax_fc = function(tsdata, ntrain, order, method, traincoef, include.mean, xreg, iprint=F) { 
  obj = arima(tsdata, order=order, init=traincoef, fixed=traincoef,
              method=method, include.mean=include.mean, xreg=xreg,
              optim.control=list(maxit=0))
  fc = tsdata - obj$residuals  
  ntotal = length(tsdata)
  holdout_fc = fc[(ntrain+1):ntotal]
  rmse = sqrt(mean((tsdata[(ntrain+1):ntotal] - holdout_fc)^2))
  list(rmse=rmse, fc=holdout_fc)
}

# Target: N02BE (Painkillers) | Predictor: R03 (Flu)
y <- data$N02BE
x <- data$R03

ntotal <- length(y)
ntrain <- floor(ntotal * 0.7)

train_y <- y[1:ntrain]
holdout_y <- y[(ntrain+1):ntotal]

# Simple Regression: N02BE ~ R03
reg_model <- lm(N02BE ~ R03, data = data[1:ntrain, ])
summary(reg_model)

# Calculate RMSE for the simple regression as a baseline
pred_reg <- predict(reg_model, newdata = data[(ntrain+1):ntotal, ])
rmse_reg <- sqrt(mean((holdout_y - pred_reg)^2))
cat("Baseline Regression RMSE:", rmse_reg, "\n")

library(forecast)
# Find the order based on regression residuals
resid_auto <- auto.arima(reg_model$residuals, stationary=TRUE, seasonal=FALSE)
print(resid_auto) # Let's assume it suggests order=c(1,0,1)

# Fit the 1-step ARMAX model
# Note: xreg must be a matrix
train_armax <- arima(train_y, order=c(1,0,1), method="CSS", 
                     include.mean=TRUE, xreg=x[1:ntrain])

# Use the professor's armax_fc function to get holdout RMSE
armax_results <- armax_fc(tsdata = y, 
                          ntrain = ntrain, 
                          order = c(1,0,1), 
                          method = "CSS",
                          traincoef = train_armax$coef, 
                          include.mean = TRUE, 
                          xreg = x)

cat("ARMAX Model RMSE:", armax_results$rmse, "\n")

# Perturbation 1: Try AR(2) instead of AR(1)
train_armax_p1 <- arima(train_y, order=c(2,0,1), xreg=x[1:ntrain])
res_p1 <- armax_fc(y, ntrain, order=c(2,0,1), "CSS", train_armax_p1$coef, TRUE, x)

# Perturbation 2: Try MA(2) instead of MA(1)
train_armax_p2 <- arima(train_y, order=c(1,0,2), xreg=x[1:ntrain])
res_p2 <- armax_fc(y, ntrain, order=c(1,0,2), "CSS", train_armax_p2$coef, TRUE, x)

# Compare them
rmse_comparison <- data.frame(
  Model = c("Auto-ARMAX", "Perturbed (2,0,1)", "Perturbed (1,0,2)"),
  RMSE = c(armax_results$rmse, res_p1$rmse, res_p2$rmse)
)
print(rmse_comparison)


# CCF between R03 (Predictor) and N02BE (Target)
# Note: It's often better to use the 'pre-whitened' residuals or differenced data 
# to avoid 'spurious correlation' from common trends.
ccf(data$R03, data$N02BE, main="Cross-Correlation: R03 vs N02BE", lag.max=21)

# Use the 'checkresiduals' function from the forecast package
# This gives you the Time Plot, ACF, and Histogram all at once
#checkresiduals(arimax_results$obj) # or whatever you named your model object

# If CCF showed a spike at Lag 1:
data$R03_Lag1 <- stats::lag(ts(data$R03), -1)

# Add it to your model
xreg_with_lag <- cbind(R03_Current = data$R03, 
                       R03_Yesterday = data$R03_Lag1, 
                       Trend = trend_term, 
                       season_dummies)


