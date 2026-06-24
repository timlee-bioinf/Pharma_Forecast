library(forecast)
library(dplyr)
library(ggplot2)

data <- read.csv("Desktop/STATS443/fixed_salesmonthly.csv", header=T)

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


# 1. Create Lag 1 of R03 (using the stats::lag or dplyr::lag)
# In monthly data, k=-1 shifts the data forward by 1 month
R03_Lag1 <- stats::lag(ts(data$R03, frequency=12), -1)

# 2. Create the xreg matrix for the 'Lagged' model
# We include Month dummies and the lagged R03
#xreg_lagged <- cbind(
#  R03_prev_month = R03_Lag1, 
#  Trend = 1:nrow(data), 
#  season_dummies
#)

# --- 2. DEFINE TARGET AND SPLIT (80/20) ---
# Monthly frequency = 12
y_N02BE <- ts(data$N02BE, frequency=12)
x_R03 <- data$R03

ntotal <- nrow(data)

# Calculate 80% for training
ntrain <- floor(0.8 * ntotal)
# The remaining 20% is our holdout 'h'
h <- ntotal - ntrain

cat("Total months:", ntotal, "| Training:", ntrain, "| Holdout:", h, "\n")

y_train <- head(y_N02BE, ntrain)
y_test <- tail(y_N02BE, h)

# Prepare xreg with R03 included
xreg_combined <- cbind(R03_Sales = x_R03, xreg_matrix)
xreg_train <- xreg_combined[1:ntrain, ]
xreg_test <- xreg_combined[(ntrain + 1):ntotal, ]

# 3. Split and Fit (using the new 80/20 ntrain)
train_xreg_lagged <- xreg_lagged[1:ntrain, ]
fit_lagged <- auto.arima(y_train, xreg = train_xreg_lagged, seasonal=FALSE)

# 4. Compare RMSE
# This will now use the new 20% holdout set
results_lagged <- armax_fc(y_N02BE, ntrain, arimaorder(fit_lagged)[1:3], 
                           "CSS", fit_lagged$coef, TRUE, xreg_lagged)

cat("Lagged ARMAX RMSE (80/20 split):", results_lagged$rmse, "\n")

# --- Step 1: Fit the 6 Perturbed Models ---
# We use the same xreg_train and y_train from your 80/20 split

# 1. Higher AR order: (2,0,1)
m1 <- arima(y_train, order=c(2,0,1), xreg=xreg_train)

# 2. Higher MA order: (1,0,2)
m2 <- arima(y_train, order=c(1,0,2), xreg=xreg_train)

# 3. Increase both: (2,0,2)
m3 <- arima(y_train, order=c(2,0,2), xreg=xreg_train)

# 4. Simple AR only: (1,0,0)
m4 <- arima(y_train, order=c(1,0,0), xreg=xreg_train)

# 5. Simple MA only: (0,0,1)
m5 <- arima(y_train, order=c(0,0,1), xreg=xreg_train)

# 6. Add Differencing (ARIMAX): (1,1,1) 
# Note: If you use d=1, you may need to difference your xreg or use auto.arima
m6 <- arima(y_train, order=c(1,1,1), xreg=xreg_train)

# Create a helper to get both stats quickly
get_stats <- function(model_obj, order_vec, name) {
  # Use the professor's armax_fc function for RMSE
  res <- armax_fc(y_N02BE, ntrain, order_vec, "CSS", model_obj$coef, TRUE, xreg_combined)
  
  data.frame(
    Model = name,
    Order = paste(order_vec, collapse=","),
    AIC = AIC(model_obj),
    RMSE = res$rmse
  )
}

# Combine all results into one table
comparison_table <- rbind(
  get_stats(arimax_model, arimaorder(arimax_model)[1:3], "Auto-ARMAX"),
  get_stats(m1, c(2,0,1), "Perturbed 1"),
  get_stats(m2, c(1,0,2), "Perturbed 2"),
  get_stats(m3, c(2,0,2), "Perturbed 3"),
  get_stats(m4, c(1,0,0), "Perturbed 4"),
  get_stats(m5, c(0,0,1), "Perturbed 5"),
  get_stats(m6, c(1,1,1), "Perturbed 6 (ARIMAX)")
)

print(comparison_table)
