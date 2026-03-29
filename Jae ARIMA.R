# =============================================================
# STAT 443 — Jae Shin | ARIMA Without Explanatory Variables
# =============================================================

library(forecast)
library(tseries)

# ── Load Data ─────────────────────────────────────────────────
sales       <- read.csv("salesdaily.csv", stringsAsFactors = FALSE)
sales$datum  <- as.Date(sales$datum, format = "%m/%d/%Y")
sales        <- sales[order(sales$datum), ]

drug_classes <- c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", "N05C", "R03", "R06")

# ── Train / Holdout Split (last 365 days = holdout) ───────────
n_train  <- nrow(sales) - 365
train_df <- sales[1:n_train, ]
hold_df  <- sales[(n_train + 1):nrow(sales), ]

# ── Fit ARIMA + Compute Holdout RMSE for Each Drug Class ──────
results <- data.frame(
  Drug         = character(),
  Best_Model   = character(),
  Holdout_RMSE = numeric(),
  stringsAsFactors = FALSE
)

for (drug in drug_classes) {
  
  ts_train <- ts(train_df[[drug]], frequency = 7)
  y_hold   <- hold_df[[drug]]
  
  # auto.arima picks the best model by AIC
  fit <- auto.arima(ts_train, stepwise = FALSE, approximation = FALSE)
  
  # Forecast over holdout period and compute RMSE
  fc   <- forecast(fit, h = 365)
  rmse <- sqrt(mean((y_hold - as.numeric(fc$mean))^2))
  
  # Store result
  o <- arimaorder(fit)
  model_str <- sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[7]",
                       o["p"], o["d"], o["q"],
                       o["P"], o["D"], o["Q"])
  
  results <- rbind(results, data.frame(
    Drug         = drug,
    Best_Model   = model_str,
    Holdout_RMSE = round(rmse, 4),
    stringsAsFactors = FALSE
  ))
  
  # Simple forecast plot
  plot(fc, main = paste(drug, "—", model_str),
       xlab = "Time (weeks)", ylab = "Daily Sales")
  lines(ts(y_hold, start = end(ts_train), frequency = 7),
        col = "red", lwd = 1.5)
  legend("topleft", legend = c("Forecast", "Actual Holdout"),
         col = c("blue", "red"), lty = 1, cex = 0.8, bty = "n")
}

# ── Final Summary Table ────────────────────────────────────────
print(results)
write.csv(results, "jae_arima_results.csv", row.names = FALSE)