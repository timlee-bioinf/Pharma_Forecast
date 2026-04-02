# ============================================================
# STAT 443 — Jae Shin | ARIMA Analysis | All 8 Drug Classes
# Monthly data via salesmonthly.csv | 80/20 train/holdout split
# ============================================================
library(forecast)

# ── 1. LOAD DATA, SPLITS & TEAM FORECAST FUNCTIONS ───────────
source('train_holdout.R', chdir = TRUE)
source('forecast_fn.R',   chdir = TRUE)

# ── 2. ARMA_FC: FREEZE-COEFFICIENT HOLDOUT FORECAST ──────────
# Freezes training coefficients, returns RMSE, MAPE, fc vector
arma_fc <- function(tsdata, ntrain, order, seasonal, method, traincoef) {
  d <- order[2]
  D <- seasonal$order[2]
  use_mean <- (d + D == 0)

  coef_fixed <- traincoef
  if (!is.null(names(coef_fixed)) && !use_mean) {
    coef_fixed <- coef_fixed[!(names(coef_fixed) %in% c("intercept", "mean", "xmean", "drift"))]
  }

  obj <- arima(
    tsdata,
    order = order,
    seasonal = seasonal,
    fixed = unname(coef_fixed),
    method = method,
    include.mean = use_mean,
    transform.pars = FALSE
  )

  fc <- tsdata - obj$residuals
  hold <- tsdata[(ntrain + 1):length(tsdata)]
  fc_h <- fc[(ntrain + 1):length(tsdata)]
  rmse <- sqrt(mean((hold - fc_h)^2))
  idx <- which(hold > 0)
  mape <- mean(abs((hold[idx] - fc_h[idx]) / hold[idx])) * 100

  list(rmse = rmse, mape = mape, fc = as.numeric(fc_h), resid = obj$residuals)
}

# ── 3. MAP DRUG CLASSES TO PRE-SPLIT ts OBJECTS ──────────────
# ts_sales_* = full series (needed by arma_fc to freeze coefs)
# ts_train_* = 80% training | ts_holdout_* = 20% holdout
drugs <- c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", "N05C", "R03", "R06")

full <- list(M01AB=ts_sales_m01ab, M01AE=ts_sales_m01ae,
             N02BA=ts_sales_n02ba, N02BE=ts_sales_n02be,
             N05B=ts_sales_n05b,   N05C=ts_sales_n05c,
             R03=ts_sales_r03,     R06=ts_sales_r06)

tr   <- list(M01AB=ts_train_m01ab, M01AE=ts_train_m01ae,
             N02BA=ts_train_n02ba, N02BE=ts_train_n02be,
             N05B=ts_train_n05b,   N05C=ts_train_n05c,
             R03=ts_train_r03,     R06=ts_train_r06)

ho   <- list(M01AB=ts_holdout_m01ab, M01AE=ts_holdout_m01ae,
             N02BA=ts_holdout_n02ba, N02BE=ts_holdout_n02be,
             N05B=ts_holdout_n05b,   N05C=ts_holdout_n05c,
             R03=ts_holdout_r03,     R06=ts_holdout_r06)

ntrain <- length(tr[["M01AB"]])
cat("Train:", ntrain, "| Holdout:", length(ho[["M01AB"]]), "\n")

# ── 4. RESULTS COLLECTOR ─────────────────────────────────────
results <- data.frame(
  Drug=character(), Auto_Model=character(), Best_Model=character(),
  Auto_AIC=numeric(), Best_AIC=numeric(),
  HW_RMSE=numeric(), Auto_RMSE=numeric(), Best_RMSE=numeric(),
  Auto_MAPE=numeric(), Best_MAPE=numeric(),
  stringsAsFactors=FALSE)

# ── 5. HELPER FUNCTIONS ───────────────────────────────────────
mk_ord  <- function(m) m$arma[c(1, 6, 2)]
mk_seas <- function(m) list(order = m$arma[c(3, 7, 4)], period = 12)
mk_str  <- function(o, s) sprintf("ARIMA(%d,%d,%d)(%d,%d,%d)[12]",
                                  o[1], o[2], o[3],
                                  s$order[1], s$order[2], s$order[3])

# ── 6. MAIN LOOP ──────────────────────────────────────────────
for (drug in drugs) {

  cat("\n\n====", drug, "====\n")

  drug_ts  <- full[[drug]]
  train_ts <- tr[[drug]]
  hold_ts  <- ho[[drug]]
  h        <- length(hold_ts)

  # 6a. Training data + ACF/PACF
  par(mfrow = c(3, 1))
  plot(train_ts, xlab="Year", ylab="Sales",
       main=paste(drug, "- Training (80%)"))
  acf(train_ts,  lag.max=36, main=paste("ACF -",  drug))
  pacf(train_ts, lag.max=36, main=paste("PACF -", drug))
  par(mfrow = c(1, 1))

  # 6b. auto.arima: stationary=TRUE vs FALSE, keep lower AIC
  fit_stat <- suppressWarnings(auto.arima(train_ts, seasonal=TRUE, stationary=TRUE))
  fit_free <- suppressWarnings(auto.arima(train_ts, seasonal=TRUE, stationary=FALSE))
  auto_fit <- if (AIC(fit_stat) <= AIC(fit_free)) fit_stat else fit_free
  cat("auto.arima selected:", mk_str(mk_ord(auto_fit), mk_seas(auto_fit)), "\n")

  # 6c. Residual ACF/PACF
  par(mfrow = c(2, 1))
  acf(auto_fit$residuals,  lag.max=36, main=paste("Residual ACF -",  drug))
  pacf(auto_fit$residuals, lag.max=36, main=paste("Residual PACF -", drug))
  par(mfrow = c(1, 1))

  # 6d. Extract orders for model grid
  p <- auto_fit$arma[1]; q <- auto_fit$arma[2]
  P <- auto_fit$arma[3]; Q <- auto_fit$arma[4]
  d <- auto_fit$arma[6]; D <- auto_fit$arma[7]

  # 6e. 13-model AIC comparison (d and D fixed from auto.arima)
  fit_grid <- function(ord, seas)
    suppressWarnings(                          # suppresses NaN in log(s2) for
      arima(train_ts,                          # unstable candidate models
            order    = ord,
            seasonal = list(order=seas, period=12),
            method   = "ML"))

  mods <- list(
    auto = fit_grid(c(p,d,q), c(P,D,Q)),
    m0   = fit_grid(c(0,d,0), c(P,D,Q)),
    m_p0 = fit_grid(c(p,d,0), c(P,D,Q)),
    m_q0 = fit_grid(c(0,d,q), c(P,D,Q)),
    m_q1 = fit_grid(c(p,d,1), c(P,D,Q)),
    m_P1 = fit_grid(c(p,d,q), c(1,D,Q)),
    m_Q1 = fit_grid(c(p,d,q), c(P,D,1)),
    mPQ1 = fit_grid(c(p,d,q), c(1,D,1)),
    mpq1 = fit_grid(c(p,d,1), c(1,D,Q)),
    m_p1 = fit_grid(c(1,d,q), c(P,D,Q)),
    m_11 = fit_grid(c(1,d,1), c(P,D,Q)),
    m_P2 = fit_grid(c(p,d,q), c(2,D,Q)),
    m_p2 = fit_grid(c(2,d,q), c(P,D,Q))
  ) # FIX 2: was missing closing parenthesis for list()

  aic_vals <- sapply(mods, AIC)
  best_fit <- mods[[which.min(aic_vals)]]
  cat("\n--- AIC Table (sorted) ---\n"); print(sort(round(aic_vals, 3)))
  cat("Best:", names(which.min(aic_vals)),
      "| AIC =", round(min(aic_vals), 3), "\n")

  # 6f. HoltWinters 1-step rolling benchmark (with NaN guard)
  hw_preds <- numeric(h)
  cur <- train_ts
  for (i in seq_len(h)) {
    hw_preds[i] <- tryCatch(
      as.numeric(predict(HoltWinters(cur, seasonal="additive"), n.ahead=1)),
      error = function(e) NA_real_)
    cur <- ts(c(cur, hold_ts[i]), frequency=12, start=start(train_ts))
  } # FIX 3: was missing closing brace for inner for-loop

  hw_preds[!is.finite(hw_preds)] <- NA
  hw_rmse <- sqrt(mean((as.numeric(hold_ts) - hw_preds)^2, na.rm=TRUE))

  # 6g. arma_fc for auto.arima and best manual model
  fc_auto <- arma_fc(drug_ts, ntrain, mk_ord(auto_fit), mk_seas(auto_fit),
                   "ML", auto_fit$coef)
fc_best <- arma_fc(drug_ts, ntrain, mk_ord(best_fit), mk_seas(best_fit),
                   "ML", best_fit$coef)

  auto_str <- mk_str(mk_ord(auto_fit), mk_seas(auto_fit))
  best_str <- mk_str(mk_ord(best_fit), mk_seas(best_fit))

  # 6h. RMSE/MAPE summary
  cat(sprintf("\n%-38s RMSE = %7.4f\n", "HoltWinters additive:", hw_rmse))
  cat(sprintf("%-38s RMSE = %7.4f  MAPE = %5.2f%%\n",
              paste0("Auto (", auto_str, "):"), fc_auto$rmse, fc_auto$mape))
  cat(sprintf("%-38s RMSE = %7.4f  MAPE = %5.2f%%\n",
              paste0("Best (", best_str, "):"), fc_best$rmse, fc_best$mape))

  # 6i. Holdout forecast plot
  t_h  <- as.numeric(time(hold_ts))
  ylim <- range(c(as.numeric(hold_ts), hw_preds, fc_auto$fc, fc_best$fc),
                na.rm=TRUE, finite=TRUE)

  # FIX 5 (plot side): fc_auto$fc and fc_best$fc are already as.numeric()
  # from arma_fc(), so lines() will not mis-dispatch through plot.ts()
  plot(hold_ts, col="black", lwd=2, ylim=ylim,
       xlab="Time", ylab="Monthly Sales",
       main=paste("Holdout Forecasts -", drug))
  lines(t_h, hw_preds,    col="blue",   lty=2, lwd=1.5)
  lines(t_h, fc_auto$fc,  col="red",    lty=2, lwd=1.5)
  lines(t_h, fc_best$fc,  col="green3", lty=2, lwd=1.5)
  legend("topright", bty="n",
         legend = c("Actual",
                    paste0("HW   (RMSE=", round(hw_rmse,       2), ")"),
                    paste0("Auto (RMSE=", round(fc_auto$rmse,  2), ")"),
                    paste0("Best (RMSE=", round(fc_best$rmse,  2), ")")),
         col = c("black","blue","red","green3"),
         lty = c(1,2,2,2), lwd = 2)

  # 6j. Store in results table
  results <- rbind(results, data.frame(
    Drug=drug, Auto_Model=auto_str, Best_Model=best_str,
    Auto_AIC=round(AIC(auto_fit),2), Best_AIC=round(min(aic_vals),2),
    HW_RMSE=round(hw_rmse,4),   Auto_RMSE=round(fc_auto$rmse,4),
    Best_RMSE=round(fc_best$rmse,4), Auto_MAPE=round(fc_auto$mape,4),
    Best_MAPE=round(fc_best$mape,4), stringsAsFactors=FALSE))

} # FIX 4: was missing closing brace for main for-loop

# ── 7. FINAL SUMMARY ─────────────────────────────────────────
cat("\n\n========== ALL DRUG CLASSES ==========\n")
print(results)

if (!dir.exists("output")) dir.create("output")
write.csv(results, "output/jae_arima_results.csv", row.names=FALSE)
cat("\nSaved to output/jae_arima_results.csv\n")