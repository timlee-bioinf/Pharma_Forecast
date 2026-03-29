# get train and holdout sets
source('train_holdout.R', chdir = TRUE)

# get forecast functions
source('forecast_fn.R', chdir = TRUE)

# choose train and holdout sets for a single column (e.g., M01AB)
z = ts_train_m01ab
z_holdout = ts_holdout_m01ab

# --------------------------------------------------------------------------------------
# 1. Simple Exponential Smoothing (SES): For no trend/seasonality
# --------------------------------------------------------------------------------------
fit_expsmo = HoltWinters(z, beta=F, gamma=F)
print(fit_expsmo)

# forecast with SES
esmfc = esm_fc(z, z_holdout, alpha=fit_expsmo$alpha, level=fit_expsmo$coefficients[1], iprint=F)

# --------------------------------------------------------------------------------------
# 2. Holt’s Linear Trend: Adds trend
# --------------------------------------------------------------------------------------
# fit Holt’s Linear Trend model
fit_hw = HoltWinters(z, gamma=F)
print(fit_hw)

# forecast with Holt’s Linear Trend
holtfc = lholt_fc(
    z, z_holdout, alpha=fit_hw$alpha, beta=fit_hw$beta, level=fit_hw$coefficients[1], 
    slope=fit_hw$coefficients[2], iprint=F
)
# --------------------------------------------------------------------------------------
# 3. Holt-Winters: Adds seasonality
# --------------------------------------------------------------------------------------
# Fit additive seasonality
wafit = HoltWinters(z, seasonal="additive")
print(wafit)

# extract parameters for additive seasonality model
add_alph = wafit$alpha; add_bet = wafit$beta; add_gamm = wafit$gamma
add_leve = wafit$coef[1]; add_slop = wafit$coef[2]; add_seaso = wafit$coef[3:14]
add_d = length(add_seaso)

# forecast with additive seasonality
aseason = aseason_fc(z, z_holdout, add_alph, add_bet, add_gamm, add_leve, add_slop, add_seaso, add_d, iprint=F)

# --------------------------------------------------------------------------------------
# Fit multiplicative seasonality
wmfit = HoltWinters(z, seasonal="multiplicative")
print(wmfit)

# extract parameters for multiplicative seasonality model
mult_alph = wmfit$alpha; mult_bet = wmfit$beta; mult_gamm = wmfit$gamma
mult_leve = wmfit$coef[1]; mult_slop = wmfit$coef[2]; mult_seaso = wmfit$coef[3:14]
mult_d = length(mult_seaso)
# forecast with multiplicative seasonality
mseason = mseason_fc(z, z_holdout, mult_alph, mult_bet, mult_gamm, mult_leve, mult_slop, mult_seaso, mult_d, iprint=F)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# get forecasted values and RMSE for each model
out = cbind(holdout$datum / 100, z_holdout, esmfc$fc, holtfc$fc, aseason$fc, mseason$fc)
colnames(out) = c("yearmon", "holdout", "ses", "holt", "add_seasonal", "mult_seasonal")
print(round(out[1:12,],2))

rmse_vec = cbind(esmfc$rmse, holtfc$rmse, aseason$rmse, mseason$rmse)
colnames(rmse_vec) = c("SES", "Holt", "Additive Seasonal", "Multiplicative Seasonal")
print(round(rmse_vec, 2))