# The formats of forecast functions are the following.

#  =======  Teevint's Simple Forecasts rule ======== 
# Persistence Forecast rule
persist_fc_rule = function(s_train, s_holdout) {
  # 1. RMSE Math
  persist_mse = 0
  persist_fc = s_train[length(s_train)] 
  persist_yt = s_holdout[1] 
  persist_fc_err = persist_yt - persist_fc 
  persist_mse = persist_mse + persist_fc_err^2 
  persist_fcvec = numeric(length(s_holdout))
  
  for (i in 2:length(s_holdout)) {
    persist_yt = s_holdout[i] 
    persist_fc = s_holdout[i - 1] 
    persist_fcvec[i] = persist_fc
    persist_fc_err = persist_yt - persist_fc
    persist_mse = persist_mse + persist_fc_err^2
  }
  persist_rmse = sqrt(persist_mse / length(s_holdout))
  
  # 2. MAPE Math
  valid_idx = which(s_holdout > 0)
  persist_forecasts = c(s_train[length(s_train)], s_holdout[-length(s_holdout)])
  persist_mape = mean(abs((s_holdout[valid_idx] - persist_forecasts[valid_idx]) / s_holdout[valid_idx])) * 100
  
  # 3. Return both as a list
  return(list(rmse = persist_rmse, mape = persist_mape))
}


# Average Forecast rule
avg_fc_rule = function(s_train, s_holdout) {
  avg = mean(s_train) 
  avg_mse = 0
  
  for (i in 1:length(s_holdout)) {
    avg_yt = s_holdout[i]
    avg_fc = avg
    avg_fc_err = avg_yt - avg_fc
    avg_mse = avg_mse + avg_fc_err^2
  }
  avg_rmse = sqrt(avg_mse / length(s_holdout))
  
  # MAPE Math
  valid_idx = which(s_holdout > 0)
  avg_mape = mean(abs((s_holdout[valid_idx] - avg) / s_holdout[valid_idx])) * 100

  return(list(rmse = avg_rmse, mape = avg_mape))
}


# ======= Sohbat's Smoothing and Seasonal Functions =========
# simple exponential smoothing
# esm_fc = function(train,holdout,alpha,level,iprint)
esm_fc = function(train, holdout, alpha, level, iprint=F) {
    fcvec <- rep(0.0, length(holdout))
    sse <- 0.0
    fc <- level
    yt <- holdout[1]
    newfc <- fc
    fcvec[1] <- fc
    fcerror <- yt-fc
    sse <- sse + fcerror^2
    for (i in 2:length(holdout)) {
        newfc <- alpha * holdout[i - 1] + (1 - alpha) * newfc
        fcvec[i] <- newfc
        yt <- holdout[i]
        fcerror <- yt - newfc
        sse <- sse + fcerror^2
    }
    
    rmse <- sqrt(sse/length(holdout))
    
    if (iprint) print(fcvec)
    
    return(list(fc = fcvec, rmse = rmse))
}

# Holt linear
# lholt_fc = function(train,holdout,alpha,beta,level,slope,iprint)
lholt_fc = function(train, holdout, alpha, beta, level, slope, iprint=F) {
    fcvec <- rep(0.0, length(holdout))
    sse <- 0.0
    fc <- level + slope
    yt <- holdout[1]
    newfc <- fc
    fcvec[1] <- fc
    fcerror <- yt-fc
    sse <- sse + fcerror^2
    ellprev <- level
    bprev <- slope
    for (i in 2:length(holdout)) {
        ellnew <- alpha * holdout[i - 1] + (1 - alpha) * (ellprev + bprev)
        bnew <- beta * (ellnew - ellprev) + (1 - beta) * bprev
        
        fc <- ellnew + bnew
        fcvec[i] <- fc
        yt <- holdout[i]
        fcerror <- yt - fc
        sse <- sse + fcerror^2
        ellprev <- ellnew
        bprev <- bnew
    }
    
    rmse <- sqrt(sse/length(holdout))
    
    if (iprint) print(fcvec)
    
    return(list(fc = fcvec, rmse = rmse))
}

# Holt-Winters
# Winters additive seasonal
# aseason_fc = function(train,holdout,alpha,beta,gamma,level,slope,season,iprint)
aseason_fc = function(
    train, holdout, alpha, beta, gamma, level, slope, season, d, iprint=F
) {
    fcvec <- rep(0.0, length(holdout))
    sse <- 0.0
    fc <- level + slope + season[1]
    yt <- holdout[1]
    newfc <- fc
    fcvec[1] <- fc
    fcerror <- yt-fc
    sse <- sse + fcerror^2
    ellprev <- level
    bprev <- slope
    for (i in 2:length(holdout)) {
        # season idx
        prev_idx <- ifelse((i-1) %% d == 0, d, (i-1) %% d)
        curr_idx <- ifelse(i %% d == 0, d, i %% d)
        
        ellnew <- alpha * (holdout[i - 1] - season[prev_idx]) + (1 - alpha) * (ellprev + bprev)
        bnew <- beta * (ellnew - ellprev) + (1 - beta) * bprev
        snew <- gamma * (holdout[i-1] - ellnew) + (1 - gamma) * season[prev_idx]
        
        fc <- ellnew + bnew + season[curr_idx]
        fcvec[i] <- fc
        yt <- holdout[i]
        fcerror <- yt - fc
        sse <- sse + fcerror^2
        ellprev <- ellnew
        bprev <- bnew
        season[prev_idx] <- snew
    }
    
    rmse <- sqrt(sse/length(holdout))
    
    if (iprint) print(fcvec)
    
    return(list(fc = fcvec, rmse = rmse))
}

# Winters multiplicative seasonal
# mseason_fc = function(train,holdout,alpha,beta,gamma,level,slope,season,iprint)
mseason_fc = function(
    train, holdout, alpha, beta, gamma, level, slope, season, d, iprint=F
) {
    fcvec <- rep(0.0, length(holdout))
    sse <- 0.0
    fc <- (level + slope) * season[1]
    yt <- holdout[1]
    newfc <- fc
    fcvec[1] <- fc
    fcerror <- yt-fc
    sse <- sse + fcerror^2
    ellprev <- level
    bprev <- slope
    for (i in 2:length(holdout)) {
        # season idx
        prev_idx <- ifelse((i-1) %% d == 0, d, (i-1) %% d)
        curr_idx <- ifelse(i %% d == 0, d, i %% d)
        
        ellnew <- alpha * (holdout[i - 1] / season[prev_idx]) + (1 - alpha) * (ellprev + bprev)
        bnew <- beta * (ellnew - ellprev) + (1 - beta) * bprev
        snew <- gamma * (holdout[i-1] / ellnew) + (1 - gamma) * season[prev_idx]
        
        fc <- (ellnew + bnew) * season[curr_idx]
        fcvec[i] <- fc
        yt <- holdout[i]
        fcerror <- yt - fc
        sse <- sse + fcerror^2
        ellprev <- ellnew
        bprev <- bnew
        season[prev_idx] <- snew
    }
    
    rmse <- sqrt(sse/length(holdout))
    
    if (iprint) print(fcvec)
    
    return(list(fc = fcvec, rmse = rmse))
}