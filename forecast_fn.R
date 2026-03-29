# The formats of forecast functions are the following.

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