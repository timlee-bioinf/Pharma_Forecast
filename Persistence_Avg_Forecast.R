s = read.csv("data/salesdaily.csv",header=T)
t = nrow(s)
ntrain = round(t * 0.80)

targets = c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", "N05C", "R03", "R06")

results_table = data.frame(
  Drug_Code = character(),
  Avg_Daily_Sales = numeric(),
  Persistence_RMSE = numeric(),
  Average_RMSE = numeric(),
  Persistence_MAPE_pct = numeric(),
  Average_MAPE_pct = numeric()
)

for (col_name in targets) {
  
  s_train = s[[col_name]][1:ntrain]
  s_holdout = s[[col_name]][(ntrain+1):t]
  
  #==persistence==
  persist_mse = 0
  persist_fc = s_train[length(s_train)] 
  persist_yt = s_holdout[1] 
  persist_fc_err = persist_yt - persist_fc 
  persist_mse = persist_mse + persist_fc_err^2 
  persist_fcvec = numeric(length(s_holdout))
  # This is for forecasting the rest of the observations in holdout
  for (i in 2:length(s_holdout)) {
  persist_yt = s_holdout[i] 
  persist_fc = s_holdout[i - 1] 
  persist_fcvec[i] = persist_fc
  persist_fc_err = persist_yt - persist_fc
  persist_mse = persist_mse + persist_fc_err^2
  }
  persist_rmse = sqrt(persist_mse / length(s_holdout))
  
  #==average==
  avg = mean(s_train) 
  avg_mse = 0
  for (i in 1:length(s_holdout)) {
    avg_yt = s_holdout[i]
    avg_fc = avg
    avg_fc_err = avg_yt - avg_fc
    avg_mse = avg_mse + avg_fc_err^2
  }
  avg_rmse = sqrt(avg_mse / length(s_holdout))
  
  # Gather Metrics
  avg_volume = mean(s_holdout)
  
  valid_idx = which(s_holdout > 0)
  persist_forecasts = c(s_train[length(s_train)], s_holdout[-length(s_holdout)])
  
  # Calculate MAPE (multiplied by 100 to make it a readable percentage)
  persist_mape = mean(abs((s_holdout[valid_idx] - persist_forecasts[valid_idx]) / s_holdout[valid_idx])) * 100
  avg_mape = mean(abs((s_holdout[valid_idx] - avg) / s_holdout[valid_idx])) * 100
  
  # Add all the metrics as a new row
  new_row = data.frame(
    Drug_Code = col_name,
    Avg_Daily_Sales = round(avg_volume, 1),
    Persistence_RMSE = round(persist_rmse, 3),
    Average_RMSE = round(avg_rmse, 3),
    Persistence_MAPE_pct = round(persist_mape, 1),
    Average_MAPE_pct = round(avg_mape, 1)
  )
  results_table = rbind(results_table, new_row)
} 

print(results_table)