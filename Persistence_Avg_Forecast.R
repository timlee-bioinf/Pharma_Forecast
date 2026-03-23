s = read.csv("data/salesdaily.csv",header=T)
t = nrow(s)
ntrain = round(t * 0.80)

targets = c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", "N05C", "R03", "R06")

for (col_name in targets) {
  
  print(paste("--- ", col_name, " Summary ---"))
  print(summary(s[[col_name]]))

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
  print(paste(col_name, "- Persistence RMSE:", round(persist_rmse, 3)))
  
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
  print(paste(col_name, "- Average RMSE:", round(avg_rmse, 3)))
  
  cat("\n")
}