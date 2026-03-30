source('data.R', chdir=TRUE)
source('forecast_fn.R', chdir=TRUE)

t = nrow(salesmonthly)
ntrain = floor(t * 0.80)

targets = c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", "N05C", "R03", "R06")

results_table = data.frame(
  Drug_Code = character(),
  Avg_Monthly_Sales = numeric(),
  Persistence_RMSE = numeric(),
  Average_RMSE = numeric(),
  Persistence_MAPE_pct = numeric(),
  Average_MAPE_pct = numeric()
)

for (col_name in targets) {
  
  s_train = salesmonthly[[col_name]][1:ntrain]
  s_holdout = salesmonthly[[col_name]][(ntrain+1):t]
  
  persist_results = persist_fc_rule(s_train, s_holdout)
  avg_results = avg_fc_rule(s_train, s_holdout)
  
  # Add the results to the table row
  new_row = data.frame(
    Drug_Code = col_name,
    Avg_Monthly_Sales = round(mean(s_holdout), 1),
    Persistence_RMSE = round(persist_results$rmse, 3),
    Average_RMSE = round(avg_results$rmse, 3),
    Persistence_MAPE_pct = round(persist_results$mape, 1),
    Average_MAPE_pct = round(avg_results$mape, 1)
  )
  results_table = rbind(results_table, new_row)
} 

results_table$Delta_Persist_MAPE_Pct = results_table$Average_MAPE_pct - results_table$Persistence_MAPE_pct

print(results_table)

write.csv(results_table, file = "output/simple_FC_results_table.csv", row.names = FALSE)