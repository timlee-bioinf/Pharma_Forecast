# get data
source("data.R", chdir = TRUE)

# holdout set: last 20% of the data
data_size <- nrow(salesmonthly)
train_size <- floor(0.8 * data_size)
train <- salesmonthly[1:train_size, ]
holdout <- salesmonthly[(train_size + 1):data_size, ]

# get end date of train set
train_end_date <- as.Date(train$datum[train_size])
print(paste("Train set end date:", train_end_date))

# get time series for train and holdout sets using window function
end_date <- c(as.numeric(format(train_end_date, "%Y")), as.numeric(format(train_end_date, "%m")))

# train sets
ts_train_m01ab <- window(ts_sales_m01ab, end = end_date)
ts_train_m01ae <- window(ts_sales_m01ae, end = end_date)
ts_train_n02ba <- window(ts_sales_n02ba, end = end_date)
ts_train_n02be <- window(ts_sales_n02be, end = end_date)
ts_train_n05b <- window(ts_sales_n05b, end = end_date)
ts_train_n05c <- window(ts_sales_n05c, end = end_date)
ts_train_r03 <- window(ts_sales_r03, end = end_date)
ts_train_r06 <- window(ts_sales_r06, end = end_date)

# holdout sets
ts_holdout_m01ab <- window(ts_sales_m01ab, start = c(end_date[1], end_date[2] + 1))
ts_holdout_m01ae <- window(ts_sales_m01ae, start = c(end_date[1], end_date[2] + 1))
ts_holdout_n02ba <- window(ts_sales_n02ba, start = c(end_date[1], end_date[2] + 1))
ts_holdout_n02be <- window(ts_sales_n02be, start = c(end_date[1], end_date[2] + 1))
ts_holdout_n05b <- window(ts_sales_n05b, start = c(end_date[1], end_date[2] + 1))
ts_holdout_n05c <- window(ts_sales_n05c, start = c(end_date[1], end_date[2] + 1))
ts_holdout_r03 <- window(ts_sales_r03, start = c(end_date[1], end_date[2] + 1))
ts_holdout_r06 <- window(ts_sales_r06, start = c(end_date[1], end_date[2] + 1))