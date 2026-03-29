# import libraries
library(readr)

# import data
salesmonthly <- read_csv("data/salesmonthly.csv")
spec(salesmonthly)

# get time series
ts_sales_m01ab = ts(salesmonthly$M01AB, start = c(2014, 1), frequency = 12)
ts_sales_m01ae = ts(salesmonthly$M01AE, start = c(2014, 1), frequency = 12)
ts_sales_n02ba = ts(salesmonthly$N02BA, start = c(2014, 1), frequency = 12)
ts_sales_n02be = ts(salesmonthly$N02BE, start = c(2014, 1), frequency = 12)
ts_sales_n05b = ts(salesmonthly$N05B, start = c(2014, 1), frequency = 12)
ts_sales_n05c = ts(salesmonthly$N05C, start = c(2014, 1), frequency = 12)
ts_sales_r03 = ts(salesmonthly$R03, start = c(2014, 1), frequency = 12)
ts_sales_r06 = ts(salesmonthly$R06, start = c(2014, 1), frequency = 12)