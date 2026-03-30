library(readr)

#import monthly data
salesmonthly <- read_csv("data/salesmonthly.csv")
spec(salesmonthly)

# ----------------------------------------------------------------------------------------------------
# IMPUTE MISSING DATA FOR JANUARY 2017 SALES
# ----------------------------------------------------------------------------------------------------
# import daily sales data
salesdaily <- read.csv("data/salesdaily.csv", header = TRUE)
targets <- c("M01AB", "M01AE", "N02BA", "N02BE", "N05B", "N05C", "R03", "R06")

# impute missing monthly data for January 2017 from daily sales
jan_daily <- salesdaily[salesdaily$Year == 2017 & salesdaily$Month == 1, ]
jan_sums <- colSums(jan_daily[, targets], na.rm = TRUE)

salesmonthly[salesmonthly$datum == "2017-01-31", targets] <- as.list(round(jan_sums, 3))

# get time series
ts_sales_m01ab = ts(salesmonthly$M01AB, start = c(2014, 1), frequency = 12)
ts_sales_m01ae = ts(salesmonthly$M01AE, start = c(2014, 1), frequency = 12)
ts_sales_n02ba = ts(salesmonthly$N02BA, start = c(2014, 1), frequency = 12)
ts_sales_n02be = ts(salesmonthly$N02BE, start = c(2014, 1), frequency = 12)
ts_sales_n05b = ts(salesmonthly$N05B, start = c(2014, 1), frequency = 12)
ts_sales_n05c = ts(salesmonthly$N05C, start = c(2014, 1), frequency = 12)
ts_sales_r03 = ts(salesmonthly$R03, start = c(2014, 1), frequency = 12)
ts_sales_r06 = ts(salesmonthly$R06, start = c(2014, 1), frequency = 12)
