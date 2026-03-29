# get data
source("data.R", chdir = TRUE)

# summary
print(paste("Number of rows:", nrow(salesmonthly)))
print(paste("Number of columns:", ncol(salesmonthly)))
print(paste("Column names:", paste(colnames(salesmonthly), collapse = ", ")))
print("Summary statistics:")
print(summary(salesmonthly))

# summaries for monthly sales by month
for (i in 1:12) {
    print(paste("Month:", i))
    print(summary(salesmonthly[format(as.Date(salesmonthly$datum), "%m") == sprintf("%02d", i), ]))
}

# summaries for monthly sales by month and column
for (col in colnames(salesmonthly)[-1]) {
    print(paste("Column:", col))
    print(tapply(salesmonthly[[col]], format(as.Date(salesmonthly$datum), "%m"), summary))
    print(tapply(salesmonthly[[col]], format(as.Date(salesmonthly$datum), "%m"), sd))
}

# combine into a single summary table
summary_table <- data.frame(
    Month = 1:12,
    M01AB_Mean = tapply(salesmonthly$M01AB, format(as.Date(salesmonthly$datum), "%m"), mean),
    M01AB_SD = tapply(salesmonthly$M01AB, format(as.Date(salesmonthly$datum), "%m"), sd),
    M01AE_Mean = tapply(salesmonthly$M01AE, format(as.Date(salesmonthly$datum), "%m"), mean),
    M01AE_SD = tapply(salesmonthly$M01AE, format(as.Date(salesmonthly$datum), "%m"), sd),
    N02BA_Mean = tapply(salesmonthly$N02BA, format(as.Date(salesmonthly$datum), "%m"), mean),
    N02BA_SD = tapply(salesmonthly$N02BA, format(as.Date(salesmonthly$datum), "%m"), sd),
    N02BE_Mean = tapply(salesmonthly$N02BE, format(as.Date(salesmonthly$datum), "%m"), mean),
    N02BE_SD = tapply(salesmonthly$N02BE, format(as.Date(salesmonthly$datum), "%m"), sd),
    N05B_Mean = tapply(salesmonthly$N05B, format(as.Date(salesmonthly$datum), "%m"), mean),
    N05B_SD = tapply(salesmonthly$N05B, format(as.Date(salesmonthly$datum), "%m"), sd),
    N05C_Mean = tapply(salesmonthly$N05C, format(as.Date(salesmonthly$datum), "%m"), mean),
    N05C_SD = tapply(salesmonthly$N05C, format(as.Date(salesmonthly$datum), "%m"), sd),
    R03_Mean = tapply(salesmonthly$R03, format(as.Date(salesmonthly$datum), "%m"), mean),
    R03_SD = tapply(salesmonthly$R03, format(as.Date(salesmonthly$datum), "%m"), sd),
    R06_Mean = tapply(salesmonthly$R06, format(as.Date(salesmonthly$datum), "%m"), mean),
    R06_SD = tapply(salesmonthly$R06, format(as.Date(salesmonthly$datum), "%m"), sd)
)
write.csv(summary_table, "output/summary_table.csv", row.names = FALSE)

# get time plots for each column
plot(ts_sales_m01ab, main = "M01AB Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_m01ae, main = "M01AE Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_n02ba, main = "N02BA Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_n02be, main = "N02BE Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_n05b, main = "N05B Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_n05c, main = "N05C Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_r03, main = "R03 Sales", xlab = "Year", ylab = "Sales")
plot(ts_sales_r06, main = "R06 Sales", xlab = "Year", ylab = "Sales")

# acf plots
acf(ts_sales_m01ab, main = "ACF of M01AB Sales")
acf(ts_sales_m01ae, main = "ACF of M01AE Sales")
acf(ts_sales_n02ba, main = "ACF of N02BA Sales")
acf(ts_sales_n02be, main = "ACF of N02BE Sales")
acf(ts_sales_n05b, main = "ACF of N05B Sales")
acf(ts_sales_n05c, main = "ACF of N05C Sales")
acf(ts_sales_r03, main = "ACF of R03 Sales")
acf(ts_sales_r06, main = "ACF of R06 Sales")

# pacf plots 
pacf(ts_sales_m01ab, main = "PACF of M01AB Sales")
pacf(ts_sales_m01ae, main = "PACF of M01AE Sales")
pacf(ts_sales_n02ba, main = "PACF of N02BA Sales")
pacf(ts_sales_n02be, main = "PACF of N02BE Sales")
pacf(ts_sales_n05b, main = "PACF of N05B Sales")
pacf(ts_sales_n05c, main = "PACF of N05C Sales")
pacf(ts_sales_r03, main = "PACF of R03 Sales")
pacf(ts_sales_r06, main = "PACF of R06 Sales")