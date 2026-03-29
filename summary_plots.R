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

# acf plots
acf(ts_sales_m01ab)
acf(ts_sales_m01ae)
acf(ts_sales_n02ba)
acf(ts_sales_n02be)
acf(ts_sales_n05b)
acf(ts_sales_n05c)
acf(ts_sales_r03)
acf(ts_sales_r06)

# pacf plots 
pacf(ts_sales_m01ab)
pacf(ts_sales_m01ae)
pacf(ts_sales_n02ba)
pacf(ts_sales_n02be)
pacf(ts_sales_n05b)
pacf(ts_sales_n05c)
pacf(ts_sales_r03)
pacf(ts_sales_r06)