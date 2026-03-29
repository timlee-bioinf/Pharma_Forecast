# get data
source("data.R", chdir = TRUE)

# holdout set: last 20% of the data
data_size <- nrow(salesmonthly)
train_size <- floor(0.8 * data_size)
train <- salesmonthly[1:train_size, ]
holdout <- salesmonthly[(train_size + 1):data_size, ]