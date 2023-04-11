library(reshape2)
library(xts)

dataset_list <- readRDS("D:\\OneDrive - HKUST Connect\\HKUST\\Kalman\\dataset_SP500_100_batch1")
data <- dataset_list[[8]]
data$log_volume = log(data$Volume)
n_bin <- 26
data.train <- data['2019-01-01/2019-05-31']

y_train <- data.train[, c("log_volume","bin")]

# xts to data.frame
# data_frame <- as.data.frame(y_train)
data_frame <- data.frame(date=index(y_train), coredata(y_train))
# modify the date colume
data_frame$date <- as.Date(data_frame$date, format = "%Y%m%d")
# long to wide
dataset <- melt(data_frame, id=c("date","bin"))
dataset <- dcast(dataset, bin ~ date)
# delete the bin col
dataset <- dataset[,-1]
data_log_volume <- dataset


# new ---------------------------------------------------------------------
dataset_list <- readRDS("R_buildignore/dataset_SP100")
data <- dataset_list[[1]]
n_bin <- 26
data.train <- data['2019-01-01/2019-06-30']

y_train <- data.train[, c("Volume","bin")]

# xts to data.frame
# data_frame <- as.data.frame(y_train)
data_frame <- data.frame(date=index(y_train), coredata(y_train))
# modify the date colume
data_frame$date <- as.Date(data_frame$date, format = "%Y%m%d")
# long to wide
dataset <- melt(data_frame, id=c("date","bin"))
dataset <- dcast(dataset, bin ~ date)
# delete the bin col
dataset <- dataset[,-1]
AAPL_volume <- as.matrix(dataset)
usethis::use_data(AAPL_volume, overwrite = TRUE)
