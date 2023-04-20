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

bin_names <- c("09:30 AM", "09:45 AM", "10:00 AM", "10:15 AM", "10:30 AM", 
               "10:45 AM", "11:00 AM", "11:15 AM", "11:30 AM", "11:45 AM", 
               "12:00 PM", "12:15 PM", "12:30 PM", "12:45 PM", "01:00 PM", 
               "01:15 PM", "01:30 PM", "01:45 PM", "02:00 PM", "02:15 PM", 
               "02:30 PM", "02:45 PM", "03:00 PM", "03:15 PM", "03:30 PM", "03:45 PM")
rownames(AAPL_volume) <- bin_names
usethis::use_data(AAPL_volume, overwrite = TRUE)
