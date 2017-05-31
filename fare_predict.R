library(data.table)

train <- fread("train.csv")

test1 <- fread("test.csv")
test$fare_amount <- 0
total_data <- rbind(train,test)
summary(total_data$mta_tax)


library(lubridate)
str(total_data$pickup_date)
total_data$pickup_date <- substring(total_data$pickup_datetime,1,10)
total_data1 <- total_data

total_data$pickup_time <-
  total_data[, .(pickup_time = substring(total_data$pickup_datetime, 12, 19)),]

total_data$dropoff_date <-
  total_data[, .(dropoff_date = as.Date(substring(total_data$dropoff_datetime, 1, 10))), ]

total_data$dropoff_time <-
  total_data[, .(dropoff_time = (substring(total_data$dropoff_datetime, 12, 19))), ]

total_data$durattion <-
  total_data[, .(duration = difftime(strptime(
    total_data$pickup_datetime, "%Y-%m-%d %H:%M:%S"
  ),
  strptime(total_data$dropoff_datetime, "%Y-%m-%d %H:%M:%S"),units = "mins")),]

total_data$pickup_date <- as.Date(total_data$pickup_date)
total_data$DOW <- total_data[,.(dow = wday(total_data$pickup_date,label = T)),]
str(total_data$TOD)

total_data$TOD <- total_data[,.(tod = hour(hms(total_data$pickup_time))),]
total_data$pickup_datetime <- NULL
total_data$dropoff_datetime <- NULL

library(lubridate)
  total_data[, month := month(total_data$pickup_date, label = T), ]
total_data$pickup_date <- NULL
total_data$pickup_time <- NULL
total_data$dropoff_time <- NULL
test <- total_data[total_data$TID %in% test1$TID,]
trainf <- total_data[!(total_data$TID %in% test1$TID),]

trainf$new_user <- as.factor(trainf$new_user)
trainf$store_and_fwd_flag <- as.factor(trainf$store_and_fwd_flag)
trainf$payment_type <- as.factor(trainf$payment_type)
trainf$DOW <- as.factor(trainf$DOW)
trainf$rate_code <- as.factor(trainf$rate_code)

summary(trainf)
#dealing with missing values
trainf$surcharge[is.na(trainf$surcharge)] <- mean(trainf$surcharge,na.rm = T)
trainf$tip_amount[is.na(trainf$tip_amount)] <- mean(trainf$tip_amount,na.rm = T)
trainf$tolls_amount[trainf$tolls_amount < 0] <- mean(subset(trainf$tolls_amount,trainf$tolls_amount >= 0))
trainf$pickup_longitude[is.na(trainf$pickup_longitude)] <- mean(trainf$pickup_longitude,na.rm = T)
trainf$pickup_latitude[is.na(trainf$pickup_latitude)] <- mean(trainf$pickup_latitude,na.rm = T)
trainf$mta_tax[trainf$mta_tax < 0] <- mean(subset(trainf$mta_tax,trainf$mta_tax >= 0))

trainf$dropoff_longitude[is.na(trainf$dropoff_longitude)] <- mean(trainf$dropoff_longitude,na.rm = T)
trainf$dropoff_latitude[is.na(trainf$dropoff_latitude)] <- mean(trainf$dropoff_latitude,na.rm = T)

summary(trainf$mta_tax)

sum(is.na(test))

test$surcharge[is.na(test$surcharge)] <- mean(test$surcharge,na.rm = T)
test$tip_amount[is.na(test$tip_amount)] <- mean(test$tip_amount,na.rm = T)
test$tolls_amount[test$tolls_amount < 0] <- mean(subset(test$tolls_amount,test$tolls_amount > 0))
test$pickup_longitude[is.na(test$pickup_longitude)] <- mean(test$pickup_longitude,na.rm = T)
test$pickup_latitude[is.na(test$pickup_latitude)] <- mean(test$pickup_latitude,na.rm = T)
test$mta_tax[test$mta_tax < 0] <- mean(subset(test$mta_tax,test$mta_tax >= 0))

test$dropoff_longitude[is.na(test$dropoff_longitude)] <- mean(test$dropoff_longitude,na.rm = T)
test$dropoff_latitude[is.na(test$dropoff_latitude)] <- mean(test$dropoff_latitude,na.rm = T)


trainf_1 <- na.omit(trainf)
test_2 <- na.omit(test)

library(geosphere)
summary(trainf_1)
trainf_1$new_user <- as.factor(trainf_1$new_user)
trainf_1$store_and_fwd_flag <- as.factor(trainf_1$store_and_fwd_flag)
trainf_1$payment_type <- as.factor(trainf_1$payment_type)
trainf_1$DOW <- as.factor(trainf_1$DOW)
trainf_1$rate_code <- as.factor(trainf_1$rate_code)

test_2$new_user <- as.factor(test_2$new_user)
test_2$store_and_fwd_flag <- as.factor(test_2$store_and_fwd_flag)
test_2$payment_type <- as.factor(test_2$payment_type)
test_2$DOW <- as.factor(test_2$DOW)
test_2$rate_code <- as.factor(test_2$rate_code)

x = data.frame(long = trainf_1$pickup_longitude, lat = trainf_1$pickup_latitude)
y = data.frame(long = trainf_1$dropoff_longitude,lat = trainf_1$dropoff_latitude)
trainf_1$distance <-
  trainf_1[ ,.(distance = distHaversine(p1 = x,p2 = y)),]
test_2$dropoff_latitude[test_2$dropoff_latitude > 90] <- mean(test_2$dropoff_latitude)
test_2$distance <-
  test_2[, .(distance = distHaversine(
    p1 = data.frame(
      long = test_2$pickup_longitude,
      lat = test_2$pickup_latitude
    ),
    p2 = data.frame(
      long = test_2$dropoff_longitude,
      lat = test_2$dropoff_latitude
    )
  ))]
trainf_1[,vendo_count := .N,by = vendor_id]
trainf_1[,date_count := .N,by = dropoff_date]

test_2[,vendo_count := .N,by = vendor_id]
test_2[,date_count := .N,by = dropoff_date]


library(caret)
set.seed(123)
intrain <- createDataPartition(trainf_1$fare_amount,p= 0.5,list = F)

train_ <- trainf_1[intrain ,]
validation1 <- trainf_1[-intrain,]
train_$month <- as.factor(as.character(train_$month))
validation1$month <- as.factor(as.character(validation1$month))
test_2$month <- as.factor(as.character(test_2$month))
str(train_)


test_2$new_user <- as.factor(test_2$new_user)
test_2$store_and_fwd_flag <- as.factor(test_2$store_and_fwd_flag)
test_2$payment_type <- as.factor(test_2$payment_type)
test_2$DOW <- as.factor(test_2$DOW)
test_2$rate_code <- as.factor(test_2$rate_code)
test_2$month <- as.factor(test_2$month)
test_2$dropoff_date <- NULL
train_$dropoff_date <- NULL
validation1$dropoff_date <- NULL

library(randomForest)
library(h2o)
localH2O <- h2o.init(nthreads = -1)


              train_h2o <- as.h2o(train_)
 validationh2o <- as.h2o(validation1)
test_2_h2o <- as.h2o(test_2)



y_dep <- 16
x_indep <- c(4:7,10,11,14,15,17:19,21,22,23)

#deeplearning

deep_h20 <- h2o.deeplearning(y= y_dep,x = x_indep,training_frame = train_h2o, epoch = 60,
                           hidden = c(100,100),
                           activation = "Rectifier")
predict.deep <- as.data.frame(h2o.predict(deep_h20, validationh2o))
predict.deep_fin <- as.data.frame(h2o.predict(deep_h20, test_2_h2o))
error_deep <- predict.deep$predict - validation1$fare_amount
mae(error_deep)

#random_Forest

rd_mod <-
  h2o.randomForest(
    y = y_dep,
    x = x_indep,
    training_frame = train_h2o,
    min_rows = 10,
    stopping_metric = "MAE"
    
  )
predict.rforest <- as.data.frame(h2o.predict(rd_mod, validationh2o))
predict.rforest_fin <-
  as.data.frame(h2o.predict(rd_mod, test_2_h2o))
error_RF <- predict.rforest$predict - validation1$fare_amount
mae(error_RF)
pred.rf_val <- data.frame(TID = validation1$TID,	fare_amount = predict.rforest$predict
)
pred.rf <- data.frame(TID = test_2$TID,	fare_amount = predict.rforest_fin$predict
)

#gbm
grid.rf <-
  h2o.grid(
    "gbm",
    x = x_indep,
    y = y_dep,
    training_frame = train_h2o,
    
    hyper_params = list(ntrees = c(50, 100, 150), max_depth = c(7,8,9))
  )


gbm_h2o <-
  h2o.gbm(
    y = y_dep,
    x = x_indep,
    training_frame = train_h2o,
    nfolds = 5,
    stopping_metric = "MAE",
    ntrees = 150,max_depth = 9,stopping_rounds = 10
  )
predict.gbm <- as.data.frame(h2o.predict(gbm_h2o, validationh2o))
predict.gbm_fin <- as.data.frame(h2o.predict(gbm_h2o, test_2_h2o))
error_gbm <- predict.gbm$predict - validation1$fare_amount
mae(error_gbm)

pred.gbm_val <- data.frame(TID = validation1$TID,	fare_amount = predict.gbm$predict
)
pred.gbm <- data.frame(TID = test_2$TID,	fare_amount = predict.gbm_fin$predict
)

#ensemble

ensemble_data <-
  data.frame(
    fare_rf = pred.rf_val$fare_amount,
    fare_gbm = pred.gbm_val$fare_amount,
    fare_deep = predict.deep$predict,
    actual_fare = validation1$fare_amount)

ensemble_test <-
  data.frame(
    fare_rf = pred.rf$fare_amount,
    fare_gbm = pred.gbm$fare_amount,
     fare_deep = predict.deep_fin$predict,
    TID = test_2$TID
  )
library(gbm)
final_mod <-
  gbm(actual_fare ~ fare_rf + fare_gbm+fare_deep , data = ensemble_data)
final_pred <- predict(final_mod, newdata = ensemble_test)

pred.fin <- data.frame(TID = test_2$TID,	fare_amount = final_pred
)
fwrite(pred.fin,"prediction_fin_1.csv",col.names = T,sep = ",")


# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
