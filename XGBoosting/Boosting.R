library(xgboost)
library(caret)
library(caTools)

#Input the data & split into Train & Test set
voice <- read.csv("voice.csv")
set.seed(1)
sample = sample.split(voice$meanfreq, SplitRatio = .75)
train = subset(voice, sample == TRUE)
test = subset(voice, sample == FALSE)

#One hot encoding for XGBoost
trainx <- sapply(train, as.numeric)
trainx[,21] <- trainx[,21] - 1
dtrain <- xgb.DMatrix(data = trainx[, -21],label = trainx[,21])

testx <- sapply(test, as.numeric)
testx[,21] <- testx[,21] - 1
dtest <- xgb.DMatrix(data = testx[, -21],label = testx[,21])

#Parameter list for XGBoost
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
               gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1, seed=1)

#Crossvalidation - find the optimal number of nrounds
#xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, 
 #                showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)


#XGBoosting Model
xgb <- xgb.train (params = params, data = dtrain, nrounds = 90, 
                   watchlist = list(val=dtest,train=dtrain), print_every_n = 10, 
                   early_stop_round = 10, maximize = F , eval_metric = "error")

#Predicting the test data
xgbpred <- predict (xgb,dtest)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

#Confusion Matrix & Accuracy
confusionMatrix(table(xgbpred, testx[,21])) 
