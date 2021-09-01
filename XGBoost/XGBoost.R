rm(list=ls())
library(caret) # Machine Learning Library
library(xgboost) # XGBoost library
library(mltools)
library(data.table)
library(ggplot2)

# Getting data from UCI.
# Dataset is on adult income.
# Predicting Adult income based on a variety of factors...

col_names <- c("age",
               "workclass",
               "fnlwgt",
               "education",
               "education_num",
               "marital_status",
               "occupation",
               "relationship",
               "race",
               "sex",
               "capital_gain",
               "capital_loss",
               "hours_per_week",
               "native_country",
                "Income_Bucket")

# Not needing to locally store the dataset, let's scrape the data.
data_train <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"))
colnames(data_train) <- col_names

# Test Set
data_test <-  read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test"), header = FALSE)
data_test <- data_test[2:dim(data_test)[1],]
colnames(data_test) <- col_names

# Summary of data (checking if any NA)
summary(data_test)
summary(data_train) #eh looks fine.

# One hot encoding independent features
lab <- data_train[,15]
dummy <- dummyVars(" ~ .", data=data_train[,-15])
newdata <- data.frame(predict(dummy, newdata = data_train[,-15])) 
data_train <- cbind(newdata, lab)
colnames(data_train)[109] <- "Income_Bucket"
# remove column 82 (since test does not have native_country_holland)
data_train <- data_train[,-c(82)]
data_train$Income_Bucket <- as.factor(data_train$Income_Bucket)

# Doing the same thing for test...
data_test$age <- as.numeric(data_test$age)
lab_test <- data_test[,15]
dummy <- dummyVars(" ~ .", data=data_test[,-15])
newdata <- data.frame(predict(dummy, newdata = data_test[,-15])) 
data_test <- cbind(newdata, lab_test)
colnames(data_test)[108] <- "Income_Bucket"
data_test$Income_Bucket <- as.factor(data_test$Income_Bucket)


# Imbalanced dataset for train?
table(data_train$Income_Bucket) # Yes. Will need to resample.
train_up_sample <- upSample(x = data_train[,-108], y = data_train$Income_Bucket)
table(train_up_sample$Class)


# Doing XGBoost for classification purposes.
grid_tune <- expand.grid(
  nrounds = c(500,1000,1500), #number of trees
  max_depth = c(2,4,6),
  eta = 0.3, #c(0.025,0.05,0.1,0.3), #Learning rate
  gamma = 0, # pruning --> Should be tuned. i.e c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0)
  colsample_bytree = 1, # c(0.4, 0.6, 0.8, 1.0) subsample ratio of columns for tree
  min_child_weight = 1, # c(1,2,3) # the larger, the more conservative the model
  #is; can be used as a stop
  subsample = 1 # c(0.5, 0.75, 1.0) # used to prevent overfitting by sampling X% training
)

train_control <- trainControl(method = "cv",
                              number=3,
                              verboseIter = TRUE,
                              allowParallel = TRUE)
xgb_tune <- train(x = train_up_sample[,-108],
                  y = train_up_sample[,108],
                  trControl = train_control,
                  tuneGrid = grid_tune,
                  method= "xgbTree",
                  verbose = TRUE)
xgb_tune

# Best tune
xgb_tune$bestTune

# Writing out the best model.

train_control <- trainControl(method = "none",
                              verboseIter = TRUE,
                              allowParallel = TRUE)
final_grid <- expand.grid(nrounds = xgb_tune$bestTune$nrounds,
                           eta = xgb_tune$bestTune$eta,
                           max_depth = xgb_tune$bestTune$max_depth,
                           gamma = xgb_tune$bestTune$gamma,
                           colsample_bytree = xgb_tune$bestTune$colsample_bytree,
                           min_child_weight = xgb_tune$bestTune$min_child_weight,
                           subsample = xgb_tune$bestTune$subsample)
xgb_model <- train(x = train_up_sample[,-108],
                   y = train_up_sample[,108],
                   trControl = train_control,
                   tuneGrid = final_grid,
                   method = "xgbTree",
                   verbose = TRUE)

predict(xgb_model, data_test)

# Prediction:
xgb.pred <- predict(xgb_model, data_test)

#' Confusion Matrix
confusionMatrix(as.factor(as.numeric(xgb.pred)),
                as.factor(as.numeric(data_test$Income_Bucket)))
