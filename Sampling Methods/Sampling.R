rm(list=ls())
# Libraries that I use.
# install.packages("themis") # for additional algorithms (sampling)
# install.packages("DMwR") # for smote
# install.packages("caret") # for confusion matrix

# Setting directory
setwd('C:/Users/Spenc/Documents/Youtube/Machine Learning/R/Sampling')
# Setting seed
set.seed(123)
data <- read.csv('loan_default.csv')
data$loan_default <- as.factor(data$loan_default)
data$Employment.Type <- as.numeric(as.factor(data$Employment.Type))
# 1 is "", 2 is Salaried, 3 is Self Employed
# Run the data -- Checking the imbalance of the dataset.
table(data$loan_default)

## 80% of the sample size
size <- floor(0.8 * nrow(data))

train_ind <- sample(seq_len(nrow(data)), size = size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

# Taking a look at this imbalanced dataset.
table(train$loan_default)
table(test$loan_default)

# Let's run a Logistic regression (raw)
model <- glm(loan_default ~.,data = train, family = binomial(link="logit"))
pred <- predict(model, newdata = test, type = "response")

library(caret)
# Getting the confusion Matrix
c_matrix <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),
                reference = test$loan_default)
c_matrix # This is telling us that the model is predicting no fraudulent activity
# Most of the time 

# No information rate - the sum of 'positive' values / Total Values 
# You want the No information rate to be significantly less than the accuracy rate


# Accuracy: (TP + TN) / (TP +TN + FP + FN)
acc <- c_matrix$overall[1]
acc # This is quite suspicious... especially since our specificity is 
# incredibly low... (Actual neg. predicted correctly)
# Sensitivity (Recall): TP / (TP + FN) --> Should be high
rec <- c_matrix$byClass[1]
rec

# Precision = TP / (TP + FP) --> Seen as Positive Prediction value
# In this case, this is the important statistic since we are trying to predict
# fraudulent transactions...
# Precision is calculates the accuracy for the minority class.
prec <- c_matrix$byClass[3]
prec
# F Score = 2 * Recall * Precision / (Recall + Precision) -> Use this to compare
# against models. A high F score is better. Combines both the results of 
# recall and Precision together.
F_score <- 2 * rec * prec / (rec + prec) [1]
names(F_score) <- 'F Score'
F_score

# Lets attempt to get better results!
# Over Sampling
# This method involves randomly selecting from the minority class and simply 
# duplicating each observation until the number of minority class observations
# are equal to the number of majority class observations.
train_up_sample <- upSample(x = train[,-9], y = train$loan_default)
model_up <- glm(Class ~.,data = train_up_sample, family = binomial(link="logit"))
pred <- predict(model_up, newdata = test, type = "response")
c_matrix_up_sample <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),
                            reference = test$loan_default)
c_matrix_up_sample
F_score_up <- 2 * c_matrix_up_sample$byClass[1] * c_matrix_up_sample$byClass[3] /
  (c_matrix_up_sample$byClass[1] + c_matrix_up_sample$byClass[3])
names(F_score_up) <- 'F_Score_Up'
F_score_up
# Pretty bad! 

# Let's try SMOTE!
library(DMwR)
smote_train <- SMOTE(loan_default ~ ., data = train)
model_smote <- glm(loan_default ~.,data = smote_train, family = binomial(link="logit"))
pred <- predict(model_smote, newdata = test, type = "response")
c_matrix_smote <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),
                                      reference = test$loan_default)
c_matrix_smote
F_score_smote <- 2 * c_matrix_smote$byClass[1] * c_matrix_smote$byClass[3] /
  (c_matrix_smote$byClass[1] + c_matrix_smote$byClass[3])
names(F_score_smote) <- 'F_Score_Up'
F_score_smote # Better?



# Let's do down sampling techniques (undersampling)
# Like random over sampling, the random oversampling method involves sampling 
# from the overrepresented class and retrieve the same number of observations 
# as seen in the minority class
train_down_sample <- downSample(x = train[,-9], y = train$loan_default)
model_down <- glm(Class ~.,data = train_down_sample, family = binomial(link="logit"))
pred <- predict(model_down, newdata = test, type = "response")
c_matrix_down <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),
                                     reference = test$loan_default)
c_matrix_down
F_score_down <- 2 * c_matrix_down$byClass[1] * c_matrix_down$byClass[3] /
  (c_matrix_down$byClass[1] + c_matrix_down$byClass[3])
names(F_score_down) <- 'F_Score_Up'
F_score_down # Better?

# Nearmiss algorithm 
# install.packages("themis")
library(themis)
# Need to have a variable called 'class' 
colnames(train)[9] <- 'class'
# Note that the the step_nearmiss is a nearmiss-1 algorithm
train_nearmiss_sample <- recipe(~., train) %>%
  step_nearmiss(class, under_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)

train_nearmiss_sample # a tibble is a new modern dataframe.
model_nearmiss <- glm(class ~.,data = train_nearmiss_sample, family = binomial(link="logit"))
pred <- predict(model_nearmiss, newdata = test, type = "response")
c_matrix_nearmiss <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),
                                 reference = test$loan_default)
c_matrix_nearmiss
F_score_nearmiss <- 2 * c_matrix_nearmiss$byClass[1] * c_matrix_nearmiss$byClass[3] /
  (c_matrix_nearmiss$byClass[1] + c_matrix_nearmiss$byClass[3])
names(F_score_nearmiss) <- 'F_Score_Up'
F_score_nearmiss # Better?

# Let's now do tomek.
train_tomek_sample <- recipe(~., train) %>%
  step_tomek(class) %>%
  prep() %>%
  bake(new_data = NULL)

train_tomek_sample 
# Note that the tomek here does not 'balance' our dataset. HOWEVER, this does
# take out 'outliers'
table(train_tomek_sample$class)
model_tomek <- glm(class ~.,data = train_tomek_sample, family = binomial(link="logit"))
pred <- predict(model_tomek, newdata = test, type = "response")
c_matrix_tomek <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),
                                     reference = test$loan_default)
c_matrix_tomek
F_score_tomek <- 2 * c_matrix_tomek$byClass[1] * c_matrix_tomek$byClass[3] /
  (c_matrix_tomek$byClass[1] + c_matrix_tomek$byClass[3])
names(F_score_tomek) <- 'F_Score_Up'
F_score_tomek # Better?
