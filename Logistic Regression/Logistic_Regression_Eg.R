# Clear the workspace
#rm(list = ls())

# Set your working directory
#setwd('')

# Load in our dataset
crime_data <- read.csv('crime-data.csv')
str(crime_data)
summary(crime_data)


plot(1:6,crime_data$target[1:6],
     xlab = 'Index',
     ylab = 'Probability of Commiting a Crime',
     main= "Probability of Commiting a Crime")
Lmod = lm(crime_data$target[1:6]~c(1:6))
abline(Lmod, col  = 'red')


attach(crime_data)

logmod <- glm(target ~ ., data=crime_data)
summary(logmod)

logmod1 <- glm(target ~ . -rm, data = crime_data)
summary(logmod1)

logmod2 <- glm(target ~. -chas -rm, data=crime_data)
summary(logmod2)

library(dplyr)
library(MASS)
# Let's find the best model using a step algorithm based on minimizing AIC values
model <- glm(target~.-rm -chas, data = crime_data) %>%
  stepAIC(trace = TRUE)



# Training and Testing Set 80/20

train = crime_data[0:round(0.8*dim(crime_data)[1]),]
test = crime_data[(round(0.8*dim(crime_data)[1])+1):dim(crime_data)[1],]

modelFinal <- glm(train$target ~., data = train) %>%
  stepAIC(trace = TRUE)
y_pred <- predict(modelFinal, test)

# Finding the Mean Squared Error of our model. 
MSE = (sum((y_pred - test$target)^2) / length(y_pred))
summary(modelFinal)

modelbase <- glm(train$target ~., data = train)
y_pred_base <- predict(modelbase, test)
MSE_base = (sum((y_pred_base - test$target)^2) / length(y_pred_base))
summary(modelbase)





detach(crime_data)