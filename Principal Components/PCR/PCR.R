# ===== Principle Component Regression ===== 
# Citation
# --Include form kaggle
# https://www.kaggle.com/kumarajarshi/life-expectancy-who
rm(list = ls())
library(Metrics)
setwd('C:/Users/Spenc/Documents/Youtube/R Machine Learning/R/Principal Component/PCR')
set.seed(21)

data <- read.csv('Life Expectancy Data.csv')
str(data)
summary(data) # I se some NA's -- just to keep things easy, I'll remove them.
data <- na.omit(data)
summary(data)

drop_class = c("Country", "Year", "Status")
data <- data[,!(names(data) %in% drop_class)]
str(data)

# -------------------------------------------------------------------------
# Split data to train and test...
## 75% of the sample size
idx <- floor(0.75 * nrow(data))

## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(data)), size = idx)
train <- data[train_ind, ]
test <- data[-train_ind, ]


# Let's do a simple regression model and see how well we do...
simple_lm <- lm(Life.expectancy ~., data = train)
summary(simple_lm)
library(Metrics)
lm_pred <- predict(simple_lm, test)
rmse(actual = test$Life.expectancy, predicted = as.numeric(lm_pred))
# RMSE : a standard way to measure the error of a model in predicting quantitative data.

# install.package("pls")
library(pls)
pcr_model <- pcr(Life.expectancy ~ .,
                 data = train,
                 scale = TRUE,
                 validation = "CV")
summary(pcr_model)
pcr_pred <- predict(pcr_model, test, ncomp = 5)
rmse(actual = test$Life.expectancy, predicted = as.numeric(pcr_pred))



# psych package
# install.packages("psych")
library(psych)
pc.fit <- prcomp(~ Adult.Mortality+ infant.deaths +     
                   Alcohol +           
                   percentage.expenditure +       
                   Hepatitis.B  +   
                   Measles +     
                   BMI + 
                   under.five.deaths +
                   Polio +
                   Total.expenditure +
                   Diphtheria +
                   HIV.AIDS +
                   GDP +
                   Population +
                   thinness..1.19.years +
                   thinness.5.9.years + 
                   Income.composition.of.resources +
                   Schooling,
                 data=train,
                 scale=TRUE) # provide principal components for X
summary(pc.fit)
screeplot(pc.fit, type = 'l', main = "Screeplot for Life Expectancy Features")
# 5 or 6 components look to be the best
trans_test <- as.data.frame(predict(pc.fit, test)[,1:5])

new_train <- as.data.frame(cbind(train$Life.expectancy, pc.fit$x[,1:5]))
colnames(new_train)[1] <- "Life.expectancy"
str(new_train)
pcr_lm_model <- lm(Life.expectancy ~., data = new_train)
summary(pcr_lm_model)

pcr_predictions <- predict(pcr_lm_model, trans_test)
rmse(actual = test$Life.expectancy, predicted = as.numeric(pcr_predictions))

