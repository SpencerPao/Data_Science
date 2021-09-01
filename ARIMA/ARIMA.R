# Reading in Data
data <- read.csv('NVDA.csv')

head(data)
summary(data)

library(xts)
NVDA <- xts(data[,-1], order.by = as.Date(data$Date, "%m/%d/%Y"))
plot(NVDA$Adj.Close, main = "NVDA Adjusted Closing Price")
plot(NVDA$Volume, main = "NVDA Trading Volume")

# Scaling the NVDA Volume
NVDA$Volume <- scale(NVDA$Volume)

# Start to Decompose data.
# Convert xts to ts object for decomposition
# Observed: Actual Plot
# trend: Increasing or Decreasing
# Seasonal: NAy monthly or yearly patterns in the data
# Random: Unexplained parts of the data.
NVDA_ts_close <- ts(NVDA$Adj.Close, start = c(2015,10), end = c(2020,10), frequency = 251)
components.ts <- decompose(NVDA_ts_close)
plot(components.ts)

# Need to Achieve Stationary.
# Taking a difference to make data stationary...
# Starting out with plotting 1 difference.
# Differencing helps stabilize the variance. It removes changes in the level of
# a time series, eliminating trend and seasonality.
# Takes d_(x) = t_(x+1) - t_(x)
plot(diff(NVDA_ts_close), main = "NVDA Diff Adjusted Closing Price")
plot(log(NVDA_ts_close), main = "NVDA Log Adjusted Closing Price")
plot(diff(log(NVDA_ts_close), lag = 1), main = "NVDIA Diff. Log Adjusted Closing Price")


# Decomposing the data into the main components: Seasonality, Trend, and Cycles.
# Seasonal Component: Fluctuations in data related to a specific cycle i.e Months,
# quarters, years etc.
# Trend Component: Increasing or Decreasing over time?
# Cycle Component: Data that is moving up or down but not in a seasonal pattern

# We can decompose in 2 ways. (Additive or Multiplicative Model)
# Additive Model: Add components together i.e Y = B0 + B1X1 ... + error
# Multiplicative Model: Multiply components together. Y = B0*B1X1*... + error



# Plotting the ACF and PACF Graphs
# Use the ACF for the AR(p) process
# Use the PACF for the MA(q) process
# Note, the 'I' in ARIMA is the differencing.

# Taking a look at both graphs
par(mfrow=c(1,2))
# We can determine if the data are stationary if the ACF plot does not go toward
# 0 rapidly.
acf(diff(log(NVDA_ts_close), lag = 1), main = "ACF for NVDIA Adjusted Closing")
pacf(diff(log(NVDA_ts_close), lag = 1), main = "PACF for NVDIA Adjusted Closing")
par(mfrow=c(1,1))


# Testing out if the time series is stationary
# Using the Augmented Dickey-Fuller test (ADF) is very popular.
# Null Hypothesis: Data are not stationary
# Alt. Hypothesis: Data are stationary
library(tseries)
adf.test(diff(log(NVDA_ts_close), lag = 1)) # P value < 0.05 indicates time series data are stationary

# Loading in packages
library(forecast)
# Finding the best model.
# Start off with a simple model.
# There is potential for seasonality.. but we will cover that in a different video

# Creating training and testing objects
# Predict 1 month.
training_Price <- NVDA$Adj.Close[1:(length(NVDA$Adj.Close) - 8)]
testing_Price <- NVDA$Adj.Close[(length(NVDA$Adj.Close) - 7):length(NVDA$Adj.Close)]
external_train <- NVDA[1:(length(NVDA$Adj.Close) - 8), 2:dim(NVDA)[2]]
external_test <- NVDA[(length(NVDA$Adj.Close) - 7):length(NVDA$Adj.Close),2:dim(NVDA)[2]]

fit <- Arima(y = training_Price, order = c(1,1,1), xreg = external_train)
summary(fit)
library(lmtest) # Used to get the model coefficients (Most important part)
coeftest(fit) # Our Volume was not very significant...
confint(fit) # Confidence interval 

# Model Evaluation
acf(fit$residuals) # Some significance that falls around Lag # 7 (Seasonality potential)
pacf(fit$residuals)

# Ljung-Box Test
# Null: Model does not lack of fit
# Alt: Model shows a lack of fit.
# *We want to fail to reject the Null. This shows that our residuals in our model
# is independent.
# Used after the model has ben fit.
Box.test(fit$residuals, lag=20, type="Ljung-Box")


# Passing in a Seasonal Model to see if that will get rid of the "significance"
fit_seasonal <- Arima(y = training_Price, order = c(1,1,1), seasonal = c(1,0,1), xreg = external_train)
coeftest(fit_seasonal) # Our Volume was not very significant...
confint(fit_seasonal) # Confidence interval 
acf(fit_seasonal$residuals) # didn't really do much here. Probably need to play more with the data.

Box.test(fit_seasonal$residuals, lag=20, type="Ljung-Box")


# Utilize a grid to find the best model with what we have to work with.
# auto.arima --> Best model is related with the AIC score (Lower the better)
?auto.arima
auto_fit <- auto.arima(d = 1,
                       max.p = 5,
                       max.q = 5,
                       max.P = 5,
                       max.Q = 5,
                       max.d = 5,
                       start.p = 1,
                       start.q = 1,
                       start.P = 1,
                       start.Q = 1,
                       seasonal = TRUE,
                       stepwise = TRUE,
                       trace = TRUE,
                       y = training_Price,
                       xreg = external_train,
                       allowdrift = TRUE)
# Best model signifies a random walk... haha
coeftest(auto_fit)
confint(auto_fit)
library(forecast)
# Prediction
forecast(auto_fit, xreg = external_test, h= 7)
f = forecast(auto_fit, xreg = external_test, h= 7)
plot(forecast(auto_fit, xreg = external_test, h= 7))

acf(auto_fit$residuals) # Some significance that falls around Lag # 7 (Seasonality potential)
pacf(auto_fit$residuals)
# Judging model --> Bad.
# Needs to be redone since this statistical evidence suggests that the model
# can't identify a trend here.
Box.test(auto_fit$residuals, lag=20, type="Ljung-Box")

library(Metrics)
rmse(as.numeric(testing_Price), as.numeric(f$mean))
# Measure of how far the original values were from the output
mae(as.numeric(testing_Price), as.numeric(f$mean))
# Maybe this learned something? Lots of room for improvement.


# Supports the hypothesis that the stock market prices are a random walk 
# and cannot be predicted based on its historical price alone.
# But, we can potentially use other data to help with the predictions.
# Or just use a different model in general.

# Idea here is that the best predictor for tomorrows price is today's price + drift
# Where drift is upward or downward trend.