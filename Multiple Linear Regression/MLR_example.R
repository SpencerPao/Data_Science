# Clear the workspace
#rm(list = ls())

# Set the working directory
# setwd('Multiple Linear Regression')

data <- read.csv('Real Estate.csv')
data <- data[order(data$TransactionDate),]


# Exploratory Data Analysis
summary(data)

str(data)

# Remove the number Variable
data <- data[,2:8]

# Correlation Matrix
cor(data)

#########################
# Checking for linearity

attach(data)

plot(TransactionDate, HousePricePerUnitArea)
data$TransactionDate <- round(data$TransactionDate)

boxplot(HousePricePerUnitArea ~ TransactionDate, data = data, main = "House price per unit vs Date")



par(mfrow=c(2,3))
plot(HouseAge,HousePricePerUnitArea)
plot(DistanceToNearestMRTStation,HousePricePerUnitArea)
plot(NumberOfConvenienceStores,HousePricePerUnitArea)
plot(Latitude, HousePricePerUnitArea)
plot(Longitude, HousePricePerUnitArea)
# Reset all the plots
dev.off()


# QQ Plots 
par(mfrow=c(2,3))
qqnorm(HouseAge, main = "Normal QQ Plot")
qqnorm(DistanceToNearestMRTStation, main = "Normal QQ Plot")
qqnorm(NumberOfConvenienceStores, main = "Normal QQ Plot")
qqnorm(Latitude, main = "Normal QQ Plot")
qqnorm(Longitude, main = "Normal QQ Plot")

# Shapiro Wilks test
# Null hyp: Data are normally distributed
# P value: 0.05
shapiro.test(HouseAge)
shapiro.test(DistanceToNearestMRTStation)
shapiro.test(NumberOfConvenienceStores)
shapiro.test(Latitude)
shapiro.test(Longitude)


# Model Creation
mod <- lm(HousePricePerUnitArea ~., data = data)

# multi-collinearity
# vif > 5 
# Vif > 10 -- definitley remove
library(car)
vif(mod)

# Autocorrelation:
acf(HousePricePerUnitArea)

# Now take a look at model
summary(mod)


# Need to take a look at the residuals
# Residuals need to be...
# Independent
# normally distributed
# have constant variance


# Histogram: data follows normal distribution?
hist(mod$residuals, main = 'Histogram of Residuals', ylab = 'Residuals')


# Residuals are independent:
library(lmtest)
# dw-test (Durbin Watson test)
# Null hyp: errors are uncorrelated
dwtest(mod)
# P value > 0.05

# Constant variance
library(MASS)
plot(studres(mod), main = "Studentized Residuals")
abline(h = 2)
abline(h = -2)



# Analysis of variance table
anova(mod)


# Confidence Interval
confint(mod, level= 0.95)


c <- confint(mod, level= 0.95)
lowerbound <- c[,1]
lowerbound
# HousePricePerUnit = -2.543619e+04 + 1.912356e+00 * TransactionDate -3.448603e-01 * HouseAge 
#                     -6.002325e-03* DistanceToNearestMRTStation + 7.600747e-01 * NumberOfConvenienceStores
#                     + 1.453206e+02*Latitude -1.083564e+02 *Longitude 
upperbound <- c[,2]
upperbound
# HousePricePerUnit = 80.24346365 + 6.53753126 * TransactionDate -0.19373132 * HouseAge 
#                     -0.00317585* DistanceToNearestMRTStation + 1.49820980  * NumberOfConvenienceStores
#                     + 319.61392390 *Latitude -82.20831167 *Longitude 
detach(data)




