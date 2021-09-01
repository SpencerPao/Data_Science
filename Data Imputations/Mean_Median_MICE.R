library(mice)
library(tidyverse)
# MEAN / MEDIAN / MICE imputation
# Paper detailing the functions of MICE if interested...
# https://cran.r-project.org/web/packages/mice/mice.pdf


setwd('C:/Users/Spenc/Documents/Youtube/Machine Learning/R/Imputations')

data <- read.csv('data.csv',stringsAsFactors = TRUE,na.strings=c("",NA))

# Easiest way for data imputation (with not always the prettiest results...)
# data$COLUMN_NAME[is.na(data$COLUMN_NAME)] <- median(data$COLUMN_NAME, na.rm=TRUE)
# data$COLUMN_NAME[is.na(data$COLUMN_NAME)] <- mean(data$COLUMN_NAME, na.rm=TRUE)
# It's really quite that simple.

dim(data)
# Checking out the NA's that we are working with...
data %>% summarise_all(funs(sum(is.na(.))))

(1836 / 32560) * 100 # Approximatley 5% of the data is missing.
# This is a good threshold for this type of data.
# If data is more than 5% then we should probably leave this feature out.


# These are 3 categorical variables -- so the regression that we will use
unique(data$Workclass) # 9 unique classes
unique(data$Occupation) # 15 unique classes
unique(data$Country) # 42 unique classes

# Let's store a subset of data (Just to keep in mind)
data_occupation_subset <- subset(data, is.na(data$Occupation))
data_workclass_subset <- subset(data, is.na(data$Workclass))
data_country_subset <- subset(data, is.na(data$Country))


# Let's take a closer look at the missing data patterns
md.pattern(data)
# Let's interpret this...
# 30,000 observations are complete -- 8% of our total data is missing.
# 7 missing values in occupation
# 1809 observations are missing in workclass and occupation
# So on and so forth...

# A more helpful representation can be found using
# Code from this website:
# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE,
                  labels=names(data),
                  cex.axis=.7,
                  gap=3,
                  ylab=c("Histogram of Missing data","Pattern"))

# Now that we have a good idea on what data we are imputing and what number
# of observations we are planning on imputing.. let us use the mice function.
# m is the number of imputed datasets (you can think of this as # of cycles)
# You can check out what regression methods you can use...
methods(mice)

# In this case, I am going to be using the random forest mice function.
# If you don't know what random forests are, I highly recommend you check out
# my explanation on what it is and how to apply it!
imputed_data <- mice(data, m=5, method = "rf")
summary(imputed_data)


# Once that has run.... (took like 10 minutes)
# Let's check out what data has been imputed.

imputed_data$imp$Workclass # Each column represents the imputed data
# Each row represents the observation that is being imputed.
unique(data$Workclass)

# Country
imputed_data$imp$Country
unique(data$Country)

# Occupation
imputed_data$imp$Occupation
unique(data$Occupation)

# Choose which of these imputed datasets you would like...
finished_imputed_data <- complete(imputed_data,1)
# Check for any missing values?
sapply(finished_imputed_data, function(x) sum(is.na(x))) # good to go!
# You can easily apply this to any other data set you may choose.
# However, you can also plug away ALL of your imputed data sets and see what 
# happens!

# Since our target is a binary outcome.. let's use a logistic regression model..
model <- with(imputed_data, glm(Target ~ Workclass + Country + Occupation, family = binomial))
summary(pool(model))

# Some individual observation groups are statistically significant...
# Some things to note.. if you want accuracy measurements, I would recommend
# that you would duplicate your original data set and insert "randomly"
# Null out some of your observations  in your data set. Run through
# the entire process again and retrieve your imputations.
# Compare the imputations with the unedited data set to get a sense as to how
# well your data has been massaged. You can have a general rule of accuracy
# to judge this off of.
