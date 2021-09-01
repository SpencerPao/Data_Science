rm(list=ls())

# load the library
library(AppliedPredictiveModeling) # Abalone data set
library(caret) # Machine Learning library
library(mlbench) # Soybean data set

# Data Wrangling in R
setwd('...')

# The dataset we will be using today... abalone
data(abalone)
dim(abalone)
head(abalone)

# Set to data
data <- abalone
str(data)
# One Hot Encoding
dummy <- dummyVars("~.", data = data)
newdata <- data.frame(predict(dummy, newdata = data))
View(newdata)
str(newdata)

# Let's check out the apply family.
# Maybe I want everything to be an integer? How would I go about doing this?
# apply(X, MARGIN, FUN, ...)
str(data) # (Going back to the original dataset)
d <- data[,2:dim(data)[2]] # Remove the first column
str(d)

?apply
# Returns an array of attributes.
apply(d, MARGIN = 2, FUN = sum) # Switch to MARGIN = 1 compare the sums of rows;

?lapply
# Returns a list (dataframe)
lapply(d, MARGIN = 2, FUN = sum)

?sapply
# A wrapper function of lapply -- only difference is that sapply tries tries
# to simplify the elements to its most basic structure. If simplify = FALSE,
# you get the same output as lapply.
sapply(d, MARGIN = 2, FUN = sum, simplify = TRUE)

?mapply
# This is a multivariate version of sapply
# mapply(FUN, data, # of times you call function on the data)
mapply(rep,1:4, 3)



# Add, Remove and place features in to a dataset
library(dplyr)
str(data) # Let's work with this dataset.
# %>% this is a pipe operator. You are running a function on an object.
# This return a resulting object. You can therefore stack many operators on
# eachother.

# For instance, let's select 1 column.
data %>%
  select(Type)

# 2 columns? Run a head function?
data %>%
  select(Type, LongestShell) %>%
  head()

# Let's attempt to subset data. Let's get all the rows that have only 'M' in Type.
data %>%
  filter(Type == "M")


# Maybe you'd want to start plotting?
data %>%
  select(LongestShell, Diameter) %>%
  plot()



# How but if we only want the type whose Diameter > 0.3? Similar concept.
# How but less than 10 rings? Use the | and & operator
data %>%
  filter(Type == "M" & Diameter > 0.3 & Rings < 10)
# How but if I want part of a key word? (or parts of a number?)
# Let's use Soybean dataset.
data("Soybean")
str(Soybean)
Soybean$Class

Soybean %>%
  filter(grepl("frog", Class))
# How but if I don't want 'frog' inside of my rows?

Soybean %>%
  filter(!grepl("frog", Class))

# Let's go back to the original 'data'.. How would I sort by a feature?
str(data)
data %>%
  arrange(Rings)

# Let's start creating variables.
library(lubridate)
str(data)
data %>%
  mutate(Rings_Times_Height = Rings * Diameter) %>%
  head()
# Create more than 1 column? No problem.

data %>%
  mutate(Rings_Times_Height = Rings * Diameter,
         round_whole_height = round(WholeWeight)) %>%
  head()

# Maybe you'd want to create a column that has very specific conditions based on
# other columns? No worries. We can have an ifelse statement in one line.
data %>%
  mutate(Old = ifelse(Rings > 10, "Old", "Young")) # True, False


# Maybe you are doing some data analyst work and you want to generate some quick
# statistics. This can be done real easy via the pipes operator.

data %>%
  group_by(Type) %>%
  summarise(average_rings_by_type = mean(Rings, na.rm = TRUE),
            sum_rings_by_type = sum(Rings, na.rm=TRUE)) # If there is any NA? You can ignore.
