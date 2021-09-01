rm(list=ls())
setwd('C:/Users/Spenc/Documents/Youtube/R Machine Learning/R/Naive Bayes')
# Financial News data
# Kaggle https://www.kaggle.com/ankurzing/sentiment-analysis-for-financial-news
# Naive Bayes in the Natural Language Processing Sense.
data <- read.csv('all-data.csv', col.names = c("Sentiment", "Corpus"))
summary(data)
str(data)
table(data$Sentiment)

# Let's remove the neutral terms to make things very simple for naive bayes
data <- subset(data, Sentiment != "neutral")
table(data$Sentiment)


# Initial probability
init_neg <- (table(data$Sentiment) / sum(table(data$Sentiment)))[1]
init_pos <- (table(data$Sentiment) / sum(table(data$Sentiment)))[2]

# Let's do some NLP oriented stuff.
# i.e Tokenize 
library(stopwords)
library(tokenizers)
library(SnowballC)
library(dplyr)
# Sped up the process by looking at this: https://www.r-bloggers.com/2019/11/learning-data-science-sentiment-analysis-with-naive-bayes/
# The cleaning process here can be a lot more intense.. but this is just an example
# "digia" is not an english word for instance.
data$Corpus <- (tokenize_words(data$Corpus,
                               lowercase = TRUE,
                               strip_punct = TRUE,
                               strip_numeric = TRUE,
                               stopwords = stopwords::stopwords("en")))

# Functions for setting up the model.
calc_Probs <- function(tokens) {
  counts <- table(unlist(tokens)) + 1
  (counts/sum(counts))
}

# Separate data
pos_data <- subset(data, Sentiment == "positive")
neg_data <- subset(data, Sentiment == "negative")

# Calculate Probabilities
pos_probs <- calc_Probs(pos_data$Corpus)
neg_probs <- calc_Probs(neg_data$Corpus)


classify_sentiment <- function(input) {
  test <- unlist(tokenize_words(input,
                         lowercase = TRUE,
                         strip_punct = TRUE,
                         strip_numeric = TRUE,
                         stopwords = stopwords::stopwords("en")))
  print(test)
  pos_pred <- init_pos * ifelse(is.na(prod(pos_probs[test])), 1, prod(pos_probs[test]))
  neg_pred <- init_neg * ifelse(is.na(prod(neg_probs[test])), 1, prod(neg_probs[test]))
  if (pos_pred > neg_pred){
    return ("Positive")
  } else {
    return ("Negative")
  }
}
classify_sentiment("According to Gran , the company has no plans to move all production to Russia , although that is where the company is growing .")
classify_sentiment("The company is going under")
classify_sentiment("You're fired")

classify_sentiment("Operating Profit rose")
classify_sentiment("Purchase agreement occurred")
classify_sentiment("Net income increased")





