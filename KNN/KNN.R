# K Nearest Neighbors implementation

# Importing the dataset
data = iris

row_labels = data[,5]

# Encoding the target feature as factor
data$Species <- as.numeric(data$Species)


# Scale the data since we will be using distance formulas on the data
# and we want to reduce complexity and computation when computing 
# especially when our datasets are huge!
data[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")] <- scale(
  data[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")])

# Split into test and train 80/20
set.seed(123)

size <- floor(0.8 *  nrow(data))


train_ind <- sample(seq_len(nrow(data)), size = size)

train_labels <- data[train_ind, 5]

data_train <- data[train_ind,1:4]
data_test <- data[-train_ind,1:4]

data_test_labels <- row_labels[-train_ind]
# Fit KNN Model
# Most popular package for using KNN models is the standard library class:
# https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/kNN
library(class)

predictions <- knn(train = data_train,
                   test = data_test,
                   cl = train_labels,
                   k= 11)

# Notice that I am only getting 2 dimensions 
plot_predictions <- data.frame(
  data_test$Sepal.Length,
  data_test$Sepal.Width,
  data_test$Petal.Length,
  data_test$Petal.Width,
  predicted = predictions)

colnames(plot_predictions) <- c("Sepal.Length",
                                "Sepal.Width",
                                "Petal.Length",
                                "Petal.Width",
                                'predicted')
# Visualize the KNN algorithm results.
library(ggplot2)
library(plyr)
require(gridExtra)



p1 <- ggplot(plot_predictions, aes(Petal.Length, Petal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_text(aes(label=data_test_labels),hjust=1, vjust=2) +
  ggtitle("Predicted relationship between Petal Length and Width") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")


p2 <- ggplot(plot_predictions, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_text(aes(label=data_test_labels),hjust=1, vjust=2) +
  ggtitle("Predicted relationship between Sepal Length and Sepal") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

grid.arrange(p1, p2, ncol=2)
