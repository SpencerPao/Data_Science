library(tidyr)
library(dplyr)

data<-read.csv("subset_credit_card_data.csv") #subset_credit_card_data.csv
View(data)
set.seed(123)

#employment as binary 0 or 1
search_for <- c("Salaried"=1,"Self employed"=0)
data$Employment.Type <- search_for[as.character(data$Employment.Type)]
data$Employment.Type[is.na(data$Employment.Type)] <- 0 #we are gonna assume NA as self employed

data <- select(data,-1)

kmean_data <- kmeans(data.matrix(data), centers = 30) # centers = 100 (however many clusters you'd want...)


data$Cluster_ID <- kmean_data$cluster

data %>%
  group_by(data$Cluster_ID) %>%
  summarise_all(funs(mean)) -> data
data <-select(data,-1)

write.csv(x = data,file = "Kmeans_scaled_data.csv")
