## Author: Hemant Thapa
## K means Clustering 
## Programming Language: R
## Date Pushed to GitHub: 14.02.2024
## Email: hemantthapa1998@gmail.com

# loading all libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(glmnet)
library(rpart)
library(nlme) 
library(lubridate)
library(gridExtra)
library(scales)
library(cluster)
library(clusterCrit)
library(fpc)

# 1. load the data set 
dataset <- read.csv('Mall_Customers.csv')
print(dim(dataset))

# 2. view the data set
View(dataset)

# 3. first 10 rows
head(dataset, 10)

# 4. last 10 rows
tail(dataset, 10)

# 5. return the class of an object
class(dataset)

# 6. return the internal structure of an R object
str(dataset)

# 7. unique column names and data types
sapply(dataset, class)
print(names(dataset))

# 8. summary statistics  
summary(dataset)

# 9. feature selection 
X <- dataset[, c("Annual_Income", "Spending_Score")]
y <- dataset$Spending_Score

# scatter plot for feature selection
ggplot(dataset, aes(x = Annual_Income, y = Spending_Score)) +
  geom_point() +
  labs(x = "Annual Income", y = "Spending Score") +
  ggtitle("Annual Income vs. Spending Score")

# elbow Method for optimal number of clusters
wcss <- numeric(length = 10) 

# WCSS for k = 1 to 10
for (i in 1:10) {
  kmeans_model <- kmeans(X, centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

elbow_df <- data.frame(K = 1:10, WCSS = wcss)

# Elbow Method graph
elbow_plot <- ggplot(elbow_df, aes(x = K, y = WCSS)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(x = "Number of Clusters (K)", y = "Within-Cluster Sum of Squares (WCSS)") +
  ggtitle("Elbow Method for Optimal K") +
  theme_minimal()

print(elbow_plot)

# K-means clustering with 5 clusters
kmeans_model <- kmeans(X, centers = 5, nstart = 10)
clustered_dataset <- cbind(dataset, Cluster = as.factor(kmeans_model$cluster))

# scatter plot with clustered data points
scatter_plot <- ggplot(clustered_dataset, aes(x = Annual_Income, y = Spending_Score, color = Cluster)) +
  geom_point() +
  labs(x = "Annual Income", y = "Spending Score", color = "Cluster") +
  ggtitle("Clustered Scatter Plot") +
  theme_minimal()

print(scatter_plot)

# silhouette Score
silhouette_score <- silhouette(kmeans_model$cluster, dist(X))
print(paste("Silhouette Score:", mean(silhouette_score[, "sil_width"])))

# WCSS (already calculated during the elbow method)
print(paste("WCSS:", kmeans_model$tot.withinss))

