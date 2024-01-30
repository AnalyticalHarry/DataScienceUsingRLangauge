## Author: Hemant Thapa
## Programming Language: R
## Date Pushed to GitHub: 30.01.2024
## Email: hemantthapa1998@gmail.com


#Loading all libraries
library(skimr)
library(RCurl)
library('stringi')
library('ggplot2')
library(dplyr)
library(tidyr)
library(dendextend)
library(cluster)

#Height of studnets 
heights <- c(150, 155, 160, 175, 180)
names(heights) <- c('Person A', 'Person B', 'Person C', 'Person D', 'Person E')

hc <- hclust(dist(heights), method = "ward.D2")

plot(hc, main = "Hierarchical Clustering Dendrogram", xlab = "Individuals", ylab = "Distance")

#cophenetic correlation coefficient
cophenetic_coefficient <- cor(cophenetic(hc), dist(heights))
print(paste("Cophenetic Correlation Coefficient:", cophenetic_coefficient))

#silhouette score
#assuming 2 clusters
clusters <- cutree(hc, k = 2)
silhouette_score <- silhouette(clusters, dist(heights))
mean(silhouette_score[, 3])

#country GDP
countries <- data.frame(
  Country = c('Country A', 'Country B', 'Country C', 'Country D', 'Country E'),
  GDP_per_Capita = c(40000, 50000, 30000, 70000, 60000),
  Life_Expectancy = c(80, 82, 75, 78, 81),
  Literacy_Rate = c(98, 85, 88, 90, 95)
)

#standardise the data
scaled_data <- scale(countries[, -1])


#hierarchical clustering
hc_countries <- hclust(dist(scaled_data), method = "ward.D2")

#dendrogram
plot(hc_countries, labels = countries$Country, main = "Hierarchical Clustering Dendrogram", xlab = "Country", ylab = "Distance")

#cophenetic correlation coefficient
cophenetic_coefficient_countries <- cor(cophenetic(hc_countries), dist(scaled_data))
print(paste("Cophenetic Correlation Coefficient:", cophenetic_coefficient_countries))

#silhouette Score
#assuming 2 clusters
clusters_countries <- cutree(hc_countries, k = 2)
silhouette_score_countries <- silhouette(clusters_countries, dist(scaled_data))
mean(silhouette_score_countries[, 3]) 
