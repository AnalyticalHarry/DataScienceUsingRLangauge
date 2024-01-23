## Author: Hemant Thapa
## Programming Language: R
## Date Pushed to GitHub: 23.01.2024
## Email: hemantthapa1998@gmail.com


#library install.packages('skimr')
#library install.packages('Rcurl')

#Loading all libraries
library(skimr)
library(RCurl)
library('stringi')
library('ggplot2')
library(dplyr)
library(tidyr)

# 1. Loading  laod dataset and checking dimension
dataset <- read.csv('Advertising.csv')
print(dim(dataset))

# 2. View tab for showing data set
View(dataset)

# 3. print the first 10 rows
head(dataset, 10)

# 4. print the last 10 rows
tail(dataset, 10)


# 5. Function returns the class of an object
class(dataset)

# 6. Function will return internal structure of R object
str(dataset)

# 7. Function will print unique columns name and data types
sapply(dataset, class)
print(names(dataset))


# 8. Summary statistics 
summary(dataset)

# 9. Checking missing columns
missing_values_per_column <- sapply(dataset, function(x) sum(is.na(x)))
print(missing_values_per_column)

# Missing values in dataset
total_missing_values <- sum(is.na(dataset))
print(paste("Total missing values in the dataset:", total_missing_values))

# 10. For loop to iterate over columns for checking numerical
# Iterate over each column in the dataset
for(i in names(dataset)) {
  # Check if the column is numeric
  numeric_check <- is.numeric(dataset[[i]])
  
  # Print the result
  cat(i, "is numeric:", numeric_check, "\n")
}


# 11. For loop to iterate over the dataset and checking integer columns
for(i in names(dataset)) {
  integer_check <- is.integer(dataset[[i]])
  cat(i, "is integer:", integer_check, "\n")
}

# 12. Checking is their any duplicate columns
duplicates_check <- any(duplicated(dataset))
cat("Are there any duplicate rows? ", duplicates_check, "\n")


# 15. checking unique character and white space
skim(dataset)

# 16. Scatter plot between Sales and TV Advertising budget
ggplot(dataset, aes(x = TV, y = Sales)) +
  geom_point() +  # Add point layer
  theme_minimal() +
  labs(title = "Scatter Plot of Sales vs. TV",
       x = "TV Advertising Budget",
       y = "Sales")

# 17. Scatter plot between Sales vs Radio
ggplot(dataset, aes(x = Radio, y = Sales)) +
  geom_point() +  # Add point layer
  theme_minimal() +
  labs(title = "Scatter Plot of Sales vs. Radio",
       x = "Radio Advertising Budget",
       y = "Sales")

# 18. scatter plot between Newspaper vs. Sales
ggplot(dataset, aes(x = Newspaper, y = Sales)) +
  geom_point() +  # Add point layer
  theme_minimal() +
  labs(title = "Scatter Plot of Sales vs. Newspaper",
       x = "Newspaper Advertising Budget",
       y = "Sales")

# 19. Sales Histo plot
ggplot(dataset, aes(x = Sales)) +
  geom_histogram(binwidth = 0.5, fill = "black", color = "grey") +
  theme_minimal() +
  labs(title = "Histogram of Sales",
       x = "Sales",
       y = "Count")

# 20. Sales KDE plot 
ggplot(dataset, aes(x = Sales)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "KDE Plot of Sales",
       x = "Sales",
       y = "Density")

# 21. Histogram and KDE plot of Sales
ggplot(dataset, aes(x = Sales)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "gray", color = "black", alpha = 0.6) +
  geom_density(color = "blue") +
  theme_minimal() +
  labs(title = "Histogram and KDE of Sales",
       x = "Sales",
       y = "Density")

# 22. boxplot
dataset_long <- dataset %>%
  pivot_longer(
    cols = c(TV, Radio, Newspaper, Sales),
    names_to = "variable",
    values_to = "value"
  )

# 23. box plot for all four variables
ggplot(dataset_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Box Plots of TV, Radio, Newspaper, and Sales",
       x = "Variable",
       y = "Value")

# 24. Linear Regression 
model <- lm(Sales ~ TV, data = dataset)
summary(model)

ggplot(dataset, aes(x = TV, y = Sales)) +
  geom_point() +  # Add point layer
  geom_smooth(method = "lm", se = FALSE, colorc = "red") +  # Add regression line
  theme_minimal() +
  labs(title = "Linear Regression of Sales on TV Advertising",
       x = "TV Advertising Budget",
       y = "Sales")

# 25. R square and Mean square error
# r suare
r_squared <- summary(model)$r.squared
#mean square error
residuals <- resid(model)
mse <- mean(residuals^2)
#root mean square error
rmse <- sqrt(mean(residuals^2))
#mean absolute error
mae <- mean(abs(residuals))

cat("R-squared (R2):", round(r_squared, 4), "\n")
cat("Mean Squared Error (MSE):", round(mse, 4), "\n")
cat("RMSE (Root Mean Squared Error):", round(rmse, 4), "\n")
cat("Mean Absolute Error (MAE):", round(mae, 4), "\n")

#26. Residual analysis
#scatter plot of residuals against the fitted values
plot(fitted(model), residuals(model), main = "Residual Scatter Plot",
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "black")
abline(h = 0, col = "red", lty = 2)
grid()

# QQ plot for residuals
qqnorm(residuals(model))
qqline(residuals(model), col = 2)
grid()

# box plot of residuals
boxplot(residuals(model), main = "Box Plot of Residuals")
grid()

# calculate and plot Cook's distance
cooks_dist <- cooks.distance(model)

plot(cooks_dist, pch = 20, main = "Cook's Distance Plot")
abline(h = 4/length(cooks_dist), col = "red", lty = 2)
grid()

