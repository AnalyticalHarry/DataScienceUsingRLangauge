## Author: Hemant Thapa
## Generalized Least Squares (GLS) 
## Programming Language: R
## Date Pushed to GitHub: 23.01.2024
## Email: hemantthapa1998@gmail.com

# Loading all libraries
library(skimr)
library(RCurl)
library('stringi')
library('ggplot2')
library(dplyr)
library(tidyr)
library(nlme)

# 1. Load the dataset and check dimensions
dataset <- read.csv('Advertising.csv')
print(dim(dataset))

# 2. View the dataset
View(dataset)

# 3. Print the first 10 rows
head(dataset, 10)

# 4. Print the last 10 rows
tail(dataset, 10)

# 5. Function to return the class of an object
class(dataset)

# 6. Function to return the internal structure of an R object
str(dataset)

# 7. Function to print unique column names and data types
sapply(dataset, class)
print(names(dataset))

# 8. Summary statistics 
summary(dataset)

# 9. Checking missing values per column
missing_values_per_column <- sapply(dataset, function(x) sum(is.na(x)))
print(missing_values_per_column)

# Missing values in the dataset
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

# 11. For loop to iterate over the dataset and check integer columns
for(i in names(dataset)) {
  integer_check <- is.integer(dataset[[i]])
  cat(i, "is integer:", integer_check, "\n")
}

# 12. Checking if there are any duplicate rows
duplicates_check <- any(duplicated(dataset))
cat("Are there any duplicate rows? ", duplicates_check, "\n")

# 15. Checking unique character and white space
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

# 18. Scatter plot between Newspaper vs. Sales
ggplot(dataset, aes(x = Newspaper, y = Sales)) +
  geom_point() +  # Add point layer
  theme_minimal() +
  labs(title = "Scatter Plot of Sales vs. Newspaper",
       x = "Newspaper Advertising Budget",
       y = "Sales")

# 19. Sales Histogram
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

# 22. Boxplot
dataset_long <- dataset %>%
  pivot_longer(
    cols = c(TV, Radio, Newspaper, Sales),
    names_to = "variable",
    values_to = "value"
  )

# 23. Box plot for all four variables
ggplot(dataset_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Box Plots of TV, Radio, Newspaper, and Sales",
       x = "Variable",
       y = "Value")

# 24. Linear Regression 
gls_model <- gls(Sales ~ TV, data = dataset)
summary(gls_model)

# Model Evaluation
anova_results <- anova(gls_model)
print(anova_results)

# 25. Calculating Errors
# Mean square error
gls_residuals <- residuals(gls_model)
mse <- mean(gls_residuals^2)
# Root mean square error
rmse <- sqrt(mean(gls_residuals^2))
# Mean absolute error
mae <- mean(abs(gls_residuals))

cat("Mean Squared Error (MSE) for GLS:", round(mse, 4), "\n")
cat("RMSE (Root Mean Squared Error) for GLS:", round(rmse, 4), "\n")
cat("Mean Absolute Error (MAE) for GLS:", round(mae, 4), "\n")

# 26. Residual analysis
# Scatter plot of residuals against the fitted values
plot(fitted(gls_model), gls_residuals, main = "Residual Scatter Plot for GLS Model",
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "black", cex = 1)
abline(h = 0, col = "red", lty = 2)
grid()

# QQ plot for residuals
qqnorm(gls_residuals)
qqline(gls_residuals, col = 2)
grid()

# Box plot of residuals
boxplot(gls_residuals, main = "Box Plot of Residuals for GLS Model")
grid()
