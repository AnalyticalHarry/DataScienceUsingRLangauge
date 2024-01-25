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
library(caret)
library(randomForest)
library(glmnet)
library(rpart)
library(nlme) 
library(lubridate)
library(gridExtra)
library(scales)

# 1. Load the dataset and check its dimensions
dataset <- read.csv('DataSet/Advertising.csv')
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

x <- dataset$TV
y <- dataset$Sales

# Splitting data into train and test
split_data <- function(X, y, train_proportion = 0.8, seed = 100) {
  # Ensure both X and y have the same length
  min_length <- min(length(X), length(y))
  X <- X[1:min_length]
  y <- y[1:min_length]
  
  # Set the seed for reproducibility
  set.seed(seed)  
  
  # Number of data points
  n_total <- length(X)
  n_train <- round(train_proportion * n_total)
  n_test <- n_total - n_train
  
  # Randomly shuffle indices
  indices <- sample(n_total)
  
  # Split the indices into training and testing sets
  train_indices <- indices[1:n_train]
  test_indices <- indices[(n_train + 1):n_total]
  
  # Split X and y using the selected indices
  X_train <- X[train_indices]
  y_train <- y[train_indices]
  X_test <- X[test_indices]
  y_test <- y[test_indices]
  
  # Return the training and testing sets as a list
  data_list <- list(
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test
  )
  
  return(data_list)
}

#split the data into training and testing sets
data_split <- split_data(x, y, train_proportion = 0.8, seed = 100)

#print the dimensions of the training and testing sets
cat("Dimensions of training set (X_train, y_train):\n")
cat("X_train: ", length(data_split$X_train), "\n")
cat("y_train: ", length(data_split$y_train), "\n\n")

cat("Dimensions of testing set (X_test, y_test):\n")
cat("X_test: ", length(data_split$X_test), "\n")
cat("y_test: ", length(data_split$y_test), "\n")

# Function to add polynomial features 
poly_features <- function(X, degree) {
  poly_X <- poly(X, degree, raw = TRUE)
  colnames(poly_X) <- paste("X^", 1:degree, sep = "")
  return(poly_X)
}

degree_of_polynomial <- 5
#X_poly_train <- poly_features(data_split$X_train, degree_of_polynomial)
#X_poly_test <- poly_features(data_split$X_test, degree_of_polynomial)

X_train = data_split$X_train
y_train = data_split$y_train

X_test = data_split$X_test
y_test = data_split$y_test

#training Model
gls_model <- glm(y_train ~ poly(X_train, degree_of_polynomial, raw = TRUE))
summary(gls_model)

#model Evaluation
anova_results <- anova(gls_model)
print(anova_results)

y_pred_train <- predict(gls_model, newdata = data.frame(X_train = X_train))
y_pred_test <- predict(gls_model, newdata = data.frame(X_train = X_test))


# Scatter Plot with Training Data
ggplot() +
  geom_point(aes(x = data_split$X_train, y = data_split$y_train), color = "black") +
  geom_line(aes(x = data_split$X_train, y = y_pred_train), color = "red") +
  labs(title = "Training Data with GLM Predictions", x = "X-axis", y = "Y-axis") +
  theme_minimal()

# Scatter Plot with Testing Data
ggplot() +
  geom_point(aes(x = data_split$X_test, y = data_split$y_test), color = "black") +
  geom_line(aes(x = data_split$X_test, y = y_pred_test), color = "blue") +
  labs(title = "Testing Data with GLM Predictions", x = "X-axis", y = "Y-axis") +
  theme_minimal()

# 25. Calculating Errors for Train and Test set
train_residuals <- y_train - y_pred_train
mse_train <- mean(train_residuals^2)
rmse_train <- sqrt(mse_train)
mae_train <- mean(abs(train_residuals))

cat("Training Data Metrics:\n")
cat("Mean Squared Error (MSE):", round(mse_train, 4), "\n")
cat("Root Mean Squared Error (RMSE):", round(rmse_train, 4), "\n")
cat("Mean Absolute Error (MAE):", round(mae_train, 4), "\n")

test_residuals <- y_test - y_pred_test
mse_test <- mean(test_residuals^2)
rmse_test <- sqrt(mse_test)
mae_test <- mean(abs(test_residuals))

cat("\nTesting Data Metrics:\n")
cat("Mean Squared Error (MSE):", round(mse_test, 4), "\n")
cat("Root Mean Squared Error (RMSE):", round(rmse_test, 4), "\n")
cat("Mean Absolute Error (MAE):", round(mae_test, 4), "\n")

# 26. McFadden's R-squared
#null deviance (model with only the intercept)
null_deviance <- sum((y_train - mean(y_train))^2)
#residual deviance
residual_deviance <- deviance(gls_model)
#McFadden's R-squared
pseudo_r_squared <- 1 - (residual_deviance / null_deviance)
cat("Pseudo R-squared for the GLM:", round(pseudo_r_squared, 4), "\n")

# 26. Residual analysis
ggplot() +
  geom_point(aes(x = y_pred_train, y = train_residuals), color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (Training Data)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggplot() +
  geom_point(aes(x = y_pred_test, y = test_residuals), color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (Testing Data)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# QQ plot for residuals
ggplot() + 
  geom_qq(aes(sample = train_residuals)) + 
  geom_qq_line(aes(sample = train_residuals)) +
  labs(title = "QQ Plot of Training Residuals") +
  theme_minimal()

ggplot() + 
  geom_qq(aes(sample = test_residuals)) + 
  geom_qq_line(aes(sample = test_residuals)) +
  labs(title = "QQ Plot of Testing Residuals") +
  theme_minimal()


# Box plot of residuals
ggplot() +
  geom_boxplot(aes(y = train_residuals, x = 1), fill = "blue", color = "black") +
  labs(title = "Box Plot of Residuals for Training Data",
       x = "",
       y = "Residuals") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())


ggplot() +
  geom_boxplot(aes(y = test_residuals, x = 1), fill = "red", color = "black") +
  labs(title = "Box Plot of Residuals for Testing Data",
       x = "",
       y = "Residuals") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

#Kernel Density Estimate (KDE) 
combined_residuals <- data.frame(
  residuals = c(train_residuals, test_residuals),
  dataset = rep(c("Training", "Testing"), times = c(length(train_residuals), length(test_residuals)))
)
ggplot(combined_residuals, aes(x = residuals, fill = dataset)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, color = "black", alpha = 0.6) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ dataset, scales = "free") +
  scale_fill_manual(values = c("Training" = "blue", "Testing" = "grey")) +
  labs(title = "Histogram and KDE Plots of Training and Testing Data Residuals",
       x = "Residuals",
       y = "Density") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Dataset"))

