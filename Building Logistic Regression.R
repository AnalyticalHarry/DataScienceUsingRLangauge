## Author: Hemant Thapa
## Programming Language: R Programming
## Building Logistic Regression Model
## Date Pushed to GitHub: 24.01.2024
## Email: hemantthapa1998@gmail.com

#####################################
######## Loading Libraries ##########
#####################################


# Loading Libraries
library(ggplot2)
library(dplyr)
library(caret)
library(scales)

#####################################
######## Loading Data Set ###########
#####################################

# 1. Loading the Dataset
dataset <- read.csv('car_data.csv')
print(dim(dataset))

#####################################
##### Exploratory Data Analysis #####
#####################################

print(head(dataset))
print(tail(dataset))
str(dataset)
summary(dataset)

# Scatter Plot for Age vs. Annual Salary
plot(dataset$Age, dataset$AnnualSalary, col = ifelse(dataset$Purchased == 1, "red", "blue"), 
     xlab = "Age", ylab = "Annual Salary", main = "Age vs. Annual Salary")

# Bar Chart for Gender Distribution
barplot(table(dataset$Gender), col = c("pink", "lightblue"), 
        main = "Gender Distribution", xlab = "Gender", ylab = "Count")

#####################################
### Building Logistic Regression ####
#####################################

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

logistic_regression <- function(X, y, learning_rate = 0.01, num_iterations = 1000) {
  num_samples <- nrow(X)
  num_features <- ncol(X)
  weights <- matrix(0, nrow = num_features, ncol = 1)
  bias <- 0
  
  for (i in 1:num_iterations) {
    linear_model <- X %*% weights + bias
    predictions <- sigmoid(linear_model)
    dw <- (1 / num_samples) * t(X) %*% (predictions - y)
    db <- (1 / num_samples) * sum(predictions - y)
    weights <- weights - learning_rate * dw
    bias <- bias - learning_rate * db
  }
  
  list("weights" = weights, "bias" = bias)
}

predict_logistic_regression <- function(X, model) {
  linear_model <- X %*% model$weights + model$bias
  predictions <- sigmoid(linear_model)
  round(predictions)
}

# 5. Data Preparation with Feature Scaling
dataset$Age <- scale(dataset$Age)
dataset$AnnualSalary <- scale(dataset$AnnualSalary)

set.seed(123)
split <- createDataPartition(dataset$Purchased, p = 0.7, list = FALSE)
train_data <- dataset[split, ]
test_data <- dataset[-split, ]

# 6. Model Training
X_train <- as.matrix(train_data[, c("Age", "AnnualSalary")])
y_train <- as.matrix(train_data$Purchased)
model <- logistic_regression(X_train, y_train, learning_rate = 0.001, num_iterations = 1000)

# 7. Model Prediction
X_test <- as.matrix(test_data[, c("Age", "AnnualSalary")])
y_pred <- predict_logistic_regression(X_test, model)
y_pred_class <- ifelse(y_pred >= 0.5, 1, 0)

# 8. Model Evaluation
accuracy <- mean(y_pred_class == test_data$Purchased)
print(paste("Accuracy:", accuracy))
# Confusion Matrix 
confusionMatrix(factor(y_pred_class, levels = c("0", "1")), 
                factor(test_data$Purchased, levels = c("0", "1")), 
                positive = "1")
# 25. Truth Table
ConfusionMatrix <- function(actual, predicted) {
  truth_table <- data.frame(Actual = actual, Predicted = predicted)
  true_positives <- sum(truth_table$Actual == 1 & truth_table$Predicted == 1)
  true_negatives <- sum(truth_table$Actual == 0 & truth_table$Predicted == 0)
  false_positives <- sum(truth_table$Actual == 0 & truth_table$Predicted == 1)
  false_negatives <- sum(truth_table$Actual == 1 & truth_table$Predicted == 0)
  
  cat("True Positives:", true_positives, "\n")
  cat("True Negatives:", true_negatives, "\n")
  cat("False Positives:", false_positives, "\n")
  cat("False Negatives:", false_negatives, "\n")
  
  list(
    TruePositives = true_positives,
    TrueNegatives = true_negatives,
    FalsePositives = false_positives,
    FalseNegatives = false_negatives
  )
}

# Actual and Predicted
actual <- test_data$Purchased
predicted <- y_pred_class # Ensure this is your model's prediction

# Calling Function
result <- ConfusionMatrix(actual, predicted)

# 26. Classification Report
calculate_classification_metrics <- function(actual, predicted) {
  true_positives <- sum(actual == 1 & predicted == 1)
  true_negatives <- sum(actual == 0 & predicted == 0)
  false_positives <- sum(actual == 0 & predicted == 1)
  false_negatives <- sum(actual == 1 & predicted == 0)
  
  accuracy <- (true_positives + true_negatives) / (true_positives + true_negatives + false_positives + false_negatives)
  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    Specificity = specificity,
    F1_Score = f1_score
  )
}

# Calculate Metrics
metrics <- calculate_classification_metrics(actual, predicted)
print(metrics)

