## Author: Hemant Thapa
## Programming Language: R Programming
## Date Pushed to GitHub: 24.01.2024
## Email: hemantthapa1998@gmail.com

#####################################
######## Loading Libraries ##########
#####################################

#Loading all libraries
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


#####################################
######## Loading Data Set ###########
#####################################

# 1. Loading  laod dataset and checking dimension
dataset <- read.csv('car_data.csv')
print(dim(dataset))

#####################################
######### Pre Processing ############
#####################################

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

#####################################
##### Exploratory Data Analysis #####
#####################################

# 9.scatter plot betweem Age and Salary 
plot(dataset$Age, dataset$AnnualSalary, 
     xlab = "Age", ylab = "Annual Salary", 
     main = "Scatter Plot of Age vs. Annual Salary")

# adding points with custom marker colors based on the "Purchased" variable
points(dataset$Age[dataset$Purchased == 1], dataset$AnnualSalary[dataset$Purchased == 1], 
       col = "red", pch = 16) 

points(dataset$Age[dataset$Purchased == 0], dataset$AnnualSalary[dataset$Purchased == 0], 
       col = "blue", pch = 16) 

# 10. Bar chart between female and male count
barplot(table(dataset$Gender), col = c("pink", "lightblue"), 
        main = "Gender Distribution", xlab = "Gender", ylab = "Count")

# 11. Box plot for Annual Salary by Gender
boxplot(AnnualSalary ~ Gender, data = dataset, col = c("pink", "lightblue"), 
        main = "Box Plot of Annual Salary by Gender", xlab = "Gender", ylab = "Annual Salary")

# 12. Annual salary histogram plot
hist(dataset$AnnualSalary, 
     col = "lightblue",             
     main = "Histogram of Annual Salary", 
     xlab = "Annual Salary",        
     ylab = "Frequency")          

# 13. Annual Salary KDE plot
# Compute the kernel density estimate
density_estimate <- density(dataset$AnnualSalary)

plot(density_estimate, 
     main = "Kernel Density Estimation Plot of Annual Salary",  
     xlab = "Annual Salary",        
     ylab = "Density",             
     col = "blue",                 
     lwd = 2)                      

# 14. histogram plot for annual salary for males
hist(dataset$AnnualSalary[dataset$Gender == "Male"], 
     col = "lightblue", 
     main = "Histogram of Annual Salary for Males",
     xlab = "Annual Salary",
     ylab = "Frequency")

# 15. histogram plot for annual salary for females
hist(dataset$AnnualSalary[dataset$Gender == "Female"], 
     col = "pink", 
     main = "Histogram of Annual Salary for Females",
     xlab = "Annual Salary",
     ylab = "Frequency")

# 16. Feature Selection 
# Defining X (features) and Y (target)
X <- dataset[, c("Age", "AnnualSalary")]
Y <- dataset$Purchased

# 17. Splitting data for train and test set
# Split data into training (80%) and testing (20%) sets
set.seed(100)
train_indices <- createDataPartition(Y, p = 0.7, list = FALSE)
train_data <- data.frame(X[train_indices, ], Purchased = Y[train_indices])
test_data <- data.frame(X[-train_indices, ], Purchased = Y[-train_indices])

# Convert Purchased to a factor with 2 levels
train_data$Purchased <- factor(train_data$Purchased)

#####################################
######### Post Processing ###########
#####################################

# 18. Selecting classification algorithms
# Logistic Regression
model_logistic <- glmnet(
  x = as.matrix(train_data[, c("Age", "AnnualSalary")]), 
  y = train_data$Purchased, 
  family = "binomial"
)

# Decision Tree
model_tree <- rpart(
  Purchased ~ Age + AnnualSalary, 
  data = train_data, 
  method = "class"
)

# 19. Making predictions on the test data for Logistic Regression and Decision Tree
predictions_logistic <- predict(model_logistic, newx = as.matrix(test_data[, c("Age", "AnnualSalary")]), 
                                s = 0.01, type = "response")

predictions_logistic <- as.numeric(predictions_logistic > 0.5)

predictions_tree <- predict(model_tree, newdata = test_data, type = "class")

# 20. Accuracy for Logistic Regression and Decision Tree
accuracy_logistic <- mean(predictions_logistic == test_data$Purchased)
accuracy_tree <- mean(predictions_tree == test_data$Purchased)

print(paste("Accuracy for Logistic Regression:", accuracy_logistic))
print(paste("Accuracy for Decision Tree:", accuracy_tree))

# 21. Cross-Validation for Decision Tree
ctrl_tree <- trainControl(
  method = "cv",    
  number = 10,      # Number of folds
  verboseIter = TRUE
)

model_tree_cv <- train(
  Purchased ~ Age + AnnualSalary, 
  data = train_data, 
  method = "rpart",
  trControl = ctrl_tree,
  tuneLength = 10   # Number of complexity parameter values to try
)

# 22. Cross-Validation for Logistic Regression
ctrl_logistic <- trainControl(
  method = "cv",    
  number = 10,      # Number of folds
  verboseIter = TRUE
)

model_logistic_cv <- train(
  Purchased ~ Age + AnnualSalary, 
  data = train_data, 
  method = "glmnet",
  trControl = ctrl_logistic,
  tuneLength = 10   # Number of complexity parameter values to try
)

# 23. Accuracy for Decision Tree and Logistic Regression with Cross-Validation
resamples_cv <- resamples(list(DecisionTree = model_tree_cv, LogisticRegression = model_logistic_cv))
summary(resamples_cv)

# 24. Decision boundary for binary class predictions
# Range of values for Age and annual salary
age_range <- range(dataset$Age)
salary_range <- range(dataset$AnnualSalary)
# Grid of values for Age and annual salary
grid_age <- seq(age_range[1] - 1, age_range[2] + 1, by = 1)
grid_salary <- seq(salary_range[1] - 1000, salary_range[2] + 1000, by = 100)
# Combinations of age and annual salary
grid_data <- expand.grid(Age = grid_age, AnnualSalary = grid_salary)
# Predicting the Purchased values for the grid data using the Decision Tree model
grid_data$Purchased <- predict(model_tree, newdata = grid_data, type = "class")
# Plot the decision boundary
ggplot() +
  geom_tile(data = grid_data, aes(x = Age, y = AnnualSalary, fill = Purchased), alpha = 0.3) +
  geom_point(data = dataset, aes(x = Age, y = AnnualSalary, color = as.factor(Purchased)), size = 2) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightpink")) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  labs(title = "Decision Boundary for Decision Tree Model",
       x = "Age",
       y = "Annual Salary") +
  theme_minimal()

# 25. Truth Table
ConfusionMatrix <- function(actual, predicted) {
  # truth table dataframe
  truth_table <- data.frame(Actual = actual, Predicted = predicted)
  
  # contingency table
  contingency_table <- table(truth_table)
  
  # true positives, true negatives, false positives, and false negatives
  true_positives <- sum(truth_table$Actual == 1 & truth_table$Predicted == 1)
  true_negatives <- sum(truth_table$Actual == 0 & truth_table$Predicted == 0)
  false_positives <- sum(truth_table$Actual == 0 & truth_table$Predicted == 1)
  false_negatives <- sum(truth_table$Actual == 1 & truth_table$Predicted == 0)
  
  cat("True Positives:", true_positives, "\n")
  cat("True Negatives:", true_negatives, "\n")
  cat("False Positives:", false_positives, "\n")
  cat("False Negatives:", false_negatives, "\n")
  
  # return the results as a list
  return(list(
    TruePositives = true_positives,
    TrueNegatives = true_negatives,
    FalsePositives = false_positives,
    FalseNegatives = false_negatives
  ))
}
#actual and predicted
actual <- test_data$Purchased
predicted <- predictions_tree
#calling function
result <- ConfusionMatrix(actual, predicted)

# 26. Classification report
calculate_classification_metrics <- function(actual, predicted) {
  #true positives, true negatives, false positives, and false negatives
  true_positives <- sum(actual == 1 & predicted == 1)
  true_negatives <- sum(actual == 0 & predicted == 0)
  false_positives <- sum(actual == 0 & predicted == 1)
  false_negatives <- sum(actual == 1 & predicted == 0)
  
  #classification metrics
  accuracy <- (true_positives + true_negatives) / (true_positives + true_negatives + false_positives + false_negatives)
  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # data frame to store the metrics
  metrics <- data.frame(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    Specificity = specificity,
    F1_Score = f1_score
  )
  
  return(metrics)
}

metrics <- calculate_classification_metrics(actual, predicted)
print(metrics)
