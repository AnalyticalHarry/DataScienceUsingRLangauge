# 1. Creating Synthetic Data
set.seed(123)
X <- 1:100
Y <- 2 * X + rnorm(100, mean = 0, sd = 10)

# 2. Spliting the Data into Training and Testing Sets
# 80% for training, 20% for testing
split_ratio <- 0.8  
num_samples <- length(X)
num_train <- round(num_samples * split_ratio)
train_indices <- sample(1:num_samples, num_train)
X_train <- X[train_indices]
Y_train <- Y[train_indices]
X_test <- X[-train_indices]
Y_test <- Y[-train_indices]

# 3. Building Linear Regression Model 
linear_regression <- function(X, Y, learning_rate = 0.0001, num_iterations = 50) {
  N <- length(X)
  # Initialising the weight and bias
  w <- runif(1) 
  b <- runif(1)  
  # Store costs over iterations
  costs <- numeric(num_iterations)  
  # Store weights over iterations
  weights <- numeric(num_iterations)  
  # Store biases over iterations
  biases <- numeric(num_iterations) 
  
  for (i in 1:num_iterations) {
    # Predictions
    Y_pred <- w * X + b
    
    # Gradients
    dw <- (2/N) * sum((Y_pred - Y) * X)
    db <- (2/N) * sum(Y_pred - Y)
    
    # Update parameters
    w <- w - learning_rate * dw
    b <- b - learning_rate * db
    
    # Cost, weights, and biases
    costs[i] <- mean((Y_pred - Y)^2)
    weights[i] <- w
    biases[i] <- b
  }
  
  return(list("weight" = w, "bias" = b, "costs" = costs, "weights" = weights, "biases" = biases))
}


# 4. R-squared (R2)
calculate_r_squared <- function(actual, predicted) {
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  return(r_squared)
}

# 5. Root Mean Squared Error (RMSE)
calculate_rmse <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2))
  return(rmse)
}

# 6. Mean Squared Error (MSE)
calculate_mse <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  return(mse)
}

# 7. Training the Linear Regression Model
model <- linear_regression(X_train, Y_train)

# 8. Model Parameters
cat("Final Parameters - Weight:", model$weight, "Bias:", model$bias, "\n")

# 9. Evaluating the Model on the Testing Set
Y_pred_test <- model$weight * X_test + model$bias

# 10. R-squared on Testing Set
r_squared_test <- calculate_r_squared(Y_test, Y_pred_test)
cat("R-squared on Testing Set (R2):", r_squared_test, "\n")

# 11. RMSE on Testing Set
rmse_test <- calculate_rmse(Y_test, Y_pred_test)
cat("Root Mean Squared Error on Testing Set (RMSE):", rmse_test, "\n")

# 12. MSE on Testing Set
mse_test <- calculate_mse(Y_test, Y_pred_test)
cat("Mean Squared Error on Testing Set (MSE):", mse_test, "\n")

# 13. Best-fit line, and add metrics to the plot
plot(X, Y, main = "Linear Regression", xlab = "X", ylab = "Y")
abline(a = model$bias, b = model$weight, col = "red")
grid()

# 14. Cost Function over Iterations
par(mfrow = c(1, 2))  # Create a 1x2 plot grid
plot(1:length(model$costs), model$costs, type = "l", xlab = "Iteration", ylab = "Cost", 
     main = "Cost Function")
grid()
# Plot the Changes in Weights and Biases over Iterations
plot(1:length(model$weights), model$weights, type = "l", xlab = "Iteration", ylab = "Weight", 
     main = "Weight")
lines(1:length(model$biases), model$biases, col = "red")
grid()

