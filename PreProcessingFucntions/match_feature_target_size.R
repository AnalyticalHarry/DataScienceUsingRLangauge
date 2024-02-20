match_feature_target_size <- function(X, y) {
  X_length <- length(X)
  y_length <- length(y)
  
  if (X_length > y_length) {
    return(list(X = X[1:y_length], y = y))
  } else if (y_length > X_length) {
    return(list(X = X, y = y[1:X_length]))
  } else {
    return(list(X = X, y = y))
  }
}

result <- match_feature_target_size(X, y)
X <- result$X
y <- result$y

## Author : Hemant Thapa
## Date pushed in Github: 20.02.2024
## Programming Language: R
