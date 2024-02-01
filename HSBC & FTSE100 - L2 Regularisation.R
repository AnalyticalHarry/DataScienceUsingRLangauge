## Author: Hemant Thapa
## FTSE 100 & HSBC Holdings
## L2 Regularisation (Ridge Regression) 
## Programming Language: R Programming
## Date Pushed to GitHub: 24.01.2024
## Email: hemantthapa1998@gmail.com

#####################################
######## Loading Libraries ##########
#####################################

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

#####################################
######## Loading DataSet ############
#####################################

#loading the dataset and checking dimensions
hsbc = read.csv('hsbc_dataset.csv')
ftse = read.csv('ftse_dataset.csv')

View(hsbc)
View(ftse)

#divide the Close columns by 100 to convert into pounds
hsbc$Close <- hsbc$Close / 100

#print head of dataset (10 rows)
head(hsbc, 10)
head(ftse, 10)

#print tail of dataset (10 rows)
tail(hsbc, 10)
tail(ftse, 10)

#printing summary of dataset
summary(hsbc)
summary(ftse)

#checking unique character and white space
skim(hsbc)
skim(ftse)

#checking for missing data in all columns of hsbc data frame
hsbc_missing_data <- skim_without_charts(hsbc)

#checking for missing data in all columns of ftse data frame
ftse_missing_data <- skim_without_charts(ftse)

#summary of missing data for hsbc
print("Missing data summary for hsbc:")
print(hsbc_missing_data)

#summary of missing data for ftse
print("Missing data summary for ftse:")
print(ftse_missing_data)

#####################################
####### Time Series Analysis ########
#####################################

#function will print unique columns name and data types
sapply(hsbc, class)
print(names(hsbc))

sapply(ftse, class)
print(names(ftse))

#converting the date column to a proper date object
hsbc$Date <- as.Date(hsbc$Date)
ftse$Date <- as.Date(ftse$Date)

#time series for hsbc holding
ggplot(hsbc, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)", title = "HSBC")

#time series plot for ftse100 index
ggplot(ftse, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)", title = "ftse")


# Combine the hsbc and ftse100 index
hsbc$Symbol <- "HSBC"
ftse$Symbol <- "FTSE100"
combined_data <- rbind(hsbc, ftse)

# Separate time series plots for HSBC and ftse100 index with dashed grid lines
plot_hsbc <- ggplot(subset(combined_data, Symbol == "HSBC"), aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)") +
  ggtitle("HSBC") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dashed"))  # Set linetype to "dashed"

plot_ftse <- ggplot(subset(combined_data, Symbol == "FTSE100"), aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)") +
  ggtitle("FTSE100") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dashed"))  # Set linetype to "dashed"

grid.arrange(plot_hsbc, plot_ftse, ncol = 1)

#####################################
###### Simple Moving Averages #######
#####################################

#Function for Simple Moving Averages
sma_time_series_plot <- function(data, symbol, window_size = 50, grid_linetype = "solid") {
  # Simple Moving Averages
  calculate_sma <- function(data, window_size) {
    data %>%
      mutate(SMA = zoo::rollmean(Close, window_size, align = "right", fill = NA))
  }
  
  data <- data %>%
    arrange(Symbol, Date) %>%
    mutate(SMA_50 = calculate_sma(data, window_size)$SMA,
           SMA_200 = calculate_sma(data, 200)$SMA)
  
  #Time series plot
  plot <- ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_line(aes(x = Date, y = SMA_50), color = "blue", linetype = "dashed", size = 1.2) +
    geom_line(aes(x = Date, y = SMA_200), color = "red", linetype = "dashed", size = 1.2) +
    labs(x = "Date", y = "Close Price (0.01 GBP)") +
    ggtitle(paste(symbol, "Stock Price with SMAs")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = grid_linetype))
  
  return(plot)
}

#Calling function for SMA plot for HSBC and FTSE
hsbc_plot <- sma_time_series_plot(hsbc, "HSBC", window_size = 50, grid_linetype = "dashed")
ftse_plot <- sma_time_series_plot(ftse, "FTSE100", window_size = 50, grid_linetype = "dashed")
grid.arrange(hsbc_plot, ftse_plot, ncol = 1)


#####################################
#### Exponential Moving Averages ####
#####################################

#function for Exponential Moving Averages (EMAs)
ema_time_series_plot <- function(data, symbol, window_size = 50, grid_linetype = "solid") {
  # Filter out rows with missing values
  data <- data[complete.cases(data), ]
  
  #exponential moving averages
  calculate_ema <- function(data, window_size) {
    data %>%
      mutate(EMA = EMA(Close, n = window_size))
  }
  
  data <- data %>%
    arrange(Symbol, Date) %>%
    mutate(EMA_50 = calculate_ema(data, window_size)$EMA,
           EMA_200 = calculate_ema(data, 200)$EMA)
  
  # Time series plot
  plot <- ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_line(aes(x = Date, y = EMA_50), color = "blue", linetype = "dashed", size = 1.2) +
    geom_line(aes(x = Date, y = EMA_200), color = "red", linetype = "dashed", size = 1.2) +
    labs(x = "Date", y = "Close Price (0.01 GBP)") +
    ggtitle(paste(symbol, "Stock Price with EMAs")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = grid_linetype))
  
  return(plot)
}

#Calling function for EMA plot for HSBC and FTSE
hsbc_ema_plot <- ema_time_series_plot(hsbc, "HSBC", window_size = 50, grid_linetype = "dashed")
ftse_ema_plot <- ema_time_series_plot(ftse, "FTSE100", window_size = 50, grid_linetype = "dashed")
grid.arrange(hsbc_ema_plot, ftse_ema_plot, ncol = 1)

###############################################
############### Bollinger Bands ###############
###############################################

#bollinger Bands Function
bollinger_bands_time_series_plot <- function(data, symbol, window = 20, sd_multiplier = 2, grid_linetype = "solid") {
  # Filtering out rows with missing values
  data <- data[complete.cases(data), ]
  
  #bollinger Bands formula
  data <- data %>%
    mutate(
      SMA = zoo::rollmean(Close, window, align = "right", fill = NA),
      SD = zoo::rollapply(Close, window, sd, align = "right", fill = NA),
      UpperBand = SMA + sd_multiplier * SD,
      LowerBand = SMA - sd_multiplier * SD
    )
  
  #time Series Plot
  plot <- ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_line(aes(x = Date, y = UpperBand), color = "blue", size = 0.9) +
    geom_line(aes(x = Date, y = LowerBand), color = "red", size = 0.9) +
    labs(x = "Date", y = "Price") +
    ggtitle(paste(symbol, "Bollinger Bands")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = grid_linetype))
  
  return(list(data = data, plot = plot))
}

#bollinger Bands plots for HSBC and FTSE100
hsbc_bollinger_bands_result <- bollinger_bands_time_series_plot(hsbc, "HSBC")
ftse_bollinger_bands_result <- bollinger_bands_time_series_plot(ftse, "FTSE100")

#plots
hsbc_bollinger_bands_plot <- hsbc_bollinger_bands_result$plot
ftse_bollinger_bands_plot <- ftse_bollinger_bands_result$plot

#plots in rows
grid.arrange(hsbc_bollinger_bands_plot, ftse_bollinger_bands_plot, nrow = 2)


###############################################
###### Relative Strength Index (RSI) ##########
###############################################

#function for relative strength index
rsi_time_series_plot <- function(data, symbol, window = 14, grid_linetype = "solid") {
  # Filtering out rows with missing values
  data <- data[complete.cases(data), ]
  
  #RSI formula
  data <- data %>%
    mutate(
      PriceChange = Close - lag(Close),
      Gain = ifelse(PriceChange > 0, PriceChange, 0),
      Loss = ifelse(PriceChange < 0, -PriceChange, 0),
      AvgGain = zoo::rollapply(Gain, window, mean, align = "right", fill = NA),
      AvgLoss = zoo::rollapply(Loss, window, mean, align = "right", fill = NA),
      RS = AvgGain / AvgLoss,
      RSI = 100 - (100 / (1 + RS))
    )
  
  #time series plot for RSI
  rsi_plot <- ggplot(data, aes(x = Date, y = RSI)) +
    geom_line(color = "blue", linetype = grid_linetype, size = 1) +
    labs(x = "Date", y = "RSI") +
    ggtitle(paste(symbol, "Relative Strength Index (RSI)")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = grid_linetype))
  
  return(rsi_plot)
}

#function for stock price chart
stock_price_plot <- function(data, symbol) {
  plot <- ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    labs(x = "Date", y = "Close Price") +
    ggtitle(paste(symbol, "Price")) +
    theme_minimal()
  
  return(plot)
}

#stock price chart
hsbc_stock_plot <- stock_price_plot(hsbc, "HSBC")
ftse_stock_plot <- stock_price_plot(ftse, "FTSE100")

#RSI Plots
hsbc_rsi_plot <- rsi_time_series_plot(hsbc, "HSBC")
ftse_rsi_plot <- rsi_time_series_plot(ftse, "FTSE100")

#arrange the plots
grid.arrange(hsbc_stock_plot, ftse_stock_plot, hsbc_rsi_plot, ftse_rsi_plot, nrow = 2, ncol = 2)


###############################################
############ Cummulative Return  ##############
###############################################

#daily returns
calculate_returns <- function(data) {
  data %>%
    arrange(Date) %>%
    mutate(Return = c(NA, diff(Close) / head(Close, -1))) %>%
    # Replace NA in Return with 0 before calculating cumulative return
    mutate(Return = ifelse(is.na(Return), 0, Return)) %>%
    mutate(CumulativeReturn = cumprod(1 + Return) - 1)
}

#applying the function to HSBC and FTSE
hsbc_returns <- calculate_returns(hsbc)
ftse_returns <- calculate_returns(ftse)

#cumulative returns for HSBC
ggplot(hsbc_returns, aes(x = Date, y = CumulativeReturn)) +
  geom_line(color = "blue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Date", y = "Cumulative Return", title = "Cumulative Returns for HSBC") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")

#cumulative returns for FTSE
ggplot(ftse_returns, aes(x = Date, y = CumulativeReturn)) +
  geom_line(color = "red") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Date", y = "Cumulative Return", title = "Cumulative Returns for FTSE") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")

###############################################
####### Comparing Cumulative Return  ##########
###############################################

#same daily returns function from above
#apply the same function to HSBC and FTSE
calculate_returns <- function(data, name) {
  data %>%
    arrange(Date) %>%
    mutate(Return = c(NA, diff(Close) / head(Close, -1))) %>%
    mutate(Return = ifelse(is.na(Return), 0, Return)) %>%
    mutate(CumulativeReturn = cumprod(1 + Return) - 1) %>%
    mutate(Stock = name)
}

hsbc_returns <- calculate_returns(hsbc, "HSBC")
ftse_returns <- calculate_returns(ftse, "FTSE")


#combine the data sets
combined_returns <- rbind(hsbc_returns, ftse_returns)

#plot the cumulative returns for both HSBC and FTSE
ggplot(combined_returns, aes(x = Date, y = CumulativeReturn, color = Stock)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Date", y = "Cumulative Return", title = "Cumulative Returns for HSBC and FTSE") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_color_manual(values = c("HSBC" = "blue", "FTSE" = "red"))



###############################################
############### Total Growth  #################
###############################################

#initial and final values for HSBC
initial_hsbc <- hsbc$Close[1]
final_hsbc <- hsbc$Close[nrow(hsbc)]

#initial and final values for FTSE
initial_ftse <- ftse$Close[1]
final_ftse <- ftse$Close[nrow(ftse)]

#total growth for HSBC and FTSE
total_growth_hsbc <- (final_hsbc - initial_hsbc) / initial_hsbc
total_growth_ftse <- (final_ftse - initial_ftse) / initial_ftse

#total growth for both stocks
cat("Total Growth for HSBC:", total_growth_hsbc, "\n")
cat("Total Growth for FTSE:", total_growth_ftse, "\n")

###############################################
##############  Remove Outliers  ##############
###############################################

x <- ftse$Close
y <- hsbc$Close

remove_outliers <- function(X, Y, zscore_threshold = 3) {
  # Calculate Z-scores for X and Y
  z_scores_X <- (X - mean(X)) / sd(X)
  z_scores_Y <- (Y - mean(Y)) / sd(Y)
  
  # Identify and remove outliers based on Z-scores
  X_filtered <- X[abs(z_scores_X) <= zscore_threshold]
  Y_filtered <- Y[abs(z_scores_Y) <= zscore_threshold]
  
  # Return the filtered X and Y
  return(list(X_filtered, Y_filtered))
}
filtered_data <- remove_outliers(x, y)

x_ <- filtered_data[[1]]
y <- filtered_data[[2]]

###############################################
############ Feature Engineering  #############
###############################################

split_data <- function(X, y, train_proportion = 0.8, seed = 100) {
  #ensure both X and y have the same length
  min_length <- min(length(X), length(y))
  X <- X[1:min_length]
  y <- y[1:min_length]
  
  #set the seed for reproducibility
  set.seed(seed)  
  
  #number of data points
  n_total <- length(X)
  n_train <- round(train_proportion * n_total)
  n_test <- n_total - n_train
  
  #randomly shuffle indices
  indices <- sample(n_total)
  
  #split the indices into training and testing sets
  train_indices <- indices[1:n_train]
  test_indices <- indices[(n_train + 1):n_total]
  
  #split X and y using the selected indices
  X_train <- X[train_indices]
  y_train <- y[train_indices]
  X_test <- X[test_indices]
  y_test <- y[test_indices]
  
  #return the training and testing sets as a list
  data_list <- list(
    X_train = X_train,
    y_train = y_train,
    X_test = X_test,
    y_test = y_test
  )
  
  return(data_list)
}

#split the data into training and testing sets
data_split <- split_data(x,y, train_proportion = 0.8, seed = 100)

#print the dimensions of the training and testing sets
cat("Dimensions of training set (X_train, y_train):\n")
cat("X_train: ", length(data_split$X_train), "\n")
cat("y_train: ", length(data_split$y_train), "\n\n")

cat("Dimensions of testing set (X_test, y_test):\n")
cat("X_test: ", length(data_split$X_test), "\n")
cat("y_test: ", length(data_split$y_test), "\n")


###############################################
############ Correlation Analysis  ############
###############################################

plot_scatter <- function(data_split) {
  # Set up larger margins and character expansion for bigger plot elements
  par(mar = c(5.1, 4.1, 4.1, 2.1), cex = 1.5)
  
  # Scatter plot for training data
  plot(data_split$X_train, data_split$y_train, 
       main = "Scatter Plot - Training and Testing Data",
       xlab = "FTSE100", ylab = "HSBC", col = "blue", pch = 20,
       xlim = c(min(data_split$X_train, data_split$X_test), 
                max(data_split$X_train, data_split$X_test)),
       ylim = c(min(data_split$y_train, data_split$y_test), 
                max(data_split$y_train, data_split$y_test)))
  
  # Add testing data points
  points(data_split$X_test, data_split$y_test, col = "red", pch = 20)
  
  # Add a legend
  legend("topright", legend = c("Training Data", "Testing Data"), 
         col = c("blue", "red"), pch = 20)
  
  # Reset to default plotting parameters and add grid
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, cex = 1)
  grid()
}

# Now you can call this function with your data_split object
plot_scatter(data_split)


###############################################
############# L1 Regularization ###############
###############################################
#empty list to store the results
results <- list()

#loop through each degree of polynomial features
degrees <- c(10, 20, 30, 40, 50) 

#store the predictions and R-squared values
plot_data <- data.frame(X_test = data_split$X_test, y_test = data_split$y_test)

#empty data frame to store the metrics
metrics_table <- data.frame(Degree = degrees, RMSE = numeric(length(degrees)), MSE = numeric(length(degrees)), R_Squared = numeric(length(degrees)))

#loop through each degree
for (degree in degrees) {
  #polynomial features
  X_train_poly <- poly(data_split$X_train, degree, raw = TRUE)
  X_test_poly <- poly(data_split$X_test, degree, raw = TRUE)
  
  #response variable
  y_train <- data_split$y_train
  y_test <- data_split$y_test
  
  #regularized linear regression (Ridge)
  #l2 regularization (Ridge)
  alpha <- 0  
  #regularization strength
  lambda <- 0.001  
  
  #X_train_poly is a matrix with multiple columns
  #adding a column of 1s for the intercept
  X_train_poly <- cbind(1, X_train_poly)  
  #adding a column of 1s for the intercept
  X_test_poly <- cbind(1, X_test_poly)  
  
  #model
  lasso_model <- glmnet(X_train_poly, y_train, alpha = alpha, lambda = lambda)
  
  #predict on the test data
  y_pred <- predict(lasso_model, s = lambda, newx = X_test_poly)
  
  #RMSE (Root Mean Squared Error) for evaluation
  rmse <- sqrt(mean((y_pred - y_test)^2))
  
  #MSE (Mean Square Error)
  mse <- mean((y_pred - y_test)^2)
  
  #R-squared (R²) for evaluation
  r_squared <- 1 - (sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2))
  
  #appending the results to the list
  results[[paste("Degree", degree)]] <- list(
    RMSE = rmse,
    MSE = mse,
    R_Squared = r_squared,
    Predicted = y_pred
  )
  
  #storing predictions in the plot_data data frame
  plot_data[[paste("Degree", degree)]] <- y_pred
  
  #adding metrics to the metrics table
  metrics_table[metrics_table$Degree == degree, "RMSE"] <- rmse
  metrics_table[metrics_table$Degree == degree, "MSE"] <- mse
  metrics_table[metrics_table$Degree == degree, "R_Squared"] <- r_squared
}

#printing the R-squared values
cat("R-squared values for different degrees of polynomial features:\n")
for (degree in degrees) {
  r_squared <- results[[paste("Degree", degree)]]$R_Squared
  cat(sprintf("Degree %d: %.4f\n", degree, r_squared))
}

#plotting the best fit lines for different degrees with R-squared values
ggplot(plot_data, aes(x = X_test)) +
  geom_point(aes(y = y_test), color = "blue", alpha = 0.5) +
  geom_line(aes(y = `Degree 10`), color = "red", linetype = "solid", size = 1) +
  geom_line(aes(y = `Degree 20`), color = "green", linetype = "solid", size = 1) +
  geom_line(aes(y = `Degree 30`), color = "purple", linetype = "solid", size = 1) +
  geom_line(aes(y = `Degree 40`), color = "orange", linetype = "solid", size = 1) +
  geom_line(aes(y = `Degree 50`), color = "pink", linetype = "solid", size = 1) +
  labs(title = "Best Fit Lines for Different Degrees of Polynomial Features",
       x = "X_test",
       y = "Predicted Values",
       caption = "Different degrees represented by different colors") +
  theme_minimal()


cat("Metrics for Different Degrees of Polynomial Features:\n")
print(metrics_table)

#####################################
####### Residual Analysis ###########
#####################################

residual_analysis <- function(degrees, data_split, results) {
  #set up a multi-pane plot
  par(mfrow=c(3, length(degrees)))  # Change the number of rows and columns as needed
  
  for (degree in degrees) {
    actual <- data_split$y_test
    predicted <- results[[paste("Degree", degree)]]$Predicted
    residuals <- actual - predicted
    
    #plot residual histogram for each degree
    hist(residuals, main = paste("Residuals (Degree", degree, ")", sep = " "), xlab = "Residuals")
    grid()
    
    #plot residual vs. fitted values scatterplot for each degree
    plot(x = predicted, y = residuals, main = paste("Residuals vs. Fitted (Degree", degree, ")", sep = " "), xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at y=0
    grid()
    
    #plot a Q-Q plot for each degree to check for normality
    qqnorm(residuals, main = paste("Q-Q Plot (Degree", degree, ")", sep = " "))
    qqline(residuals, col='red')
    grid()
  }
}

#residual analysis
residual_analysis(degrees, data_split, results)


