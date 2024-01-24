## Author: Hemant Thapa
## L1 Regularisation (lasso Regression) 
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
######## Loading Data Set ###########
#####################################

# 1. Loading the dataset and checking dimensions
hsbc = read.csv('hsbc_stock_data.csv')
lloyds = read.csv('lloyds_stock_data.csv')

View(hsbc)
View(lloyds)

#divide the Close columns by 100 to convert into pounds
hsbc$Close <- hsbc$Close / 100
lloyds$Close <- lloyds$Close / 100

#print head of dataset (10 rows)
head(hsbc, 10)
head(lloyds, 10)

#print tail of dataset (10 rows)
tail(hsbc, 10)
tail(lloys, 10)

#printing summary of dataset
summary(hsbc)
summary(lloyds)

#checking unique character and white space
skim(hsbc)
skim(lloyds)

#checking for missing data in all columns of hsbc data frame
hsbc_missing_data <- skim_without_charts(hsbc)

#checking for missing data in all columns of lloyds data frame
lloyds_missing_data <- skim_without_charts(lloyds)

#summary of missing data for hsbc
print("Missing data summary for hsbc:")
print(hsbc_missing_data)

#summary of missing data for lloyds
print("Missing data summary for lloyds:")
print(lloyds_missing_data)

#####################################
####### Time Series Analysis ########
#####################################

#function will print unique columns name and data types
sapply(hsbc, class)
print(names(hsbc))

sapply(lloyds, class)
print(names(lloyds))

#converting the date column to a proper date object
hsbc$Date <- as.Date(hsbc$Date)
lloyds$Date <- as.Date(lloyds$Date)

#time series for hsbc holding
ggplot(hsbc, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)", title = "HSBC")

#time series plot for lloyds holding
ggplot(lloyds, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)", title = "Lloyds")


#combine the hsbc and lloyds 
hsbc$Symbol <- "HSBC"
lloyds$Symbol <- "Lloyds"
combined_data <- rbind(hsbc, lloyds)

#separate time series plots for HSBC and Lloyds 
plot_hsbc <- ggplot(subset(combined_data, Symbol == "HSBC"), aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)") +
  ggtitle("HSBC") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", alpha = 0.5))

plot_lloyds <- ggplot(subset(combined_data, Symbol == "Lloyds"), aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price (0.01 GBP)") +
  ggtitle("Lloyds") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", alpha = 0.5))
grid.arrange(plot_hsbc, plot_lloyds, ncol = 1)

#####################################
###### Simple Moving Averages #######
#####################################

#function for Simple Moving Averages
sma_time_series_plot <- function(data, symbol, window_size = 50, grid_linetype = "solid") {
  #simple Moving Averages
  calculate_sma <- function(data, window_size) {
    data %>%
      mutate(SMA = zoo::rollmean(Close, window_size, align = "right", fill = NA))
  }
  
  data <- data %>%
    arrange(Symbol, Date) %>%
    mutate(SMA_50 = calculate_sma(data, window_size)$SMA,
           SMA_200 = calculate_sma(data, 200)$SMA)
  
  #time series plot
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

#calling function for sma plot for hsbc and lloyds
hsbc_plot <- sma_time_series_plot(hsbc, "HSBC", window_size = 50, grid_linetype = "dashed")
lloyds_plot <- sma_time_series_plot(lloyds, "Lloyds", window_size = 50, grid_linetype = "dashed")
grid.arrange(hsbc_plot, lloyds_plot, ncol = 1)

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
  
  #time series plot
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

#calling function for ema plot for hsbc and lloyds
hsbc_ema_plot <- ema_time_series_plot(hsbc, "HSBC", window_size = 50, grid_linetype = "dashed")
lloyds_ema_plot <- ema_time_series_plot(lloyds, "Lloyds", window_size = 50, grid_linetype = "dashed")
grid.arrange(hsbc_ema_plot, lloyds_ema_plot, ncol = 1)

###############################################
#### Moving average convergence/divergence ####
###############################################

#function for Moving Averages Convergence Divergence (MACD)
macd <- function(data, symbol, short_window = 12, long_window = 26, signal_window = 9, grid_linetype = "solid") {
  #filtering out rows with missing values
  data <- data[complete.cases(data), ]
  
  #MACD
  calculate_macd <- function(data, short_window, long_window, signal_window) {
    macd_result <- TTR::MACD(Close, nFast = short_window, nSlow = long_window, nSig = signal_window)
    
    data %>%
      mutate(
        MACD = macd_result$macd,
        Signal = macd_result$signal,
        Histogram = macd_result$hist
      )
  }
  
  data <- data %>%
    arrange(Symbol, Date) %>%
    mutate(MACD = calculate_macd(data, short_window, long_window, signal_window)$MACD)
  
  #time series plot
  plot <- ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    geom_line(aes(x = Date, y = MACD), color = "blue", linetype = "solid", size = 1) +
    labs(x = "Date", y = "Close Price (0.01 GBP)") +
    ggtitle(paste(symbol, "MACD")) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = grid_linetype))
  
  return(plot)
}

#time series plots for HSBC and Lloyds
hsbc_macd_plot <- macdt(hsbc, "HSBC", short_window = 12, long_window = 26, signal_window = 9, grid_linetype = "solid")
lloyds_macd_plot <- macd(lloyds, "Lloyds", short_window = 12, long_window = 26, signal_window = 9, grid_linetype = "solid")
grid.arrange(hsbc_macd_plot, lloyds_macd_plot, ncol = 1)


###############################################
############### Bollinger Bands ###############
###############################################

#Bollinger Bands Function
bollinger_bands_time_series_plot <- function(data, symbol, window = 20, sd_multiplier = 2, grid_linetype = "solid") {
  # Filtering out rows with missing values
  data <- data[complete.cases(data), ]
  
  #Bollinger Bands fromula
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

#bollinger Bands plots for HSBC and Lloyds
hsbc_bollinger_bands_result <- bollinger_bands_time_series_plot(hsbc, "HSBC")
lloyds_bollinger_bands_result <- bollinger_bands_time_series_plot(lloyds, "Lloyds")
#Plots
hsbc_bollinger_bands_plot <- hsbc_bollinger_bands_result$plot
lloyds_bollinger_bands_plot <- lloyds_bollinger_bands_result$plot
#plots in rows
grid.arrange(hsbc_bollinger_bands_plot, lloyds_bollinger_bands_plot, nrow = 2)

###############################################
###### Relative Strength Index (RSI) ##########
###############################################

#function for relative strength index
rsi_time_series_plot <- function(data, symbol, window = 14, grid_linetype = "solid") {
  #filtering out rows with missing values
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

stock_price_plot <- function(data, symbol) {
  plot <- ggplot(data, aes(x = Date, y = Close)) +
    geom_line() +
    labs(x = "Date", y = "Close Price") +
    ggtitle(paste(symbol, "Stock Price")) +
    theme_minimal()
  
  return(plot)
}

#stock price chart
hsbc_stock_plot <- stock_price_plot(hsbc, "HSBC")
lloyds_stock_plot <- stock_price_plot(lloyds, "Lloyds")

#RSI Plots
hsbc_rsi_plot <- rsi_time_series_plot(hsbc, "HSBC")
lloyds_rsi_plot <- rsi_time_series_plot(lloyds, "Lloyds")
#arrange the plots
grid.arrange(hsbc_stock_plot, lloyds_stock_plot, hsbc_rsi_plot, lloyds_rsi_plot, nrow = 2, ncol = 2)


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

#applying the function to HSBC and Lloyd
hsbc_returns <- calculate_returns(hsbc)
lloyds_returns <- calculate_returns(lloyds)

#cumulative returns for HSBC
ggplot(hsbc_returns, aes(x = Date, y = CumulativeReturn)) +
  geom_line(color = "blue") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Date", y = "Cumulative Return", title = "Cumulative Returns for HSBC") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")

#cumulative returns for Lloyd
ggplot(lloyds_returns, aes(x = Date, y = CumulativeReturn)) +
  geom_line(color = "red") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Date", y = "Cumulative Return", title = "Cumulative Returns for Lloyds") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y")

###############################################
####### Comparing Cumulative Return  ##########
###############################################

#same daily returns function from above
#apply the same function to HSBC and Lloyd
calculate_returns <- function(data, name) {
  data %>%
    arrange(Date) %>%
    mutate(Return = c(NA, diff(Close) / head(Close, -1))) %>%
    mutate(Return = ifelse(is.na(Return), 0, Return)) %>%
    mutate(CumulativeReturn = cumprod(1 + Return) - 1) %>%
    mutate(Stock = name)
}

hsbc_returns <- calculate_returns(hsbc, "HSBC")
lloyds_returns <- calculate_returns(lloyds, "Lloyds")


#combine the data sets
combined_returns <- rbind(hsbc_returns, lloyds_returns)

#plot the cumulative returns for both HSBC and Lloyd
ggplot(combined_returns, aes(x = Date, y = CumulativeReturn, color = Stock)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Date", y = "Cumulative Return", title = "Cumulative Returns for HSBC and Lloyds") +
  theme_minimal() +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_color_manual(values = c("HSBC" = "blue", "Lloyds" = "red"))


###############################################
############### Total Growth  #################
###############################################

#initial and final values for HSBC
initial_hsbc <- hsbc$Close[1]
final_hsbc <- hsbc$Close[nrow(hsbc)]

#initial and final values for Lloyd
initial_lloyds <- lloyds$Close[1]
final_lloyds <- lloyds$Close[nrow(lloyds)]

#total growth for HSBC and Lloyd
total_growth_hsbc <- (final_hsbc - initial_hsbc) / initial_hsbc
total_growth_lloyds <- (final_lloyds - initial_lloyds) / initial_lloyds

#total growth for both stocks
cat("Total Growth for HSBC:", total_growth_hsbc, "\n")
cat("Total Growth for Lloyds:", total_growth_lloyds, "\n")


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
data_split <- split_data(hsbc$Close, lloyds$Close, train_proportion = 0.8, seed = 100)

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


#scatter plot for both training and testing data
plot(data_split$X_train, data_split$y_train, 
     main = "Scatter Plot - Training and Testing Data",
     xlab = "HSBC Close Price", ylab = "Lloyds Close Price", col = "blue", pch = 16,
     xlim = c(min(data_split$X_train, data_split$X_test), max(data_split$X_train, data_split$X_test)),
     ylim = c(min(data_split$y_train, data_split$y_test), max(data_split$y_train, data_split$y_test)))

points(data_split$X_test, data_split$y_test, col = "red", pch = 16)
legend("topright", legend = c("Training Data", "Testing Data"), col = c("blue", "red"), pch = 16)
grid()

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
  
  #regularized linear regression (Lasso)
  #l1 regularization (Lasso)
  alpha <- 1  
  #regularization strength
  lambda <- 0.0001  
  
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
  #change the number of rows and columns as needed
  par(mfrow=c(3, length(degrees))) 
  
  for (degree in degrees) {
    actual <- data_split$y_test
    predicted <- results[[paste("Degree", degree)]]$Predicted
    residuals <- actual - predicted
    
    #plot residual histogram for each degree
    hist(residuals, main = paste("Residuals (Degree", degree, ")", sep = " "), xlab = "Residuals")
    grid()
    
    #plot residual vs. fitted values scatter plot for each degree
    plot(x = predicted, y = residuals, main = paste("Residuals vs. Fitted (Degree", degree, ")", sep = " "), xlab = "Fitted Values", ylab = "Residuals")
    #adding a horizontal line at y=0
    abline(h = 0, col = "red", lty = 2)  
    grid()
    
    #plot a Q-Q plot for each degree to check for normality
    qqnorm(residuals, main = paste("Q-Q Plot (Degree", degree, ")", sep = " "))
    qqline(residuals, col='red')
    grid()
  }
}

#residual analysis
residual_analysis(degrees, data_split, results)


