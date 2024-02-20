before_rows <- nrow(df)

removing_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 2 * IQR
  upper_bound <- Q3 + 2 * IQR
  
  return(data[(data[[column]] >= lower_bound) & (data[[column]] <= upper_bound), ])
}

for (i in names(df)) {
  df <- removing_outliers(df, i)
}

filtered <- nrow(df)
cat(paste("Before shape was", before_rows, "and after removing outliers our shape is", filtered))
