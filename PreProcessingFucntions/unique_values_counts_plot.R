library(ggplot2)

unique_values_counts_plot <- function(data, columns) {
  num_columns <- length(columns)
  num_rows <- ceiling((num_columns + 2) / 3)
  plot_width <- 16
  plot_height <- 4 * num_rows
  
  par(mfrow = c(num_rows, 3), mar = c(4, 4, 2, 1))
  
  for (i in 1:num_columns) {
    if (columns[i] %in% names(data)) {
      processed_data <- as.character(data[[columns[i]]])
      counts <- table(processed_data)
      sorted_counts <- sort(counts)
      keys <- names(sorted_counts)
      values <- as.numeric(sorted_counts)
      
      p <- ggplot(data.frame(keys, values), aes(x = keys, y = values)) +
             geom_bar(stat = "identity", fill = "black", alpha = 0.8) +
             labs(title = paste("Unique Value Counts for", columns[i]),
                  x = "Unique Values", y = "Counts") +
             theme_minimal() +
             theme(axis.text.x = element_text(angle = 60, hjust = 1),
                   axis.title = element_text(size = 10))
      
      print(p)
    } else {
      cat(paste("Column '", columns[i], "' not found in data.\n"))
    }
  }
}

# unique_values_counts_plot(df, c('credit_policy','inq_last_6mths', 'delinq_2yrs', 'pub_rec', 'not_fully_paid'))
