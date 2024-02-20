library(ggplot2)

columns_hist_plot <- function(data, columns) {
  num_cols <- length(columns)
  num_rows <- ceiling(num_cols / 4)
  par(mfrow = c(num_rows, 4), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
  
  for (i in 1:num_cols) {
    hist(data[[columns[i]]], main = paste("Distribution of", columns[i]), xlab = columns[i], 
         ylab = "Frequency", col = "black", border = "grey", breaks = 30)
    abline(v = mean(data[[columns[i]]]), col = "red", lwd = 2, lty = 2, labels = paste("Mean (", round(mean(data[[columns[i]]]), 2), ")"))
    abline(v = median(data[[columns[i]]]), col = "cyan", lwd = 2, lty = 2, labels = paste("Median (", round(median(data[[columns[i]]]), 2), ")"))
    grid(nx = NULL, ny = NULL, lty = 3, col = "grey", lwd = par("lwd"))
    if (i %% 4 != 0) {
      par(mar = c(4, 4, 2, 1) + 0.1)
    }
  }
}

columns_hist_plot(df, names(df))
