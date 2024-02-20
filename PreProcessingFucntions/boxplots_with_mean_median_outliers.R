boxplots_with_mean_median_outliers <- function(df, columns, plots_per_row = 4) {
  num_plots <- length(columns)
  nrows <- ceiling(num_plots / plots_per_row)
  plot_width <- plots_per_row * 6
  plot_height <- nrows * 4
  
  par(mfrow = c(nrows, plots_per_row), mar = c(4, 4, 2, 1))
  
  for (i in 1:num_plots) {
    if (i <= num_plots) {
      boxplot(df[[columns[i]]], main = paste("Box Plot of", columns[i]), col = "black", outline = TRUE, horizontal = TRUE)
      abline(v = mean(df[[columns[i]]]), col = "red", lwd = 2, lty = 2, labels = paste("Mean (", round(mean(df[[columns[i]]]), 2), ")"))
      abline(v = median(df[[columns[i]]]), col = "cyan", lty = 3, labels = paste("Median (", round(median(df[[columns[i]]]), 2), ")"))
      grid(nx = NULL, ny = NULL, lty = 3, col = "grey", lwd = par("lwd"))
    }
  }
}

# boxplots_with_mean_median_outliers(df, colnames(df))
