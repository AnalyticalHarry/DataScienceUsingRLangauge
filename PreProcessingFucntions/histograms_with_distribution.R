library(ggplot2)
library(gridExtra)

histograms_with_distribution <- function(df, columns, plots_per_row = 4) {
  num_plots <- length(columns)
  nrows <- ceiling(num_plots / plots_per_row)
  plot_list <- list()

  # Iterate over columns and create a histogram for each
  for (i in 1:num_plots) {
    p <- ggplot(df, aes_string(x = columns[i])) +
           geom_histogram(binwidth = 30, color = "grey", fill = "black") +
           geom_density(alpha = 0.2, fill = "cyan") +
           ggtitle(paste("Histogram of", columns[i])) +
           labs(x = columns[i], y = "Frequency") +
           theme_minimal() +
           theme(plot.title = element_text(hjust = 0.5))
    
    plot_list[[i]] <- p
  }

  grid.arrange(grobs = plot_list, ncol = plots_per_row)
}

histograms_with_distribution(df, names(df))
