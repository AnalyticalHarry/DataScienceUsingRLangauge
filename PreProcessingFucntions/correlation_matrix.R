library(ggplot2)
library(reshape2) 

# function to create a correlation matrix plot
correlation_matrix <- function(corr_matrix, threshold = 0.7) {
  # reshape correlation matrix for plotting
  corr_matrix_melted <- melt(corr_matrix)  # Use melt from reshape2 package
  
  # heatmap
  ggplot(data = corr_matrix_melted, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), vjust = 1) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "gray",
                         midpoint = 0, limit = c(-1, 1), na.value = "grey50") +
    theme_minimal() +
    coord_equal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Matrix",
         x = "Variables", y = "Variables", fill = "Correlation") +
    geom_rect(data = subset(corr_matrix_melted, abs(value) > threshold),
              aes(xmin = as.numeric(Var1) - 0.5, xmax = as.numeric(Var1) + 0.5,
                  ymin = as.numeric(Var2) - 0.5, ymax = as.numeric(Var2) + 0.5),
              fill = NA, color = "red", size = 1.5) +
    theme(plot.title = element_text(hjust = 0.5))
}

# calcualte correlation 
corr_matrix <- cor(data)

# plot the correlation matrix
correlation_matrix(corr_matrix)

