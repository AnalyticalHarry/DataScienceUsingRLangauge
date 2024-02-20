library(ggplot2)
library(corrplot)

# combine numeric columns with one-hot encoded categorical columns
numeric_cols <- df[, sapply(df, is.numeric)]
categorical_cols <- df[, sapply(df, function(x) !is.numeric(x))]
one_hot_encoded <- model.matrix(~ 0 + categorical_cols)

combined_df <- cbind(numeric_cols, one_hot_encoded)

# correlation matrix
correlation_matrix <- cor(combined_df)
                                
corrplot(correlation_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(10), 
         addCoef.col = "black", number.cex = 0.7)
