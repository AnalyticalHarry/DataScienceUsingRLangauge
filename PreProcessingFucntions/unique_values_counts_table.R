unique_values_counts_table <- function(data, columns) {
  tables <- list()
  
  for (column in columns) {
    if (column %in% names(data)) {
      # Counting unique values
      counts <- table(data[[column]])
      
      # Sorting the counts in ascending order
      sorted_counts <- sort(counts)
      
      # Formatting widths
      max_key_length <- max(nchar(names(sorted_counts)))
      max_value_length <- max(nchar(sorted_counts))
      
      # Header and separator
      header <- paste("| ", format(names(sorted_counts), width = max_key_length, justify = "left"), 
                      " | ", "Count", " |", sep = "")
      separator <- paste("+", rep("-", max_key_length + 2), "+", rep("-", max_value_length + 7), "+", sep = "")
      
      # Table rows
      table_rows <- paste("| ", format(names(sorted_counts), width = max_key_length, justify = "left"), 
                          " | ", format(sorted_counts, width = max_value_length, justify = "right"), " |", sep = "")
      
      # Combining all parts of the table
      table <- paste(separator, '\n', header, '\n', separator, '\n', table_rows, '\n', separator, sep = '')
      tables <- c(tables, list(table))
    } else {
      tables <- c(tables, paste("Column '", column, "' not found in data."))
    }
  }
  
  return(paste(tables, collapse = "\n\n"))
}

# Example usage
# print(unique_values_counts_table(df, c('credit_policy', 'inq_last_6mths', 'delinq_2yrs', 'pub_rec', 'not_fully_paid')))
