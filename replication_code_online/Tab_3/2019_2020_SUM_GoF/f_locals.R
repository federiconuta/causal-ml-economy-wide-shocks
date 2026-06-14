# Local functions


#1)  Mode function
calculate_mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#2)  Convert factor to character
factor_to_character <- function(x) {
  switch(x,
         '1'='0',
         '2'='1',
         etc...)
}


# 3) Creates variable date and sector from original data
date_sector_function <- function(df){
  df <- df %>%
    mutate(year = period %/% 100,
           month = period %% 100,
           month = str_pad(month, 2, pad = "0"),
           hs10 = str_pad(hs10, 10, pad = "0"),
           sector = substr(hs10, start = 1, stop = 2),
    )
}

# 4) Incorporate industry variable 
industry_function <- function(df) {
  df <- df %>%
    left_join(., industry_dictionary, by = "sector")
}

# 5) Clean data for main variables
clean_variables <- function(df){
  df <- df %>%
    mutate(year = period %/% 100,
           month = period %% 100,
           hs6 = floor(hs10/10000),
           hs6 = str_pad(hs6, 6, pad="0"),
           sector = floor(hs10/100000000),
           sector = str_pad(sector, 2, pad="0")
    ) %>%
    select(id, period, year, month, iso, via, hs6, sector, region, value_fob)
} 

# Goodness of Fit functions -----------------------------------------------


#5) F1- Score
    # predicted: vector of predicted values
    # expected: vector of observed value
    # positive.class: class of binary predictions we are mostly interested in (e.g., "1", "0")

f1_score <- function(predicted, expected, positive.class) {
  
  # Generate Confusion Matrix
  c.matrix = as.matrix(table(expected, predicted))
  
  # Compute Precision
  precision <- diag(c.matrix) / colSums(c.matrix)
  
  # Compute Recall
  recall <- diag(c.matrix) / rowSums(c.matrix)
  
  # Compute F-1 Score
  f1 <-  ifelse(precision + recall == 0, 0, 2*precision*recall/(precision + recall))
  
  # Extract F1-score for the pre-defined "positive class"
  f1 <- f1[positive.class]
  
  # Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  # Return F1-score
  return(f1)
}

#6) Balanced Accuracy (BACC)
    # predicted: vector of predicted values
    # expected: vector of observed value

balanced_accuracy <- function(predicted, expected) {
  
  # Generate Confusion Matrix
  c.matrix = as.matrix(table(predicted, expected))
  
  # First Row Generation
  first.row <- c.matrix[1,1] / (c.matrix[1,1] + c.matrix[1,2])  
  
  # Second Row Generation
  second.row <- c.matrix[2,2] / (c.matrix[2,1] + c.matrix[2,2])  
  
  # # "Balanced" proportion correct (you can use different weighting if needed)
  acc <- (first.row + second.row)/2 
  
  # Return Balanced Accuracy
  return(acc)
}

#7) BACC when predicting just positive class (only ones)
balanced_accuracy_only_positive_class <- function(predicted, expected) {
  
  # Generate Confusion Matrix
  c.matrix = as.matrix(table(predicted, expected))
  
  # First Row Generation
  first.row <- c.matrix[1,1] / (c.matrix[1,1] + c.matrix[1,2])  
  
  # Second Row Generation
  second.row <- 0  
  
  # # "Balanced" proportion correct (you can use different weighting if needed)
  acc <- (first.row + second.row)/2 
  
  # Return Balanced Accuracy
  return(acc)
}

#8) BACC when predicting just negative class (only zeros)
balanced_accuracy_only_negative_class <- function(predicted, expected) {
  
  # Generate Confusion Matrix
  c.matrix = as.matrix(table(predicted, expected))
  
  # First Row Generation
  first.row <- 0  
  
  # Second Row Generation
  second.row <- c.matrix[2,2] / (c.matrix[2,1] + c.matrix[2,2])  
 
  
  # # "Balanced" proportion correct (you can use different weighting if needed)
  acc <- (first.row + second.row)/2 
  
  # Return Balanced Accuracy
  return(acc)
}