# Final logit-lasso (COVID aware)
rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})
package_dir <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
data_out_dir <- file.path(package_dir, "data", "data_out")

#----------------------------------------------------
library("dplyr")
library("ggplot2")
library("scales")
library("bbmle")
library(foreign)
library(PerformanceAnalytics)
library(remotes)
library(haven)
library(rlang)
library(gtools) #to apply "smartbind" (row binds even when columns do not have same names) 
library(ggplot2)

library(tidyverse) 
library(caret)     #for logit-LASSO
library(glmnet)    #for logit-LASSO
library(ROCR)

library(randomForest)


figu.dir <- "figures/"
data.in <- "data_in/"
data.out <- "data_out/"

# ---------------------------------
load(file.path(data_out_dir, "final_data_19.RData"))

#----------------------------------------------------
options(scipen=999) #remove scientific notation
#----------------------------------------------------
source(file.path(script_dir, "f_locals.R"))   # Import local functions created for this project

#----------------------------------------------------
# REMOVE ALL CONSTANT COLUMNS
#select only numeric/factor columns
nums <- unlist(lapply(final_data_19, is.numeric))
fact <- unlist(lapply(final_data_19, is.factor))
#subset columns from original data into numeric and factor
nums <- final_data_19[ , nums]
fact <- final_data_19[ , fact]
#Drop constant variables (checking whether the maximum is equal to the minimum is sufficient)
fact <- fact[,!apply(fact, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
#Create back the dataset final_data_19 (without constant columns)
final_data_19 <- cbind(nums,fact)



##generate interactions between size and (industry,sector,via,iso)
interaction_size <- function(df){
  df <- df
  df <- type.convert(df)    # still some factor variabeles are treated as integer (e.g. via) and others are numeric (e.g. iso_...)
  #create vector of column names that you want to multiply with size
  cols <- grep('industry|sector|via|iso', names(df), value = TRUE)
  #convert factor to integer and multiply it with all the columns to create new columns
  df[paste0('size_', cols)] <- as.integer(df$size) * df[cols]
  #Change the classes
  df
}

final_data_19_logit <- interaction_size(final_data_19)
cols_to_factor <- grep('via_|industry_|sector_', names(final_data_19_logit), value = TRUE)
final_data_19_logit[cols_to_factor] <- lapply(final_data_19_logit[cols_to_factor], factor)  ## as.factor() could also be used
#convert integer variables (via_, size_via_, industry_, industry_via, sector, sector_via_) into factor
#cols_to_factor <- grep('via_|industry_|sector_', names(final_data_19_logit), value = TRUE)
#final_data_19_logit[cols_to_factor] <- lapply(final_data_19_logit[cols_to_factor], factor)  ## as.factor() could also be used
#convert export_future to numeric

#final_data_19_logit$export_future <- as.numeric(final_data_19_logit$export_future)

##------------

rm(final_data_19)


# OPTIMIZE HYPERPARAMETERS LASSO, ELASTIC, RIDGE AND RF for companies in 2019 (no-COVID), every month -------------------
set.seed(123)

filters <- as.character(unique(final_data_19_logit$month))
desired_length <- length(filters)

lasso.lambda.months.19 <- vector(mode = "list", length = desired_length)  #generate empty 
names(lasso.lambda.months.19) <- filters

ridge.lambda.months.19 <- lasso.lambda.months.19
names(ridge.lambda.months.19) <- filters

library(doParallel)
cores <- parallel::detectCores() - 1
registerDoParallel(cores = cores)
library(janitor)

# LASSO + RIDGE OPTIMIZATION
for (i in seq_along(filters)) {
  data_logit <- final_data_19_logit %>%
    filter(month == filters[[i]]) %>% select(-id, -month, -year)   
  
  data_logit <- remove_constant(data_logit)
  x.data <- sparse.model.matrix(export_future~., data_logit)[,-1]
  y.data <- data_logit$export_future
  
  #has_NA = apply(is.na(x.data), 1, any) #= 1 if any column in that row is NA
  #x.data = x.data[!has_NA,]
  #y.data = y.data[!has_NA,]
   
  lasso.fit <- cv.glmnet(x.data, y.data, type.measure = "auc",
                         alpha = 1, family = "binomial",  #alpha=1 stands for LASSO
                         nfolds = 5, standardize = TRUE, parallel=TRUE, nlambda = 30)
  
  gc()
  
  lasso.lambda.months.19[[i]] = lasso.fit$lambda.1se
  rm(data_logit, lasso.fit, x.data, y.data)
  
  gc()
}
proc.time()

save(lasso.lambda.months.19, file = file.path(script_dir, "lasso.lambda.months.19.RData"))



###### SAM:
# Final logit-lasso (COVID aware)
rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})
package_dir <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
data_out_dir <- file.path(package_dir, "data", "data_out")

#----------------------------------------------------
library("dplyr")
library("ggplot2")
library("scales")
library("bbmle")
library(foreign)
library(PerformanceAnalytics)
library(remotes)
library(haven)
library(rlang)
library(gtools) #to apply "smartbind" (row binds even when columns do not have same names) 
library(ggplot2)

library(tidyverse) 
library(caret)     #for logit-LASSO
library(glmnet)    #for logit-LASSO
library(ROCR)
library(janitor)
library(randomForest)


figu.dir <- "figures/"
data.in <- "data_in/"
data.out <- "data_out/"

# ---------------------------------
load(file.path(data_out_dir, "final_data_19.RData"))

#----------------------------------------------------
options(scipen=999) #remove scientific notation
#----------------------------------------------------
source(file.path(script_dir, "f_locals.R"))   # Import local functions created for this project

#----------------------------------------------------
# REMOVE ALL CONSTANT COLUMNS
#select only numeric/factor columns
nums <- unlist(lapply(final_data_19, is.numeric))
fact <- unlist(lapply(final_data_19, is.factor))
#subset columns from original data into numeric and factor
nums <- final_data_19[ , nums]
fact <- final_data_19[ , fact]
#Drop constant variables (checking whether the maximum is equal to the minimum is sufficient)
fact <- fact[,!apply(fact, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
#Create back the dataset final_data_19 (without constant columns)
final_data_19 <- cbind(nums,fact)



##generate interactions between size and (industry,sector,via,iso)
interaction_size <- function(df){
  df <- df
  df <- type.convert(df)    # still some factor variabeles are treated as integer (e.g. via) and others are numeric (e.g. iso_...)
  #create vector of column names that you want to multiply with size
  cols <- grep('industry|sector|via|iso', names(df), value = TRUE)
  #convert factor to integer and multiply it with all the columns to create new columns
  df[paste0('size_', cols)] <- as.integer(df$size) * df[cols]
  #Change the classes
  df
}

final_data_19_logit <- interaction_size(final_data_19)
cols_to_factor <- grep('via_|industry_|sector_', names(final_data_19_logit), value = TRUE)
final_data_19_logit[cols_to_factor] <- lapply(final_data_19_logit[cols_to_factor], factor)  ## as.factor() could also be used
#convert integer variables (via_, size_via_, industry_, industry_via, sector, sector_via_) into factor
#cols_to_factor <- grep('via_|industry_|sector_', names(final_data_19_logit), value = TRUE)
#final_data_19_logit[cols_to_factor] <- lapply(final_data_19_logit[cols_to_factor], factor)  ## as.factor() could also be used
#convert export_future to numeric

#final_data_19_logit$export_future <- as.numeric(final_data_19_logit$export_future)

rm(final_data_19)

load(file.path(script_dir, "lasso.lambda.months.19.RData"))


library(glmnet) #required for LASSO, Ridge and Elastic net regression

# generate structure to save intermediate results (one prediction per fold)
k <- 5 
cv <- k

# generate structure to save final results (one prediction per month)

lasso_preds_19 <- vector(mode = "list", length = k) #predictions for each fold in every month
months <- as.character(unique(final_data_19_logit$month))
desired_length <- length(months)

lasso_preds_df_19 <- vector(mode = "list", length = desired_length) #predictions aggregated by folds, for each month
names(lasso_preds_df_19) <- months


library(caTools)
library(precrec)

for (j in seq_along(months)) {
  final_df <- final_data_19_logit %>%   #Doesnt matter to use "final_data_19_logit" or "final_data_19_rf" because they have same number of rows
    filter(month == months[[j]])
  
  ## Manual folds
  #Create 5 equally size folds
  folds <- as.data.frame(runif(nrow(final_df))) #first random uniform variable
  folds <- folds %>%
    rename(folds = "runif(nrow(final_df))")
  quantile = quantile(folds, seq(0, 1, 0.20), na.rm = TRUE)
  quantile[1] = 0.99*quantile[1] ## Make the lower bound slightly smaller so the smallest value is classified
  quantile[6] = 1.01*quantile[6] ## Make the upper bound slightly larger so the largest value is classified
  quantile
  folds <- folds %>%      #Mirar "cut" function para hacer esto
    mutate(folds = ifelse(folds > quantile[5] & folds <= quantile[6], 5,
                          ifelse(folds > quantile[4] & folds <= quantile[5], 4,
                                 ifelse(folds > quantile[3] & folds <= quantile[4], 3,
                                        ifelse(folds > quantile[2] & folds <= quantile[3], 2,1
                                        )
                                 )
                          )
    ))
  table(folds$folds)
  rm(quantile)
  
  
  lasso.lambda <- lasso.lambda.months.19[[j]]  #optimal LASSO lambda for this given month
   
  for(i in 1:k){
    #Segment your data by fold using the which() function 
    testIndexes <- which(folds$folds==i,arr.ind=TRUE)
    
    testData <- final_df[testIndexes, ]
    test_id_month <- testData %>%
      select(id, month, export_future)
    testData <- testData %>% select(-id, -year, -month)
    testData<- remove_constant(testData)
    x.test_full <- sparse.model.matrix(export_future~., testData)[,-1]
    y.test <- testData$export_future

    trainData <- final_df[-testIndexes, ]
    trainData <- trainData %>% select(-id, -year, -month)
    trainData<- remove_constant(trainData)
    x.train <- sparse.model.matrix(export_future~., trainData)[,-1]
    y.train <- trainData$export_future
    
    train_cols <- colnames(x.train)
    common_cols <- intersect(colnames(x.test_full), train_cols)
    x.test <- x.test_full[, common_cols, drop = FALSE]
    x.train <- x.train[, common_cols, drop = FALSE]
    

    lasso.fit_19 <- glmnet(x.train, y.train, type.measure="auc",    
                           alpha = 1, family = "binomial",         # alpha=1 stands for LASSO
                           standardize = T)          
    lasso.pred_19 <- predict(lasso.fit_19, s=lasso.lambda, type= "response",newx=x.test)       # Out-sample predictions / lambda.1se is the value resulting from model with fewest non-zero parameters and was within 1 std error of the lambda that had the smallest sum
    
    
    #colnames(lasso.pred_19) <- "pred"
    #lasso_preds_19[[i]] <- lasso.pred_19
    
    pred_vec <- as.numeric(lasso.pred_19)
    
    # Combine id, month, outcome and prediction in one data frame
    lasso_preds_19[[i]] <- test_id_month %>%
      mutate(pred = pred_vec)
    
    
  }
  lasso.preds_df_19 <- do.call(rbind, lasso_preds_19)    # from list to dataframe
  lasso.preds_df_19 <- as.data.frame(lasso.preds_df_19)  # matrix -> data.frame
  
  lasso.preds_df_19 <- lasso.preds_df_19 %>%
    mutate(pred_class = ifelse(pred >= 0.5, 1, 0),
           pred_class = as.factor(as.character(pred_class)))
  lasso_preds_df_19[[j]] <- lasso.preds_df_19
  
}
proc.time()

lasso_preds <- do.call(rbind, lasso_preds_df_19)      # Integrate lists on a single dataframe

lasso_preds_19 <- lasso_preds %>%
  group_by(id, month) %>%
  distinct()


save(lasso_preds_19, file = file.path(script_dir, "lasso_preds_19.RData"))



