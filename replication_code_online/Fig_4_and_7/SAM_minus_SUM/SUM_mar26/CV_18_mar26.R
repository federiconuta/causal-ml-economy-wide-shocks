######### CV 2018
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
# Install necessary libraries

library(caret)
library(randomForest)
library("dplyr")
library("ggplot2")
library("scales")
library("bbmle")
library(Matrix)
library(foreign)
library(PerformanceAnalytics)
library(remotes)
library(haven)
library(rlang)
library(gtools) #to apply "smartbind" (row binds even when columns do not have same names) 
library(ggplot2)
library(pROC)
library(tidyverse) 
library(caret)     #for logit-LASSO
library(glmnet)    #for logit-LASSO
library(ROCR)
library(randomForest)
library(data.table)  # For fast data manipulation
library(dplyr)       # For data manipulation
library(tidyr)       # For grid generation (expand.grid)
library(glmnet)      # For LASSO and Ridge
library(randomForest) # For Random Forest
library(xgboost)     # For XGBoost
library(e1071)       # For SVM (Support Vector Machines)
library(tidyverse) 
library(caret)     #for logit-LASSO
library(glmnet)    #for logit-LASSO
library(ROCR)


load(file.path(data_out_dir, "final_data_18.RData"))

final_data_18$month <- as.factor(final_data_18$month)

#----------------------------------------------------
# REMOVE ALL CONSTANT COLUMNS
#select only numeric/factor columns
nums <- unlist(lapply(final_data_18, is.numeric))
fact <- unlist(lapply(final_data_18, is.factor))
#subset columns from original data into numeric and factor
nums <- final_data_18[ , nums]
fact <- final_data_18[ , fact]
#Drop constant variables (checking whether the maximum is equal to the minimum is sufficient)
fact <- fact[,!apply(fact, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
#Create back the dataset final_data_19 (without constant columns)
final_data_18 <- cbind(nums,fact)


##generate interactions between size and (industry,sector,via,iso)
interaction_size <- function(df){
  df <- df
  df <- type.convert(df)
  #create vector of column names that you want to multiply with size
  cols <- grep('industry|sector|via|iso', names(df), value = TRUE)
  #convert factor to integer and multiply it with all the columns to create new columns
  df[paste0('size_', cols)] <- as.integer(df$size) * df[cols]
  #Change the classes
  df
}

final_data_18_logit <- interaction_size(final_data_18)
#convert integer variables (via_, size_via_, industry_, industry_via, sector, sector_via_) into factor
#cols_to_factor <- grep('via_|industry_|sector_', names(final_data_18_logit), value = TRUE)
#final_data_18_logit[cols_to_factor] <- lapply(final_data_18_logit[cols_to_factor], factor)  ## as.factor() could also be used

#final_data_18_logit$export_future <- as.numeric(final_data_18_logit$export_future)

##------------
final_data_18_rf <- final_data_18    #define dataset for Random Forest (not including interactions because it generates them)

filters <- as.character(unique(final_data_18$month))
desired_length <- length(filters)

lasso.lambda.months.18 <- vector(mode = "list", length = desired_length)  #generate empty 
names(lasso.lambda.months.18) <- filters

ridge.lambda.months.18 <- lasso.lambda.months.18
names(ridge.lambda.months.18) <- filters

library(doParallel)
cores <- parallel::detectCores() - 1
registerDoParallel(cores = cores)


set.seed(123)
for (i in seq_along(filters)) {
  data_logit <- final_data_18 %>%
    filter(month == filters[[i]]) %>% dplyr::select(-id, -month)   
  
  x.data <- sparse.model.matrix(export_future ~ ., data = data_logit)[, -1]
  y.data <- data_logit$export_future
  
  #has_NA = apply(is.na(x.data), 1, any) #= 1 if any column in that row is NA
  #x.data = x.data[!has_NA,]
  #y.data = y.data[!has_NA,]
  
  lasso.fit <- cv.glmnet(x.data, y.data, type.measure = "auc",
                         alpha = 1, family = "binomial",  #alpha=1 stands for LASSO
                         nfolds = 5, standardize = TRUE, parallel=TRUE, nlambda = 30)
  

  gc()
  
  ridge.fit <- cv.glmnet(x.data, y.data, type.measure = "auc",
                         alpha = 0, family = "binomial",  #alpha=0 stands for RIDGE
                         nfolds = 5, standardize = TRUE, parallel = TRUE, nlambda = 30,thresh = 1e-2)
  
  lasso.lambda.months.18[[i]] = lasso.fit$lambda.1se
  ridge.lambda.months.18[[i]] = ridge.fit$lambda.1se
  rm(data_logit, lasso.fit, ridge.fit, x.data, y.data)
  gc()
  
}
proc.time()
stopImplicitCluster()


save(lasso.lambda.months.18, file = file.path(script_dir, "lasso.lambda.months.18.RData"))
save(ridge.lambda.months.18, file = file.path(script_dir, "ridge.lambda.months.18.RData"))



