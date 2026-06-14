# In this code we generate predictions from SUM (SHOCK UNAWARE MACHINE)
# We take optimal parameters obtained from CV in 2018
rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})
package_dir <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
data_out_dir <- file.path(package_dir, "data", "data_out")

# Set Directories -------------------------------------------------------------

figu.dir <- "figures/"
data.in <- "data_in/"
data.out <- "data_out/"
# ---------------------------------

library(dplyr)
library(caret)    
library(glmnet)    #for logit-LASSO/RIDGE
library(randomForest)    #for Random Forest


#----------------------------------------------------
options(scipen=999) #remove scientific notation
#----------------------------------------------------
source(file.path(script_dir, "f_locals.R"))   # Import local functions created for this project


# Load predictions by each of the 3 models (LASSO, RIDGE, RF)
load(file.path(data_out_dir, "final_data_18.RData"))
load(file.path(data_out_dir, "final_data_19.RData"))


# Load optimal values of lamdba in 2018 (optimal number of trees in 2018 is n=500)
load(file.path(script_dir, "lasso.lambda.months.18.RData"))

# Prepare dataset for LOGIT (RIDGE; LASSO) and for RF
final_data_18$month <- as.factor(final_data_18$month)
final_data_19$month <- as.factor(final_data_19$month)



#----------------------------------------------------
# REMOVE ALL CONSTANT COLUMNS
# 2018
#select only numeric/factor columns
nums <- unlist(lapply(final_data_18, is.numeric))
fact <- unlist(lapply(final_data_18, is.factor))
#subset columns from original data into numeric and factor
nums <- final_data_18[ , nums]
fact <- final_data_18[ , fact]
#Drop constant variables (checking whether the maximum is equal to the minimum is sufficient)
fact <- fact[,!apply(fact, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
#Create back the dataset final_data_18 (without constant columns)
final_data_18 <- cbind(nums,fact)
# 2019
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
  df <- type.convert(df)
  #create vector of column names that you want to multiply with size
  cols <- grep('industry|sector|via|iso', names(df), value = TRUE)
  #convert factor to integer and multiply it with all the columns to create new columns
  df[paste0('size_', cols)] <- as.integer(df$size) * df[cols]
  #Change the classes
  df
}

final_data_18_logit <- interaction_size(final_data_18)
final_data_19_logit <- interaction_size(final_data_19)

#convert integer variables (via_, size_via_, industry_, industry_via, sector, sector_via_) into factor
cols_to_factor <- grep('via_|industry_|sector_', names(final_data_19_logit), value = TRUE)
final_data_19_logit[cols_to_factor] <- lapply(final_data_19_logit[cols_to_factor], factor)  ## as.factor() could also be used
cols_to_factor <- grep('via_|industry_|sector_', names(final_data_18_logit), value = TRUE)
final_data_18_logit[cols_to_factor] <- lapply(final_data_18_logit[cols_to_factor], factor)  ## as.factor() could also be used

##------------
rm(final_data_18, final_data_19)


# Choose only intersection of variables in the two different years 
# (otherwise cant estimate by prediting and training on different data)
common_variables_logit <- intersect(names(final_data_18_logit), names(final_data_19_logit))
df_train_logit <- final_data_18_logit[ , common_variables_logit]
df_test_logit <- final_data_19_logit[ , common_variables_logit]


# Generate empty lists to save predictions  
filters <- as.character(unique(final_data_18_logit$month))
desired_length <- length(filters)

SUM_preds_lasso <- vector(mode = "list", length = desired_length)  #generate empty list to save SUM predictions for LASSO
names(SUM_preds_lasso) <- filters

set.seed(2021)
library(janitor)

for (i in seq_along(filters)) {
  lasso.lambda <- lasso.lambda.months.18[[i]]  #optimal LASSO lambda for this given month
  

  ### Predictions for Logit: Lasso, Ridge and Logit###
  df_test_logit_loop <- df_test_logit %>%
    filter(month == filters[[i]])
  
  companies_logit <- df_test_logit_loop %>%
    dplyr::select(month, id, export_future)    # Create a data saving each id, month and export_future
  
  df_train_logit_loop <- df_train_logit %>%
    filter(month == filters[[i]])
  
  df_train_logit_loop <- df_train_logit_loop %>% dplyr::select(-id, -month)
  df_test_logit_loop <- df_test_logit_loop %>% dplyr::select(-id, -month)
  

  df_test_logit_loop <-remove_constant(df_test_logit_loop)
  df_train_logit_loop <-remove_constant(df_train_logit_loop)
  
  x.train.loop <- sparse.model.matrix(export_future~., df_train_logit_loop)[,-1]
  y.train.loop <- df_train_logit_loop$export_future
  
  train_cols <- colnames(x.train.loop)
  
  x.test.full <- sparse.model.matrix(export_future~., df_test_logit_loop)[,-1]
  y.test.loop <- df_test_logit_loop$export_future

  common_cols <- intersect(colnames(x.test.full), train_cols)
  x.test.loop <- x.test.full[, common_cols, drop = FALSE]
  x.train.loop <- x.train.loop[, common_cols, drop = FALSE]
  
  lasso.fit_SUM <- glmnet(x.train.loop, y.train.loop, type.measure="auc",    
                         alpha = 1, family = "binomial",         # alpha=1 stands for LASSO
                         standardize = T)    
  lasso.pred_SUM <- predict(lasso.fit_SUM, s=lasso.lambda, type= "response",newx=x.test.loop)       # Out-sample predictions / lambda.1se is the value resulting from model with fewest non-zero parameters and was within 1 std error of the lambda that had the smallest sum
  colnames(lasso.pred_SUM) <- "pred"
  
  #common_variables_logit2 <- intersect(names(df_train_logit_loop), names(df_test_logit_loop))
  #df_train_logit_loop <- df_train_logit_loop[ , common_variables_logit2]
  #df_test_logit_loop <- df_test_logit_loop[ , common_variables_logit2]
  
  
  ## Join predictions with original observations ##
  lasso.pred_SUM <- cbind(companies_logit, lasso.pred_SUM)
  
  
  ### Save the predictions of SUM (for the 3 models) ##
  
  
  SUM_preds_lasso[[i]] <- lasso.pred_SUM

  
}

SUM_preds_lasso <- do.call(rbind, SUM_preds_lasso)    # from list to dataframe
SUM_preds_lasso <- SUM_preds_lasso %>%
  mutate(pred_class = ifelse(pred >= 0.5, 1, 0),
         pred_class = as.factor(as.character(pred_class)))



save(SUM_preds_lasso, file = file.path(script_dir, "SUM_preds_lasso.RData"))

proc.time()

#load("data_out/SUM_preds_lasso.RData")
#load("data_out/SUM_preds_ridge.RData")
#load("data_out/SUM_preds_rf.RData")
#load("data_out/SUM_preds_logit.RData")


#1) GOODNESS OF FIT FOR ALL MONTHS: 2018_CV  <- Choose the best model
#2) GOODNESS OF FIT FOR ALL MONTHS: 2018/19_SUM
#3) GOODNESS OF FIT FOR ALL MONTHS: 2019_CV_SAM
#4) ESTIMATE TREATMENT EFFECT (with the best model)



