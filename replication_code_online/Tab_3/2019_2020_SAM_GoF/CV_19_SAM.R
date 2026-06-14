# Final logit-lasso (COVID aware)
rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})
repo_dir <- normalizePath(file.path(script_dir, "..", ".."), mustWork = TRUE)
data_out_dir <- file.path(repo_dir, "data", "data_out")

# ----------------------------------------------------
# Libraries
# ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)
library(bbmle)
library(foreign)
library(PerformanceAnalytics)
library(remotes)
library(haven)
library(rlang)
library(gtools)
library(tidyverse)
library(caret)
library(glmnet)
library(ROCR)
library(randomForest)
library(janitor)
library(doParallel)

# ----------------------------------------------------
# Set Directories
# ----------------------------------------------------
setwd(script_dir)

figu.dir <- "figures/"
data.in <- "data_in/"
data.out <- "data_out/"
rf.simulations <- "rf_simulations"

options(scipen = 999)

source("f_locals.R")   # must contain remove_constant()

# ----------------------------------------------------
# Helper functions
# ----------------------------------------------------

drop_constant_factor_cols <- function(df) {
  nums_idx <- unlist(lapply(df, is.numeric))
  fact_idx <- unlist(lapply(df, is.factor))
  
  nums <- df[, nums_idx, drop = FALSE]
  fact <- df[, fact_idx, drop = FALSE]
  
  if (ncol(fact) > 0) {
    fact <- fact[, !apply(fact, 2, function(x) {
      max(x, na.rm = TRUE) == min(x, na.rm = TRUE)
    }), drop = FALSE]
  }
  
  cbind(nums, fact)
}

interaction_size <- function(df) {
  df <- type.convert(df, as.is = TRUE)
  cols <- grep("industry|sector|via|iso", names(df), value = TRUE)
  if ("size" %in% names(df) && length(cols) > 0) {
    df[paste0("size_", cols)] <- as.integer(df$size) * df[cols]
  }
  df
}

extract_glmnet_pred <- function(pred_obj) {
  as.numeric(pred_obj[, 1])
}

bind_prediction <- function(id_df, pred_vec) {
  out <- data.frame(id_df, pred = as.numeric(pred_vec))
  out
}

# ====================================================
# PART 1: OPTIMIZE LASSO / RIDGE HYPERPARAMETERS IN 2019
# ====================================================

load(file.path(data_out_dir, "final_data_19.RData"))

final_data_19 <- drop_constant_factor_cols(final_data_19)

final_data_19_logit <- interaction_size(final_data_19)

# keep export_future numeric
final_data_19_logit$export_future <- as.numeric(as.character(final_data_19_logit$export_future))

filters <- sort(unique(as.character(final_data_19_logit$month)))

lasso.lambda.months.19 <- vector("list", length(filters))
ridge.lambda.months.19 <- vector("list", length(filters))
names(lasso.lambda.months.19) <- filters
names(ridge.lambda.months.19) <- filters

cores <- max(1, parallel::detectCores() - 1)
registerDoParallel(cores = cores)

set.seed(123)

for (i in seq_along(filters)) {
  cat("Optimizing month:", filters[[i]], "\n")
  
  data_logit <- final_data_19_logit %>%
    filter(month == filters[[i]]) %>%
    dplyr::select(-id, -month)
  
  data_logit <- remove_constant(data_logit)
  
  x.data <- sparse.model.matrix(export_future ~ ., data_logit)[, -1, drop = FALSE]
  y.data <- data_logit$export_future
  
  lasso.fit <- cv.glmnet(
    x.data, y.data,
    type.measure = "auc",
    alpha = 1,
    family = "binomial",
    nfolds = 5,
    standardize = TRUE,
    parallel = TRUE,
    nlambda = 30
  )
  
  ridge.fit <- cv.glmnet(
    x.data, y.data,
    type.measure = "auc",
    alpha = 0,
    family = "binomial",
    nfolds = 5,
    standardize = TRUE,
    parallel = TRUE,
    nlambda = 30,
    thresh = 1e-2
  )
  
  lasso.lambda.months.19[[filters[[i]]]] <- lasso.fit$lambda.1se
  ridge.lambda.months.19[[filters[[i]]]] <- ridge.fit$lambda.1se
  
  rm(data_logit, x.data, y.data, lasso.fit, ridge.fit)
  gc()
}

stopImplicitCluster()

save(lasso.lambda.months.19, file = "lasso.lambda.months.19.RData")
save(ridge.lambda.months.19, file = "ridge.lambda.months.19.RData")

# ====================================================
# PART 2: COVID-AWARE PREDICTIONS IN 2019
# ====================================================

rm(list = ls())

library(dplyr)
library(ggplot2)
library(scales)
library(bbmle)
library(foreign)
library(PerformanceAnalytics)
library(remotes)
library(haven)
library(rlang)
library(gtools)
library(tidyverse)
library(caret)
library(glmnet)
library(ROCR)
library(janitor)
library(randomForest)

setwd(script_dir)

options(scipen = 999)
source("f_locals.R")

drop_constant_factor_cols <- function(df) {
  nums_idx <- unlist(lapply(df, is.numeric))
  fact_idx <- unlist(lapply(df, is.factor))
  
  nums <- df[, nums_idx, drop = FALSE]
  fact <- df[, fact_idx, drop = FALSE]
  
  if (ncol(fact) > 0) {
    fact <- fact[, !apply(fact, 2, function(x) {
      max(x, na.rm = TRUE) == min(x, na.rm = TRUE)
    }), drop = FALSE]
  }
  
  cbind(nums, fact)
}

interaction_size <- function(df) {
  df <- type.convert(df, as.is = TRUE)
  cols <- grep("industry|sector|via|iso", names(df), value = TRUE)
  if ("size" %in% names(df) && length(cols) > 0) {
    df[paste0("size_", cols)] <- as.integer(df$size) * df[cols]
  }
  df
}

extract_glmnet_pred <- function(pred_obj) {
  as.numeric(pred_obj[, 1])
}

bind_prediction <- function(id_df, pred_vec) {
  data.frame(id_df, pred = as.numeric(pred_vec))
}

load(file.path(data_out_dir, "final_data_19.RData"))

final_data_19 <- drop_constant_factor_cols(final_data_19)

final_data_19_logit <- interaction_size(final_data_19)
final_data_19_logit$export_future <- as.numeric(as.character(final_data_19_logit$export_future))

final_data_19_rf <- type.convert(final_data_19, as.is = TRUE)
final_data_19_rf$export_future <- as.numeric(as.character(final_data_19_rf$export_future))

rm(final_data_19)

load(file.path(script_dir, "lasso.lambda.months.19.RData"))
load(file.path(script_dir, "ridge.lambda.months.19.RData"))

k <- 5

months <- sort(unique(as.character(final_data_19_logit$month)))

lasso_preds_19 <- vector("list", length = k)
ridge_preds_19 <- vector("list", length = k)
rf_preds_19    <- vector("list", length = k)
logit_preds_19 <- vector("list", length = k)

lasso_preds_df_19 <- vector("list", length = length(months))
ridge_preds_df_19 <- vector("list", length = length(months))
rf_preds_df_19    <- vector("list", length = length(months))
logit_preds_df_19 <- vector("list", length = length(months))

names(lasso_preds_df_19) <- months
names(ridge_preds_df_19) <- months
names(rf_preds_df_19)    <- months
names(logit_preds_df_19) <- months

set.seed(123)

for (j in seq_along(months)) {
  cat("Predicting month:", months[[j]], "\n")
  
  final_df <- final_data_19_logit %>%
    filter(month == months[[j]])
  
  final_df_rf <- final_data_19_rf %>%
    filter(month == months[[j]])
  
  # manual folds
  folds <- data.frame(u = runif(nrow(final_df)))
  qs <- quantile(folds$u, seq(0, 1, 0.20), na.rm = TRUE)
  qs[1] <- 0.99 * qs[1]
  qs[6] <- 1.01 * qs[6]
  
  folds$folds <- ifelse(folds$u > qs[5] & folds$u <= qs[6], 5,
                        ifelse(folds$u > qs[4] & folds$u <= qs[5], 4,
                               ifelse(folds$u > qs[3] & folds$u <= qs[4], 3,
                                      ifelse(folds$u > qs[2] & folds$u <= qs[3], 2, 1))))
  
  lasso.lambda <- lasso.lambda.months.19[[months[[j]]]]
  ridge.lambda <- ridge.lambda.months.19[[months[[j]]]]
  
  for (i in 1:k) {
    testIndexes <- which(folds$folds == i)
    
    # ---------------------------
    # LASSO / RIDGE / LOGIT
    # ---------------------------
    testData <- final_df[testIndexes, ]
    test_id_month <- testData %>%
      dplyr::select(id, month, export_future)
    
    testData <- testData %>% dplyr::select(-id, -month)
    testData <- remove_constant(testData)
    
    x.test.full <- sparse.model.matrix(export_future ~ ., testData)[, -1, drop = FALSE]
    
    trainData <- final_df[-testIndexes, ]
    trainData <- trainData %>% dplyr::select(-id, -month)
    trainData <- remove_constant(trainData)
    
    x.train <- sparse.model.matrix(export_future ~ ., trainData)[, -1, drop = FALSE]
    y.train <- trainData$export_future
    
    common_cols <- intersect(colnames(x.test.full), colnames(x.train))
    x.test <- x.test.full[, common_cols, drop = FALSE]
    x.train <- x.train[, common_cols, drop = FALSE]
    
    lasso.fit_19 <- glmnet(
      x.train, y.train,
      alpha = 1,
      family = "binomial",
      standardize = TRUE
    )
    lasso.pred_19 <- predict(
      lasso.fit_19,
      s = lasso.lambda,
      type = "response",
      newx = x.test
    )
    lasso.pred_19 <- extract_glmnet_pred(lasso.pred_19)
    
    ridge.fit_19 <- glmnet(
      x.train, y.train,
      alpha = 0,
      family = "binomial",
      standardize = TRUE
    )
    ridge.pred_19 <- predict(
      ridge.fit_19,
      s = ridge.lambda,
      type = "response",
      newx = x.test
    )
    ridge.pred_19 <- extract_glmnet_pred(ridge.pred_19)
    
    common_variables_logit2 <- intersect(names(trainData), names(testData))
    trainData2 <- trainData[, common_variables_logit2, drop = FALSE]
    testData2  <- testData[, common_variables_logit2, drop = FALSE]
    
    logit.fit_19 <- glm(export_future ~ ., data = trainData2, family = "binomial")
    logit.pred_19 <- predict(logit.fit_19, newdata = testData2, type = "response")
    
    # ---------------------------
    # RF
    # ---------------------------
    testData_rf <- final_df_rf[testIndexes, ]
    testData_rf <- testData_rf %>% dplyr::select(-id, -month)
    
    trainData_rf <- final_df_rf[-testIndexes, ]
    trainData_rf <- trainData_rf %>% dplyr::select(-id, -month)
    
    trainData_rf <- remove_constant(trainData_rf)
    testData_rf  <- remove_constant(testData_rf)
    
    common_variables_rf <- intersect(names(trainData_rf), names(testData_rf))
    trainData_rf <- trainData_rf[, common_variables_rf, drop = FALSE]
    testData_rf  <- testData_rf[, common_variables_rf, drop = FALSE]
    
    rf.fit_19 <- randomForest(export_future ~ ., data = trainData_rf, ntree = 500)
    rf.pred_19 <- predict(rf.fit_19, newdata = testData_rf)
    
    # ---------------------------
    # Save fold predictions
    # ---------------------------
    lasso_preds_19[[i]] <- bind_prediction(test_id_month, lasso.pred_19)
    ridge_preds_19[[i]] <- bind_prediction(test_id_month, ridge.pred_19)
    rf_preds_19[[i]]    <- bind_prediction(test_id_month, rf.pred_19)
    logit_preds_19[[i]] <- bind_prediction(test_id_month, logit.pred_19)
  }
  
  lasso_preds_df_19[[j]] <- do.call(rbind, lasso_preds_19) %>%
    mutate(pred_class = factor(ifelse(pred >= 0.5, 1, 0)))
  
  ridge_preds_df_19[[j]] <- do.call(rbind, ridge_preds_19) %>%
    mutate(pred_class = factor(ifelse(pred >= 0.5, 1, 0)))
  
  rf_preds_df_19[[j]] <- do.call(rbind, rf_preds_19) %>%
    mutate(pred_class = factor(ifelse(pred >= 0.5, 1, 0)))
  
  logit_preds_df_19[[j]] <- do.call(rbind, logit_preds_19) %>%
    mutate(pred_class = factor(ifelse(pred >= 0.5, 1, 0)))
}

lasso_preds_19 <- do.call(rbind, lasso_preds_df_19) %>%
  rename(pred_19 = pred, pred_class_19 = pred_class) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

ridge_preds_19 <- do.call(rbind, ridge_preds_df_19) %>%
  rename(pred_19 = pred, pred_class_19 = pred_class) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

rf_preds_19 <- do.call(rbind, rf_preds_df_19) %>%
  rename(pred_19 = pred, pred_class_19 = pred_class) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

logit_preds_19 <- do.call(rbind, logit_preds_df_19) %>%
  rename(pred_19 = pred, pred_class_19 = pred_class) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

save(lasso_preds_19, file = "lasso_preds_19.RData")
save(ridge_preds_19, file = "ridge_preds_19.RData")
save(rf_preds_19, file = "rf_preds_19.RData")
save(logit_preds_19, file = "logit_preds_19.RData")
