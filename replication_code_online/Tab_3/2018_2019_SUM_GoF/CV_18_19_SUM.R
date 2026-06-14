######### CV 2018
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
library(tidyr)
library(glmnet)
library(randomForest)
library(caret)
library(Matrix)
library(doParallel)

# ----------------------------------------------------
# Helper function: remove constant columns
# ----------------------------------------------------
remove_constant <- function(df) {
  keep <- sapply(df, function(x) length(unique(x[!is.na(x)])) > 1)
  df[, keep, drop = FALSE]
}

# ----------------------------------------------------
# Load data
# ----------------------------------------------------
load(file.path(data_out_dir, "final_data_18.RData"))

final_data_18$month <- as.factor(final_data_18$month)

# ----------------------------------------------------
# REMOVE ALL CONSTANT COLUMNS
# ----------------------------------------------------
nums <- unlist(lapply(final_data_18, is.numeric))
fact <- unlist(lapply(final_data_18, is.factor))

nums <- final_data_18[, nums, drop = FALSE]
fact <- final_data_18[, fact, drop = FALSE]

if (ncol(fact) > 0) {
  fact <- fact[, !apply(fact, 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE)), drop = FALSE]
}

final_data_18 <- cbind(nums, fact)

# ----------------------------------------------------
# Generate interactions between size and (industry, sector, via, iso)
# ----------------------------------------------------
interaction_size <- function(df) {
  df <- type.convert(df, as.is = TRUE)
  
  cols <- grep("industry|sector|via|iso", names(df), value = TRUE)
  
  if ("size" %in% names(df) && length(cols) > 0) {
    df[paste0("size_", cols)] <- as.integer(df$size) * df[cols]
  }
  
  df
}

final_data_18_logit <- interaction_size(final_data_18)
final_data_18_rf <- final_data_18

# ----------------------------------------------------
# Estimate monthly optimal lambdas
# ----------------------------------------------------
filters <- as.character(unique(final_data_18$month))
desired_length <- length(filters)

lasso.lambda.months.18 <- vector(mode = "list", length = desired_length)
names(lasso.lambda.months.18) <- filters

ridge.lambda.months.18 <- vector(mode = "list", length = desired_length)
names(ridge.lambda.months.18) <- filters

cores <- max(1, parallel::detectCores() - 1)
registerDoParallel(cores = cores)

set.seed(123)

for (i in seq_along(filters)) {
  data_logit <- final_data_18 %>%
    filter(month == filters[[i]]) %>%
    dplyr::select(-id, -month)
  
  data_logit <- remove_constant(data_logit)
  
  x.data <- sparse.model.matrix(export_future ~ ., data = data_logit)[, -1, drop = FALSE]
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
  
  lasso.lambda.months.18[[i]] <- lasso.fit$lambda.1se
  ridge.lambda.months.18[[i]] <- ridge.fit$lambda.1se
  
  rm(data_logit, lasso.fit, ridge.fit, x.data, y.data)
  gc()
}

stopImplicitCluster()

save(
  lasso.lambda.months.18,
  file = file.path(script_dir, "lasso.lambda.months.18.RData")
)

save(
  ridge.lambda.months.18,
  file = file.path(script_dir, "ridge.lambda.months.18.RData")
)

# ----------------------------------------------------
# SUM 18/19: prediction step
# ----------------------------------------------------
load(file.path(script_dir, "lasso.lambda.months.18.RData"))
load(file.path(script_dir, "ridge.lambda.months.18.RData"))

final_data_18_rf <- type.convert(final_data_18_rf, as.is = TRUE)

# ----------------------------------------------------
# Structures to save predictions
# ----------------------------------------------------
k <- 5

lasso_preds_18 <- vector("list", length = k)
ridge_preds_18 <- vector("list", length = k)
rf_preds_18    <- vector("list", length = k)
logit_preds_18 <- vector("list", length = k)

months <- as.character(unique(final_data_18_logit$month))
desired_length <- length(months)

lasso_preds_df_18 <- vector("list", length = desired_length)
names(lasso_preds_df_18) <- months

ridge_preds_df_18 <- vector("list", length = desired_length)
names(ridge_preds_df_18) <- months

rf_preds_df_18 <- vector("list", length = desired_length)
names(rf_preds_df_18) <- months

logit_preds_df_18 <- vector("list", length = desired_length)
names(logit_preds_df_18) <- months

set.seed(123)

for (j in seq_along(months)) {
  
  final_df <- final_data_18_logit %>%
    filter(month == months[[j]])
  
  final_df_rf <- final_data_18_rf %>%
    filter(month == months[[j]])
  
  # -----------------------
  # Manual 5-fold split
  # -----------------------
  fold_u <- runif(nrow(final_df))
  qtiles <- quantile(fold_u, probs = seq(0, 1, 0.2), na.rm = TRUE)
  qtiles[1] <- qtiles[1] * 0.99
  qtiles[6] <- qtiles[6] * 1.01
  
  folds <- cut(
    fold_u,
    breaks = qtiles,
    include.lowest = TRUE,
    labels = FALSE
  )
  
  lasso.lambda <- lasso.lambda.months.18[[j]]
  ridge.lambda <- ridge.lambda.months.18[[j]]
  
  for (i in 1:k) {
    
    testIndexes <- which(folds == i)
    
    # -----------------------
    # LOGIT / LASSO / RIDGE
    # -----------------------
    testData <- final_df[testIndexes, , drop = FALSE]
    test_id_month <- testData %>% select(id, month, export_future)
    testData <- testData %>% select(-id, -month)
    testData <- remove_constant(testData)
    
    trainData <- final_df[-testIndexes, , drop = FALSE]
    trainData <- trainData %>% select(-id, -month)
    trainData <- remove_constant(trainData)
    
    # common variables before model.matrix
    common_variables_logit <- intersect(names(trainData), names(testData))
    trainData <- trainData[, common_variables_logit, drop = FALSE]
    testData  <- testData[, common_variables_logit, drop = FALSE]
    
    x.train <- sparse.model.matrix(export_future ~ ., data = trainData)[, -1, drop = FALSE]
    x.test.full <- sparse.model.matrix(export_future ~ ., data = testData)[, -1, drop = FALSE]
    
    y.train <- trainData$export_future
    y.test  <- testData$export_future
    
    train_cols <- colnames(x.train)
    common_cols <- intersect(colnames(x.test.full), train_cols)
    
    x.train <- x.train[, common_cols, drop = FALSE]
    x.test  <- x.test.full[, common_cols, drop = FALSE]
    
    # LASSO
    lasso.fit_18 <- glmnet(
      x.train, y.train,
      alpha = 1,
      family = "binomial",
      standardize = TRUE
    )
    
    lasso.pred_18 <- as.numeric(
      predict(lasso.fit_18, s = lasso.lambda, type = "response", newx = x.test)
    )
    lasso.pred_18 <- cbind(test_id_month, pred = lasso.pred_18)
    lasso.pred_18 <- as.data.frame(lasso.pred_18)
    
    # RIDGE
    ridge.fit_18 <- glmnet(
      x.train, y.train,
      alpha = 0,
      family = "binomial",
      standardize = TRUE
    )
    
    ridge.pred_18 <- as.numeric(
      predict(ridge.fit_18, s = ridge.lambda, type = "response", newx = x.test)
    )
    ridge.pred_18 <- cbind(test_id_month, pred = ridge.pred_18)
    ridge.pred_18 <- as.data.frame(ridge.pred_18)
    
    # LOGIT
    common_variables_logit2 <- intersect(names(trainData), names(testData))
    trainData2 <- trainData[, common_variables_logit2, drop = FALSE]
    testData2  <- testData[, common_variables_logit2, drop = FALSE]
    
    logit.fit_18 <- glm(export_future ~ ., data = trainData2, family = "binomial")
    logit.pred_18 <- as.numeric(
      predict(logit.fit_18, newdata = testData2, type = "response")
    )
    logit.pred_18 <- cbind(test_id_month, pred = logit.pred_18)
    logit.pred_18 <- as.data.frame(logit.pred_18)
    
    # -----------------------
    # RANDOM FOREST
    # -----------------------
    testData_rf <- final_df_rf[testIndexes, , drop = FALSE]
    testData_rf <- testData_rf %>% select(-id, -month)
    
    trainData_rf <- final_df_rf[-testIndexes, , drop = FALSE]
    trainData_rf <- trainData_rf %>% select(-id, -month)
    
    trainData_rf <- remove_constant(trainData_rf)
    testData_rf  <- remove_constant(testData_rf)
    
    common_variables_rf <- intersect(names(trainData_rf), names(testData_rf))
    trainData_rf <- trainData_rf[, common_variables_rf, drop = FALSE]
    testData_rf  <- testData_rf[, common_variables_rf, drop = FALSE]
    
    # force classification
    trainData_rf$export_future <- as.factor(trainData_rf$export_future)
    
    rf.fit_18 <- randomForest(
      export_future ~ .,
      data = trainData_rf,
      ntree = 500
    )
    
    rf.pred_18 <- predict(rf.fit_18, newdata = testData_rf, type = "prob")[, "1"]
    rf.pred_18 <- cbind(test_id_month, pred = as.numeric(rf.pred_18))
    rf.pred_18 <- as.data.frame(rf.pred_18)
    
    # store fold predictions
    lasso_preds_18[[i]] <- lasso.pred_18
    ridge_preds_18[[i]] <- ridge.pred_18
    rf_preds_18[[i]]    <- rf.pred_18
    logit_preds_18[[i]] <- logit.pred_18
    
    gc()
  }
  
  # aggregate folds within month
  lasso.preds_df_18 <- do.call(rbind, lasso_preds_18) %>%
    mutate(
      pred = as.numeric(pred),
      pred_class = ifelse(pred >= 0.5, 1, 0),
      pred_class = as.factor(as.character(pred_class))
    )
  lasso_preds_df_18[[j]] <- lasso.preds_df_18
  
  ridge.preds_df_18 <- do.call(rbind, ridge_preds_18) %>%
    mutate(
      pred = as.numeric(pred),
      pred_class = ifelse(pred >= 0.5, 1, 0),
      pred_class = as.factor(as.character(pred_class))
    )
  ridge_preds_df_18[[j]] <- ridge.preds_df_18
  
  rf.preds_df_18 <- do.call(rbind, rf_preds_18) %>%
    mutate(
      pred = as.numeric(pred),
      pred_class = ifelse(pred >= 0.5, 1, 0),
      pred_class = as.factor(as.character(pred_class))
    )
  rf_preds_df_18[[j]] <- rf.preds_df_18
  
  logit.preds_df_18 <- do.call(rbind, logit_preds_18) %>%
    mutate(
      pred = as.numeric(pred),
      pred_class = ifelse(pred >= 0.5, 1, 0),
      pred_class = as.factor(as.character(pred_class))
    )
  logit_preds_df_18[[j]] <- logit.preds_df_18
}

# ----------------------------------------------------
# Final monthly prediction files
# ----------------------------------------------------
lasso_preds <- do.call(rbind, lasso_preds_df_18)
lasso_preds_18 <- lasso_preds %>%
  rename(
    pred_18 = pred,
    pred_class_18 = pred_class
  ) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

ridge_preds <- do.call(rbind, ridge_preds_df_18)
ridge_preds_18 <- ridge_preds %>%
  rename(
    pred_18 = pred,
    pred_class_18 = pred_class
  ) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

rf_preds <- do.call(rbind, rf_preds_df_18)
rf_preds_18 <- rf_preds %>%
  rename(
    pred_18 = pred,
    pred_class_18 = pred_class
  ) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

logit_preds <- do.call(rbind, logit_preds_df_18)
logit_preds_18 <- logit_preds %>%
  rename(
    pred_18 = pred,
    pred_class_18 = pred_class
  ) %>%
  group_by(id, month) %>%
  distinct() %>%
  ungroup()

# ----------------------------------------------------
# Save outputs
# ----------------------------------------------------
save(
  lasso_preds_18,
  file = file.path(script_dir, "lasso_preds_18.RData")
)

save(
  ridge_preds_18,
  file = file.path(script_dir, "ridge_preds_18.RData")
)

save(
  rf_preds_18,
  file = file.path(script_dir, "rf_preds_18.RData")
)

save(
  logit_preds_18,
  file = file.path(script_dir, "logit_preds_18.RData")
)
