# In this code we generate predictions from SUM (SHOCK UNAWARE MACHINE)
# We take optimal parameters obtained from CV in 2018

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
# Set directories
# ----------------------------------------------------
setwd(script_dir)

# ----------------------------------------------------
# Libraries
# ----------------------------------------------------
library(dplyr)
library(caret)
library(glmnet)
library(randomForest)
library(janitor)

# ----------------------------------------------------
options(scipen = 999)
# ----------------------------------------------------
source("f_locals.R")   # must contain remove_constant()

# ----------------------------------------------------
# Load data
# ----------------------------------------------------
load(file.path(data_out_dir, "final_data_18.RData"))
load(file.path(data_out_dir, "final_data_19.RData"))

# Load optimal lambda values from 2018 CV
load(file.path(script_dir, "lasso.lambda.months.18.RData"))
load(file.path(script_dir, "ridge.lambda.months.18.RData"))

ntree_rf <- 500

# ----------------------------------------------------
# Helpers
# ----------------------------------------------------

# Remove constant factor columns, keep numeric columns as in original logic
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

# Generate interactions between size and (industry, sector, via, iso)
interaction_size <- function(df) {
  df <- type.convert(df, as.is = TRUE)
  cols <- grep("industry|sector|via|iso", names(df), value = TRUE)
  if ("size" %in% names(df) && length(cols) > 0) {
    df[paste0("size_", cols)] <- as.integer(df$size) * df[cols]
  }
  df
}

# Convert glmnet prediction to a plain numeric vector
extract_glmnet_pred <- function(pred_obj) {
  as.numeric(pred_obj[, 1])
}

# Build prediction dataframe robustly
bind_prediction <- function(id_df, pred_vec) {
  out <- cbind(id_df, pred = as.numeric(pred_vec))
  out <- as.data.frame(out)
  out
}

# ----------------------------------------------------
# Prepare datasets
# ----------------------------------------------------
final_data_18$month <- as.factor(final_data_18$month)
final_data_19$month <- as.factor(final_data_19$month)

# Remove constant factor columns
final_data_18 <- drop_constant_factor_cols(final_data_18)
final_data_19 <- drop_constant_factor_cols(final_data_19)

# Datasets for logit / lasso / ridge
final_data_18_logit <- interaction_size(final_data_18)
final_data_19_logit <- interaction_size(final_data_19)

# Convert selected columns to factor if present
cols_to_factor_19 <- grep("via_|industry_|sector_", names(final_data_19_logit), value = TRUE)
if (length(cols_to_factor_19) > 0) {
  final_data_19_logit[cols_to_factor_19] <- lapply(final_data_19_logit[cols_to_factor_19], factor)
}

cols_to_factor_18 <- grep("via_|industry_|sector_", names(final_data_18_logit), value = TRUE)
if (length(cols_to_factor_18) > 0) {
  final_data_18_logit[cols_to_factor_18] <- lapply(final_data_18_logit[cols_to_factor_18], factor)
}

# Datasets for RF
final_data_18_rf <- type.convert(final_data_18, as.is = TRUE)
final_data_19_rf <- type.convert(final_data_19, as.is = TRUE)

# IMPORTANT: RF should be classification, not regression
final_data_18_rf$export_future <- as.factor(final_data_18_rf$export_future)
final_data_19_rf$export_future <- as.factor(final_data_19_rf$export_future)

rm(final_data_18, final_data_19)

# ----------------------------------------------------
# Keep only common variables across years
# ----------------------------------------------------
common_variables_logit <- intersect(names(final_data_18_logit), names(final_data_19_logit))
df_train_logit <- final_data_18_logit[, common_variables_logit, drop = FALSE]
df_test_logit  <- final_data_19_logit[, common_variables_logit, drop = FALSE]

common_variables_rf <- intersect(names(final_data_18_rf), names(final_data_19_rf))
df_train_rf <- final_data_18_rf[, common_variables_rf, drop = FALSE]
df_test_rf  <- final_data_19_rf[, common_variables_rf, drop = FALSE]

# ----------------------------------------------------
# Empty lists for predictions
# ----------------------------------------------------
filters <- as.character(unique(df_train_logit$month))

SUM_preds_lasso <- vector("list", length(filters))
SUM_preds_ridge <- vector("list", length(filters))
SUM_preds_rf    <- vector("list", length(filters))
SUM_preds_logit <- vector("list", length(filters))

names(SUM_preds_lasso) <- filters
names(SUM_preds_ridge) <- filters
names(SUM_preds_rf)    <- filters
names(SUM_preds_logit) <- filters

# ----------------------------------------------------
# Main loop by month
# ----------------------------------------------------
set.seed(2021)

for (i in seq_along(filters)) {
  cat("Running month:", filters[[i]], "\n")
  
  lasso.lambda <- lasso.lambda.months.18[[i]]
  ridge.lambda <- ridge.lambda.months.18[[i]]
  
  # -------------------------
  # LASSO / Ridge / Logit
  # -------------------------
  df_test_logit_loop <- df_test_logit %>%
    filter(month == filters[[i]])
  
  df_train_logit_loop <- df_train_logit %>%
    filter(month == filters[[i]])
  
  companies_logit <- df_test_logit_loop %>%
    dplyr::select(month, id, export_future)
  
  df_train_logit_loop <- df_train_logit_loop %>% dplyr::select(-id, -month)
  df_test_logit_loop  <- df_test_logit_loop  %>% dplyr::select(-id, -month)
  
  df_train_logit_loop <- remove_constant(df_train_logit_loop)
  df_test_logit_loop  <- remove_constant(df_test_logit_loop)
  
  # Make sure the outcome is numeric 0/1 for glmnet / glm
  df_train_logit_loop$export_future <- as.numeric(as.character(df_train_logit_loop$export_future))
  df_test_logit_loop$export_future  <- as.numeric(as.character(df_test_logit_loop$export_future))
  
  x.train.loop <- sparse.model.matrix(export_future ~ ., df_train_logit_loop)[, -1, drop = FALSE]
  y.train.loop <- df_train_logit_loop$export_future
  
  x.test.full <- sparse.model.matrix(export_future ~ ., df_test_logit_loop)[, -1, drop = FALSE]
  
  common_cols <- intersect(colnames(x.test.full), colnames(x.train.loop))
  x.train.loop <- x.train.loop[, common_cols, drop = FALSE]
  x.test.loop  <- x.test.full[, common_cols, drop = FALSE]
  
  # LASSO
  lasso.fit_SUM <- glmnet(
    x.train.loop, y.train.loop,
    alpha = 1,
    family = "binomial",
    standardize = TRUE
  )
  lasso.pred_SUM <- predict(
    lasso.fit_SUM,
    s = lasso.lambda,
    type = "response",
    newx = x.test.loop
  )
  lasso.pred_SUM <- extract_glmnet_pred(lasso.pred_SUM)
  
  # Ridge
  ridge.fit_SUM <- glmnet(
    x.train.loop, y.train.loop,
    alpha = 0,
    family = "binomial",
    standardize = TRUE
  )
  ridge.pred_SUM <- predict(
    ridge.fit_SUM,
    s = ridge.lambda,
    type = "response",
    newx = x.test.loop
  )
  ridge.pred_SUM <- extract_glmnet_pred(ridge.pred_SUM)
  
  # Logit
  common_variables_logit2 <- intersect(names(df_train_logit_loop), names(df_test_logit_loop))
  df_train_logit_loop <- df_train_logit_loop[, common_variables_logit2, drop = FALSE]
  df_test_logit_loop  <- df_test_logit_loop[, common_variables_logit2, drop = FALSE]
  
  logit.fit_SUM <- glm(export_future ~ ., data = df_train_logit_loop, family = "binomial")
  logit.pred_SUM <- predict(logit.fit_SUM, newdata = df_test_logit_loop, type = "response")
  
  # -------------------------
  # Random Forest
  # -------------------------
  df_test_rf_loop <- df_test_rf %>%
    filter(month == filters[[i]])
  
  df_train_rf_loop <- df_train_rf %>%
    filter(month == filters[[i]])
  
  companies_rf <- df_test_rf_loop %>%
    dplyr::select(month, id, export_future)
  
  df_test_rf_loop  <- df_test_rf_loop  %>% dplyr::select(-id, -month)
  df_train_rf_loop <- df_train_rf_loop %>% dplyr::select(-id, -month)
  
  df_train_rf_loop <- remove_constant(df_train_rf_loop)
  df_test_rf_loop  <- remove_constant(df_test_rf_loop)
  
  common_variables_rf2 <- intersect(names(df_train_rf_loop), names(df_test_rf_loop))
  df_train_rf_loop <- df_train_rf_loop[, common_variables_rf2, drop = FALSE]
  df_test_rf_loop  <- df_test_rf_loop[, common_variables_rf2, drop = FALSE]
  
  # Ensure factor outcome for classification
  df_train_rf_loop$export_future <- as.factor(as.character(df_train_rf_loop$export_future))
  df_test_rf_loop$export_future  <- as.factor(as.character(df_test_rf_loop$export_future))
  
  rf.fit_SUM <- randomForest(
    export_future ~ .,
    data = df_train_rf_loop,
    ntree = ntree_rf
  )
  
  # Get probability of class "1"
  rf.pred_prob <- predict(rf.fit_SUM, newdata = df_test_rf_loop, type = "prob")
  if (!"1" %in% colnames(rf.pred_prob)) {
    stop("RF probability output does not contain class '1'.")
  }
  rf.pred_SUM <- rf.pred_prob[, "1"]
  
  # -------------------------
  # Join predictions with ids
  # -------------------------
  lasso.pred_SUM <- bind_prediction(companies_logit, lasso.pred_SUM)
  ridge.pred_SUM <- bind_prediction(companies_logit, ridge.pred_SUM)
  logit.pred_SUM <- bind_prediction(companies_logit, logit.pred_SUM)
  rf.pred_SUM    <- bind_prediction(companies_rf, rf.pred_SUM)
  
  # Save monthly predictions
  SUM_preds_lasso[[i]] <- lasso.pred_SUM
  SUM_preds_ridge[[i]] <- ridge.pred_SUM
  SUM_preds_logit[[i]] <- logit.pred_SUM
  SUM_preds_rf[[i]]    <- rf.pred_SUM
  
  gc()
}

# ----------------------------------------------------
# Bind all months
# ----------------------------------------------------
SUM_preds_lasso <- do.call(rbind, SUM_preds_lasso) %>%
  mutate(
    pred_class = ifelse(pred >= 0.5, 1, 0),
    pred_class = as.factor(as.character(pred_class))
  )

SUM_preds_ridge <- do.call(rbind, SUM_preds_ridge) %>%
  mutate(
    pred_class = ifelse(pred >= 0.5, 1, 0),
    pred_class = as.factor(as.character(pred_class))
  )

SUM_preds_logit <- do.call(rbind, SUM_preds_logit) %>%
  mutate(
    pred_class = ifelse(pred >= 0.5, 1, 0),
    pred_class = as.factor(as.character(pred_class))
  )

SUM_preds_rf <- do.call(rbind, SUM_preds_rf) %>%
  mutate(
    pred_class = ifelse(pred >= 0.5, 1, 0),
    pred_class = as.factor(as.character(pred_class))
  )

# ----------------------------------------------------
# Save
# ----------------------------------------------------
save(SUM_preds_lasso, file = file.path(script_dir, "SUM_preds_lasso.RData"))
save(SUM_preds_ridge, file = file.path(script_dir, "SUM_preds_ridge.RData"))
save(SUM_preds_rf,    file = file.path(script_dir, "SUM_preds_rf.RData"))
save(SUM_preds_logit, file = file.path(script_dir, "SUM_preds_logit.RData"))

proc.time()
