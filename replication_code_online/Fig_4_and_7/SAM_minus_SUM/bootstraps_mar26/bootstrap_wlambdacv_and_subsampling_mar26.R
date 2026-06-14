# Bootstrap for SUM (SHOCK UNAWARE MACHINE) Lasso
# Lambda recomputed at each bootstrap replication using cv.glmnet()

rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})
package_dir <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
data_out_dir <- file.path(package_dir, "data", "data_out")

library(dplyr)
library(caret)
library(glmnet)
library.randomForest <- library(randomForest)
library(Matrix)

options(scipen = 999)

source(file.path(script_dir, "f_locals.R"))

load(file.path(data_out_dir, "final_data_18.RData"))
load(file.path(data_out_dir, "final_data_19.RData"))

# Prepare dataset for LASSO
final_data_18$month <- as.factor(final_data_18$month)
final_data_19$month <- as.factor(final_data_19$month)

interaction_size <- function(df){
  df <- type.convert(df)
  cols <- grep('industry|sector|via|iso', names(df), value = TRUE)
  df[paste0('size_', cols)] <- as.integer(df$size) * df[cols]
  df
}

final_data_18_logit <- interaction_size(final_data_18)
final_data_19_logit <- interaction_size(final_data_19)

rm(final_data_18, final_data_19)

common_variables_logit <- intersect(names(final_data_18_logit), names(final_data_19_logit))
df_train_logit <- final_data_18_logit[, common_variables_logit]
df_test_logit  <- final_data_19_logit[, common_variables_logit]

df_test_logit_SAM <- final_data_19_logit

# ---------------------------------------------
k <- 5
bootstrap <- 100

filters <- as.character(unique(final_data_18_logit$month))
desired_length <- length(filters)

lasso.pred_SUM_boot <- vector("list", desired_length)
names(lasso.pred_SUM_boot) <- filters

lasso.pred_SAM_boot <- vector("list", desired_length)
names(lasso.pred_SAM_boot) <- filters

for (i in seq_along(filters)) {
  
  df_test_logit_loop <- df_test_logit %>%
    filter(month == filters[[i]])
  
  df_train_logit_loop <- df_train_logit %>%
    filter(month == filters[[i]])
  
  df_test_logit_loop_SAM <- final_data_19_logit %>%
    filter(month == filters[[i]])
  
  lasso.pred_SUM_boot[[i]] <- vector("list", bootstrap)
  lasso.pred_SAM_boot[[i]] <- vector("list", bootstrap)
  
  subsample_prop <- 0.50
  
  for (m in 1:bootstrap) {
    set.seed(100 + m)
    
    # =========================
    # SUM: bootstrap 2018 train
    # =========================
    df_train_logit_loop_boot <- df_train_logit_loop
    n_train0 <- nrow(df_train_logit_loop_boot)
    
    sampleRows_train <- sample(
      n_train0,
      size = floor(subsample_prop * n_train0),
      replace = TRUE
    )
    
    df_train_logit_loop_boot <- df_train_logit_loop_boot[sampleRows_train, ]
    
    if (length(unique(df_train_logit_loop_boot$export_future[!is.na(df_train_logit_loop_boot$export_future)])) < 2) next
    
    df_test_logit_loop_boot <- df_test_logit_loop
    
    companies_logit <- df_test_logit_loop_boot %>%
      dplyr::select(month, id, export_future)
    
    trainData_SUM <- df_train_logit_loop_boot %>%
      dplyr::select(-id, -year, -month)
    
    testData_SUM <- df_test_logit_loop_boot %>%
      dplyr::select(-id, -year, -month)
    
    uniq_counts_SUM <- sapply(trainData_SUM, function(x) length(unique(x[!is.na(x)])))
    one_level_vars_SUM <- names(uniq_counts_SUM[uniq_counts_SUM <= 1])
    
    if (length(one_level_vars_SUM) > 0) {
      message("SUM - dropping vars with <=1 unique value in TRAIN: ",
              paste(one_level_vars_SUM, collapse = ", "))
      trainData_SUM <- trainData_SUM[, !(names(trainData_SUM) %in% one_level_vars_SUM), drop = FALSE]
      testData_SUM  <- testData_SUM[,  !(names(testData_SUM)  %in% one_level_vars_SUM), drop = FALSE]
    }
    
    joint_SUM <- rbind(trainData_SUM, testData_SUM)
    mm_SUM <- sparse.model.matrix(export_future ~ ., joint_SUM)
    
    n_train_SUM <- nrow(trainData_SUM)
    
    x.train_SUM <- mm_SUM[1:n_train_SUM, -1, drop = FALSE]
    x.test_SUM  <- mm_SUM[(n_train_SUM + 1):nrow(mm_SUM), -1, drop = FALSE]
    
    y.train_SUM <- trainData_SUM$export_future
    
    # lambda scelta ogni volta via CV
    cv.fit_SUM <- cv.glmnet(
      x.train_SUM, y.train_SUM,
      family = "binomial",
      alpha = 1,
      nfolds = k,
      type.measure = "auc",
      standardize = TRUE
    )
    
    lasso.pred_SUM <- predict(
      cv.fit_SUM,
      s = "lambda.min",   # oppure "lambda.1se"
      type = "response",
      newx = x.test_SUM
    )
    
    colnames(lasso.pred_SUM) <- "pred"
    lasso.pred_SUM <- cbind(companies_logit, lasso.pred_SUM)
    lasso.pred_SUM_boot[[i]][[m]] <- lasso.pred_SUM
    
    # =========================
    # SAM: bootstrap 2019 train
    # =========================
    sam_train_boot <- df_test_logit_loop_SAM
    n_sam_train0 <- nrow(sam_train_boot)
    
    sampleRows_sam <- sample(
      n_sam_train0,
      size = floor(subsample_prop * n_sam_train0),
      replace = TRUE
    )
    
    sam_train_boot <- sam_train_boot[sampleRows_sam, ]
    
    if (length(unique(sam_train_boot$export_future[!is.na(sam_train_boot$export_future)])) < 2) next
    
    sam_test_fixed <- df_test_logit_loop_SAM
    
    test_id_month <- sam_test_fixed %>%
      dplyr::select(id, month, export_future)
    
    trainData_SAM <- sam_train_boot %>%
      dplyr::select(-id, -year, -month)
    
    testData_SAM <- sam_test_fixed %>%
      dplyr::select(-id, -year, -month)
    
    uniq_counts_SAM <- sapply(trainData_SAM, function(x) length(unique(x[!is.na(x)])))
    one_level_vars_SAM <- names(uniq_counts_SAM[uniq_counts_SAM <= 1])
    
    if (length(one_level_vars_SAM) > 0) {
      message("SAM - dropping vars with <=1 unique value in TRAIN: ",
              paste(one_level_vars_SAM, collapse = ", "))
      trainData_SAM <- trainData_SAM[, !(names(trainData_SAM) %in% one_level_vars_SAM), drop = FALSE]
      testData_SAM  <- testData_SAM[,  !(names(testData_SAM)  %in% one_level_vars_SAM), drop = FALSE]
    }
    
    joint_SAM <- rbind(trainData_SAM, testData_SAM)
    mm_SAM <- sparse.model.matrix(export_future ~ ., joint_SAM)
    
    n_train_SAM <- nrow(trainData_SAM)
    
    x.train_SAM <- mm_SAM[1:n_train_SAM, -1, drop = FALSE]
    x.test_SAM  <- mm_SAM[(n_train_SAM + 1):nrow(mm_SAM), -1, drop = FALSE]
    
    y.train_SAM <- trainData_SAM$export_future
    
    # lambda scelta ogni volta via CV
    cv.fit_SAM <- cv.glmnet(
      x.train_SAM, y.train_SAM,
      family = "binomial",
      alpha = 1,
      nfolds = k,
      type.measure = "auc",
      standardize = TRUE
    )
    
    lasso.pred_19 <- predict(
      cv.fit_SAM,
      s = "lambda.min",   # oppure "lambda.1se"
      type = "response",
      newx = x.test_SAM
    )
    
    colnames(lasso.pred_19) <- "pred"
    lasso.pred_19 <- cbind(test_id_month, lasso.pred_19)
    lasso.pred_SAM_boot[[i]][[m]] <- lasso.pred_19
  }
}

proc.time()

save(lasso.pred_SUM_boot, file = file.path(script_dir, "lasso.pred_SUM_boot_subsamp_cvlam_marzo26.RData"))
save(lasso.pred_SAM_boot, file = file.path(script_dir, "lasso.pred_SAM_boot_subsamp_cvlam_marzo26.RData"))
