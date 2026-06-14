# Bootstrap for Lasso

rm(list = ls())

# Set Directories -------------------------------------------------------------
script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})

fig1_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
package_dir <- normalizePath(file.path(fig1_dir, ".."), mustWork = TRUE)
setwd(fig1_dir)
figu.dir <- "figures/"
data.in <- "data_in/"
data.out <- "data_out/"
# ---------------------------------

library(dplyr)
library(glmnet)
library(doParallel)

#----------------------------------------------------
options(scipen = 999) # remove scientific notation
#----------------------------------------------------
source(file.path(fig1_dir, "f_locals.R"))   # must contain remove_constant()

# ---------------------------------------------------
# Helpers
# ---------------------------------------------------
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

make_manual_folds <- function(n, k = 5) {
  folds <- data.frame(folds = runif(n))
  qv <- quantile(folds$folds, seq(0, 1, length.out = k + 1), na.rm = TRUE)
  qv[1] <- 0.99 * qv[1]
  qv[length(qv)] <- 1.01 * qv[length(qv)]
  
  folds$folds <- ifelse(folds$folds > qv[5] & folds$folds <= qv[6], 5,
                        ifelse(folds$folds > qv[4] & folds$folds <= qv[5], 4,
                               ifelse(folds$folds > qv[3] & folds$folds <= qv[4], 3,
                                      ifelse(folds$folds > qv[2] & folds$folds <= qv[3], 2, 1))))
  folds
}

# ---------------------------------------------------
# Load data
# ---------------------------------------------------
load(file.path(package_dir, "data", "data_out", "final_data_18.RData"))
load(file.path(package_dir, "data", "data_out", "final_data_19.RData"))

# Load optimal lambdas
load(file.path(package_dir, "Tab_3", "2019_2020_SUM_GoF", "lasso.lambda.months.18.RData"))
load(file.path(package_dir, "Tab_3", "2019_2020_SAM_GoF", "lasso.lambda.months.19.RData"))

# ---------------------------------------------------
# Prepare data
# ---------------------------------------------------
final_data_18$month <- as.factor(final_data_18$month)
final_data_19$month <- as.factor(final_data_19$month)

final_data_18 <- drop_constant_factor_cols(final_data_18)
final_data_19 <- drop_constant_factor_cols(final_data_19)

final_data_18_logit <- interaction_size(final_data_18)
final_data_19_logit <- interaction_size(final_data_19)

cols_to_factor <- grep("via_|industry_|sector_", names(final_data_19_logit), value = TRUE)
if (length(cols_to_factor) > 0) {
  final_data_19_logit[cols_to_factor] <- lapply(final_data_19_logit[cols_to_factor], factor)
}

cols_to_factor <- grep("via_|industry_|sector_", names(final_data_18_logit), value = TRUE)
if (length(cols_to_factor) > 0) {
  final_data_18_logit[cols_to_factor] <- lapply(final_data_18_logit[cols_to_factor], factor)
}

# Keep only common variables across years
common_variables_logit <- intersect(names(final_data_18_logit), names(final_data_19_logit))
df_train_logit <- final_data_18_logit[, common_variables_logit, drop = FALSE]
df_test_logit  <- final_data_19_logit[, common_variables_logit, drop = FALSE]

# Full 2019 sample used for SAM bootstrap-CV part
df_test_logit_SAM <- final_data_19_logit

rm(final_data_18, final_data_19)

# Parallel
cores <- parallel::detectCores() - 1
registerDoParallel(cores = cores)

# ---------------------------------------------
# Bootstrap settings
# ---------------------------------------------
k <- 5
bootstrap <- 100

filters <- as.character(unique(df_train_logit$month))
desired_length <- length(filters)

# outer list: months
# inner list: bootstraps
lasso.pred_SUM_boot <- vector("list", desired_length)
names(lasso.pred_SUM_boot) <- filters

lasso.pred_SAM <- vector("list", desired_length)
names(lasso.pred_SAM) <- filters

lasso.pred_SAM_boot <- vector("list", desired_length)
names(lasso.pred_SAM_boot) <- filters

for (i in seq_along(filters)) {
  
  cat("Running month:", filters[[i]], "\n")
  
  lasso.lambda.18 <- lasso.lambda.months.18[[i]]
  lasso.lambda.19 <- lasso.lambda.months.19[[i]]
  
  df_test_logit_loop <- df_test_logit %>%
    filter(month == filters[[i]])
  
  df_train_logit_loop <- df_train_logit %>%
    filter(month == filters[[i]]) %>%
    select(-id, -month)
  
  df_test_logit_loop_SAM <- df_test_logit_SAM %>%
    filter(month == filters[[i]])
  
  length_divisor <- 1
  
  lasso.pred_SUM_boot[[i]] <- vector("list", bootstrap)
  lasso.pred_SAM[[i]] <- vector("list", bootstrap)
  lasso.pred_SAM_boot[[i]] <- vector("list", bootstrap)
  
  for (m in 1:bootstrap) {
    
    set.seed(100 + m)
    
    # --------------------------
    # Bootstrap training sample for SUM
    # --------------------------
    df_train_logit_loop_boot <- df_train_logit_loop
    sampleRows_train <- sample(
      nrow(df_train_logit_loop_boot),
      size = floor(nrow(df_train_logit_loop_boot) / length_divisor),
      replace = TRUE
    )
    df_train_logit_loop_boot <- df_train_logit_loop_boot[sampleRows_train, , drop = FALSE]
    
    # --------------------------
    # Bootstrap test sample for SUM prediction storage
    # --------------------------
    df_test_logit_loop_boot <- df_test_logit_loop
    sampleRows_test <- sample(
      nrow(df_test_logit_loop_boot),
      size = floor(nrow(df_test_logit_loop_boot) / length_divisor),
      replace = TRUE
    )
    df_test_logit_loop_boot <- df_test_logit_loop_boot[sampleRows_test, , drop = FALSE]
    
    # --------------------------
    # Bootstrap sample for SAM cross-fitted predictions
    # --------------------------
    df_test_logit_loop_boot_CV <- df_test_logit_loop_SAM
    sampleRows_test_CV <- sample(
      nrow(df_test_logit_loop_boot_CV),
      size = floor(nrow(df_test_logit_loop_boot_CV) / length_divisor),
      replace = TRUE
    )
    df_test_logit_loop_boot_CV <- df_test_logit_loop_boot_CV[sampleRows_test_CV, , drop = FALSE]
    
    companies_logit <- df_test_logit_loop_boot %>%
      select(month, id, export_future)
    
    df_test_logit_loop_boot <- df_test_logit_loop_boot %>%
      select(-id, -month)
    
    df_train_logit_loop_boot <- remove_constant(df_train_logit_loop_boot)
    df_test_logit_loop_boot  <- remove_constant(df_test_logit_loop_boot)
    
    x.train.loop <- sparse.model.matrix(export_future ~ ., df_train_logit_loop_boot)[, -1, drop = FALSE]
    y.train.loop <- df_train_logit_loop_boot$export_future
    
    x.test.loop.full <- sparse.model.matrix(export_future ~ ., df_test_logit_loop_boot)[, -1, drop = FALSE]
    
    train_cols <- colnames(x.train.loop)
    common_cols <- intersect(colnames(x.test.loop.full), train_cols)
    
    x.test.loop  <- x.test.loop.full[, common_cols, drop = FALSE]
    x.train.loop <- x.train.loop[, common_cols, drop = FALSE]
    
    # ======
    # SUM predictions
    # ======
    lasso.fit_SUM_boot <- glmnet(
      x.train.loop, y.train.loop,
      alpha = 1,
      family = "binomial",
      standardize = TRUE,
      parallel = TRUE,
      nlambda = 30,
      maxit = 200000
    )
    
    lasso.pred_SUM <- predict(
      lasso.fit_SUM_boot,
      s = lasso.lambda.18,
      type = "response",
      newx = x.test.loop
    )
    
    lasso.pred_SUM <- data.frame(
      companies_logit,
      pred_SUM = as.numeric(lasso.pred_SUM)
    )
    
    lasso.pred_SUM_boot[[i]][[m]] <- lasso.pred_SUM
    
    rm(x.train.loop, y.train.loop, x.test.loop, x.test.loop.full)
    gc()
    
    # ======
    # SAM predictions
    # ======
    folds <- make_manual_folds(nrow(df_test_logit_loop_boot_CV), k = k)
    
    lasso.pred_SAM[[i]][[m]] <- vector("list", k)
    
    for (j in 1:k) {
      
      testIndexes <- which(folds$folds == j, arr.ind = TRUE)
      
      testData <- df_test_logit_loop_boot_CV[testIndexes, , drop = FALSE]
      test_id_month <- testData %>%
        select(id, month, export_future)
      
      testData <- testData %>%
        select(-id, -month)
      
      testData <- remove_constant(testData)
      x.test.full <- sparse.model.matrix(export_future ~ ., testData)[, -1, drop = FALSE]
      
      trainData <- df_test_logit_loop_boot_CV[-testIndexes, , drop = FALSE]
      trainData <- trainData %>%
        select(-id, -month)
      
      trainData <- remove_constant(trainData)
      x.train <- sparse.model.matrix(export_future ~ ., trainData)[, -1, drop = FALSE]
      y.train <- trainData$export_future
      
      train_cols2 <- colnames(x.train)
      common_cols2 <- intersect(colnames(x.test.full), train_cols2)
      
      x.test  <- x.test.full[, common_cols2, drop = FALSE]
      x.train <- x.train[, common_cols2, drop = FALSE]
      
      lasso.fit_19 <- glmnet(
        x.train, y.train,
        alpha = 1,
        family = "binomial",
        standardize = TRUE,
        parallel = TRUE,
        nlambda = 30,
        maxit = 200000
      )
      
      lasso.pred_19 <- predict(
        lasso.fit_19,
        s = lasso.lambda.19,
        type = "response",
        newx = x.test
      )
      
      lasso.pred_19 <- data.frame(
        test_id_month,
        pred_SAM = as.numeric(lasso.pred_19)
      )
      
      lasso.pred_SAM[[i]][[m]][[j]] <- lasso.pred_19
    }
    
    lasso.pred_SAM_boot[[i]][[m]] <- do.call(rbind, lasso.pred_SAM[[i]][[m]])
  }
}

proc.time()

save(
  lasso.pred_SUM_boot,
  file = file.path(script_dir, "lasso.pred_SUM_boot.RData")
)

save(
  lasso.pred_SAM,
  file = file.path(script_dir, "lasso.pred_SAM.RData")
)

save(
  lasso.pred_SAM_boot,
  file = file.path(script_dir, "lasso.pred_SAM_boot.RData")
)

stopImplicitCluster()
