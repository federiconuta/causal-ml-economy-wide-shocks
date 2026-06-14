# Bootstrap for RF


rm(list = ls())

# -------------------------------------------------------------
# Set Directories
# -------------------------------------------------------------
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
data.in  <- "data_in/"
data.out <- "data_out/"

# -------------------------------------------------------------
# Libraries
# -------------------------------------------------------------
library(dplyr)
library(randomForest)

# -------------------------------------------------------------
options(scipen = 999)
# -------------------------------------------------------------
source(file.path(fig1_dir, "f_locals.R"))   # if needed elsewhere in the project

# -------------------------------------------------------------
# Load database
# -------------------------------------------------------------
load(file.path(package_dir, "data", "data_out", "final_data_18.RData"))
load(file.path(package_dir, "data", "data_out", "final_data_19.RData"))

# -------------------------------------------------------------
# Prepare dataset for RF
# -------------------------------------------------------------
final_data_18$month <- as.factor(final_data_18$month)
final_data_19$month <- as.factor(final_data_19$month)

final_data_18_rf <- final_data_18
final_data_19_rf <- final_data_19

rm(final_data_18, final_data_19)

# Keep original behavior: export_future remains numeric if that is what type.convert does
final_data_18_rf <- type.convert(final_data_18_rf, as.is = TRUE)
final_data_19_rf <- type.convert(final_data_19_rf, as.is = TRUE)

# -------------------------------------------------------------
# Keep only common variables across years (for SUM)
# -------------------------------------------------------------
common_variables_rf <- intersect(names(final_data_18_rf), names(final_data_19_rf))
df_train_rf <- final_data_18_rf[, common_variables_rf, drop = FALSE]
df_test_rf  <- final_data_19_rf[, common_variables_rf, drop = FALSE]

# -------------------------------------------------------------
# Bootstrap settings
# -------------------------------------------------------------
k <- 5
bootstrap <- 100
ntree_opt <- 500

filters <- as.character(unique(final_data_18_rf$month))
desired_length <- length(filters)

rf.pred_SUM_boot <- vector("list", length = desired_length)
names(rf.pred_SUM_boot) <- filters

rf.pred_SAM <- replicate(desired_length, rep(list(NULL), bootstrap), simplify = FALSE)
names(rf.pred_SAM) <- filters

rf.pred_SAM_boot <- vector("list", length = desired_length)
names(rf.pred_SAM_boot) <- filters

# -------------------------------------------------------------
# Helper: safely bind prediction with ids
# -------------------------------------------------------------
bind_pred_df <- function(id_df, pred_vec, pred_name) {
  out <- id_df
  out[[pred_name]] <- as.numeric(pred_vec)
  out
}

# -------------------------------------------------------------
# Main loop
# -------------------------------------------------------------
for (i in seq_along(filters)) {
  cat("Running month:", filters[[i]], "\n")
  
  df_test_rf_loop <- df_test_rf %>%
    filter(month == filters[[i]])
  
  df_train_rf_loop <- df_train_rf %>%
    filter(month == filters[[i]]) %>%
    select(-id, -month)
  
  df_test_rf_loop_SAM <- final_data_19_rf %>%
    filter(month == filters[[i]])
  
  length_divisor <- 1
  
  for (m in 1:bootstrap) {
    set.seed(100 + m)
    
    # -----------------------------
    # Bootstrap sample for SUM train
    # -----------------------------
    df_train_rf_loop_boot <- df_train_rf_loop
    sampleRows_train <- sample(
      nrow(df_train_rf_loop_boot),
      size = floor(nrow(df_train_rf_loop) / length_divisor),
      replace = TRUE
    )
    df_train_rf_loop_boot <- df_train_rf_loop_boot[sampleRows_train, , drop = FALSE]
    
    # same boot sample used later as SAM source
    df_train_rf_loop_boot_CV <- df_train_rf_loop_boot
    
    # -----------------------------
    # Bootstrap sample for SUM test
    # -----------------------------
    df_test_rf_loop_boot <- df_test_rf_loop
    sampleRows_test <- sample(
      nrow(df_test_rf_loop_boot),
      size = floor(nrow(df_test_rf_loop_boot) / length_divisor),
      replace = TRUE
    )
    df_test_rf_loop_boot <- df_test_rf_loop_boot[sampleRows_test, , drop = FALSE]
    
    # -----------------------------
    # Bootstrap sample for SAM CV data
    # -----------------------------
    df_test_rf_loop_boot_CV <- df_test_rf_loop_SAM
    sampleRows_test_CV <- sample(
      nrow(df_test_rf_loop_boot_CV),
      size = floor(nrow(df_test_rf_loop_boot_CV) / length_divisor),
      replace = TRUE
    )
    df_test_rf_loop_boot_CV <- df_test_rf_loop_boot_CV[sampleRows_test_CV, , drop = FALSE]
    
    # -----------------------------
    # SUM predictions
    # -----------------------------
    companies_rf <- df_test_rf_loop_boot %>%
      select(month, id, export_future)
    
    test_rf_SUM <- df_test_rf_loop_boot %>%
      select(-id, -month)
    
    rf.fit_SUM_boot <- randomForest(
      export_future ~ .,
      data = df_train_rf_loop_boot,
      ntree = ntree_opt
    )
    
    rf.pred_SUM_vec <- predict(
      rf.fit_SUM_boot,
      newdata = test_rf_SUM,
      type = "response"
    )
    
    rf.pred_SUM_boot[[i]][[m]] <- bind_pred_df(
      companies_rf,
      rf.pred_SUM_vec,
      "pred_SUM"
    )
    
    # -----------------------------
    # SAM predictions with manual 5 folds
    # -----------------------------
    folds <- data.frame(folds = runif(nrow(df_test_rf_loop_boot_CV)))
    
    qtiles <- quantile(folds$folds, seq(0, 1, 0.20), na.rm = TRUE)
    qtiles[1] <- 0.99 * qtiles[1]
    qtiles[6] <- 1.01 * qtiles[6]
    
    folds <- folds %>%
      mutate(
        folds = ifelse(folds > qtiles[5] & folds <= qtiles[6], 5,
                       ifelse(folds > qtiles[4] & folds <= qtiles[5], 4,
                              ifelse(folds > qtiles[3] & folds <= qtiles[4], 3,
                                     ifelse(folds > qtiles[2] & folds <= qtiles[3], 2, 1))))
      )
    
    for (j in 1:k) {
      testIndexes <- which(folds$folds == j)
      
      testData <- df_test_rf_loop_boot_CV[testIndexes, , drop = FALSE]
      test_id_month <- testData %>%
        select(id, month, export_future)
      testData <- testData %>%
        select(-id, -month)
      
      trainData <- df_test_rf_loop_boot_CV[-testIndexes, , drop = FALSE] %>%
        select(-id, -month)
      
      rf.fit_19 <- randomForest(
        export_future ~ .,
        data = trainData,
        ntree = ntree_opt
      )
      
      rf.pred_19_vec <- predict(
        rf.fit_19,
        newdata = testData,
        type = "response"
      )
      
      rf.pred_SAM[[i]][[m]][[j]] <- bind_pred_df(
        test_id_month,
        rf.pred_19_vec,
        "pred_SAM"
      )
    }
    
    rf.pred_SAM_boot[[i]][[m]] <- do.call(rbind, rf.pred_SAM[[i]][[m]])
  }
}

proc.time()

# -------------------------------------------------------------
# Save
# -------------------------------------------------------------
save(
  rf.pred_SUM_boot,
  file = file.path(script_dir, "rf.pred_SUM_boot.RData")
)

save(
  rf.pred_SAM,
  file = file.path(script_dir, "rf.pred_SAM.RData")
)

save(
  rf.pred_SAM_boot,
  file = file.path(script_dir, "rf.pred_SAM_boot.RData")
)
