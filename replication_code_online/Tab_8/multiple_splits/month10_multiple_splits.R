######### ONLY WITH Ridge and RIDGE:
rm(list = ls())
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
library(caret)     #for logit-Ridge
library(glmnet)    #for logit-Ridge
library(ROCR)
library(randomForest)
library(data.table)  # For fast data manipulation
library(dplyr)       # For data manipulation
library(tidyr)       # For grid generation (expand.grid)
library(glmnet)      # For Ridge and Ridge
library(randomForest) # For Random Forest
library(xgboost)     # For XGBoost
library(e1071)       # For SVM (Support Vector Machines)
library(causalDML)
library(hdm)
library(tidyverse)
library(grf)
library(estimatr)
library(lmtest)
library(sandwich)
library(psych)
library(latex2exp)

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

tab8_dir <- normalizePath(Sys.getenv("OBES_TAB8_DIR", file.path(script_dir, "..")), winslash = "/", mustWork = FALSE)
replication_dir <- normalizePath(Sys.getenv("OBES_REPLICATION_DIR", file.path(tab8_dir, "..")), winslash = "/", mustWork = FALSE)
data_out_dir <- normalizePath(Sys.getenv("OBES_DATA_OUT_DIR", file.path(replication_dir, "data", "data_out")), winslash = "/", mustWork = FALSE)
split_output_dir <- normalizePath(Sys.getenv("OBES_TAB8_SPLITS_DIR", file.path(tab8_dir, "multiple_splits", "multiple_splits_datasets")), winslash = "/", mustWork = FALSE)
dir.create(split_output_dir, showWarnings = FALSE, recursive = TRUE)

load(file.path(data_out_dir, "final_data_18.RData"))
load(file.path(data_out_dir, "final_data_19.RData"))

data_18 <- final_data_18
data_19 <- final_data_19


# Identify common variables, excluding variables available only in one year
common_vars <- intersect(names(data_18), names(data_19))

# Subset datasets to retain only common variables
data_18 <- data_18[common_vars]
data_19 <- data_19[common_vars]

# Add the W variable
data_18$W <- 0  # Year 18
data_19$W <- 1  # Year 19

# Merge the datasets
merged_18_19 <- bind_rows(data_18, data_19)

# Save the merged dataset
#save(merged_18_19, file = "merged_18_19.RData")

# Print structure of merged dataset
str(merged_18_19)




#Start here
###########
###########

# General adjustments for all months
##########################################
##########################################
#load("merged_18_19.RData")

merged_data=merged_18_19

# Remove the 'year' variable 
merged_data <- merged_data %>% select(-year)


# a command to check how many values have each variable in the data
sapply(merged_data, function(x) length(unique(x)))

# Identify variables with more than one unique value
vars_to_keep <- names(merged_data)[sapply(merged_data, function(x) length(unique(x)) > 1)]

# Identify variables with one unique value
vars_to_drop <- names(merged_data)[sapply(merged_data, function(x) length(unique(x)) == 1)]
print(vars_to_drop)

# Subset the dataset to keep only those variables
merged_data <- merged_data[vars_to_keep]


# Remove dots from variable names
clean_names <- gsub("\\.", "", names(merged_data))

# Rename the dataset variables without dots
names(merged_data) <- clean_names


# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(merged_data),
  data_type = sapply(merged_data, class),  # Get the type of each variable
  unique_values = sapply(merged_data, function(x) length(unique(x)))  # Count unique values
)

# Variables starting with sector and ending with exp are interactions between
# two variables and are not needed here; remove them.

names(merged_data)[grepl("^sector.*exp$", names(merged_data))]
merged_data <- merged_data |> select(-matches("^sector.*exp$"))

# Continuous iso variables (import and export) are encoded as factors.

# Identify variables starting with "iso"
iso_vars <- grep("^iso", names(merged_data), value = TRUE)

# Convert them to numeric, replacing the original variables
merged_data <- merged_data %>%
  mutate(across(all_of(iso_vars), ~ as.numeric(as.character(.))))

# Check for missing values after conversion
missing_check <- colSums(is.na(merged_data[iso_vars]))

# Print warning if missing values exist
if (any(missing_check > 0)) {
  warning("Missing values introduced in: ", paste(names(missing_check[missing_check > 0]), collapse = ", "))
} else {
  message("All 'iso' variables converted successfully without missing values.")
}

# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(merged_data),
  data_type = sapply(merged_data, class),  # Get the type of each variable
  unique_values = sapply(merged_data, function(x) length(unique(x)))  # Count unique values
)

# Select column names starting with "iso_import_"
import_vars <- grep("^iso_import_[A-Z]{3}$", names(merged_data), value = TRUE)

# Count how many of these variables are positive in each row
merged_data$NO <- rowSums(merged_data[, import_vars, drop = FALSE] > 0)

# From here, focus on the target month 
#####################################
#######################################

# Filter dataset for month 1
month_1_data <- merged_data %>% filter(month == 10)



# Test differences in the distribution of X for the target month
###################################################################


# Exclude variables that should not be analyzed
excluded_vars <- c("month", "id", "export_future", "W")
numeric_vars <- setdiff(names(month_1_data)[sapply(month_1_data, is.numeric)], excluded_vars)

# Run KS and Wilcoxon tests for each numeric variable
ks_results <- sapply(numeric_vars, function(var) {
  ks.test(month_1_data[[var]][month_1_data$W == 0], 
          month_1_data[[var]][month_1_data$W == 1])$p.value
})

wilcox_results <- sapply(numeric_vars, function(var) {
  wilcox.test(month_1_data[[var]][month_1_data$W == 0], 
              month_1_data[[var]][month_1_data$W == 1])$p.value
})

# Create the data frame with the full results
test_results <- data.frame(Variable = numeric_vars, 
                           KS_p_value = ks_results,
                           Wilcox_p_value = wilcox_results)

# Identify variables significant in at least one of the two tests (p < 0.05)
significant_cont_vars <- test_results %>%
  filter(KS_p_value < 0.05 | Wilcox_p_value < 0.05) %>%
  select(Variable)

# Print variables significant in at least one of the two tests (p < 0.05)
print("Continuous variables significantly different between W = 0 and W = 1:")
print(significant_cont_vars$Variable)




# Select categorical variables only 
categorical_vars <- setdiff(names(month_1_data)[sapply(month_1_data, is.factor)], excluded_vars)

# Run the chi-squared test for each categorical variable
chi_results <- sapply(categorical_vars, function(var) {
  tbl <- table(month_1_data[[var]], month_1_data$W)
  if (min(tbl) > 0) {  # Chi-squared cannot be run with empty cells
    chisq.test(tbl)$p.value
  } else {
    NA
  }
})

# Create the data frame with the results
chi_results_df <- data.frame(Variable = categorical_vars, Chi2_p_value = chi_results)

# Keep only variables with p-value < 0.05
significant_cat_vars <- chi_results_df %>% filter(Chi2_p_value < 0.05)

# Print significant variables (p-value < 0.05)
print("Categorical variables significantly different between W = 0 and W = 1:")
print(significant_cat_vars$Variable)


#Define treatment variable
##################################
# Define treatment variable (assuming it's binary: 0 = control, 1 = treated)
treatment_var <- "W"



# Data adjustments for the target month
##########################################

#load("month_1_data.RData")
# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(month_1_data),
  data_type = sapply(month_1_data, class),  # Get the type of each variable
  unique_values = sapply(month_1_data, function(x) length(unique(x)))  # Count unique values
)

# Identify variables with one unique value
vars_to_drop <- names(month_1_data)[sapply(month_1_data, function(x) length(unique(x)) == 1)]
print(vars_to_drop)

# Subset the dataset to keep only those variables
month_1_data <- month_1_data |>  select(-all_of(vars_to_drop))

# Identify factor variables with exactly two levels
binary_factors <- sapply(month_1_data, function(x) is.factor(x) && nlevels(x) == 2)

# Convert each one to numeric 0/1 based on the level order
month_1_data[binary_factors] <- lapply(month_1_data[binary_factors], function(x) {
  as.numeric(as.character(x))
})


# Create dummies from the levels of the "size" factor
size_dummies <- model.matrix(~ size - 1, data = month_1_data)

# Rename columns by removing the "size" prefix
colnames(size_dummies) <- gsub("^size", "", colnames(size_dummies))

# Add the columns to the dataset
month_1_data <- cbind(month_1_data, size_dummies)

# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(month_1_data),
  data_type = sapply(month_1_data, class),  # Get the type of each variable
  unique_values = sapply(month_1_data, function(x) length(unique(x)))  # Count unique values
)

# Remove size
month_1_data <- month_1_data |>  select(-size)



#### Define useful vectors and matrix


# Outcome
Y = month_1_data$export_future

# Treatment
W = month_1_data$W

# Create main effects matrix
vars_to_exclude <- c("W", "month", "id", "export_future", "propensity_score")
# Take all variable names except those to exclude
vars <- setdiff(names(month_1_data), vars_to_exclude)
# Create the formula dynamically
formula_str <- paste("~ 0 +", paste(vars, collapse = " + "))
# Create the model matrix
X <- model.matrix(as.formula(formula_str), data = month_1_data)



library(grf)
library(estimatr)

set.seed(1598)
n_splits <- 100
n_rows <- nrow(X)

for (i in 1:n_splits) {
  indices <- sample(1:n_rows, size = 0.6 * n_rows)
  X_train <- X[indices, ]
  X_test  <- X[-indices, ]
  W_train <- W[indices]
  W_test  <- W[-indices]
  Y_train <- Y[indices]
  Y_test  <- Y[-indices]
  
  # S-Learner
  WX_train <- cbind(W_train, X_train)
  rf_sl <- regression_forest(WX_train, Y_train)
  W0X_test <- cbind(rep(0, nrow(X_test)), X_test)
  W1X_test <- cbind(rep(1, nrow(X_test)), X_test)
  cate_sl_test <- predict(rf_sl, W1X_test)$predictions - predict(rf_sl, W0X_test)$predictions
  result_s_learner <- cbind(cate_sl_test, X_test)
  save(result_s_learner, file = file.path(split_output_dir, paste0("cates_s_october_learner_split", i, ".RData")))
  
  # T-Learner
  rfm1_tl <- regression_forest(X_train[W_train == 1, ], Y_train[W_train == 1])
  rfm0_tl <- regression_forest(X_train[W_train == 0, ], Y_train[W_train == 0])
  mu1_test <- predict(rfm1_tl, X_test)$predictions
  mu0_test <- predict(rfm0_tl, X_test)$predictions
  cate_tl_all <- mu1_test - mu0_test
  result_t_learner <- cbind(cate_tl_all, X_test)
  save(result_t_learner, file = file.path(split_output_dir, paste0("cates_t_october_learner_split", i, ".RData")))
  
  # R-Learner
  m_rf <- regression_forest(X_train, Y_train)
  mhat_train <- predict(m_rf)$predictions
  e_rf <- regression_forest(X_train, W_train)
  ehat_train <- predict(e_rf)$predictions
  res_y_train <- Y_train - mhat_train
  res_w_train <- W_train - ehat_train
  X_wc_train <- cbind(1, X_train)
  Xstar_train <- X_wc_train * res_w_train
  rl_ols <- lm(res_y_train ~ 0 + Xstar_train)
  X_wc_test <- cbind(1, X_test)
  pseudo_rl_train <- res_y_train / res_w_train
  weights_rl_train <- res_w_train^2
  rrf_fit <- regression_forest(X_train, pseudo_rl_train, sample.weights = weights_rl_train)
  cate_rl_rf_test <- predict(rrf_fit, X_test)$predictions
  result_r_learner <- cbind(cate_rl_rf_test, X_test)
  save(result_r_learner, file = file.path(split_output_dir, paste0("cates_r_october_learner_split", i, ".RData")))
  
  # DR-Learner
  rfm0 <- regression_forest(X_train[W_train == 0, ], Y_train[W_train == 0])
  rfm1 <- regression_forest(X_train[W_train == 1, ], Y_train[W_train == 1])
  m0hat_train <- predict(rfm0, X_train)$predictions
  m1hat_train <- predict(rfm1, X_train)$predictions
  rfp <- regression_forest(X_train, W_train)
  ehat_train <- predict(rfp)$predictions
  Y_tilde_train <- m1hat_train - m0hat_train +
    W_train * (Y_train - m1hat_train) / ehat_train -
    (1 - W_train) * (Y_train - m0hat_train) / (1 - ehat_train)
  rf_dr <- regression_forest(X_train, Y_tilde_train)
  cate_dr_test <- predict(rf_dr, X_test)$predictions
  result_dr_learner <- cbind(cate_dr_test, X_test)
  save(result_dr_learner, file = file.path(split_output_dir, paste0("cates_dr_october_learner_split", i, ".RData")))
  
  # CF-Learner
  CF <- causal_forest(X_train, Y_train, W_train, tune.parameters = "all")
  cate_cf_test <- predict(CF, X_test)$predictions
  result_cf_learner <- cbind(cate_cf_test, X_test)
  save(result_cf_learner, file = file.path(split_output_dir, paste0("cates_cf_october_learner_split", i, ".RData")))
  
  # Best Linear Predictor (BLP)
  aipw_test <- DML_aipw(Y_test, W_test, X_test)
  pseudoY <- aipw_test$ATE$delta
  pseudoY_all <- pseudoY
  
  cates_list_all <- list(
    cate_sl_test,
    cate_tl_all,
    cate_rl_rf_test,
    cate_dr_test,
    cate_cf_test
  )
  
  names(cates_list_all) <- c(
    "S-learner", "T-learner",
    "R-learner", "DR-learner", "Causal Forest"
  )
  
  blp_results_all <- lapply(cates_list_all, function(cates) {
    demeaned_cates <- cates - mean(cates)
    lm_blp <- lm_robust(pseudoY_all ~ demeaned_cates)
    return(summary(lm_blp))
  })
  
  print(blp_results_all)
  
  save(blp_results_all, file = file.path(split_output_dir, paste0("blp_all_october_learners_split", i, ".RData")))
  # GATES Estimates
  K <- 4
  method_colors <- RColorBrewer::brewer.pal(7, "Set1")
  names(method_colors) <- names(cates_list_all)
  
  gates_data_list <- mapply(function(cates, method_name) {
    slices <- factor(as.numeric(cut(cates, breaks = quantile(cates, probs = seq(0, 1, length = K + 1)), include.lowest = TRUE)))
    G_ind <- model.matrix(~ 0 + slices)
    gates_fit <- lm_robust(pseudoY_all ~ 0 + G_ind)
    se <- gates_fit$std.error
    gates_df <- data.frame(
      Method = method_name,
      Group = factor(paste("Group", 1:K), levels = paste("Group", 1:K)),
      Coefficient = gates_fit$coefficients,
      cil = gates_fit$coefficients - 1.96 * se,
      ciu = gates_fit$coefficients + 1.96 * se
    )
    return(gates_df)
  }, cates_list_all, names(cates_list_all), SIMPLIFY = FALSE)
  
  gates_combined_df <- do.call(rbind, gates_data_list)
  
  save(gates_combined_df, file = file.path(split_output_dir, paste0("gates_df_split_october_", i, ".RData")))
  
} 



# Save the BLP beta and
# p-value, the GATES betas and confidence intervals, and the CLAN beta and p-value for all variables.













