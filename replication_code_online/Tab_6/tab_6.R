# ============================================================
# TABLE 6 REPLICATION: BLP across repeated A-M splits for all months
# FINAL-PAPER VERSION
# ============================================================

# ============================================================
# NOTE ON TABLE 6 (BLP OF THE CATE)
# ============================================================
#
# What Table 6 does:
# For each month and for each learner, the paper runs the
# Best Linear Predictor (BLP) regression on the main sample M:
#
#   \tilde{Y}^{ATE} = beta_1 + beta_2 * ( \hat{\Delta}(X) - E[\hat{\Delta}(X)] ) + u
#
# where:
# - \tilde{Y}^{ATE} is the AIPW pseudo-outcome;
# - \hat{\Delta}(X) is the learner-specific predicted CATE,
#   estimated in the auxiliary sample A and evaluated in the main sample M;
# - beta_2 is the coefficient of interest.
#
# Interpretation of beta_2:
# - if beta_2 > 0 and statistically significant, the learner is picking up
#   genuine treatment effect heterogeneity;
# - if beta_2 is close to 1, the estimated CATE is well aligned with the
#   true CATE in scale;
# - a higher R^2 means that the learner explains a larger share of the
#   heterogeneity signal in the pseudo-outcome.
#
# Why this works:
# the AIPW pseudo-outcome satisfies
#
#   E[ \tilde{Y}^{ATE} | X ] = \Delta(X),
#
# so regressing the pseudo-outcome on the estimated CATE provides a way to
# test whether the estimated CATE contains information about the true CATE.
#
# Important nuance:
# if \hat{\Delta}(X) != \Delta(X), this means the CATE is not estimated
# perfectly, but not necessarily uselessly.
# For example, the estimated CATE may still rank observations correctly
# even if it is mis-scaled. Therefore, the BLP is not testing exact recovery
# of the true CATE; rather, it tests whether the estimated CATE contains
# meaningful signal about heterogeneity in the true treatment effect.
#
# In Table 6, the reported numbers are, for each month and learner:
# - the median beta_2 across repeated random A-M splits,
# - the corresponding median p-value,
# - and the median R^2 of the BLP regression.
# ============================================================

rm(list = ls())

# ---------------------------
# Libraries
# ---------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(grf)
library(causalDML)
library(estimatr)
library(haven)

# ---------------------------
# Paths
# ---------------------------
script_path <- Sys.getenv("OBES_TAB6_SCRIPT", unset = NA_character_)
if (!is.na(script_path) && nzchar(script_path) && file.exists(script_path)) {
  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
} else {
  script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
}
if (is.na(script_path)) {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  script_candidate <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else NA_character_
  script_path <- if (!is.na(script_candidate) && nzchar(script_candidate) && file.exists(script_candidate)) normalizePath(script_candidate, winslash = "/", mustWork = TRUE) else NA_character_
}
script_dir <- if (is.na(script_path)) normalizePath(getwd(), winslash = "/", mustWork = TRUE) else dirname(script_path)
replication_dir <- normalizePath(Sys.getenv("OBES_REPLICATION_DIR", file.path(script_dir, "..")), winslash = "/", mustWork = FALSE)
data_out_dir <- normalizePath(Sys.getenv("OBES_DATA_OUT_DIR", file.path(replication_dir, "data", "data_out")), winslash = "/", mustWork = FALSE)
output_dir <- normalizePath(Sys.getenv("OBES_TAB6_OUTPUT_DIR", script_dir), winslash = "/", mustWork = FALSE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# SPEED CONTROLS
# ---------------------------
# FINAL-PAPER SETTINGS
N_SPLITS <- 100
TRAIN_SHARE <- 0.6
MONTHS_TO_RUN <- 1:12           # use 1:12 for all months, or e.g. 12 for one month
BASE_SEED <- 1598

NUM_TREES_RF <- 2000
NUM_TREES_CF <- 2000
TUNE_GRF <- "all"
AIPW_CF_FOLDS <- 5

# For quick smoke tests, users can temporarily reduce N_SPLITS,
# NUM_TREES_RF, NUM_TREES_CF, and AIPW_CF_FOLDS.

# ---------------------------
# Load data
# ---------------------------
load(file.path(data_out_dir, "final_data_18.RData"))
load(file.path(data_out_dir, "final_data_19.RData"))

final_data_18_subset <- final_data_18
final_data_19_subset <- final_data_19

# ---------------------------
# Build pooled dataset
# ---------------------------
data_18 <- final_data_18_subset
data_19 <- final_data_19_subset

common_vars <- intersect(names(data_18), names(data_19))
data_18 <- data_18[common_vars]
data_19 <- data_19[common_vars]

data_18$W <- 0
data_19$W <- 1

merged_18_19 <- bind_rows(data_18, data_19)

# ---------------------------
# Global preprocessing
# ---------------------------
merged_data <- merged_18_19 %>%
  select(-year)

vars_to_keep <- names(merged_data)[sapply(merged_data, function(x) length(unique(x)) > 1)]
merged_data <- merged_data[vars_to_keep]

names(merged_data) <- gsub("\\.", "", names(merged_data))

merged_data <- merged_data %>% select(-matches("^sector.*exp$"))

iso_vars <- grep("^iso", names(merged_data), value = TRUE)
merged_data <- merged_data %>%
  mutate(across(all_of(iso_vars), ~ as.numeric(as.character(.))))

import_vars <- grep("^iso_import_[A-Z]{3}$", names(merged_data), value = TRUE)
merged_data$NO <- rowSums(merged_data[, import_vars, drop = FALSE] > 0, na.rm = TRUE)

# ---------------------------
# Helper: month-specific preprocessing
# ---------------------------
prepare_month_data <- function(month_num, data_in) {
  month_data <- data_in %>% filter(month == month_num)
  
  vars_to_drop <- names(month_data)[sapply(month_data, function(x) length(unique(x)) == 1)]
  month_data <- month_data %>% select(-all_of(vars_to_drop))
  
  binary_factors <- sapply(month_data, function(x) is.factor(x) && nlevels(x) == 2)
  month_data[binary_factors] <- lapply(month_data[binary_factors], function(x) {
    as.numeric(as.character(x))
  })
  
  if ("size" %in% names(month_data)) {
    size_dummies <- model.matrix(~ size - 1, data = month_data)
    colnames(size_dummies) <- gsub("^size", "", colnames(size_dummies))
    month_data <- cbind(month_data, size_dummies)
    month_data <- month_data %>% select(-size)
  }
  
  month_data
}

# ---------------------------
# Helper: build X, Y, W, id
# ---------------------------
build_design <- function(month_data) {
  Y <- month_data$export_future
  W <- month_data$W
  id_vec <- month_data$id
  
  vars_to_exclude <- c("W", "month", "id", "export_future", "propensity_score")
  vars <- setdiff(names(month_data), vars_to_exclude)
  
  formula_str <- paste("~ 0 +", paste(vars, collapse = " + "))
  X <- model.matrix(as.formula(formula_str), data = month_data)
  
  list(Y = Y, W = W, X = X, id = id_vec)
}

# ---------------------------
# Helper: estimate all CATE learners on one split
# ---------------------------
fit_learners_one_split <- function(X_train, Y_train, W_train, X_test) {
  
  # ---------- S-learner ----------
  WX_train <- cbind(W_train, X_train)
  rf_sl <- regression_forest(
    WX_train, Y_train,
    num.trees = NUM_TREES_RF
  )
  
  W0X_test <- cbind(rep(0, nrow(X_test)), X_test)
  W1X_test <- cbind(rep(1, nrow(X_test)), X_test)
  
  cate_sl_test <- predict(rf_sl, W1X_test)$predictions -
    predict(rf_sl, W0X_test)$predictions
  
  # ---------- T-learner ----------
  rfm1_tl <- regression_forest(
    X_train[W_train == 1, , drop = FALSE],
    Y_train[W_train == 1],
    num.trees = NUM_TREES_RF
  )
  rfm0_tl <- regression_forest(
    X_train[W_train == 0, , drop = FALSE],
    Y_train[W_train == 0],
    num.trees = NUM_TREES_RF
  )
  
  mu1_test <- predict(rfm1_tl, X_test)$predictions
  mu0_test <- predict(rfm0_tl, X_test)$predictions
  cate_tl_all <- mu1_test - mu0_test
  
  # ---------- R-learner ----------
  m_rf <- regression_forest(
    X_train, Y_train,
    num.trees = NUM_TREES_RF
  )
  mhat_train <- predict(m_rf)$predictions
  
  e_rf <- regression_forest(
    X_train, W_train,
    num.trees = NUM_TREES_RF
  )
  ehat_train <- predict(e_rf)$predictions
  
  res_y_train <- Y_train - mhat_train
  res_w_train <- W_train - ehat_train
  
  keep_rl <- abs(res_w_train) > 1e-6
  pseudo_rl_train <- res_y_train[keep_rl] / res_w_train[keep_rl]
  weights_rl_train <- res_w_train[keep_rl]^2
  
  rrf_fit <- regression_forest(
    X_train[keep_rl, , drop = FALSE],
    pseudo_rl_train,
    sample.weights = weights_rl_train,
    num.trees = NUM_TREES_RF
  )
  cate_rl_rf_test <- predict(rrf_fit, X_test)$predictions
  
  # ---------- DR-learner ----------
  rfm0 <- regression_forest(
    X_train[W_train == 0, , drop = FALSE],
    Y_train[W_train == 0],
    num.trees = NUM_TREES_RF
  )
  rfm1 <- regression_forest(
    X_train[W_train == 1, , drop = FALSE],
    Y_train[W_train == 1],
    num.trees = NUM_TREES_RF
  )
  
  m0hat_train <- predict(rfm0, X_train)$predictions
  m1hat_train <- predict(rfm1, X_train)$predictions
  
  rfp <- regression_forest(
    X_train, W_train,
    num.trees = NUM_TREES_RF
  )
  ehat_train_dr <- predict(rfp)$predictions
  ehat_train_dr <- pmin(pmax(ehat_train_dr, 0.01), 0.99)
  
  Y_tilde_train <- m1hat_train - m0hat_train +
    W_train * (Y_train - m1hat_train) / ehat_train_dr -
    (1 - W_train) * (Y_train - m0hat_train) / (1 - ehat_train_dr)
  
  rf_dr <- regression_forest(
    X_train, Y_tilde_train,
    num.trees = NUM_TREES_RF
  )
  cate_dr_test <- predict(rf_dr, X_test)$predictions
  
  # ---------- Causal forest ----------
  cf_fit <- causal_forest(
    X_train, Y_train, W_train,
    num.trees = NUM_TREES_CF,
    tune.parameters = TUNE_GRF
  )
  cate_cf_test <- predict(cf_fit, X_test)$predictions
  
  list(
    "S-learner" = cate_sl_test,
    "T-learner" = cate_tl_all,
    "R-learner" = cate_rl_rf_test,
    "DR-learner" = cate_dr_test,
    "Causal RF" = cate_cf_test
  )
}

# ---------------------------
# Helper: BLP on one split

#This function evaluates the learners for one particular train/test split.
#It receives:
# - cates_list: the predicted CATEs from each learner on the test sample
# - Y_test: observed outcomes in the test sample
# - W_test: treatment indicator in the test sample
# - X_test: covariates in the test sample
# - id_test: firm ids in the test sample, used for clustered standard errors

#notes:
#the code computes an AIPW pseudo-outcome; this pseudo-outcome is a stand-in for the true individual 
#treatment effect and we do that because the true CATE is not observed. So instead of regressing the 
#true treatment effect on the predicted CATE, the code uses a pseudo-outcome whose conditional mean 
#equals the true CATE.
# ---------------------------
run_blp_one_split <- function(cates_list, Y_test, W_test, X_test, id_test) {
  
  forest <- create_method(
    "forest_grf",
    args = list(tune.parameters = TUNE_GRF)
  )
  
  aipw_test <- DML_aipw(
    Y_test, W_test, X_test,
    ml_w = list(forest),
    ml_y = list(forest),
    cf = AIPW_CF_FOLDS
  )
  
  pseudoY <- aipw_test$ATE$delta
  
  map_dfr(names(cates_list), function(method_name) {
    cates <- cates_list[[method_name]]
    demeaned_cates <- cates - mean(cates, na.rm = TRUE)
    
    fit <- lm_robust(
      pseudoY ~ demeaned_cates,
      clusters = id_test
    )
    
    coef_table <- summary(fit)$coefficients
    
    data.frame(
      Method = method_name,
      beta2 = unname(coef(fit)[["demeaned_cates"]]),
      p_value = coef_table["demeaned_cates", "Pr(>|t|)"],
      r2 = fit$r.squared
    )
  })
}

# ---------------------------
# Main function: one month, many splits
# ---------------------------
run_month_blp <- function(month_num, data_in, n_splits = N_SPLITS,
                          train_share = TRAIN_SHARE, base_seed = BASE_SEED) {
  
  month_data <- prepare_month_data(month_num, data_in)
  des <- build_design(month_data)
  
  Y <- des$Y
  W <- des$W
  X <- des$X
  id_vec <- des$id
  
  n <- nrow(X)
  
  split_results <- map_dfr(seq_len(n_splits), function(s) {
    set.seed(base_seed + month_num * 10000 + s)
    
    train_idx <- sample(seq_len(n), size = floor(train_share * n))
    test_idx <- setdiff(seq_len(n), train_idx)
    
    X_train <- X[train_idx, , drop = FALSE]
    X_test  <- X[test_idx, , drop = FALSE]
    W_train <- W[train_idx]
    W_test  <- W[test_idx]
    Y_train <- Y[train_idx]
    Y_test  <- Y[test_idx]
    id_test <- id_vec[test_idx]
    
    cates_list <- fit_learners_one_split(
      X_train = X_train,
      Y_train = Y_train,
      W_train = W_train,
      X_test = X_test
    )
    
    run_blp_one_split(cates_list, Y_test, W_test, X_test, id_test) %>%
      mutate(split = s, month = month_num)
  })
  
  month_summary <- split_results %>%
    group_by(month, Method) %>%
    summarise(
      beta2 = median(beta2, na.rm = TRUE),
      p_value = median(p_value, na.rm = TRUE),
      r2 = median(r2, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(raw = split_results, summary = month_summary)
}

# ---------------------------
# Run selected months
# ---------------------------
month_labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

months_vec <- if (length(MONTHS_TO_RUN) == 1) MONTHS_TO_RUN else MONTHS_TO_RUN
all_results <- vector("list", length = length(months_vec))

for (j in seq_along(months_vec)) {
  m <- months_vec[j]
  cat("Running month", m, "with", N_SPLITS, "splits...\n")
  
  all_results[[j]] <- run_month_blp(
    month_num = m,
    data_in = merged_data
  )
}

# Raw results across all chosen months/splits
blp_raw_df <- bind_rows(lapply(all_results, `[[`, "raw"))

# Median table
blp_table6_df <- bind_rows(lapply(all_results, `[[`, "summary")) %>%
  mutate(
    Month = factor(month_labels[month], levels = month_labels),
    Method = factor(Method, levels = c("S-learner", "T-learner", "R-learner", "DR-learner", "Causal RF"))
  ) %>%
  select(Month, Method, beta2, p_value, r2) %>%
  arrange(Month, Method)

print(blp_table6_df)

# Wide display table like Table 6
table6_display <- blp_table6_df %>%
  mutate(cell = sprintf("%.3f\n(%.3f)\nR^2 = %.5f", beta2, p_value, r2)) %>%
  select(Month, Method, cell) %>%
  pivot_wider(names_from = Method, values_from = cell)

print(table6_display, width = Inf)

# Save outputs
save(blp_raw_df, blp_table6_df, table6_display,
     file = file.path(output_dir, "table6_blp_results.RData"))

write.csv(blp_table6_df,
          file.path(output_dir, "table6_blp_results.csv"),
          row.names = FALSE)

write.csv(table6_display,
          file.path(output_dir, "table6_blp_display.csv"),
          row.names = FALSE)
