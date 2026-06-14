# ============================================================
# TABLE 7 REPLICATION: GATES across repeated A-M splits
# FINAL-PAPER VERSION
# ============================================================

# ============================================================
# NOTE ON TABLE 7 (GATES OF THE CATE)
# ============================================================
#
# For each month and each learner, the paper partitions the
# predicted CATE distribution into K = 4 quartile groups:
#
#   G_k = { \hat{\Delta}(X) in I_k },  k = 1, ..., 4
#
# and estimates the Group Average Treatment Effects (GATES):
#
#   gamma_k = E[ \tilde{Y}^{ATE} | G_k ]
#
# where \tilde{Y}^{ATE} is the AIPW pseudo-outcome computed
# on the main sample M, and the predicted CATE is estimated in
# the auxiliary sample A and evaluated in M.
#
# Interpretation:
# - if the gamma_k are similar across groups, there is little
#   evidence of systematic treatment effect heterogeneity;
# - if the gamma_k differ across groups, the learner is detecting
#   heterogeneity across the CATE distribution.
#
# Table 7 reports, for each month / learner / group:
# - the median GATES coefficient across repeated random A-M splits
# - the corresponding median p-value.
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
script_path <- Sys.getenv("OBES_TAB7_SCRIPT", unset = NA_character_)
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
output_dir <- normalizePath(Sys.getenv("OBES_TAB7_OUTPUT_DIR", script_dir), winslash = "/", mustWork = FALSE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# SPEED CONTROLS
# ---------------------------
N_SPLITS <- 100
TRAIN_SHARE <- 0.6
MONTHS_TO_RUN <- 1:12
BASE_SEED <- 1598

NUM_TREES_RF <- 2000
NUM_TREES_CF <- 2000
TUNE_GRF <- "all"
AIPW_CF_FOLDS <- 5
K_GATES <- 4

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
    "S-Learner" = cate_sl_test,
    "T-Learner" = cate_tl_all,
    "R-Learner" = cate_rl_rf_test,
    "DR-Learner" = cate_dr_test,
    "Generalized Random Forest" = cate_cf_test
  )
}

# ---------------------------
# Helper: GATES on one split
# ---------------------------
run_gates_one_split <- function(cates_list, Y_test, W_test, X_test, id_test, K = K_GATES) {
  
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
    
    # quartile groups based on predicted CATE
    probs <- seq(0, 1, length.out = K + 1)
    brks <- quantile(cates, probs = probs, na.rm = TRUE)
    
    # guard against duplicated quantile cutpoints
    brks <- unique(brks)
    
    if (length(brks) < 2) {
      return(data.frame(
        Method = method_name,
        Group = paste0("Group", 1:K),
        gamma = NA_real_,
        p_value = NA_real_
      ))
    }
    
    slices_num <- cut(
      cates,
      breaks = brks,
      include.lowest = TRUE,
      labels = FALSE
    )
    
    # if ties collapse some groups, reindex and keep consistent labels
    observed_groups <- sort(unique(stats::na.omit(slices_num)))
    slices <- factor(slices_num, levels = observed_groups)
    
    G_ind <- model.matrix(~ 0 + slices)
    
    fit <- lm_robust(
      pseudoY ~ 0 + G_ind,
      clusters = id_test
    )
    
    coef_table <- summary(fit)$coefficients
    
    out <- data.frame(
      Method = method_name,
      Group = paste0("Group", observed_groups),
      gamma = unname(coef(fit)),
      p_value = coef_table[, "Pr(>|t|)"],
      stringsAsFactors = FALSE
    )
    
    # fill absent groups with NA so every split has Group1..GroupK
    full_out <- data.frame(
      Method = method_name,
      Group = paste0("Group", 1:K),
      gamma = NA_real_,
      p_value = NA_real_,
      stringsAsFactors = FALSE
    )
    
    full_out[match(out$Group, full_out$Group), c("gamma", "p_value")] <- out[, c("gamma", "p_value")]
    full_out
  })
}

# ---------------------------
# Main function: one month, many splits
# ---------------------------
run_month_gates <- function(month_num, data_in, n_splits = N_SPLITS,
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
    
    run_gates_one_split(cates_list, Y_test, W_test, X_test, id_test, K = K_GATES) %>%
      mutate(split = s, month = month_num)
  })
  
  month_summary <- split_results %>%
    group_by(month, Method, Group) %>%
    summarise(
      gamma = median(gamma, na.rm = TRUE),
      p_value = median(p_value, na.rm = TRUE),
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
  
  all_results[[j]] <- run_month_gates(
    month_num = m,
    data_in = merged_data
  )
}

# Raw results across all chosen months/splits
gates_raw_df <- bind_rows(lapply(all_results, `[[`, "raw"))

# Long table
gates_table7_df <- bind_rows(lapply(all_results, `[[`, "summary")) %>%
  mutate(
    Month = factor(month_labels[month], levels = month_labels),
    Method = factor(
      Method,
      levels = c("S-Learner", "T-Learner", "R-Learner", "DR-Learner", "Generalized Random Forest")
    ),
    Group_num = as.integer(gsub("Group", "", Group))
  ) %>%
  select(Method, Group, Group_num, Month, gamma, p_value) %>%
  arrange(Method, Group_num, Month)

print(gates_table7_df)

# ---------------------------
# Wide display table like Table 7
# ---------------------------
table7_display <- gates_table7_df %>%
  mutate(cell = sprintf("%.3f\n(%.3f)", gamma, p_value)) %>%
  select(Method, Group, Group_num, Month, cell) %>%
  pivot_wider(names_from = Month, values_from = cell) %>%
  arrange(Method, Group_num) %>%
  select(-Group_num)

print(table7_display, width = Inf)

# ---------------------------
# Optional: LaTeX-ready table body
# ---------------------------
table7_latex <- gates_table7_df %>%
  mutate(
    gamma = sprintf("%.3f", gamma),
    p_value = sprintf("(%.3f)", p_value),
    cell = paste(gamma, p_value, sep = " \\\\ ")
  ) %>%
  select(Method, Group, Group_num, Month, cell) %>%
  pivot_wider(names_from = Month, values_from = cell) %>%
  arrange(Method, Group_num) %>%
  select(-Group_num)

print(table7_latex, width = Inf)

# ---------------------------
# Save outputs
# ---------------------------
save(gates_raw_df, gates_table7_df, table7_display,
     file = file.path(output_dir, "table7_gates_results.RData"))

write.csv(gates_table7_df,
          file.path(output_dir, "table7_gates_results.csv"),
          row.names = FALSE)

write.csv(table7_display,
          file.path(output_dir, "table7_gates_display.csv"),
          row.names = FALSE)
