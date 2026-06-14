get_section45_dir <- function() {
  env_dir <- Sys.getenv("OBES_SECTION45_SCRIPT_DIR", unset = "")
  if (nzchar(env_dir)) {
    return(normalizePath(env_dir, winslash = "/", mustWork = FALSE))
  }

  script_path <- tryCatch(
    normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
    error = function(e) NA_character_
  )

  if (is.na(script_path)) {
    command_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", command_args, value = TRUE)
    if (length(file_arg) > 0) {
      script_path <- normalizePath(sub("^--file=", "", file_arg[1]),
                                   winslash = "/", mustWork = TRUE)
    }
  }

  if (is.na(script_path)) {
    normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  } else {
    dirname(script_path)
  }
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grf)
  library(causalDML)
  library(estimatr)
  library(gridExtra)
  library(RColorBrewer)
})

month_names <- c(
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december"
)

make_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

prepare_section45_data <- function(data_out_dir) {
  load(file.path(data_out_dir, "final_data_18.RData"))
  load(file.path(data_out_dir, "final_data_19.RData"))

  data_18 <- final_data_18
  data_19 <- final_data_19
  common_vars <- intersect(names(data_18), names(data_19))

  data_18 <- data_18[common_vars]
  data_19 <- data_19[common_vars]
  data_18$W <- 0
  data_19$W <- 1

  merged_data <- bind_rows(data_18, data_19) %>%
    select(-year)

  vars_to_keep <- names(merged_data)[sapply(merged_data, function(x) length(unique(x)) > 1)]
  merged_data <- merged_data[vars_to_keep]
  names(merged_data) <- gsub("\\.", "", names(merged_data))
  merged_data <- merged_data %>% select(-matches("^sector.*exp$"))

  iso_vars <- grep("^iso", names(merged_data), value = TRUE)
  merged_data <- merged_data %>%
    mutate(across(all_of(iso_vars), ~ as.numeric(as.character(.))))

  import_vars <- grep("^iso_import_[A-Z]{3}$", names(merged_data), value = TRUE)
  merged_data$NO <- rowSums(merged_data[, import_vars, drop = FALSE] > 0)

  merged_data
}

prepare_month_design <- function(merged_data, month_num) {
  month_data <- merged_data %>% filter(month == month_num)

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

  Y <- month_data$export_future
  W <- month_data$W
  vars_to_exclude <- c("W", "month", "id", "export_future", "propensity_score")
  vars <- setdiff(names(month_data), vars_to_exclude)
  formula_str <- paste("~ 0 +", paste(vars, collapse = " + "))
  X <- model.matrix(as.formula(formula_str), data = month_data)

  list(month_data = month_data, Y = Y, W = W, X = X)
}

save_aipw_and_propensity_figures <- function(Y, W, X, month_num, effect_dir,
                                             propensity_dir) {
  forest <- create_method("forest_grf", args = list(tune.parameters = "all"))

  set.seed(12344)
  pl_5f <- DML_partial_linear(Y, W, X, ml_w = list(forest), ml_y = list(forest), cf = 5)

  propensity_df <- data.frame(
    ehat = pl_5f$e_hat,
    treated = factor(W, levels = c(0, 1), labels = c("Control", "Treated"))
  )

  propensity_plot <- ggplot(propensity_df, aes(x = ehat, fill = treated)) +
    geom_density(alpha = 0.5) +
    labs(
      title = "Propensity Score Distribution (5-fold GRF)",
      x = "Estimated Propensity Score",
      y = "Density",
      fill = "Group"
    ) +
    theme_minimal()

  propensity_file <- file.path(
    propensity_dir,
    paste0("propensity_plot_month_", month_num, "5_folds.pdf")
  )
  ggsave(propensity_file, propensity_plot, width = 6.5, height = 4.5)

  write.csv(
    propensity_df,
    file.path(propensity_dir, paste0("propensity_plot_month_", month_num, "5_folds_values.csv")),
    row.names = FALSE
  )

  propensity_summary <- propensity_df %>%
    group_by(treated) %>%
    summarise(
      n = n(),
      min = min(ehat),
      q1 = quantile(ehat, 0.25),
      median = median(ehat),
      mean = mean(ehat),
      q3 = quantile(ehat, 0.75),
      max = max(ehat),
      .groups = "drop"
    )
  write.csv(
    propensity_summary,
    file.path(propensity_dir, paste0("propensity_plot_month_", month_num, "5_folds_summary.csv")),
    row.names = FALSE
  )

  set.seed(12344)
  aipw <- DML_aipw(Y, W, X, ml_w = list(forest), ml_y = list(forest), cf = 5)
  apo_att <- APO_dml_atet(Y, aipw$APO$m_mat, aipw$APO$w_mat,
                          aipw$APO$e_mat, aipw$APO$cf_mat)
  att <- ATE_dml(apo_att)

  att_result <- if (!is.null(att$results)) att$results else att$result
  effect_df <- data.frame(
    Effect = c(aipw$ATE$result[1], att_result[1]),
    se = c(aipw$ATE$result[2], att_result[2]),
    Target = c("ATE", "ATT")
  ) %>%
    mutate(
      cil = Effect - 1.96 * se,
      ciu = Effect + 1.96 * se
    )

  effect_plot <- ggplot(effect_df, aes(x = Target, y = Effect, ymin = cil, ymax = ciu)) +
    geom_point(size = 2.5) +
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept = 0) +
    xlab("Target parameter")

  effect_file <- file.path(effect_dir, paste0("effect_plot_month_", month_num, "r.pdf"))
  ggsave(effect_file, effect_plot, width = 5.5, height = 4)

  write.csv(effect_df,
            file.path(effect_dir, paste0("effect_plot_month_", month_num, "r_values.csv")),
            row.names = FALSE)

  list(
    pl_5f = pl_5f,
    aipw = aipw,
    att = att,
    propensity_file = propensity_file,
    effect_file = effect_file
  )
}

estimate_cates_and_gates <- function(Y, W, X, month_num, clans_dir, gates_dir) {
  month_name <- month_names[month_num]

  set.seed(1598)
  n_rows <- nrow(X)
  indices <- sample(seq_len(n_rows), size = 0.6 * n_rows)

  X_train <- X[indices, , drop = FALSE]
  X_test <- X[-indices, , drop = FALSE]
  W_train <- W[indices]
  W_test <- W[-indices]
  Y_train <- Y[indices]
  Y_test <- Y[-indices]

  WX_train <- cbind(W_train, X_train)
  rf_sl <- regression_forest(WX_train, Y_train)
  W0X_test <- cbind(rep(0, nrow(X_test)), X_test)
  W1X_test <- cbind(rep(1, nrow(X_test)), X_test)
  cate_sl_test <- predict(rf_sl, W1X_test)$predictions -
    predict(rf_sl, W0X_test)$predictions
  result_s_learner <- cbind(cate_sl_test, X_test)
  save(result_s_learner,
       file = file.path(clans_dir, paste0("cates_s_learner_", month_name, ".RData")))

  rfm1_tl <- regression_forest(X_train[W_train == 1, , drop = FALSE], Y_train[W_train == 1])
  rfm0_tl <- regression_forest(X_train[W_train == 0, , drop = FALSE], Y_train[W_train == 0])
  mu1_test <- predict(rfm1_tl, X_test)$predictions
  mu0_test <- predict(rfm0_tl, X_test)$predictions
  cate_tl_all <- mu1_test - mu0_test
  result_t_learner <- cbind(cate_tl_all, X_test)
  save(result_t_learner,
       file = file.path(clans_dir, paste0("cates_t_learner_", month_name, ".RData")))

  m_rf <- regression_forest(X_train, Y_train)
  mhat_train <- predict(m_rf)$predictions
  e_rf <- regression_forest(X_train, W_train)
  ehat_train <- predict(e_rf)$predictions
  res_y_train <- Y_train - mhat_train
  res_w_train <- W_train - ehat_train
  pseudo_rl_train <- res_y_train / res_w_train
  weights_rl_train <- res_w_train^2
  rrf_fit <- regression_forest(X_train, pseudo_rl_train, sample.weights = weights_rl_train)
  cate_rl_rf_test <- predict(rrf_fit, X_test)$predictions
  result_r_learner <- cbind(cate_rl_rf_test, X_test)
  save(result_r_learner,
       file = file.path(clans_dir, paste0("cates_r_learner_", month_name, ".RData")))

  rfm0 <- regression_forest(X_train[W_train == 0, , drop = FALSE], Y_train[W_train == 0])
  rfm1 <- regression_forest(X_train[W_train == 1, , drop = FALSE], Y_train[W_train == 1])
  m0hat_train <- predict(rfm0, X_train)$predictions
  m1hat_train <- predict(rfm1, X_train)$predictions
  rfp <- regression_forest(X_train, W_train)
  ehat_train_dr <- predict(rfp)$predictions
  Y_tilde_train <- m1hat_train - m0hat_train +
    W_train * (Y_train - m1hat_train) / ehat_train_dr -
    (1 - W_train) * (Y_train - m0hat_train) / (1 - ehat_train_dr)
  rf_dr <- regression_forest(X_train, Y_tilde_train)
  cate_dr_test <- predict(rf_dr, X_test)$predictions
  result_dr_learner <- cbind(cate_dr_test, X_test)
  save(result_dr_learner,
       file = file.path(clans_dir, paste0("cates_dr_learner_", month_name, ".RData")))

  cf <- causal_forest(X_train, Y_train, W_train, tune.parameters = "all")
  cate_cf_test <- predict(cf, X_test)$predictions
  result_cf_learner <- cbind(cate_cf_test, X_test)
  save(result_cf_learner,
       file = file.path(clans_dir, paste0("cates_cf_learner_", month_name, ".RData")))

  cates_list_all <- list(
    cate_sl_test,
    cate_tl_all,
    cate_rl_rf_test,
    cate_dr_test,
    cate_cf_test
  )
  names(cates_list_all) <- c(
    "S-learner", "T-learner", "R-learner", "DR-learner", "Causal Forest"
  )
  save(cates_list_all, file = file.path(clans_dir, paste0("cates_", month_name, ".RData")))

  aipw_test <- DML_aipw(Y_test, W_test, X_test)
  pseudoY_all <- aipw_test$ATE$delta
  K <- 4

  gates_combined_df <- do.call(rbind, mapply(function(cates, method_name) {
    slices <- factor(as.numeric(cut(
      cates,
      breaks = quantile(cates, probs = seq(0, 1, length = K + 1)),
      include.lowest = TRUE
    )))
    G_ind <- model.matrix(~ 0 + slices)
    gates_fit <- lm_robust(pseudoY_all ~ 0 + G_ind)
    gates_wc <- lm_robust(pseudoY_all ~ G_ind[, -1])
    se <- gates_fit$std.error
    wc_pval <- round(summary(gates_wc)$coefficients[
      nrow(summary(gates_wc)$coefficients), "Pr(>|t|)"
    ], 3)

    data.frame(
      Method = method_name,
      Group = factor(paste("Group", 1:K), levels = paste("Group", 1:K)),
      Coefficient = gates_fit$coefficients,
      cil = gates_fit$coefficients - 1.96 * se,
      ciu = gates_fit$coefficients + 1.96 * se,
      pval = wc_pval
    )
  }, cates_list_all, names(cates_list_all), SIMPLIFY = FALSE))

  method_colors <- RColorBrewer::brewer.pal(7, "Set1")
  names(method_colors) <- names(cates_list_all)

  gates_plot_all <- ggplot(
    gates_combined_df,
    aes(x = Group, y = Coefficient, color = Method, group = Method)
  ) +
    geom_point(position = position_dodge(width = 0.4), size = 3) +
    geom_errorbar(aes(ymin = cil, ymax = ciu),
                  width = 0.15, position = position_dodge(width = 0.4)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(
      title = "GATES Estimates Across Methods",
      y = "Coefficient Estimate",
      x = "Group"
    ) +
    scale_color_manual(values = method_colors)

  gates_pdf <- file.path(gates_dir, paste0("gates_", month_name, "_r.pdf"))
  ggsave(gates_pdf, gates_plot_all, width = 6.8, height = 4.8)

  if (month_num == 12) {
    ggsave(file.path(gates_dir, "Gates_december_r.pdf"),
           gates_plot_all, width = 6.8, height = 4.8)
  }

  if (month_num %in% c(1, 4, 10)) {
    main_png <- file.path(gates_dir, paste0("gates_", month_name, "_r4_NEW.png"))
    ggsave(main_png, gates_plot_all, width = 9.04, height = 6.31, dpi = 150)
  } else {
    main_png <- NA_character_
  }

  save(gates_combined_df,
       file = file.path(gates_dir, paste0("gates_", month_name, "_values.RData")))
  write.csv(gates_combined_df,
            file.path(gates_dir, paste0("gates_", month_name, "_values.csv")),
            row.names = FALSE)

  list(
    cates_list_all = cates_list_all,
    gates_combined_df = gates_combined_df,
    gates_plot_all = gates_plot_all,
    gates_pdf = gates_pdf,
    main_png = main_png
  )
}

run_section_4_point_5_month <- function(month_num) {
  if (!month_num %in% seq_along(month_names)) {
    stop("month_num must be an integer from 1 to 12.")
  }

  section_dir <- get_section45_dir()
  replication_dir <- normalizePath(
    Sys.getenv("OBES_REPLICATION_DIR", file.path(section_dir, "..")),
    winslash = "/", mustWork = FALSE
  )
  data_out_dir <- normalizePath(
    Sys.getenv("OBES_DATA_OUT_DIR", file.path(replication_dir, "data", "data_out")),
    winslash = "/", mustWork = FALSE
  )

  output_dir <- make_dir(Sys.getenv("OBES_SECTION45_OUTPUT_DIR", section_dir))
  gates_dir <- make_dir(Sys.getenv("OBES_GATES_OUTPUT_DIR", output_dir))
  effect_dir <- make_dir(Sys.getenv("OBES_EFFECT_PLOTS_DIR",
                                    file.path(output_dir, "effect_plots")))
  propensity_dir <- make_dir(Sys.getenv("OBES_PROPENSITY_PLOTS_DIR",
                                        file.path(output_dir, "propensity_scores")))
  clans_dir <- make_dir(Sys.getenv("OBES_CLANS_DIR", file.path(output_dir, "CLANs")))

  message("Running section_4_point_5 month ", month_num, " (", month_names[month_num], ")")
  merged_data <- prepare_section45_data(data_out_dir)
  design <- prepare_month_design(merged_data, month_num)

  aipw_outputs <- save_aipw_and_propensity_figures(
    Y = design$Y,
    W = design$W,
    X = design$X,
    month_num = month_num,
    effect_dir = effect_dir,
    propensity_dir = propensity_dir
  )

  gates_outputs <- estimate_cates_and_gates(
    Y = design$Y,
    W = design$W,
    X = design$X,
    month_num = month_num,
    clans_dir = clans_dir,
    gates_dir = gates_dir
  )

  invisible(list(
    month_data = design$month_data,
    aipw_outputs = aipw_outputs,
    gates_outputs = gates_outputs
  ))
}
