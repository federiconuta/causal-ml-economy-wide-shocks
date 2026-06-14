rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})

auc_rank <- function(expected, predicted) {
  expected <- as.numeric(as.character(expected))
  predicted <- as.numeric(as.character(predicted))
  keep <- is.finite(expected) & is.finite(predicted)
  expected <- expected[keep]
  predicted <- predicted[keep]
  n_pos <- sum(expected == 1)
  n_neg <- sum(expected == 0)
  if (n_pos == 0 || n_neg == 0 || length(unique(predicted)) <= 1) return(NA_real_)
  ranks <- rank(predicted, ties.method = "average")
  (sum(ranks[expected == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

calculate_gof <- function(preds, model_name, pred_col) {
  if (is.list(preds) && !is.data.frame(preds)) preds <- do.call(rbind, preds)
  months <- sort(unique(as.numeric(as.character(preds$month))))
  out <- data.frame(year = 20, month = months)
  out[[paste0("RMSE_", model_name)]] <- vapply(months, function(month_i) {
    df <- preds[as.numeric(as.character(preds$month)) == month_i, ]
    expected <- as.numeric(as.character(df$export_future))
    predicted <- as.numeric(as.character(df[[pred_col]]))
    sqrt(mean((expected - predicted)^2, na.rm = TRUE))
  }, numeric(1))
  out[[paste0("AUC_", model_name)]] <- vapply(months, function(month_i) {
    df <- preds[as.numeric(as.character(preds$month)) == month_i, ]
    auc_rank(df$export_future, df[[pred_col]])
  }, numeric(1))
  out
}

load(file.path(script_dir, "SUM_preds_lasso.RData"))
load(file.path(script_dir, "SUM_preds_ridge.RData"))
load(file.path(script_dir, "SUM_preds_rf.RData"))
load(file.path(script_dir, "SUM_preds_logit.RData"))

GoF_all <- Reduce(function(x, y) merge(x, y, by = c("year", "month"), all = TRUE), list(
  calculate_gof(SUM_preds_lasso, "lasso", "pred"),
  calculate_gof(SUM_preds_ridge, "ridge", "pred"),
  calculate_gof(SUM_preds_rf, "rf", "pred"),
  calculate_gof(SUM_preds_logit, "logit", "pred")
))
GoF_all <- GoF_all[order(as.numeric(as.character(GoF_all$month))), ]

write.csv(GoF_all, file.path(script_dir, "table3_gof_19_20_sum.csv"), row.names = FALSE)
print(GoF_all)
