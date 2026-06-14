# This script reproduces Figure 1 of the main text.
# It uses the SUM and SAM prediction files to compute monthly average
# treatment effects, and bootstrap prediction draws to compute confidence
# intervals.

rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})

package_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
local_lib <- file.path(
  normalizePath(file.path(package_dir, ".."), mustWork = FALSE),
  ".r_libs",
  paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
)
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

library(ggplot2)

find_input <- function(file_name, folders) {
  candidates <- file.path(folders, file_name)
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(
      "Missing input file: ", file_name, "\n",
      "Run the Table 3 prediction scripts and the Figure 1 bootstrap scripts first.",
      call. = FALSE
    )
  }
  existing[[1]]
}

load_object <- function(file_path, object_name) {
  env <- new.env(parent = emptyenv())
  load(file_path, envir = env)
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop("Object ", object_name, " not found in ", file_path, call. = FALSE)
  }
  get(object_name, envir = env)
}

prediction_folders_sum <- c(
  script_dir,
  file.path(package_dir, "Tab_3", "2019_2020_SUM_GoF")
)

prediction_folders_sam <- c(
  script_dir,
  file.path(package_dir, "Tab_3", "2019_2020_SAM_GoF")
)

bootstrap_dir <- file.path(script_dir, "bootstrap_for_CI")

SUM_preds_lasso <- load_object(
  find_input("SUM_preds_lasso.RData", prediction_folders_sum),
  "SUM_preds_lasso"
)
SUM_preds_rf <- load_object(
  find_input("SUM_preds_rf.RData", prediction_folders_sum),
  "SUM_preds_rf"
)
lasso_preds_19 <- load_object(
  find_input("lasso_preds_19.RData", prediction_folders_sam),
  "lasso_preds_19"
)
rf_preds_19 <- load_object(
  find_input("rf_preds_19.RData", prediction_folders_sam),
  "rf_preds_19"
)

original_estimates <- function(sum_df, sam_df, model_label) {
  names(sum_df)[names(sum_df) == "pred"] <- "pred_SUM"
  names(sam_df)[names(sam_df) == "pred_19"] <- "pred_SAM"

  merged <- merge(sam_df, sum_df, by = c("id", "month", "export_future"))
  merged$effect <- as.numeric(merged$pred_SAM) - as.numeric(merged$pred_SUM)

  out <- aggregate(effect ~ month, merged, mean, na.rm = TRUE)
  out$model <- model_label
  out
}

bootstrap_effects <- function(sum_file, sam_file, sum_object, sam_object, model_label) {
  sum_boot <- load_object(file.path(bootstrap_dir, sum_file), sum_object)
  sam_boot <- load_object(file.path(bootstrap_dir, sam_file), sam_object)

  rows <- list()
  row_id <- 1

  for (month_id in names(sum_boot)) {
    n_boot <- min(length(sum_boot[[month_id]]), length(sam_boot[[month_id]]))

    for (bootstrap_id in seq_len(n_boot)) {
      sum_df <- sum_boot[[month_id]][[bootstrap_id]]
      sam_df <- sam_boot[[month_id]][[bootstrap_id]]

      names(sum_df)[names(sum_df) %in% c("pred", "pred_SUM")] <- "pred_SUM"
      names(sam_df)[names(sam_df) %in% c("pred", "pred_SAM")] <- "pred_SAM"

      merged <- merge(sam_df, sum_df, by = c("id", "month"))
      rows[[row_id]] <- data.frame(
        model = model_label,
        month = as.integer(month_id),
        bootstrap = bootstrap_id,
        effect = mean(as.numeric(merged$pred_SAM) - as.numeric(merged$pred_SUM), na.rm = TRUE)
      )
      row_id <- row_id + 1
    }
  }

  do.call(rbind, rows)
}

point_estimates <- rbind(
  original_estimates(SUM_preds_lasso, lasso_preds_19, "LASSO (SAM-SUM)"),
  original_estimates(SUM_preds_rf, rf_preds_19, "RF (SAM-SUM)")
)

bootstrap_estimates <- rbind(
  bootstrap_effects(
    "lasso.pred_SUM_boot.RData",
    "lasso.pred_SAM_boot.RData",
    "lasso.pred_SUM_boot",
    "lasso.pred_SAM_boot",
    "LASSO (SAM-SUM)"
  ),
  bootstrap_effects(
    "rf.pred_SUM_boot.RData",
    "rf.pred_SAM_boot.RData",
    "rf.pred_SUM_boot",
    "rf.pred_SAM_boot",
    "RF (SAM-SUM)"
  )
)

bootstrap_sd <- aggregate(effect ~ model + month, bootstrap_estimates, sd, na.rm = TRUE)
names(bootstrap_sd)[names(bootstrap_sd) == "effect"] <- "bootstrap_sd"

bootstrap_reps <- aggregate(effect ~ model + month, bootstrap_estimates, length)
names(bootstrap_reps)[names(bootstrap_reps) == "effect"] <- "bootstrap_reps"

plot_data <- merge(point_estimates, bootstrap_sd, by = c("model", "month"))
plot_data <- merge(plot_data, bootstrap_reps, by = c("model", "month"))
plot_data$ci_low <- plot_data$effect - qnorm(.975) * plot_data$bootstrap_sd
plot_data$ci_high <- plot_data$effect + qnorm(.975) * plot_data$bootstrap_sd
plot_data$month_name <- factor(month.abb[as.integer(plot_data$month)], levels = month.abb)
plot_data$model <- factor(plot_data$model, levels = c("LASSO (SAM-SUM)", "RF (SAM-SUM)"))
plot_data <- plot_data[order(plot_data$model, plot_data$month), ]

write.csv(plot_data, file.path(script_dir, "fig1_values.csv"), row.names = FALSE)

if (any(plot_data$bootstrap_reps < 20)) {
  warning("Some model-month cells have fewer than 20 bootstrap replications.")
}

figure <- ggplot(
  plot_data,
  aes(x = month_name, y = effect, group = model, color = model, linetype = model, shape = model)
) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey20") +
  geom_vline(xintercept = 3.5, linetype = "longdash", color = "grey20") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.45, linewidth = 0.55) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 2.0) +
  scale_color_manual(
    name = "Model",
    values = c("LASSO (SAM-SUM)" = "black", "RF (SAM-SUM)" = "#0072B2")
  ) +
  scale_linetype_manual(
    name = "Model",
    values = c("LASSO (SAM-SUM)" = "solid", "RF (SAM-SUM)" = "dashed")
  ) +
  scale_shape_manual(
    name = "Model",
    values = c("LASSO (SAM-SUM)" = 16, "RF (SAM-SUM)" = 17)
  ) +
  scale_y_continuous(limits = c(-0.235, 0.065), breaks = seq(-0.20, 0.05, by = 0.05)) +
  labs(x = "Month", y = "COVID-19 effect (average)") +
  theme_grey(base_size = 12) +
  theme(
    legend.position = c(.78, .15),
    legend.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path(script_dir, "intervals_lasso_rf_month_r3.pdf"),
  figure,
  width = 7,
  height = 7
)

ggsave(
  file.path(script_dir, "intervals_lasso_rf_month_r3.png"),
  figure,
  width = 7,
  height = 7,
  dpi = 150
)
