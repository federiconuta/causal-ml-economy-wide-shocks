# This script reproduces Figure 4 of the main text.
# It uses the SUM/SAM LASSO prediction files and their bootstrap draws.

rm(list = ls())

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(script_path) > 0) dirname(normalizePath(script_path)) else getwd()
})

package_dir <- normalizePath(file.path(script_dir, "..", "..", ".."), mustWork = TRUE)
local_lib <- file.path(
  normalizePath(file.path(package_dir, ".."), mustWork = FALSE),
  ".r_libs",
  paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
)
if (dir.exists(local_lib)) {
  .libPaths(c(local_lib, .libPaths()))
}

library(data.table)
library(ggplot2)

sam_sum_dir <- Sys.getenv(
  "OBES_FIG4_SAM_SUM_DIR",
  unset = normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
)
out_dir <- Sys.getenv("OBES_FIG4_OUT_DIR", unset = sam_sum_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pred_sum_file <- file.path(sam_sum_dir, "SUM_mar26", "SUM_preds_lasso.RData")
pred_sam_file <- file.path(sam_sum_dir, "SAM_mar26", "lasso_preds_19.RData")
boot_sum_file <- file.path(
  sam_sum_dir,
  "bootstraps_mar26",
  "lasso.pred_SUM_boot_subsamp_cvlam_marzo26.RData"
)
boot_sam_file <- file.path(
  sam_sum_dir,
  "bootstraps_mar26",
  "lasso.pred_SAM_boot_subsamp_cvlam_marzo26.RData"
)

required_files <- c(pred_sum_file, pred_sam_file, boot_sum_file, boot_sam_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n"),
    "\nRun the SUM_mar26, SAM_mar26, and bootstraps_mar26 scripts first.",
    call. = FALSE
  )
}

load_object <- function(path, object_name) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!exists(object_name, envir = env, inherits = FALSE)) {
    stop("Object '", object_name, "' not found in ", path, call. = FALSE)
  }
  get(object_name, envir = env)
}

standardize_prediction <- function(dt, pred_name) {
  dt <- as.data.table(dt)
  if ("pred_19" %in% names(dt)) {
    setnames(dt, "pred_19", pred_name)
  } else if ("pred" %in% names(dt)) {
    setnames(dt, "pred", pred_name)
  } else {
    stop("Prediction column not found.", call. = FALSE)
  }
  dt[, `:=`(
    id = as.integer(as.character(id)),
    month = as.integer(as.character(month))
  )]
  dt
}

bind_bootstrap_predictions <- function(boot_list, pred_name) {
  reps_per_month <- lengths(boot_list)
  if (length(unique(reps_per_month)) != 1) {
    stop("Bootstrap files do not have the same number of replications for each month.", call. = FALSE)
  }

  n_reps <- unique(reps_per_month)
  out <- vector("list", n_reps)
  for (rep_id in seq_len(n_reps)) {
    rep_dt <- rbindlist(lapply(boot_list, function(month_list) {
      as.data.table(month_list[[rep_id]])
    }), fill = TRUE)

    if (!"pred" %in% names(rep_dt)) {
      stop("Prediction column 'pred' not found in bootstrap object.", call. = FALSE)
    }
    setnames(rep_dt, "pred", pred_name)
    rep_dt[, `:=`(
      bootstrap = rep_id,
      id = as.integer(as.character(id)),
      month = as.integer(as.character(month))
    )]
    out[[rep_id]] <- rep_dt[, .(bootstrap, id, month, export_future, value = get(pred_name))]
    setnames(out[[rep_id]], "value", pred_name)
  }

  rbindlist(out, use.names = TRUE, fill = TRUE)
}

sum_preds <- standardize_prediction(load_object(pred_sum_file, "SUM_preds_lasso"), "pred_SUM")
sam_preds <- standardize_prediction(load_object(pred_sam_file, "lasso_preds_19"), "pred_SAM")

original_predictions_lasso <- merge(
  sum_preds[, .(id, month, export_future, pred_SUM)],
  sam_preds[, .(id, month, export_future, pred_SAM)],
  by = c("id", "month", "export_future"),
  all.x = TRUE
)
original_predictions_lasso[, TE_indiv := pred_SAM - pred_SUM]

quantiles <- seq(0.02, 0.98, by = 0.01)
z_value <- qnorm(0.975)

spe <- original_predictions_lasso[, .(
  SPE_boot_orig = as.numeric(quantile(TE_indiv, probs = quantiles, na.rm = TRUE, type = 7)),
  quantiles = quantiles
), by = month]

ape <- original_predictions_lasso[, .(
  APE_boot_orig = mean(TE_indiv, na.rm = TRUE)
), by = month]

sum_boot <- bind_bootstrap_predictions(
  load_object(boot_sum_file, "lasso.pred_SUM_boot"),
  "pred_SUM"
)
sam_boot <- bind_bootstrap_predictions(
  load_object(boot_sam_file, "lasso.pred_SAM_boot"),
  "pred_SAM"
)

lasso_pred_boot <- merge(
  sam_boot[, .(bootstrap, id, month, pred_SAM)],
  sum_boot[, .(bootstrap, id, month, pred_SUM)],
  by = c("bootstrap", "id", "month")
)
lasso_pred_boot[, TE_indiv := pred_SAM - pred_SUM]

spe_boots <- lasso_pred_boot[, .(
  SPE_boot = as.numeric(quantile(TE_indiv, probs = quantiles, na.rm = TRUE, type = 7)),
  quantiles = quantiles
), by = .(bootstrap, month)]

spe_ci <- spe_boots[, .(
  estimate_sd = sd(SPE_boot, na.rm = TRUE),
  bootstrap_reps = .N
), by = .(month, quantiles)]
spe_ci[, `:=`(
  interval_min = -z_value * estimate_sd,
  interval_max = z_value * estimate_sd
)]

ape_boots <- lasso_pred_boot[, .(
  APE_boot = mean(TE_indiv, na.rm = TRUE)
), by = .(bootstrap, month)]

ape_ci <- ape_boots[, .(
  estimate_sd = sd(APE_boot, na.rm = TRUE),
  bootstrap_reps = .N
), by = month]
ape_ci[, `:=`(
  interval_min = -z_value * estimate_sd,
  interval_max = z_value * estimate_sd
)]

ape_final <- merge(ape_ci, ape, by = "month")
ape_final[, `:=`(
  CI_min_APE = APE_boot_orig + interval_min,
  CI_max_APE = APE_boot_orig + interval_max
)]
ape_final <- ape_final[, .(month, APE_boot_orig, CI_min_APE, CI_max_APE, bootstrap_reps)]

spe_final <- merge(spe_ci, spe, by = c("month", "quantiles"))
spe_final[, `:=`(
  CI_min_SPE = SPE_boot_orig + interval_min,
  CI_max_SPE = SPE_boot_orig + interval_max
)]
spe_final <- spe_final[, .(month, quantiles, SPE_boot_orig, CI_min_SPE, CI_max_SPE)]

final_data <- merge(spe_final, ape_final, by = "month")
final_data[, `:=`(
  month = as.integer(as.character(month)),
  month_name = factor(month.abb[as.integer(as.character(month))], levels = month.abb)
)]
setorder(final_data, month, quantiles)

fwrite(final_data, file.path(out_dir, "fig4_spe_values.csv"))

figure <- ggplot(final_data, aes(x = quantiles)) +
  geom_ribbon(aes(ymin = CI_min_SPE, ymax = CI_max_SPE), fill = "lightskyblue2") +
  geom_line(aes(y = SPE_boot_orig, color = "SPE")) +
  geom_line(aes(y = APE_boot_orig, color = "APE")) +
  geom_line(aes(y = CI_min_APE), linetype = "twodash") +
  geom_line(aes(y = CI_max_APE), linetype = "twodash") +
  ylab("Change probability to export due to COVID-19") +
  xlab("Percentile index OLS") +
  facet_wrap(~ month_name) +
  scale_colour_manual(
    name = NULL,
    values = c("SPE" = "red", "APE" = "black")
  ) +
  scale_y_continuous(
    limits = c(-0.75, 0.50),
    breaks = seq(0.5, -0.75, by = -0.25)
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm")
  )

fig_width <- 7.3
fig_height <- 5.1

ggsave(file.path(out_dir, "fig_4_goodlegend.pdf"), figure, width = fig_width, height = fig_height)
ggsave(file.path(out_dir, "fig_4_goodlegend.png"), figure, width = fig_width, height = fig_height, dpi = 150)
ggsave(file.path(out_dir, "spe_sum_sam_fig4.pdf"), figure, width = fig_width, height = fig_height)
