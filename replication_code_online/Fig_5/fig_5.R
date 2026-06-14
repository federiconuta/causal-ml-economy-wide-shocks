######## FIG. 5
rm(list = ls())

local_libs <- c(
  file.path(getwd(), ".r_libs", paste0(R.version$major, ".", R.version$minor)),
  file.path(getwd(), ".r_libs", paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]))
)
local_libs <- normalizePath(local_libs[dir.exists(local_libs)], mustWork = FALSE)
if (length(local_libs) > 0) {
  .libPaths(c(local_libs, .libPaths()))
}

library(data.table)
library(ggplot2)

args_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(args_file) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", args_file[[1]])))
} else {
  script_dir <- getwd()
}

default_tab4_dir <- normalizePath(file.path(script_dir, "..", "Tab_4"), mustWork = FALSE)
tab4_dir <- Sys.getenv("OBES_TAB4_DIR", default_tab4_dir)
out_dir <- Sys.getenv("OBES_FIG5_OUT_DIR", script_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

orig_file <- file.path(tab4_dir, "data_heterogeneity_original.csv")
boots_file <- file.path(tab4_dir, "data_heterogeneity_boots.csv")
if (!file.exists(orig_file)) {
  stop("Missing input file: ", orig_file)
}
if (!file.exists(boots_file)) {
  stop("Missing input file: ", boots_file)
}

month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

orig <- fread(
  orig_file,
  select = c("month", "TE_indiv", "export_future", "pred_SUM")
)
orig[, month := factor(month, levels = 1:12, labels = month_levels)]
orig[, `:=`(
  sam_sum = TE_indiv,
  y_sum = export_future - pred_SUM
)]

point_est <- orig[, .(
  mean_sam_sum = mean(sam_sum, na.rm = TRUE),
  mean_y_sum = mean(y_sum, na.rm = TRUE)
), by = month]

boots <- fread(
  boots_file,
  select = c("bootstrap", "month", "export_future.x", "pred_SAM", "pred_SUM")
)
setnames(boots, "export_future.x", "export_future")
boots[, month := factor(month, levels = 1:12, labels = month_levels)]
boots[, `:=`(
  sam_sum = pred_SAM - pred_SUM,
  y_sum = export_future - pred_SUM
)]

boot_means <- boots[, .(
  mean_sam_sum = mean(sam_sum, na.rm = TRUE),
  mean_y_sum = mean(y_sum, na.rm = TRUE)
), by = .(bootstrap, month)]

boot_se <- boot_means[, .(
  se_sam_sum = sd(mean_sam_sum, na.rm = TRUE),
  se_y_sum = sd(mean_y_sum, na.rm = TRUE),
  bootstrap_reps = uniqueN(bootstrap)
), by = month]

fig5_df <- merge(point_est, boot_se, by = "month", all.x = TRUE)
fig5_df[, `:=`(
  sam_lo = mean_sam_sum - 1.96 * se_sam_sum,
  sam_hi = mean_sam_sum + 1.96 * se_sam_sum,
  y_lo = mean_y_sum - 1.96 * se_y_sum,
  y_hi = mean_y_sum + 1.96 * se_y_sum
)]
setorder(fig5_df, month)

plot_df <- rbindlist(list(
  fig5_df[, .(
    month,
    model = "LASSO (SAM-SUM)",
    estimate = mean_sam_sum,
    ci_lo = sam_lo,
    ci_hi = sam_hi
  )],
  fig5_df[, .(
    month,
    model = "LASSO (Y-SUM)",
    estimate = mean_y_sum,
    ci_lo = y_lo,
    ci_hi = y_hi
  )]
))

pd <- position_dodge(width = 0.25)

p <- ggplot(plot_df, aes(x = month, y = estimate, group = model,
                         color = model, linetype = model, shape = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_vline(xintercept = 4, linetype = "dashed", linewidth = 0.5) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, position = pd) +
  geom_line(position = pd) +
  geom_point(size = 2, position = pd) +
  scale_color_manual(values = c("LASSO (SAM-SUM)" = "black",
                                "LASSO (Y-SUM)" = "darkorange")) +
  scale_linetype_manual(values = c("LASSO (SAM-SUM)" = "solid",
                                   "LASSO (Y-SUM)" = "dashed")) +
  scale_shape_manual(values = c("LASSO (SAM-SUM)" = 16,
                                "LASSO (Y-SUM)" = 17)) +
  labs(
    x = "Month",
    y = "COVID-19 effect (average)",
    color = "Model",
    linetype = "Model",
    shape = "Model"
  ) +
  theme_gray(base_size = 14) +
  theme(
    legend.position = c(0.80, 0.14),
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(out_dir, "intervals_lasso_month_Y_SUM_r3.pdf"),
       p, width = 7.5, height = 7.5)
ggsave(file.path(out_dir, "intervals_lasso_month_Y_SUM_r3.png"),
       p, width = 7.5, height = 7.5, dpi = 300)
fwrite(fig5_df, file.path(out_dir, "fig5_monthly_effects.csv"))

print(fig5_df)
