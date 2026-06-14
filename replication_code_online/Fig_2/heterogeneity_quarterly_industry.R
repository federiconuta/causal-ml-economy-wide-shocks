# This script reproduces Figure 2 of the main text.
# It uses the heterogeneity datasets created in the Table 4 workflow.

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

library(data.table)
library(ggplot2)

tab4_dir <- Sys.getenv("OBES_TAB4_DIR", unset = file.path(package_dir, "Tab_4"))
out_dir <- Sys.getenv("OBES_FIG2_OUT_DIR", unset = script_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

industry_labs <- c(
  `1`  = "Animal (01)",
  `2`  = "Vegetable (02)",
  `3`  = "Fats/oils (03)",
  `4`  = "Prep. food (04)",
  `5`  = "Mineral (05)",
  `6`  = "Chemical (06)",
  `7`  = "Plastics (07)",
  `8`  = "Leather (08)",
  `9`  = "Wood (09)",
  `10` = "Paper (10)",
  `11` = "Textile (11)",
  `12` = "Footwear (12)",
  `13` = "Cement (13)",
  `14` = "Jewel (14)",
  `15` = "Metals (15)",
  `16` = "Machinery (16)",
  `17` = "Vehicles (17)",
  `18` = "Precis. inst. (18)",
  `19` = "Other (19)",
  `20` = "Manuf. (20)"
)

keep_industries <- c(
  "Animal (01)", "Vegetable (02)", "Fats/oils (03)", "Prep. food (04)",
  "Mineral (05)", "Chemical (06)", "Plastics (07)", "Leather (08)",
  "Wood (09)", "Paper (10)", "Textile (11)", "Footwear (12)",
  "Cement (13)", "Jewel (14)", "Metals (15)", "Machinery (16)",
  "Vehicles (17)", "Precis. inst. (18)"
)

find_input <- function(stem) {
  candidates <- file.path(tab4_dir, paste0(stem, c(".csv", ".dta")))
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0) {
    stop(
      "Missing input file for ", stem, ". Run the Table 4 data-construction scripts first.",
      call. = FALSE
    )
  }
  existing[[1]]
}

read_selected <- function(path, need_bootstrap = FALSE) {
  requested <- c("month", "pred_sam", "pred_sum", "industry_mode")
  if (need_bootstrap) requested <- c("bootstrap", requested)

  if (grepl("[.]csv$", path, ignore.case = TRUE)) {
    header <- names(fread(path, nrows = 0))
    key <- setNames(header, tolower(header))
    missing <- requested[!requested %in% names(key)]
    if (length(missing) > 0) {
      stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
    }
    dt <- fread(path, select = unname(key[requested]), showProgress = TRUE)
    setnames(dt, unname(key[requested]), requested)
    return(dt)
  }

  if (grepl("[.]dta$", path, ignore.case = TRUE)) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' is required to read .dta files. CSV inputs are preferred.", call. = FALSE)
    }
    dt <- as.data.table(haven::read_dta(path))
    key <- setNames(names(dt), tolower(names(dt)))
    missing <- requested[!requested %in% names(key)]
    if (length(missing) > 0) {
      stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
    }
    dt <- dt[, ..unname(key[requested])]
    setnames(dt, unname(key[requested]), requested)
    return(dt)
  }

  stop("Unsupported file type: ", path, call. = FALSE)
}

prep_heterogeneity_data <- function(dt) {
  dt[, month := as.integer(month)]
  dt[, te_indiv := as.numeric(pred_sam) - as.numeric(pred_sum)]
  dt[, quarter := fifelse(
    month %in% 1:3, "Quarter 1",
    fifelse(month %in% 4:6, "Quarter 2",
      fifelse(month %in% 7:9, "Quarter 3",
        fifelse(month %in% 10:12, "Quarter 4", NA_character_)
      )
    )
  )]
  industry_raw <- as.character(dt$industry_mode)
  industry_mapped <- industry_labs[industry_raw]
  dt[, industry := fifelse(!is.na(industry_mapped), industry_mapped, industry_raw)]
  dt[!is.na(quarter) & !is.na(industry) & industry %in% keep_industries]
}

orig_df <- prep_heterogeneity_data(read_selected(find_input("data_heterogeneity_original")))
boot_df <- prep_heterogeneity_data(read_selected(find_input("data_heterogeneity_boots"), need_bootstrap = TRUE))
boot_df[, bootstrap := as.integer(bootstrap)]

orig_ind_q <- orig_df[, .(
  effect_orig = mean(te_indiv, na.rm = TRUE)
), by = .(quarter, industry)]

boot_ind_q <- boot_df[, .(
  effect_boot = mean(te_indiv, na.rm = TRUE)
), by = .(bootstrap, quarter, industry)]

boot_ci <- boot_ind_q[, .(
  se_boot = sd(effect_boot, na.rm = TRUE),
  bootstrap_reps = .N
), by = .(quarter, industry)]

plot_df <- merge(orig_ind_q, boot_ci, by = c("quarter", "industry"), all.x = TRUE)
plot_df[, ci_low := effect_orig - qnorm(0.975) * se_boot]
plot_df[, ci_high := effect_orig + qnorm(0.975) * se_boot]

order_q1 <- plot_df[quarter == "Quarter 1"][order(-effect_orig), industry]
plot_df[, industry := factor(industry, levels = rev(order_q1))]
plot_df[, quarter := factor(quarter, levels = c("Quarter 1", "Quarter 2", "Quarter 3", "Quarter 4"))]
setorder(plot_df, quarter, industry)

fwrite(plot_df, file.path(out_dir, "fig2_industry_quarter_values.csv"))

figure <- ggplot(plot_df, aes(x = effect_orig, y = industry)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    colour = "dodgerblue3"
  ) +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    orientation = "y",
    width = 0.15,
    linewidth = 0.5
  ) +
  geom_point(shape = 17, size = 3, colour = "blue") +
  facet_wrap(~ quarter, ncol = 2, scales = "fixed") +
  labs(
    x = "COVID-19 effect (average)",
    y = "Industry"
  ) +
  theme_grey(base_size = 12)

ggsave(
  file.path(out_dir, "intervals_lasso_industry_quarter_r_NEW.png"),
  figure,
  width = 9,
  height = 6.3,
  dpi = 150
)

ggsave(
  file.path(out_dir, "intervals_lasso_industry_quarter_r_NEW.pdf"),
  figure,
  width = 9,
  height = 6.3
)
