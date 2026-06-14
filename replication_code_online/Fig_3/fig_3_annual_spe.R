# This script reproduces Figure 3 of the main text.
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
out_dir <- Sys.getenv("OBES_FIG3_OUT_DIR", unset = script_dir)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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
  requested <- c("pred_sam", "pred_sum")
  if (need_bootstrap) requested <- c("bootstrap", requested)

  if (grepl("[.]csv$", path, ignore.case = TRUE)) {
    header <- names(fread(path, nrows = 0))
    key <- setNames(header, tolower(header))
    missing <- requested[!requested %in% names(key)]
    if (length(missing) > 0) {
      stop("Missing columns in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
    }
    cols <- unname(key[requested])
    dt <- fread(path, select = cols, showProgress = TRUE)
    setnames(dt, cols, requested)
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
    cols <- unname(key[requested])
    dt <- dt[, ..cols]
    setnames(dt, cols, requested)
    return(dt)
  }

  stop("Unsupported file type: ", path, call. = FALSE)
}

orig <- read_selected(find_input("data_heterogeneity_original"))
boots <- read_selected(find_input("data_heterogeneity_boots"), need_bootstrap = TRUE)

orig[, te := as.numeric(pred_sam) - as.numeric(pred_sum)]
boots[, `:=`(
  bootstrap = as.integer(bootstrap),
  te = as.numeric(pred_sam) - as.numeric(pred_sum)
)]

percentiles <- 1:100
probs <- percentiles / 100
z_value <- qnorm(0.975)

spe_orig <- data.table(
  percentile = percentiles,
  spe = as.numeric(quantile(orig$te, probs = probs, na.rm = TRUE, type = 7))
)
ape_orig <- mean(orig$te, na.rm = TRUE)

spe_boot_wide <- boots[
  ,
  setNames(
    as.list(as.numeric(quantile(te, probs = probs, na.rm = TRUE, type = 7))),
    paste0("p", percentiles)
  ),
  by = bootstrap
]

spe_boot_long <- melt(
  spe_boot_wide,
  id.vars = "bootstrap",
  variable.name = "percentile",
  value.name = "spe_boot"
)
spe_boot_long[, percentile := as.integer(sub("^p", "", percentile))]

spe_se <- spe_boot_long[, .(
  se_spe = sd(spe_boot, na.rm = TRUE),
  bootstrap_reps = .N
), by = percentile]

ape_boot <- boots[, .(
  ape_boot = mean(te, na.rm = TRUE)
), by = bootstrap]
ape_se <- sd(ape_boot$ape_boot, na.rm = TRUE)

fig3_df <- merge(spe_orig, spe_se, by = "percentile", all.x = TRUE)
setorder(fig3_df, percentile)
fig3_df[, `:=`(
  spe_lo = spe - z_value * se_spe,
  spe_hi = spe + z_value * se_spe,
  ape = ape_orig,
  ape_se = ape_se,
  ape_lo = ape_orig - z_value * ape_se,
  ape_hi = ape_orig + z_value * ape_se
)]

fwrite(fig3_df, file.path(out_dir, "fig3_spe_values.csv"))

figure <- ggplot(fig3_df, aes(x = percentile, y = spe)) +
  geom_ribbon(
    aes(ymin = spe_lo, ymax = spe_hi),
    fill = "lightskyblue2"
  ) +
  geom_line(color = "red", linewidth = 0.7) +
  geom_hline(yintercept = ape_orig, color = "black", linewidth = 0.6) +
  geom_hline(yintercept = fig3_df$ape_lo[[1]], color = "black", linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = fig3_df$ape_hi[[1]], color = "black", linetype = "dashed", linewidth = 0.5) +
  labs(
    x = "Percentile index LASSO",
    y = "Change probability to export due to COVID-19"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  file.path(out_dir, "SPE_fig3_lightblue.pdf"),
  figure,
  width = 7,
  height = 5
)
ggsave(
  file.path(out_dir, "SPE_fig3_lightblue.png"),
  figure,
  width = 7,
  height = 5,
  dpi = 150
)
ggsave(
  file.path(out_dir, "fig_3_SPE.pdf"),
  figure,
  width = 7,
  height = 5
)
