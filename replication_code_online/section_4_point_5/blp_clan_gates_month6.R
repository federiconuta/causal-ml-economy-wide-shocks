rm(list = ls())

script_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NA_character_
)
if (is.na(script_path)) {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]),
                                 winslash = "/", mustWork = TRUE)
  }
}
script_dir <- if (is.na(script_path)) normalizePath(getwd(), winslash = "/", mustWork = TRUE) else dirname(script_path)
Sys.setenv(OBES_SECTION45_SCRIPT_DIR = script_dir)

source(file.path(script_dir, "run_section_4_point_5_month.R"))
run_section_4_point_5_month(6)
