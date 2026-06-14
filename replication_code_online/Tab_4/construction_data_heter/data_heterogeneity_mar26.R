## This script creates a summary of each exporter in 2019 to be used in the heterogeneity analysis:

# Clean working environment
rm(list = ls())

# Libraries required
library(dplyr)
library(stringr)    # For str_pad
library(readxl)
library(tidyr)


# Set Directories -------------------------------------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

heter_dir <- normalizePath(Sys.getenv("OBES_TAB4_HETER_DIR", script_dir), winslash = "/", mustWork = FALSE)
tab4_dir <- normalizePath(Sys.getenv("OBES_TAB4_DIR", file.path(heter_dir, "..")), winslash = "/", mustWork = FALSE)
replication_dir <- normalizePath(Sys.getenv("OBES_REPLICATION_DIR", file.path(tab4_dir, "..")), winslash = "/", mustWork = FALSE)
data_out_dir <- normalizePath(Sys.getenv("OBES_DATA_OUT_DIR", file.path(replication_dir, "data", "data_out")), winslash = "/", mustWork = FALSE)
sam_sum_dir <- normalizePath(Sys.getenv("OBES_SAM_SUM_DIR", file.path(replication_dir, "Fig_4_and_7", "SAM_minus_SUM")), winslash = "/", mustWork = FALSE)

setwd(heter_dir)

#data.out <- "output_final/"
# ---------------------------------

# Load predictions for Logit-LASSO for all 2020 (by months)
load(file.path(sam_sum_dir, "SUM_mar26", "SUM_preds_lasso.RData"))   # Original LASSO predictions for SUM
load(file.path(sam_sum_dir, "SAM_mar26", "lasso_preds_19.RData"))    # Original LASSO predictions for SAM

  
  
# Read data -------------------------------------------------------------
load(file.path(data_out_dir, "final_data_19.RData"))
load(file.path(heter_dir, "info_mode.RData"))


# Prepare dataset for LOGIT (RIDGE; LASSO) and for RF
final_data_19$month <- as.factor(final_data_19$month)



#----------------------------------------------------
# REMOVE ALL CONSTANT COLUMNS
# 2019
#select only numeric/factor columns
nums <- unlist(lapply(final_data_19, is.numeric))
fact <- unlist(lapply(final_data_19, is.factor))
#subset columns from original data into numeric and factor
nums <- final_data_19[ , nums]
fact <- final_data_19[ , fact]
#Drop constant variables (checking whether the maximum is equal to the minimum is sufficient)
fact <- fact[,!apply(fact, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
#Create back the dataset final_data_19 (without constant columns)
final_data_19 <- cbind(nums,fact)




##generate interactions between size and (industry,sector,via,iso)
interaction_size <- function(df){
  df <- df
  df <- type.convert(df)
  #create vector of column names that you want to multiply with size
  cols <- grep('industry|sector|via|iso', names(df), value = TRUE)
  #convert factor to integer and multiply it with all the columns to create new columns
  df[paste0('size_', cols)] <- as.integer(df$size) * df[cols]
  #Change the classes
  df
}

final_data_19_logit <- interaction_size(final_data_19)




##   STEPS:
#1) indiv. treatment effects from original LASSO
#2) create variables we want to select
#3) merge selected data with original predictions from LASSO 
#4) run OLS with fm1 formula
#5) run OLS boots
#6) sort the individual treatment effect (alpha) in percentiles for the original data and for every boostrap


qt1 <- c("1", "2", "3")
qt2 <- c("4", "5", "6")
qt3 <- c("7", "8", "9")
qt4 <- c("10", "11", "12")
########################################################
# 1) INDIVIDUAL TREATMENT EFFECTS FROM ORIGINAL LASSO
########################################################

SUM_preds_lasso <- SUM_preds_lasso %>%
  rename(pred_SUM = pred)

lasso_preds_19 <- lasso_preds_19 %>%
  rename(pred_SAM = pred)


original_predictions_lasso <- merge(SUM_preds_lasso, lasso_preds_19, by = c("id", "month", "export_future"), all.x = T)

original_predictions_lasso <- original_predictions_lasso %>%
  mutate(quarter = ifelse(month %in% qt1, "qt1",
                          ifelse(month %in% qt2, "qt2",
                                 ifelse(month %in% qt3, "qt3", "qt4" ))))



original_predictions_lasso <- original_predictions_lasso %>%
  pivot_longer(
    cols = c(pred_SUM, pred_SAM),      # las columnas que colapsas
    names_to = "modelo",               # nombre auxiliar
    values_to = "pred_lasso"           # nueva variable con los valores
  ) %>%
  mutate(
    covid_aware = if_else(modelo == "pred_SAM", 1L, 0L)  # 1 si SAM, 0 si SUM
  ) %>%
  select(id, month, pred_lasso, covid_aware)


rm(SUM_preds_lasso, lasso_preds_19)


info_mode <- info_mode %>%
  mutate(id = as.numeric(id))


info_mode <- info_mode %>% distinct(id, month, .keep_all = TRUE)
# Merge into one dataset -------------------------------------------------------------
info_mode$month <- as.integer(info_mode$month)
data_heterogeneity <- left_join(original_predictions_lasso, final_data_19_logit,
                    by=c("id", "month"))

data_heterogeneity <- data_heterogeneity %>%
  mutate(id = as.numeric(id))
info_mode <- info_mode %>% select(-NP, -ND, -HHk, -HHd, -lnX, -size) 
data_heterogeneity <- left_join(data_heterogeneity, info_mode, 
                                by=c("id", "month"))

#----------------------------------------------------

char_cols <- sapply(data_heterogeneity, is.character)
data_heterogeneity[ , char_cols][is.na(data_heterogeneity[ , char_cols])] <- "missing"

fac_cols <- sapply(data_heterogeneity, is.factor)

for (nm in names(data_heterogeneity)[fac_cols]) {
  x <- as.character(data_heterogeneity[[nm]])
  x[is.na(x)] <- "missing"
  data_heterogeneity[[nm]] <- factor(x)
}

# Save data -------------------------------------------------------------
save(data_heterogeneity, file = file.path(heter_dir, "data_heterogeneity.RData"))
#----------------------------------------------------
