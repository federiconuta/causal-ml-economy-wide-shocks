######### ONLY WITH Ridge and RIDGE:
rm(list = ls())
#----------------------------------------------------
# Install necessary libraries

library(caret)
library(randomForest)
library("dplyr")
library("ggplot2")
library("scales")
library("bbmle")
library(Matrix)
library(foreign)
library(PerformanceAnalytics)
library(remotes)
library(haven)
library(rlang)
library(gtools) #to apply "smartbind" (row binds even when columns do not have same names) 
library(ggplot2)
library(pROC)
library(tidyverse) 
library(caret)     #for logit-Ridge
library(glmnet)    #for logit-Ridge
library(ROCR)
library(randomForest)
library(data.table)  # For fast data manipulation
library(dplyr)       # For data manipulation
library(tidyr)       # For grid generation (expand.grid)
library(glmnet)      # For Ridge and Ridge
library(randomForest) # For Random Forest
library(xgboost)     # For XGBoost
library(e1071)       # For SVM (Support Vector Machines)
library(causalDML)
library(hdm)
library(tidyverse)
library(grf)
library(estimatr)
library(lmtest)
library(sandwich)
library(psych)
library(latex2exp)

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

split_input_dir <- normalizePath(Sys.getenv("OBES_TAB8_SPLITS_DIR", file.path(script_dir, "multiple_splits", "multiple_splits_datasets")), winslash = "/", mustWork = FALSE)
results_dir <- normalizePath(Sys.getenv("OBES_TAB8_RESULTS_DIR", file.path(script_dir, "multiple_splits_results")), winslash = "/", mustWork = FALSE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
n_splits <- 100


# Initialize a list to store all datasets
clan_results_list <- list()


# File: process_splits_by_month_unified.R

months <- c("january", "february", "march", "april", "may", "june",
            "july", "august", "september", "october", "november", "december")

all_clan_results <- list()
common_cols <- NULL
counter <- 0

for (month_name in months) {
  counter <- counter + 1
  for (i in seq_len(n_splits)) {
    file_path <- file.path(split_input_dir, paste0("cates_r_", month_name, "_learner_split", i, ".RData"))
    
    if (file.exists(file_path)) {
      load(file_path)
      
      clan_results <- result_r_learner
      clan_results <- data.frame(clan_results)
      
      # Skip empty results
      if (nrow(clan_results) == 0) {
        message("Empty result in: ", file_path)
        next
      }
      
      clan_results$month <- counter
      clan_results$split <- i
      
      if (is.null(common_cols)) {
        common_cols <- colnames(clan_results)
      } else {
        common_cols <- intersect(common_cols, colnames(clan_results))
      }
      
      all_clan_results[[paste0(month_name, "_split", i)]] <- clan_results
    } else {
      message("File not found: ", file_path)
    }
  }
}

# Keep only common columns
all_clan_results_common <- lapply(all_clan_results, function(df) df[ , common_cols])

final_results <- do.call(rbind, all_clan_results_common)
final_results<- data.frame(final_results)

#### Run CLAN operations on the combined dataset.


library(dplyr)
library(fastDummies)

# Step 1: Map month number to month name
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

final_results <- final_results %>%
  mutate(month_name = factor(month_names[month], levels = month_names))

# Step 2: Create dummy variables
final_results <- dummy_cols(final_results, select_columns = "month_name", remove_first_dummy = FALSE, remove_selected_columns = TRUE)


# Rename columns by removing the "month_name_" prefix
colnames(final_results) <- gsub("^month_name_", "", colnames(final_results))

# Create industry aggregates
industry_groups <- list(
  Agriculture = c("industry_Animal01", "industry_Vegetable02", "industry_Fatsoils03", "industry_Prepfood04"),
  Chemicals = c("industry_Chemical06", "industry_Plastics07"),
  Manufacturing = c("industry_Machinery16", "industry_Vehicles17", "industry_Manuf20"),
  Metals = c("industry_Mineral05", "industry_Cement13", "industry_Jewel14", "industry_Metals15"),
  Special = c("industry_Precisinst18", "industry_Arms19", "industry_Art21", "industry_Special22"),
  Textile = c("industry_Leather08", "industry_Textile11", "industry_Footwear12"),
  Wood = c("industry_Wood09", "industry_Paper10")
)

for (group in names(industry_groups)) {
  vars <- industry_groups[[group]]
  existing_vars <- vars[vars %in% names(final_results)]
  
  if (length(existing_vars) > 0) {
    # Convert to numeric before summing
    numeric_data <- sapply(final_results[, existing_vars, drop = FALSE], as.numeric)
    
    # If only one column, sapply will return a vector instead of a matrix; fix that:
    if (is.vector(numeric_data)) numeric_data <- matrix(numeric_data, ncol = 1)
    
    final_results[[group]] <- as.numeric(rowSums(numeric_data) > 0)
  } else {
    final_results[[group]] <- 0
  }
}


# Código de la vía de transporte 1 MARITIMO 2 FERREO 3 TERRESTRE 4 AEREO 5 CORREO 6 MULTIMODAL 7 INSTALACIONES DE TRANSPORTE FIJAS (TUBERIAS, CABLE, ETC.) 8 VIAS NAVEGABLES INTERIORES 9 OTRO MODO DE TRANSPORTE ]

if ("via_1" %in% names(final_results)) {
  final_results <- final_results %>% rename(sea = via_1)
}

if ("via_3" %in% names(final_results)) {
  final_results <- final_results %>% rename(land = via_3)
}

if ("via_4" %in% names(final_results)) {
  final_results <- final_results %>% rename(air = via_4)
}

# Define variables to inlude in CAdiff
vars_CAdiff <- c("cate_rl_rf_test","Agriculture", "Chemicals", "Manufacturing", "Metals", "Special", "Textile", "Wood", "air", "land", "sea", "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","ND", "NO", "NP", "lnX", "lnX_import", "split")
formula_str <- paste("~ 0 +", paste(vars_CAdiff, collapse = " + "))
XCA <- model.matrix(as.formula(formula_str), data = final_results)

X_testCA <- final_results[ , vars_CAdiff]

X_testCA <- as.data.frame(X_testCA)
split_ids <- unique(X_testCA$split)
results_by_split_normal <- list()
results_by_split_industry <- list()
results_by_split_industry_month <- list()

sectors <- c("Agriculture", "Chemicals", "Manufacturing", 
             "Metals", "Special", "Textile", "Wood")

sec_mo <- c("q1_q4_label", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
            "Agriculture", "Chemicals", "Manufacturing", "Metals",
            "Special", "Textile", "Wood")

library(mitools)
for (sp in split_ids) {
  data_split <- X_testCA[X_testCA$split == sp, ]
  
  # Compute Q1-Q4 labels
  cates_sl <- data_split$cate_rl_rf_test
  q <- quantile(cates_sl, probs = c(0.25, 0.75), na.rm = TRUE)
  q1_q4_index <- which(cates_sl <= q[1] | cates_sl >= q[2])
  q1_q4_label <- ifelse(cates_sl[q1_q4_index] <= q[1], 1, 0)
  
  # Select predictors
  predictors <- data_split[q1_q4_index, ]
  predictors <- predictors %>%
    select(-cate_rl_rf_test, -split)
  
  results_normal <- data.frame(
    variable = character(),
    estimate = numeric(),
    std_error = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  results_industry_month <- data.frame(
    variable = character(),
    estimate = numeric(),
    std_error = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  results_industry_ctrl <- data.frame(
    variable = character(),
    estimate = numeric(),
    std_error = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (varname in names(predictors)) {
    fmla <- reformulate("q1_q4_label", response = varname)
    normal <- lm_robust(fmla, data = cbind(q1_q4_label = q1_q4_label, predictors))
    
    if (!(varname %in% sectors)) {
      controls2 <- c("q1_q4_label", sectors)
      fmla2 <- reformulate(controls2, response = varname)
      industry_ctrl <- lm_robust(fmla2, data = predictors)
      results_industry_ctrl <- rbind(results_industry_ctrl, data.frame(
        variable = varname,
        estimate = round(coef(industry_ctrl)["q1_q4_label"], 4),
        std_error = round(industry_ctrl$std.error["q1_q4_label"], 4),
        p_value = round(industry_ctrl$p.value["q1_q4_label"], 4)
      ))
      results_by_split_industry[[as.character(sp)]] <- results_industry_ctrl
    }
    
    if (!(varname %in% sec_mo)) {
      fmla3 <- reformulate(
        c("q1_q4_label", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
          "Jul", "Aug", "Sep", "Oct", "Nov", sectors), # Exclude Dec to avoid multicollinearity
        response = varname
      )
      
      industry_months_ctrl <- lm_robust(fmla3, data = predictors)
      results_industry_month <- rbind(results_industry_month, data.frame(
        variable = varname,
        estimate = round(coef(industry_months_ctrl)["q1_q4_label"], 4),
        std_error = round(industry_months_ctrl$std.error["q1_q4_label"], 4),
        p_value = round(industry_months_ctrl$p.value["q1_q4_label"], 4)
      ))
      results_by_split_industry_month[[as.character(sp)]] <- results_industry_month
    }
    #fmla3 <- reformulate(c("q1_q4_label","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Agriculture", "Chemicals", "Manufacturing", "Metals", "Special", "Textile", "Wood"), response = varname)
    #industry_months_ctrl <- lm_robust(fmla3, data = cbind(q1_q4_label = q1_q4_label, predictors))
    
    
    results_normal <- rbind(results_normal, data.frame(
      variable = varname,
      estimate = round(coef(normal)["q1_q4_label"], 4),
      std_error = round(normal$std.error["q1_q4_label"], 4),
      p_value = round(normal$p.value["q1_q4_label"], 4)
    ))
    
  }
  
  results_normal$split <- sp
  results_by_split_normal[[as.character(sp)]] <- results_normal
}

# Combine all results
all_results_normal <- do.call(rbind, results_by_split_normal)
all_results_industry <- do.call(rbind, results_by_split_industry)
all_results_industry_month <-  do.call(rbind, results_by_split_industry_month)

# Calculate medians
median_results_industry_month <- all_results_industry_month %>%
  group_by(variable) %>%
  summarise(
    median_estimate = median(estimate, na.rm = TRUE),
    median_p_value = median(p_value, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate medians
median_results_normal <- all_results_normal %>%
  group_by(variable) %>%
  summarise(
    median_estimate = median(estimate, na.rm = TRUE),
    median_p_value = median(p_value, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate medians
median_results_industry <- all_results_industry %>%
  group_by(variable) %>%
  summarise(
    median_estimate = median(estimate, na.rm = TRUE),
    median_p_value = median(p_value, na.rm = TRUE),
    .groups = "drop"
  )



custom_order_normal <- c(
  "Agriculture", "Chemicals", "Manufacturing", "Metals", "Special", "Textile", "Wood",
  "air", "land", "sea",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
  "ND", "NO", "NP",
  "lnX", "lnX_import"
)

custom_order_industry <- c("air", "land", "sea",
                           "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                           "ND", "NO", "NP",
                           "lnX", "lnX_import"
)

custom_order_industry_month <- c("air", "land", "sea","ND", "NO", "NP","lnX", "lnX_import")


# Sort the data frame according to the desired order
median_results_ordered_industry_month <- median_results_industry_month %>%
  mutate(variable = factor(variable, levels = custom_order_industry_month)) %>%
  arrange(variable)


median_results_ordered_industry_month$median_p_value <- p.adjust(
  median_results_ordered_industry_month$median_p_value,
  method = "BY",
  n = length(median_results_ordered_industry_month$median_p_value)
)

# Sort the data frame according to the desired order
median_results_ordered_normal <- median_results_normal %>%
  mutate(variable = factor(variable, levels = custom_order_normal)) %>%
  arrange(variable)


median_results_ordered_normal$median_p_value <- p.adjust(
  median_results_ordered_normal$median_p_value,
  method = "BY",
  n = length(median_results_ordered_normal$median_p_value)
)
write.csv(
  median_results_ordered_normal,
  file = file.path(results_dir, "median_results_ordered_r_learner_normal.csv"),
  row.names = FALSE
)





# Sort the data frame according to the desired order
median_results_ordered_industry <- median_results_industry %>%
  mutate(variable = factor(variable, levels = custom_order_industry)) %>%
  arrange(variable)


median_results_ordered_industry$median_p_value <- p.adjust(
  median_results_ordered_industry$median_p_value,
  method = "BY",
  n = length(median_results_ordered_industry$median_p_value)
)

write.csv(
  median_results_ordered_industry,
  file = file.path(results_dir, "median_results_ordered_r_learner_industry.csv"),
  row.names = FALSE
)






# Sort the data frame according to the desired order
median_results_ordered_industry_month <- median_results_ordered_industry_month %>%
  mutate(variable = factor(variable, levels = custom_order_industry_month)) %>%
  arrange(variable)


median_results_ordered_industry_month$median_p_value <- p.adjust(
  median_results_ordered_industry_month$median_p_value,
  method = "BY",
  n = length(median_results_ordered_industry_month$median_p_value)
)

write.csv(
  median_results_ordered_industry_month,
  file = file.path(results_dir, "median_results_ordered_r_learner_industry_month.csv"),
  row.names = FALSE
)












