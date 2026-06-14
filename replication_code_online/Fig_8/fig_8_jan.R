######### ONLY WITH Ridge and RIDGE:
rm(list = ls())
#----------------------------------------------------
# Install necessary libraries

library(randomForest)
library("dplyr")
library("ggplot2")
library("scales")
library(Matrix)
library(foreign)
library(rlang)
library(gtools) #to apply "smartbind" (row binds even when columns do not have same names) 
library(ggplot2)
library(ROCR)
library(randomForest)
library(data.table)  # For fast data manipulation
library(dplyr)       # For data manipulation
library(randomForest) # For Random Forest

#dir.create(path.expand("~/.R"), showWarnings = FALSE)

#writeLines(c(
#  "FC = /opt/homebrew/bin/gfortran",
#  "F77 = /opt/homebrew/bin/gfortran",
#  paste0(
#    "FLIBS = -L", system("brew --prefix gcc", intern = TRUE), "/lib/gcc/current ",
#    "-L", system("brew --prefix gcc", intern = TRUE), "/lib/gcc/current/gcc/aarch64-apple-darwin25 ",
#    "-lgfortran -lquadmath"
#  )
#), path.expand("~/.R/Makevars"))

#options(download.file.method = "curl")

library(grf)
library(estimatr)
library(lmtest)
library(sandwich)
library(psych)
library(latex2exp)


script_path <- Sys.getenv("OBES_FIG8_SCRIPT", unset = NA_character_)
if (!is.na(script_path) && nzchar(script_path) && file.exists(script_path)) {
  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)
} else {
  script_path <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE), error = function(e) NA_character_)
}
if (is.na(script_path)) {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  script_candidate <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else NA_character_
  script_path <- if (!is.na(script_candidate) && nzchar(script_candidate) && file.exists(script_candidate)) normalizePath(script_candidate, winslash = "/", mustWork = TRUE) else NA_character_
}
script_dir <- if (is.na(script_path)) normalizePath(getwd(), winslash = "/", mustWork = TRUE) else dirname(script_path)
replication_dir <- normalizePath(Sys.getenv("OBES_REPLICATION_DIR", file.path(script_dir, "..")), winslash = "/", mustWork = FALSE)
data_out_dir <- normalizePath(Sys.getenv("OBES_DATA_OUT_DIR", file.path(replication_dir, "data", "data_out")), winslash = "/", mustWork = FALSE)
figure_output_dir <- normalizePath(Sys.getenv("OBES_FIG8_OUTPUT_DIR", script_dir), winslash = "/", mustWork = FALSE)
dir.create(figure_output_dir, showWarnings = FALSE, recursive = TRUE)

load(file.path(data_out_dir, "final_data_18.RData"))
load(file.path(data_out_dir, "final_data_19.RData"))

final_data_18_subset <- final_data_18 
final_data_19_subset <- final_data_19

# Main datasets
############################

# The final file combines merged 2018-2019 data with the COVID-related variables.
data_18 <- final_data_18_subset
data_19 <- final_data_19_subset

# Identify common variables, excluding variables available only in one year
common_vars <- intersect(names(data_18), names(data_19))

# Subset datasets to retain only common variables
data_18 <- data_18[common_vars]
data_19 <- data_19[common_vars]

# Add the W variable
data_18$W <- 0  # Year 18
data_19$W <- 1  # Year 19

# Merge the datasets
merged_18_19 <- bind_rows(data_18, data_19)

# Save the merged dataset
#save(merged_18_19, file = "merged_18_19.RData")

# Print structure of merged dataset
str(merged_18_19)




#Start here
###########
###########

# General adjustments for all months
##########################################
##########################################

#load("merged_18_19.RData")

merged_data=merged_18_19

# Remove the 'year' variable 
merged_data <- merged_data %>% select(-year)


# a command to check how many values have each variable in the data
sapply(merged_data, function(x) length(unique(x)))

# Identify variables with more than one unique value
vars_to_keep <- names(merged_data)[sapply(merged_data, function(x) length(unique(x)) > 1)]

# Identify variables with one unique value
vars_to_drop <- names(merged_data)[sapply(merged_data, function(x) length(unique(x)) == 1)]
print(vars_to_drop)

# Subset the dataset to keep only those variables
merged_data <- merged_data[vars_to_keep]


# Remove dots from variable names
clean_names <- gsub("\\.", "", names(merged_data))

# Rename the dataset variables without dots
names(merged_data) <- clean_names


# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(merged_data),
  data_type = sapply(merged_data, class),  # Get the type of each variable
  unique_values = sapply(merged_data, function(x) length(unique(x)))  # Count unique values
)

# Variables starting with sector and ending with exp are interactions between
# two variables and are not needed here; remove them.

names(merged_data)[grepl("^sector.*exp$", names(merged_data))]
merged_data <- merged_data |> select(-matches("^sector.*exp$"))

# Continuous iso variables (import and export) are encoded as factors.

# Identify variables starting with "iso"
iso_vars <- grep("^iso", names(merged_data), value = TRUE)

# Convert them to numeric, replacing the original variables
merged_data <- merged_data %>%
  mutate(across(all_of(iso_vars), ~ as.numeric(as.character(.))))

# Check for missing values after conversion
missing_check <- colSums(is.na(merged_data[iso_vars]))

# Print warning if missing values exist
if (any(missing_check > 0)) {
  warning("Missing values introduced in: ", paste(names(missing_check[missing_check > 0]), collapse = ", "))
} else {
  message("All 'iso' variables converted successfully without missing values.")
}

# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(merged_data),
  data_type = sapply(merged_data, class),  # Get the type of each variable
  unique_values = sapply(merged_data, function(x) length(unique(x)))  # Count unique values
)

# Select column names starting with "iso_import_"
import_vars <- grep("^iso_import_[A-Z]{3}$", names(merged_data), value = TRUE)

# Count how many of these variables are positive in each row
merged_data$NO <- rowSums(merged_data[, import_vars, drop = FALSE] > 0)

# From here, focus on the target month 
#####################################
#######################################

# Filter dataset for month 1
month_1_data <- merged_data %>% filter(month == 1)



# Test differences in the distribution of X for the target month
###################################################################


# Exclude variables that should not be analyzed
excluded_vars <- c("month", "id", "export_future", "W")
numeric_vars <- setdiff(names(month_1_data)[sapply(month_1_data, is.numeric)], excluded_vars)

# Run KS and Wilcoxon tests for each numeric variable
ks_results <- sapply(numeric_vars, function(var) {
  ks.test(month_1_data[[var]][month_1_data$W == 0], 
          month_1_data[[var]][month_1_data$W == 1])$p.value
})

wilcox_results <- sapply(numeric_vars, function(var) {
  wilcox.test(month_1_data[[var]][month_1_data$W == 0], 
              month_1_data[[var]][month_1_data$W == 1])$p.value
})

# Create the data frame with the full results
test_results <- data.frame(Variable = numeric_vars, 
                           KS_p_value = ks_results,
                           Wilcox_p_value = wilcox_results)

# Identify variables significant in at least one of the two tests (p < 0.05)
significant_cont_vars <- test_results %>%
  filter(KS_p_value < 0.05 | Wilcox_p_value < 0.05) %>%
  select(Variable)

# Print variables significant in at least one of the two tests (p < 0.05)
print("Continuous variables significantly different between W = 0 and W = 1:")
print(significant_cont_vars$Variable)




# Select categorical variables only 
categorical_vars <- setdiff(names(month_1_data)[sapply(month_1_data, is.factor)], excluded_vars)

# Run the chi-squared test for each categorical variable
chi_results <- sapply(categorical_vars, function(var) {
  tbl <- table(month_1_data[[var]], month_1_data$W)
  if (min(tbl) > 0) {  # Chi-squared cannot be run with empty cells
    chisq.test(tbl)$p.value
  } else {
    NA
  }
})

# Create the data frame with the results
chi_results_df <- data.frame(Variable = categorical_vars, Chi2_p_value = chi_results)

# Keep only variables with p-value < 0.05
significant_cat_vars <- chi_results_df %>% filter(Chi2_p_value < 0.05)

# Print significant variables (p-value < 0.05)
print("Categorical variables significantly different between W = 0 and W = 1:")
print(significant_cat_vars$Variable)


#Define treatment variable
##################################
# Define treatment variable (assuming it's binary: 0 = control, 1 = treated)
treatment_var <- "W"


# SAVE HERE DATA FOR MONTH 1
###############################

#save(month_1_data, file = "month_1_data.RData")


#PS with logit senza iso variables  (sostituito con RF)
##################################

# Select all independent variables, excluding "iso" variables, "month", "id", and "export_future"
#independent_vars <- setdiff(names(merged_data), c(treatment_var, "month", "id", "export_future"))
#independent_vars <- independent_vars[!grepl("^iso", independent_vars)]  # Remove "iso" variables


# Create the formula
#ps_formula <- reformulate(independent_vars, response = treatment_var)


# Keep meaningful regressors only; repeat the constant-variable check for the January sample.
########### (constant variables are removed below)

#independent_vars <- independent_vars[sapply(month_1_data[independent_vars], function(x) length(unique(x)) > 1)]

#ps_formula <- reformulate(independent_vars, response = treatment_var)

#print(ps_formula)


# Fit the logistic regression model for propensity score estimation

#ps_model <- glm(ps_formula, data = month_1_data, family = binomial)


# Extract the estimated propensity scores
#month_1_data$propensity_score <- predict(ps_model, type = "response")

# Check summary of propensity scores
#summary(month_1_data$propensity_score)

# Compute formatted statistics for each level of W
#res <- by(month_1_data$propensity_score, month_1_data$W, function(x) {
#  format(summary(x), digits = 7, nsmall = 7)
#})

# Convert the by object into a list and print it
#print(as.list(res))


# Plot density of propensity scores by group
#ggplot(month_1_data, aes(x = propensity_score, fill = as.factor(W))) +
#  geom_density(alpha = 0.5) +
#  labs(title = "Density Plot of Propensity Scores",
#       x = "Propensity Score",
#       y = "Density",
#       fill = "W (Treatment Group)") +
#  theme_minimal() +
#  scale_fill_manual(values = c("blue", "red"))

#install.packages("lmtest")

#library(lmtest)  # For Likelihood Ratio Test

#Likelihood Ratio Test: Compare model vs. null model (only intercept)
#null_model <- glm(W ~ 1, data = month_1_data, family = binomial)
#lr_test <- lrtest(ps_model, null_model)

# Print test results
#print(lr_test)

#Pseudo R2

#install.packages("DescTools")
#library(DescTools)

#pseudo=PseudoR2(ps_model, which = "all")
#pseudo_r2_rounded <- round(pseudo, 4)
#pseudo_r2_rounded




# Data adjustments for the target month
##########################################


#load("month_1_data.RData")
# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(month_1_data),
  data_type = sapply(month_1_data, class),  # Get the type of each variable
  unique_values = sapply(month_1_data, function(x) length(unique(x)))  # Count unique values
)

# Identify variables with one unique value
vars_to_drop <- names(month_1_data)[sapply(month_1_data, function(x) length(unique(x)) == 1)]
print(vars_to_drop)

# Subset the dataset to keep only those variables
month_1_data <- month_1_data |>  select(-all_of(vars_to_drop))

# Identify factor variables with exactly two levels
binary_factors <- sapply(month_1_data, function(x) is.factor(x) && nlevels(x) == 2)

# Convert each one to numeric 0/1 based on the level order
month_1_data[binary_factors] <- lapply(month_1_data[binary_factors], function(x) {
  as.numeric(as.character(x))
})


# Create dummies from the levels of the "size" factor
size_dummies <- model.matrix(~ size - 1, data = month_1_data)

# Rename columns by removing the "size" prefix
colnames(size_dummies) <- gsub("^size", "", colnames(size_dummies))

# Add the columns to the dataset
month_1_data <- cbind(month_1_data, size_dummies)

# Create a data frame with variable information : name, class and the number of different values
variables_info <- data.frame(
  variable_name = names(month_1_data),
  data_type = sapply(month_1_data, class),  # Get the type of each variable
  unique_values = sapply(month_1_data, function(x) length(unique(x)))  # Count unique values
)

# Remove size
month_1_data <- month_1_data |>  select(-size)



#### Define useful vectors and matrix


# Outcome
Y = month_1_data$export_future

# Treatment
W = month_1_data$W

# Create main effects matrix
vars_to_exclude <- c("W", "month", "id", "export_future", "propensity_score")
# Take all variable names except those to exclude
vars <- setdiff(names(month_1_data), vars_to_exclude)
# Create the formula dynamically
formula_str <- paste("~ 0 +", paste(vars, collapse = " + "))
# Create the model matrix
X <- model.matrix(as.formula(formula_str), data = month_1_data)








#Hand-coded Double Selection
############################

# To understand the procedure of Double Selection, we proceed step by step using only the main effects for simplicity:

# Select variables in the outcome regression without the treatment:

if (requireNamespace("hdm", quietly = TRUE)) {
  library(hdm)
  # Select variables in outcome regression
  sel_y = rlasso(X,Y)
  
  # Which variables are selected?
  which(sel_y$beta != 0)  
  
  # Select variables in treatment regression
  sel_w = rlasso(X,W)
  which(sel_w$beta != 0)
  
  # Use the union of selected variables to run a standard OLS regression with robust standard errors:
  # Double selection
  X_sel_union = X[,sel_y$beta != 0 | sel_w$beta != 0]
  ds_hand = lm(Y ~ W + X_sel_union)
  summary(ds_hand)
  
  #Double Selection with hdm package
  ##################################
  
  # The rlassoEffect command from the hdm package implements this step directly.
  ds1 = rlassoEffect(X,Y,W)
  summary(ds1)
} else {
  cat("hdm is not available; skipping double-selection diagnostic.\n")
}



#Hand-coded Double ML for partially linear model
################################################

# If a linear model is not imposed, random forests can be used to estimate nuisance parameters in a partially linear model.

#1  Split the sample in two random subsamples, S1 and S2

#2 Form prediction models in S1, use it to predict in S2

#3 Form prediction models in S2, use it to predict in S1

#4 Run residual-on-residual regression with the combined predictions


# Initialize nuisance vectors
n = length(Y)
mhat = ehat = rep(NA,n)
#This last line above initializes two vectors, mhat and ehat, with the same length as n, filling them with NA values ("Not Available" or missing values).
# rep(NA, n) creates a vector of length n where all elements are NA.
# The assignment operator = is used twice:
# - first to assign rep(NA, n) to ehat;
# - then to assign the value of ehat (which is rep(NA, n)) to mhat.

# Draw random indices for sample 1
set.seed(12324)
index_s1 = sample(1:n,n/2)
# Create S1
x1 = X[index_s1,]
w1 = W[index_s1]
y1 = Y[index_s1]
# Create sample 2 with those not in S1
x2 = X[-index_s1,]
w2 = W[-index_s1]
y2 = Y[-index_s1]
# Model in S1, predict in S2
rf = regression_forest(x1,w1)
ehat[-index_s1] = predict(rf,newdata=x2)$predictions
rf = regression_forest(x1,y1)
mhat[-index_s1] = predict(rf,newdata=x2)$predictions
# Model in S2, predict in S1
rf = regression_forest(x2,w2)
ehat[index_s1] = predict(rf,newdata=x1)$predictions
rf = regression_forest(x2,y2)
mhat[index_s1] = predict(rf,newdata=x1)$predictions
# RORR
res_y = Y-mhat
res_w = W-ehat
pl_2f = lm_robust(res_y ~ 0+res_w)
summary(pl_2f)


#Estimated propensity score with RF with 2 folds
######################################

# Create a data frame for plotting
df_propensity <- data.frame(
  ehat = ehat,
  treated = factor(W, levels = c(0, 1), labels = c("Control", "Treated"))
)

# Plot the distributions of ehat
ggplot(df_propensity, aes(x = ehat, fill = treated)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Propensity Score Distribution by Treatment Group",
    x = "Estimated Propensity Score with RF (2 folds)",
    y = "Density",
    fill = "Group"
  ) +
  theme_minimal()

# Compute formatted statistics for each level of W
statis <- by(df_propensity$ehat, df_propensity$treated, function(x) {
  format(summary(x), digits = 7, nsmall = 7)
})

# Convert the by object into a list and print it
print(as.list(statis))




#Double ML for partially linear model with causalDML package
############################################################

#2-fold cross-fitting is easy to implement by hand but especially in small sample sizes, using only 50% of the data to estimate the nuisance parameters 
# might lead to unstable predictions.

# The DML_partial_linear function from the causalDML package is used to run 5-fold cross-fitting.
# This package requires user-defined methods because it allows for ensemble methods.
# The implementation below focuses on random forests. For details, see https://github.com/MCKnaus/causalDML.

#With 5-fold cross-fitting, the program splits the sample in 5 folds and uses 4 folds (80% of the data) to predict the left out fold (20% of the data). 
#It iterates such that every fold is left out once.

# 5-fold cross-fitting with causalDML package
# Create learner

if (requireNamespace("causalDML", quietly = TRUE)) {
  library(causalDML)
  set.seed(12344)
  forest = create_method("forest_grf",args=list(tune.parameters = "all"))
  # Run partially linear model
  pl_5f = DML_partial_linear(Y,W,X,ml_w=list(forest),ml_y=list(forest),cf=5)
  summary(pl_5f)
  
  # Extract estimated propensity score from 5-fold DML
  ehat <- pl_5f$e_hat
} else {
  cat("causalDML is not available; skipping 5-fold DML diagnostic.\n")
}



#Estimated propensity score with RF with 5 folds
##################################################

# Build a data frame for plotting
df_propensity <- data.frame(
  ehat = ehat,
  treated = factor(W, levels = c(0, 1), labels = c("Control", "Treated"))
)

# Plot the distributions
ggplot(df_propensity, aes(x = ehat, fill = treated)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Propensity Score Distribution (5-fold GRF)",
    x = "Estimated Propensity Score",
    y = "Density",
    fill = "Group"
  ) +
  theme_minimal()





# Check summary of propensity scores
summary(df_propensity$ehat)

# Compute formatted statistics for each level of W
stati <- by(df_propensity$ehat, df_propensity$treated, function(x) {
  format(summary(x), digits = 7, nsmall = 7)
})

# Convert the by object into a list and print it
print(as.list(stati))


# ROC diagnostic, skipped when pROC is not available.
if (requireNamespace("pROC", quietly = TRUE)) {
  ehat_roc <- as.vector(ehat)
  roc_rf <- pROC::roc(W, ehat_roc)
  cat("AUC:", pROC::auc(roc_rf), "\n")
  plot(roc_rf,
       col = "blue",
       lwd = 3,
       main = "ROC Curve - Propensity Score (Random Forest)",
       print.auc = TRUE,
       legacy.axes = TRUE)
  abline(a = 0, b = 1, lty = 2, col = "gray")
} else {
  cat("pROC is not available; skipping ROC diagnostic.\n")
}


# Compute log loss
log_loss <- -mean(W * log(ehat) + (1 - W) * log(1 - ehat))
print(log_loss)




#Comparison of results
#####################

# Compare the different methods.

# Collect the results and plot them
Coefficient <- c()
se <- c()
Method <- c()
if (exists("ds1")) {
  Coefficient <- c(Coefficient, ds1$alpha)
  se <- c(se, ds1$se)
  Method <- c(Method, "DS1")
}
Coefficient <- c(Coefficient, pl_2f$coefficients)
se <- c(se, pl_2f$std.error)
Method <- c(Method, "PL 2-fold")
if (exists("pl_5f")) {
  Coefficient <- c(Coefficient, pl_5f$result[1])
  se <- c(se, pl_5f$result[2])
  Method <- c(Method, "PL 5-fold")
}
data.frame(Coefficient,se,
           Method = Method,
           cil = Coefficient - 1.96*se,
           ciu = Coefficient + 1.96*se)  %>% 
  ggplot(aes(x=Method,y=Coefficient,ymin=cil,ymax=ciu)) + geom_point(size=2.5) + geom_errorbar(width=0.15)  +
  geom_hline(yintercept=0)




#AIPW Double ML for average treatment effects
#############################################
#############################################

# AIPW Double ML Hand-coded
###########################

# AIPW implementation estimates the nuisance parameters
# e(X)=E[W|X], m(0,X)=E[Y|W=0,X], and m(1,X)=E[Y|W=1,X] using random forests, then plugs the predictions into the pseudo-outcome.



#  Split the sample in two random subsamples, S1 and S2

# Form prediction models in S1, use it to predict in S2

# Form prediction models in S2, use it to predict in S1

# 2-fold cross-fitting
n = length(Y)
m0hat = m1hat = ehat = rep(NA,n)
# Draw random indices for sample 1
set.seed(12344)
index_s1 = sample(1:n,n/2)
# Create S1
x1 = X[index_s1,]
w1 = W[index_s1]
y1 = Y[index_s1]
# Create sample 2 with those not in S1
x2 = X[-index_s1,]
w2 = W[-index_s1]
y2 = Y[-index_s1]
# Model in S1, predict in S2
rf = regression_forest(x1,w1)
ehat[-index_s1] = predict(rf,newdata=x2)$predictions
rf = regression_forest(x1[w1==0,],y1[w1==0])
m0hat[-index_s1] = predict(rf,newdata=x2)$predictions
rf = regression_forest(x1[w1==1,],y1[w1==1])
m1hat[-index_s1] = predict(rf,newdata=x2)$predictions
# Model in S2, predict in S1
rf = regression_forest(x2,w2)
ehat[index_s1] = predict(rf,newdata=x1)$predictions
rf = regression_forest(x2[w2==0,],y2[w2==0])
m0hat[index_s1] = predict(rf,newdata=x1)$predictions
rf = regression_forest(x2[w2==1,],y2[w2==1])
m1hat[index_s1] = predict(rf,newdata=x1)$predictions

#Now, create the pseudo-outcomes to be averaged to estimate the Average Potential Outcomes APOs

Y_t_0 = m0hat + (1-W)*(Y-m0hat)/(1-ehat)
Y_t_1 = m1hat + W*(Y-m1hat)/ehat

# Use the APO pseudo-outcomes in a simple OLS with only a constant to get the APO estimates and inference 
# (this is equivalent to running a t-test on the mean of the pseudo-outcome)

summary(lm(Y_t_0 ~ 1))
mean(Y_t_0)
summary(lm(Y_t_1 ~ 1))
mean(Y_t_1)

# Create the pseudo-outcome for ATE and use it in a simple OLS with only a constant to get the ATE point estimate and inference:

Y_ate = Y_t_1 - Y_t_0
summary(lm(Y_ate ~ 1))



#----------------------------------------------
# ATT (Average Treatment Effect on the Treated)
#----------------------------------------------
# Double Robust ATT estimator:
# ATT pseudo-outcome
Y_att <- W * (Y - m0hat) - (1 - W) * (ehat / (1 - ehat)) * (Y - m0hat)

# Intercept-only regression to obtain the estimate, standard error, and p-value
att_model <- lm(Y_att/ mean(W) ~ 1)

cat("ATT (AIPW) with robust inference:\n")
summary(att_model)



# Double ML for AIPW with causalDML package
###########################################

#2-fold cross-fitting is easy to implement but especially in small sample sizes, 
#using only 50% of the data to estimate the nuisance parameters might lead to unstable predictions.

# The DML_aipw function from the causalDML package is used to run 5-fold cross-fitting.
# This package requires user-defined methods because it allows for ensemble methods.
# The implementation below focuses on honest random forests.

# With 5-fold cross-fitting, the sample is split into 5 folds, and 4 folds (80% of the data) predict the left-out fold (20% of the data).
# The procedure iterates until every fold has been left out once.

# 5-fold cross-fitting with causalDML package
if (requireNamespace("causalDML", quietly = TRUE)) {
library(causalDML)
# Create learner
forest = create_method("forest_grf",args=list(tune.parameters = "all"))
# Run and store
aipw = DML_aipw(Y,W,X,ml_w=list(forest),ml_y=list(forest),cf=5)

# Estimated average potential outcomes:
summary(aipw$APO)
plot(aipw$APO)

#The average treatment effect is then just the difference between the two potential outcomes:
summary(aipw$ATE)

# The same nuisance parameters can be used to estimate the ATT.
# The APO_dml_atet() function computes the Average Potential Outcome (APO) for the treated group.
# This function uses the following inputs:
# -  Y: The observed outcomes (dependent variable).
# -  aipw$APO$m_mat: This represents the matrix of outcome models. These are predictions of 
# -  aipw$APO$w_mat: This represents the matrix of the treatment variable 
# -  aipw$APO$cf_mat: This represents the cross-fitting matrices used in double machine learning (DML).
# The APO_dml_atet() function combines these components to compute the potential outcomes under no treatment for the treated group, 
# adjusting for confounding and utilizing the nuisance parameters. 
APO_att = APO_dml_atet(Y,aipw$APO$m_mat,aipw$APO$w_mat,aipw$APO$e_mat,aipw$APO$cf_mat)

# Then, ATE_dml() calculates differences in potential outcomes,
# which can be used for either ATE or ATT depending on the APOs provided as inputs.
# By using APO_dml_atet(), you ensure the inputs focus solely on the treated group, making the result from ATE_dml() the ATT.
ATT = ATE_dml(APO_att)
summary(ATT)


# Collect and plot the results
Effect = c(aipw$ATE$result[1],ATT$results[1])
se = c(aipw$ATE$result[2],ATT$results[2])
data.frame(Effect,se,
           Target = c("ATE","ATT"),
           cil = Effect - 1.96*se,
           ciu = Effect + 1.96*se)  %>% 
  ggplot(aes(x=Target,y=Effect,ymin=cil,ymax=ciu)) + geom_point(size=2.5) + geom_errorbar(width=0.15)  +
  geom_hline(yintercept=0) + xlab("Target parameter")
} else {
  cat("causalDML is not available; skipping AIPW-DML diagnostic.\n")
}







# Effect heterogeneity and its validation/inspection
#######################################################
##############################################################

# 1398 (funziona con 60)
# 1357 (funziona con 50)
set.seed(1598) # for replicability

#To illustrate how the ideas for validating experiments generalize to the unconfoundedness setting, create a 50:50 sample split:

# Determine the number of rows in X
n_rows <- nrow(X)

# Generate a random vector of indices
indices <- sample(1:n_rows, size = 0.6*n_rows)

# Split the data
X_train <- X[indices,]
X_test <- X[-indices,]
W_train <- W[indices]
W_test <- W[-indices]
Y_train <- Y[indices]
Y_test <- Y[-indices]




# S-learner: Training on training set, predicting on test set
##############################################################

# Step 1: Prepare the training matrix [W | X]
# Combine the treatment indicator and covariates into a single feature matrix
WX_train <- cbind(W_train, X_train)

# Step 2: Train a regression forest to estimate E[Y | W, X]
# This model learns the conditional expectation of Y given treatment status and covariates
rf_sl <- regression_forest(WX_train, Y_train)

# Step 3: Create two test matrices for counterfactual prediction
# One where all individuals are assigned W = 0 (untreated)
W0X_test <- cbind(rep(0, nrow(X_test)), X_test)

# One where all individuals are assigned W = 1 (treated)
W1X_test <- cbind(rep(1, nrow(X_test)), X_test)

# Step 4: Estimate CATE for each individual in the test set
# CATE = predicted outcome under treatment - predicted outcome under control
cate_sl_test <- predict(rf_sl, W1X_test)$predictions - predict(rf_sl, W0X_test)$predictions


# Step 5: Visualization of CATE for each individual in the test set

hist(cate_sl_test,
     col = "skyblue",
     border = "white",
     main = "S-learner: Predicted CATEs on Test Set",
     xlab = "Predicted Individual Treatment Effect",
     breaks = 30)




# T-learner on training sample, predictions on test set
#########################################################

# Step 1: Train separate models on training data  #### CROSSVALIDATION HERE
# One model for treated (W = 1), one model for controls (W = 0)
rfm1_tl <- regression_forest(X_train[W_train == 1, ], Y_train[W_train == 1])
rfm0_tl <- regression_forest(X_train[W_train == 0, ], Y_train[W_train == 0])

# Step 2: Predict potential outcomes on test data
# Predict E[Y(1) | X] and E[Y(0) | X] for test observations
mu1_test <- predict(rfm1_tl, X_test)$predictions
mu0_test <- predict(rfm0_tl, X_test)$predictions

# Step 3: Compute CATE estimates on test data
cate_tl_all <- mu1_test - mu0_test

# Step 4: CATT-like effect: only for test set treated individuals
X_test_treated <- X_test[W_test == 1, ]
mu1_treated <- predict(rfm1_tl, X_test_treated)$predictions
mu0_treated <- predict(rfm0_tl, X_test_treated)$predictions
cate_tl_treated <- mu1_treated - mu0_treated

# Step 5: CATU-like effect: only for test set untreated individuals
X_test_untreated <- X_test[W_test == 0, ]
mu1_untreated <- predict(rfm1_tl, X_test_untreated)$predictions
mu0_untreated <- predict(rfm0_tl, X_test_untreated)$predictions
cate_tl_untreated <- mu1_untreated - mu0_untreated

# Step 6: Cerqua-style effect: observed Y minus predicted Y(0)
cate_tl_cerqua <- Y_test[W_test == 1] - mu0_treated

# Step 6: Plot the distributions with relative frequency
hist(cate_tl_all,
     col = rgb(1, 0, 0, 0.4),
     border = "white",
     probability = TRUE,
     main = expression("T-learner: CATE, " * CATT ~ (hat(alpha)) * ", CATT " * (hat(hat(alpha)))),
     xlab = "Individual Treatment Effect",
     xlim = range(c(cate_tl_all, cate_tl_treated, cate_tl_cerqua)),
     breaks = 30)

hist(cate_tl_treated,
     col = rgb(0, 0, 1, 0.4),
     border = "white",
     probability = TRUE,
     add = TRUE,
     breaks = 30)

hist(cate_tl_cerqua,
     col = rgb(1, 0.5, 0, 0.4),
     border = "white",
     probability = TRUE,
     add = TRUE,
     breaks = 30)

# Add mean lines
abline(v = mean(cate_tl_all), col = "red", lwd = 2, lty = 2)
abline(v = mean(cate_tl_treated), col = "blue", lwd = 2, lty = 2)
abline(v = mean(cate_tl_cerqua), col = "orange", lwd = 2, lty = 2)

# Add legend with math labels
legend("topright",
       legend = c("CATE", expression(CATT ~ (hat(alpha))), expression(CATT ~ (hat(hat(alpha))))),
       fill = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4), rgb(1, 0.5, 0, 0.4)),
       bty = "n")

# Print means
cat("Mean CATE (all):", round(mean(cate_tl_all), 4), "\n")
cat("Mean CATT (treated):", round(mean(cate_tl_treated), 4), "\n")
cat("Mean Cerqua-style:", round(mean(cate_tl_cerqua), 4), "\n")



### Final Figure 8 plot

figure8_output_file <- file.path(figure_output_dir, "CATE_CATU_CATT.png")
png(filename = figure8_output_file, width = 1162, height = 668)

# Istogramma base: CATE
hist(cate_tl_all,
     col = rgb(1, 0, 0, 0.4),       # rosso trasparente
     border = "white",
     probability = TRUE,
     
     main = expression("T-learner: CATE, " *   CATT ~ (hat(alpha))    * " and CATU. " * CATT ~ (hat(hat(alpha)))),
     
     xlab = "Predicted Individual Treatment Effect",
     xlim = range(c(cate_tl_all, cate_tl_treated, cate_tl_cerqua, cate_tl_untreated)),
     breaks = 50)

# CATT (trattati)
hist(cate_tl_treated,
     col = rgb(0, 0, 1, 0.4),       # blu trasparente
     border = "white",
     probability = TRUE,
     add = TRUE,
     breaks = 30)

# Cerqua-style
hist(cate_tl_cerqua,
     col = rgb(1, 0.5, 0, 0.4),     # arancione trasparente
     border = "white",
     probability = TRUE,
     add = TRUE,
     breaks = 30)

# CATU (non trattati)
hist(cate_tl_untreated,
     col = rgb(0, 0.5, 0, 0.4),     # verde scuro trasparente
     border = "white",
     probability = TRUE,
     add = TRUE,
     breaks = 30)

# Linee verticali delle medie
abline(v = mean(cate_tl_all), col = "red", lwd = 2, lty = 2)
abline(v = mean(cate_tl_treated), col = "blue", lwd = 2, lty = 2)
abline(v = mean(cate_tl_untreated), col = "darkgreen", lwd = 2, lty = 2)
abline(v = mean(cate_tl_cerqua), col = "orange", lwd = 2, lty = 2)

# Updated legend
legend("topright",
       legend = c("CATE",
                  expression(CATT ~ (hat(alpha))),
                  expression(CATT ~ (hat(hat(alpha)))),
                  expression(CATU)),
       fill = c(rgb(1, 0, 0, 0.4),
                rgb(0, 0, 1, 0.4),
                rgb(1, 0.5, 0, 0.4),
                rgb(0, 0.5, 0, 0.4)),
       bty = "n")

dev.off()
cat("Figure 8 saved to:", figure8_output_file, "\n")

# Print means to console
cat("Mean CATE (all):", round(mean(cate_tl_all), 4), "\n")
cat("Mean CATT (treated):", round(mean(cate_tl_treated), 4), "\n")
cat("Mean Cerqua-style:", round(mean(cate_tl_cerqua), 4), "\n")
cat("Mean CATU (untreated):", round(mean(cate_tl_untreated), 4), "\n")



# R-learner on training sample, predictions on test set
########################################################

# Step 1: Estimate nuisance components on training set       #### CROSSVALIDATION HERE
# Regression forests are used for mhat and ehat
m_rf <- regression_forest(X_train, Y_train)
mhat_train <- predict(m_rf)$predictions

e_rf <- regression_forest(X_train, W_train)
ehat_train <- predict(e_rf)$predictions

# Step 2: Create residuals for training set
res_y_train <- Y_train - mhat_train
res_w_train <- W_train - ehat_train

# Step 3: Modified covariates (with intercept)
n_train <- length(Y_train)
X_wc_train <- cbind(rep(1, n_train), X_train)
Xstar_train <- X_wc_train * res_w_train

# Step 4: Fit R-learner via OLS on modified covariates
rl_ols <- lm(res_y_train ~ 0 + Xstar_train)

# Step 5: Predict CATE on test set using original covariates
X_wc_test <- cbind(rep(1, nrow(X_test)), X_test)
#cate_rl_ols_test <- X_wc_test %*% rl_ols$coefficients

# Step 6: Plot histogram of CATE predictions from OLS
#hist(cate_rl_ols_test,
#     col = "darkorange",
#     border = "white",
#     main = "R-learner (OLS): CATE estimates on Test Set",
#     xlab = "Predicted Individual Treatment Effect",
#     probability = TRUE,
#     breaks = 30)

# Print mean CATE (OLS)
#cat("Mean CATE (R-learner OLS, test set):", round(mean(cate_rl_ols_test), 4), "\n")


# Additional: R-learner with Random Forest (weighted regression)
########################################################
# Step 7: Create pseudo outcome and weights
pseudo_rl_train <- res_y_train / res_w_train
weights_rl_train <- res_w_train^2

# Step 8: Train weighted regression forest on training data
rrf_fit <- regression_forest(X_train, pseudo_rl_train, sample.weights = weights_rl_train)
cate_rl_rf_test <- predict(rrf_fit, X_test)$predictions

# Step 9: Plot histogram of RF-based CATE estimates
hist(cate_rl_rf_test,
     col = "steelblue",
     border = "white",
     main = "R-learner (RF): CATE estimates on Test Set",
     xlab = "Predicted Individual Treatment Effect",
     probability = TRUE,
     breaks = 30)

# Print mean CATE (RF)
cat("Mean CATE (R-learner RF, test set):", round(mean(cate_rl_rf_test), 4), "\n")





# DR-learner on training sample, predictions on test set
########################################################

# Step 1: Fit m(0,X) and m(1,X) using regression forests             #### CROSSVALIDATION HERE
rfm0 <- regression_forest(X_train[W_train == 0, ], Y_train[W_train == 0])
rfm1 <- regression_forest(X_train[W_train == 1, ], Y_train[W_train == 1])

# Step 2: Predict m0 and m1 for all training data
m0hat_train <- predict(rfm0, X_train)$predictions
m1hat_train <- predict(rfm1, X_train)$predictions

# Step 3: Predict ehat (propensity score)
rfp <- regression_forest(X_train, W_train)
ehat_train <- predict(rfp)$predictions

# Step 4: Compute pseudo-outcome for DR-learner
Y_tilde_train <- m1hat_train - m0hat_train +
  W_train * (Y_train - m1hat_train) / ehat_train -
  (1 - W_train) * (Y_train - m0hat_train) / (1 - ehat_train)

# Step 5: Fit final model on training data using pseudo-outcome
rf_dr <- regression_forest(X_train, Y_tilde_train)

# Step 6: Predict CATE on test data
cate_dr_test <- predict(rf_dr, X_test)$predictions

# Step 7: Plot DR-learner CATE distribution
hist(cate_dr_test,
     col = "purple",
     border = "white",
     main = "DR-learner: CATE estimates on Test Set",
     xlab = "Predicted Individual Treatment Effect",
     probability = TRUE,
     breaks = 30)

# Step 8: Print mean CATE
cat("Mean CATE (DR-learner, test set):", round(mean(cate_dr_test), 4), "\n")


# Run causal forest in the training sample:


# Causal Forest (CF) learner on training sample, predict on test set
######################################################################
CF <- causal_forest(X_train, Y_train, W_train, tune.parameters = "all")      #### CROSSVALIDATION DOVREBBE GIÁ ESSERE FATTA
cate_cf_test <- predict(CF, X_test)$predictions

# Plot CF CATE distribution
hist(cate_cf_test,
     col = "forestgreen",
     border = "white",
     main = "Causal Forest: CATE estimates on Test Set",
     xlab = "Predicted Individual Treatment Effect",
     probability = TRUE,
     breaks = 30)

# Print mean CATE for CF
cat("Mean CATE (Causal Forest, test set):", round(mean(cate_cf_test), 4), "\n")



###Plot the results against each other to see that they are correlated, butfar from finding the same predictions.


# Store and plot predictions



results_all = cbind(cate_sl_test, cate_tl_all, cate_tl_treated, cate_tl_cerqua, cate_rl_rf_test, cate_dr_test, cate_cf_test )
colnames(results_all) = c("S-learner","T-learner","CATT_alpha_hat", "CATT_alpha_double_hat","R-learner","DR-learner","Causal Forest")
pairs.panels(results_all,method = "pearson")
describe(results_all)


results_treated <- cbind(
  cate_sl_test[W_test == 1],
  cate_tl_all[W_test == 1],
  cate_tl_treated,
  cate_tl_cerqua,
  cate_rl_rf_test[W_test == 1],
  cate_dr_test[W_test == 1],
  cate_cf_test[W_test == 1]
)
colnames(results_treated) = c("S-learner","T-learner","CATT_alpha_hat", "CATT_alpha_double_hat","R-learner","DR-learner","Causal Forest")
pairs.panels(results_treated,method = "pearson")
describe(results_treated)


cates_list_all <- list(
  cate_sl_test,
  cate_tl_all,
  cate_rl_rf_test,
  cate_dr_test,
  cate_cf_test
)

names(cates_list_all) <- c(
  "S-learner", "T-learner",
  "R-learner", "DR-learner", "Causal Forest"
)


### Best linear predictor (BLP)

# Get the pseudo-outcome in the test sample (The pseudo-outcome is used as a dependent variable to validate the predicted CATEs):

if (requireNamespace("causalDML", quietly = TRUE)) {
library(causalDML)
aipw_test = DML_aipw(Y_test,W_test,X_test)
pseudoY = aipw_test$ATE$delta


library(estimatr)

pseudoY_all <- pseudoY

cates_list_all <- list(
  cate_sl_test,
  cate_tl_all,
  cate_rl_rf_test,
  cate_dr_test,
  cate_cf_test
)

names(cates_list_all) <- c(
  "S-learner", "T-learner",
  "R-learner", "DR-learner", "Causal Forest"
)

blp_results_all <- lapply(cates_list_all, function(cates) {
  demeaned_cates <- cates - mean(cates)
  lm_blp <- lm_robust(pseudoY_all ~ demeaned_cates)
  return(summary(lm_blp))
})

print(blp_results_all)


### Sorted Group Average Treatment Effects (GATES)


# GATES analysis for each CATE
library(dplyr)
library(ggplot2)

K <- 4

method_colors <- RColorBrewer::brewer.pal(7, "Set1")
names(method_colors) <- names(cates_list_all)

gates_results <- mapply(function(cates, method_name) {
  slices <- factor(as.numeric(cut(cates, breaks = quantile(cates, probs = seq(0, 1, length = K + 1)), include.lowest = TRUE)))
  G_ind <- model.matrix(~ 0 + slices)
  gates_fit <- lm_robust(pseudoY_all ~ 0 + G_ind)
  gates_wc <- lm_robust(pseudoY_all ~ G_ind[,-1])
  se <- gates_fit$std.error
  gates_df <- data.frame(
    Variable = paste("Group", 1:K),
    Coefficient = gates_fit$coefficients,
    cil = gates_fit$coefficients - 1.96 * se,
    ciu = gates_fit$coefficients + 1.96 * se
  )
  # Extract p-value for last coefficient (gamma_K - gamma_1)
  wc_pval <- round(summary(gates_wc)$coefficients[nrow(summary(gates_wc)$coefficients), "Pr(>|t|)"], 3)
  gates_plot <- ggplot(gates_df, aes(x = Variable, y = Coefficient, ymin = cil, ymax = ciu)) +
    geom_point(size = 3, color = method_colors[[method_name]]) +
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = mean(gates_fit$coefficients), linetype = "dashed") +
    ggtitle(paste0("GATES Estimates - ", method_name, "\n", "H0: gamma[5] - gamma[1] = 0, p = ", wc_pval))
  list(summary = summary(gates_fit), wc_test = summary(gates_wc), plot = gates_plot)
}, cates_list_all, names(cates_list_all), SIMPLIFY = FALSE)

names(gates_results) <- names(cates_list_all)

#install.packages("gridExtra")   # solo la prima volta
library(gridExtra)

grid.arrange(grobs = lapply(gates_results, function(x) x$plot), ncol = 2)


####Versione con test quartile 1 == quartile 4 in figura
##########################################################

# GATES analysis for each CATE
library(dplyr)
library(ggplot2)

K <- 4

method_colors <- RColorBrewer::brewer.pal(7, "Set1")
names(method_colors) <- names(cates_list_all)

gates_results <- mapply(function(cates, method_name) {
  slices <- factor(as.numeric(cut(cates, breaks = quantile(cates, probs = seq(0, 1, length = K + 1)), include.lowest = TRUE)))
  G_ind <- model.matrix(~ 0 + slices)
  gates_fit <- lm_robust(pseudoY_all ~ 0 + G_ind)
  gates_wc <- lm_robust(pseudoY_all ~ G_ind[,-1])
  se <- gates_fit$std.error
  gates_df <- data.frame(
    Variable = paste("Group", 1:K),
    Coefficient = gates_fit$coefficients,
    cil = gates_fit$coefficients - 1.96 * se,
    ciu = gates_fit$coefficients + 1.96 * se
  )
  # Extract p-value for last coefficient (gamma_K - gamma_1)
  wc_pval <- round(summary(gates_wc)$coefficients[nrow(summary(gates_wc)$coefficients), "Pr(>|t|)"], 3)
  gates_plot <- ggplot(gates_df, aes(x = Variable, y = Coefficient, ymin = cil, ymax = ciu)) +
    geom_point(size = 3, color = method_colors[[method_name]]) +
    geom_errorbar(width = 0.15) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = mean(gates_fit$coefficients), linetype = "dashed") +
    ggtitle(paste0("GATES Estimates - ", method_name, "\n", "H0: gamma[5] - gamma[1] = 0, p = ", wc_pval))
  list(summary = summary(gates_fit), wc_test = summary(gates_wc), plot = gates_plot)
}, cates_list_all, names(cates_list_all), SIMPLIFY = FALSE)

names(gates_results) <- names(cates_list_all)
library(gridExtra)
grid.arrange(grobs = lapply(gates_results, function(x) x$plot), ncol = 2)

} else {
  cat("causalDML is not available; skipping BLP and GATES diagnostics that use DML_aipw.\n")
}






###  Classification analysis (CLAN)

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
  existing_vars <- vars[vars %in% names(month_1_data)]
  month_1_data[[group]] <- if (length(existing_vars) > 0) {
    as.numeric(rowSums(month_1_data[, existing_vars, drop = FALSE]) > 0)
  } else {
    0
  }
}


# Código de la vía de transporte 1 MARITIMO 2 FERREO 3 TERRESTRE 4 AEREO 5 CORREO 6 MULTIMODAL 7 INSTALACIONES DE TRANSPORTE FIJAS (TUBERIAS, CABLE, ETC.) 8 VIAS NAVEGABLES INTERIORES 9 OTRO MODO DE TRANSPORTE ]

if ("via_1" %in% names(month_1_data)) {
  month_1_data <- month_1_data %>% rename(sea = via_1)
}

if ("via_3" %in% names(month_1_data)) {
  month_1_data <- month_1_data %>% rename(land = via_3)
}

if ("via_4" %in% names(month_1_data)) {
  month_1_data <- month_1_data %>% rename(air = via_4)
}
#month_1_data <- month_1_data %>%  rename(pipelines = via_7)




# Define variables to inlude in CAdiff
vars_CAdiff <- c("Agriculture", "Chemicals", "Manufacturing", "Metals", "Special", "Textile", "Wood", "air", "land","ND", "NO", "NP", "sea", "lnX", "lnX_import")
formula_str <- paste("~ 0 +", paste(vars_CAdiff, collapse = " + "))
XCA <- model.matrix(as.formula(formula_str), data = month_1_data)

# --- Regressions on quartile extremes for each CATE estimator ---


X_testCA <- XCA[-indices,]



library(estimatr)

cate_quartile_regressions <- lapply(cates_list_all, function(cates) {
  q <- quantile(cates, probs = c(0.25, 0.75))
  q1_q4_index <- which(cates <= q[1] | cates >= q[2])
  q1_q4_label <- ifelse(cates[q1_q4_index] <= q[1], 1, 0)
  predictors <- as.data.frame(X_testCA[q1_q4_index, ])
  results <- lapply(names(predictors), function(varname) {
    fmla <- reformulate(varname, response = "q1_q4_label")
    lm_robust(fmla, data = cbind(q1_q4_label = q1_q4_label, predictors))
  })
  names(results) <- colnames(predictors)
  results
})

names(cate_quartile_regressions) <- names(cates_list_all)


