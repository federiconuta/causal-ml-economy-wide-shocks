# This script sorts, manually,the treatment effects from COVID-19 in Colombian exporters
# to take into account the variation of the alphas (exploiting information coming from our bootstraps)
# =============================================
# =============================================
rm(list = ls())
library(dplyr)
library(stats)





# Set Directories -------------------------------------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

tab5_dir <- normalizePath(Sys.getenv("OBES_TAB5_DIR", script_dir), winslash = "/", mustWork = FALSE)
replication_dir <- normalizePath(Sys.getenv("OBES_REPLICATION_DIR", file.path(tab5_dir, "..")), winslash = "/", mustWork = FALSE)
data_out_dir <- normalizePath(Sys.getenv("OBES_DATA_OUT_DIR", file.path(replication_dir, "data", "data_out")), winslash = "/", mustWork = FALSE)
sam_sum_dir <- normalizePath(Sys.getenv("OBES_SAM_SUM_DIR", file.path(replication_dir, "Fig_4_and_7", "SAM_minus_SUM")), winslash = "/", mustWork = FALSE)
tab4_heter_dir <- normalizePath(Sys.getenv("OBES_TAB4_HETER_DIR", file.path(replication_dir, "Tab_4", "construction_data_heter")), winslash = "/", mustWork = FALSE)

setwd(tab5_dir)

load(file.path(data_out_dir, "final_data_19.RData"))

final_data_19_indices <- final_data_19 %>%
  select(id, month, index_health_w, index_economy_w, index_government_w, index_stringency_w, index_health_w_import, index_economy_w_import, index_government_w_import, index_stringency_w_import) %>%
  mutate(id = as.character(id), 
         month = as.character(month))
# ---------------------------------
library(stringr) # required for str_replace_all
library(dplyr)
library(caret)    
library(glmnet)    #for logit-LASSO/RIDGE
library(randomForest)    #for Random Forest
library(purrr)       # to work with lists
library(ggplot2)


#----------------------------------------------------
options(scipen=999) #remove scientific notation
#----------------------------------------------------


# Load predictions for Logit-LASSO for all 2020 (by months)
load(file.path(sam_sum_dir, "SUM_mar26", "SUM_preds_lasso.RData"))   # Original LASSO predictions for SUM
load(file.path(sam_sum_dir, "SAM_mar26", "lasso_preds_19.RData"))    # Original LASSO predictions for SAM
load(file.path(tab4_heter_dir, "data_heterogeneity.RData"))

#----------------------------------------------------

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
original_predictions_lasso <- original_predictions_lasso %>% mutate(TE_indiv = export_future - pred_SUM)

original_predictions_lasso <- original_predictions_lasso %>%
  mutate(quarter = ifelse(month %in% qt1, "qt1",
                          ifelse(month %in% qt2, "qt2",
                                 ifelse(month %in% qt3, "qt3", "qt4" ))))

rm(SUM_preds_lasso, lasso_preds_19)

########################################################
# 2) CREATE VARIABLES WE WANT TO SELECT
########################################################
#data_heterogeneity<-data_heterogeneity[complete.cases(data_heterogeneity),]
data_heterogeneity$month=as.factor(data_heterogeneity$month)

# Modifying industry_mode:

index <- data_heterogeneity$industry_mode == "Arms (19)" | data_heterogeneity$industry_mode == "Art (21)"| data_heterogeneity$industry_mode == "Precis. inst. (18)"| data_heterogeneity$industry_mode =="Special (22)"
data_heterogeneity$industrymode_aggregated[index] <- "Special"

index1 <- data_heterogeneity$industry_mode == "Animal (01)" |data_heterogeneity$industry_mode=="Vegetable (02)" |data_heterogeneity$industry_mode == "Prep. food (04)"| data_heterogeneity$industry_mode == "Fats/oils (03)"
data_heterogeneity$industrymode_aggregated[index1] <- "Agricolture"

index2 <- data_heterogeneity$industry_mode == "Textile (11)" | data_heterogeneity$industry_mode == "Leather (08)"| data_heterogeneity$industry_mode == "Footwear (12)"
data_heterogeneity$industrymode_aggregated[index2] <- "Textile_industry"

index3 <- data_heterogeneity$industry_mode == "Machinery (16)" | data_heterogeneity$industry_mode == "Manuf. (20)"| data_heterogeneity$industry_mode == "Vehicles (17)"
data_heterogeneity$industrymode_aggregated[index3] <- "Industry"

index4 <- data_heterogeneity$industry_mode == "Metals (15)" | data_heterogeneity$industry_mode == "Jewel (14)" | data_heterogeneity$industry_mode == "Mineral (05)"| data_heterogeneity$industry_mode == "Cement (13)"
data_heterogeneity$industrymode_aggregated[index4] <- "Metals"

index5 <- data_heterogeneity$industry_mode == "Chemical (06)" | data_heterogeneity$industry_mode == "Plastics (07)" | data_heterogeneity$industry_mode == "Mineral (05)"| data_heterogeneity$industry_mode == "Cement (13)"
data_heterogeneity$industrymode_aggregated[index5] <- "Chemicals"

index6 <- data_heterogeneity$industry_mode == "Wood (09)" | data_heterogeneity$industry_mode == "Paper (10)" 
data_heterogeneity$industrymode_aggregated[index6] <- "Wood_preparations"

#indix <- data_heterogeneity$continent_import == "0"
#data_heterogeneity$continent_import[indix] <- "None"


indices <- data_heterogeneity$region_mode == "5" | data_heterogeneity$region_mode == "8"| data_heterogeneity$region_mode == "13" | data_heterogeneity$region_mode == "20"| data_heterogeneity$region_mode == "23"| data_heterogeneity$region_mode == "44"| data_heterogeneity$region_mode == "47"|
  data_heterogeneity$region_mode == "70" 
data_heterogeneity$region_mode_aggregated[indices] <- "Caribe"

indices1 <- data_heterogeneity$region_mode == "11" | data_heterogeneity$region_mode == "15"| data_heterogeneity$region_mode == "17" | data_heterogeneity$region_mode == "19"| data_heterogeneity$region_mode == "25"| data_heterogeneity$region_mode == "41"| data_heterogeneity$region_mode == "54"|
  data_heterogeneity$region_mode == "68"|data_heterogeneity$region_mode == "73" 
data_heterogeneity$region_mode_aggregated[indices1] <- "Andes"

indices2 <- data_heterogeneity$region_mode == "18" | data_heterogeneity$region_mode == "86"| data_heterogeneity$region_mode == "91" | data_heterogeneity$region_mode == "94"| data_heterogeneity$region_mode == "95"| data_heterogeneity$region_mode == "97"
data_heterogeneity$region_mode_aggregated[indices2] <- "Amazonia"

indices3 <- data_heterogeneity$region_mode == "27" | data_heterogeneity$region_mode == "52"| data_heterogeneity$region_mode == "76"
data_heterogeneity$region_mode_aggregated[indices3] <- "Pacifico"

indices4 <- data_heterogeneity$region_mode == "50" | data_heterogeneity$region_mode == "81"| data_heterogeneity$region_mode == "85" | data_heterogeneity$region_mode == "99"
data_heterogeneity$region_mode_aggregated[indices4] <- "Orinoquia"

indices5 <- data_heterogeneity$region_mode == "63" | data_heterogeneity$region_mode == "66"| data_heterogeneity$region_mode == "88" 
data_heterogeneity$region_mode_aggregated[indices5] <- "Insular"


#Transforming all character variables into factors
data_heterogeneity <- as.data.frame(unclass(data_heterogeneity),stringsAsFactors=TRUE)

data_heterogeneity <- data_heterogeneity %>% select(-export_future)  #drop not necessary variables

data_heterogeneity$lnX2=(data_heterogeneity$lnX)^2
data_heterogeneity$lnX3=(data_heterogeneity$lnX)^3

data_heterogeneity$NP2=(data_heterogeneity$NP)^2
data_heterogeneity$NP3=(data_heterogeneity$NP)^3
data_heterogeneity$ND2=(data_heterogeneity$ND)^2
data_heterogeneity$ND3=(data_heterogeneity$ND)^3
#data_heterogeneity$NO2= (data_heterogeneity$NO)^2
#data_heterogeneity$NO3 = (data_heterogeneity$NO)^3
data_heterogeneity$index_economy_w_import2=(data_heterogeneity$index_economy_w_import)^2
data_heterogeneity$index_economy_w_import3=(data_heterogeneity$index_economy_w_import)^3
data_heterogeneity$index_government_w2=(data_heterogeneity$index_government_w)^2
data_heterogeneity$index_government_w3=(data_heterogeneity$index_government_w)^3
data_heterogeneity$index_stringency_w2=(data_heterogeneity$index_stringency_w)^2
data_heterogeneity$index_stringency_w3=(data_heterogeneity$index_stringency_w)^3
data_heterogeneity$index_health_w_import2=(data_heterogeneity$index_health_w_import)^2
data_heterogeneity$index_health_w_import3=(data_heterogeneity$index_health_w_import)^3
data_heterogeneity$index_stringency_w_import2=(data_heterogeneity$index_stringency_w_import)^2
data_heterogeneity$index_stringency_w_import3=(data_heterogeneity$index_stringency_w_import)^3
data_heterogeneity$index_health_w2=(data_heterogeneity$index_health_w)^2
data_heterogeneity$index_health_w3=(data_heterogeneity$index_health_w)^3
data_heterogeneity$index_economy_w2=(data_heterogeneity$index_economy_w)^2
data_heterogeneity$index_economy_w3=(data_heterogeneity$index_economy_w)^3
data_heterogeneity$index_government_w_import2=(data_heterogeneity$index_government_w_import)^2
data_heterogeneity$index_government_w_import3=(data_heterogeneity$index_government_w_import)^3
data_heterogeneity$ln_exper_12_months_export2=(data_heterogeneity$ln_exper_12_months_export)^2
data_heterogeneity$ln_exper_12_months_export3=(data_heterogeneity$ln_exper_12_months_export)^3
data_heterogeneity$ln_exper_12_months_import2=(data_heterogeneity$ln_exper_12_months_import)^2
data_heterogeneity$ln_exper_12_months_import3=(data_heterogeneity$ln_exper_12_months_import)^3
############################################################################


# Remove constant variables monthly (train dataset): 
n_cols_train <- ncol(data_heterogeneity)
constant_list_train <- matrix(, nrow = n_cols_train, ncol = 1)
for (v in 1:n_cols_train) {
  constant_list_train[v,] <- (length(unique(data_heterogeneity[,v]))==1) *1
}
col_names_train <- colnames(data_heterogeneity)
constant_list_train <- as.integer(constant_list_train)
df <- cbind(col_names_train, constant_list_train) %>% as.data.frame %>% filter(constant_list_train==0) %>% select(col_names_train) 
data_heterogeneity <- select(data_heterogeneity, df$col_names_train)
rm(n_cols_train, constant_list_train, col_names_train, df)


names_variables <- names(data_heterogeneity)
#removing covid_aware from variables;
names_variables <- names_variables[!names_variables %in% "id"]
names_variables <- names_variables[!names_variables %in% "covid_aware"]
names_variables <- names_variables[!names_variables %in% "pred_rf"]
names_variables <- names_variables[!names_variables %in% "pred_lasso"]
names_variables <- names_variables[!names_variables %in% "pred_class_lasso"]
names_variables <- names_variables[!names_variables %in% "continent_export"]
names_variables <- names_variables[!names_variables %in% "continent_import"]
names_variables <- names_variables[!names_variables %in% "size"]
names_variables <- names_variables[!names_variables %in% "pred_class_rf"]
names_variables <- names_variables[!names_variables %in% "quarter"]
names_variables <- names_variables[!names_variables %in% "quartile_str_export"]
names_variables <- names_variables[!names_variables %in% "quartile_str_health_export"]
names_variables <- names_variables[!names_variables %in% "quartile_str_econ_export"]
names_variables <- names_variables[!names_variables %in% "quartile_str_gov_export"]
names_variables <- names_variables[!names_variables %in% "NP_distribution"] 
names_variables <- names_variables[!names_variables %in% "ND_distribution"] 
names_variables <- names_variables[!names_variables %in% "NO_distribution"]
names_variables <- names_variables[!names_variables %in% "quartile_str_import"] 
names_variables <- names_variables[!names_variables %in% "quartile_str_health_import"]
names_variables <- names_variables[!names_variables %in% "quartile_str_econ_import"]
names_variables <- names_variables[!names_variables %in% "quartile_str_gov_import"]
names_variables <- names_variables[!names_variables %in% "quartile_str_health_import_num"]
names_variables <- names_variables[!names_variables %in% "quartile_str_econ_import_num"] 
names_variables <- names_variables[!names_variables %in% "quartile_str_gov_import_num"]
names_variables <- names_variables[!names_variables %in% "NP_distribution_num"]
names_variables <- names_variables[!names_variables %in% "NO_distribution_num"]
names_variables <- names_variables[!names_variables %in% "exper_last_12_month_export"]
names_variables <- names_variables[!names_variables %in% "exper_last_12_month_import"]
##removing aggregated variables:
#names_variables <- names_variables[!names_variables %in% "iso_export_mode"]
#names_variables <- names_variables[!names_variables %in% "iso_import_mode"]
#names_variables <- names_variables[!names_variables %in% "sector_mode"]
names_variables <- names_variables[!names_variables %in% "industry_mode"]
#names_variables <- names_variables[!names_variables %in% "region_mode"]
names_variables <- names_variables[!names_variables %in% "continent_America"]
names_variables <- names_variables[!names_variables %in% "continent_Africa"]
names_variables <- names_variables[!names_variables %in% "continent_Oceania"]
names_variables <- names_variables[!names_variables %in% "continent_Europe"]
names_variables <- names_variables[!names_variables %in% "continent_Asia"]
names_variables <- names_variables[!names_variables %in% "region"]
names_variables <- names_variables[!names_variables %in% "natural_region"]
names_variables <- names_variables[!names_variables %in% "Xtot_avg"]
names_variables <- names_variables[!names_variables %in% "industrymode_aggregated"]
names_variables <- names_variables[!names_variables %in% "region_mode_aggregated"]




####DELETING VARIABLES
#Selecting only iso_import with more than 14 charachters (i.e. excluding iso_import_CHN...)
isoimport<-grep("iso_import_", names(data_heterogeneity), value=TRUE)
isoimport[nchar(isoimport)==14]
#Removing the iso_import_dummiesdummies (but keeping the interactions):
names_variables <- names_variables[!names_variables %in% isoimport]
#Removing the sector_mode dummies (but keeping the interactions which all contain _exp at the end):
sectors<-grep("sector_", names(data_heterogeneity), value=TRUE)
sectors <- sectors[!grepl("^sizeQ", sectors)]
sectors=sectors[nchar(sectors)>11]
names_variables <- names_variables[!names_variables %in% sectors]


industries<-grep("industry_", names(data_heterogeneity), value=TRUE)
industries <- industries[!grepl("^sizeQ", industries)]
industries=industries[nchar(industries)>13]
names_variables <- names_variables[!names_variables %in% industries]


#Removing other dummies already included with squares and cube
viadata<-grep("via_", names(data_heterogeneity), value=TRUE)
viadata=viadata[nchar(viadata)==5]
names_variables <- names_variables[!names_variables %in% viadata]

regiondata<-grep("region_", names(data_heterogeneity), value=TRUE)
regiondata1=regiondata[nchar(regiondata)==8]
regiondata2=regiondata[nchar(regiondata)==9]
names_variables <- names_variables[!names_variables %in% regiondata1]
names_variables <- names_variables[!names_variables %in% regiondata2]


isostrimport<-grep("iso_stringency_index_import_", names(data_heterogeneity), value=TRUE)
isostrimport=isostrimport[nchar(isostrimport)==31]
names_variables <- names_variables[!names_variables %in% isostrimport]

isohtimport<-grep("iso_health_index_import_", names(data_heterogeneity), value=TRUE)
isohtimport=isohtimport[nchar(isohtimport)==27]
names_variables <- names_variables[!names_variables %in% isohtimport]

isohtexport<-grep("iso_health_index_export_", names(data_heterogeneity), value=TRUE)
isohtexport=isohtexport[nchar(isohtexport)==27]
names_variables <- names_variables[!names_variables %in% isohtexport]

isoecimport<-grep("iso_economy_index_import_", names(data_heterogeneity), value=TRUE)
isoecimport=isoecimport[nchar(isoecimport)==28]
names_variables <- names_variables[!names_variables %in% isoecimport]

isogovimport<-grep("iso_government_index_import_", names(data_heterogeneity), value=TRUE)
isogovimport=isogovimport[nchar(isogovimport)==31]
names_variables <- names_variables[!names_variables %in% isogovimport]

isoexper<-grep("iso_exper_", names(data_heterogeneity), value=TRUE)
isoexper=isoexper[nchar(isoexper)==13]
names_variables <- names_variables[!names_variables %in% isoexper]

isosimple<-grep("iso_", names(data_heterogeneity), value=TRUE)
isosimple=isosimple[nchar(isosimple)==7]
names_variables <- names_variables[!names_variables %in% isosimple]

############################################################################ F.Q.

allvars<-names_variables[grepl("^sizeQ", names_variables)]
names_variables <- names_variables[!names_variables %in% allvars]

names_variables<-append(names_variables, "sizeQ3_iso_USA")
names_variables<-append(names_variables, "sizeQ3_iso_exper_USA")
names_variables<-append(names_variables, "sizeQ4_via_1")
names_variables<-append(names_variables, "iso_exper_USA")





#################################################
names_variables[which(names(names_variables) %in% c("pred_rf","covid_aware"))] <- NULL #drop outcome and treatment variable from set of predictors


names_variables <- c("id", "covid_aware", "pred_lasso", "pred_rf", "industry_mode", names_variables)
data_heterogeneity <- data_heterogeneity[, colnames(data_heterogeneity) %in% names_variables]
#data_heterogeneity <- data_heterogeneity %>% group_by(id, month) %>% unique()  

########################################################
# 3) MERGE SELECTED DATA WITH ORIGINAL PREDICTIONS FROM LASSO
########################################################
data_heterogeneity <- data_heterogeneity %>%
  mutate(id = as.integer(as.character(id)),     #adequate variables for the merge
         month = as.integer(as.character(month))) 


data_heterogeneity_original <- data_heterogeneity %>%
  left_join(., original_predictions_lasso, by = c("id", "month"))


#REVISE IT
#data_heterogeneity_original <- original_predictions_lasso %>%
#    left_join(., data_heterogeneity, by = c("id", "month"))


########################################################
# 4) RUN OLS WITH fm1 FORMULA
########################################################
quarter <- unique(data_heterogeneity_original$quarter)
month <- unique(data_heterogeneity_original$month)

#define empty list with the length of number of quarters (four)
filters <- as.character(quarter)
desired_length <- length(filters)
ols_predictions_list <- vector(mode = "list", length = desired_length)  #generate empty list to save SUM predictions for LASSO
names(ols_predictions_list) <- filters


#for (i in quarter) {
#  id_month_covid_aware <- data_heterogeneity_original %>%
#    filter(quarter == i) %>%
#    select(id, month, covid_aware, export_future, pred_lasso)
#  data <- data_heterogeneity_original %>% filter(quarter == i) %>% select(-month)
#  ols_model <- lm(fm1, data)
#  ols_predictions <- as.data.frame(ols_model$fitted.values)
#  ols_predictions <- cbind(id_month_covid_aware,ols_predictions) %>% rename(pred_ols = `ols_model$fitted.values`) %>% select(-export_future)
#  # calculate indiv. treatment effects
#  ols_predictions <- reshape(ols_predictions, idvar = c("id", "month"), timevar = "covid_aware", direction = "wide")
#  ols_predictions$TE_indiv_lasso = ols_predictions$pred_lasso.1 - ols_predictions$pred_lasso.0
#  ols_predictions$TE_indiv_ols = ols_predictions$pred_ols.1 - ols_predictions$pred_ols.0
#  ols_predictions_list[[i]] <- ols_predictions
#}
#ols_predictions_list <- do.call(rbind, ols_predictions_list)    # from list to dataframe
#ols_predictions_list$quarter <- rownames(ols_predictions_list)  #recover variable quarter
#ols_predictions_list$quarter <- substr(ols_predictions_list$quarter, 0, 3)  #keep only three first elements variable quarter ("qt1", "qt2", etc)

predictors_ols <- names_variables

predictors_ols <- unique(predictors_ols)

predictors_ols <- predictors_ols[predictors_ols %in% names(data_heterogeneity_original)]

predictors_ols <- setdiff(
  predictors_ols,
  c("id", "month", "quarter", "covid_aware", "pred_lasso", "pred_rf", "pred")
)

fm1 <- as.formula(
  paste("pred_lasso ~ covid_aware * (", paste(predictors_ols, collapse = " + "), ")")
)

fm2 <- as.formula(
  paste("pred_rf ~ covid_aware * (", paste(predictors_ols, collapse = " + "), ")")
)

fm3 <- as.formula(
  paste("pred ~ covid_aware * (", paste(predictors_ols, collapse = " + "), ")")
)



#define empty list with the length of number of months (twelve)
filters<- as.character(month)
desired_length <- length(filters)

ols_predictions_list <- vector(mode = "list", length = desired_length)  #generate empty list to save SUM predictions for LASSO
names(ols_predictions_list) <- filters


library(haven)
library(readr)

data_heterogeneity_original <- data_heterogeneity_original %>% select(-pred_lasso, -covid_aware) %>% unique()

library(readr)

write_csv(
  data_heterogeneity_original,
  file.path(tab5_dir, "data_heterogeneity_original_y_sum.csv")
)

