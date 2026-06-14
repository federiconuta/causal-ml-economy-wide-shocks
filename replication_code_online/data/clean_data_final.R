#Differente machines
rm(list = ls())

library(dplyr)
library(foreign)
library(lubridate)
library(ggplot2)
library(tidyr)
#library(finalfit) 

# No scientific notation
options(scipen=999)


# WARNING 1: make sure you have one "id" for each company
# WARNING 2: destination variable MUST start with "iso"
# WARNING 3: sector variable MUST start with "sector"
# WARNING 4: via variable MUST start with "via"
# WARNING 5: outcome variable MUST be called "export_future"
# WARNING 6: code cannot be run under "MASS" package in the library



# Set Directories -------------------------------------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
script_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

t.d <- normalizePath(Sys.getenv("OBES_DATA_DIR", script_dir), winslash = "/", mustWork = FALSE)
setwd(t.d)
figu.dir <- "figures/"
data.in <- "data_in/"
data.out <- "data_out/"
dir.create(data.out, showWarnings = FALSE, recursive = TRUE)
# ---------------------------------


# Import "exports" data - CORRER -------------------------------------------------------------
months_2017 <- read.dta("data_in/months_2017.dta")
months_2018 <- read.dta("data_in/months_2018.dta") #hacer en el resto de wd
months_2019 <- read.dta("data_in/months_2019.dta")
months_2020 <- read.dta("data_in/months_2020.dta")
load("data_in/industry_dictionary.RData")
# Import "imports" data - CORRER -------------------------------------------------------------
import <- readRDS("data_in/imports_origin.rds")
import <- import %>% rename(iso_import = iso,
                            value_fob = IM.io) 
import <- import %>% filter(iso_import != "COL")




data <- rbind(months_2017, months_2018, months_2019, months_2020)
# dim(months_2018)[1]+dim(months_2019)[1]+dim(months_2020)[1] == dim(data)[1]
# [1] TRUE
rm(list = "months_2018", "months_2019", "months_2020")

###################################################################
###### NOTE: Keep month/year variables numeric ########
data <- data %>%
  mutate(year = period %/% 100,        # Extract year and month from the period
         month = period %% 100,
         sector = floor(hs10/100000000))

#- Join data with indsutry and sector information
data <- left_join(data, industry_dictionary, by="sector")
data$sector <- NULL  #remove old variable "sector"
data <- data %>% rename(sector = sector_name)  #rename new sector variable
data <- data %>% filter(iso != "XCF") %>% filter(iso != "ZZZ") %>% filter(iso != "") #remove shipments to destinations XCF and ZZZ (they dont correspond to countries) amd empty countries
rm(industry_dictionary)
#-----------------------------------------------------------------




#-----------------------------------------------------------------
# Create function that converts "period" var into "year" and "month" var
year_month_function <- function(df){
  df <- df %>%
    mutate(year = as.numeric(period) %/% 100,        # Extract year and month from the period
           month = as.numeric(period) %% 100) %>%
    select(id, period, year, month, everything())
}
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Create a function that converts variables (var) into dummies 
dummyfier <- function(data, variable) {
  column <- deparse(substitute(variable))
  data %>%
    select(id, period, {{variable}}) %>%
    filter({{variable}} != "") %>%
    distinct() %>%
    pivot_wider(names_from = {{variable}}, values_from = {{variable}}, 
                values_fn = length, values_fill = 0,       #Use `values_fn = length` to identify where the duplicates arise
                names_prefix = paste0(column, '_'))
}
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Create a function that converts variables (var) into WEIGHTED dummies 
dummyfier_weight <- function(data, period, variable_name) {
  column <- deparse(substitute(variable_name))
  data %>%
    select(id, {{period}}, value_fob, {{variable_name}}) %>%
    filter({{variable_name}} != "") %>%
    group_by(id, {{period}}, {{variable_name}}) %>%
    summarise(value_fob = sum(value_fob, na.rm = T)) %>%
    group_by(id, {{period}}) %>%
    mutate(share = value_fob/sum(value_fob, na.rm = T)) %>%
    select(- value_fob) %>%
    pivot_wider(., names_from = {{variable_name}}, values_from = share,       #Use `values_fn = length` to identify where the duplicates arise
                values_fill = 0, names_prefix = paste0(column, '_'))
}
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Create a function that modifies previous WEIGHTED function but it has a modification for indeces export data 
dummyfier_weight_index_export <- function(data, period, variable_name, variable_value) {
  column <- deparse(substitute(variable_name))
  data %>%
    select(id, {{period}}, {{variable_value}}, {{variable_name}}) %>%
    filter({{variable_name}} != "") %>%
    group_by(id, {{period}}) %>%
    pivot_wider(., names_from = {{variable_name}}, values_from = {{variable_value}},       #Use `values_fn = length` to identify where the duplicates arise
                values_fill = 0, names_prefix = paste0(column, '_index_export_'))
}
#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Create a function that modifies previous WEIGHTED function but it has a modification for indeces import data 
dummyfier_weight_index_import <- function(data, period, variable_name, variable_value) {
  column <- deparse(substitute(variable_name))
  data %>%
    select(id, {{period}}, {{variable_value}}, {{variable_name}}) %>%
    filter({{variable_name}} != "") %>%
    group_by(id, {{period}}) %>%
    pivot_wider(., names_from = {{variable_name}}, values_from = {{variable_value}},       #Use `values_fn = length` to identify where the duplicates arise
                values_fill = 0, names_prefix = paste0(column, '_index_import_'))
}
#-----------------------------------------------------------------



#-----------------------------------------------------------------
# Create a function that modifies previous WEIGHTED function but it has a modification for experience data 
dummyfier_weight_exper <- function(data, period, variable_name, variable_value) {
  column <- deparse(substitute(variable_name))
  data %>%
    select(id, {{period}}, {{variable_value}}, {{variable_name}}) %>%
    filter({{variable_name}} != "") %>%
    group_by(id, {{period}}) %>%
    pivot_wider(., names_from = {{variable_name}}, values_from = {{variable_value}},       #Use `values_fn = length` to identify where the duplicates arise
                values_fill = 0, names_prefix = paste0(column, '_exper_'))
}
#-----------------------------------------------------------------




#-----------------------------------------------------------------
# Create a function that filters the data based on the desired year 
select_year <- function(data, yr) {
  data %>%
    filter(year == yr)
}
#-----------------------------------------------------------------

## COVID-19 data (cases+deaths)
## https://mdl.library.utoronto.ca/technology/tutorials/covid-19-data-r
## IMPORT RAW DATA: Johns Hopkins Github data
#confirmedraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
#str(confirmedraw) # Check latest date at the end of data
#deathsraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
## Note differences in the number of rows/columns
#
## DATA CLEANING: To create country level and global combined data
## Convert each data set from wide to long AND aggregate at country level
#library(tidyr)
#library(dplyr)
#confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
#deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
#summary(confirmed)
#
## Final data: combine the two
#covid <- full_join(confirmed, deaths) 
#
## Date variable
## Fix date variable and convert from character to date
#str(covid) # check date character
#covid$date <- covid$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
#str(covid) # check date Date
#covid <- covid %>% group_by(Country.Region, year = year(date), month = month(date)) %>% 
#  summarise(sum_cases = sum(confirmed, na.rm = T), sum_deaths = sum(deaths, na.rm = T)) %>% mutate(day = 1, date = make_datetime(year, month, day), days_month = days_in_month(date)) %>%
#  filter(year == "2020")
#
#covid$year <- NULL
#covid$month <- NULL
#covid$day <- NULL
## Create new variable: number of days
##covid <- covid %>% group_by(Country.Region) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)
#
##merge with iso-code and population by iso https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data
##library("wpp2019")
##data(pop)
#pop <- read.csv("COVID19data_4sept2020.csv", stringsAsFactors=FALSE)
#pop <- as.data.frame(pop)
#pop <- pop %>% select(countriesAndTerritories, countryterritoryCode, continentExp, popData2019) %>%
#  distinct()
#
#continents_dictionary <- pop[,1:2] 
#continents_dictionary <- continents_dictionary %>%
#  unique()
#
#
#covid <- merge(covid, pop, by.x = "Country.Region", by.y = "countriesAndTerritories", all.x = T) %>% rename(iso = countryterritoryCode)
#  
#data <- merge(data, continents_dictionary, by.x = "iso", by.y = "countryterritoryCode", all.x = T)



# COVID-19 data (https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data) - CORRER -----------------------------------------------------------
covid <- read.csv("data_in/COVID19data_14dec2020.csv", stringsAsFactors=FALSE)
covid <- as.data.frame(covid)
covid$month <- as.numeric(covid$month)


#library(stringr)  #to add leading zeros
covid <- covid %>%
  #select(month, year, cases, deaths, countryterritoryCode, popData2019, continentExp) %>%
  #rename(iso = countryterritoryCode) %>%
  #population2019 = popData2019,
  #continent = continentExp) %>%
  #arrange(iso, year, month) %>%
  mutate(yr = year,
         year = year %% 100, 
         day = 1,
         date = make_datetime(yr, month, day),
         #month = str_pad(month, 2, pad = "0"),    #add leading zeros
         period = paste0(year, month),
         days_in_month = days_in_month(date)) %>%
  filter(countryterritoryCode != "") %>%
  #group_by(iso, continentExp, year, month, popData2019, date, days_in_month, period) %>%
  group_by(countryterritoryCode, continentExp, year, month, popData2019, date, days_in_month, period) %>%
  summarise(cases = sum(cases, na.rm = T),
            deaths = sum(deaths, na.rm = T)) %>%
  #ungroup() %>%
  mutate_if(is.integer,funs(as.numeric)) %>%
  mutate(cases_daily_average_100000 = (cases/popData2019)*100000/days_in_month,
         deaths_daily_average_100000 = (deaths/popData2019)*100000/days_in_month)

covid$days_in_month <- NULL

continents_dictionary <- covid[,1:2] 
continents_dictionary <- continents_dictionary %>%
  unique()

data <- merge(data, continents_dictionary, by.x = "iso", by.y = "countryterritoryCode", all.x = T)


# Generate variables (correr solo una vez) ------------------------------------------------------

# set working directory where we will save the data 

data <- data[complete.cases(data),]  #clean NA's
data <- data %>%
  filter(value_fob>0) %>%
  filter(iso != "ZZZ")


#Clean Indeces Data
index_health <- read.csv("data_in/containment_health_index.csv", stringsAsFactors=FALSE)
index_economy <- read.csv("data_in/economic_support_index.csv", stringsAsFactors=FALSE)
index_government <- read.csv("data_in/government_response_index.csv", stringsAsFactors=FALSE)
index_stringency <- read.csv("data_in/stringency_index.csv", stringsAsFactors=FALSE)

library(reshape2)
library(stringr)
library(lubridate)
library(stringr)

indeces_convert_monthly <- function(df, old_name, new_name){
  df$X <- NULL
  df <- df %>%
    reshape2::melt(
      id.vars       = c("country_name", "country_code"),
      variable.name = "date",
      value.name    = "value",
      na.rm         = TRUE
    ) %>%
    mutate(date = as.character(date)) %>%
    rename(iso = country_code) %>%
    select(iso:value) %>%
    filter(iso != "") %>%
    mutate(
      date  = str_sub(date, 2),  # Delete first X
      date  = format(dmy(date), "%d-%b-%Y"),
      date  = lubridate::dmy(date),
      year  = as.numeric(format(date, "%Y")),
      month = as.numeric(format(date, "%m")),
      month = str_pad(month, 2, pad = "0"),
      month = as.numeric(month),
      value = as.numeric(value)
    ) %>%
    filter(year == 2020) %>%
    group_by(iso, year, month) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    select(iso, value, month) %>%
    rename_with(~ new_name, all_of(old_name)) %>%  # modern replacement for rename_
    as.data.frame()
}


index_health <- indeces_convert_monthly(index_health, "value", "index_health")
index_economy <- indeces_convert_monthly(index_economy, "value","index_economy")
index_government <- indeces_convert_monthly(index_government, "value", "index_government")
index_stringency <- indeces_convert_monthly(index_stringency, "value", "index_stringency")

indeces <- merge(index_health, index_stringency, by = c("iso", "month"), all.x = T)       #Economy/government have some more NAs than other indices
indeces <- merge(indeces, index_government, by = c("iso", "month"), all.x = T)
indeces <- merge(indeces, index_economy, by = c("iso", "month"), all.x = T)

indeces <- indeces[complete.cases(indeces),]   #remove those missing countries that are not well reported


rm("index_health", "index_economy", "index_government", "index_stringency")




# Treat dataset so to eliminate those companies high high export exposition to countries that dont appear in the indeces dataset
# Identify observations id-month from that appear in original "data" but dont appear in "indices" data 
#(because indices are reported at country level, while exports include as countries some comercial departments)

#missing_indeces_exports <- left_join(data, indeces, by=c("iso", "month"), all.x = T)

missing_indeces_exports <- left_join(data, indeces, by = c("iso", "month"))

missing_indeces_exports <- missing_indeces_exports %>% mutate(missing_index_country = ifelse((rowSums(is.na(missing_indeces_exports)) > 0) == T, 1, 0)) %>%  # takes 1 if this row has some NA
  group_by(id, period, missing_index_country) %>%
  summarise(sum_value = sum(value_fob, na.rm = T)) %>%
  group_by(id, period) %>%
  mutate(share_value = sum_value / sum(sum_value, na.rm = T)) %>%
  select(id, period, missing_index_country, sum_value, share_value, everything()) %>%  # Take share value in destinations with and without NA 
  mutate(drop = as.numeric(any(missing_index_country==1 & share_value>=0.5)),  # Value = 1 -> Selects those observations not missing (0) or those that are missing but the missing represents less than 50% of export value
         drop = ifelse(drop == 1, "TRUE", "FALSE")) %>%
  select(id, period, drop) %>%
  ungroup()       #take only the position to be deleted from the original dataset

data <- left_join(data, missing_indeces_exports, by=c("id", "period")) %>% unique() %>% filter(drop==F)


# Repeat the same with IMPORTS
import <- year_month_function(import)
indeces <- indeces %>% rename(iso_import = iso) #change name to make the merge
missing_indeces_imports <- left_join(import, indeces, by=c("iso_import", "month"))
missing_indeces_imports <- missing_indeces_imports %>% 
  ungroup() %>%
  mutate(missing_index_country = ifelse((rowSums(is.na(missing_indeces_imports)) > 0) == T, 1, 0)) %>%  # takes 1 if this row has some NA
  group_by(id, period, missing_index_country) %>%
  summarise(sum_value = sum(value_fob, na.rm = T)) %>%
  group_by(id, period) %>%
  mutate(share_value = sum_value / sum(sum_value, na.rm = T)) %>%
  select(id, period, missing_index_country, sum_value, share_value, everything()) %>%  # Take share value in destinations with and without NA 
  mutate(drop = as.numeric(any(missing_index_country==1 & share_value>=0.5)),  # Value = 1 -> Selects those observations not missing (0) or those that are missing but the missing represents less than 50% of export value
         drop = ifelse(drop == 1, "TRUE", "FALSE")) %>%
  select(id, period, drop) %>%
  ungroup()       #take only the position to be deleted from the original dataset

import <- left_join(import, missing_indeces_imports, by=c("id", "period")) %>% unique() %>% filter(drop==F)

indeces <- indeces %>% rename(iso = iso_import)  #recover original name






# Separate data into different years
data_20 <- data %>%
  filter(year == 20)
data_19 <- data %>%
  filter(year == 19) 
data_18 <- data %>%
  filter(year == 18) 
data_17 <- data %>%
  filter(year == 17)








#  Herfindahl-Hirschman Index (correr solo una vez) --------------------------------------------
##1) Herfindahl-Hirschman Index by product 6-digits

#--------------------------------------
# NP=number of products (by HS6)
HH.p <- data %>%
  mutate(hs6=floor(hs10/10000)) %>% 
  group_by(id, hs6, period, year, month) %>%
  summarize(Xp=sum(value_fob, na.rm=T)) %>%
  group_by(id, period, year, month) %>%   
  mutate(Xtot=sum(Xp, na.rm=T),
         sh.p2=(Xp/Xtot)^2) %>%
  summarize(HHp=sum(sh.p2, na.rm=T),
            NP=n()) %>%
  as.data.frame()

HH.d <- data %>%
  group_by(id, iso, period, year, month) %>%
  summarize(Xd=sum(value_fob, na.rm=T)) %>%
  group_by(id, period, year, month) %>%   
  mutate(Xtot=sum(Xd, na.rm=T),
         sh.d2=(Xd/Xtot)^2) %>%
  summarize(HHd=sum(sh.d2, na.rm=T),
            ND=n()) %>%
  as.data.frame()

HH <- merge(HH.p, HH.d, by = c("id", "year", "month", "period"), all = T)
rm(HH.d,HH.p)


#4) destinations registered (monthly) by company 
# Destination dummies (weighted)
dest.dummies <- dummyfier_weight(data, period, iso)
dest.dummies <- year_month_function(dest.dummies) # Extract variables "year" and "month" from the "period" var

# 5) Destination experience dummies (taking the info from destination dummies)
#dest.dummies.exper <- dest.dummies %>%
#  as_tibble() %>%
#  arrange(id, period) %>%
#  group_by(id) %>%
#  mutate(across(starts_with("iso"), cumsum)) %>%   #destination variable MUST start with iso
#  #mutate(id = as.character(id), period = as.character(period)) %>%
#  as.data.frame
#dest.dummies.exper[c(-1,-2)][dest.dummies.exper[c(-1,-2)]>1]=1     #convert into 1's all companies exporting to the same destination for more than 1 period 
#dest.dummies.exper <- dest.dummies.exper %>% 
#  rename_with(~paste0(., '_exp'), starts_with('iso'))   #add "_exp" to colnames because these variables represent the experience
#dest.dummies.exper <- year_month_function(dest.dummies.exper) # Extract variables "year" and "month" from the "period" var


#6) via used by company (monthly)
via.dummies <- dummyfier(data, via)
via.dummies <- year_month_function(via.dummies) # Extract variables "year" and "month" from the "period" var

#7) region operated by company (monthly) 
region.dummies <- dummyfier(data, region)
region.dummies <- year_month_function(region.dummies) # Extract variables "year" and "month" from the "period" var

#8) sectors operated by company (monthly)
sector.dummies <- dummyfier(data, sector)
sector.dummies <- year_month_function(sector.dummies) # Extract variables "year" and "month" from the "period" var


#9) previous experience on that sector  
sector.dummies.exper <- sector.dummies %>%
  as_tibble() %>%
  arrange(id, period) %>%
  group_by(id) %>%
  mutate(across(starts_with("sector"), cumsum)) %>%   #destination variable MUST start with sector
  as.data.frame
sector.dummies.exper[c(-1,-2)][sector.dummies.exper[c(-1,-2)]>1]=1     #convert into 1's all companies exporting to the same sector for more than 1 period 
sector.dummies.exper <- sector.dummies.exper %>% 
  rename_with(~paste0(., '_exp'), starts_with('sector'))   #Assigns names adding experience "_exp" to the colnames

sector.dummies.exper <- year_month_function(sector.dummies.exper) # Extract variables "year" and "month" from the "period" var


#10) continent 
data <- data %>% rename(continent = continentExp)
continent.dummies <- dummyfier(data, continent)
continent.dummies <- year_month_function(continent.dummies) 


#11) COVID-19: cases, deaths 
#year2020
covid_merge <- covid %>% as.data.frame()
covid_merge <- covid_merge %>%                       #adapt "covid" dataframe to "data" frame
  filter(year == 20) %>%
  rename(iso = "countryterritoryCode") %>%
  select(iso, year, month, cases_daily_average_100000, deaths_daily_average_100000) %>%
  mutate(month = as.numeric(month))

covid_cases_20 <- data_20 %>%
  select(id, year, month, iso) %>%
  unique() %>%
  mutate(month = as.numeric(month))

covid_cases_20 <- left_join(covid_cases_20, covid_merge, by = c("iso", "year", "month"))
covid_cases_20 <- covid_cases_20 %>% pivot_wider(names_from = iso,values_from=c(cases_daily_average_100000,deaths_daily_average_100000))

covid_cases_20[is.na(covid_cases_20)] <- 0


#year2019         -> COVID cases/deaths in 2020 countries assigned to companies operating in 2019
covid_merge <- covid %>% as.data.frame()
covid_merge <- covid_merge %>%                       #adapt "covid" dataframe to "data" frame
  filter(year == 20) %>%
  rename(iso = "countryterritoryCode") %>%
  select(iso, year, month, cases_daily_average_100000, deaths_daily_average_100000) %>%
  mutate(month = as.numeric(month),
         year = 19)   #change year to 2019 so we can merge it with 2019 dataset

covid_cases_19 <- data_19 %>%
  select(id, year, month, iso) %>%
  unique() %>%
  mutate(month = as.numeric(month))

covid_cases_19 <- left_join(covid_cases_19, covid_merge, by = c("year", "iso", "month"))
covid_cases_19 <- covid_cases_19 %>% pivot_wider(names_from = iso,values_from=c(cases_daily_average_100000,deaths_daily_average_100000))
covid_cases_19[is.na(covid_cases_19)] <- 0


#12)  Import dummies
import.dummies <- dummyfier_weight(import, period, iso_import)  # ATTENTION: it has different dimension than the rest of dummies
import.dummies <- year_month_function(import.dummies) # Extract variable "year" and "month" from the "period" var


#13) Govern.response indeces 
#2019: Assign indeces of COVID to 2019 dataset (according to the destination country of EXPORTS)
merge_data <- data_19 %>%
  select(id, iso, period, month, value_fob) %>%
  filter(iso != "") %>%
  distinct() %>%
  group_by(id, iso, month) %>%
  summarise(value_fob = sum(value_fob, na.rm = T)) %>% 
  group_by(id, month) %>%
  mutate(total_fob = sum(value_fob, na.rm = T),
         weight_fob = value_fob/total_fob) %>%
  select(-value_fob,-total_fob) %>%
  arrange(id, month) %>%
  as.data.frame() 


countries_original_data <- merge_data$iso %>% unique() #define those countries we have in our data 
countries_indeces <- indeces$iso %>% unique()          #define those countries we observe in the indeces 

countries_not_appear_indices <- as.data.frame(setdiff(countries_original_data, countries_indeces)) %>% #take (disjoint set between) countries found in original export dataset that dont appear in the indeces one (even though we eliminated some problematic countries, we may find some NA's for less problematic countries <- we will impute 0's instead)
  rename(iso = `setdiff(countries_original_data, countries_indeces)`) %>% 
  mutate(index_health = 0, index_stringency = 0, index_government = 0, index_economy = 0) 

countries_not_appear_indices <- countries_not_appear_indices[rep(seq_len(nrow(countries_not_appear_indices)), each = 12), ] %>%
  group_by(iso) %>%
  mutate(month = row_number(), .after = "iso")  # generate variable month and choose position after iso

indeces <- rbind(indeces, countries_not_appear_indices)

indeces_19 <- left_join(merge_data, indeces, by = c("iso", "month"))      #generates some NA's because original data reports some countries like Curasao (CUW), which indeces data doesnt <- now we have imputed fake 0's (wont create NA)
indeces_19 <- indeces_19[complete.cases(indeces_19),]
indeces_19 <- indeces_19 %>% 
  mutate(index_health = index_health*weight_fob,
         index_economy = index_economy*weight_fob,
         index_government = index_government*weight_fob,
         index_stringency = index_stringency*weight_fob) %>%
  rename(index_health_w = index_health,
         index_economy_w = index_economy,
         index_government_w = index_government,
         index_stringency_w = index_stringency) %>%
  select(-weight_fob)



#Add also the dummies of indeces at EXPORT destination
index_health_export_weight_dummies <- indeces_19 %>%   #dummies for health index at export destination
  select(id, iso, month, index_health_w) %>%
  rename(iso_health = iso)
index_health_export_weight_dummies <- dummyfier_weight_index_export(data=index_health_export_weight_dummies, period=month, variable_name = iso_health, variable_value = index_health_w)

index_stringency_export_weight_dummies <- indeces_19 %>%  #dummies for stringency index at export destination
  select(id, iso, month, index_stringency_w) %>%
  rename(iso_stringency = iso)
index_stringency_export_weight_dummies <- dummyfier_weight_index_export(data=index_stringency_export_weight_dummies, period=month, variable_name = iso_stringency, variable_value = index_stringency_w)

index_government_export_weight_dummies <- indeces_19 %>%  #dummies for government index at export destination
  select(id, iso, month, index_government_w) %>%
  rename(iso_government = iso)
index_government_export_weight_dummies <- dummyfier_weight_index_export(data=index_government_export_weight_dummies, period=month, variable_name = iso_government, variable_value = index_government_w)

index_economy_export_weight_dummies <- indeces_19 %>%  #dummies for economy index at export destination
  select(id, iso, month, index_economy_w) %>%
  rename(iso_economy = iso)
index_economy_export_weight_dummies <- dummyfier_weight_index_export(data=index_economy_export_weight_dummies, period=month, variable_name = iso_economy, variable_value = index_economy_w)


indeces_19 <- indeces_19 %>% group_by(id, month) %>%
  summarise(index_health_w = mean(index_health_w, na.rm = T),
            index_economy_w = mean(index_economy_w, na.rm = T),
            index_government_w = mean(index_government_w, na.rm = T),
            index_stringency_w = mean(index_stringency_w, na.rm = T))
  


rm(merge_data)


# Indeces according to the IMPORT country
#2019: Assign indeces of COVID to 2019 dataset (according to the IMPORT destination country)
merge_data <- import %>%
  filter(iso_import != "",
         value_fob > 0) %>%
  distinct() %>%
  mutate(year = period %/% 100,        # Extract year and month from the period
         month = period %% 100) %>%
  filter(year == 19) %>%
  group_by(id, iso_import, month) %>%
  summarise(value_fob = sum(value_fob, na.rm = T)) %>% 
  group_by(id, month) %>%
  mutate(total_fob = sum(value_fob, na.rm = T),
         weight_fob = value_fob/total_fob) %>%
  select(-value_fob,-total_fob) %>%
  rename(iso = iso_import) %>%
  as.data.frame() 


indeces_import_19 <- left_join(merge_data, indeces, by = c("iso", "month"))    #generates some NA's because some import countries are not found in the indeces data
indeces_import_19 <- indeces_import_19[complete.cases(indeces_import_19),]     #we dont care about those countries that import but dont export

indeces_import_19 <- indeces_import_19 %>% 
  mutate(index_health = index_health*weight_fob,
         index_economy = index_economy*weight_fob,
         index_government = index_government*weight_fob,
         index_stringency = index_stringency*weight_fob) %>%
  rename(index_health_w_import = index_health,
         index_economy_w_import = index_economy,
         index_government_w_import = index_government,
         index_stringency_w_import = index_stringency) %>%
  select(-weight_fob)
#Add also the dummies of indeces at IMPORT destination
index_health_import_weight_dummies <- indeces_import_19 %>%   #dummies for health index at import destination
  select(id, iso, month, index_health_w_import) %>%
  rename(iso_health = iso)
index_health_import_weight_dummies <- dummyfier_weight_index_import(data=index_health_import_weight_dummies, period=month, variable_name = iso_health, variable_value = index_health_w_import)

index_stringency_import_weight_dummies <- indeces_import_19 %>%  #dummies for stringency index at import destination
  select(id, iso, month, index_stringency_w_import) %>%
  rename(iso_stringency = iso)
index_stringency_import_weight_dummies <- dummyfier_weight_index_import(data=index_stringency_import_weight_dummies, period=month, variable_name = iso_stringency, variable_value = index_stringency_w_import)

index_government_import_weight_dummies <- indeces_import_19 %>%  #dummies for government index at import destination
  select(id, iso, month, index_government_w_import) %>%
  rename(iso_government = iso)
index_government_import_weight_dummies <- dummyfier_weight_index_import(data=index_government_import_weight_dummies, period=month, variable_name = iso_government, variable_value = index_government_w_import)

index_economy_import_weight_dummies <- indeces_import_19 %>%  #dummies for economy index at import destination
  select(id, iso, month, index_economy_w_import) %>%
  rename(iso_economy = iso)
index_economy_import_weight_dummies <- dummyfier_weight_index_import(data=index_economy_import_weight_dummies, period=month, variable_name = iso_economy, variable_value = index_economy_w_import)


indeces_import_19 <- indeces_import_19 %>% group_by(id, month) %>%
  summarise(index_health_w_import = mean(index_health_w_import, na.rm = T),
            index_economy_w_import = mean(index_economy_w_import, na.rm = T),
            index_government_w_import = mean(index_government_w_import, na.rm = T),
            index_stringency_w_import = mean(index_stringency_w_import, na.rm = T))



rm(merge_data, indeces)

#14)  classification industry (based on sectors) 
industry.dummies <- dummyfier(data, industry)
industry.dummies <- year_month_function(industry.dummies) 

#15) Import value by month
import.value <- import %>%
  group_by(id, period, year, month) %>%
  summarise(import_value = sum(value_fob, na.rm = T)) %>%
  mutate(lnX_import = log(import_value)) %>%
  select(-import_value) %>%
  filter(lnX_import >= 0)


#16) Experience at destination (accumulated): Accumulate total value of exports at destination during last 12 months (proxy for experience at EXPORT destination)
library(lubridate) # for ymd() function
library(purrr)
experience_export_value <- data %>% 
  select(id, iso, period, year, month, value_fob)  %>%
  group_by(id, iso, period) %>%
  summarise(value_fob = sum(value_fob, na.rm = T))
experience_export_value <- experience_export_value %>%
  mutate(yrmd = ymd(paste0(period, "01"))) %>% #transforms period into date format
  group_by(id, iso) %>%
  mutate(acc_value_last_12_months = purrr::map_dbl(yrmd, 
                                                   ~sum(value_fob[yrmd > (.x - months(13)) & yrmd <= (.x-1)]))) %>% #include one more month
  group_by(id, period) %>%
  mutate(share_value_last_12_months = acc_value_last_12_months/sum(acc_value_last_12_months, na.rm = T))
experience_export_value[is.na(experience_export_value)] <- 0    # NA's generated for those observations without previous experience in a given month

exper.export.12.dummies <- dummyfier_weight_exper(experience_export_value, period, iso, share_value_last_12_months)  #dummify info at id-destination-period level

experience_export_value <- experience_export_value %>%
  group_by(id, period) %>%
  summarise(exper_last_12_month_export = mean(acc_value_last_12_months, na.rm = T)) %>%
  mutate(ln_exper_12_months_export = log(exper_last_12_month_export + 1))    #if value is different from 0, transform value into log(), otherwise 0
                                                                    #convert negative log(values) into 0's (original value is so small that approximates 0)



#17) 
# Accumulate total value of exports at destination during last 12 months (proxy for experience at IMPORT destination)
experience_import_value <- import %>% 
  select(id, iso_import, period, year, month, value_fob)  %>%
  group_by(id, iso_import, period) %>%
  summarise(value_fob = sum(value_fob, na.rm = T))
experience_import_value <- experience_import_value %>%
  mutate(yrmd = ymd(paste0(period, "01"))) %>% #transforms period into date format
  group_by(id, iso_import) %>%
  mutate(acc_value_last_12_months = purrr::map_dbl(yrmd, 
                                                   ~sum(value_fob[yrmd > (.x - months(13)) & yrmd <= (.x-1)]))) %>% #include one more month
  group_by(id, period) %>%
  mutate(share_value_last_12_months = acc_value_last_12_months/sum(acc_value_last_12_months, na.rm = T))
experience_import_value[is.na(experience_import_value)] <- 0    # NA's generated for those observations without previous experience in a given month

exper.import.12.dummies <- dummyfier_weight_exper(experience_import_value, period, iso_import, share_value_last_12_months)  #dummify info at id-destination-period level

experience_import_value <- experience_import_value %>%
  group_by(id, period) %>%
  summarise(exper_last_12_month_import = mean(acc_value_last_12_months, na.rm = T)) %>%
  mutate(ln_exper_12_months_import = log(exper_last_12_month_import + 1))    #if value is different from 0, transform value into log(), otherwise 0
#convert negative log(values) into 0's (original value is so small that approximates 0)






## merge monthly variables generated: (HH.k, HH.d, via, region, sectors, destinations, experience_destination, experience_sector)
# All years dataframe
monthly_variables <- merge(HH, via.dummies, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- merge(monthly_variables, region.dummies, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- merge(monthly_variables, sector.dummies, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- merge(monthly_variables, sector.dummies.exper, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- merge(monthly_variables, dest.dummies, by = c("id", "period", "year", "month"), all.x = T)
#monthly_variables <- merge(monthly_variables, dest.dummies.exper, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- merge(monthly_variables, continent.dummies, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- merge(monthly_variables, industry.dummies, by = c("id", "period", "year", "month"), all.x = T)
monthly_variables <- left_join(monthly_variables, import.dummies, by = c("id", "period", "year", "month"))  #it generates a lot of NA's (for those companies that doesnt import)
monthly_variables <- left_join(monthly_variables, import.value, by = c("id", "period", "year", "month"))
monthly_variables <- left_join(monthly_variables, exper.export.12.dummies, by = c("id", "period"))
monthly_variables <- left_join(monthly_variables, exper.import.12.dummies, by = c("id", "period"))
monthly_variables <- left_join(monthly_variables, experience_export_value, by = c("id", "period"))
monthly_variables <- left_join(monthly_variables, experience_import_value, by = c("id", "period"))


#aa <- monthly_variables[complete.cases(monthly_variables),]
#sapply(monthly_variables, function(x) sum(is.na(x)))
monthly_variables[is.na(monthly_variables)] <- 0  # 0's to fill NA's generated by exporting companies not importing this month


#year2017
monthly_variables_17 <- select_year(monthly_variables, 17)
#year2018
monthly_variables_18 <- select_year(monthly_variables, 18)
#year2019
monthly_variables_19 <- select_year(monthly_variables, 19)
monthly_variables_19 <- merge(monthly_variables_19, indeces_19, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at EXPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_health_export_weight_dummies , by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at EXPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_stringency_export_weight_dummies, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at EXPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_government_export_weight_dummies, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at EXPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_economy_export_weight_dummies, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at EXPORT COUNTRY level

monthly_variables_19 <- merge(monthly_variables_19, indeces_import_19, by = c("id", "month"), all.x = T)    #add COVID info to 2019 firms at IMPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_health_import_weight_dummies , by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at IMPORT COUNTRY level


monthly_variables_19 <- merge(monthly_variables_19, index_stringency_import_weight_dummies, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at IMPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_government_import_weight_dummies, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at IMPORT COUNTRY level
monthly_variables_19 <- merge(monthly_variables_19, index_economy_import_weight_dummies, by = c("id", "month"), all.x = T)   #add COVID info to 2019 firms at IMPORT COUNTRY level


monthly_variables_19[is.na(monthly_variables_19)] <- 0  # 0's to fill NA's generated by companies that dont import this month

#year2020
monthly_variables_20 <- select_year(monthly_variables, 20)


rm(list = "HH", "via.dummies", "region.dummies", "sector.dummies", "sector.dummies.exper", "dest.dummies", "dest.dummies.exper", "continent.dummies", "industry.dummies", "import.dummies", 
   "index", "index_health_export_weight_dummies", "index_stringency_export_weight_dummies", "index_government_export_weight_dummies", "index_economy_export_weight_dummies", 
   "index_health_import_weight_dummies", "index_stringency_import_weight_dummies", "index_government_import_weight_dummies", "index_economy_import_weight_dummies") 
rm(list = "indeces_19", "indeces_import_19") 




## MONTHLY exports pdf, to define FIRM SIZE (correr solo una vez) ----------------------------------
#year2017
size.x.17 <- data_17 %>% 
  group_by(id, month) %>%
  mutate(years_this_month = n_distinct(year)) %>%
  group_by(id, month, years_this_month) %>%
  summarize(Xtot=sum(value_fob, na.rm=T)) %>%
  mutate(Xtot_avg=Xtot/years_this_month,
         lnX=log(Xtot_avg)) %>%
  filter(lnX>0) %>% 
  as.data.frame
## even if the PDF is not gaussian, it has a central tendency 
hist(size.x.17$lnX, 40)
## Assign a size class to each firm
size.q <- quantile(size.x.17$lnX, seq(0, 1, 0.25), na.rm = TRUE)
size.q[1] <- 0.99*size.q[1] ## Make the lower bound slightly smaller so the smallest firm is classified
size.q[5] <- 1.01*size.q[5] ## Make the upper bound slightly larger so the largest firm is classified
size.x.17 <- mutate(size.x.17, size = cut(lnX, size.q))
size.x.17$years_this_month <- NULL
size.x.17$Xtot <- NULL
#year2018
size.x.18 <- data_18 %>% 
  group_by(id, month) %>%
  mutate(years_this_month = n_distinct(year)) %>%
  group_by(id, month, years_this_month) %>%
  summarize(Xtot=sum(value_fob, na.rm=T)) %>%
  mutate(Xtot_avg=Xtot/years_this_month,
         lnX=log(Xtot_avg)) %>%
  filter(lnX>0) %>% 
  as.data.frame
## even if the PDF is not gaussian, it has a central tendency 
hist(size.x.18$lnX, 40)
## Assign a size class to each firm
size.q <- quantile(size.x.18$lnX, seq(0, 1, 0.25), na.rm = TRUE)
size.q[1] <- 0.99*size.q[1] ## Make the lower bound slightly smaller so the smallest firm is classified
size.q[5] <- 1.01*size.q[5] ## Make the upper bound slightly larger so the largest firm is classified
size.x.18 <- mutate(size.x.18, size = cut(lnX, size.q))
size.x.18$years_this_month <- NULL
size.x.18$Xtot <- NULL
#year2019
size.x.19 <- data_19 %>% 
  group_by(id, month) %>%
  mutate(years_this_month = n_distinct(year)) %>%
  group_by(id, month, years_this_month) %>%
  summarize(Xtot=sum(value_fob, na.rm=T)) %>%
  mutate(Xtot_avg=Xtot/years_this_month,
         lnX=log(Xtot_avg)) %>%
  filter(lnX>0) %>% 
  as.data.frame
## even if the PDF is not gaussian, it has a central tendency 
hist(size.x.19$lnX, 40)
## Assign a size class to each firm
size.q <- quantile(size.x.19$lnX, seq(0, 1, 0.25), na.rm = TRUE)
size.q[1] <- 0.99*size.q[1] ## Make the lower bound slightly smaller so the smallest firm is classified
size.q[5] <- 1.01*size.q[5] ## Make the upper bound slightly larger so the largest firm is classified
size.x.19 <- mutate(size.x.19, size = cut(lnX, size.q))
size.x.19$years_this_month <- NULL
size.x.19$Xtot <- NULL
#year2020
size.x.20 <- data_20 %>% 
  group_by(id, month) %>%
  mutate(years_this_month = n_distinct(year)) %>%
  group_by(id, month, years_this_month) %>%
  summarize(Xtot=sum(value_fob, na.rm=T)) %>%
  mutate(Xtot_avg=Xtot/years_this_month,
         lnX=log(Xtot_avg)) %>%
  filter(lnX>0) %>% 
  as.data.frame
## even if the PDF is not gaussian, it has a central tendency 
hist(size.x.20$lnX, 40)
## Assign a size class to each firm
size.q <- quantile(size.x.20$lnX, seq(0, 1, 0.25), na.rm = TRUE)
size.q[1] <- 0.99*size.q[1] ## Make the lower bound slightly smaller so the smallest firm is classified
size.q[5] <- 1.01*size.q[5] ## Make the upper bound slightly larger so the largest firm is classified
size.x.20 <- mutate(size.x.20, size = cut(lnX, size.q))
size.x.20$years_this_month <- NULL
size.x.20$Xtot <- NULL
## Data now classify firms into four size intervals


# Create final data  ---------------------------------------------------------
final_data_17 <- merge(monthly_variables_17, size.x.17, by = c("id", "month"), all.x = T)
final_data_18 <- merge(monthly_variables_18, size.x.18, by = c("id", "month"), all.x = T)
final_data_19 <- merge(monthly_variables_19, size.x.19, by = c("id", "month"), all.x = T)
final_data_20 <- merge(monthly_variables_20, size.x.20, by = c("id", "month"), all.x = T)
rm("monthly_variables", "monthly_variables_18", "monthly_variables_19", "monthly_variables_20","size.x.17", "size.x.18", "size.x.19", "size.x.20", "size.q")



# Remove NA's generated from merging
#colnames(october)[colSums(is.na(october)) > 0]  #which column do have NA's
#a <- rowSums(is.na(october))
colnames(final_data_19)[colSums(is.na(final_data_19)) > 0]  #which column do have NA's
colnames(final_data_20)[colSums(is.na(final_data_20)) > 0]  #which column do have NA's


#na_18 <- size.x.18[rowSums(is.na(size.x.18)) > 0,]  %>% #NA's by rows 
#  select(id, month, lnX, Xtot_avg, size)
#
#na_19 <- final_data_19[rowSums(is.na(final_data_19)) > 0,]  %>% #NA's by rows 
#  select(id, month, lnX, Xtot_avg, size, index_health, index_stringency, index_health_w_import, index_stringency_w_import)

final_data_17 <- final_data_17[complete.cases(final_data_17),]  #clean NA's
final_data_18 <- final_data_18[complete.cases(final_data_18),]  #clean NA's
final_data_19 <- final_data_19[complete.cases(final_data_19),]  #clean NA's   <- we lose so much info that eliminates all observations from October (because import info for 2019/10 is missing)
final_data_20 <- final_data_20[complete.cases(final_data_20),]  #clean NA's

# Create OUTCOME variable ("export_future") for set of companies in 2018 and 2019 (takes value 1 if a company exports the same month next year and 0 otherwise)
companies_18 <- data_18 %>% select(id, month) %>% distinct() %>% mutate(export_future = 1)
companies_19 <- data_19 %>% select(id, month) %>% distinct() %>% mutate(export_future = 1)
companies_20 <- data_20 %>% select(id, month) %>% distinct() %>% mutate(export_future = 1)
    #year 2017
final_data_17 <- left_join(final_data_17, companies_18, by = c("id", "month")) %>%
  select(id, year, month, export_future, everything())
final_data_17$export_future[is.na(final_data_17$export_future)] <- 0
    #year 2018
final_data_18 <- left_join(final_data_18, companies_19, by = c("id", "month")) %>%
  select(id, year, month, export_future, everything())
final_data_18$export_future[is.na(final_data_18$export_future)] <- 0
    #year 2019
final_data_19 <- left_join(final_data_19, companies_20, by = c("id", "month")) %>%
  select(id, year, month, export_future, everything())
final_data_19$export_future[is.na(final_data_19$export_future)] <- 0

rm(companies_18, companies_19, companies_20)
## Convert binary variables "iso_*", "via_*", "sector_*", "region_*", "experience_dest_*", "experience_sector_*" y "continent_*" into factors
#year2017
names1 <- final_data_17[,grep("iso", names(final_data_17), value=TRUE)]
names1 <- colnames(names1)
names2 <- final_data_17[,grep("via", names(final_data_17), value=TRUE)]
names2 <- colnames(names2)
names3 <- final_data_17[,grep("sector", names(final_data_17), value=TRUE)]
names3 <- colnames(names3)
names4 <- final_data_17[,grep("region", names(final_data_17), value=TRUE)]
names4 <- colnames(names4)
names5 <- final_data_17[,grep("experience_dest", names(final_data_17), value=TRUE)]
names5 <- colnames(names5)
names6 <- final_data_17[,grep("experience_sector", names(final_data_17), value=TRUE)]
names6 <- colnames(names6)
names7 <- final_data_17[,grep("continent", names(final_data_17), value=TRUE)]
names7 <- colnames(names7)
names8 <- final_data_17[,grep("industry", names(final_data_17), value=TRUE)]
names8 <- colnames(names8)
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names1, funs(as.factor(.))) 
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names2, funs(as.factor(.))) 
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names3, funs(as.factor(.))) 
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names4, funs(as.factor(.))) 
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names5, funs(as.factor(.)))
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names6, funs(as.factor(.)))
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names7, funs(as.factor(.)))
final_data_17 <-  final_data_17 %>%   
  mutate_if(names(.) %in% names8, funs(as.factor(.)))

final_data_17$sector_mode <-  NULL     # Delete this variable because "industry" groups better the info

final_data_17 <- final_data_17 %>% mutate(id= as.factor(id),
                                          month = as.factor(month),
                                          export_future = as.factor(export_future))
#year2018
names1 <- final_data_18[,grep("iso", names(final_data_18), value=TRUE)]
names1 <- colnames(names1)
names2 <- final_data_18[,grep("via", names(final_data_18), value=TRUE)]
names2 <- colnames(names2)
names3 <- final_data_18[,grep("sector", names(final_data_18), value=TRUE)]
names3 <- colnames(names3)
names4 <- final_data_18[,grep("region", names(final_data_18), value=TRUE)]
names4 <- colnames(names4)
names5 <- final_data_18[,grep("experience_dest", names(final_data_18), value=TRUE)]
names5 <- colnames(names5)
names6 <- final_data_18[,grep("experience_sector", names(final_data_18), value=TRUE)]
names6 <- colnames(names6)
names7 <- final_data_18[,grep("continent", names(final_data_18), value=TRUE)]
names7 <- colnames(names7)
names8 <- final_data_18[,grep("industry", names(final_data_18), value=TRUE)]
names8 <- colnames(names8)
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names1, funs(as.factor(.))) 
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names2, funs(as.factor(.))) 
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names3, funs(as.factor(.))) 
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names4, funs(as.factor(.))) 
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names5, funs(as.factor(.)))
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names6, funs(as.factor(.)))
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names7, funs(as.factor(.)))
final_data_18 <-  final_data_18 %>%   
  mutate_if(names(.) %in% names8, funs(as.factor(.)))

final_data_18$sector_mode <-  NULL     # Delete this variable because "industry" groups better the info

final_data_18 <- final_data_18 %>% mutate(id= as.factor(id),
                                          month = as.factor(month),
                                          export_future = as.factor(export_future))
#year2019
names1 <- final_data_19[,grep("iso", names(final_data_19), value=TRUE)]
names1 <- colnames(names1)
names2 <- final_data_19[,grep("via", names(final_data_19), value=TRUE)]
names2 <- colnames(names2)
names3 <- final_data_19[,grep("sector", names(final_data_19), value=TRUE)]
names3 <- colnames(names3)
names4 <- final_data_19[,grep("region", names(final_data_19), value=TRUE)]
names4 <- colnames(names4)
names5 <- final_data_19[,grep("experience_dest", names(final_data_19), value=TRUE)]
names5 <- colnames(names5)
names6 <- final_data_19[,grep("experience_sector", names(final_data_19), value=TRUE)]
names6 <- colnames(names6)
names7 <- final_data_19[,grep("continent", names(final_data_19), value=TRUE)]
names7 <- colnames(names7)
names8 <- final_data_19[,grep("industry", names(final_data_19), value=TRUE)]
names8 <- colnames(names8)
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names1, funs(as.factor(.))) 
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names2, funs(as.factor(.))) 
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names3, funs(as.factor(.))) 
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names4, funs(as.factor(.))) 
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names5, funs(as.factor(.)))
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names6, funs(as.factor(.)))
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names7, funs(as.factor(.)))
final_data_19 <-  final_data_19 %>%   
  mutate_if(names(.) %in% names8, funs(as.factor(.)))

final_data_19$sector_mode <-  NULL     # Delete this variable because "industry" groups better the info

final_data_19 <- final_data_19 %>% mutate(id= as.factor(id),
                                          month = as.factor(month),
                                          export_future = as.factor(export_future))
#year2020
names1 <- final_data_20[,grep("iso", names(final_data_20), value=TRUE)]
names1 <- colnames(names1)
names2 <- final_data_20[,grep("via", names(final_data_20), value=TRUE)]
names2 <- colnames(names2)
names3 <- final_data_20[,grep("sector", names(final_data_20), value=TRUE)]
names3 <- colnames(names3)
names4 <- final_data_20[,grep("region", names(final_data_20), value=TRUE)]
names4 <- colnames(names4)
names5 <- final_data_20[,grep("experience_dest", names(final_data_20), value=TRUE)]
names5 <- colnames(names5)
names6 <- final_data_20[,grep("experience_sector", names(final_data_20), value=TRUE)]
names6 <- colnames(names6)
names7 <- final_data_20[,grep("continent", names(final_data_20), value=TRUE)]
names7 <- colnames(names7)
names8 <- final_data_20[,grep("industry", names(final_data_20), value=TRUE)]
names8 <- colnames(names8)
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names1, funs(as.factor(.))) 
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names2, funs(as.factor(.))) 
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names3, funs(as.factor(.))) 
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names4, funs(as.factor(.))) 
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names5, funs(as.factor(.)))
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names6, funs(as.factor(.)))
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names7, funs(as.factor(.)))
final_data_20 <-  final_data_20 %>%   
  mutate_if(names(.) %in% names8, funs(as.factor(.)))

final_data_20$sector_mode <-  NULL     # Delete this variable because "industry" groups better the info

final_data_20 <- final_data_20 %>% mutate(id= as.factor(id),
                                          month = as.factor(month))


rm(data_18,data_19,data_20)
rm(names1, names2, names3, names4, names5, names6, names7, names8)



# standarize the factor levels of size (from number intervals to names)
final_data_17$period <- NULL
final_data_18$period <- NULL
final_data_19$period <- NULL
final_data_20$period <- NULL
levels(final_data_17$size) <- c("Q1","Q2","Q3","Q4")
levels(final_data_18$size) <- c("Q1","Q2","Q3","Q4")
levels(final_data_19$size) <- c("Q1","Q2","Q3","Q4")
levels(final_data_20$size) <- c("Q1","Q2","Q3","Q4")

names(final_data_17) <- make.names(names(final_data_17)) #convert ilegal names (for R) into legal ones
names(final_data_18) <- make.names(names(final_data_18)) #convert ilegal names (for R) into legal ones
names(final_data_19) <- make.names(names(final_data_19)) #convert ilegal names (for R) into legal ones
names(final_data_20) <- make.names(names(final_data_20)) #convert ilegal names (for R) into legal ones

# set working directory where we will save the data 
 save(final_data_17, file = "data_out/final_data_17.RData")
# load("data_out/final_data_17.Rdata")
 save(final_data_18, file = "data_out/final_data_18.RData")
# load("data_out/final_data_18.Rdata")
 save(final_data_19, file = "data_out/final_data_19.RData")
# load("data_out/final_data_19.Rdata")
 save(final_data_20, file = "data_out/final_data_20.RData")
# load("data_out/final_data_20.Rdata")
