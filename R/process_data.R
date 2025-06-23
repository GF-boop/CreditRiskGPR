# RUN process_eurostat.R 

# Chargement des packages 
library(data.table)
library(zoo)
library(lmtest)     # Test Durbin-Watson, Breusch-Pagan
library(car)        # VIF
library(tseries)    # Test Ljung-Box
library(mgcv)       # GAM
library(tseries)


# Chargement des fonctions
source("utils/my_functions.R")
PATH <- "data/raw/"

### === Traitement rapide === ###

# Lecture des données macro niveau Europe et monde
data_macro <- read.csv(paste0(PATH, "data_macro_estimation.csv"), sep = ";")
data_macro$Date <- as.Date(data_macro$Date, format = "%d/%m/%Y")
data_macro <- df_clean_numeric(data_macro, c("GDP", "Infla", "EPU_world",
                                             "FCI_house", "FCI_corpo",
                                             "FCI_total"))
data_macro <-  Date_num_format(data_macro, "Date")


# Lecture des données FCI par pays
data_macro_FCI <- read.csv(paste0(PATH, "data_macro_estimation_FCI.csv"),
                           sep = ";")
data_macro_FCI <- df_clean_numeric(data_macro_FCI, names(data_macro_FCI)[-1])
data_macro_FCI <- Date_num_format(data_macro_FCI, "Date")

# Lecture des données macro par pays (Eurostat)
data_macro_country <- read.csv2("data/processed/eurostat/cleaned_data_macro.csv")
data_macro_country <- setDT(data_macro_country)
data_macro_country_subset <- data_macro_country[Date %in% data_macro_FCI$Date]

data_macro_country <-  setDF(data_macro_country)
data_macro_country_subset <- setDF(data_macro_country_subset) 

# Merge des données macro country et FCI Country
data_country <- merge(data_macro_country_subset,data_macro_FCI,
                      by = "Date", all = TRUE)
fwrite(data_country,file="data/processed/data_macro_country.csv")
# Merge de toutes les données :
data_full <- merge(data_country, data_macro,
                   by = "Date", all = TRUE)
fwrite(data_full,file="data/processed/data_macro.csv")