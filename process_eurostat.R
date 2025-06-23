library(eurostat)
library(dplyr)
library(data.table)
library(tidyr)
library(zoo)

source("utils/my_functions.R")

# Choisir le dossier d'exportation
SAVE_PATH <- "data/processed/eurostat/"

# Chargement eurostat
toc <- get_eurostat_toc()

# Filtrer dans le TOC
toc_filt2 <- toc %>% filter(grepl("-", data.end))

# toc_filtq <- toc %>% filter(grepl("price", title) &
#                               !grepl("GDP",title) &
#                               !grepl("Gross",title) &
#                               grepl("quarter",title))
toc_filtq <- toc %>% filter(grepl("rate", title))


# ========= GDP ========= #
id <- "namq_10_gdp"
data_full <- setDT(get_eurostat(id, type = "label", time_format = "num",
                                cache = FALSE))

time_list <- sort(unique(data_full$TIME_PERIOD))
adj_list <- unique(data_full$s_adj)
geo_list <- unique(data_full$geo)

extract_gdp <- function(data, base_year, adj_type) {
  unit_str <- sprintf("Chain linked volumes (%s), million euro", base_year)
  df <- data[unit == unit_str &
               na_item == "Value added, gross" &
               s_adj == adj_type,
             .(Date = TIME_PERIOD, Country = geo, GDP = values)]
  
  # Transformation (pivot)
  df_wide <- dcast(df, Date ~ Country, value.var = "GDP")
  return(df_wide)
}

# Extraire et transformer les données en format wide
gdp <- list(
  cl2005_c_ns = extract_gdp(data_full, "2005", adj_list[1]),
  cl2005_nc_ns = extract_gdp(data_full, "2005", adj_list[2]),
  cl2005_nc_s = extract_gdp(data_full, "2005", adj_list[3]),
  cl2005_c_s = extract_gdp(data_full, "2005", adj_list[4]),
  cl2010_c_n = extract_gdp(data_full, "2010", adj_list[1]),
  cl2010_nc_n = extract_gdp(data_full, "2010", adj_list[2]),
  cl2010_nc_s = extract_gdp(data_full, "2010", adj_list[3]),
  cl2010_c_s = extract_gdp(data_full, "2010", adj_list[4])
)


for (index in 1:length(gdp)){
  write.csv2(gdp[index], 
             file = paste0(paste0(SAVE_PATH,"gdp/cleaned_"),
                           paste0(names(gdp)[index],".csv")),
             row.names = FALSE)
}


# ========= Infla ========= #
# # Enfaite ici c'étiat House Price Index...
# id <-  "ei_hppi_q" # ei_hppi_q, tipsho40
# data_full <- setDT(get_eurostat(id, type = "label", time_format = "num",
#                                 cache = FALSE))
# 
# unit_list <- unique(data_full$unit)
# 
# df_infla <- data_full[unit == unit_list[1],
#                       .(Date = TIME_PERIOD, Country = geo, CPI = values)]
# df_infla <- dcast(df_infla, Date ~ Country, value.var = "CPI")
# 
# write.csv2(df_infla, 
#            file = paste0(SAVE_PATH,
#                          "infla/cleaned_infla.csv"),
#            row.names = FALSE)

# ID de l'indice HICP
id <- "prc_hicp_midx"

# Téléchargement des données Eurostat
data_full <- setDT(get_eurostat(id, type = "label", time_format = "num", cache = FALSE))

# Détection de l’unité à conserver
unit_list <- unique(data_full$unit)
if (id == "prc_hicp_midx") {
  target_unit <- grep("Index", unit_list, value = TRUE)[1]
} else {
  target_unit <- unit_list[1]
}

# Préparation des données
df_infla <- data_full[unit == target_unit,
                      .(Date = as.Date(as.yearmon(TIME_PERIOD)), Country = geo, CPI = values)]

# Ajout colonne trimestre (zoo::as.yearqtr)
df_infla[, Quarter := as.yearqtr(Date)]

# Agrégation (moyenne trimestrielle par défaut)
df_infla_qtr <- df_infla[, .(CPI_qtr = mean(CPI, na.rm = TRUE)), by = .(Quarter, Country)]

# Conversion Quarter en format numérique 2020.25, 2020.5, etc.
df_infla_qtr[, Quarter_num := as.numeric(floor(Quarter)) + (cycle(Quarter) - 1) * 0.25]

# Passage en format large (1 colonne par pays)
df_wide_qtr <- dcast(df_infla_qtr, Quarter_num ~ Country, value.var = "CPI_qtr")

# Renommage de la colonne date
setnames(df_wide_qtr, "Quarter_num", "Date")

# Sauvegarde
write.csv2(df_wide_qtr,
           file = paste0(SAVE_PATH, "infla/cleaned_infla.csv"),
           row.names = FALSE)


# ========= Employ ========= #
id <-  "tipsun30"
data_full <- setDT(get_eurostat(id, type = "label", time_format = "num",
                                cache = FALSE))

df_unemp <- data_full[,.(Date = TIME_PERIOD,
                        Country = geo, Unemployment = values)]
df_unemp <- dcast(df_unemp, Date ~ Country, value.var = "Unemployment")

write.csv2(df_unemp, 
           file = paste0(SAVE_PATH,"employ/cleaned_unemp.csv"),
           row.names = FALSE)


# ========= Main refinancing rate ========= #

### Peut être les prendres ###

### Ou prendre sur Eurostat le "Money market" ###



# ========= Merge Data ========= #

# Calcul de la croissance du PIB avant le merge :
gdp_data <- gdp$cl2010_nc_n
growth_data <- compute_gdp_growth(gdp_data)

gdp_yoy <- growth_data$yoy
gdp_qoq <- growth_data$qoq

df_gdp          <- setDT(gdp_data)
df_gdp_growth   <- setDT(gdp_qoq)
df_HCPI        <- setDT(df_wide_qtr)
df_unemp        <- setDT(df_unemp)

common_col <- Reduce(intersect, list(names(df_gdp), names(df_gdp_growth),
                                     names(df_HCPI),names(df_unemp)))

df_gdp          <- df_gdp[, ..common_col]
df_gdp_growth   <- df_gdp_growth[, ..common_col]
df_HCPI        <- df_HCPI[, ..common_col]
df_unemp        <- df_unemp[, ..common_col]

setnames(df_gdp, setdiff(common_col, "Date"),
         paste0(setdiff(common_col, "Date"), "_GDP"))
setnames(df_gdp_growth, setdiff(common_col, "Date"),
         paste0(setdiff(common_col, "Date"), "_GDPGROWTH"))
setnames(df_HCPI, setdiff(common_col, "Date"),
         paste0(setdiff(common_col, "Date"), "_HCPI"))
setnames(df_unemp, setdiff(common_col, "Date"),
         paste0(setdiff(common_col, "Date"), "_UNEMP"))


df_macro <- merge(df_gdp, df_gdp_growth, by = "Date",
                  all = TRUE)
df_macro <- merge(df_macro, df_HCPI, by = "Date",
                  all = TRUE)

df_macro <- merge(df_macro, df_unemp, by = "Date",
                  all = TRUE)

write.csv2(df_macro, 
           file = paste0(SAVE_PATH,"cleaned_data_macro.csv"),
           row.names = FALSE)