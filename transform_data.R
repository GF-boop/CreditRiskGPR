# Run process_eurostat.R
# Run process_data.R

# ========= Chargement des données ========= #

library(tseries)
library(data.table)

df_macro <-  fread("data/processed/data_macro.csv")
colnames(df_macro)[colnames(df_macro) == "GDP"] <- "GDPGROWTH"


# ################################################################################# 
# ##### ==================== STATIO ==================== ########
# ################################################################################# 




# ========= RULES : ========= #

# Règles de stationnarisation :
# - GDP : si en taux (%), faire diff(x) ; si en niveau, faire diff(log(x)) → croissance du PIB.
# - INFLA : si en % (ex. inflation mensuelle), faire diff(x) → accélération de l'inflation ;
#           si c'est un indice (ex. CPI), faire diff(log(x)) → inflation.
# - UNEMP : taux de chômage en %, faire diff(x) → variation du chômage.
# - FCI_* : indices financiers, faire diff(x) ; log-diff possible si forte hétéroscédasticité.
# - EPU_world : indice toujours positif et volatile → faire diff(log(x)).

# On a du GPD, GDPGROWTH, INFLA (En niveau), UNEMP (en taux), FCI et EPU en niveau


stationarize <- function(x, method = c("difflog", "diff", "doublediff", "none")) {
  method <- match.arg(method)
  n <- length(x)
  
  if (method == "difflog") {
    if (any(x <= 0, na.rm = TRUE)) {
      warning("logdiff impossible : valeurs ≤ 0 détectées")
      return(rep(NA, n))
    }
    return(c(NA, diff(log(x))))
  } 
  
  if (method == "diff") {
    return(c(NA, diff(x)))
  }
  
  if (method == "doublediff") {
    return(c(NA, NA, diff(diff(x))))
  }
  if (method == "none") {
    return(x)
  }
  
  stop("Méthode inconnue")
}




# Transformation fixe sans prise en compte de la statio
get_fixed_transformation <- function(varname) {
  if (grepl("_GDPGROWTH$|^GDPGROWTH$|^Infla$", varname)) {
    return("none")  # taux : on ne transforme pas
  }
  if (grepl("_GDP$|_HCPI$|^EPU_world$", varname)) {
    return("difflog")  # variables en niveau strictement positif
  }
  if (grepl("^FCI_|_UNEMP$", varname)) {
    return("diff")  # indices financiers : diff simple
  }
  return(NA)  # transformation non définie
}



# Initialisation du nouveau data.frame avec la colonne Date
df_macro_transform <- data.frame(Date = df_macro$Date)

# Liste des colonnes à traiter (hors Date)
cols_to_transform <- setdiff(colnames(df_macro), "Date")

# Boucle sur chaque colonne
for (col in cols_to_transform) {
  method <- get_fixed_transformation(col)
  
  if (!is.na(method)) {
    suffix <- switch(method,
                     "difflog" = "_LD",
                     "diff" = "_D",
                     "doublediff" = "_DD",
                     "none" = "")
    
    new_colname <- paste0(col, suffix)
    
    # Appliquer la transformation et ajouter à df_macro_transform
    df_macro_transform[[new_colname]] <- tryCatch(
      stationarize(df_macro[[col]], method = method),
      error = function(e) {
        warning(paste("Erreur pour", col, ":", e$message))
        rep(NA, length(df_macro[[col]]))
      }
    )
  }
}



# Fonction fournie pour exécuter les tests ADF et KPSS
run_stationarity_tests <- function(var_name, ts_data) {
  cat(sprintf("Variable : %s\n", var_name))
  
  adf <- tryCatch(adf.test(ts_data, alternative = "stationary"),
                  error = function(e) list(p.value = NA, statistic = NA,
                                           method = paste("Error:", e$message)))
  kpss <- tryCatch(kpss.test(ts_data, null = "Level"),
                   error = function(e) list(p.value = NA, statistic = NA,
                                            method = paste("Error:", e$message)))
  
  if (!is.na(adf$p.value)) {
    cat(sprintf("  ADF:   Stat = %.3f, p = %.3f => %s\n", adf$statistic, adf$p.value,
                ifelse(adf$p.value < 0.05, "Stationnaire", "Non stationnaire")))
  } else {
    cat("  Erreur ADF:", adf$method, "\n")
  }
  
  if (!is.na(kpss$p.value)) {
    cat(sprintf("  KPSS: Stat = %.3f, p = %.3f => %s\n", kpss$statistic, kpss$p.value,
                ifelse(kpss$p.value < 0.05, "Non stationnaire", "Stationnaire")))
  } else {
    cat("  Erreur KPSS:", kpss$method, "\n")
  }
  
  cat("\n")
}
df_macro_transform <- na.omit(df_macro_transform)

df_statio <- copy(df_macro_transform)
col_non_statio <- c()
col_statio <- c()
# Boucle sur toutes les colonnes sauf "Date"
for (col in setdiff(colnames(df_statio), "Date")) {
  ts_data <- df_statio[[col]]
  
  # Tester seulement si la colonne est numérique et sans NA (ou très peu)
  if (is.numeric(ts_data) && sum(!is.na(ts_data)) > 20) {
    run_stationarity_tests(col, ts_data)
    adf <- adf.test(ts_data, alternative = "stationary")
    if (adf$p.value < 0.05) {col_statio = append(col_statio, col)}
    else                    {col_non_statio = append(col_non_statio, col)}
  } else {
    cat(sprintf("Variable : %s\n", col))
    cat("  Skipped (non numérique ou trop de NA)\n\n")
  }
}

fwrite(df_macro_transform, file = "data/processed/data_macro_transform.csv")

# ======= Stationarisation d'une série avec chaîne de méthodes ======= #

get_method_chain <- function(varname) {
  if (grepl("_GDP$", varname)) return(c("none", "difflog", "doublediff"))
  if (grepl("_GDPGROWTH$|^GDPGROWTH$", varname)) return(c("none", "diff", "doublediff"))
  if (grepl("_HCPI$", varname)) return(c("none", "difflog", "doublediff"))
  if (grepl("_UNEMP|^Infla$$", varname)) return(c("none", "diff", "doublediff"))
  if (grepl("^FCI_", varname)) return(c("none", "diff", "doublediff"))
  if (grepl("^EPU_world$", varname)) return(c("none", "difflog", "doublediff"))
  return(NULL)
}

stationarize_chain <- function(x, methods) {
  x <- x
  for (m in methods){
    x_statio <- stationarize(x, method = m)
    x_statio <- x_statio
    adf <- adf.test(na.omit(x_statio), alternative = "stationary")
    pval <- adf$p.value
    
    if (pval < 0.05){
      # la série est statio avec la méthode m, on sort de la fonction
      return(list(series = x_statio, method = m, fail = FALSE))
    }
  }
  # la série n'est pas statio peu importe la méthode utilisé
  return(list(series = x, fail = TRUE))
}

df_macro_statio <- data.frame(Date = df_macro$Date)


for (col in setdiff(names(df_macro), "Date")) {
  cat("\n =========  On essaye la colonne ", col, " ========= \n")
  method_chain <- get_method_chain(col)
  
  if (!is.null(method_chain)) {
    result <- stationarize_chain(df_macro[[col]], method_chain)
    
    method_used <- if (result$fail) "FAIL" else result$method
    suffix <- switch(method_used,
                     "none" = "",
                     "diff" = "_D",
                     "difflog" = "_LD",
                     "doublediff" = "_DD",
                     "FAIL" = "_FAIL")
    
    new_name <- paste0(col, suffix)
    df_macro_statio[[new_name]] <- result$series
    cat("Ajout de", new_name, "avec méthode", method_used, "\n")
  }
}




# Les séries sont normalement toutes stationnaires.
# Remarque : certaines séries ne le sont plus après avoir retiré quelques observations
# en tête, en raison des transformations (diff, double diff) appliquées à 
# d'autres variables.



df_macro_statio <- na.omit(df_macro_statio)
df_statio <- copy(df_macro_statio)
col_non_statio <- c()
col_statio <- c()
for (col in setdiff(colnames(df_statio), "Date")) {
  ts_data <- df_statio[[col]]
  
  # Tester seulement si la colonne est numérique et sans NA
  if (is.numeric(ts_data) && sum(!is.na(ts_data)) > 20) {
    run_stationarity_tests(col, ts_data)
    adf <- adf.test(ts_data, alternative = "stationary")
    if (adf$p.value < 0.05) {col_statio = append(col_statio, col)}
    else                    {col_non_statio = append(col_non_statio, col)}
  } else {
    cat(sprintf("Variable : %s\n", col))
    cat("  Skipped (non numérique ou trop de NA)\n\n")
  }
}


# for (col in col_non_statio){
#   plot(x = df_statio$Date, y = 
#          df_statio[["Finland_HCPI_LD"]], type = "l", title = col)
# }

fwrite(df_macro_statio, file = "data/processed/data_macro_statio.csv")
