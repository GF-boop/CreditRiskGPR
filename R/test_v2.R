# --- Préambule : Installation des packages et définition du répertoire ---

if (!require("rstudioapi")) install.packages("rstudioapi")
if (!require("dplyr")) install.packages("dplyr")
if (!require("parallel")) install.packages("parallel")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################################################
#
# DÉMARCHE D'ANALYSE
#
# Architecture en deux phases :
# 1. Calcul Parallèle : Chaque noeud calcule le meilleur modèle pour un pays et
#    renvoie un objet R contenant tous les résultats (modèle, données, etc.).
# 2. Écriture Séquentielle : Le processus principal génère tous les fichiers
#    (résumés, graphiques) à partir des objets reçus.
#
################################################################################

# --- Paramètres de modélisation statistique ---
P_VALUE_THRESHOLD <- 0.05
TRAIN_TEST_SPLIT_RATIO <- 0.8

# ... (Les fonctions f_Z_estimation et la préparation des données sont identiques) ...
f_Z_estimation <- function(vect_TD) {
  var_Z_rho <- function(rho) {
    if (rho <= 0 || rho >= 1) return(Inf)
    Z_estim <- (qnorm(mean(vect_TD)) - qnorm(vect_TD) * sqrt(1 - rho)) / sqrt(rho)
    return(var(Z_estim) - 1)
  }
  result <- tryCatch(uniroot(var_Z_rho, lower = 1e-6, upper = 1 - 1e-6), error = function(e) return(NULL))
  if(is.null(result)) return(list(Z = rep(NA, length(vect_TD)), rho = NA))
  rho_opt <- result$root
  Z_estim <- (qnorm(mean(vect_TD)) - qnorm(vect_TD) * sqrt(1 - rho_opt)) / sqrt(rho_opt)
  return(list(Z = Z_estim, rho = rho_opt))
}
data_risk <- read.csv("../data/data_risk_EBA.csv", header = TRUE, sep = ";")
data_risk <- data_risk[data_risk$Date != "31/12/2024", ]
countries <- unique(data_risk$Country)
Z_results <- lapply(countries, function(c) f_Z_estimation(data_risk$Corpo_DR_WA[data_risk$Country == c]))
Z_country <- sapply(Z_results, `[[`, "Z"); colnames(Z_country) <- countries
data_macro <- read.csv("../data/data_macro_statio.csv", header = TRUE, sep = ",")

year <- floor(data_macro$Date)
fraction <- data_macro$Date %% 1

final_month <- integer(length(year))
final_year <- integer(length(year))

for (i in seq_along(data_macro$Date)) {
  if (fraction[i] == 0.25) {
    final_month[i] <- 3
    final_year[i] <- year[i]
  } else if (fraction[i] == 0.50) {
    final_month[i] <- 6
    final_year[i] <- year[i]
  } else if (fraction[i] == 0.75) {
    final_month[i] <- 9
    final_year[i] <- year[i]
  } else if (fraction[i] == 0.00) {
    final_month[i] <- 12
    final_year[i] <- year[i] - 1 # Use the previous year
  }
}

data_macro$Date <- as.Date(paste(final_year, final_month, "01", sep="-"))

nb_lags <- 4 # The maximum number of lags you want to create.
lag_indices <- 0:nb_lags # This will create lags from 0 (the current value) up to 6.

# 2. Automatically Identify All Covariate Columns to Lag
# We take all column names from your source data frame 'data_macro' and exclude the 'Date' column.
# This is the key change that makes the code adaptable.
vars_to_lag <- setdiff(names(data_macro), "Date")

# Safety check in case the input is not as expected
if (length(vars_to_lag) == 0) {
  stop("No covariate columns found in 'data_macro' to create lags for.")
}

# 3. Create a List of Lagged Variables
# We loop through each variable name identified above.
# For each variable, we create all the required lags (0 through 6).
# Note: Ensure the 'dplyr' package is loaded for the lag() function.
library(dplyr)
lagged_vars_list <- lapply(vars_to_lag, function(var_name) {
  # For each variable, create a matrix where columns are the lagged versions
  sapply(lag_indices, function(k) {
    # Use dplyr's lag function on the column from the original dataframe
    dplyr::lag(data_macro[[var_name]], n = k)
  })
})

# 4. Combine all Lagged Variables into a Single Data Frame
# The list 'lagged_vars_list' is efficiently combined column-wise into a new dataframe.
data_lags <- as.data.frame(do.call(cbind, lagged_vars_list))

# 5. Assign Clear and Consistent Names to the New Columns
# This creates names like "VarName_lag0", "VarName_lag1", etc. for every variable.
names(data_lags) <- paste0(
  rep(vars_to_lag, each = length(lag_indices)), # Repeat each original variable name (nb_lags + 1) times
  "_lag",                                       # Add the suffix
  lag_indices                                   # Append the lag number (0, 1, 2, ... 6)
)

# 6. Add the 'Date' Column back for Future Joins
# This is crucial for correctly merging with your dependent variable later on.
data_lags$Date <- data_macro$Date

country_map <- list(
  "Austria" = c("Austria"),
  "Belgium" = c("Belgium", "_BE_"),
  "Bulgaria" = c("Bulgaria"),
  "Croatia" = c("Croatia"),
  "Cyprus" = c("Cyprus"),
  "Czechia" = c("Czechia"),
  "Denmark" = c("Denmark"),
  "Estonia" = c("Estonia"),
  "Finland" = c("Finland"),
  "France" = c("France", "_FR_"),
  "Germany" = c("Germany", "_GE_"),
  "Greece" = c("Greece"),
  "Hungary" = c("Hungary"),
  "Ireland" = c("Ireland"),
  "Italy" = c("Italy", "_IT_"),
  "Latvia" = c("Latvia"),
  "Lithuania" = c("Lithuania"),
  "Luxembourg" = c("Luxembourg", "_LU_"),
  "Malta" = c("Malta"),
  "Netherlands" = c("Netherlands", "_NT_"),
  "Poland" = c("Poland"),
  "Portugal" = c("Portugal"),
  "Romania" = c("Romania"),
  "Slovakia" = c("Slovakia"),
  "Slovenia" = c("Slovenia"),
  "Spain" = c("Spain", "_SP_"),
  "Sweden" = c("Sweden")
)

#########################################################################
# ---      FONCTION DE CALCUL POUR UN PAYS                          --- #
#########################################################################
# It's good practice to pass the map as an argument to the function
compute_model_for_country <- function(country_to_model, country_map_data) {
  
  tryCatch({
    # Ensure necessary libraries are loaded
    if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
    if (!requireNamespace("lubridate", quietly = TRUE)) stop("Package 'lubridate' is required.")
    library(dplyr)
    library(lubridate)
    
    
    # --- Data Preparation (your existing code) ---
    # NOTE: The data frames 'data_risk', 'Z_country', and 'data_lags' 
    # must be available in the global environment or passed as arguments.
    
    dates_risk <- unique(as.Date(data_risk$Date, tryFormats = c("%d/%m/%Y")))
    Y_df <- data.frame(Date = dates_risk, Y = Z_country[, country_to_model])
    if(all(is.na(Y_df$Y))) stop("Calculation of the Z factor is impossible (all NA).")
    
    Y_df$Date <- floor_date(Y_df$Date, unit = "month")
    data_lags$Date <- floor_date(data_lags$Date, unit = "month")
    
    model_df <- dplyr::inner_join(Y_df, data_lags, by = "Date") %>% na.omit()
    if(nrow(model_df) < 20) stop("Not enough valid data points after merging and removing NAs.")
    
    # --- Train/Test Split (your existing code) ---
    split_point <- floor(nrow(model_df) * TRAIN_TEST_SPLIT_RATIO)
    train_df <- model_df[1:split_point, ]
    test_df <- model_df[(split_point + 1):nrow(model_df), ]
    
    # ==============================================================================
    # --- Dynamic Model Selection ---
    # ==============================================================================
    
    # 1. Get all available lagged variable names from the modeling data frame.
    all_lagged_vars <- names(model_df)[grep("_lag[0-9]+$", names(model_df))]
    
    # 2. Get the specific identifiers for the country being modeled from the map.
    if (!country_to_model %in% names(country_map_data)) {
      stop(paste("Country '", country_to_model, "' not found in the provided country_map."))
    }
    country_identifiers <- country_map_data[[country_to_model]]
    
    # 3. Define the global/regional identifiers that should always be considered as predictors.
    # Using anchors (^) for generic names prevents partially matching other variable names.
    global_identifiers <- c("EPU_world", "FCI_total_EU", "^GDPGROWTH", "^Infla_DD", "^FCI_total$")
    
    # 4. Combine country-specific and global identifiers into a single search pattern.
    selection_regex <- paste(c(country_identifiers, global_identifiers), collapse = "|")
    
    # 5. Filter the list of all lagged variables to get our final set of explanatory variables (X_names).
    # This single step now correctly selects all relevant variables.
    X_names <- grep(selection_regex, all_lagged_vars, value = TRUE)
    
    if (length(X_names) == 0) {
      stop("No explanatory variables found for this country after filtering.")
    }
    
    # ==============================================================================
    # --- Model Fitting and Evaluation (your existing code) ---
    # ==============================================================================
    
    combs <- unlist(lapply(1:min(4, length(X_names)), function(k) combn(X_names, k, simplify = FALSE)), recursive = FALSE)
  
  
  model_results <- lapply(combs, function(vars) {
    formula <- as.formula(paste("Y ~", paste(vars, collapse = " + ")))
    model <- lm(formula, data = train_df)
    p_values <- summary(model)$coefficients[-1, "Pr(>|t|)"]
    if (any(is.na(p_values)) || any(p_values > P_VALUE_THRESHOLD)) return(NULL) # Added NA check
    list(model = model, vars = vars, aic = AIC(model))
  })
  
  valid_models <- Filter(Negate(is.null), model_results)
  valid_models <- Filter(Negate(is.null), model_results)
  
  if (length(valid_models) == 0) {
    return(list(
      country = country_to_model,
      status = "No Model Found", # Changé pour être plus spécifique
      message = "No combination of variables resulted in a model where all predictors were statistically significant."
    ))
  }
  
  #best_model_info <- valid_models[[which.min(sapply(valid_models, `[[`, "aic"))]]
  best_model_info <- valid_models[[which.min(sapply(valid_models, `[[`, "rmse_oos"))]]
  best_model <- best_model_info$model
  
  # --- RMSE Calculation and Return (your existing code) ---
  predictions_oos <- predict(best_model, newdata = test_df)
  rmse_oos <- sqrt(mean((predictions_oos - test_df$Y)^2))
  
  return(list(
    country = country_to_model,
    status = "Success",
    best_model_object = best_model,
    best_model_info = best_model_info,
    selected_vars = X_names, # Added for debugging
    rmse_oos = rmse_oos,
    plot_data = model_df,
    split_point = split_point
  ))
  
  }, error = function(e) {
    # In case of any error, return an informative error object
    return(list(country = country_to_model, status = "Error", message = e$message))
  })
}


#########################################################################
# ---       PHASE 1 : EXÉCUTION PARALLÈLE DES CALCULS                 --- #
#########################################################################

cat("--- Phase 1: Lancement des calculs en parallèle ---\n")

num_cores <- parallel::detectCores() - 1
if (num_cores < 1) num_cores <- 1
cat("Utilisation de", num_cores, "coeurs de processeur.\n")

cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, varlist = c("data_risk", "Z_country", "data_lags", "country_map", 
                                        "compute_model_for_country", "P_VALUE_THRESHOLD", 
                                        "TRAIN_TEST_SPLIT_RATIO"))

results_list <- parallel::parLapply(cl, countries, function(c) {
  # On encapsule l'appel dans une fonction pour que les librairies soient chargées sur chaque noeud
  compute_model_for_country(c, country_map)
})

parallel::stopCluster(cl)
cat("--- Phase 1: Calculs terminés. ---\n\n")


#########################################################################
# --- PHASE 2 : ÉCRITURE SÉQUENTIELLE DES RÉSULTATS ET GRAPHIQUES   --- #
#########################################################################

cat("--- Phase 2: Génération des fichiers de diagnostic ---\n")

output_main_dir <- "../outputs_best_rmse_oos"

# On vérifie si ce dossier n'existe pas, et on le crée si nécessaire.
# C'est plus robuste que de se fier à showWarnings = FALSE.
if (!dir.exists(output_main_dir)) {
  cat("Création du dossier de sortie principal:", output_main_dir, "\n")
  dir.create(output_main_dir)
}


for (result in results_list) {
  
  country <- result$country
  
  # Gérer les cas d'échec (inchangé)
  if (result$status != "Success") {
    cat(sprintf("Pays: %-12s | Statut: %-15s | Raison: %s\n", country, result$status, result$message))
    next # On passe au pays suivant
  }
  
  # --- MODIFIÉ : Le message de log est plus clair ---
  cat(sprintf("Pays: %-12s | Statut: Succès | Écriture des fichiers dans './%s/%s/'...\n", country, output_main_dir, country))
  
  # --- MODIFIÉ : Création du sous-dossier spécifique au pays DANS le dossier output ---
  # On utilise file.path() pour construire le chemin de manière sécurisée.
  country_dir_path <- file.path(output_main_dir, country)
  # recursive = TRUE est une sécurité supplémentaire qui créera le dossier parent s'il manque.
  dir.create(country_dir_path, showWarnings = FALSE, recursive = TRUE)
  
  # Unpacking des résultats (inchangé)
  best_model <- result$best_model_object
  best_model_info <- result$best_model_info
  rmse_oos <- result$rmse_oos
  model_df <- result$plot_data
  split_point <- result$split_point
  
  # --- MODIFIÉ : Construire les chemins des fichiers de sortie avec file.path() ---
  summary_path <- file.path(country_dir_path, "model_summary.txt")
  
  # Écriture du résumé texte
  sink(summary_path)
  cat("=====================================================\n")
  cat("  Résumé du Meilleur Modèle pour:", country, "\n")
  cat("=====================================================\n\n")
  cat("Critère de sélection : AIC (sur l'échantillon d'entraînement)\n")
  cat("AIC du modèle sélectionné :", best_model_info$aic, "\n\n")
  cat("--- Diagnostic de Performance (sur l'échantillon de test) ---\n")
  cat("RMSE (Out-of-Sample) :", rmse_oos, "\n\n")
  cat("--- Détails du Modèle ---\n")
  print(summary(best_model))
  sink()
  
  # --- MODIFIÉ : Construire le chemin du graphique avec file.path() ---
  plot_path <- file.path(country_dir_path, "Z_factor_fit_diagnostic.png")
  
  # Génération du graphique
  png(plot_path, width = 800, height = 600)
  
  train_preds <- predict(best_model, newdata = model_df[1:split_point, ])
  test_preds <- predict(best_model, newdata = model_df[(split_point + 1):nrow(model_df), ])
  
  plot(model_df$Y, type = "l", col = "grey50", lwd = 2,
       ylab = "Valeur de Z", xlab = "Temps",
       main = paste("Diagnostic du Modèle pour", country),
       sub = paste("RMSE (Test) =", round(rmse_oos, 4)))
  
  lines(1:split_point, train_preds, col = "darkgreen", lwd = 2)
  lines((split_point + 1):nrow(model_df), test_preds, col = "red", lwd = 2)
  abline(v = split_point + 0.5, col = "blue", lty = 2, lwd = 2)
  
  legend("topleft", bty = "n",
         legend = c("Z Observé", "Ajustement (Entraînement)", "Prédiction (Test)", "Séparation"),
         col = c("grey50", "darkgreen", "red", "blue"),
         lwd = 2, lty = c(1, 1, 1, 2))
  
  dev.off()
}

cat("--- Phase 2: Terminé. ---\n")

