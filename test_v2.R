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
data_risk <- read.csv("data_risk_EBA.csv", header = TRUE, sep = ";")
data_risk <- data_risk[data_risk$Date != "31/12/2024", ]
countries <- unique(data_risk$Country)
Z_results <- lapply(countries, function(c) f_Z_estimation(data_risk$Corpo_DR_WA[data_risk$Country == c]))
Z_country <- sapply(Z_results, `[[`, "Z"); colnames(Z_country) <- countries
data_macro <- read.csv("data_macro_estimation.csv", header = TRUE, sep = ";")
data_macro_FCI <- read.csv("data_macro_estimation_FCI.csv", header = TRUE, sep = ";")
all_data <- dplyr::full_join(data_macro, data_macro_FCI, by = "Date")
all_data$Date <- as.Date(all_data$Date, tryFormats = c("%d/%m/%Y"))
all_data <- all_data %>%
  dplyr::mutate(dplyr::across(-Date, ~ as.numeric(gsub(",", ".", .)))) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(INF = Infla - dplyr::lag(Infla), EPU = EPU_world - dplyr::lag(EPU_world))
nb_lags <- 6; lag_indices <- 0:nb_lags
vars_to_lag <- c("GDP", "INF", "EPU", "FCI_total", "FCI_corpo", "FCI_house",
                 grep("FCI_.*_[A-Z]{2}", names(all_data), value = TRUE))
lagged_vars <- lapply(vars_to_lag, function(v) sapply(lag_indices, function(k) dplyr::lag(all_data[[v]], k)))
data_lags <- as.data.frame(do.call(cbind, lagged_vars))
names(data_lags) <- paste0(rep(vars_to_lag, each = nb_lags + 1), "_lag", lag_indices)
data_lags$Date <- all_data$Date


#########################################################################
# ---      FONCTION DE CALCUL POUR UN PAYS (SANS ÉCRITURE)            --- #
#########################################################################

compute_model_for_country <- function(country_to_model) {
  
  tryCatch({
    library(dplyr)
    
    # Préparation des données
    dates_risk <- unique(as.Date(data_risk$Date, tryFormats = c("%d/%m/%Y")))
    Y_df <- data.frame(Date = dates_risk, Y = Z_country[, country_to_model])
    if(all(is.na(Y_df$Y))) stop("Calcul du facteur Z impossible.")
    
    model_df <- dplyr::inner_join(Y_df, data_lags, by = "Date") %>% na.omit()
    if(nrow(model_df) < 20) stop("Pas assez de données valides.")
    
    # Division train/test
    split_point <- floor(nrow(model_df) * TRAIN_TEST_SPLIT_RATIO)
    train_df <- model_df[1:split_point, ]
    test_df <- model_df[(split_point + 1):nrow(model_df), ]
    
    # Sélection du modèle
    param_set <- "total"
    if (param_set == "total") X_names <- names(model_df)[grep("^(GDP|INF|EPU|FCI_total)_lag", names(model_df))]
    
    combs <- unlist(lapply(1:5, function(k) combn(X_names, k, simplify = FALSE)), recursive = FALSE)
    model_results <- lapply(combs, function(vars) {
      formula <- as.formula(paste("Y ~", paste(vars, collapse = " + ")))
      model <- lm(formula, data = train_df)
      p_values <- summary(model)$coefficients[-1, "Pr(>|t|)"]
      if (any(p_values > P_VALUE_THRESHOLD)) return(NULL)
      list(model = model, vars = vars, aic = AIC(model))
    })
    
    valid_models <- Filter(Negate(is.null), model_results)
    if (length(valid_models) == 0) stop("Aucun modèle avec des variables significatives.")
    
    best_model_info <- valid_models[[which.min(sapply(valid_models, `[[`, "aic"))]]
    best_model <- best_model_info$model
    
    # Calcul du diagnostic RMSE
    predictions_oos <- predict(best_model, newdata = test_df)
    rmse_oos <- sqrt(mean((predictions_oos - test_df$Y)^2))
    
    # Retourner un objet complet avec tous les résultats
    return(list(
      country = country_to_model,
      status = "Succès",
      best_model_object = best_model,
      best_model_info = best_model_info,
      rmse_oos = rmse_oos,
      plot_data = model_df,
      split_point = split_point
    ))
    
  }, error = function(e) {
    # En cas d'erreur, retourner un objet d'erreur
    return(list(country = country_to_model, status = "Erreur", message = e$message))
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
parallel::clusterExport(cl, varlist = ls())

# Lancement des calculs : chaque noeud renvoie un objet R complet
results_list <- parallel::parLapply(cl, countries, compute_model_for_country)

parallel::stopCluster(cl)
cat("--- Phase 1: Calculs terminés. ---\n\n")


#########################################################################
# --- PHASE 2 : ÉCRITURE SÉQUENTIELLE DES RÉSULTATS ET GRAPHIQUES   --- #
#########################################################################

cat("--- Phase 2: Génération des fichiers de diagnostic ---\n")

for (result in results_list) {
  
  country <- result$country
  
  # Cas 1: Le calcul a échoué
  if (result$status == "Erreur") {
    cat(sprintf("Pays: %-12s | Statut: ERREUR | Raison: %s\n", country, result$message))
    next # On passe au pays suivant
  }
  
  # Cas 2: Le calcul a réussi
  cat(sprintf("Pays: %-12s | Statut: Succès | Écriture des fichiers...\n", country))
  
  # Création du dossier
  dir.create(country, showWarnings = FALSE)
  
  # Unpacking des résultats
  best_model <- result$best_model_object
  best_model_info <- result$best_model_info
  rmse_oos <- result$rmse_oos
  model_df <- result$plot_data
  split_point <- result$split_point
  
  # Écriture du résumé texte
  summary_path <- file.path(country, "model_summary.txt")
  sink(summary_path)
  cat("=====================================================\n")
  cat("   Résumé du Meilleur Modèle pour:", country, "\n")
  cat("=====================================================\n\n")
  cat("Critère de sélection : AIC (sur l'échantillon d'entraînement)\n")
  cat("AIC du modèle sélectionné :", best_model_info$aic, "\n\n")
  cat("--- Diagnostic de Performance (sur l'échantillon de test) ---\n")
  cat("RMSE (Out-of-Sample) :", rmse_oos, "\n\n")
  cat("--- Détails du Modèle ---\n")
  print(summary(best_model))
  sink()
  
  # Génération du graphique
  plot_path <- file.path(country, "Z_factor_fit_diagnostic.png")
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