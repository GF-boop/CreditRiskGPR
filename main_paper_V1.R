setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#
# Démarche implémtée
#
# Etape 1 : Estimation du facteur systémique du modèle de Merton pour plusieurs pays
# Etape 2 : Régression du facteur systémique par rapport à un ensemble de variables macroéconomiques (+ risque géopolitique pour voir si sensible ou pas)
# Etape 3 : Régression quantile des variables macroéconomiques par rapport au risque géopolique
# Etape 4 : Analyse IRF pour simuler l'impact d'un risque géopolitique sur les variables du modèle de Merton
# Etape 5 : Réintégration dans le facteur systémique des chocs générés et estimation sur les probabilités de défaut



###############################
#                             #
#          Fonctions          #
#                             #
###############################

# --------------- Calibration du rho sectoriel ---------------

# Input : 
# - TD : moyenne des défauts d'un secteur
# Formule du régulateur (272) : https://www.bis.org/publ/bcbs128fre.pdf
# Output : 
# - rho estimé

f_rho_calibration <- function(TD){
  rho_estim <- 0.12*(1-exp(-50*TD))/(1-exp(-50))+0.24*(1-(1-exp(-50*TD))/(1-exp(-50)))
  return(rho_estim)
}


# --------------- Estimation du facteur Z ---------------

# Input : 
# - vect_TD : vecteur de l'historique du taux de défaut 
# cf. Formule de Merton
# Output : 
# une liste avec deux éléments qui sont :
# - Z : vecteur Z représentant le risque systématique
# - rho : rho qui permet d'avoir une variance de Z=1

f_Z_estimation <- function(vect_TD) {
  var_Z_rho <- function(rho) {
    if (rho <= 0 || rho >= 1) return(Inf)  # rho doit être dans (0,1)
    
    mean_TD <- mean(vect_TD)
    Z_estim <- (qnorm(mean_TD) - qnorm(vect_TD) * sqrt(1 - rho)) / sqrt(rho)
    return(var(Z_estim) - 1)
  }
  
  # Recherche du rho optimal 
  result <- uniroot(var_Z_rho, lower = 1e-6, upper = 1 - 1e-6)
  rho_opt <- result$root
  
  # Calcul de Z_estim avec le rho trouvé
  mean_TD <- mean(vect_TD)
  Z_estim <- (qnorm(mean_TD) - qnorm(vect_TD) * sqrt(1 - rho_opt)) / sqrt(rho_opt)
  
  return(list(Z = Z_estim, rho = rho_opt))
}

###############################
#                             #
#           ETAPE 1           #
#                             #
###############################

# Récupération des données sur les historiques de défaut

data_risk <- read.csv("data_risk_EBA.csv", header = T, sep=";")
head(data_risk)
# Suppression de la données 31/12/2024 car données macro ne vont pas jusque là
data_risk <- data_risk[data_risk$Date != "31/12/2024", ]


# Calcul du facteur systémique par pays

nb_country <- length(unique(data_risk$Country))
Z_country <- matrix(0,ncol=nb_country, nrow=(nrow(data_risk)/nb_country))
rho_country <- matrix(0,ncol=nb_country, nrow=1)

for (count in 1:nb_country){
  # sélection d'un pays
  country_select <- unique(data_risk$Country)[count]
  # récupération des données du pays
  data_risk_sub <- data_risk[data_risk$Country %in% country_select, ]
  vect_PD <- as.vector(data_risk_sub$Corpo_DR_WA)
  # calcul du facteur Z
  results_Z <- f_Z_estimation(vect_PD)
  
  # résultats
  Z_country[,count] <- results_Z$Z
  rho_country[count] <- results_Z$rho
  colnames(Z_country) <- unique(data_risk$Country)
  colnames(rho_country) <- unique(data_risk$Country)
}

# test affichage densité Z par pays :
count <- 1
colnames(Z_country)[count]
plot(density(Z_country[,count]))

###############################
#                             #
#           ETAPE 2           #
#                             #
###############################

# Récupération des données macroéconomiques

# Données macro sur la période 2010:2024Q3
#  croissance GDP,	Inflation, EPU_world (Economic Policy Uncertainty),	FCI_house	FCI_corpo	FCI_total (Financial Condition Index Euro)
data_macro <- read.csv("data_macro_estimation.csv", header = T, sep=";")
head(data_macro)

# Données FCI à la maille pays
data_macro_FCI <- read.csv("data_macro_estimation_FCI.csv", header = T, sep=";")
head(data_macro_FCI)

data_macro["Date"] <- as.Date(data_macro["Date"][,1], tryFormats=c("%d/%m/%Y"))
data_macro["GDP"] <- as.numeric(sub(",", ".", data_macro["GDP"][,1]))
data_macro["Infla"] <- as.numeric(sub(",", ".", data_macro["Infla"][,1]))
data_macro["EPU_world"] <- as.numeric(sub(",", ".", data_macro["EPU_world"][,1]))
data_macro["FCI_house"] <- as.numeric(sub(",", ".", data_macro["FCI_house"][,1]))
data_macro["FCI_corpo"] <- as.numeric(sub(",", ".", data_macro["FCI_corpo"][,1]))
data_macro["FCI_total"] <- as.numeric(sub(",", ".", data_macro["FCI_total"][,1]))

data_macro_FCI["FCI_house_BE"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_BE"][,1]))
data_macro_FCI["FCI_corpo_BE"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_BE"][,1]))
data_macro_FCI["FCI_total_BE"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_BE"][,1]))
data_macro_FCI["FCI_house_FR"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_FR"][,1]))
data_macro_FCI["FCI_corpo_FR"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_FR"][,1]))
data_macro_FCI["FCI_total_FR"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_FR"][,1]))
data_macro_FCI["FCI_house_GE"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_GE"][,1]))
data_macro_FCI["FCI_corpo_GE"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_GE"][,1]))
data_macro_FCI["FCI_total_GE"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_GE"][,1]))
data_macro_FCI["FCI_house_IT"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_IT"][,1]))
data_macro_FCI["FCI_corpo_IT"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_IT"][,1]))
data_macro_FCI["FCI_total_IT"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_IT"][,1]))
data_macro_FCI["FCI_house_LU"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_LU"][,1]))
data_macro_FCI["FCI_corpo_LU"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_LU"][,1]))
data_macro_FCI["FCI_total_LU"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_LU"][,1]))
data_macro_FCI["FCI_house_NT"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_NT"][,1]))
data_macro_FCI["FCI_corpo_NT"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_NT"][,1]))
data_macro_FCI["FCI_total_NT"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_NT"][,1]))
data_macro_FCI["FCI_house_SP"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_house_SP"][,1]))
data_macro_FCI["FCI_corpo_SP"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_corpo_SP"][,1]))
data_macro_FCI["FCI_total_SP"] <- as.numeric(sub(",", ".", data_macro_FCI["FCI_total_SP"][,1]))

# Stationarité
Inflation <- diff((data_macro$Infla))
EPU <- diff((data_macro$EPU_world))

###################
# DATA MANAGEMENT #
###################

# du 31/12/2015 au 30/09/2024
nb_date <- 35
nb_lag <- 0
Date_lag_0 <- data_macro["Date"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
GDP_lag_0 <- data_macro["GDP"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INFinit_lag_0 <- data_macro["Infla"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INF_lag_0 <- Inflation[(length(Inflation)-nb_date-nb_lag):(length(Inflation)-nb_lag)]
EPU_lag_0 <- EPU[(length(EPU)-nb_date-nb_lag):(length(EPU)-nb_lag)]
FCI_lag_0 <- data_macro["FCI_total"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIc_lag_0 <- data_macro["FCI_corpo"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIh_lag_0 <- data_macro["FCI_house"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]

FCI_lag_BE_0 <- data_macro_FCI["FCI_total_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_BE_0 <- data_macro_FCI["FCI_corpo_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_BE_0 <- data_macro_FCI["FCI_house_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_FR_0 <- data_macro_FCI["FCI_total_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_FR_0 <- data_macro_FCI["FCI_corpo_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_FR_0 <- data_macro_FCI["FCI_house_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_GE_0 <- data_macro_FCI["FCI_total_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_GE_0 <- data_macro_FCI["FCI_corpo_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_GE_0 <- data_macro_FCI["FCI_house_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_IT_0 <- data_macro_FCI["FCI_total_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_IT_0 <- data_macro_FCI["FCI_corpo_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_IT_0 <- data_macro_FCI["FCI_house_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_LU_0 <- data_macro_FCI["FCI_total_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_LU_0 <- data_macro_FCI["FCI_corpo_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_LU_0 <- data_macro_FCI["FCI_house_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_NT_0 <- data_macro_FCI["FCI_total_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_NT_0 <- data_macro_FCI["FCI_corpo_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_NT_0 <- data_macro_FCI["FCI_house_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_SP_0 <- data_macro_FCI["FCI_total_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_SP_0 <- data_macro_FCI["FCI_corpo_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_SP_0 <- data_macro_FCI["FCI_house_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]




# du 30/09/2015 au 30/06/2024
nb_date <- 35
nb_lag <- 1
Date_lag_1 <- data_macro["Date"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
GDP_lag_1 <- data_macro["GDP"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INFinit_lag_1 <- data_macro["Infla"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INF_lag_1 <- Inflation[(length(Inflation)-nb_date-nb_lag):(length(Inflation)-nb_lag)]
EPU_lag_1 <- EPU[(length(EPU)-nb_date-nb_lag):(length(EPU)-nb_lag)]
FCI_lag_1 <- data_macro["FCI_total"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIc_lag_1 <- data_macro["FCI_corpo"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIh_lag_1 <- data_macro["FCI_house"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]

FCI_lag_BE_1 <- data_macro_FCI["FCI_total_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_BE_1 <- data_macro_FCI["FCI_corpo_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_BE_1 <- data_macro_FCI["FCI_house_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_FR_1 <- data_macro_FCI["FCI_total_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_FR_1 <- data_macro_FCI["FCI_corpo_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_FR_1 <- data_macro_FCI["FCI_house_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_GE_1 <- data_macro_FCI["FCI_total_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_GE_1 <- data_macro_FCI["FCI_corpo_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_GE_1 <- data_macro_FCI["FCI_house_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_IT_1 <- data_macro_FCI["FCI_total_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_IT_1 <- data_macro_FCI["FCI_corpo_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_IT_1 <- data_macro_FCI["FCI_house_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_LU_1 <- data_macro_FCI["FCI_total_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_LU_1 <- data_macro_FCI["FCI_corpo_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_LU_1 <- data_macro_FCI["FCI_house_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_NT_1 <- data_macro_FCI["FCI_total_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_NT_1 <- data_macro_FCI["FCI_corpo_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_NT_1 <- data_macro_FCI["FCI_house_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_SP_1 <- data_macro_FCI["FCI_total_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_SP_1 <- data_macro_FCI["FCI_corpo_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_SP_1 <- data_macro_FCI["FCI_house_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]



# du 30/06/2015 au 31/03/2024

nb_date <- 35
nb_lag <- 2
Date_lag_2 <- data_macro["Date"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
GDP_lag_2 <- data_macro["GDP"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INFinit_lag_2 <- data_macro["Infla"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INF_lag_2 <- Inflation[(length(Inflation)-nb_date-nb_lag):(length(Inflation)-nb_lag)]
EPU_lag_2 <- EPU[(length(EPU)-nb_date-nb_lag):(length(EPU)-nb_lag)]
FCI_lag_2 <- data_macro["FCI_total"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIc_lag_2 <- data_macro["FCI_corpo"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIh_lag_2 <- data_macro["FCI_house"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]

FCI_lag_BE_2 <- data_macro_FCI["FCI_total_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_BE_2 <- data_macro_FCI["FCI_corpo_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_BE_2 <- data_macro_FCI["FCI_house_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_FR_2 <- data_macro_FCI["FCI_total_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_FR_2 <- data_macro_FCI["FCI_corpo_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_FR_2 <- data_macro_FCI["FCI_house_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_GE_2 <- data_macro_FCI["FCI_total_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_GE_2 <- data_macro_FCI["FCI_corpo_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_GE_2 <- data_macro_FCI["FCI_house_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_IT_2 <- data_macro_FCI["FCI_total_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_IT_2 <- data_macro_FCI["FCI_corpo_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_IT_2 <- data_macro_FCI["FCI_house_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_LU_2 <- data_macro_FCI["FCI_total_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_LU_2 <- data_macro_FCI["FCI_corpo_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_LU_2 <- data_macro_FCI["FCI_house_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_NT_2 <- data_macro_FCI["FCI_total_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_NT_2 <- data_macro_FCI["FCI_corpo_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_NT_2 <- data_macro_FCI["FCI_house_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_SP_2 <- data_macro_FCI["FCI_total_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_SP_2 <- data_macro_FCI["FCI_corpo_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_SP_2 <- data_macro_FCI["FCI_house_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]

# du 31/03/2015 au 31/12/2023

nb_date <- 35
nb_lag <- 3
Date_lag_3 <- data_macro["Date"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
GDP_lag_3 <- data_macro["GDP"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INFinit_lag_3 <- data_macro["Infla"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INF_lag_3 <- Inflation[(length(Inflation)-nb_date-nb_lag):(length(Inflation)-nb_lag)]
EPU_lag_3 <- EPU[(length(EPU)-nb_date-nb_lag):(length(EPU)-nb_lag)]
FCI_lag_3 <- data_macro["FCI_total"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIc_lag_3 <- data_macro["FCI_corpo"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIh_lag_3 <- data_macro["FCI_house"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]

FCI_lag_BE_3 <- data_macro_FCI["FCI_total_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_BE_3 <- data_macro_FCI["FCI_corpo_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_BE_3 <- data_macro_FCI["FCI_house_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_FR_3 <- data_macro_FCI["FCI_total_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_FR_3 <- data_macro_FCI["FCI_corpo_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_FR_3 <- data_macro_FCI["FCI_house_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_GE_3 <- data_macro_FCI["FCI_total_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_GE_3 <- data_macro_FCI["FCI_corpo_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_GE_3 <- data_macro_FCI["FCI_house_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_IT_3 <- data_macro_FCI["FCI_total_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_IT_3 <- data_macro_FCI["FCI_corpo_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_IT_3 <- data_macro_FCI["FCI_house_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_LU_3 <- data_macro_FCI["FCI_total_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_LU_3 <- data_macro_FCI["FCI_corpo_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_LU_3 <- data_macro_FCI["FCI_house_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_NT_3 <- data_macro_FCI["FCI_total_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_NT_3 <- data_macro_FCI["FCI_corpo_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_NT_3 <- data_macro_FCI["FCI_house_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_SP_3 <- data_macro_FCI["FCI_total_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_SP_3 <- data_macro_FCI["FCI_corpo_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_SP_3 <- data_macro_FCI["FCI_house_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]

# du 31/12/2014 au 30/09/2023
nb_date <- 35
nb_lag <- 4
Date_lag_4 <- data_macro["Date"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
GDP_lag_4 <- data_macro["GDP"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INFinit_lag_4 <- data_macro["Infla"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
INF_lag_4 <- Inflation[(length(Inflation)-nb_date-nb_lag):(length(Inflation)-nb_lag)]
EPU_lag_4 <- EPU[(length(EPU)-nb_date-nb_lag):(length(EPU)-nb_lag)]
FCI_lag_4 <- data_macro["FCI_total"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIc_lag_4 <- data_macro["FCI_corpo"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]
FCIh_lag_4 <- data_macro["FCI_house"][c((length(data_macro["Date"][,1])-nb_date-nb_lag):(length(data_macro["Date"][,1])-nb_lag)),]

FCI_lag_BE_4 <- data_macro_FCI["FCI_total_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_BE_4 <- data_macro_FCI["FCI_corpo_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_BE_4 <- data_macro_FCI["FCI_house_BE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_FR_4 <- data_macro_FCI["FCI_total_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_FR_4 <- data_macro_FCI["FCI_corpo_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_FR_4 <- data_macro_FCI["FCI_house_FR"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_GE_4 <- data_macro_FCI["FCI_total_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_GE_4 <- data_macro_FCI["FCI_corpo_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_GE_4 <- data_macro_FCI["FCI_house_GE"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_IT_4 <- data_macro_FCI["FCI_total_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_IT_4 <- data_macro_FCI["FCI_corpo_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_IT_4 <- data_macro_FCI["FCI_house_IT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_LU_4 <- data_macro_FCI["FCI_total_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_LU_4 <- data_macro_FCI["FCI_corpo_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_LU_4 <- data_macro_FCI["FCI_house_LU"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_NT_4 <- data_macro_FCI["FCI_total_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_NT_4 <- data_macro_FCI["FCI_corpo_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_NT_4 <- data_macro_FCI["FCI_house_NT"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCI_lag_SP_4 <- data_macro_FCI["FCI_total_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIc_lag_SP_4 <- data_macro_FCI["FCI_corpo_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]
FCIh_lag_SP_4 <- data_macro_FCI["FCI_house_SP"][c((length(data_macro_FCI["Date"][,1])-nb_date-nb_lag):(length(data_macro_FCI["Date"][,1])-nb_lag)),]


#############################
#                           #
#  Selection des variables  #
#                           #
#############################

#Belgium, France, Germany, Italy, Luxembourg, Netherlands and Spain

# choix du pays
Country <- "Spain"
# choix des variables à tester avec : 
#     - "FCI" si FCI maille pays
#     - "FCI detail" si FCI niveau 2 maille pays
#     - "total" si variables macro euro (PIB, infla, FCI) et EPU
param <- "total" 

Y <- Z_country[,Country]
if (Country == "Belgium"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_BE_0, FCI_lag_BE_1, FCI_lag_BE_2, FCI_lag_BE_3, FCI_lag_BE_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_BE_0, FCIc_lag_BE_1, FCIc_lag_BE_2, FCIc_lag_BE_3, FCIc_lag_BE_4,
                   FCIh_lag_BE_0, FCIh_lag_BE_1, FCIh_lag_BE_2, FCIh_lag_BE_3, FCIh_lag_BE_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}else if (Country == "France"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_FR_0, FCI_lag_FR_1, FCI_lag_FR_2, FCI_lag_FR_3, FCI_lag_FR_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_FR_0, FCIc_lag_FR_1, FCIc_lag_FR_2, FCIc_lag_FR_3, FCIc_lag_FR_4,
                   FCIh_lag_FR_0, FCIh_lag_FR_1, FCIh_lag_FR_2, FCIh_lag_FR_3, FCIh_lag_FR_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}else if (Country == "Germany"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_GE_0, FCI_lag_GE_1, FCI_lag_GE_2, FCI_lag_GE_3, FCI_lag_GE_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_GE_0, FCIc_lag_GE_1, FCIc_lag_GE_2, FCIc_lag_GE_3, FCIc_lag_GE_4,
                   FCIh_lag_GE_0, FCIh_lag_GE_1, FCIh_lag_GE_2, FCIh_lag_GE_3, FCIh_lag_GE_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}else if (Country == "Italy"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_IT_0, FCI_lag_IT_1, FCI_lag_IT_2, FCI_lag_IT_3, FCI_lag_IT_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_IT_0, FCIc_lag_IT_1, FCIc_lag_IT_2, FCIc_lag_IT_3, FCIc_lag_IT_4,
                   FCIh_lag_IT_0, FCIh_lag_IT_1, FCIh_lag_IT_2, FCIh_lag_IT_3, FCIh_lag_IT_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}else if (Country == "Luxembourg"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_LU_0, FCI_lag_LU_1, FCI_lag_LU_2, FCI_lag_LU_3, FCI_lag_LU_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_LU_0, FCIc_lag_LU_1, FCIc_lag_LU_2, FCIc_lag_LU_3, FCIc_lag_LU_4,
                   FCIh_lag_LU_0, FCIh_lag_LU_1, FCIh_lag_LU_2, FCIh_lag_LU_3, FCIh_lag_LU_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}else if (Country == "Netherlands"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_NT_0, FCI_lag_NT_1, FCI_lag_NT_2, FCI_lag_NT_3, FCI_lag_NT_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_NT_0, FCIc_lag_NT_1, FCIc_lag_NT_2, FCIc_lag_NT_3, FCIc_lag_NT_4,
                   FCIh_lag_NT_0, FCIh_lag_NT_1, FCIh_lag_NT_2, FCIh_lag_NT_3, FCIh_lag_NT_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}else if (Country == "Spain"){
  if (param=="FCI"){
    X_list <- list(FCI_lag_SP_0, FCI_lag_SP_1, FCI_lag_SP_2, FCI_lag_SP_3, FCI_lag_SP_4)
  }else if (param=="FCI detail"){
    X_list <- list(FCIc_lag_SP_0, FCIc_lag_SP_1, FCIc_lag_SP_2, FCIc_lag_SP_3, FCIc_lag_SP_4,
                   FCIh_lag_SP_0, FCIh_lag_SP_1, FCIh_lag_SP_2, FCIh_lag_SP_3, FCIh_lag_SP_4)
  }else if (param=="total"){
    X_list <- list(GDP_lag_0, GDP_lag_1, GDP_lag_2, GDP_lag_3, GDP_lag_4,
                   INF_lag_0, INF_lag_1, INF_lag_2, INF_lag_3, INF_lag_4,
                   EPU_lag_0, EPU_lag_1, EPU_lag_2, EPU_lag_3, EPU_lag_4,
                   FCI_lag_0, FCI_lag_1, FCI_lag_2, FCI_lag_3, FCI_lag_4)
  }
}
names(X_list) <- paste0("X", 1:length(X_list))


df <- data.frame(Y, X_list)

# Noms des colonnes explicatives
X_names <- names(df)[-1]


# Génération de toutes les combinaisons de 1 à 4 variables
combs <- unlist(
  lapply(1:4, function(k) combn(X_names, k, simplify = FALSE)),
  recursive = FALSE
)

# Pour chaque combinaison, ajuster un modèle et stocker l'AIC
results <- lapply(combs, function(vars) {
  formula <- as.formula(paste("Y ~", paste(vars, collapse = " + ")))
  model <- lm(formula, data = df)
  aic <- AIC(model)
  list(model = model, vars = vars, aic = aic)
})

best_model_info <- results[[which.min(sapply(results, function(x) x$aic))]]

best_model <- best_model_info$model
summary(best_model)
best_vars <- best_model_info$vars
best_aic <- best_model_info$aic


# Trier les résultats par AIC croissant
sorted_results <- results[order(sapply(results, function(x) x$aic))]

# Garder les 10 meilleurs
top_10 <- head(sorted_results, 10)

# On reprend le meilleur modèle (déjà obtenu dans le code précédent)
best_model_2 <- sorted_results[[2]]$model
summary(best_model)
best_model_3 <- sorted_results[[3]]$model
summary(best_model)
best_model_4 <- sorted_results[[4]]$model
summary(best_model)
best_model_5 <- sorted_results[[5]]$model
summary(best_model)

# Prédire les valeurs de Y avec ce modèle
Y_hat <- predict(best_model)

# Tracer Y réel et Y estimé
plot(Y, type = "l", col = "black", lwd = 2, 
     ylab = "Valeur", xlab = "Index (ou temps)", 
     main = "Valeurs observées vs. estimées de Z")
lines(Y_hat, col = "red", lwd = 2, lty = 2)

legend("topleft", legend = c(paste("Z observé", Country), "Z estimé"),
       col = c("black", "red"), lwd = 2, lty = c(1, 2))