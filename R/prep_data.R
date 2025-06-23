# ==================== 1. LIBRAIRIES ==================== #
library(data.table)
source("utils/my_functions.R")


# ==================== 2. Traitement des données ==================== #
# Import des données macro (ici ce sont des données niveau Europe que j'avais
# utilisé dans un VAR mais tu peux les remplacer par les données MACRO que tu veux)
# df_macro <- fread("data/processed/data_macro_statio.csv")
df_macro <- fread("data/processed/data_macro_transform.csv")

Country <- c("Germany")
countries <- c("France","Germany","Luxembourg","Netherlands","Belgium",
               "Italy","Spain")

suffix_dict <- c("Belgium" = "BE", "France" = "FR", "Germany" = "GE",
                 "Italy" = "IT", "Luxembourg" = "LU",
                 "Netherlands" = "NT", "Spain" = "SP")

suffixes <- suffix_dict[countries]

explicit_cols <- grep(paste0("^(", paste(countries, collapse = "|"), ")_"),
                      names(df_macro), value = TRUE)
suffix_cols   <- grep(paste0("(", paste(suffixes, collapse = "|"), ")$"), 
                      names(df_macro), value = TRUE)

# Quand même vérifier que l'on a bien les variables macro au niveau interna
cols_to_keep <- c("Date", names(df_macro)[(ncol(df_macro) - 5) : ncol(df_macro)])

final_cols <- c(cols_to_keep, explicit_cols, suffix_cols)
df_macro_selected <- df_macro[, ..final_cols]

# Nouveau nommage
new_names <- names(df_macro_selected)

# Renommer seulement les colonnes avec lesquels on a le suffix.
for (country in countries) {
  suf <- suffix_dict[country]
  # Pour chaque nom de colonne se terminant par _SUFFIX (ex: _SP)
  for (i in seq_along(new_names)) {
    if (grepl(paste0("_", suf, "$"), new_names[i])) {
      root <- sub(paste0("_", suf, "$"), "", new_names[i])
      new_names[i] <- paste0(country, "_", root)
    }
  }
}

# Appliquer les nouveaux noms
setnames(df_macro_selected, old = names(df_macro_selected), new = new_names)
