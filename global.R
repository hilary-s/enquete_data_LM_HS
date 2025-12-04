############ Import des données ############
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)
library(DT)



data_enquete <- read_csv2("Data/enquete_data_raw.csv")

############ Traitements et nettoyage des données ############

# Suppression des réponses des étudiants/en recherche d'emploi
data_enquete <- data_enquete %>% 
  filter(!A2_statut %in% c("En recherche d'emploi", "Etudiant (hors alternance / stage)"))

# Suppression des réponses des postes "Autre"
data_enquete <- data_enquete %>% 
  filter(!A3_poste == "Autre")

# Suppression des colonnes commençant par B1       
data_enquete <- data_enquete[, !grepl("^B1", names(data_enquete))]

# Renommage des valeurs de la variable A3_poste
data_enquete <- data_enquete %>%
  mutate(
    A3_poste = case_when(
      A3_poste == "Chargé d’études / chargé d’études statistiques" ~ "Data Analyst / analyste",
      A3_poste == "Statisticien" ~ "Data Scientist",
      TRUE ~ A3_poste
    )
  )

# Colonnes B2 existantes
b2_cols <- grep("^B2_", names(data_enquete), value = TRUE)
b2_cols_existantes <- intersect(b2_cols, names(data_enquete))

# Colonnes à sélectionner pour data_outils
cols_outils <- c("id", "A2_statut", "A3_poste", "Y1A_genre", b2_cols_existantes)

data_outils <- data_enquete %>%
  dplyr::select(all_of(cols_outils)) %>%
  rename(
    statut = A2_statut,
    poste  = A3_poste,
    genre  = Y1A_genre
  ) %>%
  pivot_longer(
    cols = all_of(b2_cols_existantes),
    names_to = "outil",
    values_to = "frequence"
  ) %>%
  mutate(
    outil = str_remove(outil, "^B2_frequence_utilisation_"),
    frequence = replace_na(frequence, "Jamais")
  )

# Colonnes classiques à garder (hors B2)
cols_classique <- c(
  "id", "date", "A2_statut", "A2_autre", "A3_poste",
  "A4_secteur", "A4_secteur_autre", "A5_teleravail",
  "A6_experience", "D1_satisfaction", "Y1A_genre",
  "Y1B_age", "Y4_diplome", "Y2_region"
)

data_classique <- data_enquete %>%
  dplyr::select(all_of(cols_classique)) %>%
  rename(
    statut        = A2_statut,
    statut_autre  = A2_autre,
    poste         = A3_poste,
    secteur       = A4_secteur,
    secteur_autre = A4_secteur_autre,
    teletravail   = A5_teleravail,
    experience    = A6_experience,
    satisfaction  = D1_satisfaction,
    genre         = Y1A_genre,
    age           = Y1B_age,
    diplome       = Y4_diplome,
    region        = Y2_region
  ) %>%
  distinct(id, .keep_all = TRUE) %>%   # une ligne par répondant
  mutate(
    date = dmy_hm(date),
    date = as.Date(date)
  )

# Colonnes qualitatives
vars_quali <- c("statut", "statut_autre", "poste", "secteur", "secteur_autre",
                "teletravail", "genre", "diplome", "region", "outil", "frequence")

# Colonnes quantitatives
vars_quanti <- c("id", "experience", "satisfaction", "age")

# --- Remplacement des NA par "Non renseigné" dans les colonnes existantes ---
# Pour data_classique
vars_quali_classique <- intersect(vars_quali, names(data_classique))
data_classique[vars_quali_classique] <- lapply(data_classique[vars_quali_classique], function(x) {
  x[is.na(x)] <- "Non renseigné"
  x
})

# Pour data_outils
vars_quali_outils <- intersect(vars_quali, names(data_outils))
data_outils[vars_quali_outils] <- lapply(data_outils[vars_quali_outils], function(x) {
  x[is.na(x)] <- "Non renseigné"
  x
})