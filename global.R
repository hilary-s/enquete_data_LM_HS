############ Chargement des packages ############ 
library(readr)      #Tidyvers : chargement des fichiers plats
library(readxl)     #Tidyvers : chargement des fichiers excel
library(dplyr)      #Tidyvers : traitement des données
library(ggplot2)    #Tidyvers : conception de graphique
library(forcats)    #Tidyvers : utilisation de facteurs
library(lubridate)  #Tidyvers : traitement des dates
library(stringr)
library(tidyr)

############ Import des données ############
data_enquete <- read_csv2("Data/enquete_data_raw.csv")

############ Traitements et nettoyage des données ############
#Suppression des réponses des étudiants/ en recheche d'emploi
data_enquete <- data_enquete %>% 
  filter(!A2_statut %in% c("En recherche d'emploi", "Etudiant (hors alternance / stage)"))
    #vérification
    data_enquete %>% count(A2_statut)
    
#Suppression des réponses des postes "Autre"
  data_enquete <- data_enquete %>% 
    filter(!A3_poste == "Autre")
    #vérification
    data_enquete %>% count(A3_poste)

#Suppression des colonnes commençant par B1       
    data_enquete <- data_enquete %>%    
      select(-starts_with("B1"))

#Renommage des valeurs de la variable A3_poste
    data_enquete <- data_enquete %>%
      mutate(
        A3_poste = case_when(
          A3_poste == "Chargé d’études / chargé d’études statistiques" ~ "Data Analyst / analyste",
          A3_poste == "Statisticien" ~ "Data Scientist",
          TRUE ~ A3_poste
        )
      )
    #vérification
    data_enquete %>% count(B1_utilisation_powerBI)   
    
#Traitements des questions préfixées par B2_
    b2_cols <- names(data_enquete)[str_detect(names(data_enquete), "^B2_")]
    
    data_enquete <- data_enquete %>%
      pivot_longer(
        cols = all_of(b2_cols),   # seulement les colonnes B2 passent en long
        names_to = "outil",
        values_to = "frequence"
      ) %>%
      mutate(
        outil = str_remove(outil, "^B2_frequence_utilisation_"),      # enlever le préfixe b2_
        frequence = replace_na(frequence, "Jamais")
      )
    
    #vérification
    data_enquete %>% count(outil) 
    