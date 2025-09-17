############ Chargement des packages ############ 
library(readr)      #Tidyvers : chargement des fichiers plats
library(readxl)     #Tidyvers : chargement des fichiers excel
library(dplyr)      #Tidyvers : traitement des données
library(ggplot2)    #Tidyvers : conception de graphique
library(forcats)    #Tidyvers : utilisation de facteurs
library(lubridate)  #Tidyvers : traitement des dates


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
