ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(
      a(href = "", tags$img(src = "icone.png", height = "30px", width = "auto")),
      "Data Enquête" %>% strong %>% span(),
      class = "dropdown"
    ),
    titleWidth = "230px"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "tab_accueil", icon = icon("house")),
      menuItem("Données", tabName = "tab_données", icon = icon("database")),
      menuItem("Statistiques", tabName = "tab_stat", icon = icon("list-alt")),
      menuItem("Analyses", tabName = "tab_analyse", icon = icon("table")),
      menuItem("Carte", tabName = "tab_carte", icon = icon("map")),
      actionButton(inputId = "update", label = "Update", icon = icon("refresh")),
      
      # Filtres globaux
      pickerInput("statut", "Statuts :", 
                  choices = sort(unique(data_classique$statut)),
                  selected = sort(unique(data_classique$statut), decreasing = TRUE), multiple = TRUE,
                  options = list(`actions-box` = TRUE)),
      pickerInput("poste", "Postes :", 
                  choices = sort(unique(data_classique$poste)),
                  selected = sort(unique(data_classique$poste)), multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      pickerInput("outil", "Outils :", 
                  choices = sort(unique(data_outils$outil)), 
                  selected = sort(unique(data_outils$outil)), multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE)),
      selectInput("genre", "Genre :", 
                  choices = c("Tous" = "all", "Homme" = "Un homme", "Femme" = "Une femme", "Non précisé" = NA),
                  selected = "all"),
      # Ajouter un bouton pour télécharger le guide utilisateur (PDF)
      div(style = "position: fixed; bottom: 10px; left: 10px; width: 100%;",
          downloadButton("download_guide", "Télécharger guide utilisateur", icon = icon("file-pdf")))
    )
  ),
  
  dashboardBody(
    includeCSS("www/styles_v2.css"),
    
    tabItems(
      tabItem(
        tabName = "tab_accueil",
        div(
          id="accueil_page",
          fluidRow(
            div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
                div(class = "overlay-text",
                    p(
                      span("Visualisation et analyse statistique d'une enquête menée auprès des professionnels de la data", class = "title"), br(),
                      span("Morgane LAURENT - Hilary SOM", class = "names"), br(),
                      span("Master SDD", class = "names")
                    )
                ))
          )
        )),
      
      tabItem(tabName = "tab_données",
              h2(strong("Extrait des données"), class = "titre-blanc"),
              box(title = strong("Contexte"), width = 12,
                  div(style = "text-align:center;", a(href = "", tags$img(src = "contexte.png", height = "300px")))),
              box(title = "Nombre de lignes à afficher :", sliderInput("Nblignes", "", min = 2, max = 20, value = 10)),
              box(title = strong("Tableau de données"), DTOutput("tab"), width = 12)
      ),
      
      tabItem(tabName = "tab_stat",
              fluidRow(
                
                valueBoxOutput("effectif_total_box", width = 5),
                valueBoxOutput("age_moyen_box", width = 5),
                valueBoxOutput("experience_moy_box", width = 5),
                valueBoxOutput("satisfaction_moy_box", width = 5),
                box(title = "Paramètres", width = 5,
                    selectInput("stat_question", "Choisir une question :", 
                                choices = c(
                                  "Secteur (A4)" = "secteur",
                                  "Télétravail (A5)" = "teletravail",
                                  "Expérience (A6)" = "experience",
                                  "Fréquence outils (B2)" = "outil"
                                ))
                ),
                box(title = "Tableau récapitulatif", width = 12, DTOutput("table_stat"))
              ),
              fluidRow(
                box(title = "Graphique", width = 12, plotOutput("plot_stat"))
              )
      ),
      
      tabItem(tabName = "tab_analyse",
              tabsetPanel(
                
                tabPanel(title = strong("Analyses Univariées"),
                         fluidRow(
                           box(title = "Paramétrage", width = 4,
                               selectInput("var_uni2", "Choisissez une variable :", choices = c(vars_quanti, vars_quali), selected = vars_quanti[1])
                           ),
                           box(title = "Étude descriptive d'une variable", width = 8, plotOutput("plot_uni2"))
                         ),
                         fluidRow(
                           box(title = "Tableau récapitulatif", DTOutput("table_uni2"), width = 12)
                         )
                ),
                
                tabPanel(title = strong("Analyses Bivariées"),
                         fluidRow(
                           box(title = "Paramétrages", width = 4,
                               selectInput("x_var2", "Variable X :", choices = c(vars_quanti, vars_quali), selected = vars_quanti[1]),
                               selectInput("y_var2", "Variable Y :", choices = c(vars_quanti, vars_quali), selected = vars_quanti[2]),
                               checkboxInput("color_enable2", "Activer la coloration", value = FALSE),
                               conditionalPanel("input.color_enable2 == true",
                                                selectInput("color_var2", "Variable de couleur :", choices = vars_quali))
                           ),
                           box(title = "Graphique", width = 8, plotOutput("plot_bi2"))
                         )
                )
              )
      ),
      tabItem(tabName = "tab_carte",
              fluidRow(
                box(title = "Carte de France - Effectif par région", width = 12,
                    leafletOutput("map_region", height = 600)
                )
              )
      )
      
    )
  )
)