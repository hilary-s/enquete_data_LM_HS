server <- function(input, output, session) {
  
  # --- Filtres principaux réactifs ---
  filtered_data <- eventReactive(input$update, {
    req(data_classique)
    df <- data_classique
    if (!is.null(input$statut)) df <- df %>% filter(statut %in% input$statut)
    if (!is.null(input$poste)) df <- df %>% filter(poste %in% input$poste)
    if (!is.null(input$genre) && !("all" %in% input$genre)) df <- df %>% filter(genre %in% input$genre)
    df
  })
  
  filtered_data_outils <- eventReactive(input$update, {
    req(data_outils)
    df <- data_outils
    if (!is.null(input$statut)) df <- df %>% filter(statut %in% input$statut)
    if (!is.null(input$poste)) df <- df %>% filter(poste %in% input$poste)
    if (!is.null(input$genre) && !("all" %in% input$genre)) df <- df %>% filter(genre %in% input$genre)
    if (!is.null(input$outil) && length(input$outil) > 0) df <- df %>% filter(outil %in% input$outil)
    df
  })
  
  # ValueBoxes
  output$effectif_total_box <- renderValueBox({
    df <- if(input$stat_question == "outil") filtered_data_outils() else filtered_data()
    n_obs <- if(nrow(df) > 0) n_distinct(df$id) else 0
    valueBox(value = n_obs, subtitle = "Effectif total", icon = icon("users"), color = "blue")
  })
  
  output$age_moyen_box <- renderValueBox({
    df <- filtered_data()
    valueBox(value = round(mean(df$age, na.rm = TRUE),1),
             subtitle = "Âge moyen", icon = icon("birthday-cake"), color = "purple")
  })
  
  output$experience_moy_box <- renderValueBox({
    df <- filtered_data()
    valueBox(value = round(mean(df$experience, na.rm = TRUE),1),
             subtitle = "Expérience moyenne (ans)", icon = icon("chart-line"), color = "green")
  })
  
  output$satisfaction_moy_box <- renderValueBox({
    df <- filtered_data()
    valueBox(value = round(mean(df$satisfaction, na.rm = TRUE),1),
             subtitle = "Satisfaction moyenne", icon = icon("smile"), color = "yellow")
  })
  
  
  
  # --- Tableau Statistiques ---
  output$table_stat <- renderDT({
    req(input$stat_question)
    question <- input$stat_question
    df <- if(question == "outil") filtered_data_outils() else filtered_data()
    
    if(nrow(df) == 0) return(datatable(tibble(Message="Aucune donnée pour ces filtres")))
    
    tab <- switch(question,
                  "outil" = {
                    df_scores <- df %>% mutate(score_outil = case_when(
                      frequence == "Jamais" ~ 0,
                      frequence == "Occasionnellement" ~ 1,
                      frequence == "Régulièrement" ~ 2,
                      TRUE ~ NA_real_
                    ))
                    df_scores %>%
                      group_by(outil) %>%
                      summarise(
                        Effectif_total = n_distinct(id),
                        Effectif_utilisation = sum(score_outil > 0, na.rm=TRUE),
                        Pourcentage = round(mean(score_outil, na.rm=TRUE)/2*100, 1),
                        .groups="drop"
                      ) %>% arrange(desc(Effectif_total))
                  },
                  "secteur" = {
                    df2 <- df %>% distinct(id, .keep_all = TRUE)
                    df2 %>% count(secteur) %>% mutate(Pourcentage = round(n / sum(n) * 100, 1)) %>% arrange(desc(n))
                  },
                  "teletravail" = {
                    df2 <- df %>% distinct(id, .keep_all = TRUE) %>%
                      mutate(score_teletravail = case_when(
                        teletravail == "Présentiel complet (pas ou quasiment pas de travail à distance)" ~ 0,
                        teletravail == "Hybride avec en moyenne un ou deux jours de télétravail / semaine" ~ 1,
                        teletravail == "Hybride avec en moyenne trois ou quatre jours de télétravail / semaine" ~ 2,
                        teletravail == "Distanciel complet (pas de travail sur site)" ~ 3,
                        TRUE ~ NA_real_
                      ))
                    tab <- df2 %>% count(teletravail) %>% mutate(Pourcentage = round(n / sum(n) * 100, 1))
                    tab <- bind_rows(tab, tibble(teletravail="Score moyen", n=NA, Pourcentage=round(mean(df2$score_teletravail, na.rm=TRUE),2)))
                    tab
                  },
                  "experience" = {
                    df2 <- df %>% distinct(id, .keep_all = TRUE)
                    df2 %>% summarise(
                      Moyenne = round(mean(experience, na.rm=TRUE),1),
                      Médiane = median(experience, na.rm=TRUE),
                      Min = min(experience, na.rm=TRUE),
                      Max = max(experience, na.rm=TRUE)
                    )
                  }
    )
    
    datatable(tab, options=list(scrollX=TRUE))
  })
  
  # --- Graphiques Statistiques ---
  output$plot_stat <- renderPlot({
    req(input$stat_question)
    question <- input$stat_question
    df <- if(question == "outil") filtered_data_outils() else filtered_data()
    if(nrow(df) == 0){ plot.new(); text(0.5,0.5,"Aucune donnée pour ces filtres"); return() }
    
    if(question == "outil"){
      ggplot(df, aes(x=outil, fill=frequence)) +
        geom_bar(position="fill") +
        scale_y_continuous(labels=scales::percent_format()) +
        labs(x="Outil", y="Pourcentage", fill="Fréquence") +
        theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1))
    } else if(question == "secteur"){
      ggplot(df, aes(x=secteur)) +
        geom_bar(fill="#2c7fb8") +
        labs(x="Secteur", y="Effectifs") +
        theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1))
    } else if(question == "teletravail"){
      df <- df %>% mutate(score_teletravail = case_when(
        teletravail == "Présentiel complet (pas ou quasiment pas de travail à distance)" ~ 0,
        teletravail == "Hybride avec en moyenne un ou deux jours de télétravail / semaine" ~ 1,
        teletravail == "Hybride avec en moyenne trois ou quatre jours de télétravail / semaine" ~ 2,
        teletravail == "Distanciel complet (pas de travail sur site)" ~ 3,
        TRUE ~ NA_real_
      ))
      ggplot(df, aes(y=score_teletravail)) +
        geom_boxplot(fill="#5B95AF") +
        labs(y="Score télétravail") + theme_minimal()
    } else if(question == "experience"){
      ggplot(df, aes(y=experience)) +
        geom_boxplot(fill="#5B95AF") +
        labs(y="Années d'expérience") + theme_minimal()
    }
  })
  
  # --- Analyse univariée ---
  output$plot_uni2 <- renderPlot({
    req(input$var_uni2)
    var <- input$var_uni2
    df <- if(var %in% c("outil","frequence")) filtered_data_outils() else filtered_data()
    if(nrow(df) == 0 || !(var %in% names(df))) { plot.new(); text(0.5,0.5,"Aucune donnée"); return() }
    
    if(var %in% vars_quanti){
      ggplot(df, aes(y=.data[[var]])) +
        geom_boxplot(fill="#5B95AF") +
        labs(title=paste("Boxplot de", var), y=var) + theme_minimal()
    } else {
      df <- df %>% mutate(!!var := factor(.data[[var]], levels=names(sort(table(.data[[var]]), decreasing=TRUE))))
      ggplot(df, aes(x=.data[[var]])) +
        geom_bar(fill="black") +
        labs(title=paste("Répartition de", var), x=var, y="Fréquence") +
        theme_minimal() + theme(axis.text.x=element_text(angle=45, hjust=1))
    }
  })
  
  output$table_uni2 <- renderDT({
    req(input$var_uni2)
    var <- input$var_uni2
    df <- if(var %in% c("outil","frequence")) filtered_data_outils() else filtered_data()
    if(nrow(df)==0 || !(var %in% names(df))) return(NULL)
    
    if(var %in% vars_quali){
      df_summary <- df %>%
        group_by(.data[[var]]) %>%
        summarise(
          `Effectif` = n(),
          `Expérience moyenne (année)` = paste0(round(mean(experience, na.rm=TRUE),2)),
          `Satisfaction moyenne (/10)` = paste0(round(mean(satisfaction, na.rm=TRUE),2)),
          `Âge moyen` = paste0(round(mean(age, na.rm=TRUE),0)),
          .groups="drop"
        ) %>%
        arrange(desc(`Effectif`)) 
      
      datatable(
        df_summary,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          autoWidth = TRUE,
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        class = "cell-border stripe hover compact"
      )
    }
  })
  
  # --- Analyse bivariée ---
  output$plot_bi2 <- renderPlot({
    req(input$x_var2, input$y_var2)
    
    x_var <- input$x_var2
    y_var <- input$y_var2
    color_enable <- input$color_enable2
    color_var <- if(color_enable && !is.null(input$color_var2)) input$color_var2 else NULL
    
    # Choix du dataset selon les variables
    dataset_vars <- c(x_var, y_var)
    df <- if(any(dataset_vars %in% c("outil", "frequence"))) filtered_data_outils() else filtered_data()
    
    # Vérification du dataset
    if(nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "Aucune donnée pour ces filtres")
      return()
    }
    
    # Vérification que les colonnes existent
    missing_cols <- dataset_vars[!dataset_vars %in% names(df)]
    if(length(missing_cols) > 0) {
      plot.new()
      text(0.5, 0.5, paste("Colonne manquante:", paste(missing_cols, collapse=", ")))
      return()
    }
    
    # Gestion des couleurs
    if(!is.null(color_var) && color_var %in% names(df)) {
      df[[color_var]] <- as.factor(df[[color_var]])
    } else {
      color_var <- NULL
    }
    
    # Déterminer le type de graphique
    x_is_num <- x_var %in% vars_quanti
    y_is_num <- y_var %in% vars_quanti
    
    if(x_is_num && y_is_num) {
      # Scatter plot
      p <- if(!is.null(color_var)) {
        ggplot(df, aes(x=.data[[x_var]], y=.data[[y_var]], color=.data[[color_var]])) +
          geom_point(alpha=0.7, size=2)
      } else {
        ggplot(df, aes(x=.data[[x_var]], y=.data[[y_var]])) +
          geom_point(alpha=0.7, size=2)
      }
    } else if(!x_is_num && y_is_num) {
      # Boxplot
      if(!is.null(color_var)) {
        p <- ggplot(df, aes(x=.data[[x_var]], y=.data[[y_var]], fill=.data[[color_var]])) +
          geom_boxplot()
      } else {
        p <- ggplot(df, aes(x=.data[[x_var]], y=.data[[y_var]])) +
          geom_boxplot(fill="#2ca25f")
      }
    } else if(!x_is_num && !y_is_num) {
      # Barplot groupé
      df[[x_var]] <- as.factor(df[[x_var]])
      df[[y_var]] <- as.factor(df[[y_var]])
      p <- ggplot(df, aes(x=.data[[x_var]], fill=.data[[y_var]])) +
        geom_bar(position="dodge") +
        labs(fill=y_var)
    } else {
      # Cas non géré
      plot.new()
      text(0.5, 0.5, "Type de variable non géré")
      return()
    }
    
    # Ajouter thèmes et axes
    p + theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = x_var, y = y_var)
  })
  
  
  
  output$tab <- renderDT({
    # Utiliser le dataset filtré via le bouton Update
    df <- filtered_data()
    
    # Affichage selon le nombre de lignes choisi
    head(df, input$Nblignes)
  })
  
  output$map_region <- renderLeaflet({
    
    # Dataset filtré selon les inputs
    df <- filtered_data()
    
    # Comptage par région et par poste
    df_region_poste <- df %>%
      group_by(region, poste) %>%
      summarise(effectif = n_distinct(id), .groups = "drop") %>%
      group_by(region) %>%
      summarise(label_postes = paste0(poste, ": ", effectif, collapse = "<br>"), 
                effectif_total = sum(effectif), .groups = "drop")
    
    # Charger les géométries des régions de France
    regions_geo <- geojsonio::geojson_read("www/regions.geojson", what = "sp")
    
    # Joindre les effectifs et labels aux géométries
    regions_geo@data <- regions_geo@data %>%
      left_join(df_region_poste, by = c("nom" = "region")) 
    regions_geo@data$effectif_total[is.na(regions_geo@data$effectif_total)] <- 0
    regions_geo@data$label_postes[is.na(regions_geo@data$label_postes)] <- "Aucun répondant"
    
    # Palette de couleurs selon l'effectif total
    pal <- colorNumeric(palette = "YlOrRd", domain = regions_geo@data$effectif_total)
    
    # Carte
    leaflet(regions_geo) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(effectif_total),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~lapply(paste0(nom, "<br>Total: ", effectif_total, "<br>", label_postes), htmltools::HTML)
      ) %>%
      addLegend(
        pal = pal, values = ~effectif_total, opacity = 0.7, title = "Effectif total par région",
        position = "bottomright"
      )
  })
  
  # Logique pour télécharger le guide utilisateur (fichier PDF)
  output$download_guide <- downloadHandler(
    filename = function() {
      "guide_utilisateur.pdf"  # Nom du fichier téléchargé
    },
    content = function(file) {
      # Copie le fichier PDF depuis le répertoire www vers l'emplacement de téléchargement
      file.copy("www/guide.pdf", file)
    }
  )
  
  
}