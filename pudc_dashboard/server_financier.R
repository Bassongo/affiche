# ===============================
# Module serveur : Suivi financier
# ===============================

# Initialisation des outputs financiers
init_financier_outputs <- function(output, app_data) {
  # Graphique barres par trimestre
  output$bar_trimestre_financier <- renderPlotly({
    cat("📊 Génération du graphique en barres\n")
    data <- app_data()
    if (is.null(data) || !"execution_budgetaire" %in% names(data)) {
      return(plot_ly() %>% layout(title = "Données non disponibles"))
    }
    execution_data <- data$execution_budgetaire
    projet_selectionne <- input$projet_select
    if (is.null(projet_selectionne) || projet_selectionne == "") {
      projet_selectionne <- "PUDC-Phase2/ Budget 2025 Etat"
      cat("🔧 Utilisation du projet par défaut\n")
    }
    df_filtered <- execution_data %>%
      filter(Projet == projet_selectionne) %>%
      arrange(Trimestre)
    if (nrow(df_filtered) == 0) {
      return(plot_ly() %>% layout(title = paste("Aucune donnée pour:", projet_selectionne)))
    }
    plot_ly(df_filtered,
            x = ~factor(Trimestre),
            y = ~Montant_reel_decaissé,
            type = "bar",
            marker = list(color = '#558C7C', line = list(color = '#003366', width = 1)),
            text = ~paste0(round(Montant_reel_decaissé/1000000, 1), "M"),
            textposition = 'outside',
            hovertemplate = paste(
              '<b>Trimestre %{x}</b><br>',
              'Montant: %{y:,.0f} FCFA<br>',
              'Projet: ', projet_selectionne, '<br>',
              '<extra></extra>'
            )) %>%
      layout(
        title = list(text = paste("Décaissements -", projet_selectionne), font = list(size = 14, color = '#003366')),
        xaxis = list(title = "Trimestre", tickfont = list(size = 12)),
        yaxis = list(title = "Montant (FCFA)", tickformat = ".2s", tickfont = list(size = 12)),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Camembert par volet
  output$pie_budget_volet <- renderPlotly({
    req(app_data())
    budget_data <- app_data()$budget_par_projet
    df_volet <- budget_data %>%
      group_by(Volet) %>%
      summarise(Budget_Total = sum(Budget_FCFA, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Budget_Total)) %>%
      head(8)
    colors <- c('#003366', '#558C7C', '#20c997', '#6610f2', '#fd7e14', '#e83e8c', '#ffc107', '#28a745')
    plot_ly(df_volet,
            labels = ~Volet,
            values = ~Budget_Total,
            type = 'pie',
            textinfo = 'percent',
            textposition = 'inside',
            textfont = list(size = 12, color = 'white'),
            marker = list(colors = colors[1:nrow(df_volet)], line = list(color = '#FFFFFF', width = 2)),
            hovertemplate = paste('<b>%{label}</b><br>', 'Budget: %{value:,.0f} FCFA<br>', 'Pourcentage: %{percent}<br>', '<extra></extra>')) %>%
      layout(
        title = list(text = "", font = list(size = 16)),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1.02, y = 0.5, font = list(size = 9), bgcolor = 'rgba(255,255,255,0.8)', bordercolor = 'rgba(0,0,0,0.2)', borderwidth = 1),
        margin = list(l = 20, r = 120, t = 50, b = 20),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Table simple (non utilisée mais conservée)
  output$table_execution_simple <- renderTable({
    req(app_data())
    execution_data <- app_data()$execution_budgetaire
    execution_data %>%
      mutate(`Budget (M FCFA)` = round(Budget_PTBA_2025_FCFA / 1000000, 1),
             `Décaissé (M FCFA)` = round(get("Montant_reel_decaissé") / 1000000, 1),
             `Taux (%)` = round((get("Montant_reel_decaissé") / Budget_PTBA_2025_FCFA) * 100, 1)) %>%
      select(Projet, Source, Trimestre, `Budget (M FCFA)`, `Décaissé (M FCFA)`, `Taux (%)`)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')

  # Tableau interactif complet
  output$table_execution_complete <- DT::renderDataTable({
    req(app_data())
    execution_data <- app_data()$execution_budgetaire
    df_table <- execution_data %>%
      mutate(`Budget (M FCFA)` = round(Budget_PTBA_2025_FCFA / 1000000, 1),
             `Décaissé (M FCFA)` = round(get("Montant_reel_decaissé") / 1000000, 1),
             `Taux (%)` = round((get("Montant_reel_decaissé") / Budget_PTBA_2025_FCFA) * 100, 1)) %>%
      select(Projet, Source, Trimestre, `Budget (M FCFA)`, `Décaissé (M FCFA)`, `Taux (%)`)
    DT::datatable(
      df_table,
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        info = TRUE,
        paging = TRUE,
        language = list(paginate = list("next" = "Suivant", "previous" = "Précédent"))
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle('Taux (%)', backgroundColor = DT::styleInterval(c(50, 80), values = c("#ffe6e6", "#fff2e6", "#e6ffe6")))
  }, server = FALSE)

  # Statistiques réactives
  output$total_budget <- renderText({
    req(app_data())
    tryCatch({
      total <- sum(app_data()$execution_budgetaire$Budget_PTBA_2025_FCFA, na.rm = TRUE)
      paste0(round(total/1000000000, 1), " Md")
    }, error = function(e) {"N/A"})
  })

  output$total_decaisse <- renderText({
    req(app_data())
    tryCatch({
      data <- app_data()$execution_budgetaire
      if ("Montant_reel_decaissé" %in% names(data)) {
        total <- sum(data[["Montant_reel_decaissé"]], na.rm = TRUE)
        paste0(round(total/1000000000, 1), " Md")
      } else {
        "N/A"
      }
    }, error = function(e) {"N/A"})
  })

  output$taux_execution <- renderText({
    req(app_data())
    tryCatch({
      data <- app_data()$execution_budgetaire
      if ("Montant_reel_decaissé" %in% names(data) && "Budget_PTBA_2025_FCFA" %in% names(data)) {
        taux <- (sum(data[["Montant_reel_decaissé"]], na.rm = TRUE) / sum(data$Budget_PTBA_2025_FCFA, na.rm = TRUE)) * 100
        paste0(round(taux, 1), "%")
      } else {
        "N/A"
      }
    }, error = function(e) {"N/A"})
  })

  output$nb_projets <- renderText({
    req(app_data())
    tryCatch({
      data <- app_data()$execution_budgetaire
      if ("Projet" %in% names(data)) {
        length(unique(data$Projet))
      } else {
        "N/A"
      }
    }, error = function(e) {"N/A"})
  })

  # Téléchargement CSV
  output$download_execution_csv <- downloadHandler(
    filename = function() { paste0("execution_budgetaire_PUDC_", Sys.Date(), ".csv") },
    content = function(file) {
      data_to_export <- app_data()$execution_budgetaire
      write.csv(data_to_export, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# Initialisation des observers financiers
init_financier_observers <- function(input, output, session, app_data) {
  projets_disponibles <- reactiveVal(NULL)
  observe({
    cat("🔍 Observer du menu projets (version simplifiée)\n")
    cat("✅ Menu initialisé avec choix statiques dans l'UI\n")
    data <- app_data()
    if (!is.null(data) && "execution_budgetaire" %in% names(data)) {
      execution_data <- data$execution_budgetaire
      projets_reels <- unique(execution_data$Projet)
      cat("📋 Projets dans les données:", paste(projets_reels, collapse = ", "), "\n")
      projets_disponibles(projets_reels)
    }
  })
  observe({
    if (!is.null(input$projet_select) && input$projet_select != "") {
      cat("🎯 Projet sélectionné dans le menu:", input$projet_select, "\n")
    } else {
      cat("⚠️ Menu toujours vide, projet sélectionné:", input$projet_select, "\n")
    }
  })
}
