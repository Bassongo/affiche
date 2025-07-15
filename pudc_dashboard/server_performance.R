# ===============================
# Module serveur : Performance
# ===============================

library(readxl)

# Initialisation des outputs performance
init_performance_outputs <- function(input, output, session) {
  # Chargement des données performance
  performance_data <- reactive({
    tryCatch({
      f_pm <- "data/Performances en PM.xlsx"
      if (!file.exists(f_pm)) {
        cat("❌ Fichier 'Performances en PM.xlsx' non trouvé\n")
        return(NULL)
      }
      
      list(
        exec_physique = read_excel(f_pm, sheet = "Exécution Physique"),
        exec_budget = read_excel(f_pm, sheet = "EXECUTION BUDGETAIRE"),
        pm = read_excel(f_pm, sheet = "Plan de passation de marchés")
      )
    }, error = function(e) {
      cat("❌ Erreur lors du chargement des données performance:", e$message, "\n")
      return(NULL)
    })
  })

  # Mise à jour du sélecteur de projet
  observe({
    data <- performance_data()
    if (!is.null(data)) {
      projets_disponibles <- unique(c(data$exec_physique$Projets, data$exec_budget$PROJETS, data$pm$PROJETS))
      updateSelectInput(session, "projet_pm", choices = projets_disponibles)
    }
  })

  # Graphique exécution physique
  output$graph_exec_physique <- renderPlotly({
    req(input$projet_pm)
    data <- performance_data()
    if (is.null(data)) return(plot_ly() %>% layout(title = "Données non disponibles"))
    
    df <- data$exec_physique %>% filter(Projets == input$projet_pm)
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "Aucune donnée pour ce projet"))
    
    plot_ly(df, x = ~Trimestre) %>%
      add_trace(y = ~`Objectif (%)` * 100, name = "Objectif", type = 'bar', marker = list(color = '#aed6f1')) %>%
      add_trace(y = ~`Réalisation (%)` * 100, name = "Réalisation", type = 'bar', marker = list(color = '#fcf3cf')) %>%
      layout(barmode = 'group', xaxis = list(title = "Trimestre"), 
             yaxis = list(title = "Pourcentage (%)"),
             title = list(text = "Exécution physique trimestrielle", x = 0.5))
  })

  # Graphique exécution budgétaire
  output$graph_exec_budget <- renderPlotly({
    req(input$projet_pm)
    data <- performance_data()
    if (is.null(data)) return(plot_ly() %>% layout(title = "Données non disponibles"))
    
    df <- data$exec_budget %>% filter(PROJETS == input$projet_pm)
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "Aucune donnée pour ce projet"))
    
    plot_ly(df, x = ~Trimestre) %>%
      add_trace(y = ~`Objectif (%)` * 100, name = "Objectif", type = 'bar', marker = list(color = '#abebc6')) %>%
      add_trace(y = ~`Réalisation (%)` * 100, name = "Réalisation", type = 'bar', marker = list(color = '#fadbd8')) %>%
      layout(barmode = 'group', xaxis = list(title = "Trimestre"), 
             yaxis = list(title = "Pourcentage (%)"),
             title = list(text = "Exécution budgétaire trimestrielle", x = 0.5))
  })

  # Graphique performance PM
  output$graph_pm <- renderPlotly({
    req(input$projet_pm)
    data <- performance_data()
    if (is.null(data)) return(plot_ly() %>% layout(title = "Données non disponibles"))
    
    df <- data$pm %>% filter(PROJETS == input$projet_pm)
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "Aucune donnée pour ce projet"))
    
    plot_ly(df, x = ~Trimestre, y = ~`Taux d'exécution` * 100, type = 'bar', name = "Taux d'exécution",
            text = ~paste0(round(`Taux d'exécution` * 100, 1), "%"), textposition = 'auto', 
            marker = list(color = '#f7c6c7')) %>%
      layout(xaxis = list(title = "Trimestre"), yaxis = list(title = "Taux d'exécution (%)"),
             title = list(text = "Performance en passation de marchés", x = 0.5))
  })
} 