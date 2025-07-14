# ===============================
# Module serveur : Réalisation globale des indicateurs
# ===============================

# Initialisation des outputs indicateurs
init_indicateurs_outputs <- function(output, app_data, input) {
  output$pie_financement <- renderPlotly({
    data <- app_data()
    if (is.null(data) || !"par_financement" %in% names(data)) return(plot_ly())
    plot_ly(data$par_financement, labels = ~Source, values = ~`Montant (%)`, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = data$par_financement$Couleur)) %>%
      layout(title = "Répartition par source de financement")
  })

  output$bar_region <- renderPlotly({
    data <- app_data()
    if (is.null(data) || !"par_region" %in% names(data)) return(plot_ly())
    plot_ly(data$par_region, x = ~Région, y = ~`Taux de Réalisation (%)`, type = 'bar',
            text = ~paste0(`Taux de Réalisation (%)`, "%"),
            marker = list(color = '#636EFA')) %>%
      layout(title = "Taux de réalisation par région", yaxis = list(title = "Taux (%)"))
  })

  output$line_evolution <- renderPlotly({
    data <- app_data()
    if (is.null(data) || !"evolution" %in% names(data)) return(plot_ly())
    evol <- data$evolution
    evol$Mois <- factor(evol$Mois, levels = c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc"))
    plot_ly(evol, x = ~Mois, y = ~`Taux de Réalisation (%)`, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#00CC96', width = 3), marker = list(color = '#636EFA', size = 8)) %>%
      layout(title = "Évolution du taux de réalisation", yaxis = list(title = "Taux (%)"))
  })

  output$obs_alertes <- renderUI({
    data <- app_data()
    if (is.null(data) || !"observations" %in% names(data)) return(NULL)
    lapply(seq_len(nrow(data$observations)), function(i) {
      cat <- data$observations$Catégorie[i]
      obs <- data$observations$Observation[i]
      couleur <- switch(cat,
        "Secteurs en Retard" = "#dc3545",
        "Contrats Résiliés" = "#ffc107",
        "Performances Excellentes" = "#28a745",
        "Régions Prioritaires" = "#636EFA",
        "#888888")
      div(style = paste0("background:", couleur, ";color:white;padding:12px;margin:8px 0;border-radius:10px;font-size:16px;"),
          tags$b(cat), tags$br(), obs)
    })
  })
}

# Initialisation des observers indicateurs
init_indicateurs_observers <- function(input, output, session, app_data) {
  observe({
    # Aucun observateur spécifique pour le moment
    NULL
  })
}
