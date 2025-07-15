performanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel="stylesheet", href="app0.css")),
    div(
      class="page",
      div(style="position: relative; text-align: center;",
          tags$img(src="poste_sante.png", style="width:100%; filter:blur(2px); height:220px; object-fit:cover;"),
          div(style="position:absolute; top:50%; left:50%; transform:translate(-50%, -50%); color:white; font-size:50px; font-weight:bold; text-shadow:2px 2px 6px black; z-index:2;", "Performance en passation des marches")
      ),
      div(style="background-color:white; border:2px solid #003366; padding:25px; margin:30px auto; width:90%; border-radius:10px; font-size:15px;",
          fluidRow(column(1, tags$img(src="icon_info.png", width="60px")),
                   column(11, HTML("<b>Suivi intégré</b> : Cette section présente l'exécution physique, budgétaire et la performance en passation des marchés pour l'année 2025.")))
      ),
      div(style="width:300px; margin:0 auto; text-align:center;", selectInput(ns("projet_pm"), "Choisir un projet :", choices=NULL)),
      br(),
      fluidRow(column(12,
        h3("\u2705 Exécution physique trimestrielle", style="color:#2E86C1; text-align:center; font-weight:bold;"),
        plotlyOutput(ns("graph_exec_physique"), height="400px"),
        br(),
        h3("\U1F4B0 Exécution budgétaire trimestrielle", style="color:#239B56; text-align:center; font-weight:bold;"),
        plotlyOutput(ns("graph_exec_budget"), height="400px"),
        br(),
        h3("\u2696\uFE0F Taux de performance en passation de marchés", style="color:#CA6F1E; text-align:center; font-weight:bold;"),
        plotlyOutput(ns("graph_pm"), height="400px")
      ))
    )
  )
}

performanceServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    f_pm <- "data/Performances en PM.xlsx"
    df_exec_physique <- read_excel(f_pm, sheet="Exécution Physique")
    df_exec_budget <- read_excel(f_pm, sheet="EXECUTION BUDGETAIRE")
    df_pm <- read_excel(f_pm, sheet="Plan de passation de marchés")

    observe({
      projets_disponibles <- unique(c(df_exec_physique$Projets, df_exec_budget$PROJETS, df_pm$PROJETS))
      updateSelectInput(session, "projet_pm", choices=projets_disponibles)
    })

    output$graph_exec_physique <- renderPlotly({
      req(input$projet_pm)
      df <- df_exec_physique %>% filter(Projets == input$projet_pm)
      plot_ly(df, x=~Trimestre) %>%
        add_trace(y=~`Objectif (%)`*100, name="Objectif", type='bar', marker=list(color='#aed6f1')) %>%
        add_trace(y=~`Réalisation (%)`*100, name="Réalisation", type='bar', marker=list(color='#fcf3cf')) %>%
        layout(barmode='group', xaxis=list(title="Trimestre"), yaxis=list(title="Pourcentage (%)"))
    })

    output$graph_exec_budget <- renderPlotly({
      req(input$projet_pm)
      df <- df_exec_budget %>% filter(PROJETS == input$projet_pm)
      plot_ly(df, x=~Trimestre) %>%
        add_trace(y=~`Objectif (%)`*100, name="Objectif", type='bar', marker=list(color='#abebc6')) %>%
        add_trace(y=~`Réalisation (%)`*100, name="Réalisation", type='bar', marker=list(color='#fadbd8')) %>%
        layout(barmode='group', xaxis=list(title="Trimestre"), yaxis=list(title="Pourcentage (%)"))
    })

    output$graph_pm <- renderPlotly({
      req(input$projet_pm)
      df <- df_pm %>% filter(PROJETS == input$projet_pm)
      plot_ly(df, x=~Trimestre, y=~`Taux d'exécution`*100, type='bar', name="Taux d'exécution",
              text=~paste0(round(`Taux d'exécution`*100,1), "%"), textposition='auto', marker=list(color='#f7c6c7')) %>%
        layout(xaxis=list(title="Trimestre"), yaxis=list(title="Taux d'exécution (%)"))
    })
  })
}
