# Application principale PUDC Dashboard
# Toutes les dépendances et fonctions sont chargées depuis les fichiers sources

source("global.R")
source("ui.R")
source("server_financier.R")
source("server_indicateurs.R")
source("server_assistant_ia.R")

server <- function(input, output, session) {
  # Chargement des données
  app_data <- reactive({
    load_excel_data()
  })

  # Forcer le chargement au démarrage
  observe({ app_data() })

  # Navigation entre les pages
  current_page <- reactiveVal("accueil")
  observeEvent(input$nav_accueil, { current_page("accueil") })
  observeEvent(input$nav_financier, { current_page("financier") })
  observeEvent(input$nav_indicateurs, { current_page("indicateurs") })
  observeEvent(input$nav_assistant, { current_page("assistant") })

  output$main_content <- renderUI({
    switch(current_page(),
           "accueil" = accueil_page(),
           "financier" = financier_page(),
           "indicateurs" = indicateurs_page(),
           "assistant" = assistant_page())
  })

  # Carrousel sur la page d'accueil
  output$carousel <- renderSlickR({
    images <- c("piste.png", "electrification.png", "poste_sante.png", "real.png")
    slickR(images, height = 500, width = "100%") + settings(
      autoplay = TRUE,
      autoplaySpeed = 3000,
      arrows = TRUE,
      dots = TRUE,
      infinite = TRUE,
      fade = FALSE,
      cssEase = 'ease-in-out',
      pauseOnHover = TRUE,
      slidesToShow = 1,
      slidesToScroll = 1,
      speed = 1000
    )
  })

  # Initialisation des modules
  init_financier_outputs(input, output, app_data)
  init_financier_observers(input, output, session, app_data)
  init_indicateurs_outputs(output, app_data, input)
  init_indicateurs_observers(input, output, session, app_data)
  init_assistant_ia_outputs(input, output, session, app_data)
}

shinyApp(ui = ui, server = server)
