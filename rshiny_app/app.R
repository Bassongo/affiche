library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)
library(plotly)
library(shiny.router)
# slickR is optional

#---- UI pages ----
accueil_page <- function() {
  div(
    br(),
    column(12, align = "center",
           tags$img(src = "pudc_logo.png", height = "100px"),
           br(),
           div(style = "background-color: #558C7C; color: white; padding: 20px;",
               tags$h1("Tableau de bord de suivi", style = "font-weight:bold; font-size: 36px; margin:0;"),
               tags$h2("PTBA DU PUDC 2025", style = "margin:0;")
           ),
           br(),
           tags$img(src = "arbre.png", height = "180px"),
           br(),
           div(style = "border: 1px solid #003366; background-color: #f0f8ff; display: flex; align-items: center; justify-content: space-between; padding: 10px 20px; margin-bottom: 20px;",
               tags$h3("Nos Réalisations", style = "color: #003366; font-weight: bold; margin: 0;"),
               div(
                 tags$img(src = "miniature1.png", height = "30px", style = "margin-left: 10px;"),
                 tags$img(src = "miniature2.png", height = "30px", style = "margin-left: 10px;"),
                 tags$img(src = "miniature3.png", height = "30px", style = "margin-left: 10px;")
               )
           ),
           br(), br(),
           uiOutput("carousel"),
           br(),
           fluidRow(
             column(2, tags$img(src = "miniature1.png", width = "100%")),
             column(2, tags$img(src = "miniature2.png", width = "100%")),
             column(2, tags$img(src = "miniature3.png", width = "100%")),
             column(2, tags$img(src = "miniature4.png", width = "100%")),
             column(2, tags$img(src = "miniature5.png", width = "100%")),
             column(2, tags$img(src = "miniature6.png", width = "100%"))
           ),
           br(),
           div(style = "background-color: #003366; color: white; padding: 20px; font-size: 14px;",
               "Programme d'Urgence de Développement Communautaire | Contact | Mentions légales",
               tags$br(),
               "Ministère du Développement communautaire, de la Solidarité nationale et de l'Équité territoriale"
           )
    )
  )
}

financier_page <- function() {
  div(
    h2("Suivi Financier"),
    plotOutput("budgetPlot"),
    tableOutput("budgetTable")
  )
}

indicateurs_page <- function() {
  div(
    h2("Indicateurs"),
    plotOutput("composantePlot")
  )
}

resume_page <- function() {
  div(h2("Page en construction"))
}

assistant_page <- function() {
  div(h2("Page en construction"))
}
#---- Routes ----
# Define a router with a root path so the app loads the home page
router <- make_router(
  route("/", accueil_page),
  route("accueil", accueil_page),
  route("financier", financier_page),
  route("indicateurs", indicateurs_page),
  route("resume", resume_page),
  route("assistant", assistant_page)
)
#---- Main UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: white !important; }
      .navbar-custom { display: flex; align-items: center; justify-content: space-between; padding: 10px 30px; background-color: white; }
      .navbar-logo img { height: 60px; }
      .navbar-menu a {
        margin-left: 20px;
        color: #003366;
        font-weight: bold;
        text-decoration: none;
        font-size: 16px;
      }
      .navbar-menu a:hover {
        border-bottom: 2px solid #558C7C;
      }
      .slick-slide img {
        object-fit: cover;
        height: 400px !important;
        margin: auto;
      }
    "))
  ),
  div(class = "navbar-custom",
      div(class = "navbar-logo", tags$img(src = "logo.png")),
      div(class = "navbar-menu",
          tags$a(href = route_link("accueil"), "Accueil"),
          tags$a(href = route_link("financier"), "Suivi Financier"),
          tags$a(href = route_link("indicateurs"), "Indicateurs"),
          tags$a(href = route_link("resume"), "Résumé"),
          tags$a(href = route_link("assistant"), "Assistant")
      )
  ),
  router_ui(router)
)

#---- Server ----
server <- function(input, output, session) {
  router_server(router)

  data <- read_excel("data/Base0.xlsx")

  images <- c("piste.png", "electrification.png", "poste_sante.png", "real.png")

  if (requireNamespace("slickR", quietly = TRUE)) {
    output$carousel <- renderSlickR({
      slickR::slickR(images, slideId = 'car1') +
        slickR::settings(
          autoplay = TRUE,
          autoplaySpeed = 2500,
          arrows = TRUE,
          dots = TRUE,
          infinite = TRUE
        )
    })
  } else {
    output$carousel <- renderUI({
      div(lapply(images, function(img) tags$img(src = img, width = '100%')))
    })
  }

  output$budgetPlot <- renderPlot({
    df <- data %>% group_by(Source) %>% summarise(Budget = sum(as.numeric(Budget_FCFA), na.rm = TRUE))
    ggplot(df, aes(x = reorder(Source, Budget), y = Budget, fill = Source)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(x = '', y = 'Budget (FCFA)')
  })

  output$budgetTable <- renderTable({
    data
  })

  output$composantePlot <- renderPlot({
    df <- data %>% group_by(Composante) %>% summarise(Budget = sum(as.numeric(Budget_FCFA), na.rm = TRUE))
    ggplot(df, aes(x = reorder(Composante, Budget), y = Budget, fill = Composante)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      theme_minimal() +
      labs(x = '', y = 'Budget (FCFA)')
  })
}

shinyApp(ui, server)
