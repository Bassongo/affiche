library(shiny)
# Additional packages for charts and maps
library(ggplot2)
library(plotly)
library(readxl)
library(leaflet)
library(sf)
library(dplyr)
# For image carousel on home page
# For image carousel on home page (optional)
if (requireNamespace("slickR", quietly = TRUE)) {
  library(slickR)
  slick_available <- TRUE
} else {
  slick_available <- FALSE
}
# For API calls in Assistant tab
library(httr)

# Path to the Excel workbook containing all PTBA data
excel_path <- "data/PTBA_2025.xlsx"

# Helper function to safely read sheets (returns NULL if file missing)
read_sheet <- function(sheet) {
  tryCatch(read_excel(excel_path, sheet = sheet), error = function(e) NULL)
}

# Load datasets (they may be NULL if the Excel file is absent)
tech_data       <- read_sheet("Exécution technique du PTBA 2025")
budget_repart   <- read_sheet("Répartition du budget PTBA par projet")
budget_exec     <- read_sheet("Exécution budgétaire PTBA 2025")
decaissements   <- read_sheet("Décaissements globaux 2025")
ppm_data        <- read_sheet("Analyse par Projet")
perf_data       <- read_sheet("PERFORMANCE GLOBALE 2025")
resume_data     <- read_sheet("TEC. BUDGETAIRE. PPM")


# ---- UI ----
ui <- navbarPage(
  title = "Tableau de bord de suivi du PTBA du PUDC 2025",
  tabPanel(
    "Accueil",
    # Carousel of images (replace URLs with images from the PUDC site)
    uiOutput("carousel")
  ),
  tabPanel(
    "Suivi Technique",
    sidebarLayout(
      sidebarPanel(
        # Dropdown to filter by project
        uiOutput("projet_selector")
      ),
      mainPanel(
        plotlyOutput("tech_plot")
      )
    )
  ),
  tabPanel(
    "Suivi Financier",
    tabsetPanel(
      tabPanel("Répartition du budget", plotlyOutput("repartition_plot")),
      tabPanel("Exécution budgétaire", plotlyOutput("exec_plot")),
      tabPanel("Décaissements globaux", plotlyOutput("decaissement_plot"))
    )
  ),
  tabPanel(
    "Performance Passation Marchés",
    plotlyOutput("ppm_plot")
  ),
  tabPanel(
    "Réalisation Indicateurs",
    fluidRow(
      column(6, leafletOutput("sn_map")),
      column(6, plotlyOutput("indicateur_bar"))
    )
  ),
  tabPanel(
    "Résumé",
    plotlyOutput("resume_plot")
  ),
  tabPanel(
    "Assistant IA",
    textAreaInput("user_prompt", "Posez votre question :", rows = 3),
    actionButton("ask_ai", "Envoyer"),
    verbatimTextOutput("ai_response")
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # ----- Page Accueil -----
  output$carousel <- renderUI({
    if (slick_available) {
      slickR::slickR(
        list(
          img(src = "https://via.placeholder.com/800x400?text=Pistes+rurales"),
          img(src = "https://via.placeholder.com/800x400?text=Electrification"),
          img(src = "https://via.placeholder.com/800x400?text=Postes+de+sante")
        ),
        slideIdx = TRUE
      )
    } else {
      tagList(
        tags$p("Package slickR non installé. Installez-le pour activer le carrousel."),
        img(src = "https://via.placeholder.com/800x400?text=Pistes+rurales", style = "width:80%;"),
        img(src = "https://via.placeholder.com/800x400?text=Electrification", style = "width:80%;"),
        img(src = "https://via.placeholder.com/800x400?text=Postes+de+sante", style = "width:80%;")
      )
    }
  })

  # ----- Suivi Technique -----
  output$projet_selector <- renderUI({
    if (is.null(tech_data) || !"Projet" %in% names(tech_data)) return(NULL)
    selectInput("projet", "Choisir le projet", choices = unique(tech_data$Projet))
  })

  tech_filtered <- reactive({
    req(tech_data)
    if (!is.null(input$projet)) tech_data %>% filter(Projet == input$projet) else tech_data
  })

  output$tech_plot <- renderPlotly({
    req(tech_filtered())
    df <- tech_filtered()
    gg <- ggplot(df, aes(x = Volet)) +
      geom_bar(aes(y = Objectifs, fill = "Objectifs", text = Commentaire),
               stat = "identity", position = "dodge") +
      geom_bar(aes(y = Réalisations, fill = "Réalisations", text = Commentaire),
               stat = "identity", position = "dodge") +
      ylab("Objectifs vs Réalisations")
    ggplotly(gg, tooltip = c("x", "y", "text"))
  })

  # ----- Suivi Financier -----
  output$repartition_plot <- renderPlotly({
    req(budget_repart)
    gg <- ggplot(budget_repart, aes(x = Projet, y = Budget, fill = Projet)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(gg)
  })

  output$exec_plot <- renderPlotly({
    req(budget_exec)
    gg <- ggplot(budget_exec, aes(x = Volet, y = Montant, fill = Etat)) +
      geom_bar(stat = "identity", position = "dodge")
    ggplotly(gg)
  })

  output$decaissement_plot <- renderPlotly({
    req(decaissements)
    gg <- ggplot(decaissements, aes(x = Mois, y = Montant)) + geom_line()
    ggplotly(gg)
  })

  # ----- Performance Passation des Marchés -----
  output$ppm_plot <- renderPlotly({
    req(ppm_data)
    gg <- ggplot(ppm_data, aes(x = Projet, y = Performance, fill = Projet)) +
      geom_bar(stat = "identity")
    ggplotly(gg)
  })

  # ----- Réalisation Indicateurs -----
  output$sn_map <- renderLeaflet({
    req(perf_data)
    # Example uses a GeoJSON with regions; replace with correct file
    sn_shape <- tryCatch(read_sf("data/senegal_regions.geojson"), error = function(e) NULL)
    if (is.null(sn_shape)) return(leaflet())

    sn_shape <- left_join(sn_shape, perf_data, by = c("region_name" = "Region"))
    pal <- colorNumeric("Blues", sn_shape$Realisation_globale)

    leaflet(sn_shape) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Realisation_globale),
        label = ~paste(Region, "<br>Globale:", Realisation_globale,
                       "<br>Annuelle:", Realisation_annuelle),
        highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7)
      )
  })

  output$indicateur_bar <- renderPlotly({
    req(perf_data)
    gg <- ggplot(perf_data, aes(x = Region)) +
      geom_bar(aes(y = Cible_globale, fill = "Cible globale"), stat = "identity", position = "dodge") +
      geom_bar(aes(y = Realisation_globale, fill = "Réalisation globale"), stat = "identity", position = "dodge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(gg)
  })

  # ----- Résumé -----
  output$resume_plot <- renderPlotly({
    req(resume_data)
    gg <- ggplot(resume_data, aes(x = Element, y = Valeur, fill = Element)) + geom_bar(stat = "identity")
    ggplotly(gg)
  })

  # ----- Assistant IA -----
  observeEvent(input$ask_ai, {
    req(input$user_prompt)
    # Example call to a language model API (replace URL and key with real values)
    res <- POST(
      url = "https://api.groq.com/v1/chat/completions", # placeholder
      add_headers(Authorization = paste("Bearer", Sys.getenv("GROQ_API_KEY"))),
      encode = "json",
      body = list(messages = list(list(role = "user", content = input$user_prompt)))
    )
    if (status_code(res) == 200) {
      out <- content(res)$choices[[1]]$message$content
    } else {
      out <- "Erreur lors de l'appel à l'API"
    }
    output$ai_response <- renderText(out)
  })
}

shinyApp(ui, server)
