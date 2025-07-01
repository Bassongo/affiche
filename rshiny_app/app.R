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
# Updated Excel file (added to the data directory)
excel_path <- "data/PUDC_SUIVI TRIMESTRE  DU PTBA 2025 VF.xlsx"

# Helper function to safely read sheets (returns NULL if file missing)
read_sheet <- function(sheet) {
  tryCatch(read_excel(excel_path, sheet = sheet), error = function(e) NULL)
}

# Load datasets (they may be NULL if the Excel file is absent)
tech_data       <- read_sheet("Execution technique  PTBA 2025")
budget_repart   <- read_sheet("Repartit budget PTBA par projet")
budget_exec     <- read_sheet("Execution budgetaire PTBA 2025")
decaissements   <- read_sheet("DECAISSEMENT GLOBAUX 2025")
ppm_data        <- read_sheet("Analyse par Projet")
perf_data       <- read_sheet("PERFORMANCE GLOBALE 2025")
resume_data     <- read_sheet("TEC. BUDGETAIRE. PPM ")

# Adapt column names from the Excel sheets to the names expected by the app
if (!is.null(tech_data)) {
  names(tech_data) <- trimws(names(tech_data))
  if (!"Projet" %in% names(tech_data) && "Financement" %in% names(tech_data))
    tech_data <- rename(tech_data, Projet = Financement)
  obj_cols  <- grep("^Objectif", names(tech_data), value = TRUE)
  real_cols <- grep("^R[ée]alisation", names(tech_data), value = TRUE)
  if (!"Objectifs" %in% names(tech_data) && length(obj_cols) > 0)
    tech_data$Objectifs <- rowMeans(tech_data[obj_cols], na.rm = TRUE)
  if (!"Réalisations" %in% names(tech_data) && length(real_cols) > 0)
    tech_data$Réalisations <- rowMeans(tech_data[real_cols], na.rm = TRUE)
}

if (!is.null(budget_repart)) {
  names(budget_repart) <- trimws(names(budget_repart))
  if (!"Projet" %in% names(budget_repart) && "Volet" %in% names(budget_repart))
    budget_repart <- rename(budget_repart, Projet = Volet)
  if (!"Budget" %in% names(budget_repart) && "Budget (FCFA)" %in% names(budget_repart))
    budget_repart$Budget <- budget_repart$`Budget (FCFA)`
}

if (!is.null(perf_data)) {
  names(perf_data) <- trimws(names(perf_data))
  if (!"Cible_globale" %in% names(perf_data)) {
    cible <- grep("cible", names(perf_data), ignore.case = TRUE, value = TRUE)
    if (length(cible) > 0) perf_data$Cible_globale <- perf_data[[cible[1]]]
  }
  if (!"Realisation_globale" %in% names(perf_data)) {
    real <- grep("r[ée]alisation", names(perf_data), ignore.case = TRUE, value = TRUE)
    if (length(real) > 0) perf_data$Realisation_globale <- perf_data[[real[1]]]
  }
}

if (!is.null(ppm_data)) {
  names(ppm_data) <- trimws(names(ppm_data))
  if (!"Projet" %in% names(ppm_data)) {
    proj <- grep("projet", names(ppm_data), ignore.case = TRUE, value = TRUE)
    if (length(proj) > 0) names(ppm_data)[names(ppm_data) == proj[1]] <- "Projet"
  }
  if (!"Performance" %in% names(ppm_data)) {
    perf <- grep("perf", names(ppm_data), ignore.case = TRUE, value = TRUE)
    if (length(perf) > 0) names(ppm_data)[names(ppm_data) == perf[1]] <- "Performance"
  }
}

if (!is.null(budget_exec)) {
  names(budget_exec) <- trimws(names(budget_exec))
  if (!"Montant" %in% names(budget_exec)) {
    mnt <- grep("montant", names(budget_exec), ignore.case = TRUE, value = TRUE)
    if (length(mnt) > 0) budget_exec$Montant <- budget_exec[[mnt[1]]]
  }
}

if (!is.null(decaissements)) {
  names(decaissements) <- trimws(names(decaissements))
  if (!"Montant" %in% names(decaissements)) {
    mnt <- grep("montant", names(decaissements), ignore.case = TRUE, value = TRUE)
    if (length(mnt) > 0) decaissements$Montant <- decaissements[[mnt[1]]]
  }
}

if (!is.null(resume_data)) {
  names(resume_data) <- trimws(names(resume_data))
  if (!"Element" %in% names(resume_data)) {
    el <- grep("element", names(resume_data), ignore.case = TRUE, value = TRUE)
    if (length(el) > 0) resume_data$Element <- resume_data[[el[1]]]
  }
}


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
