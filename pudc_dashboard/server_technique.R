# ===============================
# Module serveur : Suivi Technique
# ===============================

library(readxl)

# Initialisation des outputs technique
init_technique_outputs <- function(input, output, session) {
  # Chargement des données technique
  technique_data <- reactive({
    tryCatch({
      f <- "data/Suivi technique.xlsx"
      if (!file.exists(f)) {
        cat("❌ Fichier 'Suivi technique.xlsx' non trouvé\n")
        return(NULL)
      }
      sheets <- readxl::excel_sheets(f)
      list_tech <- lapply(sheets, function(s) {
        df <- read_excel(f, sheet = s)
        df$projet <- s
        df
      })
      df_tech <- dplyr::bind_rows(list_tech)
      colnames(df_tech) <- c("Volets", "Objectifs_annuels", "Objectif", "Réalisation", "Observations", "projet")
      df_tech <- df_tech %>% mutate(SousVolets = Volets)
      return(df_tech)
    }, error = function(e) {
      cat("❌ Erreur lors du chargement des données technique:", e$message, "\n")
      return(NULL)
    })
  })

  # Génération des onglets par projet
  output$technique_tabs <- renderUI({
    df_tech <- technique_data()
    if (is.null(df_tech)) {
      return(div("Données non disponibles"))
    }
    
    do.call(tabsetPanel, c(list(id = "projets", type = "tabs"),
      lapply(unique(df_tech$projet), function(proj_name) {
        safe_id <- gsub("[^a-zA-Z0-9]", "_", proj_name)
        tabPanel(title = proj_name,
          div(class = "marquee-container", uiOutput(paste0("marquee_", safe_id))),
          div(class = "graph-container", plotlyOutput(paste0("graph_", safe_id), height = "400px")),
          br(), 
          DTOutput(paste0("table_", safe_id))
        )
      })
    ))
  })

  # Génération des graphiques et tableaux pour chaque projet
  observe({
    df_tech <- technique_data()
    if (is.null(df_tech)) return()
    
    for (proj in unique(df_tech$projet)) {
      local({
        proj_name <- proj
        safe_id <- gsub("[^a-zA-Z0-9]", "_", proj_name)
        
        # Graphique pour le projet
        output[[paste0("graph_", safe_id)]] <- renderPlotly({
          dat <- df_tech %>% filter(projet == proj_name) %>%
            mutate(Objectif = as.numeric(Objectif) * 100, Réalisation = as.numeric(Réalisation) * 100) %>%
            filter(!is.na(Objectif), !is.na(Réalisation))
          
          if (nrow(dat) == 0) {
            return(plot_ly() %>% layout(title = "Pas de données disponibles pour ce projet"))
          }
          
          plot_ly(dat, x = ~Volets) %>%
            add_bars(y = ~Objectif, name = "Objectif (%)", marker = list(color = "#aed6f1"), 
                    text = ~paste(round(Objectif, 1), "%"), hoverinfo = "text", textposition = "auto") %>%
            add_bars(y = ~Réalisation, name = "Réalisation (%)", marker = list(color = "#fcfaa4"), 
                    text = ~paste(round(Réalisation, 1), "%"), hoverinfo = "text", textposition = "auto") %>%
            layout(barmode = "group", xaxis = list(title = "Volets", tickangle = -45), 
                   yaxis = list(title = "Taux (%)", range = c(0, 120)), 
                   title = list(text = paste("Exécution technique -", proj_name), x = 0.5))
        })

        # Marquee pour les observations
        output[[paste0("marquee_", safe_id)]] <- renderUI({
          dat <- df_tech %>% filter(projet == proj_name) %>% 
                 filter(!is.na(Observations), Observations != "")
          if (nrow(dat) == 0) return(NULL)
          
          colors <- c("linear-gradient(135deg, #99e6f2, #7dd3fc)", "linear-gradient(135deg, #f28280, #fca5a5)", 
                     "linear-gradient(135deg, #bbfcef, #86efac)", "linear-gradient(135deg, #e8d21d, #fde047)", 
                     "linear-gradient(135deg, #f0e6ff, #ddd6fe)")
          
          html_blocks <- mapply(function(obj, obs, volet, i) {
            col <- colors[(i %% length(colors)) + 1]
            sprintf("<span style='display:inline-block; background:%s; margin:0 20px; padding:15px 20px; border-radius:12px; box-shadow:0 4px 15px rgba(0,0,0,0.1);'> <b>🎯 Volet:</b> %s<br><b>📋 Objectif:</b> %s<br><span style='color:#dc2626; font-weight:bold;'>🔍 %s</span></span>", 
                   col, volet, obj, obs)
          }, dat$Objectifs_annuels, dat$Observations, dat$Volets, seq_along(dat$Volets))
          
          HTML(paste0("<marquee behavior='scroll' direction='left' scrollamount='3' style='padding: 20px; font-size: 14px; font-weight: 500;'>", 
                     paste(html_blocks, collapse = ""), "</marquee>"))
        })

        # Tableau pour le projet
        output[[paste0("table_", safe_id)]] <- renderDT({
          df_tech %>% filter(projet == proj_name) %>% select(Volets, SousVolets, Objectifs_annuels)
        }, options = list(pageLength = 10, autoWidth = TRUE, dom = 'tip', 
                         stripeClasses = c('stripe1', 'stripe2'), 
                         language = list(search = "Filtrer :", lengthMenu = "Afficher _MENU_ lignes")), 
          rownames = FALSE)
      })
    }
  })
} 