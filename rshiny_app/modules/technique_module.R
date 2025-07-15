techniqueUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel="stylesheet", href="app0.css")),
    div(
      class="page",
      div(class="tech-particles",
          tags$div(class="tech-particle", style="left:5%;animation-delay:0s;"),
          tags$div(class="tech-particle", style="left:15%;animation-delay:3s;"),
          tags$div(class="tech-particle", style="left:25%;animation-delay:6s;"),
          tags$div(class="tech-particle", style="left:35%;animation-delay:9s;"),
          tags$div(class="tech-particle", style="left:45%;animation-delay:12s;"),
          tags$div(class="tech-particle", style="left:55%;animation-delay:15s;"),
          tags$div(class="tech-particle", style="left:65%;animation-delay:18s;"),
          tags$div(class="tech-particle", style="left:75%;animation-delay:1s;"),
          tags$div(class="tech-particle", style="left:85%;animation-delay:4s;"),
          tags$div(class="tech-particle", style="left:95%;animation-delay:7s;")
      ),
      div(class="tech-header-container",
          tags$img(src="electrification.png", style="width:100%;height:220px;object-fit:cover;"),
          div(class="tech-header-overlay"),
          div(class="tech-title-neon", style="position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);z-index:2;", "Suivi technique par projet")
      ),
      div(class="tech-intro",
          fluidRow(
            column(1, div(class="info-icon", tags$img(src="icon_info.png", width="60px"))),
            column(11, HTML("Cet onglet présente un état d'avancement détaillé des différentes activités techniques mises en œuvre dans le cadre du Programme d'Urgence de Développement Communautaire (PUDC)."))
          )
      ),
      uiOutput(ns("tabs"))
    )
  )
}

techniqueServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    f <- "data/Suivi technique.xlsx"
    sheets <- excel_sheets(f)
    list_tech <- lapply(sheets, function(s) {
      df <- read_excel(f, sheet = s)
      df$projet <- s
      df
    })
    df_tech <- bind_rows(list_tech)
    colnames(df_tech) <- c("Volets", "Objectifs_annuels", "Objectif", "Réalisation", "Observations", "projet")
    df_tech <- df_tech %>% mutate(SousVolets = Volets)

    output$tabs <- renderUI({
      do.call(tabsetPanel, c(list(id=ns("projets"), type="tabs"),
        lapply(unique(df_tech$projet), function(proj_name){
          safe_id <- gsub("[^a-zA-Z0-9]","_",proj_name)
          tabPanel(title=proj_name,
            div(class="marquee-container", uiOutput(ns(paste0("marquee_",safe_id)))),
            div(class="graph-container", plotlyOutput(ns(paste0("graph_",safe_id)), height="400px")),
            br(), DTOutput(ns(paste0("table_",safe_id)))
          )
        })
      ))
    })

    observe({
      for(proj in unique(df_tech$projet)){
        local({
          proj_name <- proj
          safe_id <- gsub("[^a-zA-Z0-9]","_",proj_name)
          output[[paste0("graph_",safe_id)]] <- renderPlotly({
            dat <- df_tech %>% filter(projet==proj_name) %>%
              mutate(Objectif=as.numeric(Objectif)*100, Réalisation=as.numeric(Réalisation)*100) %>%
              filter(!is.na(Objectif), !is.na(Réalisation))
            validate(need(nrow(dat)>0, "Pas de données disponibles pour ce projet"))
            plot_ly(dat, x=~Volets) %>%
              add_bars(y=~Objectif, name="Objectif (%)", marker=list(color="#aed6f1"), text=~paste(round(Objectif,1),"%"), hoverinfo="text", textposition="auto") %>%
              add_bars(y=~Réalisation, name="Réalisation (%)", marker=list(color="#fcfaa4"), text=~paste(round(Réalisation,1),"%"), hoverinfo="text", textposition="auto") %>%
              layout(barmode="group", xaxis=list(title="Volets", tickangle=-45), yaxis=list(title="Taux (%)", range=c(0,120)), title=list(text=paste("Exécution technique -",proj_name),x=0.5))
          })

          output[[paste0("marquee_",safe_id)]] <- renderUI({
            dat <- df_tech %>% filter(projet==proj_name) %>% filter(!is.na(Observations), Observations!="")
            if(nrow(dat)==0) return(NULL)
            colors <- c("linear-gradient(135deg, #99e6f2, #7dd3fc)", "linear-gradient(135deg, #f28280, #fca5a5)", "linear-gradient(135deg, #bbfcef, #86efac)", "linear-gradient(135deg, #e8d21d, #fde047)", "linear-gradient(135deg, #f0e6ff, #ddd6fe)")
            html_blocks <- mapply(function(obj, obs, volet, i){
              col <- colors[(i %% length(colors))+1]
              sprintf("<span style='display:inline-block; background:%s; margin:0 20px; padding:15px 20px; border-radius:12px; box-shadow:0 4px 15px rgba(0,0,0,0.1);'> <b>🎯 Volet:</b> %s<br><b>📋 Objectif:</b> %s<br><span style='color:#dc2626; font-weight:bold;'>🔍 %s</span></span>", col, volet, obj, obs)
            }, dat$Objectifs_annuels, dat$Observations, dat$Volets, seq_along(dat$Volets))
            HTML(paste0("<marquee behavior='scroll' direction='left' scrollamount='3' style='padding: 20px; font-size: 14px; font-weight: 500;'>", paste(html_blocks, collapse=""), "</marquee>"))
          })

          output[[paste0("table_",safe_id)]] <- renderDT({
            df_tech %>% filter(projet==proj_name) %>% select(Volets, SousVolets, Objectifs_annuels)
          }, options=list(pageLength=10, autoWidth=TRUE, dom='tip', stripeClasses=c('stripe1','stripe2'), language=list(search="Filtrer :", lengthMenu="Afficher _MENU_ lignes")), rownames=FALSE)
        })
      }
    })
  })
}
