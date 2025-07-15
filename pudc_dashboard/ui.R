# ===============================
# Pages de l'application
# ===============================

# Page d'accueil avec carrousel slickR (version avancée)
accueil_page <- function() {
  div(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app0.css"),
      tags$script(HTML("// Fonction onglets PUDC\nshowPudcTab = function(tab){\n  document.querySelectorAll('.pudc-tab-content').forEach(el=>el.classList.remove('active'));\n  document.querySelectorAll('.pudc-tab').forEach(el=>el.classList.remove('active'));\n  document.getElementById('pudc-' + tab).classList.add('active');\n}"))
    ),
    div(
      class = "page",
      div(class = "floating-particles",
          tags$div(class = "particle", style = "left:10%;animation-delay:0s;"),
          tags$div(class = "particle", style = "left:20%;animation-delay:2s;"),
          tags$div(class = "particle", style = "left:30%;animation-delay:4s;"),
          tags$div(class = "particle", style = "left:40%;animation-delay:1s;"),
          tags$div(class = "particle", style = "left:50%;animation-delay:3s;"),
          tags$div(class = "particle", style = "left:60%;animation-delay:5s;"),
          tags$div(class = "particle", style = "left:70%;animation-delay:1.5s;"),
          tags$div(class = "particle", style = "left:80%;animation-delay:3.5s;"),
          tags$div(class = "particle", style = "left:90%;animation-delay:0.5s;")
      ),
      br(),
      column(12, align = "center",
             div(class = "main-logo", tags$img(src = "pudc_logo.png", height = "100px")),
             br(),
             div(class = "animated-title",
                 tags$h1(class = "typing-title", "Tableau de bord de suivi"),
                 tags$h2(class = "sliding-subtitle", "PTBA DU PUDC 2025")
             ),
             br(),
             div(class = "vertical-scroll-container",
                 div(class = "vertical-scroll-track",
                     div(class = "scroll-item", style = "background-image:url('electrification.png');", div(class = "scroll-text", "Électrification Rurale")),
                     div(class = "scroll-item", style = "background-image:url('piste.png');", div(class = "scroll-text", "Désenclavement des Zones Rurales")),
                     div(class = "scroll-item", style = "background-image:url('poste_sante.png');", div(class = "scroll-text", "Amélioration de la Santé")),
                     div(class = "scroll-item", style = "background-image:url('real.png');", div(class = "scroll-text", "Réalisations Concrètes")),
                     div(class = "scroll-item", style = "background-image:url('electrification.png');", div(class = "scroll-text", "Électrification Rurale")),
                     div(class = "scroll-item", style = "background-image:url('piste.png');", div(class = "scroll-text", "Désenclavement des Zones Rurales")),
                     div(class = "scroll-item", style = "background-image:url('poste_sante.png');", div(class = "scroll-text", "Amélioration de la Santé")),
                     div(class = "scroll-item", style = "background-image:url('real.png');", div(class = "scroll-text", "Réalisations Concrètes"))
                 )
             ),
             div(class = "pudc-presentation",
                 div(class = "pudc-content",
                     div(class = "pudc-tabs",
                         tags$button(class = "pudc-tab active", onclick = "showPudcTab('presentation')", "Présentation"),
                         tags$button(class = "pudc-tab", onclick = "showPudcTab('missions')", "Missions"),
                         tags$button(class = "pudc-tab", onclick = "showPudcTab('objectifs')", "Objectifs"),
                         tags$button(class = "pudc-tab", onclick = "showPudcTab('resultats')", "Résultats")
                     ),
                     div(id = "pudc-presentation", class = "pudc-tab-content active",
                         div(class = "pudc-title", "Programme d'Urgence de Développement Communautaire"),
                         div(class = "pudc-text", "Le PUDC est un programme ambitieux initié par le Gouvernement du Sénégalais pour accélérer le développement des communautés rurales."),
                         div(class = "pudc-highlight", "\"Une approche intégrée pour un développement durable et inclusif des territoires sénégalais\""),
                         div(class = "pudc-stats",
                             div(class = "pudc-stat", span(class = "pudc-stat-number", "14"), span(class = "pudc-stat-label", "Régions")),
                             div(class = "pudc-stat", span(class = "pudc-stat-number", "557"), span(class = "pudc-stat-label", "Communes")),
                             div(class = "pudc-stat", span(class = "pudc-stat-number", "8M"), span(class = "pudc-stat-label", "Bénéficiaires"))
                         )
                     ),
                     div(id = "pudc-missions", class = "pudc-tab-content",
                         div(class = "pudc-title", "Missions du PUDC"),
                         div(class = "pudc-text", "Le PUDC a pour mission principale de contribuer à l'amélioration des conditions de vie des populations rurales à travers le développement d'infrastructures de base et l'accès aux services essentiels.")),
                     div(id = "pudc-objectifs", class = "pudc-tab-content",
                         div(class = "pudc-title", "Objectifs Stratégiques"),
                         div(class = "pudc-text", "• Désenclavement des zones rurales\n• Électrification rurale\n• Accès à l'eau potable\n• Amélioration de la santé et de l'éducation\n• Développement économique local")),
                     div(id = "pudc-resultats", class = "pudc-tab-content",
                         div(class = "pudc-title", "Résultats atteints"),
                         div(class = "pudc-text", "Le PUDC a permis d'améliorer significativement les conditions de vie de millions de Sénégalais à travers des réalisations concrètes dans tous les secteurs prioritaires.")
                     )
                 )
             ),
             br(),
             div(class = "carousel-container", slickROutput("carousel", width = "80%", height = "400px")),
             br(),
             fluidRow(
               column(2, div(class = "miniature-container miniature-1 shine-effect", tags$img(src = "miniature1.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-2 shine-effect", tags$img(src = "miniature2.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-3 shine-effect", tags$img(src = "miniature3.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-4 shine-effect", tags$img(src = "miniature4.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-5 shine-effect", tags$img(src = "miniature5.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-6 shine-effect", tags$img(src = "miniature6.png", width = "100%")))
             ),
             br(),
             div(class = "animated-footer",
                 "Programme d'Urgence de Développement Communautaire | Contact | Mentions légales",
                 tags$br(),
                 "Ministère du Développement communautaire, de la Solidarité nationale et de l'Équité territoriale")
      )
    )
  )
}

# Page Suivi financier
financier_page <- function() {
  div(
    div(style = "background: linear-gradient(135deg, #003366, #558C7C); color: white; padding: 2rem; margin-bottom: 2rem; border-radius: 15px; text-align: center;",
        tags$h2("💰 Suivi Financier", style = "margin: 0; font-size: 2.5rem; font-weight: bold;"),
        tags$p("Analyse des décaissements et répartition budgétaire", style = "margin: 0.5rem 0 0 0; font-size: 1.2rem; opacity: 0.9;")
    ),
    fluidRow(
      column(12,
             div(style = "background: white; padding: 1.5rem; border-radius: 15px; margin-bottom: 2rem; box-shadow: 0 5px 15px rgba(0,0,0,0.1);",
                 fluidRow(
                   column(6,
                          selectInput("projet_select",
                                      tags$div(tags$strong("🎯 Sélectionner un projet :")),
                                      choices = list(
                                        "PUDC-Phase2/ Budget 2025 Etat" = "PUDC-Phase2/ Budget 2025 Etat",
                                        "PUDC-Phase 2/BID" = "PUDC-Phase 2/BID",
                                        "PUDC-Phase 2/BAD" = "PUDC-Phase 2/BAD",
                                        "PUDC-Phase 2/FSD" = "PUDC-Phase 2/FSD",
                                        "PUDC-Phase 2 /P.2000 VILLAGES" = "PUDC-Phase 2 /P.2000 VILLAGES"
                                      ),
                                      selected = "PUDC-Phase2/ Budget 2025 Etat",
                                      width = "100%")
                   ),
                   column(6,
                          div(style = "padding-top: 25px;",
                              tags$div(
                                style = "background: linear-gradient(135deg, #f8f9fa, #e9ecef); padding: 1rem; border-radius: 10px; text-align: center;",
                                tags$strong("📊 Données en temps réel", style = "color: #003366; font-size: 1.1rem;"),
                                tags$br(),
                                tags$span("Dernière mise à jour : ", style = "color: #666; font-size: 0.9rem;"),
                                tags$span(Sys.Date(), style = "color: #558C7C; font-weight: bold;")
                              )
                          )
                   )
                 )
             )
      )
    ),
    fluidRow(
      column(3,
             div(style = "background: linear-gradient(135deg, #28a745, #20c997); color: white; padding: 1.5rem; border-radius: 10px; text-align: center; margin: 10px;",
                 tags$h3(textOutput("total_budget"), style = "margin: 0; font-size: 1.8rem;"),
                 tags$p("Budget Total PTBA", style = "margin: 0; font-size: 0.9rem;")
             )
      ),
      column(3,
             div(style = "background: linear-gradient(135deg, #007bff, #6610f2); color: white; padding: 1.5rem; border-radius: 10px; text-align: center; margin: 10px;",
                 tags$h3(textOutput("total_decaisse"), style = "margin: 0; font-size: 1.8rem;"),
                 tags$p("Montant Décaissé", style = "margin: 0; font-size: 0.9rem;")
             )
      ),
      column(3,
             div(style = "background: linear-gradient(135deg, #ffc107, #fd7e14); color: white; padding: 1.5rem; border-radius: 10px; text-align: center; margin: 10px;",
                 tags$h3(textOutput("taux_execution"), style = "margin: 0; font-size: 1.8rem;"),
                 tags$p("Taux d'Exécution", style = "margin: 0; font-size: 0.9rem;")
             )
      ),
      column(3,
             div(style = "background: linear-gradient(135deg, #dc3545, #e83e8c); color: white; padding: 1.5rem; border-radius: 10px; text-align: center; margin: 10px;",
                 tags$h3(textOutput("nb_projets"), style = "margin: 0; font-size: 1.8rem;"),
                 tags$p("Projets Actifs", style = "margin: 0; font-size: 0.9rem;")
             )
      )
    ),
    fluidRow(
      column(6,
             div(style = "background: white; padding: 2rem; border-radius: 15px; margin: 10px; box-shadow: 0 5px 15px rgba(0,0,0,0.1);",
                 tags$h4("📊 Montants Décaissés par Trimestre",
                         style = "color: #003366; text-align: center; margin-bottom: 1.5rem;"),
                 plotlyOutput("bar_trimestre_financier", height = "350px")
             )
      ),
      column(6,
             div(style = "background: white; padding: 2rem; border-radius: 15px; margin: 10px; box-shadow: 0 5px 15px rgba(0,0,0,0.1);",
                 tags$h4("🥧 Répartition du Budget par Volet",
                         style = "color: #003366; text-align: center; margin-bottom: 1.5rem;"),
                 plotlyOutput("pie_budget_volet", height = "350px")
             )
      )
    ),
    fluidRow(
      column(12,
             div(style = "background: white; padding: 2rem; border-radius: 15px; margin: 10px; box-shadow: 0 5px 15px rgba(0,0,0,0.1);",
                 tags$h4("📋 Tableau d'Exécution Budgétaire",
                         style = "color: #003366; margin-bottom: 1.5rem;"),
                 DT::dataTableOutput("table_execution_complete"),
                 br(),
                 div(style = "text-align: center;",
                     downloadButton("download_execution_csv",
                                    "📥 Télécharger CSV",
                                    class = "btn btn-primary",
                                    style = "background: #558C7C; border: none; padding: 12px 30px; border-radius: 25px;")
                 )
             )
      )
    )
  )
}

# Page Indicateurs
indicateurs_page <- function() {
  div(
    h2("Réalisation globale et annuelle des indicateurs", style = "text-align:center; margin-top:20px;"),
    fluidRow(
      column(6,
        div(style="margin:10px;",
          plotlyOutput("pie_financement")
        )
      ),
      column(6,
        div(style="margin:10px;",
          plotlyOutput("bar_region")
        )
      )
    ),
    fluidRow(
      column(6,
        div(style="margin:10px;",
          plotlyOutput("line_evolution")
        )
      ),
      column(6,
        div(style="margin:10px;",
          uiOutput("obs_alertes")
        )
      )
    )
  )
}

# Page Assistant
assistant_page <- function() {
  div(
    tags$h2("🤖 Assistant IA", style = "color: #003366; text-align: center; margin: 20px 0;"),
    uiOutput("chat_history"),
    textInput("user_question", "Posez votre question à l'IA :", ""),
    actionButton("ask_ia", "Envoyer", icon = icon("paper-plane"))
  )
}

# Page Suivi Technique
technique_page <- function() {
  div(
    tags$head(tags$link(rel="stylesheet", href="app0.css")),
    div(
      class="page",
      div(class="tech-particles",
          tags$div(class="tech-particle", style="left:5%;animation-delay:0s;"),
          tags$div(class="tech-particle", style="left:15%;animation-delay:3s;"),
          tags$div(class = "tech-particle", style = "left:25%;animation-delay:6s;"),
          tags$div(class = "tech-particle", style = "left:35%;animation-delay:9s;"),
          tags$div(class = "tech-particle", style = "left:45%;animation-delay:12s;"),
          tags$div(class = "tech-particle", style = "left:55%;animation-delay:15s;"),
          tags$div(class = "tech-particle", style = "left:65%;animation-delay:18s;"),
          tags$div(class = "tech-particle", style = "left:75%;animation-delay:1s;"),
          tags$div(class = "tech-particle", style = "left:85%;animation-delay:4s;"),
          tags$div(class = "tech-particle", style = "left:95%;animation-delay:7s;")
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
      uiOutput("technique_tabs")
    )
  )
}

# Page Performance
performance_page <- function() {
  div(
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
      div(style="width:300px; margin:0 auto; text-align:center;", selectInput("projet_pm", "Choisir un projet :", choices=NULL)),
      br(),
      fluidRow(column(12,
        h3("✅ Exécution physique trimestrielle", style="color:#2E86C1; text-align:center; font-weight:bold;"),
        plotlyOutput("graph_exec_physique", height="400px"),
        br(),
        h3("💰 Exécution budgétaire trimestrielle", style="color:#239B56; text-align:center; font-weight:bold;"),
        plotlyOutput("graph_exec_budget", height="400px"),
        br(),
        h3("⚖️ Taux de performance en passation de marchés", style="color:#CA6F1E; text-align:center; font-weight:bold;"),
        plotlyOutput("graph_pm", height="400px")
      ))
    )
  )
}

# Page Résumé
resume_page <- function() {
  div(
    tags$head(tags$link(rel="stylesheet", href="app0.css")),
    div(class="page",
        h2("Résumé consolidé", style="text-align:center; margin-top:20px;"),
        tableOutput("resume_table")
    )
  )
}

# ===============================
# Interface utilisateur
# ===============================

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body {\n        background-color: #f8f9fa !important;\n        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;\n      }\n      .navbar-custom {\n        display: flex;\n        align-items: center;\n        justify-content: space-between;\n        padding: 10px 30px;\n        background-color: white;\n        box-shadow: 0 2px 5px rgba(0,0,0,0.1);\n        margin-bottom: 20px;\n      }\n      .navbar-logo img {\n        height: 60px;\n      }\n      .navbar-menu a, .navbar-menu .action-button {\n        margin-left: 20px;\n        color: #003366;\n        font-weight: bold;\n        text-decoration: none;\n        font-size: 16px;\n        cursor: pointer;\n        background: none;\n        border: none;\n        padding: 8px 16px;\n        border-radius: 20px;\n        transition: all 0.3s ease;\n      }\n      .navbar-menu a:hover, .navbar-menu .action-button:hover {\n        background-color: #558C7C;\n        color: white !important;\n        text-decoration: none;\n      }\n      .main-content {\n        max-width: 1200px;\n        margin: 0 auto;\n        padding: 20px;\n      }\n      .slick-slider {\n        border-radius: 20px;\n        overflow: hidden;\n        max-width: 1100px;\n        margin: 0 auto;\n        box-shadow: 0 10px 30px rgba(0,0,0,0.2);\n        background: white;\n      }\n      .slick-slide img {\n        width: 100%;\n        height: 500px;\n        object-fit: scale-down;\n        border-radius: 20px;\n        background-color: #f8f9fa;\n        padding: 10px;\n      }\n      .slick-dots {\n        bottom: 30px;\n      }\n      .slick-dots li button:before {\n        color: #003366;\n        font-size: 16px;\n        opacity: 0.6;\n      }\n      .slick-dots li.slick-active button:before {\n        color: #558C7C;\n        opacity: 1;\n        font-size: 18px;\n      }\n      .slick-prev, .slick-next {\n        z-index: 10;\n        width: 60px;\n        height: 60px;\n        background-color: rgba(85, 140, 124, 0.9);\n        border-radius: 50%;\n      }\n      .slick-prev {\n        left: 30px;\n      }\n      .slick-next {\n        right: 30px;\n      }\n      .slick-prev:before, .slick-next:before {\n        color: white;\n        font-size: 28px;\n      }\n    "))
  ),
  div(class = "navbar-custom",
      div(class = "navbar-logo",
          tags$img(src = "logo.png", alt = "Logo PUDC")),
      div(class = "navbar-menu",
          actionLink("nav_accueil", "🏠 Accueil"),
          actionLink("nav_financier", "💰 Suivi Financier"),
          actionLink("nav_technique", "🔧 Suivi Technique"),
          actionLink("nav_performance", "📊 Performance"),
          actionLink("nav_resume", "📋 Résumé"),
          actionLink("nav_indicateurs", "📊 Réalisation globale des indicateurs"),
          actionLink("nav_assistant", "🤖 Assistant IA")
      )
  ),
  div(class = "main-content",
      uiOutput("main_content")
  )
)
