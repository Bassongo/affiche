# ===============================
# Pages de l'application
# ===============================

# Page d'accueil avec carrousel slickR
accueil_page <- function() {
  div(
    br(),
    column(12, align = "center",
           tags$img(src = "pudc_logo.png", height = "100px"),
           br(),
           div(style = "background-color: #558C7C; color: white; padding: 20px; border-radius: 10px; margin: 20px 0;",
               tags$h1("Tableau de bord de suivi",
                       style = "font-weight:bold; font-size: 36px; margin:0;"),
               tags$h2("PTBA DU PUDC 2025",
                       style = "margin:0; font-size: 24px;")
           ),
           br(),
           div(style = "margin: 40px auto; border-radius: 20px; overflow: hidden; box-shadow: 0 10px 30px rgba(0,0,0,0.2); max-width: 1100px; background: white; padding: 20px;",
               slickROutput("carousel", height = "500px", width = "100%")
           ),
           br(), br(),
           div(style = "border: 1px solid #003366; background-color: #f0f8ff; display: flex; align-items: center; justify-content: space-between; padding: 10px 20px; margin-bottom: 20px; border-radius: 10px;",
               tags$h3("Nos Réalisations",
                       style = "color: #003366; font-weight: bold; margin: 0;"),
               div(
                 tags$img(src = "miniature1.png", height = "30px", style = "margin-left: 10px; border-radius: 3px;"),
                 tags$img(src = "miniature2.png", height = "30px", style = "margin-left: 10px; border-radius: 3px;"),
                 tags$img(src = "miniature3.png", height = "30px", style = "margin-left: 10px; border-radius: 3px;")
               )
           ),
           tags$div(
             style = "margin: 80px 0 40px 0;",
             tags$h4("Galerie de nos Réalisations", style = "color: #003366; text-align: center; margin-bottom: 40px; font-size: 22px; font-weight: bold;"),
             fluidRow(
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature1.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition:transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Renforcement des infrastructures", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature2.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition:transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Électrification rurale", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature3.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition:transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Accès à l'eau potable", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature4.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition:transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Santé communautaire", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature5.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition:transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Hydraulique villageoise", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature6.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition:transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Autres réalisations", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               ))
             )
           ),
           br(), br(),
           div(style = "background-color: #003366; color: white; padding: 20px; font-size: 14px; border-radius: 10px;",
               "Programme d'Urgence de Développement Communautaire | Contact | Mentions légales",
               tags$br(),
               "Ministère du Développement communautaire, de la Solidarité nationale et de l'Équité territoriale"
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
          actionLink("nav_indicateurs", "📊 Réalisation globale des indicateurs"),
          actionLink("nav_assistant", "🤖 Assistant IA")
      )
  ),
  div(class = "main-content",
      uiOutput("main_content")
  )
)
