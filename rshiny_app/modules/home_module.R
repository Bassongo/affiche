homeUI <- function(id) {
  ns <- NS(id)
  tagList(
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
                         div(class = "pudc-text", "Le PUDC est un programme ambitieux initié par le Gouvernement du Sénégal pour accélérer le développement des communautés rurales."),
                         div(class = "pudc-highlight", "\"Une approche intégrée pour un développement durable et inclusif des territoires sénégalais\""),
                         div(class = "pudc-stats",
                             div(class = "pudc-stat", span(class = "pudc-stat-number", "14"), span(class = "pudc-stat-label", "Régions")),
                             div(class = "pudc-stat", span(class = "pudc-stat-number", "557"), span(class = "pudc-stat-label", "Communes")),
                             div(class = "pudc-stat", span(class = "pudc-stat-number", "8M"), span(class = "pudc-stat-label", "Bénéficiaires"))
                         )
                     ),
                     div(id = "pudc-missions", class = "pudc-tab-content",
                         div(class = "pudc-title", "Missions du PUDC"),
                         div(class = "pudc-text", "Le PUDC a pour mission principale de contribuer à l'amélioration des conditions de vie.")),
                     div(id = "pudc-objectifs", class = "pudc-tab-content",
                         div(class = "pudc-title", "Objectifs Stratégiques"),
                         div(class = "pudc-text", "...")),
                     div(id = "pudc-resultats", class = "pudc-tab-content",
                         div(class = "pudc-title", "Résultats atteints"),
                         div(class = "pudc-text", "...")
                     )
                 )
             ),
             br(),
             div(class = "carousel-container", slickROutput(ns("carousel"), width = "80%", height = "400px")),
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

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    images <- c("piste.png", "electrification.png", "poste_sante.png", "real.png")
    output$carousel <- renderSlickR({
      slickR(images, slideId = 'car1') +
        settings(autoplay = TRUE, autoplaySpeed = 4000, arrows = TRUE, dots = TRUE, infinite = TRUE)
    })
  })
}
