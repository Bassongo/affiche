# =============================================================================
# TABLEAU DE BORD PTBA PUDC 2025 - VERSION COMPLÈTE CORRIGÉE
# =============================================================================

# Chargement des librairies essentielles
library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(slickR)

source("modules/home_module.R")
source("modules/technique_module.R")
source("modules/performance_module.R")
source("modules/resume_module.R")

# =============================================================================
# STRUCTURE DES DONNÉES EXCEL IDENTIFIÉE
# =============================================================================
# 1. budget_par_projet: Composante, Volet, Source, Budget_FCFA (43 lignes)
# 2. execution_budgetaire: Projet, Source, Budget_PTBA_2025_FCFA, Trimestre, Montant_reel_decaissé (18 lignes)
# 3. Par Secteur: Secteur, Taux de Réalisation (%), Cible, Réalisé (8 lignes)
# 4. Par Financement: Source, Montant (%), Nombre de Projets, Couleur (4 lignes)
# 5. Par Région: Région, Nombre de Projets, Taux de Réalisation (%) (10 lignes)
# 6. Évolution: Mois, Taux de Réalisation (%) (12 lignes)
# 7. observations_alertes: Catégorie, Observation (4 lignes)

# =============================================================================
# FONCTION DE CHARGEMENT DES DONNÉES
# =============================================================================

load_excel_data <- function() {
  tryCatch({
    cat("📁 Tentative de chargement des données Excel...\n")
    cat("📂 Répertoire de travail actuel:", getwd(), "\n")
    
    # Vérifier si le fichier existe
    file_path <- "data/Base0.xlsx"
    if (!file.exists(file_path)) {
      cat("❌ Fichier Excel non trouvé à:", file_path, "\n")
      cat("📁 Contenu du répertoire actuel:\n")
      print(list.files())
      if (dir.exists("data")) {
        cat("📁 Contenu du dossier data:\n")
        print(list.files("data"))
      }
      cat("🔄 Utilisation des données de démonstration...\n")
      return(create_demo_data())
    }
    
    cat("✅ Fichier Excel trouvé, lecture en cours...\n")
    
    # Chargement de toutes les feuilles
    data_list <- list(
      budget_par_projet = read_excel(file_path, sheet = "budget_par_projet"),
      execution_budgetaire = read_excel(file_path, sheet = "execution_budgetaire"),
      par_secteur = read_excel(file_path, sheet = "Par Secteur"),
      par_financement = read_excel(file_path, sheet = "Par Financement"),
      par_region = read_excel(file_path, sheet = "Par Région"),
      evolution = read_excel(file_path, sheet = "Évolution"),
      observations = read_excel(file_path, sheet = "observations_alertes")
    )
    
    cat("✅ Données chargées avec succès!\n")
    cat("📊 Budget par projet:", nrow(data_list$budget_par_projet), "lignes\n")
    cat("💰 Exécution budgétaire:", nrow(data_list$execution_budgetaire), "lignes\n")
    
    # Afficher les premières lignes pour vérification
    if (nrow(data_list$execution_budgetaire) > 0) {
      cat("📋 Colonnes execution_budgetaire:", paste(names(data_list$execution_budgetaire), collapse = ", "), "\n")
      cat("📋 Première ligne execution_budgetaire:\n")
      print(data_list$execution_budgetaire[1, ])
    }
    
    return(data_list)
    
  }, error = function(e) {
    cat("❌ Erreur lors du chargement:", e$message, "\n")
    cat("🔄 Utilisation des données de démonstration...\n")
    return(create_demo_data())
  })
}

# Fonction pour créer des données de démonstration (EXACTEMENT comme vos vraies données)
create_demo_data <- function() {
  cat("🔧 Création des données de démonstration basées sur votre structure réelle...\n")
  
  demo_data <- list(
    budget_par_projet = data.frame(
      Composante_num = rep(1, 6),
      Composante = rep("Développement des Infrastructures de Base", 6),
      Volet_num = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6),
      Volet = c("PISTES RURALES", "ELECTRIFICATION", "HYDRAULIQUE", "EDUCATION", "SANTE", "REBOISEMENT"),
      Source = c("PUDC Budget Etat du Sénégal 2025", "PUDC Phase 2/BID", "PUDC Phase 2/BAD", "PUDC Phase 2/FSD", "PUDC Phase 2/BCI", "PUDC Budget Etat"),
      Budget_FCFA = c(244000000, 942163119, 1913813647, 2476162858, 850000000, 1200000000),
      stringsAsFactors = FALSE
    ),
    execution_budgetaire = data.frame(
      Projet = c(
        rep("PUDC-Phase2/ Budget 2025 Etat", 4),
        rep("PUDC-Phase 2/BID", 4), 
        rep("PUDC-Phase 2/BAD", 4),
        rep("PUDC-Phase 2/FSD", 4),
        rep("PUDC-Phase 2 /P.2000 VILLAGES", 2)
      ),
      Source = c(
        rep("ETAT", 4),
        rep("BID", 4),
        rep("BAD", 4), 
        rep("FSD", 4),
        rep("ETAT", 2)
      ),
      Budget_PTBA_2025_FCFA = c(
        rep(5000000000, 4),    # ETAT
        rep(5715595470, 4),    # BID
        rep(9118488485, 4),    # BAD
        rep(5176589987, 4),    # FSD
        rep(9000000000, 2)     # P.2000 VILLAGES
      ),
      Trimestre = c(
        1:4,  # ETAT
        1:4,  # BID
        1:4,  # BAD
        1:4,  # FSD
        3:4   # P.2000 VILLAGES
      ),
      Montant_reel_decaissé = c(
        # PUDC-Phase2/ Budget 2025 Etat
        273548125, 529863366, 3093117478, 3149438963,
        # PUDC-Phase 2/BID  
        23727679, 26290824, 282616234, 338937719,
        # PUDC-Phase 2/BAD
        232083106, 257714630, 283347171, 339668656,
        # PUDC-Phase 2/FSD
        160204280, 185835804, 211468345, 267789830,
        # PUDC-Phase 2 /P.2000 VILLAGES
        256398741, 312720226
      ),
      stringsAsFactors = FALSE
    ),
    par_secteur = data.frame(
      Secteur = c("Pistes Rurales", "Électrification", "Hydraulique", "Éducation", "Santé", "Reboisement", "Chaîne Lait", "Appui Institution"),
      "Taux de Réalisation (%)" = c(45, 48, 82, 0, 8, 143, 100, 75),
      Cible = c(790, 2092, 244, 21, 50, 100, 15, 20),
      Réalisé = c(326, 886, 101, 0, 4, 143, 15, 15),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    par_financement = data.frame(
      Source = c("BID", "BAD", "FSD", "BCI"),
      "Montant (%)" = c(45, 35, 15, 5),
      "Nombre de Projets" = c(12, 15, 8, 6),
      Couleur = c("#8884d8", "#82ca9d", "#ffc658", "#ff7300"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    par_region = data.frame(
      Région = c("Kolda", "Sédhiou", "Ziguinchor", "Louga", "Matam", "Tambacounda", "Kédougou", "Saint-Louis", "Kaffrine", "Kaolack"),
      "Nombre de Projets" = c(28, 25, 22, 20, 18, 16, 14, 12, 10, 8),
      "Taux de Réalisation (%)" = c(75, 68, 72, 45, 52, 63, 58, 41, 38, 35),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    evolution = data.frame(
      Mois = c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc"),
      "Taux de Réalisation (%)" = c(15, 28, 42, 55, 68, 75, 82, 88, 92, 95, 97, 100),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    observations = data.frame(
      Catégorie = c("Secteurs en Retard", "Contrats Résiliés", "Performances Excellentes", "Régions Prioritaires"),
      Observation = c(
        "Éducation (0%), Santé (8%) nécessitent une attention urgente",
        "Plusieurs contrats résiliés dans les secteurs santé et éducation",
        "Reboisement (143%), Chaîne Lait (100%) dépassent les objectifs",
        "Kolda, Sédhiou, Ziguinchor concentrent le plus de projets"
      ),
      stringsAsFactors = FALSE
    )
  )
  
  cat("✅ Données de démonstration créées avec VOTRE structure réelle:\n")
  cat("   - ", nrow(demo_data$execution_budgetaire), "lignes d'exécution budgétaire\n")
  cat("   - ", length(unique(demo_data$execution_budgetaire$Projet)), "projets uniques\n")
  cat("   - Projets:\n")
  for(projet in unique(demo_data$execution_budgetaire$Projet)) {
    cat("     *", projet, "\n")
  }
  
  return(demo_data)
}

# =============================================================================
# PAGES DE L'APPLICATION
# =============================================================================

# Page d'accueil avec carrousel slickR
accueil_page <- function() {
  div(
    br(),
    column(12, align = "center",
           # Logo PUDC
           tags$img(src = "pudc_logo.png", height = "100px"),
           br(),
           
           # Titre principal avec style PUDC
           div(style = "background-color: #558C7C; color: white; padding: 20px; border-radius: 10px; margin: 20px 0;",
               tags$h1("Tableau de bord de suivi", 
                       style = "font-weight:bold; font-size: 36px; margin:0;"),
               tags$h2("PTBA DU PUDC 2025", 
                       style = "margin:0; font-size: 24px;")
           ),
           
           br(),
           
           # Carrousel slickR des réalisations PUDC (cadre encore plus grand)
           div(style = "margin: 40px auto; border-radius: 20px; overflow: hidden; box-shadow: 0 10px 30px rgba(0,0,0,0.2); max-width: 1100px; background: white; padding: 20px;",
               slickROutput("carousel", height = "500px", width = "100%")
           ),
           
           br(), br(),
           
           # Section "Nos Réalisations" avec miniatures statiques
           div(style = "border: 1px solid #003366; background-color: #f0f8ff; display: flex; align-items: center; justify-content: space-between; padding: 10px 20px; margin-bottom: 20px; border-radius: 10px;",
               tags$h3("Nos Réalisations", 
                       style = "color: #003366; font-weight: bold; margin: 0;"),
               div(
                 tags$img(src = "miniature1.png", height = "30px", style = "margin-left: 10px; border-radius: 3px;"),
                 tags$img(src = "miniature2.png", height = "30px", style = "margin-left: 10px; border-radius: 3px;"),
                 tags$img(src = "miniature3.png", height = "30px", style = "margin-left: 10px; border-radius: 3px;")
               )
           ),
           
           # Galerie avec titres (espacée et descendue)
           tags$div(
             style = "margin: 80px 0 40px 0;",
             tags$h4("Galerie de nos Réalisations", style = "color: #003366; text-align: center; margin-bottom: 40px; font-size: 22px; font-weight: bold;"),
             fluidRow(
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature1.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition: transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Renforcement des infrastructures", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature2.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition: transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Électrification rurale", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature3.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition: transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Accès à l'eau potable", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature4.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition: transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Santé communautaire", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature5.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition: transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Hydraulique villageoise", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               )),
               column(2, div(style = "margin-bottom: 20px;",
                             tags$img(src = "miniature6.png", width = "100%", style = "border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.15); transition: transform 0.3s ease; cursor: pointer;", onclick = "this.style.transform = this.style.transform ? '' : 'scale(1.08)'"),
                             tags$p("Autres réalisations", style = "text-align:center; font-size:14px; margin-top: 12px; color: #003366; font-weight: 600; line-height: 1.3;")
               ))
             )
           ),
           
           br(), br(),
           
           # Footer
           div(style = "background-color: #003366; color: white; padding: 20px; font-size: 14px; border-radius: 10px;",
               "Programme d'Urgence de Développement Communautaire | Contact | Mentions légales",
               tags$br(),
               "Ministère du Développement communautaire, de la Solidarité nationale et de l'Équité territoriale"
           )
    )
  )
}

# Page suivi financier COMPLÈTE
financier_page <- function() {
  div(
    # Titre de la page
    div(style = "background: linear-gradient(135deg, #003366, #558C7C); color: white; padding: 2rem; margin-bottom: 2rem; border-radius: 15px; text-align: center;",
        tags$h2("💰 Suivi Financier", style = "margin: 0; font-size: 2.5rem; font-weight: bold;"),
        tags$p("Analyse des décaissements et répartition budgétaire", style = "margin: 0.5rem 0 0 0; font-size: 1.2rem; opacity: 0.9;")
    ),
    
    # Contrôles
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
    
    # Statistiques rapides
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
    
    # Première ligne de graphiques
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
    
    # Tableau interactif
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

# Pages autres (structures de base)
indicateurs_page <- function() {
  div(
    tags$h2("📊 Indicateurs", style = "color: #003366; text-align: center; margin: 20px 0;"),
    div(style = "background: white; padding: 2rem; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
        tags$h4("🚧 Page en cours de développement", style = "color: #558C7C; text-align: center;")
    )
  )
}

assistant_page <- function() {
  div(
    tags$h2("🤖 Assistant IA", style = "color: #003366; text-align: center; margin: 20px 0;"),
    div(style = "background: white; padding: 2rem; border-radius: 10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);",
        tags$h4("🚧 Page en cours de développement", style = "color: #558C7C; text-align: center;")
    )
  )
}

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

ui <- fluidPage(
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      body { 
        background-color: #f8f9fa !important; 
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      .navbar-custom { 
        display: flex; 
        align-items: center; 
        justify-content: space-between; 
        padding: 10px 30px; 
        background-color: white;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      
      .navbar-logo img { 
        height: 60px; 
      }
      
      .navbar-menu a, .navbar-menu .action-button {
        margin-left: 20px;
        color: #003366;
        font-weight: bold;
        text-decoration: none;
        font-size: 16px;
        cursor: pointer;
        background: none;
        border: none;
        padding: 8px 16px;
        border-radius: 20px;
        transition: all 0.3s ease;
      }
      
      .navbar-menu a:hover, .navbar-menu .action-button:hover {
        background-color: #558C7C;
        color: white !important;
        text-decoration: none;
      }
      
      .main-content {
        max-width: 1200px;
        margin: 0 auto;
        padding: 20px;
      }
      
      /* Personnalisation du carrousel slickR */
      .slick-slider {
        border-radius: 20px;
        overflow: hidden;
        max-width: 1100px;
        margin: 0 auto;
        box-shadow: 0 10px 30px rgba(0,0,0,0.2);
        background: white;
      }
      
      .slick-slide img {
        width: 100%;
        height: 500px;
        object-fit: scale-down;
        border-radius: 20px;
        background-color: #f8f9fa;
        padding: 10px;
      }
      
      .slick-dots {
        bottom: 30px;
      }
      
      .slick-dots li button:before {
        color: #003366;
        font-size: 16px;
        opacity: 0.6;
      }
      
      .slick-dots li.slick-active button:before {
        color: #558C7C;
        opacity: 1;
        font-size: 18px;
      }
      
      .slick-prev, .slick-next {
        z-index: 10;
        width: 60px;
        height: 60px;
        background-color: rgba(85, 140, 124, 0.9);
        border-radius: 50%;
      }
      
      .slick-prev {
        left: 30px;
      }
      
      .slick-next {
        right: 30px;
      }
      
      .slick-prev:before, .slick-next:before {
        color: white;
        font-size: 28px;
      }
    "))
  ),
  
  # Barre de navigation
  div(class = "navbar-custom",
      div(class = "navbar-logo", 
          tags$img(src = "logo.png", alt = "Logo PUDC")),
      div(class = "navbar-menu",
        actionLink("nav_accueil", "🏠 Accueil"),
        actionLink("nav_technique", "🔧 Suivi Technique"),
        actionLink("nav_performance", "📈 Performance PM"),
        actionLink("nav_financier", "💰 Suivi Financier"),
        actionLink("nav_indicateurs", "📊 Indicateurs"),
        actionLink("nav_resume", "📄 Résumé"),
        actionLink("nav_assistant", "🤖 Assistant IA")
      )
  ),
  
  # Contenu principal
  div(class = "main-content",
      uiOutput("main_content")
  )
)

# =============================================================================
# SERVEUR
# =============================================================================

server <- function(input, output, session) {
  
  # Chargement des données au démarrage (avec forçage)
  app_data <- reactive({
    cat("🔄 Reactive app_data() appelé\n")
    data <- load_excel_data()
    cat("📊 Données chargées, execution_budgetaire a", nrow(data$execution_budgetaire), "lignes\n")
    return(data)
  })
  
  # Force l'initialisation des données au démarrage
  observe({
    cat("🚀 Observer de démarrage appelé\n")
    isolate({
      data <- app_data()
      cat("✅ Données forcées au démarrage\n")
    })
  })
  
  # Navigation
  current_page <- reactiveVal("accueil")
  
  observeEvent(input$nav_accueil, { current_page("accueil") })
  observeEvent(input$nav_technique, { current_page("technique") })
  observeEvent(input$nav_performance, { current_page("performance") })
  observeEvent(input$nav_financier, { current_page("financier") })
  observeEvent(input$nav_indicateurs, { current_page("indicateurs") })
  observeEvent(input$nav_resume, { current_page("resume") })
  observeEvent(input$nav_assistant, { current_page("assistant") })
  
  # Affichage de la page courante
  output$main_content <- renderUI({
    switch(current_page(),
           "accueil" = homeUI("home"),
           "technique" = techniqueUI("tech"),
           "performance" = performanceUI("perf"),
           "financier" = financier_page(),
           "indicateurs" = indicateurs_page(),
           "resume" = resumeUI("res"),
           "assistant" = assistant_page()
    )
  })
  

  homeServer("home")
  techniqueServer("tech")
  performanceServer("perf")
  resumeServer("res")
  
  # Variable réactive pour stocker les projets
  projets_disponibles <- reactiveVal(NULL)
  
  # Mise à jour du menu projets (version simplifiée - les choix sont déjà dans l'UI)
  observe({
    cat("🔍 Observer du menu projets (version simplifiée)\n")
    cat("✅ Menu initialisé avec choix statiques dans l'UI\n")
    
    # Vérifier que les données correspondent aux choix statiques
    data <- app_data()
    if (!is.null(data) && "execution_budgetaire" %in% names(data)) {
      execution_data <- data$execution_budgetaire
      projets_reels <- unique(execution_data$Projet)
      cat("📋 Projets dans les données:", paste(projets_reels, collapse = ", "), "\n")
      projets_disponibles(projets_reels)
    }
  })
  
  # Observer pour surveiller la sélection
  observe({
    if (!is.null(input$projet_select) && input$projet_select != "") {
      cat("🎯 Projet sélectionné dans le menu:", input$projet_select, "\n")
    } else {
      cat("⚠️ Menu toujours vide, projet sélectionné:", input$projet_select, "\n")
    }
  })
  
  # Graphique barres trimestre (version utilisant input$projet_select directement)
  output$bar_trimestre_financier <- renderPlotly({
    cat("📊 Génération du graphique en barres\n")
    
    # Récupérer les données
    data <- app_data()
    if (is.null(data) || !"execution_budgetaire" %in% names(data)) {
      return(plot_ly() %>% layout(title = "Données non disponibles"))
    }
    
    execution_data <- data$execution_budgetaire
    if (!is.data.frame(execution_data)) {
      validate(HTML("<b>Erreur :</b> le jeu de donn\u00e9es n'est pas valide."))
    }
    
    # Utiliser le projet sélectionné ou le premier par défaut
    projet_selectionne <- input$projet_select
    if (is.null(projet_selectionne) || projet_selectionne == "") {
      projet_selectionne <- "PUDC-Phase2/ Budget 2025 Etat"  # Valeur par défaut
      cat("🔧 Utilisation du projet par défaut\n")
    }
    
    cat("🎯 Projet pour le graphique:", projet_selectionne, "\n")
    
    # Filtrer par projet
    df_filtered <- execution_data %>%
      filter(Projet == projet_selectionne) %>%
      arrange(Trimestre)
    
    cat("🔍 Données filtrées:", nrow(df_filtered), "lignes\n")
    
    if (nrow(df_filtered) == 0) {
      return(plot_ly() %>% layout(title = paste("Aucune donnée pour:", projet_selectionne)))
    }
    
    # Créer le graphique
    p <- plot_ly(df_filtered, 
                 x = ~factor(Trimestre), 
                 y = ~Montant_reel_decaissé, 
                 type = "bar",
                 marker = list(color = '#558C7C', line = list(color = '#003366', width = 1)),
                 text = ~paste0(round(Montant_reel_decaissé/1000000, 1), "M"),
                 textposition = 'outside',
                 hovertemplate = paste(
                   '<b>Trimestre %{x}</b><br>',
                   'Montant: %{y:,.0f} FCFA<br>',
                   'Projet: ', projet_selectionne, '<br>',
                   '<extra></extra>'
                 )) %>%
      layout(
        title = list(
          text = paste("Décaissements -", projet_selectionne),
          font = list(size = 14, color = '#003366')
        ),
        xaxis = list(title = "Trimestre", tickfont = list(size = 12)),
        yaxis = list(title = "Montant (FCFA)", tickformat = ".2s", tickfont = list(size = 12)),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
    
    cat("✅ Graphique créé\n")
    return(p)
  })
  
  # Camembert budget par volet (version avec légende optimisée)
  output$pie_budget_volet <- renderPlotly({
    req(app_data())
    
    budget_data <- app_data()$budget_par_projet
    if (!is.data.frame(budget_data)) {
      validate(HTML("<b>Erreur :</b> donn\u00e9es budgetaires invalides."))
    }
    df_volet <- budget_data %>%
      group_by(Volet) %>%
      summarise(Budget_Total = sum(Budget_FCFA, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Budget_Total)) %>%
      head(8)  # Limiter à 8 volets pour éviter trop de légendes
    
    colors <- c('#003366', '#558C7C', '#20c997', '#6610f2', '#fd7e14', '#e83e8c', '#ffc107', '#28a745')
    
    p <- plot_ly(df_volet, 
                 labels = ~Volet, 
                 values = ~Budget_Total, 
                 type = 'pie',
                 textinfo = 'percent',  # Afficher seulement les pourcentages sur le camembert
                 textposition = 'inside',
                 textfont = list(size = 12, color = 'white'),
                 marker = list(
                   colors = colors[1:nrow(df_volet)],
                   line = list(color = '#FFFFFF', width = 2)
                 ),
                 hovertemplate = paste(
                   '<b>%{label}</b><br>',
                   'Budget: %{value:,.0f} FCFA<br>',
                   'Pourcentage: %{percent}<br>',
                   '<extra></extra>'
                 )) %>%
      layout(
        title = list(text = "", font = list(size = 16)),
        showlegend = TRUE,
        legend = list(
          orientation = "v",
          x = 1.02,         # Position à droite
          y = 0.5,          # Centré verticalement
          font = list(size = 9),  # Police plus petite
          bgcolor = 'rgba(255,255,255,0.8)',  # Fond semi-transparent
          bordercolor = 'rgba(0,0,0,0.2)',
          borderwidth = 1
        ),
        margin = list(l = 20, r = 120, t = 50, b = 20),  # Plus de marge à droite pour la légende
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # Alternative : Tableau HTML simple (sans DataTables)
  output$table_execution_simple <- renderTable({
    req(app_data())
    
    execution_data <- app_data()$execution_budgetaire
    
    df_table <- execution_data %>%
      mutate(
        `Budget (M FCFA)` = round(Budget_PTBA_2025_FCFA / 1000000, 1),
        `Décaissé (M FCFA)` = round(get("Montant_reel_decaissé") / 1000000, 1),
        `Taux (%)` = round((get("Montant_reel_decaissé") / Budget_PTBA_2025_FCFA) * 100, 1)
      ) %>%
      select(Projet, Source, Trimestre, `Budget (M FCFA)`, `Décaissé (M FCFA)`, `Taux (%)`)
    
    df_table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
  
  # Garder aussi la version DataTables pour test
  # Tableau d'exécution (version ultra-simplifiée)
  output$table_execution_complete <- DT::renderDataTable({
    req(app_data())
    
    execution_data <- app_data()$execution_budgetaire
    
    # Préparer les données de façon très simple
    df_table <- execution_data %>%
      mutate(
        `Budget (M FCFA)` = round(Budget_PTBA_2025_FCFA / 1000000, 1),
        `Décaissé (M FCFA)` = round(get("Montant_reel_decaissé") / 1000000, 1),
        `Taux (%)` = round((get("Montant_reel_decaissé") / Budget_PTBA_2025_FCFA) * 100, 1)
      ) %>%
      select(Projet, Source, Trimestre, `Budget (M FCFA)`, `Décaissé (M FCFA)`, `Taux (%)`)
    
    # DataTable ultra-basique pour éviter les erreurs Ajax
    DT::datatable(
      df_table,
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        searching = TRUE,
        ordering = TRUE,
        info = TRUE,
        paging = TRUE
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Taux (%)',
        backgroundColor = DT::styleInterval(
          cuts = c(50, 80),
          values = c("#ffe6e6", "#fff2e6", "#e6ffe6")
        )
      )
  }, server = FALSE)  # Changement à server = FALSE
  
  # Statistiques réactives (version sécurisée)
  output$total_budget <- renderText({
    req(app_data())
    tryCatch({
      total <- sum(app_data()$execution_budgetaire$Budget_PTBA_2025_FCFA, na.rm = TRUE)
      paste0(round(total/1000000000, 1), " Md")
    }, error = function(e) {
      "N/A"
    })
  })
  
  output$total_decaisse <- renderText({
    req(app_data())
    tryCatch({
      data <- app_data()$execution_budgetaire
      if ("Montant_reel_decaissé" %in% names(data)) {
        total <- sum(data[["Montant_reel_decaissé"]], na.rm = TRUE)
        paste0(round(total/1000000000, 1), " Md")
      } else {
        "N/A"
      }
    }, error = function(e) {
      "N/A"
    })
  })
  
  output$taux_execution <- renderText({
    req(app_data())
    tryCatch({
      data <- app_data()$execution_budgetaire
      if ("Montant_reel_decaissé" %in% names(data) && "Budget_PTBA_2025_FCFA" %in% names(data)) {
        taux <- (sum(data[["Montant_reel_decaissé"]], na.rm = TRUE) / sum(data$Budget_PTBA_2025_FCFA, na.rm = TRUE)) * 100
        paste0(round(taux, 1), "%")
      } else {
        "N/A"
      }
    }, error = function(e) {
      "N/A"
    })
  })
  
  output$nb_projets <- renderText({
    req(app_data())
    tryCatch({
      data <- app_data()$execution_budgetaire
      if ("Projet" %in% names(data)) {
        nb <- length(unique(data$Projet))
        as.character(nb)
      } else {
        "N/A"
      }
    }, error = function(e) {
      "N/A"
    })
  })
  
  # Téléchargement CSV
  output$download_execution_csv <- downloadHandler(
    filename = function() {
      paste0("execution_budgetaire_PUDC_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_to_export <- app_data()$execution_budgetaire
      write.csv(data_to_export, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

# =============================================================================
# LANCEMENT DE L'APPLICATION
# =============================================================================

cat("🚀 Démarrage du Tableau de Bord PTBA PUDC 2025\n")
cat("📂 Structure Excel analysée et intégrée\n")
cat("💰 Onglet Suivi Financier développé\n\n")

shinyApp(ui = ui, server = server)