# ===============================
# Chargement des librairies
# ===============================
library(shiny)
library(readxl)
library(dplyr)
library(plotly)
library(DT)
library(slickR)

# ===============================
# Palette de couleurs PUDC
# ===============================
couleurs_pudc <- list(
  bleu_fonce = "#003366",
  vert_pudc = "#558C7C",
  vert_clair = "#20c997",
  violet = "#6610f2",
  orange = "#fd7e14",
  rose = "#e83e8c",
  jaune = "#ffc107",
  vert = "#28a745"
)

# ===============================
# Fonctions utilitaires
# ===============================
# Formatage des montants en FCFA
format_fcfa <- function(x) {
  format(x, big.mark = " ", scientific = FALSE)
}

# Somme sécurisée
safe_sum <- function(x) {
  sum(x, na.rm = TRUE)
}

# ===============================
# Chargement des données
# ===============================
load_excel_data <- function() {
  tryCatch({
    cat("📁 Tentative de chargement des données Excel...\n")
    file_path <- "data/Base0.xlsx"
    if (!file.exists(file_path)) {
      cat("❌ Fichier Excel non trouvé :", file_path, "\n")
      cat("🔄 Utilisation des données de démonstration...\n")
      return(create_demo_data())
    }
    cat("✅ Fichier Excel trouvé, lecture en cours...\n")
    data_list <- list(
      budget_par_projet = read_excel(file_path, sheet = "budget_par_projet"),
      execution_budgetaire = read_excel(file_path, sheet = "execution_budgetaire"),
      par_secteur = read_excel(file_path, sheet = "Par Secteur"),
      par_financement = read_excel(file_path, sheet = "Par Financement"),
      par_region = read_excel(file_path, sheet = "Par Région"),
      evolution = read_excel(file_path, sheet = "Évolution"),
      observations = read_excel(file_path, sheet = "observations_alertes")
    )
    cat("✅ Données chargées avec succès !\n")
    return(data_list)
  }, error = function(e) {
    cat("❌ Erreur lors du chargement :", e$message, "\n")
    cat("🔄 Utilisation des données de démonstration...\n")
    return(create_demo_data())
  })
}

# Données de démonstration
create_demo_data <- function() {
  cat("🔧 Création des données de démonstration...\n")
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
        rep(5000000000, 4),
        rep(5715595470, 4),
        rep(9118488485, 4),
        rep(5176589987, 4),
        rep(9000000000, 2)
      ),
      Trimestre = c(
        1:4,
        1:4,
        1:4,
        1:4,
        3:4
      ),
      Montant_reel_decaissé = c(
        273548125, 529863366, 3093117478, 3149438963,
        23727679, 26290824, 282616234, 338937719,
        232083106, 257714630, 283347171, 339668656,
        160204280, 185835804, 211468345, 267789830,
        256398741, 312720226
      ),
      stringsAsFactors = FALSE
    ),
    par_secteur = data.frame(
      Secteur = c("Pistes Rurales", "Électrification", "Hydraulique", "Éducation", "Santé", "Reboisement", "Chaîne Lait", "Appui Institution"),
      `Taux de Réalisation (%)` = c(45, 48, 82, 0, 8, 143, 100, 75),
      Cible = c(790, 2092, 244, 21, 50, 100, 15, 20),
      Réalisé = c(326, 886, 101, 0, 4, 143, 15, 15),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    par_financement = data.frame(
      Source = c("BID", "BAD", "FSD", "BCI"),
      `Montant (%)` = c(45, 35, 15, 5),
      `Nombre de Projets` = c(12, 15, 8, 6),
      Couleur = c("#8884d8", "#82ca9d", "#ffc658", "#ff7300"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    par_region = data.frame(
      Région = c("Kolda", "Sédhiou", "Ziguinchor", "Louga", "Matam", "Tambacounda", "Kédougou", "Saint-Louis", "Kaffrine", "Kaolack"),
      `Nombre de Projets` = c(28, 25, 22, 20, 18, 16, 14, 12, 10, 8),
      `Taux de Réalisation (%)` = c(75, 68, 72, 45, 52, 63, 58, 41, 38, 35),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    evolution = data.frame(
      Mois = c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc"),
      `Taux de Réalisation (%)` = c(15, 28, 42, 55, 68, 75, 82, 88, 92, 95, 97, 100),
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
  return(demo_data)
}

