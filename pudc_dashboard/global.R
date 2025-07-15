# =============================================================================
# GLOBAL.R - CONFIGURATION ET DONNÉES PUDC 2025 - VERSION CORRIGÉE
# =============================================================================

# =============================================================================
# CHARGEMENT DES PACKAGES DANS L'ORDRE OPTIMAL POUR ÉVITER LES CONFLITS
# =============================================================================

# Packages de base
library(shiny)
library(readxl)
library(dplyr)

# Charger plotly AVANT httr pour éviter les conflits config/layout
library(plotly)

# Autres packages
library(DT)
library(slickR)

# Packages IA en dernier (si utilisés)
if (requireNamespace("httr", quietly = TRUE)) library(httr)
if (requireNamespace("jsonlite", quietly = TRUE)) library(jsonlite)
if (requireNamespace("tidyr", quietly = TRUE)) library(tidyr)

# =============================================================================
# RÉSOLUTION EXPLICITE DES CONFLITS CRITIQUES
# =============================================================================

# Forcer l'utilisation des bonnes fonctions pour éviter l'erreur "request"
config <- plotly::config
layout <- plotly::layout
filter <- dplyr::filter
select <- dplyr::select
arrange <- dplyr::arrange
mutate <- dplyr::mutate
summarise <- dplyr::summarise
group_by <- dplyr::group_by

# Confirmer la résolution
cat("✅ Conflits résolus - Plotly et dplyr prioritaires\n")

# =============================================================================
# PALETTE DE COULEURS PUDC (VOTRE VERSION ÉTENDUE)
# =============================================================================

couleurs_pudc <- list(
  # Vos couleurs originales
  bleu_fonce = "#003366",
  vert_pudc = "#558C7C", 
  vert_clair = "#20c997",
  violet = "#6610f2",
  orange = "#fd7e14",
  rose = "#e83e8c",
  jaune = "#ffc107",
  vert = "#28a745",
  
  # Ajouts pour compatibilité avec le code modulaire
  primaire = "#003366",
  secondaire = "#558C7C",
  succes = "#28a745",
  attention = "#ffc107",
  danger = "#dc3545",
  info = "#007bff",
  gradient_principal = "linear-gradient(135deg, #003366, #558C7C)",
  palette_secteurs = c('#003366', '#558C7C', '#20c997', '#6610f2', '#fd7e14', '#e83e8c', '#ffc107', '#28a745')
)

# =============================================================================
# FONCTIONS UTILITAIRES (VOS FONCTIONS + NOUVELLES)
# =============================================================================

# Vos fonctions originales
format_fcfa <- function(x) {
  format(x, big.mark = " ", scientific = FALSE)
}

safe_sum <- function(x) {
  sum(x, na.rm = TRUE)
}

# Nouvelles fonctions utilitaires
format_montant <- function(montant) {
  if (is.na(montant) || montant == 0) return("0")
  
  if (montant >= 1000000000) {
    return(paste0(round(montant / 1000000000, 1), " Md"))
  } else if (montant >= 1000000) {
    return(paste0(round(montant / 1000000, 1), " M"))
  } else if (montant >= 1000) {
    return(paste0(round(montant / 1000, 1), " K"))
  } else {
    return(as.character(round(montant, 0)))
  }
}

calc_taux_realisation <- function(realisation, cible) {
  if (is.na(cible) || cible == 0) return(0)
  return(round((realisation / cible) * 100, 1))
}

get_couleur_performance <- function(taux) {
  ifelse(taux >= 75, couleurs_pudc$succes,
         ifelse(taux >= 50, couleurs_pudc$attention, couleurs_pudc$danger))
}

# Opérateur de concaténation
`%+%` <- function(a, b) paste0(a, b)

# =============================================================================
# CHARGEMENT DES DONNÉES (VOTRE FONCTION CORRIGÉE)
# =============================================================================

load_excel_data <- function() {
  tryCatch({
    cat("📁 Tentative de chargement des données Excel...\n")
    
    # Votre chemin + nouveau chemin
    file_paths <- c(
      "data/Base0.xlsx",
      "PUDC_SUIVI TRIMESTRE  DU PTBA 2025 VF.xlsx"
    )
    
    file_path <- NULL
    for (path in file_paths) {
      if (file.exists(path)) {
        file_path <- path
        break
      }
    }
    
    if (is.null(file_path)) {
      cat("❌ Fichier Excel non trouvé dans les emplacements:", paste(file_paths, collapse = ", "), "\n")
      cat("🔄 Utilisation des données de démonstration...\n")
      return(create_demo_data())
    }
    
    cat("✅ Fichier Excel trouvé:", file_path, "\n")
    
    # Tentative de lecture avec vos feuilles + nouvelles feuilles
    data_list <- tryCatch({
      list(
        budget_par_projet = read_excel(file_path, sheet = "budget_par_projet"),
        execution_budgetaire = read_excel(file_path, sheet = "execution_budgetaire"),
        par_secteur = read_excel(file_path, sheet = "Par Secteur"),
        par_financement = read_excel(file_path, sheet = "Par Financement"),
        par_region = read_excel(file_path, sheet = "Par Région"),
        evolution = read_excel(file_path, sheet = "Évolution"),
        observations = read_excel(file_path, sheet = "observations_alertes")
      )
    }, error = function(e) {
      cat("⚠️ Feuilles originales non trouvées, tentative avec nouvelles feuilles...\n")
      list(
        budget_par_projet = read_excel(file_path, sheet = "Repartit budget PTBA par projet"),
        execution_budgetaire = read_excel(file_path, sheet = "Execution budgetaire PTBA 2025"),
        performance_globale = read_excel(file_path, sheet = "PERFORMANCE GLOBALE 2025"),
        par_secteur = create_demo_data()$par_secteur,
        par_financement = create_demo_data()$par_financement,
        par_region = create_demo_data()$par_region,
        evolution = create_demo_data()$evolution,
        observations = create_demo_data()$observations
      )
    })
    
    cat("✅ Données chargées avec succès !\n")
    return(data_list)
    
  }, error = function(e) {
    cat("❌ Erreur lors du chargement :", e$message, "\n")
    cat("🔄 Utilisation des données de démonstration...\n")
    return(create_demo_data())
  })
}

# =============================================================================
# DONNÉES DE DÉMONSTRATION (VOTRE VERSION ÉTENDUE)
# =============================================================================

create_demo_data <- function() {
  cat("🔧 Création des données de démonstration...\n")
  
  demo_data <- list(
    # Vos données originales
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
        rep("ETAT", 4), rep("BID", 4), rep("BAD", 4), rep("FSD", 4), rep("ETAT", 2)
      ),
      Budget_PTBA_2025_FCFA = c(
        rep(5000000000, 4), rep(5715595470, 4), rep(9118488485, 4), rep(5176589987, 4), rep(9000000000, 2)
      ),
      Trimestre = c(1:4, 1:4, 1:4, 1:4, 3:4),
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
    ),
    
    # Nouvelles données pour les indicateurs
    secteurs_indicateurs = data.frame(
      secteur = c("PISTES RURALES", "ELECTRIFICATION RURALE", "Volet Hydraulique", 
                  "Volet Education", "Volet Santé", "Environnement et Economie verte"),
      cible_annuelle = c(90, 54, 13, 17, 14, 585),
      realisation_annuelle = c(105.62, 0, 6, 0, 0, 430),
      cible_globale = c(790, 2092, 244, 21, 50, 1000),
      realisation_globale = c(325.92, 844, 101, 0, 4, 430),
      taux_realisation = c(117.4, 0, 46.2, 0, 0, 73.5),
      observations = c(
        "Objectif dépassé avec 117.4% de réalisation",
        "Retard important - 0% réalisé sur la cible annuelle",
        "Avancement modéré - 46.2% de la cible",
        "Pas de réalisation - secteur en difficulté",
        "Pas de réalisation - secteur en difficulté",
        "Bonne performance - 73.5% de réalisation"
      ),
      stringsAsFactors = FALSE
    ),
    
    regions_indicateurs = data.frame(
      region = c("Thiès", "Sédhiou", "Kolda", "Louga", "Ziguinchor", "Diourbel", "Fatick"),
      nb_projets = c(4, 7, 7, 7, 1, 6, 4),
      total_realisation = c(36.5, 181.8, 250.12, 39, 15, 14, 5),
      taux_realisation = c(78, 68, 72, 45, 52, 41, 38),
      secteur_principal = c("PISTES RURALES", "ELECTRIFICATION RURALE", "Volet Hydraulique",
                            "Volet Hydraulique", "ELECTRIFICATION RURALE", "Volet Hydraulique", "Volet Hydraulique"),
      observations = c(
        "Région performante avec focus sur les pistes rurales",
        "Forte activité en électrification rurale",
        "Concentration importante de projets hydrauliques",
        "Région avec potentiel d'amélioration",
        "Électrification rurale en cours",
        "Diversité de projets hydrauliques",
        "Activité hydraulique modérée"
      ),
      stringsAsFactors = FALSE
    )
  )
  
  cat("✅ Données de démonstration créées\n")
  return(demo_data)
}

# =============================================================================
# COORDONNÉES GÉOGRAPHIQUES DU SÉNÉGAL
# =============================================================================

coords_senegal <- data.frame(
  region = c("Thiès", "Sédhiou", "Kolda", "Louga", "Ziguinchor", "Tamba", 
             "Kédougou", "Diourbel", "Fatick", "Kaffrine", "Kaolack", "Matam", "Saint Louis"),
  lat = c(14.8, 12.7, 12.9, 15.6, 12.6, 13.8, 12.6, 14.7, 14.3, 14.1, 14.2, 15.7, 16.0),
  lon = c(-16.9, -15.6, -14.9, -15.4, -16.3, -13.7, -12.2, -16.2, -16.4, -15.6, -16.1, -13.3, -16.5),
  stringsAsFactors = FALSE
)

# =============================================================================
# FONCTIONS UTILITAIRES SUPPLÉMENTAIRES
# =============================================================================

# Calcul du taux de réalisation
calc_taux_realisation <- function(realisation, cible) {
  if (is.na(cible) || cible == 0) return(0)
  return(round((realisation / cible) * 100, 1))
}

# Couleur selon le taux de performance
get_couleur_performance <- function(taux) {
  ifelse(taux >= 75, couleurs_pudc$succes,
         ifelse(taux >= 50, couleurs_pudc$attention, couleurs_pudc$danger))
}

# Opérateur de concaténation pour les messages
`%+%` <- function(a, b) paste0(a, b)

cat("✅ Configuration globale chargée - PUDC 2025\n")