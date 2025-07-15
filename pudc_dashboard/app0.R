library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)
library(plotly)
library(slickR)
library(DT)

# === 📦 Chargement des données Excel ===
f <- "C:/Users/LENOVO/Desktop/Tableau_DB/www/Suivi technique.xlsx"
sheets <- excel_sheets(f)
list_tech <- lapply(sheets, function(s) {
  df <- read_excel(f, sheet = s)
  df$projet <- s
  df
})
df_tech <- bind_rows(list_tech)

# Harmonisation des noms de colonnes
colnames(df_tech) <- c("Volets", "Objectifs_annuels", "Objectif", "Réalisation", "Observations", "projet")

# Ajout des sous-volets
df_tech <- df_tech %>% mutate(SousVolets = Volets)

# === UI ===
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
/* Styles existants */
body { background-color: white !important; }
.navbar-custom { display: flex; align-items: center; justify-content: space-between; padding: 10px 30px; background-color: white; }
.navbar-logo img { height: 60px; }
.navbar-menu a {
  margin-left: 20px;
  color: #003366;
  font-weight: bold;
  text-decoration: none;
  font-size: 16px;
}
.navbar-menu a:hover, .navbar-menu a.active {
  border-bottom: 2px solid #558C7C;
}
.slick-slide img {
  object-fit: cover;
  height: 400px !important;
  margin: auto;
}
.scroll-banner {
  background-color: #fef9e7;
  color: #6e2c00;
  font-size: 15px;
  font-weight: bold;
  padding: 10px;
  border-radius: 10px;
  margin: 15px auto;
  width: 90%;
}
.centered-plot {
  display: flex;
  justify-content: center;
}

/* PREMIÈRE MODIFICATION : Effets lumineux pour la page technique */

/* Background animé pour la page technique */
#technique {
  background: linear-gradient(135deg, #f8f9fa 0%, #e3f2fd 50%, #f1f8e9 100%);
  background-size: 400% 400%;
  animation: gradientShift 8s ease infinite;
  position: relative;
  overflow: hidden;
  min-height: 100vh;
}

@keyframes gradientShift {
  0% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
  100% { background-position: 0% 50%; }
}

/* Titre avec effet néon */
.tech-title-neon {
  color: #54b377;
  font-size: 60px;
  font-weight: bold;
  text-shadow: 
    0 0 10px rgba(84, 179, 119, 0.8),
    0 0 20px rgba(84, 179, 119, 0.6),
    0 0 30px rgba(84, 179, 119, 0.4);
  animation: titleNeon 2s ease-in-out infinite alternate;
}

/* Particules spéciales pour la page technique */
.tech-particles {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  z-index: -1;
}

.tech-particle {
  position: absolute;
  width: 3px;
  height: 3px;
  background: rgba(84, 179, 119, 0.6);
  border-radius: 50%;
  animation: techFloat 20s infinite linear;
}

@keyframes techFloat {
  0% {
    transform: translateY(100vh) translateX(0px) rotate(0deg);
    opacity: 0;
  }
  10% {
    opacity: 1;
  }
  90% {
    opacity: 1;
  }
  100% {
    transform: translateY(-100px) translateX(100px) rotate(360deg);
    opacity: 0;
  }
}

/* Zone d'introduction avec effet cristal */
.tech-intro {
  background: linear-gradient(135deg, 
    rgba(255, 255, 255, 0.9) 0%, 
    rgba(240, 248, 255, 0.8) 50%, 
    rgba(248, 255, 248, 0.9) 100%);
  backdrop-filter: blur(10px);
  border: 2px solid rgba(0, 51, 102, 0.3);
  border-radius: 20px;
  padding: 25px;
  margin: 30px auto;
  width: 90%;
  position: relative;
  overflow: hidden;
  box-shadow: 
    0 8px 32px rgba(0, 0, 0, 0.1),
    inset 0 2px 16px rgba(255, 255, 255, 0.5);
  animation: crystalFloat 6s ease-in-out infinite;
}

@keyframes crystalFloat {
  0%, 100% { transform: translateY(0px) rotateX(0deg); }
  25% { transform: translateY(-3px) rotateX(1deg); }
  50% { transform: translateY(0px) rotateX(0deg); }
  75% { transform: translateY(3px) rotateX(-1deg); }
}

.tech-intro::before {
  content: '';
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background: linear-gradient(45deg, 
    transparent 40%, 
    rgba(84, 179, 119, 0.1) 50%, 
    transparent 60%);
  animation: shimmer 4s infinite;
}

/* Onglets avec effets néon pour la page technique */
.nav-tabs {
  background: linear-gradient(90deg, 
    rgba(0, 51, 102, 0.05) 0%, 
    rgba(84, 179, 119, 0.05) 50%, 
    rgba(0, 51, 102, 0.05) 100%);
  border-radius: 15px;
  padding: 10px;
  margin: 20px 0;
  position: relative;
  overflow: hidden;
}

.nav-tabs::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, 
    transparent, 
    rgba(84, 179, 119, 0.2), 
    transparent);
  animation: tabSweep 3s infinite;
}

@keyframes tabSweep {
  0% { left: -100%; }
  100% { left: 100%; }
}

.nav-tabs .nav-link {
  background: linear-gradient(135deg, #003366, #558C7C) !important;
  color: white !important;
  border: none !important;
  border-radius: 12px !important;
  margin: 0 5px;
  padding: 12px 20px;
  font-weight: bold;
  transition: all 0.3s ease;
  position: relative;
  overflow: hidden;
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
}

.nav-tabs .nav-link::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, 
    transparent, 
    rgba(255, 255, 255, 0.3), 
    transparent);
  transition: left 0.5s;
}

.nav-tabs .nav-link:hover::before {
  left: 100%;
}

.nav-tabs .nav-link:hover {
  transform: translateY(-3px) scale(1.05) !important;
  box-shadow: 
    0 8px 25px rgba(0, 0, 0, 0.3),
    0 0 20px rgba(84, 179, 119, 0.6) !important;
  background: linear-gradient(135deg, #558C7C, #003366) !important;
}

.nav-tabs .nav-link.active {
  background: linear-gradient(135deg, #FFD700, #FFA500) !important;
  color: #003366 !important;
  transform: scale(1.1);
  box-shadow: 
    0 0 30px rgba(255, 215, 0, 0.8),
    0 8px 25px rgba(0, 0, 0, 0.3) !important;
  animation: activeTabGlow 2s ease-in-out infinite;
}

@keyframes activeTabGlow {
  0%, 100% { box-shadow: 0 0 30px rgba(255, 215, 0, 0.8); }
  50% { box-shadow: 0 0 40px rgba(255, 215, 0, 1); }
}

/* Marquee holographique */
.marquee-container {
  background: linear-gradient(135deg, 
    rgba(253, 253, 253, 0.9) 0%, 
    rgba(240, 248, 255, 0.8) 50%, 
    rgba(253, 253, 253, 0.9) 100%);
  border: 2px solid rgba(84, 179, 119, 0.3);
  border-radius: 15px;
  margin: 20px 0;
  position: relative;
  overflow: hidden;
  box-shadow: 
    0 8px 32px rgba(0, 0, 0, 0.1),
    inset 0 1px 0 rgba(255, 255, 255, 0.5);
}

.marquee-container::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: linear-gradient(45deg, 
    transparent 30%, 
    rgba(84, 179, 119, 0.1) 50%, 
    transparent 70%);
  animation: hologram 4s infinite;
}

@keyframes hologram {
  0% { transform: translateX(-100%) skewX(-15deg); }
  100% { transform: translateX(200%) skewX(-15deg); }
}

/* Container des graphiques avec effet 3D */
.graph-container {
  background: rgba(255, 255, 255, 0.95);
  border-radius: 20px;
  padding: 25px;
  margin: 30px auto;
  max-width: 1000px;
  box-shadow: 
    0 20px 60px rgba(0, 0, 0, 0.1),
    0 8px 25px rgba(0, 0, 0, 0.05),
    inset 0 1px 0 rgba(255, 255, 255, 0.8);
  position: relative;
  overflow: hidden;
  animation: graphContainerFloat 8s ease-in-out infinite;
}

@keyframes graphContainerFloat {
  0%, 100% { transform: translateY(0px) rotateX(0deg); }
  25% { transform: translateY(-5px) rotateX(1deg); }
  50% { transform: translateY(0px) rotateX(0deg); }
  75% { transform: translateY(5px) rotateX(-1deg); }
}

.graph-container::before {
  content: '';
  position: absolute;
  top: -100%;
  left: -100%;
  width: 300%;
  height: 300%;
  background: radial-gradient(circle, 
    rgba(84, 179, 119, 0.05) 0%, 
    transparent 40%);
  animation: radialPulse 6s ease-in-out infinite;
}

@keyframes radialPulse {
  0%, 100% { 
    transform: scale(0.8) rotate(0deg);
    opacity: 0.5;
  }
  50% { 
    transform: scale(1.2) rotate(180deg);
    opacity: 0.8;
  }
}

/* Tableaux lumineux */
.dataTables_wrapper {
  background: rgba(255, 255, 255, 0.98);
  border-radius: 15px;
  padding: 20px;
  margin: 20px 0;
  box-shadow: 
    0 10px 30px rgba(0, 0, 0, 0.1),
    inset 0 1px 0 rgba(255, 255, 255, 0.6);
  position: relative;
  overflow: hidden;
}

.dataTables_wrapper::before {
  content: '';
  position: absolute;
  top: 0;
  left: -50%;
  width: 200%;
  height: 100%;
  background: linear-gradient(90deg, 
    transparent 40%, 
    rgba(84, 179, 119, 0.05) 50%, 
    transparent 60%);
  animation: tableShimmer 5s infinite;
}

@keyframes tableShimmer {
  0% { transform: translateX(-100%); }
  100% { transform: translateX(100%); }
}

.dataTables_wrapper table {
  position: relative;
  z-index: 2;
}

.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: linear-gradient(135deg, #558C7C, #003366);
  color: white !important;
  transform: translateY(-2px);
  box-shadow: 0 5px 15px rgba(0, 0, 0, 0.2);
}

.dataTables_wrapper table tbody tr:hover {
  background: linear-gradient(90deg, 
    rgba(84, 179, 119, 0.15) 0%, 
    rgba(84, 179, 119, 0.1) 50%, 
    rgba(84, 179, 119, 0.15) 100%) !important;
  transform: scale(1.01);
  box-shadow: 0 4px 15px rgba(0, 0, 0, 0.1);
  transition: all 0.3s ease;
}

/* Icône avec glow */
.info-icon {
  position: relative;
  animation: iconGlow 3s ease-in-out infinite;
}

@keyframes iconGlow {
  0%, 100% { 
    filter: drop-shadow(0 0 8px rgba(84, 179, 119, 0.6));
    transform: scale(1);
  }
  50% { 
    filter: drop-shadow(0 0 16px rgba(84, 179, 119, 0.9));
    transform: scale(1.1);
  }
}

@keyframes titleNeon {
  from {
    text-shadow: 
      0 0 10px rgba(84, 179, 119, 0.8),
      0 0 20px rgba(84, 179, 119, 0.6),
      0 0 30px rgba(84, 179, 119, 0.4);
  }
  to {
    text-shadow: 
      0 0 20px rgba(84, 179, 119, 1),
      0 0 30px rgba(84, 179, 119, 0.8),
      0 0 40px rgba(84, 179, 119, 0.6);
  }
}

/* NOUVELLES ANIMATIONS DYNAMIQUES */

/* Animation du logo principal */
.main-logo {
  animation: logoFloat 3s ease-in-out infinite;
  transition: transform 0.3s ease;
}

.main-logo:hover {
  transform: scale(1.1) rotate(5deg);
}

@keyframes logoFloat {
  0%, 100% { transform: translateY(0px); }
  50% { transform: translateY(-10px); }
}

/* Animation du titre principal */
.animated-title {
  background: linear-gradient(45deg, #558C7C, #003366, #558C7C);
  background-size: 200% 200%;
  animation: gradientMove 4s ease infinite, titlePulse 2s ease-in-out infinite;
  color: white;
  padding: 20px;
  border-radius: 15px;
  box-shadow: 0 8px 25px rgba(0,0,0,0.3);
  position: relative;
  overflow: hidden;
}

.animated-title::before {
  content: '';
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background: linear-gradient(45deg, transparent, rgba(255,255,255,0.1), transparent);
  animation: shimmer 3s infinite;
}

@keyframes gradientMove {
  0% { background-position: 0% 50%; }
  50% { background-position: 100% 50%; }
  100% { background-position: 0% 50%; }
}

@keyframes titlePulse {
  0%, 100% { transform: scale(1); }
  50% { transform: scale(1.02); }
}

@keyframes shimmer {
  0% { transform: translateX(-100%) translateY(-100%) rotate(30deg); }
  100% { transform: translateX(100%) translateY(100%) rotate(30deg); }
}

/* Titre principal avec effet de dactylographie */
.typing-title {
  font-weight: bold;
  font-size: 36px;
  margin: 0;
  overflow: hidden;
  border-right: 3px solid white;
  white-space: nowrap;
  animation: typing 4s steps(40, end), blink-caret 0.75s step-end infinite;
}

@keyframes typing {
  from { width: 0; }
  to { width: 100%; }
}

@keyframes blink-caret {
  from, to { border-color: transparent; }
  50% { border-color: white; }
}

/* Sous-titre avec effet de glissement */
.sliding-subtitle {
  margin: 0;
  animation: slideInUp 1.5s ease-out 2s both;
  opacity: 0;
}

@keyframes slideInUp {
  from {
    opacity: 0;
    transform: translateY(30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Carousel avec effet de zoom */
.carousel-container {
  position: relative;
  overflow: hidden;
  border-radius: 20px;
  box-shadow: 0 15px 35px rgba(0,0,0,0.3);
  animation: carouselFloat 6s ease-in-out infinite;
}

@keyframes carouselFloat {
  0%, 100% { transform: translateY(0px) scale(1); }
  33% { transform: translateY(-5px) scale(1.01); }
  66% { transform: translateY(5px) scale(0.99); }
}

/* Miniatures avec animations au survol */
.miniature-container {
  transition: all 0.3s ease;
  border-radius: 15px;
  overflow: hidden;
  position: relative;
  animation: miniatureEntrance 0.8s ease-out;
}

.miniature-container:hover {
  transform: translateY(-10px) scale(1.05);
  box-shadow: 0 15px 30px rgba(0,0,0,0.4);
}

.miniature-container::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent);
  transition: left 0.5s;
}

.miniature-container:hover::before {
  left: 100%;
}

@keyframes miniatureEntrance {
  from {
    opacity: 0;
    transform: translateY(20px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

/* Animation des miniatures en cascade */
.miniature-1 { animation-delay: 0.1s; }
.miniature-2 { animation-delay: 0.2s; }
.miniature-3 { animation-delay: 0.3s; }
.miniature-4 { animation-delay: 0.4s; }
.miniature-5 { animation-delay: 0.5s; }
.miniature-6 { animation-delay: 0.6s; }

/* Footer avec effet de vague */
.animated-footer {
  background: linear-gradient(45deg, #003366, #002244);
  color: white;
  padding: 20px;
  font-size: 14px;
  position: relative;
  overflow: hidden;
  animation: footerGlow 4s ease-in-out infinite;
}

.animated-footer::before {
  content: '';
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background: radial-gradient(circle, rgba(255,255,255,0.1) 0%, transparent 70%);
  animation: wave 8s linear infinite;
}

@keyframes footerGlow {
  0%, 100% { box-shadow: 0 0 20px rgba(85,140,124,0.3); }
  50% { box-shadow: 0 0 40px rgba(85,140,124,0.6); }
}

@keyframes wave {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

/* Effet de particules flottantes */
.floating-particles {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  z-index: 1;
}

.particle {
  position: absolute;
  width: 4px;
  height: 4px;
  background: rgba(85,140,124,0.6);
  border-radius: 50%;
  animation: float 15s infinite linear;
}

@keyframes float {
  0% {
    transform: translateY(100vh) rotate(0deg);
    opacity: 0;
  }
  10% {
    opacity: 1;
  }
  90% {
    opacity: 1;
  }
  100% {
    transform: translateY(-100px) rotate(360deg);
    opacity: 0;
  }
}

/* Animations pour les données techniques */
.dataTables_wrapper .dataTables_paginate .paginate_button {
  padding: 2px 10px;
  margin: 2px;
  border-radius: 5px;
  background-color: #f2f2f2;
  border: 1px solid #ccc;
}
.dataTables_wrapper .dataTables_length, 
.dataTables_wrapper .dataTables_filter {
  margin: 10px 0;
}

/* Images défilantes verticales */
.vertical-scroll-container {
  height: 300px;
  overflow: hidden;
  position: relative;
  border-radius: 20px;
  box-shadow: 0 10px 30px rgba(0,0,0,0.3);
  margin: 30px auto;
  width: 90%;
  background: linear-gradient(135deg, #558C7C, #003366);
}

.vertical-scroll-track {
  display: flex;
  flex-direction: column;
  animation: verticalScroll 20s linear infinite;
}

@keyframes verticalScroll {
  0% { transform: translateY(0); }
  100% { transform: translateY(-100%); }
}

.scroll-item {
  position: relative;
  height: 300px;
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  background-size: cover;
  background-position: center;
  margin-bottom: 20px;
}

.scroll-item::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: rgba(0,0,0,0.4);
  z-index: 1;
}

.scroll-text {
  position: relative;
  z-index: 2;
  color: white;
  font-size: 28px;
  font-weight: bold;
  text-align: center;
  text-shadow: 2px 2px 4px rgba(0,0,0,0.8);
  padding: 20px;
  background: rgba(85,140,124,0.8);
  border-radius: 15px;
  animation: textPulse 3s ease-in-out infinite;
}

@keyframes textPulse {
  0%, 100% { transform: scale(1); }
  50% { transform: scale(1.05); }
}

/* Zone de présentation PUDC */
.pudc-presentation {
  margin: 40px auto;
  width: 95%;
  background: linear-gradient(135deg, #f8f9fa, #e9ecef);
  border-radius: 25px;
  padding: 30px;
  box-shadow: 0 15px 35px rgba(0,0,0,0.1);
  position: relative;
  overflow: hidden;
}

.pudc-presentation::before {
  content: '';
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background: radial-gradient(circle, rgba(85,140,124,0.05) 0%, transparent 70%);
  animation: rotate 20s linear infinite;
}

@keyframes rotate {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

.pudc-content {
  position: relative;
  z-index: 2;
}

.pudc-tabs {
  display: flex;
  justify-content: center;
  margin-bottom: 30px;
  flex-wrap: wrap;
  gap: 10px;
}

.pudc-tab {
  padding: 12px 25px;
  background: linear-gradient(45deg, #558C7C, #003366);
  color: white;
  border: none;
  border-radius: 25px;
  cursor: pointer;
  font-weight: bold;
  transition: all 0.3s ease;
  position: relative;
  overflow: hidden;
}

.pudc-tab::before {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
  transition: left 0.5s;
}

.pudc-tab:hover::before {
  left: 100%;
}

.pudc-tab:hover {
  transform: translateY(-3px);
  box-shadow: 0 10px 20px rgba(0,0,0,0.3);
}

.pudc-tab.active {
  background: linear-gradient(45deg, #003366, #558C7C);
  transform: scale(1.05);
}

.pudc-tab-content {
  display: none;
  background: white;
  padding: 25px;
  border-radius: 15px;
  box-shadow: inset 0 2px 10px rgba(0,0,0,0.1);
  animation: fadeInUp 0.5s ease-out;
}

.pudc-tab-content.active {
  display: block;
}

@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(20px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.pudc-title {
  color: #003366;
  font-size: 24px;
  font-weight: bold;
  margin-bottom: 15px;
  text-align: center;
  position: relative;
}

.pudc-title::after {
  content: '';
  position: absolute;
  bottom: -5px;
  left: 50%;
  transform: translateX(-50%);
  width: 60px;
  height: 3px;
  background: linear-gradient(90deg, #558C7C, #003366);
  border-radius: 2px;
}

.pudc-text {
  font-size: 16px;
  line-height: 1.8;
  color: #444;
  text-align: justify;
  margin-bottom: 15px;
}

.pudc-highlight {
  background: linear-gradient(120deg, #558C7C20, #003366020);
  padding: 15px;
  border-left: 4px solid #558C7C;
  border-radius: 0 10px 10px 0;
  margin: 15px 0;
  font-style: italic;
}

.pudc-stats {
  display: flex;
  justify-content: space-around;
  margin-top: 20px;
  flex-wrap: wrap;
}

.pudc-stat {
  text-align: center;
  padding: 15px;
  background: linear-gradient(135deg, #558C7C, #003366);
  color: white;
  border-radius: 15px;
  min-width: 120px;
  margin: 5px;
  animation: statPulse 2s ease-in-out infinite;
}

@keyframes statPulse {
  0%, 100% { transform: scale(1); }
  50% { transform: scale(1.05); }
}

.pudc-stat-number {
  font-size: 28px;
  font-weight: bold;
  display: block;
}

.pudc-stat-label {
  font-size: 12px;
  opacity: 0.9;
}

/* Effet de brillance sur les éléments interactifs */
.shine-effect {
  position: relative;
  overflow: hidden;
}

.shine-effect::after {
  content: '';
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent);
  transition: left 0.5s;
}

.shine-effect:hover::after {
  left: 100%;
}

/* En-tête avec glow pulsant et overlay animé */
.tech-header-container {
  position: relative;
  overflow: hidden;
  border-radius: 20px;
  margin: 20px auto;
  box-shadow: 0 0 40px rgba(84, 179, 119, 0.6);
  animation: headerGlow 4s ease-in-out infinite;
}

@keyframes headerGlow {
  0%, 100% { 
    box-shadow: 0 0 40px rgba(84, 179, 119, 0.6);
    transform: scale(1);
  }
  50% { 
    box-shadow: 0 0 80px rgba(84, 179, 119, 0.9);
    transform: scale(1.02);
  }
}

.tech-header-overlay {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background: linear-gradient(45deg, 
    rgba(84, 179, 119, 0.2) 0%, 
    rgba(0, 51, 102, 0.3) 50%, 
    rgba(84, 179, 119, 0.2) 100%);
  animation: overlayPulse 3s ease-in-out infinite;
}

@keyframes overlayPulse {
  0%, 100% { opacity: 0.7; }
  50% { opacity: 0.9; }
}

/* Zone d'introduction premium */
.tech-intro-premium {
  background: linear-gradient(135deg, 
    rgba(255, 255, 255, 0.95) 0%, 
    rgba(240, 248, 255, 0.9) 25%,
    rgba(248, 255, 248, 0.9) 75%,
    rgba(255, 255, 255, 0.95) 100%);
  backdrop-filter: blur(15px);
  border: 3px solid rgba(84, 179, 119, 0.4);
  border-radius: 25px;
  padding: 30px;
  margin: 30px auto;
  width: 90%;
  position: relative;
  overflow: hidden;
  box-shadow: 
    0 15px 45px rgba(0, 0, 0, 0.12),
    inset 0 2px 20px rgba(255, 255, 255, 0.6),
    0 0 30px rgba(84, 179, 119, 0.2);
  animation: premiumFloat 8s ease-in-out infinite;
}

@keyframes premiumFloat {
  0%, 100% { 
    transform: translateY(0px) rotateX(0deg) scale(1);
    box-shadow: 0 15px 45px rgba(0, 0, 0, 0.12);
  }
  25% { 
    transform: translateY(-4px) rotateX(1deg) scale(1.01);
    box-shadow: 0 20px 55px rgba(0, 0, 0, 0.15);
  }
  50% { 
    transform: translateY(0px) rotateX(0deg) scale(1);
    box-shadow: 0 15px 45px rgba(0, 0, 0, 0.12);
  }
  75% { 
    transform: translateY(4px) rotateX(-1deg) scale(0.99);
    box-shadow: 0 10px 35px rgba(0, 0, 0, 0.1);
  }
}

.tech-intro-premium::before {
  content: '';
  position: absolute;
  top: -100%;
  left: -100%;
  width: 300%;
  height: 300%;
  background: radial-gradient(circle at center, 
    rgba(84, 179, 119, 0.1) 0%, 
    transparent 30%);
  animation: premiumPulse 6s ease-in-out infinite;
}

@keyframes premiumPulse {
  0%, 100% { 
    transform: scale(0.8) rotate(0deg);
    opacity: 0.3;
  }
  50% { 
    transform: scale(1.4) rotate(180deg);
    opacity: 0.6;
  }
}

.tech-intro-premium::after {
  content: '';
  position: absolute;
  top: -50%;
  left: -50%;
  width: 200%;
  height: 200%;
  background: linear-gradient(45deg, 
    transparent 40%, 
    rgba(84, 179, 119, 0.15) 50%, 
    transparent 60%);
  animation: premiumShimmer 5s infinite;
}

@keyframes premiumShimmer {
  0% { transform: translateX(-100%) translateY(-100%) rotate(45deg); }
  100% { transform: translateX(150%) translateY(150%) rotate(45deg); }
}

/* Icône avec animation premium */
.info-icon-premium {
  position: relative;
  animation: iconPremiumGlow 4s ease-in-out infinite;
  z-index: 3;
}

@keyframes iconPremiumGlow {
  0%, 100% { 
    filter: drop-shadow(0 0 12px rgba(84, 179, 119, 0.7));
    transform: scale(1) rotate(0deg);
  }
  25% { 
    filter: drop-shadow(0 0 20px rgba(84, 179, 119, 1));
    transform: scale(1.1) rotate(5deg);
  }
  50% { 
    filter: drop-shadow(0 0 24px rgba(84, 179, 119, 1));
    transform: scale(1.15) rotate(0deg);
  }
  75% { 
    filter: drop-shadow(0 0 20px rgba(84, 179, 119, 1));
    transform: scale(1.1) rotate(-5deg);
  }
}

/* Contenu avec animation d'entrée */
.tech-content-premium {
  position: relative;
  z-index: 3;
  animation: contentSlideIn 1.2s ease-out;
}

@keyframes contentSlideIn {
  from {
    opacity: 0;
    transform: translateX(30px);
  }
  to {
    opacity: 1;
    transform: translateX(0);
  }
}

/* Titre de la zone d'introduction */
.tech-intro-title {
  color: #003366;
  font-size: 22px;
  font-weight: bold;
  margin-bottom: 15px;
  position: relative;
  animation: titleShine 3s ease-in-out infinite;
}

@keyframes titleShine {
  0%, 100% { color: #003366; }
  50% { color: #558C7C; }
}

.tech-intro-title::after {
  content: '';
  position: absolute;
  bottom: -3px;
  left: 0;
  width: 100%;
  height: 2px;
  background: linear-gradient(90deg, #558C7C, #003366, #558C7C);
  border-radius: 1px;
  animation: underlineGlow 2s ease-in-out infinite;
}

@keyframes underlineGlow {
  0%, 100% { opacity: 0.7; }
  50% { opacity: 1; box-shadow: 0 0 8px rgba(84, 179, 119, 0.6); }
}

/* Texte avec effet de dactylographie amélioré */
.tech-intro-text {
  font-size: 16px;
  line-height: 1.8;
  color: #444;
  text-align: justify;
  position: relative;
  z-index: 3;
}

/* Badge indicateur */
.tech-badge {
  display: inline-block;
  background: linear-gradient(135deg, #558C7C, #003366);
  color: white;
  padding: 8px 16px;
  border-radius: 20px;
  font-size: 12px;
  font-weight: bold;
  margin-top: 15px;
  animation: badgePulse 2s ease-in-out infinite;
  box-shadow: 0 4px 15px rgba(84, 179, 119, 0.3);
}

@keyframes badgePulse {
  0%, 100% { transform: scale(1); }
  50% { transform: scale(1.05); }
}
"))
  ),
  
  # === Navbar ===
  div(class = "navbar-custom",
      div(class = "navbar-logo", tags$img(src = "logo.png")),
      div(class = "navbar-menu",
          tags$a("Home", href = "#accueil", class = "active"),
          tags$a("Suivi Technique", href = "#technique"),
          tags$a("Suivi financier", href = "#financier"),
          tags$a("Performance P.M.", href = "#marches"),
          tags$a("Réalisation globale", href = "#indicateurs"),
          tags$a("Résumé", href = "#resume"),
          tags$a("Assistant", href = "#assistant")
      )
  ),
  
  # === Page accueil avec animations dynamiques ===
  div(id = "accueil", class = "page",
      # Particules flottantes
      div(class = "floating-particles",
          tags$div(class = "particle", style = "left: 10%; animation-delay: 0s;"),
          tags$div(class = "particle", style = "left: 20%; animation-delay: 2s;"),
          tags$div(class = "particle", style = "left: 30%; animation-delay: 4s;"),
          tags$div(class = "particle", style = "left: 40%; animation-delay: 1s;"),
          tags$div(class = "particle", style = "left: 50%; animation-delay: 3s;"),
          tags$div(class = "particle", style = "left: 60%; animation-delay: 5s;"),
          tags$div(class = "particle", style = "left: 70%; animation-delay: 1.5s;"),
          tags$div(class = "particle", style = "left: 80%; animation-delay: 3.5s;"),
          tags$div(class = "particle", style = "left: 90%; animation-delay: 0.5s;")
      ),
      
      br(), 
      column(12, align = "center",
             # Logo principal avec animation
             div(class = "main-logo",
                 tags$img(src = "pudc_logo.png", height = "100px")
             ), 
             br(),
             
             # Titre principal avec animations
             div(class = "animated-title",
                 tags$h1(class = "typing-title", "Tableau de bord de suivi"),
                 tags$h2(class = "sliding-subtitle", "PTBA DU PUDC 2025")
             ), 
             br(),
             
             # Images défilantes verticales
             div(class = "vertical-scroll-container",
                 div(class = "vertical-scroll-track",
                     div(class = "scroll-item", 
                         style = "background-image: url('electrification.png');",
                         div(class = "scroll-text", "Électrification Rurale")),
                     div(class = "scroll-item", 
                         style = "background-image: url('piste.png');",
                         div(class = "scroll-text", "Désenclavement des Zones Rurales")),
                     div(class = "scroll-item", 
                         style = "background-image: url('poste_sante.png');",
                         div(class = "scroll-text", "Amélioration de la Santé")),
                     div(class = "scroll-item", 
                         style = "background-image: url('real.png');",
                         div(class = "scroll-text", "Réalisations Concrètes")),
                     # Duplication pour l'effet de boucle infinie
                     div(class = "scroll-item", 
                         style = "background-image: url('electrification.png');",
                         div(class = "scroll-text", "Électrification Rurale")),
                     div(class = "scroll-item", 
                         style = "background-image: url('piste.png');",
                         div(class = "scroll-text", "Désenclavement des Zones Rurales")),
                     div(class = "scroll-item", 
                         style = "background-image: url('poste_sante.png');",
                         div(class = "scroll-text", "Amélioration de la Santé")),
                     div(class = "scroll-item", 
                         style = "background-image: url('real.png');",
                         div(class = "scroll-text", "Réalisations Concrètes"))
                 )
             ),
             
             # Zone de présentation du PUDC
             div(class = "pudc-presentation",
                 div(class = "pudc-content",
                     # Onglets de navigation
                     div(class = "pudc-tabs",
                         tags$button(class = "pudc-tab active", 
                                     onclick = "showPudcTab('presentation')", 
                                     "Présentation"),
                         tags$button(class = "pudc-tab", 
                                     onclick = "showPudcTab('missions')", 
                                     "Missions"),
                         tags$button(class = "pudc-tab", 
                                     onclick = "showPudcTab('objectifs')", 
                                     "Objectifs"),
                         tags$button(class = "pudc-tab", 
                                     onclick = "showPudcTab('resultats')", 
                                     "Résultats")
                     ),
                     
                     # Contenu Présentation
                     div(id = "pudc-presentation", class = "pudc-tab-content active",
                         div(class = "pudc-title", "Programme d'Urgence de Développement Communautaire"),
                         div(class = "pudc-text",
                             "Le PUDC est un programme ambitieux initié par le Gouvernement du Sénégal pour accélérer le développement des communautés rurales et péri-urbaines. Ce programme vise à réduire les inégalités territoriales et à améliorer les conditions de vie des populations les plus vulnérables."
                         ),
                         div(class = "pudc-highlight",
                             "\"Une approche intégrée pour un développement durable et inclusif des territoires sénégalais\""
                         ),
                         div(class = "pudc-stats",
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "14"),
                                 span(class = "pudc-stat-label", "Régions")
                             ),
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "557"),
                                 span(class = "pudc-stat-label", "Communes")
                             ),
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "8M"),
                                 span(class = "pudc-stat-label", "Bénéficiaires")
                             )
                         )
                     ),
                     
                     # Contenu Missions
                     div(id = "pudc-missions", class = "pudc-tab-content",
                         div(class = "pudc-title", "Missions du PUDC"),
                         div(class = "pudc-text",
                             "Le PUDC a pour mission principale de contribuer à l'amélioration des conditions de vie des populations rurales et péri-urbaines à travers :"
                         ),
                         div(class = "pudc-text",
                             "• Le renforcement des infrastructures socio-économiques de base",
                             tags$br(),
                             "• L'amélioration de l'accès aux services sociaux essentiels",
                             tags$br(),
                             "• Le développement des activités génératrices de revenus",
                             tags$br(),
                             "• La promotion de la gouvernance locale participative"
                         ),
                         div(class = "pudc-highlight",
                             "Mission transversale : Renforcer la cohésion sociale et l'équité territoriale"
                         )
                     ),
                     
                     # Contenu Objectifs
                     div(id = "pudc-objectifs", class = "pudc-tab-content",
                         div(class = "pudc-title", "Objectifs Stratégiques"),
                         div(class = "pudc-text",
                             "Les objectifs stratégiques du PUDC s'articulent autour de quatre piliers fondamentaux :"
                         ),
                         div(class = "pudc-text",
                             "🏗️ **Infrastructures** : Désenclavement et électrification rurale",
                             tags$br(),
                             "🏥 **Services sociaux** : Santé, éducation et accès à l'eau",
                             tags$br(),
                             "💼 **Économie locale** : Appui aux activités productives",
                             tags$br(),
                             "🤝 **Gouvernance** : Renforcement des capacités locales"
                         ),
                         div(class = "pudc-stats",
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "1200"),
                                 span(class = "pudc-stat-label", "Projets")
                             ),
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "75%"),
                                 span(class = "pudc-stat-label", "Taux d'exécution")
                             )
                         )
                     ),
                     
                     # Contenu Résultats
                     div(id = "pudc-resultats", class = "pudc-tab-content",
                         div(class = "pudc-title", "Résultats et Impact"),
                         div(class = "pudc-text",
                             "Les résultats du PUDC témoignent de son impact positif sur le développement territorial :"
                         ),
                         div(class = "pudc-text",
                             "✅ Plus de 500 km de routes rurales réhabilités",
                             tags$br(),
                             "✅ 300 villages électrifiés",
                             tags$br(),
                             "✅ 150 postes de santé construits ou réhabilités",
                             tags$br(),
                             "✅ 200 forages réalisés pour l'accès à l'eau potable"
                         ),
                         div(class = "pudc-highlight",
                             "Impact majeur : Réduction de 30% des inégalités territoriales"
                         ),
                         div(class = "pudc-stats",
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "95%"),
                                 span(class = "pudc-stat-label", "Satisfaction")
                             ),
                             div(class = "pudc-stat",
                                 span(class = "pudc-stat-number", "2M"),
                                 span(class = "pudc-stat-label", "Emplois créés")
                             )
                         )
                     )
                 )
             ),
             br(),
             
             # Carousel avec container animé
             div(class = "carousel-container",
                 slickROutput("carousel", width = "80%", height = "400px")
             ), 
             br(),
             
             # Miniatures avec animations en cascade
             fluidRow(
               column(2, div(class = "miniature-container miniature-1 shine-effect",
                             tags$img(src = "miniature1.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-2 shine-effect",
                             tags$img(src = "miniature2.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-3 shine-effect",
                             tags$img(src = "miniature3.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-4 shine-effect",
                             tags$img(src = "miniature4.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-5 shine-effect",
                             tags$img(src = "miniature5.png", width = "100%"))),
               column(2, div(class = "miniature-container miniature-6 shine-effect",
                             tags$img(src = "miniature6.png", width = "100%")))
             ), 
             br(),
             
             # Footer avec animations
             div(class = "animated-footer",
                 "Programme d'Urgence de Développement Communautaire | Contact | Mentions légales", 
                 tags$br(),
                 "Ministère du Développement communautaire, de la Solidarité nationale et de l'Équité territoriale"
             )
      )
  ),
  
  # === Suivi Technique avec PREMIÈRE MODIFICATION : titre néon ===
  div(id = "technique", class = "page",
      div(class = "tech-particles",
          tags$div(class = "tech-particle", style = "left: 5%; animation-delay: 0s;"),
          tags$div(class = "tech-particle", style = "left: 15%; animation-delay: 3s;"),
          tags$div(class = "tech-particle", style = "left: 25%; animation-delay: 6s;"),
          tags$div(class = "tech-particle", style = "left: 35%; animation-delay: 9s;"),
          tags$div(class = "tech-particle", style = "left: 45%; animation-delay: 12s;"),
          tags$div(class = "tech-particle", style = "left: 55%; animation-delay: 15s;"),
          tags$div(class = "tech-particle", style = "left: 65%; animation-delay: 18s;"),
          tags$div(class = "tech-particle", style = "left: 75%; animation-delay: 1s;"),
          tags$div(class = "tech-particle", style = "left: 85%; animation-delay: 4s;"),
          tags$div(class = "tech-particle", style = "left: 95%; animation-delay: 7s;")
      ),
      div(class = "tech-header-container",
          tags$img(src = "electrification.png",
                   style = "width: 100%; height: 220px; object-fit: cover;"),
          div(class = "tech-header-overlay"),
          div(class = "tech-title-neon",
              style = "
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        z-index: 2;",
              "Suivi technique par projet"
          )
      ),
      
      # 📝 Texte d'introduction
      div(class = "tech-intro",
          fluidRow(
            column(1, div(class = "info-icon",
                          tags$img(src = "icon_info.png", width = "60px"))),
            column(11,
                   HTML("Cet onglet présente un état d'avancement détaillé des différentes activités techniques mises en œuvre dans le cadre du Programme d'Urgence de Développement Communautaire (PUDC). Il permet de visualiser en temps réel les progrès réalisés par rapport aux objectifs fixés, de suivre l'exécution physique des projets (infrastructures, équipements, services) et d'identifier les écarts éventuels entre les prévisions et les réalisations.")
            )
          )
      ),
      do.call(tabsetPanel, c(
        list(id = "projets_tabs", type = "tabs"),
        lapply(unique(df_tech$projet), function(proj_name) {
          safe_id <- gsub("[^a-zA-Z0-9]", "_", proj_name)
          tabPanel(title = proj_name,
                   div(class = "marquee-container",
                       uiOutput(outputId = paste0("marquee_", safe_id))
                   ),
                   div(class = "graph-container",
                       plotlyOutput(outputId = paste0("graph_", safe_id), height = "400px")
                   ),
                   br(),
                   DTOutput(outputId = paste0("table_", safe_id))
          )
        })
      ))
  ),
  
  div(id = "financier", class = "page", h2("Suivi financier")),
  div(id = "marches", class = "page",
      div(style = "position: relative; text-align: center;",
          tags$img(src = "poste_sante.png",
                   style = "width: 100%; filter: blur(2px); height: 220px; object-fit: cover;"),
          div(style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); color: white; font-size: 50px; font-weight: bold; text-shadow: 2px 2px 6px black; z-index: 2;",
              "Performance en passation des marches"
          )
      ),
      
      div(style = "background-color: white; border: 2px solid #003366; padding: 25px; margin: 30px auto; width: 90%; border-radius: 10px; font-size: 15px;",
          fluidRow(
            column(1, tags$img(src = "icon_info.png", width = "60px")),
            column(11, HTML("<b>Suivi intégré</b> : Cette section présente l'exécution physique, budgétaire et la performance en passation des marchés pour l'année 2025. Les graphiques permettent une comparaison entre les objectifs et les réalisations pour chaque trimestre."))
          )
      ),
      
      div(style = "width: 300px; margin: 0 auto; text-align: center;",
          selectInput("projet_pm", "Choisir un projet :", choices = NULL)
      ),
      br(),
      
      fluidRow(
        column(12,
               h3("\u2705 Exécution physique trimestrielle", style = "color:#2E86C1; text-align:center; font-weight:bold;"),
               plotlyOutput("graph_exec_physique", height = "400px"),
               br(),
               
               h3("\uD83D\uDCB0 Exécution budgétaire trimestrielle", style = "color:#239B56; text-align:center; font-weight:bold;"),
               plotlyOutput("graph_exec_budget", height = "400px"),
               br(),
               
               h3("\u2696\uFE0F Taux de performance en passation de marchés", style = "color:#CA6F1E; text-align:center; font-weight:bold;"),
               plotlyOutput("graph_pm", height = "400px")
        )
      )
  ),
  
  
  div(id = "indicateurs", class = "page", h2("Réalisation globale")),
  div(id = "resume", class = "page", h2("Résumé")),
  div(id = "assistant", class = "page", h2("Assistant")),
  
  tags$script(HTML("
      // Fonction pour gérer les onglets PUDC
      function showPudcTab(tabName) {
        // Masquer tous les contenus
        document.querySelectorAll('.pudc-tab-content').forEach(function(content) {
          content.classList.remove('active');
        });
        
        // Désactiver tous les onglets
        document.querySelectorAll('.pudc-tab').forEach(function(tab) {
          tab.classList.remove('active');
        });
        
        // Afficher le contenu sélectionné
        document.getElementById('pudc-' + tabName).classList.add('active');
        
        // Activer l'onglet cliqué
        event.target.classList.add('active');
      }
      
      // Navigation principale
      $(document).on('click', '.navbar-menu a', function(e){
        e.preventDefault();
        $('.navbar-menu a').removeClass('active');
        $(this).addClass('active');
        $('.page').hide();
        $($(this).attr('href')).show();
      });
      
      $(document).ready(function(){
        $('.page').hide();
        $('#accueil').show();
      });
    "))
)

# === Serveur ===
server <- function(input, output, session) {
  images <- c("piste.png", "electrification.png", "poste_sante.png", "real.png")
  output$carousel <- renderSlickR({
    slickR(images, slideId = 'car1') +
      settings(autoplay = TRUE, autoplaySpeed = 4000, arrows = TRUE, dots = TRUE, infinite = TRUE)
  })
  
  for (proj in unique(df_tech$projet)) {
    local({
      proj_name <- proj
      safe_id <- gsub("[^a-zA-Z0-9]", "_", proj_name)
      
      output[[paste0("graph_", safe_id)]] <- renderPlotly({
        dat <- df_tech %>%
          filter(projet == proj_name) %>%
          mutate(Objectif = as.numeric(Objectif) * 100,
                 Réalisation = as.numeric(Réalisation) * 100) %>%
          filter(!is.na(Objectif), !is.na(Réalisation))
        
        validate(need(nrow(dat) > 0, "Pas de données disponibles pour ce projet"))
        
        plot_ly(dat, x = ~Volets) %>%
          add_bars(y = ~Objectif, name = "Objectif (%)",
                   marker = list(color = "#c6f5eb"),
                   text = ~paste( round(Objectif, 1), "%"),
                   hoverinfo = "text", textposition = "auto") %>%
          add_bars(y = ~Réalisation, name = "Réalisation (%)",
                   marker = list(color = "#fcfaa4"),
                   text = ~paste( round(Réalisation, 1), "%"),
                   hoverinfo = "text", textposition = "auto") %>%
          layout(barmode = "group",
                 xaxis = list(title = "Volets", tickangle = -45),
                 yaxis = list(title = "Taux (%)", range = c(0, 120)),
                 title = list(text = paste("Exécution technique -", proj_name), x = 0.5))
      })
      
      output[[paste0("marquee_", safe_id)]] <- renderUI({
        dat <- df_tech %>% filter(projet == proj_name)
        dat <- dat %>% filter(!is.na(Observations), Observations != "")
        
        if (nrow(dat) == 0) return(NULL)
        
        # Couleurs avec gradients pour blocs lumineux
        colors <- c(
          "linear-gradient(135deg, #99e6f2, #7dd3fc)",
          "linear-gradient(135deg, #f28280, #fca5a5)", 
          "linear-gradient(135deg, #bbfcef, #86efac)",
          "linear-gradient(135deg, #e8d21d, #fde047)",
          "linear-gradient(135deg, #f0e6ff, #ddd6fe)"
        )
        
        html_blocks <- mapply(function(obj, obs, volet, i) {
          col <- colors[(i %% length(colors)) + 1]
          sprintf(
            "<span style='display:inline-block; background: %s; margin:0 20px; padding:15px 20px; border-radius:12px; box-shadow: 0 4px 15px rgba(0,0,0,0.1); backdrop-filter: blur(5px);'>
     <b>🎯 Volet:</b> %s<br>
     <b>📋 Objectif:</b> %s<br>
     <span style='color: #dc2626; font-weight: bold;'>🔍 %s</span>
   </span>",
            col, volet, obj, obs
          )
        },
        dat$Objectifs_annuels, dat$Observations, dat$Volets, seq_along(dat$Volets)
        )
        
        HTML(paste0(
          "<marquee behavior='scroll' direction='left' scrollamount='3' style='padding: 20px; font-size: 14px; font-weight: 500;'>",
          paste(html_blocks, collapse = ""),
          "</marquee>"
        ))
      })
      
      output[[paste0("table_", safe_id)]] <- renderDT({
        df_tech %>%
          filter(projet == proj_name) %>%
          select(Volets, SousVolets, Objectifs_annuels)
      }, options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'tip',
        stripeClasses = c('stripe1', 'stripe2'),
        language = list(search = "Filtrer :", lengthMenu = "Afficher _MENU_ lignes")
      ), rownames = FALSE)
    })
  }
  # PERFORMANCES EN PASSATION DE MARCHE
  # Chemin vers le fichier
  f_pm <- "www/Performances en PM.xlsx"
  
  # Lecture des 3 feuilles
  df_exec_physique <- read_excel(f_pm, sheet = "Exécution Physique")
  df_exec_budget <- read_excel(f_pm, sheet = "EXECUTION BUDGETAIRE")
  df_pm <- read_excel(f_pm, sheet = "Plan de passation de marchés")
  
  # Mise à jour du sélecteur de projet
  observe({
    projets_disponibles <- unique(c(df_exec_physique$Projets, df_exec_budget$PROJETS, df_pm$PROJETS))
    updateSelectInput(session, "projet_pm", choices = projets_disponibles)
  })
  
  # === Graphique 1 : Exécution Physique ===
  output$graph_exec_physique <- renderPlotly({
    req(input$projet_pm)
    df <- df_exec_physique %>% filter(Projets == input$projet_pm)
    plot_ly(df, x = ~Trimestre) %>%
      add_trace(y = ~`Objectif (%)`*100, name = "Objectif", type = 'bar', marker = list(color = '#aed6f1')) %>%
      add_trace(y = ~`Réalisation (%)`*100, name = "Réalisation", type = 'bar', marker = list(color = '#fcf3cf')) %>%
      layout(barmode = 'group', xaxis = list(title = "Trimestre"), yaxis = list(title = "Pourcentage (%)"))
  })
  
  # === Graphique 2 : Exécution Budgétaire ===
  output$graph_exec_budget <- renderPlotly({
    req(input$projet_pm)
    df <- df_exec_budget %>% filter(PROJETS == input$projet_pm)
    plot_ly(df, x = ~Trimestre) %>%
      add_trace(y = ~`Objectif (%)`*100, name = "Objectif", type = 'bar', marker = list(color = '#abebc6')) %>%
      add_trace(y = ~`Réalisation (%)`*100, name = "Réalisation", type = 'bar', marker = list(color = '#fadbd8')) %>%
      layout(barmode = 'group', xaxis = list(title = "Trimestre"), yaxis = list(title = "Pourcentage (%)"))
  })
  
  # === Graphique 3 : Plan de passation de marchés ===
  output$graph_pm <- renderPlotly({
    req(input$projet_pm)
    df <- df_pm %>% filter(PROJETS == input$projet_pm)
    plot_ly(df, x = ~Trimestre, y = ~`Taux d'exécution`*100, type = 'bar', name = "Taux d'exécution",
            text = ~paste0(round(`Taux d'exécution`*100, 1), "%"),
            textposition = 'auto', marker = list(color = '#f7c6c7')) %>%
      layout(xaxis = list(title = "Trimestre"), yaxis = list(title ="Trimestre"),
             yaxis = list(title = "Taux d'exécution (%)"))
  })
}



# === Lancement de l'application ===
shinyApp(ui, server)