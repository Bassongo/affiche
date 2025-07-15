# ===============================
# Module serveur : Résumé
# ===============================

library(readxl)

# Initialisation des outputs résumé
init_resume_outputs <- function(input, output, session) {
  # Chargement des données résumé
  resume_data <- reactive({
    tryCatch({
      f <- "data/Resume.xlsx"
      if (!file.exists(f)) {
        cat("❌ Fichier 'Resume.xlsx' non trouvé\n")
        return(data.frame())
      }
      read_excel(f)
    }, error = function(e) {
      cat("❌ Erreur lors du chargement des données résumé:", e$message, "\n")
      return(data.frame())
    })
  })

  # Affichage du tableau résumé
  output$resume_table <- renderTable({
    data <- resume_data()
    if (nrow(data) == 0) {
      return(data.frame(Message = "Données non disponibles"))
    }
    head(data, 20)  # Limiter à 20 lignes pour l'affichage
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs')
} 