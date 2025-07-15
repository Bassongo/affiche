# ===============================
# Module serveur : Assistant IA (Groq)
# ===============================

library(httr)
library(jsonlite)

# Fonction pour interroger l'API Groq (OpenAI compatible)
get_groq_response <- function(question) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (api_key == "") return("[Erreur] Token Groq manquant.")
  url <- "https://api.groq.com/openai/v1/chat/completions"
  headers <- httr::add_headers(
    "Authorization" = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  )
  body <- jsonlite::toJSON(list(
    model = "llama3-8b-8192",
    messages = list(list(role = "user", content = question)),
    max_tokens = 200
  ), auto_unbox = TRUE)
  res <- tryCatch({
    response <- httr::POST(url, headers, body = body)
    if (httr::status_code(response) == 200) {
      result <- httr::content(response)
      return(result$choices[[1]]$message$content)
    } else {
      return(paste("[Erreur API]", httr::status_code(response), httr::content(response, "text")))
    }
  }, error = function(e) {
    paste("[Erreur de connexion]", e$message)
  })
  res
}

# Fonction pour répondre aux questions sur le dashboard
get_dashboard_answer <- function(question, app_data) {
  q <- tolower(question)
  data <- app_data()
  if (grepl("budget", q)) {
    total <- sum(data$execution_budgetaire$Budget_PTBA_2025_FCFA, na.rm = TRUE)
    return(paste0("Le budget total PTBA est de ", round(total/1e9, 1), " milliards FCFA."))
  }
  if (grepl("projet", q) && grepl("actif|nombre|combien", q)) {
    n <- length(unique(data$execution_budgetaire$Projet))
    return(paste0("Il y a ", n, " projets actifs dans le tableau de bord."))
  }
  if (grepl("d[ée]cai", q) || grepl("montant d[ée]caiss[ée]", q)) {
    total <- sum(data$execution_budgetaire$Montant_reel_decaissé, na.rm = TRUE)
    return(paste0("Le montant total décaissé est de ", round(total/1e9, 1), " milliards FCFA."))
  }
  if (grepl("taux d'?ex[ée]cution", q)) {
    taux <- (sum(data$execution_budgetaire$Montant_reel_decaissé, na.rm = TRUE) / sum(data$execution_budgetaire$Budget_PTBA_2025_FCFA, na.rm = TRUE)) * 100
    return(paste0("Le taux d'exécution global est de ", round(taux, 1), "%"))
  }
  return(NULL)
}

# Initialisation des outputs de l'assistant IA avec historique façon chat
init_assistant_ia_outputs <- function(input, output, session, app_data) {
  chat_history <- reactiveVal(list())

  observeEvent(input$ask_ia, {
    req(input$user_question)
    old_history <- chat_history()
    user_msg <- list(role = "user", content = input$user_question)
    chat_history(append(old_history, list(user_msg)))
    dashboard_reply <- get_dashboard_answer(input$user_question, app_data)
    if (!is.null(dashboard_reply)) {
      ia_msg <- list(role = "assistant", content = dashboard_reply)
    } else {
      ia_reply <- get_groq_response(input$user_question)
      ia_msg <- list(role = "assistant", content = ia_reply)
    }
    chat_history(append(chat_history(), list(ia_msg)))
    updateTextInput(session, "user_question", value = "")
  })

  output$chat_history <- renderUI({
    history <- chat_history()
    tagList(
      lapply(history, function(msg) {
        if (msg$role == "user") {
          div(style = "text-align: right; margin: 10px;",
              span(style = "background: #e6f7ff; color: #003366; padding: 10px 15px; border-radius: 15px; display: inline-block; max-width: 70%;",
                   msg$content)
          )
        } else {
          div(style = "text-align: left; margin: 10px;",
              span(style = "background: #f0f0f0; color: #222; padding: 10px 15px; border-radius: 15px; display: inline-block; max-width: 70%;",
                   msg$content)
          )
        }
      })
    )
  })
} 