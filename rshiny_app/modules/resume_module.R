resumeUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$link(rel="stylesheet", href="app0.css")),
    div(class="page",
        h2("Résumé consolidé", style="text-align:center; margin-top:20px;"),
        tableOutput(ns("resume_table"))
    )
  )
}

resumeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    f <- "data/Resume.xlsx"
    resume_data <- tryCatch(read_excel(f), error=function(e) data.frame())
    output$resume_table <- renderTable({
      head(resume_data)
    })
  })
}
