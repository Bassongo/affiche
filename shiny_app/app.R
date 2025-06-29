library(shiny)

ui <- fluidPage(
    titlePanel("Bienvenue à R Shiny"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("num", "Choisissez un nombre", 1, 100, 50)
        ),
        mainPanel(
            textOutput("result")
        )
    )
)

server <- function(input, output, session) {
    output$result <- renderText({
        paste("Vous avez choisi:", input$num)
    })
}

shinyApp(ui, server)
