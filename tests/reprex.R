library(shiny)

test_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput("test", "test")
  )
}

test_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$test, {
      rv$text <- input$test
    })
  })
}

ui <- fluidPage(
  actionButton("dev", "dev"),
  uiOutput("test")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    text = ""
  )
  
  output$test <- renderUI({
    test_UI("outer")
  })
  
  test_server("outer")
  
  observeEvent(input$dev, {
    browser()
  })
}

shinyApp(ui, server)