library(shiny)

test_UI <- function(id) {
  ns <- NS(id)
  
  tabPanel("One", textInput(ns("text"), "Write 'nope'"))
}

test_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$text, {
      text <- input$text
      
      shinyFeedback::hideFeedback("text")
      
      if(text == "nope")
        shinyFeedback::showFeedbackSuccess("text")
      else
        shinyFeedback::showFeedbackDanger("text", "No !")
    })
  })
}

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  actionButton("dev", "dev"),
  uiOutput("test")
)

server <- function(input, output, session) {
  output$test <- renderUI({
    do.call(
      tabsetPanel,
      args = c(
        id = session$ns("tabset"),
        lapply(
          c(session$ns("One"), session$ns("Two")),
          test_UI
        )
      )
    )
  })
  
  sapply(c("One","Two"), test_server)
  
  observeEvent(input$dev, {
    browser()
  })
  
  observeEvent(input$tabset, {
    message(sprintf("[Out] You are at %s", input$tabset))
  })
}

shinyApp(ui, server)