library(shiny)
library(shinyTree)

testShinyTree <- function(doc){
  ui <- pageWithSidebar(
    headerPanel("Automated shinyTree"),
    
    sidebarPanel(
      shinyTree("tree")
    ),
    mainPanel(
      verbatimTextOutput("selected"),
      actionButton("browse", "Browse", width = "100%")
    )
  )
  
  server <- function(input, output, session) {
    
    output$tree <- renderTree({
      doc
    })
    
    output$selected <- renderPrint({
      tree <- input$tree
      req(tree)
      get_selected(tree)
    })
    
    observeEvent(input$browse, {
      browser()
    })
  }
  
  shinyApp(ui, server)
}