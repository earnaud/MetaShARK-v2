library(shiny)
library(shinyTree)
library(shinycssloaders)

source("R/utils-followPath.R")

# Main app ====
renderShinyTree <- function(doc, tree){
  ui <- fluidPage(
    h1("Automated shinyTree"),
    
    testShinyTree_UI("doc")
  )
  
  server <- function(input, output, session) {
    callModule(testShinyTree, "doc", doc, tree)
  }
  
  shinyApp(ui, server)
}

# Module ====
testShinyTree_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # shinydashboard::box(width = 4,
      column(4,
        shinyTree(
          ns("tree"),
          theme = "proton"
        ) %>% withSpinner
      ),
      # shinydashboard::box(width = 8,
      column(8,
        uiOutput(ns("doc"))
        # , verbatimTextOutput(ns("selected")),
        , actionButton(ns("browse"), "Browse", width = "100%")
      )
    ) 
  )
}

testShinyTree <- function(input, output, session, doc, tree) {
  output$tree <- renderTree({
    tree
  })
  
  # output$selected <- renderPrint({
  #   browser()
  #   tree <- input$tree
  #   req(tree)
  #   get_selected(tree)
  # })
  
  output$doc <- renderUI({
    tree.node <- get_selected(input$tree)
    validate(
      need(unlist(tree.node), "(Select an item first)")
    )
    path <- paste(c(attr(tree.node[[1]], "ancestry"), unlist(tree.node)), collapse = "/")
    doc.node <- followPath(doc, path)
    if("annotation" %in% names(doc.node))
      doc.node$annotation
    else
      helpText("No content found at:", path)
  })
  
  observeEvent(input$browse, {
    browser()
  })
}
