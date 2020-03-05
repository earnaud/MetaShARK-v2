#' @title markdownInputUI
#' 
#' @description A shiny input module designed to allow the user to write markdown and render it as 
#' a HTML fix. 
#' 
#' It relies on the _Ace technology_ [https://ace.c9.io/#nav=about] with the package {shinyAce}.
#' 
#' @param id character. Shiny module id 
#' @param preview logical. Shall a preview panel appear? (right-sided, 50\% width)
#' @param value character. Initial value of the text input (default to "") 
#' 
#' @return A HTML `reactive` containing the formatted input. Do call it as a reactive.
#' 
#' @importFrom shinyAce aceEditor
markdownInputUI <- function(id, preview = FALSE, value = "") {
  ns <- NS(id)
  div(
    fluidRow(
      column(if(preview) 6 else 12,
        aceEditor(
          ns("md"),
          value = value, 
          mode = "markdown",
          showLineNumbers = FALSE
        )
      ),
      if(preview)
        column(6,
          h3("Preview:"),
          uiOutput(ns("preview"))
        )
    ),
    style = "border: 2px solid lightgrey; padding: 5px"
  )
}


#' @describeIn markdownInputUI Server side of the module.
#' 
#' @importFrom markdown markdownToHTML
markdownInput <- function(input, output, session, preview = FALSE) {
  rv <- reactive({
    HTML(markdownToHTML(file = NULL, text = input$md))
  })
  
  if(preview)
    output$preview <- renderUI({
      rv()
    })
  
  return(rv)
}
