#' @title markdownInputUI
#' 
#' @description A shiny input module designed to allow the user to write markdown and render it as 
#' a HTML fix. 
#' 
#' @param inputId character. The input slot that will be used to access the value.
#' @param label character. Display label for the control, or NULL for no label.
#' @param preview logical. Shall a preview panel appear? (right-sided, 50\% width)
#' @param value character. Initial value of the text input (default to "") 
#' 
#' @return A HTML `reactive` containing the formatted input. Do call it as a reactive.
#' 
#' @references Ace technologies \url{https://ace.c9.io/#nav=about}
#' 
#' @importFrom shinyAce aceEditor
#' 
#' @examples 
#' 
#' ui <- fluidPage(
#'   markdownInputUI("md", label = "Write markdown here", preview = TRUE)
#' )
#' 
#' server <- function(input, output, session) {
#'   callModule(markdownInput, "md", preview = TRUE)
#' }
#' 
#' shinyApp(ui, server)
#' 
markdownInputUI <- function(inputId, label = "Text", icon = TRUE, preview = FALSE, value = "") {
  ns <- NS(inputId)
  div(
    fluidRow(
      column(if(preview) 6 else 12,
        if(isFALSE(icon)) tags$b(label) else span(tags$b(label), "(", icon("markdown"), "supported)"),
        aceEditor(
          ns("md"),
          value = value, 
          mode = "markdown",
          showLineNumbers = FALSE,
          tabSize = 2
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
    input$md
  })
  
  if(preview)
    output$preview <- renderUI({
      HTML(markdownToHTML(file = NULL, text = rv()))
    })
  
  return(rv)
}
