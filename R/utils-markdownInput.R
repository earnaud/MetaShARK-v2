#' @title markdownInputUI
#'
#' @description A shiny input module designed to allow the user to write markdown and render it as
#' a HTML fix.
#'
#' @param id character. The input slot that will be used to access the value.
#' @param label character. Display label for the control, or NULL for no label.
#' @param icon 	character. An optional `icon()` to appear on the button.
#' @param preview logical. Shall a preview panel appear? (right-sided, 50\% width)
#' @param value character. Initial value of the text input (default to "")
#'
#' @return A HTML `reactive` containing the formatted input. Do call it as a reactive.
#'
#' @references Ace technologies \url{https://ace.c9.io/#nav=about}
#'
#' @import shiny
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
markdownInputUI <- function(id, label = "Text", icon = TRUE, preview = FALSE, value = "") {
  div(
    fluidRow(
      column(
        if (preview) 6 else 12,
        if (isFALSE(icon)) {
          tags$b(label)
        } else {
          span(tags$b(label), "(", icon("markdown"), "supported)")
        },
        shinyAce::aceEditor(
          NS(id, "md"),
          value = value,
          mode = "markdown",
          showLineNumbers = FALSE,
          tabSize = 2
        )
      ),
      if (preview) {
        column(
          6,
          h3("Preview:"),
          uiOutput(NS(id, "preview"))
        )
      }
    ),
    style = "border: 2px solid lightgrey; padding: 5px"
  )
}


#' @describeIn markdownInputUI Server side of the module.
#'
#' @import shiny
#' @importFrom markdown markdownToHTML
markdownInput <- function(id, preview = FALSE) {
  moduleServer(id, function(input, output, session) {
    rv <- reactive({
      input$md
    })

    if (preview) {
      output$preview <- renderUI({
        HTML(markdown::markdownToHTML(file = NULL, text = rv()))
      })
    }

    return(rv)
  })
}
