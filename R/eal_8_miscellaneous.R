#' @import shiny
#' @importFrom shinyFiles shinyFilesButton
#' 
#' @noRd
MiscellaneousUI <- function(id, help.label = NULL, value = "") {
  
  fluidRow(
    # file selection
    column(
      4,
      tags$b(paste0("Select '", gsub(".*-(.*)$", "\\1", id), "' file.")),
      tags$br(),
      div(
        fileInput(
          NS(id, "file"),
          "",
          multiple = FALSE,
          buttonLabel = span("Load file", icon("file")),
        )
      ),
      div(textOutput(NS(id, "selected")), class = "ellipsis")
    ),
    # Content edition
    column(
      8,
      tagList(
        tags$b("Content"),
        help.label,
        markdownInputUI(
          NS(id, "content"),
          label = "",
          value = value,
          preview = FALSE
        )
      )
    )
  ) # end of fluidRow
}

#' @import shiny
#' @importFrom shinyAce updateAceEditor
#' 
#' @noRd
Miscellaneous <- function(id){
  moduleServer(id, function(id, input, output, session, save.variable, rv) {
    # Get content ----
    rv$content <- markdownInput("content", preview = FALSE)
    
    # Get file ----
    observeEvent(input$file,
      {
        req(input$file)
        rv$file <- input$file$datapath
      },
      priority = 1
    )
    
    observeEvent(input$file,
      {
        req(
          isTruthy(input$file) ||
            isTruthy(names(input))
        )
        updateAceEditor(
          session,
          "content-md",
          value = readPlainText(rv$file)
        )
      },
      priority = 0
    )
    
    # UI Verbose
    output$selected <- renderText({
      paste(
        basename(rv$file),
        "\n(in:", dirname(rv$file), ")"
      )
    })
    
    # Output ----
    return(rv)
  })
}
