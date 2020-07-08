#' @title MiscellaneousUI
#'
#' @description
#'
#' @import shiny
#' textOutput div tagList
#' @importFrom shinyFiles shinyFilesButton
MiscellaneousUI <- function(id, help_label = NULL, value = "") {
  ns <- NS(id)

  fluidRow(
    # file selection
    column(
      4,
      tags$b(paste0("Select '", gsub(".*-(.*)$", "\\1", id), "' file.")),
      tags$br(),
      div(
        # if (isTRUE(server)) {
        fileInput(
          ns("file"),
          "",
          multiple = FALSE,
          buttonLabel = span("Load file", icon("file")),
        )
        # } else {
        #   shinyFilesButton(
        #     ns("file"),
        #     "Load file",
        #     paste0("Select '", gsub(".*-(.*)$", "\\1", id), "' file."),
        #     multiple = FALSE,
        #     icon = icon("file")
        #   )
        # }
      ),
      div(textOutput(ns("selected")), class = "ellipsis")
    ),
    # Content edition
    column(
      8,
      tagList(
        tags$b("Content"),
        help_label,
        markdownInputUI(
          ns("content"),
          label = "",
          value = value,
          preview = FALSE
        )
      )
    )
  ) # end of fluidRow
}

#' @title Miscellaneous
#'
#' @import shiny
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @importFrom shinyAce updateAceEditor
#' @importFrom fs path_home
Miscellaneous <- function(input, output, session, savevar, rv) {
  # Variable initialization -----------------------------------------------------
  ns <- session$ns
  volumes <- c(Home = path_home(), getVolumes()())

  # Get content -----------------------------------------------------
  rv$content <- callModule(markdownInput, "content", preview = FALSE)

  # Get file -----------------------------------------------------
  # if (isTRUE(server)) {
  observeEvent(input$file,
    {
      req(input$file)
      rv$file <- input$file$datapath
    },
    priority = 1
  )
  # }
  # else {
  #   shinyFileChoose(input, "file",
  #     roots = volumes,
  #     session = session
  #   )
  #   observeEvent(input$file,
  #     {
  #       req(input$file)
  #       rv$file <- parseFilePaths(volumes, input$file)$datapath
  #     },
  #     priority = 1
  #   )
  # }

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

  # Output -----------------------------------------------------
  return(rv)
}
