#' @title multiFIlesInputUI
#'
#' @description UI part of the multiFilesInput. Allow the user to chose multiple files, and remove all or part of his selection.
#'
#' @param id shiny module id
#' @param helpText a character vector to give the user an explanation. Will be inserted into a tags$p() call.
#'
#' @importFrom shiny NS tagList tags icon actionButton uiOutput
#' @importFrom shinyFiles shinyFilesButton
multiFIlesInputUI <- function(id, helpText = NULL) {
  ns <- NS(id)

  tagList(
    tags$p(helpText),
    tags$div(
      shinyFilesButton(ns("add_files"),
        "Load files",
        "Select data file(s) from your dataset",
        multiple = TRUE,
        icon = icon("plus-circle")
      ),
      style = "display: inline-block; vertical-align: top;"
    ),
    actionButton(ns("remove_files"), "Remove",
      icon = icon("minus-circle"),
      class = "redButton"
    ),
    uiOutput(ns("files"))
  )
}

#' @title multiFilesInput
#'
#' @description server part for the multiFilesInput module
#'
#' @param input shiny module input
#' @param output shiny module output
#' @param session shiny module session
#'
#' @importFrom shiny reactiveValues observeEvent req renderUI checkboxGroupInput
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
multiFIlesInput <- function(input, output, session) {
  ns <- session$ns

  # Variable initialization ----
  rv <- reactiveValues(
    # to save
    files = data.frame()
    # local only
  )
  volumes <- c(Home = fs::path_home(), getVolumes()())

  # Add data files ----
  shinyFileChoose(input, "add_files",
    roots = volumes,
    # defaultRoot = HOME,
    session = session
  )

  observeEvent(input$add_files, {

    # validity checks
    req(input$add_files)

    # actions
    loadedFiles <- as.data.frame(
      parseFilePaths(volumes, input$add_files)
    )

    if (identical(rv$files, data.frame())) {
      rv$files <- loadedFiles
    } else {
      for (filename in loadedFiles$name) {
        if (!grepl("\\.", filename)) {
          message(filename, " is a folder or misformed file.")
        } else {
          rv$files <- unique(rbind(
            rv$files,
            loadedFiles[loadedFiles$name == filename, ]
          ))
        }
      }
    }
  })

  # Remove data files ----
  observeEvent(input$remove_files, {

    # validity check
    req(input$select_files)

    # actions
    rv$files <- rv$files[
      rv$files$name != input$select_files,
    ]
  })

  # Display data files ----
  output$files <- renderUI({

    # actions
    if (!identical(rv$files, data.frame()) &&
      !is.null(rv$files)) {
      checkboxGroupInput(ns("select_files"),
        "Select files to delete (all files here will be kept otherwise)",
        choices = rv$files$name
      )
    }
    else {
      return(NULL)
    }
  })

  # Out -----
  return(reactive(rv$files))
}
