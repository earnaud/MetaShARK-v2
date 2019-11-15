#' @title DPfilesUI
#'
#' @description UI part of the EML AL module (step 2: select data files for
#' the data package)
#'
#' @param title page title
#' @param dev logical. Shall the dev items appear?
#'
#' @importFrom shiny NS fluidPage column fluidRow HTML tags tagList icon
#' actionButton uiOutput textOutput
#' @importFrom shinyFiles shinyFilesButton
DPfilesUI <- function(id, title, dev = FALSE) {
  ns <- NS(id)
  return(
    fluidPage(
      # main panel
      column(
        10,
        tags$h4("Data files"),
        HTML("When selecting your files, you can't select
                    folders. You can delete file(s) from your
                    selection by ticking their box and clicking
                    the 'Remove' button.<br>"),
        tags$div(
          shinyFilesButton(ns("add_data_files"),
            "Load files",
            "Select data file(s) from your dataset",
            multiple = TRUE,
            icon = icon("plus-circle")
          ),
          style = "display: inline-block; vertical-align: top;"
        ),
        actionButton(ns("remove_data_files"), "Remove",
          icon = icon("minus-circle"),
          class = "redButton"
        ),
        uiOutput(ns("data_files"))
      ), # end of column 1
      column(
        2,
        navSidebar(ns("nav"),
          .prev = FALSE,
          ... = tagList(
            textOutput(ns("warning_data_size")),
            textOutput(ns("overwrite"))
          )
        ),
        if (dev) actionButton(ns("checkDPfiles"), "Dev")
      ) # end column 2^
    ) # end fluidPage
  ) # end return
}

#' @title DPfiles
#'
#' @description server part of the EML AL module (step 2: select data files for
#' the data package)
#'
#' @param savevar global reactiveValue containing the saved information
#' entered by the user.
#' @param globals global list containing fixed setting values for the
#' app.
#'
#' @importFrom shiny observeEvent reactiveValues callModule req renderUI
#' checkboxGroupInput renderText
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @importFrom shinyjs enable disable
#' @importFrom EMLassemblyline template_table_attributes
DPfiles <- function(input, output, session,
                    savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$checkDPfiles, {
      browser()
    })
  }

  # Variable initialization ----
  rv <- reactiveValues(
    # to save
    data_files = data.frame()
    # local only
  )
  volumes <- c(Home = globals$HOME, getVolumes()())
  updateFileListTrigger <- makeReactiveTrigger()

  # On arrival on screen
  observeEvent(globals$EMLAL$PREVIOUS, {
    # dev: might evolve in `switch` if needed furtherly
    rv$data_files <- if (globals$EMLAL$PREVIOUS == "create") { # from create button in selectDP
      data.frame()
    } else {
      savevar$emlal$DPfiles$dp_data_files
    }

    updateFileListTrigger$trigger()
  })

  # Navigation buttons ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$selectDP$dp_path,
    savevar$emlal$selectDP$dp_name
  )
  callModule(
    nextTab, "nav",
    globals, "DPfiles"
  )

  # Data file upload ----
  # Add data files
  shinyFileChoose(input, "add_data_files",
    roots = volumes,
    session = session
  )

  observeEvent(input$add_data_files, {
    # validity checks
    req(input$add_data_files)

    # actions
    loadedFiles <- as.data.frame(
      parseFilePaths(volumes, input$add_data_files)
    )

    if (identical(rv$data_files, data.frame())) {
      rv$data_files <- loadedFiles
    } else {
      for (filename in loadedFiles$name) {
        if (!grepl("\\.", filename)) {
          message(filename, " is a folder.")
        } else {
          rv$data_files <- unique(rbind(
            rv$data_files,
            loadedFiles[loadedFiles$name == filename, ]
          ))
        }
      }
    }

    # variable modifications
    savevar$emlal$DPfiles$dp_data_files <- rv$data_files
  })

  # Remove data files
  observeEvent(input$remove_data_files, {

    # validity check
    req(input$select_data_files)

    # actions
    rv$data_files <- rv$data_files[
      rv$data_files$name != input$select_data_files,
    ]
  })

  # Display data files
  output$data_files <- renderUI({
    updateFileListTrigger$depend()

    # actions
    if (!identical(rv$data_files, data.frame()) &&
      !is.null(rv$data_files)) {
      enable("nav-nextTab")
      checkboxGroupInput(ns("select_data_files"),
        "Select files to delete (all files here will be kept otherwise)",
        choices = rv$data_files$name
      )
    }
    else {
      disable("nav-nextTab")
      return(NULL)
    }
  })

  # Warnings ----
  # data size
  output$warning_data_size <- renderText({
    if (sum(rv$data_files$size) > globals$THRESHOLDS$data_files_size_max) {
      paste(
        "WARNING:", sum(rv$data_files$size),
        "bytes are about to be duplicated for data package assembly"
      )
    } else {
      ""
    }
  })

  # overwrite files
  output$warning_overwrite <- renderText({
    if (identical(
      dir(paste0(path, "/", dp, "/data_objects/")),
      character(0)
    )
    ) {
      paste("WARNING:", "Selected files will overwrite
            already loaded ones.")
    } else {
      ""
    }
  })

  # Process files ----
  # Template table
  observeEvent(input[["nav-nextTab"]],
    {
      # variable initialization
      dp <- savevar$emlal$selectDP$dp_name
      path <- savevar$emlal$selectDP$dp_path

      # actions
      # -- copy files to <dp>_emldp/<dp>/data_objects
      sapply(rv$data_files$datapath,
        file.copy,
        to = paste0(path, "/", dp, "/data_objects/"),
        recursive = TRUE
      )
      # -- modify paths in save variable
      tmp <- savevar$emlal$DPfiles$dp_data_files
      tmp$datapath <- sapply(
        rv$data_files$name,
        function(dpname) {
          force(dpname)
          paste0(path, "/", dp, "/data_objects/", dpname)
        }
      )
      tmp$metadatapath <- sapply(
        rv$data_files$name,
        function(dpname) {
          force(dpname)
          paste0(
            path, "/", dp, "/metadata_templates/",
            sub(
              "(.*)\\.[a-zA-Z0-9]*$",
              "attributes_\\1.txt",
              dpname
            )
          )
        }
      )
      savevar$emlal$DPfiles$dp_data_files <- tmp

      # EMLAL templating function
      template_table_attributes(
        path = paste0(path, "/", dp, "/metadata_templates"),
        data.path = paste0(path, "/", dp, "/data_objects"),
        data.table = rv$data_files$name,
      )
      cat("Done\n")
    },
    priority = 1
  )

  # Output ----
  return(savevar)
}
