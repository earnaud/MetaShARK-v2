#' @title Data Package files
#'
#' @description UI part of the DataFiles module.
#'
#' @importFrom shiny NS fluidPage column tags HTML icon actionButton uiOutput tagList textOutput
#' @importFrom shinyFiles shinyFilesButton
DataFilesUI <- function(id, dev = FALSE) {
  ns <- NS(id)

  return(
    fluidPage(
      tags$b("Disclaimers:"),
      tags$ul(
        tags$li("Until now, only table files are supported."),
        tags$li("Selecting a file will immediately upload it: beware of heavy files (> 5 Mb)."),
        class = "disclaimer"
      ),
      tags$div(
        # if (isTRUE(server)) {
        tagList(
          fileInput(
            ns("add_data_files"),
            "Select data file(s) from your dataset",
            buttonLabel = span("Load files", icon("plus-circle")),
            multiple = TRUE
          )
        ),
        style = "display: inline-block; vertical-align: top;"
      ),
      uiOutput(ns("data_files")),
      actionButton(ns("remove_data_files"), "Remove",
        icon = icon("minus-circle"),
        class = "redButton"
      )
    ) # end fluidPage
  ) # end return
}

#' @title Data Package files
#'
#' @description server part of the DataFiles module.
#'
#' @importFrom shiny observeEvent reactiveValues callModule req checkboxGroupInput renderUI renderText
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @importFrom shinyjs onclick enable disable
#' @importFrom EMLassemblyline template_table_attributes
DataFiles <- function(input, output, session,
                      savevar, globals, NSB) {
  ns <- session$ns
  if (globals$dev) {
    onclick("dev",
      {
        req(globals$EMLAL$NAVIGATE == 2)
        browser()
      },
      asis = TRUE
    )
  }

  # Variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    data.files = data.frame()
  )

  if (checkTruth(savevar$emlal$DataFiles)) { # from create button in SelectDP
    .ind <- which(file.exists(savevar$emlal$DataFiles$datapath))
    .col <- which(names(savevar$emlal$DataFiles) != "metadatapath")
    rv$data.files <- savevar$emlal$DataFiles[.ind, .col]
  }

  # Data file upload -----------------------------------------------------
  # * Add data files -----------------------------------------------------
  observeEvent(input$add_data_files,
    {
      # validity checks
      req(input$add_data_files)

      # retrieve data files info
      loadedFiles <- input$add_data_files

      req(checkTruth(loadedFiles))

      # remove spaces
      loadedFiles$name <- gsub(" ", "_", loadedFiles$name)

      # add URL, description and table name columns
      loadedFiles$url <- rep("", dim(loadedFiles)[1])
      loadedFiles$description <- rep("", dim(loadedFiles)[1])
      loadedFiles$table_name <- rep("", dim(loadedFiles)[1])

      # bind into input
      if (isFALSE(checkTruth(rv$data.files) && all(dim(rv$data.files) > 0))) {
        rv$data.files <- loadedFiles
      } else {
        sapply(loadedFiles$name, function(filename){
        # for (filename in loadedFiles$name) {
          if (fs::is_dir(filename)) {
            showNotification(
              paste(filename, "is a folder."),
              type = "warning"
            )
          } else
          if (!filename %in% rv$data.files$name) {
            rv$data.files <- unique(rbind(
              rv$data.files,
              loadedFiles[loadedFiles$name == filename, ]
            ))
          }
        })
      }

      # copies on the server
      withProgress(
        {
          file.copy(
            rv$data.files$datapath,
            paste0(globals$TEMP.PATH, rv$data.files$name)
          )
          incProgress(0.8)

          rv$data.files$datapath <- paste0(globals$TEMP.PATH, rv$data.files$name)
          incProgress(0.2)
        },
        message = "Downloading data files"
      )
    },
    ignoreInit = TRUE,
    label = "EAL2: add files"
  )

  # * Remove data files -----------------------------------------------------
  observeEvent(input$remove_data_files,
    {
      # validity check
      req(input$select_data_files)

      browser()
      
      # actions
      rv$data.files <- rv$data.files[
        !(rv$data.files$name %in% input$select_data_files),
      ]
    },
    label = "EAL2: remove files"
  )

  # Display data files -----------------------------------------------------
  # * UI ----
  observeEvent(rv$data.files, {
    df <- isolate(rv$data.files)

    output$data_files <- renderUI({
      validate(
        need(
          !any(dim(df) == 0) && !is.null(df),
          "Select files to describe."
        )
      )

      checkboxGroupInput(ns("select_data_files"),
        "Select files to delete (all files here will be kept otherwise)",
        choiceNames = lapply(
          df$name,
          function(label) {
            # Variable initialization
            id <- match(label, df$name)
            xls.warning <- if (grepl("xlsx?$", label)) {
              tags$div(
                "Only the first sheet of Excel files will be read.",
                style = "color: red"
              )
            } else {
              NULL
            }

            # Output
            collapsibleUI(
              id = ns(id),
              label = label,
              hidden = FALSE,
              class = "inputBox",
              tagList(
                fluidRow(
                  column(
                    6,
                    textInput(
                      ns(paste0(id, "-dataName")),
                      "Data table name",
                      value = label
                    )
                  ),
                  column(
                    6,
                    URL_Input_UI(
                      ns(paste0(id, "-dataURL")),
                      label = "Data remote location"
                    )
                  ),
                ),
                xls.warning,
                fluidRow(
                  column(
                    12,
                    textAreaInput(
                      ns(paste0(id, "-dataDesc")),
                      "Data Table Description",
                      value = paste("Content of", label),
                      width = "100%"
                    )
                  )
                )
              )
            )
          }
        ),
        choiceValues = df$name
      )
    })
  })

  # * Server ----
  observeEvent(names(input), {
    req(
      any(grepl("dataName", names(input))) ||
        any(grepl("dataURL", names(input))) ||
        any(grepl("dataDesc", names(input)))
    )
    sapply(rv$data.files$name, function(id) {
      callModule(collapsible, id)
      ind <- match(id, rv$data.files$name)

      # Data name
      observeEvent(input[[paste0(ind, "-dataName")]],
        {
          isolate(
            rv$data.files[ind, "table_name"] <- input[[paste0(ind, "-dataName")]]
          )
        },
        ignoreInit = FALSE
      )
      # Data URL
      observeEvent(input[[paste0(ind, "-dataURL")]],
        {
          isolate(
            rv$data.files[ind, "url"] <- callModule(URL_Input, paste0(ind, "-dataURL"))
          )
        },
        ignoreInit = FALSE
      )
      # Description
      observeEvent(input[[paste0(ind, "-dataDesc")]],
        {
          isolate(
            rv$data.files[ind, "description"] <- input[[paste0(ind, "-dataDesc")]]
          )
        },
        ignoreInit = FALSE
      )
    })
  })

  # Warnings: data size
  observeEvent(rv$data.files, {
    req(checkTruth(rv$data.files))
    files_size <- if (checkTruth(rv$data.files$size)) {
      sum(rv$data.files$size)
    } else {
      0
    }
    files_size_max <- globals$THRESHOLDS$data_files_size_max

    style <- if (files_size < 0.9 * files_size_max) {
      "color: green;"
    } else if (files_size >= 0.9 * files_size_max && files_size < files_size_max) {
      "color: gold;"
    } else {
      "color: red"
    }

    NSB$tagList <- tagList(
      "Files size:",
      tags$p(
        utils:::format.object_size(files_size, "auto"),
        if (files_size >= files_size_max) {
          paste("Max. recommended:", utils:::format.object_size(files_size_max, "auto"))
        } else {
          NULL
        },
        style = style
      )
    )
  })

  # Saves -----------------------------------------------------
  observe({
    globals$EMLAL$COMPLETE_CURRENT <- checkTruth(rv$data.files) &&
      all(dim(rv$data.files) > 0)
  })

  observeEvent(NSB$SAVE,
    {
      req(tail(globals$EMLAL$HISTORY, 1) == "Data Files")
      req(isTruthy(rv$data.files$name))

      savevar <- saveReactive(
        savevar = savevar,
        rv = list(DataFiles = rv)
      )
    },
    label = "Save_DataFiles",
    ignoreInit = TRUE
  )

  # Process files -----------------------------------------------------
  observeEvent(NSB$NEXT,
    {
      req(globals$EMLAL$CURRENT == "Data Files")
      # Save
      savevar <- saveReactive(
        savevar,
        rv = list(DataFiles = rv)
      )

      # EMLAL templating function
      try(
        template_table_attributes(
          path = savevar$emlal$SelectDP$dp_metadata_path,
          data.path = savevar$emlal$SelectDP$dp_data_path,
          data.table = savevar$emlal$DataFiles$name
        )
      )
    },
    priority = 1,
    ignoreInit = TRUE
  )

  # Output -----------------------------------------------------
  return(savevar)
}
