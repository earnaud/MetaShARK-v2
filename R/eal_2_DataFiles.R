#' @import shiny
#' @importFrom shinyFiles shinyFilesButton
#' 
#' @noRd
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

#' @import shiny
#' @importFrom shinyFiles getVolumes shinyFileChoose parseFilePaths
#' @importFrom shinyjs onclick enable disable
#' @importFrom EMLassemblyline template_table_attributes
#' 
#' @noRd
DataFiles <- function(input, output, session, save.variable, main.env, NSB) {
  ns <- session$ns
  if (main.env$DEV) {
    shinyjs::onclick("dev",
      {
        req(main.env$EAL$navigate == 2)
        browser()
      },
      asis = TRUE
    )
  }

  # Variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    data.files = data.frame(stringsAsFactors = FALSE)
  )

  if (checkTruth(save.variable$emlal$DataFiles)) { # from create button in SelectDP
    .ind <- which(file.exists(save.variable$emlal$DataFiles$datapath))
    .col <- which(names(save.variable$emlal$DataFiles) != "metadatapath")
    rv$data.files <- save.variable$emlal$DataFiles[.ind, .col]
  }

  # Data file upload -----------------------------------------------------
  # * Add data files -----------------------------------------------------
  observeEvent(input$add_data_files,
    {
      # validity checks
      req(input$add_data_files)

      # retrieve data files info
      loaded.files <- input$add_data_files

      req(checkTruth(loaded.files))

      # remove spaces
      loaded.files$name <- gsub(" ", "_", loaded.files$name)

      # add URL, description and table name columns
      loaded.files$url <- rep("", dim(loaded.files)[1])
      loaded.files$description <- rep("", dim(loaded.files)[1])
      loaded.files$table.name <- rep("", dim(loaded.files)[1])

      # bind into input
      if (isFALSE(checkTruth(rv$data.files) && all(dim(rv$data.files) > 0))) {
        rv$data.files <- loaded.files
      } else {
        sapply(loaded.files$name, function(filename){
          if (fs::is_dir(filename)) {
            showNotification(
              paste(filename, "is a folder."),
              type = "warning"
            )
          } else
          if (!filename %in% rv$data.files$name) {
            rv$data.files <- unique(rbind(
              rv$data.files,
              loaded.files[loaded.files$name == filename, ]
            ))
          }
        })
      }

      # copies on the server
      withProgress(
        {
          file.copy(
            rv$data.files$datapath,
            paste0(main.env$PATHS$eal.tmp, rv$data.files$name)
          )
          incProgress(0.8)

          rv$data.files$datapath <- paste0(main.env$PATHS$eal.tmp, rv$data.files$name)
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
            rv$data.files[ind, "table.name"] <- input[[paste0(ind, "-dataName")]]
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
    files.size <- if (checkTruth(rv$data.files$size)) {
      sum(rv$data.files$size)
    } else {
      0
    }
    files.size.max <- main.env$VALUES$thresholds$files.size.max

    style <- if (files.size < 0.9 * files.size.max) {
      "color: green;"
    } else if (files.size >= 0.9 * files.size.max && files.size < files.size.max) {
      "color: gold;"
    } else {
      "color: red"
    }

    NSB$tagList <- tagList(
      "Files size:",
      tags$p(
        utils::object.size(files.size),
        if (files.size >= files.size.max) {
          paste("Max. recommended:", utils::object.size(files.size.max))
        } else {
          NULL
        },
        style = style
      )
    )
  })

  # Saves -----------------------------------------------------
  observe({
    main.env$EAL$current[2] <- checkTruth(rv$data.files) &&
      all(dim(rv$data.files) > 0)
  })

  observeEvent(NSB$SAVE,
    {
      req(utils::tail(main.env$EAL$history, 1) == "Data Files")
      req(isTruthy(rv$data.files$name))

      save.variable <- saveReactive(
        save.variable = savevar,
        rv = list(DataFiles = rv)
      )
    },
    label = "Save_DataFiles",
    ignoreInit = TRUE
  )

  # Process files ----
  observeEvent(NSB$NEXT,
    {
      req(main.env$EAL$current[1] == "Data Files")
      # Save
      save.variable <- saveReactive(
        save.variable,
        rv = list(DataFiles = rv)
      )

      # EMLAL templating function
      try(
        EMLassemblyline::template_table_attributes(
          path = save.variable$emlal$SelectDP$dp.metadata.path,
          data.path = save.variable$emlal$SelectDP$dp.data.path,
          data.table = save.variable$emlal$DataFiles$name
        )
      )
    },
    priority = 1,
    ignoreInit = TRUE
  )

  # Output -----------------------------------------------------
  return(save.variable)
}
