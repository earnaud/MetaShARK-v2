#' @import shiny
#' @importFrom shinyFiles shinyFilesButton
#'
#' @noRd
DataFilesUI <- function(id, main.env) {
  return(
    fluidPage(
      tags$b("Disclaimers:"),
      tags$ul(
        tags$li("Until now, only table files are supported."),
        tags$li("Selecting a file will immediately upload it: beware of heavy files (> 5 Mb)."),
        class = "disclaimer"
      ),
      fluidRow(
        column(6,
          div(
            fileInput(
              NS(id, "add_data_files"),
              "Select data file(s) from your dataset",
              buttonLabel = span("Load files", icon("plus-circle")),
              multiple = TRUE,
              width = "100%"
            ),
            style = "display: inline-block; vertical-align: top;"
          )
        ),
        column(6,
          if(main.env$wip){
            wipRow(
              URL_Input_UI(
                NS(id, "url_files"),
                "Select data file(s) from an URL"
              ),
              actionButton(
                NS(id,"add_url_files"),
                label = "Get",
                icon = icon("download")
              )
            )
          }
        )
      ),
      uiOutput(NS(id, "data_files")),
      actionButton(NS(id, "remove_data_files"), "Remove",
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
DataFiles <- function(id, full.id, main.env) {
  moduleServer(id, function(input, output, session) {
    # Variable initialization ----
    
    # Data file upload ====
    # * Add data files ----
    observeEvent(input$add_data_files,
      {
        # validity checks
        req(isContentTruthy(input$add_data_files))
        withProgress(
          {
            # retrieve data files info
            .loaded.files <- input$add_data_files
            incProgress(0.2)
            
            # remove spaces
            .loaded.files$name <- gsub(" ", "_", .loaded.files$name)
            # add URL, description and table name columns
            .loaded.files$url <- rep("", dim(.loaded.files)[1])
            .loaded.files$description <- rep("", dim(.loaded.files)[1])
            .loaded.files$table.name <- rep("", dim(.loaded.files)[1])
            incProgress(0.2)
            
            # bind into input
            # empty variable
            if (isFALSE(
              isContentTruthy(main.env$local.rv$data.files) && 
              all(dim(main.env$local.rv$data.files) > 0)
            )) {
              main.env$local.rv$data.files <- .loaded.files
            } else { # non-empty variable
              sapply(.loaded.files$name, function(filename) {
                if (fs::is_dir(filename)) {
                  showNotification(
                    paste(filename, "is a folder."),
                    type = "warning"
                  )
                } else {
                  if (!filename %in% main.env$local.rv$data.files$name) {
                    main.env$local.rv$data.files <- unique(rbind(
                      main.env$local.rv$data.files,
                      .loaded.files[.loaded.files$name == filename, ]
                    ))
                  }
                }
              })
            }
            incProgress(0.2)
            
            # copies to the server
            file.copy(
              main.env$local.rv$data.files$datapath,
              paste0(main.env$PATHS$eal.tmp, main.env$local.rv$data.files$name)
            )
            incProgress(0.2)

            main.env$local.rv$data.files$datapath <- paste0(
              main.env$PATHS$eal.tmp, 
              main.env$local.rv$data.files$name
            )
            incProgress(0.2)
          },
          message = "Downloading data files"
        )
      },
      ignoreInit = TRUE,
      label = "EAL2: add files"
    )

    # * Remove data files ----
    observeEvent(input$remove_data_files,
      {
        # validity check
        req(input$select_data_files)

        # actions
        main.env$local.rv$data.files <- main.env$local.rv$data.files[
          !(main.env$local.rv$data.files$name %in% input$select_data_files),
        ]
      },
      label = "EAL2: remove files"
    )

    # Display data files ----
    # * UI ----
    output$data_files <- renderUI({
        validate(
          need(
            !is.null(main.env$local.rv$data.files) &&
              !any(dim(main.env$local.rv$data.files) == 0) &&
              !is.null(main.env$local.rv$data.files),
            "Select files to describe."
          )
        )
        
        df <- main.env$local.rv$data.files
        checkboxGroupInput(
          NS(full.id, "select_data_files"),
          "Select files to delete (all files here will be kept otherwise)",
          choiceNames = lapply(
            df$name,
            function(label) {
              # Variable initialization
              .id <- match(label, df$name)
              xls.warning <- if (grepl("xlsx?$", label))
                helpText("Only the first sheet of Excel files will be read.")
              else
                NULL
              
              # Output
              collapsibleUI(
                id = NS(id, .id),
                label = label,
                .hidden = FALSE,
                class = "inputBox",
                tagList(
                  fluidRow(
                    column(
                      6,
                      textInput(
                        NS(id, paste0(.id, "-dataName")),
                        "Data table name",
                        value = label
                      )
                    ),
                    column(
                      6,
                      URL_Input_UI(
                        NS(id, paste0(.id, "-dataURL")),
                        label = "Data remote location"
                      )
                    ),
                  ),
                  xls.warning,
                  fluidRow(
                    column(
                      12,
                      textAreaInput(
                        NS(id, paste0(.id, "-dataDesc")),
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
    
    # * Server ----
    observeEvent(names(input), {
      req(
        any(grepl("dataName", names(input))) ||
          any(grepl("dataURL", names(input))) ||
          any(grepl("dataDesc", names(input)))
      )
      sapply(main.env$local.rv$data.files$name, function(.id) {
        collapsible(.id)
        ind <- match(.id, main.env$local.rv$data.files$name)

        # Data name
        observeEvent(input[[paste0(ind, "-dataName")]],
          {
            isolate(
              main.env$local.rv$data.files[ind, "table.name"] <- input[[paste0(ind, "-dataName")]]
            )
          },
          ignoreInit = FALSE
        )
        # Data URL
        observeEvent(input[[paste0(ind, "-dataURL")]],
          {
            isolate(
              main.env$local.rv$data.files[ind, "url"] <- URL_Input(paste0(ind, "-dataURL"))
            )
          },
          ignoreInit = FALSE
        )
        # Description
        observeEvent(input[[paste0(ind, "-dataDesc")]],
          {
            isolate(
              main.env$local.rv$data.files[ind, "description"] <- input[[paste0(ind, "-dataDesc")]]
            )
          },
          ignoreInit = FALSE
        )
      })
    }, 
    label = "EAL2: set servers"
    )

    # * Data size ----
    observeEvent(main.env$local.rv$data.files, {
      req(isContentTruthy(main.env$local.rv$data.files))
      files.size <- if (isContentTruthy(main.env$local.rv$data.files$size)) {
        sum(main.env$local.rv$data.files$size)
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

      main.env$EAL$tag.list <- tagList(
        "Files size:",
        tags$p(
          utils::object.size(files.size),
          if (files.size >= files.size.max) {
            paste("Max. recommended:", utils:::format.object_size(files.size.max))
          } else {
            NULL
          },
          style = style
        )
      )
    },
    label = "EAL2: data files size"
    )

    # Saves ----
    observe({
      req(main.env$EAL$page == 2)
      invalidateLater(1000)
      main.env$EAL$completed <- isContentTruthy(main.env$local.rv$data.files) &&
        all(dim(main.env$local.rv$data.files) > 0)
    },
    label = "EAL2: set completed"
    )

    # Process data ----
    observeEvent(main.env$EAL$.next, {
      req(main.env$EAL$current == "Data Files")
      # Save
      saveReactive(main.env)
      
      # EMLAL templating function
      x <- try(
        EMLassemblyline::template_table_attributes(
          path = isolate(main.env$save.variable$SelectDP$dp.metadata.path),
          data.path = isolate(main.env$save.variable$SelectDP$dp.data.path),
          data.table = isolate(main.env$save.variable$DataFiles$name)
        )
      )
      if(class(x) == "try-error") {
        main.env$EAL$page <- main.env$EAL$page - 1
        browser()
      }
    },
    priority = 1,
    ignoreInit = TRUE,
    label = "EAL2: process data"
    )
  })
}
