#' @title Data Package files
#'
#' @description UI part of the DataFiles module.
#'
#' @importFrom shiny NS fluidPage column tags HTML icon actionButton uiOutput tagList textOutput
#' @importFrom shinyFiles shinyFilesButton
DataFilesUI <- function(id, title, dev = FALSE, server = FALSE) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Features -----------------------------------------------------
      column(
        10,
        tags$h4("Data files"),
        tags$p("When selecting your files, you can't select
          folders. You can delete file(s) from your selection
          by ticking their box and clicking the 'Remove' button."),
        tags$p("DISCLAIMER: MetaShARK only supports tabulated files."),
        tags$div(
          if (isTRUE(server)) {
            tagList(
              fileInput(
                ns("add_data_files"),
                "Select data file(s) from your dataset",
                buttonLabel = span("Load files", icon("plus-circle")),
                multiple = TRUE
              )
            )
          } else {
            shinyFilesButton(
              ns("add_data_files"),
              "Load files",
              "Select data file(s) from your dataset",
              multiple = TRUE,
              icon = icon("plus-circle")
            )
          },
          style = "display: inline-block; vertical-align: top;"
        ),
        uiOutput(ns("data_files")),
        actionButton(ns("remove_data_files"), "Remove",
          icon = icon("minus-circle"),
          class = "redButton"
        )
      ), # end of column 1
      # NSB -----------------------------------------------------
      column(
        2,
        navSidebar(ns("nav"),
          .prev = FALSE,
          ... = tagList(
            textOutput(ns("warning_data_size")),
            textOutput(ns("overwrite"))
          )
        )
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
#' @importFrom shinyjs enable disable
#' @importFrom EMLassemblyline template_table_attributes
DataFiles <- function(input, output, session, savevar, globals, server) {
  ns <- session$ns
  
  # Variable initialization -----------------------------------------------------
  rv <- reactiveValues(
    data_files = data.frame(),
    files_list = character()
  )
  if (isTRUE(server)) {
    rv$tmpPaths <- character()
  }
  if (!isTRUE(server)) {
    volumes <- c(Home = globals$HOME, getVolumes()())
  }
  updateFileListTrigger <- makeReactiveTrigger()
  
  # On arrival on screen
  observeEvent(globals$EMLAL$HISTORY, {
    # dev: might evolve in `switch` if needed furtherly
    if (all(dim(savevar$emlal$DataFiles) == c(0, 0))) { # from create button in SelectDP
      rv$data_files <- data.frame()
    } else {
      rv$data_files <- savevar$emlal$DataFiles
      rv$files_list = rv$data_files$name
    }
  })
  
  # NSB -----------------------------------------------------
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar
  )
  callModule(
    nextTab, "nav",
    globals, "DataFiles"
  )
  
  # Data file upload -----------------------------------------------------
  # * Add data files -----------------------------------------------------
  if (!isTRUE(server)) {
    shinyFileChoose(
      input,
      "add_data_files",
      roots = volumes,
      session = session
    )
  }
  
  observeEvent(input$add_data_files, {
    # validity checks
    req(input$add_data_files)
    
    # retrieve data files info
    if (isTRUE(server)) {
      loadedFiles <- input$add_data_files
    } else {
      loadedFiles <- as.data.frame(
        parseFilePaths(volumes, input$add_data_files)
      )
    }
    # add URL, description and table name columns
    loadedFiles$url <- rep("", dim(loadedFiles)[1])
    loadedFiles$description <- rep("", dim(loadedFiles)[1])
    loadedFiles$table_name <- rep("", dim(loadedFiles)[1])
    
    if (any(dim(rv$data_files) == 0)) {
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
    
    # copies on the server
    if (isTRUE(server)) {
      withProgress(
        {
          file.copy(rv$data_files$datapath, paste0(globals$TEMP.PATH, rv$data_files$name))
          incProgress(1)
        },
        message = "Downloading data files"
      )
      
      rv$data_files$datapath <- paste0(globals$TEMP.PATH, rv$data_files$name)
    }
    
    rv$files_list <- rv$data_files$name
    
    # variable modifications
    savevar$emlal$DataFiles <- rv$data_files
  })
  
  # * Remove data files -----------------------------------------------------
  observeEvent(input$remove_data_files, {
    
    # validity check
    req(input$select_data_files)
    
    rv$files_list <- rv$files_list[!(rv$files_list %in% input$select_data_files)]
    
    # actions
    rv$data_files <- rv$data_files[
      rv$data_files$name != input$select_data_files,
      ]
    
    # variable modifications
    savevar$emlal$DataFiles <- rv$data_files
  })
  
  # Display data files -----------------------------------------------------
  # -- UI
  observeEvent(rv$files_list, {
    req(rv$files_list)
    df <- isolate(rv$data_files)
    
    output$data_files <- renderUI({
      disable("nav-nextTab")
      validate(
        need(
          !any(dim(df) == 0) && !is.null(df),
          "Select files to describe."
        )
      )
      enable("nav-nextTab")
      
      checkboxGroupInput(ns("select_data_files"),
        "Select files to delete (all files here will be kept otherwise)",
        choiceNames = lapply(
          df$name,
          function(label) {
            id <- match(label, df$name)
            collapsibleUI(
              id = ns(id),
              label = label,
              hidden = FALSE,
              class = "inputBox",
              tagList(
                fluidRow(
                  column(6,
                    textInput(
                      ns(paste0(id, "-dataName")),
                      "Data table name",
                      value = label
                    )
                  ),
                  column(6,
                    URL_Input_UI(
                      ns(paste0(id, "-dataURL")),
                      label = "Data remote location"
                    )
                  ),
                ),
                fluidRow(
                  column(12,
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

    # -- Server
    observeEvent(names(input), {
      req(
        any(grepl("dataName", names(input))) ||
          any(grepl("dataURL", names(input))) ||
          any(grepl("dataDesc", names(input)))
      )
      sapply(rv$data_files$name, function(id) {
        callModule(collapsible, id)
        ind <- match(id, rv$data_files$name)
        # Data name
        observeEvent(input[[paste0(ind, "-dataName")]], {
          isolate(
          rv$data_files[ind, "table_name"] <- input[[paste0(ind, "-dataName")]]
          )
        })
        # Data URL
        observeEvent(input[[paste0(ind, "-dataURL")]], {
          isolate(
          rv$data_files[ind, "url"] <- callModule(URL_Input, paste0(ind, "-dataURL"))
          )
        })
        # Description
        observeEvent(input[[paste0(ind, "-dataDesc")]], {
          isolate(
          rv$data_files[ind, "description"] <- input[[paste0(ind, "-dataDesc")]]
          )
        })
      })
    })
    
  # Warnings -----------------------------------------------------
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
  
  observeEvent(rv$data_files, {
    req(isTruthy(rv$data_files) &&
        all(dim(rv$data_files) != 0))
    savevar$emlal$DataFiles <- rv$data_files
  })
    
  # Process files -----------------------------------------------------
  # Template table
  observeEvent(input[["nav-nextTab"]],
    {
      # actions
      # -- copy files to <dp>_emldp/<dp>/data_objects
      sapply(rv$data_files$datapath,
        file.copy,
        to = savevar$emlal$SelectDP$dp_data_path,
        recursive = TRUE
      )
      
      # -- modify paths in save variable
      tmp <- savevar$emlal$DataFiles
      tmp$datapath <- sapply(
        rv$data_files$name,
        function(fname) {
          force(fname)
          paste0(savevar$emlal$SelectDP$dp_data_path, "/", fname)
        }
      )
      # Set metadatapath
      tmp$metadatapath <- sapply(
        rv$data_files$name,
        function(fname) {
          force(fname)
          paste(
            savevar$emlal$SelectDP$dp_metadata_path,
            sub(
              "(.*)\\.[a-zA-Z0-9]*$",
              "attributes_\\1.txt",
              fname
            ),
            sep = "/"
          )
        }
      )
      # Set table name
      tmp$table_name <- rv$data_files$table_name
      # Set description
      tmp$description <- rv$data_files$description
      # Set URL
      tmp$url <- rv$data_files$url
      
      savevar$emlal$DataFiles <- tmp
      
      # EMLAL templating function
      template_table_attributes(
        path = savevar$emlal$SelectDP$dp_metadata_path,
        data.path = savevar$emlal$SelectDP$dp_data_path,
        data.table = savevar$emlal$DataFiles$name
      )
    },
    priority = 1
  )
  
  # Output -----------------------------------------------------
  return(savevar)
}
