#' @title Data Package files
#'
#' @description UI part of the DataFiles module.
#'
#' @importFrom shiny NS fluidPage column tags HTML icon actionButton uiOutput tagList textOutput
#' @importFrom shinyFiles shinyFilesButton
DataFilesUI <- function(id, dev = FALSE, server = FALSE) {
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
  savevar, globals, server, NSB) {
  ns <- session$ns
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 2)
      browser()
    }, asis=TRUE)
  
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
  
  if (!isTruthy(unlist(savevar$emlal$DataFiles))) { # from create button in SelectDP
    rv$data_files <- data.frame()
  } else {
    rv$data_files <- savevar$emlal$DataFiles
    rv$files_list <- rv$data_files$name
  }
  
  # Clean existing data files in data_object
  if(isTruthy(rv$data_files$datapath)){
    file.copy(
      rv$data_files$datapath,
      to = globals$TEMP.PATH
    )
    rv$data_files$datapath <- paste0(globals$TEMP.PATH, basename(rv$data_files$datapath))
  }
  
  file.remove(
    dir(
      savevar$emlal$SelectDP$dp_data_path,
      full.names = TRUE
    )
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
    
    req(checkTruth(loadedFiles))
    
    # add URL, description and table name columns
    loadedFiles$url <- rep("", dim(loadedFiles)[1])
    loadedFiles$description <- rep("", dim(loadedFiles)[1])
    loadedFiles$table_name <- rep("", dim(loadedFiles)[1])
    
    if (any(dim(rv$data_files) == 0)) {
      rv$data_files <- loadedFiles
    } else {
      for (filename in loadedFiles$name) {
        if (!grepl("\\.", filename)) {
          showNotification(
            paste(filename, "is a folder."),
            type = "warning"
          )
        } else {
          rv$data_files <- unique(rbind(
            rv$data_files,
            loadedFiles[loadedFiles$name == filename, ]
          ))
        }
      }
    }
    
    # copies on the server
    # if (isTRUE(server)) {
    withProgress({
      file.copy(
        rv$data_files$datapath, 
        paste0(globals$TEMP.PATH, rv$data_files$name)
      )
      incProgress(0.8)
      
      rv$data_files$datapath <- paste0(globals$TEMP.PATH, rv$data_files$name)
      incProgress(0.2)
    }, message = "Downloading data files")
    # }
    
    # variable modifications
    rv$files_list <- rv$data_files$name
    savevar$emlal$DataFiles <- rv$data_files
  }, ignoreInit = TRUE)
  
  # * Remove data files -----------------------------------------------------
  onclick("remove_data_files", {
    
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
  # * UI ----
  observeEvent(rv$files_list, {
    # req(rv$files_list)
    df <- isolate(rv$data_files)
    
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
            xls.warning <- if(grepl("xlsx?$", label)) 
              tags$div(
                "Only the first sheet of Excel files will be read.",
                style = "color: red"
              )
            else
              NULL
            
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
    sapply(rv$data_files$name, function(id) {
      callModule(collapsible, id)
      ind <- match(id, rv$data_files$name)
      # Data name
      observeEvent(input[[paste0(ind, "-dataName")]], {
        isolate(
          rv$data_files[ind, "table_name"] <- input[[paste0(ind, "-dataName")]]
        )
      }, ignoreInit = FALSE)
      # Data URL
      observeEvent(input[[paste0(ind, "-dataURL")]], {
        isolate(
          rv$data_files[ind, "url"] <- callModule(URL_Input, paste0(ind, "-dataURL"))
        )
      }, ignoreInit = FALSE)
      # Description
      observeEvent(input[[paste0(ind, "-dataDesc")]], {
        isolate(
          rv$data_files[ind, "description"] <- input[[paste0(ind, "-dataDesc")]]
        )
      }, ignoreInit = FALSE)
    })
  })
  
  # Warnings: data size
  observeEvent(rv$data_files, {
    req(checkTruth(rv$data_files))
    files_size <- if(checkTruth(rv$data_files$size))
      sum(rv$data_files$size)
    else
      0
    files_size_max <- globals$THRESHOLDS$data_files_size_max
    
    style <- if(files_size < 0.9*files_size_max){
      "color: green;"
    } else if(files_size >= 0.9*files_size_max && files_size < files_size_max){
      "color: gold;"
    } else {
      "color: red"
    }
    
    NSB$tagList <- tagList(
      "Files size:",
      tags$p(
        utils:::format.object_size(files_size, "auto"),
        if(files_size >= files_size_max) 
          paste("Max. recommended:", utils:::format.object_size(files_size_max, "auto") )
        else 
          NULL,
        style = style
      )
    )
  })
  
  # Saves -----------------------------------------------------
  observeEvent(rv$data_files, {
    globals$EMLAL$COMPLETE_CURRENT <- checkTruth(rv$data_files) && all(dim(rv$data_files) != 0)
    req(globals$EMLAL$COMPLETE_CURRENT)
    savevar$emlal$DataFiles <- rv$data_files
  })
  
  observeEvent(NSB$SAVE, {
    req(isTruthy(rv$data_files$name))
    savevar <- .saveDataFiles(savevar = savevar, rv = rv)
  }, ignoreInit = TRUE)
  
  # Process files -----------------------------------------------------
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Data Files")
    
    # -- copy files to <dp>_emldp/<dp>/data_objects
    file.copy(
      from = rv$data_files$datapath,
      to = savevar$emlal$SelectDP$dp_data_path
    )
    
    # -- modify paths in save variable
    savevar <- .saveDataFiles(savevar = savevar, rv = rv)
    
    # EMLAL templating function
    template_table_attributes(
      path = savevar$emlal$SelectDP$dp_metadata_path,
      data.path = savevar$emlal$SelectDP$dp_data_path,
      data.table = savevar$emlal$DataFiles$name
    )
  },
    priority = 1,
    ignoreInit = TRUE
  )
  
  # Output -----------------------------------------------------
  return(savevar)
}

.saveDataFiles <- function(savevar, rv){
  tmp <- savevar$emlal$DataFiles
  
  if(!checkTruth(tmp))
    tmp <- data.frame(
      name = character(),
      size = character(),
      type = character(),
      datapath = character()
    )
  
  # -- Get files data
  tmp$datapath <- paste0(
    savevar$emlal$SelectDP$dp_data_path, 
    "/", rv$data_files$name
  )
  
  # -- set metadatapath
  tmp$metadatapath <- paste(
    savevar$emlal$SelectDP$dp_metadata_path,
    sub(
      "(.*)\\.[a-zA-Z0-9]*$",
      "attributes_\\1.txt",
      rv$data_files$name
    ),
    sep = "/"
  )
  
  # Set table name
  tmp$table_name <- rv$data_files$table_name
  # Set description
  tmp$description <- rv$data_files$description
  # Set URL
  tmp$url <- rv$data_files$url
  
  savevar$emlal$DataFiles <- tmp
  
  return(savevar)
}