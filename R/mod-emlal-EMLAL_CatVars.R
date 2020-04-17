#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny NS fluidPage column fluidRow actionButton tags tagList
CatVarsUI <- function(id, title, dev) {
  ns <- NS(id)
  
  return(
    fluidPage(
      fluidRow(
        # Navigation
        fluidRow(
          column(1,
            actionButton(ns("file_prev"),
              "",
              icon("chevron-left")
            )
          ),
          column(10,
            uiOutput(ns("current_file"),
              inline = TRUE
            )
          ),
          column(1,
            actionButton(ns("file_next"),
              "",
              icon("chevron-right")
            )
          ),
          style = "padding: 5px;"
        ),
        # content form
        uiOutput(ns("edit_catvar"))
      )
    ) # end of fluidPage
  ) # end of return
}

#' @title Geographic coverage
#'
#' @description UI part for the Geographic Coverage module
#'
#' @importFrom shiny observeEvent callModule tags tagList reactiveValues renderUI textAreaInput
#' @importFrom dplyr %>% filter select mutate
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @importFrom shinyjs onclick
CatVars <- function(input, output, session, 
  savevar, globals, NSB) {
  ns <- session$ns
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 4)
      browser()
    }, asis=TRUE)
  
  # variables initialization -----------------------------------------------------
  rv <- reactiveValues(
    catvarFiles = list.files(
      savevar$emlal$SelectDP$dp_metadata_path,
      pattern = "catvar",
      full.names = TRUE
    ),
    currentIndex = 1
  )
  
  # update current file
  observeEvent(rv$currentIndex, {
    rv$currentFile <- basename(rv$catvarFiles[rv$currentIndex])
  }, priority = 1, ignoreInit = FALSE)
  
  # Set each reactivevalues per file
  sapply(rv$catvarFiles, function(file_path){
    file_name <- basename(file_path)
    rv[[file_name]] <- reactiveValues()
    
    # * Init data frame ====
    rv[[file_name]]$CatVars <- fread(
      file_path,
      data.table = FALSE, stringsAsFactors = FALSE,
      na.strings = "NA"
      ) %>% 
      mutate(
        definition = if(definition == "NA" || is.na(definition))
          paste("Value:", code, "for attribute:", attributeName)
        else
          definition
      )
    
    # * Write UI ====
    .content <- lapply(unique(rv[[file_name]]$CatVars$attributeName), function(attribute){
        # get codes aka values for `attribute` in catvar_*.txt
        codes <- rv[[file_name]]$CatVars %>%
          filter(attributeName == attribute) %>%
          select(code)
        
        bsCollapsePanel(
          title = attribute,
          # value = attribute,
          ... = tagList(
            lapply(unlist(codes), function(cod){
              inputId <- paste(
                attribute, 
                cod%>%
                  gsub("[ [:punct:]]","", .),
                sep="-"
              )
              
              textAreaInput(
                ns(inputId), 
                cod,
                value = rv[[file_name]]$CatVars %>%
                  filter(attributeName == attribute, code == cod) %>%
                  select(definition)
              ) 
            })
          ) # end of "tagapply" -- text areas
        ) # end of bsCollapsePanel
      }) # end of "tagapply" -- collapse boxes
    rv[[file_name]]$UI <- do.call(
      bsCollapse,
      c(
        .content,
        id = file_name,
        multiple = FALSE
      )
    )
    
    # * Write server ====
    rv[[file_name]]$obs <- sapply(seq(dim(rv[[file_name]]$CatVars)[1]), function(row){
      inputId <- paste(
        rv[[file_name]]$CatVars$attributeName[row],
        rv[[file_name]]$CatVars$code[row] %>%
          gsub("[ [:punct:]]","", .),
        sep="-"
      )
      
      return(
        observeEvent(input[[inputId]], {
          req(input[[inputId]])
          rv[[file_name]]$CatVars[row, "definition"] <- input[[inputId]]
        }, suspended = TRUE)
      )
    })
  })

  # Navigation buttons -----------------------------------------------------
  # Previous file
  onclick("file_prev", {
    req(rv$currentIndex, rv$catvarFiles)
    savevar$emlal$CatVars[[rv$currentFile]] <- rv[[rv$currentFile]]$CatVars
    if (rv$currentIndex > 1) {
      rv$currentIndex <- rv$currentIndex - 1
    }
  })
  
  # Next file
  onclick("file_next", {
    req(rv$currentIndex, rv$catvarFiles)
    savevar$emlal$CatVars[[rv$currentFile]] <- rv[[rv$currentFile]]$CatVars
    if (rv$currentIndex < length(rv$catvarFiles)) {
      rv$currentIndex <- rv$currentIndex + 1
    }
  })
  
  # Current file
  output$current_file <- renderUI({
    tags$div(
      rv$currentFile,
      class = "ellipsis",
      style = paste0(
        "display: inline-block;
        font-size:14pt;
        text-align:center;
        width:100%;
        background: linear-gradient(90deg, #3c8dbc ",
        round(100 * rv$currentIndex / length(rv$catvarFiles)),
        "%, white ",
        round(100 * rv$currentIndex / length(rv$catvarFiles)),
        "%);"
      )
    )
  })
  
  # Set UI -----------------------------------------------------
  output$edit_catvar <- renderUI({
    validate(
      need(rv[[rv$currentFile]]$UI, "No UI set.")
    )
    rv[[rv$currentFile]]$UI
  }) # end of renderUI
  
  # Set Server -----------------------------------------------------
  
  # Suspend observers
  observeEvent(rv$currentIndex, {
    req(rv$currentFile)
    sapply(rv[[rv$currentFile]]$obs, function(obs){
      obs$suspend()
    })
  }, priority = 2)
  
  # Run observers
  observeEvent(rv$currentFile, {
    sapply(rv[[rv$currentFile]]$obs, function(obs){
      obs$resume()
    })
  }, priority = 0)
  
  # Saves -----------------------------------------------------
  observe({
    complete <- sapply(basename(rv$catvarFiles), function(file_name){
      all(sapply(rv[[file_name]]$CatVars$definition, isTruthy))
    })
    
    globals$EMLAL$COMPLETE_CURRENT <- all(complete)
    
    req(isTRUE(complete[rv$currentIndex]))
    
    savevar$emlal$CatVars[[rv$currentFile]] <- rv[[rv$currentFile]]$CatVars
  })
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Categorical Variables")
    
    sapply(rv$catvarFiles, function(file_path){
      file_name <- basename(file_path)
      savevar$emlal$CatVars[[file_name]] <- rv[[file_name]]$CatVars
      
      fwrite(
        savevar$emlal$CatVars[[file_name]],
        file_path,
        sep = "\t"
      )
    })
  }, priority = 1, ignoreInit = TRUE)
  
  # Output -----------------------------------------------------
  return(savevar)
}
