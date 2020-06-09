#' @title Taxonomic coverage
#'
#' @description UI part for the Taxonomic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
#' @importFrom shinyjs hidden disabled
TaxCovUI <- function(
  id, title, dev, data.files, taxa.authorities, default
) {
  ns <- NS(id)
  
  if(isFALSE(checkTruth(default)))
    default = ""
  
  return(
    fluidPage(
      fluidRow(
        column(6,
          # taxa table(s)
          selectizeInput(
            ns("taxa.table"),
            "Files containing taxonomic references",
            choices = basename(data.files),
            selected = if(isTRUE(default$taxa.table %in% basename(data.files)))
              unlist(default$taxa.table),
            multiple = TRUE
          ),
          # taxa columns
          disabled(
            selectizeInput(
              ns("taxa.col"),
              "Columns from selected files",
              choices = list("(None selected)" = NULL)
            )
          )
        ),
        column(6,
          # taxa name types -- scientific or common
          checkboxGroupInput(
            ns("taxa.name.type"),
            "Select one or both taxonomic name notation",
            c("scientific", "common"),
            selected = if(default$taxa.name.type == "both")
              c("scientific", "common")
            else
              default$taxa.name.type
          ),
          # taxa authority
          selectizeInput(
            ns("taxa.authority"),
            "Select taxonomic authority.ies",
            choices = taxa.authorities$authority,
            selected = taxa.authorities %>% 
              filter(id == default$taxa.authority) %>%
              select(authority),
            multiple = TRUE
          )
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @title Taxonomic coverage
#'
#' @description server part for the Taxonomic Coverage module
#'
#' @importFrom shiny observeEvent callModule
#' @importFrom shinyBS updateCollapse
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom EMLassemblyline template_taxonomic_coverage
#' @importFrom shinyjs onclick
TaxCov <- function(input, output, session,
  savevar, globals, NSB) {
  ns <- session$ns
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 6)
      browser()
    }, asis=TRUE)
  
  # Variable Initialization -----------------------------------------------------
  rv <- reactiveValues(
    taxa.table = character(), 
    taxa.col = character(),
    taxa.name.type = character(),
    taxa.authority = character(),
    complete = FALSE
  )
  
  if(sapply(printReactiveValues(savevar$emlal$TaxCov), checkTruth) %>% all){
    rv$taxa.table <- savevar$emlal$TaxCov$taxa.table
    rv$taxa.col <- savevar$emlal$TaxCov$taxa.col
    rv$taxa.name.type <- savevar$emlal$TaxCov$taxa.authority
    rv$taxa.authority <- savevar$emlal$TaxCov$taxa.authority
  }
  
  # observeEvent requires UI to be set
  observeEvent(names(input), {
    updateVar <- rv$taxa.table
    updateSelectizeInput(
      session,
      "taxa.table",
      selected = updateVar
    )
    
    enable("taxa.col")
    updateVar <- rv$taxa.col
    updateSelectizeInput(
      session,
      "taxa.col",
      selected = updateVar
    )
    
    updateVar <- rv$taxa.name.type
    if (isTRUE(updateVar == "both")) {
      updateVar <- c("scientific", "common")
    }
    updateCheckboxGroupInput(
      session,
      "taxa.name.type",
      selected = updateVar
    )
    
    updateVar <- globals$FORMAT$AUTHORITIES %>%
      filter(id == rv$taxa.authority) %>%
      select(authority)
    updateSelectizeInput(
      session,
      "taxa.authority",
      selected = updateVar
    )
  },once = TRUE)
  
  # Taxonomic coverage input -----------------------------------------------------
  
  # Taxa files
  observeEvent(input$taxa.table, {
    # invalid-selected/no value(s)
    if (!isTruthy(input$taxa.table)) {
      disable("taxa.col")
      
      updateSelectizeInput(
        session,
        "taxa.col",
        choices = list("(None selected)" = NULL)
      )
    }
    # valid-selected value(s)
    else {
      enable("taxa.col")
      
      taxa.col.list <- lapply(input$taxa.table, function(file) {
        all.files <- savevar$emlal$DataFiles
        file <- all.files[all.files$name == file, "datapath"]
        df <- readDataTable(file, stringsAsFactors = FALSE)
        return(colnames(df))
      })
      names(taxa.col.list) <- input$taxa.table
      
      updateVar <- if(savevar$emlal$TaxCov$taxa.col %in% taxa.col.list)
        savevar$emlal$TaxCov$taxa.col
      else
        NULL
      
      updateSelectizeInput(
        session,
        "taxa.col",
        choices = taxa.col.list,
        selected = updateVar
      )
    }
    
    # save
    rv$taxa.table <- list(input$taxa.table)
    savevar$emlal$TaxCov$taxa.table <- rv$taxa.table
  }, priority = -1)
  
  # Taxa columns to read
  observeEvent(input$taxa.col, {
    req(input$taxa.col)
    
    rv$taxa.col <- input$taxa.col
    savevar$emlal$TaxCov$taxa.col <- rv$taxa.col
  })
  
  # Taxa columns to read
  observeEvent(input$taxa.name.type, {
    req(input$taxa.name.type)
    
    rv$taxa.name.type <- input$taxa.name.type
    
    if ("scientific" %in% rv$taxa.name.type &&
        "common" %in% rv$taxa.name.type) {
      rv$taxa.name.type <- "both"
    }
    savevar$emlal$TaxCov$taxa.name.type <- rv$taxa.name.type
  })
  
  # Taxa columns to read
  observeEvent(input$taxa.authority, {
    req(input$taxa.authority)
    
    rv$taxa.authority <- globals$FORMAT$AUTHORITIES %>%
      filter(authority %in% input$taxa.authority) %>%
      select(id) %>%
      unlist()
    savevar$emlal$TaxCov$taxa.authority <- rv$taxa.authority
  })
  
  # Saves -----------------------------------------------------
  globals$EMLAL$COMPLETE_CURRENT <- TRUE
  observe({
    req(globals$EMLAL$CURRENT == "Taxonomic Coverage")
    
    rv$complete <- all(
      length(rv$taxa.table) > 0 &&
        length(rv$taxa.col) > 0 &&
        length(rv$taxa.name.type) > 0 &&
        length(rv$taxa.authority) > 0
    )
  })
  
  observeEvent(NSB$SAVE, {
    req(tail(globals$EMLAL$HISTORY,1) == "Taxonomic Coverage")
    
    savevar <- saveReactive(
      savevar = savevar, 
      rv = list(TaxCov = rv)
    )
  }, ignoreInit = TRUE)
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Taxonomic Coverage")
    
    choices <- c(
      "Yes - Taxonomic coverage will be written as file" = if (rv$complete) 1 else NULL,
      "No - Taxonomic coverage will be left blank" = 0
    )
    
    nextTabModal <- modalDialog(
      title = "Proceed Taxonomic Coverage",
      tagList(
        "You are getting ready to proceed.",
        radioButtons(
          ns("filled"),
          "Is Taxonomic Coverage filled?",
          choices = choices
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm"), "Proceed")
      )
    )
    
    showModal(nextTabModal)
  }, ignoreInit = TRUE)
  
  onclick("confirm", {
    removeModal()
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE + 1
    NSB$tagList <- tagList()
    
    # Write files
    if (input$filled == "1") {
      savevar <- saveReactive(
        savevar, 
        rv = list(TaxCov = rv)
      )
      
      # Template coverage
      try(
        template_taxonomic_coverage(
          savevar$emlal$SelectDP$dp_metadata_path,
          savevar$emlal$SelectDP$dp_data_path,
          taxa.table = rv$taxa.table,
          taxa.col = rv$taxa.col,
          taxa.name.type = rv$taxa.name.type,
          taxa.authority = rv$taxa.authority
        )
      )
      showNotification(
        "Taxonomic Coverage has been written.",
        type = "message"
      )
    }
    else
      showNotification(
        "Taxonomic Coverage has been skipped.",
        type = "message"
      )
  })
  
  # Output -----------------------------------------------------
  return(savevar)
}
