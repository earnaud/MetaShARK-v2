#' @title Taxonomic coverage
#'
#' @description UI part for the Taxonomic Coverage module
#'
#' @importFrom shiny NS fluidPage fluidRow column tagList tags actionButton
#' @importFrom shinyjs hidden disabled
TaxCovUI <- function(id, title, dev, data.files, taxa.authorities) {
  ns <- NS(id)

  return(
    fluidPage(
      # Features UI ----
      column(
        10,
        fluidRow(
          tags$h4(title),
          tags$p("This step is facultative."),
          # taxa.table(s)
          column(6,
            selectizeInput(
              ns("taxa.table"),
              "Files containing taxonomic references",
              choices = basename(data.files),
              multiple = TRUE
            ),
            # taxa columns -- to select from 1st selectizeInput
            disabled(
              selectizeInput(
                ns("taxa.col"),
                "Columns from selected files",
                choices = list("(None selected)" = NULL)
              )
            )
          ),
          column(6,
            checkboxGroupInput(
              ns("taxa.name.type"),
              "Select a taxonomic name notation (select one or both):", 
              c("scientific name", "common name")
            ),
            selectizeInput(
              ns("taxa.authority"),
              "Files containing taxonomic references",
              choices = taxa.authorities$authority,
              multiple = TRUE
            )
          )
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(
          ns("nav"),
          # disableNext = TRUE,
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
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
TaxCov <- function(input, output, session, savevar, globals) {
  ns <- session$ns

  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }

  # Variable Initialization ----
  rv <- reactiveValues()

  # Taxonomic coverage input ----

  # Taxa files
  observe({
    # invalid-selected/no value(s)
    if(!isTruthy(input$taxa.table)){
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
        all.files <- savevar$emlal$DataFiles$dp_data_files
        file <- all.files[all.files$name == file, "datapath"]
        df <- fread(file)
        return(colnames(df))
      })
      names(taxa.col.list) <- input$taxa.table
      
      updateSelectizeInput(
        session,
        "taxa.col",
        choices = taxa.col.list
      )
    }
    
    # save
    rv$taxa.table <- paste(
      savevar$emlal$SelectDP$dp_path,
      savevar$emlal$SelectDP$dp_name,
      "data_objects",
      input$taxa.table,
      sep = "/"
    )
  })
    
  # Taxa columns to read
  observeEvent(input$taxa.col, {
    req(input$taxa.col)
    
    rv$taxa.col <- input$taxa.col
  })
    
  # Taxa columns to read
  observeEvent(input$taxa.name.type, {
    req(input$taxa.name.type)
    
    rv$taxa.name.type <- input$taxa.name.type
  })
  
  # Taxa columns to read
  observeEvent(input$taxa.authority, {
    req(input$taxa.authority)
    
    rv$taxa.authority <- input$taxa.authority
  })
  
  # Navigation buttons ----
  callModule(
    onQuit, "nav",
    # additional arguments
    globals, savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  callModule(
    onSave, "nav",
    # additional arguments
    savevar,
    savevar$emlal$SelectDP$dp_path,
    savevar$emlal$SelectDP$dp_name
  )
  # callModule(
  #   nextTab, "nav",
  #   globals, "TaxCov"
  # )
  callModule(
    prevTab, "nav",
    globals
  )

  # Complete ----
  observe({
    rv$complete <- all(
      length(rv$taxa.table) > 0 &&
      length(rv$taxa.col) > 0 &&
      length(rv$taxa.name.type) > 0 &&
      length(rv$taxa.authority) > 0 
    )
  })
  
  # Process data ----
  observeEvent(input$`nav-nextTab`, {
    choices = c(
      "Yes - Taxonomic coverage will be written as file" = if(rv$complete) 1 else NULL,
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
        actionButton(ns("confirm"),"Proceed")
      )
    )
    
    showModal(nextTabModal)
  })
  
  observeEvent(input$confirm, {
    removeModal()
    
    if(input$filled != "0"){
      # TODO EDIutils::validate_file_names invalidate inputs
      # edit template
      template_taxonomic_coverage(
        paste(
          savevar$emlal$SelectDP$dp_path,
          savevar$emlal$SelectDP$dp_name,
          "metadata_templates",
          sep = "/"
        ),
        paste(
          savevar$emlal$SelectDP$dp_path,
          savevar$emlal$SelectDP$dp_name,
          "data_objects",
          sep = "/"
        ),
        taxa.table = rv$taxa.table,
        taxa.col = rv$taxa.col,
        taxa.name.type = rv$taxa.name.type,
        taxa.authority = rv$taxa.authority
      )
      
      # save
      savevar$emlal$TaxCov <- reactiveValues(
        taxa.table = rv$taxa.table,
        taxa.col = rv$taxa.col,
        taxa.name.type = rv$taxa.name.type,
        taxa.authority = rv$taxa.authority
      )
    }
    
    globals$EMLAL$NAVIGATE <- globals$EMLAL$NAVIGATE+1
    globals$EMLAL$HISTORY <- c(globals$EMLAL$HISTORY, "TaxCov")
  }, priority = 1)
  
  # Output ----
  return(savevar)
}