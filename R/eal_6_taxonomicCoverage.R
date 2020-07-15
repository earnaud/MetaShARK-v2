#' @import shiny
#' @importFrom shinyjs hidden disabled
#' 
#' @noRd
TaxCovUI <- function(id, main.env) {
  ns <- NS(id)
  
  return(
    fluidPage(
      fluidRow(
        column(
          6,
          uiOutput(NS(id, "taxa.table")),
          uiOutput(NS(id, "taxa.col"))
        ),
        column(
          6,
          uiOutput(NS(id, "taxa.name.type")),
          uiOutput(NS(id, "taxa.authority"))
        )
      )
    ) # end of fluidPage
  ) # end of return
}

#' @import shiny
#' @importFrom stringr str_extract_all
#' @importFrom dplyr %>%
#' @importFrom EMLassemblyline template_taxonomic_coverage
#' @importFrom shinyjs onclick
#' 
#' @noRd
TaxCov <-function(id, main.env) {
  moduleServer(id, function(input, output, session){
    save.variable <- main.env$save.variable
    ns <- session$ns
    
    # Variable Initialization ----
    rv <- reactiveValues(
      taxa.table = character(),
      taxa.col = character(),
      taxa.name.type = character(),
      taxa.authority = character(),
      complete = FALSE
    )
    
    if (sapply(printReactiveValues(save.variable$emlal$TaxCov), checkTruth) %>% all()) {
      rv$taxa.table <- save.variable$emlal$TaxCov$taxa.table
      rv$taxa.col <- save.variable$emlal$TaxCov$taxa.col
      rv$taxa.name.type <- save.variable$emlal$TaxCov$taxa.authority
      rv$taxa.authority <- save.variable$emlal$TaxCov$taxa.authority
    }
    
    # Set UI ====
    
    # * taxa.table ====
    output$taxa.table <- renderUI({
      isolate({
        data.files <- basename(save.variable$emlal$DataFiles$datapath)
        value <- rv$taxa.table
        if(isFALSE(value %in% data.files))
          value <- NULL
      })
      
      selectInput(
        NS(id, "taxa.table"),
        "Files containing taxonomic references",
        choices = data.files,
        selected = value,
        multiple = FALSE
      )
    })
    
    # * taxa.col ====
    output$taxa.col <- renderUI({
      .ind <- match(rv$taxa.table, save.variable$emlal$DataFiles$name)
      if(isTruthy(.ind)) {
        choices <- isolate(save.variable$emlal$Attributes[[.ind]]$attributeName)
        value <- isolate(rv$taxa.col)
        if(isFALSE(value %in% choices))
          value <- NULL
        .embed <- tagList
      }
      else {
        choices <- NULL
        value <- NULL 
        .embed <- shinyjs::disabled
      }
      
      .embed(
        selectInput(
          NS(id, "taxa.col"),
          "Columns from selected files",
          choices = choices,
          selected = value
        )
      )
    })
    
    # * taxa.name.type ====
    output$taxa.name.type <- renderUI({
      isolate({
        value <- rv$taxa.name.type
        if(isTRUE(value == "both"))
          value <- c("scientific", "common")
        else if(isFALSE(value %in% c("scientific", "common")))
          value <- NULL
      })
      
      checkboxGroupInput(
        NS(id, "taxa.name.type"),
        "Select one or both taxonomic name notation",
        c("scientific", "common"),
        selected = value
      )
    })
    
    # * taxa.authority ====
    output$taxa.authority <- renderUI({
      isolate({
        taxa.authority <- main.env$FORMATS$taxa.authorities
        choices <- taxa.authority$authority
        value <- if(isTruthy(rv$taxa.authority)){
          taxa.authority %>%
            dplyr::filter(id == rv$taxa.authority) %>%
            dplyr::select(authority)
        }
      })
      
      selectInput(
        NS(id, "taxa.authority"),
        "Select taxonomic authority.ies",
        choices = choices,
        selected = value,
        multiple = TRUE
      )
    })
    
    # Taxonomic coverage input ----
    
    # * Taxa files ====
    observeEvent(input$taxa.table,
      {
        # invalid-selected/no value(s)
        if (!isTruthy(input$taxa.table)) {
          shinyjs::disable("taxa.col")
          
          updateSelectizeInput(
            session,
            "taxa.col",
            choices = list("(None selected)" = NULL)
          )
        }
        # valid-selected value(s)
        else {
          shinyjs::enable("taxa.col")
          
          taxa.col.list <- lapply(input$taxa.table, function(file) {
            all.files <- save.variable$emlal$DataFiles
            file <- all.files[all.files$name == file, "datapath"]
            df <- readDataTable(file, stringsAsFactors = FALSE)
            return(colnames(df))
          })
          names(taxa.col.list) <- input$taxa.table
          
          .update.var <- if (isTRUE(rv$taxa.col %in% taxa.col.list))
            rv$taxa.col
          else
            NULL
          
          updateSelectizeInput(
            session,
            "taxa.col",
            choices = taxa.col.list,
            selected = .update.var
          )
        }
        
        # save
        rv$taxa.table <- list(input$taxa.table)
        isolate({save.variable$emlal$TaxCov$taxa.table <- rv$taxa.table})
      },
      priority = -1
    )
    
    # * Taxa columns to read ====
    observeEvent(input$taxa.col, {
      req(input$taxa.col)
      
      rv$taxa.col <- input$taxa.col
      isolate({save.variable$emlal$TaxCov$taxa.col <- rv$taxa.col})
    })
    
    # * Taxa type ====
    observeEvent(input$taxa.name.type, {
      req(input$taxa.name.type)
      
      rv$taxa.name.type <- input$taxa.name.type
      
      if ("scientific" %in% rv$taxa.name.type &&
          "common" %in% rv$taxa.name.type) {
        rv$taxa.name.type <- "both"
      }
      save.variable$emlal$TaxCov$taxa.name.type <- rv$taxa.name.type
    })
    
    # * Taxa authority ====
    observeEvent(input$taxa.authority, {
      req(input$taxa.authority)
      
      rv$taxa.authority <- main.env$FORMATS$taxa.authorities %>%
        dplyr::filter(authority %in% input$taxa.authority) %>%
        dplyr::select(id) %>%
        unlist()
      save.variable$emlal$TaxCov$taxa.authority <- rv$taxa.authority
    })
    
    # Saves ----
    main.env$EAL$completed <- TRUE
    observe({
      req(main.env$EAL$current == "Taxonomic Coverage")
      
      rv$complete <- all(
        length(rv$taxa.table) > 0 &&
          length(rv$taxa.col) > 0 &&
          length(rv$taxa.name.type) > 0 &&
          length(rv$taxa.authority) > 0
      )
    })
    
    
    # observeEvent(NSB$SAVE,
    shinyjs::onclick(
      "fill-wizard-save",
      asis = TRUE,
      add = TRUE,
      {
        req(utils::tail(main.env$EAL$history, 1) == "Taxonomic Coverage")
        
        save.variable <- saveReactive(
          save.variable = savevar,
          rv = list(TaxCov = rv)
        )
      },
      ignoreInit = TRUE
    )
    
    # Process data ----
    observeEvent(EAL$.next,
      {
        req(main.env$EAL$current == "Taxonomic Coverage")
        
        choices <- c(
          "Yes - Taxonomic coverage will be written as file" = if (rv$complete) 1 else NULL,
          "No - Taxonomic coverage will be left blank" = 0
        )
        
        nextTabModal <- modalDialog(
          title = "Proceed Taxonomic Coverage",
          tagList(
            "You are getting ready to proceed.",
            radioButtons(
              NS(id, "filled"),
              "Is Taxonomic Coverage filled?",
              choices = choices
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(NS(id, "confirm"), "Proceed")
          )
        )
        
        showModal(nextTabModal)
      },
      ignoreInit = TRUE
    )
    
    observeEvent(input$confirm, {
      removeModal()
      main.env$EAL$page <- 7
      EAL$tag.list <- tagList()
      
      # Write files
      if (input$filled == "1") {
        save.variable <- saveReactive(
          save.variable,
          rv = list(TaxCov = rv)
        )
        
        # Template coverage
        try(
          template_taxonomic_coverage(
            save.variable$emlal$SelectDP$dp.metadata.path,
            save.variable$emlal$SelectDP$dp.data.path,
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
      else {
        showNotification(
          "Taxonomic Coverage has been skipped.",
          type = "message"
        )
      }
    })
    
    # Output ----
    return(save.variable)
  })
}
