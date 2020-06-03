#' @title MiscUI
#'
#' @description UI for "last but not least" module
#'
#' @importFrom shiny h5
#' @importFrom tagsinput tagsTextInput
#' @importFrom data.table fread
MiscUI <- function(id, title, dev, savevar, server) {
  ns <- NS(id)
  
  keywords <- fread(
    paste0(savevar$emlal$SelectDP$dp_metadata_path, "/keywords.txt"),
    data.table = FALSE, stringsAsFactors = FALSE
  )
  if(checkTruth(keywords)){
    kw <- keywords$keyword %>%
      strsplit(split = ",") %>%
      unlist %>%
      paste(collapse = ",")
  }

    
  return(
    fluidPage(
      fluidRow(
        HTML("
            <h5>DISCLAIMER</h5>
            <ul>
              <li>Do not use special characters, symbols, formatting, or hyperlinks (URLs are acceptable).</li>
              <li>Any file selected will not be overwritten but will be used as a template.</li>
            </ul>
          "),
        bsCollapse(
          id = ns("Miscs"),
          
          # * Abstract -----------------------------------------------------
          bsCollapsePanel(
            title = with_red_star("Abstract"),
            value = 1,
            MiscellaneousUI(
              ns("abstract"),
              server = server,
              value = readPlainText(
                paste0(savevar$emlal$SelectDP$dp_metadata_path, "/abstract.txt")
              )
            )
          ),
          
          # * Methods -----------------------------------------------------
          bsCollapsePanel(
            title = with_red_star("Methods"),
            value = 2,
            MiscellaneousUI(
              ns("methods"),
              server = server,
              value = readPlainText(
                paste0(savevar$emlal$SelectDP$dp_metadata_path, "/methods.txt")
              )
            )
          ),
          
          # * Keywords -----------------------------------------------------
          bsCollapsePanel(
            title = with_red_star("Keywords"),
            value = 3,
            tagList(
              column(6,
                tagsTextInput(
                  ns("keywords"),
                  tags$p("List the keywords that best describe your dataset.
                    Type a 'tab' to separate each keyword."),
                  value = if(checkTruth(keywords)) keywords[, 1] else c()
                )
              ),
              column(6,
                tags$h4("Associated thesaurus"),
                tags$p("NOTE: use of thesaurus will be improved. Currently,
                    no control is made about thesaurus input field and this
                    can be invalided."),
                tags$p("You may associate a thesaurus to each keyword."),
                uiOutput(ns("thesaurus"))
              )
            )
          ),
          
          # * Temporal coverage -----------------------------------------------------
          bsCollapsePanel(
            title = "Temporal coverage",
            value = 4,
            fluidRow(
              column(10,
                offset = 1,
                dateRangeInput(
                  ns("temporal_coverage"),
                  "Dates between which dataset's content was produced",
                  max = Sys.Date(),
                  autoclose = FALSE
                )
              )
            )
          ),
          
          # * Additional Info -----------------------------------------------------
          bsCollapsePanel(
            title = "Additional Info",
            value = 5,
            MiscellaneousUI(
              ns("additional_info"),
              help_label = tags$p(
                "If you have additional information that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset) about your dataset, you may share it here."
              ),
              server = server,
              value = readPlainText(
                paste0(savevar$emlal$SelectDP$dp_metadata_path, "/additional_info.txt")
              )
            )
          )
        )
      )
    ) # end of fluidPage
  ) # end of return
}


#' @title Misc
#'
#' @describeIn MiscUI
#'
#' @importFrom shiny dateRangeInput
#' @importFrom shinyjs enable disable
#' @importFrom data.table fread
Misc <- function(input, output, session,
  savevar, globals, server, NSB) {
  ns <- session$ns
  
  if(globals$dev)
    onclick("dev", {
      req(globals$EMLAL$NAVIGATE == 8)
      browser()
    }, asis=TRUE)
  
  # Variable initialization -----------------------------------------------------
  kw <- fread(
    paste0(savevar$emlal$SelectDP$dp_metadata_path, "/keywords.txt"),
    data.table = FALSE, stringsAsFactors = FALSE
  )
  
  rv <- reactiveValues(
    # Abstract
    abstract = reactiveValues(
      content = character(),
      file = paste(
        isolate(savevar$emlal$SelectDP$dp_metadata_path),
        "abstract.txt",
        sep = "/"
      )
    ),
    # Methods
    methods = reactiveValues(
      content = character(),
      file = paste(
        isolate(savevar$emlal$SelectDP$dp_metadata_path),
        "methods.txt",
        sep = "/"
      )
    ),
    # Keywords
    keywords = reactiveValues(
      keyword = kw$keyword,
      keywordThesaurus = kw$keywordThesaurus
    ),
    # Temporal coverage
    temporal_coverage = c(Sys.Date() - 1, Sys.Date()),
    # Additional information
    additional_information = reactiveValues(
      content = character(),
      file = paste(
        isolate(savevar$emlal$SelectDP$dp_metadata_path),
        "additional_info.txt",
        sep = "/"
      )
    )
  )
  
  # Fill -----------------------------------------------------
  # * Abstract ====
  rv$abstract <- callModule(
    Miscellaneous,
    "abstract",
    savevar,
    rv = rv$abstract,
    server = server
  )
  
  # * Methods ====
  rv$methods <- callModule(
    Miscellaneous,
    "methods",
    savevar,
    rv = rv$methods,
    server = server
  )
  
  # * Keywords ====
  observeEvent(input$keywords, {
    req(input$keywords)
    
    rv$keywords$keyword <- unique(input$keywords)
    
    output$thesaurus <- renderUI({
      validate(
        need(checkTruth(rv$keywords$keyword), "No keyword input")
      )
      tagList(
        lapply(seq_along(rv$keywords$keyword), function(k_id) {
          keyword <- rv$keywords$keyword[k_id]
          valKT <- rv$keywords$keywordThesaurus[k_id]
          
          textInput(
            ns(paste0("thesaurus-for-", keyword)),
            keyword,
            value = if(isTruthy(valKT)) valKT else ""
          )
        })
      )
    })
  })
  
  # NOTE observers are still active after being deleted
  observe({
    validate(
      need(rv$keywords$keyword, "No keyword input")
    )
    sapply(seq_along(rv$keywords$keyword), function(k_id) {
      keyword <- rv$keywords$keyword[k_id]
      input_id <- paste0("thesaurus-for-", keyword)
      .val <- if(isTruthy(input[[input_id]])) input[[input_id]] else ""
        
      rv$keywords$keywordThesaurus[k_id] <- .val
    })
  })
  
  # * Temporal coverage ====
  if (!is.null(savevar$emlal$Misc$temporal_coverage)) {
    rv$temporal_coverage <- savevar$emlal$Misc$temporal_coverage
    updateDateRangeInput(
      session,
      "temporal_coverage",
      start = rv$temporal_coverage[1],
      end = rv$temporal_coverage[2]
    )
  }
  observeEvent(input$temporal_coverage, {
    rv$temporal_coverage <- input$temporal_coverage
  })
  
  # * Additional information ====
  rv$additional_info <- callModule(
    Miscellaneous,
    "additional_info",
    savevar,
    rv = rv$additional_information,
    server = server
  )
  
  # Saves -----------------------------------------------------
  observe({
    globals$EMLAL$COMPLETE_CURRENT <- all(
      isTruthy(rv$abstract$content()) &&
        isTruthy(rv$methods$content()) &&
        isTruthy(rv$keywords$keyword) &&
        isTruthy(rv$temporal_coverage)
    )
  })
  
  observeEvent(NSB$SAVE, {
    req(globals$EMLAL$CURRENT == "Miscellaneous")
    
    savevar <- saveReactive(
      savevar = savevar,
      rv = list(Misc = rv)
    )
  }, ignoreInit = TRUE)
  
  # Process data -----------------------------------------------------
  observeEvent(NSB$NEXT, {
    req(globals$EMLAL$CURRENT == "Miscellaneous")
    
    savevar <- saveReactive(
      savevar = savevar, 
      rv = list(Misc = rv))
  }, priority = 1, ignoreInit = TRUE)
  
  # Output -----------------------------------------------------
  return(savevar)
}