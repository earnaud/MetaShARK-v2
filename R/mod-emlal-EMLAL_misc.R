#' @title MiscUI
#' 
#' @description UI for "last but not least" module
#'
#' @importFrom shiny h5
MiscUI <- function(id, title, dev, savevar, server) {
  ns <- NS(id)
  
  return(
    fluidPage(
      # Features UI ----
      column(
        10,
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
            
            # * Abstract ----
            bsCollapsePanel(
              title = "Abstract",
              value = 5,
              MiscellaneousUI(ns("abstract"), server = server)
            ),
            
            # * Methods ----
            bsCollapsePanel(
              title = "Methods",
              value = 5,
              MiscellaneousUI(ns("methods"), server = server)
            ),
            
            # * Keywords ----
            bsCollapsePanel(
              title = "Keywords",
              value = 3,
              tagList(
                column(6,
                  tagsinput::tagsTextInput(
                    ns("keywords"),
                    tags$p("List the keywords that best describe your dataset.
                  Type a 'tab' to separate each keyword.")
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
            
            # * Temporal coverage ----
            bsCollapsePanel(
              title = "Temporal coverage",
              value = 4,
              fluidRow(
                column(10, offset = 1,
                  dateRangeInput(
                    ns("temporal_coverage"),
                    "Dates between which dataset's content was produced",
                    max = Sys.Date(), 
                    autoclose = FALSE
                  )
                )
              )
            ),
            
            # * Additional Info ----
            bsCollapsePanel(
              title = "Additional Info",
              value = 5,
              MiscellaneousUI(
                ns("additional_info"),
                help_label = tags$p(
                  "If you have additional information that doesn't fall under the scope of the abstract or methods (e.g. a list of research articles or theses derived from this dataset) about your dataset, you may share it here."
                ),
                server = server
              )
            )
            
          )
        )
      ), # end of column1
      # Navigation UI ----
      column(
        2,
        navSidebar(
          ns("nav"),
          ... = tagList(
            if (dev) actionButton(ns("check"), "Dev Check")
          )
        )
      ) # end of column2
      
    ) # end of fluidPage
  ) # end of return
}


#' @title Misc
#' 
#' @describeIn MiscUI
#' 
#' @importFrom shiny dateRangeInput
#' @importFrom shinyjs enable disable
Misc <- function(input, output, session, savevar, globals) {
  ns <- session$ns
  
  if (globals$dev) {
    observeEvent(input$check, {
      browser()
    })
  }
  
  # Variable initialization ----
  rv <- reactiveValues(
    # Abstract
    abstract = reactiveValues(
      content = character(),
      file = paste(
        savevar$emlal$SelectDP$dp_path,
        "abstract.txt",
        sep = "/"
      )
    ),
    # Methods
    methods = reactiveValues(
      content = character(),
      file = paste(
        savevar$emlal$SelectDP$dp_path,
        "methods.txt",
        sep = "/"
      )
    ),
    # Keywords
    keywords =reactiveValues(
      keywords = character(),
      keywordsThesaurus = character()
    ),
    # Temporal coverage
    temporal_coverage = c(Sys.Date()-1, Sys.Date()),
    # Additional information
    additional_information = reactiveValues(
      content = character(),
      file = paste(
        savevar$emlal$SelectDP$dp_path,
        "additional_information.txt",
        sep = "/"
      )
    ),
    # Complete
    complete = FALSE
  )  
  
  # Fill ----
  # Abstract
  rv$abstract <- callModule(
    Miscellaneous,
    "abstract",
    savevar,
    rv = rv$abstract
  )
  
  # Methods
  rv$methods <- callModule(
    Miscellaneous,
    "methods",
    savevar,
    rv = rv$methods
  )
  
  # Keywords
  observeEvent(input$keywords,{
    rv$keywords$keywords <- input$keywords
    
    output$thesaurus <- renderUI({
      validate(
        need(rv$keywords$keywords, "No keyword input")
      )
      tagList(
        lapply(seq_along(rv$keywords$keywords), function(k_id){
          keyword <- rv$keywords$keywords[k_id]
          textInput(
            ns(paste0("thesaurus-for-", keyword)), 
            keyword 
          )
        })
      )
    })
    
  })
  
  # NOTE observers are still active after being deleted
  observe({
    validate(
      need(rv$keywords$keywords, "No keyword input")
    )
    sapply(seq_along(rv$keywords$keywords), function(k_id){
      keyword <- rv$keywords$keywords[k_id]
      input_id <- paste0("thesaurus-for-", keyword)
      validate(
        need(isTruthy(input[[input_id]]) ||
            input[[input_id]] == "", 
          "No thesaurus input")
      )
      rv$keywords$keywordsThesaurus[k_id] <- input[[input_id]]
    })
    
  })
  
  # Temporal coverage
  observeEvent(input$temporal_coverage, {
    rv$temporal_coverage <- input$temporal_coverage
  })
  
  # Additional information
  rv$additional_info <- callModule(
    Miscellaneous,
    "additional_info",
    savevar,
    rv = rv$additional_information
  )
  
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
  callModule(
    nextTab, "nav",
    globals, "Misc"
  )
  callModule(
    prevTab, "nav",
    globals
  )
  
  # Complete ----
  observe({
    rv$complete <- all(
      # keywords
      isTruthy(rv$keywords$keywords)
      # check temporal coverage
    )
    
    if(rv$complete)
      enable("nav-nextTab")
    else
      disable("nav-nextTab")
  })
  
  # Process data ----
  observeEvent(input[["nav-nextTab"]], {
    savevar$emlal$Misc <- rv
    
    message("Writing 'abstract.txt'.")
    write.text(
      rv$abstract$content,
      paste0(
        savevar$emlal$SelectDP$dp_path,
        "/abstract.txt"
      )
    )
    
    message("Writing 'methods.txt'.")
    write.text(
      rv$methods$content,
      paste0(
        savevar$emlal$SelectDP$dp_path,
        "/methods.txt"
      )
    )
    
    message("Writing 'keywords.txt'.")
    fwrite(
      data.frame(
        keyword = rv$keywords$keywords,
        keywordThesaurus = rv$keywords$keywordsThesaurus
      ),
      paste0(
        savevar$emlal$SelectDP$dp_path,
        "/keywords.txt"
      ),
      sep = "\t"
    )
    
    message("Writing 'additional_info.txt'.")
    write.text(
      rv$additional_info$content,
      paste0(
        savevar$emlal$SelectDP$dp_path,
        "/additional_info.txt"
      )
    )
    
    message("Done with Misc.")
  }, priority = 1)
  
  # Output ----
  return(savevar)
}